// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3803: a parenthesized field assignment in value position (`Transcript show:
//! (self.n := 5)`) is a legal expression (`docs/beamtalk-language-features.md`, "Parenthesized
//! assignment") whose `State` rebind must reach the statement sequence that consumes it.
//!
//! `generate_field_assignment_open` renders the write to an opaque `Statement` plus a value
//! variable and produces no `Bind`, so a consumer that compiled the assignment as a plain
//! sub-expression scoped the `State{n}` rebind inside the send's argument: the reply and
//! every later statement then read an unbound `State{n}` (`erlc`: `unbound variable
//! 'State1'`). The owner of that scope is `threaded_expression`, which now treats a field
//! assignment as a producer (`lower_field_assignment_bind`: a real `Bind`, spliced into the
//! caller's statement sequence), mirrored by `subexpr_needs_prelude`.
//!
//! [`field_assignment_value_position_corpus`] proves it over value position x enclosing
//! shape; each program must pass the debug `ThreadedIr` verifier (which includes
//! `StateEffectEscapesExpression` from the closed cascade rendering) and compile through
//! `erlc`.

use super::{assert_compiles_through_erlc, codegen, erlc_available};
use std::panic::{AssertUnwindSafe, catch_unwind};

const HEADER: &str = "Actor subclass: FieldAssignProbe
  state: n = 0
  state: m = 0

  bump -> Integer =>
    self.n := self.n + 1
    self.n

  mk -> Object =>
    self.n := self.n + 1
    Object classBuilder

  take: x -> Integer => x
  take: x with: y -> Integer => x
";

/// The BT-3803 repro verbatim, plus the acceptance criterion's two shapes: a field
/// assignment as a send argument, and followed by a statement that reads state.
#[test]
fn send_argument_field_assignment_followed_by_state_read() {
    let code = codegen(
        "Actor subclass: P
  state: n = 0
  probe =>
    Transcript show: (self.n := 5)
    self.n
",
    );
    assert_compiles_through_erlc("test", &code);
}

/// BT-3819: `^super sel: <operand>` threaded its operand through a closed
/// argument list (the early-return `SuperSend` arm skipped the sequencing the
/// statement form has), tripping `StateEffectEscapesExpression` when the operand
/// wrote a field. Statement, last-statement and `^` forms all sequence it.
#[test]
fn super_send_operand_with_field_write_is_sequenced_in_every_statement_form() {
    for body in [
        "r := super isKindOf: ((c) ifTrue: [self.n := 1. #a] ifFalse: [#b])\n    r",
        "^super isKindOf: ((c) ifTrue: [self.n := 1. #a] ifFalse: [#b])",
        "super isKindOf: ((c) ifTrue: [self.n := 1. #a] ifFalse: [#b])\n    self.n",
        "^super isKindOf: (self.n := 5)",
    ] {
        let src = format!(
            "Actor subclass: SuperOperandProbe\n  state: n = 0\n\n  go: c =>\n    {body}\n"
        );
        let code = codegen(&src);
        assert_compiles_through_erlc("test", &code);
    }
}

/// A value position: a template over `EXPR`, a parenthesized field assignment.
const POSITIONS: &[(&str, &str)] = &[
    ("self_send_arg", "self take: EXPR"),
    ("transcript_arg", "Transcript show: EXPR"),
    ("binary_left", "EXPR + 1"),
    ("binary_right", "1 + EXPR"),
    ("unary_receiver", "EXPR printString"),
    ("two_keyword_args", "self take: EXPR with: (self.m := 7)"),
    (
        "keyword_arg_then_state_read",
        "self take: EXPR with: self.n",
    ),
    ("list_literal_element", "#(1, EXPR)"),
    ("string_interpolation", "\"v={EXPR}\""),
    ("assigned_to_local", "c := EXPR. c"),
    ("assigned_to_field", "self.m := EXPR"),
    ("nested_field_assignment", "(self.m := EXPR)"),
    ("returned", "^ EXPR"),
    (
        "cascade_argument",
        "Object classBuilder name: #A; superclass: EXPR",
    ),
    (
        "cascade_receiver_argument",
        "(self take: EXPR) printString; displayString",
    ),
    ("after_self_send_sibling", "self take: self bump with: EXPR"),
];

/// How the position's statement is embedded in the method, and what follows it.
const SHAPES: &[(&str, &str)] = &[
    ("then_read", "    STMT.\n    self.n\n"),
    ("last_statement", "    STMT\n"),
    (
        "between_field_writes",
        "    self.m := 1.\n    STMT.\n    self.m := self.m + 1.\n    self.n\n",
    ),
    (
        "conditional_arm",
        "    self.m > 5 ifTrue: [STMT] ifFalse: [self bump].\n    self.n\n",
    ),
    (
        "conditional_condition",
        "    ((STMT) > 3) ifTrue: [self bump].\n    self.n\n",
    ),
    (
        "loop_body",
        "    i := 0.\n    [i < 2] whileTrue: [STMT. i := i + 1].\n    self.n\n",
    ),
    (
        "do_block_body",
        "    #(1, 2) do: [:e | STMT].\n    self.n\n",
    ),
    (
        "on_do_protected",
        "    [STMT] on: Error do: [:e | 0].\n    self.n\n",
    ),
    (
        "on_do_handler",
        "    [1] on: Error do: [:e | STMT].\n    self.n\n",
    ),
    (
        "ensure_body",
        "    [STMT] ensure: [self.m := 1].\n    self.n\n",
    ),
    ("ensure_cleanup", "    [1] ensure: [STMT].\n    self.n\n"),
    ("to_do_body", "    1 to: 2 do: [:i | STMT].\n    self.n\n"),
    (
        "times_repeat_body",
        "    2 timesRepeat: [STMT].\n    self.n\n",
    ),
    (
        "inject_into_body",
        "    #(1, 2) inject: 0 into: [:a :e | STMT. a].\n    self.n\n",
    ),
    (
        "collect_body",
        "    #(1, 2) collect: [:e | STMT].\n    self.n\n",
    ),
    ("block_value", "    [STMT] value.\n    self.n\n"),
    ("block_value_arg", "    [:x | STMT] value: 3.\n    self.n\n"),
    // BT-3812: the remaining block-taking positions that could still compile a field
    // assignment closed.
    (
        "block_value_two_args",
        "    [:x :y | STMT] value: 3 value: 4.\n    self.n\n",
    ),
    (
        "and_block",
        "    ((self.m > 0) and: [STMT. true]).\n    self.n\n",
    ),
    (
        "or_block",
        "    ((self.m > 0) or: [STMT. true]).\n    self.n\n",
    ),
    (
        "detect_body",
        "    #(1, 2) detect: [:e | STMT. true].\n    self.n\n",
    ),
    (
        "select_body",
        "    #(1, 2) select: [:e | STMT. true].\n    self.n\n",
    ),
    (
        "reject_body",
        "    #(1, 2) reject: [:e | STMT. true].\n    self.n\n",
    ),
    (
        "any_satisfy_body",
        "    #(1, 2) anySatisfy: [:e | STMT. true].\n    self.n\n",
    ),
    (
        "sort_comparator_body",
        "    #(2, 1) sort: [:a :b | STMT. a < b].\n    self.n\n",
    ),
    ("if_nil_body", "    nil ifNil: [STMT].\n    self.n\n"),
    (
        "if_not_nil_body",
        "    3 ifNotNil: [:x | STMT].\n    self.n\n",
    ),
    (
        "while_false_body",
        "    i := 0.\n    [i >= 2] whileFalse: [STMT. i := i + 1].\n    self.n\n",
    ),
    (
        "to_by_do_body",
        "    1 to: 5 by: 2 do: [:i | STMT].\n    self.n\n",
    ),
    (
        "keys_and_values_do_body",
        "    #{#a => 1} keysAndValuesDo: [:k :v | STMT].\n    self.n\n",
    ),
];

/// Whole-method programs for the mixed-family hybrid loops (a field written in the loop
/// body both directly and as a nested operand, so the loop extracts both as fun
/// parameters), where the assignment is the loop body's own statement shape rather than an
/// operand of a templated position.
const HYBRID_LOOP_METHODS: &[(&str, &str)] = &[
    (
        "while_nested_rhs",
        "    i := 0.\n    [i < 3] whileTrue: [self.m := (self.n := i). i := i + 1].\n    self.n\n",
    ),
    (
        "while_nested_rhs_then_read",
        "    i := 0.\n    [i < 3] whileTrue: [self.m := (self.n := i). self.m := self.n + 1. i := i + 1].\n    self.m\n",
    ),
    (
        "while_operand_of_binary_rhs",
        "    i := 0.\n    [i < 3] whileTrue: [self.m := 1 + (self.n := i). i := i + 1].\n    self.n\n",
    ),
    (
        "to_do_nested_rhs",
        "    1 to: 3 do: [:i | self.m := (self.n := i)].\n    self.n\n",
    ),
    (
        "times_repeat_nested_rhs",
        "    2 timesRepeat: [self.m := (self.n := self.n + 1)].\n    self.n\n",
    ),
    (
        "do_nested_rhs",
        "    #(1, 2) do: [:e | self.m := (self.n := e)].\n    self.n\n",
    ),
    (
        "inject_nested_rhs",
        "    #(1, 2) inject: 0 into: [:a :e | self.m := (self.n := e). a].\n    self.n\n",
    ),
];

// Not covered by this corpus: class methods (a class-variable write is an in-place `put`,
// ADR 0130 §2, which threads nothing) and the REPL (no `self` field to write). Value types
// are covered by [`value_type_field_assignment_value_position_corpus`] below; the only
// exclusion there is a plain `Value subclass:`, which reports the immutability error.

/// The families of [`SHAPES`], as index ranges, one test each below
/// (`per_family_tests!`, BT-3888; each shape is one `erlc` run): the
/// statement-level and conditional shapes; a loop, `do:` or exception body;
/// the other fold, block and boolean bodies; the iteration and nil-testing
/// bodies.
const SHAPE_FAMILIES: &[std::ops::Range<usize>] = &[0..5, 5..11, 11..20, 20..30];

/// Every position of [`POSITIONS`] in one family of [`SHAPES`]: the whole
/// proof is the per-family tests below (and the hybrid loops,
/// [`field_assignment_value_position_corpus_hybrid_loops`]).
fn field_assignment_value_position_corpus(shapes: &[(&str, &str)]) {
    let passes = |src: &str| {
        catch_unwind(AssertUnwindSafe(|| {
            assert_compiles_through_erlc("test", &codegen(src));
        }))
        .is_ok()
    };
    let mut failures: Vec<String> = Vec::new();
    for (shape_name, shape) in shapes {
        let cases: Vec<(&str, String)> = POSITIONS
            .iter()
            .enumerate()
            .map(|(i, (position_name, position))| {
                let stmt = position.replace("EXPR", "(self.n := 5)");
                (
                    *position_name,
                    format!("  probe{i} =>\n{}", shape.replace("STMT", &stmt)),
                )
            })
            .collect();
        // One `erlc` run per shape over every position at once...
        let methods: Vec<&str> = cases.iter().map(|(_, m)| m.as_str()).collect();
        if passes(&format!("{HEADER}\n{}", methods.join("\n"))) {
            continue;
        }
        // ...and one per case only when that fails, so a failure names its case.
        for (position_name, method) in &cases {
            let single = format!("{HEADER}\n{method}");
            if !passes(&single) {
                failures.push(format!("{shape_name} / {position_name}:\n{single}"));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "{} field-assignment case(s) fail verified codegen:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

beamtalk_core::per_family_tests! {
    list = SHAPES,
    families = SHAPE_FAMILIES,
    check = field_assignment_value_position_corpus,
    partition = the_field_assignment_value_position_corpus_tests_cover_every_shape,
    field_assignment_value_position_corpus_statement_and_conditional_shapes => 0,
    field_assignment_value_position_corpus_loop_do_and_exception_shapes => 1,
    field_assignment_value_position_corpus_fold_block_and_boolean_shapes => 2,
    field_assignment_value_position_corpus_iteration_and_nil_shapes => 3,
}

/// The mixed-family hybrid loops of [`HYBRID_LOOP_METHODS`], each a whole
/// method: the rest of the actor corpus above.
#[test]
fn field_assignment_value_position_corpus_hybrid_loops() {
    let passes = |src: &str| {
        catch_unwind(AssertUnwindSafe(|| {
            assert_compiles_through_erlc("test", &codegen(src));
        }))
        .is_ok()
    };
    let mut failures: Vec<String> = Vec::new();
    for (name, body) in HYBRID_LOOP_METHODS {
        let src = format!("{HEADER}\n  probe =>\n{body}");
        if !passes(&src) {
            failures.push(format!("hybrid_loop / {name}:\n{src}"));
        }
    }
    assert!(
        failures.is_empty(),
        "{} hybrid-loop field-assignment case(s) fail verified codegen:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

// ── BT-3812: value types ────────────────────────────────────────────────────────────────
//
// `TestCase` is a `Value subclass:` exempt from the value-immutability error
// (`lint_validators.rs`, `hierarchy_queries.rs::is_testcase_subclass`), so `self.field :=`
// in value position reaches the `ValueType` (`Self`) producer path of
// `lower_field_write`. (An earlier version of this file excluded value types on the false
// premise that they cannot write fields.) A plain `Value subclass:` reports the
// immutability error instead; that is the one true exclusion.

pub(super) const VT_HEADER: &str = "TestCase subclass: FieldAssignVtProbe
  field: n = 0
  field: m = 0

  take: x -> Integer => x
  take: x with: y -> Integer => x
";

/// Positions valid for a value type (no actor self-send `bump`: a value-type self-send is a
/// plain call that threads nothing).
pub(super) const VT_POSITIONS: &[(&str, &str)] = &[
    ("self_send_arg", "self take: EXPR"),
    ("binary_left", "EXPR + 1"),
    ("binary_right", "1 + EXPR"),
    ("unary_receiver", "EXPR printString"),
    ("two_keyword_args", "self take: EXPR with: (self.m := 7)"),
    (
        "keyword_arg_then_state_read",
        "self take: EXPR with: self.n",
    ),
    ("list_literal_element", "#(1, EXPR)"),
    ("string_interpolation", "\"v={EXPR}\""),
    ("assigned_to_local", "c := EXPR. c"),
    ("assigned_to_field", "self.m := EXPR"),
    ("nested_field_assignment", "(self.m := EXPR)"),
    ("returned", "^ EXPR"),
    (
        "cascade_argument",
        "Object classBuilder name: #A; superclass: EXPR",
    ),
    // A conditional whose receiver is EXPR, in value position.
    (
        "conditional_wrapping_arg",
        "self take: (EXPR > 3 ifTrue: [1] ifFalse: [2])",
    ),
    // BT-3821: the conditional's ARMS write fields too, in every value position. The arms'
    // `Self` rebinds are merged into the caller's statement sequence
    // (`vt_conditional_value_producer`), so each must compile in every statement-level shape;
    // the `ConstructFamilyDropped` verifier invariant covers the merge.
    (
        "conditional_wrapping_arg_with_field_arms",
        "self take: (EXPR > 3 ifTrue: [self.m := 1] ifFalse: [0])",
    ),
    (
        "conditional_wrapping_assigned_with_field_arms",
        "c := (EXPR > 3 ifTrue: [self.m := 1] ifFalse: [0]). c",
    ),
    (
        "conditional_field_arms_binary_operand",
        "1 + (EXPR > 3 ifTrue: [self.m := 4] ifFalse: [0])",
    ),
    (
        "conditional_field_arms_one_armed",
        "self take: (EXPR > 3 ifTrue: [self.m := 4])",
    ),
    (
        "conditional_field_arms_returned",
        "^ EXPR > 3 ifTrue: [self.m := 1] ifFalse: [0]",
    ),
    (
        "conditional_field_arms_bare_statement",
        "EXPR > 3 ifTrue: [self.m := 1] ifFalse: [0]",
    ),
    // BT-3830: the same conditional parenthesised. `is_vt_field_conditional_producer`
    // unwraps the parens; the body classifier must agree, or a non-last one is rejected.
    (
        "conditional_field_arms_parenthesised_statement",
        "(EXPR > 3 ifTrue: [self.m := 1] ifFalse: [0])",
    ),
    (
        "conditional_field_arms_into_field_rhs",
        "self.m := (EXPR > 3 ifTrue: [self.n := 1] ifFalse: [0])",
    ),
    (
        "conditional_field_arms_second_operand",
        "self take: self.n with: (EXPR > 3 ifTrue: [self.m := 4] ifFalse: [0])",
    ),
    (
        "conditional_field_arms_list_element",
        "#(1, (EXPR > 3 ifTrue: [self.m := 4] ifFalse: [0]), self.m)",
    ),
    (
        "conditional_field_arms_interpolated",
        "\"v={EXPR > 3 ifTrue: [self.m := 4] ifFalse: [0]}\"",
    ),
    // The arms rebind an outer local as well as writing a field.
    (
        "conditional_field_and_local_arms",
        "t := 0. self take: (EXPR > 3 ifTrue: [t := t + 1. self.m := t] ifFalse: [0]). t",
    ),
    // The arms only thread an outer local, over a receiver holding a field write (a bot
    // suggestion on BT-3816: the inline-control-flow producer must still lower the arms once
    // the receiver is sequenced).
    (
        "conditional_local_arms_field_receiver",
        "t := 0. self take: (EXPR > 3 ifTrue: [t := t + 1] ifFalse: [0]). t",
    ),
    (
        "conditional_local_read_write_arms_field_receiver",
        "t := 1. self take: (EXPR > 3 ifTrue: [t := t * 2] ifFalse: [t := t + 1]). t",
    ),
    // A conditional that is the receiver of another conditional whose arms write.
    (
        "conditional_field_arms_as_receiver",
        "self take: ((EXPR > 3 ifTrue: [self.m := 1. true] ifFalse: [false]) ifTrue: [self.n := 2] ifFalse: [0])",
    ),
    // Loops in value position: counted and `whileTrue:` loops thread `Self` through their own
    // tuple (`do:` / `collect:` / `inject:into:` fold bodies carry it in the fold accumulator,
    // below).
    (
        "assigned_loop_receiver",
        "c := EXPR timesRepeat: [self.m := self.m + 1]. c",
    ),
    (
        "loop_as_send_argument",
        "self take: (EXPR timesRepeat: [self.m := self.m + 1])",
    ),
    (
        "assigned_to_do_loop",
        "c := 1 to: EXPR do: [:i | self.m := self.m + i]. c",
    ),
    (
        "assigned_while_loop_then_operand",
        "i := 0. c := [i < 2] whileTrue: [self.m := self.m + 1. i := i + 1]. c + EXPR",
    ),
    (
        "conditional_wrapping_field_rhs",
        "self.m := (EXPR > 3 ifTrue: [1] ifFalse: [2])",
    ),
    // BT-3827: `do:` / `collect:` / `inject:into:` bodies that write a field carry `Self` through
    // the fold accumulator (`FoldFamilySlots`), so each compiles in every position of every
    // statement-level shape outside `VT_FOLD_REJECTING_SHAPES`, and is a structured rejection in
    // every nested shape (`fold_writing_position_must_be_rejected`); the `FoldFamilyNotCarried`
    // verifier invariant covers the lambda's tail (`value_type_fold_family_corpus.rs`).
    ("do_field_write_statement", "#(1, 2) do: [:e | self.m := e]"),
    (
        "do_field_write_send_arg",
        "self take: (#(1, 2) do: [:e | self.m := e])",
    ),
    (
        "collect_field_write_assigned",
        "xs := #(1, 2) collect: [:e | self.m := e]. xs",
    ),
    (
        "inject_field_write_send_arg",
        "self take: (#(1, 2) inject: 0 into: [:a :e | self.m := e. a])",
    ),
    (
        "do_field_write_reads_field",
        "#(1, 2) do: [:e | self.m := self.m + e]",
    ),
    (
        "do_two_field_writes",
        "#(1, 2) do: [:e | self.m := e. self.n := e + 1]",
    ),
    (
        "do_field_write_and_local",
        "t := 0. #(1, 2) do: [:e | t := t + e. self.m := t]. t",
    ),
    (
        "collect_field_write_and_local",
        "t := 0. xs := #(1, 2) collect: [:e | t := t + e. self.m := t. e * 2]. xs",
    ),
    (
        "inject_field_write_accumulates",
        "#(1, 2) inject: 0 into: [:a :e | self.m := self.m + e. a + e]",
    ),
    (
        "inject_field_write_binary_operand",
        "(#(1, 2) inject: 0 into: [:a :e | self.m := e. a + e]) + 1",
    ),
    (
        "inject_field_write_returned",
        "^ #(1, 2) inject: 0 into: [:a :e | self.m := e. a + e]",
    ),
    (
        "collect_field_write_returned",
        "^ #(1, 2) collect: [:e | self.m := e. e + 1]",
    ),
    (
        "do_field_write_then_field_read_operand",
        "#(1, 2) do: [:e | self.m := e]. self.m + EXPR",
    ),
    (
        "collect_field_write_empty_list",
        "#() collect: [:e | self.m := e]",
    ),
];

/// Enclosing shapes, with whether a field write in the shape may be rejected with a structured
/// diagnostic instead of compiling. A value-type field write inside a conditional arm or loop
/// body is either threaded or rejected (`ValueSelf*` errors); both are fine. The statement-level
/// shapes (`may_reject == false`) have no such escape: every case must compile. What is never
/// fine is an `erlc` failure, a verifier failure or any other error.
pub(super) const VT_SHAPES: &[(&str, &str, bool)] = &[
    ("then_read", "    STMT.\n    self.n\n", false),
    ("last_statement", "    STMT\n", false),
    (
        "between_field_writes",
        "    self.m := 1.\n    STMT.\n    self.m := self.m + 1.\n    self.n\n",
        false,
    ),
    (
        "conditional_arm",
        "    self.m > 5 ifTrue: [STMT] ifFalse: [self.m := 1].\n    self.n\n",
        true,
    ),
    // BT-3844: an arm that also rebinds an outer local before and after STMT, so the
    // conditional takes the local-threading lowering (the arm's statements are an open chain
    // whose locals the post-`case` rebinds read) rather than only the field-write merge.
    (
        "local_threading_conditional_arm",
        "    t := 0.\n    self.m > 5 ifTrue: [t := 1. STMT. t := t + 1] ifFalse: [t := 2].\n    t + self.n\n",
        true,
    ),
    (
        "conditional_condition",
        "    ((STMT) > 3) ifTrue: [self.m := 1].\n    self.n\n",
        false,
    ),
    (
        "loop_body",
        "    i := 0.\n    [i < 2] whileTrue: [STMT. i := i + 1].\n    self.n\n",
        true,
    ),
    (
        "do_block_body",
        "    #(1, 2) do: [:e | STMT].\n    self.n\n",
        true,
    ),
    (
        "on_do_protected",
        "    [STMT] on: Error do: [:e | 0].\n    self.n\n",
        true,
    ),
    ("block_value", "    [STMT] value.\n    self.n\n", false),
];

/// Statement-level shapes in which a field-writing fold position is a structured rejection
/// rather than a compile (BT-3827). In both a fold's `Self` slot has no reader, and the lowering
/// refuses it (`CoreErlangGenerator::value_self_fold_slot`) instead of dropping the write:
///
/// * `block_value`: an evaluated block (`[STMT] value`) whose body is lowered closed unless the
///   block itself writes a field at its top level;
/// * `conditional_condition`: the fold is an operand of a conditional's receiver, which is not a
///   sequenced operand site of the method body.
///
/// The same rejection a fold gets in a stored block, a cascade or a receiver
/// (`field_writing_folds_in_closed_contexts_stay_a_structured_rejection`).
pub(super) const VT_FOLD_REJECTING_SHAPES: &[&str] = &["block_value", "conditional_condition"];

/// BT-3844: in every shape that nests STMT below the method body (`may_reject == true`: a
/// conditional arm, plain or local-threading, a loop body, a `do:` body, an `on:do:` block) a
/// field-writing fold is not merely *allowed* to be rejected, it MUST be: the enclosing
/// construct is lowered by the shared up-front rejection of a field write below an arm's / a
/// body's top level (`reject_nested_vt_conditional_self_field_writes`,
/// `reject_unthreadable_value_self_field_write`), so no consumer reads the fold's `Self` slot
/// there. A fold that starts compiling in one of these shapes has had that rejection lifted
/// without the runtime coverage that proves its write survives (the corpus's `maps:put` check
/// alone cannot: the `maps:put` is inside the fold lambda whether or not its `Self` reaches the
/// code after the construct). Lifting it means adding that coverage first (`BUnit` cases that
/// read the field after the construct, and a position in `value_type_fold_family_corpus.rs`'s
/// oracle run), then exempting that shape here.
fn fold_writing_position_must_be_rejected(shape_may_reject: bool, position: &str) -> bool {
    shape_may_reject && is_fold_writing_position(position)
}

/// Whether a [`VT_POSITIONS`] template is one of the BT-3827 field-writing fold positions.
pub(super) fn is_fold_writing_position(template: &str) -> bool {
    [
        "do: [:e | self.",
        "collect: [:e | self.",
        "inject: 0 into: [:a :e | self.",
        "[:e | t := t + e. self.",
    ]
    .iter()
    .any(|fold| template.contains(fold))
}

pub(super) enum VtOutcome {
    /// Compiled through `erlc`; carries the generated Core Erlang.
    Compiles(String),
    /// A structured value-type diagnostic: the construct is rejected, not miscompiled.
    RejectedCleanly,
    Fails(String),
}

/// Compiles `src` as a value type and classifies the result.
///
/// A `ThreadedIr` verifier finding is a failure in every build profile: a debug build panics in
/// `report_threaded_ir_verify_errors` (caught here), a release build only attaches an
/// `internal:` warning, which `generate_module_surfacing_verifier` returns and this reports.
/// `erlc` rejection is a failure only when `erlc` exists (see [`erlc_available`]).
pub(super) fn vt_outcome(src: &str) -> VtOutcome {
    use crate::core_erlang::{CodeGenError, CodegenOptions, generate_module_surfacing_verifier};
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let generated = catch_unwind(AssertUnwindSafe(|| {
        generate_module_surfacing_verifier(&module, CodegenOptions::new("test"))
    }));
    match generated {
        Err(_) => VtOutcome::Fails("debug ThreadedIr verifier / codegen panic".to_string()),
        Ok(Err(
            CodeGenError::ValueSelfFieldAssignmentInMatchArm { .. }
            | CodeGenError::ValueSelfMutationLostAcrossNestedLoop { .. }
            | CodeGenError::FieldAssignmentInUnsupportedBlock { .. },
        )) => VtOutcome::RejectedCleanly,
        Ok(Err(other)) => VtOutcome::Fails(format!("codegen error: {other:?}")),
        Ok(Ok((_, verifier))) if !verifier.is_empty() => VtOutcome::Fails(format!(
            "ThreadedIr verifier diagnostic(s): {}",
            verifier
                .iter()
                .map(|d| d.message.to_string())
                .collect::<Vec<_>>()
                .join("; ")
        )),
        Ok(Ok((code, _))) => {
            if catch_unwind(AssertUnwindSafe(|| {
                assert_compiles_through_erlc("test", &code);
            }))
            .is_ok()
            {
                VtOutcome::Compiles(code)
            } else {
                VtOutcome::Fails("erlc rejected the generated Core Erlang".to_string())
            }
        }
    }
}

/// BT-3821: whether every field `probe{index}` writes (`self.n :=`, `self.m :=`) is written in
/// the generated Core Erlang of that function (`'maps':'put'('<field>'`). A conditional or loop
/// whose arm / body write is lowered into a closed operand and dropped still compiles; this
/// catches that class without running the module. Never tautological: a `Compiles` case only
/// reaches it after the verifier and `erlc` accepted the output.
fn written_fields_survive(src: &str, index: usize, code: &str) -> Result<(), String> {
    // A template can splice a multi-statement position (`c := EXPR. c`) into a parenthesised
    // shape (`((STMT) > 3)`), which is not a program: its statements parse to something else
    // and "the method writes `self.m`" is no longer a statement about the generated function.
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (_, diagnostics) = beamtalk_core::source_analysis::parse(tokens);
    if diagnostics
        .iter()
        .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error)
    {
        return Ok(());
    }
    let method_header = format!("  probe{index} =>");
    let method_src = src.split_once(&method_header).map_or("", |(_, rest)| rest);
    let fn_header = format!("'probe{index}'/1 = fun");
    let fn_code = code.split_once(&fn_header).map_or("", |(_, rest)| {
        // The function ends at the next top-level definition (column-0 quoted name).
        rest.split("\n'").next().unwrap_or(rest)
    });
    if fn_code.is_empty() {
        return Err(format!("no generated function for probe{index}"));
    }
    for field in ["n", "m"] {
        if method_src.contains(&format!("self.{field} :="))
            && !fn_code.contains(&format!("'maps':'put'('{field}'"))
        {
            return Err(format!(
                "the method writes `self.{field}` but its generated function has no \
                 `maps:put('{field}'`: the write was dropped"
            ));
        }
    }
    Ok(())
}

/// PIN-BUG list: `shape / position` cases that fail today, each citing an open issue. The corpus
/// asserts the failing set EQUALS this list, so a fix flips every pin it covers (the equality
/// fails until the fixed entries are removed). BT-3816 flipped all of its pins; empty.
const VT_PINNED_FAILURES: &[&str] = &[];

/// One family per shape of [`VT_SHAPES`] (`per_family_tests!`, BT-3888): each shape is one
/// `erlc` run per position, so one shape x one position family is the unit nextest spreads over
/// cores.
const VT_SHAPE_FAMILIES: &[std::ops::Range<usize>] =
    &[0..1, 1..2, 2..3, 3..4, 4..5, 5..6, 6..7, 7..8, 8..9, 9..10];

/// The families of [`VT_POSITIONS`], as index ranges (BT-3888): the plain value positions; the
/// conditionals whose receiver or arms write; the loops and folds (and a conditional in a
/// field's value). Every shape family runs once per position family
/// (`VT_SHAPE_FAMILIES` x `VT_POSITION_FAMILIES`), so a statement-level shape, whose every
/// position is an `erlc` run, is three tests rather than one long one.
const VT_POSITION_FAMILIES: &[std::ops::Range<usize>] = &[0..13, 13..29, 29..48];

/// The position families partition [`VT_POSITIONS`] (one `per_family_tests!` invocation each
/// below, so every shape family x position family pair has exactly one test).
#[test]
fn the_value_type_position_families_cover_every_position() {
    beamtalk_core::test_helpers::corpus_families::assert_families_partition_the_list(
        VT_POSITION_FAMILIES,
        VT_POSITIONS.len(),
        &[0, 1, 2],
    );
}

/// The pins of [`VT_PINNED_FAILURES`] whose `shape / position` names a shape of `shapes` and a
/// position of `positions`.
fn vt_pins_for(shapes: &[(&str, &str, bool)], positions: &[(&str, &str)]) -> Vec<String> {
    VT_PINNED_FAILURES
        .iter()
        .filter(|pin| {
            let (shape_name, position_name) = pin.split_once(" / ").expect("`shape / position`");
            shapes.iter().any(|(name, _, _)| *name == shape_name)
                && positions.iter().any(|(name, _)| *name == position_name)
        })
        .map(|s| (*s).to_string())
        .collect()
}

/// Every pin of [`VT_PINNED_FAILURES`] names a shape of [`VT_SHAPES`] and a position of
/// [`VT_POSITIONS`]: a pin for a case no family holds would never be checked, so it could
/// neither fail nor flip.
#[test]
fn every_value_type_field_assignment_corpus_pin_names_a_corpus_shape() {
    let pinned = vt_pins_for(VT_SHAPES, VT_POSITIONS);
    assert_eq!(
        pinned.len(),
        VT_PINNED_FAILURES.len(),
        "pin(s) naming no shape of VT_SHAPES or position of VT_POSITIONS: {:?}",
        VT_PINNED_FAILURES
            .iter()
            .filter(|p| !pinned.iter().any(|q| q == *p))
            .collect::<Vec<_>>()
    );
}

/// The value-type corpus over one family of [`VT_SHAPES`] x one family of [`VT_POSITIONS`]:
/// the whole proof is the per-family tests below.
fn value_type_field_assignment_value_position_corpus(
    shapes: &[(&str, &str, bool)],
    positions: &[(&str, &str)],
) {
    let mut failing: Vec<String> = Vec::new();
    let mut details: Vec<String> = Vec::new();
    let mut compiled = 0usize;
    // Statement-level shapes cannot be rejected: every unpinned case of one must compile.
    let mut must_compile = 0usize;
    let mut must_compile_compiled = 0usize;
    for (shape_name, shape, shape_may_reject) in shapes {
        for (i, (position_name, position)) in positions.iter().enumerate() {
            let stmt = position.replace("EXPR", "(self.n := 5)");
            let src = format!(
                "{VT_HEADER}\n  probe{i} =>\n{}",
                shape.replace("STMT", &stmt)
            );
            let may_reject = *shape_may_reject
                || (is_fold_writing_position(position)
                    && VT_FOLD_REJECTING_SHAPES.contains(shape_name));
            let name = format!("{shape_name} / {position_name}");
            let pinned = VT_PINNED_FAILURES.contains(&name.as_str());
            if !may_reject && !pinned {
                must_compile += 1;
            }
            match vt_outcome(&src) {
                VtOutcome::Compiles(_)
                    if fold_writing_position_must_be_rejected(*shape_may_reject, position) =>
                {
                    failing.push(name.clone());
                    details.push(format!(
                        "{name}: a field-writing fold compiled below the method body's top \
                         level, where it must be a structured rejection until its write is \
                         proven at runtime (see `fold_writing_position_must_be_rejected`)\n{src}"
                    ));
                }
                VtOutcome::Compiles(code) => {
                    compiled += 1;
                    if !may_reject && !pinned {
                        must_compile_compiled += 1;
                    }
                    // `erlc`-clean is not enough: a write the lowering silently drops
                    // still compiles. Every field the method writes must survive as a
                    // `maps:put` in the generated function (the runtime half is
                    // `stdlib/test/value_type_conditional_field_arms_value_position_test.bt`).
                    if let Err(why) = written_fields_survive(&src, i, &code) {
                        failing.push(name.clone());
                        details.push(format!("{name}: {why}\n{src}"));
                    }
                }
                VtOutcome::RejectedCleanly => {}
                VtOutcome::Fails(why) => {
                    failing.push(name.clone());
                    details.push(format!("{name}: {why}\n{src}"));
                }
            }
        }
    }
    let pinned = vt_pins_for(shapes, positions);
    let unexpected: Vec<&String> = failing.iter().filter(|f| !pinned.contains(f)).collect();
    assert!(
        unexpected.is_empty(),
        "new value-type field-assignment failure(s) not pinned to an open issue: {unexpected:?}\n\n{}",
        details.join("\n\n")
    );
    // Without `erlc` a case that only fails at `erlc` reports as compiling, so a pin may look
    // fixed when it is not: the pin-equality half is only meaningful with `erlc`.
    if erlc_available() {
        let fixed: Vec<&String> = pinned.iter().filter(|p| !failing.contains(p)).collect();
        assert!(
            fixed.is_empty(),
            "pin(s) now pass; remove them from VT_PINNED_FAILURES: {fixed:?}"
        );
    }
    // The floor is derived, not a magic number: a shape that cannot be rejected must compile
    // every unpinned case, so a regression to blanket rejection cannot hide behind the
    // `RejectedCleanly` allowance, and at least one case of such a shape must compile at all.
    // A family of shapes that may be rejected may compile none: today every case of
    // `conditional_arm`, `local_threading_conditional_arm`, `loop_body`, `do_block_body` and
    // `on_do_protected` is a structured rejection.
    assert_eq!(
        must_compile_compiled, must_compile,
        "{must_compile_compiled} of {must_compile} unpinned statement-level value-type cases compiled; they cannot be rejected"
    );
    let holds_a_statement_level_shape = shapes.iter().any(|(_, _, may_reject)| !may_reject);
    assert!(
        compiled >= must_compile && (compiled > 0 || !holds_a_statement_level_shape),
        "only {compiled} value-type cases compiled in {:?}",
        shapes.iter().map(|(name, _, _)| name).collect::<Vec<_>>()
    );
}

/// [`value_type_field_assignment_value_position_corpus`] over the plain value positions.
fn vt_corpus_in_plain_positions(shapes: &[(&str, &str, bool)]) {
    value_type_field_assignment_value_position_corpus(
        shapes,
        &VT_POSITIONS[VT_POSITION_FAMILIES[0].clone()],
    );
}

/// [`value_type_field_assignment_value_position_corpus`] over the conditional positions.
fn vt_corpus_in_conditional_positions(shapes: &[(&str, &str, bool)]) {
    value_type_field_assignment_value_position_corpus(
        shapes,
        &VT_POSITIONS[VT_POSITION_FAMILIES[1].clone()],
    );
}

/// [`value_type_field_assignment_value_position_corpus`] over the loop and fold positions.
fn vt_corpus_in_loop_and_fold_positions(shapes: &[(&str, &str, bool)]) {
    value_type_field_assignment_value_position_corpus(
        shapes,
        &VT_POSITIONS[VT_POSITION_FAMILIES[2].clone()],
    );
}

beamtalk_core::per_family_tests! {
    list = VT_SHAPES,
    families = VT_SHAPE_FAMILIES,
    check = vt_corpus_in_plain_positions,
    partition = the_value_type_field_assignment_value_position_corpus_tests_cover_every_shape_in_plain_positions,
    value_type_field_assignment_value_position_corpus_then_read_in_plain_positions => 0,
    value_type_field_assignment_value_position_corpus_last_statement_in_plain_positions => 1,
    value_type_field_assignment_value_position_corpus_between_field_writes_in_plain_positions => 2,
    value_type_field_assignment_value_position_corpus_conditional_arm_in_plain_positions => 3,
    value_type_field_assignment_value_position_corpus_local_threading_conditional_arm_in_plain_positions => 4,
    value_type_field_assignment_value_position_corpus_conditional_condition_in_plain_positions => 5,
    value_type_field_assignment_value_position_corpus_loop_body_in_plain_positions => 6,
    value_type_field_assignment_value_position_corpus_do_block_body_in_plain_positions => 7,
    value_type_field_assignment_value_position_corpus_on_do_protected_in_plain_positions => 8,
    value_type_field_assignment_value_position_corpus_block_value_in_plain_positions => 9,
}

beamtalk_core::per_family_tests! {
    list = VT_SHAPES,
    families = VT_SHAPE_FAMILIES,
    check = vt_corpus_in_conditional_positions,
    partition = the_value_type_field_assignment_value_position_corpus_tests_cover_every_shape_in_conditional_positions,
    value_type_field_assignment_value_position_corpus_then_read_in_conditional_positions => 0,
    value_type_field_assignment_value_position_corpus_last_statement_in_conditional_positions => 1,
    value_type_field_assignment_value_position_corpus_between_field_writes_in_conditional_positions => 2,
    value_type_field_assignment_value_position_corpus_conditional_arm_in_conditional_positions => 3,
    value_type_field_assignment_value_position_corpus_local_threading_conditional_arm_in_conditional_positions => 4,
    value_type_field_assignment_value_position_corpus_conditional_condition_in_conditional_positions => 5,
    value_type_field_assignment_value_position_corpus_loop_body_in_conditional_positions => 6,
    value_type_field_assignment_value_position_corpus_do_block_body_in_conditional_positions => 7,
    value_type_field_assignment_value_position_corpus_on_do_protected_in_conditional_positions => 8,
    value_type_field_assignment_value_position_corpus_block_value_in_conditional_positions => 9,
}

beamtalk_core::per_family_tests! {
    list = VT_SHAPES,
    families = VT_SHAPE_FAMILIES,
    check = vt_corpus_in_loop_and_fold_positions,
    partition = the_value_type_field_assignment_value_position_corpus_tests_cover_every_shape_in_loop_and_fold_positions,
    value_type_field_assignment_value_position_corpus_then_read_in_loop_and_fold_positions => 0,
    value_type_field_assignment_value_position_corpus_last_statement_in_loop_and_fold_positions => 1,
    value_type_field_assignment_value_position_corpus_between_field_writes_in_loop_and_fold_positions => 2,
    value_type_field_assignment_value_position_corpus_conditional_arm_in_loop_and_fold_positions => 3,
    value_type_field_assignment_value_position_corpus_local_threading_conditional_arm_in_loop_and_fold_positions => 4,
    value_type_field_assignment_value_position_corpus_conditional_condition_in_loop_and_fold_positions => 5,
    value_type_field_assignment_value_position_corpus_loop_body_in_loop_and_fold_positions => 6,
    value_type_field_assignment_value_position_corpus_do_block_body_in_loop_and_fold_positions => 7,
    value_type_field_assignment_value_position_corpus_on_do_protected_in_loop_and_fold_positions => 8,
    value_type_field_assignment_value_position_corpus_block_value_in_loop_and_fold_positions => 9,
}

/// BT-3827 (flips the former `field_writing_fold_bodies_stay_a_structured_rejection` pin): a
/// `do:` / `collect:` / `inject:into:` body that writes a field threads `Self` through the fold
/// accumulator in a value type, in statement position and in every value position. The shapes the
/// pin named, now compiling, with the write surviving into the generated function.
#[test]
fn field_writing_fold_bodies_compile_with_the_write_threaded() {
    for body in [
        "self take: (#(1, 2) do: [:e | self.m := e])",
        "xs := #(1, 2) collect: [:e | self.m := e]. xs",
        "self take: (#(1, 2) inject: 0 into: [:a :e | self.m := e. a])",
        "#(1, 2) do: [:e | self.m := e]",
    ] {
        let src = format!("{VT_HEADER}\n  probe0 =>\n    {body}\n    self.n\n");
        let VtOutcome::Compiles(code) = vt_outcome(&src) else {
            panic!("`{body}` must compile now that fold accumulators carry `Self`:\n{src}");
        };
        written_fields_survive(&src, 0, &code).unwrap_or_else(|why| panic!("{why}\n{src}"));
    }
}

/// The folds [`SELF_THREADING_FOLDS`] does not name keep the structured rejection of a field
/// write in their body (never a silently dropped write), as does a write nested below the fold
/// body's top level — the same limit the Letrec loops have (a conditional arm's `Self` rebind
/// is scoped to its arm). Widening either means opting the construct in, not editing this list.
#[test]
fn field_writing_bodies_the_fold_slot_does_not_cover_stay_a_structured_rejection() {
    for body in [
        "#(1, 2) select: [:e | self.m := e. true]",
        "#(1, 2) reject: [:e | self.m := e. true]",
        "#(1, 2) detect: [:e | self.m := e. true]",
        "#(1, 2) anySatisfy: [:e | self.m := e. true]",
        "#(1, 2) count: [:e | self.m := e. true]",
        "#(1, 2) sort: [:a :b | self.m := a. a < b]",
        "#(1, 2) do: [:e | e > 1 ifTrue: [self.m := e]]",
        "#(1, 2) do: [:e | self take: (self.m := e)]",
        "#(1, 2) inject: 0 into: [:a :e | e > 1 ifTrue: [self.m := e]. a]",
    ] {
        let src = format!("{VT_HEADER}\n  probe =>\n    {body}\n    self.n\n");
        assert!(
            matches!(vt_outcome(&src), VtOutcome::RejectedCleanly),
            "`{body}` is expected to be rejected with a structured diagnostic:\n{src}"
        );
    }
}

/// A fold that writes a field hands its `Self` back as a trailing slot of its result tuple, so it
/// is lowered only where a consumer reads that slot (`with_fold_self_slot_consumer`). Anywhere
/// else (a stored block, an evaluated block with no top-level write of its own, a cascade, a
/// receiver, an `and:` argument) it is the structured rejection, never a dropped write.
#[test]
fn field_writing_folds_in_closed_contexts_stay_a_structured_rejection() {
    for body in [
        "b := [#(1, 2) do: [:e | self.m := e]]. b value",
        "[#(1, 2) do: [:e | self.m := e]] value",
        "c := [#(1, 2) do: [:e | self.m := e]] value. c",
        "[:x | #(1, 2) collect: [:e | self.m := e]] value: 3",
        "(#(1, 2) do: [:e | self.m := e]) printString; displayString",
        "#(1, 2) do: [:e | self.m := e]; yourself",
        "(#(1, 2) collect: [:e | self.m := e]) do: [:x | x]",
        "(#(1, 2) do: [:e | self.m := e]) ifNil: [3]",
        "true and: [#(1, 2) do: [:e | self.m := e]. true]",
    ] {
        let src = format!("{VT_HEADER}\n  probe =>\n    {body}\n    self.n\n");
        assert!(
            matches!(vt_outcome(&src), VtOutcome::RejectedCleanly),
            "`{body}` is expected to be rejected with a structured diagnostic:\n{src}"
        );
    }
}

/// The BT-3816 repro verbatim: the `Self1` rebind of `(self.n := 5)` must reach the read after it.
#[test]
fn value_type_field_assignment_repro_threads_self_to_the_read() {
    let code = codegen(
        "TestCase subclass: P
  field: n = 0
  take: x -> Integer => x
  probe =>
    self take: (self.n := 5).
    self.n
",
    );
    assert_compiles_through_erlc("test", &code);
    assert!(
        code.contains("call 'maps':'get'('n', Self1)"),
        "the read after the write must see the rebound Self:\n{code}"
    );
}

/// Value-type statement shapes beyond the corpus's 13 positions: a field assignment as an operand
/// of a threading construct (conditional condition, loop receiver), an inlined block value with
/// several statements / arguments / a captured local, a destructure value, and last position.
#[test]
fn value_type_field_assignment_in_construct_operands_and_block_values_compiles() {
    let methods = [
        "[self.m := 1. self take: (self.n := 5)] value.\n    self.n + self.m",
        "[:x | self take: (self.n := x)] value: 3.\n    self.n",
        "x := [self.n := 5. 7] value.\n    x + self.n",
        "#(1, 2) do: [:e | ^ e].\n    self take: (self.n := 5).\n    self.n",
        "self take: (self.n := 5)",
        "^ [self.n := 5. self.n] value",
        "[self.n := 5] value",
        "c := 0.\n    [c := c + 1. self take: (self.n := c)] value.\n    c + self.n",
        "#[a, b] := #[(self.n := 5), 2].\n    a + self.n",
        "((self.n := 5) > 3) ifTrue: [self.m := 1] ifFalse: [self.m := 2].\n    self.m + self.n",
        "(self.n := 5) timesRepeat: [self.m := self.m + 1].\n    self.m",
        // A `^` value whose receiver holds a field write, in a method with a non-local return.
        "#(1) do: [:e | ^ e].\n    ^ ((self.n := 5) > 3) ifTrue: [1] ifFalse: [2]",
        "^ ((self.n := 5) > 3) ifTrue: [1] ifFalse: [2]",
        // A `^` inside a spliced block body.
        "[self.m := 1. ((self.n := 5) > 3) ifTrue: [self.m := 2]. 7] value.\n    self.n",
        "[self.n := 5. ^ 3] value.\n    self.n",
        "#(1) do: [:e | ^ e].\n    [self.n := 5. ^ 3] value.\n    self.n",
        // A construct nested in a spliced block body keeps its open-chain builder.
        "c := true.\n    [self.n := 5. c ifTrue: [self.m := 9]. 7] value.\n    self.n + self.m",
    ];
    for (i, body) in methods.iter().enumerate() {
        let src = format!("{VT_HEADER}\n  probe{i} =>\n    {body}\n");
        assert!(
            matches!(vt_outcome(&src), VtOutcome::Compiles(_)),
            "value-type case {i} must compile:\n{src}"
        );
    }
}

/// BT-3830: a NON-last statement's value is discarded, so `(s)` must lower exactly like `s`.
///
/// **Invariant:** the value-type body classifier and emitter see the same node for a
/// parenthesised non-last statement. Before the fix `classify_vt_body_expr` (which does not look
/// through parentheses) called these `Pure` while the bare forms got a threading kind:
/// `(t := t + 1)` and `([t < 3] whileTrue: [t := t + 1])` silently dropped the local write (the
/// later `t` read the pre-statement value) and `(self.m := 5)` trapped its `Self` rebind in a
/// closed expression (a verifier finding in debug builds, a lost write in release builds).
///
/// Pinned per form: both spellings reach the same outcome AND generate the same function, so the
/// parenthesised one cannot diverge (silently or otherwise) from the bare one.
#[test]
fn parenthesised_non_last_statements_lower_exactly_like_their_bare_form() {
    fn summary(r: &Result<Option<String>, String>) -> String {
        match r {
            Ok(Some(_)) => "compiles".to_string(),
            Ok(None) => "rejected".to_string(),
            Err(why) => format!("FAILS ({})", why.lines().next().unwrap_or("")),
        }
    }

    // `(form, expected outcome of BOTH spellings)`: `true` compiles, `false` is a clean rejection.
    let forms: &[(&str, bool)] = &[
        ("c ifTrue: [self.m := 1] ifFalse: [0]", true),
        ("c ifTrue: [self.m := 1] ifFalse: [t]", true),
        ("c ifTrue: [t := t + 1]", true),
        ("c ifTrue: [t := t + 1. self.m := 1] ifFalse: [0]", true),
        ("c ifTrue: [t := t + 1] ifFalse: [t := t + 2]", true),
        ("[self.m := 1] on: Error do: [:e | 0]", true),
        ("[self.m := 1. t := t + 1] on: Error do: [:e | 0]", true),
        ("[self.m := 1] ensure: [0]", true),
        ("#(1, 2) do: [:e | t := t + e]", true),
        // BT-3827: a top-level field write in a value-type `do:` body threads `Self` through the
        // fold accumulator, so both spellings compile.
        ("#(1, 2) do: [:e | self.m := e]", true),
        ("1 to: 3 do: [:i | t := t + i]", true),
        ("[t < 3] whileTrue: [t := t + 1]", true),
        ("3 timesRepeat: [t := t + 1]", true),
        ("#(1, 2) inject: 0 into: [:a :e | t := t + e. a + e]", true),
        ("self.m := 5", true),
        ("t := t + 1", true),
    ];
    let lower = |k: usize, stmt: &str| {
        let src = format!(
            "{VT_HEADER}\n  probe{k} =>\n    t := 0\n    c := self.n >= 0\n    {stmt}\n    t + self.m\n"
        );
        match vt_outcome(&src) {
            VtOutcome::Compiles(code) => {
                if let Err(why) = written_fields_survive(&src, k, &code) {
                    return Err(format!("{why}\n{src}"));
                }
                let fn_code = code
                    .split_once(&format!("'probe{k}'/1 = fun"))
                    .map_or("", |(_, rest)| rest.split("\n'").next().unwrap_or(rest))
                    .to_string();
                Ok(Some(fn_code))
            }
            VtOutcome::RejectedCleanly => Ok(None),
            VtOutcome::Fails(why) => Err(format!("{why}\n{src}")),
        }
    };
    let mut problems = Vec::new();
    for (k, (form, compiles)) in forms.iter().enumerate() {
        let bare = lower(k, form);
        let paren = lower(k, &format!("({form})"));
        match (&bare, &paren) {
            (Ok(Some(b)), Ok(Some(p))) if *compiles && b == p => {}
            (Ok(None), Ok(None)) if !*compiles => {}
            _ => problems.push(format!(
                "`{form}`: bare {} / parenthesised {}",
                summary(&bare),
                summary(&paren)
            )),
        }
    }
    assert!(problems.is_empty(), "{}", problems.join("\n"));
}

/// BT-3838 item 1: a parenthesised LAST value-type statement keeps its written form.
///
/// BT-3830 unwraps parentheses only on a non-last statement, because the last statement is the
/// method's value: a bare last field assignment answers the updated `Self`, a parenthesised one
/// answers the assignment's value (and `(t := t + 1)` answers `t + 1`). Both stay as written
/// there, so this pins that they still compile with no verifier diagnostic, with and without a
/// preceding `#(1) do: [:e | ^ e]` (a non-local return, whose tail returns
/// `{Result, FinalSelf}`), and that a field write in them survives.
#[test]
fn parenthesised_last_statements_compile_with_and_without_a_non_local_return() {
    let lasts = [
        "(self.m := 5)",
        "(t := t + 1)",
        "(self.m := self.m + 1)",
        "([t < 3] whileTrue: [t := t + 1])",
        "(c ifTrue: [self.m := 1] ifFalse: [0])",
    ];
    let prefixes = ["", "#(1) do: [:e | ^ e].\n    "];
    let mut problems = Vec::new();
    let mut k = 0;
    for prefix in prefixes {
        for last in lasts {
            let src = format!(
                "{VT_HEADER}\n  probe{k} =>\n    t := 0.\n    c := self.n >= 0.\n    {prefix}{last}\n"
            );
            match vt_outcome(&src) {
                VtOutcome::Compiles(code) => {
                    if let Err(why) = written_fields_survive(&src, k, &code) {
                        problems.push(format!("{why}\n{src}"));
                    }
                }
                VtOutcome::RejectedCleanly => {}
                VtOutcome::Fails(why) => problems.push(format!("{why}\n{src}")),
            }
            k += 1;
        }
    }
    assert!(problems.is_empty(), "{}", problems.join("\n\n"));
}

/// BT-3838 item 2: the actor / class-method counterpart of
/// `parenthesised_non_last_statements_lower_exactly_like_their_bare_form`. A NON-last statement's
/// value is discarded, so `(s)` must lower exactly like `s` in an `Actor subclass:` method and a
/// class method too, not only in a value type.
#[test]
fn parenthesised_non_last_statements_lower_like_bare_in_actors_and_class_methods() {
    const ACTOR: &str = "Actor subclass: ParenProbe
  state: n = 0
  state: m = 0
";
    const CLASS: &str = "Object subclass: ParenProbe
  classState: n = 0
  classState: m = 0
";
    let forms: &[&str] = &[
        "self.m := 5",
        "t := t + 1",
        "c ifTrue: [self.m := 1] ifFalse: [0]",
        "c ifTrue: [t := t + 1]",
        "c ifTrue: [t := t + 1. self.m := 1] ifFalse: [0]",
        "[self.m := 1] on: Error do: [:e | 0]",
        "[self.m := 1. t := t + 1] on: Error do: [:e | 0]",
        "[self.m := 1] ensure: [0]",
        "#(1, 2) do: [:e | t := t + e]",
        "#(1, 2) do: [:e | self.m := e]",
        "1 to: 3 do: [:i | t := t + i]",
        "[t < 3] whileTrue: [t := t + 1]",
        "3 timesRepeat: [t := t + 1]",
        "3 timesRepeat: [self.m := self.m + 1]",
        "#(1, 2) inject: 0 into: [:a :e | t := t + e. a + e]",
    ];
    // The generated function for `probe{k}`, so source offsets elsewhere in the module do not
    // make two spellings differ.
    let lower = |header: &str, class_side: bool, k: usize, stmt: &str| -> Result<String, String> {
        let sel = if class_side { "class " } else { "" };
        let src = format!(
            "{header}\n  {sel}probe{k} =>\n    t := 0.\n    c := self.n >= 0.\n    {stmt}.\n    t + self.m\n"
        );
        match vt_outcome(&src) {
            VtOutcome::Compiles(code) => {
                let at = code
                    .find(&format!("probe{k}'"))
                    .ok_or_else(|| format!("no generated function for probe{k}\n{src}"))?;
                // Actor methods are clauses of one `dispatch`: stop at the next clause.
                let body = code[at..].split("\n'").next().unwrap_or("");
                Ok(body
                    .split("<OtherSelector>")
                    .next()
                    .unwrap_or("")
                    .to_string())
            }
            VtOutcome::RejectedCleanly => Ok("rejected".to_string()),
            VtOutcome::Fails(why) => Err(format!("{why}\n{src}")),
        }
    };
    let mut problems = Vec::new();
    for (name, header, class_side) in [("actor", ACTOR, false), ("class method", CLASS, true)] {
        for (k, form) in forms.iter().enumerate() {
            let bare = lower(header, class_side, k, form);
            let paren = lower(header, class_side, k, &format!("({form})"));
            // The same statement parenthesised inside a conditional arm (a non-last arm statement).
            let arm_bare = lower(header, class_side, k, &format!("c ifTrue: [{form}. 0]"));
            let arm_paren = lower(header, class_side, k, &format!("c ifTrue: [({form}). 0]"));
            if arm_bare != arm_paren {
                problems.push(format!(
                    "{name} arm statement `{form}`:\n bare: {arm_bare:?}\n paren: {arm_paren:?}"
                ));
            }
            match (&bare, &paren) {
                (Ok(b), Ok(p)) if b == p => {}
                _ => problems.push(format!(
                    "{name} `{form}`:\n bare: {bare:?}\n paren: {paren:?}"
                )),
            }
        }
    }
    assert!(problems.is_empty(), "{}", problems.join("\n\n"));
}
