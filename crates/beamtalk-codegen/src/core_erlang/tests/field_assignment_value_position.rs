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

/// Not covered by this corpus: class methods (a class-variable write is an in-place `put`,
/// ADR 0130 §2, which threads nothing) and the REPL (no `self` field to write). Value types
/// are covered by [`value_type_field_assignment_value_position_corpus`] below; the only
/// exclusion there is a plain `Value subclass:`, which reports the immutability error.

#[test]
fn field_assignment_value_position_corpus() {
    let passes = |src: &str| {
        catch_unwind(AssertUnwindSafe(|| {
            assert_compiles_through_erlc("test", &codegen(src));
        }))
        .is_ok()
    };
    let mut failures: Vec<String> = Vec::new();
    for (shape_name, shape) in SHAPES {
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
    for (name, body) in HYBRID_LOOP_METHODS {
        let src = format!("{HEADER}\n  probe =>\n{body}");
        if !passes(&src) {
            failures.push(format!("hybrid_loop / {name}:\n{src}"));
        }
    }
    assert!(
        failures.is_empty(),
        "{} field-assignment case(s) fail verified codegen:\n\n{}",
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
    // tuple (`do:` / `collect:` / `inject:into:` bodies cannot write a field in a value type
    // in any position, see `VT_UNSUPPORTED_FIELD_WRITING_BODIES`).
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
];

/// Loop-shaped constructs whose BODY writes a field are rejected in every position of a value
/// type, statement position included: their accumulator has no `Self` slot yet. A gap separate
/// from BT-3821's value-position threading, pinned so a fix flips it.
const VT_UNSUPPORTED_FIELD_WRITING_BODIES: &[&str] = &[
    "self take: (#(1, 2) do: [:e | self.m := e])",
    "xs := #(1, 2) collect: [:e | self.m := e]. xs",
    "self take: (#(1, 2) inject: 0 into: [:a :e | self.m := e. a])",
    "#(1, 2) do: [:e | self.m := e]",
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

enum VtOutcome {
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
fn vt_outcome(src: &str) -> VtOutcome {
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

#[test]
fn value_type_field_assignment_value_position_corpus() {
    let mut failing: Vec<String> = Vec::new();
    let mut details: Vec<String> = Vec::new();
    let mut compiled = 0usize;
    // Statement-level shapes cannot be rejected: every unpinned case of one must compile.
    let mut must_compile = 0usize;
    let mut must_compile_compiled = 0usize;
    for (shape_name, shape, shape_may_reject) in VT_SHAPES {
        for (i, (position_name, position)) in VT_POSITIONS.iter().enumerate() {
            let stmt = position.replace("EXPR", "(self.n := 5)");
            let src = format!(
                "{VT_HEADER}\n  probe{i} =>\n{}",
                shape.replace("STMT", &stmt)
            );
            let may_reject = *shape_may_reject;
            let name = format!("{shape_name} / {position_name}");
            let pinned = VT_PINNED_FAILURES.contains(&name.as_str());
            if !may_reject && !pinned {
                must_compile += 1;
            }
            match vt_outcome(&src) {
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
    let pinned: Vec<String> = VT_PINNED_FAILURES
        .iter()
        .map(|s| (*s).to_string())
        .collect();
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
    // `RejectedCleanly` allowance, and at least one case must compile at all.
    assert_eq!(
        must_compile_compiled, must_compile,
        "{must_compile_compiled} of {must_compile} unpinned statement-level value-type cases compiled; they cannot be rejected"
    );
    assert!(
        compiled >= must_compile && compiled > 0,
        "only {compiled} value-type cases compiled"
    );
}

/// `do:` / `collect:` / `inject:into:` bodies that write a field are not threaded in a value type
/// at all (their accumulator has no `Self` slot), statement position included: a gap separate
/// from BT-3821's value-position conditional threading. They must stay a structured rejection
/// (never a silently dropped write); the day one compiles, this fails and the entry moves to
/// [`VT_POSITIONS`].
#[test]
fn field_writing_fold_bodies_stay_a_structured_rejection() {
    for body in VT_UNSUPPORTED_FIELD_WRITING_BODIES {
        let src = format!("{VT_HEADER}\n  probe =>\n    {body}\n    self.n\n");
        assert!(
            matches!(vt_outcome(&src), VtOutcome::RejectedCleanly),
            "`{body}` is expected to be rejected with a structured diagnostic; if it now \
             compiles, move it into VT_POSITIONS:\n{src}"
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
