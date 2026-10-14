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

use super::{assert_compiles_through_erlc, codegen};
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

const VT_HEADER: &str = "TestCase subclass: FieldAssignVtProbe
  field: n = 0
  field: m = 0

  take: x -> Integer => x
  take: x with: y -> Integer => x
";

/// Positions valid for a value type (no actor self-send `bump`: a value-type self-send is a
/// plain call that threads nothing).
const VT_POSITIONS: &[(&str, &str)] = &[
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
];

/// Enclosing shapes. A value-type field write inside a conditional arm or loop body is either
/// threaded or rejected with a structured diagnostic (`ValueSelf*` errors); both are fine.
/// What is not fine is an `erlc` failure, a debug-verifier failure or any other error.
const VT_SHAPES: &[(&str, &str)] = &[
    ("then_read", "    STMT.\n    self.n\n"),
    ("last_statement", "    STMT\n"),
    (
        "between_field_writes",
        "    self.m := 1.\n    STMT.\n    self.m := self.m + 1.\n    self.n\n",
    ),
    (
        "conditional_arm",
        "    self.m > 5 ifTrue: [STMT] ifFalse: [self.m := 1].\n    self.n\n",
    ),
    (
        "conditional_condition",
        "    ((STMT) > 3) ifTrue: [self.m := 1].\n    self.n\n",
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
    ("block_value", "    [STMT] value.\n    self.n\n"),
];

enum VtOutcome {
    Compiles,
    /// A structured value-type diagnostic: the construct is rejected, not miscompiled.
    RejectedCleanly,
    Fails(String),
}

fn vt_outcome(src: &str) -> VtOutcome {
    use crate::core_erlang::{CodeGenError, CodegenOptions, generate_module};
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let generated = catch_unwind(AssertUnwindSafe(|| {
        generate_module(&module, CodegenOptions::new("test"))
    }));
    match generated {
        Err(_) => VtOutcome::Fails("debug ThreadedIr verifier / codegen panic".to_string()),
        Ok(Err(
            CodeGenError::ValueSelfFieldAssignmentInMatchArm { .. }
            | CodeGenError::ValueSelfMutationLostAcrossNestedLoop { .. }
            | CodeGenError::FieldAssignmentInUnsupportedBlock { .. },
        )) => VtOutcome::RejectedCleanly,
        Ok(Err(other)) => VtOutcome::Fails(format!("codegen error: {other:?}")),
        Ok(Ok(code)) => {
            if catch_unwind(AssertUnwindSafe(|| {
                assert_compiles_through_erlc("test", &code);
            }))
            .is_ok()
            {
                VtOutcome::Compiles
            } else {
                VtOutcome::Fails("erlc rejected the generated Core Erlang".to_string())
            }
        }
    }
}

/// PIN-BUG (BT-3816): value-type field assignment in value position is not threaded by the
/// value-type method-body lowering (`emit_vt_body_expr`), so these `shape / position` cases fail
/// `erlc` (unbound `Self{N}`) or the debug `ThreadedIr` verifier. The corpus asserts the failing
/// set EQUALS this list: a fix for BT-3816 makes every pin fail the equality, and flips them by
/// emptying the list.
const VT_PINNED_FAILURES: &[&str] = &[
    "then_read / self_send_arg",
    "then_read / binary_left",
    "then_read / binary_right",
    "then_read / unary_receiver",
    "then_read / two_keyword_args",
    "then_read / keyword_arg_then_state_read",
    "then_read / list_literal_element",
    "then_read / string_interpolation",
    "then_read / assigned_to_local",
    "then_read / assigned_to_field",
    "then_read / nested_field_assignment",
    "then_read / returned",
    "then_read / cascade_argument",
    "last_statement / string_interpolation",
    "last_statement / assigned_to_local",
    "last_statement / assigned_to_field",
    "last_statement / nested_field_assignment",
    "last_statement / returned",
    "last_statement / cascade_argument",
    "between_field_writes / self_send_arg",
    "between_field_writes / binary_left",
    "between_field_writes / binary_right",
    "between_field_writes / unary_receiver",
    "between_field_writes / two_keyword_args",
    "between_field_writes / keyword_arg_then_state_read",
    "between_field_writes / list_literal_element",
    "between_field_writes / string_interpolation",
    "between_field_writes / assigned_to_local",
    "between_field_writes / assigned_to_field",
    "between_field_writes / nested_field_assignment",
    "between_field_writes / returned",
    "between_field_writes / cascade_argument",
    "conditional_condition / self_send_arg",
    "conditional_condition / binary_left",
    "conditional_condition / binary_right",
    "conditional_condition / unary_receiver",
    "conditional_condition / two_keyword_args",
    "conditional_condition / keyword_arg_then_state_read",
    "conditional_condition / list_literal_element",
    "conditional_condition / string_interpolation",
    "conditional_condition / assigned_to_local",
    "conditional_condition / assigned_to_field",
    "conditional_condition / nested_field_assignment",
    "conditional_condition / returned",
    "conditional_condition / cascade_argument",
    "block_value / self_send_arg",
    "block_value / binary_left",
    "block_value / binary_right",
    "block_value / unary_receiver",
    "block_value / two_keyword_args",
    "block_value / keyword_arg_then_state_read",
    "block_value / list_literal_element",
    "block_value / string_interpolation",
    "block_value / assigned_to_local",
    "block_value / assigned_to_field",
    "block_value / nested_field_assignment",
    "block_value / returned",
    "block_value / cascade_argument",
];

#[test]
fn value_type_field_assignment_value_position_corpus() {
    let mut failing: Vec<String> = Vec::new();
    let mut details: Vec<String> = Vec::new();
    let mut compiled = 0usize;
    for (shape_name, shape) in VT_SHAPES {
        for (i, (position_name, position)) in VT_POSITIONS.iter().enumerate() {
            let stmt = position.replace("EXPR", "(self.n := 5)");
            let src = format!(
                "{VT_HEADER}\n  probe{i} =>\n{}",
                shape.replace("STMT", &stmt)
            );
            match vt_outcome(&src) {
                VtOutcome::Compiles => compiled += 1,
                VtOutcome::RejectedCleanly => {}
                VtOutcome::Fails(why) => {
                    failing.push(format!("{shape_name} / {position_name}"));
                    details.push(format!("{shape_name} / {position_name}: {why}\n{src}"));
                }
            }
        }
    }
    let pinned: Vec<String> = VT_PINNED_FAILURES
        .iter()
        .map(|s| (*s).to_string())
        .collect();
    let unexpected: Vec<&String> = failing.iter().filter(|f| !pinned.contains(f)).collect();
    let fixed: Vec<&String> = pinned.iter().filter(|p| !failing.contains(p)).collect();
    assert!(
        unexpected.is_empty(),
        "new value-type field-assignment failure(s) not pinned to an open issue: {unexpected:?}\n\n{}",
        details.join("\n\n")
    );
    assert!(
        fixed.is_empty(),
        "BT-3816 pin(s) now pass; remove them from VT_PINNED_FAILURES: {fixed:?}"
    );
    // The unpinned cases are compiled or rejected with a structured diagnostic; pin that some compile (the last-statement shapes).
    assert!(
        compiled >= 7,
        "only {compiled} value-type cases compiled; the unpinned ones must"
    );
}
