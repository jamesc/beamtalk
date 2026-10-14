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

/// Not covered by this corpus: value-type methods (BT-3812 adds them) and class
/// methods (a class-variable write is an in-place `put`, ADR 0130 §2, which threads
/// nothing). The REPL has no `self` field to write.

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
