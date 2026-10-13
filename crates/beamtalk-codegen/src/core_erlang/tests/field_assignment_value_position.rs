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
];

/// Positions where a self-send consumes the field assignment as an operand, inside a
/// `do:` fold body. They are skipped there because the fold-body self-send path does not
/// sequence its operands at all, independent of field assignments: `#(1, 2) do: [:e | self
/// take: (self bump) with: (self bump)]` fails `erlc` with the same `unbound variable
/// 'StateAcc1'` without any field assignment, and a single operand's rebind is dropped
/// silently. Tracked as its own issue; every other shape (including `whileTrue:` loop
/// bodies) is covered.
const FOLD_BODY_SELF_SEND_OPERANDS: &[&str] = &[
    "self_send_arg",
    "two_keyword_args",
    "keyword_arg_then_state_read",
    "after_self_send_sibling",
];

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
            .filter(|(_, (position_name, _))| {
                !(*shape_name == "do_block_body"
                    && FOLD_BODY_SELF_SEND_OPERANDS.contains(position_name))
            })
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
