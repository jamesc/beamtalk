// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4318: a `super` send that threads the method's `State` is a producer in
//! every position at an actor method root.
//!
//! `generate_super_send`'s method-root form answers `beamtalk_dispatch:super/5`'s
//! raw `{reply, Result, NewState}` tuple. Before BT-4318 only the statement and
//! `^` arms unpacked it, so the same send as an assignment value, an operand or a
//! literal element became the tuple itself and dropped the callee's state change.
//! The one unpacking is now `threaded_expression`'s `super_send_producer` (a
//! `State` `Bind` from `element(3, _)`, the value `element(2, _)`), and
//! `generate_super_send` reports any root-form compile that does not come from it
//! (`VerifyError::StateEffectEscapesExpression`).
//!
//! [`super_send_value_position_corpus_threads_state`] proves it over value
//! position x statement shape; each program must pass the debug verifier and
//! compile through `erlc`.
//! [`the_super_send_corpus_fails_without_the_producer`] proves the check fires on
//! every root-form cell when the producer is switched off.

use super::{assert_compiles_through_erlc, codegen};
use crate::core_erlang::threaded_expr::fault::{self, LocalRebindFault};
use std::panic::{AssertUnwindSafe, catch_unwind};

const HEADER: &str = "Actor subclass: SuperValueProbe
  state: n = 0

  bump -> Integer =>
    self.n := self.n + 1
    self.n

  take: x -> Object => x
";

/// A `super` send (`SUPER`, a `String`-answering send to `Object`) at the named
/// value position.
const POSITIONS: &[(&str, &str)] = &[
    ("bare", "SUPER"),
    ("binary_left", "SUPER size + 1"),
    ("binary_right", "1 + SUPER size"),
    ("unary_receiver", "SUPER size"),
    ("keyword_arg_of_self_send", "self take: SUPER"),
    (
        "keyword_arg_of_other_send",
        "#(1, 2) inject: SUPER into: [:a :b | a]",
    ),
    ("array_element", "#(SUPER, self bump, SUPER)"),
    ("map_value", "#{#a => SUPER}"),
    ("interpolation", "\"x{SUPER}y\""),
    (
        "conditional_receiver",
        "SUPER isEmpty ifTrue: [1] ifFalse: [2]",
    ),
    ("after_self_send_operand", "self bump + SUPER size"),
    ("super_argument", "super take: SUPER"),
    ("value_position_assignment", "(r := SUPER) size"),
    // The receiver or scrutinee of a construct that writes state at the
    // method root (lowered through its own mutation-threading path), and a
    // `super` send inside an inlined arm of one.
    (
        "mutating_loop_receiver",
        "SUPER do: [:c | self.n := self.n + 1]",
    ),
    (
        "mutating_conditional_receiver",
        "SUPER isEmpty ifTrue: [self.n := 1] ifFalse: [0]",
    ),
    (
        "mutating_match_scrutinee",
        "SUPER size match: [0 -> self.n := 1; _ -> 2]",
    ),
    (
        "inside_mutating_arm",
        "self.n > 0 ifTrue: [self.n := 1. SUPER] ifFalse: [0]",
    ),
];

/// Positions whose `super` send runs against the actor-state cell
/// (`beamtalk_actor_state:super_send`, ADR 0137 §3), which answers a plain
/// value and threads no `State`: an inlined arm is a block body here. They are
/// in the corpus for their verified codegen, not compiled by the producer.
const CELL_FORM_POSITIONS: &[&str] = &["inside_mutating_arm"];

/// A method-root statement shape consuming the position (`POS`), followed by a
/// state read where the shape is not the last statement.
const SHAPES: &[(&str, &str)] = &[
    ("last_expression", "    POS\n"),
    ("returned", "    ^POS\n"),
    ("statement_then_read", "    POS.\n    self.n\n"),
    ("assigned_then_read", "    x := POS.\n    x.\n    self.n\n"),
    ("field_value", "    self.n := POS.\n    self.n\n"),
    (
        "field_assigned",
        "    self.n := self.n + 1.\n    x := POS.\n    self.n := self.n + 1.\n    x\n",
    ),
];

/// `(name, method body, root form)` for every shape x position.
fn cases() -> Vec<(String, String, bool)> {
    let mut cases = Vec::new();
    for (shape_name, shape) in SHAPES {
        for (position_name, position) in POSITIONS {
            let pos = position.replace("SUPER", "(super printString)");
            cases.push((
                format!("{shape_name} / {position_name}"),
                shape.replace("POS", &pos),
                !CELL_FORM_POSITIONS.contains(position_name),
            ));
        }
    }
    cases
}

fn failure(method_body: &str) -> Option<String> {
    let src = format!("{HEADER}\n  probe =>\n{method_body}");
    catch_unwind(AssertUnwindSafe(|| {
        assert_compiles_through_erlc("test", &codegen(&src));
    }))
    .err()
    .map(|payload| {
        payload
            .downcast_ref::<String>()
            .cloned()
            .or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_string()))
            .unwrap_or_else(|| "<non-string panic>".to_string())
    })
}

#[test]
fn super_send_value_position_corpus_threads_state() {
    let failures: Vec<String> = cases()
        .iter()
        .filter_map(|(name, body, _)| failure(body).map(|f| format!("{name}:\n{body}\n{f}")))
        .collect();
    assert!(
        failures.is_empty(),
        "{} super-send case(s) fail verified codegen:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

#[test]
fn the_super_send_corpus_fails_without_the_producer() {
    let escaped: Vec<String> = cases()
        .iter()
        .filter(|(_, _, root_form)| *root_form)
        .filter(|(_, body, _)| {
            !fault::with(LocalRebindFault::SuperSendNotAProducer, || failure(body))
                .is_some_and(|f| f.contains("StateEffectEscapesExpression"))
        })
        .map(|(name, _, _)| name.clone())
        .collect();
    assert!(
        escaped.is_empty(),
        "cells not reported without the super producer: {escaped:?}"
    );
}
