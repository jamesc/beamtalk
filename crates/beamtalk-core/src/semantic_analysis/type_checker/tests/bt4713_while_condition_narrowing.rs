// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4713: the body of `[cond] whileTrue: [...]` (`whileFalse:`) is narrowed
//! by the facts the condition block's last expression proves, for bindings the
//! body never assigns. `self.<field>` facts are never applied to a loop body.

use super::common::*;

fn method(body: &str) -> String {
    format!(
        r"
typed Object subclass: Repro
  state: f :: Integer | Nil = nil

  class need: n :: Integer -> Integer => n
  need: n :: Integer -> Integer => n
  bump -> Integer =>
    self.f := nil
    0

  go: x :: Integer | Nil n: n :: Integer | Nil p: p :: Boolean -> Integer =>
{body}
"
    )
}

fn assert_clean(body: &str) {
    let diags = type_dnu_diags(&method(body));
    assert!(diags.is_empty(), "expected clean:\n{body}\ngot: {diags:#?}");
}

fn assert_rejected(body: &str) {
    let diags = type_dnu_diags(&method(body));
    assert!(!diags.is_empty(), "expected a diagnostic:\n{body}");
}

#[test]
fn while_true_not_nil() {
    assert_clean("    [x notNil] whileTrue: [Repro need: x. ^0]\n    0");
}

#[test]
fn while_true_and_chain() {
    assert_clean("    [n notNil and: [p]] whileTrue: [Repro need: n. ^0]\n    0");
}

#[test]
fn while_false_is_nil() {
    assert_clean("    [n isNil] whileFalse: [Repro need: n. ^0]\n    0");
}

#[test]
fn condition_with_leading_statements() {
    assert_clean("    [p. x notNil] whileTrue: [Repro need: x. ^0]\n    0");
}

#[test]
fn wrong_polarity_stays_rejected() {
    assert_rejected("    [x isNil] whileTrue: [Repro need: x. ^0]\n    0");
    assert_rejected("    [x notNil] whileFalse: [Repro need: x. ^0]\n    0");
}

#[test]
fn not_narrowed_after_loop() {
    assert_rejected("    [x notNil] whileTrue: [^0]\n    Repro need: x");
}

#[test]
fn reassigned_variable_is_not_narrowed() {
    assert_rejected(
        "    m :: Integer | Nil := n\n    [m notNil] whileTrue: [m := nil. Repro need: m. ^0]\n    0",
    );
}

#[test]
fn reassigned_variable_not_narrowed_even_before_assignment() {
    // Conservative: the body assigns `m`, so no fact about it is applied.
    assert_rejected(
        "    m :: Integer | Nil := n\n    [m notNil] whileTrue: [Repro need: m. m := nil]\n    0",
    );
}

#[test]
fn unrelated_variable_still_narrowed_when_other_assigned() {
    assert_clean(
        "    m :: Integer | Nil := n\n    [x notNil and: [m notNil]] whileTrue: [Repro need: x. m := nil. ^0]\n    0",
    );
}

#[test]
fn field_facts_are_not_applied_to_loop_bodies() {
    // Pins the documented limitation: no `self.<field>` narrowing in a loop
    // body, even when the body cannot write the field.
    assert_rejected("    [self.f notNil] whileTrue: [Repro need: self.f. ^0]\n    0");
}

#[test]
fn write_from_nested_block_in_body_drops_fact() {
    assert_rejected(
        "    m :: Integer | Nil := n\n    [m notNil] whileTrue: [#(1) do: [:e | m := nil]. Repro need: m. ^0]\n    0",
    );
}

#[test]
fn class_test_condition_adds_no_new_hint() {
    // `whileTrue:` conditions never reported the "can never be true" class-test
    // hint; narrowing the body must not start doing so.
    assert_clean("    [p] whileTrue: [^0]\n    [x isKindOf: String] whileTrue: [^0]\n    0");
}

#[test]
fn condition_block_assigns_the_tested_variable() {
    // The common shape: the condition block reads into a local, then tests it.
    assert_clean(
        "    m :: Integer | Nil := nil\n    [m := n. m notNil] whileTrue: [Repro need: m. ^0]\n    0",
    );
}
