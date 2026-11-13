// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4713: the body of `[cond] whileTrue: [...]` (`whileFalse:`) is narrowed
//! by the facts the condition block's last expression proves, for bindings the
//! body never assigns. `self.<field>` facts are applied too (BT-4788); a write to
//! the field, or a self-send, in the body drops them for the statements after it.

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
fn field_facts_narrow_the_loop_body() {
    // BT-4788: `self.<field>` facts reach the body like any other fact.
    assert_clean("    [self.f notNil] whileTrue: [Repro need: self.f. ^0]\n    0");
    assert_clean("    [self.f isNil] whileFalse: [Repro need: self.f. ^0]\n    0");
    assert_clean("    [self.f notNil and: [p]] whileTrue: [Repro need: self.f. ^0]\n    0");
}

/// An actor, where a self-send or a stored block can reassign the field.
fn actor_with(body: &str) -> String {
    format!(
        r"
typed Actor subclass: Holder
  state: f :: Integer | Nil = nil
  reset -> Nil => self.f := nil
  need: v :: Integer -> Integer => v
{body}
"
    )
}

fn actor_diags(body: &str) -> Vec<Diagnostic> {
    type_diags(&actor_with(body))
}

#[test]
fn actor_field_fact_kept_until_a_self_send_in_the_body() {
    let ok = actor_diags(
        "  run -> Integer =>\n    [self.f notNil] whileTrue: [self need: self.f. self reset. ^0]\n    0",
    );
    assert!(ok.is_empty(), "got: {ok:#?}");
    let leak = actor_diags(
        "  run -> Integer =>\n    [self.f notNil] whileTrue: [self reset. self need: self.f. ^0]\n    0",
    );
    assert!(!leak.is_empty(), "a self-send must drop the field fact");
}

#[test]
fn actor_field_fact_dropped_after_a_field_write_in_the_body() {
    let leak = actor_diags(
        "  run -> Integer =>\n    [self.f notNil] whileTrue: [self.f := nil. self need: self.f. ^0]\n    0",
    );
    assert!(!leak.is_empty(), "a field write must drop the field fact");
}

#[test]
fn field_fact_wrong_polarity_stays_rejected() {
    assert_rejected("    [self.f isNil] whileTrue: [Repro need: self.f. ^0]\n    0");
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
