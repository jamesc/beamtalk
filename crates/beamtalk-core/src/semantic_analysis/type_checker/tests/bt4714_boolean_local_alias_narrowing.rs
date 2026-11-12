// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4714: a local assigned a condition (`c := x notNil`) stands for what the
//! condition proves, until the local or a variable in the facts is written.

use super::common::*;

fn type_diagnostics(source: &str) -> Vec<Diagnostic> {
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    run_with_expect(&module, &hierarchy)
        .into_iter()
        .filter(|d| {
            matches!(
                d.category,
                Some(DiagnosticCategory::Type | DiagnosticCategory::Dnu)
            )
        })
        .collect()
}

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
    let diags = type_diagnostics(&method(body));
    assert!(diags.is_empty(), "expected clean:\n{body}\ngot: {diags:#?}");
}

fn assert_rejected(body: &str) {
    let diags = type_diagnostics(&method(body));
    assert!(!diags.is_empty(), "expected a diagnostic:\n{body}");
}

#[test]
fn alias_if_true() {
    assert_clean("    c := x notNil.\n    c ifTrue: [Repro need: x. ^0].\n    0");
}

#[test]
fn alias_if_false_is_nil() {
    assert_clean("    c := x isNil.\n    c ifFalse: [Repro need: x. ^0].\n    0");
}

#[test]
fn alias_if_true_if_false() {
    assert_clean("    c := x isNil.\n    ^c ifTrue: [0] ifFalse: [Repro need: x]");
}

#[test]
fn alias_and_chain() {
    assert_clean(
        "    c := x notNil and: [n notNil].\n    c ifTrue: [Repro need: x. Repro need: n. ^0].\n    0",
    );
}

#[test]
fn alias_not() {
    assert_clean("    c := x isNil.\n    c not ifTrue: [Repro need: x. ^0].\n    0");
}

#[test]
fn alias_post_guard() {
    assert_clean("    c := x isNil.\n    c ifTrue: [^0].\n    Repro need: x");
}

#[test]
fn alias_in_while_condition() {
    assert_clean("    c := x notNil.\n    [c] whileTrue: [Repro need: x. ^0].\n    0");
}

#[test]
fn wrong_polarity_stays_rejected() {
    assert_rejected("    c := x notNil.\n    c ifFalse: [Repro need: x. ^0].\n    0");
}

#[test]
fn reassigned_tested_variable_drops_alias() {
    assert_rejected("    c := x notNil.\n    x := n.\n    c ifTrue: [Repro need: x. ^0].\n    0");
}

#[test]
fn reassigned_alias_drops_alias() {
    assert_rejected("    c := x notNil.\n    c := p.\n    c ifTrue: [Repro need: x. ^0].\n    0");
}

#[test]
fn reassigned_alias_to_new_condition_uses_new_facts() {
    assert_rejected(
        "    c := x notNil.\n    c := n notNil.\n    c ifTrue: [Repro need: x. ^0].\n    0",
    );
}

#[test]
fn self_referential_alias_is_not_kept() {
    assert_rejected("    x := x notNil.\n    x ifTrue: [Repro need: x. ^0].\n    0");
}

#[test]
fn alias_does_not_outlive_reassignment_in_branch() {
    assert_rejected(
        "    c := x notNil.\n    p ifTrue: [x := n].\n    c ifTrue: [Repro need: x. ^0].\n    0",
    );
}

#[test]
fn block_written_alias_is_not_trusted() {
    assert_rejected(
        "    c := x notNil.\n    [x := n] value.\n    c ifTrue: [Repro need: x. ^0].\n    0",
    );
}

#[test]
fn field_facts_are_not_kept() {
    assert_rejected("    c := self.f notNil.\n    c ifTrue: [Repro need: self.f. ^0].\n    0");
}

#[test]
fn copied_alias_is_dropped_with_the_tested_variable() {
    assert_clean("    c := x notNil.\n    d := c.\n    d ifTrue: [Repro need: x. ^0].\n    0");
    assert_rejected(
        "    c := x notNil.\n    d := c.\n    x := n.\n    d ifTrue: [Repro need: x. ^0].\n    0",
    );
}

#[test]
fn negated_alias_in_place_swaps_facts() {
    assert_clean("    c := x isNil.\n    c := c not.\n    c ifTrue: [Repro need: x. ^0].\n    0");
}

#[test]
fn shadowing_block_parameter_is_not_narrowed() {
    assert_rejected(
        "    c := x notNil.\n    ^[:x | c ifTrue: [Repro need: x] ifFalse: [0]] value: n",
    );
}

#[test]
fn alias_created_inside_a_block_is_not_trusted() {
    // `block_written_locals` treats every name assigned in a block as shared;
    // the alias is simply not recorded (the safe direction).
    assert_rejected("    p ifTrue: [c := x notNil. c ifTrue: [Repro need: x. ^0]].\n    0");
}
