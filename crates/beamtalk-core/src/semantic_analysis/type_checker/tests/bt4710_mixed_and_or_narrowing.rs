// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4710: narrowing flows through `and:` / `or:` chains that mix nil tests,
//! `isKindOf:` tests and conditions the checker cannot read. One unrecognised
//! part of a chain contributes no facts but does not stop the rest narrowing.

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

/// Wraps `body` in a typed method over `x, y :: Integer | Nil`,
/// `p :: Boolean`, `k :: Integer | String`.
fn method(body: &str) -> String {
    format!(
        r"
typed Object subclass: Repro
  class need: n :: Integer -> Integer => n
  class need: a :: Integer and: b :: Integer -> Integer => a + b

  class go: x :: Integer | Nil y: y :: Integer | Nil p: p :: Boolean k: k :: Integer | String -> Integer =>
{body}
"
    )
}

fn assert_clean(body: &str) {
    let diags = type_diagnostics(&method(body));
    assert!(
        diags.is_empty(),
        "expected no diagnostics for:\n{body}\ngot: {diags:#?}"
    );
}

fn assert_rejected(body: &str) {
    let diags = type_diagnostics(&method(body));
    assert!(
        !diags.is_empty(),
        "expected a diagnostic for:\n{body}\ngot none"
    );
}

// --- mixed chains that must narrow ---------------------------------------

#[test]
fn unknown_and_not_nil_if_true() {
    assert_clean("    (p and: [x notNil]) ifTrue: [^self need: x]\n    0");
}

#[test]
fn not_nil_and_unknown_if_true() {
    assert_clean("    (x notNil and: [p]) ifTrue: [^self need: x]\n    0");
}

#[test]
fn not_nil_and_unknown_if_true_if_false() {
    assert_clean("    (x notNil and: [p]) ifTrue: [self need: x] ifFalse: [0]");
}

#[test]
fn is_nil_or_unknown_if_false() {
    assert_clean("    (x isNil or: [p]) ifFalse: [^self need: x]\n    0");
}

#[test]
fn is_nil_or_unknown_early_return() {
    assert_clean("    (x isNil or: [p]) ifTrue: [^0]\n    self need: x");
}

#[test]
fn not_nil_and_unknown_early_return_on_false() {
    assert_clean("    (x notNil and: [p]) ifFalse: [^0]\n    self need: x");
}

#[test]
fn nested_and_with_unknown_tail() {
    assert_clean("    (x notNil and: [y notNil and: [p]]) ifTrue: [^self need: x and: y]\n    0");
}

#[test]
fn nested_and_with_unknown_middle() {
    assert_clean("    (x notNil and: [p and: [y notNil]]) ifTrue: [^self need: x and: y]\n    0");
}

#[test]
fn nil_and_kind_of() {
    assert_clean("    (x notNil and: [k isKindOf: Integer]) ifTrue: [^self need: x and: k]\n    0");
}

#[test]
fn kind_of_in_a_compound_chain() {
    assert_clean(
        "    ((k isKindOf: Integer) and: [k isKindOf: Integer]) ifTrue: [^self need: k]\n    0",
    );
}

#[test]
fn kind_of_with_unknown_and() {
    assert_clean("    ((k isKindOf: Integer) and: [p]) ifTrue: [^self need: k]\n    0");
}

#[test]
fn kind_of_or_early_return() {
    assert_clean("    ((k isKindOf: String) or: [p]) ifFalse: [^self need: k]\n    0");
}

#[test]
fn and_block_sees_unknown_prefix_facts() {
    // The second test is checked under the first one's facts.
    assert_clean("    (x notNil and: [p and: [x > 0]]) ifTrue: [^x]\n    0");
}

#[test]
fn and_or_block_argument_narrowing() {
    // `or:` block runs when the receiver is false: x is non-nil there.
    assert_clean("    (x isNil or: [x > 0]) ifTrue: [^0]\n    x");
}

#[test]
fn and_block_with_mixed_receiver() {
    assert_clean("    ((p and: [x notNil]) and: [x > 0]) ifTrue: [^x]\n    0");
}

#[test]
fn pure_chains_still_narrow() {
    assert_clean("    (x isNil or: [y isNil]) ifTrue: [^0]\n    self need: x and: y");
    assert_clean("    (x notNil and: [y notNil]) ifFalse: [^0]\n    self need: x and: y");
}

#[test]
fn disjunction_of_tests_joins_facts() {
    // Both branches of the `or:` prove `x` non-nil.
    assert_clean(
        "    ((x notNil and: [p]) or: [x notNil and: [y notNil]]) ifTrue: [^self need: x]\n    0",
    );
}

#[test]
fn unknown_or_is_nil_if_false() {
    // `or:` is false only when both sides are: `x isNil` is false.
    assert_clean("    (p or: [x isNil]) ifFalse: [^self need: x]\n    0");
}

// --- negative controls: must stay rejected -------------------------------

#[test]
fn or_with_unknown_head_does_not_narrow_true() {
    assert_rejected("    (p or: [x notNil]) ifTrue: [^self need: x]\n    0");
}

#[test]
fn not_nil_or_unknown_does_not_narrow_true() {
    assert_rejected("    (x notNil or: [p]) ifTrue: [^self need: x]\n    0");
}

#[test]
fn is_nil_and_unknown_does_not_narrow_false() {
    assert_rejected("    (x isNil and: [p]) ifFalse: [^self need: x]\n    0");
}

#[test]
fn and_does_not_narrow_false_branch() {
    assert_rejected("    (p and: [x notNil]) ifFalse: [^self need: x]\n    0");
}

#[test]
fn and_does_not_narrow_false_when_only_one_side_narrows() {
    assert_rejected("    (p and: [x isNil]) ifFalse: [^self need: x]\n    0");
}

#[test]
fn disjunction_with_one_unnarrowed_side_does_not_narrow() {
    assert_rejected("    ((x notNil and: [p]) or: [p]) ifTrue: [^self need: x]\n    0");
}

#[test]
fn early_return_on_unknown_and_does_not_narrow() {
    // Falling through `(p and: [x notNil]) ifTrue: [^0]` proves nothing.
    assert_rejected("    (p and: [x notNil]) ifTrue: [^0]\n    self need: x");
}

#[test]
fn multi_statement_block_contributes_nothing() {
    // The first statement could reassign the tested local; the block is not
    // understood, so nothing is proven.
    assert_rejected("    l := x\n    (p and: [l := nil. l notNil]) ifTrue: [^self need: l]\n    0");
}

#[test]
fn diverging_false_arm_narrows_by_true_facts() {
    assert_clean("    (x notNil and: [p]) ifTrue: [0] ifFalse: [^0]\n    self need: x");
}

#[test]
fn narrowing_does_not_escape_the_block() {
    assert_rejected("    (p and: [x notNil]) ifTrue: [0]\n    self need: x");
}

// --- a right-hand argument that invalidates the left side's facts --------

#[test]
fn rhs_block_reassigning_a_tested_local_voids_and_facts() {
    assert_rejected(
        "    l := x\n    (l notNil and: [l := nil. true]) ifTrue: [^self need: l]\n    0",
    );
}

#[test]
fn rhs_single_expression_reassignment_voids_and_facts() {
    assert_rejected(
        "    l := x\n    (l notNil and: [(l := nil) isNil]) ifTrue: [^self need: l]\n    0",
    );
}

#[test]
fn rhs_block_reassigning_a_tested_local_voids_or_post_guard_facts() {
    assert_rejected(
        "    l := x\n    (l isNil or: [l := nil. false]) ifTrue: [^0]\n    self need: l",
    );
}

#[test]
fn unrelated_rhs_assignment_keeps_the_left_facts() {
    assert_clean(
        "    l := x\n    m := 0\n    (l notNil and: [m := 1. true]) ifTrue: [^self need: l]\n    0",
    );
}

fn assert_field_leak(diags: &[Diagnostic]) {
    assert!(
        diags
            .iter()
            .any(|d| d.message.contains("expects Integer, got Integer | Nil")),
        "expected the `need:` argument mismatch, got: {diags:#?}"
    );
}

fn actor_diagnostics(body: &str) -> Vec<Diagnostic> {
    type_diagnostics(&format!(
        r"
typed Actor subclass: Holder
  state: f :: Integer | Nil = nil
  reset -> Boolean => self.f := nil. true
  need: v :: Integer -> Integer => v
{body}
"
    ))
}

#[test]
fn rhs_self_send_voids_field_facts_in_arm() {
    let diags = actor_diagnostics(
        "  run -> Integer =>\n    (self.f isNil or: [self reset]) ifFalse: [^self need: self.f]\n    0",
    );
    assert_field_leak(&diags);
}

#[test]
fn rhs_without_self_send_keeps_field_facts() {
    let diags = actor_diagnostics(
        "  run: p :: Boolean -> Integer =>\n    (self.f isNil or: [p]) ifFalse: [^self need: self.f]\n    0",
    );
    assert!(diags.is_empty(), "got: {diags:#?}");
}

#[test]
fn rhs_stored_block_voids_field_facts() {
    let diags = actor_diagnostics(
        "  run -> Integer =>\n    blk := [self.f := nil. true]\n    (self.f notNil and: blk) ifTrue: [^self need: self.f]\n    0",
    );
    assert_field_leak(&diags);
}

#[test]
fn rhs_block_literal_running_a_stored_block_voids_field_facts() {
    let diags = actor_diagnostics(
        "  run -> Integer =>\n    blk := [self.f := nil. true]\n    (self.f notNil and: [blk value]) ifTrue: [^self need: self.f]\n    0",
    );
    assert_field_leak(&diags);
}
