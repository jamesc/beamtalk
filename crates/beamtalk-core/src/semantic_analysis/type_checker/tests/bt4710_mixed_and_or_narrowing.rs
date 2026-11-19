// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4710: narrowing flows through `and:` / `or:` chains that mix nil tests,
//! `isKindOf:` tests and conditions the checker cannot read. One unrecognised
//! part of a chain contributes no facts but does not stop the rest narrowing.

use super::common::*;

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

fn assert_clean(name: &str, body: &str) {
    assert_clean_diags(&type_dnu_diags(&method(body)), &format!("[{name}] {body}"));
}

fn assert_rejected(name: &str, body: &str) {
    assert_has_diags(&type_dnu_diags(&method(body)), &format!("[{name}] {body}"));
}

// --- BT-4908: the narrowing cases, as tables ------------------------------
//
// Each row is (case name, method body); the name appears in the failure message.

/// Chains that must narrow (and `not`-swapped forms): no diagnostics.
const CLEAN: &[(&str, &str)] = &[
    (
        "unknown_and_not_nil_if_true",
        "    (p and: [x notNil]) ifTrue: [^self need: x]\n    0",
    ),
    (
        "not_nil_and_unknown_if_true",
        "    (x notNil and: [p]) ifTrue: [^self need: x]\n    0",
    ),
    (
        "not_nil_and_unknown_if_true_if_false",
        "    (x notNil and: [p]) ifTrue: [self need: x] ifFalse: [0]",
    ),
    (
        "is_nil_or_unknown_if_false",
        "    (x isNil or: [p]) ifFalse: [^self need: x]\n    0",
    ),
    (
        "is_nil_or_unknown_early_return",
        "    (x isNil or: [p]) ifTrue: [^0]\n    self need: x",
    ),
    (
        "not_nil_and_unknown_early_return_on_false",
        "    (x notNil and: [p]) ifFalse: [^0]\n    self need: x",
    ),
    (
        "nested_and_with_unknown_tail",
        "    (x notNil and: [y notNil and: [p]]) ifTrue: [^self need: x and: y]\n    0",
    ),
    (
        "nested_and_with_unknown_middle",
        "    (x notNil and: [p and: [y notNil]]) ifTrue: [^self need: x and: y]\n    0",
    ),
    (
        "nil_and_kind_of",
        "    (x notNil and: [k isKindOf: Integer]) ifTrue: [^self need: x and: k]\n    0",
    ),
    (
        "kind_of_in_a_compound_chain",
        "    ((k isKindOf: Integer) and: [k isKindOf: Integer]) ifTrue: [^self need: k]\n    0",
    ),
    (
        "kind_of_with_unknown_and",
        "    ((k isKindOf: Integer) and: [p]) ifTrue: [^self need: k]\n    0",
    ),
    (
        "kind_of_or_early_return",
        "    ((k isKindOf: String) or: [p]) ifFalse: [^self need: k]\n    0",
    ),
    // The second test is checked under the first one's facts.
    (
        "and_block_sees_unknown_prefix_facts",
        "    (x notNil and: [p and: [x > 0]]) ifTrue: [^x]\n    0",
    ),
    // `or:` block runs when the receiver is false: x is non-nil there.
    (
        "and_or_block_argument_narrowing",
        "    (x isNil or: [x > 0]) ifTrue: [^0]\n    x",
    ),
    (
        "and_block_with_mixed_receiver",
        "    ((p and: [x notNil]) and: [x > 0]) ifTrue: [^x]\n    0",
    ),
    (
        "pure_chains_still_narrow#1",
        "    (x isNil or: [y isNil]) ifTrue: [^0]\n    self need: x and: y",
    ),
    (
        "pure_chains_still_narrow#2",
        "    (x notNil and: [y notNil]) ifFalse: [^0]\n    self need: x and: y",
    ),
    // Both branches of the `or:` prove `x` non-nil.
    (
        "disjunction_of_tests_joins_facts",
        "    ((x notNil and: [p]) or: [x notNil and: [y notNil]]) ifTrue: [^self need: x]\n    0",
    ),
    // `or:` is false only when both sides are: `x isNil` is false.
    (
        "unknown_or_is_nil_if_false",
        "    (p or: [x isNil]) ifFalse: [^self need: x]\n    0",
    ),
    (
        "diverging_false_arm_narrows_by_true_facts",
        "    (x notNil and: [p]) ifTrue: [0] ifFalse: [^0]\n    self need: x",
    ),
    (
        "unrelated_rhs_assignment_keeps_the_left_facts",
        "    l := x\n    m := 0\n    (l notNil and: [m := 1. true]) ifTrue: [^self need: l]\n    0",
    ),
    (
        "not_is_nil_if_true",
        "    x isNil not ifTrue: [^self need: x]\n    0",
    ),
    (
        "not_not_nil_early_return",
        "    x notNil not ifTrue: [^0]\n    self need: x",
    ),
    (
        "not_kind_of_early_return",
        "    (k isKindOf: Integer) not ifTrue: [^0]\n    self need: k",
    ),
    (
        "not_of_disjunction_narrows_both",
        "    (x isNil or: [y isNil]) not ifTrue: [^self need: x and: y]\n    0",
    ),
    (
        "not_composes_inside_and_or#1",
        "    (x isNil not and: [p]) ifTrue: [^self need: x]\n    0",
    ),
    (
        "not_composes_inside_and_or#2",
        "    (p and: [x isNil not]) ifTrue: [^self need: x]\n    0",
    ),
    (
        "not_composes_inside_and_or#3",
        "    (x notNil not or: [p]) ifTrue: [^0]\n    self need: x",
    ),
    (
        "not_composes_inside_and_or#4",
        "    (x isNil not and: [x > 0]) ifTrue: [^x]\n    0",
    ),
    (
        "double_not_cancels#1",
        "    x notNil not not ifTrue: [^self need: x]\n    0",
    ),
];

/// Negative controls: each must stay rejected.
const REJECTED: &[(&str, &str)] = &[
    (
        "or_with_unknown_head_does_not_narrow_true",
        "    (p or: [x notNil]) ifTrue: [^self need: x]\n    0",
    ),
    (
        "not_nil_or_unknown_does_not_narrow_true",
        "    (x notNil or: [p]) ifTrue: [^self need: x]\n    0",
    ),
    (
        "is_nil_and_unknown_does_not_narrow_false",
        "    (x isNil and: [p]) ifFalse: [^self need: x]\n    0",
    ),
    (
        "and_does_not_narrow_false_branch",
        "    (p and: [x notNil]) ifFalse: [^self need: x]\n    0",
    ),
    (
        "and_does_not_narrow_false_when_only_one_side_narrows",
        "    (p and: [x isNil]) ifFalse: [^self need: x]\n    0",
    ),
    (
        "disjunction_with_one_unnarrowed_side_does_not_narrow",
        "    ((x notNil and: [p]) or: [p]) ifTrue: [^self need: x]\n    0",
    ),
    // Falling through `(p and: [x notNil]) ifTrue: [^0]` proves nothing.
    (
        "early_return_on_unknown_and_does_not_narrow",
        "    (p and: [x notNil]) ifTrue: [^0]\n    self need: x",
    ),
    // The first statement could reassign the tested local; the block is not
    // understood, so nothing is proven.
    (
        "multi_statement_block_contributes_nothing",
        "    l := x\n    (p and: [l := nil. l notNil]) ifTrue: [^self need: l]\n    0",
    ),
    (
        "narrowing_does_not_escape_the_block",
        "    (p and: [x notNil]) ifTrue: [0]\n    self need: x",
    ),
    (
        "rhs_block_reassigning_a_tested_local_voids_and_facts",
        "    l := x\n    (l notNil and: [l := nil. true]) ifTrue: [^self need: l]\n    0",
    ),
    (
        "rhs_single_expression_reassignment_voids_and_facts",
        "    l := x\n    (l notNil and: [(l := nil) isNil]) ifTrue: [^self need: l]\n    0",
    ),
    (
        "rhs_block_reassigning_a_tested_local_voids_or_post_guard_facts",
        "    l := x\n    (l isNil or: [l := nil. false]) ifTrue: [^0]\n    self need: l",
    ),
    (
        "double_not_cancels#2",
        "    x isNil not not ifTrue: [^self need: x]\n    0",
    ),
    (
        "not_negative_controls_stay_rejected#1",
        "    x notNil not ifTrue: [^self need: x]\n    0",
    ),
    (
        "not_negative_controls_stay_rejected#2",
        "    p not ifTrue: [^self need: x]\n    0",
    ),
    (
        "not_negative_controls_stay_rejected#3",
        "    (x isNil not and: [p]) ifFalse: [^self need: x]\n    0",
    ),
    (
        "not_negative_controls_stay_rejected#4",
        "    (k isKindOf: Integer) not ifFalse: [^0]\n    self need: k",
    ),
    (
        "not_keeps_reassignment_guard",
        "    l := x\n    (l isNil not and: [l := nil. true]) ifTrue: [^self need: l]\n    0",
    ),
];

#[test]
fn mixed_chains_narrow() {
    for (name, body) in CLEAN {
        assert_clean(name, body);
    }
}

#[test]
fn negative_controls_stay_rejected() {
    for (name, body) in REJECTED {
        assert_rejected(name, body);
    }
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
    type_dnu_diags(&format!(
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

#[test]
fn rhs_block_literal_running_a_dynamic_value_voids_field_facts() {
    // `cb` is an unannotated parameter (Dynamic): it may be a field-writing block.
    let diags = actor_diagnostics(
        "  run: cb =>\n    (self.f notNil and: [cb value]) ifTrue: [^self need: self.f]\n    0",
    );
    assert_field_leak(&diags);
}

#[test]
fn rhs_running_a_negation_typed_value_voids_field_facts() {
    // `cb` is `Object \ Number` in the `ifFalse:` arm: still possibly a block.
    let diags = actor_diagnostics(
        "  run: cb :: Object -> Integer =>\n    (cb isKindOf: Number) ifFalse: [(self.f notNil and: [cb value]) ifTrue: [^self need: self.f]]\n    0",
    );
    assert_field_leak(&diags);
}
