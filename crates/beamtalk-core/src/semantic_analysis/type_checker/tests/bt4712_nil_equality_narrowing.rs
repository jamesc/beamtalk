// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4712: `x =:= nil` / `x == nil` / `x =/= nil` / `x /= nil` (either operand
//! order) narrow `x` exactly as `x isNil` / `x notNil` do.

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

/// Wraps `body` (statements using `x :: Integer | Nil`) in a typed method.
fn method(body: &str) -> String {
    format!(
        r"
typed Object subclass: Repro
  class go: x :: Integer | Nil -> Integer =>
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

/// Asserts the body is rejected *because* `x` still (or only) includes nil:
/// the diagnostic must be the `+` send failing on the nil type.
fn assert_rejected(body: &str) {
    let diags = type_diagnostics(&method(body));
    assert!(
        diags.iter().any(|d| d.message.contains('+')
            && (d.message.contains("Nil") || d.message.contains("UndefinedObject"))),
        "expected `+` to be rejected on a nil type for:\n{body}\ngot: {diags:#?}"
    );
}

#[test]
fn eq_nil_if_false_narrows_non_nil() {
    assert_clean("    (x =:= nil) ifFalse: [^x + 1]\n    0");
}

#[test]
fn loose_eq_nil_early_return_narrows_after_guard() {
    assert_clean("    (x == nil) ifTrue: [^0]\n    x + 1");
}

#[test]
fn reversed_operands_early_return_narrows() {
    assert_clean("    (nil =:= x) ifTrue: [^0]\n    x + 1");
}

#[test]
fn neq_nil_if_true_narrows_non_nil() {
    assert_clean("    (x =/= nil) ifTrue: [^x + 1]\n    0");
}

#[test]
fn loose_neq_nil_if_true_narrows_non_nil() {
    assert_clean("    (x /= nil) ifTrue: [^x + 1]\n    0");
}

#[test]
fn neq_nil_if_false_early_return_narrows_after_guard() {
    assert_clean("    (x =/= nil) ifFalse: [^0]\n    x + 1");
}

#[test]
fn neq_nil_reversed_if_true_narrows() {
    assert_clean("    (nil =/= x) ifTrue: [^x + 1]\n    0");
}

#[test]
fn if_true_if_false_narrows_both_arms() {
    assert_clean("    ^(x =:= nil) ifTrue: [0] ifFalse: [x + 1]");
    assert_clean("    ^(x =/= nil) ifTrue: [x + 1] ifFalse: [0]");
}

#[test]
fn neq_nil_diverging_false_arm_narrows_after_statement() {
    assert_clean("    (x =/= nil) ifTrue: [0] ifFalse: [^0]\n    x + 1");
}

#[test]
fn and_or_blocks_see_the_narrowing() {
    assert_clean("    ((x =/= nil) and: [x > 0]) ifTrue: [^1]\n    0");
    assert_clean("    ((x =:= nil) or: [x > 0]) ifTrue: [^1]\n    0");
}

#[test]
fn eq_nil_true_branch_is_nil() {
    // True branch narrows to UndefinedObject, which has no `+`.
    assert_rejected("    (x =:= nil) ifTrue: [^x + 1]\n    0");
}

#[test]
fn neq_nil_false_branch_is_nil() {
    assert_rejected("    (x =/= nil) ifFalse: [^x + 1]\n    0");
}

#[test]
fn guard_that_does_not_exclude_nil_is_still_rejected() {
    assert_rejected("    (x =:= nil) ifFalse: [^0]\n    x + 1");
    assert_rejected("    (x =/= nil) ifTrue: [^0]\n    x + 1");
}

fn field_source(guard_and_use: &str) -> String {
    format!(
        r"
typed Object subclass: Holder
  state: n :: Integer | Nil = nil
  take: m :: Integer -> Integer => m
  go -> Integer =>
{guard_and_use}
"
    )
}

#[test]
fn self_field_eq_nil_narrows() {
    for body in [
        "    (self.n =:= nil) ifTrue: [^0]\n    self take: self.n",
        "    (self.n == nil) ifFalse: [^self take: self.n]\n    0",
        "    (self.n =/= nil) ifTrue: [^self take: self.n]\n    0",
        "    (self.n /= nil) ifFalse: [^0]\n    self take: self.n",
    ] {
        let diags = type_diagnostics(&field_source(body));
        assert!(diags.is_empty(), "for:\n{body}\ngot: {diags:#?}");
    }
}

#[test]
fn self_field_without_narrowing_is_rejected() {
    for body in [
        "    self take: self.n",
        "    (self.n =:= nil) ifFalse: [^0]\n    self take: self.n",
        "    (self.n =/= nil) ifTrue: [^0]\n    self take: self.n",
    ] {
        let diags = type_diagnostics(&field_source(body));
        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Nil") || d.message.contains("UndefinedObject")),
            "expected a nil-type mismatch for:\n{body}\ngot: {diags:#?}"
        );
    }
}
