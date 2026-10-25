// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Compound nil-guard narrowing (BT-4107).
//!
//! A guard-and-return on one variable narrows it (`a isNil ifTrue: [^nil]`
//! proves `a` non-nil afterwards), but the combined forms did not:
//! `(a isNil or: [b isNil]) ifTrue: [^nil]` left both `a` and `b` nullable,
//! and likewise `(a notNil and: [b notNil]) ifFalse: [^nil]`.
//!
//! By De Morgan duality for the lazy `Boolean>>or:` / `Boolean>>and:`
//! (`stdlib/src/boolean.bt`):
//! - `or:` is false only when *both* disjuncts are false, so a diverging
//!   `ifTrue:` guard (or the `ifFalse:` arm) proves *every* tested var non-nil.
//! - `and:` is true only when *both* conjuncts are true, so a diverging
//!   `ifFalse:` guard (or the `ifTrue:` arm) proves *every* tested var non-nil.
//!
//! The complementary directions are *disjunctive* (`or:` true means some var
//! is nil, but not which) and must NOT narrow — several tests below lock that
//! soundness boundary in.

use super::common::*;

fn type_diagnostics(source: &str) -> Vec<Diagnostic> {
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    run_with_expect(&module, &hierarchy)
        .into_iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect()
}

/// AC: the issue repro — `(c isNil or: [r isNil]) ifTrue: [^nil]` narrows
/// both `c` and `r`, so the `use:and:` call below sees two `Pt`s.
#[test]
fn bt4107_or_nil_guard_narrows_both_vars_issue_repro() {
    let source = r"
typed Object subclass: Pt
  x -> Integer => 1

typed Object subclass: N
  class use: a :: Pt and: b :: Pt -> Integer => a x + b x
  class f: c :: Pt | Nil g: r :: Pt | Nil -> Integer | Nil =>
    (c isNil or: [r isNil]) ifTrue: [^nil]
    self use: c and: r
";
    let diags = type_diagnostics(source);
    assert!(
        diags.is_empty(),
        "both `c` and `r` should narrow to Pt after the combined guard, got: {diags:#?}"
    );
}

/// AC: instance-method shape — both params narrow after the `or:` guard.
#[test]
fn bt4107_or_nil_guard_narrows_both_params_post_guard() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: a :: Integer | Nil with: b :: Integer | Nil =>
    (a isNil or: [b isNil]) ifTrue: [^nil]
    Receiver process: a
    Receiver process: b
";
    let diags = type_diagnostics(source);
    assert!(
        diags.is_empty(),
        "both `a` and `b` should narrow to Integer after the guard, got: {diags:#?}"
    );
}

/// AC: `(a notNil and: [b notNil]) ifFalse: [^nil]` narrows both vars —
/// the `and:` dual of the `or:` guard.
#[test]
fn bt4107_and_not_nil_guard_narrows_both_params_post_guard() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: a :: Integer | Nil with: b :: Integer | Nil =>
    (a notNil and: [b notNil]) ifFalse: [^nil]
    Receiver process: a
    Receiver process: b
";
    let diags = type_diagnostics(source);
    assert!(
        diags.is_empty(),
        "both `a` and `b` should narrow to Integer after the guard, got: {diags:#?}"
    );
}

/// A non-diverging `or:` guard must NOT narrow — both vars stay nullable.
#[test]
fn bt4107_or_nil_guard_non_diverging_does_not_narrow() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: a :: Integer | Nil with: b :: Integer | Nil =>
    (a isNil or: [b isNil]) ifTrue: [42]
    Receiver process: a
    Receiver process: b
";
    let diags = type_diagnostics(source);
    let mismatches: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("'process:'") && d.message.contains("Nil"))
        .collect();
    assert_eq!(
        mismatches.len(),
        2,
        "non-diverging guard must leave both vars nullable (2 warnings), got: {diags:#?}"
    );
}

/// Soundness: `(a isNil or: [b isNil]) ifFalse: [^nil]` proves only the
/// *disjunction* (some var is nil) on fall-through — neither var narrows.
#[test]
fn bt4107_or_nil_guard_if_false_diverge_does_not_narrow() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: a :: Integer | Nil with: b :: Integer | Nil =>
    (a isNil or: [b isNil]) ifFalse: [^nil]
    Receiver process: a
    Receiver process: b
";
    let diags = type_diagnostics(source);
    let mismatches: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("'process:'") && d.message.contains("Nil"))
        .collect();
    assert_eq!(
        mismatches.len(),
        2,
        "disjunctive fall-through must leave both vars nullable (2 warnings), got: {diags:#?}"
    );
}

/// Soundness: `(a notNil and: [b notNil]) ifTrue: [^nil]` proves only the
/// *disjunction* (some var is nil) on fall-through — neither var narrows.
#[test]
fn bt4107_and_not_nil_guard_if_true_diverge_does_not_narrow() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: a :: Integer | Nil with: b :: Integer | Nil =>
    (a notNil and: [b notNil]) ifTrue: [^nil]
    Receiver process: a
    Receiver process: b
";
    let diags = type_diagnostics(source);
    let mismatches: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("'process:'") && d.message.contains("Nil"))
        .collect();
    assert_eq!(
        mismatches.len(),
        2,
        "disjunctive fall-through must leave both vars nullable (2 warnings), got: {diags:#?}"
    );
}

/// The combined `ifTrue: [^nil] ifFalse: [...]` `or:` guard narrows both
/// vars when the false block does not reassign them.
#[test]
fn bt4107_or_nil_guard_if_true_if_false_diverge_narrows() {
    let source = r#"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: a :: Integer | Nil with: b :: Integer | Nil =>
    (a isNil or: [b isNil]) ifTrue: [^nil] ifFalse: [Transcript show: "ok"]
    Receiver process: a
    Receiver process: b
"#;
    let diags = type_diagnostics(source);
    assert!(
        diags.is_empty(),
        "both vars should narrow after the combined guard, got: {diags:#?}"
    );
}

/// ... but a false block reassigning a tested var cancels the narrowing.
#[test]
fn bt4107_or_nil_guard_if_true_if_false_reassign_cancels() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: a :: Integer | Nil with: b :: Integer | Nil =>
    (a isNil or: [b isNil]) ifTrue: [^nil] ifFalse: [a := nil]
    Receiver process: a
    Receiver process: b
";
    let diags = type_diagnostics(source);
    let mismatches: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("'process:'") && d.message.contains("Nil"))
        .collect();
    assert!(
        !mismatches.is_empty(),
        "reassigning `a` in the false block must cancel narrowing, got: {diags:#?}"
    );
}

/// Nested `or:` chains narrow every tested var, not just the first two.
#[test]
fn bt4107_or_nil_chain_of_three_narrows_all() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: a :: Integer | Nil =>
    b := a
    c := a
    (a isNil or: [b isNil or: [c isNil]]) ifTrue: [^nil]
    Receiver process: a
    Receiver process: b
    Receiver process: c
";
    let diags = type_diagnostics(source);
    assert!(
        diags.is_empty(),
        "all three vars should narrow after the chained guard, got: {diags:#?}"
    );
}

/// `self.field` spellings narrow through the same synthetic keys as locals.
#[test]
fn bt4107_or_nil_guard_self_fields_narrow() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Actor subclass: Engine
  state: left :: Integer | Nil = nil
  state: right :: Integer | Nil = nil
  run =>
    (self.left isNil or: [self.right isNil]) ifTrue: [^nil]
    Receiver process: self.left
    Receiver process: self.right
";
    let diags = type_diagnostics(source);
    assert!(
        diags.is_empty(),
        "both fields should narrow after the guard, got: {diags:#?}"
    );
}

/// In-branch dual: both vars are non-nil inside the `ifFalse:` arm of an
/// `or:` of nil tests.
#[test]
fn bt4107_or_nil_guard_narrows_in_false_branch() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: a :: Integer | Nil with: b :: Integer | Nil =>
    (a isNil or: [b isNil]) ifFalse: [
      Receiver process: a
      Receiver process: b
    ]
";
    let diags = type_diagnostics(source);
    assert!(
        diags.is_empty(),
        "both vars should narrow inside the `ifFalse:` arm, got: {diags:#?}"
    );
}

/// In-branch dual: both vars are non-nil inside the `ifTrue:` arm of an
/// `and:` of not-nil tests.
#[test]
fn bt4107_and_not_nil_guard_narrows_in_true_branch() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: a :: Integer | Nil with: b :: Integer | Nil =>
    (a notNil and: [b notNil]) ifTrue: [
      Receiver process: a
      Receiver process: b
    ]
";
    let diags = type_diagnostics(source);
    assert!(
        diags.is_empty(),
        "both vars should narrow inside the `ifTrue:` arm, got: {diags:#?}"
    );
}

/// Soundness: the `ifTrue:` arm of an `or:` of nil tests is disjunctive —
/// neither var narrows inside it.
#[test]
fn bt4107_or_nil_guard_does_not_narrow_in_true_branch() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: a :: Integer | Nil with: b :: Integer | Nil =>
    (a isNil or: [b isNil]) ifTrue: [
      Receiver process: a
      Receiver process: b
    ]
";
    let diags = type_diagnostics(source);
    let mismatches: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("'process:'") && d.message.contains("Nil"))
        .collect();
    assert_eq!(
        mismatches.len(),
        2,
        "disjunctive true arm must leave both vars nullable (2 warnings), got: {diags:#?}"
    );
}

/// A lone `a notNil` is an `and:`-chain of length 1, so it narrows
/// through the same path: `ifFalse: [^nil]` proves `a` non-nil afterwards.
/// (Previously no rule matched a bare `notNil` test at all.)
#[test]
fn bt4107_single_not_nil_if_false_diverge_narrows_post_guard() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: a :: Integer | Nil =>
    a notNil ifFalse: [^nil]
    Receiver process: a
";
    let diags = type_diagnostics(source);
    assert!(
        diags.is_empty(),
        "`a` should narrow to Integer after the guard, got: {diags:#?}"
    );
}

/// ... and narrows inside the `ifTrue:` arm.
#[test]
fn bt4107_single_not_nil_narrows_in_true_branch() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: a :: Integer | Nil =>
    a notNil ifTrue: [Receiver process: a]
";
    let diags = type_diagnostics(source);
    assert!(
        diags.is_empty(),
        "`a` should narrow to Integer inside the `ifTrue:` arm, got: {diags:#?}"
    );
}

/// Soundness: the `ifFalse:` arm of an `and:` of not-nil tests is
/// disjunctive — neither var narrows inside it.
#[test]
fn bt4107_and_not_nil_guard_does_not_narrow_in_false_branch() {
    let source = r"
typed Object subclass: Receiver
  class process: v :: Integer => v

typed Object subclass: Caller
  run: a :: Integer | Nil with: b :: Integer | Nil =>
    (a notNil and: [b notNil]) ifFalse: [
      Receiver process: a
      Receiver process: b
    ]
";
    let diags = type_diagnostics(source);
    let mismatches: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("'process:'") && d.message.contains("Nil"))
        .collect();
    assert_eq!(
        mismatches.len(),
        2,
        "disjunctive false arm must leave both vars nullable (2 warnings), got: {diags:#?}"
    );
}
