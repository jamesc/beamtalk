// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4635: a type test on the receiver of `and:` / `or:` narrows the tested
//! variable inside the lazy block argument, exactly as the matching
//! `ifTrue:` / `ifFalse:` arm would.
//!
//! `(each isKindOf: Tuple) and: [(each at: 1) =:= #k]` used to report
//! "Object does not understand 'at:'" because only `notNil` receivers
//! (BT-2017) pushed a refinement before checking the block.

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

/// The issue repro: `isKindOf:` on the receiver of `and:`.
#[test]
fn bt4635_is_kind_of_and_narrows_block() {
    let source = r"
typed Object subclass: Repro
  class narrow: items :: List(Object) -> Integer =>
    items do: [:each |
      ((each isKindOf: Tuple) and: [(each at: 1) =:= #k]) ifTrue: [^1]
    ]
    0
";
    let diags = type_diagnostics(source);
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:#?}");
}

/// Negated test: the `or:` block runs only when `isKindOf:` answered false,
/// so `each` is the complement there. `Object` minus `Tuple` still has no
/// `at:`, so the send must still be reported (the narrowing must not
/// over-reach into "any send is fine").
#[test]
fn bt4635_is_kind_of_or_block_sees_complement_not_kind() {
    let source = r"
typed Object subclass: Repro
  class narrow: items :: List(Object) -> Integer =>
    items do: [:each |
      ((each isKindOf: Tuple) or: [(each at: 1) =:= #k]) ifTrue: [^1]
    ]
    0
";
    let diags = type_diagnostics(source);
    assert!(
        diags.iter().any(|d| d.message.contains("at:")),
        "`each` is not a Tuple in the `or:` block, so `at:` must still be flagged, got: {diags:#?}"
    );
}

/// Negative control: without the type test the send is still flagged.
#[test]
fn bt4635_and_without_type_test_still_reports_dnu() {
    let source = r"
typed Object subclass: Repro
  class narrow: items :: List(Object) -> Integer =>
    items do: [:each |
      (each notNil and: [(each at: 1) =:= #k]) ifTrue: [^1]
    ]
    0
";
    let diags = type_diagnostics(source);
    assert!(
        diags.iter().any(|d| d.message.contains("at:")),
        "expected DNU for `at:` on Object, got: {diags:#?}"
    );
}

/// `or:` complement via the `is_nil_check` fallback: the block runs only when
/// `x isNil` answered false, so `x` is non-nil there. Fails without the fix
/// (`Integer | Nil` has no `+`).
#[test]
fn bt4635_is_nil_or_narrows_block_to_non_nil() {
    let source = r"
typed Object subclass: Repro
  check: x :: Integer | Nil -> Boolean =>
    x isNil or: [x + 1 > 0]
";
    let diags = type_diagnostics(source);
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:#?}");
}

/// `or:` complement via the explicit `false_type`: not-a-`String` leaves
/// `Integer`, so `+` is valid in the block.
#[test]
fn bt4635_is_kind_of_or_narrows_block_to_complement() {
    let source = r"
typed Object subclass: Repro
  check: x :: Integer | String -> Boolean =>
    (x isKindOf: String) or: [x + 1 > 0]
";
    let diags = type_diagnostics(source);
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:#?}");
}

/// `and:` with a positive test on a union narrows to the tested member.
#[test]
fn bt4635_is_kind_of_and_narrows_union_member() {
    let source = r"
typed Object subclass: Repro
  check: x :: Integer | String -> Boolean =>
    (x isKindOf: Integer) and: [x + 1 > 0]
";
    let diags = type_diagnostics(source);
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:#?}");
}

/// The newly reachable "can never be true" class-test hint fires exactly once
/// for an unrelated class in `and:` position (no duplicate from the block).
#[test]
fn bt4635_unrelated_is_kind_of_and_hint_fires_once() {
    let source = r"
typed Object subclass: Repro
  check -> Boolean =>
    x := 3
    (x isKindOf: String) and: [true]
";
    let diags = type_diagnostics(source);
    let hints = diags
        .iter()
        .filter(|d| d.message.contains("can never be true"))
        .count();
    assert_eq!(hints, 1, "expected exactly one hint, got: {diags:#?}");
}
