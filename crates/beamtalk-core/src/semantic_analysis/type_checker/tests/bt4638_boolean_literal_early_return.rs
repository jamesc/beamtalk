// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4638: `(x =:= false) ifTrue: [^...]` removes `False` from `x` for the
//! statements after the guard, as `x isNil ifTrue: [^...]` does for `Nil`.
//! The same holds for `=:= true` and the loose `==` spelling.

use super::common::*;

fn guard_source(ty: &str, guard: &str) -> String {
    format!(
        r"
typed Object subclass: Repro
  class go: x :: {ty} -> Integer =>
    {guard} ifTrue: [^0]
    x + 1
"
    )
}

#[test]
fn bt4638_strict_eq_false_narrows_false_out() {
    let diags = type_dnu_diags(&guard_source("Integer | False", "(x =:= false)"));
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:#?}");
}

#[test]
fn bt4638_loose_eq_false_narrows_false_out() {
    let diags = type_dnu_diags(&guard_source("Integer | False", "(x == false)"));
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:#?}");
}

#[test]
fn bt4638_strict_eq_true_narrows_true_out() {
    let diags = type_dnu_diags(&guard_source("Integer | True", "(x =:= true)"));
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:#?}");
}

#[test]
fn bt4638_loose_eq_true_narrows_true_out() {
    let diags = type_dnu_diags(&guard_source("Integer | True", "(x == true)"));
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:#?}");
}

#[test]
fn bt4638_reversed_operands_narrow() {
    let diags = type_dnu_diags(&guard_source("Integer | False", "(false =:= x)"));
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:#?}");
}

/// Guarding on `true` must not remove `False`: the send is still unsafe.
#[test]
fn bt4638_guard_on_other_boolean_does_not_narrow() {
    let diags = type_dnu_diags(&guard_source("Integer | False", "(x =:= true)"));
    assert!(
        !diags.is_empty(),
        "`False` is still possible after a `true` guard, expected a diagnostic"
    );
}

/// `ifFalse: [^...]` proves the test held, so `x` is `False` afterwards.
#[test]
fn bt4638_if_false_diverging_narrows_to_the_boolean() {
    let source = r"
typed Object subclass: Repro
  class go: x :: Integer | False -> Boolean =>
    (x =:= false) ifFalse: [^true]
    x not
";
    let diags = type_dnu_diags(source);
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:#?}");
}

/// In-branch narrowing: `False` is removed from `x` in the `ifFalse:` arm.
#[test]
fn bt4638_if_true_if_false_narrows_each_branch() {
    let source = r"
typed Object subclass: Repro
  class go: x :: Integer | False -> Integer =>
    (x =:= false) ifTrue: [0] ifFalse: [x + 1]
";
    let diags = type_dnu_diags(source);
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:#?}");
}

/// Negated spellings are deliberately not narrowed. An `ifFalse: [^0]` guard
/// is the one that would narrow `False` out if `/=` were recognised, so the
/// diagnostic here proves it is not (an `ifTrue:` guard would pass either way).
#[test]
fn bt4638_negated_spelling_does_not_narrow() {
    let source = r"
typed Object subclass: Repro
  class go: x :: Integer | False -> Integer =>
    (x /= false) ifFalse: [^0]
    x + 1
";
    let diags = type_dnu_diags(source);
    assert!(
        !diags.is_empty(),
        "`/=` is not recognised, so `False` is still possible; expected a diagnostic"
    );
}

/// Testing an inferred `Integer` against `false` can never be true.
#[test]
fn bt4638_integer_eq_false_hints_never_true() {
    let source = r"
typed Object subclass: Repro
  class go -> Integer =>
    n := 3
    (n =:= false) ifTrue: [^0]
    n
";
    let hinted = run_with_expect(
        &parse_source(source),
        &ClassHierarchy::build(&parse_source(source)).0.unwrap(),
    )
    .iter()
    .any(|d| d.message.contains("can never be true"));
    assert!(hinted, "expected a `can never be true` hint");
}
