// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4638: `(x =:= false) ifTrue: [^...]` removes `False` from `x` for the
//! statements after the guard, as `x isNil ifTrue: [^...]` does for `Nil`.
//! The same holds for `=:= true` and the loose `==` spelling.

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
    let diags = type_diagnostics(&guard_source("Integer | False", "(x =:= false)"));
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:#?}");
}

#[test]
fn bt4638_loose_eq_false_narrows_false_out() {
    let diags = type_diagnostics(&guard_source("Integer | False", "(x == false)"));
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:#?}");
}

#[test]
fn bt4638_strict_eq_true_narrows_true_out() {
    let diags = type_diagnostics(&guard_source("Integer | True", "(x =:= true)"));
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:#?}");
}

#[test]
fn bt4638_loose_eq_true_narrows_true_out() {
    let diags = type_diagnostics(&guard_source("Integer | True", "(x == true)"));
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:#?}");
}

#[test]
fn bt4638_reversed_operands_narrow() {
    let diags = type_diagnostics(&guard_source("Integer | False", "(false =:= x)"));
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:#?}");
}

/// Guarding on `true` must not remove `False`: the send is still unsafe.
#[test]
fn bt4638_guard_on_other_boolean_does_not_narrow() {
    let diags = type_diagnostics(&guard_source("Integer | False", "(x =:= true)"));
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
    let diags = type_diagnostics(source);
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:#?}");
}
