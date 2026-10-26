// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4199: residual type-checker false positives from the weekend PRs.
//!
//! 1. `self.f := v` on a `:: Dynamic` field must not warn.
//! 2. `self.f := C` on a `:: X class` field is covariant (`C` subclass of `X`).
//! 3. `ifTrue: [...] ifFalse: [^...]` post-guard narrowing (single and `and:`).
//! 4. `Collection>>includes:` accepts any `Object`.

use super::common::*;

fn type_diagnostics(source: &str) -> Vec<Diagnostic> {
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    run_with_expect(&module, &hierarchy)
        .into_iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect()
}

#[test]
fn bt4199_dynamic_field_accepts_any_assignment() {
    let diags = type_diagnostics(
        r"
typed Actor subclass: Foo
  state: t :: Dynamic = 0
  sett -> Integer =>
    self.t := 3
    3
",
    );
    assert!(diags.is_empty(), "got: {diags:#?}");
}

#[test]
fn bt4199_non_dynamic_field_still_warns() {
    let diags = type_diagnostics(
        r"
typed Actor subclass: Foo
  state: t :: String = ''
  sett -> Integer =>
    self.t := 3
    3
",
    );
    assert!(
        diags.iter().any(|d| d.message.contains("Type mismatch")),
        "got: {diags:#?}"
    );
}

#[test]
fn bt4199_metaclass_slot_accepts_subclass_value() {
    let diags = type_diagnostics(
        r"
typed Actor subclass: Holder
  state: kind :: Number class = Number
  setKind -> Object =>
    self.kind := Integer
",
    );
    assert!(diags.is_empty(), "got: {diags:#?}");
}

#[test]
fn bt4199_metaclass_slot_rejects_unrelated_class() {
    let diags = type_diagnostics(
        r"
typed Actor subclass: Holder
  state: kind :: Number class = Number
  setKind -> Object =>
    self.kind := String
",
    );
    assert!(
        diags.iter().any(|d| d.message.contains("Type mismatch")),
        "got: {diags:#?}"
    );
}

#[test]
fn bt4199_and_not_nil_if_true_if_false_narrows() {
    let diags = type_diagnostics(
        r"
typed Object subclass: N
  class f: a :: Integer | Nil g: b :: Integer | Nil -> Integer =>
    (a notNil and: [b notNil]) ifTrue: [0] ifFalse: [^1]
    a + b
",
    );
    assert!(diags.is_empty(), "got: {diags:#?}");
}

#[test]
fn bt4199_lone_not_nil_if_true_if_false_narrows() {
    let diags = type_diagnostics(
        r"
typed Object subclass: N
  class f: a :: Integer | Nil -> Integer =>
    a notNil ifTrue: [0] ifFalse: [^1]
    a
",
    );
    assert!(diags.is_empty(), "got: {diags:#?}");
}

#[test]
fn bt4199_non_diverging_false_block_does_not_narrow() {
    let diags = type_diagnostics(
        r"
typed Object subclass: N
  class f: a :: Integer | Nil -> Integer =>
    a notNil ifTrue: [0] ifFalse: [1]
    a
",
    );
    assert!(!diags.is_empty(), "expected a nilable warning, got none");
}

#[test]
fn bt4199_interval_includes_accepts_any_object() {
    let diags = type_diagnostics(
        r"
typed Object subclass: N
  class f: x :: Number -> Boolean =>
    (1 to: 10) includes: x
  class g -> Boolean =>
    (1 to: 10) includes: 2.0
",
    );
    assert!(diags.is_empty(), "got: {diags:#?}");
}
