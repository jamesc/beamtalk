// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4458: a `^expr` arm in `match:` (or `ifTrue:ifFalse:`) never produces a
//! value for the expression, so it must not contribute `expr`'s type to the
//! result type.

use super::common::*;

/// Messages of the `Type` diagnostics for `source`, asserting it parses cleanly.
fn type_messages(source: &str) -> Vec<String> {
    let (_, parse_diags) =
        crate::source_analysis::parse(crate::source_analysis::lex_with_eof(source));
    assert!(
        parse_diags.is_empty(),
        "fixture must parse: {parse_diags:?}"
    );
    type_diags(source)
        .iter()
        .map(|d| d.message.to_string())
        .collect()
}

#[test]
fn match_return_arm_does_not_pollute_result_type() {
    let diags = type_messages(
        r"typed Object subclass: ZzRepro
  class pick: v :: String | Nil -> Tuple(Integer, Integer) =>
    s := v match: [
      t :: String -> t;
      _ -> ^{0, 0}
    ]
    {self len: s, 1}

  class len: s :: String -> Integer => s size
",
    );
    assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
}

#[test]
fn match_non_return_arm_still_contributes() {
    let diags = type_messages(
        r"typed Object subclass: ZzRepro
  class pick: v :: String | Nil -> Integer =>
    s := v match: [
      t :: String -> t;
      _ -> 5
    ]
    self len: s

  class len: s :: String -> Integer => s size
",
    );
    assert!(
        diags.iter().any(|m| m.contains("expects String")),
        "a value-producing arm must still widen the result: {diags:?}"
    );
}

#[test]
fn if_true_if_false_return_branch_does_not_pollute_result_type() {
    let diags = type_messages(
        r"typed Object subclass: ZzRepro
  class pick: v :: String | Nil -> Integer =>
    s := v isNil ifTrue: [^0] ifFalse: [v]
    self len: s

  class len: s :: String -> Integer => s size
",
    );
    assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
}

/// An unannotated method whose tail is an all-`^` `match:` returns through
/// those `^`s, so it is not `-> Never`: a send to it must not count as
/// diverging and narrow the caller's local.
#[test]
fn all_return_match_tail_does_not_make_method_never() {
    let diags = type_messages(
        r"typed Object subclass: ZzRepro
  kind: v => v match: [1 -> ^#one; _ -> ^#other]

  pick: x :: String | Nil -> Integer =>
    x isNil ifTrue: [self kind: 1]
    self len: x

  len: s :: String -> Integer => s size
",
    );
    assert!(
        diags.iter().any(|m| m.contains("expects String")),
        "`self kind: 1` returns, so `x` must stay `String | Nil`: {diags:?}"
    );
}

/// A mixed-arm tail `match:` still reports a `^` value that disagrees with the
/// declared return type (the `^` arm's type reaches the method's return type).
#[test]
fn mixed_arm_match_tail_still_checks_return_value_against_declared_type() {
    let diags = type_messages(
        r"typed Object subclass: ZzRepro
  count: v :: String | Nil -> Integer =>
    v match: [
      nil -> ^#none;
      x :: String -> x size
    ]
",
    );
    assert!(
        diags
            .iter()
            .any(|m| m.contains("declares return type Integer") && m.contains("#none")),
        "`^#none` must still be checked against `-> Integer`: {diags:?}"
    );
}

/// `x isNil ifTrue: [^a] ifFalse: [^b]` as a method tail must not make the
/// method `-> Never` either.
#[test]
fn all_return_if_true_if_false_tail_does_not_make_method_never() {
    let diags = type_messages(
        r"typed Object subclass: ZzRepro
  kind: v => v isNil ifTrue: [^#one] ifFalse: [^#other]

  pick: x :: String | Nil -> Integer =>
    x isNil ifTrue: [self kind: x]
    self len: x

  len: s :: String -> Integer => s size
",
    );
    assert!(
        diags.iter().any(|m| m.contains("expects String")),
        "`self kind: x` returns, so `x` must stay `String | Nil`: {diags:?}"
    );
}

/// A `^` arm whose value has no recorded type (unresolved `foo`/`bar` infer
/// `Dynamic(Unknown)`, which `type_map` skips) must not leave the tail as
/// `Never`.
#[test]
fn all_return_match_tail_with_untyped_values_does_not_make_method_never() {
    let diags = type_messages(
        r"typed Object subclass: ZzRepro
  kind: v => v match: [1 -> ^foo; _ -> ^bar]

  pick: x :: String | Nil -> Integer =>
    x isNil ifTrue: [self kind: 1]
    self len: x

  len: s :: String -> Integer => s size
",
    );
    assert!(
        diags.iter().any(|m| m.contains("expects String")),
        "`self kind: 1` returns, so `x` must stay `String | Nil`: {diags:?}"
    );
}

// BT-5283: every `^` path is checked against the declared return type, not
// just a `^` arm of a tail `match:`.

#[test]
fn early_return_checked_against_declared_return_type() {
    let diags = type_messages(
        r"typed Object subclass: ZzEarly
  class early: n :: Integer -> Tuple =>
    n < 1 ifTrue: [^#more]
    {#ok, n}
",
    );
    assert!(
        diags
            .iter()
            .any(|m| m.contains("declares return type Tuple") && m.contains("#more")),
        "`^#more` in a `-> Tuple` method must warn: {diags:?}"
    );
}

#[test]
fn early_return_inside_nested_block_checked() {
    let diags = type_messages(
        r"typed Object subclass: ZzEarly
  class pick: n :: Integer -> Tuple =>
    #(1, 2) do: [:x | x > n ifTrue: [^#more]]
    {#ok, n}
",
    );
    assert!(
        diags
            .iter()
            .any(|m| m.contains("declares return type Tuple") && m.contains("#more")),
        "`^` inside a block (non-local return) must warn: {diags:?}"
    );
}

#[test]
fn early_return_of_declared_type_is_clean() {
    let diags = type_messages(
        r"typed Object subclass: ZzEarly
  class early: n :: Integer -> Tuple =>
    n < 1 ifTrue: [^{#none, 0}]
    {#ok, n}
",
    );
    assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
}

#[test]
fn dynamic_early_return_does_not_hide_tail_mismatch() {
    let diags = type_messages(
        r#"typed Object subclass: ZzEarly
  class early: n :: Integer -> Integer =>
    n < 1 ifTrue: [^self foo]
    "oops"
"#,
    );
    assert!(
        diags
            .iter()
            .any(|m| m.contains("declares return type Integer")),
        "a Dynamic early `^` must not hide the String tail: {diags:?}"
    );
}

#[test]
fn nil_return_in_handler_of_integer_method_warns() {
    let diags = type_messages(
        r"typed Object subclass: ZzEarly
  class run -> Integer =>
    [1 + 1] on: Error do: [:e | ^nil]
    2
",
    );
    assert!(
        diags
            .iter()
            .any(|m| m.contains("declares return type Integer")),
        "`^nil` from an `-> Integer` method must warn: {diags:?}"
    );
}

#[test]
fn early_return_in_undeclared_method_is_not_checked() {
    let diags = type_messages(
        r"typed Object subclass: ZzEarly
  class early: n :: Integer =>
    n < 1 ifTrue: [^#more]
    {#ok, n}
",
    );
    assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
}

// BT-5283: a `^` in a block that escapes the method still answers the home
// method, so it is checked against the declared return type (pinned design).
#[test]
fn escaping_block_return_counts_as_method_return() {
    let diags = type_messages(
        r"typed Object subclass: ZzEscape
  class finder: n :: Integer -> Block =>
    [:x :: Integer | x > n ifTrue: [^x]. nil]
",
    );
    assert!(
        diags
            .iter()
            .any(|m| m.contains("declares return type Block") && m.contains("Integer")),
        "`^x` in an escaping block must be checked as a method return: {diags:?}"
    );
}

// BT-5283 review: an early `^` must not hide a tail mismatch in an untyped
// class (the union check only runs for `typed` classes).
#[test]
fn untyped_class_early_return_does_not_hide_tail_mismatch() {
    let diags = type_messages(
        r#"Object subclass: ZzUntyped
  class bar: x -> Integer =>
    x ifTrue: [^nil]
    "oops"
"#,
    );
    assert!(
        diags
            .iter()
            .any(|m| m.contains("declares return type Integer") && m.contains("String")),
        "tail mismatch must still be reported: {diags:?}"
    );
}

#[test]
fn shape_early_return_is_checked() {
    let diags = type_messages(
        r"typed Object subclass: ZzShape
  class bar: x :: Integer -> Integer =>
    x < 1 ifTrue: [^#{#a => 1}]
    x
",
    );
    assert!(
        diags
            .iter()
            .any(|m| m.contains("declares return type Integer")),
        "a shape early `^` must be checked: {diags:?}"
    );
}
