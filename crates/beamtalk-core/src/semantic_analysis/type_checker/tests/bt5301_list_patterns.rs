// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5301: list patterns (`#()`, `#(a, b)`, `#(h | t)`) in `match:` arms
//! and destructuring. Over a `List(T)` value an element binds `T` and a tail
//! binds `List(T)`; `matchExhaustive:` proves `#()` plus `#(h | t)`
//! exhaustive for `List` and names any missing shape.

use super::common::analyse_diags;
use crate::source_analysis::{Diagnostic, Severity};

fn errors(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect()
}

fn warnings(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.severity == Severity::Warning)
        .collect()
}

fn mismatches(diags: &[Diagnostic]) -> usize {
    diags
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .count()
}

/// A typed class whose one method is `sel: xs :: List(Integer) -> Ret => body`.
fn method(ret: &str, body: &str) -> Vec<Diagnostic> {
    analyse_diags(&format!(
        "typed Object subclass: Demo\n  class run: xs :: List(Integer) -> {ret} =>\n    {body}\n"
    ))
}

#[test]
fn empty_plus_cons_is_exhaustive() {
    let diags = method(
        "Integer",
        "xs matchExhaustive: [#() -> 0; #(h | t) -> h + t size]",
    );
    assert!(errors(&diags).is_empty(), "{diags:?}");
    assert!(warnings(&diags).is_empty(), "{diags:?}");
}

#[test]
fn fixed_lengths_below_an_open_arm_are_exhaustive() {
    // The `pairUp:acc:` shape: `#()`, `#(a)`, `#(a, b | rest)`.
    let diags = method(
        "Integer",
        "xs matchExhaustive: [#() -> 0; #(a) -> a; #(a, b | rest) -> a + b + rest size]",
    );
    assert!(errors(&diags).is_empty(), "{diags:?}");
}

#[test]
fn nested_cons_tail_counts_as_flattened() {
    let diags = method(
        "Integer",
        "xs matchExhaustive: [#() -> 0; #(a) -> a; #(a | #(b | t)) -> a + b + t size]",
    );
    assert!(errors(&diags).is_empty(), "{diags:?}");
}

#[test]
fn cons_alone_is_missing_empty() {
    let diags = method("Integer", "xs matchExhaustive: [#(h | t) -> h]");
    let errs = errors(&diags);
    assert_eq!(errs.len(), 1, "{diags:?}");
    assert!(errs[0].message.contains("`#()`"), "{}", errs[0].message);
}

#[test]
fn fixed_lengths_only_are_missing_longer_lists() {
    let diags = method("Integer", "xs matchExhaustive: [#() -> 0; #(a) -> a]");
    let errs = errors(&diags);
    assert_eq!(errs.len(), 1, "{diags:?}");
    assert!(
        errs[0].message.contains("`#(_, _ | _)`"),
        "{}",
        errs[0].message
    );
}

#[test]
fn gap_below_open_arm_is_named() {
    let diags = method(
        "Integer",
        "xs matchExhaustive: [#() -> 0; #(a, b | t) -> a + b]",
    );
    let errs = errors(&diags);
    assert_eq!(errs.len(), 1, "{diags:?}");
    assert!(errs[0].message.contains("`#(_)`"), "{}", errs[0].message);
}

#[test]
fn refutable_or_guarded_arms_do_not_cover() {
    // A literal element, a nested type test and a guard can each miss.
    for arms in [
        "#() -> 0; #(1 | t) -> 1",
        "#() -> 0; #(n :: Integer | t) -> n",
        "#() -> 0; #(h | t) when: [h > 0] -> h",
    ] {
        let diags = method("Integer", &format!("xs matchExhaustive: [{arms}]"));
        assert_eq!(errors(&diags).len(), 1, "{arms}: {diags:?}");
    }
}

#[test]
fn arm_after_full_coverage_is_unreachable() {
    let diags = method(
        "Integer",
        "xs matchExhaustive: [#() -> 0; #(h | t) -> h; #(a) -> a]",
    );
    assert!(errors(&diags).is_empty(), "{diags:?}");
    assert!(
        warnings(&diags)
            .iter()
            .any(|d| d.message.contains("unreachable arm")),
        "{diags:?}"
    );
}

#[test]
fn match_arm_element_and_tail_are_typed_from_list() {
    // `h` is an Integer and `t` a List(Integer): returning either as a
    // String is a mismatch.
    let head = method("String", "xs match: [#(h | _) -> h; _ -> \"\"]");
    assert_eq!(mismatches(&head), 1, "{head:?}");
    let tail = method("String", "xs match: [#(_ | t) -> t; _ -> \"\"]");
    assert_eq!(mismatches(&tail), 1, "{tail:?}");
    let ok = method("Integer", "xs match: [#(h | t) -> h + t size; _ -> 0]");
    assert!(errors(&ok).is_empty() && mismatches(&ok) == 0, "{ok:?}");
}

#[test]
fn cons_destructure_types_element_and_tail() {
    let head = method("String", "#(h | _) := xs\n    h");
    assert_eq!(mismatches(&head), 1, "{head:?}");
    let nested = method("String", "#(_ | #(b | _)) := xs\n    b");
    assert_eq!(mismatches(&nested), 1, "{nested:?}");
}

#[test]
fn nested_type_pattern_in_list_narrows_and_annotates() {
    // BT-5294 / BT-5332: a nested `x :: T` in a match-arm list pattern
    // narrows to `T`; in a destructure it declares the local's type.
    let arm = analyse_diags(
        "typed Object subclass: Demo\n  class run: xs :: List -> String =>\n    xs match: [#(n :: Integer | _) -> n; _ -> \"\"]\n",
    );
    assert_eq!(mismatches(&arm), 1, "{arm:?}");
    let destructure = analyse_diags(
        "typed Object subclass: Demo\n  class run: xs :: List -> String =>\n    #(n :: Integer | _) := xs\n    n\n",
    );
    assert_eq!(mismatches(&destructure), 1, "{destructure:?}");
}
