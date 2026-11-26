// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5292: an early `^` that misses the declared return type is reported
//! exactly once, anchored on the `^` expression, and worded as a return.
//! Tail-expression diagnostics keep their `method.span` anchor and "body
//! returns" wording.

use super::common::*;

fn return_diags(source: &str) -> Vec<Diagnostic> {
    type_diags(source)
        .into_iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect()
}

fn text(source: &str, d: &Diagnostic) -> String {
    source[d.span.start() as usize..d.span.end() as usize].to_string()
}

#[test]
fn early_return_matching_union_tail_member_is_reported_once() {
    let source = r#"typed Object subclass: ZzOnce
  class foo: c :: Boolean -> Integer =>
    c ifTrue: [^"early"]
    c ifTrue: [1] ifFalse: ["x"]
"#;
    let diags = return_diags(source);
    assert_eq!(diags.len(), 1, "exactly one report expected: {diags:?}");
}

#[test]
fn early_return_diagnostic_is_anchored_on_the_caret_span() {
    let source = r"typed Object subclass: ZzEarly
  class early: n :: Integer -> Tuple =>
    n < 1 ifTrue: [^#more]
    {#ok, n}
";
    let diags = return_diags(source);
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert_eq!(text(source, &diags[0]), "^#more");
    assert!(
        diags[0].message.starts_with("`^` returns"),
        "early return wording: {}",
        diags[0].message
    );
    assert!(!diags[0].message.contains("body returns"), "{diags:?}");
}

#[test]
fn each_mismatching_caret_is_reported_at_its_own_span() {
    let source = r"typed Object subclass: ZzEarly
  class early: n :: Integer -> Tuple =>
    n < 1 ifTrue: [^#more]
    n < 2 ifTrue: [^#more]
    {#ok, n}
";
    let diags = return_diags(source);
    assert_eq!(diags.len(), 2, "{diags:?}");
    assert_ne!(diags[0].span, diags[1].span);
}

#[test]
fn tail_diagnostic_keeps_method_span_and_body_wording() {
    let source = r#"typed Object subclass: ZzTail
  class foo -> Integer =>
    "oops"
"#;
    let diags = return_diags(source);
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert!(diags[0].message.contains("but body returns"), "{diags:?}");
    assert!(
        text(source, &diags[0]).starts_with("class foo"),
        "{diags:?}"
    );
}
