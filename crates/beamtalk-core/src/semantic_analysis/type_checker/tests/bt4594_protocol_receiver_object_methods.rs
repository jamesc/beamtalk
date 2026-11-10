// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4594: a protocol-typed receiver is still an object. A send to it
//! resolves against the protocol's requirements first, then against the
//! methods every object answers (`Object`/`ProtoObject`), taking the return
//! type from whichever method it found. Only a selector that is neither is a
//! does-not-understand.

use super::common::*;

/// Type and DNU diagnostic messages for `source`, after asserting it parses.
fn checker_messages(source: &str) -> Vec<String> {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    crate::semantic_analysis::analyse(&module)
        .diagnostics
        .into_iter()
        .filter(|d| {
            matches!(
                d.category,
                Some(DiagnosticCategory::Type | DiagnosticCategory::Dnu)
            )
        })
        .map(|d| d.message.to_string())
        .collect()
}

/// A `Callable` protocol and `{body}` appended verbatim as the class
/// methods of a typed `P`.
fn source_with(body: &str) -> String {
    format!(
        r"
Protocol define: Callable
  call: x :: Object -> Object

typed Object subclass: P
{body}"
    )
}

#[test]
fn object_methods_are_not_dnu_on_a_protocol_receiver() {
    let msgs = checker_messages(&source_with(
        r"
  class go: h :: Callable -> Object =>
    (h respondsTo: #call:) ifFalse: [^nil]
    h printString
",
    ));
    assert!(msgs.is_empty(), "{msgs:#?}");
}

#[test]
fn requirement_still_resolves_on_a_protocol_receiver() {
    let msgs = checker_messages(&source_with(
        r"
  class go: h :: Callable -> Object => h call: 1
",
    ));
    assert!(msgs.is_empty(), "{msgs:#?}");
}

#[test]
fn object_method_return_types_flow_from_a_protocol_receiver() {
    let msgs = checker_messages(&source_with(
        r"
  class str: h :: Callable -> Integer => h printString
  class responds: h :: Callable -> Integer => h respondsTo: #call:
",
    ));
    assert_eq!(msgs.len(), 2, "{msgs:#?}");
    assert!(msgs[0].contains("String"), "{msgs:#?}");
    assert!(msgs[1].contains("Boolean"), "{msgs:#?}");
}

#[test]
fn unknown_selector_on_a_protocol_receiver_is_dnu() {
    let msgs = checker_messages(&source_with(
        r"
  class go: h :: Callable -> Object => h frobnicate
",
    ));
    assert!(
        msgs.iter()
            .any(|m| m == "Callable does not understand 'frobnicate'"),
        "{msgs:#?}"
    );
}

#[test]
fn union_with_protocol_after_is_kind_of_narrowing() {
    let msgs = checker_messages(&source_with(
        r"
  class go: h :: Callable | Block(Object, Object) -> Object =>
    (h isKindOf: Block) ifTrue: [^h value: 1]
    (h respondsTo: #call:) ifFalse: [^nil]
    h printString
  class bad: h :: Callable | Block(Object, Object) -> Object =>
    (h isKindOf: Block) ifTrue: [^h value: 1]
    h frobnicate
",
    ));
    assert_eq!(
        msgs,
        vec!["Callable does not understand 'frobnicate'".to_string()],
        "{msgs:#?}"
    );
}

#[test]
fn provided_selector_is_not_dnu_on_a_protocol_receiver() {
    // A protocol's type is required ∪ provided (ADR 0127 §8), so a send of
    // a provided selector to a protocol-typed receiver is not a DNU either.
    let msgs = checker_messages(
        r#"
Protocol define: Labelled
  label -> String
  announce -> String => "announce: " ++ self label

typed Object subclass: P
  class go: l :: Labelled -> Object => l announce
"#,
    );
    assert!(msgs.is_empty(), "{msgs:#?}");
}

#[test]
fn union_member_protocol_object_method_return_type() {
    // The receiver is still a union at the send, so this goes through the
    // per-member union loop: the `Callable` member answers `printString`
    // through `Object` (`String`), as does `Integer`.
    let msgs = checker_messages(&source_with(
        r"
  class s: h :: Callable | Integer -> Integer => h printString
",
    ));
    assert_eq!(msgs.len(), 1, "{msgs:#?}");
    assert!(msgs[0].contains("String"), "{msgs:#?}");
}

#[test]
fn union_member_protocol_selector_responds() {
    // `call:` is `Callable`'s own selector: that member responds, so only
    // `Integer` is reported missing.
    let msgs = checker_messages(&source_with(
        r"
  class c: h :: Callable | Integer -> Object => h call: 1
",
    ));
    assert_eq!(
        msgs,
        vec!["Integer does not understand 'call:' (in union Callable | Integer)".to_string()],
        "{msgs:#?}"
    );
}

#[test]
fn union_member_protocol_unknown_selector_is_missing() {
    // Neither member answers `frobnicate`: the protocol member is reported
    // missing like any closed class, not skipped as uncertain.
    let msgs = checker_messages(&source_with(
        r"
  class f: h :: Callable | Integer -> Object => h frobnicate
",
    ));
    assert_eq!(msgs.len(), 1, "{msgs:#?}");
    assert!(
        msgs[0].contains("Callable") && msgs[0].contains("Integer"),
        "{msgs:#?}"
    );
}
