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
