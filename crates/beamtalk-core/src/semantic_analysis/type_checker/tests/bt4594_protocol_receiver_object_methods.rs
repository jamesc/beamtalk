// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4594: a protocol-typed receiver is still an object. A send to it
//! resolves against the protocol's requirements first, then against the
//! methods every object answers (`Object`/`ProtoObject`), taking the return
//! type from whichever method it found. Only a selector that is neither is a
//! does-not-understand.

use super::common::*;
use crate::source_analysis::Severity;

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
    let msgs = messages(&source_with(
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
    let msgs = messages(&source_with(
        r"
  class go: h :: Callable -> Object => h call: 1
",
    ));
    assert!(msgs.is_empty(), "{msgs:#?}");
}

#[test]
fn object_method_return_types_flow_from_a_protocol_receiver() {
    let msgs = messages(&source_with(
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
    let msgs = messages(&source_with(
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
    let msgs = messages(&source_with(
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
    let msgs = messages(
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
    let msgs = messages(&source_with(
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
    let msgs = messages(&source_with(
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
    // missing (not silently skipped), though it caps the severity at `Hint`.
    let msgs = messages(&source_with(
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

/// DNU diagnostics for `source` as `(severity, message)` pairs.
fn dnu_diagnostics(source: &str) -> Vec<(Severity, String)> {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    crate::semantic_analysis::analyse(&module)
        .diagnostics
        .into_iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Dnu))
        .map(|d| (d.severity, d.message.to_string()))
        .collect()
}

#[test]
fn protocol_dnu_is_a_hint_on_bare_and_union_receivers() {
    // A conformer may override `doesNotUnderstand:`, so a protocol's method
    // set is never closed: the DNU stays a `Hint` whether the protocol is the
    // whole receiver type or one member of a union (ADR 0100 Rule 1).
    let diags = dnu_diagnostics(&source_with(
        r"
  class bare: h :: Callable -> Object => h frobnicate
  class union: h :: Callable | Integer -> Object => h frobnicate
",
    ));
    assert_eq!(diags.len(), 2, "{diags:#?}");
    assert!(
        diags.iter().all(|(sev, _)| *sev == Severity::Hint),
        "{diags:#?}"
    );
}

/// A `Wire` protocol with a `class label` requirement and `{body}` appended
/// verbatim as the class methods of a typed `P`.
fn wire_source_with(body: &str) -> String {
    format!(
        r"
Protocol define: Wire
  class label -> String

typed Object subclass: P
{body}"
    )
}

#[test]
fn class_of_a_protocol_receiver_is_the_protocol_metatype() {
    // `w class` is some class conforming to `Wire` (ADR 0144), not
    // `Object class`, so its `class` requirement resolves with its type.
    let msgs = messages(&wire_source_with(
        r"
  class go: w :: Wire -> String => w class label
  class bad: w :: Wire -> Integer => w class label
",
    ));
    assert_eq!(msgs.len(), 1, "{msgs:#?}");
    assert!(msgs[0].contains("String"), "{msgs:#?}");
}

#[test]
fn union_member_protocol_unknown_selector_widens_to_dynamic() {
    // The protocol member is uncertain (a conformer may override
    // `doesNotUnderstand:`), so — like every other uncertain union member —
    // it widens the send's type with `Dynamic` rather than letting
    // `Integer`'s `abs` alone decide it.
    let msgs = messages(&source_with(
        r"
  class a: h :: Callable | Integer -> String => h abs
",
    ));
    // The send was checked (the union DNU hint is present) ...
    assert!(
        msgs.iter()
            .any(|m| m == "Callable does not understand 'abs' (in union Callable | Integer)"),
        "{msgs:#?}"
    );
    // ... and its type is not `Integer`, so the declared `String` return
    // raises no mismatch.
    assert!(
        !msgs.iter().any(|m| m.contains("returns Integer")),
        "{msgs:#?}"
    );
}
