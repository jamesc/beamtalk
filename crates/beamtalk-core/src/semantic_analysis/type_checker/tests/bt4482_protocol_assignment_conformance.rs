// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4482: an annotated local `x :: P := v`, a block parameter
//! `[:x :: P | …] value: v` and a `List(P)` literal element accept a value
//! whose class structurally conforms to protocol `P`, and still warn for one
//! that does not — the shared `detect_assignment_type_mismatch` rule checks
//! protocol conformance rather than only walking superclass chains.

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

/// `Sized`/`Quacker`/`LoudQuacker` protocols, a `Duck` conformer of
/// `Quacker`, a `Taker` with a `List(Quacker)` parameter, and `{body}`
/// appended verbatim as the methods of `Caller`.
fn source_with(body: &str) -> String {
    format!(
        r#"
Protocol define: Sized
  size -> Integer

Protocol define: Quacker
  quack -> String

Protocol define: LoudQuacker
  quack -> String
  shout -> String

Value subclass: Duck
  quack -> String => "quack"
  size -> Integer => 1

Object subclass: Taker
  class quackers: xs :: List(Quacker) -> Integer => 1

Object subclass: Caller
{body}"#
    )
}

#[test]
fn local_accepts_a_structural_conformer() {
    let msgs = checker_messages(&source_with(
        r#"
  run => x :: Sized := "abc". x
  duck => q :: Quacker := Duck new. q
"#,
    ));
    assert!(msgs.is_empty(), "{msgs:#?}");
}

#[test]
fn local_rejects_a_non_conformer() {
    let msgs = checker_messages(&source_with(
        r#"
  run => q :: Quacker := "abc". q
"#,
    ));
    assert_eq!(
        msgs,
        vec!["Type mismatch: declared as Quacker, got String".to_string()]
    );
}

#[test]
fn local_union_with_a_protocol_member() {
    let msgs = checker_messages(&source_with(
        r"
  run => q :: Quacker | Nil := Duck new. q
  none => q :: Quacker | Nil := nil. q
",
    ));
    assert!(msgs.is_empty(), "{msgs:#?}");
    let msgs = checker_messages(&source_with(
        r#"
  run => q :: Quacker | Nil := "abc". q
"#,
    ));
    assert_eq!(
        msgs,
        vec!["Type mismatch: declared as Quacker | Nil, got String".to_string()]
    );
}

#[test]
fn union_value_needs_every_member_to_conform() {
    let msgs = checker_messages(&source_with(
        r#"
  run: flag :: Boolean =>
    q :: Sized := flag ifTrue: [Duck new] ifFalse: ["abc"]
    q
"#,
    ));
    assert!(msgs.is_empty(), "{msgs:#?}");
    let msgs = checker_messages(&source_with(
        r#"
  run: flag :: Boolean =>
    q :: Quacker := flag ifTrue: [Duck new] ifFalse: ["abc"]
    q
"#,
    ));
    assert_eq!(
        msgs,
        vec!["Type mismatch: declared as Quacker, got Duck | String".to_string()]
    );
}

#[test]
fn intersection_needs_every_protocol() {
    let msgs = checker_messages(&source_with(
        r"
  run => q :: Quacker & Sized := Duck new. q
",
    ));
    assert!(msgs.is_empty(), "{msgs:#?}");
    let msgs = checker_messages(&source_with(
        r#"
  run => q :: Quacker & Sized := "abc". q
"#,
    ));
    assert_eq!(msgs.len(), 1, "{msgs:#?}");
    assert!(
        msgs[0].starts_with("Type mismatch: declared as "),
        "{msgs:#?}"
    );
}

#[test]
fn protocol_typed_value_fits_a_protocol_it_subsumes() {
    let msgs = checker_messages(&source_with(
        r"
  run: loud :: LoudQuacker => q :: Quacker := loud. q
",
    ));
    assert!(msgs.is_empty(), "{msgs:#?}");
}

#[test]
fn block_parameter_accepts_a_conformer_and_rejects_a_non_conformer() {
    let msgs = checker_messages(&source_with(
        r"
  run => [:q :: Quacker | q quack] value: Duck new
",
    ));
    assert!(msgs.is_empty(), "{msgs:#?}");
    let msgs = checker_messages(&source_with(
        r#"
  run => [:q :: Quacker | q] value: "abc"
"#,
    ));
    assert_eq!(
        msgs,
        vec!["Type mismatch: declared as Quacker, got String".to_string()]
    );
}

#[test]
fn list_literal_elements_are_checked_against_a_protocol() {
    let msgs = checker_messages(&source_with(
        r"
  run => Taker quackers: #(Duck new)
",
    ));
    assert!(msgs.is_empty(), "{msgs:#?}");
    let msgs = checker_messages(&source_with(
        r#"
  run => Taker quackers: #(Duck new, "abc")
"#,
    ));
    assert!(
        msgs.contains(
            &"Element 2 of argument 1 of 'quackers:' on Taker expects Quacker, got String"
                .to_string()
        ),
        "{msgs:#?}"
    );
}

/// A singleton is a `Symbol`, not an unknown class the open-world
/// conformance gate waves through.
#[test]
fn singleton_is_judged_as_a_symbol() {
    let msgs = checker_messages(&source_with(
        r"
  run => q :: Quacker := #foo. q
",
    ));
    assert_eq!(msgs.len(), 1, "{msgs:#?}");
    assert!(
        msgs[0].starts_with("Type mismatch: declared as Quacker, got "),
        "{msgs:#?}"
    );
}

/// A protocol-typed value is some conformer: it fits `Object`, and no
/// narrower class — not even `Protocol`.
#[test]
fn protocol_typed_value_fits_object_but_not_protocol() {
    let msgs = checker_messages(&source_with(
        r"
  run: q :: Quacker => o :: Object := q. o
",
    ));
    assert!(msgs.is_empty(), "{msgs:#?}");
    let msgs = checker_messages(&source_with(
        r"
  run: q :: Quacker => p :: Protocol := q. p
",
    ));
    assert_eq!(
        msgs,
        vec!["Type mismatch: declared as Protocol, got Quacker".to_string()]
    );
}
