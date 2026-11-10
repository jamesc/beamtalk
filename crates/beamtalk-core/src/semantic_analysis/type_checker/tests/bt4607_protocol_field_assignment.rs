// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4607: a field assignment `self.f := v` and a state default
//! `state: f :: P = v` accept a value whose class structurally conforms to a
//! protocol-typed slot `f :: P` (including `P | Nil` and `P1 & P2`), and still
//! warn for one that does not — by the same rule as an annotated local
//! (BT-4482, `instances_widen` in `inference/assignment.rs`).

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

/// `Sized`/`Quacker` protocols, a `Duck` conformer of both, and `{body}`
/// appended verbatim as the body of `Holder`.
fn source_with(body: &str) -> String {
    format!(
        r#"
Protocol define: Sized
  size -> Integer

Protocol define: Quacker
  quack -> String

Value subclass: Duck
  quack -> String => "quack"
  size -> Integer => 1

Actor subclass: Holder
{body}"#
    )
}

#[test]
fn field_accepts_a_structural_conformer() {
    let msgs = checker_messages(&source_with(
        r#"
  state: q :: Quacker = Duck new
  state: s :: Sized = "abc"
  put => self.q := Duck new
  putSized => self.s := "abc"
"#,
    ));
    assert!(msgs.is_empty(), "{msgs:#?}");
}

#[test]
fn field_rejects_a_non_conformer() {
    let msgs = checker_messages(&source_with(
        r#"
  state: q :: Quacker = Duck new
  put => self.q := "abc"
"#,
    ));
    assert_eq!(
        msgs,
        vec!["Type mismatch: field `q` declared as Quacker, got String".to_string()]
    );
}

#[test]
fn nilable_protocol_field_accepts_conformer_and_nil() {
    let msgs = checker_messages(&source_with(
        r"
  state: q :: Quacker | Nil = nil
  put => self.q := Duck new
  clear => self.q := nil
",
    ));
    assert!(msgs.is_empty(), "{msgs:#?}");
}

#[test]
fn nilable_protocol_field_rejects_a_non_conformer() {
    let msgs = checker_messages(&source_with(
        r#"
  state: q :: Quacker | Nil = nil
  put => self.q := "abc"
"#,
    ));
    assert_eq!(
        msgs,
        vec!["Type mismatch: field `q` declared as Quacker | Nil, got String".to_string()]
    );
}

#[test]
fn union_value_with_a_protocol_field() {
    let msgs = checker_messages(&source_with(
        r#"
  state: q :: Quacker | Nil = nil
  put: flag :: Boolean => self.q := flag ifTrue: [Duck new] ifFalse: [nil]
  bad: flag :: Boolean => self.q := flag ifTrue: [Duck new] ifFalse: ["abc"]
"#,
    ));
    assert_eq!(msgs.len(), 1, "{msgs:#?}");
    assert!(
        msgs[0].starts_with("Type mismatch: field `q` declared as Quacker | Nil, got "),
        "{msgs:#?}"
    );
}

#[test]
fn intersection_field_needs_every_part() {
    let msgs = checker_messages(&source_with(
        r#"
  state: q :: Quacker & Sized = Duck new
  put => self.q := Duck new
  bad => self.q := "abc"
"#,
    ));
    assert_eq!(
        msgs,
        vec!["Type mismatch: field `q` declared as Quacker & Sized, got String".to_string()]
    );
}

#[test]
fn state_default_rejects_a_non_conformer() {
    let msgs = checker_messages(&source_with(
        r#"
  state: q :: Quacker = "abc"
"#,
    ));
    assert_eq!(
        msgs,
        vec!["Type mismatch: state `q` declared as Quacker, default is String".to_string()]
    );
}
