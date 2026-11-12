// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4444: a `value`-family send directly to a block literal checks each
//! argument against the parameter's `:: T` annotation by the same rule as an
//! annotated local — including `C class` and `P class` metatypes (ADR 0083,
//! ADR 0144).

use super::common::*;

/// A `Wire` protocol with one class-side requirement, a conformer `Tcp`,
/// and `{body}` appended verbatim.
fn source_with(body: &str) -> String {
    format!(
        r#"
Protocol define: Wire
  class label -> String

sealed Object subclass: Tcp
  class label -> String => "tcp"

Object subclass: Caller
{body}"#
    )
}

#[test]
fn ordinary_type_accepts_matching_argument() {
    let msgs = messages(&source_with(
        r"
  run -> Integer => [:x :: Integer | x + 1] value: 3
",
    ));
    assert!(msgs.is_empty(), "{msgs:#?}");
}

#[test]
fn ordinary_type_rejects_mismatched_argument() {
    let msgs = messages(&source_with(
        r#"
  run => [:x :: Integer | x] value: "s"
"#,
    ));
    assert!(
        msgs.contains(&"Type mismatch: declared as Integer, got String".to_string()),
        "{msgs:#?}"
    );
}

#[test]
fn every_annotated_parameter_is_checked_and_unannotated_ones_are_not() {
    let msgs = messages(&source_with(
        r#"
  run => [:a :b :: Integer :c :: String | a] value: "x" value: "y" value: 3
"#,
    ));
    assert_eq!(
        msgs,
        vec![
            "Type mismatch: declared as Integer, got String".to_string(),
            "Type mismatch: declared as String, got Integer".to_string(),
        ],
    );
}

#[test]
fn parenthesised_block_literal_is_checked() {
    let msgs = messages(&source_with(
        r#"
  run => ([:x :: Integer | x]) value: "s"
"#,
    ));
    assert!(
        msgs.contains(&"Type mismatch: declared as Integer, got String".to_string()),
        "{msgs:#?}"
    );
}

#[test]
fn class_metatype_accepts_the_class_and_subclasses() {
    let msgs = messages(&source_with(
        r"
  run =>
    [:c :: Integer class | c] value: Integer
    [:c :: Number class | c] value: Integer
",
    ));
    assert!(msgs.is_empty(), "{msgs:#?}");
}

#[test]
fn class_metatype_rejects_an_unrelated_class() {
    let msgs = messages(&source_with(
        r"
  run => [:c :: Integer class | c] value: String
",
    ));
    assert!(
        msgs.contains(&"Type mismatch: declared as Integer class, got String class".to_string()),
        "{msgs:#?}"
    );
}

#[test]
fn protocol_metatype_accepts_a_conformer() {
    let msgs = messages(&source_with(
        r"
  run => [:w :: Wire class | w label] value: Tcp
",
    ));
    assert!(msgs.is_empty(), "{msgs:#?}");
}

#[test]
fn protocol_metatype_rejects_a_non_conformer_and_the_protocol_itself() {
    let msgs = messages(&source_with(
        r"
  run =>
    [:w :: Wire class | w] value: String
    [:w :: Wire class | w] value: Wire
",
    ));
    assert!(
        msgs.contains(&"String class does not conform to Wire class".to_string()),
        "non-conformer: {msgs:#?}"
    );
    assert_eq!(
        msgs.len(),
        2,
        "the protocol literal is refused too: {msgs:#?}"
    );
}
