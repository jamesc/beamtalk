// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4385: a list literal `#(...)` passed to a parameter declared
//! `List(E)` checks each element against `E` — symbol-union aliases
//! (`List(RegexOption)`), positional tuples (`List(Tuple(...))`) and plain
//! element types alike. Non-literal arguments keep the nominal `List` check.

use super::common::*;

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

/// `Opt` mirrors `RegexOption`'s shape (a symbol-union alias); `Taker`
/// declares one parameter per element shape the issue names.
fn source_with(body: &str) -> String {
    format!(
        r#"
type Opt = #a | #b

Object subclass: Taker
  class flags: xs :: List(Opt) -> Integer => 1
  class ints: xs :: List(Integer) -> Integer => 1
  class pairs: xs :: List(Tuple(Symbol, Integer)) -> Integer => 1
  class from: p :: String options: opts :: List(Opt) -> Integer => 1

Object subclass: Caller
{body}"#
    )
}

#[test]
fn symbol_alias_rejects_symbol_outside_union() {
    let msgs = checker_messages(&source_with("  run => Taker flags: #(#bogus)\n"));
    assert_eq!(
        msgs,
        vec![
            "Element 1 of argument 1 of 'flags:' on Taker expects Opt (#a | #b), got #bogus"
                .to_string()
        ]
    );
}

#[test]
fn symbol_alias_rejects_non_symbol_element() {
    let msgs = checker_messages(&source_with("  run => Taker flags: #(#a, 3)\n"));
    assert_eq!(
        msgs,
        vec![
            "Element 2 of argument 1 of 'flags:' on Taker expects Opt (#a | #b), got Integer"
                .to_string()
        ]
    );
}

#[test]
fn symbol_alias_accepts_union_members() {
    let msgs = checker_messages(&source_with("  run => Taker flags: #(#a, #b)\n"));
    assert_eq!(msgs, Vec::<String>::new());
}

#[test]
fn keyword_send_checks_the_matching_argument() {
    let msgs = checker_messages(&source_with(
        r#"  run => Taker from: "x" options: #(#bogus)
"#,
    ));
    assert_eq!(
        msgs,
        vec![
            "Element 1 of argument 2 of 'from:options:' on Taker expects Opt (#a | #b), got #bogus"
                .to_string()
        ]
    );
}

#[test]
fn plain_element_type_rejects_mismatch() {
    let msgs = checker_messages(&source_with(
        r#"  run => Taker ints: #("a")
"#,
    ));
    assert_eq!(
        msgs,
        vec!["Element 1 of argument 1 of 'ints:' on Taker expects Integer, got String".to_string()]
    );
}

#[test]
fn plain_element_type_accepts_matching_elements() {
    let msgs = checker_messages(&source_with("  run => Taker ints: #(1, 2)\n"));
    assert_eq!(msgs, Vec::<String>::new());
}

#[test]
fn tuple_element_type_rejects_mismatch() {
    let msgs = checker_messages(&source_with(
        r#"  run => Taker pairs: #({#a, "x"})
"#,
    ));
    assert_eq!(msgs, vec!["Element 1 of argument 1 of 'pairs:' on Taker expects Tuple(Symbol, Integer), got Tuple(#a, String)".to_string()]);
}

#[test]
fn tuple_element_type_accepts_matching_tuple() {
    let msgs = checker_messages(&source_with("  run => Taker pairs: #({#a, 1})\n"));
    assert_eq!(msgs, Vec::<String>::new());
}

#[test]
fn empty_list_is_accepted() {
    let msgs = checker_messages(&source_with("  run => Taker ints: #()\n"));
    assert_eq!(msgs, Vec::<String>::new());
}

#[test]
fn parenthesised_literal_is_checked() {
    let msgs = checker_messages(&source_with(
        r#"  run => Taker ints: (#(1, "a"))
"#,
    ));
    assert_eq!(
        msgs,
        vec!["Element 2 of argument 1 of 'ints:' on Taker expects Integer, got String".to_string()]
    );
}

#[test]
fn every_mismatched_element_is_reported() {
    let msgs = checker_messages(&source_with(
        r#"  run => Taker ints: #("a", 1, #b)
"#,
    ));
    assert_eq!(
        msgs,
        vec![
            "Element 1 of argument 1 of 'ints:' on Taker expects Integer, got String".to_string(),
            "Element 3 of argument 1 of 'ints:' on Taker expects Integer, got #b".to_string()
        ]
    );
}

#[test]
fn symbol_typed_element_is_accepted_as_a_possible_member() {
    let msgs = checker_messages(&source_with("  run: s :: Symbol => Taker flags: #(s)\n"));
    assert_eq!(msgs, Vec::<String>::new());
}

#[test]
fn dynamic_element_is_not_checked() {
    let msgs = checker_messages(&source_with("  run: s => Taker ints: #(s)\n"));
    assert_eq!(msgs, Vec::<String>::new());
}

#[test]
fn non_literal_argument_keeps_nominal_check() {
    let msgs = checker_messages(&source_with(
        "  run: xs :: List(Symbol) => Taker flags: xs\n",
    ));
    assert_eq!(msgs, Vec::<String>::new());
}

/// A parameter typed by the receiver's own type param (`List>>++`'s
/// `List(E)`) is not an element check site: the declared element type is a
/// placeholder, not a signature constraint.
#[test]
fn class_type_param_element_is_not_checked() {
    let msgs = checker_messages(&source_with(
        r#"  run => #(1, 2) ++ #("a")
"#,
    ));
    assert!(!msgs.iter().any(|m| m.starts_with("Element ")), "{msgs:#?}");
}
