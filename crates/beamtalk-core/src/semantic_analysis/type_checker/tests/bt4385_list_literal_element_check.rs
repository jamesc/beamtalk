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
        r"
type Opt = #a | #b
type Opts = List(Opt)

Object subclass: Taker
  class flags: xs :: List(Opt) -> Integer => 1
  class ints: xs :: List(Integer) -> Integer => 1
  class pairs: xs :: List(Tuple(Symbol, Integer)) -> Integer => 1
  class from: p :: String options: opts :: List(Opt) -> Integer => 1
  class opts: xs :: Opts -> Integer => 1
  class maybe: xs :: List(Opt) | Nil -> Integer => 1
  class classes: xs :: List(Behaviour) -> Integer => 1
  class anyOf: xs :: List(T) -> Integer => 1
  takeInts: xs :: List(Integer) -> Integer => 1

Object subclass: Caller
{body}"
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

/// A generic class's own type param — single-letter or multi-letter — is a
/// placeholder in its signature, so the element check leaves the argument
/// to the whole-argument check, which judges it against the receiver's
/// binding (`Box(Integer)`): one warning, not an `Element` one.
#[test]
fn generic_class_param_element_is_left_to_the_whole_argument_check() {
    let source = r#"
Object subclass: Box(Item)
  addAll: xs :: List(Item) -> Integer => 1

Object subclass: Caller
  run: b :: Box(Integer) => b addAll: #("a")
"#;
    assert_eq!(
        checker_messages(source),
        vec!["Argument 1 of 'addAll:' on Box expects List(Integer), got List(String)".to_string()]
    );
}

#[test]
fn method_local_type_variable_element_is_not_checked() {
    let msgs = checker_messages(&source_with(
        r#"  run => Taker anyOf: #("a", 1)
"#,
    ));
    assert_eq!(msgs, Vec::<String>::new());
}

/// An instance-side send with a uniformly wrong literal is reported once,
/// by element — the whole-argument variance check skips a list literal the
/// element check judges.
#[test]
fn instance_side_send_reports_once() {
    let msgs = checker_messages(&source_with(
        r#"  run: t :: Taker => t takeInts: #("a")
"#,
    ));
    assert_eq!(
        msgs,
        vec![
            "Element 1 of argument 1 of 'takeInts:' on Taker expects Integer, got String"
                .to_string()
        ]
    );
}

#[test]
fn self_send_reports_once() {
    let source = r#"
Object subclass: Holder
  takeInts: xs :: List(Integer) -> Integer => 1
  run => self takeInts: #("a")
"#;
    assert_eq!(
        checker_messages(source),
        vec![
            "Element 1 of argument 1 of 'takeInts:' on Holder expects Integer, got String"
                .to_string()
        ]
    );
}

#[test]
fn cascade_send_is_checked() {
    let msgs = checker_messages(&source_with(
        r#"  run: t :: Taker => t takeInts: #(1); takeInts: #("a")
"#,
    ));
    assert_eq!(
        msgs,
        vec![
            "Element 1 of argument 1 of 'takeInts:' on Taker expects Integer, got String"
                .to_string()
        ]
    );
}

#[test]
fn inherited_method_is_checked() {
    let source = r#"
Object subclass: Base
  takeInts: xs :: List(Integer) -> Integer => 1

Base subclass: Derived

Object subclass: Caller
  run: d :: Derived => d takeInts: #("a")
"#;
    assert_eq!(
        checker_messages(source),
        vec![
            "Element 1 of argument 1 of 'takeInts:' on Derived expects Integer, got String"
                .to_string()
        ]
    );
}

/// A parameter typed through an alias of the whole list type is checked.
#[test]
fn alias_of_list_parameter_is_checked() {
    let msgs = checker_messages(&source_with(
        r"  run => Taker opts: #(#bogus)
",
    ));
    assert_eq!(
        msgs,
        vec![
            "Element 1 of argument 1 of 'opts:' on Taker expects Opt (#a | #b), got #bogus"
                .to_string()
        ]
    );
}

/// An optional list parameter (`List(Opt) | Nil`) checks a literal's
/// elements against its one `List` arm; `nil` itself is fine.
#[test]
fn optional_list_parameter_is_checked() {
    assert_eq!(
        checker_messages(&source_with(
            r"  run => Taker maybe: #(#bogus)
"
        )),
        vec![
            "Element 1 of argument 1 of 'maybe:' on Taker expects Opt (#a | #b), got #bogus"
                .to_string()
        ]
    );
    assert_eq!(
        checker_messages(&source_with(
            r"  run => Taker maybe: nil
"
        )),
        Vec::<String>::new()
    );
}

/// A protocol element type is judged by structural conformance (BT-4482),
/// not nominally: conforming elements pass, a non-conforming one warns.
#[test]
fn protocol_element_type_is_judged_structurally() {
    let source = r#"
Protocol define: Sized
  size -> Integer

Protocol define: Quacker
  quack -> String

Object subclass: Taker
  class sized: xs :: List(Sized) -> Integer => 1
  class quackers: xs :: List(Quacker) -> Integer => 1

Object subclass: Caller
  run => Taker sized: #("abc", #(1))
  bad => Taker quackers: #("abc")
"#;
    let msgs = checker_messages(source);
    assert_eq!(
        msgs.iter()
            .filter(|m| m.starts_with("Element "))
            .collect::<Vec<_>>(),
        vec!["Element 1 of argument 1 of 'quackers:' on Taker expects Quacker, got String"],
        "{msgs:#?}"
    );
}

#[test]
fn class_reference_and_nil_elements_against_behaviour() {
    let msgs = checker_messages(&source_with(
        r"  run => Taker classes: #(Integer, String)
",
    ));
    assert_eq!(msgs, Vec::<String>::new());
    let msgs = checker_messages(&source_with(
        r"  run => Taker classes: #(Integer, 3)
",
    ));
    assert_eq!(
        msgs,
        vec![
            "Element 2 of argument 1 of 'classes:' on Taker expects Behaviour, got Integer"
                .to_string()
        ]
    );
}

/// A cons literal checks its heads and its tail's element type.
#[test]
fn cons_literal_checks_heads_and_tail() {
    let run = |body: &str| checker_messages(&source_with(body));
    assert_eq!(
        run("  run: t :: Taker rest: r :: List(String) => t takeInts: #(1 | r)\n"),
        vec![
            "Each tail element of argument 1 of 'takeInts:' on Taker expects Integer, got String"
                .to_string()
        ]
    );
    assert_eq!(
        run("  run: t :: Taker rest: r :: List(Integer) => t takeInts: #(\"a\" | r)\n"),
        vec![
            "Element 1 of argument 1 of 'takeInts:' on Taker expects Integer, got String"
                .to_string()
        ]
    );
    assert_eq!(
        run("  run: t :: Taker rest: r :: List(Integer) => t takeInts: #(1 | r)\n"),
        Vec::<String>::new()
    );
    assert_eq!(
        run("  run: t :: Taker rest: r => t takeInts: #(1 | r)\n"),
        Vec::<String>::new()
    );
}
