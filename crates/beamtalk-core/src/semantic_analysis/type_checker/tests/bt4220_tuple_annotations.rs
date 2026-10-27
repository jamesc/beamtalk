// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4220: user-written `:: Tuple(T1, …, Tn)` annotations.
//!
//! The checker already infers positional tuple types (tuple literals, the FFI
//! spec importer); these tests pin that the same type is also accepted as a
//! written annotation — in every annotation position, at any arity — and
//! behaves exactly like the inferred one (literal-index `at:` typing,
//! assignability against bare `Tuple` and between positional tuples).

use crate::source_analysis::Diagnostic;

/// Parses `source` and runs the full semantic-analysis pipeline, returning
/// every diagnostic (so the Phase 2d generic-arity check runs too).
fn analyse_diagnostics(source: &str) -> Vec<Diagnostic> {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    crate::semantic_analysis::analyse(&module).diagnostics
}

fn mismatches(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| {
            d.message.contains("Type mismatch")
                || d.message.contains("type mismatch")
                || d.message.contains("but body returns")
        })
        .collect()
}

#[test]
fn bt4220_tuple_annotation_accepted_in_every_position() {
    let diags = analyse_diagnostics(
        r#"
typed Actor subclass: Holder
  state: pair :: Tuple(Symbol, Integer) = {#a, 1}

  marker -> Tuple(Symbol, Integer | Nil) => {#marker, nil}

  take: p :: Tuple(Symbol, Integer) -> Integer => p at: 2

  local -> Integer =>
    q :: Tuple(Symbol, Integer) := {#b, 2}
    q at: 2

  triple -> Tuple(Symbol, Integer, String) => {#c, 3, "x"}
"#,
    );
    assert!(diags.is_empty(), "got: {diags:#?}");
}

#[test]
fn bt4220_tuple_annotation_no_type_parameters_warning() {
    let diags = analyse_diagnostics(
        r"
Object subclass: Holder
  marker -> Tuple(Symbol) => {#marker}
",
    );
    assert!(
        !diags
            .iter()
            .any(|d| d.message.contains("no type parameters")),
        "got: {diags:#?}"
    );
}

#[test]
fn bt4220_bare_tuple_annotation_unchanged() {
    let diags = analyse_diagnostics(
        r"
typed Object subclass: Holder
  marker -> Tuple => {#marker, 1}
  take: p :: Tuple -> Integer => p size
",
    );
    assert!(diags.is_empty(), "got: {diags:#?}");
}

#[test]
fn bt4220_annotated_param_literal_index_at_is_typed() {
    // `p at: 1` is `Symbol`, so returning it as `String` is a mismatch —
    // the same as on an inferred `Tuple(Symbol, Integer)`.
    let diags = analyse_diagnostics(
        r"
typed Object subclass: Holder
  take: p :: Tuple(Symbol, Integer) -> String => p at: 1
",
    );
    assert_eq!(mismatches(&diags).len(), 1, "got: {diags:#?}");
}

#[test]
fn bt4220_annotated_local_literal_index_at_is_typed() {
    let diags = analyse_diagnostics(
        r"
typed Object subclass: Holder
  run =>
    p :: Tuple(Symbol, Integer) := {#a, 1}
    s :: String := p at: 2
    s
",
    );
    assert_eq!(mismatches(&diags).len(), 1, "got: {diags:#?}");
}

#[test]
fn bt4220_out_of_range_or_non_literal_index_is_dynamic() {
    let diags = analyse_diagnostics(
        r"
typed Object subclass: Holder
  take: p :: Tuple(Symbol, Integer) at: i :: Integer -> String =>
    a :: String := p at: 3
    b :: String := p at: i
    b
",
    );
    assert!(mismatches(&diags).is_empty(), "got: {diags:#?}");
}

#[test]
fn bt4220_positional_tuple_flows_into_bare_tuple() {
    let diags = analyse_diagnostics(
        r"
typed Object subclass: Holder
  take: p :: Tuple -> Integer => p size
  run: q :: Tuple(Symbol, Integer) -> Integer => self take: q
  widen: q :: Tuple(Symbol, Integer) -> Tuple => q
",
    );
    assert!(diags.is_empty(), "got: {diags:#?}");
}

#[test]
fn bt4220_bare_tuple_flows_into_positional_tuple() {
    // Gradual: an unparameterised `Tuple` (and `Dynamic`) flows into a
    // positional tuple annotation without a warning.
    let diags = analyse_diagnostics(
        r"
typed Object subclass: Holder
  take: p :: Tuple(Symbol, Integer) -> Integer => p at: 2
  run: q :: Tuple -> Integer => self take: q
  narrow: q :: Tuple -> Tuple(Symbol, Integer) => q
  dyn: q :: Dynamic -> Tuple(Symbol, Integer) => q
",
    );
    assert!(diags.is_empty(), "got: {diags:#?}");
}

#[test]
fn bt4220_arity_mismatch_return_is_type_error() {
    let diags = analyse_diagnostics(
        r"
typed Object subclass: Holder
  marker -> Tuple(Symbol, Integer) => {#a, 1, 2}
",
    );
    assert_eq!(mismatches(&diags).len(), 1, "got: {diags:#?}");
}

#[test]
fn bt4220_arity_mismatch_argument_is_type_error() {
    let diags = analyse_diagnostics(
        r"
typed Object subclass: Holder
  take: p :: Tuple(Symbol, Integer) -> Integer => p at: 2
  run: q :: Tuple(Symbol, Integer, Integer) -> Integer => self take: q
",
    );
    assert!(
        !diags.is_empty(),
        "arity mismatch should be diagnosed, got none"
    );
}

#[test]
fn bt4220_arity_mismatch_local_is_type_error() {
    let diags = analyse_diagnostics(
        r"
typed Object subclass: Holder
  run -> Tuple(Symbol) =>
    p :: Tuple(Symbol) := {#a, 1}
    p
",
    );
    assert_eq!(mismatches(&diags).len(), 1, "got: {diags:#?}");
}

#[test]
fn bt4220_element_type_mismatch_is_type_error() {
    let diags = analyse_diagnostics(
        r#"
typed Object subclass: Holder
  marker -> Tuple(Symbol, Integer) => {#a, "x"}
"#,
    );
    assert_eq!(mismatches(&diags).len(), 1, "got: {diags:#?}");
}

#[test]
fn bt4220_zero_arity_tuple_annotation_accepted() {
    // `Tuple()` is the arity-0 positional tuple; like the `{}` literal it
    // carries no element types, so it behaves as bare `Tuple`.
    let diags = analyse_diagnostics(
        r"
typed Object subclass: Holder
  empty -> Tuple() => {}
  widen: p :: Tuple() -> Tuple => p
",
    );
    assert!(diags.is_empty(), "got: {diags:#?}");
}

#[test]
fn bt4220_nested_positional_tuple_in_list_annotation() {
    // The Catalina `getstat` shape: `List(Tuple(Symbol, Integer))` — the
    // element's literal-index `at:` is typed through the block parameter.
    let diags = analyse_diagnostics(
        r"
typed Object subclass: Holder
  stats: s :: List(Tuple(Symbol, Integer)) -> Integer =>
    total := 0
    s do: [:entry | total := total + (entry at: 2)]
    total
  bad: s :: List(Tuple(Symbol, Integer)) -> Integer =>
    s do: [:entry | (entry at: 1) + 1]
    0
",
    );
    let dnu: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert_eq!(
        dnu.len(),
        1,
        "expected `#sym + 1` DNU only, got: {diags:#?}"
    );
    assert_eq!(diags.len(), 1, "got: {diags:#?}");
}
