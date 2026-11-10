// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4487: `{a, b} := value` types plain-variable elements from a positional
//! `Tuple(A, B)` right-hand side, like `value at: i` does. `Object`/`Dynamic`
//! positions stay `Dynamic`; anything not a same-arity tuple falls back to
//! `Dynamic` bindings.
//!
//! Each probe returns a binding as `String`: a typed binding that is not a
//! `String` yields exactly one "declares return type" mismatch, a `Dynamic`
//! binding yields none.

use crate::source_analysis::Diagnostic;

fn analyse_diagnostics(source: &str) -> Vec<Diagnostic> {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    crate::semantic_analysis::analyse(&module).diagnostics
}

fn mismatches(diags: &[Diagnostic]) -> usize {
    diags
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .count()
}

/// Probe: destructure a `p` of type `ty` with `pattern`, return `ret` as String.
fn probe(ty: &str, pattern: &str, ret: &str) -> usize {
    let diags = analyse_diagnostics(&format!(
        "typed Object subclass: Holder\n  run: p :: {ty} -> String =>\n    {pattern} := p\n    {ret}\n"
    ));
    mismatches(&diags)
}

#[test]
fn each_position_is_typed() {
    assert_eq!(probe("Tuple(Integer, String)", "{a, b}", "a"), 1);
    assert_eq!(probe("Tuple(Integer, String)", "{a, b}", "b"), 0);
}

#[test]
fn object_position_stays_dynamic() {
    assert_eq!(probe("Tuple(Object, Integer)", "{a, b}", "a"), 0);
    assert_eq!(probe("Tuple(Object, Integer)", "{a, b}", "b"), 1);
}

#[test]
fn arity_mismatch_binds_dynamic() {
    assert_eq!(probe("Tuple(Integer, String)", "{a, b, c}", "a"), 0);
}

#[test]
fn bare_tuple_binds_dynamic() {
    assert_eq!(probe("Tuple", "{a, b}", "a"), 0);
}

#[test]
fn nil_is_dropped_from_union() {
    assert_eq!(probe("Tuple(Integer, String) | Nil", "{a, b}", "a"), 1);
}

#[test]
fn union_of_same_arity_tuples_is_per_position() {
    // Position 1 is `Integer | Symbol`: not a String -> mismatch.
    assert_eq!(
        probe("Tuple(Integer, String) | Tuple(Symbol, String)", "{a, b}", "a"),
        1
    );
    // Position 2 is String in both -> no mismatch.
    assert_eq!(
        probe("Tuple(Integer, String) | Tuple(Symbol, String)", "{a, b}", "b"),
        0
    );
}

#[test]
fn mixed_union_binds_dynamic() {
    assert_eq!(probe("Tuple(Integer, String) | Integer", "{a, b}", "a"), 0);
    assert_eq!(
        probe("Tuple(Integer, String) | Tuple(Integer)", "{a, b}", "a"),
        0
    );
}

#[test]
fn wildcard_and_literal_elements_bind_nothing() {
    assert_eq!(probe("Tuple(Integer, String)", "{_, b}", "b"), 0);
    assert_eq!(probe("Tuple(Integer, String)", "{#ok, b}", "b"), 0);
}

#[test]
fn send_to_binding_gets_typed_warning() {
    let diags = analyse_diagnostics(
        "typed Object subclass: Holder\n  run: p :: Tuple(Integer, String) -> Object =>\n    {a, b} := p\n    a fooBarBaz\n",
    );
    assert!(
        diags.iter().any(|d| d.message.contains("fooBarBaz")),
        "expected DNU warning on typed binding, got: {diags:#?}"
    );
}

#[test]
fn port_shape_warns() {
    // BT-4487: `{_ip, port} := ...; port` declared `-> Integer` while the
    // position is `Integer | Binary`.
    let diags = analyse_diagnostics(
        "typed Object subclass: Holder\n  port: addr :: Tuple(Symbol, Integer | Binary) -> Integer =>\n    {_ip, port} := addr\n    port\n",
    );
    assert_eq!(mismatches(&diags), 1, "got: {diags:#?}");
}
