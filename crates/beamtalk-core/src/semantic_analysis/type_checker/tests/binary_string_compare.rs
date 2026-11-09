// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4257: the `binary-string-compare` hint.
//!
//! A `String` and a raw `Binary` holding the same bytes are never equal, so
//! `==` answers `false` and a String-keyed `Dictionary` / String `Set` never
//! finds the Binary. The hint fires only when the Binary side is provably a
//! raw Binary (a Binary-only producer, or a local last assigned from one) and
//! the other side is statically `String`. A declared `Binary` may hold a
//! String (ADR 0086), and `Dynamic` is never evidence (ADR 0100 Rule 1).

use super::common::*;
use crate::compilation::diagnostics_policy::{
    apply_diagnostics_table, apply_expect_directives, parse_diagnostics_table_from_manifest_toml,
};
use crate::source_analysis::Severity;

fn diagnostics(source: &str) -> Vec<Diagnostic> {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let mut diags = checker.take_diagnostics();
    apply_expect_directives(&module, &mut diags);
    diags
}

fn compare_hints(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::BinaryStringCompare))
        .collect()
}

/// A class whose `run:` method takes `s :: String` and runs `body`.
fn probe(body: &str) -> String {
    format!("Object subclass: Probe\n  run: s :: String =>\n    {body}\n")
}

/// A probe whose `run:` takes `param` (e.g. `d :: Dictionary(String, Integer)`).
fn probe_with(param: &str, body: &str) -> String {
    format!("Object subclass: Probe\n  run: {param} =>\n    {body}\n")
}

fn assert_one_hint(source: &str) {
    let diags = diagnostics(source);
    let hints = compare_hints(&diags);
    assert_eq!(
        hints.len(),
        1,
        "expected one hint for {source:?}: {diags:#?}"
    );
    let d = hints[0];
    assert_eq!(d.severity, Severity::Hint);
    assert_eq!(
        d.message,
        "a Binary never equals a String, even with the same bytes"
    );
    assert_eq!(
        d.hint.as_deref(),
        Some(
            "convert the Binary first with `asString` (validated, answers a Result) \
             or `asStringUnchecked`"
        )
    );
}

fn assert_no_hint(source: &str) {
    let diags = diagnostics(source);
    assert!(
        compare_hints(&diags).is_empty(),
        "expected no binary-string-compare hint for {source:?}: {diags:#?}"
    );
}

// ── Equality ────────────────────────────────────────────────────────────

#[test]
fn producer_equals_string_literal_hints() {
    assert_one_hint(&probe("(Binary fromBytes: #(97, 98, 99)) == \"abc\""));
}

#[test]
fn string_equals_producer_hints_in_either_order() {
    assert_one_hint(&probe("s == (Binary fromBytes: #(97))"));
}

#[test]
fn every_equality_operator_hints() {
    for op in ["==", "/=", "=:=", "=/="] {
        assert_one_hint(&probe(&format!("(Binary fromBytes: #(97)) {op} s")));
    }
}

#[test]
fn local_bound_to_producer_hints() {
    assert_one_hint(&probe("b := Binary fromIolist: #(\"a\")\n    b == s"));
}

#[test]
fn every_class_side_producer_hints() {
    for producer in [
        "Binary fromBytes: #(97)",
        "Binary fromIolist: #(\"a\")",
        "Binary withAll: #(97)",
        "Binary serialize: 1",
        "Binary empty",
        "Digest sha256: s",
        "Digest md5: s",
    ] {
        assert_one_hint(&probe(&format!("({producer}) == s")));
    }
}

#[test]
fn binary_construction_hints() {
    assert_one_hint(&probe("<<97, 98>> == s"));
}

#[test]
fn concat_and_part_of_a_raw_binary_stay_raw() {
    assert_one_hint(&probe(
        "b := (Binary fromBytes: #(97)) concat: (Binary fromBytes: #(98))\n    b == s",
    ));
    assert_one_hint(&probe(
        "b := Binary fromBytes: #(97, 98)\n    c := b part: 0 size: 1\n    c == s",
    ));
}

// ── Dictionary keys and Set elements ────────────────────────────────────

#[test]
fn string_keyed_dictionary_operations_hint() {
    for call in [
        "d at: b",
        "d at: b ifAbsent: [0]",
        "d at: b put: 1",
        "d includesKey: b",
        "d removeKey: b",
    ] {
        assert_one_hint(&probe_with(
            "d :: Dictionary(String, Integer)",
            &format!("b := Binary fromBytes: #(97)\n    {call}"),
        ));
    }
}

#[test]
fn string_set_membership_hints() {
    for call in ["e includes: b", "e add: b"] {
        assert_one_hint(&probe_with(
            "e :: Set(String)",
            &format!("b := Binary fromBytes: #(97)\n    {call}"),
        ));
    }
}

// ── Silence ─────────────────────────────────────────────────────────────

#[test]
fn declared_binary_may_hold_a_string_so_no_hint() {
    // ADR 0086: a `Binary`-typed parameter may be a String.
    assert_no_hint(&probe_with("b :: Binary with: s :: String", "b == s"));
    assert_no_hint(&probe_with(
        "b :: Binary in: d :: Dictionary(String, Integer)",
        "d at: b",
    ));
}

#[test]
fn reassigned_local_loses_the_fact() {
    assert_no_hint(&probe(
        "b := Binary fromBytes: #(97)\n    b := s\n    b == s",
    ));
}

#[test]
fn local_written_by_a_block_is_never_raw() {
    // The block runs in a child environment, so its write cannot clear the
    // flow fact; any block write disqualifies the local.
    assert_no_hint(&probe_with(
        "s :: String flag: f :: Boolean",
        "b := Binary fromBytes: #(97)\n    f ifTrue: [b := s]\n    b == s",
    ));
    assert_no_hint(&probe(
        "b := Binary fromBytes: #(97)\n    #(1, 2) do: [:i | b := s]\n    b == s",
    ));
    assert_no_hint(&probe(
        "blk := [b := s]\n    b := Binary fromBytes: #(97)\n    blk value\n    b == s",
    ));
}

#[test]
fn local_written_by_a_match_arm_is_never_raw() {
    // BT-3947: an arm's write to a bound outer local is threaded back.
    assert_no_hint(&probe_with(
        "s :: String flag: f",
        "y := Binary fromBytes: #(104)\n    f match: [#a -> y := s; _ -> nil]\n    y == s",
    ));
    assert_no_hint(&probe_with(
        "s :: String flag: f",
        "y := Binary fromBytes: #(104)\n    \
         f match: [#a -> (f match: [#b -> y := s; _ -> nil]); _ -> nil]\n    y == s",
    ));
}

#[test]
fn later_argument_rebinding_the_operand_is_not_trusted() {
    // The operand is read before the argument rebinds it to a producer.
    assert_no_hint(&probe_with(
        "s :: String b: b :: Binary",
        "y := b\n    y == (y := Binary fromBytes: #(104)) asStringUnchecked",
    ));
    assert_no_hint(&probe_with(
        "d :: Dictionary(String, Integer) b: b :: Binary",
        "y := b\n    d at: y put: (y := Binary fromBytes: #(1)) size",
    ));
}

#[test]
fn every_method_kind_resets_the_block_written_set() {
    // A block write in one method must not silence another; a class-side
    // method and a standalone definition are checked like instance methods.
    assert_one_hint(
        "Object subclass: Probe\n  \
         other: s :: String =>\n    \
         #(1) do: [:i | b := s]\n  \
         class run: s :: String =>\n    \
         b := Binary fromBytes: #(97)\n    \
         b == s\n",
    );
    assert_one_hint(
        "Object subclass: Probe\n\n\
         Probe >> run: s :: String =>\n  \
         b := Binary fromBytes: #(97)\n  \
         b == s\n",
    );
}

#[test]
fn string_segment_construction_is_still_raw() {
    assert_one_hint(&probe("<<s/binary>> == s"));
}

#[test]
fn dictionary_key_hint_accompanies_the_argument_type_warning() {
    // The generic argument check already warns that the key is not a
    // String; the hint adds why the lookup can never succeed.
    let diags = diagnostics(&probe_with(
        "d :: Dictionary(String, Integer)",
        "d at: (Binary fromBytes: #(97))",
    ));
    assert_eq!(compare_hints(&diags).len(), 1, "{diags:#?}");
    assert!(
        diags
            .iter()
            .any(|d| d.category == Some(DiagnosticCategory::Type)),
        "{diags:#?}"
    );
}

#[test]
fn block_local_of_the_same_name_does_not_leak_out() {
    // A block's own parameter `b` is not the method local; the hint inside
    // the block is about the parameter, which is a String, so nothing fires.
    assert_no_hint(&probe(
        "b := Binary fromBytes: #(97)\n    #(\"x\") do: [:b | b == s]",
    ));
}

#[test]
fn local_bound_from_a_declared_binary_is_not_raw() {
    assert_no_hint(&probe_with(
        "x :: Binary with: s :: String",
        "b := x\n    b == s",
    ));
}

#[test]
fn concat_on_a_declared_binary_is_conservatively_silent() {
    assert_no_hint(&probe_with(
        "x :: Binary with: s :: String",
        "(x concat: (Binary fromBytes: #(97))) == s",
    ));
}

#[test]
fn dynamic_or_unknown_sides_are_silent() {
    assert_no_hint(&probe_with(
        "x with: s :: String",
        "(Binary fromBytes: #(97)) == x",
    ));
    assert_no_hint(&probe_with(
        "d :: Dictionary",
        "d at: (Binary fromBytes: #(97))",
    ));
    assert_no_hint(&probe_with(
        "d :: Dictionary(Dynamic, Integer)",
        "d at: (Binary fromBytes: #(97))",
    ));
}

#[test]
fn binary_against_binary_and_string_against_string_are_silent() {
    assert_no_hint(&probe(
        "(Binary fromBytes: #(97)) == (Binary fromBytes: #(97))",
    ));
    assert_no_hint(&probe("s == \"a\""));
}

#[test]
fn reverse_collection_case_is_silent() {
    // A `Binary`-keyed Dictionary may hold String keys (ADR 0086).
    assert_no_hint(&probe_with(
        "d :: Dictionary(Binary, Integer) with: s :: String",
        "d at: s",
    ));
    assert_no_hint(&probe_with(
        "e :: Set(Binary) with: s :: String",
        "e includes: s",
    ));
}

#[test]
fn non_key_selectors_are_silent() {
    assert_no_hint(&probe_with(
        "d :: Dictionary(String, Integer)",
        "d includes: (Binary fromBytes: #(97))",
    ));
}

#[test]
fn user_class_named_like_a_producer_selector_is_silent() {
    assert_no_hint(&probe_with(
        "x :: Object with: s :: String",
        "(x fromBytes: #(97)) == s",
    ));
}

// ── Suppression ─────────────────────────────────────────────────────────

#[test]
fn expect_binary_string_compare_suppresses_the_hint() {
    assert_eq!(
        crate::ast::ExpectCategory::from_name("binary_string_compare"),
        Some(crate::ast::ExpectCategory::BinaryStringCompare)
    );
    assert!(crate::ast::ExpectCategory::valid_names().contains(&"binary_string_compare"));

    let diags = diagnostics(&probe(
        "@expect binary_string_compare\n    (Binary fromBytes: #(97)) == s",
    ));
    assert!(
        diags.is_empty(),
        "@expect binary_string_compare should suppress the hint without going stale: {diags:#?}"
    );
}

#[test]
fn diagnostics_table_off_drops_the_hint() {
    let diags = diagnostics(&probe("(Binary fromBytes: #(97)) == s"));
    assert_eq!(compare_hints(&diags).len(), 1);

    let table = parse_diagnostics_table_from_manifest_toml(
        "[package]\nname = \"demo\"\n\n[diagnostics]\nbinary-string-compare = \"off\"\n",
    )
    .unwrap();
    assert_eq!(
        table.get(&DiagnosticCategory::BinaryStringCompare),
        Some(&crate::compilation::diagnostics_policy::DiagnosticSeverityOverride::Off)
    );
    let diags = apply_diagnostics_table(diags, &table);
    assert!(compare_hints(&diags).is_empty(), "{diags:#?}");
}
