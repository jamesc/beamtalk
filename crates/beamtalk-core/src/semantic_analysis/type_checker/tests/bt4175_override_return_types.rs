// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4175: declared return types are checked covariantly — both for an
//! instance method overriding an inherited one
//! (`check_override_return_compatibility`) and for a class satisfying a
//! protocol requirement at a `:: Protocol` argument site
//! (`ProtocolRegistry::conformance_return_type_mismatches`). Both share
//! `TypeChecker::is_return_type_refinement`. Warnings, never errors.

use super::common::*;
use crate::source_analysis::Severity;

fn type_diagnostics(source: &str) -> Vec<Diagnostic> {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    crate::semantic_analysis::analyse(&module)
        .diagnostics
        .into_iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect()
}

fn override_return_warnings(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.message.starts_with("Return type of") && d.message.contains("parent's"))
        .collect()
}

fn protocol_return_warnings(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.message.contains("incompatible with protocol"))
        .collect()
}

fn override_source(parent_rt: &str, child_rt: &str, child_body: &str) -> String {
    format!(
        r"
Object subclass: Base
  value -> {parent_rt} => 1

Base subclass: Derived
  value{child_rt} => {child_body}
"
    )
}

// --- Override return types ---

#[test]
fn override_with_unrelated_return_type_warns() {
    let diags = type_diagnostics(&override_source("Number", " -> String", "\"x\""));
    let hits = override_return_warnings(&diags);
    assert_eq!(
        hits.len(),
        1,
        "expected one override warning, got: {diags:#?}"
    );
    assert_eq!(hits[0].severity, Severity::Warning);
    assert!(
        hits[0].message.contains("'value' in Derived is String")
            && hits[0].message.contains("parent's Number"),
        "unexpected message: {}",
        hits[0].message
    );
}

#[test]
fn override_narrowing_return_type_is_silent() {
    let diags = type_diagnostics(&override_source("Number", " -> Integer", "2"));
    assert!(
        override_return_warnings(&diags).is_empty(),
        "narrowing Number to Integer is a valid covariant override: {diags:#?}"
    );
}

#[test]
fn override_widening_to_union_warns() {
    // `Integer | Nil` is not assignable to `Integer`: the `Nil` member widens.
    let diags = type_diagnostics(&override_source("Integer", " -> Integer | Nil", "nil"));
    assert_eq!(
        override_return_warnings(&diags).len(),
        1,
        "widening Integer to Integer | Nil should warn: {diags:#?}"
    );
}

#[test]
fn override_narrowing_union_is_silent() {
    let diags = type_diagnostics(&override_source("Integer | Nil", " -> Integer", "2"));
    assert!(
        override_return_warnings(&diags).is_empty(),
        "dropping a union member narrows: {diags:#?}"
    );
}

#[test]
fn unannotated_override_is_not_checked() {
    let diags = type_diagnostics(&override_source("Number", "", "\"x\""));
    assert!(
        override_return_warnings(&diags).is_empty(),
        "an override without a declared return type has nothing to compare: {diags:#?}"
    );
}

#[test]
fn override_of_self_returning_parent_is_skipped() {
    let source = r"
Object subclass: Base
  copyIt -> Self => self

Base subclass: Derived
  copyIt -> Derived => self
";
    let diags = type_diagnostics(source);
    assert!(
        override_return_warnings(&diags).is_empty(),
        "`Self` is receiver-relative and not judged by the nominal walk: {diags:#?}"
    );
}

#[test]
fn override_return_warning_suppressed_by_expect_type() {
    let source = r#"
Object subclass: Base
  value -> Number => 1

Base subclass: Derived
  @expect type
  value -> String => "x"
"#;
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let mut diags = crate::semantic_analysis::analyse(&module).diagnostics;
    assert_eq!(
        override_return_warnings(&diags).len(),
        1,
        "the warning fires before suppression: {diags:#?}"
    );
    crate::compilation::diagnostics_policy::apply_expect_directives(&module, &mut diags);
    assert!(
        override_return_warnings(&diags).is_empty(),
        "`@expect type` on the method suppresses the override warning: {diags:#?}"
    );
    assert!(
        !diags.iter().any(|d| d.message.contains("stale")),
        "the `@expect type` is consumed, not stale: {diags:#?}"
    );
}

// --- The shared refinement rule ---

#[test]
fn refinement_rule_basic_shapes() {
    let h = ClassHierarchy::with_builtins();
    let p = DeclaredType::parse;
    let ok = |a: &str, e: &str| TypeChecker::is_return_type_refinement(&p(a), &p(e), &h);
    assert!(ok("Integer", "Number"));
    assert!(ok("Integer", "Object"));
    assert!(!ok("String", "Integer"));
    assert!(ok("Integer", "Integer | Nil"));
    assert!(!ok("Integer | Nil", "Integer"));
    assert!(!ok("List(String)", "List(Integer)"));
    // Conservative (opaque) shapes never warn.
    assert!(ok("E", "Integer"));
    assert!(ok("List(E)", "List(Integer)"));
    assert!(ok("Dynamic", "Integer"));
    assert!(ok("Integer", "Self"));
}

/// Known unsoundness pinned (BT-4169 follow-up): `Binary>>asString` is
/// declared `-> Result(String, Error)` — it validates UTF-8 — which widens the
/// inherited `Collection>>asString -> String`. The stdlib source suppresses
/// the resulting override warning with `@expect type`; this pins that the
/// rule itself flags the pair.
#[test]
fn binary_as_string_does_not_refine_collection_as_string() {
    let h = ClassHierarchy::with_builtins();
    let binary = h
        .find_method("Binary", "asString")
        .and_then(|m| m.return_type)
        .expect("Binary>>asString has a declared return type");
    let collection = h
        .find_method("Collection", "asString")
        .and_then(|m| m.return_type)
        .expect("Collection>>asString has a declared return type");
    assert_eq!(binary.to_string(), "Result(String, Error)");
    assert_eq!(collection.to_string(), "String");
    assert!(!TypeChecker::is_return_type_refinement(
        &binary,
        &collection,
        &h
    ));
}

// --- Protocol conformance return types ---

const PRINTABLE: &str = r"
Protocol define: Printable
  asString -> String
  printString -> String
";

/// `Binary>>asString` widens the inherited, `Printable`-conforming
/// `Collection>>asString -> String`. That unsoundness is reported once, at
/// the override (where the stdlib suppresses it with `@expect type`), not at
/// every call site that passes a `Binary` to a `:: Printable` parameter —
/// callers can't fix it.
#[test]
fn binary_passed_as_printable_is_not_rewarned_at_call_site() {
    let source = format!(
        r"{PRINTABLE}
Object subclass: Caller
  show: p :: Printable -> String => p printString
  run => self show: (Binary serialize: 42)
"
    );
    let diags = type_diagnostics(&source);
    assert!(
        protocol_return_warnings(&diags).is_empty(),
        "inherited-widening mismatch belongs to the override site: {diags:#?}"
    );
    assert!(
        !diags
            .iter()
            .any(|d| d.message.contains("does not conform to protocol")),
        "selector-presence conformance still holds: {diags:#?}"
    );
}

/// A user override that widens a conforming inherited return type warns
/// once at its definition and is silent at the protocol-typed call site.
#[test]
fn user_override_widening_conforming_parent_warns_at_definition_only() {
    let source = format!(
        r#"{PRINTABLE}
Object subclass: Base
  asString -> String => "b"

Base subclass: Derived
  asString -> Integer => 1

Object subclass: Caller
  show: p :: Printable -> String => p printString
  run => self show: Derived new
"#
    );
    let diags = type_diagnostics(&source);
    assert_eq!(
        override_return_warnings(&diags).len(),
        1,
        "expected the override warning: {diags:#?}"
    );
    assert!(
        protocol_return_warnings(&diags).is_empty(),
        "no duplicate call-site warning: {diags:#?}"
    );
}

/// The parent itself violating the protocol keeps the call-site warning:
/// no override site covers that mismatch.
#[test]
fn override_of_non_conforming_parent_still_warns_at_call_site() {
    let source = format!(
        r#"{PRINTABLE}
Object subclass: Base
  asString -> Integer => 1

Base subclass: Derived
  asString -> Integer => 2

Object subclass: Caller
  show: p :: Printable -> String => p printString
  run => self show: Derived new
"#
    );
    let diags = type_diagnostics(&source);
    let hits = protocol_return_warnings(&diags);
    assert_eq!(
        hits.len(),
        1,
        "expected one protocol return warning: {diags:#?}"
    );
    assert!(
        hits[0]
            .message
            .contains("Derived>>asString returns Integer"),
        "unexpected message: {}",
        hits[0].message
    );
}

#[test]
fn string_passed_as_printable_is_silent() {
    let source = format!(
        r#"{PRINTABLE}
Object subclass: Caller
  show: p :: Printable -> String => p printString
  run => self show: "hi"
"#
    );
    let diags = type_diagnostics(&source);
    assert!(
        protocol_return_warnings(&diags).is_empty(),
        "String>>asString -> String conforms: {diags:#?}"
    );
}

#[test]
fn user_class_with_widened_protocol_return_warns() {
    let source = format!(
        r#"{PRINTABLE}
Object subclass: Widget
  asString -> Integer => 1
  printString -> String => "w"

Object subclass: Caller
  show: p :: Printable -> String => p printString
  run => self show: Widget new
"#
    );
    let diags = type_diagnostics(&source);
    let hits = protocol_return_warnings(&diags);
    assert_eq!(
        hits.len(),
        1,
        "expected one protocol return warning: {diags:#?}"
    );
    assert!(
        hits[0].message.contains("Widget>>asString returns Integer"),
        "unexpected message: {}",
        hits[0].message
    );
}

#[test]
fn registry_reports_no_mismatch_for_unannotated_method() {
    let source = format!(
        r#"{PRINTABLE}
Object subclass: Widget
  asString => "w"
  printString -> String => "w"

Object subclass: Caller
  show: p :: Printable -> String => p printString
  run => self show: Widget new
"#
    );
    let diags = type_diagnostics(&source);
    assert!(
        protocol_return_warnings(&diags).is_empty(),
        "an undeclared return type is not contradicted: {diags:#?}"
    );
}
