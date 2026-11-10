// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4526: a class-side send of a selector the receiver class does not
//! define must not be reported as "expression inferred as Dynamic in typed
//! class (unannotated return)" — the method is absent, not unannotated.
//!
//! * `ClosedComplete` receiver: the does-not-understand diagnostic names the
//!   selector and receiver, and is the only diagnostic for the send — so a
//!   `type = error` table no longer fails the build on a misleading `Type`
//!   error while the real problem is a hint.
//! * `Open` receiver (the pre-WS3 dependency guard, ADR 0100 Rule 2 — the
//!   `beamtalk-buccaneer` repro): DNU stays suppressed, and the typed-class
//!   warning says the selector is unresolved and names it.
//! * A genuinely dynamic receiver still infers `Dynamic` with no DNU.

use super::common::*;
use crate::compilation::diagnostics_policy::{
    DiagnosticSeverityOverride, DiagnosticsTable, apply_diagnostics_table,
};
use crate::semantic_analysis::receiver_knowledge::KnowledgeScope;
use crate::source_analysis::Severity;

/// The `hpack 0.1.1` shape: `HPACK` is sealed and no longer defines
/// `validateHeader:value:`; a typed caller still sends it.
const REPRO: &str = "sealed Object subclass: HPACK
  class encode: x :: Integer -> Integer => x
typed Object subclass: Stream
  check: name :: String value: value :: String -> Nil =>
    HPACK validateHeader: name value: value
    nil
";

fn diagnostics(source: &str, with_dependencies: bool) -> Vec<Diagnostic> {
    let module = parse_source(source);
    let mut hierarchy = ClassHierarchy::build(&module).0.unwrap();
    if with_dependencies {
        hierarchy.set_knowledge_scope(KnowledgeScope::ProjectComplete);
        hierarchy.set_dependency_extensions_unknown(true);
    }
    run_with_expect(&module, &hierarchy)
}

fn type_error_table() -> DiagnosticsTable {
    let mut table = DiagnosticsTable::new();
    table.insert(DiagnosticCategory::Type, DiagnosticSeverityOverride::Error);
    table
}

#[test]
fn absent_selector_on_closed_class_reports_dnu_not_unannotated_return() {
    let diags = diagnostics(REPRO, false);
    assert_eq!(diags.len(), 1, "got: {diags:#?}");
    let dnu = &diags[0];
    assert_eq!(dnu.category, Some(DiagnosticCategory::Dnu));
    assert_eq!(
        dnu.message.as_str(),
        "HPACK class does not understand 'validateHeader:value:'"
    );
}

#[test]
fn type_error_table_surfaces_only_the_dnu_on_closed_class() {
    let diags = apply_diagnostics_table(diagnostics(REPRO, false), &type_error_table());
    assert!(
        !diags.iter().any(|d| d.severity == Severity::Error),
        "the `type = error` gate must not fire on a DNU'd send: {diags:#?}"
    );
    assert!(
        diags
            .iter()
            .all(|d| d.category == Some(DiagnosticCategory::Dnu)),
        "got: {diags:#?}"
    );

    // With `dnu = error` as well, the DNU is the error surfaced.
    let mut table = type_error_table();
    table.insert(DiagnosticCategory::Dnu, DiagnosticSeverityOverride::Error);
    let diags = apply_diagnostics_table(diagnostics(REPRO, false), &table);
    let errors: Vec<_> = diags
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert_eq!(errors.len(), 1, "got: {diags:#?}");
    assert!(errors[0].message.contains("validateHeader:value:"));
}

#[test]
fn absent_selector_on_open_class_names_the_unresolved_selector() {
    // A package with dependencies: an unseen extension could define the
    // selector, so no DNU — but the typed-class warning must name the
    // selector instead of blaming an unannotated return.
    let diags = diagnostics(REPRO, true);
    assert_eq!(diags.len(), 1, "got: {diags:#?}");
    let d = &diags[0];
    assert_eq!(d.category, Some(DiagnosticCategory::Type));
    assert_eq!(
        d.message.as_str(),
        "expression inferred as Dynamic in typed class `Stream` (unresolved selector 'validateHeader:value:')"
    );
    assert!(!d.message.contains("unannotated return"));
    assert!(
        d.hint
            .as_deref()
            .is_some_and(|h| h.contains("validateHeader:value:")),
        "got: {d:#?}"
    );
}

#[test]
fn absent_selector_in_untyped_class_reports_only_the_dnu() {
    let diags = diagnostics(&REPRO.replace("typed Object", "Object"), false);
    assert_eq!(diags.len(), 1, "got: {diags:#?}");
    assert_eq!(diags[0].category, Some(DiagnosticCategory::Dnu));
}

#[test]
fn defined_but_unannotated_class_method_keeps_unannotated_return() {
    // The selector exists and simply declares no return type: that *is* an
    // unannotated return, and no DNU.
    let source = "sealed Object subclass: HPACK
  class validateHeader: n value: v => n
typed Object subclass: Stream
  check: name :: String value: value :: String -> Nil =>
    HPACK validateHeader: name value: value
    nil
";
    let diags = diagnostics(source, false);
    assert!(
        !diags
            .iter()
            .any(|d| d.category == Some(DiagnosticCategory::Dnu)),
        "got: {diags:#?}"
    );
    assert!(
        diags
            .iter()
            .any(|d| d.message.contains("(unannotated return)")),
        "got: {diags:#?}"
    );
}

#[test]
fn class_object_protocol_and_implicit_new_are_not_unresolved() {
    // `name` answers through the class-object chain; `new` is the implicit
    // constructor. Neither is an unresolved selector, open or closed.
    let source = "sealed Object subclass: HPACK
typed Object subclass: Stream
  check -> Nil =>
    HPACK name
    HPACK new
    nil
";
    for with_dependencies in [false, true] {
        let diags = diagnostics(source, with_dependencies);
        assert!(
            !diags
                .iter()
                .any(|d| d.category == Some(DiagnosticCategory::Dnu)
                    || d.message.contains("unresolved selector")),
            "deps={with_dependencies}: {diags:#?}"
        );
    }
}

#[test]
fn dynamic_receiver_still_infers_dynamic_without_dnu() {
    let source = "typed Object subclass: Stream
  check: codec value: value :: String -> Nil =>
    codec validateHeader: value value: value
    nil
";
    let diags = diagnostics(source, false);
    assert!(
        !diags
            .iter()
            .any(|d| d.category == Some(DiagnosticCategory::Dnu)
                || d.message.contains("unresolved selector")),
        "got: {diags:#?}"
    );
}

#[test]
fn wrapped_unresolved_send_warns_once_naming_the_selector() {
    // A parenthesized or assigned send passes the send's `Dynamic` through;
    // only the send itself warns, so the wrapper must not add a second,
    // selector-less warning with the old "add a type annotation" hint.
    for body in [
        "(HPACK validateHeader: name value: name)",
        "h := HPACK validateHeader: name value: name",
    ] {
        let source = format!(
            "sealed Object subclass: HPACK
typed Object subclass: Stream
  check: name :: String -> Nil =>
    {body}
    nil
"
        );
        let diags = diagnostics(&source, true);
        assert_eq!(diags.len(), 1, "{body}: {diags:#?}");
        assert!(
            diags[0]
                .message
                .contains("(unresolved selector 'validateHeader:value:')"),
            "{body}: {diags:#?}"
        );
    }
}

#[test]
fn cascade_on_unresolved_send_warns_once_naming_the_selector() {
    // A cascade's type is its first send's, which `infer_cascade`
    // post-processes on its own; the cascade node must not repeat it.
    let source = "sealed Object subclass: HPACK
  class encode: x :: Integer -> Integer => x
typed Object subclass: Stream
  check: name :: String -> Nil =>
    HPACK validateHeader: name value: name; encode: 1
    nil
";
    let diags = diagnostics(source, true);
    assert_eq!(diags.len(), 1, "got: {diags:#?}");
    assert!(
        diags[0]
            .message
            .contains("(unresolved selector 'validateHeader:value:')"),
        "got: {diags:#?}"
    );
}

#[test]
fn block_result_pass_through_sends_are_not_blamed() {
    // `ifTrue:ifFalse:` / `value` only pass the block's `Dynamic` along:
    // each unresolved inner send warns once under its own selector, and the
    // pass-through send never warns under *its* (resolved) selector.
    for (body, expected) in [
        (
            "flag ifTrue: [HPACK validateHeader: name value: name] ifFalse: [HPACK other: name]",
            vec!["validateHeader:value:", "other:"],
        ),
        (
            "[HPACK validateHeader: name value: name] value",
            vec!["validateHeader:value:"],
        ),
    ] {
        let source = format!(
            "sealed Object subclass: HPACK
typed Object subclass: Stream
  check: name :: String flag: flag :: Boolean -> Nil =>
    {body}
    nil
"
        );
        let diags = diagnostics(&source, true);
        let named: Vec<_> = diags
            .iter()
            .filter_map(|d| {
                d.message
                    .split("(unresolved selector '")
                    .nth(1)
                    .map(|rest| rest.trim_end_matches("')").to_string())
            })
            .collect();
        assert_eq!(named, expected, "{body}: {diags:#?}");
        assert_eq!(diags.len(), expected.len(), "{body}: {diags:#?}");
    }
}
