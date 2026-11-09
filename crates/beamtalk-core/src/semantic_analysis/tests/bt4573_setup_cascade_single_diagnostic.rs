// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4573: a `TestCase` `setUp` ending in a multi-`with*:` cascade on
//! `self` gets exactly one diagnostic, the type checker's
//! `cascade-drops-update` at the dropped message — not a second,
//! `setUp`-specific lint warning on the whole cascade. Runs the full
//! diagnostic pipeline (`compute_diagnostics`: semantic analysis, type
//! checking, the `setUp` lint and `@expect` resolution) against the real
//! stdlib `TestCase`.

use crate::compilation::diagnostics_policy::compute_diagnostics;
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Severity, lex_with_eof, parse};

/// Compiles a `TestCase` subclass whose `setUp` body is `setup_body`,
/// returning the source and every diagnostic it draws.
fn compile_setup(setup_body: &str) -> (String, Vec<Diagnostic>) {
    let source = format!(
        "TestCase subclass: AbTest\n  field: a :: Integer = 0\n  field: b :: Integer = 0\n\n  \
         setUp =>\n{setup_body}\n\n  testA => self assert: self a equals: 1\n"
    );
    let (module, parse_diags) = parse(lex_with_eof(&source));
    assert!(
        parse_diags.iter().all(|d| d.severity != Severity::Error),
        "parse failed: {parse_diags:#?}"
    );
    let diags = compute_diagnostics(&module, parse_diags);
    (source, diags)
}

fn assert_single_cascade_drops_update(setup_body: &str) {
    let (source, diags) = compile_setup(setup_body);
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic, got: {diags:#?}"
    );
    let diag = &diags[0];
    assert_eq!(
        diag.category,
        Some(DiagnosticCategory::CascadeDropsUpdate),
        "got: {diag:#?}"
    );
    assert_eq!(diag.message.as_str(), "the result of `withA:` is discarded");
    let at = &source[diag.span.start() as usize..diag.span.end() as usize];
    // The cascade's first message is the folded `self withA: 1` send.
    assert_eq!(
        at, "self withA: 1",
        "expected the warning at the `withA:` send"
    );
}

#[test]
fn trailing_multi_with_cascade_gets_only_cascade_drops_update() {
    assert_single_cascade_drops_update("    self withA: 1; withB: 2");
}

#[test]
fn trailing_local_bound_to_multi_with_cascade_gets_only_cascade_drops_update() {
    assert_single_cascade_drops_update("    updated := self withA: 1; withB: 2\n    updated");
}

#[test]
fn expect_cascade_drops_update_silences_the_whole_case() {
    let (_, diags) = compile_setup("    @expect cascade_drops_update\n    self withA: 1; withB: 2");
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:#?}");
}

#[test]
fn trailing_single_with_cascade_is_silent() {
    let (_, diags) = compile_setup("    self yourself; withB: 2");
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:#?}");
}

/// A hand-written `with*:` method with no `-> Self` annotation gives the
/// type checker nothing to prove the drop with, so `cascade-drops-update`
/// stays silent — and the `setUp` check's generic warning still fires, so
/// the lost update is never unreported.
#[test]
fn unannotated_hand_written_with_star_cascade_keeps_setup_warning() {
    let source = "TestCase subclass: AbTest\n  field: a :: Integer = 0\n  field: b :: Integer = 0\n\n  \
                  withBoth: x => (self withA: x) withB: x\n\n  \
                  setUp =>\n    self withBoth: 1; withB: 2\n\n  \
                  testA => self assert: self a equals: 1\n";
    let (module, parse_diags) = parse(lex_with_eof(source));
    let diags = compute_diagnostics(&module, parse_diags);
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic, got: {diags:#?}"
    );
    assert_eq!(
        diags[0].category,
        Some(DiagnosticCategory::Lint),
        "got: {diags:#?}"
    );
    assert!(
        diags[0].message.contains("silently dropped"),
        "got: {}",
        diags[0].message
    );
}

#[test]
fn nested_with_form_is_silent() {
    let (_, diags) = compile_setup("    (self withA: 1) withB: 2");
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:#?}");
}
