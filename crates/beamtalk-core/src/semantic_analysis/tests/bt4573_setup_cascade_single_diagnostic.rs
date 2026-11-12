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

/// Compiles the `AbTest` class with a hand-written, unannotated `withBoth:`
/// (which the type checker cannot prove answers an updated copy) and the
/// given `setUp` body.
fn compile_with_unannotated_with_both(setup_body: &str) -> Vec<Diagnostic> {
    let source = format!(
        "TestCase subclass: AbTest\n  field: a :: Integer = 0\n  field: b :: Integer = 0\n\n  \
         withBoth: x => (self withA: x) withB: x\n\n  \
         setUp =>\n{setup_body}\n\n  \
         testA => self assert: self a equals: 1\n"
    );
    let (module, parse_diags) = parse(lex_with_eof(&source));
    compute_diagnostics(&module, parse_diags)
}

fn has_setup_lint_warning(diags: &[Diagnostic]) -> bool {
    diags.iter().any(|d| {
        d.category == Some(DiagnosticCategory::Lint) && d.message.contains("silently dropped")
    })
}

/// Only some of the drops are proven: `cascade-drops-update` fires at
/// `withA:`, but the dropped `withBoth:` is unannotated and goes unproven.
/// The `setUp` warning must still fire, or `withBoth:`'s lost update would
/// be unreported.
#[test]
fn partially_proven_cascade_keeps_setup_warning() {
    let diags = compile_with_unannotated_with_both("    self withA: 1; withBoth: 2; withB: 3");
    assert!(
        diags
            .iter()
            .any(|d| d.category == Some(DiagnosticCategory::CascadeDropsUpdate)),
        "expected the proven drop at `withA:` to warn, got: {diags:#?}"
    );
    assert!(
        has_setup_lint_warning(&diags),
        "expected the setUp warning for the unproven drop, got: {diags:#?}"
    );
}

/// The only `cascade-drops-update` belongs to a cascade nested in an
/// argument; the outer cascade's own drop (`withBoth:`) is unproven, so the
/// `setUp` warning must still fire.
#[test]
fn nested_argument_cascade_warning_does_not_cover_outer_cascade() {
    let diags = compile_with_unannotated_with_both(
        "    self withBoth: (self withA: 1; withB: 2) a; withB: 2",
    );
    assert!(
        diags
            .iter()
            .any(|d| d.category == Some(DiagnosticCategory::CascadeDropsUpdate)),
        "expected the inner cascade to warn, got: {diags:#?}"
    );
    assert!(
        has_setup_lint_warning(&diags),
        "expected the setUp warning for the outer cascade, got: {diags:#?}"
    );
}

/// A dropped message after the first one is reported at its own message
/// span, and the `setUp` lint reads that same span: two proven drops, two
/// `cascade-drops-update` warnings, no `setUp` warning.
#[test]
fn later_dropped_message_is_reported_at_its_own_span() {
    let (source, diags) = compile_setup("    self withA: 1; withB: 2; withA: 3");
    let at: Vec<&str> = diags
        .iter()
        .map(|d| {
            assert_eq!(
                d.category,
                Some(DiagnosticCategory::CascadeDropsUpdate),
                "got: {diags:#?}"
            );
            &source[d.span.start() as usize..d.span.end() as usize]
        })
        .collect();
    assert_eq!(at, ["self withA: 1", "withB: 2"], "got: {diags:#?}");
}

/// BT-4522: a non-final `self withA: 1.` statement in `setUp` draws the
/// type checker's `discarded-update` and no second `setUp` warning.
#[test]
fn setup_non_final_with_star_statement_gets_only_discarded_update() {
    for tail in ["    self", "    nil"] {
        let (_, diags) = compile_setup(&format!("    self withA: 1\n{tail}"));
        assert_eq!(diags.len(), 1, "tail {tail:?}, got: {diags:#?}");
        assert_eq!(
            diags[0].category,
            Some(DiagnosticCategory::DiscardedUpdate),
            "tail {tail:?}, got: {diags:#?}"
        );
    }
}
