// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0108 / BT-2897: display-name provenance for diagnostics.
//!
//! Hover coverage lives in `queries::hover_provider`'s own test module
//! (`compute_hover` is the hover entry point, not part of this crate's
//! `semantic_analysis::type_checker`). This file covers the diagnostic side:
//! membership-violation wording naming the alias (with a "did you mean"
//! suggestion, reusing `validation.rs`'s `edit_distance` "did you mean"
//! machinery), and the core `TypeProvenance::Aliased` display mechanics that
//! back both surfaces.

/// Parses `source` and runs the full `analyse_with_options_and_classes`
/// pipeline (alias registration + `check_module_with_protocols_and_aliases`,
/// exactly as a real compile would — see `type_alias_exhaustiveness.rs`'s
/// identical helper), returning every diagnostic.
fn analyse_diagnostics(source: &str) -> Vec<crate::source_analysis::Diagnostic> {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse_with_options_and_classes(
        &module,
        &crate::CompilerOptions::default(),
        vec![],
    );
    result.diagnostics
}

// ── Membership diagnostics name the alias (ADR 0108 Error examples) ────────

#[test]
fn singleton_comparison_against_alias_names_alias_and_suggests_correction() {
    // ADR 0108's own example: `#premanent` vs `#permanent`. The receiver's
    // declared type is `RestartStrategy`, so the diagnostic must render
    // `RestartStrategy (#temporary | #transient | #permanent)`, not the bare
    // expansion — and, since the union is closed, a "did you mean" typo
    // suggestion (mirroring `validation.rs`'s `closest_state_slot`).
    let source = r"
type RestartStrategy = #temporary | #transient | #permanent

Object subclass: Supervisor
  restart: policy :: RestartStrategy =>
    policy =:= #premanent
";
    let diags = analyse_diagnostics(source);
    let hits: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("comparison can never be true"))
        .collect();
    assert_eq!(hits.len(), 1, "expected one diagnostic, got: {diags:?}");
    let message = &hits[0].message;
    assert!(
        message.contains("RestartStrategy (#temporary | #transient | #permanent)"),
        "should name the alias with its expansion, got: {message}"
    );
    assert!(
        message.contains("did you mean `#permanent`?"),
        "should suggest the closest member, got: {message}"
    );
}

#[test]
fn singleton_comparison_against_spelled_out_union_has_no_did_you_mean_without_alias() {
    // Sibling to the alias case above with the union spelled out directly —
    // pins that plain structural unions behave exactly as they did before
    // BT-2897 (no alias name, and still no false "did you mean" for an
    // unrelated typo — `#premanent` is edit-distance 2 from `#permanent`
    // either way, so this test's real point is the *absence* of the alias
    // parenthetical, not the suggestion itself).
    let source = r"
Object subclass: Supervisor
  restart: policy :: #temporary | #transient | #permanent =>
    policy =:= #premanent
";
    let diags = analyse_diagnostics(source);
    let hits: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("comparison can never be true"))
        .collect();
    assert_eq!(hits.len(), 1, "expected one diagnostic, got: {diags:?}");
    let message = &hits[0].message;
    assert!(
        !message.contains("RestartStrategy"),
        "no alias in scope — must not mention one, got: {message}"
    );
    assert!(
        message.contains("did you mean `#permanent`?"),
        "did-you-mean suggestion is alias-independent, got: {message}"
    );
}

#[test]
fn singleton_comparison_far_typo_gets_no_suggestion() {
    // A singleton with no close match in the union gets the base message
    // only — no fabricated suggestion.
    let source = r"
type RestartStrategy = #temporary | #transient | #permanent

Object subclass: Supervisor
  restart: policy :: RestartStrategy =>
    policy =:= #zzz
";
    let diags = analyse_diagnostics(source);
    let hits: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("comparison can never be true"))
        .collect();
    assert_eq!(hits.len(), 1, "expected one diagnostic, got: {diags:?}");
    let message = &hits[0].message;
    assert!(
        message.contains("RestartStrategy (#temporary | #transient | #permanent)"),
        "should still name the alias, got: {message}"
    );
    assert!(
        !message.contains("did you mean"),
        "no close match — must not suggest anything, got: {message}"
    );
}

// ── Unknown-alias diagnostic (ADR 0108 Error examples) ──────────────────────

#[test]
fn misspelled_alias_reference_suggests_the_real_name() {
    // ADR 0108's own example: `RestartStrateg` vs `RestartStrategy`.
    let source = r"
type RestartStrategy = #temporary | #transient | #permanent

Object subclass: Supervisor
  restart: policy :: RestartStrateg => policy
";
    let diags = analyse_diagnostics(source);
    let hits: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("unknown type"))
        .collect();
    assert_eq!(hits.len(), 1, "expected one diagnostic, got: {diags:?}");
    assert!(
        hits[0]
            .message
            .contains("unknown type `RestartStrateg` (did you mean `RestartStrategy`?)"),
        "got: {}",
        hits[0].message
    );
}

#[test]
fn unrelated_unknown_class_reference_is_not_flagged_by_alias_check() {
    // A totally unrelated unresolved name (nowhere near any alias) must not
    // be flagged by the alias-typo checker — that's `check_unresolved_classes`'s
    // job (and only fires for expression-position class references, not
    // annotations, which this checker deliberately does not duplicate).
    let source = r"
type RestartStrategy = #temporary | #transient | #permanent

Object subclass: Supervisor
  restart: policy :: SomeTotallyUnrelatedName => policy
";
    let diags = analyse_diagnostics(source);
    assert!(
        diags.iter().all(|d| !d.message.contains("unknown type")),
        "unrelated unknown name must not trigger the alias-typo check, got: {diags:?}"
    );
}

#[test]
fn alias_free_module_never_triggers_unknown_alias_check() {
    // No aliases registered at all — the checker must be a strict no-op
    // (including on a near-miss of a *class* name, which is not its job).
    let source = "Object subclass: Foo\n  bar: x :: Integr => x\n";
    let diags = analyse_diagnostics(source);
    assert!(
        diags.iter().all(|d| !d.message.contains("unknown type")),
        "alias-free module must never trigger the unknown-alias check, got: {diags:?}"
    );
}

#[test]
fn negated_singleton_comparison_names_alias_without_did_you_mean() {
    // The "always true" (negated, `/=`) case still gets the alias name via
    // `display_for_diagnostic`, but never a "did you mean" — that suggestion
    // is scoped to the "can never be true" (typo) shape only.
    let source = r"
type RestartStrategy = #temporary | #transient | #permanent

Object subclass: Supervisor
  restart: policy :: RestartStrategy =>
    policy /= #premanent
";
    let diags = analyse_diagnostics(source);
    let hits: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("comparison is always true"))
        .collect();
    assert_eq!(hits.len(), 1, "expected one diagnostic, got: {diags:?}");
    let message = &hits[0].message;
    assert!(
        message.contains("RestartStrategy (#temporary | #transient | #permanent)"),
        "should still name the alias, got: {message}"
    );
    assert!(
        !message.contains("did you mean"),
        "negated case must never suggest, got: {message}"
    );
}
