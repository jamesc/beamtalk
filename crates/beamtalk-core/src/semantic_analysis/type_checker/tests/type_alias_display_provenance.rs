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

// Note: the "unknown-alias" diagnostic (`check_unresolved_type_aliases`,
// ADR 0108 Error examples — `unknown type RestartStrateg (did you mean
// RestartStrategy?)`) is gated on cross-file metadata being loaded, exactly
// like the pre-existing `check_unresolved_classes` (see both functions'
// docs) — this file's `analyse_diagnostics` helper passes no pre-loaded
// classes, so that gate is always closed here. Its tests live directly
// alongside `check_unresolved_classes`'s own in
// `semantic_analysis::validators::structural_validators`'s test module,
// which calls the checker function directly rather than through the full
// pipeline (matching that module's existing convention).

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
// ── BT-2911: EcoString-based diagnostic paths never touched by BT-2897 ─────

#[test]
fn field_assignment_mismatch_on_alias_typed_state_field_names_the_alias() {
    // `check_field_assignment` reads `declared_type` as a bare `EcoString`
    // from `ClassHierarchy::state_field_type` — pre-ADR-0108 storage that
    // never carries a `TypeProvenance::Aliased` tag the way an
    // `InferredType` does. Assigning an incompatible `Integer` to a
    // `RestartStrategy`-typed `state:` field must still name the alias in
    // the "Type mismatch: field ... declared as ..." message, matching what
    // an alias-typed parameter mismatch already renders (BT-2897).
    let source = r"
type RestartStrategy = #temporary | #transient | #permanent

Actor subclass: Supervisor
  state: policy :: RestartStrategy = #temporary

  reset =>
    self.policy := 42
";
    let diags = analyse_diagnostics(source);
    let hits: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("Type mismatch: field"))
        .collect();
    assert_eq!(hits.len(), 1, "expected one diagnostic, got: {diags:?}");
    assert!(
        hits[0]
            .message
            .contains("RestartStrategy (#temporary | #transient | #permanent)"),
        "should name the alias with its expansion, got: {}",
        hits[0].message
    );
}

#[test]
fn field_assignment_mismatch_on_plain_typed_state_field_is_unaffected() {
    // Sibling to the alias case above with a plain (non-alias) declared
    // field type — pins that the BT-2911 fix is a no-op when there is no
    // alias registry entry to resolve, matching pre-BT-2911 behaviour.
    let source = r#"
Actor subclass: Supervisor
  state: count :: Integer = 0

  reset =>
    self.count := "oops"
"#;
    let diags = analyse_diagnostics(source);
    let hits: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("Type mismatch: field"))
        .collect();
    assert_eq!(hits.len(), 1, "expected one diagnostic, got: {diags:?}");
    assert!(
        hits[0].message.contains("declared as Integer, got String"),
        "plain declared type must render unchanged, got: {}",
        hits[0].message
    );
}

#[test]
fn argument_mismatch_on_alias_typed_parameter_names_the_alias() {
    // `check_argument_types` reads `expected_ty` as a bare `EcoString` from
    // `MethodInfo::param_types` — the same pre-ADR-0108 storage
    // `check_field_assignment` sits on. Unlike that sibling, this path
    // cannot be exercised through the *full* compile pipeline: an alias
    // name is never registered as a class in `ClassHierarchy`
    // (`hierarchy.has_class("RestartStrategy")` is always `false`), and
    // `is_type_compatible`'s pre-existing, unrelated "unknown declared
    // class → conservatively compatible" fallback (validation.rs, the
    // `!hierarchy.has_class(actual_base) || !hierarchy.has_class(expected_base)`
    // check) absorbs *every* argument against a bare alias-typed parameter
    // as compatible, so the mismatch branch this test targets never runs
    // end-to-end today. That gap is pre-existing and out of BT-2911's
    // display-only scope (its own acceptance criteria: no change to
    // `is_assignable_to`/`is_type_compatible`) — fixing it would mean
    // resolving `param_types` through the alias table before the
    // compatibility check itself, a bigger change than display provenance.
    //
    // So this test calls `check_argument_types` directly (mirroring
    // `local_annotations.rs`'s `register_test_alias` pattern) with a
    // hierarchy where a `RestartStrategy` *class* happens to also exist —
    // purely to satisfy `is_type_compatible`'s `has_class` guard and reach
    // the mismatch branch — while the `AliasRegistry` alias of the same
    // name is seeded independently via `register_test_alias`, bypassing
    // `AliasRegistry::register_module`'s namespace-collision check (which
    // would legitimately reject a real alias/class name collision — see
    // that method's doc). This is an intentionally synthetic setup to reach
    // otherwise-unreachable code; it does not assert anything about
    // real-world alias/class collisions.
    use crate::semantic_analysis::alias_registry::{AliasInfo, AliasRegistry};

    let source = r"
Object subclass: RestartStrategy

Object subclass: Widget
  restart: policy :: RestartStrategy => policy
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let (hierarchy, hierarchy_diags) = crate::semantic_analysis::ClassHierarchy::build(&module);
    let hierarchy = hierarchy.expect("hierarchy should build");
    assert!(
        hierarchy_diags.is_empty(),
        "unexpected: {hierarchy_diags:?}"
    );

    let mut alias_registry = AliasRegistry::new();
    alias_registry.register_test_alias(AliasInfo {
        name: "RestartStrategy".into(),
        annotation: crate::ast::TypeAnnotation::Union {
            types: vec![
                crate::ast::TypeAnnotation::Singleton {
                    name: "temporary".into(),
                    span: crate::source_analysis::Span::new(0, 1),
                },
                crate::ast::TypeAnnotation::Singleton {
                    name: "transient".into(),
                    span: crate::source_analysis::Span::new(0, 1),
                },
                crate::ast::TypeAnnotation::Singleton {
                    name: "permanent".into(),
                    span: crate::source_analysis::Span::new(0, 1),
                },
            ],
            span: crate::source_analysis::Span::new(0, 1),
        },
        is_internal: false,
        package: None,
        span: crate::source_analysis::Span::new(0, 1),
    });

    let mut checker = crate::semantic_analysis::type_checker::TypeChecker::new();
    checker.set_alias_registry(alias_registry);
    checker.check_argument_types(
        &"Widget".into(),
        "restart:",
        &[crate::semantic_analysis::InferredType::known("Integer")],
        crate::source_analysis::Span::new(0, 1),
        &hierarchy,
        false,
        None,
        None,
    );
    let diags = checker.diagnostics();
    let hits: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert_eq!(hits.len(), 1, "expected one diagnostic, got: {diags:?}");
    assert!(
        hits[0].message.contains(
            "expects RestartStrategy (#temporary | #transient | #permanent), got Integer"
        ),
        "should name the alias with its expansion, got: {}",
        hits[0].message
    );
}

#[test]
fn field_assignment_of_a_valid_alias_member_no_longer_false_positives_bt2953() {
    // Was a KNOWN BUG, tracked as BT-2953: `check_field_assignment` called
    // `is_assignable_to` (validation.rs) with the *bare* alias name (e.g.
    // `"RestartStrategy"`), never expanded — `ClassHierarchy` predates ADR
    // 0108 and has no `AliasRegistry` access. `is_assignable_to` has no
    // "unknown declared class" escape hatch, so a bare alias name that never
    // matches any registered class fell through to a superclass-chain walk
    // that always returned `false` — reporting EVERY value as incompatible
    // with an alias-typed field, including values that ARE valid members of
    // the alias (like `#transient` here).
    //
    // Fixed under BT-2939 (surfaced while aliasing stdlib's own
    // `RestartStrategy`/`Timeout` — `just build-stdlib` hit this exact false
    // positive on `TimeoutProxy>>setTimeoutMs:`'s `self.timeoutMs := ms`):
    // `check_field_assignment` now expands `declared_type` through the
    // `AliasRegistry` to its structural form before any compatibility check,
    // so it compares against the real union members instead of the bare
    // alias name. This closes BT-2953's field-assignment case (case 1); its
    // argument-type-checking case (case 2 — `is_type_compatible`'s
    // "unknown declared class → conservatively compatible" escape hatch
    // silently accepting *any* value for an alias-typed parameter) is a
    // separate call site this fix does not touch.
    let source = r"
type RestartStrategy = #temporary | #transient | #permanent

Actor subclass: Supervisor
  state: policy :: RestartStrategy = #temporary

  reset =>
    self.policy := #transient
";
    let diags = analyse_diagnostics(source);
    let hits: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("Type mismatch: field"))
        .collect();
    assert_eq!(
        hits.len(),
        0,
        "assigning a valid alias member should not warn, got: {diags:?}"
    );
}

#[test]
fn field_assignment_of_an_invalid_alias_member_still_flags_bt2953() {
    // Companion negative case for the fix above: an assignment that is
    // genuinely NOT a member of the alias's expansion must still be
    // flagged — the fix expands the declared type for comparison, it must
    // not turn the check into a no-op.
    let source = r"
type RestartStrategy = #temporary | #transient | #permanent

Actor subclass: Supervisor
  state: policy :: RestartStrategy = #temporary

  reset =>
    self.policy := #bogus
";
    let diags = analyse_diagnostics(source);
    let hits: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("Type mismatch: field"))
        .collect();
    assert_eq!(
        hits.len(),
        1,
        "assigning a value that is not a member of the alias should still \
         warn, got: {diags:?}"
    );
    assert!(
        hits[0]
            .message
            .contains("RestartStrategy (#temporary | #transient | #permanent)"),
        "display should still name the alias, got: {}",
        hits[0].message
    );
}

#[test]
fn spawn_with_value_mismatch_on_alias_typed_slot_names_the_alias() {
    // `check_spawn_with_value` (validation.rs) sits on the same
    // pre-ADR-0108 `state_field_type` `EcoString` boundary as
    // `check_field_assignment` — found during BT-2911's own adversarial
    // review as a fourth EcoString-based display path the issue's three
    // named call sites didn't enumerate. `C spawnWith: #{ slot: value }`
    // must name the alias in its "type mismatch for state key ..." message
    // too.
    let source = r"
type RestartStrategy = #temporary | #transient | #permanent

Actor subclass: Supervisor
  state: policy :: RestartStrategy = #temporary

Supervisor spawnWith: #{#policy => 42}
";
    let diags = analyse_diagnostics(source);
    let hits: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("type mismatch for state key"))
        .collect();
    assert_eq!(hits.len(), 1, "expected one diagnostic, got: {diags:?}");
    assert!(
        hits[0]
            .message
            .contains("RestartStrategy (#temporary | #transient | #permanent)"),
        "should name the alias with its expansion, got: {}",
        hits[0].message
    );
}
