// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-2745 / ADR 0102 §4: advisory `match:` exhaustiveness for singleton-union
//! scrutinees.
//!
//! Covers the residual computation (`difference(scrutinee, covered)`), the
//! gating (fires only on a known-closed singleton union — silent on
//! `Dynamic`, bare/open `Symbol`, `Negation`, and a union with any
//! non-singleton member), and the severity (`Warning`, never `Error`).
//!
//! Also pins that BT-1299's pattern-based sealed-constructor exhaustiveness
//! error (`validators::match_validators::check_match_exhaustiveness`) is
//! untouched by this change — it lives in a separate module and is not
//! exercised by this file at all, so its own test suite
//! (`validators::match_validators::tests`) remains the regression pin.

use super::super::*;
use super::common::*;

use crate::semantic_analysis::ClassHierarchy;
use crate::source_analysis::Severity;

// ── Fixtures ────────────────────────────────────────────────────────────

/// An unguarded `#name -> body` match arm.
fn sym_arm(name: &str, body: Expression) -> MatchArm {
    MatchArm::new(
        Pattern::Literal(Literal::Symbol(name.into()), span()),
        body,
        span(),
    )
}

/// A guarded `#name when: [guard] -> body` match arm — does NOT count as
/// coverage (mirrors BT-1299's guarded-arm rule).
fn guarded_sym_arm(name: &str, guard: Expression, body: Expression) -> MatchArm {
    MatchArm::with_guard(
        Pattern::Literal(Literal::Symbol(name.into()), span()),
        guard,
        body,
        span(),
    )
}

/// An unguarded `_ -> body` wildcard arm — full coverage.
fn wildcard_arm(body: Expression) -> MatchArm {
    MatchArm::new(Pattern::Wildcard(span()), body, span())
}

/// A guarded `_ when: [guard] -> body` wildcard arm — does NOT suppress the
/// check (a guard means it will not always match).
fn guarded_wildcard_arm(guard: Expression, body: Expression) -> MatchArm {
    MatchArm::with_guard(Pattern::Wildcard(span()), guard, body, span())
}

fn match_expr(value: Expression, arms: Vec<MatchArm>) -> Expression {
    Expression::Match {
        exhaustive: false,
        value: Box::new(value),
        arms,
        span: span(),
    }
}

fn compass_type() -> InferredType {
    InferredType::simple_union(&["#north", "#south", "#east", "#west"])
}

/// Runs inference over `expr` with `compass` bound to `scrutinee_ty` in the
/// environment, and returns every "non-exhaustive" warning produced.
fn exhaustiveness_warnings(scrutinee_ty: InferredType, arms: Vec<MatchArm>) -> Vec<Diagnostic> {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();
    env.set_local("compass", scrutinee_ty);
    let expr = match_expr(var("compass"), arms);
    let mut checker = TypeChecker::new();
    let _ = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("non-exhaustive"))
        .cloned()
        .collect()
}

// ── Residual computation / firing ───────────────────────────────────────

/// The ADR 0102 §4 headline example: three of four compass directions
/// covered — `#west` is missing, so the checker warns.
#[test]
fn missing_one_member_of_four_warns_naming_it() {
    let warnings = exhaustiveness_warnings(
        compass_type(),
        vec![
            sym_arm("north", str_lit("up")),
            sym_arm("south", str_lit("down")),
            sym_arm("east", str_lit("right")),
        ],
    );
    assert_eq!(
        warnings.len(),
        1,
        "expected exactly one non-exhaustive warning, got: {warnings:?}"
    );
    assert_eq!(warnings[0].severity, Severity::Warning);
    assert!(
        warnings[0].message.contains("#west"),
        "message should name the missing member #west: {}",
        warnings[0].message
    );
    assert!(
        warnings[0].message.contains("residual type"),
        "message should report the residual type: {}",
        warnings[0].message
    );
}

/// All four members covered — the residual is `Never`, so no warning.
#[test]
fn full_coverage_no_warning() {
    let warnings = exhaustiveness_warnings(
        compass_type(),
        vec![
            sym_arm("north", str_lit("up")),
            sym_arm("south", str_lit("down")),
            sym_arm("east", str_lit("right")),
            sym_arm("west", str_lit("left")),
        ],
    );
    assert!(
        warnings.is_empty(),
        "expected no warning with full coverage, got: {warnings:?}"
    );
}

/// An unguarded `_ ->` wildcard arm silences the check entirely, even with
/// only one real arm covered.
#[test]
fn unguarded_wildcard_silences_check() {
    let warnings = exhaustiveness_warnings(
        compass_type(),
        vec![
            sym_arm("north", str_lit("up")),
            wildcard_arm(str_lit("other")),
        ],
    );
    assert!(
        warnings.is_empty(),
        "expected no warning: unguarded wildcard should suppress the check, got: {warnings:?}"
    );
}

/// A *guarded* wildcard does NOT suppress the check — the guard means it
/// will not always match, so the remaining members are still uncovered.
#[test]
fn guarded_wildcard_does_not_silence_check() {
    let warnings = exhaustiveness_warnings(
        compass_type(),
        vec![
            sym_arm("north", str_lit("up")),
            sym_arm("south", str_lit("down")),
            sym_arm("east", str_lit("right")),
            guarded_wildcard_arm(var("flag"), str_lit("other")),
        ],
    );
    assert_eq!(
        warnings.len(),
        1,
        "expected a warning: guarded wildcard must not suppress, got: {warnings:?}"
    );
    assert!(warnings[0].message.contains("#west"));
}

/// A *guarded* symbol arm does not count as coverage of that member — the
/// guard means the arm will not always match `#west`.
#[test]
fn guarded_symbol_arm_not_counted_as_coverage() {
    let warnings = exhaustiveness_warnings(
        compass_type(),
        vec![
            sym_arm("north", str_lit("up")),
            sym_arm("south", str_lit("down")),
            sym_arm("east", str_lit("right")),
            guarded_sym_arm("west", var("flag"), str_lit("left")),
        ],
    );
    assert_eq!(
        warnings.len(),
        1,
        "expected a warning: guarded `#west` arm should not count as coverage, got: {warnings:?}"
    );
    assert!(warnings[0].message.contains("#west"));
}

/// No arms at all cover any singleton — the whole domain is missing, and
/// the residual names every member.
#[test]
fn no_singleton_arms_reports_full_domain_missing() {
    let warnings = exhaustiveness_warnings(compass_type(), vec![sym_arm("wat", str_lit("?"))]);
    // "wat" is not a member of the compass union, so nothing is subtracted —
    // all four original members remain in the residual.
    assert_eq!(warnings.len(), 1, "expected one warning, got: {warnings:?}");
    for member in ["#north", "#south", "#east", "#west"] {
        assert!(
            warnings[0].message.contains(member),
            "residual should still list {member}: {}",
            warnings[0].message
        );
    }
}

/// A two-member union missing its second member — the smallest possible
/// repro of the residual computation.
#[test]
fn two_member_union_missing_second_member_warns() {
    let warnings = exhaustiveness_warnings(
        InferredType::simple_union(&["#ok", "#error"]),
        vec![sym_arm("ok", str_lit("success"))],
    );
    assert_eq!(warnings.len(), 1, "expected one warning, got: {warnings:?}");
    assert!(warnings[0].message.contains("#error"));
}

// ── Gating (ADR 0100 conservatism) ──────────────────────────────────────

/// `Dynamic` scrutinee — the checker cannot enumerate any members, so it
/// must stay silent (open world).
#[test]
fn dynamic_scrutinee_is_silent() {
    let warnings = exhaustiveness_warnings(
        InferredType::Dynamic(DynamicReason::Unknown),
        vec![sym_arm("north", str_lit("up"))],
    );
    assert!(
        warnings.is_empty(),
        "Dynamic scrutinee must never warn, got: {warnings:?}"
    );
}

/// A bare, open `Symbol` (not a closed union of specific singletons) is an
/// open-ended atom set — silent, same as `check_impossible_singleton_comparison`.
#[test]
fn bare_open_symbol_scrutinee_is_silent() {
    let warnings = exhaustiveness_warnings(
        InferredType::known("Symbol"),
        vec![sym_arm("north", str_lit("up"))],
    );
    assert!(
        warnings.is_empty(),
        "bare Symbol scrutinee must never warn, got: {warnings:?}"
    );
}

/// A `Negation` scrutinee (`Symbol \ #foo`, a co-finite atom set from a prior
/// narrowing) is open-ended — silent.
#[test]
fn negation_scrutinee_is_silent() {
    let negation = InferredType::difference(
        &InferredType::known("Symbol"),
        &InferredType::known("#foo"),
        TypeProvenance::Inferred(Span::default()),
        None,
    );
    assert!(
        matches!(negation, InferredType::Negation { .. }),
        "fixture sanity check: expected a Negation, got: {negation:?}"
    );
    let warnings = exhaustiveness_warnings(negation, vec![sym_arm("bar", str_lit("x"))]);
    assert!(
        warnings.is_empty(),
        "Negation scrutinee must never warn, got: {warnings:?}"
    );
}

/// A union containing so much as one non-singleton member (here, a
/// `Dynamic` arm folded directly into the stored `Union`, bypassing
/// `union_of`'s "Dynamic absorbs the whole union" collapse) downgrades the
/// *whole* union to open, per ADR 0100 Rule 1 — silent, not a partial hint.
#[test]
fn union_with_non_singleton_member_is_silent() {
    let mixed = InferredType::Union {
        members: vec![
            InferredType::known("#north"),
            InferredType::Dynamic(DynamicReason::Unknown),
        ],
        provenance: TypeProvenance::Inferred(Span::default()),
    };
    let warnings = exhaustiveness_warnings(mixed, vec![sym_arm("north", str_lit("up"))]);
    assert!(
        warnings.is_empty(),
        "union with a non-singleton member must never warn, got: {warnings:?}"
    );
}

/// A union of ordinary classes (no singletons at all) is not a singleton
/// union — silent (this check never touches BT-1299's territory).
#[test]
fn union_of_ordinary_classes_is_silent() {
    let warnings = exhaustiveness_warnings(
        InferredType::simple_union(&["Integer", "String"]),
        vec![sym_arm("north", str_lit("up"))],
    );
    assert!(
        warnings.is_empty(),
        "non-singleton union must never warn, got: {warnings:?}"
    );
}

// ── BT-1299 regression: pattern-based sealed-constructor check untouched ──

/// BT-1299's pattern-based check is a completely separate validator
/// (`validators::match_validators::check_match_exhaustiveness`) that this
/// change does not call, modify, or gate. This test pins that the new
/// type-based check does not fire for (and does not interfere with) a
/// `Result`-constructor-pattern match, which has no singleton-union
/// scrutinee type at all.
#[test]
fn result_constructor_match_is_not_touched_by_singleton_check() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();
    let expr = match_expr(
        var("r"),
        vec![MatchArm::new(
            Pattern::Constructor {
                class: ident("Result"),
                keywords: vec![(ident("ok:"), Pattern::Variable(ident("v")))],
                span: span(),
            },
            var("v"),
            span(),
        )],
    );
    let mut checker = TypeChecker::new();
    let _ = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    let non_exhaustive: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("non-exhaustive"))
        .collect();
    assert!(
        non_exhaustive.is_empty(),
        "the type-based singleton check must not fire for a non-singleton-union \
         scrutinee, got: {non_exhaustive:?}"
    );
}

#[test]
fn unguarded_variable_binding_arm_silences_check() {
    // An unguarded variable-binding arm (`direction -> ...`) always matches —
    // coverage-equivalent to `_ ->` — so no warning even with members uncovered.
    let warnings = exhaustiveness_warnings(
        compass_type(),
        vec![
            sym_arm("north", str_lit("up")),
            MatchArm::new(
                Pattern::Variable(crate::ast::Identifier::new("direction", span())),
                str_lit("other"),
                span(),
            ),
        ],
    );
    assert!(
        warnings.is_empty(),
        "expected variable-binding catch-all to silence the check, got: {warnings:?}"
    );
}
