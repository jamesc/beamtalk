// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0106: opt-in **asserted** `matchExhaustive:` exhaustiveness.
//!
//! Covers the two `Error`-severity failure modes — a non-empty residual on a
//! closed singleton union, and a non-closed scrutinee (the "cannot verify"
//! loud failure) — plus the regression pin that the advisory `match:`
//! (`Warning`, never `Error`) is completely unaffected by this change.
//!
//! Mirrors the fixture/helper shape of `singleton_match_exhaustiveness.rs`
//! (the advisory test file this one sits beside) so the two suites read as a
//! matched pair.

use super::super::*;
use super::common::*;

use crate::semantic_analysis::ClassHierarchy;
use crate::source_analysis::Severity;
use crate::test_helpers::test_support::wildcard_arm;

// ── Fixtures ────────────────────────────────────────────────────────────

/// An unguarded `#name -> body` match arm.
fn sym_arm(name: &str, body: Expression) -> MatchArm {
    MatchArm::new(
        Pattern::Literal(Literal::Symbol(name.into()), span()),
        body,
        span(),
    )
}

fn asserted_match_expr(value: Expression, arms: Vec<MatchArm>) -> Expression {
    Expression::Match {
        value: Box::new(value),
        arms,
        exhaustive: true,
        span: span(),
    }
}

fn plain_match_expr(value: Expression, arms: Vec<MatchArm>) -> Expression {
    Expression::Match {
        value: Box::new(value),
        arms,
        exhaustive: false,
        span: span(),
    }
}

fn compass_type() -> InferredType {
    InferredType::simple_union(&["#north", "#south", "#east", "#west"])
}

/// Runs inference over an *asserted* `matchExhaustive:` and returns every
/// diagnostic produced (there is at most one: exhaustiveness either passes
/// silently or reports exactly one `Error`).
fn asserted_diagnostics(scrutinee_ty: InferredType, arms: Vec<MatchArm>) -> Vec<Diagnostic> {
    let hierarchy = ClassHierarchy::with_builtins();

    let expr = asserted_match_expr(var("compass"), arms);

    let (_, diags) = infer_with_diags(&expr, &hierarchy, &[("compass", scrutinee_ty)]);
    diags
}

/// Runs inference over the equivalent *advisory* `match:` (same scrutinee
/// and arms, `exhaustive: false`) and returns every "non-exhaustive"
/// warning — used to pin that the advisory path is untouched.
fn advisory_warnings(scrutinee_ty: InferredType, arms: Vec<MatchArm>) -> Vec<Diagnostic> {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut session = InferSession::new();
    session.env.set_local("compass", scrutinee_ty);
    let expr = plain_match_expr(var("compass"), arms);

    let _ = session.infer(&expr, &hierarchy);
    session
        .checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("non-exhaustive"))
        .cloned()
        .collect()
}

// ── Residual computation / firing (Error, not Warning) ──────────────────

/// The asserted counterpart of the ADR 0102 §4 headline example: three of
/// four compass directions covered — `#west` is missing, so `matchExhaustive:`
/// fails at `Error` severity (not `Warning`).
#[test]
fn missing_one_member_of_four_errors_naming_it() {
    let diags = asserted_diagnostics(
        compass_type(),
        vec![
            sym_arm("north", str_lit("up")),
            sym_arm("south", str_lit("down")),
            sym_arm("east", str_lit("right")),
        ],
    );
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic, got: {diags:?}"
    );
    assert_eq!(
        diags[0].severity,
        Severity::Error,
        "matchExhaustive: must be Error, not Warning: {diags:?}"
    );
    assert!(
        diags[0].message.contains("#west"),
        "message should name the missing member #west: {}",
        diags[0].message
    );
    assert!(
        diags[0].message.contains("residual type"),
        "message should report the residual type: {}",
        diags[0].message
    );
    assert!(
        diags[0].message.contains("matchExhaustive:"),
        "message should name the matchExhaustive: keyword: {}",
        diags[0].message
    );
}

/// All four members covered — the residual is `Never`, so `matchExhaustive:`
/// is silently confirmed (no diagnostic at all — success is quiet).
#[test]
fn full_coverage_no_diagnostic() {
    let diags = asserted_diagnostics(
        compass_type(),
        vec![
            sym_arm("north", str_lit("up")),
            sym_arm("south", str_lit("down")),
            sym_arm("east", str_lit("right")),
            sym_arm("west", str_lit("left")),
        ],
    );
    assert!(
        diags.is_empty(),
        "expected no diagnostic with full coverage, got: {diags:?}"
    );
}

/// An unguarded `_ ->` wildcard arm is full coverage under `matchExhaustive:`
/// too — a wildcard *does* handle every remaining case, so the assertion
/// legitimately passes.
#[test]
fn unguarded_wildcard_satisfies_assertion() {
    let diags = asserted_diagnostics(
        compass_type(),
        vec![
            sym_arm("north", str_lit("up")),
            wildcard_arm(str_lit("other")),
        ],
    );
    assert!(
        diags.is_empty(),
        "expected no diagnostic: unguarded wildcard satisfies the assertion, got: {diags:?}"
    );
}

// ── Non-closed scrutinee: the "cannot verify" loud failure ──────────────

/// `Dynamic` scrutinee — the assertion cannot be verified, so it fails
/// loudly at `Error` severity (the opposite of the advisory path's silence).
#[test]
fn dynamic_scrutinee_errors_loudly() {
    let diags = asserted_diagnostics(
        InferredType::Dynamic(DynamicReason::Unknown),
        vec![sym_arm("north", str_lit("up"))],
    );
    assert_eq!(
        diags.len(),
        1,
        "Dynamic scrutinee under matchExhaustive: must error, got: {diags:?}"
    );
    assert_eq!(diags[0].severity, Severity::Error);
    assert!(
        diags[0].message.contains("cannot verify"),
        "message should explain the assertion could not be verified: {}",
        diags[0].message
    );
}

/// A bare, open `Symbol` (not a closed union of specific singletons) cannot
/// be proven exhaustive either — `Error`, not silence.
#[test]
fn bare_open_symbol_scrutinee_errors_loudly() {
    let diags = asserted_diagnostics(
        InferredType::known("Symbol"),
        vec![sym_arm("north", str_lit("up"))],
    );
    assert_eq!(diags.len(), 1, "expected one error, got: {diags:?}");
    assert_eq!(diags[0].severity, Severity::Error);
    assert!(diags[0].message.contains("Symbol"));
}

/// A `Negation` scrutinee (`Symbol \ #foo`) is open-ended — cannot verify,
/// `Error`.
#[test]
fn negation_scrutinee_errors_loudly() {
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
    let diags = asserted_diagnostics(negation, vec![sym_arm("bar", str_lit("x"))]);
    assert_eq!(diags.len(), 1, "expected one error, got: {diags:?}");
    assert_eq!(diags[0].severity, Severity::Error);
}

/// A union containing any non-singleton member (e.g. a `Dynamic` arm) is not
/// a closed singleton union — cannot verify, `Error`.
#[test]
fn union_with_non_singleton_member_errors_loudly() {
    let mixed = InferredType::Union {
        members: vec![
            InferredType::known("#north"),
            InferredType::Dynamic(DynamicReason::Unknown),
        ],
        provenance: TypeProvenance::Inferred(Span::default()),
    };
    let diags = asserted_diagnostics(mixed, vec![sym_arm("north", str_lit("up"))]);
    assert_eq!(diags.len(), 1, "expected one error, got: {diags:?}");
    assert_eq!(diags[0].severity, Severity::Error);
}

/// A union of ordinary classes (no singletons) cannot be proven exhaustive
/// by this checker — `Error`, naming the actual inferred type so the user
/// knows why.
#[test]
fn union_of_ordinary_classes_errors_loudly() {
    let diags = asserted_diagnostics(
        InferredType::simple_union(&["Integer", "String"]),
        vec![sym_arm("north", str_lit("up"))],
    );
    assert_eq!(diags.len(), 1, "expected one error, got: {diags:?}");
    assert_eq!(diags[0].severity, Severity::Error);
    assert!(diags[0].message.contains("Integer"));
}

// ── Regression: advisory `match:` path is completely unchanged ──────────

/// The same scrutinee/arms that make `matchExhaustive:` fail at `Error`
/// severity still produce only a `Warning` under plain `match:` — its
/// advisory path is untouched by this feature.
#[test]
fn advisory_match_still_warns_not_errors_for_same_gap() {
    let arms = vec![
        sym_arm("north", str_lit("up")),
        sym_arm("south", str_lit("down")),
        sym_arm("east", str_lit("right")),
    ];
    let warnings = advisory_warnings(compass_type(), arms);
    assert_eq!(warnings.len(), 1, "expected one warning, got: {warnings:?}");
    assert_eq!(
        warnings[0].severity,
        Severity::Warning,
        "advisory match: must stay Warning, got: {warnings:?}"
    );
    assert!(warnings[0].message.contains("#west"));
    // The advisory message must not be confused with the asserted one.
    assert!(!warnings[0].message.contains("matchExhaustive:"));
}

/// A `Dynamic` scrutinee stays completely silent under plain `match:` — the
/// advisory check's ADR 0100 conservatism is unaffected by adding the
/// asserted path.
#[test]
fn advisory_match_still_silent_on_dynamic_scrutinee() {
    let warnings = advisory_warnings(
        InferredType::Dynamic(DynamicReason::Unknown),
        vec![sym_arm("north", str_lit("up"))],
    );
    assert!(
        warnings.is_empty(),
        "Dynamic scrutinee under plain match: must stay silent, got: {warnings:?}"
    );
}

// ── `Result` as a closed `ok:` / `error:` pair (BT-5278) ────────────────

fn result_type() -> InferredType {
    InferredType::known_with_args(
        "Result",
        vec![
            InferredType::known("Integer"),
            InferredType::known("String"),
        ],
    )
}

fn result_arm(selector: &str, binding: Pattern, body: Expression) -> MatchArm {
    MatchArm::new(
        Pattern::Constructor {
            class: Identifier::new("Result", span()),
            keywords: vec![(Identifier::new(selector, span()), binding)],
            span: span(),
        },
        body,
        span(),
    )
}

#[test]
fn result_scrutinee_with_ok_and_error_arms_is_accepted() {
    let diags = asserted_diagnostics(
        result_type(),
        vec![
            result_arm(
                "ok:",
                Pattern::Variable(Identifier::new("v", span())),
                str_lit("a"),
            ),
            result_arm("error:", Pattern::Wildcard(span()), str_lit("b")),
        ],
    );
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:?}");
}

#[test]
fn result_scrutinee_with_redundant_wildcard_warns_unreachable() {
    let diags = asserted_diagnostics(
        result_type(),
        vec![
            result_arm(
                "ok:",
                Pattern::Variable(Identifier::new("v", span())),
                str_lit("a"),
            ),
            result_arm("error:", Pattern::Wildcard(span()), str_lit("b")),
            wildcard_arm(str_lit("c")),
        ],
    );
    assert_eq!(diags.len(), 1, "got: {diags:?}");
    assert_eq!(diags[0].severity, Severity::Warning);
    assert!(diags[0].message.contains("unreachable"), "{diags:?}");
}
