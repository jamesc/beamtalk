// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-2856 / ADR 0107 Phase A: `nil`/`Type`-pattern exhaustiveness.
//!
//! Extends BT-2745's advisory `Warning` / BT-2763's asserted `Error`
//! `matchExhaustive:` machinery to reach closed `Known | Nil` unions and
//! small closed unions of concrete leaf classes, covered by `nil` and
//! `binding :: ClassName` arms — reusing the exact severity split verbatim
//! (see `singleton_match_exhaustiveness.rs` / `asserted_match_exhaustiveness.rs`,
//! the sibling suites this one is modelled on).

use super::common::*;

use crate::semantic_analysis::ClassHierarchy;
use crate::source_analysis::Severity;

// ── Fixtures ────────────────────────────────────────────────────────────

/// An unguarded `nil -> body` arm.
fn nil_arm(body: Expression) -> MatchArm {
    MatchArm::new(Pattern::Nil(span()), body, span())
}

/// A guarded `nil when: [guard] -> body` arm — does NOT count as coverage.
fn guarded_nil_arm(guard: Expression, body: Expression) -> MatchArm {
    MatchArm::with_guard(Pattern::Nil(span()), guard, body, span())
}

/// An unguarded `binding :: ClassName -> body` arm.
fn type_arm(binding: &str, class: &str, body: Expression) -> MatchArm {
    MatchArm::new(
        Pattern::Type {
            binding: ident(binding),
            class: ident(class),
            span: span(),
        },
        body,
        span(),
    )
}

/// A guarded `binding :: ClassName when: [guard] -> body` arm — does NOT
/// count as coverage.
fn guarded_type_arm(binding: &str, class: &str, guard: Expression, body: Expression) -> MatchArm {
    MatchArm::with_guard(
        Pattern::Type {
            binding: ident(binding),
            class: ident(class),
            span: span(),
        },
        guard,
        body,
        span(),
    )
}

/// An unguarded `_ -> body` wildcard arm — full coverage.
fn wildcard_arm(body: Expression) -> MatchArm {
    MatchArm::new(Pattern::Wildcard(span()), body, span())
}

fn match_expr(exhaustive: bool, value: Expression, arms: Vec<MatchArm>) -> Expression {
    Expression::Match {
        exhaustive,
        value: Box::new(value),
        arms,
        span: span(),
    }
}

/// `raw :: String | Nil`.
fn string_or_nil() -> InferredType {
    InferredType::simple_union(&["String", "UndefinedObject"])
}

/// Runs inference with `raw` bound to `scrutinee_ty` and returns every
/// diagnostic produced by a plain `match:` (`exhaustive: false`).
fn advisory_diagnostics(scrutinee_ty: InferredType, arms: Vec<MatchArm>) -> Vec<Diagnostic> {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();
    env.set_local("raw", scrutinee_ty);
    let expr = match_expr(false, var("raw"), arms);
    let mut checker = TypeChecker::new();
    let _ = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("non-exhaustive"))
        .cloned()
        .collect()
}

/// Runs inference with `raw` bound to `scrutinee_ty` and returns every
/// diagnostic produced by `matchExhaustive:` (`exhaustive: true`).
fn asserted_diagnostics(scrutinee_ty: InferredType, arms: Vec<MatchArm>) -> Vec<Diagnostic> {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut env = TypeEnv::new();
    env.set_local("raw", scrutinee_ty);
    let expr = match_expr(true, var("raw"), arms);
    let mut checker = TypeChecker::new();
    let _ = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    checker.diagnostics().to_vec()
}

// ── ADR 0107 headline example: `String | Nil` missing the `nil` arm ────

/// `raw :: String | Nil; raw matchExhaustive: [s :: String -> s size]` —
/// missing the `nil` arm — is an `Error`, naming the residual `Nil`.
#[test]
fn asserted_missing_nil_arm_errors_naming_residual_nil() {
    let diags = asserted_diagnostics(string_or_nil(), vec![type_arm("s", "String", var("s"))]);
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic, got: {diags:?}"
    );
    assert_eq!(diags[0].severity, Severity::Error);
    assert!(
        diags[0].message.contains("matchExhaustive:"),
        "message should name the matchExhaustive: keyword: {}",
        diags[0].message
    );
    assert!(
        diags[0].message.contains("`Nil`"),
        "message should name the missing `Nil` arm (not `UndefinedObject`): {}",
        diags[0].message
    );
    assert!(
        !diags[0].message.contains("UndefinedObject"),
        "message must not leak the internal `UndefinedObject` class name: {}",
        diags[0].message
    );
}

/// The same missing-arm case via plain `match:` produces a `Warning`, not an
/// `Error` — the advisory/asserted severity split holds for this new
/// exhaustiveness reach exactly as it does for the existing symbol-union
/// case.
#[test]
fn advisory_missing_nil_arm_warns_not_errors() {
    let diags = advisory_diagnostics(string_or_nil(), vec![type_arm("s", "String", var("s"))]);
    assert_eq!(
        diags.len(),
        1,
        "expected exactly one diagnostic, got: {diags:?}"
    );
    assert_eq!(
        diags[0].severity,
        Severity::Warning,
        "plain match: must stay Warning, got: {diags:?}"
    );
    assert!(diags[0].message.contains("`Nil`"));
}

/// A fully-covered `nil` + `s :: String` produces no diagnostic from either
/// variant.
#[test]
fn full_coverage_nil_and_type_arm_silent_for_both_keywords() {
    let arms = vec![nil_arm(int_lit(0)), type_arm("s", "String", var("s"))];
    assert!(
        advisory_diagnostics(string_or_nil(), arms.clone()).is_empty(),
        "expected no advisory warning with full nil+String coverage"
    );
    assert!(
        asserted_diagnostics(string_or_nil(), arms).is_empty(),
        "expected no asserted error with full nil+String coverage"
    );
}

/// Coverage via a `Type` arm plus a wildcard (instead of an explicit `nil`
/// arm) is also full coverage — the wildcard catches `nil` too.
#[test]
fn type_arm_plus_wildcard_is_full_coverage() {
    let arms = vec![type_arm("s", "String", var("s")), wildcard_arm(int_lit(0))];
    assert!(advisory_diagnostics(string_or_nil(), arms.clone()).is_empty());
    assert!(asserted_diagnostics(string_or_nil(), arms).is_empty());
}

// ── Guarded arms do not count as coverage ───────────────────────────────

/// A guarded `nil when: [...] ->` arm does not guarantee coverage of `nil`.
#[test]
fn guarded_nil_arm_not_counted_as_coverage() {
    let diags = asserted_diagnostics(
        string_or_nil(),
        vec![
            guarded_nil_arm(var("flag"), int_lit(0)),
            type_arm("s", "String", var("s")),
        ],
    );
    assert_eq!(diags.len(), 1, "expected one error, got: {diags:?}");
    assert_eq!(diags[0].severity, Severity::Error);
    assert!(diags[0].message.contains("`Nil`"));
}

/// A guarded `binding :: ClassName when: [...] ->` arm does not guarantee
/// coverage of `ClassName`.
#[test]
fn guarded_type_arm_not_counted_as_coverage() {
    let diags = asserted_diagnostics(
        string_or_nil(),
        vec![
            nil_arm(int_lit(0)),
            guarded_type_arm("s", "String", var("flag"), var("s")),
        ],
    );
    assert_eq!(diags.len(), 1, "expected one error, got: {diags:?}");
    assert_eq!(diags[0].severity, Severity::Error);
    assert!(diags[0].message.contains("String"));
}

// ── Small closed unions of concrete leaf classes (no `Nil` involved) ────

/// `Integer | String`, fully covered by two `Type` arms (no `nil` involved
/// at all) — exhaustive, silent under both keywords.
#[test]
fn closed_leaf_class_union_without_nil_full_coverage_is_silent() {
    let ty = InferredType::simple_union(&["Integer", "String"]);
    let arms = vec![
        type_arm("n", "Integer", var("n")),
        type_arm("s", "String", var("s")),
    ];
    assert!(advisory_diagnostics(ty.clone(), arms.clone()).is_empty());
    assert!(asserted_diagnostics(ty, arms).is_empty());
}

/// The same union missing its `String` arm is non-exhaustive.
#[test]
fn closed_leaf_class_union_without_nil_missing_arm_errors() {
    let ty = InferredType::simple_union(&["Integer", "String"]);
    let diags = asserted_diagnostics(ty, vec![type_arm("n", "Integer", var("n"))]);
    assert_eq!(diags.len(), 1, "expected one error, got: {diags:?}");
    assert_eq!(diags[0].severity, Severity::Error);
    assert!(diags[0].message.contains("String"));
}

// ── Gating: existing symbol-union / ordinary-class-union behaviour is
//    unaffected by this new mechanism ────────────────────────────────────

/// A `match:`/`matchExhaustive:` whose scrutinee happens to be a closed
/// union of leaf classes, but whose arms are ordinary symbol patterns (no
/// `nil`/`Type` pattern anywhere), must not newly start warning/erroring —
/// this mechanism only engages when the arms actually use `nil`/`Type`
/// patterns (`arms_use_nil_or_type_pattern`). Pins the `union_of_ordinary_classes_*`
/// regression from the sibling suites stays true even now that the scrutinee
/// type itself would otherwise qualify as a closed leaf-class union.
#[test]
fn ordinary_class_union_with_symbol_arms_is_unaffected() {
    let ty = InferredType::simple_union(&["Integer", "String"]);
    let sym_arm = MatchArm::new(
        Pattern::Literal(Literal::Symbol("north".into()), span()),
        str_lit("up"),
        span(),
    );
    assert!(
        advisory_diagnostics(ty.clone(), vec![sym_arm.clone()]).is_empty(),
        "no nil/Type pattern present — must stay silent under match:"
    );
    let asserted = asserted_diagnostics(ty, vec![sym_arm]);
    assert_eq!(asserted.len(), 1, "expected one error, got: {asserted:?}");
    assert!(
        asserted[0].message.contains("cannot verify"),
        "must fall back to the 'cannot verify' loud failure, not a residual \
         computation over irrelevant symbol arms: {}",
        asserted[0].message
    );
}

/// A non-leaf class in a `Type` arm (has subclasses) must not be treated as
/// closed — `matchExhaustive:` falls back to "cannot verify" rather than
/// asserting a false exhaustiveness proof. Mirrors
/// `match_validators`'s own "has subclasses" compile-error restriction
/// (BT-2854): the two mechanisms must never disagree about which classes
/// are closed.
#[test]
fn non_leaf_class_type_arm_is_not_closed() {
    let hierarchy_src = "Object subclass: Shape\nShape subclass: Circle\n";
    let (module, _) =
        crate::source_analysis::parse(crate::source_analysis::lex_with_eof(hierarchy_src));
    let (hierarchy, _) = ClassHierarchy::build_with_options(&module, false);
    let hierarchy = hierarchy.unwrap();

    let mut env = TypeEnv::new();
    env.set_local(
        "raw",
        InferredType::simple_union(&["Shape", "UndefinedObject"]),
    );
    let expr = match_expr(
        true,
        var("raw"),
        vec![nil_arm(int_lit(0)), type_arm("s", "Shape", var("s"))],
    );
    let mut checker = TypeChecker::new();
    let _ = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    let diags = checker.diagnostics();
    assert_eq!(diags.len(), 1, "expected one error, got: {diags:?}");
    assert_eq!(diags[0].severity, Severity::Error);
    assert!(
        diags[0].message.contains("cannot verify"),
        "a non-leaf class must not be treated as a closed union member: {}",
        diags[0].message
    );
}

// ── Regression: existing BT-2745/BT-1299/ADR-0106 behaviour is untouched ─

/// A closed `#symbol` singleton union with symbol arms behaves exactly as
/// before (BT-2745) — this new mechanism is additive, not a replacement.
#[test]
fn symbol_singleton_union_behaviour_is_unaffected() {
    let ty = InferredType::simple_union(&["#north", "#south"]);
    let arm = MatchArm::new(
        Pattern::Literal(Literal::Symbol("north".into()), span()),
        str_lit("up"),
        span(),
    );
    let warnings = advisory_diagnostics(ty.clone(), vec![arm.clone()]);
    assert_eq!(warnings.len(), 1, "expected one warning, got: {warnings:?}");
    assert_eq!(warnings[0].severity, Severity::Warning);
    assert!(warnings[0].message.contains("#south"));
}
