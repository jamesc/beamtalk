// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Match exhaustiveness validators.
//!
//! **DDD Context:** Semantic Analysis
//!
//! Validates pattern match exhaustiveness for sealed types (BT-1299).

use crate::ast::{Expression, Identifier, Module, Pattern};
use crate::ast_walker::walk_module;
use crate::semantic_analysis::ClassHierarchy;
use crate::source_analysis::{Diagnostic, DiagnosticCategory};

// ── BT-1299: Match exhaustiveness for sealed types ────────────────────────────

/// BT-1299: Error when a `match:` on a sealed type omits a known constructor
/// variant and has no wildcard arm.
///
/// Only applies to stdlib sealed types with a known complete variant set
/// (Phase 1: `Result`). Wildcard `_` suppresses the check.
///
/// # Design: pattern-based, not type-based
///
/// Beamtalk is dynamically typed — there is no resolved scrutinee type available
/// at compile time. The check is therefore keyed on the *constructor patterns in
/// the arms*, not on the static type of the matched expression:
///
/// - If any arm contains a `Result ok:` or `Result error:` constructor pattern,
///   the programmer has asserted that the value can be a `Result`, and the check
///   requires full coverage (or a wildcard `_` escape hatch).
/// - If the scrutinee happens to be a non-`Result` value, using `Result`
///   constructor patterns on it is already a programmer error (the patterns will
///   never match at runtime). The exhaustiveness error is an additional signal
///   that coverage is incomplete; the wildcard `_` arm is the correct way to opt
///   out of the check.
pub(crate) fn check_match_exhaustiveness(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    walk_module(module, &mut |expr| {
        visit_match_exhaustiveness(expr, diagnostics);
    });
}

/// Returns all constructor selectors for a sealed stdlib type, or `None` if the
/// type is not a known sealed type (exhaustiveness check does not apply).
fn sealed_type_all_constructors(class: &str) -> Option<&'static [&'static str]> {
    match class {
        "Result" => Some(&["ok:", "error:"]),
        _ => None,
    }
}

/// Visitor for match exhaustiveness (BT-1299).
fn visit_match_exhaustiveness(expr: &Expression, diagnostics: &mut Vec<Diagnostic>) {
    let Expression::Match { arms, span, .. } = expr else {
        return;
    };

    // An unguarded wildcard arm suppresses exhaustiveness checking.
    // A guarded wildcard (`_ when: [cond] -> body`) is conditional and does
    // NOT guarantee coverage of all remaining cases.
    if arms
        .iter()
        .any(|arm| arm.guard.is_none() && matches!(arm.pattern, Pattern::Wildcard(_)))
    {
        return;
    }

    // Collect covered constructor selectors by class name.
    // Only unguarded arms count: a guarded constructor arm (`Result ok: v when: [v > 0] -> ...`)
    // is conditional and does not guarantee coverage of that variant.
    let mut covered: std::collections::HashMap<&str, Vec<String>> =
        std::collections::HashMap::new();
    for arm in arms {
        if arm.guard.is_some() {
            continue;
        }
        if let Pattern::Constructor {
            class, keywords, ..
        } = &arm.pattern
        {
            let selector: String = keywords.iter().map(|(kw, _)| kw.name.as_str()).collect();
            covered
                .entry(class.name.as_str())
                .or_default()
                .push(selector);
        }
    }

    // For each sealed class with constructor arms, check that all variants are covered.
    for (class_name, used_selectors) in &covered {
        let Some(all_selectors) = sealed_type_all_constructors(class_name) else {
            // Not a known sealed type — codegen already emits an error for unknown classes.
            continue;
        };

        let missing: Vec<&str> = all_selectors
            .iter()
            .copied()
            .filter(|sel| !used_selectors.iter().any(|u| u == *sel))
            .collect();

        if !missing.is_empty() {
            let missing_str = missing.join(", ");
            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "Non-exhaustive match: on sealed type `{class_name}` — \
                         missing arm{}: {missing_str}.",
                        if missing.len() == 1 { "" } else { "s" }
                    ),
                    *span,
                )
                .with_hint(
                    "Add the missing arm(s), or add a wildcard `_ ->` arm to handle \
                     remaining cases."
                        .to_string(),
                ),
            );
        }
    }
}

// ── Assignment-in-match-arm footgun warning ─────────────────────────────────

/// Warn when a match arm body contains a local variable assignment (`:=`).
///
/// Core Erlang `case` arm bodies are single expressions — `let` bindings
/// inside an arm do not escape to the enclosing scope.  A local assignment
/// like `result := "success"` inside a match arm silently evaluates to the
/// RHS value without updating the variable for code after the match.
///
/// The correct pattern is to capture the match result:
/// ```beamtalk
/// result := value match: [
///   #ok -> "success";
///   _ -> "default"
/// ]
/// ```
pub(crate) fn warn_assignment_in_match_arms(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    walk_module(module, &mut |expr| {
        visit_assignment_in_match_arm(expr, diagnostics);
    });
}

fn visit_assignment_in_match_arm(expr: &Expression, diagnostics: &mut Vec<Diagnostic>) {
    let Expression::Match { arms, .. } = expr else {
        return;
    };

    for arm in arms {
        if let Expression::Assignment { target, span, .. } = &arm.body {
            match target.as_ref() {
                Expression::Identifier(id) => {
                    diagnostics.push(
                        Diagnostic::warning(
                            format!(
                                "Assignment to `{}` inside a match arm has no effect — \
                                 the variable is not updated after the match expression.",
                                id.name,
                            ),
                            *span,
                        )
                        .with_hint(format!(
                            "Capture the match result instead: `{} := value match: [...]`",
                            id.name,
                        ))
                        .with_category(DiagnosticCategory::DeadAssignment),
                    );
                }
                Expression::FieldAccess {
                    receiver, field, ..
                } if matches!(
                    receiver.as_ref(),
                    Expression::Identifier(r) if r.name == "self"
                ) =>
                {
                    diagnostics.push(
                        Diagnostic::warning(
                            format!(
                                "Assignment to `self.{}` inside a match arm has no effect — \
                                 the state update is lost after the match expression.",
                                field.name,
                            ),
                            *span,
                        )
                        .with_hint(
                            "Move the field assignment outside the match, \
                             or use an `ifTrue:ifFalse:` chain instead."
                                .to_string(),
                        )
                        .with_category(DiagnosticCategory::DeadAssignment),
                    );
                }
                _ => {}
            }
        }
    }
}

// ── BT-2854 / ADR 0107 Phase A: `Pattern::Type` class validation ───────────

/// Stdlib primitives ADR 0107 Phase A explicitly names as supported
/// `Pattern::Type` classes (Decision §Phase A scope), each with its own
/// dedicated BIF-test codegen strategy once BT-2855 lands.
///
/// These are exempted from the general hierarchy-based leaf check below:
/// several of them are *nominally* non-leaf in `ClassHierarchy` for reasons
/// that have nothing to do with runtime dispatch ambiguity — `Boolean` has
/// stdlib subclasses `True`/`False` (`stdlib/src/True.bt`,
/// `stdlib/src/False.bt`) purely to hang `ifTrue:`/`ifFalse:` double-dispatch
/// methods off of, not because a plain `true`/`false` atom carries any
/// distinguishing runtime tag — Phase A's `x :: Boolean` still means exactly
/// "is this value `'true'` or `'false'`", tested as one BIF-style guard, not
/// a class-tag test that could plausibly need Phase B's subclass dispatch.
/// This list is the authority for which primitives get that treatment; it is
/// intentionally not derived from the hierarchy (unlike the general leaf
/// check), because "is a Phase A primitive" and "is a hierarchy leaf" are
/// different questions that happen to coincide for ordinary user classes.
const PHASE_A_PRIMITIVE_CLASSES: &[&str] = &[
    "String",
    "Integer",
    "Float",
    "List",
    "Dictionary",
    "Boolean",
    "Symbol",
];

/// BT-2856 / ADR 0107 Phase A: is `class_name` a concrete, exhaustively
/// enumerable leaf class in the exact sense `Pattern::Type` patterns already
/// require — the identical leaf check `validate_type_pattern_class`'s
/// "has subclasses" compile error enforces below, factored out so the
/// `Nil`/`Type`-pattern exhaustiveness residual computation in
/// `type_checker::inference` (`is_closed_leaf_type_union`) can never disagree
/// with what a `Type` pattern arm is legally allowed to test: a class this
/// function accepts is always one a `binding :: class_name` arm could
/// legally appear in, and vice versa.
///
/// `Character` is excluded (shares `Integer`'s runtime representation, no
/// distinguishing tag — see `validate_type_pattern_class`'s doc), and an
/// unresolved class name is conservatively rejected (an exhaustiveness
/// *gate* must not assume open-world unknowns are closed).
pub(crate) fn is_concrete_leaf_class(class_name: &str, hierarchy: &ClassHierarchy) -> bool {
    if class_name == "Character" {
        return false;
    }
    if PHASE_A_PRIMITIVE_CLASSES.contains(&class_name) {
        return true;
    }
    if !hierarchy.has_class(class_name) {
        return false;
    }
    if hierarchy.is_supervisor_subclass(class_name)
        || hierarchy.is_dynamic_supervisor_subclass(class_name)
    {
        return false;
    }
    hierarchy.direct_subclasses(class_name).is_empty()
}

/// BT-2854 / ADR 0107 Phase A: validate the class name in every
/// `Pattern::Type` (`binding :: ClassName`) arm of a `match:` expression,
/// including type patterns nested inside container patterns (`Tuple`,
/// `Array`, `List`, `Map`, `Constructor` keywords) — a `Pattern::Type` can
/// appear anywhere a sub-pattern can, not just as an arm's top-level pattern.
///
/// This is purely a semantic-analysis-stage check — `Pattern::Type` has no
/// codegen yet (that lands with bindings/narrowing in BT-2855), so there is
/// nothing to gate at codegen time. Three checks, in order:
///
/// - **`Character`** is never accepted, known or not: it shares `Integer`'s
///   runtime representation with no distinct tag (ADR 0107 §Phase A scope),
///   so `x :: Character` could never be distinguished from `x :: Integer` at
///   runtime. (Unlike `Boolean`, above, `Character` genuinely cannot be
///   disambiguated from `Integer` by any runtime test — it compiles to a
///   plain Erlang integer, not merely a nominal hierarchy child.)
/// - **Unknown class name** reuses the existing unresolved-class diagnostic
///   verbatim (ADR 0100) — same `Warning` severity, same
///   [`DiagnosticCategory::UnresolvedClass`] category, and the same
///   open-world gating (`has_cross_file_classes`) as
///   [`super::structural_validators::check_unresolved_classes`]: without
///   cross-file metadata loaded, any class reference might be a
///   not-yet-seen cross-file dependency, so the check stays silent.
/// - **Non-leaf class** (has one or more subclasses) is a compile `Error` —
///   Phase A only supports concrete/leaf classes; matching on a class with
///   subclasses needs either compile-time subclass enumeration or a wrapped
///   runtime dispatch call, deferred to ADR 0107 Phase B. Classes in
///   [`PHASE_A_PRIMITIVE_CLASSES`] are exempt from this check (see its doc).
pub(crate) fn check_type_pattern_classes(
    module: &Module,
    hierarchy: &ClassHierarchy,
    has_cross_file_classes: bool,
    diagnostics: &mut Vec<Diagnostic>,
) {
    walk_module(module, &mut |expr| {
        visit_type_pattern_classes(expr, hierarchy, has_cross_file_classes, diagnostics);
    });
}

fn visit_type_pattern_classes(
    expr: &Expression,
    hierarchy: &ClassHierarchy,
    has_cross_file_classes: bool,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Expression::Match { arms, .. } = expr else {
        return;
    };

    for arm in arms {
        check_pattern_type_classes(&arm.pattern, hierarchy, has_cross_file_classes, diagnostics);
    }
}

/// Recursively finds every `Pattern::Type` in `pattern` (including ones
/// nested inside container sub-patterns) and validates its class name.
fn check_pattern_type_classes(
    pattern: &Pattern,
    hierarchy: &ClassHierarchy,
    has_cross_file_classes: bool,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match pattern {
        Pattern::Type { class, .. } => {
            validate_type_pattern_class(class, hierarchy, has_cross_file_classes, diagnostics);
        }
        Pattern::Tuple { elements, .. } => {
            for elem in elements {
                check_pattern_type_classes(elem, hierarchy, has_cross_file_classes, diagnostics);
            }
        }
        Pattern::List { elements, tail, .. } => {
            for elem in elements {
                check_pattern_type_classes(elem, hierarchy, has_cross_file_classes, diagnostics);
            }
            if let Some(tail_pat) = tail {
                check_pattern_type_classes(
                    tail_pat,
                    hierarchy,
                    has_cross_file_classes,
                    diagnostics,
                );
            }
        }
        Pattern::Array { elements, rest, .. } => {
            for elem in elements {
                check_pattern_type_classes(elem, hierarchy, has_cross_file_classes, diagnostics);
            }
            if let Some(rest_pat) = rest {
                check_pattern_type_classes(
                    rest_pat,
                    hierarchy,
                    has_cross_file_classes,
                    diagnostics,
                );
            }
        }
        Pattern::Map { pairs, .. } => {
            for pair in pairs {
                check_pattern_type_classes(
                    &pair.value,
                    hierarchy,
                    has_cross_file_classes,
                    diagnostics,
                );
            }
        }
        Pattern::Binary { segments, .. } => {
            for segment in segments {
                check_pattern_type_classes(
                    &segment.value,
                    hierarchy,
                    has_cross_file_classes,
                    diagnostics,
                );
            }
        }
        Pattern::Constructor { keywords, .. } => {
            for (_, binding) in keywords {
                check_pattern_type_classes(binding, hierarchy, has_cross_file_classes, diagnostics);
            }
        }
        Pattern::Wildcard(_) | Pattern::Literal(_, _) | Pattern::Variable(_) | Pattern::Nil(_) => {}
    }
}

fn validate_type_pattern_class(
    class: &Identifier,
    hierarchy: &ClassHierarchy,
    has_cross_file_classes: bool,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let class_name = class.name.as_str();

    // `Character` shares `Integer`'s runtime representation (no distinct
    // tag) — never valid in a type pattern, regardless of whether it
    // resolves in the hierarchy.
    if class_name == "Character" {
        diagnostics.push(
            Diagnostic::error(
                "`Character` is not supported in a type pattern — it shares `Integer`'s \
                 runtime representation with no distinct tag, so `x :: Character` can never \
                 be distinguished from `x :: Integer` at runtime"
                    .to_string(),
                class.span,
            )
            .with_hint("Use `x :: Integer` instead.".to_string())
            .with_category(DiagnosticCategory::Type),
        );
        return;
    }

    // Phase A's explicitly-named primitives are always valid — see
    // `PHASE_A_PRIMITIVE_CLASSES`'s doc for why they skip the leaf check.
    if PHASE_A_PRIMITIVE_CLASSES.contains(&class_name) {
        return;
    }

    // `Nil` is a legacy alias for the nil class (`UndefinedObject`, BT-2016)
    // recognised in annotation position (`structural_validators`'s
    // `BUILTIN_CLASS_NAMES`), but — unlike `UndefinedObject` itself — it has
    // no entry in `ClassHierarchy` to resolve against, so it would
    // otherwise trip the unresolved-class check below despite being a
    // codegen-supported (BT-2855, `generate_type_pattern`'s exact-atom-match
    // branch), always-valid type-pattern class name.
    if class_name == "Nil" {
        return;
    }

    if !hierarchy.has_class(class_name) {
        // Unknown class — reuse the existing unresolved-class diagnostic
        // (ADR 0100) verbatim, gated the same way as
        // `check_unresolved_classes`: open-world assumption when
        // cross-file metadata isn't loaded.
        if has_cross_file_classes {
            diagnostics.push(
                Diagnostic::warning(format!("Unresolved class `{class_name}`"), class.span)
                    .with_hint(
                        "This class is not defined in the current compilation unit or \
                         standard library. Suppress with @expect unresolved_class if it \
                         exists at runtime.",
                    )
                    .with_category(DiagnosticCategory::UnresolvedClass),
            );
        }
        return;
    }

    // `Supervisor`/`DynamicSupervisor` subclasses reference a supervisor
    // process, tagged `{'beamtalk_supervisor' | 'beamtalk_supervisor_new',
    // ClassName, Module, Pid}` at runtime (`beamtalk_supervisor.erl`) — a
    // third shape distinct from both the tagged-map (`Value`/`Object`) and
    // actor-tuple (`'beamtalk_object'`) strategies `generate_type_pattern`
    // already handles. Rather than risk silently-wrong codegen for a shape
    // this session hasn't fully characterised (e.g. the transient
    // `'beamtalk_supervisor_new'` tag during `class initialize:`), reject
    // explicitly — the same "fail loudly, not silently wrong" choice
    // already made for `Character` above and non-leaf classes below.
    if hierarchy.is_supervisor_subclass(class_name)
        || hierarchy.is_dynamic_supervisor_subclass(class_name)
    {
        diagnostics.push(
            Diagnostic::error(
                format!(
                    "`{class_name}` is a Supervisor/DynamicSupervisor subclass; type patterns \
                     are not yet supported for supervisor references"
                ),
                class.span,
            )
            .with_hint("Use `isKindOf:` guard clauses instead.".to_string())
            .with_category(DiagnosticCategory::Type),
        );
        return;
    }

    // Non-leaf class — Phase A only supports concrete/leaf classes.
    if !hierarchy.direct_subclasses(class_name).is_empty() {
        diagnostics.push(
            Diagnostic::error(
                format!(
                    "`{class_name}` has subclasses; type patterns are not yet supported \
                     for non-leaf classes — see ADR 0107 Phase B"
                ),
                class.span,
            )
            .with_hint(
                "Match on the concrete subclasses instead, or use `isKindOf:` guard clauses."
                    .to_string(),
            )
            .with_category(DiagnosticCategory::Type),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::Severity;
    use crate::source_analysis::lex_with_eof;
    use crate::source_analysis::parse;

    // ── BT-1299: Match exhaustiveness for sealed types ────────────────────────

    /// Missing `error:` arm without wildcard → compile error.
    #[test]
    fn match_exhaustiveness_result_missing_error_arm_is_error() {
        let src = "(Result ok: 42) match: [Result ok: v -> [v + 1]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_match_exhaustiveness(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for missing error: arm, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(
            diagnostics[0].message.contains("error:"),
            "Expected 'error:' in message, got: {}",
            diagnostics[0].message
        );
        assert!(
            diagnostics[0].message.contains("Result"),
            "Expected 'Result' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// Missing `ok:` arm without wildcard → compile error.
    #[test]
    fn match_exhaustiveness_result_missing_ok_arm_is_error() {
        let src = "(Result error: #x) match: [Result error: e -> [e]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_match_exhaustiveness(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for missing ok: arm, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(
            diagnostics[0].message.contains("ok:"),
            "Expected 'ok:' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// Both arms present → no error.
    #[test]
    fn match_exhaustiveness_result_both_arms_no_error() {
        let src = "r match: [Result ok: v -> [v]; Result error: e -> [0]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_match_exhaustiveness(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no error when both Result arms present, got: {diagnostics:?}"
        );
    }

    /// Wildcard arm suppresses exhaustiveness check.
    #[test]
    fn match_exhaustiveness_wildcard_suppresses_check() {
        let src = "r match: [Result ok: v -> [v]; _ -> [0]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_match_exhaustiveness(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no error when wildcard present, got: {diagnostics:?}"
        );
    }

    /// Non-sealed type match (no constructor patterns) → no error.
    #[test]
    fn match_exhaustiveness_non_constructor_match_no_error() {
        let src = "x match: [1 -> [#one]; 2 -> [#two]; _ -> [#other]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_match_exhaustiveness(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no error for non-constructor match, got: {diagnostics:?}"
        );
    }

    /// Non-Result scrutinee with a Result constructor arm still triggers the check.
    ///
    /// Using `Result ok:` on a non-Result value is itself a programmer error
    /// (the pattern will never match at runtime). The exhaustiveness error fires
    /// as an additional signal; the programmer should add a wildcard `_` arm
    /// or cover all Result variants.
    ///
    /// This test pins the pattern-based (not type-based) boundary: the check is
    /// keyed on which constructor patterns appear in the arms, not on the static
    /// type of the scrutinee expression.
    #[test]
    fn match_exhaustiveness_non_result_scrutinee_with_result_arm_is_error() {
        let src = "42 match: [Result ok: v -> v]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_match_exhaustiveness(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error when Result constructor arm appears with non-Result scrutinee, \
             got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(
            diagnostics[0].message.contains("error:"),
            "Expected 'error:' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// Exhaustiveness check fires inside a method body.
    #[test]
    fn match_exhaustiveness_fires_inside_method() {
        let src = "Object subclass: Foo\n  run: r => r match: [Result ok: v -> [v]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_match_exhaustiveness(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error inside method, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
    }

    /// Guarded wildcard does NOT suppress exhaustiveness — the guard means it
    /// won't always match, so missing variants are still uncovered.
    #[test]
    fn match_exhaustiveness_guarded_wildcard_does_not_suppress() {
        let src = "r match: [Result ok: v -> [v]; _ when: [true] -> [0]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_match_exhaustiveness(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error: guarded wildcard should not suppress exhaustiveness, \
             got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(
            diagnostics[0].message.contains("error:"),
            "Expected 'error:' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// Guarded constructor arm does NOT count as coverage — the guard means
    /// it won't match when the guard is false.
    #[test]
    fn match_exhaustiveness_guarded_constructor_arm_not_counted() {
        let src = "r match: [Result ok: v when: [v > 0] -> [v]; Result error: e -> [0]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_match_exhaustiveness(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error: guarded ok: arm should not count as full ok: coverage, \
             got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(
            diagnostics[0].message.contains("ok:"),
            "Expected 'ok:' in message, got: {}",
            diagnostics[0].message
        );
    }

    /// Unguarded wildcard still suppresses exhaustiveness.
    #[test]
    fn match_exhaustiveness_unguarded_wildcard_still_suppresses() {
        let src = "r match: [Result ok: v -> [v]; _ -> [0]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_match_exhaustiveness(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no error: unguarded wildcard should suppress check, got: {diagnostics:?}"
        );
    }

    /// Unguarded constructor arm still counts as coverage.
    #[test]
    fn match_exhaustiveness_unguarded_constructor_arm_counts() {
        let src = "r match: [Result ok: v -> [v]; Result error: _ -> [0]]";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        check_match_exhaustiveness(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no error: both unguarded arms present, got: {diagnostics:?}"
        );
    }

    // ── Assignment-in-match-arm footgun warning ─────────────────────────────

    /// Local assignment inside a match arm body → warning.
    #[test]
    fn assignment_in_match_arm_warns() {
        let src = r#"x match: [#ok -> result := "yes"; _ -> result := "no"]"#;
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        warn_assignment_in_match_arms(&module, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            2,
            "Expected 2 warnings (one per arm), got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(
            diagnostics[0].message.contains("result"),
            "Expected variable name in message, got: {}",
            diagnostics[0].message
        );
    }

    /// No assignment in match arm → no warning.
    #[test]
    fn no_assignment_in_match_arm_no_warning() {
        let src = r#"x match: [#ok -> "yes"; _ -> "no"]"#;
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let mut diagnostics = Vec::new();
        warn_assignment_in_match_arms(&module, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no warnings, got: {diagnostics:?}"
        );
    }

    /// Regression test: referencing a `Pattern::Type` binding in its own arm
    /// body must not raise a spurious "Undefined variable" error. The
    /// binding is registered in `pattern_bindings.rs` even though narrowing
    /// its type to `class` is deferred to BT-2855.
    #[test]
    fn type_pattern_binding_is_not_undefined_variable() {
        let src = "Object subclass: Foo\n  test: x =>\n    x match: [path :: String -> path; _ -> \"\"]\n";
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "parse diags: {parse_diags:?}");
        let result = crate::semantic_analysis::analyse(&module);
        assert!(
            !result
                .diagnostics
                .iter()
                .any(|d| d.message.contains("Undefined variable")),
            "Expected no undefined-variable error for a Pattern::Type binding, got: {:?}",
            result.diagnostics
        );
    }

    // ── BT-2854 / ADR 0107 Phase A: `Pattern::Type` class validation ────────

    fn hierarchy_for(src: &str) -> (crate::ast::Module, ClassHierarchy) {
        let tokens = lex_with_eof(src);
        let (module, parse_diags) = parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let (hierarchy, _) = ClassHierarchy::build_with_options(&module, false);
        (module, hierarchy.unwrap())
    }

    /// A known leaf stdlib class (`String`) → no diagnostics.
    #[test]
    fn type_pattern_known_leaf_class_no_diagnostics() {
        let (module, hierarchy) = hierarchy_for("x match: [s :: String -> s; _ -> \"\"]");
        let mut diagnostics = Vec::new();
        check_type_pattern_classes(&module, &hierarchy, true, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no diagnostics for a known leaf class, got: {diagnostics:?}"
        );
    }

    /// `Integer` is a known Phase A primitive even though `Character` is a
    /// nominal `Integer subclass:` in the stdlib — `PHASE_A_PRIMITIVE_CLASSES`
    /// exempts it from the general hierarchy-based leaf check.
    #[test]
    fn type_pattern_integer_not_flagged_non_leaf() {
        let (module, hierarchy) = hierarchy_for("x match: [n :: Integer -> n; _ -> 0]");
        let mut diagnostics = Vec::new();
        check_type_pattern_classes(&module, &hierarchy, true, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no diagnostics for Integer, got: {diagnostics:?}"
        );
    }

    /// `Boolean` is an explicitly-named Phase A primitive (ADR 0107 Decision
    /// §Phase A scope) even though it has nominal stdlib subclasses `True`/
    /// `False` (`stdlib/src/True.bt`, `stdlib/src/False.bt`) — those exist to
    /// hang `ifTrue:`/`ifFalse:` double-dispatch methods off of, not because
    /// `true`/`false` atoms carry a distinguishing runtime tag. Regression
    /// test for the bug where a naive hierarchy-only leaf check would wrongly
    /// reject `x :: Boolean` as non-leaf.
    #[test]
    fn type_pattern_boolean_not_flagged_non_leaf() {
        let (module, hierarchy) = hierarchy_for("x match: [b :: Boolean -> b; _ -> 0]");
        let mut diagnostics = Vec::new();
        check_type_pattern_classes(&module, &hierarchy, true, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no diagnostics for Boolean, got: {diagnostics:?}"
        );
    }

    /// Unknown class name, cross-file metadata loaded → reuses the existing
    /// unresolved-class diagnostic (ADR 0100) verbatim.
    #[test]
    fn type_pattern_unknown_class_warns_when_cross_file_loaded() {
        let (module, hierarchy) = hierarchy_for("x match: [s :: Sting -> s; _ -> \"\"]");
        let mut diagnostics = Vec::new();
        check_type_pattern_classes(&module, &hierarchy, true, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 diagnostic for unknown class, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert_eq!(
            diagnostics[0].category,
            Some(DiagnosticCategory::UnresolvedClass)
        );
        assert!(diagnostics[0].message.contains("Sting"));
    }

    /// Unknown class name, no cross-file metadata → silent (open-world
    /// policy, same gating as `check_unresolved_classes`).
    #[test]
    fn type_pattern_unknown_class_silent_without_cross_file() {
        let (module, hierarchy) = hierarchy_for("x match: [s :: Sting -> s; _ -> \"\"]");
        let mut diagnostics = Vec::new();
        check_type_pattern_classes(&module, &hierarchy, false, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no diagnostics without cross-file metadata, got: {diagnostics:?}"
        );
    }

    /// A class with subclasses → compile error (ADR 0107 Phase A leaf-only
    /// restriction).
    #[test]
    fn type_pattern_non_leaf_class_is_error() {
        let (module, hierarchy) = hierarchy_for(
            "Object subclass: Shape\nShape subclass: Circle\nx match: [s :: Shape -> s; _ -> 0]",
        );
        let mut diagnostics = Vec::new();
        check_type_pattern_classes(&module, &hierarchy, true, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error for non-leaf class, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(diagnostics[0].message.contains("Shape"));
        assert!(diagnostics[0].message.contains("subclasses"));
    }

    /// A leaf class (no subclasses) → no diagnostics.
    #[test]
    fn type_pattern_leaf_user_class_no_diagnostics() {
        let (module, hierarchy) =
            hierarchy_for("Object subclass: Circle\nx match: [s :: Circle -> s; _ -> 0]");
        let mut diagnostics = Vec::new();
        check_type_pattern_classes(&module, &hierarchy, true, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no diagnostics for a leaf user class, got: {diagnostics:?}"
        );
    }

    /// `Character` is never accepted, even though it resolves in the
    /// hierarchy — it shares `Integer`'s runtime representation.
    #[test]
    fn type_pattern_character_is_rejected() {
        let (module, hierarchy) = hierarchy_for("x match: [c :: Character -> c; _ -> 0]");
        let mut diagnostics = Vec::new();
        check_type_pattern_classes(&module, &hierarchy, true, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected 1 error rejecting Character, got: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert!(diagnostics[0].message.contains("Character"));
        assert!(diagnostics[0].message.contains("Integer"));
    }

    /// A `nil` pattern arm alongside a `Constructor` pattern arm must not
    /// trigger the type-pattern validator (no `Pattern::Type` present).
    #[test]
    fn type_pattern_validator_ignores_non_type_patterns() {
        let (module, hierarchy) = hierarchy_for("x match: [nil -> 0; Result ok: v -> v; _ -> 1]");
        let mut diagnostics = Vec::new();
        check_type_pattern_classes(&module, &hierarchy, true, &mut diagnostics);
        assert!(
            diagnostics.is_empty(),
            "Expected no diagnostics when no Pattern::Type is present, got: {diagnostics:?}"
        );
    }

    /// A `Pattern::Type` nested inside a tuple sub-pattern is still
    /// validated — the check must recurse into container patterns, not just
    /// inspect the arm's top-level pattern.
    #[test]
    fn type_pattern_nested_in_tuple_is_validated() {
        let (module, hierarchy) = hierarchy_for("x match: [{c :: Character, y} -> y; _ -> 0]");
        let mut diagnostics = Vec::new();
        check_type_pattern_classes(&module, &hierarchy, true, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected the nested Character type pattern to be flagged, got: {diagnostics:?}"
        );
        assert!(diagnostics[0].message.contains("Character"));
    }

    /// A `Pattern::Type` nested inside an array sub-pattern is still
    /// validated (same recursion path as the tuple case above; `Array`
    /// elements also route through the general `parse_pattern`, unlike
    /// `Constructor` keyword bindings and `Map` values, which don't yet
    /// accept a `::` type pattern at the grammar level).
    #[test]
    fn type_pattern_nested_in_array_is_validated() {
        let (module, hierarchy) = hierarchy_for(
            "Object subclass: Shape\nShape subclass: Circle\n\
             x match: [#[s :: Shape, y] -> y; _ -> 0]",
        );
        let mut diagnostics = Vec::new();
        check_type_pattern_classes(&module, &hierarchy, true, &mut diagnostics);
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected the nested non-leaf Shape type pattern to be flagged, got: {diagnostics:?}"
        );
        assert!(diagnostics[0].message.contains("Shape"));
    }
}
