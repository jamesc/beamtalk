// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Match exhaustiveness validators.
//!
//! **DDD Context:** Semantic Analysis
//!
//! Validates pattern match exhaustiveness for sealed types (BT-1299).

use crate::ast::{Expression, Module, Pattern};
use crate::ast_walker::walk_module;
use crate::source_analysis::Diagnostic;

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
                        )),
                    );
                }
                Expression::FieldAccess { field, .. } => {
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
                        ),
                    );
                }
                _ => {}
            }
        }
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
}
