// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lint passes for Beamtalk source code.
//!
//! **DDD Context:** Compilation
//!
//! Lint checks are style/redundancy checks that are distinct from compiler
//! warnings. They are suppressed during normal `check`/`compile` and only
//! reported by `beamtalk lint`.
//!
//! Each lint pass takes a [`Module`] reference and returns a list of
//! [`Diagnostic`]s with [`Severity::Lint`].
//!
//! # Adding a New Lint
//!
//! 1. Write a function `check_<name>(module: &Module, diagnostics: &mut Vec<Diagnostic>)`.
//! 2. Call it from [`run_lint_passes`].

use crate::ast::{Expression, MethodDefinition, Module};
use crate::source_analysis::Diagnostic;

/// Run all lint passes on a parsed module and return any lint diagnostics.
///
/// The returned diagnostics all have [`Severity::Lint`] and will not appear
/// during normal compilation.
#[must_use]
pub fn run_lint_passes(module: &Module) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    check_trailing_caret(module, &mut diagnostics);
    diagnostics
}

// ── Trailing-caret lint (BT-947) ────────────────────────────────────────────

/// Lint: flag a `^` (return) on the last expression of a method body.
///
/// In Beamtalk, `^` is for early returns only. When it appears as the final
/// statement of a method, it is redundant — the value is returned implicitly.
///
/// ```text
/// // Bad — trailing ^ is redundant
/// increment => ^(count + 1)
///
/// // Good — implicit return
/// increment => count + 1
/// ```
fn check_trailing_caret(module: &Module, diagnostics: &mut Vec<Diagnostic>) {
    for class in &module.classes {
        for method in &class.methods {
            check_method_trailing_caret(method, diagnostics);
        }
    }
}

/// Check a single method body for a trailing `^` on its last expression.
fn check_method_trailing_caret(method: &MethodDefinition, diagnostics: &mut Vec<Diagnostic>) {
    let Some(last_expr) = method.body.last() else {
        return;
    };

    if let Expression::Return { span, .. } = last_expr {
        let method_name = method.selector.name();
        let mut diag = Diagnostic::lint(
            format!(
                "trailing `^` in method `{method_name}` is redundant — \
                 the last expression is returned implicitly"
            ),
            *span,
        );
        diag.hint = Some(
            "Remove the `^` — in Beamtalk, the last expression is always returned implicitly"
                .into(),
        );
        diagnostics.push(diag);
    }
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::{Severity, lex_with_eof, parse};

    fn lint(source: &str) -> Vec<Diagnostic> {
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        run_lint_passes(&module)
    }

    #[test]
    fn trailing_caret_on_last_expression_is_flagged() {
        let diags = lint("Object subclass: Foo\n  increment => ^42\n");
        assert_eq!(diags.len(), 1, "expected one lint diagnostic");
        assert_eq!(diags[0].severity, Severity::Lint);
        assert!(
            diags[0].message.contains("trailing `^`"),
            "message: {}",
            diags[0].message
        );
    }

    #[test]
    fn method_without_caret_is_clean() {
        let diags = lint("Object subclass: Foo\n  increment => 42\n");
        assert!(diags.is_empty());
    }

    #[test]
    fn lint_diagnostic_has_hint() {
        let diags = lint("Object subclass: Foo\n  value => ^42\n");
        assert_eq!(diags.len(), 1);
        assert!(
            diags[0].hint.is_some(),
            "lint diagnostic should include a fix hint"
        );
    }

    #[test]
    fn lint_diagnostic_has_lint_category() {
        use crate::source_analysis::DiagnosticCategory;
        let diags = lint("Object subclass: Foo\n  value => ^42\n");
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].category, Some(DiagnosticCategory::Lint));
    }
}
