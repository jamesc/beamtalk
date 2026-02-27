// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lint: flag a redundant trailing `^` on the last expression of a method body.
//!
//! In Beamtalk, `^` is for early returns only. When it appears as the final
//! statement of a method, it is redundant — the value is returned implicitly.
//!
//! ```text
//! // Bad — trailing ^ is redundant
//! increment => ^(count + 1)
//!
//! // Good — implicit return
//! increment => count + 1
//! ```

use crate::ast::{Expression, MethodDefinition, Module};
use crate::lint::LintPass;
use crate::source_analysis::Diagnostic;

/// Lint pass that flags redundant trailing `^` on the last method expression.
pub(crate) struct TrailingCaretPass;

impl LintPass for TrailingCaretPass {
    fn check(&self, module: &Module, diagnostics: &mut Vec<Diagnostic>) {
        for class in &module.classes {
            for method in &class.methods {
                check_method(method, diagnostics);
            }
        }
    }
}

fn check_method(method: &MethodDefinition, diagnostics: &mut Vec<Diagnostic>) {
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

#[cfg(test)]
mod tests {
    use crate::lint::run_lint_passes;
    use crate::source_analysis::{DiagnosticCategory, Severity, lex_with_eof, parse};

    fn lint(source: &str) -> Vec<crate::source_analysis::Diagnostic> {
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
        let diags = lint("Object subclass: Foo\n  value => ^42\n");
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].category, Some(DiagnosticCategory::Lint));
    }
}
