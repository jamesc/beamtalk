// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lint: flag effect-free expressions in statement position (BT-951).
//!
//! Warns when a non-last expression in a method body, block body, or module-level
//! sequence is a literal, variable reference, or pure arithmetic/comparison whose
//! result is silently discarded.
//!
//! ```text
//! // Bad — literal discarded
//! doStuff =>
//!   42.
//!   self actualWork
//!
//! // Good — literal is the return value
//! answer => 42
//! ```

use crate::ast::Module;
use crate::lint::LintPass;
use crate::source_analysis::Diagnostic;

/// Lint pass that flags effect-free statements whose values are silently discarded.
pub(crate) struct EffectFreeStatementPass;

impl LintPass for EffectFreeStatementPass {
    fn check(&self, module: &Module, diagnostics: &mut Vec<Diagnostic>) {
        crate::semantic_analysis::validators::check_effect_free_statements(module, diagnostics);
    }
}

#[cfg(test)]
mod tests {
    use crate::lint::run_lint_passes;
    use crate::source_analysis::{Severity, lex_with_eof, parse};

    fn lint(source: &str) -> Vec<crate::source_analysis::Diagnostic> {
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        assert!(
            parse_diags.is_empty(),
            "Parse failed for lint fixture: {parse_diags:?}"
        );
        run_lint_passes(&module)
    }

    #[test]
    fn discarded_literal_surfaced_by_lint_runner() {
        let diags = lint("Object subclass: Foo\n  bar =>\n    42.\n    self doSomething");
        let effect_free: Vec<_> = diags
            .iter()
            .filter(|d| d.message.contains("no effect"))
            .collect();
        assert_eq!(
            effect_free.len(),
            1,
            "Expected 1 effect-free lint, got: {effect_free:?}"
        );
        assert_eq!(effect_free[0].severity, Severity::Lint);
    }

    #[test]
    fn clean_method_no_effect_free_lint() {
        let diags = lint("Object subclass: Foo\n  bar => 42");
        let effect_free: Vec<_> = diags
            .iter()
            .filter(|d| d.message.contains("no effect"))
            .collect();
        assert!(
            effect_free.is_empty(),
            "Expected no effect-free lints, got: {effect_free:?}"
        );
    }
}
