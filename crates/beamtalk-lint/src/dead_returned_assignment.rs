// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lint: warn when `^t := e` writes a local nothing can read afterwards.
//!
//! **DDD Context:** Compilation
//!
//! `^t := e` is ordinary Smalltalk and compiles (BT-3910, ADR 0131 §1): the
//! assignment rebinds `t`, then the method returns the assigned value. Nothing
//! runs after the return that could read `t`, so the write is dead and `^e`
//! says the same thing. This covers the non-local return from a block
//! (`[:k | ^t := k]`) too.
//!
//! ```text
//! // Warned — nothing reads `count` after the return
//! next =>
//!     ^count := count + 1
//! ```
//!
//! A `^` out of the protected block of an `ensure:` whose cleanup reads a local
//! the block writes is a compile error for now (BT-3947: the cleanup would read
//! the old value). The lint asks the compiler ([`local_threading_diagnostics`])
//! and stays silent inside the span of any error it reports, as
//! `dead_block_assignment` does; the compile error owns it.
//!
//! It is a warning, never a compile error.

use crate::LintPass;
use crate::hierarchy_for_lint;
use beamtalk_core::ast::{Expression, Module};
use beamtalk_core::ast_walker::walk_expression;
use beamtalk_core::semantic_analysis::local_threading_diagnostics;
use beamtalk_core::source_analysis::{Diagnostic, Span};

/// Lint pass that flags `^t := e` the compiler does not already reject.
pub(crate) struct DeadReturnedAssignmentPass;

impl LintPass for DeadReturnedAssignmentPass {
    fn check(&self, module: &Module, diagnostics: &mut Vec<Diagnostic>) {
        let hierarchy = hierarchy_for_lint(module);
        let rejected: Vec<Span> = local_threading_diagnostics(module, &hierarchy)
            .iter()
            .map(|d| d.span)
            .collect();
        for method in module.all_methods() {
            let mut returns: Vec<(String, Span)> = Vec::new();
            for stmt in &method.body {
                walk_expression(&stmt.expression, &mut |expr| {
                    if let Expression::Return { value, .. } = expr
                        && let assignment @ Expression::Assignment { target, .. } =
                            value.unwrap_parens()
                        && let Expression::Identifier(id) = target.as_ref()
                    {
                        returns.push((id.name.to_string(), assignment.span()));
                    }
                });
            }
            for (name, span) in returns {
                let owned_by_compile_error = rejected
                    .iter()
                    .any(|p| p.start() <= span.start() && span.end() <= p.end());
                if !owned_by_compile_error {
                    diagnostics.push(
                        Diagnostic::lint(
                            format!(
                                "assignment to `{name}` is returned, and nothing reads `{name}` \
                                 after the return"
                            ),
                            span,
                        )
                        .with_hint(format!(
                            "return the value directly: `^expr` instead of `^{name} := expr`"
                        )),
                    );
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::run_lint_passes;
    use beamtalk_core::source_analysis::{Diagnostic, lex_with_eof, parse};

    fn lint(source: &str) -> Vec<Diagnostic> {
        let (module, _) = parse(lex_with_eof(source));
        run_lint_passes(&module)
            .into_iter()
            .filter(|d| d.message.contains("is returned, and nothing reads"))
            .collect()
    }

    fn in_method(body: &str) -> String {
        format!("Object subclass: Foo\n  run =>\n    t := 0\n    {body}\n")
    }

    #[test]
    fn a_returned_assignment_at_the_method_root_is_dead() {
        let diags = lint(&in_method("^t := t + 1"));
        assert_eq!(diags.len(), 1, "{diags:?}");
        assert!(diags[0].message.contains("`t`"), "{}", diags[0].message);
        assert!(
            diags[0]
                .hint
                .as_deref()
                .is_some_and(|h| h.contains("`^expr`")),
            "{:?}",
            diags[0].hint
        );
    }

    #[test]
    fn a_parenthesised_returned_assignment_is_dead() {
        assert_eq!(lint(&in_method("^(t := t + 1)")).len(), 1);
    }

    #[test]
    fn a_returned_assignment_in_a_non_local_return_block_is_dead() {
        let diags = lint(&in_method(
            "#(1, 2) do: [:k | k > 1 ifTrue: [^t := k]]\n    0",
        ));
        assert_eq!(diags.len(), 1, "{diags:?}");
    }

    #[test]
    fn inside_an_ensure_protected_block_the_compile_error_owns_it() {
        let diags = lint(&in_method("[^t := t + 1] ensure: [t printString]"));
        assert!(diags.is_empty(), "{diags:?}");
    }

    #[test]
    fn returning_the_value_directly_is_clean() {
        assert!(lint(&in_method("^t + 1")).is_empty());
        assert!(lint(&in_method("t := t + 1\n    ^t")).is_empty());
    }

    #[test]
    fn a_returned_assignment_outside_the_protected_block_is_dead() {
        let diags = lint(&in_method("[0] ensure: [t printString]\n    ^t := 5"));
        assert_eq!(diags.len(), 1, "{diags:?}");
    }

    #[test]
    fn a_returned_assignment_whose_cleanup_reads_nothing_is_dead() {
        let diags = lint(&in_method("[^t := t + 1] ensure: [0]"));
        assert_eq!(diags.len(), 1, "{diags:?}");
    }

    #[test]
    fn a_returned_assignment_in_an_ensure_cleanup_is_dead() {
        let diags = lint(&in_method("[0] ensure: [^t := 5]"));
        assert_eq!(diags.len(), 1, "{diags:?}");
    }
}
