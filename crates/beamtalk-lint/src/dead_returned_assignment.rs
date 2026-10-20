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
//! A `^` out of the protected block of an `ensure:` whose cleanup reads the
//! local is not dead: the throw hands the cleanup the value written (BT-3947),
//! so the lint stays silent for it. It also asks the compiler
//! ([`local_threading_diagnostics`]) and stays silent inside the span of any
//! error it reports, as `dead_block_assignment` does; the compile error owns it.
//!
//! It is a warning, never a compile error.

use crate::LintPass;
use crate::hierarchy_for_lint;
use beamtalk_core::ast::{Expression, MessageSelector, Module};
use beamtalk_core::ast_walker::walk_expression;
use beamtalk_core::semantic_analysis::block_facts::analyze_block;
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
            // The protected blocks of the method's `ensure:`s, with the locals
            // each cleanup reads.
            let mut ensures: Vec<(Span, std::collections::HashSet<String>)> = Vec::new();
            for stmt in &method.body {
                walk_expression(&stmt.expression, &mut |expr| {
                    if let Expression::MessageSend {
                        receiver,
                        selector: MessageSelector::Keyword(parts),
                        arguments,
                        ..
                    } = expr
                        && parts.len() == 1
                        && parts[0].keyword == "ensure:"
                        && let Expression::Block(protected) = receiver.as_ref()
                        && let [Expression::Block(cleanup)] = arguments.as_slice()
                    {
                        ensures.push((protected.span, analyze_block(cleanup).local_reads));
                    }
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
                // BT-3947: an `ensure:` whose cleanup reads `name` reads what
                // the `^` wrote.
                let read_by_cleanup = ensures.iter().any(|(protected, reads)| {
                    protected.start() <= span.start()
                        && span.end() <= protected.end()
                        && reads.contains(&name)
                });
                if !owned_by_compile_error && !read_by_cleanup {
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
    fn inside_an_ensure_protected_block_whose_cleanup_reads_it_it_is_live() {
        let diags = lint(&in_method("[^t := t + 1] ensure: [t printString]"));
        assert!(diags.is_empty(), "{diags:?}");
        let diags = lint(&in_method(
            "[#(1) do: [:k | ^t := k]] ensure: [t printString]\n    0",
        ));
        assert!(diags.is_empty(), "{diags:?}");
    }

    #[test]
    fn inside_an_ensure_protected_block_whose_cleanup_does_not_read_it_it_is_dead() {
        let diags = lint(&in_method("[^t := t + 1] ensure: [0]"));
        assert_eq!(diags.len(), 1, "{diags:?}");
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
