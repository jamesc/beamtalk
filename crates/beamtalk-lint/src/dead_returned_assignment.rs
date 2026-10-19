// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lint: warn when `^t := e` writes a local nothing can read afterwards.
//!
//! **DDD Context:** Compilation
//!
//! `^t := e` is ordinary Smalltalk and compiles (BT-3910, ADR 0131 §1): the
//! assignment rebinds `t`, then the method returns the assigned value. The
//! write is observable only by code that runs after the return and still sees
//! `t`: an `ensure:` cleanup that encloses the `^` and reads `t`. When no
//! enclosing cleanup reads `t` the write is dead, and `^e` says the same
//! thing. This covers the non-local return from a block (`[:k | ^t := k]`)
//! too.
//!
//! ```text
//! // Warned — nothing reads `count` after the return
//! next =>
//!     ^count := count + 1
//!
//! // Fine — the cleanup runs after the return and reads `count`
//! next =>
//!     [^count := count + 1] ensure: [self log: count]
//! ```
//!
//! It is a warning, never a compile error.

use crate::LintPass;
use beamtalk_core::ast::{Expression, MessageSelector, Module};
use beamtalk_core::ast_walker::walk_expression;
use beamtalk_core::source_analysis::{Diagnostic, Span};
use std::collections::HashSet;

/// Lint pass that flags `^t := e` whose write no enclosing `ensure:` cleanup
/// reads.
pub(crate) struct DeadReturnedAssignmentPass;

/// An `ensure:` whose protected block spans `protected` and whose cleanup
/// mentions the locals `read`.
struct Cleanup {
    protected: Span,
    read: HashSet<String>,
}

impl LintPass for DeadReturnedAssignmentPass {
    fn check(&self, module: &Module, diagnostics: &mut Vec<Diagnostic>) {
        for method in module.all_methods() {
            let mut cleanups: Vec<Cleanup> = Vec::new();
            let mut returns: Vec<(String, Span)> = Vec::new();
            for stmt in &method.body {
                walk_expression(&stmt.expression, &mut |expr| {
                    if let Some(cleanup) = ensure_cleanup(expr) {
                        cleanups.push(cleanup);
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
                let observed = cleanups.iter().any(|c| {
                    c.protected.start() <= span.start()
                        && span.end() <= c.protected.end()
                        && c.read.contains(&name)
                });
                if !observed {
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

/// The cleanup of `expr` when it is `[...] ensure: [...]` with two block
/// literals: the protected block's span and the names its cleanup mentions.
fn ensure_cleanup(expr: &Expression) -> Option<Cleanup> {
    let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Keyword(parts),
        arguments,
        ..
    } = expr
    else {
        return None;
    };
    if parts.len() != 1 || parts[0].keyword != "ensure:" {
        return None;
    }
    let (Expression::Block(protected), Some(Expression::Block(cleanup))) =
        (receiver.as_ref(), arguments.first())
    else {
        return None;
    };
    let mut read = HashSet::new();
    for stmt in &cleanup.body {
        walk_expression(&stmt.expression, &mut |e| {
            if let Expression::Identifier(id) = e {
                read.insert(id.name.to_string());
            }
        });
    }
    Some(Cleanup {
        protected: protected.span,
        read,
    })
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
    fn an_ensure_cleanup_that_reads_the_local_keeps_the_write_live() {
        let diags = lint(&in_method("[^t := t + 1] ensure: [t printString]"));
        assert!(diags.is_empty(), "{diags:?}");
    }

    #[test]
    fn an_ensure_cleanup_that_reads_another_local_does_not() {
        let diags = lint(&in_method("[^t := t + 1] ensure: [Transcript show: \"x\"]"));
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
}
