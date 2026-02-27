// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lint: warn when the same receiver has 3+ consecutive message sends in a
//! statement sequence that could be written as a cascade.
//!
//! ```text
//! // Bad — three separate sends to the same receiver
//! arr add: 1.
//! arr add: 2.
//! arr add: 3.
//!
//! // Good — use cascade syntax
//! arr add: 1; add: 2; add: 3
//! ```
//!
//! Only simple receivers (local variables, `self`, class references) are
//! checked.  Complex receiver expressions (e.g. `(foo bar) baz`) are skipped
//! because they may have side effects and are not straightforward to extract
//! into a single cascade receiver.

use crate::ast::{Block, Expression, Identifier, MethodDefinition, Module};
use crate::lint::LintPass;
use crate::source_analysis::Diagnostic;

/// Lint pass that warns on 3+ consecutive message sends to the same simple
/// receiver that could be rewritten as a cascade.
pub(crate) struct CascadeCandidatePass;

impl LintPass for CascadeCandidatePass {
    fn check(&self, module: &Module, diagnostics: &mut Vec<Diagnostic>) {
        check_sequence(&module.expressions, diagnostics);
        for class in &module.classes {
            for method in &class.methods {
                check_method(method, diagnostics);
            }
            for method in &class.class_methods {
                check_method(method, diagnostics);
            }
        }
        for standalone in &module.method_definitions {
            check_method(&standalone.method, diagnostics);
        }
    }
}

fn check_method(method: &MethodDefinition, diagnostics: &mut Vec<Diagnostic>) {
    check_sequence(&method.body, diagnostics);
}

/// Returns the simple receiver name if `expr` is a `MessageSend` whose
/// receiver is an identifier (variable, `self`) or a class reference.
fn simple_receiver_name(expr: &Expression) -> Option<&str> {
    if let Expression::MessageSend { receiver, .. } = expr {
        match receiver.as_ref() {
            Expression::Identifier(Identifier { name, .. }) => Some(name.as_str()),
            Expression::ClassReference { name, .. } => Some(name.name.as_str()),
            _ => None,
        }
    } else {
        None
    }
}

/// Check a flat sequence of statements for runs of 3+ consecutive sends to
/// the same simple receiver, then recurse into nested expressions.
fn check_sequence(exprs: &[Expression], diagnostics: &mut Vec<Diagnostic>) {
    let mut i = 0;
    while i < exprs.len() {
        if let Some(recv) = simple_receiver_name(&exprs[i]) {
            let run_start = i;
            while i < exprs.len() && simple_receiver_name(&exprs[i]) == Some(recv) {
                i += 1;
            }
            let run_len = i - run_start;
            if run_len >= 3 {
                let first_span = exprs[run_start].span();
                let last_span = exprs[i - 1].span();
                let span = first_span.merge(last_span);
                let mut diag = Diagnostic::lint(
                    format!(
                        "`{recv}` receives {run_len} consecutive messages; consider using a cascade"
                    ),
                    span,
                );
                diag.hint = Some(
                    format!(
                        "Replace the {run_len} separate sends with a cascade: \
                         `{recv} msg1; msg2; ...`"
                    )
                    .into(),
                );
                diagnostics.push(diag);
            }
            // Recurse into each expression in the run
            for expr in &exprs[run_start..i] {
                recurse(expr, diagnostics);
            }
        } else {
            recurse(&exprs[i], diagnostics);
            i += 1;
        }
    }
}

/// Recurse into nested statement sequences (block bodies, etc.) that may
/// themselves contain cascade candidates.
fn recurse(expr: &Expression, diagnostics: &mut Vec<Diagnostic>) {
    match expr {
        Expression::Block(Block { body, .. }) => {
            check_sequence(body, diagnostics);
        }

        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            recurse(receiver, diagnostics);
            for arg in arguments {
                recurse(arg, diagnostics);
            }
        }

        Expression::Assignment { target, value, .. } => {
            recurse(target, diagnostics);
            recurse(value, diagnostics);
        }

        Expression::Return { value, .. } => {
            recurse(value, diagnostics);
        }

        Expression::Cascade {
            receiver, messages, ..
        } => {
            recurse(receiver, diagnostics);
            for msg in messages {
                for arg in &msg.arguments {
                    recurse(arg, diagnostics);
                }
            }
        }

        Expression::Parenthesized { expression, .. } => {
            recurse(expression, diagnostics);
        }

        Expression::FieldAccess { receiver, .. } => {
            recurse(receiver, diagnostics);
        }

        Expression::Match { value, arms, .. } => {
            recurse(value, diagnostics);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    recurse(guard, diagnostics);
                }
                recurse(&arm.body, diagnostics);
            }
        }

        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                recurse(&pair.key, diagnostics);
                recurse(&pair.value, diagnostics);
            }
        }

        Expression::ListLiteral { elements, tail, .. } => {
            for elem in elements {
                recurse(elem, diagnostics);
            }
            if let Some(t) = tail {
                recurse(t, diagnostics);
            }
        }

        Expression::ArrayLiteral { elements, .. } => {
            for elem in elements {
                recurse(elem, diagnostics);
            }
        }

        Expression::StringInterpolation { segments, .. } => {
            for seg in segments {
                if let crate::ast::StringSegment::Interpolation(e) = seg {
                    recurse(e, diagnostics);
                }
            }
        }

        // Leaf nodes — nothing to recurse into.
        Expression::Literal(..)
        | Expression::Identifier(..)
        | Expression::ClassReference { .. }
        | Expression::Super(..)
        | Expression::Primitive { .. }
        | Expression::ExpectDirective { .. }
        | Expression::Error { .. } => {}
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
    fn three_consecutive_sends_flagged() {
        let diags = lint(
            "Object subclass: Foo\n\
             add: arr =>\n\
             arr add: 1.\n\
             arr add: 2.\n\
             arr add: 3\n",
        );
        assert_eq!(
            diags.len(),
            1,
            "expected one lint diagnostic, got: {diags:?}"
        );
        assert_eq!(diags[0].severity, Severity::Lint);
        assert!(
            diags[0].message.contains("cascade"),
            "message: {}",
            diags[0].message
        );
    }

    #[test]
    fn four_consecutive_sends_flagged_once() {
        let diags = lint(
            "Object subclass: Foo\n\
             add: arr =>\n\
             arr add: 1.\n\
             arr add: 2.\n\
             arr add: 3.\n\
             arr add: 4\n",
        );
        assert_eq!(
            diags.len(),
            1,
            "expected one diagnostic for the whole run, got: {diags:?}"
        );
        assert!(
            diags[0].message.contains('4'),
            "message should mention run length: {}",
            diags[0].message
        );
    }

    #[test]
    fn two_consecutive_sends_not_flagged() {
        let diags = lint(
            "Object subclass: Foo\n\
             add: arr =>\n\
             arr add: 1.\n\
             arr add: 2\n",
        );
        assert!(
            diags.is_empty(),
            "two sends should not be flagged, got: {diags:?}"
        );
    }

    #[test]
    fn interleaved_sends_not_flagged() {
        let diags = lint(
            "Object subclass: Foo\n\
             add: arr with: other =>\n\
             arr add: 1.\n\
             other add: 99.\n\
             arr add: 2.\n\
             arr add: 3\n",
        );
        assert!(
            diags.is_empty(),
            "non-consecutive sends should not be flagged, got: {diags:?}"
        );
    }

    #[test]
    fn lint_has_category_and_hint() {
        let diags = lint(
            "Object subclass: Foo\n\
             add: arr =>\n\
             arr add: 1.\n\
             arr add: 2.\n\
             arr add: 3\n",
        );
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].category, Some(DiagnosticCategory::Lint));
        assert!(diags[0].hint.is_some(), "expected a hint");
    }

    #[test]
    fn self_receiver_flagged() {
        let diags = lint(
            "Object subclass: Foo\n\
             run =>\n\
             self doA.\n\
             self doB.\n\
             self doC\n",
        );
        assert_eq!(
            diags.len(),
            1,
            "self sends should be flagged, got: {diags:?}"
        );
        assert!(
            diags[0].message.contains("self"),
            "message: {}",
            diags[0].message
        );
    }

    #[test]
    fn complex_receiver_not_flagged() {
        let diags = lint(
            "Object subclass: Foo\n\
             run: x =>\n\
             (x builder) add: 1.\n\
             (x builder) add: 2.\n\
             (x builder) add: 3\n",
        );
        assert!(
            diags.is_empty(),
            "complex receivers should not be flagged, got: {diags:?}"
        );
    }

    #[test]
    fn cascade_in_block_body_flagged() {
        let diags = lint(
            "Object subclass: Foo\n\
             run: arr =>\n\
             [:x | arr add: 1. arr add: 2. arr add: 3]\n",
        );
        assert_eq!(
            diags.len(),
            1,
            "consecutive sends in block body should be flagged, got: {diags:?}"
        );
    }
}
