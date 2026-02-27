// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lint: warn on unnecessary parentheses that do not change message precedence.
//!
//! In Beamtalk/Smalltalk, parentheses are only needed to override the default
//! message precedence (unary > binary > keyword). When the inner expression
//! already has the highest possible precedence in all contexts — atoms, unary
//! sends, field accesses, and nested parens — the enclosing `(...)` are
//! redundant.
//!
//! ```text
//! // Bad — parentheses are redundant
//! (42)
//! (myVar)
//! (self.field)
//! (obj size)
//! ((foo check: bar))
//!
//! // OK — parens change precedence
//! arr do: (foo check: bar)    // keyword arg requires parens
//! a + (b * c)                 // right-hand binary operand needs parens
//! ```

use crate::ast::{Block, Expression, MessageSelector, MethodDefinition, Module};
use crate::lint::LintPass;
use crate::source_analysis::Diagnostic;

/// Lint pass that warns on `Expression::Parenthesized` wrapping expressions
/// that never require precedence disambiguation.
pub(crate) struct UnnecessaryParensPass;

impl LintPass for UnnecessaryParensPass {
    fn check(&self, module: &Module, diagnostics: &mut Vec<Diagnostic>) {
        for expr in &module.expressions {
            check_expr(expr, diagnostics);
        }
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
    for expr in &method.body {
        check_expr(expr, diagnostics);
    }
}

/// Returns `true` when `expr` is an expression whose precedence is already
/// as high as possible, meaning parentheses around it can never change how
/// the surrounding code is parsed.
///
/// Conservative: binary and keyword sends are excluded because their necessity
/// depends on the surrounding context (e.g., as an argument to another message).
fn is_always_unnecessary(expr: &Expression) -> bool {
    match expr {
        // Atoms, field access, and double-parens: self-contained, no operator
        // precedence to disambiguate.  Field access is already the tightest
        // binding syntax; double parens force grouping via the inner node.
        Expression::Literal(..)
        | Expression::Identifier(..)
        | Expression::ClassReference { .. }
        | Expression::Super(..)
        | Expression::Block(..)
        | Expression::MapLiteral { .. }
        | Expression::ListLiteral { .. }
        | Expression::ArrayLiteral { .. }
        | Expression::Primitive { .. }
        | Expression::StringInterpolation { .. }
        | Expression::FieldAccess { .. }
        | Expression::Parenthesized { .. } => true,

        // Unary sends have the highest message-precedence level; they can
        // never be "stolen" by an outer operator.
        Expression::MessageSend { selector, .. } => {
            matches!(selector, MessageSelector::Unary(_))
        }

        _ => false,
    }
}

/// Like `check_expr`, but skips the unnecessary-parens diagnostic for a
/// top-level `Parenthesized` node and only recurses into its contents.
///
/// Used for map literal keys: `#{(x) => v}` uses parens to prevent the
/// bare-identifier-to-symbol conversion (BT-591), so `(x)` there is
/// semantically meaningful.  Deeper nesting (e.g., `#{((x)) => v}`) still
/// recurses and may flag the inner `(x)`.
fn check_map_key(expr: &Expression, diagnostics: &mut Vec<Diagnostic>) {
    if let Expression::Parenthesized { expression, .. } = expr {
        check_expr(expression, diagnostics);
    } else {
        check_expr(expr, diagnostics);
    }
}

/// Like `check_expr`, but skips the unnecessary-parens diagnostic for a
/// top-level `Parenthesized` node used as a message receiver.
///
/// Parentheses around a receiver expression (e.g. `(x builder) add: 1`) are
/// often written to visually group the receiver even when Beamtalk precedence
/// rules already bind it correctly.  Flagging them as unnecessary creates
/// noisy, unhelpful output — we only recurse into the inner expression.
fn check_receiver(expr: &Expression, diagnostics: &mut Vec<Diagnostic>) {
    if let Expression::Parenthesized { expression, .. } = expr {
        check_expr(expression, diagnostics);
    } else {
        check_expr(expr, diagnostics);
    }
}

/// Recursively inspects `expr`, emitting a lint for each `Parenthesized`
/// node whose inner expression is always-unnecessary.
fn check_expr(expr: &Expression, diagnostics: &mut Vec<Diagnostic>) {
    match expr {
        Expression::Parenthesized { expression, span } => {
            if is_always_unnecessary(expression) {
                let mut diag = Diagnostic::lint("unnecessary parentheses".to_string(), *span);
                diag.hint =
                    Some("Remove the parentheses — they do not affect precedence here".into());
                diagnostics.push(diag);
            }
            check_expr(expression, diagnostics);
        }

        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            check_receiver(receiver, diagnostics);
            for arg in arguments {
                check_expr(arg, diagnostics);
            }
        }

        Expression::Block(Block { body, .. }) => {
            for e in body {
                check_expr(e, diagnostics);
            }
        }

        Expression::Assignment { target, value, .. } => {
            check_expr(target, diagnostics);
            check_expr(value, diagnostics);
        }

        Expression::Return { value, .. } => {
            check_expr(value, diagnostics);
        }

        Expression::Cascade {
            receiver, messages, ..
        } => {
            check_receiver(receiver, diagnostics);
            for msg in messages {
                for arg in &msg.arguments {
                    check_expr(arg, diagnostics);
                }
            }
        }

        Expression::FieldAccess { receiver, .. } => {
            check_expr(receiver, diagnostics);
        }

        Expression::Match { value, arms, .. } => {
            check_expr(value, diagnostics);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    check_expr(guard, diagnostics);
                }
                check_expr(&arm.body, diagnostics);
            }
        }

        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                // BT-591: bare lowercase identifiers before `=>` become symbols,
                // so `#{(x) => v}` is the only way to use a variable as a dynamic
                // key. A top-level `Parenthesized` around the key is therefore
                // semantically significant — skip that node's lint but still
                // recurse into the inner expression for further checks.
                check_map_key(&pair.key, diagnostics);
                check_expr(&pair.value, diagnostics);
            }
        }

        Expression::ListLiteral { elements, tail, .. } => {
            for elem in elements {
                check_expr(elem, diagnostics);
            }
            if let Some(t) = tail {
                check_expr(t, diagnostics);
            }
        }

        Expression::ArrayLiteral { elements, .. } => {
            for elem in elements {
                check_expr(elem, diagnostics);
            }
        }

        Expression::StringInterpolation { segments, .. } => {
            for seg in segments {
                if let crate::ast::StringSegment::Interpolation(e) = seg {
                    check_expr(e, diagnostics);
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
    fn parens_around_literal_is_flagged() {
        let diags = lint("Object subclass: Foo\n  value => (42)\n");
        assert_eq!(
            diags.len(),
            1,
            "expected one lint diagnostic, got: {diags:?}"
        );
        assert_eq!(diags[0].severity, Severity::Lint);
        assert!(
            diags[0].message.contains("unnecessary parentheses"),
            "message: {}",
            diags[0].message
        );
    }

    #[test]
    fn parens_around_identifier_is_flagged() {
        let diags = lint("Object subclass: Foo\n  value: x => (x)\n");
        assert_eq!(diags.len(), 1, "got: {diags:?}");
        assert_eq!(diags[0].severity, Severity::Lint);
    }

    #[test]
    fn parens_around_block_is_flagged() {
        let diags = lint("Object subclass: Foo\n  value => ([42])\n");
        assert_eq!(diags.len(), 1, "got: {diags:?}");
        assert_eq!(diags[0].severity, Severity::Lint);
    }

    #[test]
    fn parens_around_unary_send_is_flagged() {
        let diags = lint("Object subclass: Foo\n  value: x => (x size)\n");
        assert_eq!(diags.len(), 1, "got: {diags:?}");
        assert_eq!(diags[0].severity, Severity::Lint);
    }

    #[test]
    fn parens_around_field_access_is_flagged() {
        let diags = lint("Object subclass: Foo\n  state: x = 0\n  value => (self.x)\n");
        assert_eq!(diags.len(), 1, "got: {diags:?}");
        assert_eq!(diags[0].severity, Severity::Lint);
    }

    #[test]
    fn double_parens_is_flagged() {
        // The outer parens wrap a Parenthesized — always unnecessary.
        let diags = lint("Object subclass: Foo\n  value: x => ((x))\n");
        // Two lint diagnostics: outer wraps inner Parenthesized; inner wraps identifier.
        assert_eq!(diags.len(), 2, "got: {diags:?}");
        assert!(diags.iter().all(|d| d.severity == Severity::Lint));
    }

    #[test]
    fn lint_diagnostic_has_lint_category() {
        let diags = lint("Object subclass: Foo\n  value => (42)\n");
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].category, Some(DiagnosticCategory::Lint));
    }

    #[test]
    fn lint_diagnostic_has_hint() {
        let diags = lint("Object subclass: Foo\n  value => (42)\n");
        assert_eq!(diags.len(), 1);
        assert!(diags[0].hint.is_some(), "expected a fix hint");
    }

    #[test]
    fn keyword_send_as_arg_is_not_flagged() {
        // `arr do: (item check: bar)` — keyword inside keyword arg requires parens;
        // the outer context isn't tracked here, so the lint should NOT fire on
        // the binary/keyword inner cases (it's conservative).
        let diags = lint("Object subclass: Foo\n  value: arr => arr do: (arr check: 1)\n");
        assert!(
            diags.is_empty(),
            "keyword-in-keyword-arg should not be flagged, got: {diags:?}"
        );
    }

    #[test]
    fn binary_send_as_arg_is_not_flagged() {
        // `a + (b * c)` — parens may be needed for binary associativity.
        let diags = lint("Object subclass: Foo\n  value => 1 + (2 * 3)\n");
        assert!(
            diags.is_empty(),
            "binary-in-binary-arg should not be flagged, got: {diags:?}"
        );
    }

    #[test]
    fn no_false_positive_on_clean_code() {
        let diags = lint("Object subclass: Foo\n  value: x => x size\n");
        assert!(diags.is_empty(), "got: {diags:?}");
    }

    /// BT-591: `#{(key) => val}` uses parens to prevent bare-identifier-to-symbol
    /// conversion — these parens are semantically required and must NOT be flagged.
    #[test]
    fn parens_around_map_key_variable_not_flagged() {
        let diags = lint("Object subclass: Foo\n  value: k => #{(k) => 1}\n");
        assert!(
            diags.is_empty(),
            "dynamic map key parens must not be flagged, got: {diags:?}"
        );
    }

    /// Map value parens are still flagged — only the key is special.
    #[test]
    fn parens_around_map_value_still_flagged() {
        let diags = lint("Object subclass: Foo\n  value => #{#k => (42)}\n");
        assert_eq!(
            diags.len(),
            1,
            "parens around map value should be flagged, got: {diags:?}"
        );
        assert_eq!(diags[0].severity, Severity::Lint);
    }

    /// BT-957: Parentheses around a message receiver (e.g. `(x builder) add: 1`)
    /// are used for visual grouping even when precedence makes them redundant.
    /// They should NOT be flagged as unnecessary.
    #[test]
    fn parens_around_receiver_not_flagged() {
        let diags = lint("Object subclass: Foo\n  value: x => (x builder) add: 1\n");
        assert!(
            diags.is_empty(),
            "receiver parens must not be flagged, got: {diags:?}"
        );
    }
}
