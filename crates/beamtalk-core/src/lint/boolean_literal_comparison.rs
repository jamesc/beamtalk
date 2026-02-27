// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lint: flag redundant comparisons to boolean literals.
//!
//! **DDD Context:** Compilation
//!
//! Comparing a value to `true` or `false` using `=` is redundant:
//!
//! - `x = true`  can be simplified to `x`
//! - `x = false` can be simplified to `x not`
//!
//! ```text
//! // Bad — redundant boolean comparison
//! x = true   ifTrue: [...]
//! x = false  ifTrue: [...]
//!
//! // Good — use the value directly
//! x ifTrue: [...]
//! x not ifTrue: [...]
//! ```

use crate::ast::{Expression, Identifier, MessageSelector, Module};
use crate::lint::LintPass;
use crate::source_analysis::{Diagnostic, Span};

/// Lint pass that flags `expr = true` and `expr = false` comparisons.
pub(crate) struct BooleanLiteralComparisonPass;

impl LintPass for BooleanLiteralComparisonPass {
    fn check(&self, module: &Module, diagnostics: &mut Vec<Diagnostic>) {
        for expr in &module.expressions {
            walk_expression(expr, diagnostics);
        }
        for class in &module.classes {
            for method in class.methods.iter().chain(class.class_methods.iter()) {
                for expr in &method.body {
                    walk_expression(expr, diagnostics);
                }
            }
        }
        for standalone in &module.method_definitions {
            for expr in &standalone.method.body {
                walk_expression(expr, diagnostics);
            }
        }
    }
}

/// Returns a human-readable representation of an expression for use in diagnostic messages.
///
/// Returns `None` if the expression is too complex to represent concisely.
fn display_receiver(expr: &Expression) -> Option<String> {
    match expr {
        Expression::Identifier(Identifier { name, .. }) => Some(name.to_string()),
        Expression::MessageSend {
            receiver, selector, ..
        } => {
            let receiver_str = display_receiver(receiver)?;
            match selector {
                MessageSelector::Unary(name) => Some(format!("{receiver_str} {name}")),
                _ => None,
            }
        }
        Expression::FieldAccess {
            receiver, field, ..
        } => {
            let receiver_str = display_receiver(receiver)?;
            Some(format!("{receiver_str}.{}", field.name))
        }
        _ => None,
    }
}

/// Returns `true` if `expr` is an identifier named `"true"` or `"false"`.
fn is_boolean_identifier(expr: &Expression) -> Option<&str> {
    if let Expression::Identifier(Identifier { name, .. }) = expr {
        let s = name.as_str();
        if s == "true" || s == "false" {
            return Some(s);
        }
    }
    None
}

/// Checks and reports redundant boolean literal comparisons.
///
/// Detects both `x = true` (boolean as argument) and `true = x` (boolean as receiver).
fn check_boolean_comparison(
    receiver: &Expression,
    arg: &Expression,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Determine which side is the boolean literal and which is the "other" expression.
    let (bool_val, other) = if let Some(val) = is_boolean_identifier(arg) {
        (val, receiver)
    } else if let Some(val) = is_boolean_identifier(receiver) {
        (val, arg)
    } else {
        return;
    };

    let other_repr = display_receiver(other).unwrap_or_else(|| "expr".to_string());
    let (suggestion, hint_text) = if bool_val == "true" {
        (
            format!("use `{other_repr}` instead"),
            format!("Replace with just `{other_repr}`"),
        )
    } else {
        (
            format!("use `{other_repr} not` instead"),
            format!("Replace with `{other_repr} not`"),
        )
    };
    let mut diag = Diagnostic::lint(format!("Redundant boolean comparison: {suggestion}"), span);
    diag.hint = Some(hint_text.into());
    diagnostics.push(diag);
}

fn walk_expression(expr: &Expression, diagnostics: &mut Vec<Diagnostic>) {
    // Check this node.
    if let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        span,
        ..
    } = expr
    {
        if matches!(selector, MessageSelector::Binary(op) if op == "=") {
            if let Some(arg) = arguments.first() {
                check_boolean_comparison(receiver, arg, *span, diagnostics);
            }
        }
    }

    // Recurse into children.
    match expr {
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            walk_expression(receiver, diagnostics);
            for arg in arguments {
                walk_expression(arg, diagnostics);
            }
        }
        Expression::Block(block) => {
            for e in &block.body {
                walk_expression(e, diagnostics);
            }
        }
        Expression::Assignment { value, .. } | Expression::Return { value, .. } => {
            walk_expression(value, diagnostics);
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            walk_expression(receiver, diagnostics);
            for msg in messages {
                for arg in &msg.arguments {
                    walk_expression(arg, diagnostics);
                }
            }
        }
        Expression::Parenthesized { expression, .. } => {
            walk_expression(expression, diagnostics);
        }
        Expression::FieldAccess { receiver, .. } => {
            walk_expression(receiver, diagnostics);
        }
        Expression::Match { value, arms, .. } => {
            walk_expression(value, diagnostics);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    walk_expression(guard, diagnostics);
                }
                walk_expression(&arm.body, diagnostics);
            }
        }
        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                walk_expression(&pair.key, diagnostics);
                walk_expression(&pair.value, diagnostics);
            }
        }
        Expression::ListLiteral { elements, tail, .. } => {
            for elem in elements {
                walk_expression(elem, diagnostics);
            }
            if let Some(t) = tail {
                walk_expression(t, diagnostics);
            }
        }
        Expression::StringInterpolation { segments, .. } => {
            for seg in segments {
                if let crate::ast::StringSegment::Interpolation(e) = seg {
                    walk_expression(e, diagnostics);
                }
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use crate::lint::run_lint_passes;
    use crate::source_analysis::{Severity, lex_with_eof, parse};

    fn lint(source: &str) -> Vec<crate::source_analysis::Diagnostic> {
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        run_lint_passes(&module)
    }

    #[test]
    fn comparison_to_true_is_flagged() {
        let diags = lint("Object subclass: Foo\n  check: x => x = true\n");
        assert_eq!(
            diags.len(),
            1,
            "expected one lint diagnostic, got: {diags:?}"
        );
        assert_eq!(diags[0].severity, Severity::Lint);
        assert!(
            diags[0].message.contains("use `x` instead"),
            "message: {}",
            diags[0].message
        );
    }

    #[test]
    fn comparison_to_false_is_flagged() {
        let diags = lint("Object subclass: Foo\n  check: x => x = false\n");
        assert_eq!(
            diags.len(),
            1,
            "expected one lint diagnostic, got: {diags:?}"
        );
        assert_eq!(diags[0].severity, Severity::Lint);
        assert!(
            diags[0].message.contains("use `x not` instead"),
            "message: {}",
            diags[0].message
        );
    }

    #[test]
    fn lint_diagnostic_has_hint() {
        let diags = lint("Object subclass: Foo\n  check: x => x = true\n");
        assert_eq!(diags.len(), 1);
        assert!(
            diags[0].hint.is_some(),
            "lint diagnostic should include a fix hint"
        );
    }

    #[test]
    fn comparison_without_boolean_is_clean() {
        let diags = lint("Object subclass: Foo\n  check: x => x = 42\n");
        assert!(diags.is_empty(), "expected no diagnostics, got: {diags:?}");
    }

    #[test]
    fn unrelated_equality_is_clean() {
        let diags = lint("Object subclass: Foo\n  check: x => x = 'hello'\n");
        assert!(diags.is_empty(), "expected no diagnostics, got: {diags:?}");
    }

    #[test]
    fn boolean_in_other_message_is_clean() {
        // `x ifTrue: [...]` does not involve = comparison; should be clean
        let diags = lint("Object subclass: Foo\n  run: x => x ifTrue: [42]\n");
        assert!(diags.is_empty(), "expected no diagnostics, got: {diags:?}");
    }

    #[test]
    fn comparison_to_true_in_block_is_flagged() {
        let diags = lint("Object subclass: Foo\n  run: x => [x = true] value\n");
        assert_eq!(diags.len(), 1, "expected lint inside block, got: {diags:?}");
    }

    #[test]
    fn reverse_true_equals_x_is_flagged() {
        let diags = lint("Object subclass: Foo\n  check: x => true = x\n");
        assert_eq!(
            diags.len(),
            1,
            "expected lint for reversed comparison, got: {diags:?}"
        );
        assert!(
            diags[0].message.contains("use `x` instead"),
            "message: {}",
            diags[0].message
        );
    }

    #[test]
    fn reverse_false_equals_x_is_flagged() {
        let diags = lint("Object subclass: Foo\n  check: x => false = x\n");
        assert_eq!(
            diags.len(),
            1,
            "expected lint for reversed comparison, got: {diags:?}"
        );
        assert!(
            diags[0].message.contains("use `x not` instead"),
            "message: {}",
            diags[0].message
        );
    }

    #[test]
    fn comparison_to_false_hint_is_correct() {
        let diags = lint("Object subclass: Foo\n  check: x => x = false\n");
        assert_eq!(diags.len(), 1);
        let hint = diags[0].hint.as_ref().expect("hint should be present");
        assert!(
            hint.contains("x not"),
            "hint should mention 'x not', got: {hint}"
        );
    }

    #[test]
    fn nested_boolean_comparisons_both_flagged() {
        // Both `x = true` and `y = false` should be independently flagged.
        let src = "Object subclass: Foo\n  run: x => (x = true) and: [x = false]\n";
        let diags = lint(src);
        assert_eq!(
            diags.len(),
            2,
            "expected two lint diagnostics, got: {diags:?}"
        );
    }
}
