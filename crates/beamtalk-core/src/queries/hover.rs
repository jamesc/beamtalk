// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Hover information query implementation.
//!
//! This module provides hover information for the language service.
//!
//! # Design
//!
//! Hover information shows:
//! - Type signatures (future work - requires type system)
//! - Documentation comments
//! - Value information for constants
//!
//! # Performance
//!
//! Must respond in <50ms for typical file sizes.

use crate::ast::{Expression, Literal, MessageSelector, Module};
use crate::language_service::{HoverInfo, Position};
use crate::parse::Span;

/// Computes hover information at a given position.
///
/// # Arguments
///
/// * `module` - The parsed AST
/// * `source` - The source text
/// * `position` - The cursor position
///
/// # Returns
///
/// Hover information if the position is over a relevant symbol, `None` otherwise.
#[must_use]
#[expect(
    clippy::cast_possible_truncation,
    reason = "source files over 4GB are not supported"
)]
pub fn compute_hover(module: &Module, source: &str, position: Position) -> Option<HoverInfo> {
    let offset = position.to_offset(source)? as u32;

    // Find the expression at this position
    for expr in &module.expressions {
        if let Some(hover) = find_hover_in_expr(expr, offset) {
            return Some(hover);
        }
    }

    None
}

/// Creates hover information for a message selector.
fn selector_hover_info(selector: &MessageSelector, span: Span) -> HoverInfo {
    let (kind, name, arity) = match selector {
        MessageSelector::Unary(name) => ("Unary message", name.as_str(), 0),
        MessageSelector::Binary(op) => ("Binary message", op.as_str(), 1),
        MessageSelector::Keyword(parts) => {
            let name = selector.name();
            return HoverInfo::new(
                format!("Keyword message: `{name}` (arity: {})", parts.len()),
                span,
            )
            .with_documentation("Keyword messages have named arguments ending in ':'");
        }
    };
    HoverInfo::new(format!("{kind}: `{name}` (arity: {arity})"), span)
}

/// Recursively searches for hover information in an expression.
#[expect(clippy::too_many_lines, reason = "match on Expression variants")]
fn find_hover_in_expr(expr: &Expression, offset: u32) -> Option<HoverInfo> {
    let span = expr.span();
    if offset < span.start() || offset >= span.end() {
        return None;
    }

    match expr {
        Expression::Identifier(ident) => {
            if offset >= ident.span.start() && offset < ident.span.end() {
                Some(HoverInfo::new(
                    format!("Identifier: `{}`", ident.name),
                    ident.span,
                ))
            } else {
                None
            }
        }
        Expression::Literal(lit, span) => {
            if offset >= span.start() && offset < span.end() {
                let info = match lit {
                    Literal::Integer(n) => format!("Integer: `{n}`"),
                    Literal::Float(f) => format!("Float: `{f}`"),
                    Literal::String(s) => format!("String: `\"{s}\"`"),
                    Literal::Symbol(s) => format!("Symbol: `#{s}`"),
                    Literal::Array(_) => "Array literal".to_string(),
                    Literal::Character(c) => format!("Character: `${c}`"),
                };
                Some(HoverInfo::new(info, *span))
            } else {
                None
            }
        }
        Expression::Assignment { target, value, .. } => {
            find_hover_in_expr(target, offset).or_else(|| find_hover_in_expr(value, offset))
        }
        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            span,
        } => {
            // First check receiver and arguments
            if let Some(hover) = find_hover_in_expr(receiver, offset) {
                return Some(hover);
            }
            if let Some(hover) = arguments
                .iter()
                .find_map(|arg| find_hover_in_expr(arg, offset))
            {
                return Some(hover);
            }

            // If we're in the message send span but not in receiver/arguments,
            // we're likely hovering on the selector
            if offset >= span.start() && offset < span.end() {
                // Provide hover info for the selector
                let hover_info = selector_hover_info(selector, *span);
                return Some(hover_info);
            }
            None
        }
        Expression::Block(block) => block
            .body
            .iter()
            .find_map(|expr| find_hover_in_expr(expr, offset)),
        Expression::Return { value, .. } => find_hover_in_expr(value, offset),
        Expression::Parenthesized { expression, .. } => find_hover_in_expr(expression, offset),
        Expression::FieldAccess {
            receiver, field, ..
        } => {
            if offset >= field.span.start() && offset < field.span.end() {
                Some(HoverInfo::new(
                    format!("Field: `{}`", field.name),
                    field.span,
                ))
            } else {
                find_hover_in_expr(receiver, offset)
            }
        }
        Expression::CompoundAssignment { target, value, .. } => {
            find_hover_in_expr(target, offset).or_else(|| find_hover_in_expr(value, offset))
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            if let Some(hover) = find_hover_in_expr(receiver, offset) {
                return Some(hover);
            }
            // Check each cascaded message
            for msg in messages {
                // Check arguments
                if let Some(hover) = msg
                    .arguments
                    .iter()
                    .find_map(|arg| find_hover_in_expr(arg, offset))
                {
                    return Some(hover);
                }
                // Check if offset is within this message's span (selector area)
                if offset >= msg.span.start() && offset < msg.span.end() {
                    return Some(selector_hover_info(&msg.selector, msg.span));
                }
            }
            None
        }
        Expression::Pipe { value, target, .. } => {
            find_hover_in_expr(value, offset).or_else(|| find_hover_in_expr(target, offset))
        }
        Expression::Match { value, arms, .. } => find_hover_in_expr(value, offset).or_else(|| {
            arms.iter()
                .find_map(|arm| find_hover_in_expr(&arm.body, offset))
        }),
        Expression::Error { message, span } => {
            if offset >= span.start() && offset < span.end() {
                Some(HoverInfo::new(format!("Error: {message}"), *span))
            } else {
                None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::{lex_with_eof, parse};

    #[test]
    fn compute_hover_on_identifier() {
        let source = "x := 42";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);

        let hover = compute_hover(&module, source, Position::new(0, 0));
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(hover.contents.contains('x'));
    }

    #[test]
    fn compute_hover_on_integer_literal() {
        let source = "x := 42";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);

        // Position 5 is at "42"
        let hover = compute_hover(&module, source, Position::new(0, 5));
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(hover.contents.contains("42"));
    }

    #[test]
    fn compute_hover_on_string_literal() {
        let source = r#"x := "hello""#;
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);

        // Position 5 is at the string
        let hover = compute_hover(&module, source, Position::new(0, 5));
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(hover.contents.contains("String"));
    }

    #[test]
    fn compute_hover_outside_expr_returns_none() {
        let source = "x := 42";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);

        // Position 100 is way out of bounds
        let hover = compute_hover(&module, source, Position::new(10, 0));
        assert!(hover.is_none());
    }

    #[test]
    fn compute_hover_on_binary_message() {
        let source = "x := 3 + 4";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);

        // Position 7 is at the "+" operator
        let hover = compute_hover(&module, source, Position::new(0, 7));
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(hover.contents.contains("Binary message"));
        assert!(hover.contents.contains('+'));
    }

    #[test]
    fn compute_hover_on_unary_message() {
        let source = "x := obj size";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);

        // Position 9 is at "size"
        let hover = compute_hover(&module, source, Position::new(0, 9));
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(hover.contents.contains("Unary message"));
        assert!(hover.contents.contains("size"));
    }
}
