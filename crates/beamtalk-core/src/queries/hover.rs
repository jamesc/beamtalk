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

use crate::ast::{Expression, Literal, Module};
use crate::language_service::{HoverInfo, Position};

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

/// Recursively searches for hover information in an expression.
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
            selector: _,
            arguments,
            span: _,
        } => {
            // Check if hovering over the selector
            // For now, just recurse into receiver and arguments
            find_hover_in_expr(receiver, offset).or_else(|| {
                arguments
                    .iter()
                    .find_map(|arg| find_hover_in_expr(arg, offset))
            })
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
        } => find_hover_in_expr(receiver, offset).or_else(|| {
            messages.iter().find_map(|msg| {
                msg.arguments
                    .iter()
                    .find_map(|arg| find_hover_in_expr(arg, offset))
            })
        }),
        Expression::Pipe { value, target, .. } => {
            find_hover_in_expr(value, offset).or_else(|| find_hover_in_expr(target, offset))
        }
        Expression::Await { future, .. } => find_hover_in_expr(future, offset),
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
}
