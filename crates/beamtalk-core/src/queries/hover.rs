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

            // At this point we know we're not hovering over the receiver or any argument.
            // Compute a more precise selector span based on selector type.
            let receiver_span = receiver.span();
            let in_selector = match selector {
                // For unary and binary messages, the selector appears between the end
                // of the receiver and the start of the first argument (if any), or
                // up to the end of the message send when there are no arguments.
                MessageSelector::Unary(_) | MessageSelector::Binary(_) => {
                    let start = receiver_span.end();
                    let end = arguments
                        .first()
                        .map_or_else(|| span.end(), |arg| arg.span().start());
                    offset >= start && offset < end
                }
                // For keyword messages, selector parts are interleaved with arguments.
                // We approximate by treating any position within the message send span
                // (that's not the receiver or an argument) as being over the selector.
                MessageSelector::Keyword(_) => offset >= span.start() && offset < span.end(),
            };

            if in_selector {
                // Compute a span for the selector region
                let selector_span = match selector {
                    MessageSelector::Unary(_) | MessageSelector::Binary(_) => {
                        let start = receiver_span.end();
                        let end = arguments
                            .first()
                            .map_or_else(|| span.end(), |arg| arg.span().start());
                        Span::new(start, end)
                    }
                    // For keyword messages, use the full span since parts are interleaved
                    MessageSelector::Keyword(_) => *span,
                };
                return Some(selector_hover_info(selector, selector_span));
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
                // Check arguments first
                if let Some(hover) = msg
                    .arguments
                    .iter()
                    .find_map(|arg| find_hover_in_expr(arg, offset))
                {
                    return Some(hover);
                }
                // Compute selector span: from message start to first argument (or message end)
                if offset >= msg.span.start() && offset < msg.span.end() {
                    let selector_end = msg
                        .arguments
                        .first()
                        .map_or_else(|| msg.span.end(), |arg| arg.span().start());
                    let selector_span = Span::new(msg.span.start(), selector_end);

                    // Only show hover if we're in the selector region (before first arg)
                    if offset >= msg.span.start() && offset < selector_end {
                        return Some(selector_hover_info(&msg.selector, selector_span));
                    }
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
        Expression::MapLiteral { entries, span } => {
            if offset >= span.start() && offset < span.end() {
                // Check if hovering over a specific key or value
                for entry in entries {
                    if let Some(info) = find_hover_in_expr(&entry.key, offset) {
                        return Some(info);
                    }
                    if let Some(info) = find_hover_in_expr(&entry.value, offset) {
                        return Some(info);
                    }
                }
                // Hovering over the map literal itself
                Some(HoverInfo::new(
                    format!("Map literal with {} entries", entries.len()),
                    *span,
                ))
            } else {
                None
            }
        }
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
