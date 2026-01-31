// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Pratt parsing module for binary operator precedence.
//!
//! This module provides an alternative implementation for parsing binary
//! operators using Pratt parsing (top-down operator precedence parsing).
//! The main advantage is that adding new operators or precedence levels
//! requires only updating a precedence table, not modifying the parser structure.
//!
//! # References
//!
//! - [Pratt Parsing Made Easy](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html)
//! - [rust-analyzer parser](https://github.com/rust-lang/rust-analyzer/tree/master/crates/parser)
//!
//! # Example
//!
//! ```ignore
//! // To add a new operator (e.g., `**` for exponentiation):
//! // 1. Add to PRECEDENCE_TABLE: ("**", 50, 51) // right-associative: right > left
//! // That's it! No structural changes needed.
//! ```
//!
//! # Status
//!
//! This is a prototype for evaluation. The types and functions are public
//! for testing but not yet integrated into the main parser.

// Allow unused code since this is a prototype module for evaluation
#![allow(dead_code)]

use crate::ast::{Expression, MessageSelector};
use crate::parse::{Token, TokenKind};

/// Binding power for binary operators.
///
/// Higher values bind tighter. Left and right binding powers differ
/// for associativity:
/// - Left-associative: `left_bp == right_bp - 1` (e.g., `+`, `-`)
/// - Right-associative: `left_bp == right_bp + 1` (e.g., `**`)
#[derive(Debug, Clone, Copy)]
pub struct BindingPower {
    /// Left binding power (how tightly this operator binds to its left operand).
    pub left: u8,
    /// Right binding power (how tightly this operator binds to its right operand).
    pub right: u8,
}

impl BindingPower {
    /// Creates a left-associative binding power.
    #[must_use]
    pub const fn left_assoc(precedence: u8) -> Self {
        Self {
            left: precedence,
            right: precedence + 1,
        }
    }

    /// Creates a right-associative binding power.
    #[must_use]
    pub const fn right_assoc(precedence: u8) -> Self {
        Self {
            left: precedence + 1,
            right: precedence,
        }
    }
}

/// Gets the binding power for a binary operator.
///
/// Returns `None` for unknown operators, allowing the parser to treat
/// them as end of expression (useful for error recovery).
///
/// # Precedence Levels (from lowest to highest)
///
/// | Level | Operators | Associativity |
/// |-------|-----------|---------------|
/// | 10 | `=` `!=` | Left |
/// | 20 | `<` `>` `<=` `>=` | Left |
/// | 30 | `+` `-` | Left |
/// | 40 | `*` `/` `%` | Left |
///
/// To add a new operator, just add an entry here. For example:
/// ```ignore
/// // Bitwise OR (between comparison and additive)
/// "|" => Some(BindingPower::left_assoc(25)),
///
/// // Exponentiation (right-associative, higher than multiplication)
/// "**" => Some(BindingPower::right_assoc(50)),
/// ```
#[must_use]
pub fn binary_binding_power(op: &str) -> Option<BindingPower> {
    match op {
        // Equality (lowest binary precedence)
        "=" | "!=" => Some(BindingPower::left_assoc(10)),

        // Comparison
        "<" | ">" | "<=" | ">=" => Some(BindingPower::left_assoc(20)),

        // Additive
        "+" | "-" => Some(BindingPower::left_assoc(30)),

        // Multiplicative
        "*" | "/" | "%" => Some(BindingPower::left_assoc(40)),

        // Unknown operator - return None to stop binary expression parsing
        _ => None,
    }
}

/// Pratt parser for binary expressions.
///
/// This function implements the core Pratt parsing algorithm. It can be
/// integrated into the main parser by replacing the precedence-climbing
/// methods (`parse_comparison`, `parse_additive`, `parse_multiplicative`)
/// with a single call to this function.
///
/// # Arguments
///
/// * `tokens` - The token stream (current position tracked externally)
/// * `current` - Mutable reference to current token index
/// * `min_bp` - Minimum binding power to continue parsing
/// * `parse_unary` - Function to parse unary expressions (the "atoms")
///
/// # Returns
///
/// The parsed expression with correct precedence structure.
pub fn parse_binary_expr<F>(
    tokens: &[Token],
    current: &mut usize,
    min_bp: u8,
    parse_unary: &mut F,
) -> Expression
where
    F: FnMut(&[Token], &mut usize) -> Expression,
{
    // Parse the left-hand side (unary expression)
    let mut left = parse_unary(tokens, current);

    while let TokenKind::BinarySelector(op) = get_token(tokens, *current).kind() {
        let op = op.clone();

        // Get binding power; unknown operators end the expression
        let Some(bp) = binary_binding_power(&op) else {
            break;
        };

        // Stop if this operator binds less tightly than our minimum
        if bp.left < min_bp {
            break;
        }

        // Consume the operator
        let _op_token = advance(tokens, current);

        // Parse the right-hand side with the operator's right binding power
        let right = parse_binary_expr(tokens, current, bp.right, parse_unary);

        // Build the message send expression
        let span = left.span().merge(right.span());
        left = Expression::MessageSend {
            receiver: Box::new(left),
            selector: MessageSelector::Binary(op),
            arguments: vec![right],
            span,
        };
    }

    left
}

// Helper functions to access tokens (match main parser's interface)

fn get_token(tokens: &[Token], index: usize) -> &Token {
    tokens
        .get(index)
        .unwrap_or_else(|| tokens.last().expect("Expected at least EOF token"))
}

fn advance(tokens: &[Token], current: &mut usize) -> Token {
    let token = get_token(tokens, *current).clone();
    if !matches!(token.kind(), TokenKind::Eof) {
        *current += 1;
    }
    token
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Literal;
    use crate::parse::lex_with_eof;

    // Simple unary parser for testing - just parses integers
    fn parse_simple_unary(tokens: &[Token], current: &mut usize) -> Expression {
        let token = get_token(tokens, *current);
        match token.kind() {
            TokenKind::Integer(n) => {
                let span = token.span();
                let value: i64 = n.parse().unwrap_or(0);
                *current += 1;
                Expression::Literal(Literal::Integer(value), span)
            }
            _ => Expression::Error {
                message: "Expected integer".into(),
                span: token.span(),
            },
        }
    }

    #[test]
    fn test_simple_addition() {
        let tokens = lex_with_eof("1 + 2");
        let mut current = 0;
        let expr = parse_binary_expr(&tokens, &mut current, 0, &mut parse_simple_unary);

        // Should be MessageSend(1, +, [2])
        assert!(matches!(expr, Expression::MessageSend { .. }));
        if let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = expr
        {
            assert!(matches!(
                *receiver,
                Expression::Literal(Literal::Integer(1), _)
            ));
            assert_eq!(selector, MessageSelector::Binary("+".into()));
            assert_eq!(arguments.len(), 1);
            assert!(matches!(
                arguments[0],
                Expression::Literal(Literal::Integer(2), _)
            ));
        }
    }

    #[test]
    fn test_precedence_mul_over_add() {
        // 1 + 2 * 3 should parse as 1 + (2 * 3)
        let tokens = lex_with_eof("1 + 2 * 3");
        let mut current = 0;
        let expr = parse_binary_expr(&tokens, &mut current, 0, &mut parse_simple_unary);

        // Top level should be: MessageSend(1, +, [MessageSend(2, *, [3])])
        if let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = expr
        {
            assert!(matches!(
                *receiver,
                Expression::Literal(Literal::Integer(1), _)
            ));
            assert_eq!(selector, MessageSelector::Binary("+".into()));
            assert_eq!(arguments.len(), 1);

            if let Expression::MessageSend {
                receiver: inner_recv,
                selector: inner_sel,
                arguments: inner_args,
                ..
            } = &arguments[0]
            {
                assert!(matches!(
                    **inner_recv,
                    Expression::Literal(Literal::Integer(2), _)
                ));
                assert_eq!(*inner_sel, MessageSelector::Binary("*".into()));
                assert!(matches!(
                    inner_args[0],
                    Expression::Literal(Literal::Integer(3), _)
                ));
            } else {
                panic!("Expected nested MessageSend for 2 * 3");
            }
        } else {
            panic!("Expected MessageSend for 1 + ...");
        }
    }

    #[test]
    fn test_left_associativity() {
        // 1 - 2 - 3 should parse as (1 - 2) - 3
        let tokens = lex_with_eof("1 - 2 - 3");
        let mut current = 0;
        let expr = parse_binary_expr(&tokens, &mut current, 0, &mut parse_simple_unary);

        // Top level should be: MessageSend(MessageSend(1, -, [2]), -, [3])
        if let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = expr
        {
            assert_eq!(selector, MessageSelector::Binary("-".into()));
            assert!(matches!(
                arguments[0],
                Expression::Literal(Literal::Integer(3), _)
            ));

            if let Expression::MessageSend {
                receiver: inner_recv,
                selector: inner_sel,
                arguments: inner_args,
                ..
            } = *receiver
            {
                assert!(matches!(
                    *inner_recv,
                    Expression::Literal(Literal::Integer(1), _)
                ));
                assert_eq!(inner_sel, MessageSelector::Binary("-".into()));
                assert!(matches!(
                    inner_args[0],
                    Expression::Literal(Literal::Integer(2), _)
                ));
            } else {
                panic!("Expected nested MessageSend for 1 - 2");
            }
        } else {
            panic!("Expected MessageSend");
        }
    }

    #[test]
    fn test_comparison_lowest_precedence() {
        // 1 + 2 < 3 * 4 should parse as (1 + 2) < (3 * 4)
        let tokens = lex_with_eof("1 + 2 < 3 * 4");
        let mut current = 0;
        let expr = parse_binary_expr(&tokens, &mut current, 0, &mut parse_simple_unary);

        if let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = expr
        {
            assert_eq!(selector, MessageSelector::Binary("<".into()));

            // Left side: 1 + 2
            if let Expression::MessageSend {
                selector: left_sel, ..
            } = *receiver
            {
                assert_eq!(left_sel, MessageSelector::Binary("+".into()));
            } else {
                panic!("Expected 1 + 2 on left");
            }

            // Right side: 3 * 4
            if let Expression::MessageSend {
                selector: right_sel,
                ..
            } = &arguments[0]
            {
                assert_eq!(*right_sel, MessageSelector::Binary("*".into()));
            } else {
                panic!("Expected 3 * 4 on right");
            }
        } else {
            panic!("Expected comparison at top level");
        }
    }

    #[test]
    fn test_binding_power_left_assoc() {
        let bp = BindingPower::left_assoc(30);
        assert_eq!(bp.left, 30);
        assert_eq!(bp.right, 31);
    }

    #[test]
    fn test_binding_power_right_assoc() {
        let bp = BindingPower::right_assoc(50);
        assert_eq!(bp.left, 51);
        assert_eq!(bp.right, 50);
    }

    #[test]
    fn test_unknown_operator_stops_parsing() {
        // Using `++` which is not in our precedence table
        // This should parse just `1` and stop
        let tokens = lex_with_eof("1 ++ 2");
        let mut current = 0;
        let expr = parse_binary_expr(&tokens, &mut current, 0, &mut parse_simple_unary);

        // Should only parse the `1`
        assert!(matches!(expr, Expression::Literal(Literal::Integer(1), _)));
        // current should be at `++` token
        assert_eq!(current, 1);
    }
}
