// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Recursive descent parser for Beamtalk source code.
//!
//! This parser builds an AST from a stream of tokens. It is designed
//! for IDE use with comprehensive error recovery and diagnostics.
//!
//! # Design Philosophy
//!
//! - **Error recovery is mandatory** - Parser MUST always produce an AST
//! - **Multiple errors** - Report all errors, don't stop at first
//! - **Precise spans** - Every diagnostic points to exact source location
//! - **Synchronization points** - Recover at statement boundaries
//!
//! # Message Precedence
//!
//! Beamtalk follows Smalltalk's three-tier message precedence:
//!
//! 1. **Unary messages**: `object message` (highest precedence)
//! 2. **Binary messages**: `3 + 4` with **standard math precedence**
//!    - `* / %` before `+ -`
//!    - NOT strictly left-to-right like traditional Smalltalk!
//! 3. **Keyword messages**: `array at: 1 put: 'x'` (lowest precedence)
//!
//! # Usage
//!
//! ```
//! use beamtalk_core::parse::{lex_with_eof, parse};
//!
//! let tokens = lex_with_eof("x := 3 + 4");
//! let (module, diagnostics) = parse(tokens);
//!
//! assert!(diagnostics.is_empty());
//! assert_eq!(module.expressions.len(), 1);
//! ```

use crate::ast::{
    Block, BlockParameter, CascadeMessage, Comment, CommentKind, CompoundOperator, Expression,
    Identifier, KeywordPart, Literal, MessageSelector, Module,
};
use crate::parse::{Span, Token, TokenKind};
use ecow::EcoString;

/// Parse a sequence of tokens into a module.
///
/// This is the main entry point for parsing. It always returns a [`Module`],
/// even if there are syntax errors. Check the returned diagnostics for errors.
///
/// # Examples
///
/// ```
/// use beamtalk_core::parse::{lex_with_eof, parse};
///
/// let tokens = lex_with_eof("x := 42");
/// let (module, diagnostics) = parse(tokens);
///
/// assert!(diagnostics.is_empty());
/// assert_eq!(module.expressions.len(), 1);
/// ```
#[must_use]
pub fn parse(tokens: Vec<Token>) -> (Module, Vec<Diagnostic>) {
    let mut parser = Parser::new(tokens);
    let module = parser.parse_module();
    (module, parser.diagnostics)
}

/// A diagnostic message (error or warning).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    /// The severity of the diagnostic.
    pub severity: Severity,
    /// The error message.
    pub message: EcoString,
    /// The source location.
    pub span: Span,
}

impl Diagnostic {
    /// Creates a new error diagnostic.
    #[must_use]
    pub fn error(message: impl Into<EcoString>, span: Span) -> Self {
        Self {
            severity: Severity::Error,
            message: message.into(),
            span,
        }
    }

    /// Creates a new warning diagnostic.
    #[must_use]
    pub fn warning(message: impl Into<EcoString>, span: Span) -> Self {
        Self {
            severity: Severity::Warning,
            message: message.into(),
            span,
        }
    }
}

/// Diagnostic severity level.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Severity {
    /// An error that prevents compilation.
    Error,
    /// A warning that should be addressed.
    Warning,
}

/// The parser state.
struct Parser {
    /// The tokens being parsed.
    tokens: Vec<Token>,
    /// Current token index.
    current: usize,
    /// Accumulated diagnostics.
    diagnostics: Vec<Diagnostic>,
}

impl Parser {
    /// Creates a new parser for the given tokens.
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            diagnostics: Vec::new(),
        }
    }

    // ========================================================================
    // Token Management
    // ========================================================================

    /// Returns the current token.
    fn current_token(&self) -> &Token {
        self.tokens.get(self.current).unwrap_or_else(|| {
            // Should never happen if lexer adds EOF token
            panic!("Parser advanced past end of tokens")
        })
    }

    /// Returns the current token kind.
    fn current_kind(&self) -> &TokenKind {
        self.current_token().kind()
    }

    /// Peeks at the next token without consuming.
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current + 1)
    }

    /// Peeks at the next token kind.
    fn peek_kind(&self) -> Option<&TokenKind> {
        self.peek().map(Token::kind)
    }

    /// Checks if we're at the end of input.
    fn is_at_end(&self) -> bool {
        matches!(self.current_kind(), TokenKind::Eof)
    }

    /// Advances to the next token and returns the previous one.
    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.tokens[self.current - 1].clone()
    }

    /// Checks if the current token matches the given kind.
    fn check(&self, kind: &TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }
        std::mem::discriminant(self.current_kind()) == std::mem::discriminant(kind)
    }

    /// Consumes the current token if it matches the given kind.
    fn match_token(&mut self, kind: &TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Expects the current token to match the given kind, advancing if it does.
    ///
    /// If the token doesn't match, reports an error and returns `None`.
    fn expect(&mut self, kind: &TokenKind, message: &str) -> Option<Token> {
        if self.check(kind) {
            Some(self.advance())
        } else {
            let span = self.current_token().span();
            self.diagnostics.push(Diagnostic::error(message, span));
            None
        }
    }

    // ========================================================================
    // Error Handling & Recovery
    // ========================================================================

    /// Reports an error at the current token.
    fn error(&mut self, message: impl Into<EcoString>) {
        let span = self.current_token().span();
        self.diagnostics.push(Diagnostic::error(message, span));
    }

    /// Creates an error expression node.
    fn error_expression(&mut self, message: impl Into<EcoString>) -> Expression {
        let span = self.current_token().span();
        let msg: EcoString = message.into();
        self.diagnostics.push(Diagnostic::error(msg.clone(), span));
        Expression::Error { message: msg, span }
    }

    /// Synchronizes parser to a safe recovery point.
    ///
    /// Advances until we find a statement boundary or synchronization point:
    /// - Period (`.`) - statement terminator
    /// - Right bracket (`]`) - block end
    /// - Right paren (`)`) - expression end
    /// - Right brace (`}`) - tuple end
    /// - Semicolon (`;`) - cascade separator
    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            // Found a statement boundary
            if matches!(
                self.current_kind(),
                TokenKind::Period
                    | TokenKind::RightBracket
                    | TokenKind::RightParen
                    | TokenKind::RightBrace
                    | TokenKind::Semicolon
            ) {
                return;
            }

            // New statement may be starting
            if matches!(self.current_kind(), TokenKind::Caret) {
                return;
            }

            self.advance();
        }
    }

    // ========================================================================
    // Module Parsing
    // ========================================================================

    /// Parses a complete module (top-level).
    fn parse_module(&mut self) -> Module {
        let start = self.current_token().span();
        let mut expressions = Vec::new();
        let mut comments = Vec::new();

        // Collect leading comments
        for trivia in self.current_token().leading_trivia() {
            if trivia.is_comment() {
                comments.push(Comment {
                    content: trivia.as_str().into(),
                    span: start, // TODO: track trivia spans
                    kind: match trivia {
                        super::Trivia::LineComment(_) => CommentKind::Line,
                        super::Trivia::BlockComment(_) => CommentKind::Block,
                        super::Trivia::Whitespace(_) => continue,
                    },
                });
            }
        }

        // Parse statements until EOF
        while !self.is_at_end() {
            match self.parse_expression() {
                expr if expr.is_error() => {
                    // Error already reported, try to recover
                    self.synchronize();
                }
                expr => {
                    expressions.push(expr);
                    // Optional statement terminator
                    self.match_token(&TokenKind::Period);
                }
            }
        }

        // Get end span
        let end = if self.current > 0 {
            self.tokens[self.current - 1].span()
        } else {
            start
        };
        let span = start.merge(end);

        Module::with_comments(expressions, span, comments)
    }

    // ========================================================================
    // Expression Parsing
    // ========================================================================

    /// Parses any expression.
    ///
    /// Entry point for expression parsing. Handles all precedence levels.
    fn parse_expression(&mut self) -> Expression {
        // Check for return statement first
        if self.match_token(&TokenKind::Caret) {
            return self.parse_return();
        }

        // Try to parse assignment or regular expression
        self.parse_assignment()
    }

    /// Parses a return statement.
    fn parse_return(&mut self) -> Expression {
        let start = self.tokens[self.current - 1].span();
        let value = Box::new(self.parse_assignment());
        let end = value.span();
        let span = start.merge(end);

        Expression::Return { value, span }
    }

    /// Parses an assignment or regular expression.
    fn parse_assignment(&mut self) -> Expression {
        let expr = self.parse_cascade();

        // Check for assignment operators
        if self.match_token(&TokenKind::Assign) {
            let value = Box::new(self.parse_assignment());
            let span = expr.span().merge(value.span());
            return Expression::Assignment {
                target: Box::new(expr),
                value,
                span,
            };
        }

        // Check for compound assignment (+=, -=, etc.)
        if let Some(op) = self.match_compound_operator() {
            let value = Box::new(self.parse_assignment());
            let span = expr.span().merge(value.span());
            return Expression::CompoundAssignment {
                target: Box::new(expr),
                operator: op,
                value,
                span,
            };
        }

        expr
    }

    /// Checks for compound assignment operators.
    fn match_compound_operator(&mut self) -> Option<CompoundOperator> {
        let kind = self.current_kind();
        let op = match kind {
            TokenKind::BinarySelector(s) if s.as_str() == "+=" => CompoundOperator::Add,
            TokenKind::BinarySelector(s) if s.as_str() == "-=" => CompoundOperator::Subtract,
            TokenKind::BinarySelector(s) if s.as_str() == "*=" => CompoundOperator::Multiply,
            TokenKind::BinarySelector(s) if s.as_str() == "/=" => CompoundOperator::Divide,
            TokenKind::BinarySelector(s) if s.as_str() == "%=" => CompoundOperator::Remainder,
            _ => return None,
        };
        self.advance();
        Some(op)
    }

    /// Parses a cascade (multiple messages to the same receiver).
    ///
    /// Syntax: `receiver message1; message2; message3`
    fn parse_cascade(&mut self) -> Expression {
        let receiver = self.parse_keyword_message();

        // Check if this is a cascade
        if !self.match_token(&TokenKind::Semicolon) {
            return receiver;
        }

        // This is a cascade - parse additional messages
        let mut messages = Vec::new();

        loop {
            let message = self.parse_cascade_message();
            messages.push(message);

            // Continue if there's another semicolon
            if !self.match_token(&TokenKind::Semicolon) {
                break;
            }
        }

        let last_span = messages.last().map_or(receiver.span(), |m| m.span);
        let span = receiver.span().merge(last_span);

        Expression::Cascade {
            receiver: Box::new(receiver),
            messages,
            span,
        }
    }

    /// Parses a single message in a cascade.
    ///
    /// This can be a unary, binary, or keyword message.
    fn parse_cascade_message(&mut self) -> CascadeMessage {
        let start_span = self.current_token().span();

        // Try keyword message first
        if matches!(self.current_kind(), TokenKind::Keyword(_)) {
            let mut keywords = Vec::new();
            let mut arguments = Vec::new();

            while let TokenKind::Keyword(keyword) = self.current_kind() {
                let span = self.current_token().span();
                keywords.push(KeywordPart::new(keyword.clone(), span));
                self.advance();

                // Parse argument (which is a binary message)
                arguments.push(self.parse_binary_message());
            }

            let end_span = arguments.last().map_or(start_span, Expression::span);
            let span = start_span.merge(end_span);

            return CascadeMessage::new(MessageSelector::Keyword(keywords), arguments, span);
        }

        // Try binary message
        if let TokenKind::BinarySelector(op) = self.current_kind() {
            let selector = MessageSelector::Binary(op.clone());
            self.advance();
            let arg = self.parse_unary_message();
            let span = start_span.merge(arg.span());
            return CascadeMessage::new(selector, vec![arg], span);
        }

        // Try unary message
        if let TokenKind::Identifier(name) = self.current_kind() {
            let selector = MessageSelector::Unary(name.clone());
            let tok = self.advance();
            let span = start_span.merge(tok.span());
            return CascadeMessage::new(selector, Vec::new(), span);
        }

        // Error - expected a message
        self.error("Expected message selector in cascade");
        CascadeMessage::new(
            MessageSelector::Unary("error".into()),
            Vec::new(),
            start_span,
        )
    }

    /// Parses a keyword message (lowest precedence).
    fn parse_keyword_message(&mut self) -> Expression {
        let receiver = self.parse_binary_message();

        // Check if this is a keyword message
        if !matches!(self.current_kind(), TokenKind::Keyword(_)) {
            return receiver;
        }

        // Parse keyword message
        let mut keywords = Vec::new();
        let mut arguments = Vec::new();

        while let TokenKind::Keyword(keyword) = self.current_kind() {
            let span = self.current_token().span();
            keywords.push(KeywordPart::new(keyword.clone(), span));
            self.advance();

            // Parse argument (which is a binary message)
            arguments.push(self.parse_binary_message());
        }

        let span = receiver.span().merge(arguments.last().unwrap().span());

        Expression::MessageSend {
            receiver: Box::new(receiver),
            selector: MessageSelector::Keyword(keywords),
            arguments,
            span,
        }
    }

    /// Parses a binary message with standard math precedence.
    ///
    /// Unlike traditional Smalltalk (which is strictly left-to-right),
    /// Beamtalk binary messages follow standard operator precedence:
    /// - Multiplicative: `* / %` (higher precedence)
    /// - Additive: `+ -` (lower precedence)
    /// - Comparison: `< > <= >= = !=` (lowest precedence)
    fn parse_binary_message(&mut self) -> Expression {
        self.parse_comparison()
    }

    /// Parses comparison operators (lowest binary precedence).
    fn parse_comparison(&mut self) -> Expression {
        let mut left = self.parse_additive();

        while let TokenKind::BinarySelector(op) = self.current_kind() {
            if !matches!(op.as_str(), "<" | ">" | "<=" | ">=" | "=" | "!=") {
                break;
            }

            let selector = MessageSelector::Binary(op.clone());
            self.advance();
            let right = self.parse_additive();
            let span = left.span().merge(right.span());

            left = Expression::MessageSend {
                receiver: Box::new(left),
                selector,
                arguments: vec![right],
                span,
            };
        }

        left
    }

    /// Parses additive operators (+ and -).
    fn parse_additive(&mut self) -> Expression {
        let mut left = self.parse_multiplicative();

        while let TokenKind::BinarySelector(op) = self.current_kind() {
            if !matches!(op.as_str(), "+" | "-") {
                break;
            }

            let selector = MessageSelector::Binary(op.clone());
            self.advance();
            let right = self.parse_multiplicative();
            let span = left.span().merge(right.span());

            left = Expression::MessageSend {
                receiver: Box::new(left),
                selector,
                arguments: vec![right],
                span,
            };
        }

        left
    }

    /// Parses multiplicative operators (*, /, %).
    fn parse_multiplicative(&mut self) -> Expression {
        let mut left = self.parse_unary_message();

        while let TokenKind::BinarySelector(op) = self.current_kind() {
            if !matches!(op.as_str(), "*" | "/" | "%") {
                break;
            }

            let selector = MessageSelector::Binary(op.clone());
            self.advance();
            let right = self.parse_unary_message();
            let span = left.span().merge(right.span());

            left = Expression::MessageSend {
                receiver: Box::new(left),
                selector,
                arguments: vec![right],
                span,
            };
        }

        left
    }

    /// Parses unary messages (highest message precedence).
    fn parse_unary_message(&mut self) -> Expression {
        let mut receiver = self.parse_primary();

        // Parse chain of unary messages
        while let TokenKind::Identifier(name) = self.current_kind() {
            // Check if this is actually a field access
            if self.peek_kind() == Some(&TokenKind::Period) {
                // This is the end of a statement, not a unary message
                break;
            }

            let selector = MessageSelector::Unary(name.clone());
            let tok = self.advance();
            let span = receiver.span().merge(tok.span());

            receiver = Expression::MessageSend {
                receiver: Box::new(receiver),
                selector,
                arguments: Vec::new(),
                span,
            };
        }

        receiver
    }

    /// Parses a primary expression (literals, identifiers, blocks, parentheses).
    fn parse_primary(&mut self) -> Expression {
        match self.current_kind() {
            // Literals
            TokenKind::Integer(_)
            | TokenKind::Float(_)
            | TokenKind::String(_)
            | TokenKind::Symbol(_)
            | TokenKind::Character(_) => self.parse_literal(),

            // Identifier or field access
            TokenKind::Identifier(_) => self.parse_identifier_or_field_access(),

            // Block
            TokenKind::LeftBracket => self.parse_block(),

            // Parenthesized expression
            TokenKind::LeftParen => self.parse_parenthesized(),

            // Unexpected token
            _ => self.error_expression(format!(
                "Unexpected token: expected expression, found {}",
                self.current_kind()
            )),
        }
    }

    /// Parses a literal value.
    fn parse_literal(&mut self) -> Expression {
        let token = self.advance();
        let span = token.span();

        let literal = match token.into_kind() {
            TokenKind::Integer(s) => {
                // Parse integer (handle radix notation like 16rFF)
                match parse_integer(&s) {
                    Ok(val) => Literal::Integer(val),
                    Err(e) => {
                        self.diagnostics.push(Diagnostic::error(e, span));
                        return Expression::Error {
                            message: "Invalid integer literal".into(),
                            span,
                        };
                    }
                }
            }
            TokenKind::Float(s) => {
                if let Ok(val) = s.parse::<f64>() {
                    Literal::Float(val)
                } else {
                    self.diagnostics.push(Diagnostic::error(
                        format!("Invalid float literal: {s}"),
                        span,
                    ));
                    return Expression::Error {
                        message: "Invalid float literal".into(),
                        span,
                    };
                }
            }
            TokenKind::String(s) => Literal::String(s),
            TokenKind::Symbol(s) => Literal::Symbol(s),
            TokenKind::Character(c) => Literal::Character(c),
            _ => unreachable!(),
        };

        Expression::Literal(literal, span)
    }

    /// Parses an identifier or field access.
    fn parse_identifier_or_field_access(&mut self) -> Expression {
        let name_token = self.advance();
        let TokenKind::Identifier(name) = name_token.kind() else {
            unreachable!()
        };
        let span = name_token.span();

        let mut expr = Expression::Identifier(Identifier::new(name.clone(), span));

        // Check for field access: identifier.field
        while self.match_token(&TokenKind::Period) {
            // Make sure this isn't a statement terminator
            if self.is_at_end() || !matches!(self.current_kind(), TokenKind::Identifier(_)) {
                self.error("Expected field name after '.'");
                break;
            }

            let field_token = self.advance();
            let TokenKind::Identifier(field_name) = field_token.kind() else {
                unreachable!()
            };
            let field_span = field_token.span();

            let full_span = expr.span().merge(field_span);
            expr = Expression::FieldAccess {
                receiver: Box::new(expr),
                field: Identifier::new(field_name.clone(), field_span),
                span: full_span,
            };
        }

        expr
    }

    /// Parses a block: `[:x :y | x + y]`
    fn parse_block(&mut self) -> Expression {
        let start = self
            .expect(&TokenKind::LeftBracket, "Expected '['")
            .unwrap()
            .span();

        let mut parameters = Vec::new();

        // Parse block parameters: [:x :y |
        while self.match_token(&TokenKind::Colon) {
            if let TokenKind::Identifier(name) = self.current_kind() {
                let span = self.current_token().span();
                parameters.push(BlockParameter::new(name.clone(), span));
                self.advance();
            } else {
                self.error("Expected parameter name after ':'");
                break;
            }
        }

        // Expect pipe separator if there are parameters
        if !parameters.is_empty() {
            self.expect(&TokenKind::Pipe, "Expected '|' after block parameters");
        }

        // Parse block body
        let mut body = Vec::new();
        while !self.check(&TokenKind::RightBracket) && !self.is_at_end() {
            let expr = self.parse_expression();
            body.push(expr);

            // Optional statement separator
            if !self.match_token(&TokenKind::Period) {
                break;
            }
        }

        let end = self
            .expect(&TokenKind::RightBracket, "Expected ']' to close block")
            .map_or(start, |t| t.span());

        let span = start.merge(end);
        let block = Block::new(parameters, body, span);
        Expression::Block(block)
    }

    /// Parses a parenthesized expression.
    fn parse_parenthesized(&mut self) -> Expression {
        let start = self
            .expect(&TokenKind::LeftParen, "Expected '('")
            .unwrap()
            .span();

        let inner = self.parse_expression();

        let end = self
            .expect(&TokenKind::RightParen, "Expected ')' to close parentheses")
            .map_or(start, |t| t.span());

        let span = start.merge(end);

        Expression::Parenthesized {
            expression: Box::new(inner),
            span,
        }
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Parses an integer literal, handling different radix notations.
///
/// Examples:
/// - `42` → 42 (decimal)
/// - `16rFF` → 255 (hexadecimal)
/// - `2r1010` → 10 (binary)
fn parse_integer(s: &str) -> Result<i64, String> {
    // Check for radix notation: NrVALUE
    if let Some(r_pos) = s.find('r') {
        let radix_str = &s[..r_pos];
        let value_str = &s[r_pos + 1..];

        let radix = radix_str
            .parse::<u32>()
            .map_err(|_| format!("Invalid radix: {radix_str}"))?;

        if !(2..=36).contains(&radix) {
            return Err(format!("Radix must be between 2 and 36, got {radix}"));
        }

        i64::from_str_radix(value_str, radix)
            .map_err(|_| format!("Invalid integer value '{value_str}' for radix {radix}"))
    } else {
        // Standard decimal integer
        s.parse::<i64>()
            .map_err(|_| format!("Invalid integer literal: {s}"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::lex_with_eof;

    /// Helper to parse a string and check for errors.
    fn parse_ok(source: &str) -> Module {
        let tokens = lex_with_eof(source);
        let (module, diagnostics) = parse(tokens);
        assert!(
            diagnostics.is_empty(),
            "Expected no errors, got: {diagnostics:?}"
        );
        module
    }

    /// Helper to parse a string expecting errors.
    fn parse_err(source: &str) -> Vec<Diagnostic> {
        let tokens = lex_with_eof(source);
        let (_module, diagnostics) = parse(tokens);
        diagnostics
    }

    #[test]
    fn parse_integer_literal() {
        let module = parse_ok("42");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Literal(Literal::Integer(42), _) => {}
            _ => panic!("Expected integer literal"),
        }
    }

    #[test]
    fn parse_float_literal() {
        let module = parse_ok("2.5");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Literal(Literal::Float(f), _) if (*f - 2.5_f64).abs() < 0.001 => {}
            _ => panic!("Expected float literal"),
        }
    }

    #[test]
    fn parse_string_literal() {
        let module = parse_ok("'hello'");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Literal(Literal::String(s), _) if s == "hello" => {}
            _ => panic!("Expected string literal"),
        }
    }

    #[test]
    fn parse_symbol_literal() {
        let module = parse_ok("#symbol");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Literal(Literal::Symbol(s), _) if s == "symbol" => {}
            _ => panic!("Expected symbol literal"),
        }
    }

    #[test]
    fn parse_identifier() {
        let module = parse_ok("myVar");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Identifier(id) if id.name == "myVar" => {}
            _ => panic!("Expected identifier"),
        }
    }

    #[test]
    fn parse_assignment() {
        let module = parse_ok("x := 42");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Assignment { target, value, .. } => {
                assert!(matches!(**target, Expression::Identifier(_)));
                assert!(matches!(
                    **value,
                    Expression::Literal(Literal::Integer(42), _)
                ));
            }
            _ => panic!("Expected assignment"),
        }
    }

    #[test]
    fn parse_unary_message() {
        let module = parse_ok("3 factorial");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::MessageSend {
                receiver,
                selector: MessageSelector::Unary(name),
                arguments,
                ..
            } => {
                assert!(matches!(
                    **receiver,
                    Expression::Literal(Literal::Integer(3), _)
                ));
                assert_eq!(name.as_str(), "factorial");
                assert!(arguments.is_empty());
            }
            _ => panic!("Expected unary message send"),
        }
    }

    #[test]
    fn parse_binary_message() {
        let module = parse_ok("3 + 4");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::MessageSend {
                receiver,
                selector: MessageSelector::Binary(op),
                arguments,
                ..
            } => {
                assert!(matches!(
                    **receiver,
                    Expression::Literal(Literal::Integer(3), _)
                ));
                assert_eq!(op.as_str(), "+");
                assert_eq!(arguments.len(), 1);
                assert!(matches!(
                    arguments[0],
                    Expression::Literal(Literal::Integer(4), _)
                ));
            }
            _ => panic!("Expected binary message send"),
        }
    }

    #[test]
    fn parse_binary_message_with_precedence() {
        // Test: 2 + 3 * 4 should be 2 + (3 * 4) = 14
        let module = parse_ok("2 + 3 * 4");
        assert_eq!(module.expressions.len(), 1);

        // The AST should be: (2 + (3 * 4))
        match &module.expressions[0] {
            Expression::MessageSend {
                receiver,
                selector: MessageSelector::Binary(op),
                arguments,
                ..
            } => {
                assert!(matches!(
                    **receiver,
                    Expression::Literal(Literal::Integer(2), _)
                ));
                assert_eq!(op.as_str(), "+");
                assert_eq!(arguments.len(), 1);

                // The argument should be (3 * 4)
                match &arguments[0] {
                    Expression::MessageSend {
                        receiver: r2,
                        selector: MessageSelector::Binary(op2),
                        arguments: args2,
                        ..
                    } => {
                        assert!(matches!(**r2, Expression::Literal(Literal::Integer(3), _)));
                        assert_eq!(op2.as_str(), "*");
                        assert_eq!(args2.len(), 1);
                        assert!(matches!(
                            args2[0],
                            Expression::Literal(Literal::Integer(4), _)
                        ));
                    }
                    _ => panic!("Expected multiplication as right operand"),
                }
            }
            _ => panic!("Expected addition at top level"),
        }
    }

    #[test]
    fn parse_keyword_message() {
        let module = parse_ok("array at: 1 put: 'x'");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::MessageSend {
                receiver,
                selector: MessageSelector::Keyword(parts),
                arguments,
                ..
            } => {
                assert!(matches!(**receiver, Expression::Identifier(_)));
                assert_eq!(parts.len(), 2);
                assert_eq!(parts[0].keyword.as_str(), "at:");
                assert_eq!(parts[1].keyword.as_str(), "put:");
                assert_eq!(arguments.len(), 2);
            }
            _ => panic!("Expected keyword message send"),
        }
    }

    #[test]
    fn parse_block_no_params() {
        let module = parse_ok("[42]");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Block(block) => {
                assert!(block.parameters.is_empty());
                assert_eq!(block.body.len(), 1);
            }
            _ => panic!("Expected block"),
        }
    }

    #[test]
    fn parse_block_with_params() {
        let module = parse_ok("[:x :y | x + y]");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Block(block) => {
                assert_eq!(block.parameters.len(), 2);
                assert_eq!(block.parameters[0].name.as_str(), "x");
                assert_eq!(block.parameters[1].name.as_str(), "y");
                assert_eq!(block.body.len(), 1);
            }
            _ => panic!("Expected block"),
        }
    }

    #[test]
    fn parse_return_statement() {
        let module = parse_ok("^42");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Return { value, .. } => {
                assert!(matches!(
                    **value,
                    Expression::Literal(Literal::Integer(42), _)
                ));
            }
            _ => panic!("Expected return statement"),
        }
    }

    #[test]
    fn parse_parenthesized() {
        let module = parse_ok("(3 + 4) * 2");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::MessageSend {
                receiver,
                selector: MessageSelector::Binary(op),
                arguments,
                ..
            } => {
                assert!(matches!(**receiver, Expression::Parenthesized { .. }));
                assert_eq!(op.as_str(), "*");
                assert_eq!(arguments.len(), 1);
            }
            _ => panic!("Expected binary message with parenthesized receiver"),
        }
    }

    #[test]
    fn parse_field_access() {
        let module = parse_ok("self.value");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::FieldAccess {
                receiver, field, ..
            } => {
                assert!(matches!(**receiver, Expression::Identifier(_)));
                assert_eq!(field.name.as_str(), "value");
            }
            _ => panic!("Expected field access"),
        }
    }

    #[test]
    fn parse_compound_assignment() {
        let module = parse_ok("x += 1");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::CompoundAssignment {
                target,
                operator,
                value,
                ..
            } => {
                assert!(matches!(**target, Expression::Identifier(_)));
                assert_eq!(*operator, CompoundOperator::Add);
                assert!(matches!(
                    **value,
                    Expression::Literal(Literal::Integer(1), _)
                ));
            }
            _ => panic!("Expected compound assignment"),
        }
    }

    #[test]
    fn parse_radix_integer() {
        assert_eq!(parse_integer("16rFF").unwrap(), 255);
        assert_eq!(parse_integer("2r1010").unwrap(), 10);
        assert_eq!(parse_integer("8r17").unwrap(), 15);
        assert!(parse_integer("37r10").is_err()); // Invalid radix
        assert!(parse_integer("16rGG").is_err()); // Invalid digits for radix
    }

    #[test]
    fn parse_empty_input() {
        let module = parse_ok("");
        assert!(module.expressions.is_empty());
    }

    #[test]
    fn parse_error_recovery() {
        // Parse with an intentional error
        let diagnostics = parse_err("x := @");
        assert!(!diagnostics.is_empty());
        // Parser should recover and not panic
    }

    #[test]
    fn parse_cascade() {
        let module = parse_ok("Transcript show: 'Hello'; cr; show: 'World'");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Cascade {
                receiver, messages, ..
            } => {
                // Receiver is the first message: Transcript show: 'Hello'
                assert!(matches!(**receiver, Expression::MessageSend { .. }));
                assert_eq!(messages.len(), 2);
                // First message: cr (unary)
                assert!(matches!(
                    &messages[0].selector,
                    MessageSelector::Unary(name) if name == "cr"
                ));
                assert!(messages[0].arguments.is_empty());
                // Second message: show: 'World' (keyword)
                assert!(matches!(&messages[1].selector, MessageSelector::Keyword(_)));
                assert_eq!(messages[1].arguments.len(), 1);
            }
            _ => panic!("Expected cascade expression"),
        }
    }

    #[test]
    fn parse_cascade_simple() {
        let module = parse_ok("x foo; bar; baz");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Cascade {
                receiver, messages, ..
            } => {
                // Receiver is the first unary message: x foo
                assert!(matches!(**receiver, Expression::MessageSend { .. }));
                assert_eq!(messages.len(), 2);
                assert!(matches!(
                    &messages[0].selector,
                    MessageSelector::Unary(name) if name == "bar"
                ));
                assert!(matches!(
                    &messages[1].selector,
                    MessageSelector::Unary(name) if name == "baz"
                ));
            }
            _ => panic!("Expected cascade expression"),
        }
    }
}
