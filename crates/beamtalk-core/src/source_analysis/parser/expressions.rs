// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Expression parsing for Beamtalk.
//!
//! This module contains all expression parsing methods extracted from the main
//! `Parser` implementation. Expression parsing handles:
//!
//! - Return statements (`^`)
//! - Assignment (`:=`)
//! - Message sends (unary, binary, keyword)
//! - Cascade expressions (`;`)
//! - Literals (numbers, strings, symbols, characters)
//! - Blocks (`[...]`)
//! - Map literals (`#{...}`)
//! - Parenthesized expressions
//! - Field access (`object.field`)

use crate::ast::{
    Block, BlockParameter, CascadeMessage, ExpectCategory, Expression, ExpressionStatement,
    Identifier, KeywordPart, Literal, MapPair, MatchArm, MessageSelector, Pattern, StringSegment,
};
use crate::source_analysis::{Token, TokenKind};
use ecow::EcoString;

use super::{Diagnostic, Parser, binary_binding_power};

/// Helper function for parsing integers with radix notation.
pub(super) fn parse_integer(s: &str) -> Result<i64, String> {
    if let Some((radix_str, digits)) = s.split_once('r') {
        let radix = radix_str
            .parse::<u32>()
            .map_err(|_| format!("Invalid radix: {radix_str}"))?;

        if !(2..=36).contains(&radix) {
            return Err(format!("Radix must be between 2 and 36, got {radix}"));
        }

        i64::from_str_radix(digits, radix)
            .map_err(|_| format!("Invalid digit for radix {radix}: {digits}"))
    } else {
        s.parse::<i64>()
            .map_err(|_| format!("Invalid integer: {s}"))
    }
}

impl Parser {
    // ========================================================================
    // Expression Parsing
    // ========================================================================

    /// Parses any expression.
    ///
    /// Entry point for expression parsing. Handles all precedence levels.
    /// Uses `stacker::maybe_grow` to extend the stack on the heap if
    /// remaining stack space falls below 32 KiB (prevents stack overflow
    /// even under `AddressSanitizer` during fuzzing).
    pub(super) fn parse_expression(&mut self) -> Expression {
        // Grow the stack on the heap when remaining space is low.
        // 32 KiB red zone, 256 KiB new segment.  Kept small because the
        // nesting-depth guard (MAX_NESTING_DEPTH = 64) caps recursion,
        // so we never need many segments, and a large segment size would
        // cause OOM under AddressSanitizer during fuzzing.
        stacker::maybe_grow(32 * 1024, 256 * 1024, || {
            // Guard against stack overflow from deeply nested input
            if let Err(error) = self.enter_nesting(self.current_token().span()) {
                return error;
            }

            // Check for return statement first
            let result = if self.match_token(&TokenKind::Caret) {
                self.parse_return()
            } else {
                // Try to parse assignment or regular expression
                self.parse_assignment()
            };

            self.leave_nesting();
            result
        })
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
            // Validate assignment target
            if !matches!(
                expr,
                Expression::Identifier(_) | Expression::FieldAccess { .. }
            ) {
                let span = expr.span();
                self.diagnostics.push(Diagnostic::error(
                    "Assignment target must be an identifier or field access",
                    span,
                ));
                return Expression::Error {
                    message: "Invalid assignment target".into(),
                    span,
                };
            }

            // Guard recursive call against stack overflow
            if let Err(error) = self.enter_nesting(expr.span()) {
                return error;
            }
            let value = Box::new(self.parse_assignment());
            self.leave_nesting();
            let span = expr.span().merge(value.span());
            return Expression::Assignment {
                target: Box::new(expr),
                value,
                span,
            };
        }

        expr
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
    pub(super) fn parse_keyword_message(&mut self) -> Expression {
        let receiver = self.parse_binary_message();

        // Check if this is a keyword message
        // A newline before the keyword indicates a new statement, not a message
        if !matches!(self.current_kind(), TokenKind::Keyword(_))
            || self.current_token().has_leading_newline()
        {
            return receiver;
        }

        // Special handling for `match:` — produces Expression::Match
        if matches!(self.current_kind(), TokenKind::Keyword(k) if k.as_str() == "match:")
            && self.peek_at(1) == Some(&TokenKind::LeftBracket)
        {
            return self.parse_match_expression(receiver);
        }

        // Parse keyword message
        let mut keywords = Vec::new();
        let mut arguments = Vec::new();

        while let TokenKind::Keyword(keyword) = self.current_kind() {
            // Stop if the keyword is on a new line AND looks like a method
            // definition (keyword arg =>). Otherwise allow multi-line keyword
            // messages like `ifTrue: [...]\n  ifFalse: [...]` (BT-890).
            if self.current_token().has_leading_newline()
                && !keywords.is_empty()
                && self.is_at_method_definition()
            {
                break;
            }
            let span = self.current_token().span();
            keywords.push(KeywordPart::new(keyword.clone(), span));
            self.advance();

            // Parse argument (which is a binary message)
            arguments.push(self.parse_binary_message());
        }

        // Safety: arguments is guaranteed non-empty by the while loop above
        let span = receiver.span().merge(arguments.last().unwrap().span());

        Expression::MessageSend {
            receiver: Box::new(receiver),
            selector: MessageSelector::Keyword(keywords),
            arguments,
            is_cast: false,
            span,
        }
    }

    /// Parses a binary message with standard math precedence using Pratt parsing.
    ///
    /// Unlike traditional Smalltalk (which is strictly left-to-right),
    /// Beamtalk binary messages follow standard operator precedence:
    /// - Multiplicative: `* / %` (higher precedence)
    /// - Additive: `+ -` (lower precedence)
    /// - Comparison: `< > <= >= == /= =:= =/=` (lowest precedence)
    ///
    /// This implementation uses Pratt parsing (top-down operator precedence)
    /// which makes adding new operators a single-line change in the
    /// [`binary_binding_power`] function.
    pub(super) fn parse_binary_message(&mut self) -> Expression {
        self.parse_binary_with_pratt(0)
    }

    /// Pratt parsing for binary expressions.
    ///
    /// This function implements the core Pratt parsing algorithm using binding
    /// powers from [`binary_binding_power`]. The `min_bp` parameter controls the
    /// minimum binding power required to continue parsing, enabling correct
    /// precedence handling through recursion.
    ///
    /// # Arguments
    ///
    /// * `min_bp` - Minimum binding power to continue parsing (0 for top-level)
    fn parse_binary_with_pratt(&mut self, min_bp: u8) -> Expression {
        // Parse the left-hand side (unary expression)
        let mut left = self.parse_unary_message();

        while let TokenKind::BinarySelector(op) = self.current_kind() {
            let op = op.clone();

            // BT-285: A binary selector on a new line that looks like a method definition
            // (e.g., `- other =>`) should not be consumed as a binary operator.
            // This mirrors the newline check in parse_unary_message.
            if self.current_token().has_leading_newline() && self.is_at_method_definition() {
                break;
            }

            // Get binding power; unknown operators end the expression
            let Some(bp) = binary_binding_power(&op) else {
                break;
            };

            // Stop if this operator binds less tightly than our minimum
            if bp.left < min_bp {
                break;
            }

            // Consume the operator
            self.advance();

            // Parse the right-hand side with the operator's right binding power
            let right = self.parse_binary_with_pratt(bp.right);

            // Build the message send expression
            let span = left.span().merge(right.span());
            left = Expression::MessageSend {
                receiver: Box::new(left),
                selector: MessageSelector::Binary(op),
                arguments: vec![right],
                is_cast: false,
                span,
            };
        }

        left
    }

    /// Parses unary messages (highest message precedence).
    pub(super) fn parse_unary_message(&mut self) -> Expression {
        let mut receiver = self.parse_primary();

        // Parse chain of unary messages
        // Stop if the identifier is on a new line (start of new statement)
        while let TokenKind::Identifier(name) = self.current_kind() {
            // A newline before the identifier indicates a new statement, not a message
            if self.current_token().has_leading_newline() {
                break;
            }

            let selector = MessageSelector::Unary(name.clone());
            let tok = self.advance();
            let span = receiver.span().merge(tok.span());

            receiver = Expression::MessageSend {
                receiver: Box::new(receiver),
                selector,
                arguments: Vec::new(),
                is_cast: false,
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

            // String interpolation: "text {expr} more text"
            TokenKind::StringStart(_) => self.parse_string_interpolation(),

            // Standalone '#' is not a valid primary expression.
            // (#( is already lexed as ListOpen, #{ as MapOpen, #[ as ArrayOpen, #name as Symbol)
            TokenKind::Hash => {
                let bad_token = self.advance();
                let span = bad_token.span();
                let message: EcoString =
                    "Unexpected '#': expected '#(' for a list, '#[' for an array, '#{' for a map, or '#name' / #'quoted' for a symbol"
                        .into();
                self.diagnostics
                    .push(Diagnostic::error(message.clone(), span));
                Expression::Error { message, span }
            }

            // Identifier or field access
            TokenKind::Identifier(_) => self.parse_identifier_or_field_access(),

            // Block
            TokenKind::LeftBracket => self.parse_block(),

            // Parenthesized expression
            TokenKind::LeftParen => self.parse_parenthesized(),

            // Map literal
            TokenKind::MapOpen => self.parse_map_literal(),

            // List literal
            TokenKind::ListOpen => self.parse_list_literal(),

            // Array literal
            TokenKind::ArrayOpen => self.parse_array_literal(),

            // Primitive pragma: @primitive/@intrinsic 'name' or @primitive/@intrinsic intrinsicName
            TokenKind::AtPrimitive | TokenKind::AtIntrinsic => self.parse_primitive(),

            // Diagnostic suppression directive: @expect category
            TokenKind::AtExpect => self.parse_expect_directive(),

            // Unexpected token - consume it to avoid getting stuck
            _ => {
                let bad_token = self.advance();
                let span = bad_token.span();
                let message: EcoString = format!(
                    "Unexpected token: expected expression, found {}",
                    bad_token.kind()
                )
                .into();
                self.diagnostics
                    .push(Diagnostic::error(message.clone(), span));
                Expression::Error { message, span }
            }
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

    /// Parses a string interpolation expression.
    ///
    /// Consumes `StringStart`, embedded expressions, `StringSegment`s,
    /// and `StringEnd` to produce an `Expression::StringInterpolation`.
    ///
    /// Token sequence example for `"Hello, {name}!"`:
    /// - `StringStart("Hello, ")` → expression tokens → `StringEnd("!")`
    fn parse_string_interpolation(&mut self) -> Expression {
        // Guard against stack overflow from deeply nested interpolations
        if let Err(error) = self.enter_nesting(self.current_token().span()) {
            return error;
        }

        let start_token = self.advance();
        let start_span = start_token.span();
        let TokenKind::StringStart(first_text) = start_token.into_kind() else {
            unreachable!()
        };

        let mut segments = Vec::new();

        // Add the initial literal segment (may be empty)
        if !first_text.is_empty() {
            segments.push(StringSegment::Literal(first_text));
        }

        loop {
            // Parse the interpolated expression (keyword message level)
            if !matches!(
                self.current_kind(),
                TokenKind::StringSegment(_) | TokenKind::StringEnd(_) | TokenKind::Eof
            ) {
                let expr = self.parse_keyword_message();
                segments.push(StringSegment::Interpolation(expr));
            }

            match self.current_kind().clone() {
                TokenKind::StringSegment(text) => {
                    self.advance();
                    if !text.is_empty() {
                        segments.push(StringSegment::Literal(text));
                    }
                }
                TokenKind::StringEnd(text) => {
                    let end_token = self.advance();
                    if !text.is_empty() {
                        segments.push(StringSegment::Literal(text));
                    }
                    let span = start_span.merge(end_token.span());
                    self.leave_nesting();
                    return Expression::StringInterpolation { segments, span };
                }
                _ => {
                    // Unexpected EOF or token — unterminated interpolation
                    self.diagnostics.push(Diagnostic::error(
                        "Unterminated string interpolation",
                        start_span,
                    ));
                    self.leave_nesting();
                    return Expression::Error {
                        message: "Unterminated string interpolation".into(),
                        span: start_span,
                    };
                }
            }
        }
    }

    /// Parses an identifier or field access.
    fn parse_identifier_or_field_access(&mut self) -> Expression {
        let name_token = self.advance();
        let TokenKind::Identifier(name) = name_token.kind() else {
            unreachable!()
        };
        let span = name_token.span();

        // Check for special keyword 'super'
        let mut expr = if name.as_str() == "super" {
            Expression::Super(span)
        } else {
            // Distinguish class references (uppercase) from regular identifiers.
            // Uses Unicode-aware `char::is_uppercase()` to support international class names
            // like `Über`, `Ñandú`, etc., following Rust's Unicode support philosophy.
            // Class names: uppercase first letter (Counter, MyClass, Über)
            // Variables: lowercase first letter (counter, myVar, über)
            let first_char = name.chars().next();
            if first_char.is_some_and(char::is_uppercase) {
                // Class reference (e.g., Counter, Array, MyClass)
                Expression::ClassReference {
                    name: Identifier::new(name.clone(), span),
                    span,
                }
            } else {
                // Regular identifier (variable)
                Expression::Identifier(Identifier::new(name.clone(), span))
            }
        };

        // Check for field access: identifier.field
        // Only treat a '.' as starting a field access if:
        // 1. The current token is a Period
        // 2. The next token is an Identifier
        // 3. There is no whitespace between the period and the identifier
        //
        // This distinguishes `self.value` (field access) from `n. self` (statement
        // separator followed by new expression).
        //
        // Whitespace detection: The lexer puts trailing same-line whitespace (spaces/tabs)
        // as trailing trivia on the period, and newlines + subsequent whitespace as leading
        // trivia on the next token. We check BOTH to catch all cases:
        // - `n. self` → period has trailing space
        // - `n.\n    self` → identifier has leading newline+spaces
        while self.check(&TokenKind::Period)
            && matches!(self.peek_kind(), Some(TokenKind::Identifier(_)))
            && self.current_token().trailing_trivia().is_empty()
            && self
                .peek()
                .is_some_and(|t: &Token| t.leading_trivia().is_empty())
        {
            self.advance(); // consume the period

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

        // Parse block body — statements separated by periods or newlines (BT-360)
        let mut body = Vec::new();
        while !self.check(&TokenKind::RightBracket) && !self.is_at_end() {
            let pos_before = self.current;
            // BT-987: detect blank lines (2+ newlines) before this statement
            let has_blank_line =
                !body.is_empty() && self.current_token().has_preceding_blank_line();
            let mut comments = self.collect_comment_attachment();
            let expr = self.parse_expression();
            // Only collect trailing comment if parse_expression consumed tokens;
            // otherwise collect_trailing_comment() reads the previous token's
            // trivia, which belongs to the prior statement.
            if self.current > pos_before {
                comments.trailing = self.collect_trailing_comment();
            }
            body.push(ExpressionStatement {
                comments,
                expression: expr,
                preceding_blank_line: has_blank_line,
            });

            // If parse_expression didn't consume any tokens (e.g. nesting
            // depth exceeded), break to avoid an infinite loop.
            if self.current == pos_before {
                break;
            }

            // Period, bang (!), or newline separates statements
            if self.match_token(&TokenKind::Period) {
                // Explicit period — check whether it is redundant (BT-948)
                let period_span = self.tokens[self.current - 1].span();
                if self.check(&TokenKind::RightBracket) {
                    // Trailing period before ']' — not needed
                    let mut diag =
                        Diagnostic::lint("unnecessary trailing `.` before `]`", period_span);
                    diag.hint = Some("Remove the trailing `.`".into());
                    self.diagnostics.push(diag);
                } else if !self.is_at_end() && self.current_token().has_leading_newline() {
                    // Period immediately before a newline — newline already separates statements
                    let mut diag = Diagnostic::lint(
                        "unnecessary `.` — the following newline already separates statements",
                        period_span,
                    );
                    diag.hint = Some("Remove the `.` and rely on the newline".into());
                    self.diagnostics.push(diag);
                }
            } else if self.match_token(&TokenKind::Bang) {
                // Cast terminator — mark the last expression as a cast if it's a MessageSend.
                // If the expression is not a MessageSend (e.g. `x := foo bar!`), emit an error.
                match body.last_mut().map(|s| &mut s.expression) {
                    Some(Expression::MessageSend { is_cast, .. }) => *is_cast = true,
                    Some(last) => {
                        let span = last.span();
                        self.diagnostics.push(Diagnostic::error(
                            "Cast (!) has no return value and cannot be used in an expression. Use . for a synchronous call.",
                            span,
                        ));
                    }
                    None => {}
                }
                // Continue (same as period)
            } else if !self.is_at_end()
                && !self.check(&TokenKind::RightBracket)
                && self.current_token().has_leading_newline()
            {
                // Newline acts as implicit separator (BT-360)
            } else {
                break;
            }
        }

        let end = self
            .expect(&TokenKind::RightBracket, "Expected ']' to close block")
            .map_or(start, |t: Token| t.span());

        let span = start.merge(end);
        let block = Block::new(parameters, body, span);
        Expression::Block(block)
    }

    /// Parses a match expression: `receiver match: [pattern -> body ...]`
    ///
    /// The receiver has already been parsed and `match:` keyword is the current token.
    fn parse_match_expression(&mut self, receiver: Expression) -> Expression {
        self.advance(); // consume `match:` keyword

        let bracket_start = self
            .expect(&TokenKind::LeftBracket, "Expected '[' after match:")
            .map_or_else(|| self.current_token().span(), |t: Token| t.span());

        let mut arms = Vec::new();

        while !self.check(&TokenKind::RightBracket) && !self.is_at_end() {
            arms.push(self.parse_match_arm());

            // Semicolon or newline separates arms
            if self.match_token(&TokenKind::Semicolon) {
                // Explicit semicolon — continue
            } else if !self.is_at_end()
                && !self.check(&TokenKind::RightBracket)
                && self.current_token().has_leading_newline()
            {
                // Newline acts as implicit separator
            } else {
                break;
            }
        }

        let end = self
            .expect(&TokenKind::RightBracket, "Expected ']' to close match")
            .map_or(bracket_start, |t: Token| t.span());

        let span = receiver.span().merge(end);

        Expression::Match {
            value: Box::new(receiver),
            arms,
            span,
        }
    }

    /// Parses a single match arm: `pattern -> body` or `pattern when: [guard] -> body`
    fn parse_match_arm(&mut self) -> MatchArm {
        let comments = self.collect_comment_attachment();
        let pattern = self.parse_pattern();
        let pat_span = pattern.span();

        // Check for optional guard: `when: [guard_expr]`
        let guard = if matches!(self.current_kind(), TokenKind::Keyword(k) if k.as_str() == "when:")
        {
            self.advance(); // consume `when:`
            // Guard must be in a block [expr]
            if self.check(&TokenKind::LeftBracket) {
                self.advance(); // consume [
                let guard_expr = self.parse_expression();
                self.expect(
                    &TokenKind::RightBracket,
                    "Expected ']' to close guard expression",
                );
                Some(guard_expr)
            } else {
                self.error("Expected '[' after when:");
                None
            }
        } else {
            None
        };

        // Expect -> separator
        if !self.match_binary_selector("->") {
            self.error("Expected '->' after pattern in match arm");
        }

        // Parse body expression — use keyword message level to avoid consuming
        // semicolons (arm separators) or assignment operators
        let body = self.parse_keyword_message();
        let span = pat_span.merge(body.span());

        let mut arm = if let Some(guard_expr) = guard {
            MatchArm::with_guard(pattern, guard_expr, body, span)
        } else {
            MatchArm::new(pattern, body, span)
        };
        arm.comments = comments;
        arm
    }

    /// Parses a pattern for match arms.
    ///
    /// Supported patterns:
    /// - `_` — wildcard
    /// - identifier — variable binding
    /// - integer, float, string, symbol, character — literal
    /// - `{p1, p2, ...}` — tuple
    fn parse_pattern(&mut self) -> Pattern {
        // Guard against stack overflow from deeply nested tuple patterns
        let span = self.current_token().span();
        if self.enter_nesting(span).is_err() {
            return Pattern::Wildcard(span);
        }

        let result = match self.current_kind() {
            // Wildcard: `_`
            TokenKind::Identifier(name) if name.as_str() == "_" => {
                let span = self.advance().span();
                Pattern::Wildcard(span)
            }

            // Variable binding
            TokenKind::Identifier(name) => {
                let name = name.clone();
                let token = self.advance();
                let span = token.span();
                Pattern::Variable(Identifier::new(name, span))
            }

            // Literal patterns: integer, float, string, character
            TokenKind::Integer(_)
            | TokenKind::Float(_)
            | TokenKind::String(_)
            | TokenKind::Character(_) => {
                let expr = self.parse_literal();
                let span = expr.span();
                if let Expression::Literal(lit, _) = expr {
                    Pattern::Literal(lit, span)
                } else {
                    // parse_literal returned an error
                    Pattern::Wildcard(span)
                }
            }

            // Symbol patterns: #ok, #error, etc.
            TokenKind::Symbol(_) => {
                let token = self.advance();
                let span = token.span();
                if let TokenKind::Symbol(s) = token.into_kind() {
                    Pattern::Literal(Literal::Symbol(s), span)
                } else {
                    unreachable!()
                }
            }

            // Negative number patterns: -1, -3.14
            TokenKind::BinarySelector(op) if op.as_str() == "-" => {
                let start = self.advance().span();
                match self.current_kind() {
                    TokenKind::Integer(_) | TokenKind::Float(_) => {
                        let expr = self.parse_literal();
                        let span = start.merge(expr.span());
                        if let Expression::Literal(lit, _) = expr {
                            let neg_lit = match lit {
                                Literal::Integer(n) => Literal::Integer(-n),
                                Literal::Float(f) => Literal::Float(-f),
                                other => other,
                            };
                            Pattern::Literal(neg_lit, span)
                        } else {
                            Pattern::Wildcard(span)
                        }
                    }
                    _ => {
                        self.error("Expected number after '-' in pattern");
                        Pattern::Wildcard(start)
                    }
                }
            }

            // Tuple pattern: {p1, p2, ...}
            TokenKind::LeftBrace => self.parse_tuple_pattern(),

            _ => {
                let bad_token = self.advance();
                let span = bad_token.span();
                self.diagnostics.push(Diagnostic::error(
                    format!(
                        "Unexpected token in pattern: expected identifier, literal, '_', or '{{', found {}",
                        bad_token.kind()
                    ),
                    span,
                ));
                Pattern::Wildcard(span)
            }
        };

        self.leave_nesting();
        result
    }

    /// Parses a tuple pattern: `{p1, p2, ...}`
    fn parse_tuple_pattern(&mut self) -> Pattern {
        let start = self
            .expect(&TokenKind::LeftBrace, "Expected '{'")
            .unwrap()
            .span();

        let mut elements = Vec::new();

        if !self.check(&TokenKind::RightBrace) {
            elements.push(self.parse_pattern());
            while self.match_binary_selector(",") {
                elements.push(self.parse_pattern());
            }
        }

        let end = self
            .expect(&TokenKind::RightBrace, "Expected '}'")
            .map_or(start, |t: Token| t.span());

        let span = start.merge(end);
        Pattern::Tuple { elements, span }
    }

    /// Parses a `@primitive` or `@intrinsic` pragma: `@primitive "selector"` or `@intrinsic intrinsicName`.
    ///
    /// The `@primitive`/`@intrinsic` token has already been identified by `parse_primary`.
    /// This method consumes it and parses the primitive name that follows.
    fn parse_primitive(&mut self) -> Expression {
        let start_token = self.advance(); // consume AtPrimitive or AtIntrinsic
        let is_intrinsic = matches!(start_token.kind(), TokenKind::AtIntrinsic);
        let directive = start_token.kind().to_string();
        let start = start_token.span();

        // @primitive/@intrinsic is only valid inside method bodies (after =>)
        if !self.in_method_body {
            let message: EcoString =
                format!("{directive} can only appear inside a method body (after =>)").into();
            self.diagnostics
                .push(Diagnostic::error(message.clone(), start));
            // Still parse the name for error recovery
            if matches!(
                self.current_kind(),
                TokenKind::String(_) | TokenKind::Identifier(_)
            ) {
                self.advance();
            }
            return Expression::Error {
                message,
                span: start,
            };
        }

        match self.current_kind() {
            // Quoted selector: @primitive "+"
            TokenKind::String(name) => {
                let name = name.clone();
                let end_token = self.advance();
                let span = start.merge(end_token.span());
                Expression::Primitive {
                    name,
                    is_quoted: true,
                    is_intrinsic,
                    span,
                }
            }
            // Bare identifier: @primitive basicNew
            TokenKind::Identifier(name) => {
                let name = name.clone();
                let end_token = self.advance();
                let span = start.merge(end_token.span());
                Expression::Primitive {
                    name,
                    is_quoted: false,
                    is_intrinsic,
                    span,
                }
            }
            _ => {
                let span = start;
                let message: EcoString =
                    format!("{directive} must be followed by a quoted selector or identifier")
                        .into();
                self.diagnostics
                    .push(Diagnostic::error(message.clone(), span));
                Expression::Error { message, span }
            }
        }
    }

    /// Parses an `@expect category` directive.
    ///
    /// The `@expect` token has already been identified by `parse_primary`.
    /// This method consumes it and parses the category name that follows.
    fn parse_expect_directive(&mut self) -> Expression {
        let start_token = self.advance(); // consume AtExpect
        let start = start_token.span();

        if let TokenKind::Identifier(name) = self.current_kind() {
            let name = name.clone();
            let end_token = self.advance();
            let span = start.merge(end_token.span());
            if let Some(category) = ExpectCategory::from_name(&name) {
                Expression::ExpectDirective { category, span }
            } else {
                let valid = "dnu, type, unused, all";
                let message: EcoString =
                    format!("unknown @expect category '{name}', valid categories are: {valid}")
                        .into();
                self.diagnostics
                    .push(Diagnostic::error(message.clone(), span));
                Expression::Error { message, span }
            }
        } else {
            let span = start;
            let message: EcoString =
                "@expect must be followed by a category name (dnu, type, unused, all)".into();
            self.diagnostics
                .push(Diagnostic::error(message.clone(), span));
            Expression::Error { message, span }
        }
    }

    /// Parses a map literal: `#{key => value, ...}`
    ///
    /// Map keys and values are parsed as unary expressions (primaries + unary messages),
    /// which stops at binary operators like `=>` and `,`. This allows nested maps and
    /// simple expressions as keys/values while avoiding ambiguity with the map syntax.
    fn parse_map_literal(&mut self) -> Expression {
        let start_token = self.expect(&TokenKind::MapOpen, "Expected '#{'");
        let start = start_token.map_or_else(|| self.current_token().span(), |t: Token| t.span());

        let mut pairs = Vec::new();

        // Handle empty map: #{}
        if self.check(&TokenKind::RightBrace) {
            let end_token = self.advance();
            let span = start.merge(end_token.span());
            return Expression::MapLiteral { pairs, span };
        }

        // Parse key-value pairs
        loop {
            // Guard: stop if we've reached EOF (prevents infinite loop on unclosed maps)
            if self.is_at_end() {
                break;
            }

            let pair_start = self.current_token().span();

            // BT-591: Bare identifier keys in map literals are implicit symbols.
            // If the current token is a lowercase identifier followed by `=>`,
            // treat it as a symbol literal (e.g., `#{x => 3}` means `#{#x => 3}`).
            let key = if matches!(self.current_kind(), TokenKind::Identifier(name) if name.chars().next().is_some_and(char::is_lowercase))
                && matches!(self.peek_kind(), Some(TokenKind::FatArrow))
            {
                let token = self.advance();
                let span = token.span();
                let TokenKind::Identifier(name) = token.kind() else {
                    unreachable!()
                };
                Expression::Literal(Literal::Symbol(name.clone()), span)
            } else {
                // Parse key expression (unary only - stops at `=>`, `,`, `}`)
                self.parse_unary_message()
            };

            // Expect '=>' separator between key and value
            if !matches!(self.current_kind(), TokenKind::FatArrow) {
                let bad_token = self.advance();
                let span = bad_token.span();
                self.diagnostics
                    .push(Diagnostic::error("Expected '=>' after map key", span));
                // Try to recover: skip to next comma or closing brace, handling nested braces
                let mut brace_depth = 0;
                while !matches!(self.current_kind(), TokenKind::Eof) {
                    match self.current_kind() {
                        TokenKind::MapOpen | TokenKind::LeftBrace => {
                            brace_depth += 1;
                            self.advance();
                        }
                        TokenKind::RightBrace => {
                            if brace_depth == 0 {
                                break;
                            }
                            brace_depth -= 1;
                            self.advance();
                        }
                        TokenKind::BinarySelector(s) if s.as_str() == "," && brace_depth == 0 => {
                            self.advance();
                            break;
                        }
                        _ => {
                            self.advance();
                        }
                    }
                }
                continue;
            }
            self.advance(); // consume '=>'

            // Parse value expression (binary to support operators like `+` in values)
            let value = self.parse_binary_message();

            let pair_span = pair_start.merge(value.span());
            pairs.push(MapPair::new(key, value, pair_span));

            // Check for comma (continue) or closing brace (end)
            if matches!(self.current_kind(), TokenKind::BinarySelector(s) if s.as_str() == ",") {
                self.advance(); // consume comma
                // Allow trailing comma
                if self.check(&TokenKind::RightBrace) {
                    break;
                }
            } else if self.check(&TokenKind::RightBrace) {
                break;
            } else {
                let bad_token = self.advance();
                self.diagnostics.push(Diagnostic::error(
                    format!(
                        "Expected ',' or '}}' in map literal, found {}",
                        bad_token.kind()
                    ),
                    bad_token.span(),
                ));
                break;
            }
        }

        // Expect closing brace
        let end_span = if self.check(&TokenKind::RightBrace) {
            self.advance().span()
        } else {
            self.diagnostics.push(Diagnostic::error(
                "Expected '}' to close map literal",
                self.current_token().span(),
            ));
            self.current_token().span()
        };

        let span = start.merge(end_span);
        Expression::MapLiteral { pairs, span }
    }

    /// Parses a list literal: `#(expr, expr, ...)` or `#(head | tail)` (cons)
    ///
    /// List elements are parsed as binary expressions to support operators like `+`
    /// and `->`. The `,` and `|` delimiters terminate parsing naturally since they
    /// are not valid binary selectors in element context.
    fn parse_list_literal(&mut self) -> Expression {
        let start_token = self.expect(&TokenKind::ListOpen, "Expected '#('");
        let start = start_token.map_or_else(|| self.current_token().span(), |t: Token| t.span());

        let mut elements = Vec::new();

        // Handle empty list: #()
        if self.check(&TokenKind::RightParen) {
            let end_token = self.advance();
            let span = start.merge(end_token.span());
            return Expression::ListLiteral {
                elements,
                tail: None,
                span,
            };
        }

        // Parse elements
        loop {
            // Parse element expression (binary to support operators like `->` in elements)
            let elem = self.parse_binary_message();
            elements.push(elem);

            // Check for cons operator `|`
            if self.check(&TokenKind::Pipe) {
                self.advance(); // consume `|`
                let tail = self.parse_unary_message();
                // Expect closing paren after tail
                let end_span = if self.check(&TokenKind::RightParen) {
                    self.advance().span()
                } else {
                    self.diagnostics.push(Diagnostic::error(
                        "Expected ')' to close list literal after cons tail",
                        self.current_token().span(),
                    ));
                    self.current_token().span()
                };
                let span = start.merge(end_span);
                return Expression::ListLiteral {
                    elements,
                    tail: Some(Box::new(tail)),
                    span,
                };
            }

            // Check for comma (continue) or closing paren (end)
            if matches!(self.current_kind(), TokenKind::BinarySelector(s) if s.as_str() == ",") {
                self.advance(); // consume comma
                // Allow trailing comma
                if self.check(&TokenKind::RightParen) {
                    break;
                }
            } else if self.check(&TokenKind::RightParen) {
                break;
            } else {
                let bad_token = self.advance();
                self.diagnostics.push(Diagnostic::error(
                    format!(
                        "Expected ',', '|', or ')' in list literal, found {}",
                        bad_token.kind()
                    ),
                    bad_token.span(),
                ));
                break;
            }
        }

        // Expect closing paren
        let end_span = if self.check(&TokenKind::RightParen) {
            self.advance().span()
        } else {
            self.diagnostics.push(Diagnostic::error(
                "Expected ')' to close list literal",
                self.current_token().span(),
            ));
            self.current_token().span()
        };

        let span = start.merge(end_span);
        Expression::ListLiteral {
            elements,
            tail: None,
            span,
        }
    }

    /// Parses an array literal: `#[expr, expr, ...]`
    ///
    /// Array elements are parsed as binary expressions. Closed by `]`.
    fn parse_array_literal(&mut self) -> Expression {
        let start_token = self.expect(&TokenKind::ArrayOpen, "Expected '#['");
        let start = start_token.map_or_else(|| self.current_token().span(), |t: Token| t.span());

        let mut elements = Vec::new();

        // Handle empty array: #[]
        if self.check(&TokenKind::RightBracket) {
            let end_token = self.advance();
            let span = start.merge(end_token.span());
            return Expression::ArrayLiteral { elements, span };
        }

        // Parse elements
        loop {
            let elem = self.parse_binary_message();
            elements.push(elem);

            // Check for comma (continue) or closing bracket (end)
            if matches!(self.current_kind(), TokenKind::BinarySelector(s) if s.as_str() == ",") {
                self.advance(); // consume comma
                // Allow trailing comma
                if self.check(&TokenKind::RightBracket) {
                    break;
                }
            } else if self.check(&TokenKind::RightBracket) {
                break;
            } else {
                let bad_token = self.advance();
                self.diagnostics.push(Diagnostic::error(
                    format!(
                        "Expected ',' or ']' in array literal, found {}",
                        bad_token.kind()
                    ),
                    bad_token.span(),
                ));
                break;
            }
        }

        // Expect closing bracket
        let end_span = if self.check(&TokenKind::RightBracket) {
            self.advance().span()
        } else {
            self.diagnostics.push(Diagnostic::error(
                "Expected ']' to close array literal",
                self.current_token().span(),
            ));
            self.current_token().span()
        };

        let span = start.merge(end_span);
        Expression::ArrayLiteral { elements, span }
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
            .map_or(start, |t: Token| t.span());

        let span = start.merge(end);

        Expression::Parenthesized {
            expression: Box::new(inner),
            span,
        }
    }
}

#[cfg(test)]
mod tests {
    //! Unit tests for expression parsing utilities.
    //!
    //! Primary focus: `parse_integer()` function which handles:
    //! - Standard decimal integers
    //! - Radix notation (e.g., `2r1010` for binary, `16rFF` for hex)
    //! - Validation of radix values (2-36)
    //! - Detection of invalid digits for a given radix
    //! - Overflow detection for values exceeding i64 range

    use super::*;

    // ========================================================================
    // parse_integer Tests
    // ========================================================================

    #[test]
    fn parse_integer_decimal() {
        assert_eq!(parse_integer("123").unwrap(), 123);
        assert_eq!(parse_integer("0").unwrap(), 0);
        assert_eq!(parse_integer("-42").unwrap(), -42);
    }

    #[test]
    fn parse_integer_binary() {
        assert_eq!(parse_integer("2r1010").unwrap(), 10);
        assert_eq!(parse_integer("2r0").unwrap(), 0);
        assert_eq!(parse_integer("2r1111").unwrap(), 15);
    }

    #[test]
    fn parse_integer_octal() {
        assert_eq!(parse_integer("8r777").unwrap(), 511);
        assert_eq!(parse_integer("8r100").unwrap(), 64);
    }

    #[test]
    fn parse_integer_hexadecimal() {
        assert_eq!(parse_integer("16rFF").unwrap(), 255);
        assert_eq!(parse_integer("16r10").unwrap(), 16);
        assert_eq!(parse_integer("16rABCD").unwrap(), 43981);
    }

    #[test]
    fn parse_integer_base_36() {
        assert_eq!(parse_integer("36rZ").unwrap(), 35);
        assert_eq!(parse_integer("36r10").unwrap(), 36);
    }

    #[test]
    fn parse_integer_invalid_radix() {
        assert!(parse_integer("1r0").is_err()); // radix too small
        assert!(parse_integer("37r0").is_err()); // radix too large
        assert!(parse_integer("0r0").is_err()); // radix < 2
    }

    #[test]
    fn parse_integer_invalid_digit_for_radix() {
        assert!(parse_integer("2r2").is_err()); // 2 is not a valid binary digit
        assert!(parse_integer("8r9").is_err()); // 9 is not a valid octal digit
        assert!(parse_integer("10rA").is_err()); // A is not a valid decimal digit
    }

    #[test]
    fn parse_integer_invalid_format() {
        assert!(parse_integer("not_a_number").is_err());
        assert!(parse_integer("12.34").is_err());
        assert!(parse_integer("").is_err());
    }

    #[test]
    fn parse_integer_overflow() {
        // i64::MAX is 9223372036854775807
        // Test values that exceed i64::MAX
        assert!(parse_integer("9223372036854775808").is_err()); // i64::MAX + 1
        assert!(parse_integer("99999999999999999999").is_err()); // Way over i64::MAX

        // Test hex overflow: i64::MAX is 0x7FFFFFFFFFFFFFFF
        assert!(parse_integer("16r8000000000000000").is_err()); // Just over i64::MAX
        assert!(parse_integer("16rFFFFFFFFFFFFFFFF").is_err()); // u64::MAX (overflows i64)

        // Test negative overflow: i64::MIN is -9223372036854775808
        assert!(parse_integer("-9223372036854775809").is_err()); // i64::MIN - 1
    }

    #[test]
    fn parse_integer_edge_cases() {
        // Valid radix specifier with no digits
        assert!(parse_integer("10r").is_err());

        // Malformed radix
        assert!(parse_integer("rABC").is_err());
    }

    #[test]
    fn parse_integer_max_valid_values() {
        // Test boundary values that should succeed
        assert_eq!(parse_integer("9223372036854775807").unwrap(), i64::MAX);
        assert_eq!(parse_integer("-9223372036854775808").unwrap(), i64::MIN);

        // Test i64::MAX in hex
        assert_eq!(parse_integer("16r7FFFFFFFFFFFFFFF").unwrap(), i64::MAX);
    }
}
