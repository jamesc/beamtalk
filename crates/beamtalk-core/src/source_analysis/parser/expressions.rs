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
    BinaryEndianness, BinarySegment, BinarySegmentType, BinarySignedness, Block, BlockParameter,
    CascadeMessage, ExpectCategory, Expression, ExpressionStatement, Identifier, KeywordPart,
    Literal, MapPair, MapPatternKey, MapPatternPair, MatchArm, MessageSelector, Pattern,
    StringSegment,
};
use crate::source_analysis::{Span, Token, TokenKind};
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
        // Tuple destructuring: `{a, b} := expr`
        // Tuples are not valid expressions, so we intercept `{` here before
        // parse_cascade() would produce an error.
        if self.check(&TokenKind::LeftBrace) {
            return self.parse_tuple_destructure_or_error();
        }

        let expr = self.parse_cascade();

        // Check for assignment operator
        if self.match_token(&TokenKind::Assign) {
            return self.parse_assign_rhs(expr);
        }

        expr
    }

    /// Parses a tuple destructuring assignment (`{a, b} := expr`) or reports an
    /// error if the `{...}` is not followed by `:=`.
    fn parse_tuple_destructure_or_error(&mut self) -> Expression {
        let start = self.current_token().span();
        let pattern = self.parse_tuple_pattern();
        if self.match_token(&TokenKind::Assign) {
            if let Err(error) = self.enter_nesting(start) {
                return error;
            }
            let value = Box::new(self.parse_assignment());
            self.leave_nesting();
            let span = start.merge(value.span());
            return Expression::DestructureAssignment {
                pattern,
                value,
                span,
            };
        }
        // `{...}` not followed by `:=` — tuple literals are not expressions
        let span = pattern.span();
        self.diagnostics.push(Diagnostic::error(
            "Tuple literals are not valid expressions; use `{a, b} := expr` for destructuring",
            span,
        ));
        Expression::Error {
            message: "Tuple literal used as expression".into(),
            span,
        }
    }

    /// After `:=` has been consumed, parse the right-hand side of an assignment.
    ///
    /// Dispatches to collection destructuring, map destructuring, or simple
    /// assignment depending on the LHS expression form.
    fn parse_assign_rhs(&mut self, expr: Expression) -> Expression {
        // Reject cons-style list syntax on the LHS: `#(a | rest) := expr`.
        if let Expression::ListLiteral {
            tail: Some(_),
            span: lhs_span,
            ..
        } = &expr
        {
            let span = *lhs_span;
            self.diagnostics.push(Diagnostic::error(
                "Cons syntax `#(head | tail)` is not supported in destructuring patterns; use `#(a, b, c) := expr` instead",
                span,
            ));
            return Expression::Error {
                message: "Invalid destructuring pattern".into(),
                span,
            };
        }

        // Array destructuring: `#[a, b] := expr`
        // List destructuring: `#(a, b) := expr` (BT-1279)
        let list_syntax = matches!(&expr, Expression::ListLiteral { tail: None, .. });
        if let Expression::ArrayLiteral {
            elements,
            span: lhs_span,
        }
        | Expression::ListLiteral {
            elements,
            tail: None,
            span: lhs_span,
        } = &expr
        {
            return self.parse_collection_destructure(elements, *lhs_span, list_syntax);
        }

        // Map destructuring: `#{#key => var, ...} := expr`
        if let Expression::MapLiteral {
            pairs,
            span: lhs_span,
        } = &expr
        {
            return self.parse_map_destructure(pairs, *lhs_span);
        }

        // Simple assignment: `target := value`
        self.parse_simple_assignment(expr)
    }

    /// Parses a collection (array/list) destructuring assignment.
    fn parse_collection_destructure(
        &mut self,
        elements: &[Expression],
        lhs_span: Span,
        list_syntax: bool,
    ) -> Expression {
        match Self::collection_elements_to_pattern(elements, lhs_span, list_syntax) {
            Ok(pattern) => {
                if let Err(error) = self.enter_nesting(lhs_span) {
                    return error;
                }
                let value = Box::new(self.parse_assignment());
                self.leave_nesting();
                let span = lhs_span.merge(value.span());
                Expression::DestructureAssignment {
                    pattern,
                    value,
                    span,
                }
            }
            Err(bad_span) => {
                self.diagnostics.push(Diagnostic::error(
                    "Destructuring patterns may only contain identifiers, '_', or literals",
                    bad_span,
                ));
                Expression::Error {
                    message: "Invalid destructuring pattern".into(),
                    span: lhs_span,
                }
            }
        }
    }

    /// Parses a map destructuring assignment.
    fn parse_map_destructure(&mut self, pairs: &[MapPair], lhs_span: Span) -> Expression {
        match Self::map_pairs_to_pattern(pairs, lhs_span) {
            Ok(pattern) => {
                if let Err(error) = self.enter_nesting(lhs_span) {
                    return error;
                }
                let value = Box::new(self.parse_assignment());
                self.leave_nesting();
                let span = lhs_span.merge(value.span());
                Expression::DestructureAssignment {
                    pattern,
                    value,
                    span,
                }
            }
            Err(bad_span) => {
                self.diagnostics.push(Diagnostic::error(
                    "Map destructuring: keys must be symbols or string literals, values must be identifiers or '_'",
                    bad_span,
                ));
                Expression::Error {
                    message: "Invalid map destructuring pattern".into(),
                    span: lhs_span,
                }
            }
        }
    }

    /// Parses a simple assignment (`identifier := value` or `field.access := value`).
    fn parse_simple_assignment(&mut self, expr: Expression) -> Expression {
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
        Expression::Assignment {
            target: Box::new(expr),
            value,
            span,
        }
    }

    /// Converts collection literal elements to a destructuring pattern.
    ///
    /// Returns `Ok(Pattern::Array)` if all elements are valid pattern positions
    /// (identifiers, `_`, or literals), or `Err(span)` pointing at the first
    /// invalid element. The `list_syntax` flag preserves whether the source
    /// used `#(...)` or `#[...]` delimiters.
    fn collection_elements_to_pattern(
        elements: &[Expression],
        span: Span,
        list_syntax: bool,
    ) -> Result<Pattern, Span> {
        let mut patterns = Vec::new();
        let mut rest = None;
        for elem in elements {
            if let Expression::Spread { name, span: s } = elem {
                let rest_pat = if name.name == "_" {
                    Pattern::Wildcard(*s)
                } else {
                    Pattern::Variable(name.clone())
                };
                rest = Some(Box::new(rest_pat));
                break; // rest is always last; ignore anything after
            }
            let pat = match elem {
                Expression::Identifier(id) if id.name == "_" => Pattern::Wildcard(id.span),
                Expression::Identifier(id) => Pattern::Variable(id.clone()),
                Expression::Literal(lit, s) => Pattern::Literal(lit.clone(), *s),
                _ => return Err(elem.span()),
            };
            patterns.push(pat);
        }
        Ok(Pattern::Array {
            elements: patterns,
            rest,
            list_syntax,
            span,
        })
    }

    /// Converts map literal pairs to a destructuring pattern.
    ///
    /// Returns `Ok(Pattern::Map)` if keys are symbol or string literals and values are
    /// identifiers or `_`, or `Err(span)` pointing at the first invalid element.
    fn map_pairs_to_pattern(pairs: &[MapPair], span: Span) -> Result<Pattern, Span> {
        let mut pattern_pairs = Vec::new();
        for pair in pairs {
            // Key must be a symbol or string literal
            let key = match &pair.key {
                Expression::Literal(Literal::Symbol(s), _) => MapPatternKey::Symbol(s.clone()),
                Expression::Literal(Literal::String(s), _) => MapPatternKey::StringLit(s.clone()),
                _ => return Err(pair.key.span()),
            };
            // Value must be an identifier (variable or `_`)
            let value = match &pair.value {
                Expression::Identifier(id) if id.name == "_" => Pattern::Wildcard(id.span),
                Expression::Identifier(id) => Pattern::Variable(id.clone()),
                _ => return Err(pair.value.span()),
            };
            pattern_pairs.push(MapPatternPair {
                key,
                value,
                span: pair.span,
            });
        }
        Ok(Pattern::Map {
            pairs: pattern_pairs,
            span,
        })
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

        // Try binary message (including Arrow `->` as binary selector, ADR 0047)
        // and GtGt (`>>`) as the method lookup binary operator.
        let binary_op = match self.current_kind() {
            TokenKind::BinarySelector(op) => Some(op.clone()),
            TokenKind::Arrow => Some("->".into()),
            TokenKind::GtGt => Some(">>".into()),
            _ => None,
        };
        if let Some(op) = binary_op {
            let selector = MessageSelector::Binary(op);
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

        // Check if this is a keyword message.
        // A keyword token can never validly start a new statement on its own
        // (it always requires a receiver), so a keyword on a new line is
        // unambiguously a continuation of the previous expression (BT-1061).
        // The one exception is a keyword that begins a method definition
        // (`keyword: param =>`), which must not be consumed as a continuation.
        //
        // BT-1294: Inside a class method body `is_at_method_definition()` produces
        // false positives: its lookahead scans all continuation keyword lines plus
        // the next method's `-> Type =>`, making the whole sequence look like a
        // single keyword method definition.  Method definitions cannot nest inside
        // method bodies, so for the class-body case we use indentation instead:
        // class body members are always indented 2 spaces, so any keyword at
        // col ≤ 2 signals a sibling method definition, not a continuation.
        if !matches!(self.current_kind(), TokenKind::Keyword(_)) {
            return receiver;
        }
        if self.current_token().has_leading_newline() {
            if self.in_class_body && self.in_method_body {
                // Inside a class method body: stop at the class member boundary.
                //
                // Beamtalk canonical formatting (enforced by `beamtalk fmt`) always
                // places class body members at column 0–2 and continuation keywords
                // deeper than that.  We rely on this invariant here rather than a
                // wide-lookahead `is_at_method_definition()` call, which can span
                // into the next sibling method's `-> Type =>` annotation and produce
                // false positives (BT-1294).  Non-canonically-indented source (e.g.
                // members at col 4) should be formatted with `beamtalk fmt` first.
                let col = self
                    .current_token()
                    .indentation_after_newline()
                    .unwrap_or(0);
                if col <= 2 {
                    return receiver;
                }
            } else if !self.in_method_body && (self.is_at_method_definition() || self.in_class_body)
            {
                // Outside a method body: original lookahead-based check.
                return receiver;
            } else if self.in_method_body && !self.in_class_body && self.is_at_method_definition() {
                // Inside a standalone method body (not a class): original check.
                return receiver;
            }
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
        // BT-1294: When inside a class method body, track the indentation of the
        // first keyword that appears on a new line.  Any subsequent keyword at a
        // *lesser* indentation is at the class-member level (col ≤ 2) and signals
        // the start of a sibling method definition rather than a continuation.
        // Outside class method bodies the original `is_at_method_definition()`
        // lookahead is used (methods cannot nest anyway).
        let mut continuation_indent: Option<usize> = None;

        while let TokenKind::Keyword(keyword) = self.current_kind() {
            if self.current_token().has_leading_newline() {
                if self.in_class_body && self.in_method_body {
                    // Inside a class method body: use indentation comparison.
                    // Relies on the canonical ≤ 2 column invariant for class members
                    // (see the same assumption in the initial check above).
                    let current_indent = self.current_token().indentation_after_newline();
                    match continuation_indent {
                        None => {
                            // First keyword on a new line in this message.
                            // Stop immediately if it is at the class-member level
                            // (col ≤ 2), otherwise record it as the continuation level.
                            let col = current_indent.unwrap_or(0);
                            if col <= 2 {
                                break;
                            }
                            continuation_indent = current_indent;
                        }
                        Some(ci) => {
                            // Treat None as col 0: a token with no indentation info
                            // (or a col-0 newline) is shallower than any continuation.
                            let ind = current_indent.unwrap_or(0);
                            if ind < ci {
                                // Shallower than the established continuation level:
                                // this is a sibling method definition, not a continuation.
                                break;
                            }
                        }
                    }
                } else if !keywords.is_empty() && self.is_at_method_definition() {
                    // Outside a class method body: original lookahead-based check.
                    // Guard with `!keywords.is_empty()` to match previous behaviour.
                    break;
                }
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

        // Extract binary operator string from BinarySelector, Arrow (ADR 0047), or GtGt.
        // Arrow (`->`) is treated as the binary operator `"->"`.
        // GtGt (`>>`) is treated as the binary operator `">>"` (method lookup).
        while let Some(op) = match self.current_kind() {
            TokenKind::BinarySelector(op) => Some(op.clone()),
            TokenKind::Arrow => Some("->".into()),
            TokenKind::GtGt => Some(">>".into()),
            _ => None,
        } {
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
                    package: None,
                    span,
                }
            } else if self.check(&TokenKind::At) {
                // Package-qualified class reference: `json@Parser` (ADR 0070, Section 4)
                self.parse_package_qualified_class_reference(name, span)
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

    /// Parses a package-qualified class reference: `json@Parser`.
    ///
    /// Called when we have already consumed the package identifier and the current
    /// token is `At` (`@`). Expects an uppercase identifier to follow.
    ///
    /// Error recovery: if `@` is not followed by a class name, emits an error
    /// and returns the package name as a plain identifier.
    fn parse_package_qualified_class_reference(
        &mut self,
        package_name: &EcoString,
        package_span: Span,
    ) -> Expression {
        self.advance(); // consume @

        // Expect an uppercase identifier (class name) after `@`
        if let TokenKind::Identifier(class_name) = self.current_kind() {
            let first_char = class_name.chars().next();
            if first_char.is_some_and(char::is_uppercase) {
                let class_name = class_name.clone();
                let class_span = self.current_token().span();
                self.advance(); // consume class name
                let full_span = Span::new(package_span.start(), class_span.end());
                return Expression::ClassReference {
                    name: Identifier::new(class_name, class_span),
                    package: Some(Identifier::new(package_name.clone(), package_span)),
                    span: full_span,
                };
            }
        }

        // Error: `@` not followed by a class name
        self.error("expected class name after '@' in qualified reference (e.g., json@Parser)");
        // Recover by treating the package name as a regular identifier
        Expression::Identifier(Identifier::new(package_name.clone(), package_span))
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
                !body.is_empty() && self.current_token().has_blank_line_before_first_comment();
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
                    self.diagnostics.push(
                        Diagnostic::lint("unnecessary trailing `.` before `]`", period_span)
                            .with_hint("Remove the trailing `.`"),
                    );
                } else if !self.is_at_end() && self.current_token().has_leading_newline() {
                    // Period immediately before a newline — newline already separates statements
                    self.diagnostics.push(
                        Diagnostic::lint(
                            "unnecessary `.` — the following newline already separates statements",
                            period_span,
                        )
                        .with_hint("Remove the `.` and rely on the newline"),
                    );
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

        // Expect -> separator (Arrow token, ADR 0047)
        if !self.match_token(&TokenKind::Arrow) {
            self.error("Expected '->' after pattern in match arm");
        }

        // Parse body expression.  We use keyword-message level as the base
        // because `parse_cascade` would consume `;` (the arm separator) as
        // cascade syntax.  We add explicit support for `^` (non-local return)
        // and `:=` (assignment) on top, since both are valid in arm bodies.
        let body = if self.match_token(&TokenKind::Caret) {
            // Build a Return node using keyword-message level for the value
            // (not parse_return which goes through parse_assignment/cascade).
            let start = self.tokens[self.current - 1].span();
            let value = Box::new(self.parse_keyword_message());
            let end = value.span();
            Expression::Return {
                value,
                span: start.merge(end),
            }
        } else {
            let expr = self.parse_keyword_message();
            if self.match_token(&TokenKind::Assign) {
                // Build assignment inline using keyword-message level for
                // the RHS — parse_assign_rhs would go through parse_cascade
                // which consumes `;` (arm separators).
                if matches!(
                    expr,
                    Expression::Identifier(_) | Expression::FieldAccess { .. }
                ) {
                    let value = Box::new(self.parse_keyword_message());
                    let span = expr.span().merge(value.span());
                    Expression::Assignment {
                        target: Box::new(expr),
                        value,
                        span,
                    }
                } else {
                    let span = expr.span();
                    self.diagnostics.push(Diagnostic::error(
                        "Assignment target must be an identifier or field access",
                        span,
                    ));
                    Expression::Error {
                        message: "Invalid assignment target".into(),
                        span,
                    }
                }
            } else {
                expr
            }
        };
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

            // Constructor pattern: UpperCaseIdent followed by keyword(s)
            // e.g. `Result ok: v`, `Result error: _`
            TokenKind::Identifier(name)
                if name.chars().next().is_some_and(char::is_uppercase)
                    && matches!(self.peek_kind(), Some(TokenKind::Keyword(_))) =>
            {
                self.parse_constructor_pattern()
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

            // Array pattern: #[p1, p2, ...]
            TokenKind::ArrayOpen => self.parse_array_pattern(),

            // Map pattern: #{#key => var, "key" => var, ...}
            TokenKind::MapOpen => self.parse_map_pattern(),

            // Binary pattern: <<seg, seg, ...>>
            TokenKind::LtLt => self.parse_binary_pattern(),

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

    /// Parses an array destructuring pattern: `#[p1, p2, ...rest]`
    fn parse_array_pattern(&mut self) -> Pattern {
        let start = self
            .expect(&TokenKind::ArrayOpen, "Expected '#['")
            .map_or_else(|| self.current_token().span(), |t: Token| t.span());

        let mut elements = Vec::new();
        let mut rest = None;

        if !self.check(&TokenKind::RightBracket) {
            if self.check(&TokenKind::Ellipsis) {
                rest = Some(Box::new(self.parse_rest_pattern()));
            } else {
                elements.push(self.parse_pattern());
            }
            while rest.is_none() && self.match_binary_selector(",") {
                if self.check(&TokenKind::RightBracket) {
                    break; // trailing comma
                }
                if self.check(&TokenKind::Ellipsis) {
                    rest = Some(Box::new(self.parse_rest_pattern()));
                    break;
                }
                elements.push(self.parse_pattern());
            }
        }

        let end = self
            .expect(&TokenKind::RightBracket, "Expected ']'")
            .map_or(start, |t: Token| t.span());

        Pattern::Array {
            elements,
            rest,
            list_syntax: false,
            span: start.merge(end),
        }
    }

    /// Parses a rest pattern: `...identifier` or `..._`
    fn parse_rest_pattern(&mut self) -> Pattern {
        let ellipsis_span = self
            .expect(&TokenKind::Ellipsis, "Expected '...'")
            .map_or_else(|| self.current_token().span(), |t: Token| t.span());

        match self.current_token().kind() {
            TokenKind::Identifier(name) if name.as_str() == "_" => {
                let span = self.current_token().span();
                self.advance();
                Pattern::Wildcard(ellipsis_span.merge(span))
            }
            TokenKind::Identifier(_) => {
                let id = self.parse_identifier("Expected identifier after '...'");
                Pattern::Variable(Identifier::new(&id.name, ellipsis_span.merge(id.span)))
            }
            _ => {
                self.error("Expected identifier after '...'");
                Pattern::Wildcard(ellipsis_span)
            }
        }
    }

    /// Parses the key in a map pattern pair.
    ///
    /// Accepts a symbol (`#key`) or a string literal (`"key"`).
    /// Bare lowercase identifiers (e.g. `foo`) are rejected with a diagnostic
    /// error suggesting `#foo`.
    /// Returns `None` on an invalid token (error is pushed; caller should skip the pair).
    fn parse_map_pattern_key(&mut self) -> Option<MapPatternKey> {
        match self.current_kind() {
            TokenKind::Symbol(s) => {
                let s = s.clone();
                self.advance();
                Some(MapPatternKey::Symbol(s))
            }
            TokenKind::String(s) => {
                let s = s.clone();
                self.advance();
                Some(MapPatternKey::StringLit(s))
            }
            TokenKind::Identifier(name)
                if name.chars().next().is_some_and(char::is_lowercase)
                    && matches!(self.peek_kind(), Some(TokenKind::FatArrow)) =>
            {
                let name = name.clone();
                let span = self.advance().span();
                self.diagnostics.push(Diagnostic::error(
                    format!("bare word '{name}' is not a valid map key; did you mean '#{name}'?"),
                    span,
                ));
                None
            }
            _ => {
                let bad = self.advance();
                self.diagnostics.push(Diagnostic::error(
                    "Map destructuring key must be a symbol or string (e.g. #key or \"key\")",
                    bad.span(),
                ));
                None
            }
        }
    }

    /// Parses the value in a map pattern pair.
    ///
    /// Accepts a variable identifier, `_` wildcard, or a literal (for equality matching).
    fn parse_map_pattern_value(&mut self) -> Pattern {
        let value_span = self.current_token().span();
        match self.current_kind() {
            TokenKind::Identifier(name) if name.as_str() == "_" => {
                let span = self.advance().span();
                Pattern::Wildcard(span)
            }
            TokenKind::Identifier(_) => {
                let token = self.advance();
                let span = token.span();
                let TokenKind::Identifier(name) = token.into_kind() else {
                    unreachable!()
                };
                Pattern::Variable(Identifier::new(name, span))
            }
            TokenKind::String(_)
            | TokenKind::Integer(_)
            | TokenKind::Float(_)
            | TokenKind::Character(_)
            | TokenKind::Symbol(_) => {
                let expr = self.parse_literal();
                let span = expr.span();
                if let Expression::Literal(lit, _) = expr {
                    Pattern::Literal(lit, span)
                } else {
                    Pattern::Wildcard(span)
                }
            }
            // Negative numeric literals: `-1`, `-3.14`
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
                        self.diagnostics.push(Diagnostic::error(
                            "Expected integer or float after '-' in map pattern value",
                            start,
                        ));
                        Pattern::Wildcard(start)
                    }
                }
            }
            _ => {
                let bad = self.advance();
                self.diagnostics.push(Diagnostic::error(
                    "Map pattern value must be a variable, '_', or a literal",
                    bad.span(),
                ));
                Pattern::Wildcard(value_span)
            }
        }
    }

    /// Parses a map destructuring pattern: `#{key => value, ...}`
    ///
    /// Keys must be symbol literals (`#key`) or string literals (`"key"`).
    /// Bare lowercase identifiers are a compile error (BT-1240); use `#key` instead.
    /// Values may be variable identifiers, `_` wildcards, or literals (for equality
    /// matching in `match:` arms).
    fn parse_map_pattern(&mut self) -> Pattern {
        let start = self
            .expect(&TokenKind::MapOpen, "Expected '#{'")
            .map_or_else(|| self.current_token().span(), |t: Token| t.span());

        let mut pairs = Vec::new();

        if !self.check(&TokenKind::RightBrace) {
            loop {
                if self.is_at_end() {
                    break;
                }
                let pair_start = self.current_token().span();
                let key_opt = self.parse_map_pattern_key();

                if !matches!(self.current_kind(), TokenKind::FatArrow) {
                    let bad = self.advance();
                    self.diagnostics.push(Diagnostic::error(
                        "Expected '=>' after map pattern key",
                        bad.span(),
                    ));
                    break;
                }
                self.advance(); // consume '=>'

                let value = self.parse_map_pattern_value();
                // Only push the pair if the key was valid; skip error-recovery pairs
                // so downstream passes don't see a synthetic `#_` lookup key.
                if let Some(key) = key_opt {
                    let pair_span = pair_start.merge(value.span());
                    pairs.push(MapPatternPair {
                        key,
                        value,
                        span: pair_span,
                    });
                }

                if matches!(self.current_kind(), TokenKind::BinarySelector(s) if s.as_str() == ",")
                {
                    self.advance();
                    if self.check(&TokenKind::RightBrace) {
                        break; // trailing comma
                    }
                } else {
                    break;
                }
            }
        }

        let end = self
            .expect(&TokenKind::RightBrace, "Expected '}' to close map pattern")
            .map_or(start, |t: Token| t.span());

        Pattern::Map {
            pairs,
            span: start.merge(end),
        }
    }

    /// Parses a constructor pattern: `UpperCaseIdent keyword: binding ...`
    ///
    /// Example: `Result ok: v`, `Result error: _`, `Result ok: 42`
    ///
    /// The class identifier must start with an uppercase letter. Each `keyword: binding`
    /// pair names one constructor argument. The binding is a simple variable, wildcard,
    /// or literal — not an arbitrary expression.
    fn parse_constructor_pattern(&mut self) -> Pattern {
        let class_token = self.advance(); // consume uppercase class identifier
        let start = class_token.span();
        let TokenKind::Identifier(class_name) = class_token.into_kind() else {
            unreachable!("parse_constructor_pattern called on non-identifier token")
        };
        let class = Identifier::new(class_name, start);

        let mut keywords: Vec<(Identifier, Pattern)> = Vec::new();
        let mut end_span = start;

        while let TokenKind::Keyword(kw) = self.current_kind() {
            // `when:` is the guard keyword for match arms — stop here so the
            // enclosing match arm parser can handle the guard clause.
            if kw.as_str() == "when:" {
                break;
            }
            let kw_name = kw.clone();
            let kw_span = self.current_token().span();
            self.advance(); // consume keyword
            let binding = self.parse_constructor_binding();
            end_span = binding.span();
            keywords.push((Identifier::new(kw_name, kw_span), binding));
        }

        if keywords.is_empty() {
            // Shouldn't happen (we only enter this function when peek is Keyword),
            // but produce a sensible error rather than panicking.
            self.diagnostics.push(Diagnostic::error(
                "Constructor pattern requires at least one keyword argument (e.g. `Result ok: v`)"
                    .to_string(),
                start,
            ));
        }

        Pattern::Constructor {
            class,
            keywords,
            span: start.merge(end_span),
        }
    }

    /// Parses the binding position of a constructor pattern argument.
    ///
    /// Accepts: wildcard `_`, variable identifier, or a literal (integer, float, string, symbol,
    /// or negative number like `-1`).
    fn parse_constructor_binding(&mut self) -> Pattern {
        match self.current_kind() {
            TokenKind::Identifier(name) if name.as_str() == "_" => {
                Pattern::Wildcard(self.advance().span())
            }
            TokenKind::Identifier(_) => {
                let token = self.advance();
                let span = token.span();
                let TokenKind::Identifier(name) = token.into_kind() else {
                    unreachable!()
                };
                Pattern::Variable(Identifier::new(name, span))
            }
            TokenKind::Integer(_)
            | TokenKind::Float(_)
            | TokenKind::String(_)
            | TokenKind::Character(_)
            | TokenKind::Symbol(_) => self.parse_pattern(),
            // Negative numeric literals: `-1`, `-3.14`
            TokenKind::BinarySelector(op) if op.as_str() == "-" => self.parse_pattern(),
            _ => {
                let span = self.current_token().span();
                self.diagnostics.push(Diagnostic::error(
                    "Expected variable, '_', or literal after constructor keyword".to_string(),
                    span,
                ));
                Pattern::Wildcard(span)
            }
        }
    }

    /// Parses a binary pattern: `<<seg, seg, ...>>`
    ///
    /// Segment syntax: `varname`, `varname:size`, `varname/type`, `varname:size/type-modifiers`
    /// where type is one of `integer`, `float`, `binary`, `utf8`
    /// and modifiers include `signed`/`unsigned`, `big`/`little`/`native`.
    fn parse_binary_pattern(&mut self) -> Pattern {
        let start = self.current_token().span();
        self.advance(); // consume `<<`

        let mut segments = Vec::new();

        if !matches!(self.current_kind(), TokenKind::GtGt) {
            segments.push(self.parse_binary_segment());
            while self.match_binary_selector(",") {
                if matches!(self.current_kind(), TokenKind::GtGt) {
                    self.error("Trailing comma is not allowed before '>>' in binary pattern");
                    break;
                }
                segments.push(self.parse_binary_segment());
            }
        }

        let end = if matches!(self.current_kind(), TokenKind::GtGt) {
            let t = self.advance(); // consume `>>`
            t.span()
        } else {
            self.error("Expected '>>' to close binary pattern");
            start
        };

        Pattern::Binary {
            segments,
            span: start.merge(end),
        }
    }

    /// Parses one segment of a binary pattern.
    ///
    /// Two syntactic forms are supported:
    /// - `name/type`: identifier followed by type specifiers — `rest/binary`
    /// - `name:size/type`: identifier with size, optionally followed by type — `version:8, length:16/big`
    ///
    /// Note: `name:size` lexes as `Keyword("name:")` + size-token (no space needed between
    /// the name and the colon). The `name / type` form (with space) lexes as
    /// `Identifier("name")` + `BinarySelector("/")` + `Identifier("type")`.
    fn parse_binary_segment_size(&mut self) -> Option<Box<Expression>> {
        match self.current_kind() {
            TokenKind::Integer(_) | TokenKind::Identifier(_) => {
                let expr = self.parse_primary();
                match &expr {
                    Expression::Literal(Literal::Integer(_), _) | Expression::Identifier(_) => {
                        Some(Box::new(expr))
                    }
                    _ => {
                        self.error(
                            "Binary segment size must be an integer literal or variable name",
                        );
                        None
                    }
                }
            }
            _ => {
                self.error("Expected size expression after ':' in binary segment");
                None
            }
        }
    }

    fn parse_binary_segment(&mut self) -> BinarySegment {
        let start = self.current_token().span();
        let (value, size) = match self.current_kind() {
            // Wildcard: `_` or `_:size` (fixed-width skip segment)
            TokenKind::Identifier(name) if name.as_str() == "_" => {
                let span = self.advance().span();
                let sz = if matches!(self.current_kind(), TokenKind::Colon) {
                    self.advance(); // consume `:`
                    self.parse_binary_segment_size()
                } else {
                    None
                };
                (Pattern::Wildcard(span), sz)
            }

            // `name:size` — the `:` attached to the name lexes as Keyword("name:")
            TokenKind::Keyword(kw) => {
                let kw = kw.clone();
                let token = self.advance();
                let kw_span = token.span();
                // Strip trailing `:` to recover variable name; adjust span to exclude `:`
                let name: EcoString = kw.trim_end_matches(':').into();
                let var_span = Span::new(kw_span.start(), kw_span.end().saturating_sub(1));
                let var = if name == "_" {
                    Pattern::Wildcard(var_span)
                } else {
                    Pattern::Variable(Identifier::new(name, var_span))
                };
                let sz = self.parse_binary_segment_size();
                (var, sz)
            }

            // `name` — variable, with optional explicit `:size` (Colon + size-token)
            TokenKind::Identifier(_) => {
                let token = self.advance();
                let span = token.span();
                let var = if let TokenKind::Identifier(name) = token.into_kind() {
                    Pattern::Variable(Identifier::new(name, span))
                } else {
                    unreachable!()
                };
                let sz = if matches!(self.current_kind(), TokenKind::Colon) {
                    self.advance(); // consume `:`
                    self.parse_binary_segment_size()
                } else {
                    None
                };
                (var, sz)
            }

            _ => {
                let span = self.current_token().span();
                self.error("Expected variable name in binary segment");
                (Pattern::Wildcard(span), None)
            }
        };

        // Optional type/modifiers: `/type[-modifier]*`
        let (segment_type, signedness, endianness) = self.parse_binary_segment_specifiers();

        let end = self.tokens[self.current.saturating_sub(1)].span();
        BinarySegment {
            value,
            size,
            segment_type,
            signedness,
            endianness,
            unit: None,
            span: start.merge(end),
        }
    }

    /// Parses optional `/type[-modifier]*` specifiers for a binary segment.
    ///
    /// Returns `(segment_type, signedness, endianness)`.
    fn parse_binary_segment_specifiers(
        &mut self,
    ) -> (
        Option<BinarySegmentType>,
        Option<BinarySignedness>,
        Option<BinaryEndianness>,
    ) {
        let mut segment_type = None;
        let mut signedness = None;
        let mut endianness = None;

        if matches!(self.current_kind(), TokenKind::BinarySelector(s) if s.as_str() == "/") {
            self.advance(); // consume `/`
            if !matches!(self.current_kind(), TokenKind::Identifier(_)) {
                self.error("Expected a binary segment specifier after '/'");
            }

            // Parse identifiers separated by `-` (e.g. `signed-little`)
            while let TokenKind::Identifier(name) = self.current_kind() {
                let name = name.clone();
                self.advance();
                let span = self.tokens[self.current - 1].span();
                match name.as_str() {
                    "integer" | "float" | "binary" | "bytes" | "utf8" => {
                        if segment_type.is_some() {
                            self.diagnostics.push(Diagnostic::error(
                                format!(
                                    "Conflicting binary segment type specifier '{name}': type already set"
                                ),
                                span,
                            ));
                        } else {
                            segment_type = Some(match name.as_str() {
                                "integer" => BinarySegmentType::Integer,
                                "float" => BinarySegmentType::Float,
                                "utf8" => BinarySegmentType::Utf8,
                                _ => BinarySegmentType::Binary, // "binary" | "bytes"
                            });
                        }
                    }
                    "signed" | "unsigned" => {
                        if signedness.is_some() {
                            self.diagnostics.push(Diagnostic::error(
                                format!(
                                    "Conflicting binary segment signedness specifier '{name}': signedness already set"
                                ),
                                span,
                            ));
                        } else {
                            signedness = Some(if name.as_str() == "signed" {
                                BinarySignedness::Signed
                            } else {
                                BinarySignedness::Unsigned
                            });
                        }
                    }
                    "big" | "little" | "native" => {
                        if endianness.is_some() {
                            self.diagnostics.push(Diagnostic::error(
                                format!(
                                    "Conflicting binary segment endianness specifier '{name}': endianness already set"
                                ),
                                span,
                            ));
                        } else {
                            endianness = Some(match name.as_str() {
                                "big" => BinaryEndianness::Big,
                                "little" => BinaryEndianness::Little,
                                _ => BinaryEndianness::Native, // "native"
                            });
                        }
                    }
                    unknown => {
                        self.diagnostics.push(Diagnostic::error(
                            format!("Unknown binary segment specifier: '{unknown}'"),
                            span,
                        ));
                    }
                }
                // Continue if next is `-` (chained modifiers like `signed-little`)
                if matches!(self.current_kind(), TokenKind::BinarySelector(s) if s.as_str() == "-")
                {
                    self.advance(); // consume `-`
                    if !matches!(self.current_kind(), TokenKind::Identifier(_)) {
                        self.error("Expected a specifier name after '-' in binary segment type");
                        break;
                    }
                } else {
                    break;
                }
            }
        }

        (segment_type, signedness, endianness)
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

    /// Parses an `@expect category` or `@expect category "reason"` directive.
    ///
    /// The `@expect` token has already been identified by `parse_primary`.
    /// This method consumes it, parses the category name, and optionally
    /// parses a reason string that follows (BT-1918).
    fn parse_expect_directive(&mut self) -> Expression {
        let start_token = self.advance(); // consume AtExpect
        let start = start_token.span();

        if let TokenKind::Identifier(name) = self.current_kind() {
            let name = name.clone();
            let end_token = self.advance();
            let mut span = start.merge(end_token.span());
            if let Some(category) = ExpectCategory::from_name(&name) {
                // BT-1918: Parse optional reason string after category.
                let reason = if let TokenKind::String(reason_str) = self.current_kind() {
                    let reason_str = reason_str.clone();
                    let reason_token = self.advance();
                    span = start.merge(reason_token.span());
                    Some(reason_str)
                } else {
                    None
                };
                Expression::ExpectDirective {
                    category,
                    reason,
                    span,
                }
            } else {
                let valid = ExpectCategory::valid_names().join(", ");
                let message: EcoString =
                    format!("unknown @expect category '{name}', valid categories are: {valid}")
                        .into();
                self.diagnostics
                    .push(Diagnostic::error(message.clone(), span));
                Expression::Error { message, span }
            }
        } else {
            let valid = ExpectCategory::valid_names().join(", ");
            let span = start;
            let message: EcoString =
                format!("@expect must be followed by a category name ({valid})").into();
            self.diagnostics
                .push(Diagnostic::error(message.clone(), span));
            Expression::Error { message, span }
        }
    }

    /// Parses a map literal: `#{key => value, ...}`
    ///
    /// Keys must be symbol literals (`#key`), string literals, integers, or parenthesized
    /// expressions (e.g. `#{(varKey) => v}` for dynamic keys). Bare lowercase identifiers
    /// are rejected with a diagnostic error suggesting `#key` (BT-1240).
    /// Keys are parsed as unary expressions (primaries + unary messages),
    /// which stops at binary operators like `=>` and `,`.
    /// Values are parsed as keyword messages (lowest message precedence), so
    /// `#{#key => Foo new: #{#a => 1}}` parses the nested keyword send correctly.
    /// The `,` and `}` delimiters terminate parsing naturally: `,` has no
    /// binding power (so binary parsing stops) and `}` cannot start a message
    /// send (BT-1854).
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

            // BT-1240: Bare identifier keys in map literals are a compile error.
            // `#{foo => v}` is invalid; write `#{#foo => v}` instead.
            let key = if matches!(self.current_kind(), TokenKind::Identifier(name) if name.chars().next().is_some_and(char::is_lowercase))
                && matches!(self.peek_kind(), Some(TokenKind::FatArrow))
            {
                let token = self.advance();
                let span = token.span();
                let TokenKind::Identifier(name) = token.kind() else {
                    unreachable!()
                };
                let name = name.clone();
                let message: EcoString =
                    format!("bare word '{name}' is not a valid map key; did you mean '#{name}'?")
                        .into();
                self.diagnostics
                    .push(Diagnostic::error(message.clone(), span));
                Expression::Error { message, span }
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

            // Parse value expression as a full keyword message (lowest message
            // precedence), so `#{#k => Foo new: #{#a => 1}}` correctly parses
            // the nested keyword send as the map value.  The `,` and `}` delimiters
            // terminate parsing naturally: `,` has no binding power (so binary
            // parsing stops) and `}` cannot start a message send (BT-1854).
            let value = self.parse_keyword_message();

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
    /// List elements are parsed as keyword messages (the lowest message precedence),
    /// so `#(obj kw: arg)` is a single-element list containing the keyword send.
    /// The `,`, `|`, and `)` delimiters terminate parsing naturally since they are
    /// not valid keyword selectors or binary selectors in element context.
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
            // Rest/spread element: `...ident` — only valid in destructuring patterns
            let is_spread = self.check(&TokenKind::Ellipsis);
            let elem = if is_spread {
                let ellipsis_token = self.advance();
                let ellipsis_span = ellipsis_token.span();
                let name = self.parse_identifier("Expected identifier after '...'");
                let span = ellipsis_span.merge(name.span);
                Expression::Spread { name, span }
            } else {
                // Parse element as a full keyword message (lowest message precedence), so
                // `#(obj kw: arg)` is a single-element list containing the keyword send.
                // `,`, `|`, and `)` are not keyword or binary selectors so they terminate naturally.
                self.parse_keyword_message()
            };
            let elem_span = elem.span();
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
                if is_spread {
                    self.diagnostics.push(Diagnostic::error(
                        "Rest pattern `...` must be the last element in a list pattern",
                        elem_span,
                    ));
                }
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
    /// Array elements are parsed as keyword messages (the lowest message precedence).
    /// Closed by `]`.
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
            // Rest/spread element: `...ident` — only valid in destructuring patterns
            let is_spread = self.check(&TokenKind::Ellipsis);
            let elem = if is_spread {
                let ellipsis_token = self.advance();
                let ellipsis_span = ellipsis_token.span();
                let name = self.parse_identifier("Expected identifier after '...'");
                let span = ellipsis_span.merge(name.span);
                Expression::Spread { name, span }
            } else {
                // Parse element as a full keyword message so `#[obj kw: arg]` works.
                self.parse_keyword_message()
            };
            let elem_span = elem.span();
            elements.push(elem);

            // Check for comma (continue) or closing bracket (end)
            if matches!(self.current_kind(), TokenKind::BinarySelector(s) if s.as_str() == ",") {
                if is_spread {
                    self.diagnostics.push(Diagnostic::error(
                        "Rest pattern `...` must be the last element in an array pattern",
                        elem_span,
                    ));
                }
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

    // ========================================================================
    // BT-1240: Bare-word map key error tests
    // ========================================================================

    fn parse_source(src: &str) -> (crate::ast::Module, Vec<crate::source_analysis::Diagnostic>) {
        use crate::source_analysis::{lex_with_eof, parse};
        parse(lex_with_eof(src))
    }

    #[test]
    fn bare_word_map_key_produces_error() {
        use crate::source_analysis::Severity;
        let (_module, diags) = parse_source("#{foo => 1}");
        let errors: Vec<_> = diags
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();
        assert_eq!(errors.len(), 1, "expected one error, got: {errors:?}");
        assert!(
            errors[0].message.contains("bare word 'foo'"),
            "message: {}",
            errors[0].message
        );
        assert!(
            errors[0].message.contains("'#foo'"),
            "message should suggest #foo: {}",
            errors[0].message
        );
    }

    #[test]
    fn explicit_symbol_map_key_no_error() {
        use crate::source_analysis::Severity;
        let (_module, diags) = parse_source("#{#foo => 1}");
        let errors: Vec<_> = diags
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();
        assert!(
            errors.is_empty(),
            "explicit #foo key should produce no errors, got: {errors:?}"
        );
    }

    #[test]
    fn parenthesized_dynamic_map_key_no_error() {
        use crate::source_analysis::Severity;
        // `#{(k) => 1}` is the escape hatch for variable keys — must not error
        let (_module, diags) = parse_source("#{(k) => 1}");
        let errors: Vec<_> = diags
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();
        assert!(
            errors.is_empty(),
            "parenthesized dynamic key should produce no errors, got: {errors:?}"
        );
    }

    #[test]
    fn bare_word_map_pattern_key_produces_error() {
        use crate::source_analysis::Severity;
        let (_module, diags) =
            parse_source("Object subclass: Foo\n  m: x => x match: [#{foo => v} -> v]");
        let errors: Vec<_> = diags
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();
        assert_eq!(errors.len(), 1, "expected one error, got: {errors:?}");
        assert!(
            errors[0].message.contains("bare word 'foo'"),
            "message: {}",
            errors[0].message
        );
    }
}
