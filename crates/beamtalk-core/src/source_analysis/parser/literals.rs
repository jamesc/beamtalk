// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Collection-literal parsing for Beamtalk.
//!
//! Map (`#{...}`), list (`#(...)`), array (`#[...]`) and tuple (`{...}`)
//! literal parsing, plus the comment-capture and error-recovery helpers
//! those literals share. Split out of `expressions.rs` (BT-5042); the
//! methods are unchanged and remain on [`Parser`].

use crate::ast::{Comment, CommentAttachment, Expression, LiteralCommentSlots, MapPair};
use crate::source_analysis::{Span, Token, TokenKind};
use ecow::EcoString;

use super::{Diagnostic, Parser};

impl Parser {
    /// Parses a map literal: `#{key => value, ...}`
    ///
    /// Keys must be symbol literals (`#key`), string literals, integers, or parenthesized
    /// expressions (e.g. `#{(varKey) => v}` for dynamic keys). Bare lowercase identifiers
    /// are rejected with a diagnostic error suggesting `#key`.
    /// Keys are parsed as unary expressions (primaries + unary messages),
    /// which stops at binary operators like `=>` and `,`.
    /// Values are parsed as keyword messages (lowest message precedence), so
    /// `#{#key => Foo new: #{#a => 1}}` parses the nested keyword send correctly.
    /// The `,` and `}` delimiters terminate parsing naturally: `,` has no
    /// binding power (so binary parsing stops) and `}` cannot start a message
    /// send.
    ///
    /// `pub(super)` so `declarations.rs`'s `uses:` line parser can consume
    /// (and discard) an `aliasing:` clause's `#{...}` argument for parse
    /// recovery — `aliasing:` itself is reserved but "not yet supported"
    /// in v1 (ADR 0127 §Status 4).
    #[allow(clippy::too_many_lines)] // element loop plus comment capture
    pub(super) fn parse_map_literal(&mut self) -> Expression {
        let start_token = self.expect(&TokenKind::MapOpen, "Expected '#{'");
        let start = start_token.map_or_else(|| self.current_token().span(), |t: Token| t.span());

        let mut pairs = Vec::new();
        let mut slots = LiteralCommentSlots::default();
        let open_idx = self.current.saturating_sub(1);

        // Handle empty map: #{}
        if self.check(&TokenKind::RightBrace) {
            self.collect_literal_closing(&mut slots, open_idx);
            let end_token = self.advance();
            let span = start.merge(end_token.span());
            self.record_literal_comments(span, slots);
            return Expression::MapLiteral { pairs, span };
        }

        // Parse key-value pairs
        loop {
            // Guard: stop if we've reached EOF (prevents infinite loop on unclosed maps)
            if self.is_at_end() {
                break;
            }

            let pair_start = self.current_token().span();
            let mut item_comments = self.collect_literal_item_leading(&slots, open_idx);

            // Bare identifier keys in map literals are a compile error.
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
            if matches!(self.current_kind(), TokenKind::FatArrow) {
                // Comments between the key and `=>` join the pair's leading
                // comments, so they are kept (printed above the pair).
                if let Some(after_key) = self.collect_trailing_comment() {
                    item_comments.leading.push(after_key);
                }
                item_comments
                    .leading
                    .extend(self.collect_comment_attachment().leading);
            }
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
            // Likewise comments between `=>` and the value.
            if let Some(after_arrow) = self.collect_trailing_comment() {
                item_comments.leading.push(after_arrow);
            }
            item_comments
                .leading
                .extend(self.collect_comment_attachment().leading);

            // Parse value expression as a full keyword message (lowest message
            // precedence), so `#{#k => Foo new: #{#a => 1}}` correctly parses
            // the nested keyword send as the map value.  The `,` and `}` delimiters
            // terminate parsing naturally: `,` has no binding power (so binary
            // parsing stops) and `}` cannot start a message send.
            let value = self.parse_keyword_expression();

            let pair_span = pair_start.merge(value.span());
            pairs.push(MapPair::new(key, value, pair_span));
            let item_end = self.current - 1;
            slots.items.push(item_comments);

            // Check for comma (continue) or closing brace (end)
            if matches!(self.current_kind(), TokenKind::BinarySelector(s) if s.as_str() == ",") {
                self.advance(); // consume comma
                self.collect_literal_item_trailing(&mut slots, item_end);
                // Allow trailing comma
                if self.check(&TokenKind::RightBrace) {
                    break;
                }
            } else if self.check(&TokenKind::RightBrace) {
                self.collect_literal_item_trailing(&mut slots, item_end);
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

        let end_span = self.finish_literal(
            &TokenKind::RightBrace,
            "Expected '}' to close map literal",
            &mut slots,
            open_idx,
        );

        let span = start.merge(end_span);
        self.record_literal_comments(span, slots);
        Expression::MapLiteral { pairs, span }
    }

    /// Parses a list literal: `#(expr, expr, ...)` or `#(head | tail)` (cons)
    ///
    /// List elements are parsed as keyword messages (the lowest message precedence),
    /// so `#(obj kw: arg)` is a single-element list containing the keyword send.
    /// The `,`, `|`, and `)` delimiters terminate parsing naturally since they are
    /// not valid keyword selectors or binary selectors in element context.
    ///
    /// `pub(super)` so `declarations.rs`'s `uses:` line parser
    /// (`parse_selector_symbol_list`, ADR 0127 §2) can reuse this for
    /// `excluding:`/`overriding:` selector lists rather than duplicating
    /// the comma/paren/cons/error-recovery handling.
    #[allow(clippy::too_many_lines)] // element loop plus comment capture
    pub(super) fn parse_list_literal(&mut self) -> Expression {
        let start_token = self.expect(&TokenKind::ListOpen, "Expected '#('");
        let start = start_token.map_or_else(|| self.current_token().span(), |t: Token| t.span());

        let mut elements = Vec::new();
        let mut slots = LiteralCommentSlots::default();
        let open_idx = self.current.saturating_sub(1);

        // Handle empty list: #()
        if self.check(&TokenKind::RightParen) {
            self.collect_literal_closing(&mut slots, open_idx);
            let end_token = self.advance();
            let span = start.merge(end_token.span());
            self.record_literal_comments(span, slots);
            return Expression::ListLiteral {
                elements,
                tail: None,
                span,
            };
        }

        // Parse elements
        loop {
            let item_comments = self.collect_literal_item_leading(&slots, open_idx);
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
                // A `|>` pipeline is a keyword expression too (ADR 0145).
                self.parse_keyword_expression()
            };
            let elem_span = elem.span();
            elements.push(elem);
            let item_end = self.current - 1;
            slots.items.push(item_comments);

            // Check for cons operator `|`
            if self.check(&TokenKind::Pipe) {
                self.collect_literal_item_trailing(&mut slots, item_end);
                // Comments before the `|` and between the `|` and the tail
                // belong to the tail slot.
                let mut tail_comments = self.collect_comment_attachment();
                self.advance(); // consume `|`
                if let Some(after_pipe) = self.collect_trailing_comment() {
                    tail_comments.leading.push(after_pipe);
                }
                tail_comments
                    .leading
                    .extend(self.collect_comment_attachment().leading);
                let tail = self.parse_unary_message();
                let tail_end = self.current - 1;
                slots.items.push(tail_comments);
                self.collect_literal_item_trailing(&mut slots, tail_end);
                if self.check(&TokenKind::RightParen) {
                    self.collect_literal_closing(&mut slots, open_idx);
                }
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
                self.record_literal_comments(span, slots);
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
                self.collect_literal_item_trailing(&mut slots, item_end);
                // Allow trailing comma
                if self.check(&TokenKind::RightParen) {
                    break;
                }
            } else if self.check(&TokenKind::RightParen) {
                self.collect_literal_item_trailing(&mut slots, item_end);
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

        let end_span = self.finish_literal(
            &TokenKind::RightParen,
            "Expected ')' to close list literal",
            &mut slots,
            open_idx,
        );

        let span = start.merge(end_span);
        self.record_literal_comments(span, slots);
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
    pub(super) fn parse_array_literal(&mut self) -> Expression {
        let start_token = self.expect(&TokenKind::ArrayOpen, "Expected '#['");
        let start = start_token.map_or_else(|| self.current_token().span(), |t: Token| t.span());

        let mut elements = Vec::new();
        let mut slots = LiteralCommentSlots::default();
        let open_idx = self.current.saturating_sub(1);

        // Handle empty array: #[]
        if self.check(&TokenKind::RightBracket) {
            self.collect_literal_closing(&mut slots, open_idx);
            let end_token = self.advance();
            let span = start.merge(end_token.span());
            self.record_literal_comments(span, slots);
            return Expression::ArrayLiteral { elements, span };
        }

        // Parse elements
        loop {
            let item_comments = self.collect_literal_item_leading(&slots, open_idx);
            // Rest/spread element: `...ident` — only valid in destructuring patterns
            let is_spread = self.check(&TokenKind::Ellipsis);
            let elem = if is_spread {
                let ellipsis_token = self.advance();
                let ellipsis_span = ellipsis_token.span();
                let name = self.parse_identifier("Expected identifier after '...'");
                let span = ellipsis_span.merge(name.span);
                Expression::Spread { name, span }
            } else {
                // Parse element as a full keyword expression (a keyword
                // message or `|>` pipeline) so `#[obj kw: arg]` works.
                self.parse_keyword_expression()
            };
            let elem_span = elem.span();
            elements.push(elem);
            let item_end = self.current - 1;
            slots.items.push(item_comments);

            // Check for comma (continue) or closing bracket (end)
            if matches!(self.current_kind(), TokenKind::BinarySelector(s) if s.as_str() == ",") {
                if is_spread {
                    self.diagnostics.push(Diagnostic::error(
                        "Rest pattern `...` must be the last element in an array pattern",
                        elem_span,
                    ));
                }
                self.advance(); // consume comma
                self.collect_literal_item_trailing(&mut slots, item_end);
                // Allow trailing comma
                if self.check(&TokenKind::RightBracket) {
                    break;
                }
            } else if self.check(&TokenKind::RightBracket) {
                self.collect_literal_item_trailing(&mut slots, item_end);
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

        let end_span = self.finish_literal(
            &TokenKind::RightBracket,
            "Expected ']' to close array literal",
            &mut slots,
            open_idx,
        );

        let span = start.merge(end_span);
        self.record_literal_comments(span, slots);
        Expression::ArrayLiteral { elements, span }
    }

    /// Parses a tuple literal: `{expr, expr, ...}` (ADR 0136).
    ///
    /// Evaluates to a raw Erlang tuple (class `Tuple`). Elements are parsed
    /// as keyword messages, like the other collection literals; a trailing
    /// comma is allowed, and `{}` / `{x}` are valid. There is no `...`
    /// spread form.
    pub(super) fn parse_tuple_literal(&mut self) -> Expression {
        let start_token = self.expect(&TokenKind::LeftBrace, "Expected '{'");
        let start = start_token.map_or_else(|| self.current_token().span(), |t: Token| t.span());

        let mut elements = Vec::new();
        let mut slots = LiteralCommentSlots::default();
        let open_idx = self.current.saturating_sub(1);

        // Handle empty tuple: {}
        if self.check(&TokenKind::RightBrace) {
            self.collect_literal_closing(&mut slots, open_idx);
            let end_token = self.advance();
            let span = start.merge(end_token.span());
            self.record_literal_comments(span, slots);
            return Expression::TupleLiteral { elements, span };
        }

        loop {
            let item_comments = self.collect_literal_item_leading(&slots, open_idx);
            elements.push(self.parse_keyword_expression());
            let item_end = self.current - 1;
            slots.items.push(item_comments);

            if matches!(self.current_kind(), TokenKind::BinarySelector(s) if s.as_str() == ",") {
                self.advance(); // consume comma
                self.collect_literal_item_trailing(&mut slots, item_end);
                if self.check(&TokenKind::RightBrace) {
                    break; // trailing comma
                }
            } else if self.check(&TokenKind::RightBrace)
                || self.is_at_end()
                || self.at_tuple_recovery_boundary()
            {
                self.collect_literal_item_trailing(&mut slots, item_end);
                // A `]`/`)` belongs to an enclosing construct, and a token on
                // a new line starts the next statement or method: leave both
                // for the caller and report the missing `}` below.
                break;
            } else {
                let bad_token = self.advance();
                let mut diagnostic = Diagnostic::error(
                    format!(
                        "Expected ',' or '}}' in tuple literal, found {}",
                        bad_token.kind()
                    ),
                    bad_token.span(),
                );
                if matches!(bad_token.kind(), TokenKind::Period) {
                    diagnostic = diagnostic.with_hint(
                        "tuple elements are separated by ',': write `{a, b}`, not Smalltalk's `{a. b}`",
                    );
                }
                self.diagnostics.push(diagnostic);
                self.skip_to_tuple_close();
                break;
            }
        }

        let end_span = self.finish_literal(
            &TokenKind::RightBrace,
            "Expected '}' to close tuple literal",
            &mut slots,
            open_idx,
        );

        let span = start.merge(end_span);
        self.record_literal_comments(span, slots);
        Expression::TupleLiteral { elements, span }
    }

    /// The comments before the next element of the collection literal being
    /// parsed (BT-3983): those on their own lines before it, and, for the
    /// first element, an end-of-line comment after the opening delimiter
    /// (token `open_idx`), which prints above the element.
    fn collect_literal_item_leading(
        &self,
        slots: &LiteralCommentSlots,
        open_idx: usize,
    ) -> CommentAttachment {
        let mut comments = self.collect_comment_attachment();
        comments.trailing = None;
        if slots.items.is_empty()
            && let Some(after_open) = self.literal_open_trailing(open_idx)
        {
            comments.leading.insert(0, after_open);
        }
        comments
    }

    /// The end-of-line comment after the opening delimiter at `open_idx`, if
    /// that token has been consumed.
    fn literal_open_trailing(&self, open_idx: usize) -> Option<Comment> {
        if open_idx < self.current && open_idx < self.tokens.len() {
            self.collect_trailing_comment_at(open_idx)
        } else {
            None
        }
    }

    /// Records the end-of-line comment after the item whose last token is
    /// `item_end`, or after the comma that followed it, as the last slot's
    /// trailing comment (BT-3983). Call after the comma was consumed, or when
    /// the closing delimiter follows.
    fn collect_literal_item_trailing(&self, slots: &mut LiteralCommentSlots, item_end: usize) {
        let Some(slot) = slots.items.last_mut() else {
            return;
        };
        slot.trailing = self.collect_trailing_comment_at(item_end).or_else(|| {
            let last = self.current - 1;
            (last > item_end)
                .then(|| self.collect_trailing_comment_at(last))
                .flatten()
        });
    }

    /// Consumes the closing delimiter `close` of a collection literal,
    /// recording the comments before it (BT-3983), or reports `message` and
    /// leaves the current token. Returns the delimiter's span.
    fn finish_literal(
        &mut self,
        close: &TokenKind,
        message: &'static str,
        slots: &mut LiteralCommentSlots,
        open_idx: usize,
    ) -> Span {
        if self.check(close) {
            self.collect_literal_closing(slots, open_idx);
            self.advance().span()
        } else {
            self.diagnostics
                .push(Diagnostic::error(message, self.current_token().span()));
            self.current_token().span()
        }
    }

    /// Records the comments before the closing delimiter (the current token)
    /// of the literal being parsed (BT-3983). For an empty literal this
    /// includes an end-of-line comment after the opening delimiter.
    fn collect_literal_closing(&self, slots: &mut LiteralCommentSlots, open_idx: usize) {
        if slots.items.is_empty()
            && let Some(after_open) = self.literal_open_trailing(open_idx)
        {
            slots.closing.push(after_open);
        }
        slots
            .closing
            .extend(self.collect_comment_attachment().leading);
    }

    /// Files a finished literal's comments on the parser (BT-3983); the
    /// owning definition takes them. Literals without comments are skipped.
    fn record_literal_comments(&mut self, span: Span, slots: LiteralCommentSlots) {
        if !slots.is_empty() {
            self.literal_comments.push((span, slots));
        }
    }

    /// Whether the current token, at the tuple's own nesting level, belongs
    /// outside the tuple: an enclosing construct's `]`/`)`, or a token on a
    /// new line that starts the next method definition, or that no closing
    /// `}` follows (so the tuple was left unclosed). A newline inside a
    /// tuple that *is* closed later — a missing comma in a multi-line
    /// literal — is not a boundary, so recovery still reaches that `}`.
    fn at_tuple_recovery_boundary(&self) -> bool {
        if matches!(
            self.current_kind(),
            TokenKind::RightBracket | TokenKind::RightParen
        ) {
            return true;
        }
        self.current_token().has_leading_newline()
            && (self.is_at_method_definition() || !self.tuple_close_brace_ahead())
    }

    /// Whether a `}` closes the current nesting level before an enclosing
    /// `]`/`)` or `Eof` (error-recovery lookahead only).
    ///
    /// Inherent ambiguity: that `}` may belong to an *enclosing* `{`/`#{`
    /// rather than the unclosed tuple — in `#{#k => {1, 2\n #j => 3}` the
    /// tuple claims the map's `}`, and the map then reports its own missing
    /// `}`. Only diagnostic quality is affected, never valid code.
    fn tuple_close_brace_ahead(&self) -> bool {
        let mut depth: usize = 0;
        let mut offset = 0;
        loop {
            match self.peek_at(offset) {
                None | Some(TokenKind::Eof) => return false,
                Some(kind) if kind.is_opening_bracket() => depth += 1,
                Some(kind) if kind.is_closing_bracket() => {
                    if depth == 0 {
                        return matches!(kind, TokenKind::RightBrace);
                    }
                    depth -= 1;
                }
                Some(_) => {}
            }
            offset += 1;
        }
    }

    /// Error recovery inside a tuple literal: skip tokens up to (not
    /// including) this tuple's closing `}`. At the tuple's own nesting level
    /// it also stops at a `.` and at [`Self::at_tuple_recovery_boundary`], so an unclosed
    /// `{` never swallows later statements, sibling methods, or an enclosing
    /// block's `]`. Nested brackets, parens and braces are skipped whole, so
    /// a `.` inside a nested block (`{1 2, [:x | x foo. x bar]}`) does not
    /// stop recovery.
    fn skip_to_tuple_close(&mut self) {
        let mut depth: usize = 0;
        while !self.is_at_end() {
            if depth == 0
                && (self.check(&TokenKind::RightBrace)
                    || self.check(&TokenKind::Period)
                    || self.at_tuple_recovery_boundary())
            {
                return;
            }
            let kind = self.current_kind();
            if kind.is_opening_bracket() {
                depth += 1;
            } else if kind.is_closing_bracket() {
                depth = depth.saturating_sub(1);
            }
            self.advance();
        }
    }
}
