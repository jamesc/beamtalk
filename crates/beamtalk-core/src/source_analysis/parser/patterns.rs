// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Pattern parsing for Beamtalk (match arms, destructuring).
//!
//! **DDD Context:** Compilation
//!
//! Parses the pattern grammar: wildcard, `nil`, variable and type patterns,
//! literals, tuple / array / map / constructor patterns and rest patterns.
//! Binary patterns (`<<...>>`) live in [`super::binary`].

use crate::ast::{Expression, Identifier, Literal, MapPatternKey, MapPatternPair, Pattern};
use crate::source_analysis::{Span, Token, TokenKind};

use super::{Diagnostic, Parser};

impl Parser {
    /// Parses a pattern for match arms.
    ///
    /// Supported patterns:
    /// - `_` — wildcard
    /// - `nil` — nil pattern (ADR 0107 Phase A)
    /// - identifier — variable binding
    /// - `identifier :: ClassName` — type pattern (ADR 0107 Phase A)
    /// - integer, float, string, symbol, character — literal
    /// - `{p1, p2, ...}` — tuple
    ///
    /// Bare `true`/`false` are rejected with a diagnostic rather
    /// than parsed as a variable binding — use `x :: True`/`x :: False`/
    /// `x :: Boolean` instead.
    #[allow(clippy::too_many_lines)] // one arm per pattern kind
    pub(super) fn parse_pattern(&mut self) -> Pattern {
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

            // Nil pattern: `nil` (ADR 0107 Phase A) — a reserved identifier,
            // recognized before the generic variable-binding case below.
            //
            // `nil` never carries a `:: ClassName` type annotation — `nil`
            // already fully determines the match, so an errant `nil ::
            // String` is rejected with a single targeted diagnostic
            // rather than the generic pattern-parse-error cascade (an
            // unconsumed `::`/class-name would otherwise surface as a
            // confusing second "Expected '->' after pattern" error).
            TokenKind::Identifier(name) if name.as_str() == "nil" => {
                let nil_span = self.advance().span();
                if matches!(self.current_kind(), TokenKind::DoubleColon) {
                    let double_colon_span = self.advance().span(); // consume '::'
                    let tail_span = self.consume_type_pattern_tail();
                    let full_span = nil_span.merge(tail_span.unwrap_or(double_colon_span));
                    self.diagnostics.push(Diagnostic::error(
                        "a 'nil' pattern cannot have a type annotation ('nil :: ClassName' is not allowed; 'nil' already fully determines the match)",
                        full_span,
                    ));
                    Pattern::Wildcard(full_span)
                } else {
                    Pattern::Nil(nil_span)
                }
            }

            // Bare `true`/`false` pattern: unlike `nil`, `true` and
            // `false` are *not* reserved pattern keywords — falling through
            // to the generic "Variable binding" branch below would silently
            // parse them as `Pattern::Variable(Identifier{name: "true"})` /
            // `Pattern::Variable(Identifier{name: "false"})`, an
            // unconditional catch-all binding that matches any value, not a
            // boolean-literal test. Since arms are tried in source order, a
            // `true ->` arm listed first would always win regardless of the
            // scrutinee — a silent-wrong-behavior footgun with no
            // diagnostic. Rejected here with a single targeted diagnostic
            // (mirroring `nil`'s `nil :: ClassName` diagnostic above)
            // pointing at the existing, already-working idiom for boolean
            // literal/type tests: `x :: True` / `x :: False` (exact literal)
            // or `x :: Boolean` (either), both `Pattern::Type`
            // (ADR 0107 Phase A, `generate_type_pattern`'s
            // `wrap_single_atom_test` / the `is_boolean` BIF test).
            //
            // A trailing `:: ClassName` (e.g. a plausible `true :: Boolean`
            // typo before discovering the `b :: Boolean` idiom) is consumed
            // here too, exactly like the `nil` branch above — otherwise the
            // unconsumed `::`/class-name would surface as a second, spurious
            // "Expected '->' after pattern" diagnostic.
            TokenKind::Identifier(name) if name.as_str() == "true" || name.as_str() == "false" => {
                let name = name.clone();
                let mut bad_span = self.advance().span();
                if matches!(self.current_kind(), TokenKind::DoubleColon) {
                    let double_colon_span = self.advance().span(); // consume '::'
                    let tail_span = self.consume_type_pattern_tail();
                    bad_span = bad_span.merge(tail_span.unwrap_or(double_colon_span));
                }
                self.diagnostics.push(Diagnostic::error(
                    format!(
                        "bare '{name}' in a match: pattern always matches — it binds any value to a variable named '{name}', it does not test for the boolean literal '{name}' (use 'x :: True' / 'x :: False' for an exact literal test, or 'x :: Boolean' to match either)"
                    ),
                    bad_span,
                ));
                Pattern::Wildcard(bad_span)
            }

            // Variable binding, or a type pattern if followed by `:: ClassName`
            // (ADR 0107 Phase A: `binding :: ClassName`, reusing the `::`
            // tokenization from ADR 0053's parameter-annotation parsing).
            TokenKind::Identifier(name) => {
                let name = name.clone();
                let token = self.advance();
                let span = token.span();
                let binding = Identifier::new(name, span);

                if matches!(self.current_kind(), TokenKind::DoubleColon) {
                    self.advance(); // consume '::'
                    if let TokenKind::Identifier(class_name) = self.current_kind() {
                        let class_name = class_name.clone();
                        let class_span = self.advance().span();

                        // Generic type arguments in a type pattern, e.g.
                        // `items :: List(Printable)`, are rejected with a
                        // single explicit diagnostic (ADR 0107
                        // Decision, 2026-07-13) rather than silently checking
                        // only the base class. `Pattern::Type` intentionally
                        // scopes `class` to a bare identifier, and
                        // per ADR 0068 type erasure there is no reified
                        // generic tag to verify at runtime regardless — so
                        // silently discarding `(Printable)` would be the one
                        // place in the language where a generic-looking
                        // annotation silently means less than it appears to.
                        if matches!(self.current_kind(), TokenKind::LeftParen) {
                            let args_span = self.skip_parenthesized_for_recovery();
                            let full_span = span.merge(args_span);
                            self.diagnostics.push(Diagnostic::error(
                                "generic type arguments are not supported in a type pattern yet",
                                full_span,
                            ));
                            Pattern::Wildcard(full_span)
                        } else {
                            let full_span = span.merge(class_span);
                            Pattern::Type {
                                binding,
                                class: Identifier::new(class_name, class_span),
                                span: full_span,
                            }
                        }
                    } else {
                        let bad_span = self.current_token().span();
                        self.diagnostics.push(Diagnostic::error(
                            "Expected a class name after '::' in a type pattern",
                            bad_span,
                        ));
                        // Consume the offending token so this single
                        // diagnostic doesn't cascade into a second "Expected
                        // '->' after pattern in match arm" error —
                        // but only when it plausibly stands alone as a
                        // mistyped type name (a literal, e.g. `x :: 5`).
                        // A type pattern can nest inside a tuple/array/map
                        // pattern (`{x :: 5, y}`), so anything else — a `,`
                        // element separator, a closing delimiter, `when:`,
                        // the arrow itself, ... — is deliberately left alone
                        // for the *enclosing* pattern parser to consume
                        // correctly, even at the cost of a second cascaded
                        // diagnostic.
                        if Self::looks_like_malformed_type_pattern_token(self.current_kind()) {
                            self.advance();
                        }
                        Pattern::Wildcard(span.merge(bad_span))
                    }
                } else {
                    Pattern::Variable(binding)
                }
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
                            // `parse_literal` already rejects non-finite floats, so
                            // negating `f` here cannot produce `inf`/`NaN`.
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

    /// Consumes the malformed token(s) following an errant `::` in a `nil`
    /// pattern or a bare `true`/`false` pattern, for
    /// error-recovery purposes only — the caller is responsible for
    /// emitting a single diagnostic covering the whole annotation. Mirrors
    /// the shape of a real type-pattern annotation
    /// (`Identifier` optionally followed by `(generic, args)`) so a class
    /// name with generics is consumed as one unit; a plausible mistyped
    /// literal type name (`nil :: 5`) is also consumed. Anything else is
    /// left alone — see [`Self::looks_like_malformed_type_pattern_token`]
    /// for why. Returns the span of the consumed tail, or `None` if nothing
    /// could be consumed.
    fn consume_type_pattern_tail(&mut self) -> Option<Span> {
        if matches!(self.current_kind(), TokenKind::Identifier(_)) {
            let mut span = self.advance().span();
            if matches!(self.current_kind(), TokenKind::LeftParen) {
                span = span.merge(self.skip_parenthesized_for_recovery());
            }
            Some(span)
        } else if Self::looks_like_malformed_type_pattern_token(self.current_kind()) {
            Some(self.advance().span())
        } else {
            None
        }
    }

    /// Skips a parenthesized group (e.g. `(Printable)`, `(A, B(C))`) for
    /// error recovery, honoring nesting. Assumes the current token is
    /// `LeftParen`. Returns the span covering the whole group.
    ///
    /// If the group is unterminated, stops at `Eof` *or* at the first
    /// `;`/`]` — a real generic-arguments group can never legitimately
    /// contain either (type annotations have no statement separators or
    /// array/block syntax), so seeing one while parens are still unbalanced
    /// means the `)` was simply omitted. Without this, a single missing `)`
    /// (e.g. `x :: List(Foo -> 1; b -> 2; _ -> 3]`) would otherwise consume
    /// every remaining match arm up to `Eof` as "part of the generic args",
    /// hiding unrelated errors in them behind one misleading span.
    fn skip_parenthesized_for_recovery(&mut self) -> Span {
        let mut depth: u32 = 0;
        let mut span = self.current_token().span();
        loop {
            match self.current_kind() {
                TokenKind::LeftParen => {
                    depth += 1;
                    span = span.merge(self.advance().span());
                }
                TokenKind::RightParen => {
                    span = span.merge(self.advance().span());
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                TokenKind::Eof | TokenKind::Semicolon | TokenKind::RightBracket => break,
                _ => {
                    span = span.merge(self.advance().span());
                }
            }
        }
        span
    }

    /// Returns `true` for tokens that plausibly represent a mistyped type
    /// name in a malformed type-pattern annotation (e.g. the `5` in `x :: 5`
    /// or `nil :: 5`) — a deliberately narrow whitelist of literal-ish
    /// tokens, safe to consume as part of a single error-recovery
    /// diagnostic because none of them can legitimately open the
    /// *next* construct in any pattern context. Anything not in this list
    /// (a `,` element separator in an enclosing tuple/array/map pattern, a
    /// closing delimiter, `when:`, the arrow itself, ...) is deliberately
    /// left uncontested so the enclosing pattern parser can still recover
    /// correctly, even at the cost of a second cascaded diagnostic — a
    /// blacklist of "boundary" tokens was tried first and consumed a `,`
    /// separator in `{x :: 5, y}`-shaped input, corrupting the rest of the
    /// tuple pattern into a much longer cascade.
    fn looks_like_malformed_type_pattern_token(kind: &TokenKind) -> bool {
        matches!(
            kind,
            TokenKind::Integer(_)
                | TokenKind::Float(_)
                | TokenKind::String(_)
                | TokenKind::Character(_)
                | TokenKind::Symbol(_)
        )
    }

    /// Parses a tuple pattern: `{p1, p2, ...}`
    ///
    /// Note: each element goes through [`Self::parse_pattern`],
    /// whose type-pattern (`x :: ClassName`) error recovery only ever
    /// consumes a token that could not open a `,`-separated element or a
    /// `}` — see [`Self::looks_like_malformed_type_pattern_token`]. If a
    /// future pattern kind reuses `,`/`}` (or any other token in that
    /// whitelist) as its own structural syntax, that invariant would need
    /// rechecking.
    pub(super) fn parse_tuple_pattern(&mut self) -> Pattern {
        let start = self
            .expect(&TokenKind::LeftBrace, "Expected '{'")
            .map_or_else(|| self.current_token().span(), |t: Token| t.span());

        let mut elements = Vec::new();

        if !self.check(&TokenKind::RightBrace) {
            elements.push(self.parse_pattern());
            while self.match_binary_selector(",") {
                if self.check(&TokenKind::RightBrace) {
                    break; // trailing comma (ADR 0136)
                }
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
    ///
    /// Note: see the same note on [`Self::parse_tuple_pattern`] —
    /// nested type-pattern error recovery relies on `,`/`]` never being
    /// consumed as a "malformed type name" token.
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
                            // `parse_literal` already rejects non-finite floats, so
                            // negating `f` here cannot produce `inf`/`NaN`.
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
    /// Bare lowercase identifiers are a compile error; use `#key` instead.
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
    /// Accepts: wildcard `_`, variable identifier, `nil` (the `Pattern::Nil`
    /// literal), or a literal (integer, float, string, symbol, or
    /// negative number like `-1`). Bare `true`/`false` are rejected with a
    /// diagnostic rather than parsed as a variable binding — see
    /// the dedicated match arm below for why. A trailing `:: ClassName` on
    /// `nil`, `true`/`false`, or a plain variable binding is consumed and
    /// rejected with a single diagnostic rather than left to cascade.
    fn parse_constructor_binding(&mut self) -> Pattern {
        match self.current_kind() {
            TokenKind::Identifier(name) if name.as_str() == "_" => {
                Pattern::Wildcard(self.advance().span())
            }
            // `nil` binding: mirrors `parse_pattern`'s top-level
            // `nil` handling — `nil` is a reserved identifier, not a plain
            // variable name, so `Result ok: nil` must test the wrapped value
            // *is* nil (`Pattern::Nil`), not bind a variable named `nil`
            // that matches unconditionally.
            //
            // A trailing `:: ClassName` (e.g. `nil :: SomeClass`) is
            // consumed and rejected here too — type patterns aren't supported nested
            // inside a constructor pattern at all (see `generate_pattern`'s
            // `Pattern::Type` codegen restriction), but leaving `::
            // ClassName` unconsumed would desync the constructor-pattern
            // keyword loop and cascade into a second "Expected '->' after
            // pattern" diagnostic, exactly like the `true`/`false` and
            // generic-identifier arms below.
            TokenKind::Identifier(name) if name.as_str() == "nil" => {
                let nil_span = self.advance().span();
                if matches!(self.current_kind(), TokenKind::DoubleColon) {
                    let double_colon_span = self.advance().span(); // consume '::'
                    let tail_span = self.consume_type_pattern_tail();
                    let full_span = nil_span.merge(tail_span.unwrap_or(double_colon_span));
                    self.diagnostics.push(Diagnostic::error(
                        "a 'nil' pattern cannot have a type annotation in a constructor binding position ('nil :: ClassName' is not allowed here)",
                        full_span,
                    ));
                    Pattern::Wildcard(full_span)
                } else {
                    Pattern::Nil(nil_span)
                }
            }
            // Bare `true`/`false` binding: falling through to the generic variable-binding
            // arm below would silently parse `Result ok: true` as
            // `Pattern::Variable(Identifier{name: "true"})` — an
            // unconditional catch-all that matches *any* wrapped value, not
            // a boolean-literal test. Unlike the top-level fix, the
            // diagnostic here can't point at `x :: True`/`x :: False`
            // (`Pattern::Type` has no codegen when nested inside a
            // constructor pattern — see `generate_pattern`), so it points at
            // the guard-clause idiom instead, which is already supported
            // (`Result ok: v when: [v =:= true]`) — spelled with the
            // explicit `=:=` (strict equality, per
            // `docs/beamtalk-language-features.md`'s Equality section)
            // rather than a bare `v`/`v not`, since a bare guard variable
            // reads as a truthiness check to anyone unfamiliar with Erlang's
            // guard semantics (a bare guard variable *does* require the
            // exact atom `true` — no other value passes — but `=:=` says so
            // without relying on that knowledge).
            //
            // A trailing `:: ClassName` (e.g. `true :: Boolean`) is consumed
            // here too — otherwise the unconsumed `::`/class-name
            // would surface as a second, spurious "Expected '->' after
            // pattern" diagnostic, exactly the cascade the generic-identifier
            // arm below was fixed to avoid.
            TokenKind::Identifier(name) if name.as_str() == "true" || name.as_str() == "false" => {
                let name = name.clone();
                let mut bad_span = self.advance().span();
                if matches!(self.current_kind(), TokenKind::DoubleColon) {
                    let double_colon_span = self.advance().span(); // consume '::'
                    let tail_span = self.consume_type_pattern_tail();
                    bad_span = bad_span.merge(tail_span.unwrap_or(double_colon_span));
                }
                let guard_example = if name == "true" {
                    "v =:= true"
                } else {
                    "v =:= false"
                };
                self.diagnostics.push(Diagnostic::error(
                    format!(
                        "bare '{name}' in a constructor pattern binding always matches — it binds any value to a variable named '{name}', it does not test for the boolean literal '{name}' (bind a variable and test it in a guard clause instead, e.g. 'Result ok: v when: [{guard_example}]')"
                    ),
                    bad_span,
                ));
                Pattern::Wildcard(bad_span)
            }
            // Variable binding: a trailing `:: ClassName` is not
            // supported in constructor-binding position (`Pattern::Type`
            // has no codegen when nested inside a constructor pattern — see
            // `generate_pattern`), but leaving it unconsumed would desync
            // the constructor-pattern keyword loop (`::` isn't a `Keyword`
            // token) and cascade into a generic "Expected '->' after
            // pattern" error. Consumed and rejected here with a single
            // targeted diagnostic instead, mirroring `parse_pattern`'s
            // `nil ::`/`true ::`/`false ::` consume-and-reject handling.
            TokenKind::Identifier(_) => {
                let token = self.advance();
                let span = token.span();
                let TokenKind::Identifier(name) = token.into_kind() else {
                    unreachable!()
                };
                if matches!(self.current_kind(), TokenKind::DoubleColon) {
                    let double_colon_span = self.advance().span(); // consume '::'
                    let tail_span = self.consume_type_pattern_tail();
                    let full_span = span.merge(tail_span.unwrap_or(double_colon_span));
                    self.diagnostics.push(Diagnostic::error(
                        "type annotations are not supported in a constructor pattern binding position ('x :: ClassName' is not allowed here; bind the variable and test its type in a guard clause instead, e.g. 'Result ok: v when: [v :: ClassName]')",
                        full_span,
                    ));
                    Pattern::Wildcard(full_span)
                } else {
                    Pattern::Variable(Identifier::new(name, span))
                }
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
}
