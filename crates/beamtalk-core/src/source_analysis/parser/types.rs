// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type-annotation parsing and the `skip_*` type lookahead (split from `declarations.rs`).

use crate::ast::{Identifier, ShapeField, TypeAnnotation};
use crate::source_analysis::{Span, Token, TokenKind};

use super::declarations::is_comma;
use super::{Diagnostic, Parser};

/// Tri-state result for `skip_double_colon_type` lookahead.
///
/// Distinguishes "no `::` present" from "malformed `::` annotation" so
/// callers can treat malformed typed selectors as method-start candidates
/// (allowing parse-time errors) rather than silently falling back to the
/// "untyped parameter" interpretation.
pub(super) enum DoubleColonSkip {
    /// No `::` token at the tested offset.
    NotPresent,
    /// Valid `:: Type (| Type)*` annotation; contains the offset after the annotation.
    Valid(usize),
    /// `::` was present but the type name (or union element) was missing or invalid.
    /// Contains the offset just after `::` for error-recovery lookahead.
    Malformed(usize),
}

/// Returns `true` if the optional token kind is a comma binary selector.
fn is_comma_opt(kind: Option<&TokenKind>) -> bool {
    kind.is_some_and(is_comma)
}

/// Returns `true` if the optional token kind can begin a type name in
/// lookahead — an identifier (`Integer`, `Self`) or a singleton symbol
/// (`#foo`, lexed as [`TokenKind::Symbol`]). Singleton type
/// annotations are subtypes of `Symbol` (ADR 0068).
fn is_type_name_token(kind: Option<&TokenKind>) -> bool {
    matches!(kind, Some(TokenKind::Identifier(_) | TokenKind::Symbol(_)))
}

impl Parser {
    /// Advances past a `:: Type (| Type)*` annotation in lookahead context.
    ///
    /// Returns [`DoubleColonSkip`]:
    /// - `NotPresent` — no `::` at `offset`; caller should treat param as untyped.
    /// - `Valid(o)` — full annotation consumed; `o` is the offset after the annotation.
    /// - `Malformed(o)` — `::` was present but followed by an invalid token; `o` is
    ///   the offset just past `::`. Callers should use `o` for further lookahead so the
    ///   method is still detected and parse-time errors can be emitted.
    pub(super) fn skip_double_colon_type(&self, offset: usize) -> DoubleColonSkip {
        if !matches!(self.peek_at(offset), Some(TokenKind::DoubleColon)) {
            return DoubleColonSkip::NotPresent;
        }
        let mut o = offset + 1;
        let Some(after) = self.skip_type_operand(o) else {
            return DoubleColonSkip::Malformed(o);
        };
        o = after;
        // Skip the type-operator chain: `| Type`, `\ Type`, `& Type`
        while self.is_type_chain_operator(o) {
            o += 1;
            let Some(after) = self.skip_type_operand(o) else {
                return DoubleColonSkip::Malformed(o);
            };
            o = after;
        }
        DoubleColonSkip::Valid(o)
    }

    /// Skips a single type operand in lookahead context: a type name (with
    /// optional `class` metatype suffix and generic parameter list) or a
    /// parenthesised type group `( ... )`.
    ///
    /// Returns the offset after the operand, or `None` if no type operand
    /// starts at `offset`. A malformed generic parameter list after a valid
    /// type name is tolerated (the offset stops at the `(`) so the parser can
    /// emit the specific diagnostic — matching the pre-existing lookahead
    /// behaviour.
    fn skip_type_operand(&self, offset: usize) -> Option<usize> {
        if matches!(self.peek_at(offset), Some(TokenKind::LeftParen)) {
            return self.skip_grouped_type(offset);
        }
        if matches!(self.peek_at(offset), Some(TokenKind::MapOpen)) {
            return self.skip_shape_type(offset);
        }
        if !is_type_name_token(self.peek_at(offset)) {
            return None;
        }
        let mut o = self.skip_type_name_with_metatype(offset);
        // Skip generic type parameters: `Name(Type, Type, ...)`
        if matches!(self.peek_at(o), Some(TokenKind::LeftParen))
            && let Some(after) = self.skip_paren_type_params(o)
        {
            o = after;
        }
        Some(o)
    }

    /// Skips a map-shape type `#{ ... }` (ADR 0147) in lookahead context.
    ///
    /// Starting at the `#{` token, advances past the matching `}` by brace
    /// depth (a shape's contents contain `=>`, so the generic type-chain
    /// skippers cannot be reused) and returns the offset after it. Returns
    /// `None` if the closing `}` is missing.
    fn skip_shape_type(&self, offset: usize) -> Option<usize> {
        debug_assert!(matches!(self.peek_at(offset), Some(TokenKind::MapOpen)));
        // `braces` counts `{` / `#{` openers; `nested` counts other bracket
        // openers so an unmatched closer (which belongs to an enclosing
        // construct) ends the scan instead of being walked past.
        let mut braces = 0usize;
        let mut nested = 0usize;
        let mut o = offset;
        loop {
            match self.peek_at(o)? {
                TokenKind::MapOpen | TokenKind::LeftBrace => braces += 1,
                TokenKind::LeftParen
                | TokenKind::LeftBracket
                | TokenKind::ListOpen
                | TokenKind::ArrayOpen => nested += 1,
                TokenKind::RightParen | TokenKind::RightBracket => {
                    nested = nested.checked_sub(1)?;
                }
                TokenKind::RightBrace => {
                    braces -= 1;
                    if braces == 0 {
                        return Some(o + 1);
                    }
                }
                TokenKind::Eof => return None,
                // A line starting a method definition (unary, binary or
                // keyword: the same recogniser `recover_shape_field` uses) is
                // never part of a shape: the shape is unclosed.
                // The `braces == 1 && nested == 0` guard (top level of this
                // shape) is load-bearing: `is_method_definition_at` can call
                // back into `skip_shape_type` through a `-> #{…}` return
                // type, so widening the guard to nested positions would make
                // a run of unclosed headers exponential instead of quadratic.
                // It does mean a method start inside a nested unclosed shape
                // is not a stop here, though recovery stops at any depth.
                TokenKind::Identifier(_) | TokenKind::BinarySelector(_) | TokenKind::Keyword(_)
                    if o > offset
                        && braces == 1
                        && nested == 0
                        && self
                            .peek_token_at(o)
                            .is_some_and(Token::has_leading_newline)
                        && self.is_method_definition_at(o) =>
                {
                    return None;
                }
                _ => {}
            }
            o += 1;
        }
    }

    /// Skips a parenthesised type group `( Type (op Type)* )` in lookahead
    /// context, keeping lookahead in lock-step with
    /// `parse_single_type_annotation`'s grouping-paren branch.
    ///
    /// Starting at the `(` token, advances past the matching `)` and returns
    /// the offset after it. Returns `None` if the contents are not a type
    /// chain or the closing `)` is missing.
    fn skip_grouped_type(&self, offset: usize) -> Option<usize> {
        debug_assert!(matches!(self.peek_at(offset), Some(TokenKind::LeftParen)));
        let mut o = offset + 1; // past `(`
        o = self.skip_type_operand(o)?;
        while self.is_type_chain_operator(o) {
            o += 1;
            o = self.skip_type_operand(o)?;
        }
        if matches!(self.peek_at(o), Some(TokenKind::RightParen)) {
            Some(o + 1)
        } else {
            None
        }
    }

    /// Skips a parenthesized type parameter list in lookahead context.
    ///
    /// Starting at the `(` token, advances past `(Type, Type, ...)` and returns
    /// the offset after the closing `)`. Returns `None` if the sequence is
    /// malformed (e.g., missing closing paren or non-identifier content).
    ///
    /// Also handles bounded type parameters: `(T :: Printable, E)` — skips the
    /// `:: Bound` portion when present (ADR 0068 Phase 2d). Also handles `class`
    /// metatype suffixes in type argument position: `List(Actor class)`.
    pub(super) fn skip_paren_type_params(&self, offset: usize) -> Option<usize> {
        debug_assert!(matches!(self.peek_at(offset), Some(TokenKind::LeftParen)));
        let mut o = offset + 1; // past `(`
        // Handle empty parens `()`
        if matches!(self.peek_at(o), Some(TokenKind::RightParen)) {
            return Some(o + 1);
        }
        // First type param
        o = self.skip_type_param(o)?;
        // Additional type params: `, Type`
        while is_comma_opt(self.peek_at(o)) {
            o += 1;
            o = self.skip_type_param(o)?;
        }
        if matches!(self.peek_at(o), Some(TokenKind::RightParen)) {
            Some(o + 1)
        } else {
            None
        }
    }

    /// Skips one type parameter/argument in lookahead context: a type
    /// operand (name with optional metatype/generics, or a parenthesised
    /// group — see [`skip_type_operand`](Self::skip_type_operand)), an
    /// optional `:: Bound` (ADR 0068 Phase 2d), a nested generic parameter
    /// list after the bound, and any trailing `|`/`\`/`&` operator chain.
    ///
    /// Returns the offset after the parameter, or `None` if malformed.
    fn skip_type_param(&self, offset: usize) -> Option<usize> {
        let mut o = self.skip_type_operand(offset)?;
        // Skip optional bound: `:: Protocol`
        o = self.skip_optional_type_param_bound(o);
        // Nested generic after a bound: `T :: Printable(X)`
        if matches!(self.peek_at(o), Some(TokenKind::LeftParen)) {
            o = self.skip_paren_type_params(o)?;
        }
        // Operator chain in type param position: `Type | Type`, `Type \ #a`
        while self.is_type_chain_operator(o) {
            o += 1;
            o = self.skip_type_operand(o)?;
        }
        Some(o)
    }

    /// Skips an optional `:: Identifier` bound in lookahead context.
    ///
    /// If the token at `offset` is `::` (`TypeAnnotation`) followed by an identifier,
    /// returns the offset past both. Otherwise returns `offset` unchanged.
    ///
    /// Only an identifier bound is accepted — a type parameter bound is a
    /// protocol (e.g. `T :: Printable`), so a singleton `#foo` is intentionally
    /// not a valid bound.
    ///
    /// **References:** ADR 0068 Phase 2d — type parameter bounds in lookahead
    fn skip_optional_type_param_bound(&self, offset: usize) -> usize {
        if matches!(self.peek_at(offset), Some(TokenKind::DoubleColon))
            && matches!(self.peek_at(offset + 1), Some(TokenKind::Identifier(_)))
        {
            offset + 2
        } else {
            offset
        }
    }

    /// Checks if the token at `offset` starts a `=>` or `-> Type =>` pattern.
    ///
    /// Used to detect `class` as a method name (not a modifier) at three sites:
    /// `is_class_method` in `parse_class_body`, `is_at_method_definition`, and
    /// `parse_method_definition`.
    pub(super) fn is_fat_arrow_or_return_type(&self, offset: usize) -> bool {
        matches!(self.peek_at(offset), Some(TokenKind::FatArrow))
            || self.is_return_type_then_fat_arrow(offset)
            // ADR 0066 Phase 4: `:: -> Type =>` extension type annotation syntax
            || self.is_double_colon_return_type_then_fat_arrow(offset)
    }

    /// Checks if there's a `:: -> Type =>` pattern at the given offset.
    ///
    /// This is the extension-style type annotation syntax (ADR 0066 Phase 4),
    /// used primarily on unary extension methods where there's no parameter to
    /// carry a `::` annotation: `Integer >> factorial :: -> Integer =>`.
    fn is_double_colon_return_type_then_fat_arrow(&self, offset: usize) -> bool {
        if !matches!(self.peek_at(offset), Some(TokenKind::DoubleColon)) {
            return false;
        }
        // After `::`, delegate to the standard `-> Type =>` check
        self.is_return_type_then_fat_arrow(offset + 1)
    }

    /// Checks if there's a `-> Type =>` or `-> Type | Type =>` pattern at the given offset.
    ///
    /// Also accepts `-> =>` (missing type) for error recovery — lets `parse_type_annotation`
    /// emit the specific error rather than failing to detect the method definition.
    pub(super) fn is_return_type_then_fat_arrow(&self, offset: usize) -> bool {
        if !matches!(self.peek_at(offset), Some(TokenKind::Arrow)) {
            return false;
        }
        // Skip -> Type (and possible | Type unions, generic params, groups)
        let mut o = offset + 1;
        // Allow `-> =>` (missing type) for error recovery
        if matches!(self.peek_at(o), Some(TokenKind::FatArrow)) {
            return true;
        }
        // Must have at least one type operand: a type name (including a
        // trailing `class` metatype token, so `-> Self
        // class =>` and `-> Actor class | Nil =>` are recognized) or a
        // parenthesised group (`-> (A & B) \ #c =>`).
        let Some(after) = self.skip_type_operand(o) else {
            return false;
        };
        o = after;
        // Skip the type-operator chain: `| Type`, `\ Type`, `& Type`
        while self.is_type_chain_operator(o) {
            o += 1;
            let Some(after) = self.skip_type_operand(o) else {
                return false;
            };
            o = after;
        }
        matches!(self.peek_at(o), Some(TokenKind::FatArrow))
    }

    /// Advance past a type-name token and an optional `class` metatype suffix.
    ///
    /// Given offset `o` pointing at an identifier (e.g. `Self`, `Actor`),
    /// returns the offset after it, plus a trailing `class` if the next token
    /// is the bare identifier `class` on the *same* line.
    /// A `class` token with a leading newline begins a new statement (e.g.
    /// the class-method definition on the following line) and is not consumed.
    ///
    /// Also called with a singleton `#foo` ([`TokenKind::Symbol`]) at
    /// `o`. A singleton is a single token with no metatype surface (`#foo class`
    /// is not a valid type), so it advances exactly one token — keeping this
    /// lookahead in lock-step with `parse_single_type_annotation`, which returns
    /// the `Singleton` immediately after the symbol.
    fn skip_type_name_with_metatype(&self, o: usize) -> usize {
        if matches!(self.peek_at(o), Some(TokenKind::Symbol(_))) {
            return o + 1;
        }
        if matches!(self.peek_at(o + 1), Some(TokenKind::Identifier(name)) if name == "class")
            && self
                .peek_token_at(o + 1)
                .is_some_and(|t| !t.has_leading_newline())
        {
            o + 2
        } else {
            o + 1
        }
    }

    /// Returns `true` if the token at `offset` continues a type annotation with
    /// a binary type operator: union `|`, difference `\`, intersection `&`, or
    /// the `\\` typo for `\` (ADR 0102 §3).
    ///
    /// Used by the lookahead helpers to skip over a type-operator chain when
    /// deciding whether a `-> Type =>` / `:: Type` sequence is a method header.
    /// The precedence distinction between `\`, `&`, and `|` is irrelevant here —
    /// these helpers only confirm the chain terminates at a `=>`/`)`; the parser
    /// enforces precedence in [`parse_difference_type`](Self::parse_difference_type).
    /// The `\\` typo is treated as a chain operator so the method is still
    /// recognised and the parser can emit the targeted "did you mean `\`?" error.
    pub(super) fn is_type_chain_operator(&self, offset: usize) -> bool {
        match self.peek_at(offset) {
            Some(TokenKind::Pipe) => true,
            Some(TokenKind::BinarySelector(s)) => {
                s.as_str() == "\\" || s.as_str() == "\\\\" || s.as_str() == "&"
            }
            _ => false,
        }
    }
    /// Parses a type annotation, including union types (`Integer | String`),
    /// difference types (`Symbol \ #foo`), and intersection types
    /// (`Collection(Object) & Comparable`).
    ///
    /// Precedence (ADR 0102 §3): `|` (union) is the loosest tier; `\`
    /// (difference) and `&` (intersection) bind tighter and share a tier, so
    /// `Integer | Symbol \ #foo` parses as `Integer | (Symbol \ #foo)`. The
    /// `\`/`&` tier is handled by
    /// [`parse_difference_type`](Self::parse_difference_type).
    pub(super) fn parse_type_annotation(&mut self) -> TypeAnnotation {
        // Unions are n-ary and associative, so a grouped union member —
        // `(A | B) | C`, only reachable via grouping parens — is
        // spliced into the enclosing union rather than nested. This keeps
        // the AST canonical: `(A | B) | C` and `A | B | C` are the same
        // annotation, and unparsing (`type_name`) round-trips.
        fn push_union_member(types: &mut Vec<TypeAnnotation>, ty: TypeAnnotation) {
            if let TypeAnnotation::Union { types: inner, .. } = ty {
                types.extend(inner);
            } else {
                types.push(ty);
            }
        }

        let first = self.parse_difference_type();

        // Check for union type: `Type | Type | ...`
        if !matches!(self.current_kind(), TokenKind::Pipe) {
            return first;
        }

        let start_span = first.span();
        let mut types = Vec::new();
        push_union_member(&mut types, first);

        while matches!(self.current_kind(), TokenKind::Pipe) {
            self.advance(); // consume `|`
            push_union_member(&mut types, self.parse_difference_type());
        }

        let end_span = types.last().map_or(start_span, TypeAnnotation::span);
        TypeAnnotation::Union {
            types,
            span: start_span.merge(end_span),
        }
    }

    /// Parses a difference/intersection type (`Type \ Type`, `Type & Type`),
    /// the shared precedence tier between unions (`|`) and single type atoms
    /// (ADR 0102 §3).
    ///
    /// `\` and `&` bind tighter than `|` and are each left-associative, so
    /// `A \ B \ C` parses as `(A \ B) \ C` and `A & B & C` parses as
    /// `(A & B) & C`. Per ADR 0102 §3, **mixing `&` and `\` in the same chain
    /// without explicit grouping is a deliberate parse error** — not a
    /// left-associative resolution, since `(A & B) \ C` and `A & (B \ C)`
    /// differ and neither reading is obviously dominant. Once the chain
    /// commits to one operator (whichever appears first), encountering the
    /// other emits a diagnostic; recovery keeps building the AST using the
    /// originally-committed operator so a single mismatch doesn't cascade
    /// into unrelated downstream errors.
    ///
    /// The `\`/`&` operators lex as ordinary `BinarySelector`s; they are
    /// special-cased *only* in type-annotation position — in a value
    /// expression they remain ordinary binary selectors, untouched. The
    /// modulo selector `\\` (two backslashes, which the lexer greedily
    /// merges) is almost certainly a typo for `\` here and gets a targeted
    /// diagnostic.
    fn parse_difference_type(&mut self) -> TypeAnnotation {
        let mut ty = self.parse_single_type_annotation();
        // Which operator this chain has committed to (`true` = `&`, `false` =
        // `\`) — `None` until the first `&`/`\` is seen. Used to reject a
        // same-tier operator switch without explicit grouping (ADR 0102 §3).
        let mut chain_is_and: Option<bool> = None;

        while let TokenKind::BinarySelector(op) = self.current_kind() {
            match op.as_str() {
                // `\` — the difference operator, `&` — the intersection
                // operator. Both share this precedence tier.
                "\\" | "&" => {
                    let is_and = op.as_str() == "&";
                    match chain_is_and {
                        None => chain_is_and = Some(is_and),
                        Some(started) if started != is_and => {
                            self.error(
                                "Cannot mix `&` and `\\` in a type annotation without \
                                 parentheses; parenthesise to disambiguate",
                            );
                            // Recovery: keep building the AST using the
                            // originally-committed operator so this single
                            // diagnostic doesn't cascade into a confusing
                            // secondary error at the next token.
                        }
                        Some(_) => {}
                    }
                    self.advance(); // consume `\` or `&`
                    let rhs = self.parse_single_type_annotation();
                    let span = ty.span().merge(rhs.span());
                    ty = if chain_is_and == Some(true) {
                        TypeAnnotation::intersection(ty, rhs, span)
                    } else {
                        TypeAnnotation::difference(ty, rhs, span)
                    };
                }
                // `\\` — the modulo selector, greedily lexed from `\\`. In type
                // position this is a typo for the difference operator `\`.
                "\\\\" => {
                    self.error(
                        "Unexpected `\\\\` in type annotation; did you mean `\\` (type difference)?",
                    );
                    if chain_is_and.is_none() {
                        chain_is_and = Some(false);
                    }
                    self.advance(); // consume `\\` to recover
                    let excluded = self.parse_single_type_annotation();
                    let span = ty.span().merge(excluded.span());
                    ty = TypeAnnotation::difference(ty, excluded, span);
                }
                _ => break,
            }
        }

        ty
    }

    /// Parses a single type annotation (no unions).
    ///
    /// Handles:
    /// - Simple types: `Integer`, `String`
    /// - Generic types: `Result(T, E)`, `Array(Integer)`, `Block(T, Result(R, E))`
    /// - Self type: `Self`
    /// - Self class metatype: `Self class`
    /// - Singleton types: `#foo` (a subtype of `Symbol`, ADR 0068)
    /// - Grouping parentheses: `(Type)` (see below)
    pub(super) fn parse_single_type_annotation(&mut self) -> TypeAnnotation {
        if matches!(self.current_kind(), TokenKind::LeftParen) {
            // Grouping parentheses in type-annotation position
            // (unblocking ADR 0102 §3's mixed `&`/`\` disambiguation). Parsed
            // *transparently*: the parenthesised annotation is returned with
            // its original shape — only the span widens to cover the parens
            // — rather than wrapping it in a new AST node. This is enough
            // because:
            // - the resolver (`type_resolver.rs`) matches on the existing
            //   variants and never needs to know grouping parens were there;
            // - `TypeAnnotation::type_name()` (the unparser) already
            //   re-derives parens where precedence demands them — the
            //   `Difference`/`Intersection` arms parenthesise a nested
            //   `Union`/`FalseOr`/opposite-operator operand — so a group that
            //   changed the parse (e.g. `(A & B) \ C`) round-trips, while a
            //   redundant group (e.g. `(Integer)`) collapses away, which is
            //   fine since it doesn't change the AST shape.
            // Recursing into `parse_type_annotation` (not
            // `parse_single_type_annotation`/`parse_difference_type`) allows
            // full unions and fresh `&`/`\` chains inside the group — the
            // mixed-operator diagnostic's `chain_is_and` state is local to
            // each `parse_difference_type` call, so a new chain started
            // inside the parens is independent of the chain outside it.
            let start = self.current_token().span();
            self.advance(); // consume `(`
            let inner = self.parse_type_annotation();
            let end = if matches!(self.current_kind(), TokenKind::RightParen) {
                let end = self.current_token().span();
                self.advance(); // consume `)`
                end
            } else {
                self.error("Expected ')' to close grouping parentheses in type annotation");
                inner.span()
            };
            return inner.with_span(start.merge(end));
        }
        if let TokenKind::Identifier(name) = self.current_kind() {
            let span = self.current_token().span();
            if name.as_str() == "Self" {
                self.advance();
                // Check for `Self class` metatype annotation on the same line.
                // A `class` token with a leading newline
                // starts a new statement — typically a class-method definition
                // on the next line — and must not be consumed as a metatype
                // suffix.
                if let TokenKind::Identifier(next) = self.current_kind()
                    && next.as_str() == "class"
                    && !self.current_token().has_leading_newline()
                {
                    let end_span = self.current_token().span();
                    self.advance();
                    return TypeAnnotation::SelfClass {
                        span: span.merge(end_span),
                    };
                }
                TypeAnnotation::SelfType { span }
            } else {
                let ident = Identifier::new(name.clone(), span);
                self.advance();

                // Check for generic type parameters: `Name(Type, Type)`
                if matches!(self.current_kind(), TokenKind::LeftParen) {
                    self.advance(); // consume `(`
                    let mut parameters = Vec::new();
                    if !matches!(self.current_kind(), TokenKind::RightParen) {
                        parameters.push(self.parse_type_annotation());
                        while is_comma(self.current_kind()) {
                            self.advance(); // consume `,`
                            parameters.push(self.parse_type_annotation());
                        }
                    }
                    let end_span = self.current_token().span();
                    if matches!(self.current_kind(), TokenKind::RightParen) {
                        self.advance(); // consume `)`
                    } else {
                        self.error("Expected ')' after generic type parameters");
                    }
                    TypeAnnotation::Generic {
                        base: ident,
                        parameters,
                        span: span.merge(end_span),
                    }
                } else if let TokenKind::Identifier(next) = self.current_kind() {
                    // Check for `<ClassName> class` metatype annotation.
                    // Require `class` to be on the same line as the class name so
                    // that `... -> Foo\nclass bar => ...` (a class-method
                    // definition on the next line) still parses correctly as a
                    // return type `Foo` followed by a new class method.
                    if next.as_str() == "class" && !self.current_token().has_leading_newline() {
                        let end_span = self.current_token().span();
                        self.advance();
                        TypeAnnotation::ClassOf {
                            class_name: ident,
                            span: span.merge(end_span),
                        }
                    } else {
                        TypeAnnotation::Simple(ident)
                    }
                } else {
                    TypeAnnotation::Simple(ident)
                }
            }
        } else if matches!(self.current_kind(), TokenKind::MapOpen) {
            self.parse_shape_type()
        } else if let TokenKind::Symbol(name) = self.current_kind() {
            // Singleton type annotation `#foo` — a subtype of `Symbol`
            // (ADR 0068). `#name` is lexed as `TokenKind::Symbol(name)` (the `#`
            // is consumed by the lexer), so it surfaces in type position the
            // same way an identifier does. Composing with `parse_type_annotation`'s
            // `|` loop yields singleton unions (`#a | #b`, `Integer | #infinity`).
            let name = name.clone();
            let span = self.current_token().span();
            self.advance();
            TypeAnnotation::singleton(name, span)
        } else {
            let span = self.current_token().span();
            self.error("Expected type name");
            TypeAnnotation::Simple(Identifier::new("Error", span))
        }
    }

    /// Parses a map-shape type `#{ [?]#k => T, ..., ... }` (ADR 0147).
    ///
    /// The current token is `#{` ([`TokenKind::MapOpen`]). Fields are separated
    /// by `,`; because `,` and `?` are both binary-selector characters, a
    /// separator followed directly by an optional marker (`Integer,?#b`) lexes
    /// as one `,?` token, which is split here. A trailing `...` makes the shape
    /// open. Errors (non-symbol key, duplicate key, `...` not last, missing
    /// `=>`) are reported and parsing recovers to the next `,` or `}`.
    fn parse_shape_type(&mut self) -> TypeAnnotation {
        let start = self.current_token().span();
        self.advance(); // consume `#{`

        let mut fields: Vec<ShapeField> = Vec::new();
        let mut open = false;
        // An optional marker already consumed as part of a `,?` token.
        let mut pending_optional: Option<Span> = None;
        let mut need_separator = false;

        loop {
            if matches!(self.current_kind(), TokenKind::RightBrace | TokenKind::Eof) {
                break;
            }
            // Separator between fields.
            if need_separator {
                if !self.parse_shape_separator(&mut pending_optional) {
                    break;
                }
                need_separator = false;
                // Allow a trailing comma before `}`.
                if matches!(self.current_kind(), TokenKind::RightBrace)
                    && pending_optional.is_none()
                {
                    break;
                }
            }

            if matches!(self.current_kind(), TokenKind::Ellipsis) {
                open = true;
                if self.parse_shape_ellipsis(&mut pending_optional) {
                    continue;
                }
                break;
            }

            // Optional marker: `?` (separate token), or one split off `,?`.
            let mut optional = false;
            let mut field_start = self.current_token().span();
            if let Some(sp) = pending_optional.take() {
                optional = true;
                field_start = sp;
            } else if matches!(self.current_kind(), TokenKind::BinarySelector(s) if s.as_str() == "?")
            {
                optional = true;
                self.advance();
            }

            let (key, key_span) = if let TokenKind::Symbol(name) = self.current_kind() {
                let r = (name.clone(), self.current_token().span());
                self.advance();
                r
            } else {
                self.error("Shape type keys must be symbol literals (e.g. `#name => String`)");
                if !self.recover_shape_field() {
                    break;
                }
                need_separator = true;
                continue;
            };

            if !matches!(self.current_kind(), TokenKind::FatArrow) {
                self.error("Expected '=>' after shape type key");
                if !self.recover_shape_field() {
                    break;
                }
                need_separator = true;
                continue;
            }
            self.advance(); // consume `=>`

            let value = self.parse_type_annotation();
            let span = field_start.merge(value.span());
            self.push_shape_field(
                &mut fields,
                ShapeField {
                    key,
                    key_span,
                    value,
                    optional,
                    span,
                },
            );
            need_separator = true;
        }

        let end = self.current_token().span();
        if matches!(self.current_kind(), TokenKind::RightBrace) {
            self.advance();
        } else {
            self.error("Expected '}' to close shape type");
        }
        TypeAnnotation::Shape {
            fields,
            open,
            span: start.merge(end),
        }
    }

    /// Consumes a shape's `...` marker. Returns `true` when more entries
    /// follow (an error: `...` must be last), so parsing should continue.
    fn parse_shape_ellipsis(&mut self, pending_optional: &mut Option<Span>) -> bool {
        let ellipsis_span = self.current_token().span();
        self.advance();
        if pending_optional.take().is_some() {
            self.diagnostics.push(Diagnostic::error(
                "`?` must precede a symbol key in a shape type",
                ellipsis_span,
            ));
        }
        // Allow a trailing comma after `...` before `}`.
        if is_comma(self.current_kind()) && matches!(self.peek_at(1), Some(TokenKind::RightBrace)) {
            self.advance();
        }
        if matches!(self.current_kind(), TokenKind::RightBrace | TokenKind::Eof) {
            return false;
        }
        self.diagnostics.push(Diagnostic::error(
            "`...` must be the last entry in a shape type",
            ellipsis_span,
        ));
        // Recovery: keep parsing the remaining fields.
        if is_comma(self.current_kind()) {
            self.advance();
        } else if matches!(self.current_kind(), TokenKind::BinarySelector(s) if s.as_str() == ",?")
        {
            *pending_optional = Some(self.current_token().span());
            self.advance();
        }
        true
    }

    /// Consumes the `,` / `,?` separator between shape fields. A `,?` token is
    /// split: its optional marker is returned through `pending_optional`. On a
    /// missing separator, reports an error and recovers to the next separator;
    /// returns `false` when none is found (the shape ends here).
    fn parse_shape_separator(&mut self, pending_optional: &mut Option<Span>) -> bool {
        let is_comma_question =
            |k: &TokenKind| matches!(k, TokenKind::BinarySelector(s) if s.as_str() == ",?");
        if !is_comma(self.current_kind()) && !is_comma_question(self.current_kind()) {
            self.error("Expected ',' or '}' in shape type");
            if !self.recover_shape_field() {
                return false;
            }
        }
        if is_comma_question(self.current_kind()) {
            *pending_optional = Some(self.current_token().span());
        }
        self.advance();
        true
    }

    /// Appends `field` to a shape's fields, reporting a duplicate key.
    fn push_shape_field(&mut self, fields: &mut Vec<ShapeField>, field: ShapeField) {
        if fields.iter().any(|f| f.key == field.key) {
            self.diagnostics.push(Diagnostic::error(
                format!("Duplicate key `#{}` in shape type", field.key),
                field.key_span,
            ));
        } else {
            fields.push(field);
        }
    }

    /// Skips tokens to the next top-level `,` / `,?` inside a shape type.
    ///
    /// Returns `true` when it stopped on a separator. It stops (returning
    /// `false`) at the shape's closing `}`, an unmatched `)` / `]`, EOF, or a
    /// new method definition starting a line, so an unclosed shape never
    /// swallows the declarations that follow it. Every bracket opener counts
    /// toward nesting, so a `,` inside a later literal is not mistaken for a
    /// field separator.
    fn recover_shape_field(&mut self) -> bool {
        let mut depth = 0usize;
        loop {
            if depth == 0
                && self.current_token().has_leading_newline()
                && self.is_at_method_definition()
            {
                return false;
            }
            match self.current_kind() {
                TokenKind::Eof => return false,
                TokenKind::LeftParen
                | TokenKind::LeftBracket
                | TokenKind::LeftBrace
                | TokenKind::MapOpen
                | TokenKind::ListOpen
                | TokenKind::ArrayOpen => depth += 1,
                TokenKind::RightParen | TokenKind::RightBracket | TokenKind::RightBrace => {
                    if depth == 0 {
                        return false;
                    }
                    depth -= 1;
                }
                TokenKind::BinarySelector(s) if depth == 0 && (s == "," || s == ",?") => {
                    return true;
                }
                _ => {}
            }
            self.advance();
        }
    }

    /// Parses an optional return type annotation before `=>`.
    ///
    /// Accepts two forms:
    /// - `-> ReturnType` (standard: `factorial -> Integer =>`)
    /// - `:: -> ReturnType` (extension-style: `Integer >> factorial :: -> Integer =>`)
    ///
    /// The `:: ->` form (ADR 0066 Phase 4) provides a clear visual separator
    /// between the selector and return type, especially useful on unary
    /// extension methods where there are no parameters to carry `::` annotations.
    pub(super) fn parse_optional_return_type(&mut self) -> Option<TypeAnnotation> {
        // Handle `:: -> Type` syntax (ADR 0066: extension type annotations)
        if matches!(self.current_kind(), TokenKind::DoubleColon)
            && matches!(self.peek_at(1), Some(TokenKind::Arrow))
        {
            self.advance(); // consume `::`
            self.advance(); // consume `->`
            return Some(self.parse_type_annotation());
        }
        if matches!(self.current_kind(), TokenKind::Arrow) {
            self.advance(); // consume `->`
            Some(self.parse_type_annotation())
        } else {
            None
        }
    }
}
