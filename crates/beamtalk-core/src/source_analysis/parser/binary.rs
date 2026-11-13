// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Binary segment parsing for Beamtalk (`<<...>>` patterns and constructions).
//!
//! **DDD Context:** Compilation
//!
//! Parses binary construction and binary pattern segments (ADR 0143): the
//! shared segment grammar, literal-shape checks and unit / type specifiers.

use crate::ast::{
    BinaryEndianness, BinarySegment, BinarySegmentType, BinarySegmentValue, BinarySignedness,
    Expression, Identifier, Literal, Pattern, binary_literal_size,
};
use crate::source_analysis::{Span, Token, TokenKind};
use ecow::EcoString;

use super::expressions::parse_integer;
use super::{Diagnostic, Parser};

/// Converts a literal binary segment value parsed by the match side's
/// literal parsers into a construction value (ADR 0143), so a literal reads
/// the same in both directions. The parsers answer `Pattern::Literal`, or a
/// `Pattern::Wildcard` after reporting an error (out-of-range number,
/// missing digits), which becomes an `Expression::Error` node.
fn binary_literal_value_to_expression(value: Pattern) -> Expression {
    match value {
        Pattern::Literal(lit, span) => Expression::Literal(lit, span),
        other => Expression::Error {
            message: "invalid literal in binary construction segment".into(),
            span: other.span(),
        },
    }
}

/// Which way a binary segment goes, for the wording of the shared literal
/// checks (ADR 0143 § Literal checks): a misfit literal makes a pattern arm
/// that can never match, and a construction that truncates or fails.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BinaryDirection {
    /// A `<<...>>` pattern in a match arm.
    Pattern,
    /// A `<<...>>` construction in expression position.
    Construction,
}

impl BinaryDirection {
    /// The form, as diagnostics name it.
    fn noun(self) -> &'static str {
        match self {
            Self::Pattern => "pattern",
            Self::Construction => "construction",
        }
    }

    /// What the segment does with its value: (`matches`, `match`) or
    /// (`builds`, `build`).
    fn verbs(self) -> (&'static str, &'static str) {
        match self {
            Self::Pattern => ("matches", "match"),
            Self::Construction => ("builds", "build"),
        }
    }

    /// The lead-in of a wrong-type literal warning.
    fn cannot_succeed(self) -> &'static str {
        match self {
            Self::Pattern => "this arm can never match",
            Self::Construction => "binary construction will fail",
        }
    }
}

/// The value a construction stores for an integer literal that does not fit
/// its segment: its low `size_bits` bits, read back with the segment's
/// signedness (`<<(-1):4>>` stores 15, as `erlc` does). `None` when the size
/// is too wide to print the value exactly.
fn truncated_binary_literal(value: i64, size_bits: i64, signed: bool) -> Option<i128> {
    if size_bits == 0 {
        return Some(0);
    }
    if !(1..=126).contains(&size_bits) {
        return None;
    }
    let modulus = 1i128 << size_bits;
    let low = i128::from(value).rem_euclid(modulus);
    Some(if signed && low >= modulus / 2 {
        low - modulus
    } else {
        low
    })
}

/// The `unit:` multiplier as an `i64`: `1` when no unit is given (BT-4143).
/// `parse_unit_specifier` pins units to `1..=256`, so the conversion is
/// always exact.
fn unit_multiplier(unit: Option<usize>) -> i64 {
    unit.map_or(1, |u| i64::try_from(u).unwrap_or(1))
}

/// Whether an integer segment is 8 bits wide: the default size (no size
/// given), or an explicit literal size × unit of 8 (`$é:8`, `$é:1/unit:8`).
/// A non-literal size is not known statically, so it is not.
fn is_eight_bits_wide(
    size_is_default: bool,
    size_lit: Option<(i64, crate::source_analysis::Span)>,
    unit: Option<usize>,
) -> bool {
    size_is_default || size_lit.is_some_and(|(n, _)| n.saturating_mul(unit_multiplier(unit)) == 8)
}

/// The unit a binary segment type name implies, verified against `erlc`
/// (BT-4376): `bits`/`bitstring` imply 1 and `bytes` implies 8, so a
/// different explicit `unit:` is a conflict. `binary` and the other types
/// imply none (`<<X:2/binary-unit:16>>` is accepted).
fn implied_segment_unit(type_name: &str) -> Option<usize> {
    match type_name {
        "bits" | "bitstring" => Some(1),
        "bytes" => Some(8),
        _ => None,
    }
}

/// Whether integer `value` fits in `size_bits` bits (review finding 3).
///
/// `size_bits` is non-negative (negative sizes are rejected separately).
/// Sizes of 64+ hold any `i64` magnitude; unsigned still rejects negatives.
fn binary_literal_fits(value: i64, size_bits: i64, signed: bool) -> bool {
    if size_bits < 0 {
        return false;
    }
    if size_bits == 0 {
        return value == 0;
    }
    if !signed && value < 0 {
        return false;
    }
    if size_bits >= 64 {
        return true;
    }
    // size_bits is now in 1..=63: shifts cannot overflow.
    if signed {
        let half = 1i64 << (size_bits - 1);
        (-half..half).contains(&value)
    } else {
        (0..(1i64 << size_bits)).contains(&value)
    }
}
impl Parser {
    /// Parses a binary pattern: `<<seg, seg, ...>>`
    ///
    /// Segment syntax: `varname`, `varname:size`, `varname/type`, `varname:size/type-modifiers`
    /// where type is one of `integer`, `float`, `binary`, `bits`/`bitstring`, `utf8`
    /// and modifiers include `signed`/`unsigned`, `big`/`little`/`native`.
    /// The value may also be an integer literal (`0:1`, BT-4097) and the size
    /// a parenthesized expression (`v:(8 - p)`, BT-4097).
    pub(super) fn parse_binary_pattern(&mut self) -> Pattern {
        let start = self.current_token().span();
        let (segments, end) = self.parse_binary_segment_list("pattern", Self::parse_binary_segment);
        let end = end.unwrap_or_else(|| {
            self.error("Expected '>>' to close binary pattern");
            start
        });
        Pattern::Binary {
            segments,
            span: start.merge(end),
        }
    }

    /// Parses a binary construction: `<<seg, seg, ...>>` in operand position
    /// (ADR 0143).
    ///
    /// The segment grammar is the match side's, through the same
    /// [`Self::parse_binary_segment_with`]: only the value production
    /// differs (see [`Self::parse_binary_construction_value`]).
    pub(super) fn parse_binary_construction(&mut self) -> Expression {
        let start = self.current_token().span();
        let (segments, end) =
            self.parse_binary_segment_list("construction", Self::parse_binary_construction_segment);
        // Without a closing `>>` the construction ends at the last token it
        // consumed, so its span always covers its segments.
        let end = end.unwrap_or_else(|| {
            let recovered = self.recover_unclosed_binary_construction(start);
            recovered.merge(self.tokens[self.current.saturating_sub(1)].span())
        });
        Expression::BinaryConstruction {
            segments,
            span: start.merge(end),
        }
    }

    /// Reports a construction with no closing `>>` and skips to it, so the
    /// rest of the segment list does not leak into the enclosing expression
    /// as stray sends. The usual cause is an unparenthesised send as a
    /// segment value (`<<x size:8>>` reads as segment `x` followed by the
    /// keyword `size:`), so a name there gets the parenthesise hint (ADR
    /// 0143 § Segment values). Skipping stops at the closing `>>`, or
    /// without consuming at the end of the statement (`.`, a closing
    /// bracket or parenthesis, a new line, end of input). Answers the span
    /// to end the construction at.
    fn recover_unclosed_binary_construction(&mut self, start: Span) -> Span {
        let message = "Expected '>>' to close binary construction";
        if matches!(
            self.current_kind(),
            TokenKind::Identifier(_) | TokenKind::Keyword(_)
        ) {
            let span = self.current_token().span();
            self.diagnostics.push(
                Diagnostic::error(message, span).with_hint(
                    "a segment value other than a variable or a literal must be parenthesized: <<(x size):8>>",
                ),
            );
        } else {
            self.error(message);
        }
        let mut end = start;
        loop {
            match self.current_kind() {
                TokenKind::GtGt => {
                    end = self.advance().span();
                    break;
                }
                TokenKind::Eof
                | TokenKind::Period
                | TokenKind::RightBracket
                | TokenKind::RightParen
                | TokenKind::RightBrace => break,
                _ if self.current_token().has_leading_newline() => break,
                _ => end = self.advance().span(),
            }
        }
        end
    }

    /// Parses `<<seg, seg, ...>>` with `parse_segment` for each segment,
    /// answering the segments and the closing `>>`'s span, or `None` when
    /// it is missing (the caller reports that). `what` names the form in
    /// diagnostics (`pattern` or `construction`).
    fn parse_binary_segment_list<V>(
        &mut self,
        what: &str,
        parse_segment: fn(&mut Self) -> BinarySegment<V>,
    ) -> (Vec<BinarySegment<V>>, Option<Span>) {
        self.advance(); // consume `<<`

        let mut segments = Vec::new();

        if !matches!(self.current_kind(), TokenKind::GtGt) {
            segments.push(parse_segment(self));
            while self.match_binary_selector(",") {
                if matches!(self.current_kind(), TokenKind::GtGt) {
                    self.error(format!(
                        "Trailing comma is not allowed before '>>' in binary {what}"
                    ));
                    break;
                }
                segments.push(parse_segment(self));
            }
        }

        let end = if matches!(self.current_kind(), TokenKind::GtGt) {
            Some(self.advance().span()) // consume `>>`
        } else {
            None
        };

        (segments, end)
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
            TokenKind::Integer(_) | TokenKind::Identifier(_) | TokenKind::LeftParen => {
                let expr = self.parse_primary();
                match &expr {
                    Expression::Literal(Literal::Integer(_), _)
                    | Expression::Identifier(_)
                    | Expression::Parenthesized { .. } => Some(Box::new(expr)),
                    _ => {
                        self.error(
                            "Binary segment size must be an integer literal, a variable name, or a parenthesized expression",
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
        let seg = self.parse_binary_segment_with(Self::parse_binary_pattern_value);
        // Literal shape checks (review finding 3): mirror `erlc` — error
        // where it errors, warn where it warns — so mistyped literals are
        // diagnostics, never silently dead arms or invalid Core Erlang.
        self.check_binary_literal_shapes(&seg, BinaryDirection::Pattern);
        seg
    }

    /// Parses one segment of a binary construction (ADR 0143).
    ///
    /// The literal checks are the match side's (ADR 0143 § Literal checks),
    /// worded for construction ("truncated", "will fail") rather than "can
    /// never match".
    fn parse_binary_construction_segment(&mut self) -> BinarySegment<Expression> {
        let seg = self.parse_binary_segment_with(Self::parse_binary_construction_value);
        self.check_binary_literal_shapes(&seg, BinaryDirection::Construction);
        seg
    }

    /// Parses one binary segment, in either direction: the value and its
    /// optional `:size` via `parse_value`, then the shared
    /// `/type-signedness-endianness-unit:N` specifiers.
    fn parse_binary_segment_with<V>(
        &mut self,
        parse_value: fn(&mut Self) -> (V, Option<Box<Expression>>),
    ) -> BinarySegment<V> {
        let start = self.current_token().span();
        let (value, size) = parse_value(self);

        // Optional type/modifiers: `/type[-modifier]*` (BT-4143 adds
        // `utf16`, `utf32`, and `unit:N` to the specifier set)
        let (segment_type, signedness, endianness, unit) = self.parse_binary_segment_specifiers();

        let end = self.tokens[self.current.saturating_sub(1)].span();
        BinarySegment {
            value,
            size,
            segment_type,
            signedness,
            endianness,
            unit,
            span: start.merge(end),
        }
    }

    /// Parses a binary pattern segment's value and optional `:size`: a
    /// variable, `_`, or an integer, float, string or character literal
    /// (optionally negated).
    fn parse_binary_pattern_value(&mut self) -> (Pattern, Option<Box<Expression>>) {
        match self.current_kind() {
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

            // `0:1` — integer literal segment value (BT-4097)
            TokenKind::Integer(_) => self.parse_binary_literal_value(false),

            // `1.5/float` — float literal segment value (BT-4143)
            TokenKind::Float(_) => self.parse_binary_float_value(false),

            // `"hi"` — string literal segment value (BT-4143)
            TokenKind::String(_) => self.parse_binary_string_value(),

            // `$A` — character literal segment value (BT-4143)
            TokenKind::Character(_) => self.parse_binary_character_value(),

            // `-1:8`, `-1.5/float` — negative literal segment values
            // (BT-4097 for integers, BT-4143 for floats)
            TokenKind::BinarySelector(op) if op.as_str() == "-" => {
                self.advance(); // consume `-`
                match self.current_kind() {
                    TokenKind::Float(_) => self.parse_binary_float_value(true),
                    TokenKind::Integer(_) => self.parse_binary_literal_value(true),
                    _ => {
                        let span = self.current_token().span();
                        self.error("Expected number after '-' in binary segment");
                        (Pattern::Wildcard(span), None)
                    }
                }
            }

            _ => {
                let span = self.current_token().span();
                self.error("Expected variable name or literal value in binary segment");
                (Pattern::Wildcard(span), None)
            }
        }
    }

    /// Parses a binary construction segment's value and optional `:size`
    /// (ADR 0143 § Segment values): a variable, a literal (integer, float,
    /// character or string, optionally negated), or a parenthesised
    /// expression for anything else (`<<(payload size):16>>`).
    ///
    /// Literal values go through the match side's literal parsers, so a
    /// literal reads identically in both directions.
    fn parse_binary_construction_value(&mut self) -> (Expression, Option<Box<Expression>>) {
        match self.current_kind() {
            // `name:size` — the `:` attached to the name lexes as Keyword("name:")
            TokenKind::Keyword(kw) => {
                let kw = kw.clone();
                let kw_span = self.advance().span();
                let name: EcoString = kw.trim_end_matches(':').into();
                let var_span = Span::new(kw_span.start(), kw_span.end().saturating_sub(1));
                let value = self.binary_construction_variable(name, var_span, false);
                let size = self.parse_binary_segment_size();
                (value, size)
            }

            // `name` — variable, with optional explicit `:size` (Colon + size-token)
            TokenKind::Identifier(name) => {
                let name = name.clone();
                let span = self.advance().span();
                let value = self.binary_construction_variable(name, span, true);
                if self.at_attached_field_access(true) {
                    return self.binary_construction_field_access(value);
                }
                let size = self.parse_binary_value_size_suffix();
                (value, size)
            }

            // `(expr)` — any other value, parenthesised; optional `:size`
            TokenKind::LeftParen => {
                let value = self.parse_parenthesized();
                let size = self.parse_binary_value_size_suffix();
                (value, size)
            }

            TokenKind::Integer(_)
            | TokenKind::Float(_)
            | TokenKind::String(_)
            | TokenKind::Character(_) => {
                let (pattern, size) = self.parse_binary_pattern_value();
                (binary_literal_value_to_expression(pattern), size)
            }

            // `-1:8`, `-1.5/float` — negative literal values
            TokenKind::BinarySelector(op) if op.as_str() == "-" => {
                let (pattern, size) = self.parse_binary_pattern_value();
                (binary_literal_value_to_expression(pattern), size)
            }

            _ => {
                let span = self.current_token().span();
                // Consume the stray token (`<<#foo>>`) so the segment list
                // goes on to its `>>`, unless it ends the statement.
                if !matches!(
                    self.current_kind(),
                    TokenKind::Eof
                        | TokenKind::Period
                        | TokenKind::GtGt
                        | TokenKind::RightBracket
                        | TokenKind::RightParen
                        | TokenKind::RightBrace
                ) {
                    self.advance();
                }
                let message: EcoString =
                    "Expected a variable, a literal, or a parenthesized expression in binary construction segment"
                        .into();
                self.diagnostics.push(
                    Diagnostic::error(message.clone(), span)
                        .with_hint("wrap any other value in parentheses: <<(payload size):16>>"),
                );
                (Expression::Error { message, span }, None)
            }
        }
    }

    /// A construction segment's named value: a variable, a class reference
    /// or `super`, built as the ordinary operand path builds them
    /// ([`Self::identifier_expression`]) so the same diagnostics apply.
    /// `package` allows a package-qualified class reference (`json@Parser`),
    /// which a `name:` keyword token cannot start. `_` matches anything in a
    /// pattern but has no value to build with, so it is an error here.
    fn binary_construction_variable(
        &mut self,
        name: EcoString,
        span: Span,
        package: bool,
    ) -> Expression {
        if name == "_" {
            let message: EcoString =
                "'_' cannot be a value in a binary construction; it only skips bits in a binary pattern"
                    .into();
            self.diagnostics
                .push(Diagnostic::error(message.clone(), span));
            return Expression::Error { message, span };
        }
        if !package && !name.chars().next().is_some_and(char::is_uppercase) && name != "super" {
            return Expression::Identifier(Identifier::new(name, span));
        }
        self.identifier_expression(name, span)
    }

    /// An unparenthesised field access as a construction segment value
    /// (`<<self.len:8>>`, `<<x.y>>`): reported with the parenthesise hint
    /// (ADR 0143 § Segment values) and consumed with its `:size`, so the
    /// rest of the segment does not spill out of the construction. The
    /// current token is the `.` after `receiver`.
    fn binary_construction_field_access(
        &mut self,
        receiver: Expression,
    ) -> (Expression, Option<Box<Expression>>) {
        let mut value = receiver;
        let mut keyword_field = false;
        while !keyword_field && self.at_attached_field_access(true) {
            self.advance(); // consume the period
            let field_token = self.advance();
            let field_span = field_token.span();
            let field_name: EcoString = match field_token.into_kind() {
                TokenKind::Identifier(name) => name,
                TokenKind::Keyword(kw) => {
                    // `len:` — the field name with its size colon attached.
                    keyword_field = true;
                    kw.trim_end_matches(':').into()
                }
                _ => unreachable!("at_attached_field_access checked the token"),
            };
            let field_span = if keyword_field {
                Span::new(field_span.start(), field_span.end().saturating_sub(1))
            } else {
                field_span
            };
            let span = value.span().merge(field_span);
            value = Expression::FieldAccess {
                receiver: Box::new(value),
                field: Identifier::new(field_name, field_span),
                span,
            };
        }
        self.diagnostics.push(
            Diagnostic::error(
                "a field access in a binary construction segment must be parenthesized",
                value.span(),
            )
            .with_hint(
                "a segment value other than a variable or a literal must be parenthesized: <<(self.len):8>>",
            ),
        );
        let size = if keyword_field {
            self.parse_binary_segment_size()
        } else {
            self.parse_binary_value_size_suffix()
        };
        (value, size)
    }

    /// Validates a binary segment's literal size and value against its type,
    /// in either direction (ADR 0143 § Literal checks: one policy for
    /// patterns and constructions).
    ///
    /// Mirrors `erlc` (review finding 3, BT-4143): negative literal sizes,
    /// sizes or units on `utf8`/`utf16`/`utf32` segments, a unit without a
    /// size, and any type/size on a string literal value are errors; values
    /// that cannot fit, literals of the wrong type for their segment, and
    /// bad float bit sizes are warnings. Variable sizes are unchecked
    /// (validated at runtime). `direction` words the warnings: a pattern arm
    /// "can never match", a construction truncates or fails.
    fn check_binary_literal_shapes<V: BinarySegmentValue>(
        &mut self,
        seg: &BinarySegment<V>,
        direction: BinaryDirection,
    ) {
        let value = &seg.value;
        let size = seg.size.as_deref();
        let unit = seg.unit;
        let size_lit = size.and_then(binary_literal_size);
        if let Some((n, span)) = size_lit
            && n < 0
        {
            self.diagnostics.push(Diagnostic::error(
                format!("binary segment sizes must be non-negative, got {n}"),
                span,
            ));
            return;
        }
        let seg_type = seg.effective_type();
        if self.reject_utf_size_or_unit(seg_type, value.binary_value_span(), size, size_lit, unit) {
            return;
        }
        let literal = value.binary_literal();
        if let Some((Literal::String(_), value_span)) = literal {
            self.reject_typed_string_literal(seg, value_span, direction);
            return;
        }
        // `erlc`: "a bit unit size must not be specified unless a size is
        // specified too" — only for integer and float segments; a sizeless
        // `binary`/`bits` segment takes a unit (`<<X/binary-unit:16>>`
        // matches the rest when its bit length is a multiple of 16).
        if unit.is_some()
            && size.is_none()
            && matches!(
                seg_type,
                BinarySegmentType::Integer | BinarySegmentType::Float
            )
        {
            self.diagnostics.push(Diagnostic::error(
                "a bit unit size must not be specified unless a size is specified too",
                value.binary_value_span(),
            ));
            return;
        }
        if seg_type == BinarySegmentType::Float {
            if let Some((n, span)) = size_lit {
                // `erlc` validates size × unit (`<<X:4/unit:8-float>>` is a
                // clean 32-bit float; `<<X:16/unit:8-float>>` warns).
                let effective = n.saturating_mul(unit_multiplier(unit));
                if effective != 16 && effective != 32 && effective != 64 {
                    self.diagnostics.push(Diagnostic::warning(
                        format!(
                            "bad float bit size: float segments must be 16, 32, or 64 bits, got {effective}"
                        ),
                        span,
                    ));
                }
            }
            return;
        }
        let Some((lit, value_span)) = literal else {
            return;
        };
        // Integer-valued literals: integers and characters (`$A` is its
        // codepoint, exactly like `erlc`).
        let int_value = match lit {
            Literal::Integer(v) => Some(*v),
            Literal::Character(c) => Some(i64::from(*c as u32)),
            _ => None,
        };
        if let Some(v) = int_value {
            if let (Literal::Character(c), BinarySegmentType::Integer) = (lit, seg_type)
                && is_eight_bits_wide(size.is_none(), size_lit, unit)
            {
                self.warn_non_ascii_byte_character(*c, value_span, direction);
            }
            self.check_binary_int_literal_shape(
                v,
                value_span,
                seg_type,
                size_lit,
                seg.signedness,
                unit,
                direction,
            );
            return;
        }
        // Float literals with a non-float type warn, `erlc`-style
        // (`<<1.5>>` warns that '1.5' is not of the expected type).
        if let Literal::Float(f) = lit {
            let expected = match seg_type {
                BinarySegmentType::Integer => "integer",
                BinarySegmentType::Binary => "binary",
                BinarySegmentType::Bitstring => "bitstring",
                BinarySegmentType::Utf8 => "utf8",
                BinarySegmentType::Utf16 => "utf16",
                BinarySegmentType::Utf32 => "utf32",
                BinarySegmentType::Float => return,
            };
            self.diagnostics.push(Diagnostic::warning(
                format!(
                    "{} because '{f}' is not of the expected type '{expected}'",
                    direction.cannot_succeed()
                ),
                value_span,
            ));
        }
    }

    /// String literal values take no size or unit, and no type other than
    /// an explicit `utf8`/`utf16`/`utf32` — `erlc` folds `<<"hi">>` to one
    /// integer but expands `<<"hi"/utf16>>` to one segment per character,
    /// while bare signedness/endianness (`<<"hi"/signed>>`) and every other
    /// type are errors ("must not have a type or a size"). The string error
    /// takes precedence over the unit-without-size error (verified against
    /// `erlc`: `<<"hi"/unit:8>>` reports the string error). A construction
    /// makes it an error too (ADR 0143): `erlc` only warns, but the segment
    /// can never build.
    fn reject_typed_string_literal<V>(
        &mut self,
        seg: &BinarySegment<V>,
        value_span: Span,
        direction: BinaryDirection,
    ) {
        let utf_typed = matches!(
            seg.effective_type(),
            BinarySegmentType::Utf8 | BinarySegmentType::Utf16 | BinarySegmentType::Utf32
        );
        if !utf_typed
            && (seg.size.is_some()
                || seg.segment_type.is_some()
                || seg.signedness.is_some()
                || seg.endianness.is_some()
                || seg.unit.is_some())
        {
            self.diagnostics.push(Diagnostic::error(
                format!(
                    "a literal string in a binary {} must not have a type or a size",
                    direction.noun()
                ),
                value_span,
            ));
        }
    }

    /// Warns when a non-ASCII character fills a default 8-bit integer
    /// segment (BT-4376): `<<$é>>` matches (or builds) one byte holding its
    /// code point (233, exactly like `erlc`), not its UTF-8 encoding, so it
    /// disagrees with `<<"é">>`, which stands for the UTF-8 bytes. Code
    /// points above 255 already warn that they do not fit, so only
    /// `128..=255` warns here.
    fn warn_non_ascii_byte_character(
        &mut self,
        c: char,
        span: crate::source_analysis::Span,
        direction: BinaryDirection,
    ) {
        let code_point = u32::from(c);
        if (128..=255).contains(&code_point) {
            let (verbs, verb) = direction.verbs();
            self.diagnostics.push(Diagnostic::warning(
                format!(
                    "character literal ${c} {verbs} the single byte {code_point} (its code point), not its UTF-8 encoding; use \"{c}\" or ${c}/utf8 to {verb} the UTF-8 bytes"
                ),
                span,
            ));
        }
    }

    /// Rejects sizes and units on `utf8`/`utf16`/`utf32` segments, mirroring
    /// `erlc` ("neither size nor unit must be given"). Answers whether any
    /// error was reported.
    fn reject_utf_size_or_unit(
        &mut self,
        seg_type: BinarySegmentType,
        value_span: crate::source_analysis::Span,
        size: Option<&Expression>,
        size_lit: Option<(i64, crate::source_analysis::Span)>,
        unit: Option<usize>,
    ) -> bool {
        if !matches!(
            seg_type,
            BinarySegmentType::Utf8 | BinarySegmentType::Utf16 | BinarySegmentType::Utf32
        ) {
            return false;
        }
        let type_name = match seg_type {
            BinarySegmentType::Utf8 => "utf8",
            BinarySegmentType::Utf16 => "utf16",
            _ => "utf32",
        };
        let mut errored = false;
        if size.is_some() {
            let span = size_lit.map_or(value_span, |(_, s)| s);
            self.diagnostics.push(Diagnostic::error(
                format!("size must not be given for segments of type {type_name}"),
                span,
            ));
            errored = true;
        }
        if unit.is_some() {
            self.diagnostics.push(Diagnostic::error(
                format!("unit must not be given for segments of type {type_name}"),
                value_span,
            ));
            errored = true;
        }
        errored
    }

    /// Warns when an integer-valued literal cannot do what its segment
    /// asks: integer literals with `binary`/`bitstring` type, and values
    /// that do not fit the (signed or unsigned) size — a pattern arm that
    /// can never match, or a construction that truncates to the low bits.
    #[allow(clippy::too_many_arguments)] // one call site; the segment's fields, unpacked
    fn check_binary_int_literal_shape(
        &mut self,
        v: i64,
        value_span: crate::source_analysis::Span,
        seg_type: BinarySegmentType,
        size_lit: Option<(i64, crate::source_analysis::Span)>,
        signedness: Option<BinarySignedness>,
        unit: Option<usize>,
        direction: BinaryDirection,
    ) {
        match seg_type {
            BinarySegmentType::Binary | BinarySegmentType::Bitstring => {
                let name = if seg_type == BinarySegmentType::Binary {
                    "binary"
                } else {
                    "bitstring"
                };
                self.diagnostics.push(Diagnostic::warning(
                    format!(
                        "{} because '{v}' is not of the expected type '{name}'",
                        direction.cannot_succeed()
                    ),
                    value_span,
                ));
            }
            BinarySegmentType::Integer => {
                // Default integer size is 8 bits (same as codegen); with a
                // unit the effective size is size × unit (`<<4660:2/unit:8>>`
                // is a clean 16-bit match).
                let size_bits = match size_lit {
                    Some((n, _)) => n.saturating_mul(unit_multiplier(unit)),
                    None => 8,
                };
                let signed = signedness == Some(BinarySignedness::Signed);
                if !binary_literal_fits(v, size_bits, signed) {
                    let sign = if signed { "signed" } else { "unsigned" };
                    let message = match direction {
                        BinaryDirection::Pattern if !signed && v < 0 => format!(
                            "this arm can never match because the negative value {v} will never match the value of an 'unsigned' binary segment"
                        ),
                        BinaryDirection::Pattern => format!(
                            "this arm can never match because the value {v} will not fit in an '{sign}' binary segment of size {size_bits}"
                        ),
                        BinaryDirection::Construction => {
                            match truncated_binary_literal(v, size_bits, signed) {
                                Some(t) => format!(
                                    "the value {v} does not fit in an '{sign}' binary segment of size {size_bits} and is truncated to {t}"
                                ),
                                None => format!(
                                    "the value {v} does not fit in an '{sign}' binary segment of size {size_bits} and is truncated to its low {size_bits} bits"
                                ),
                            }
                        }
                    };
                    self.diagnostics
                        .push(Diagnostic::warning(message, value_span));
                }
            }
            BinarySegmentType::Float
            | BinarySegmentType::Utf8
            | BinarySegmentType::Utf16
            | BinarySegmentType::Utf32 => {}
        }
    }

    /// Parses an integer literal binary segment value with optional `:size`.
    ///
    /// `negated` is true when the caller consumed a leading `-` (BT-4097).
    fn parse_binary_literal_value(&mut self, negated: bool) -> (Pattern, Option<Box<Expression>>) {
        let token = self.advance();
        let span = token.span();
        let TokenKind::Integer(raw) = token.into_kind() else {
            self.error("Expected integer literal in binary segment");
            return (Pattern::Wildcard(span), None);
        };
        // The lexer emits signed `Integer("-…")` tokens when `-` directly
        // precedes digits, so `raw` may itself be negative (e.g. the second
        // `-` in `<<- -5:8>>` double-negates). Negation must not overflow:
        // `- -9223372036854775808` exceeds `i64::MAX` (review finding 2).
        let value = match parse_integer(&raw) {
            Ok(val) if !negated => Pattern::Literal(Literal::Integer(val), span),
            Ok(val) => {
                if let Some(neg) = val.checked_neg() {
                    Pattern::Literal(Literal::Integer(neg), span)
                } else {
                    self.error("integer literal out of range in binary segment");
                    Pattern::Wildcard(span)
                }
            }
            Err(e) => {
                self.diagnostics.push(Diagnostic::error(e, span));
                Pattern::Wildcard(span)
            }
        };
        let size = self.parse_binary_value_size_suffix();
        (value, size)
    }

    /// Parses the optional `:size` suffix of a binary segment literal value.
    fn parse_binary_value_size_suffix(&mut self) -> Option<Box<Expression>> {
        if matches!(self.current_kind(), TokenKind::Colon) {
            self.advance(); // consume `:`
            self.parse_binary_segment_size()
        } else {
            None
        }
    }

    /// Parses a float literal binary segment value with optional `:size`.
    ///
    /// `negated` is true when the caller consumed a leading `-` (BT-4143).
    /// A signed `Float("-1.5")` token (no space after `-`) parses directly;
    /// an explicit `-` before it double-negates, exactly like integers.
    fn parse_binary_float_value(&mut self, negated: bool) -> (Pattern, Option<Box<Expression>>) {
        let token = self.advance();
        let span = token.span();
        let TokenKind::Float(raw) = token.into_kind() else {
            self.error("Expected float literal in binary segment");
            return (Pattern::Wildcard(span), None);
        };
        let value = match raw.parse::<f64>() {
            // Reject values that overflow `f64` — same rule as `parse_literal`.
            Ok(val) if val.is_finite() => {
                let val = if negated { -val } else { val };
                // Negating a finite float stays finite.
                Pattern::Literal(Literal::Float(val), span)
            }
            _ => {
                self.diagnostics.push(Diagnostic::error(
                    format!("Float literal out of range: {raw}"),
                    span,
                ));
                Pattern::Wildcard(span)
            }
        };
        let size = self.parse_binary_value_size_suffix();
        (value, size)
    }

    /// Parses a string literal binary segment value with optional `:size`.
    ///
    /// `erlc` rejects any type/size on string literals — the size and
    /// specifiers still parse here so the shape check can report the same
    /// error on the right span.
    fn parse_binary_string_value(&mut self) -> (Pattern, Option<Box<Expression>>) {
        let token = self.advance();
        let span = token.span();
        let TokenKind::String(raw) = token.into_kind() else {
            self.error("Expected string literal in binary segment");
            return (Pattern::Wildcard(span), None);
        };
        let size = self.parse_binary_value_size_suffix();
        (Pattern::Literal(Literal::String(raw), span), size)
    }

    /// Parses a character literal binary segment value with optional `:size`.
    ///
    /// `$A` is its codepoint — an integer-valued literal, exactly like `erlc`.
    fn parse_binary_character_value(&mut self) -> (Pattern, Option<Box<Expression>>) {
        let token = self.advance();
        let span = token.span();
        let TokenKind::Character(c) = token.into_kind() else {
            self.error("Expected character literal in binary segment");
            return (Pattern::Wildcard(span), None);
        };
        let size = self.parse_binary_value_size_suffix();
        (Pattern::Literal(Literal::Character(c), span), size)
    }

    /// Parses optional `/type[-modifier]*` specifiers for a binary segment.
    ///
    /// Returns `(segment_type, signedness, endianness, unit)`.
    fn parse_binary_segment_specifiers(
        &mut self,
    ) -> (
        Option<BinarySegmentType>,
        Option<BinarySignedness>,
        Option<BinaryEndianness>,
        Option<usize>,
    ) {
        let mut segment_type = None;
        let mut signedness = None;
        let mut endianness = None;
        let mut unit = None;
        // The unit a `bits`/`bitstring`/`bytes` type implies (BT-4376).
        let mut implied_unit: Option<usize> = None;

        if matches!(self.current_kind(), TokenKind::BinarySelector(s) if s.as_str() == "/") {
            self.advance(); // consume `/`
            if !matches!(
                self.current_kind(),
                TokenKind::Identifier(_) | TokenKind::Keyword(_)
            ) {
                self.error("Expected a binary segment specifier after '/'");
            }

            // Parse specifiers separated by `-` (e.g. `signed-little`,
            // `integer-unit:8`). `unit:N` lexes as `Keyword("unit:")` when
            // the colon is attached, or `Identifier("unit")` + `Colon`
            // when spaced — both forms are accepted.
            loop {
                if self.at_unit_specifier() {
                    if unit.is_some() {
                        let span = self.current_token().span();
                        self.diagnostics.push(Diagnostic::error(
                            "Conflicting binary segment unit specifier 'unit': unit already set",
                            span,
                        ));
                        self.skip_unit_specifier();
                    } else {
                        let start = self.current_token().span();
                        unit = self.parse_unit_specifier();
                        let span = start.merge(self.tokens[self.current - 1].span());
                        if let (Some(explicit), Some(implied)) = (unit, implied_unit) {
                            self.check_unit_conflict(explicit, implied, span);
                        }
                    }
                } else if let TokenKind::Identifier(name) = self.current_kind() {
                    let name = name.clone();
                    self.advance();
                    let span = self.tokens[self.current - 1].span();
                    let had_type = segment_type.is_some();
                    self.apply_named_segment_specifier(
                        &name,
                        span,
                        &mut segment_type,
                        &mut signedness,
                        &mut endianness,
                    );
                    // `bits`/`bitstring` imply unit 1 and `bytes` unit 8
                    // (`binary` implies none): `erlc` rejects a different
                    // explicit `unit:`, in either order (BT-4376).
                    if !had_type && let Some(implied) = implied_segment_unit(&name) {
                        implied_unit = Some(implied);
                        if let Some(explicit) = unit {
                            self.check_unit_conflict(implied, explicit, span);
                        }
                    }
                } else if let TokenKind::Keyword(kw) = self.current_kind() {
                    let kw = kw.clone();
                    let span = self.advance().span();
                    self.diagnostics.push(Diagnostic::error(
                        format!("Unknown binary segment specifier: '{kw}'"),
                        span,
                    ));
                    break;
                } else {
                    break;
                }
                // Continue if next is `-` (chained modifiers like `signed-little`)
                if matches!(self.current_kind(), TokenKind::BinarySelector(s) if s.as_str() == "-")
                {
                    self.advance(); // consume `-`
                    if !matches!(
                        self.current_kind(),
                        TokenKind::Identifier(_) | TokenKind::Keyword(_)
                    ) {
                        self.error("Expected a specifier name after '-' in binary segment type");
                        break;
                    }
                } else {
                    break;
                }
            }
        }

        (segment_type, signedness, endianness, unit)
    }

    /// Reports `erlc`'s "conflict in unit specification" when a segment's
    /// explicit `unit:` and the unit its type implies differ. `later` is the
    /// unit given second and `earlier` the first, the order `erlc` names
    /// them in (`<<X:2/bits-unit:8>>` reports '8' and '1'); `span` covers
    /// the later specifier.
    fn check_unit_conflict(
        &mut self,
        later: usize,
        earlier: usize,
        span: crate::source_analysis::Span,
    ) {
        if later != earlier {
            self.diagnostics.push(Diagnostic::error(
                format!("conflict in unit specification for bit field: '{later}' and '{earlier}'"),
                span,
            ));
        }
    }

    /// Applies one named (`Identifier`) segment specifier — a type,
    /// signedness, endianness, or the bare word `unit` (which needs `:N`).
    fn apply_named_segment_specifier(
        &mut self,
        name: &str,
        span: crate::source_analysis::Span,
        segment_type: &mut Option<BinarySegmentType>,
        signedness: &mut Option<BinarySignedness>,
        endianness: &mut Option<BinaryEndianness>,
    ) {
        match name {
            "integer" | "float" | "binary" | "bytes" | "bits" | "bitstring" | "utf8" | "utf16"
            | "utf32" => {
                if segment_type.is_some() {
                    self.diagnostics.push(Diagnostic::error(
                        format!(
                            "Conflicting binary segment type specifier '{name}': type already set"
                        ),
                        span,
                    ));
                } else {
                    *segment_type = Some(match name {
                        "integer" => BinarySegmentType::Integer,
                        "float" => BinarySegmentType::Float,
                        "utf8" => BinarySegmentType::Utf8,
                        "utf16" => BinarySegmentType::Utf16,
                        "utf32" => BinarySegmentType::Utf32,
                        "bits" | "bitstring" => BinarySegmentType::Bitstring,
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
                    *signedness = Some(if name == "signed" {
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
                    *endianness = Some(match name {
                        "big" => BinaryEndianness::Big,
                        "little" => BinaryEndianness::Little,
                        _ => BinaryEndianness::Native, // "native"
                    });
                }
            }
            "unit" => {
                self.diagnostics.push(Diagnostic::error(
                    "'unit' must be followed by ':' and an integer, e.g. 'unit:8'",
                    span,
                ));
            }
            unknown => {
                self.diagnostics.push(Diagnostic::error(
                    format!("Unknown binary segment specifier: '{unknown}'"),
                    span,
                ));
            }
        }
    }

    /// Whether the current token opens a `unit:N` specifier: either
    /// `Keyword("unit:")` (attached colon) or `Identifier("unit")`
    /// followed by `Colon` (spaced).
    fn at_unit_specifier(&self) -> bool {
        match self.current_kind() {
            TokenKind::Keyword(kw) => kw.trim_end_matches(':') == "unit",
            TokenKind::Identifier(name) => {
                name.as_str() == "unit"
                    && matches!(
                        self.tokens.get(self.current + 1).map(Token::kind),
                        Some(TokenKind::Colon)
                    )
            }
            _ => false,
        }
    }

    /// Consumes a `unit:N` specifier and answers its value, or `None` on
    /// error. `erlc` only accepts literal units from 1 to 256.
    fn parse_unit_specifier(&mut self) -> Option<usize> {
        match self.current_kind() {
            TokenKind::Keyword(_) => {
                self.advance(); // consume `unit:`
            }
            TokenKind::Identifier(_) => {
                self.advance(); // consume `unit`
                self.advance(); // consume `:`
            }
            _ => unreachable!("at_unit_specifier implies Keyword or Identifier"),
        }
        let TokenKind::Integer(raw) = self.current_kind() else {
            let span = self.current_token().span();
            self.diagnostics.push(Diagnostic::error(
                "Expected an integer after 'unit:' in binary segment, e.g. 'unit:8'",
                span,
            ));
            return None;
        };
        let raw = raw.clone();
        let span = self.advance().span();
        match parse_integer(&raw) {
            // The guard pins `n` to `1..=256`, so the conversion is exact.
            Ok(n) if (1..=256).contains(&n) => usize::try_from(n).ok(),
            Ok(n) => {
                self.diagnostics.push(Diagnostic::error(
                    format!("binary segment unit must be between 1 and 256, got {n}"),
                    span,
                ));
                None
            }
            Err(e) => {
                self.diagnostics.push(Diagnostic::error(e, span));
                None
            }
        }
    }

    /// Skips a duplicate `unit:N` specifier for error recovery.
    fn skip_unit_specifier(&mut self) {
        match self.current_kind() {
            TokenKind::Keyword(_) => {
                self.advance();
            }
            TokenKind::Identifier(_) => {
                self.advance();
                self.advance();
            }
            _ => unreachable!("at_unit_specifier implies Keyword or Identifier"),
        }
        if matches!(self.current_kind(), TokenKind::Integer(_)) {
            self.advance();
        }
    }
}
