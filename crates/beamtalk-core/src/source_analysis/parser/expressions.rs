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
    BinaryEndianness, BinarySegment, BinarySegmentType, BinarySegmentValue, BinarySignedness,
    Block, BlockParameter, CascadeMessage, Comment, CommentAttachment, Expression,
    ExpressionStatement, Identifier, KeywordPart, Literal, LiteralCommentSlots, MapPair,
    MapPatternKey, MapPatternPair, MatchArm, MessageSelector, Pattern, StringSegment,
    binary_literal_size,
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
    ///
    /// Supports optional type annotations: `x :: Type := expr`.
    fn parse_assignment(&mut self) -> Expression {
        // Tuple destructuring: `{a, b} := expr`. A leading `{` is either a
        // destructure pattern or a tuple-literal expression (`{a, b} size`);
        // bounded lookahead to the matching `}` decides (ADR 0136). The
        // pattern path keeps the full pattern grammar (`x :: T`, binaries,
        // nested maps), which an expression-then-convert approach would lose.
        if self.check(&TokenKind::LeftBrace) && self.leading_brace_is_destructure() {
            return self.parse_tuple_destructure();
        }

        let expr = self.parse_cascade();

        // Check for type-annotated assignment: `identifier :: Type := expr`
        if matches!(self.current_kind(), TokenKind::DoubleColon) {
            // Only identifiers may carry a local type annotation.
            // Destructuring (`{a, b} :: Type := ...`) is not supported yet.
            if matches!(expr, Expression::Identifier(_)) {
                return self.parse_annotated_assignment(expr);
            }
            // Non-identifier with `::` — consume the annotation to avoid token desync
            let span = self.current_token().span();
            self.diagnostics.push(Diagnostic::error(
                "Type annotations on destructuring assignments are not yet supported",
                span,
            ));
            self.advance(); // consume `::`
            let _ = self.parse_type_annotation();
            if self.match_token(&TokenKind::Assign) {
                let _ = self.parse_cascade();
            }
            return Expression::Error {
                message: "Type annotations on non-identifier assignments are not supported".into(),
                span,
            };
        }

        // Check for assignment operator
        if self.match_token(&TokenKind::Assign) {
            return self.parse_assign_rhs(expr);
        }

        expr
    }

    /// Parses a tuple destructuring assignment (`{a, b} := expr`).
    ///
    /// Only reached when [`Self::leading_brace_is_destructure`] has seen `:=`
    /// after the matching `}`, so the `:=` is expected here.
    fn parse_tuple_destructure(&mut self) -> Expression {
        let start = self.current_token().span();
        let pattern = self.parse_tuple_pattern();
        self.expect(&TokenKind::Assign, "Expected ':=' after tuple pattern");
        if let Err(error) = self.enter_nesting(start) {
            return error;
        }
        let value = Box::new(self.parse_assignment());
        self.leave_nesting();
        let span = start.merge(value.span());
        Expression::DestructureAssignment {
            pattern,
            value,
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
        // List destructuring: `#(a, b) := expr`
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
            type_annotation: None,
            span,
        }
    }

    /// Parses a type-annotated assignment: `identifier :: Type := expr`.
    ///
    /// Called when the current token is `::` and the LHS is an identifier.
    /// Consumes the `::`, parses the type annotation, expects `:=`, then
    /// parses the RHS value.
    fn parse_annotated_assignment(&mut self, target: Expression) -> Expression {
        let start = target.span();
        self.advance(); // consume `::`
        let annotation = self.parse_type_annotation();

        if !self.match_token(&TokenKind::Assign) {
            let span = self.current_token().span();
            self.diagnostics.push(Diagnostic::error(
                "Expected ':=' after type annotation in assignment",
                span,
            ));
            return Expression::Error {
                message: "Expected ':=' after type annotation".into(),
                span: start.merge(span),
            };
        }

        // Guard recursive call against stack overflow
        if let Err(error) = self.enter_nesting(start) {
            return error;
        }
        let value = Box::new(self.parse_assignment());
        self.leave_nesting();
        let span = start.merge(value.span());
        Expression::Assignment {
            target: Box::new(target),
            value,
            type_annotation: Some(annotation),
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

    /// Parses a cascade (multiple messages to the same receiver) or a
    /// pipeline (each stage sent to the previous stage's result).
    ///
    /// Syntax: `receiver message1; message2; message3` or
    /// `receiver message1 |> message2 |> message3` (ADR 0145).
    fn parse_cascade(&mut self) -> Expression {
        let head = self.parse_keyword_message();

        if self.check(&TokenKind::PipeGt) {
            let pipeline = self.parse_pipeline(head);
            if self.check(&TokenKind::Semicolon) {
                self.report_cascade_pipeline_mix();
                // Recover: parse the cascade onto the pipeline's last send
                // so its messages do not surface as stray statements.
                self.advance();
                return self.parse_cascade_messages(pipeline);
            }
            return pipeline;
        }

        // Check if this is a cascade
        if !self.match_token(&TokenKind::Semicolon) {
            return head;
        }
        let cascade = self.parse_cascade_messages(head);
        if self.check(&TokenKind::PipeGt) {
            self.report_cascade_pipeline_mix();
            // Recover: consume the stages so they do not surface as a stray
            // statement; the program is in error, so the cascade is answered.
            let mark = self.pipeline_sends.len();
            let _ = self.parse_pipeline(cascade.clone());
            self.pipeline_sends.truncate(mark);
        }
        cascade
    }

    /// Reports `;` and `|>` used in one expression without parentheses
    /// (ADR 0145), at the current (offending) token.
    fn report_cascade_pipeline_mix(&mut self) {
        let span = self.current_token().span();
        self.diagnostics.push(
            Diagnostic::error(
                "cannot mix cascade `;` and pipeline `|>` in one expression; \
                 parenthesise the part you mean",
                span,
            )
            .with_hint("write `(a m1 |> m2) m3; m4`, or bind the pipeline to a local first"),
        );
    }

    /// Parses a full keyword expression: a keyword message, optionally
    /// followed by `|>` pipeline stages (ADR 0145 § Precedence and layout).
    ///
    /// This is the base for every site that must not read `;` as a cascade:
    /// string interpolation, `match:` arm bodies (where `;` ends the arm), map
    /// values and list, array and tuple elements. The `;`/`|>` mixing rule
    /// does not apply here, since `;` is not a cascade at these sites.
    fn parse_keyword_expression(&mut self) -> Expression {
        let head = self.parse_keyword_message();
        self.parse_pipeline(head)
    }

    /// Parses `( '|>' stage )*` after `head`, building the left-nested sends
    /// it means: `recv m1 |> m2 |> m3` is `((recv m1) m2) m3` (ADR 0145).
    ///
    /// There is no pipeline node. Each stage is parsed as an ordinary keyword
    /// expression whose receiver is the previous stage's value, so the tree
    /// is the one the hand-nested form builds, with the same
    /// `receiver.span().merge(message end)` spans. The send each `|>`
    /// introduces is recorded, with the comments written before that `|>`,
    /// in `Parser::pipeline_sends`, so the unparser can print the `|>` back.
    fn parse_pipeline(&mut self, head: Expression) -> Expression {
        let mut value = head;
        while self.check(&TokenKind::PipeGt) {
            // Comments between the previous stage and this `|>` attach to the
            // send this `|>` introduces (see `ast::pipeline`): an end-of-line
            // comment after the previous stage, and comments on their own
            // lines before the `|>`.
            let mut comments = self.collect_comment_attachment();
            comments.trailing = self.collect_trailing_comment();
            self.advance(); // consume `|>`
            // Comments between a trailing `|>` and the stage on a later line
            // (an end-of-line comment after the `|>`, then comments on their
            // own lines before the stage) belong to that stage too. They join
            // the leading comments, so they are kept, and the unparser prints
            // them before the `|>`.
            if let Some(after_pipe) = self.collect_trailing_comment() {
                comments.leading.push(after_pipe);
            }
            comments
                .leading
                .extend(self.collect_comment_attachment().leading);
            value = self.parse_pipeline_stage(value, comments);
        }
        value
    }

    /// Parses one pipeline stage with `receiver` (the previous stage's value)
    /// supplied:
    ///
    /// ```text
    /// stage := unary-message+ binary-tail? keyword-message? | keyword-message
    /// ```
    ///
    /// So `|> keys sorted`, `|> foo at: 1` and `|> size + 1` are each one
    /// stage. This is the keyword-expression parser with the receiver
    /// supplied, not [`Self::parse_cascade_message`], which parses a single
    /// message. The keyword tail keeps `should_stop_keyword_continuation`, so
    /// a final keyword stage does not swallow the next method header.
    fn parse_pipeline_stage(
        &mut self,
        receiver: Expression,
        comments: CommentAttachment,
    ) -> Expression {
        let receiver_span = receiver.span();
        match self.current_kind() {
            TokenKind::Identifier(name) => {
                // The first unary message of a stage may sit on the line
                // after a trailing `|>`; later ones follow the usual rule.
                let selector = MessageSelector::Unary(name.clone());
                let tok = self.advance();
                let first = Expression::MessageSend {
                    receiver: Box::new(receiver),
                    selector,
                    arguments: Vec::new(),
                    is_cast: false,
                    span: receiver_span.merge(tok.span()),
                };
                self.pipeline_sends.push((first.span(), comments));
                let unary = self.parse_unary_tail(first);
                let binary = self.parse_binary_tail(unary, 0);
                self.parse_keyword_tail(binary)
            }
            TokenKind::Keyword(_) => {
                let send = self.parse_keyword_tail(receiver);
                if send.span() == receiver_span {
                    // The keyword starts the next class member, not a message.
                    self.report_missing_pipeline_stage();
                } else {
                    self.pipeline_sends.push((send.span(), comments));
                }
                send
            }
            TokenKind::BinarySelector(_) | TokenKind::Arrow | TokenKind::GtGt => {
                let token = self.current_token();
                let span = token.span();
                let operator = token.kind().to_string();
                self.diagnostics.push(
                    Diagnostic::error(
                        format!(
                            "a pipeline stage cannot start with a binary message; \
                             write `(…) {operator} …`"
                        ),
                        span,
                    )
                    .with_hint(format!("parenthesise the left side: `(…) {operator} …`")),
                );
                // Recover: parse the binary tail so its operand does not
                // surface as a stray statement.
                let binary = self.parse_binary_tail(receiver, 0);
                self.parse_keyword_tail(binary)
            }
            _ => {
                self.report_missing_pipeline_stage();
                receiver
            }
        }
    }

    /// Reports a `|>` where an expression must start, then consumes the
    /// stages that follow so they do not surface as stray statements.
    fn parse_leading_pipe(&mut self) -> Expression {
        let span = self.current_token().span();
        let message: EcoString = "a statement cannot start with `|>`".into();
        self.diagnostics
            .push(Diagnostic::error(message.clone(), span).with_hint(
                "`|>` continues the expression before it. In a source file, a line \
             starting with `|>` continues the line above; in the REPL that line \
             was already run, so wrap the whole pipeline in `( … )` or end the \
             line above with `|>`",
            ));
        // Recover: parse the stages onto an error receiver, and drop their
        // re-sugaring entries, since the program is in error.
        let mark = self.pipeline_sends.len();
        let recovered = self.parse_pipeline(Expression::Error {
            message: message.clone(),
            span,
        });
        self.pipeline_sends.truncate(mark);
        Expression::Error {
            message,
            span: span.merge(recovered.span()),
        }
    }

    /// Reports a `|>` not followed by a message, at the current token.
    fn report_missing_pipeline_stage(&mut self) {
        let span = self.current_token().span();
        self.diagnostics.push(
            Diagnostic::error("expected a message after `|>`", span).with_hint(
                "`|>` sends a message to the previous result; it does not apply a function",
            ),
        );
    }

    /// Parses the `; message` parts of a cascade whose first `;` has been
    /// consumed, onto `receiver` (the cascade's first send).
    fn parse_cascade_messages(&mut self, receiver: Expression) -> Expression {
        let mut messages = Vec::new();

        loop {
            let message = self.parse_cascade_message();
            messages.push(message);

            // Continue if there's another semicolon
            if !self.match_token(&TokenKind::Semicolon) {
                break;
            }
        }

        self.report_trailing_cascade_messages();

        let last_span = messages.last().map_or(receiver.span(), |m| m.span);
        let span = receiver.span().merge(last_span);

        Expression::Cascade {
            receiver: Box::new(receiver),
            messages,
            span,
        }
    }

    /// Reports message tokens left on the same line after the last cascade
    /// part (BT-4508).
    ///
    /// A cascade part is exactly one message, so in `x foo; size reversed`
    /// the `reversed` cannot continue the `size` part. Without this check it
    /// would silently start a new statement and the program would mean
    /// something else. A message token on a *new* line is left alone: it
    /// starts the next statement as usual.
    ///
    /// The trailing chain is consumed (unary, binary and keyword messages on
    /// the same line) so that it does not produce follow-up diagnostics as a
    /// stray statement.
    fn report_trailing_cascade_messages(&mut self) {
        let is_message_start = |kind: &TokenKind| {
            matches!(
                kind,
                TokenKind::Identifier(_)
                    | TokenKind::Keyword(_)
                    | TokenKind::BinarySelector(_)
                    | TokenKind::Arrow
                    | TokenKind::GtGt
            )
        };
        if self.current_token().has_leading_newline() || !is_message_start(self.current_kind()) {
            return;
        }

        let start = self.current_token().span();
        let mut end = start;
        while !self.current_token().has_leading_newline() {
            match self.current_kind() {
                TokenKind::Identifier(_) => end = self.advance().span(),
                TokenKind::BinarySelector(_) | TokenKind::Arrow | TokenKind::GtGt => {
                    self.advance();
                    end = self.parse_unary_message().span();
                }
                TokenKind::Keyword(_) => {
                    self.advance();
                    end = self.parse_binary_message().span();
                }
                _ => break,
            }
        }

        self.diagnostics.push(
            Diagnostic::error(
                "A cascade part is a single message; this message would not be sent to the \
                 result of the cascade part before it",
                start.merge(end),
            )
            .with_hint(
                "parenthesise the chain, as in `(x size) reversed`, or start a new statement \
                 on a new line",
            ),
        );
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
            // Mirrors parse_keyword_message's own continuation_indent —
            // without this, a keyword on a new line that actually starts the next
            // sibling class member (e.g. `nextMethod: x => ...` right after this
            // cascade message) gets silently swallowed as another keyword part of
            // THIS message instead of stopping here.
            let mut continuation_indent: Option<usize> = None;

            while let TokenKind::Keyword(keyword) = self.current_kind() {
                if self.should_stop_keyword_continuation(
                    &mut continuation_indent,
                    !keywords.is_empty(),
                ) {
                    break;
                }
                let span = self.current_token().span();
                keywords.push(KeywordPart::new(keyword.clone(), span));
                self.advance();

                // Parse argument (which is a binary message)
                arguments.push(self.parse_binary_message());
            }

            if keywords.is_empty() {
                // The very first keyword token was itself a class-member boundary
                // (e.g. a stray trailing `;` in a cascade immediately followed by
                // the next sibling method) — there is no message here to parse.
                self.error("Expected message selector in cascade");
                return CascadeMessage::new(
                    MessageSelector::Unary("error".into()),
                    Vec::new(),
                    start_span,
                );
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

    /// Returns true if the parser should stop consuming further
    /// keyword parts of the keyword message currently being parsed, because the
    /// current token — a `Keyword` with a leading newline — actually begins a new
    /// sibling class member (or, outside a class body, a new top-level construct)
    /// rather than continuing this message across multiple lines.
    ///
    /// Shared by `parse_keyword_message`'s own multi-keyword loop and
    /// `parse_cascade_message`'s keyword-message branch — both parse a sequence of
    /// `keyword: arg` parts and must stop at the same class-member boundary.
    /// Without this check, a keyword on a new line that is actually the start
    /// of the next sibling method (e.g. `nextMethod: x => ...` right after a
    /// cascade message) would be silently swallowed as another keyword part
    /// of the cascade message instead.
    ///
    /// `continuation_indent` must be a `&mut Option<usize>` local to the caller's
    /// own keyword-parsing loop (each loop tracks its own establishment of "how
    /// deep is a genuine continuation line for THIS message"). `has_prior_keywords`
    /// mirrors `parse_keyword_message`'s pre-existing `!keywords.is_empty()` guard
    /// for the outside-class-body lookahead path.
    fn should_stop_keyword_continuation(
        &self,
        continuation_indent: &mut Option<usize>,
        has_prior_keywords: bool,
    ) -> bool {
        if !self.current_token().has_leading_newline() {
            return false;
        }
        if self.in_class_body && self.in_method_body {
            // Inside a class method body: use indentation comparison. Relies on
            // the canonical ≤ 2 column invariant for class members.
            let current_indent = self.current_token().indentation_after_newline();
            match *continuation_indent {
                None => {
                    // First keyword on a new line for this message. Stop
                    // immediately if it is at the class-member level (col ≤ 2),
                    // otherwise record it as the continuation level.
                    let col = current_indent.unwrap_or(0);
                    if col <= 2 {
                        return true;
                    }
                    *continuation_indent = Some(col);
                    false
                }
                Some(ci) => {
                    // Treat None as col 0: a token with no indentation info (or a
                    // col-0 newline) is shallower than any continuation.
                    let ind = current_indent.unwrap_or(0);
                    ind < ci
                }
            }
        } else {
            // Outside a class method body: original lookahead-based check.
            //
            // Known asymmetry: unlike parse_keyword_message, which has an explicit
            // pre-loop guard for this same path, this helper alone can't stop on
            // the very *first* keyword when has_prior_keywords is false (a cascade
            // followed by a would-be method definition outside any class body).
            // Not believed reachable — a cascade directly followed by a top-level
            // method definition isn't a real construct — so left as-is rather than
            // adding an unreachable-in-practice pre-loop guard to this helper too.
            has_prior_keywords && self.is_at_method_definition()
        }
    }

    /// Parses a keyword message (lowest precedence).
    pub(super) fn parse_keyword_message(&mut self) -> Expression {
        let receiver = self.parse_binary_message();
        self.parse_keyword_tail(receiver)
    }

    /// Parses an optional keyword message sent to an already-parsed
    /// `receiver`, answering `receiver` itself when no keyword message
    /// follows (or the keyword starts the next class member).
    fn parse_keyword_tail(&mut self, receiver: Expression) -> Expression {
        // Check if this is a keyword message.
        // A keyword token can never validly start a new statement on its own
        // (it always requires a receiver), so a keyword on a new line is
        // unambiguously a continuation of the previous expression.
        // The one exception is a keyword that begins a method definition
        // (`keyword: param =>`), which must not be consumed as a continuation.
        //
        // Inside a class method body `is_at_method_definition()` produces
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
                // false positives. Non-canonically-indented source (e.g.
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

        // Special handling for `match:` / `matchExhaustive:` — both produce
        // Expression::Match; the latter sets `exhaustive: true`
        // (ADR 0106: opt-in asserted exhaustiveness).
        if matches!(self.current_kind(), TokenKind::Keyword(k)
            if k.as_str() == "match:" || k.as_str() == "matchExhaustive:")
            && self.peek_at(1) == Some(&TokenKind::LeftBracket)
        {
            let exhaustive = matches!(self.current_kind(), TokenKind::Keyword(k) if k.as_str() == "matchExhaustive:");
            return self.parse_match_expression(receiver, exhaustive);
        }

        // Parse keyword message
        let mut keywords = Vec::new();
        let mut arguments = Vec::new();
        // When inside a class method body, track the indentation of the
        // first keyword that appears on a new line.  Any subsequent keyword at a
        // *lesser* indentation is at the class-member level (col ≤ 2) and signals
        // the start of a sibling method definition rather than a continuation.
        // Outside class method bodies the original `is_at_method_definition()`
        // lookahead is used (methods cannot nest anyway).
        let mut continuation_indent: Option<usize> = None;

        while let TokenKind::Keyword(keyword) = self.current_kind() {
            if self.should_stop_keyword_continuation(&mut continuation_indent, !keywords.is_empty())
            {
                break;
            }
            let span = self.current_token().span();
            keywords.push(KeywordPart::new(keyword.clone(), span));
            self.advance();

            // Parse argument (which is a binary message)
            arguments.push(self.parse_binary_message());
        }

        // Safety: the pre-loop guard above returns early for any leading-newline
        // boundary keyword, so the `TokenKind::Keyword` check at the top of this
        // function plus that guard together guarantee the while loop consumes at
        // least one keyword part, making `arguments` non-empty here.
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
        let left = self.parse_unary_message();
        self.parse_binary_tail(left, min_bp)
    }

    /// Parses the binary messages, binding at least `min_bp` tightly, sent to
    /// an already-parsed `left` operand.
    fn parse_binary_tail(&mut self, mut left: Expression, min_bp: u8) -> Expression {
        // `<<` is not a binary operator: only a leading `<<` (in operand
        // position) means anything, opening a binary construction (ADR
        // 0143). One right after an operand on the same line is reported
        // here rather than being read as a second statement that starts a
        // construction. On a new line it does start the next statement.
        if matches!(self.current_kind(), TokenKind::LtLt)
            && !self.current_token().has_leading_newline()
        {
            left = self.report_infix_ltlt(&left);
        }

        // Extract binary operator string from BinarySelector, Arrow (ADR 0047), or GtGt.
        // Arrow (`->`) is treated as the binary operator `"->"`.
        // GtGt (`>>`) is treated as the binary operator `">>"` (method lookup).
        while let Some(op) = match self.current_kind() {
            TokenKind::BinarySelector(op) => Some(op.clone()),
            TokenKind::Arrow => Some("->".into()),
            TokenKind::GtGt => Some(">>".into()),
            _ => None,
        } {
            // A binary selector on a new line that looks like a method definition
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

    /// Reports a `<<` that follows an operand (`a << b`) and consumes it with
    /// its right operand, answering an error node spanning both sides.
    fn report_infix_ltlt(&mut self, left: &Expression) -> Expression {
        let op_span = self.advance().span(); // consume `<<`
        let message: EcoString = "'<<' is not a binary operator".into();
        self.diagnostics.push(
            Diagnostic::error(message.clone(), op_span).with_hint(
                "a leading '<<' opens a binary construction, as in <<v:8, rest/binary>>; to combine binaries use ++",
            ),
        );
        let right = self.parse_unary_message();
        Expression::Error {
            message,
            span: left.span().merge(right.span()),
        }
    }

    /// Parses unary messages (highest message precedence).
    pub(super) fn parse_unary_message(&mut self) -> Expression {
        let receiver = self.parse_primary();
        self.parse_unary_tail(receiver)
    }

    /// Parses the unary messages on the current line sent to an
    /// already-parsed `receiver`.
    fn parse_unary_tail(&mut self, mut receiver: Expression) -> Expression {
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

            // Tuple literal: `{a, b}` (ADR 0136)
            TokenKind::LeftBrace => self.parse_tuple_literal(),

            // Binary construction: `<<16:8, payload/binary>>` (ADR 0143).
            // Only a leading `<<` reaches here: `parse_primary` runs where an
            // operand is expected, while an infix `<<` after an operand is
            // seen by the binary-operator loop, never by this dispatch.
            TokenKind::LtLt => self.parse_binary_construction(),

            // Primitive pragma: @primitive/@intrinsic 'name' or @primitive/@intrinsic intrinsicName
            TokenKind::AtPrimitive | TokenKind::AtIntrinsic => self.parse_primitive(),

            // Diagnostic suppression directive: @expect category
            TokenKind::AtExpect => self.parse_expect_directive(),

            // Bare `=` is not a valid operator: equality is `=:=`
            // (value) or `==` (reference); give a targeted hint instead of
            // the generic "expected expression" fallback below.
            TokenKind::BinarySelector(s) if s == "=" => {
                let bad_token = self.advance();
                let span = bad_token.span();
                let message: EcoString = "Unexpected token: expected expression, found =".into();
                self.diagnostics.push(
                    Diagnostic::error(message.clone(), span).with_hint(
                        "'=' is not an operator in Beamtalk; use '=:=' for value equality or '==' for reference equality",
                    ),
                );
                Expression::Error { message, span }
            }

            // `|>` continues the expression before it, so it cannot start
            // one (ADR 0145). In a source file a line starting with `|>`
            // continues the line above; in the REPL that line was already
            // submitted, which is the usual way to get here.
            TokenKind::PipeGt => self.parse_leading_pipe(),

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
                match s.parse::<f64>() {
                    // Reject values that overflow `f64` to `inf`/`-inf` (e.g. `1e309`)
                    // or that parse to `NaN`. A non-finite literal would otherwise reach
                    // codegen and emit `inf.0` / `NaN.0`, which Core Erlang cannot parse,
                    // surfacing as a confusing `erlc` failure instead of a diagnostic here.
                    Ok(val) if val.is_finite() => Literal::Float(val),
                    Ok(_) => {
                        self.diagnostics.push(Diagnostic::error(
                            format!("Float literal out of range: {s}"),
                            span,
                        ));
                        return Expression::Error {
                            message: "Float literal out of range".into(),
                            span,
                        };
                    }
                    Err(_) => {
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
                let expr = self.parse_keyword_expression();
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
        let name = name.clone();
        let span = name_token.span();

        let mut expr = self.identifier_expression(name, span);

        // Check for field access: identifier.field (see
        // `at_attached_field_access`).
        while self.at_attached_field_access(false) {
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

    /// The expression a name stands for: `super`, a class reference
    /// (uppercase first letter), a package-qualified class reference
    /// (`json@Parser`, when an `@` follows), or a variable. Shared by the
    /// ordinary operand path and binary construction segment values, so a
    /// name means the same thing in both.
    fn identifier_expression(&mut self, name: EcoString, span: Span) -> Expression {
        if name.as_str() == "super" {
            return Expression::Super(span);
        }
        // Distinguish class references (uppercase) from regular identifiers.
        // Uses Unicode-aware `char::is_uppercase()` to support international class names
        // like `Über`, `Ñandú`, etc., following Rust's Unicode support philosophy.
        // Class names: uppercase first letter (Counter, MyClass, Über)
        // Variables: lowercase first letter (counter, myVar, über)
        if name.chars().next().is_some_and(char::is_uppercase) {
            // Class reference (e.g., Counter, Array, MyClass)
            Expression::ClassReference {
                name: Identifier::new(name, span),
                package: None,
                span,
            }
        } else if self.check(&TokenKind::At) {
            // Package-qualified class reference: `json@Parser` (ADR 0070, Section 4)
            self.parse_package_qualified_class_reference(&name, span)
        } else {
            // Regular identifier (variable)
            Expression::Identifier(Identifier::new(name, span))
        }
    }

    /// Whether the current `.` starts a field access (`self.value`) rather
    /// than ending a statement: the next token is an identifier (or, with
    /// `keyword_field`, a keyword such as the `len:` of `<<self.len:8>>`)
    /// and there is no whitespace on either side of the period.
    ///
    /// This distinguishes `self.value` (field access) from `n. self`
    /// (statement separator followed by a new expression). The lexer puts
    /// trailing same-line whitespace (spaces/tabs) as trailing trivia on the
    /// period, and newlines + subsequent whitespace as leading trivia on the
    /// next token, so both are checked:
    /// - `n. self` → period has trailing space
    /// - `n.\n    self` → identifier has leading newline+spaces
    fn at_attached_field_access(&self, keyword_field: bool) -> bool {
        self.check(&TokenKind::Period)
            && match self.peek_kind() {
                Some(TokenKind::Identifier(_)) => true,
                Some(TokenKind::Keyword(_)) => keyword_field,
                _ => false,
            }
            && self.current_token().trailing_trivia().is_empty()
            && self
                .peek()
                .is_some_and(|t: &Token| t.leading_trivia().is_empty())
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
            .map_or_else(|| self.current_token().span(), |t: Token| t.span());

        let mut parameters = Vec::new();

        // Parse block parameters: [:x :y | ...] or [:x :: Type :y :: Type | ...]
        //
        // Optional `:: Type` annotations after each parameter are accepted
        // and stored on the parameter so `beamtalk fmt` round-trips them
        // (BT-4108). Block parameter types are still inferred by the type
        // checker from the message signature rather than from these
        // annotations; storing (rather than merely consuming, as was done
        // for BT-2042 / BT-2043 to avoid cascading parse errors and false
        // "Unused variable" warnings) loses no information.
        //
        // Use `parse_single_type_annotation` — not `parse_type_annotation` —
        // because union `|` would otherwise swallow the block's `|` separator.
        // Callers that need a union type on a block parameter can wrap the
        // type in grouping parentheses, e.g. `:x :: (Integer | String)`.
        while self.match_token(&TokenKind::Colon) {
            if let TokenKind::Identifier(name) = self.current_kind() {
                let span = self.current_token().span();
                let name = name.clone();
                self.advance();

                // Optional `:: Type` annotation.
                if self.match_token(&TokenKind::DoubleColon) {
                    let annotation = self.parse_single_type_annotation();
                    parameters.push(BlockParameter::with_type(name, annotation, span));
                } else {
                    parameters.push(BlockParameter::new(name, span));
                }
            } else {
                self.error("Expected parameter name after ':'");
                break;
            }
        }

        // Expect pipe separator if there are parameters
        if !parameters.is_empty() {
            self.expect(&TokenKind::Pipe, "Expected '|' after block parameters");
        }

        // Parse block body — statements separated by periods or newlines
        let mut body = Vec::new();
        while !self.check(&TokenKind::RightBracket) && !self.is_at_end() {
            let pos_before = self.current;
            // Detect blank lines (2+ newlines) before this statement
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
                // Explicit period — check whether it is redundant
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
                self.apply_cast_terminator(body.last_mut().map(|s| &mut s.expression));
                // Continue (same as period)
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
            .expect(&TokenKind::RightBracket, "Expected ']' to close block")
            .map_or(start, |t: Token| t.span());

        let span = start.merge(end);
        // A parsed block is never zero-width (its span covers at least `[`):
        // `ast::normalize::written_unary_loop` reads a zero-width empty block as
        // the body it added to a unary `whileTrue`/`whileFalse` (BT-4313).
        let block = Block::new(parameters, body, span);
        Expression::Block(block)
    }

    /// Parses a match expression: `receiver match: [pattern -> body ...]`
    /// or, when `exhaustive` is set, `receiver matchExhaustive: [pattern -> body ...]`
    /// (ADR 0106: opt-in asserted exhaustiveness).
    ///
    /// The receiver has already been parsed and the `match:`/`matchExhaustive:`
    /// keyword is the current token.
    fn parse_match_expression(&mut self, receiver: Expression, exhaustive: bool) -> Expression {
        self.advance(); // consume `match:` / `matchExhaustive:` keyword

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
            exhaustive,
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
            let value = Box::new(self.parse_keyword_expression());
            let end = value.span();
            Expression::Return {
                value,
                span: start.merge(end),
            }
        } else {
            let expr = self.parse_keyword_expression();
            if self.match_token(&TokenKind::Assign) {
                // Build assignment inline using keyword-message level for
                // the RHS — parse_assign_rhs would go through parse_cascade
                // which consumes `;` (arm separators).
                if matches!(
                    expr,
                    Expression::Identifier(_) | Expression::FieldAccess { .. }
                ) {
                    let value = Box::new(self.parse_keyword_expression());
                    let span = expr.span().merge(value.span());
                    Expression::Assignment {
                        target: Box::new(expr),
                        value,
                        type_annotation: None,
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
    fn parse_tuple_pattern(&mut self) -> Pattern {
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

    /// Parses a binary pattern: `<<seg, seg, ...>>`
    ///
    /// Segment syntax: `varname`, `varname:size`, `varname/type`, `varname:size/type-modifiers`
    /// where type is one of `integer`, `float`, `binary`, `bits`/`bitstring`, `utf8`
    /// and modifiers include `signed`/`unsigned`, `big`/`little`/`native`.
    /// The value may also be an integer literal (`0:1`, BT-4097) and the size
    /// a parenthesized expression (`v:(8 - p)`, BT-4097).
    fn parse_binary_pattern(&mut self) -> Pattern {
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
    fn parse_binary_construction(&mut self) -> Expression {
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
        if let Some((n, span)) = size_lit {
            if n < 0 {
                self.diagnostics.push(Diagnostic::error(
                    format!("binary segment sizes must be non-negative, got {n}"),
                    span,
                ));
                return;
            }
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
            if let (Literal::Character(c), BinarySegmentType::Integer) = (lit, seg_type) {
                if is_eight_bits_wide(size.is_none(), size_lit, unit) {
                    self.warn_non_ascii_byte_character(*c, value_span, direction);
                }
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
                    if !had_type {
                        if let Some(implied) = implied_segment_unit(&name) {
                            implied_unit = Some(implied);
                            if let Some(explicit) = unit {
                                self.check_unit_conflict(implied, explicit, span);
                            }
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

    /// Whether the current token is the `no_user_code` mark of a primitive pragma
    /// (ADR 0137 §4), written on the pragma's own line.
    fn at_no_user_code_flag(&self) -> bool {
        matches!(self.current_kind(), TokenKind::Identifier(name) if name.as_str() == "no_user_code")
            && !self.current_token().has_leading_newline()
    }

    /// Consumes the optional `no_user_code` mark that ends a primitive pragma;
    /// answers whether it was there and the pragma's span with it.
    fn parse_no_user_code_flag(&mut self, span: Span) -> (bool, Span) {
        if self.at_no_user_code_flag() {
            let flag = self.advance();
            (true, span.merge(flag.span()))
        } else {
            (false, span)
        }
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

        // An explicit selector (quoted string or bare intrinsic identifier) is
        // only consumed when it appears on the *same* line as the directive.
        // A token on a following line is a separate statement (e.g. fallback
        // code), so a bare `@primitive` at end of line infers its selector
        // from the enclosing method.
        let has_explicit_name = matches!(
            self.current_kind(),
            TokenKind::String(_) | TokenKind::Identifier(_)
        ) && !self.current_token().has_leading_newline()
            && !self.at_no_user_code_flag();

        if has_explicit_name {
            match self.current_kind() {
                // Quoted selector: @primitive "+"
                TokenKind::String(name) => {
                    let name = name.clone();
                    let end_token = self.advance();
                    let (no_user_code, span) =
                        self.parse_no_user_code_flag(start.merge(end_token.span()));
                    Expression::Primitive {
                        name,
                        is_quoted: true,
                        is_intrinsic,
                        is_inferred: false,
                        no_user_code,
                        span,
                    }
                }
                // Bare identifier: @primitive basicNew
                TokenKind::Identifier(name) => {
                    let name = name.clone();
                    let end_token = self.advance();
                    let (no_user_code, span) =
                        self.parse_no_user_code_flag(start.merge(end_token.span()));
                    Expression::Primitive {
                        name,
                        is_quoted: false,
                        is_intrinsic,
                        is_inferred: false,
                        no_user_code,
                        span,
                    }
                }
                // Unreachable: `has_explicit_name` guarantees String/Identifier.
                _ => unreachable!("has_explicit_name implies String or Identifier"),
            }
        } else if let Some(selector) = self
            .current_method_selector
            .clone()
            // Only `@primitive` infers its selector. `@intrinsic` names a
            // structural intrinsic (`blockValue`, `basicNew`, …) that is never
            // the method's own selector, so a bare `@intrinsic` must still error
            // rather than silently infer the wrong intrinsic.
            .filter(|_| !is_intrinsic)
        {
            // Bare `@primitive` — infer the selector from the enclosing method.
            // This yields the same AST as the explicit quoted form
            // (`is_quoted: true`), so downstream codegen is identical; only
            // unparsing distinguishes it via `is_inferred`.
            let (no_user_code, span) = self.parse_no_user_code_flag(start);
            Expression::Primitive {
                name: selector,
                is_quoted: true,
                is_intrinsic,
                is_inferred: true,
                no_user_code,
                span,
            }
        } else {
            let span = start;
            let message: EcoString =
                format!("{directive} must be followed by a quoted selector or identifier").into();
            self.diagnostics
                .push(Diagnostic::error(message.clone(), span));
            Expression::Error { message, span }
        }
    }

    /// Parses an `@expect category` or `@expect category1, category2 "reason"`
    /// directive.
    ///
    /// The `@expect` token has already been identified by `parse_primary`.
    /// This method consumes it and delegates to
    /// [`Parser::parse_expect_tail`] (`declarations.rs`) for the shared
    /// category-list/reason grammar, which that helper shares with
    /// the declaration-level `@expect` form. If no valid category was found
    /// (unknown name, or nothing following `@expect` at all), this becomes
    /// an `Expression::Error` rather than an
    /// `ExpectDirective` that would silently suppress every diagnostic on
    /// the next expression.
    fn parse_expect_directive(&mut self) -> Expression {
        let start_token = self.advance(); // consume AtExpect
        let (categories, reason, span, error_message) = self.parse_expect_tail(start_token.span());
        if categories.is_empty() {
            let message = error_message.unwrap_or_else(|| "invalid @expect directive".into());
            Expression::Error { message, span }
        } else {
            Expression::ExpectDirective {
                categories,
                reason,
                span,
            }
        }
    }

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
            let item_comments = self.collect_literal_item_leading(&slots, open_idx);

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

        // Expect closing brace
        if self.check(&TokenKind::RightBrace) {
            self.collect_literal_closing(&mut slots, open_idx);
        }
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

        // Expect closing paren
        if self.check(&TokenKind::RightParen) {
            self.collect_literal_closing(&mut slots, open_idx);
        }
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
    fn parse_array_literal(&mut self) -> Expression {
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

        // Expect closing bracket
        if self.check(&TokenKind::RightBracket) {
            self.collect_literal_closing(&mut slots, open_idx);
        }
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
        self.record_literal_comments(span, slots);
        Expression::ArrayLiteral { elements, span }
    }

    /// Parses a tuple literal: `{expr, expr, ...}` (ADR 0136).
    ///
    /// Evaluates to a raw Erlang tuple (class `Tuple`). Elements are parsed
    /// as keyword messages, like the other collection literals; a trailing
    /// comma is allowed, and `{}` / `{x}` are valid. There is no `...`
    /// spread form.
    fn parse_tuple_literal(&mut self) -> Expression {
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

        if self.check(&TokenKind::RightBrace) {
            self.collect_literal_closing(&mut slots, open_idx);
        }
        let end_span = if self.check(&TokenKind::RightBrace) {
            self.advance().span()
        } else {
            self.diagnostics.push(Diagnostic::error(
                "Expected '}' to close tuple literal",
                self.current_token().span(),
            ));
            self.current_token().span()
        };

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
        if slots.items.is_empty() {
            if let Some(after_open) = self.literal_open_trailing(open_idx) {
                comments.leading.insert(0, after_open);
            }
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

    /// Records the comments before the closing delimiter (the current token)
    /// of the literal being parsed (BT-3983). For an empty literal this
    /// includes an end-of-line comment after the opening delimiter.
    fn collect_literal_closing(&self, slots: &mut LiteralCommentSlots, open_idx: usize) {
        if slots.items.is_empty() {
            if let Some(after_open) = self.literal_open_trailing(open_idx) {
                slots.closing.push(after_open);
            }
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

    /// Parses a parenthesized expression.
    fn parse_parenthesized(&mut self) -> Expression {
        let start = self
            .expect(&TokenKind::LeftParen, "Expected '('")
            .map_or_else(|| self.current_token().span(), |t: Token| t.span());

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
    // Bare-word map key error tests
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

    // ── ADR 0106: `matchExhaustive:` parsing ─────────────────────

    #[test]
    fn plain_match_sets_exhaustive_false() {
        let (module, diags) = parse_source("x match: [#a -> 1; _ -> 0]");
        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
        let expr = &module.expressions[0].expression;
        match expr {
            crate::ast::Expression::Match { exhaustive, .. } => {
                assert!(!exhaustive, "plain `match:` must set exhaustive: false");
            }
            other => panic!("expected Expression::Match, got: {other:?}"),
        }
    }

    #[test]
    fn match_exhaustive_keyword_sets_exhaustive_true() {
        let (module, diags) = parse_source("x matchExhaustive: [#a -> 1; #b -> 0]");
        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
        let expr = &module.expressions[0].expression;
        match expr {
            crate::ast::Expression::Match {
                exhaustive, arms, ..
            } => {
                assert!(exhaustive, "`matchExhaustive:` must set exhaustive: true");
                assert_eq!(arms.len(), 2);
            }
            other => panic!("expected Expression::Match, got: {other:?}"),
        }
    }

    #[test]
    fn match_exhaustive_parses_same_arm_syntax_as_match() {
        // Guards, wildcards, and destructuring all parse identically under
        // `matchExhaustive:` — only the keyword and the resulting flag differ.
        let (module, diags) =
            parse_source("x matchExhaustive: [#a when: [true] -> 1; #b -> 2; _ -> 0]");
        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
        let expr = &module.expressions[0].expression;
        match expr {
            crate::ast::Expression::Match { arms, .. } => {
                assert_eq!(arms.len(), 3);
            }
            other => panic!("expected Expression::Match, got: {other:?}"),
        }
    }

    // `match_exhaustive_unparse_round_trips_keyword` and
    // `plain_match_unparse_does_not_gain_exhaustive_keyword` moved to
    // `crate::unparse`'s own test tree (ADR 0117 Phase 4) — both
    // exercise `unparse::format_source`, which `source_analysis`'s test tree
    // no longer references.

    // ========================================================================
    // expect().unwrap() hardening — guard-bypass regression tests
    // ========================================================================
    //
    // parse_block, parse_tuple_pattern, and parse_parenthesized each start by
    // calling `expect()` for their opening delimiter and used to `.unwrap()`
    // the result directly, which would panic on a user-input parse error.
    // In normal operation this is unreachable: every call site dispatches on
    // the exact TokenKind before calling in (see `parse_primary` for '[' and
    // '(', `parse_pattern` for '{'), so `expect()` always succeeds today.
    //
    // These tests call the private parse_* methods directly on a token
    // stream that does NOT start with the expected delimiter, bypassing the
    // dispatch guard, to prove that a future dispatch bug (or a new,
    // unguarded caller) degrades to a diagnostic instead of a panic.

    fn parser_for(src: &str) -> Parser {
        Parser::new(crate::source_analysis::lex_with_eof(src))
    }

    #[test]
    fn parse_block_guard_bypass_does_not_panic() {
        let mut parser = parser_for("42"); // no leading '['
        let expr = parser.parse_block();
        assert!(matches!(expr, Expression::Block(_)));
        assert!(
            parser
                .diagnostics
                .iter()
                .any(|d| d.message.contains("Expected '['")),
            "expected a diagnostic instead of a panic, got: {:?}",
            parser.diagnostics
        );
    }

    #[test]
    fn parse_tuple_pattern_guard_bypass_does_not_panic() {
        let mut parser = parser_for("42"); // no leading '{'
        let pattern = parser.parse_tuple_pattern();
        assert!(matches!(pattern, Pattern::Tuple { .. }));
        assert!(
            parser
                .diagnostics
                .iter()
                .any(|d| d.message.contains("Expected '{'")),
            "expected a diagnostic instead of a panic, got: {:?}",
            parser.diagnostics
        );
    }

    #[test]
    fn parse_parenthesized_guard_bypass_does_not_panic() {
        let mut parser = parser_for("42"); // no leading '('
        let expr = parser.parse_parenthesized();
        assert!(matches!(expr, Expression::Parenthesized { .. }));
        assert!(
            parser
                .diagnostics
                .iter()
                .any(|d| d.message.contains("Expected '('")),
            "expected a diagnostic instead of a panic, got: {:?}",
            parser.diagnostics
        );
    }

    // ── ADR 0107 Phase A: `Pattern::Nil` and `Pattern::Type` ──────

    #[test]
    fn parse_nil_pattern() {
        let (module, diags) = parse_source("x match: [nil -> 0; _ -> 1]");
        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
        let Expression::Match { arms, .. } = &module.expressions[0].expression else {
            panic!("expected Expression::Match");
        };
        assert!(
            matches!(arms[0].pattern, Pattern::Nil(_)),
            "expected Pattern::Nil, got: {:?}",
            arms[0].pattern
        );
    }

    #[test]
    fn parse_type_pattern() {
        let (module, diags) = parse_source("x match: [path :: String -> path; _ -> 1]");
        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
        let Expression::Match { arms, .. } = &module.expressions[0].expression else {
            panic!("expected Expression::Match");
        };
        match &arms[0].pattern {
            Pattern::Type { binding, class, .. } => {
                assert_eq!(binding.name, "path");
                assert_eq!(class.name, "String");
            }
            other => panic!("expected Pattern::Type, got: {other:?}"),
        }
    }

    #[test]
    fn parse_type_pattern_missing_class_name_recovers() {
        let (_module, diags) = parse_source("x match: [path :: -> path; _ -> 1]");
        assert!(
            diags.iter().any(|d| d.message.contains("class name")),
            "expected a diagnostic about the missing class name, got: {diags:?}"
        );
    }

    #[test]
    fn parse_nil_variable_and_type_patterns_together() {
        // Combining all three pattern kinds in one match: (ADR 0107 example shape).
        let (module, diags) = parse_source("x match: [nil -> 0; path :: String -> 1; other -> 2]");
        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
        let Expression::Match { arms, .. } = &module.expressions[0].expression else {
            panic!("expected Expression::Match");
        };
        assert!(matches!(arms[0].pattern, Pattern::Nil(_)));
        assert!(matches!(arms[1].pattern, Pattern::Type { .. }));
        assert!(matches!(arms[2].pattern, Pattern::Variable(_)));
    }

    // ── Bare `true`/`false` pattern rejection ──────────────────────

    #[test]
    fn parse_bare_true_pattern_rejected_with_single_diagnostic() {
        let (_module, diags) = parse_source("x match: [true -> 0; _ -> 1]");
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        assert!(
            diags[0].message.contains("bare 'true'") && diags[0].message.contains("x :: True"),
            "unexpected diagnostic message: {:?}",
            diags[0].message
        );
    }

    #[test]
    fn parse_bare_false_pattern_rejected_with_single_diagnostic() {
        let (_module, diags) = parse_source("x match: [false -> 0; _ -> 1]");
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        assert!(
            diags[0].message.contains("bare 'false'") && diags[0].message.contains("x :: False"),
            "unexpected diagnostic message: {:?}",
            diags[0].message
        );
    }

    #[test]
    fn parse_bare_true_false_pattern_does_not_bind_as_variable() {
        let (module, diags) = parse_source("x match: [true -> 0; false -> 1]");
        assert_eq!(diags.len(), 2, "expected one diagnostic per arm: {diags:?}");
        let Expression::Match { arms, .. } = &module.expressions[0].expression else {
            panic!("expected Expression::Match");
        };
        assert!(
            matches!(arms[0].pattern, Pattern::Wildcard(_)),
            "expected bare 'true' to recover as Pattern::Wildcard, not Pattern::Variable, got: {:?}",
            arms[0].pattern
        );
        assert!(
            matches!(arms[1].pattern, Pattern::Wildcard(_)),
            "expected bare 'false' to recover as Pattern::Wildcard, not Pattern::Variable, got: {:?}",
            arms[1].pattern
        );
    }

    #[test]
    fn parse_true_false_type_pattern_still_works() {
        // `x :: True` / `x :: False` (ADR 0107 Phase A `Pattern::Type`) is
        // the correct, already-working idiom this diagnostic points users
        // at — confirm it's unaffected by the bare-identifier rejection.
        let (module, diags) = parse_source("x match: [b :: True -> 0; b :: False -> 1; _ -> 2]");
        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
        let Expression::Match { arms, .. } = &module.expressions[0].expression else {
            panic!("expected Expression::Match");
        };
        match &arms[0].pattern {
            Pattern::Type { class, .. } => assert_eq!(class.name, "True"),
            other => panic!("expected Pattern::Type, got: {other:?}"),
        }
        match &arms[1].pattern {
            Pattern::Type { class, .. } => assert_eq!(class.name, "False"),
            other => panic!("expected Pattern::Type, got: {other:?}"),
        }
    }

    #[test]
    fn parse_bare_true_pattern_nested_in_tuple_rejected() {
        // parse_pattern recurses into tuple/array/map elements, so the
        // bare true/false rejection must apply there too, not just at the
        // top level of a match arm.
        let (_module, diags) = parse_source("x match: [{true, y} -> y; _ -> 1]");
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        assert!(
            diags[0].message.contains("bare 'true'"),
            "unexpected diagnostic message: {:?}",
            diags[0].message
        );
    }

    #[test]
    fn parse_bare_true_with_type_annotation_rejected_with_single_diagnostic() {
        // A plausible typo before discovering the `b :: Boolean` idiom —
        // the trailing `:: ClassName` must be consumed along with `true`,
        // not left dangling to cascade into a second "Expected '->'" error
        // (mirroring the `nil :: ClassName` case above).
        let (_module, diags) = parse_source("x match: [true :: Boolean -> 1; _ -> 0]");
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        assert!(
            diags[0].message.contains("bare 'true'"),
            "unexpected diagnostic message: {:?}",
            diags[0].message
        );
    }

    #[test]
    fn parse_bare_false_with_type_annotation_rejected_with_single_diagnostic() {
        // Symmetric coverage of `parse_bare_true_with_type_annotation_rejected_with_single_diagnostic`
        // for the `false` arm of the branch.
        let (_module, diags) = parse_source("x match: [false :: Boolean -> 1; _ -> 0]");
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        assert!(
            diags[0].message.contains("bare 'false'"),
            "unexpected diagnostic message: {:?}",
            diags[0].message
        );
    }

    // ── Constructor pattern keyword bindings — nil/true/false ──────

    #[test]
    fn parse_constructor_binding_nil_is_nil_pattern() {
        let (module, diags) = parse_source("x match: [Result ok: nil -> 0; _ -> 1]");
        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
        let Expression::Match { arms, .. } = &module.expressions[0].expression else {
            panic!("expected Expression::Match");
        };
        let Pattern::Constructor { keywords, .. } = &arms[0].pattern else {
            panic!("expected Pattern::Constructor, got: {:?}", arms[0].pattern);
        };
        assert!(
            matches!(keywords[0].1, Pattern::Nil(_)),
            "expected 'nil' binding to parse as Pattern::Nil, not Pattern::Variable, got: {:?}",
            keywords[0].1
        );
    }

    #[test]
    fn parse_constructor_binding_nil_type_annotation_rejected_with_single_diagnostic() {
        // The `nil` arm has the
        // same "trailing `:: ClassName` cascades" bug as the `true`/`false`
        // and generic-identifier arms — `nil :: SomeClass` must consume the
        // tail rather than leaving it to cascade into a second diagnostic.
        let (_module, diags) = parse_source("x match: [Result ok: nil :: SomeClass -> 0; _ -> 1]");
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        assert!(
            diags[0]
                .message
                .contains("a 'nil' pattern cannot have a type annotation"),
            "unexpected diagnostic message: {:?}",
            diags[0].message
        );
    }

    #[test]
    fn parse_constructor_binding_nil_bare_double_colon_rejected_with_single_diagnostic() {
        // Edge case: `consume_type_pattern_tail` returns `None` when `::`
        // isn't followed by a class name, so the recovered span must fall
        // back to the `::` token itself rather than panicking or leaving
        // `->` unconsumed.
        let src = "x match: [Result ok: nil :: -> 0; _ -> 1]";
        let (_module, diags) = parse_source(src);
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        assert!(
            diags[0]
                .message
                .contains("a 'nil' pattern cannot have a type annotation"),
            "unexpected diagnostic message: {:?}",
            diags[0].message
        );
        let double_colon_end = u32::try_from(src.find("::").unwrap()).unwrap() + 2;
        assert!(
            diags[0].span.end() >= double_colon_end,
            "expected diagnostic span to cover '::' (end >= {double_colon_end}), got: {:?}",
            diags[0].span
        );
    }

    #[test]
    fn parse_constructor_binding_bare_true_bare_double_colon_rejected_with_single_diagnostic() {
        // Same bare-`::` edge case as the `nil` arm above, for the
        // `true`/`false` arm.
        let src = "x match: [Result ok: true :: -> 0; _ -> 1]";
        let (_module, diags) = parse_source(src);
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        assert!(
            diags[0].message.contains("bare 'true'"),
            "unexpected diagnostic message: {:?}",
            diags[0].message
        );
        let double_colon_end = u32::try_from(src.find("::").unwrap()).unwrap() + 2;
        assert!(
            diags[0].span.end() >= double_colon_end,
            "expected diagnostic span to cover '::' (end >= {double_colon_end}), got: {:?}",
            diags[0].span
        );
    }

    #[test]
    fn parse_constructor_binding_type_annotation_bare_double_colon_rejected_with_single_diagnostic()
    {
        // Same bare-`::` edge case as the `nil` arm above, for the generic
        // variable-binding arm.
        let src = "r match: [Result ok: x :: -> x; Result error: _ -> 0]";
        let (_module, diags) = parse_source(src);
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        assert!(
            diags[0].message.contains(
                "type annotations are not supported in a constructor pattern binding position"
            ),
            "unexpected diagnostic message: {:?}",
            diags[0].message
        );
        let double_colon_end = u32::try_from(src.find("::").unwrap()).unwrap() + 2;
        assert!(
            diags[0].span.end() >= double_colon_end,
            "expected diagnostic span to cover '::' (end >= {double_colon_end}), got: {:?}",
            diags[0].span
        );
    }

    #[test]
    fn parse_constructor_binding_bare_true_rejected_with_single_diagnostic() {
        let (_module, diags) = parse_source("x match: [Result ok: true -> 0; _ -> 1]");
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        assert!(
            diags[0].message.contains("bare 'true'") && diags[0].message.contains("when:"),
            "unexpected diagnostic message: {:?}",
            diags[0].message
        );
    }

    #[test]
    fn parse_constructor_binding_bare_false_rejected_with_single_diagnostic() {
        let (_module, diags) = parse_source("x match: [Result ok: false -> 0; _ -> 1]");
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        assert!(
            diags[0].message.contains("bare 'false'") && diags[0].message.contains("when:"),
            "unexpected diagnostic message: {:?}",
            diags[0].message
        );
    }

    #[test]
    fn parse_constructor_binding_bare_true_false_recovers_as_wildcard() {
        let (module, diags) =
            parse_source("x match: [Result ok: true -> 0; Result error: false -> 1; _ -> 2]");
        assert_eq!(diags.len(), 2, "expected one diagnostic per arm: {diags:?}");
        let Expression::Match { arms, .. } = &module.expressions[0].expression else {
            panic!("expected Expression::Match");
        };
        let Pattern::Constructor {
            keywords: ok_kw, ..
        } = &arms[0].pattern
        else {
            panic!("expected Pattern::Constructor, got: {:?}", arms[0].pattern);
        };
        assert!(
            matches!(ok_kw[0].1, Pattern::Wildcard(_)),
            "expected bare 'true' binding to recover as Pattern::Wildcard, got: {:?}",
            ok_kw[0].1
        );
        let Pattern::Constructor {
            keywords: error_kw, ..
        } = &arms[1].pattern
        else {
            panic!("expected Pattern::Constructor, got: {:?}", arms[1].pattern);
        };
        assert!(
            matches!(error_kw[0].1, Pattern::Wildcard(_)),
            "expected bare 'false' binding to recover as Pattern::Wildcard, got: {:?}",
            error_kw[0].1
        );
    }

    // ── Constructor binding `x :: ClassName` tail ──────────────────

    #[test]
    fn parse_constructor_binding_type_annotation_rejected_with_single_diagnostic() {
        let (module, diags) =
            parse_source("r match: [Result ok: x :: Integer -> x; Result error: _ -> 0]");
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        assert!(
            diags[0].message.contains(
                "type annotations are not supported in a constructor pattern binding position"
            ),
            "unexpected diagnostic message: {:?}",
            diags[0].message
        );
        let Expression::Match { arms, .. } = &module.expressions[0].expression else {
            panic!("expected Expression::Match");
        };
        let Pattern::Constructor {
            keywords: ok_kw, ..
        } = &arms[0].pattern
        else {
            panic!("expected Pattern::Constructor, got: {:?}", arms[0].pattern);
        };
        assert!(
            matches!(ok_kw[0].1, Pattern::Wildcard(_)),
            "expected 'x :: Integer' binding to recover as Pattern::Wildcard, got: {:?}",
            ok_kw[0].1
        );
    }

    #[test]
    fn parse_constructor_binding_bare_true_type_annotation_rejected_with_single_diagnostic() {
        // The `true`/`false` arm
        // has the same "trailing `:: ClassName` cascades" bug as the plain
        // identifier arm above — `true :: Boolean` must consume the tail
        // rather than leaving it to cascade into a second diagnostic.
        let (_module, diags) = parse_source("x match: [Result ok: true :: Boolean -> 0; _ -> 1]");
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        assert!(
            diags[0].message.contains("bare 'true'"),
            "unexpected diagnostic message: {:?}",
            diags[0].message
        );
    }

    #[test]
    fn parse_constructor_binding_bare_false_type_annotation_rejected_with_single_diagnostic() {
        // Symmetric coverage for the `false` path through the same branch as
        // `parse_constructor_binding_bare_true_type_annotation_rejected_with_single_diagnostic`.
        let (_module, diags) = parse_source("x match: [Result ok: false :: Boolean -> 0; _ -> 1]");
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        assert!(
            diags[0].message.contains("bare 'false'"),
            "unexpected diagnostic message: {:?}",
            diags[0].message
        );
    }

    // ── Pattern::Type parser polish ────────────────────────────────

    #[test]
    fn parse_type_pattern_generic_args_rejected_with_single_diagnostic() {
        let (_module, diags) = parse_source("x match: [items :: List(Printable) -> items; _ -> 1]");
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        assert!(
            diags[0]
                .message
                .contains("generic type arguments are not supported in a type pattern yet"),
            "unexpected diagnostic message: {:?}",
            diags[0].message
        );
    }

    #[test]
    fn parse_type_pattern_generic_args_multi_param_rejected() {
        // Multi-parameter and nested generics should still be consumed as a
        // single balanced group rather than desyncing the parser.
        let (_module, diags) =
            parse_source("x match: [items :: Result(Integer, List(String)) -> items; _ -> 1]");
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        assert!(
            diags[0]
                .message
                .contains("generic type arguments are not supported in a type pattern yet"),
            "unexpected diagnostic message: {:?}",
            diags[0].message
        );
    }

    #[test]
    fn parse_nil_pattern_with_type_annotation_rejected_with_single_diagnostic() {
        let (_module, diags) = parse_source("x match: [nil :: String -> 0; _ -> 1]");
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        assert!(
            diags[0].message.contains("'nil' pattern"),
            "unexpected diagnostic message: {:?}",
            diags[0].message
        );
    }

    #[test]
    fn parse_nil_pattern_double_colon_with_no_consumable_tail_span_covers_double_colon() {
        // When nothing follows `::` that `consume_type_pattern_tail` can
        // consume (e.g. the arrow comes right after), the diagnostic span
        // must still cover the `::` token itself, not just `nil`.
        let src = "x match: [nil :: -> 0; _ -> 1]";
        let (_module, diags) = parse_source(src);
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        let double_colon_end = u32::try_from(src.find("::").unwrap()).unwrap() + 2;
        assert!(
            diags[0].span.end() >= double_colon_end,
            "expected diagnostic span to cover '::' (end >= {double_colon_end}), got span {:?}",
            diags[0].span
        );
    }

    #[test]
    fn parse_bare_true_double_colon_with_no_consumable_tail_span_covers_double_colon() {
        let src = "x match: [true :: -> 0; _ -> 1]";
        let (_module, diags) = parse_source(src);
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        let double_colon_end = u32::try_from(src.find("::").unwrap()).unwrap() + 2;
        assert!(
            diags[0].span.end() >= double_colon_end,
            "expected diagnostic span to cover '::' (end >= {double_colon_end}), got span {:?}",
            diags[0].span
        );
    }

    #[test]
    fn parse_type_pattern_non_identifier_class_name_rejected_with_single_diagnostic() {
        let (_module, diags) = parse_source("x match: [y :: 5 -> y; _ -> 1]");
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        assert!(
            diags[0].message.contains("class name"),
            "unexpected diagnostic message: {:?}",
            diags[0].message
        );
    }

    #[test]
    fn parse_type_pattern_missing_class_name_nested_in_tuple_does_not_corrupt_recovery() {
        // Regression: a malformed type pattern's error-recovery must
        // not consume a token that the *enclosing* pattern parser needs —
        // specifically, when a `::` with no class name sits directly before
        // the `,` element separator of an enclosing tuple pattern, that `,`
        // must be left alone so the tuple (and the rest of the match arm)
        // still parses. A blacklist-based recovery (a few "boundary" tokens —
        // arrow, `]`, `;`, `when:` — with everything else consumed) would
        // eat the `,` here and cascade into 7 diagnostics instead of 1.
        let (module, diags) = parse_source("x match: [{y ::, z} -> y; _ -> 1]");
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        assert!(
            diags[0].message.contains("class name"),
            "unexpected diagnostic message: {:?}",
            diags[0].message
        );
        // The tuple, comma, and second element must still have parsed.
        let Expression::Match { arms, .. } = &module.expressions[0].expression else {
            panic!("expected Expression::Match");
        };
        match &arms[0].pattern {
            Pattern::Tuple { elements, .. } => {
                assert_eq!(elements.len(), 2, "expected both tuple elements to parse");
                assert!(matches!(elements[1], Pattern::Variable(_)));
            }
            other => panic!("expected Pattern::Tuple, got: {other:?}"),
        }
    }

    #[test]
    fn parse_nil_pattern_with_generic_type_annotation_rejected_with_single_diagnostic() {
        // `nil :: List(Printable)` combines both malformed shapes (a type
        // annotation on `nil`, and one with generic args) — must still
        // collapse to exactly one diagnostic, exercising the
        // `consume_type_pattern_tail` -> `skip_parenthesized_for_recovery`
        // path (previously untested).
        let (_module, diags) = parse_source("x match: [nil :: List(Printable) -> 0; _ -> 1]");
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        assert!(
            diags[0].message.contains("'nil' pattern"),
            "unexpected diagnostic message: {:?}",
            diags[0].message
        );
    }

    #[test]
    fn parse_nil_pattern_with_literal_type_annotation_rejected_with_single_diagnostic() {
        // Exercises `consume_type_pattern_tail`'s literal-whitelist branch
        // (previously untested) — `nil :: 5` rather than `nil :: String`.
        let (_module, diags) = parse_source("x match: [nil :: 5 -> 0; _ -> 1]");
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        assert!(
            diags[0].message.contains("'nil' pattern"),
            "unexpected diagnostic message: {:?}",
            diags[0].message
        );
    }

    #[test]
    fn parse_type_pattern_literal_class_name_in_tuple_consumes_literal_not_comma() {
        // Positive counterpart to
        // `parse_type_pattern_missing_class_name_nested_in_tuple_does_not_corrupt_recovery`:
        // proves the literal (`5`) *is* consumed by the whitelist while the
        // following `,` is correctly left for the tuple parser.
        let (module, diags) = parse_source("x match: [{y :: 5, z} -> y; _ -> 1]");
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        let Expression::Match { arms, .. } = &module.expressions[0].expression else {
            panic!("expected Expression::Match");
        };
        match &arms[0].pattern {
            Pattern::Tuple { elements, .. } => {
                assert_eq!(elements.len(), 2, "expected both tuple elements to parse");
                assert!(matches!(elements[1], Pattern::Variable(_)));
            }
            other => panic!("expected Pattern::Tuple, got: {other:?}"),
        }
    }

    #[test]
    fn parse_type_pattern_missing_class_name_nested_in_array_does_not_corrupt_recovery() {
        // Array-pattern counterpart of the tuple-nesting regression test —
        // the array path was previously unexercised.
        let (module, diags) = parse_source("x match: [#[y ::, z] -> y; _ -> 1]");
        assert_eq!(
            diags.len(),
            1,
            "expected exactly one diagnostic (no cascade), got: {diags:?}"
        );
        let Expression::Match { arms, .. } = &module.expressions[0].expression else {
            panic!("expected Expression::Match");
        };
        match &arms[0].pattern {
            Pattern::Array { elements, .. } => {
                assert_eq!(elements.len(), 2, "expected both array elements to parse");
                assert!(matches!(elements[1], Pattern::Variable(_)));
            }
            other => panic!("expected Pattern::Array, got: {other:?}"),
        }
    }

    #[test]
    fn parse_type_pattern_unterminated_generic_args_does_not_swallow_rest_of_match() {
        // Regression: a single missing `)` in a
        // generic-args group must not consume every remaining match arm as
        // "part of the generic args" — `skip_parenthesized_for_recovery`
        // stops at `;`/`]` even with parens still unbalanced, since a real
        // generic-args group can never legitimately contain either.
        let source = "x match: [a :: List(Foo -> 1; b -> 2; _ -> 3]";
        let (_module, diags) = parse_source(source);
        let generics_diag = diags
            .iter()
            .find(|d| {
                d.message
                    .contains("generic type arguments are not supported in a type pattern yet")
            })
            .unwrap_or_else(|| panic!("expected the generic-args diagnostic, got: {diags:?}"));
        // The diagnostic's span must be bounded well short of EOF — proving
        // `skip_parenthesized_for_recovery` stopped at the `;` instead of
        // treating the rest of the file (including the `b -> 2` and
        // `_ -> 3` arms) as "part of the generic args" under one span.
        assert!(
            usize::try_from(generics_diag.span.end()).unwrap() < source.len() - 10,
            "diagnostic span {:?} swallowed too much of the input (len {}), got: {diags:?}",
            generics_diag.span,
            source.len()
        );
    }
}
