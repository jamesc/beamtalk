// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Method definition, selector/parameter and method-body parsing (split from `declarations.rs`).

use crate::ast::{
    ExpressionStatement, KeywordPart, MessageSelector, MethodDefinition, MethodModifiers,
    ParameterDefinition, StandaloneMethodDefinition, TypeAnnotation,
};
use crate::source_analysis::TokenKind;

use super::declarations::{SelectorContext, is_state_like_declaration_keyword, is_uses_keyword};
use super::{Diagnostic, DiagnosticCategory, Parser};

impl Parser {
    /// Parses a method definition.
    ///
    /// Syntax:
    /// - `methodName => body`
    /// - `+ other => body`
    /// - `at: index put: value => body`
    /// - `sealed methodName => body`
    /// - `internal methodName => body` (ADR 0071)
    pub(super) fn parse_method_definition(&mut self) -> Option<MethodDefinition> {
        let start = self.current_token().span();
        // Capture this method's own header indentation before any
        // tokens are consumed, so `parse_method_body` can tell a genuine
        // class-member declaration boundary apart from body content at the
        // same column a differently-indented header would produce (see
        // `current_method_header_indent`'s doc comment).
        let header_indent = self.current_token().indentation_after_newline();
        let doc_comment = self.collect_doc_comment();
        let comments = self.collect_comment_attachment();
        let mut method_is_sealed = false;
        let mut method_is_internal = false;
        let mut method_is_class = false;

        // Parse optional modifiers
        while let TokenKind::Identifier(name) = self.current_kind() {
            match name.as_str() {
                "class" | "internal" => {
                    // Only treat as modifier if next token is not `=>` or `-> Type =>`
                    // (otherwise it's a method named `class`/`internal`, possibly with
                    // a return type). ADR 0071 for `internal` disambiguation.
                    if self.is_fat_arrow_or_return_type(1) {
                        break;
                    }
                    if name == "class" {
                        method_is_class = true;
                    } else {
                        method_is_internal = true;
                    }
                    self.advance();
                }
                "sealed" => {
                    method_is_sealed = true;
                    self.advance();
                }
                _ => break,
            }
        }

        // Parse method selector and parameters
        let (selector, parameters) = self.parse_method_selector()?;

        // Parse optional return type: `-> Type` before `=>`
        let return_type = self.parse_optional_return_type();

        // Expect fat arrow
        if !self.match_token(&TokenKind::FatArrow) {
            self.error("Expected '=>' after method selector");
            return None;
        }

        // Parse method body
        self.in_method_body = true;
        // Record the enclosing method's selector so a bare `@primitive` can
        // infer it.
        let previous_method_selector = self.current_method_selector.take();
        self.current_method_selector = Some(selector.name());
        // Thread this method's own header indentation through for
        // the duration of its body — see `current_method_header_indent`'s
        // doc comment and `parse_method_body`'s declaration-boundary checks.
        let previous_method_header_indent = self.current_method_header_indent.take();
        self.current_method_header_indent = Some(header_indent.unwrap_or(2));
        // The `|>` sends this body adds belong to this method (ADR 0145).
        let pipeline_mark = self.pipeline_sends.len();
        let literal_mark = self.literal_comments.len();
        let spelling_mark = self.number_spellings.len();
        let gap_mark = self.gap_comments.len();
        let body = self.parse_method_body();
        let pipeline_sends = self.pipeline_sends.drain(pipeline_mark..).collect();
        let literal_comments = crate::ast::LiteralComments::from_records(
            self.literal_comments.drain(literal_mark..),
            self.number_spellings.drain(spelling_mark..),
            self.gap_comments.drain(gap_mark..),
        );
        self.current_method_header_indent = previous_method_header_indent;
        self.current_method_selector = previous_method_selector;
        self.in_method_body = false;

        let end = body.last().map_or(start, |s| s.expression.span());
        let span = start.merge(end);

        let mut method = MethodDefinition::with_options(
            selector,
            parameters,
            body,
            return_type,
            MethodModifiers {
                is_sealed: method_is_sealed,
                is_internal: method_is_internal,
                is_class_method: method_is_class,
            },
            span,
        );
        method.doc_comment = doc_comment;
        method.comments = comments;
        method.pipeline_sends = pipeline_sends;
        method.literal_comments = literal_comments;
        Some(method)
    }

    /// Parses a method selector and its parameters.
    ///
    /// Returns the selector and parameter definitions (with optional type annotations).
    fn parse_method_selector(&mut self) -> Option<(MessageSelector, Vec<ParameterDefinition>)> {
        self.parse_selector_and_params(SelectorContext::Method)
    }

    /// Parses a parameter name followed by an optional `:: Type` annotation.
    fn parse_param_definition(&mut self, missing_name_msg: &str) -> ParameterDefinition {
        let param_name = self.parse_identifier(missing_name_msg);
        match self.parse_optional_param_type() {
            Some(ta) => ParameterDefinition::with_type(param_name, ta),
            None => ParameterDefinition::new(param_name),
        }
    }

    /// Parses a unary, binary, or keyword selector with its parameters.
    ///
    /// Shared by method definitions and protocol method signatures; the
    /// [`SelectorContext`] selects the guards that differ between the two.
    pub(super) fn parse_selector_and_params(
        &mut self,
        context: SelectorContext,
    ) -> Option<(MessageSelector, Vec<ParameterDefinition>)> {
        let in_protocol = context == SelectorContext::Protocol;
        match self.current_kind() {
            // Unary: `methodName`
            TokenKind::Identifier(name) => {
                // Protocol bodies: guard against consuming identifiers that
                // start the next definition (e.g. `Protocol`).
                if in_protocol
                    && (name == "Protocol"
                        || name == "abstract"
                        || name == "sealed"
                        || name == "typed")
                {
                    return None;
                }
                let selector = MessageSelector::Unary(name.clone());
                self.advance();
                Some((selector, Vec::new()))
            }
            // Binary: `+ other`, `-> other` (ADR 0047), `>> other`
            // Arrow and GtGt are separate token kinds but valid binary selectors.
            TokenKind::BinarySelector(_) | TokenKind::Arrow | TokenKind::GtGt => {
                let op_name = match self.current_kind() {
                    TokenKind::BinarySelector(op) => op.clone(),
                    TokenKind::Arrow => "->".into(),
                    TokenKind::GtGt => ">>".into(),
                    _ => unreachable!(),
                };
                let selector = MessageSelector::Binary(op_name);
                self.advance();

                let param =
                    self.parse_param_definition("Expected parameter name after binary selector");
                Some((selector, vec![param]))
            }
            // Keyword: `at: index put: value` or `deposit: amount :: Integer`
            TokenKind::Keyword(kw) => {
                // Protocol bodies: `extending:` is the protocol extension keyword.
                if in_protocol && kw == "extending:" {
                    return None;
                }
                let mut keywords = Vec::new();
                let mut parameters = Vec::new();

                while let TokenKind::Keyword(keyword) = self.current_kind() {
                    // In protocol bodies, keywords on a new line are separate
                    // signatures. Only continue if the keyword is on the same
                    // line as the previous one (i.e., a multi-keyword selector
                    // like `at: key put: value`).
                    if in_protocol
                        && !keywords.is_empty()
                        && self.current_token().has_leading_newline()
                    {
                        break;
                    }
                    let span = self.current_token().span();
                    keywords.push(KeywordPart::new(keyword.clone(), span));
                    self.advance();

                    parameters
                        .push(self.parse_param_definition("Expected parameter name after keyword"));
                }

                Some((MessageSelector::Keyword(keywords), parameters))
            }
            _ => {
                if !in_protocol {
                    self.error("Expected method selector");
                }
                None
            }
        }
    }

    /// Parses an optional `:: Type` annotation after a parameter name.
    ///
    /// Used for both binary (`+ other :: Number`) and keyword (`deposit: amount :: Integer`)
    /// method parameters.
    fn parse_optional_param_type(&mut self) -> Option<TypeAnnotation> {
        if self.match_token(&TokenKind::DoubleColon) {
            Some(self.parse_type_annotation())
        } else if matches!(self.current_kind(), TokenKind::Colon) {
            self.type_annotation_colon_error();
            Some(self.parse_type_annotation())
        } else {
            None
        }
    }

    /// Checks whether the current position starts a new class/protocol/
    /// type-alias/method/standalone-method declaration, or a `state:`/
    /// `field:`/`uses:` keyword — the set of tokens that
    /// always end a method body no matter how the body was left (a trailing
    /// period, a cast `!`, or error recovery). Factored out of
    /// `parse_method_body` because the same check is needed at three
    /// separate exit points there.
    fn is_at_member_boundary(&self) -> bool {
        self.is_at_end()
            || self.is_at_class_definition()
            || self.is_at_protocol_definition()
            || self.is_at_type_alias_boundary()
            || self.is_at_native_declaration_boundary()
            || self.is_at_method_definition()
            || self.is_at_standalone_method_definition()
            || is_state_like_declaration_keyword(self.current_kind())
            || is_uses_keyword(self.current_kind())
            || self.is_at_late_modifier()
    }

    /// `is_at_type_alias_definition()`, gated by the same indentation guard
    /// `is_at_method_definition()` uses in `parse_method_body`.
    ///
    /// Without this guard, an in-body expression like `type Port = 8080`
    /// (`type` used as an ordinary variable, sent the unary message `Port`,
    /// compared with `=`) token-matches the type-alias pattern and would
    /// falsely truncate the method body — violating the ADR's promise that
    /// `type` "remains a legal identifier everywhere else."
    fn is_at_type_alias_boundary(&self) -> bool {
        // When `in_class_body` is false (standalone method bodies only — protocol
        // bodies use the unwrapped `is_at_type_alias_definition()` directly),
        // the `!self.in_class_body` arm short-circuits the `||`, skipping the
        // indentation check. This is safe because `= expr` is not a valid
        // Beamtalk binary expression, so the `type Uppercase =` pattern cannot
        // appear as a legitimate statement start outside declaration position.
        //
        // The boundary column is the current method's own header
        // indentation, not a hardcoded `2` — see
        // `current_method_header_indent`'s doc comment.
        (!self.in_class_body
            || self
                .current_token()
                .indentation_after_newline()
                .is_none_or(|col| col <= self.current_method_header_indent.unwrap_or(2)))
            && self.is_at_type_alias_definition()
    }

    /// `is_at_native_declaration()`, gated by the same indentation guard as
    /// [`Self::is_at_type_alias_boundary`].
    ///
    /// Without this guard, an in-body statement like `declare native: 5`
    /// (`declare` used as an ordinary variable, sent the keyword message
    /// `native:`) token-matches the native-declaration pattern and would
    /// falsely truncate the method body.
    fn is_at_native_declaration_boundary(&self) -> bool {
        (!self.in_class_body
            || self
                .current_token()
                .indentation_after_newline()
                .is_none_or(|col| col <= self.current_method_header_indent.unwrap_or(2)))
            && self.is_at_native_declaration()
    }

    /// Parses a method body (expressions until the next method or end of class).
    ///
    /// Statements are separated by periods or newlines.
    pub(super) fn parse_method_body(&mut self) -> Vec<ExpressionStatement> {
        let mut body = Vec::new();

        // Parse expressions until we hit something that looks like a new method,
        // state declaration, class definition, or standalone method definition
        // When inside a class body, a token at column 0 after a newline
        // is outside the class body (trailing expression). Break to avoid consuming it.
        //
        // Guard `is_at_method_definition()` with an indentation check.
        // After the formatter breaks 3+ keyword messages onto their own indented
        // lines, continuation keywords appear at col 4–6 inside the method body.
        // `is_at_method_definition()` does a wide lookahead and can find `-> Type =>`
        // from the *next* method's signature, causing a false positive that exits
        // the body early and treats continuation keywords as a new method selector.
        // When `in_class_body` is true, canonical class members start at col 2;
        // any token deeper than col 2 is part of an expression, never a member.
        // The boundary column is the *current* method's own header
        // indentation (`current_method_header_indent`), not a hardcoded `2` —
        // see that field's doc comment for why a hardcoded literal is wrong
        // for `method_source_walker`'s synthetic-wrap strategy.
        #[allow(clippy::nonminimal_bool)]
        while !self.is_at_end()
            && !self.is_at_class_definition()
            && !self.is_at_protocol_definition()
            && !self.is_at_type_alias_boundary()
            && !(
                // Only test is_at_method_definition when the token could plausibly
                // be a class member (indentation <= header indent). Deeper tokens
                // are continuation expressions and must not trigger early exit
                // from the method body.
                (!self.in_class_body
                    || self
                        .current_token()
                        .indentation_after_newline()
                        .is_none_or(|col| col <= self.current_method_header_indent.unwrap_or(2)))
                    && self.is_at_method_definition()
            )
            // A declaration-level `@expect` must end the body of the
            // *previous* method, just like a fresh method/state declaration
            // does — see `is_at_declaration_level_expect`'s doc comment.
            && !self.is_at_declaration_level_expect()
            && !self.is_at_standalone_method_definition()
            && !is_state_like_declaration_keyword(self.current_kind())
            && !is_uses_keyword(self.current_kind())
            && !self.is_at_late_modifier()
            && !(self.in_class_body && self.current_token().indentation_after_newline() == Some(0))
        {
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
            let is_error = expr.is_error();
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

            // If we got an error, try to recover
            if is_error {
                self.synchronize();
                // After synchronization, check if we can continue parsing
                // If synchronize stopped at a method/class/state boundary, break
                if self.is_at_member_boundary() {
                    break;
                }
                // Otherwise, synchronize stopped at a newline or period, so continue parsing
                continue;
            }

            // Period, bang (!), or newline separates statements
            if self.match_token(&TokenKind::Period) {
                // Explicit period — check if next token starts a new method/state/class
                let period_span = self.tokens[self.current - 1].span();
                if self.is_at_member_boundary() {
                    // Trailing period at end of method — not needed
                    self.diagnostics.push(
                        Diagnostic::lint("unnecessary trailing `.` at end of method", period_span)
                            .with_hint("Remove the trailing `.`"),
                    );
                    break;
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
                // Otherwise continue parsing more expressions
            } else if self.match_token(&TokenKind::Bang) {
                // Cast terminator — mark the last expression as a cast if it's a MessageSend.
                // If the expression is not a MessageSend (e.g. `x := foo bar!`), emit an error.
                self.apply_cast_terminator(body.last_mut().map(|s| &mut s.expression));
                // Check if next token starts a new method/state/class (same as period)
                if self.is_at_member_boundary() {
                    break;
                }
            } else if !self.is_at_end() && self.current_token().has_leading_newline() {
                // If the next token is at column 0 (no indentation), it's a
                // top-level expression, not part of this method body. This allows
                // trailing expressions after inline class definitions.
                if self.current_token().leading_indent() == Some(0) {
                    break;
                }
                // Newline acts as implicit statement separator
                // Continue parsing — the while-loop guard handles method/class boundaries
            } else {
                // No period and no newline — end of body
                break;
            }
        }

        body
    }

    /// Parses a standalone method definition (Tonel-style).
    ///
    /// Syntax:
    /// - `ClassName >> selector => body` (instance method)
    /// - `ClassName class >> selector => body` (class method)
    ///
    /// A `package@ClassName >> ...` target still parses, so the rest of the
    /// definition does not cascade into further errors, but it is a parse
    /// error: package-qualified extension methods were removed (ADR 0152).
    pub(super) fn parse_standalone_method_definition(&mut self) -> StandaloneMethodDefinition {
        let start = self.current_token().span();

        // Collect leading comments from the class-name token's leading trivia.
        // parse_identifier() advances past the class name without reading trivia,
        // so we must collect here before the token is consumed. Also remember
        // whether a blank line preceded the whole construct (before any leading
        // comment, or before the class-name token itself if it has none) — this
        // is the accurate signal for "was there a blank line before this
        // standalone method", mirroring how classes/protocols/type-aliases
        // capture `leading_blank_line` once at the start of their own construct.
        // The later `collect_comment_attachment()` inside
        // `parse_method_definition()` looks at the selector token instead, which
        // would otherwise silently lose this signal.
        //
        // A `///` block directly above the class-name token is this method's
        // doc comment; collect it first (as class and method parsing do) so
        // `collect_comment_attachment` doesn't demote it to an orphaned
        // leading comment, which split it from the method on format.
        let class_doc_comment = self.collect_doc_comment();
        let initial_comments = self.collect_comment_attachment();
        let leading_blank_line = initial_comments.leading_blank_line;
        let class_blank_line_after_comments = initial_comments.blank_line_after_comments;
        let mut class_leading_comments = initial_comments.leading;

        // Parse the class name. A package qualifier (`identifier @ Identifier`,
        // ADR 0070) is consumed so the definition parses, and reported: a
        // package-qualified extension names another package's class, and
        // cross-file extension methods were removed (ADR 0152).
        let class_name = if matches!(self.current_kind(), TokenKind::Identifier(name) if !name.starts_with(|c: char| c.is_uppercase()))
            && matches!(self.peek_at(1), Some(TokenKind::At))
        {
            // Package-qualified: `json@Parser`
            let pkg_ident = self.parse_identifier("Expected package name");
            self.advance(); // consume `@`
            let cls_ident = self.parse_identifier("Expected class name after '@'");
            self.diagnostics.push(
                Diagnostic::error(
                    format!(
                        "`{}@{} >> ...` adds a method to a class in another package; \
                         package-qualified extension methods were removed (ADR 0152)",
                        pkg_ident.name, cls_ident.name
                    ),
                    pkg_ident.span.merge(cls_ident.span),
                )
                .with_hint(
                    "Define the method in the file that defines the class, or write a method \
                     on a class of your own that takes the value."
                        .to_string(),
                )
                .with_category(DiagnosticCategory::MisplacedExtension),
            );
            cls_ident
        } else {
            // Unqualified: `Counter`
            self.parse_identifier("Expected class name")
        };

        // Check for optional `class` modifier
        let is_class_method = if matches!(self.current_kind(), TokenKind::Identifier(name) if name == "class")
        {
            // Only treat as modifier if next token is `>>` (GtGt)
            if matches!(self.peek_at(1), Some(TokenKind::GtGt)) {
                self.advance(); // consume `class`
                true
            } else {
                false
            }
        } else {
            false
        };

        // Consume `>>` (GtGt token)
        if !self.match_token(&TokenKind::GtGt) {
            self.error("Expected '>>' in standalone method definition");
        }

        // Parse method definition (selector => body)
        let mut method = self.parse_method_definition().unwrap_or_else(|| {
            let span = self.current_token().span();
            MethodDefinition::new(
                MessageSelector::Unary("error".into()),
                Vec::new(),
                Vec::new(),
                span,
            )
        });

        if method.doc_comment.is_none() {
            method.doc_comment = class_doc_comment;
        }

        // Prepend the class-name token's leading comments to the method's
        // own comments.  parse_method_definition() reads trivia from the
        // selector token, so comments before the class name would otherwise
        // be lost.
        if !class_leading_comments.is_empty() {
            // The gap after the last leading comment belongs to whichever
            // block ends the combined list: the selector token's own
            // comments when there are any, else the class-name token's
            // (e.g. a license header followed by a blank line, BT-4488).
            if method.comments.leading.is_empty() {
                method.comments.blank_line_after_comments = class_blank_line_after_comments;
            }
            class_leading_comments.append(&mut method.comments.leading);
            method.comments.leading = class_leading_comments;
        }
        // `leading_blank_line` is consulted only by the module-level
        // unparser, to decide whether to re-emit a blank line before this
        // whole standalone-method construct (mirroring
        // `TopLevelDecl::preceding_blank_line` for classes/protocols/type
        // aliases) — never to control spacing *within* the leading
        // comment block, which `unparse_comment_attachment_leading` handles
        // itself via each comment's own `preceding_blank_line`. So the value
        // captured before the class-name token — the true start of this
        // construct — always wins here, even in the obscure case of a
        // comment sitting between `>>` and the selector (whose own,
        // internally-computed `leading_blank_line` describes a different gap
        // that nothing renders).
        method.comments.leading_blank_line = leading_blank_line;

        let span = start.merge(method.span);

        StandaloneMethodDefinition {
            class_name,
            is_class_method,
            method,
            span,
        }
    }

    // ========================================================================
    // Type Alias Definition Parsing (ADR 0108, Phase 1)
    // ========================================================================
}
