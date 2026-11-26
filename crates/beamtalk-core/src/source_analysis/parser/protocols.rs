// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type alias, protocol and native declaration parsing (split from `declarations.rs`).

use crate::ast::{
    CommentAttachment, Identifier, MethodDefinition, NativeDeclaration, ParameterDefinition,
    ProtocolDefinition, ProtocolMethodSignature, TypeAliasDefinition, TypeAnnotation,
};
use crate::source_analysis::TokenKind;

use super::declarations::{
    SelectorContext, is_class_state_keyword, is_state_like_declaration_keyword, is_uses_keyword,
};
use super::{Diagnostic, Parser};

impl Parser {
    /// Parses a type alias definition: `type Name = <TypeAnnotation>`, or
    /// `internal type Name = <TypeAnnotation>` (ADR 0071, ADR 0108 Phase 5).
    ///
    /// Syntax:
    /// ```text
    /// /// How a supervised child restarts after exit.
    /// type RestartStrategy = #temporary | #transient | #permanent
    ///
    /// internal type ParserState = Integer | String
    /// ```
    ///
    /// Only called after [`Self::is_at_type_alias_definition`] has confirmed the
    /// `(internal)? type <UppercaseName> =` shape, so the optional `internal`
    /// modifier, `type` keyword, the name, and the `=` are guaranteed present
    /// here; only the right-hand-side annotation can still be malformed
    /// (reported by `parse_type_annotation`/`parse_single_type_annotation`
    /// themselves).
    ///
    /// Single-letter names (`type T = ...`) are a declaration error: ADR 0068
    /// reserves every bare single uppercase letter in type position for
    /// implicit method-local type parameters, so alias names and type-param
    /// names must stay disjoint. The error is non-fatal — parsing continues so
    /// the rest of the declaration (and any following declarations) still get
    /// checked.
    pub(super) fn parse_type_alias_definition(&mut self) -> TypeAliasDefinition {
        let start = self.current_token().span();
        let doc_comment = self.collect_doc_comment();
        let mut comments = self.collect_comment_attachment();

        let is_internal =
            matches!(self.current_kind(), TokenKind::Identifier(name) if name == "internal");
        if is_internal {
            self.advance(); // consume `internal`
        }

        self.advance(); // consume `type`

        let name = self.parse_identifier("Expected type alias name after 'type'");

        // ADR 0108 Semantics: single-letter names are reserved for ADR 0068's
        // implicit method-local type parameters (`is_generic_type_param`).
        if name.name.chars().count() == 1 {
            self.diagnostics.push(Diagnostic::error(
                "single-letter type names are reserved for type parameters; \
                 choose a longer name (e.g. `type OptionalInt = Integer | Nil`)",
                name.span,
            ));
        }

        if !matches!(self.current_kind(), TokenKind::BinarySelector(op) if op == "=") {
            self.error("Expected '=' after type alias name in 'type' declaration");
            let span = start.merge(name.span);
            return TypeAliasDefinition {
                name,
                annotation: TypeAnnotation::Simple(Identifier::new("Error", span)),
                is_internal,
                comments,
                doc_comment,
                span,
            };
        }
        self.advance(); // consume `=`

        let annotation = self.parse_type_annotation();
        let span = start.merge(annotation.span());

        // Collect a trailing end-of-line comment on the declaration line
        // (e.g. `type Port = Integer // comment`), mirroring the state
        // declaration handling above.
        comments.trailing = self.collect_trailing_comment();

        TypeAliasDefinition {
            name,
            annotation,
            is_internal,
            comments,
            doc_comment,
            span,
        }
    }

    // ========================================================================
    // Protocol Definition Parsing (ADR 0068, Phase 2a)
    // ========================================================================

    /// Parses a protocol definition.
    ///
    /// Syntax:
    /// ```text
    /// Protocol define: <Name>
    ///   extending: <OtherProtocol>        // optional
    ///   selectorName -> ReturnType
    ///   + other :: Self -> Boolean
    ///   doSomething: arg :: Type -> ReturnType
    /// ```
    ///
    /// Protocol method signatures are like method definitions but without `=>` bodies.
    /// The parser distinguishes them from class methods by the absence of `=>`.
    pub(super) fn parse_protocol_definition(&mut self) -> ProtocolDefinition {
        let start = self.current_token().span();
        let doc_comment = self.collect_doc_comment();
        let mut comments = self.collect_comment_attachment();

        // Consume `Protocol`
        self.advance();

        // Expect `define:` keyword
        if !matches!(self.current_kind(), TokenKind::Keyword(k) if k == "define:") {
            self.error("Expected 'define:' after 'Protocol'");
            return ProtocolDefinition {
                name: Identifier::new("Error", start),
                type_params: Vec::new(),
                extending: None,
                method_signatures: Vec::new(),
                class_method_signatures: Vec::new(),
                provided_methods: Vec::new(),
                package: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: start,
            };
        }
        self.advance(); // consume `define:`

        // Parse protocol name
        let name = self.parse_identifier("Expected protocol name after 'Protocol define:'");

        // Parse optional type parameters: `Collection(E)`, `Mapping(K, V)`
        let type_params = self.parse_optional_type_params();

        // Collect a trailing end-of-line comment on the declaration header
        // line (e.g. `Protocol define: Sortable // comment`), mirroring the
        // identical handling for `type` declarations. Collected before
        // `extending:`/the body since those start on their own indented
        // lines and aren't part of the header.
        comments.trailing = self.collect_trailing_comment();

        // Parse optional `extending:` clause
        let extending = if matches!(self.current_kind(), TokenKind::Keyword(k) if k == "extending:")
        {
            self.advance(); // consume `extending:`
            Some(self.parse_identifier("Expected protocol name after 'extending:'"))
        } else {
            None
        };

        // Parse protocol body: required signatures (no `=>`) and provided
        // methods (`=>` body — ADR 0127 §1).
        let was_in_protocol_body = self.in_protocol_body;
        self.in_protocol_body = true;
        let (method_signatures, class_method_signatures, provided_methods) =
            self.parse_protocol_body();
        self.in_protocol_body = was_in_protocol_body;

        // Determine end span
        let mut end = name.span;
        if let Some(tp) = type_params.last() {
            end = end.merge(tp.span);
        }
        if let Some(ref ext) = extending {
            end = end.merge(ext.span);
        }
        if let Some(sig) = method_signatures.last() {
            end = end.merge(sig.span);
        }
        if let Some(sig) = class_method_signatures.last() {
            end = end.merge(sig.span);
        }
        if let Some(m) = provided_methods.last() {
            end = end.merge(m.span);
        }
        let span = start.merge(end);

        ProtocolDefinition {
            name,
            type_params,
            extending,
            method_signatures,
            class_method_signatures,
            provided_methods,
            package: None,
            comments,
            doc_comment,
            span,
        }
    }

    /// Parses a native (Erlang FFI) type declaration (ADR 0075, Phase 2).
    ///
    /// Syntax: `declare native: <module>` followed by an indented body of
    /// method signatures — the same shape as a protocol body, reusing
    /// [`Self::parse_protocol_method_signature_with_doc`] for each entry.
    /// No `=>` bodies, no class-side signatures, and no header type
    /// parameters: type parameters in a native declaration's
    /// signatures — `T`, `A`, `B`, `Acc` — are method-scoped, introduced
    /// per-signature rather than declared once at the header like a
    /// protocol's `Collection(E)`.
    ///
    /// Whether `declare native:` is valid at this file's location (only
    /// `stubs/`, never `src/`) is not a parser concern — the parser has no
    /// notion of source paths. That check is enforced by the build
    /// pipeline, which knows each module's file path.
    pub(super) fn parse_native_declaration(&mut self) -> NativeDeclaration {
        let start = self.current_token().span();
        let doc_comment = self.collect_doc_comment();
        let mut comments = self.collect_comment_attachment();

        // Consume `declare`
        self.advance();

        // Expect `native:` keyword
        if !matches!(self.current_kind(), TokenKind::Keyword(k) if k == "native:") {
            self.error("Expected 'native:' after 'declare'");
            return NativeDeclaration {
                module: Identifier::new("Error", start),
                method_signatures: Vec::new(),
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: start,
            };
        }
        self.advance(); // consume `native:`

        // Parse the Erlang module name
        let module = self.parse_identifier("Expected Erlang module name after 'declare native:'");

        // Collect a trailing end-of-line comment on the declaration header
        // line (e.g. `declare native: lists // comment`), mirroring
        // `parse_protocol_definition`'s identical handling.
        comments.trailing = self.collect_trailing_comment();

        // Parse the declaration body (method signatures without `=>`)
        let method_signatures = self.parse_native_declaration_body();

        // Determine end span
        let end = method_signatures
            .last()
            .map_or(module.span, |sig: &ProtocolMethodSignature| sig.span);
        let span = start.merge(end);

        NativeDeclaration {
            module,
            method_signatures,
            comments,
            doc_comment,
            span,
        }
    }

    /// Parses the body of a native declaration (method signatures without
    /// `=>`). Identical stopping conditions to [`Self::parse_protocol_body`]
    /// but without the `class`-prefix distinction — native declarations have
    /// no class-side signatures.
    fn parse_native_declaration_body(&mut self) -> Vec<ProtocolMethodSignature> {
        let mut signatures = Vec::new();

        // Skip any periods/statement terminators
        while self.match_token(&TokenKind::Period) {}

        while !self.is_at_end()
            && !self.is_at_protocol_definition()
            && !self.is_at_class_definition()
            && !self.is_at_type_alias_definition()
            && !self.is_at_native_declaration()
            && !self.is_at_standalone_method_definition()
        {
            let doc_comment = self.collect_doc_comment();
            let comments = self.collect_comment_attachment();

            match self.parse_protocol_method_signature_with_doc(doc_comment, comments) {
                Some(sig) => signatures.push(sig),
                // Not a valid signature start — end of declaration body
                None => break,
            }

            // Skip any periods/statement terminators
            while self.match_token(&TokenKind::Period) {}
        }

        signatures
    }

    /// Parses the body of a protocol definition: required signatures (no
    /// `=>`) and provided methods (`=>` body — ADR 0127 §1).
    ///
    /// Protocol method signatures and provided methods have the same
    /// selector and parameter syntax as class methods. A line with `=>` is a
    /// provided method, parsed through the ordinary class method parser
    /// ([`Self::parse_method_definition`]); a line without one stays a
    /// required signature. Instance-side entries of either kind go on
    /// `signatures`/`provided_methods`; entries prefixed with `class` are
    /// collected separately (`class_signatures` for requirements — a
    /// class-side *provision*, `class sel … =>`, is "not yet supported" in
    /// v1, ADR 0127 §1, §13, and is never added to any list).
    ///
    /// A `uses:` line (protocol-using-protocol composition) is also "not
    /// yet supported" in v1 (ADR 0127 §Status 8, §13) — reserved only when
    /// it has no `=>` (a method literally named `uses:` has a body and is
    /// parsed as an ordinary provided method above it in this loop, exactly
    /// as `uses:` is disambiguated in a class body, ADR 0127 §2).
    ///
    /// The body ends when we hit:
    /// - EOF
    /// - Another protocol definition (`Protocol define:`)
    /// - A class definition (`Superclass subclass:`)
    /// - A standalone method definition (`Class >>`)
    ///
    /// Returns `(instance_signatures, class_method_signatures, provided_methods)`.
    fn parse_protocol_body(
        &mut self,
    ) -> (
        Vec<ProtocolMethodSignature>,
        Vec<ProtocolMethodSignature>,
        Vec<MethodDefinition>,
    ) {
        let mut signatures = Vec::new();
        let mut class_signatures = Vec::new();
        let mut provided_methods = Vec::new();

        // Skip any periods/statement terminators
        while self.match_token(&TokenKind::Period) {}

        while !self.is_at_end()
            && !self.is_at_protocol_definition()
            && !self.is_at_class_definition()
            && !self.is_at_type_alias_definition()
            && !self.is_at_native_declaration()
            && !self.is_at_standalone_method_definition()
        {
            // A declaration-level `@expect category` (BT-3671) applies to the
            // provided method that follows it, exactly as in a class body —
            // it suppresses the flattened-provision diagnostics published in
            // this (the protocol's) file.
            let pending = self.parse_pending_declaration_expect();
            let pending_has_expect = pending.expect.is_some();
            if let Some((_, _, span)) = pending.expect
                && !self.is_at_method_definition()
            {
                self.diagnostics.push(Diagnostic::error(
                        "`@expect` in a protocol body applies only to a provided method (one with a `=>` body)",
                        span,
                    ));
                while self.match_token(&TokenKind::Period) {}
                continue;
            }

            // A provided method — any selector shape ending in `=>`,
            // possibly `class`-prefixed. Checked first (pure lookahead, no
            // trivia consumed yet) so `parse_method_definition` itself
            // collects the doc comment/leading comments off the right
            // token, exactly as in a class body.
            if self.is_at_method_definition() {
                if let Some(mut method) = self.parse_method_definition() {
                    if pending_has_expect {
                        pending.apply_to(
                            &mut method.expect,
                            &mut method.doc_comment,
                            &mut method.comments,
                        );
                    }
                    if method.is_class_method {
                        self.diagnostics.push(Diagnostic::error(
                            "class-side provided methods are not yet supported",
                            method.span,
                        ));
                    } else {
                        provided_methods.push(method);
                    }
                } else {
                    break;
                }
                while self.match_token(&TokenKind::Period) {}
                continue;
            }

            // A reserved `uses:` line with no body — protocol-using-protocol
            // composition (ADR 0127 §Status 8), not yet supported in v1.
            if is_uses_keyword(self.current_kind()) {
                let _ = self.collect_doc_comment();
                let _ = self.collect_comment_attachment();
                let use_line = self.parse_protocol_use_line();
                self.diagnostics.push(Diagnostic::error(
                    "a protocol using another protocol is not yet supported",
                    use_line.span,
                ));
                while self.match_token(&TokenKind::Period) {}
                continue;
            }

            // `state:`/`field:` — protocols are stateless (ADR
            // 0127 §7, §13). Parsed and discarded (rather than left for the
            // signature parser below, which would see a non-signature token
            // and silently end the body early) so a real diagnostic is
            // produced and the rest of the protocol body still parses.
            // `self.slot` reads/writes inside a provision's *body* are a
            // semantic-analysis concern (`trait_expansion::check_after_
            // hierarchy`, BT-3589) — they parse as ordinary expressions and
            // can't be rejected here.
            // An optional `late` prefix is looked through (both parse fns
            // consume it themselves), so `late state:` doesn't leak a phantom
            // `late` required selector.
            if is_state_like_declaration_keyword(self.current_kind()) || self.is_at_late_modifier()
            {
                let decl_span = if is_class_state_keyword(self.current_kind())
                    || self.is_at_late_class_state_keyword()
                {
                    self.parse_classvar_declaration().map(|d| d.span)
                } else {
                    self.parse_state_declaration().map(|d| d.span)
                };
                if let Some(span) = decl_span {
                    self.diagnostics.push(Diagnostic::error(
                        "protocols are stateless — declare `slot -> Type` as a required method",
                        span,
                    ));
                }
                while self.match_token(&TokenKind::Period) {}
                continue;
            }

            // Collect the doc comment and any non-doc leading
            // comments *before* checking for `class` prefix, because both are
            // leading trivia on the `class` token and would be lost when we
            // advance past it.
            let doc_comment = self.collect_doc_comment();
            let comments = self.collect_comment_attachment();

            // Detect `class` prefix for class method signatures.
            // Use the same lookahead as class definition parsing: `class` followed
            // by something other than `=>` or `-> Type =>` means it's a modifier.
            let is_class_method = matches!(
                self.current_kind(),
                TokenKind::Identifier(name) if name == "class"
            ) && !self.is_fat_arrow_or_return_type(1);

            if is_class_method {
                self.advance(); // consume `class`
            }

            if let Some(sig) = self.parse_protocol_method_signature_with_doc(doc_comment, comments)
            {
                if is_class_method {
                    class_signatures.push(sig);
                } else {
                    signatures.push(sig);
                }
            } else {
                // Not a valid signature start — end of protocol body
                break;
            }

            // Skip any periods/statement terminators
            while self.match_token(&TokenKind::Period) {}
        }

        (signatures, class_signatures, provided_methods)
    }

    /// Parses a single protocol method signature (no `=>` body).
    ///
    /// Returns `None` if the current position doesn't look like a method signature.
    ///
    /// `pre_doc` and `pre_comments` are the doc comment and non-doc leading
    /// comments, respectively, already collected by the caller before
    /// consuming an optional `class` prefix — both live in the `class`
    /// token's leading trivia and would be lost once the parser advances
    /// past it.
    ///
    /// Syntax:
    /// - Unary: `asString -> String`
    /// - Binary: `< other :: Self -> Boolean`
    /// - Keyword: `do: block :: Block(E, Object)`
    fn parse_protocol_method_signature_with_doc(
        &mut self,
        pre_doc: Option<String>,
        pre_comments: CommentAttachment,
    ) -> Option<ProtocolMethodSignature> {
        let start = self.current_token().span();
        let doc_comment = pre_doc.or_else(|| self.collect_doc_comment());
        let comments = pre_comments;

        let (selector, parameters) = self.parse_selector_and_params(SelectorContext::Protocol)?;

        // Parse optional return type: `-> Type`
        let return_type = self.parse_optional_return_type();

        // Protocol signatures must NOT have `=>`; if we see one, that's
        // a user error — they probably meant a class method.
        if matches!(self.current_kind(), TokenKind::FatArrow) {
            let span = self.current_token().span();
            self.diagnostics.push(
                Diagnostic::error(
                    "Protocol method signatures cannot have implementations ('=>')",
                    span,
                )
                .with_hint("Remove '=>' and the method body — protocols only declare signatures"),
            );
            // Consume the `=>` and skip to the next line/signature for recovery
            self.advance();
            // Skip tokens until newline or next signature-like token
            while !self.is_at_end()
                && !self.current_token().has_leading_newline()
                && !matches!(self.current_kind(), TokenKind::Period)
            {
                self.advance();
            }
        }

        // Determine end span
        let end = return_type
            .as_ref()
            .map(TypeAnnotation::span)
            .or(parameters.last().map(ParameterDefinition::span))
            .unwrap_or(start);
        let span = start.merge(end);

        Some(ProtocolMethodSignature {
            selector,
            parameters,
            return_type,
            comments,
            doc_comment,
            span,
        })
    }
}
