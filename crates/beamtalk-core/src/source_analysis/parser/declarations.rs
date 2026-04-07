// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Declaration parsing for Beamtalk.
//!
//! This module handles parsing of top-level declarations including:
//! - Class definitions with modifiers (`abstract`, `sealed`, `typed`, `internal`)
//! - State (field) declarations with types and default values
//! - Method definitions with optional `sealed` or `internal` modifiers

use crate::ast::{
    ClassDefinition, ClassModifiers, CommentAttachment, DeclaredKeyword, ExpectCategory,
    Expression, ExpressionStatement, Identifier, KeywordPart, MessageSelector, MethodDefinition,
    MethodKind, MethodModifiers, ParameterDefinition, ProtocolDefinition, ProtocolMethodSignature,
    StandaloneMethodDefinition, StateDeclaration, TypeAnnotation, TypeParamDecl,
};
use crate::source_analysis::{Span, TokenKind};
use ecow::EcoString;

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

/// Returns `true` if the token kind is a comma binary selector.
fn is_comma(kind: &TokenKind) -> bool {
    matches!(kind, TokenKind::BinarySelector(s) if s.as_str() == ",")
}

/// Returns `true` if the optional token kind is a comma binary selector.
fn is_comma_opt(kind: Option<&TokenKind>) -> bool {
    kind.is_some_and(is_comma)
}

impl Parser {
    /// Reports a "use `::` not `:`" error for type annotations and advances past the `:`.
    fn type_annotation_colon_error(&mut self) {
        let span = self.current_token().span();
        self.diagnostics.push(
            Diagnostic::error("Use `::` for type annotations, not `:`", span)
                .with_hint("Replace `:` with `::`"),
        );
        self.advance(); // consume legacy `:`
    }

    // ========================================================================
    // Class Definition Parsing
    // ========================================================================

    /// Parses a class definition.
    ///
    /// Syntax:
    /// ```text
    /// (internal|abstract|sealed|typed)* <Superclass> subclass: <ClassName>
    ///   state: fieldName = defaultValue
    ///   state: fieldName :: TypeName = defaultValue
    ///
    ///   methodName => body
    ///   sealed methodName => body
    ///   internal methodName => body
    /// ```
    ///
    /// Also supports package-qualified superclass (ADR 0070):
    /// - `json@Parser subclass: LenientParser`
    ///
    /// The superclass name determines the class kind:
    /// - `Actor subclass:` → `ClassKind::Actor`
    /// - `Value subclass:` → `ClassKind::Value` (ADR 0042)
    /// - Anything else → `ClassKind::Object`
    #[allow(clippy::too_many_lines)] // class parsing has many orthogonal concerns
    pub(super) fn parse_class_definition(&mut self) -> ClassDefinition {
        let start = self.current_token().span();
        let doc_comment = self.collect_doc_comment();
        let comments = self.collect_comment_attachment();
        let mut is_abstract = false;
        let mut is_sealed = false;
        let mut is_typed = false;
        let mut is_internal = false;

        // Parse optional modifiers: abstract, sealed, typed, internal (ADR 0071)
        while let TokenKind::Identifier(name) = self.current_kind() {
            if name == "abstract" {
                is_abstract = true;
                self.advance();
            } else if name == "sealed" {
                is_sealed = true;
                self.advance();
            } else if name == "typed" {
                is_typed = true;
                self.advance();
            } else if name == "internal" {
                is_internal = true;
                self.advance();
            } else {
                break;
            }
        }

        // Parse superclass name — `nil` means root class (no superclass).
        // Supports package-qualified superclasses (ADR 0070): `json@Parser subclass: ...`
        let superclass_id = self.parse_identifier("Expected superclass name");
        let (superclass, superclass_package) = if superclass_id.name == "nil" {
            (None, None)
        } else if matches!(self.current_kind(), TokenKind::At) {
            // Package-qualified superclass: `json @ Parser`
            let pkg = superclass_id;
            self.advance(); // consume `@`
            let cls = self.parse_identifier("Expected class name after '@'");
            (Some(cls), Some(pkg))
        } else {
            (Some(superclass_id), None)
        };

        // Parse optional superclass type arguments: `Collection(E) subclass: ...`
        let superclass_type_args = self.parse_optional_superclass_type_args();

        // Expect `subclass:` keyword
        if !matches!(self.current_kind(), TokenKind::Keyword(k) if k == "subclass:") {
            self.error("Expected 'subclass:' keyword");
            return ClassDefinition::with_modifiers(
                Identifier::new("Error", start),
                superclass,
                ClassModifiers {
                    is_abstract,
                    is_sealed,
                    is_typed,
                    is_internal,
                },
                Vec::new(),
                Vec::new(),
                start,
            );
        }
        self.advance(); // consume `subclass:`

        // Parse class name
        let name = self.parse_identifier("Expected class name");

        // Parse optional type parameters: `ClassName(T, E)`
        let type_params = self.parse_optional_type_params();

        // Parse optional `native: module_name` (ADR 0056)
        let backing_module = if matches!(self.current_kind(), TokenKind::Keyword(k) if k == "native:")
        {
            self.advance(); // consume `native:`
            if self.current_token().has_leading_newline() {
                // Module name must be on the same line as `native:`
                self.error("Expected Erlang module name after 'native:'");
                None
            } else if let TokenKind::Identifier(module_name) = self.current_kind() {
                let module_name = module_name.clone();
                let span = self.current_token().span();
                self.advance(); // consume module name
                Some(Identifier::new(module_name, span))
            } else {
                self.error("Expected Erlang module name after 'native:'");
                self.advance(); // skip invalid token so class-body parsing can recover
                None
            }
        } else {
            None
        };

        // Parse class body (state declarations, instance methods, class methods, class variables)
        let (state, methods, class_methods, class_variables) = self.parse_class_body();

        // Determine end span: max of last instance method, class method, state, class var, or name
        let mut end = name.span;
        if let Some(ref bm) = backing_module {
            end = end.merge(bm.span);
        }
        if let Some(s) = state.last() {
            end = end.merge(s.span);
        }
        if let Some(m) = methods.last() {
            end = end.merge(m.span);
        }
        if let Some(cm) = class_methods.last() {
            end = end.merge(cm.span);
        }
        if let Some(cv) = class_variables.last() {
            end = end.merge(cv.span);
        }
        let span = start.merge(end);

        let mut class_def = ClassDefinition::with_modifiers(
            name,
            superclass,
            ClassModifiers {
                is_abstract,
                is_sealed,
                is_typed,
                is_internal,
            },
            state,
            methods,
            span,
        );
        class_def.superclass_package = superclass_package;
        class_def.type_params = type_params;
        class_def.superclass_type_args = superclass_type_args;
        class_def.class_methods = class_methods;
        class_def.class_variables = class_variables;
        class_def.doc_comment = doc_comment;
        class_def.comments = comments;
        class_def.backing_module = backing_module;
        class_def
    }

    /// Parses optional type parameters: `(T, E)` or `(T :: Printable, E)`.
    ///
    /// Each type parameter may optionally have a protocol bound after `::`.
    /// Returns an empty `Vec` if no `(` follows the current position.
    ///
    /// **References:** ADR 0068 Phase 2d — type parameter bounds
    fn parse_optional_type_params(&mut self) -> Vec<TypeParamDecl> {
        if !matches!(self.current_kind(), TokenKind::LeftParen) {
            return Vec::new();
        }
        self.advance(); // consume `(`
        let mut params = Vec::new();
        if !matches!(self.current_kind(), TokenKind::RightParen) {
            params.push(self.parse_type_param_decl());
            while is_comma(self.current_kind()) {
                self.advance(); // consume `,`
                params.push(self.parse_type_param_decl());
            }
        }
        if matches!(self.current_kind(), TokenKind::RightParen) {
            self.advance(); // consume `)`
        } else {
            self.error("Expected ')' after type parameters");
        }
        params
    }

    /// Parses a single type parameter declaration: `T` or `T :: Printable`.
    ///
    /// The `::` syntax mirrors type annotations elsewhere in the language
    /// (ADR 0053) and reads as "T conforms to Printable".
    fn parse_type_param_decl(&mut self) -> TypeParamDecl {
        let name = self.parse_identifier("Expected type parameter name");
        // Check for `::` bound: `T :: Printable`
        if matches!(self.current_kind(), TokenKind::DoubleColon) {
            self.advance(); // consume `::`
            let bound = self.parse_identifier("Expected protocol name after '::'");
            let span = name.span.merge(bound.span);
            TypeParamDecl::bounded(name, bound, span)
        } else {
            TypeParamDecl::unbounded(name)
        }
    }

    /// Parses optional superclass type arguments: `(E)`, `(Integer)`, `(K, V)`.
    ///
    /// Used for `Collection(E) subclass: Array(E)` — the `(E)` after `Collection`.
    /// Returns an empty `Vec` if no `(` follows the superclass name.
    ///
    /// Unlike `parse_optional_type_params` which produces bare `Identifier`s,
    /// this produces `TypeAnnotation`s since the arguments may be concrete types,
    /// type param references, or even nested generics.
    fn parse_optional_superclass_type_args(&mut self) -> Vec<TypeAnnotation> {
        if !matches!(self.current_kind(), TokenKind::LeftParen) {
            return Vec::new();
        }
        self.advance(); // consume `(`
        let mut args = Vec::new();
        if !matches!(self.current_kind(), TokenKind::RightParen) {
            args.push(self.parse_type_annotation());
            while is_comma(self.current_kind()) {
                self.advance(); // consume `,`
                args.push(self.parse_type_annotation());
            }
        }
        if matches!(self.current_kind(), TokenKind::RightParen) {
            self.advance(); // consume `)`
        } else {
            self.error("Expected ')' after superclass type arguments");
        }
        args
    }

    /// Helper to parse an identifier, reporting an error if not found.
    pub(super) fn parse_identifier(&mut self, error_message: &str) -> Identifier {
        if let TokenKind::Identifier(name) = self.current_kind() {
            let span = self.current_token().span();
            let ident = Identifier::new(name.clone(), span);
            self.advance();
            ident
        } else {
            let span = self.current_token().span();
            self.error(error_message);
            Identifier::new("Error", span)
        }
    }

    /// Parses the body of a class (state declarations and methods).
    ///
    /// State declarations start with `state:`.
    /// Methods are identified by having a `=>` somewhere.
    fn parse_class_body(
        &mut self,
    ) -> (
        Vec<StateDeclaration>,
        Vec<MethodDefinition>,
        Vec<MethodDefinition>,
        Vec<StateDeclaration>,
    ) {
        let mut state = Vec::new();
        let mut methods = Vec::new();
        let mut class_methods = Vec::new();
        let mut class_variables = Vec::new();

        // Skip any periods/statement terminators
        while self.match_token(&TokenKind::Period) {}

        // BT-903: Set in_class_body so parse_method_body can use indentation
        // to detect trailing expressions outside the class.
        let was_in_class_body = self.in_class_body;
        self.in_class_body = true;

        while !self.is_at_end()
            && !self.is_at_class_definition()
            && !self.is_at_protocol_definition()
            && !self.is_at_standalone_method_definition()
        {
            // BT-1856: Check for `@expect category` before a declaration.
            // Consume it and attach to the next state/method declaration.
            let pending_expect = if matches!(self.current_kind(), TokenKind::AtExpect) {
                Some(self.parse_declaration_expect())
            } else {
                None
            };

            // Check for state/field declaration: `state: fieldName ...` or `field: fieldName ...`
            if matches!(self.current_kind(), TokenKind::Keyword(k) if k == "state:" || k == "field:")
            {
                if let Some(mut state_decl) = self.parse_state_declaration() {
                    if let Some(expect) = pending_expect {
                        state_decl.expect = Some(expect);
                    }
                    state.push(state_decl);
                }
            }
            // Check for class variable declaration: `classState: varName ...`
            else if matches!(self.current_kind(), TokenKind::Keyword(k) if k == "classState:") {
                if let Some(mut classvar_decl) = self.parse_classvar_declaration() {
                    if let Some(expect) = pending_expect {
                        classvar_decl.expect = Some(expect);
                    }
                    class_variables.push(classvar_decl);
                }
            }
            // Check for method definition (with optional modifiers including `class`)
            else if self.is_at_method_definition() {
                // Scan ahead through modifier tokens to check if `class` appears
                let is_class_method = {
                    let mut offset = 0;
                    let mut found = false;
                    loop {
                        match self.peek_at(offset) {
                            Some(TokenKind::Identifier(name))
                                if matches!(name.as_str(), "sealed" | "internal") =>
                            {
                                offset += 1;
                            }
                            Some(TokenKind::Identifier(name)) if name == "class" => {
                                // Check it's not `class => ...` or `class -> Type => ...`
                                // (method named "class", possibly with a return type annotation)
                                if !self.is_fat_arrow_or_return_type(offset + 1) {
                                    found = true;
                                }
                                break;
                            }
                            _ => break,
                        }
                    }
                    found
                };
                if let Some(mut method) = self.parse_method_definition() {
                    if let Some(expect) = pending_expect {
                        method.expect = Some(expect);
                    }
                    if is_class_method {
                        class_methods.push(method);
                    } else {
                        methods.push(method);
                    }
                }
            } else {
                // BT-1856: @expect before an invalid position (e.g., end of class body)
                if let Some((_, _, span)) = pending_expect {
                    self.diagnostics.push(Diagnostic::error(
                        "@expect in a class body must precede a state/field or method declaration",
                        span,
                    ));
                }
                // Not a state or method - end of class body
                break;
            }

            // Skip any periods/statement terminators
            while self.match_token(&TokenKind::Period) {}
        }

        // BT-903: Restore in_class_body flag
        self.in_class_body = was_in_class_body;

        (state, methods, class_methods, class_variables)
    }

    /// Checks if the current position is at the start of a method definition.
    ///
    /// Methods can start with:
    /// - An identifier followed directly by `=>` (unary method)
    /// - A binary selector followed by identifier and `=>` (binary method)
    /// - Keywords followed by identifiers and eventually `=>` (keyword method)
    /// - `sealed` or `internal` followed by one of the above
    pub(super) fn is_at_method_definition(&self) -> bool {
        let mut offset = 0;

        // Skip optional modifiers: sealed, internal, class
        // Note: `class` and `internal` are only modifiers when followed by a
        // method selector, not when followed by `=>` (which makes them method
        // names). ADR 0071 for `internal` disambiguation.
        while let Some(TokenKind::Identifier(name)) = self.peek_at(offset) {
            if matches!(name.as_str(), "sealed") {
                offset += 1;
            } else if name == "class" || name == "internal" {
                // Only treat as modifier if next token is not `=>` or `-> Type =>`
                // (i.e., not a method named "class"/"internal", possibly with a return type)
                if self.is_fat_arrow_or_return_type(offset + 1) {
                    break;
                }
                offset += 1;
            } else {
                break;
            }
        }

        // Check for method selector pattern followed by => (possibly with -> ReturnType)
        match self.peek_at(offset) {
            // Unary method: `identifier =>` or `identifier -> Type =>`
            Some(TokenKind::Identifier(_)) => {
                matches!(self.peek_at(offset + 1), Some(TokenKind::FatArrow))
                    || self.is_return_type_then_fat_arrow(offset + 1)
            }
            // Binary method: `+ other =>` or `+ other -> Type =>` or `+ other :: Type =>`
            // Arrow (`->`) is also a valid binary method selector (ADR 0047).
            // GtGt (`>>`) is also a valid binary method selector (BT-1735).
            Some(TokenKind::BinarySelector(_) | TokenKind::Arrow | TokenKind::GtGt) => {
                if !matches!(self.peek_at(offset + 1), Some(TokenKind::Identifier(_))) {
                    return false;
                }
                // Typed via DoubleColon: `+ other :: Type (| Type)* =>` (or untyped)
                let after_param = match self.skip_double_colon_type(offset + 2) {
                    // Malformed `::` (missing type): use offset after `::` so the
                    // method is still detected and parse-time errors are emitted.
                    DoubleColonSkip::Valid(o) | DoubleColonSkip::Malformed(o) => o,
                    DoubleColonSkip::NotPresent => offset + 2,
                };
                self.is_fat_arrow_or_return_type(after_param)
            }
            // Keyword method: `at: index =>` or `at: index put: value =>`
            Some(TokenKind::Keyword(_)) => self.is_keyword_method_at(offset),
            _ => false,
        }
    }

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
        if !matches!(self.peek_at(o), Some(TokenKind::Identifier(_))) {
            return DoubleColonSkip::Malformed(o);
        }
        o += 1;
        // Skip generic type parameters: `Name(Type, Type, ...)`
        if matches!(self.peek_at(o), Some(TokenKind::LeftParen)) {
            if let Some(after) = self.skip_paren_type_params(o) {
                o = after;
            }
        }
        // Skip union types: `| Type`
        while matches!(self.peek_at(o), Some(TokenKind::Pipe)) {
            o += 1;
            if !matches!(self.peek_at(o), Some(TokenKind::Identifier(_))) {
                return DoubleColonSkip::Malformed(o);
            }
            o += 1;
            // Skip generic type parameters on union member
            if matches!(self.peek_at(o), Some(TokenKind::LeftParen)) {
                if let Some(after) = self.skip_paren_type_params(o) {
                    o = after;
                }
            }
        }
        DoubleColonSkip::Valid(o)
    }

    /// Skips a parenthesized type parameter list in lookahead context.
    ///
    /// Starting at the `(` token, advances past `(Type, Type, ...)` and returns
    /// the offset after the closing `)`. Returns `None` if the sequence is
    /// malformed (e.g., missing closing paren or non-identifier content).
    ///
    /// Also handles bounded type parameters: `(T :: Printable, E)` — skips the
    /// `:: Bound` portion when present (ADR 0068 Phase 2d).
    pub(super) fn skip_paren_type_params(&self, offset: usize) -> Option<usize> {
        debug_assert!(matches!(self.peek_at(offset), Some(TokenKind::LeftParen)));
        let mut o = offset + 1; // past `(`
        // Handle empty parens `()`
        if matches!(self.peek_at(o), Some(TokenKind::RightParen)) {
            return Some(o + 1);
        }
        // First type param
        if !matches!(self.peek_at(o), Some(TokenKind::Identifier(_))) {
            return None;
        }
        o += 1;
        // Skip optional bound: `:: Protocol`
        o = self.skip_optional_type_param_bound(o);
        // Nested generic: `Name(Type(...))`
        if matches!(self.peek_at(o), Some(TokenKind::LeftParen)) {
            o = self.skip_paren_type_params(o)?;
        }
        // Union in type param position: `Type | Type`
        while matches!(self.peek_at(o), Some(TokenKind::Pipe)) {
            o += 1;
            if !matches!(self.peek_at(o), Some(TokenKind::Identifier(_))) {
                return None;
            }
            o += 1;
            if matches!(self.peek_at(o), Some(TokenKind::LeftParen)) {
                o = self.skip_paren_type_params(o)?;
            }
        }
        // Additional type params: `, Type`
        while is_comma_opt(self.peek_at(o)) {
            o += 1;
            if !matches!(self.peek_at(o), Some(TokenKind::Identifier(_))) {
                return None;
            }
            o += 1;
            // Skip optional bound: `:: Protocol`
            o = self.skip_optional_type_param_bound(o);
            // Nested generic
            if matches!(self.peek_at(o), Some(TokenKind::LeftParen)) {
                o = self.skip_paren_type_params(o)?;
            }
            // Union
            while matches!(self.peek_at(o), Some(TokenKind::Pipe)) {
                o += 1;
                if !matches!(self.peek_at(o), Some(TokenKind::Identifier(_))) {
                    return None;
                }
                o += 1;
                if matches!(self.peek_at(o), Some(TokenKind::LeftParen)) {
                    o = self.skip_paren_type_params(o)?;
                }
            }
        }
        if matches!(self.peek_at(o), Some(TokenKind::RightParen)) {
            Some(o + 1)
        } else {
            None
        }
    }

    /// Skips an optional `:: Identifier` bound in lookahead context.
    ///
    /// If the token at `offset` is `::` (`TypeAnnotation`) followed by an identifier,
    /// returns the offset past both. Otherwise returns `offset` unchanged.
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
        // Skip -> Type (and possible | Type unions, generic params)
        let mut o = offset + 1;
        // Allow `-> =>` (missing type) for error recovery
        if matches!(self.peek_at(o), Some(TokenKind::FatArrow)) {
            return true;
        }
        // Must have at least one type name
        if !matches!(self.peek_at(o), Some(TokenKind::Identifier(_))) {
            return false;
        }
        o += 1;
        // Skip generic type parameters: `Type(T, E)`
        if matches!(self.peek_at(o), Some(TokenKind::LeftParen)) {
            if let Some(after) = self.skip_paren_type_params(o) {
                o = after;
            }
        }
        // Skip union types: `| Type`
        while matches!(self.peek_at(o), Some(TokenKind::Pipe)) {
            o += 1;
            if !matches!(self.peek_at(o), Some(TokenKind::Identifier(_))) {
                return false;
            }
            o += 1;
            // Skip generic params on union member
            if matches!(self.peek_at(o), Some(TokenKind::LeftParen)) {
                if let Some(after) = self.skip_paren_type_params(o) {
                    o = after;
                }
            }
        }
        matches!(self.peek_at(o), Some(TokenKind::FatArrow))
    }

    /// Checks if there's a keyword method definition starting at the given offset.
    ///
    /// Pattern: `keyword: param keyword: param ... =>` or with typed params/return type.
    /// Delegates to the shared helper in `mod.rs`.
    fn is_keyword_method_at(&self, start_offset: usize) -> bool {
        self.is_keyword_method_params_at(start_offset)
    }

    /// Parses an `@expect category` or `@expect category "reason"` that
    /// precedes a declaration (BT-1856, BT-1918).
    ///
    /// Consumes the `@expect` token, the category identifier, and an
    /// optional reason string, returning the parsed category, optional
    /// reason, and the span of the directive. If the category is unknown,
    /// emits a parse error and returns `ExpectCategory::All` as a fallback
    /// so parsing can continue.
    fn parse_declaration_expect(&mut self) -> (ExpectCategory, Option<EcoString>, Span) {
        let start_token = self.advance(); // consume AtExpect
        let start = start_token.span();

        if let TokenKind::Identifier(name) = self.current_kind() {
            let name = name.clone();
            let end_token = self.advance();
            let mut span = start.merge(end_token.span());
            if let Some(category) = ExpectCategory::from_name(&name) {
                // BT-1918: Parse optional reason string after category (same line only).
                let reason = if matches!(self.current_kind(), TokenKind::String(_))
                    && !self.current_token().has_leading_newline()
                {
                    let reason_str = if let TokenKind::String(s) = self.current_kind() {
                        s.clone()
                    } else {
                        unreachable!()
                    };
                    let reason_token = self.advance();
                    span = start.merge(reason_token.span());
                    Some(reason_str)
                } else {
                    None
                };
                (category, reason, span)
            } else {
                let valid = ExpectCategory::valid_names().join(", ");
                let message =
                    format!("unknown @expect category '{name}', valid categories are: {valid}");
                self.diagnostics.push(Diagnostic::error(message, span));
                // Consume trailing reason string to avoid secondary parse errors.
                if matches!(self.current_kind(), TokenKind::String(_))
                    && !self.current_token().has_leading_newline()
                {
                    self.advance();
                }
                (ExpectCategory::All, None, span)
            }
        } else {
            let valid = ExpectCategory::valid_names().join(", ");
            let message = format!("@expect must be followed by a category name ({valid})");
            self.diagnostics.push(Diagnostic::error(message, start));
            (ExpectCategory::All, None, start)
        }
    }

    /// Parses a state/field declaration.
    ///
    /// Syntax:
    /// - `state: fieldName` / `field: fieldName`
    /// - `state: fieldName = defaultValue` / `field: fieldName = defaultValue`
    /// - `state: fieldName :: TypeName` / `field: fieldName :: TypeName`
    /// - `state: fieldName :: TypeName = defaultValue` / `field: ...`
    fn parse_state_declaration(&mut self) -> Option<StateDeclaration> {
        let start = self.current_token().span();
        let doc_comment = self.collect_doc_comment();
        let mut comments = self.collect_comment_attachment();

        // Determine which keyword was used and consume it
        let declared_keyword = if matches!(self.current_kind(), TokenKind::Keyword(k) if k == "state:")
        {
            DeclaredKeyword::State
        } else if matches!(self.current_kind(), TokenKind::Keyword(k) if k == "field:") {
            DeclaredKeyword::Field
        } else {
            return None;
        };
        let keyword_label = declared_keyword.as_str().trim();
        self.advance();

        // Parse field name and optional type annotation
        let (name, type_annotation) = if let TokenKind::Identifier(_) = self.current_kind() {
            let name_ident =
                self.parse_identifier(&format!("Expected field name after '{keyword_label}'"));

            // Check for optional type annotation (:: TypeName)
            let type_ann = if self.match_token(&TokenKind::DoubleColon) {
                Some(self.parse_type_annotation())
            } else if matches!(self.current_kind(), TokenKind::Colon) {
                self.type_annotation_colon_error();
                Some(self.parse_type_annotation())
            } else {
                None
            };
            (name_ident, type_ann)
        } else {
            self.error(format!("Expected field name after '{keyword_label}'"));
            let span = self.current_token().span();
            (Identifier::new("Error", span), None)
        };

        // Check for default value (= expression)
        let default_value = if matches!(self.current_kind(), TokenKind::BinarySelector(s) if s == "=")
        {
            self.advance(); // consume `=`
            Some(self.parse_expression())
        } else {
            None
        };

        // Collect trailing end-of-line comment (e.g. `state: x = 0  // comment`)
        comments.trailing = self.collect_trailing_comment();

        let end = default_value
            .as_ref()
            .map(Expression::span)
            .or(type_annotation.as_ref().map(TypeAnnotation::span))
            .unwrap_or(name.span);
        let span = start.merge(end);

        Some(StateDeclaration {
            name,
            type_annotation,
            default_value,
            declared_keyword,
            expect: None,
            comments,
            doc_comment,
            span,
        })
    }

    /// Parses a class variable declaration (BT-412).
    ///
    /// Syntax is identical to state declarations but uses `classState:` keyword:
    /// - `classState: varName`
    /// - `classState: varName = defaultValue`
    /// - `classState: varName :: TypeName`
    /// - `classState: varName :: TypeName = defaultValue`
    fn parse_classvar_declaration(&mut self) -> Option<StateDeclaration> {
        let start = self.current_token().span();
        let doc_comment = self.collect_doc_comment();
        let mut comments = self.collect_comment_attachment();

        // Consume `classState:`
        if !matches!(self.current_kind(), TokenKind::Keyword(k) if k == "classState:") {
            return None;
        }
        self.advance();

        let (name, type_annotation) = if let TokenKind::Identifier(_) = self.current_kind() {
            let name_ident = self.parse_identifier("Expected variable name after 'classState:'");
            let type_ann = if self.match_token(&TokenKind::DoubleColon) {
                Some(self.parse_type_annotation())
            } else if matches!(self.current_kind(), TokenKind::Colon) {
                self.type_annotation_colon_error();
                Some(self.parse_type_annotation())
            } else {
                None
            };
            (name_ident, type_ann)
        } else {
            self.error("Expected variable name after 'classState:'");
            let span = self.current_token().span();
            (Identifier::new("Error", span), None)
        };

        let default_value = if matches!(self.current_kind(), TokenKind::BinarySelector(s) if s == "=")
        {
            self.advance();
            Some(self.parse_expression())
        } else {
            None
        };

        // Collect trailing end-of-line comment (e.g. `classState: x = 0  // comment`)
        comments.trailing = self.collect_trailing_comment();

        let end = default_value
            .as_ref()
            .map(Expression::span)
            .or(type_annotation.as_ref().map(TypeAnnotation::span))
            .unwrap_or(name.span);
        let span = start.merge(end);

        Some(StateDeclaration {
            name,
            type_annotation,
            default_value,
            declared_keyword: DeclaredKeyword::State,
            expect: None,
            comments,
            doc_comment,
            span,
        })
    }

    /// Parses a type annotation, including union types (`Integer | String`).
    fn parse_type_annotation(&mut self) -> TypeAnnotation {
        let first = self.parse_single_type_annotation();

        // Check for union type: `Type | Type | ...`
        if !matches!(self.current_kind(), TokenKind::Pipe) {
            return first;
        }

        let start_span = first.span();
        let mut types = vec![first];

        while matches!(self.current_kind(), TokenKind::Pipe) {
            self.advance(); // consume `|`
            types.push(self.parse_single_type_annotation());
        }

        let end_span = types.last().map_or(start_span, TypeAnnotation::span);
        TypeAnnotation::Union {
            types,
            span: start_span.merge(end_span),
        }
    }

    /// Parses a single type annotation (no unions).
    ///
    /// Handles:
    /// - Simple types: `Integer`, `String`
    /// - Generic types: `Result(T, E)`, `Array(Integer)`, `Block(T, Result(R, E))`
    /// - Self type: `Self`
    fn parse_single_type_annotation(&mut self) -> TypeAnnotation {
        if let TokenKind::Identifier(name) = self.current_kind() {
            let span = self.current_token().span();
            if name.as_str() == "Self" {
                self.advance();
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
                } else {
                    TypeAnnotation::Simple(ident)
                }
            }
        } else {
            let span = self.current_token().span();
            self.error("Expected type name");
            TypeAnnotation::Simple(Identifier::new("Error", span))
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
    fn parse_optional_return_type(&mut self) -> Option<TypeAnnotation> {
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

    /// Parses a method definition.
    ///
    /// Syntax:
    /// - `methodName => body`
    /// - `+ other => body`
    /// - `at: index put: value => body`
    /// - `sealed methodName => body`
    /// - `internal methodName => body` (ADR 0071)
    fn parse_method_definition(&mut self) -> Option<MethodDefinition> {
        let start = self.current_token().span();
        let doc_comment = self.collect_doc_comment();
        let comments = self.collect_comment_attachment();
        let method_kind = MethodKind::Primary;
        let mut method_is_sealed = false;
        let mut method_is_internal = false;
        let mut _is_class_method = false;

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
                        _is_class_method = true;
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
        let body = self.parse_method_body();
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
                kind: method_kind,
            },
            span,
        );
        method.doc_comment = doc_comment;
        method.comments = comments;
        Some(method)
    }

    /// Parses a method selector and its parameters.
    ///
    /// Returns the selector and parameter definitions (with optional type annotations).
    fn parse_method_selector(&mut self) -> Option<(MessageSelector, Vec<ParameterDefinition>)> {
        match self.current_kind() {
            // Unary method: `methodName`
            TokenKind::Identifier(name) => {
                let selector = MessageSelector::Unary(name.clone());
                self.advance();
                Some((selector, Vec::new()))
            }
            // Binary method: `+ other`, `-> other` (ADR 0047), `>> other` (BT-1735)
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

                let param_name =
                    self.parse_identifier("Expected parameter name after binary selector");
                let type_annotation = self.parse_optional_param_type();
                let param = match type_annotation {
                    Some(ta) => ParameterDefinition::with_type(param_name, ta),
                    None => ParameterDefinition::new(param_name),
                };
                Some((selector, vec![param]))
            }
            // Keyword method: `at: index put: value` or `deposit: amount :: Integer`
            TokenKind::Keyword(_) => {
                let mut keywords = Vec::new();
                let mut parameters = Vec::new();

                while let TokenKind::Keyword(keyword) = self.current_kind() {
                    let span = self.current_token().span();
                    keywords.push(KeywordPart::new(keyword.clone(), span));
                    self.advance();

                    // Parse parameter name, then optional `:: Type` annotation
                    let param_name = self.parse_identifier("Expected parameter name after keyword");
                    let type_annotation = self.parse_optional_param_type();
                    let param = match type_annotation {
                        Some(ta) => ParameterDefinition::with_type(param_name, ta),
                        None => ParameterDefinition::new(param_name),
                    };
                    parameters.push(param);
                }

                let selector = MessageSelector::Keyword(keywords);
                Some((selector, parameters))
            }
            _ => {
                self.error("Expected method selector");
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

    /// Parses a method body (expressions until the next method or end of class).
    ///
    /// Statements are separated by periods or newlines (BT-360).
    pub(super) fn parse_method_body(&mut self) -> Vec<ExpressionStatement> {
        let mut body = Vec::new();

        // Parse expressions until we hit something that looks like a new method,
        // state declaration, class definition, or standalone method definition
        // BT-903: When inside a class body, a token at column 0 after a newline
        // is outside the class body (trailing expression). Break to avoid consuming it.
        //
        // BT-1294: Guard `is_at_method_definition()` with an indentation check.
        // After the formatter breaks 3+ keyword messages onto their own indented
        // lines, continuation keywords appear at col 4–6 inside the method body.
        // `is_at_method_definition()` does a wide lookahead and can find `-> Type =>`
        // from the *next* method's signature, causing a false positive that exits
        // the body early and treats continuation keywords as a new method selector.
        // When `in_class_body` is true, canonical class members start at col 2;
        // any token deeper than col 2 is part of an expression, never a member.
        #[allow(clippy::nonminimal_bool)]
        while !self.is_at_end()
            && !self.is_at_class_definition()
            && !self.is_at_protocol_definition()
            && !(
                // Only test is_at_method_definition when the token could plausibly
                // be a class member (indentation <= 2). Deeper tokens are continuation
                // expressions and must not trigger early exit from the method body.
                (!self.in_class_body
                    || self
                        .current_token()
                        .indentation_after_newline()
                        .is_none_or(|col| col <= 2))
                    && self.is_at_method_definition()
            )
            && !self.is_at_standalone_method_definition()
            && !matches!(self.current_kind(), TokenKind::Keyword(k) if k == "state:" || k == "field:" || k == "classState:")
            && !(self.in_class_body && self.current_token().indentation_after_newline() == Some(0))
        {
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
                // BT-368: After synchronization, check if we can continue parsing
                // If synchronize stopped at a method/class/state boundary, break
                if self.is_at_end()
                    || self.is_at_class_definition()
                    || self.is_at_protocol_definition()
                    || self.is_at_method_definition()
                    || self.is_at_standalone_method_definition()
                    || matches!(self.current_kind(), TokenKind::Keyword(k) if k == "state:" || k == "field:" || k == "classState:")
                {
                    break;
                }
                // Otherwise, synchronize stopped at a newline or period, so continue parsing
                continue;
            }

            // Period, bang (!), or newline separates statements
            if self.match_token(&TokenKind::Period) {
                // Explicit period — check if next token starts a new method/state/class
                let period_span = self.tokens[self.current - 1].span();
                if self.is_at_end()
                    || self.is_at_class_definition()
                    || self.is_at_protocol_definition()
                    || self.is_at_method_definition()
                    || self.is_at_standalone_method_definition()
                    || matches!(self.current_kind(), TokenKind::Keyword(k) if k == "state:" || k == "field:" || k == "classState:")
                {
                    // Trailing period at end of method — not needed (BT-948)
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
                match body.last_mut().map(|s| &mut s.expression) {
                    Some(Expression::MessageSend { is_cast, .. }) => *is_cast = true,
                    Some(last) => {
                        let span = last.span();
                        self.cast_in_expression_error(span);
                    }
                    None => {}
                }
                // Check if next token starts a new method/state/class (same as period)
                if self.is_at_end()
                    || self.is_at_class_definition()
                    || self.is_at_protocol_definition()
                    || self.is_at_method_definition()
                    || self.is_at_standalone_method_definition()
                    || matches!(self.current_kind(), TokenKind::Keyword(k) if k == "state:" || k == "field:" || k == "classState:")
                {
                    break;
                }
            } else if !self.is_at_end() && self.current_token().has_leading_newline() {
                // BT-885: If the next token is at column 0 (no indentation), it's a
                // top-level expression, not part of this method body. This allows
                // trailing expressions after inline class definitions.
                if self.current_token().leading_indent() == Some(0) {
                    break;
                }
                // Newline acts as implicit statement separator (BT-360)
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
    /// - `package@ClassName >> selector => body` (cross-package extension, ADR 0070)
    /// - `package@ClassName class >> selector => body`
    pub(super) fn parse_standalone_method_definition(&mut self) -> StandaloneMethodDefinition {
        let start = self.current_token().span();

        // Collect leading comments from the class-name token's leading trivia.
        // parse_identifier() advances past the class name without reading trivia,
        // so we must collect here before the token is consumed.
        let mut class_leading_comments = self.collect_comment_attachment().leading;

        // Parse class name, with optional package qualifier (ADR 0070).
        // Pattern: `identifier @ Identifier` or just `Identifier`.
        let (class_name, package) = if matches!(self.current_kind(), TokenKind::Identifier(name) if !name.starts_with(|c: char| c.is_uppercase()))
            && matches!(self.peek_at(1), Some(TokenKind::At))
        {
            // Package-qualified: `json@Parser`
            let pkg_ident = self.parse_identifier("Expected package name");
            self.advance(); // consume `@`
            let cls_ident = self.parse_identifier("Expected class name after '@'");
            (cls_ident, Some(pkg_ident))
        } else {
            // Unqualified: `Counter`
            let cls_ident = self.parse_identifier("Expected class name");
            (cls_ident, None)
        };

        // Check for optional `class` modifier
        let is_class_method = if matches!(self.current_kind(), TokenKind::Identifier(name) if name == "class")
        {
            // Only treat as modifier if next token is `>>` (GtGt since BT-663)
            if matches!(self.peek_at(1), Some(TokenKind::GtGt)) {
                self.advance(); // consume `class`
                true
            } else {
                false
            }
        } else {
            false
        };

        // Consume `>>` (GtGt token since BT-663)
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

        // Prepend the class-name token's leading comments to the method's
        // own comments.  parse_method_definition() reads trivia from the
        // selector token, so comments before the class name would otherwise
        // be lost.
        if !class_leading_comments.is_empty() {
            class_leading_comments.append(&mut method.comments.leading);
            method.comments.leading = class_leading_comments;
        }

        let span = start.merge(method.span);

        StandaloneMethodDefinition {
            class_name,
            package,
            is_class_method,
            method,
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
        let comments = self.collect_comment_attachment();

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

        // Parse optional `extending:` clause
        let extending = if matches!(self.current_kind(), TokenKind::Keyword(k) if k == "extending:")
        {
            self.advance(); // consume `extending:`
            Some(self.parse_identifier("Expected protocol name after 'extending:'"))
        } else {
            None
        };

        // Parse protocol body (method signatures without =>)
        let (method_signatures, class_method_signatures) = self.parse_protocol_body();

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
        let span = start.merge(end);

        ProtocolDefinition {
            name,
            type_params,
            extending,
            method_signatures,
            class_method_signatures,
            comments,
            doc_comment,
            span,
        }
    }

    /// Parses the body of a protocol definition (method signatures without `=>`).
    ///
    /// Protocol method signatures have the same selector and parameter syntax as
    /// class methods, but end before `=>`. They may include optional type annotations
    /// and return types. Signatures prefixed with `class` are collected separately
    /// as class method requirements (BT-1611).
    ///
    /// The body ends when we hit:
    /// - EOF
    /// - Another protocol definition (`Protocol define:`)
    /// - A class definition (`Superclass subclass:`)
    /// - A standalone method definition (`Class >>`)
    ///
    /// Returns `(instance_signatures, class_method_signatures)`.
    fn parse_protocol_body(
        &mut self,
    ) -> (Vec<ProtocolMethodSignature>, Vec<ProtocolMethodSignature>) {
        let mut signatures = Vec::new();
        let mut class_signatures = Vec::new();

        // Skip any periods/statement terminators
        while self.match_token(&TokenKind::Period) {}

        while !self.is_at_end()
            && !self.is_at_protocol_definition()
            && !self.is_at_class_definition()
            && !self.is_at_standalone_method_definition()
        {
            // BT-1618: Collect doc comment *before* checking for `class` prefix,
            // because the doc comment is leading trivia on the `class` token and
            // would be lost when we advance past it.
            let doc_comment = self.collect_doc_comment();

            // BT-1611: Detect `class` prefix for class method signatures.
            // Use the same lookahead as class definition parsing: `class` followed
            // by something other than `=>` or `-> Type =>` means it's a modifier.
            let is_class_method = matches!(
                self.current_kind(),
                TokenKind::Identifier(name) if name == "class"
            ) && !self.is_fat_arrow_or_return_type(1);

            if is_class_method {
                self.advance(); // consume `class`
            }

            if let Some(sig) = self.parse_protocol_method_signature_with_doc(doc_comment) {
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

        (signatures, class_signatures)
    }

    /// Parses a single protocol method signature (no `=>` body).
    ///
    /// Returns `None` if the current position doesn't look like a method signature.
    ///
    /// If `pre_doc` is `Some`, it is used as the doc comment (already collected
    /// by the caller before consuming a `class` prefix). Otherwise, collects
    /// the doc comment from the current token's leading trivia.
    ///
    /// Syntax:
    /// - Unary: `asString -> String`
    /// - Binary: `< other :: Self -> Boolean`
    /// - Keyword: `do: block :: Block(E, Object)`
    fn parse_protocol_method_signature_with_doc(
        &mut self,
        pre_doc: Option<String>,
    ) -> Option<ProtocolMethodSignature> {
        let start = self.current_token().span();
        let doc_comment = pre_doc.or_else(|| self.collect_doc_comment());
        let comments = self.collect_comment_attachment();

        // Determine what kind of signature this is
        let (selector, parameters) = match self.current_kind() {
            // Unary: identifier (not followed by `=>`, not a known delimiter keyword)
            TokenKind::Identifier(name) => {
                // Guard against consuming non-signature identifiers
                // (e.g., `Protocol` starting the next definition)
                if name == "Protocol" || name == "abstract" || name == "sealed" || name == "typed" {
                    return None;
                }
                let selector = MessageSelector::Unary(name.clone());
                self.advance();
                (selector, Vec::new())
            }
            // Binary: `+ other`, `< other`, `-> other` (ADR 0047), `>> other` (BT-1735)
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

                let param_name =
                    self.parse_identifier("Expected parameter name after binary selector");
                let type_annotation = self.parse_optional_param_type();
                let param = match type_annotation {
                    Some(ta) => ParameterDefinition::with_type(param_name, ta),
                    None => ParameterDefinition::new(param_name),
                };
                (selector, vec![param])
            }
            // Keyword: `do: block :: Block(E, Object)`
            TokenKind::Keyword(kw) => {
                // Guard: `extending:` is the protocol extension keyword, not a selector
                if kw == "extending:" {
                    return None;
                }
                let mut keywords = Vec::new();
                let mut parameters = Vec::new();

                while let TokenKind::Keyword(keyword) = self.current_kind() {
                    // In protocol bodies, keywords on a new line are separate
                    // signatures. Only continue if the keyword is on the same
                    // line as the previous one (i.e., a multi-keyword selector
                    // like `at: key put: value`).
                    if !keywords.is_empty() && self.current_token().has_leading_newline() {
                        break;
                    }
                    let span = self.current_token().span();
                    keywords.push(KeywordPart::new(keyword.clone(), span));
                    self.advance();

                    let param_name = self.parse_identifier("Expected parameter name after keyword");
                    let type_annotation = self.parse_optional_param_type();
                    let param = match type_annotation {
                        Some(ta) => ParameterDefinition::with_type(param_name, ta),
                        None => ParameterDefinition::new(param_name),
                    };
                    parameters.push(param);
                }

                let selector = MessageSelector::Keyword(keywords);
                (selector, parameters)
            }
            _ => return None,
        };

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
