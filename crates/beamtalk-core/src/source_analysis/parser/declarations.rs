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
    StandaloneMethodDefinition, StateDeclaration, TypeAliasDefinition, TypeAnnotation,
    TypeParamDecl,
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

/// Returns `true` if the optional token kind can begin a type name in
/// lookahead — an identifier (`Integer`, `Self`) or a singleton symbol
/// (`#foo`, lexed as [`TokenKind::Symbol`]). BT-2627: singleton type
/// annotations are subtypes of `Symbol` (ADR 0068).
fn is_type_name_token(kind: Option<&TokenKind>) -> bool {
    matches!(kind, Some(TokenKind::Identifier(_) | TokenKind::Symbol(_)))
}

/// BT-1856 / BT-2829: a consumed declaration-level `@expect category`,
/// bundled with the doc comment and plain leading comments that sat in its
/// own leading trivia (see [`Parser::parse_pending_declaration_expect`]).
/// `Default` (all `None`/empty) means no `@expect` was present.
#[derive(Default)]
struct PendingDeclarationExpect {
    expect: Option<(ExpectCategory, Option<EcoString>, Span)>,
    doc_comment: Option<String>,
    comments: CommentAttachment,
}

impl PendingDeclarationExpect {
    /// Applies this pending `@expect`/doc comment/leading comments onto the
    /// declaration that follows it in source. `expect` always overwrites (a
    /// declaration can't already carry one — nothing sets it before this
    /// call); `doc_comment`/`comments` only fill in if the declaration's own
    /// token had none of its own (BT-2829: both were left dangling on the
    /// `@expect` token's leading trivia otherwise — see
    /// `Parser::parse_pending_declaration_expect`).
    fn apply_to(
        self,
        expect: &mut Option<(ExpectCategory, Option<EcoString>, Span)>,
        doc_comment: &mut Option<String>,
        comments: &mut CommentAttachment,
    ) {
        if self.expect.is_some() {
            *expect = self.expect;
        }
        if doc_comment.is_none() {
            *doc_comment = self.doc_comment;
        }
        if comments.is_empty() {
            *comments = self.comments;
        }
    }
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

        // Parse optional `handleScope: #symbol` clause (ADR 0103). Appears at
        // the head of the class body, like `native:` is a header clause.
        let handle_scope = self.parse_optional_handle_scope();

        // Parse class body (state declarations, instance methods, class methods, class variables)
        let (state, methods, class_methods, class_variables) = self.parse_class_body();

        // Determine end span: max of last instance method, class method, state, class var, or name
        let mut end = name.span;
        if let Some(ref bm) = backing_module {
            end = end.merge(bm.span);
        }
        // ADR 0103: include the `handleScope:` clause so a scope-only class body
        // (no state/methods) still spans its declaration — hover/diagnostic
        // ranges must cover it.
        if let Some(ref hs) = handle_scope {
            end = end.merge(hs.span);
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
        class_def.handle_scope = handle_scope;
        class_def
    }

    /// Parses an optional class-side `handleScope: #symbol` clause (ADR 0103).
    ///
    /// The scope value is symbol-valued and the set is deliberately **open**
    /// (`#process` / `#node` ship first) — the parser accepts any symbol and
    /// lets the checker decide meaning, so no closed enum is hardcoded here.
    /// Returns the bare symbol as an [`Identifier`] (name without the leading
    /// `#`), or `None` when no clause is present.
    fn parse_optional_handle_scope(&mut self) -> Option<Identifier> {
        if !matches!(self.current_kind(), TokenKind::Keyword(k) if k == "handleScope:") {
            return None;
        }
        self.advance(); // consume `handleScope:`
        if let TokenKind::Symbol(name) = self.current_kind() {
            let name = name.clone();
            let span = self.current_token().span();
            self.advance(); // consume the symbol
            Some(Identifier::new(name, span))
        } else {
            self.error("Expected a symbol (e.g. #process or #node) after 'handleScope:'");
            // Consume the offending token so class-body parsing recovers cleanly
            // instead of cascading on it (mirrors the `native:` error path).
            if !self.current_token().has_leading_newline() && !self.is_at_end() {
                self.advance();
            }
            None
        }
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

    /// BT-1856: If the current token is a declaration-level `@expect
    /// category`, consume it (and its own doc comment / leading `//`
    /// comments) for the caller to attach to whichever state/method
    /// declaration follows. Returns `PendingDeclarationExpect::default()`
    /// (all `None`/empty) when the current token isn't `@expect`.
    ///
    /// BT-2829: both a `/// ...` doc comment and any leading `//`/`/* */`
    /// comments written above a declaration-level `@expect` sit in the
    /// `@expect` token's own leading trivia, not the following
    /// declaration's — `collect_doc_comment()` and
    /// `collect_comment_attachment()` only ever look at the *current* token.
    /// Collect both here, while `@expect` is still current, so neither is
    /// left dangling — the doc comment would otherwise surface as a spurious
    /// "doc comment not attached" warning, and the plain comments would
    /// silently vanish (no such warning exists for them).
    fn parse_pending_declaration_expect(&mut self) -> PendingDeclarationExpect {
        if !matches!(self.current_kind(), TokenKind::AtExpect) {
            return PendingDeclarationExpect::default();
        }
        // Doc comment first, then plain comments — same order
        // `parse_method_definition`/`parse_state_declaration` use, so a doc
        // comment that successfully attaches here is not *also* re-added as
        // a fallback plain comment by `collect_comment_attachment`'s own
        // `unattached_doc_comment_indices` check.
        let doc_comment = self.collect_doc_comment();
        let comments = self.collect_comment_attachment();
        let expect = Some(self.parse_declaration_expect());
        PendingDeclarationExpect {
            expect,
            doc_comment,
            comments,
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
            && !self.is_at_type_alias_definition()
            && !self.is_at_standalone_method_definition()
        {
            let pending = self.parse_pending_declaration_expect();

            // Check for state/field declaration: `state: fieldName ...` or `field: fieldName ...`
            if matches!(self.current_kind(), TokenKind::Keyword(k) if k == "state:" || k == "field:")
            {
                if let Some(mut state_decl) = self.parse_state_declaration() {
                    pending.apply_to(
                        &mut state_decl.expect,
                        &mut state_decl.doc_comment,
                        &mut state_decl.comments,
                    );
                    state.push(state_decl);
                }
            }
            // Check for class variable declaration: `classState: varName ...`
            else if matches!(self.current_kind(), TokenKind::Keyword(k) if k == "classState:") {
                if let Some(mut classvar_decl) = self.parse_classvar_declaration() {
                    pending.apply_to(
                        &mut classvar_decl.expect,
                        &mut classvar_decl.doc_comment,
                        &mut classvar_decl.comments,
                    );
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
                    pending.apply_to(
                        &mut method.expect,
                        &mut method.doc_comment,
                        &mut method.comments,
                    );
                    if is_class_method {
                        // `parse_method_definition` also records the `class ` modifier,
                        // but set it here too so every `class_methods` entry carries the
                        // flag regardless of how the modifier was tokenised (BT-2594).
                        method.is_class_method = true;
                        class_methods.push(method);
                    } else {
                        methods.push(method);
                    }
                }
            } else if matches!(self.current_kind(), TokenKind::Keyword(k) if k == "handleScope:") {
                // ADR 0103: `handleScope:` is a header clause parsed *before* the
                // body (see `parse_optional_handle_scope`). Reaching it here means
                // it was misplaced after state/method declarations — emit a
                // targeted error and consume the clause so parsing recovers
                // instead of treating it as a stray keyword message.
                self.error(
                    "'handleScope:' must appear in the class header, before state or method declarations",
                );
                self.advance(); // consume `handleScope:`
                if !self.current_token().has_leading_newline()
                    && matches!(self.current_kind(), TokenKind::Symbol(_))
                {
                    self.advance(); // consume the symbol argument to recover
                }
            } else {
                // BT-1856: @expect before an invalid position (e.g., end of class body)
                if let Some((_, _, span)) = pending.expect {
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

    /// BT-2829: checks whether the current token is a declaration-level
    /// `@expect` — one sitting at the same class-member boundary
    /// (indentation <= 2, *inside* a class body) a fresh method/state
    /// declaration would.
    ///
    /// `parse_method_body`'s statement loop must stop here exactly like it
    /// stops at `is_at_method_definition()`: otherwise a declaration-level
    /// `@expect` gets swallowed as a trailing statement of the *previous*
    /// method's body, `parse_class_body`'s own `pending_expect` capture
    /// (which only triggers when `AtExpect` is the *current* token at the
    /// top of its loop) never sees it, and the directive's target
    /// method/state-declaration keeps `expect: None` — the suppression
    /// silently fails and the swallowed directive itself gets reported stale.
    /// Statement-level `@expect` inside a body (e.g. as the first statement
    /// of a method) sits deeper than col 2 and is unaffected.
    ///
    /// Unlike `is_at_method_definition()`, this is gated on `in_class_body`
    /// alone (no `!self.in_class_body` fallback): declaration-level `@expect`
    /// is only ever captured by `parse_class_body`'s `pending_expect` loop,
    /// so outside a class body — e.g. a Tonel-style standalone `Class >>
    /// selector => body` method, which has no such capture — an `@expect`
    /// can only ever be the ordinary statement-level directive, never a
    /// boundary, regardless of indentation.
    pub(super) fn is_at_declaration_level_expect(&self) -> bool {
        self.in_class_body
            && self
                .current_token()
                .indentation_after_newline()
                .is_none_or(|col| col <= 2)
            && matches!(self.current_kind(), TokenKind::AtExpect)
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
    /// parenthesised type group `( ... )` (BT-2760).
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
        if !is_type_name_token(self.peek_at(offset)) {
            return None;
        }
        let mut o = self.skip_type_name_with_metatype(offset);
        // Skip generic type parameters: `Name(Type, Type, ...)`
        if matches!(self.peek_at(o), Some(TokenKind::LeftParen)) {
            if let Some(after) = self.skip_paren_type_params(o) {
                o = after;
            }
        }
        Some(o)
    }

    /// Skips a parenthesised type group `( Type (op Type)* )` in lookahead
    /// context (BT-2760), keeping lookahead in lock-step with
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
    /// metatype suffixes in type argument position: `List(Actor class)` (BT-2630).
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
    /// not a valid bound (BT-2627).
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
        // Must have at least one type operand: a type name (BT-1952 /
        // BT-2034: including a trailing `class` metatype token, so `-> Self
        // class =>` and `-> Actor class | Nil =>` are recognized) or a
        // parenthesised group (BT-2760: `-> (A & B) \ #c =>`).
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
    /// is the bare identifier `class` on the *same* line (BT-1952 / BT-2034).
    /// A `class` token with a leading newline begins a new statement (e.g.
    /// the class-method definition on the following line) and is not consumed.
    ///
    /// BT-2627: also called with a singleton `#foo` ([`TokenKind::Symbol`]) at
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
    /// the `\\` typo for `\` (ADR 0102 §3, BT-2742, BT-2743).
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
        // `(A | B) | C`, only reachable via grouping parens (BT-2760) — is
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

        loop {
            let TokenKind::BinarySelector(op) = self.current_kind() else {
                break;
            };
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
    /// - Grouping parentheses: `(Type)` (BT-2760, see below)
    pub(super) fn parse_single_type_annotation(&mut self) -> TypeAnnotation {
        if matches!(self.current_kind(), TokenKind::LeftParen) {
            // Grouping parentheses in type-annotation position (BT-2760,
            // unblocking ADR 0102 §3's mixed `&`/`\` disambiguation). Parsed
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
                // Check for `Self class` metatype annotation on the same line
                // (BT-1952 / BT-2034). A `class` token with a leading newline
                // starts a new statement — typically a class-method definition
                // on the next line — and must not be consumed as a metatype
                // suffix.
                if let TokenKind::Identifier(next) = self.current_kind() {
                    if next.as_str() == "class" && !self.current_token().has_leading_newline() {
                        let end_span = self.current_token().span();
                        self.advance();
                        return TypeAnnotation::SelfClass {
                            span: span.merge(end_span),
                        };
                    }
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
                    // Check for `<ClassName> class` metatype annotation (BT-2034).
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
        } else if let TokenKind::Symbol(name) = self.current_kind() {
            // BT-2627: Singleton type annotation `#foo` — a subtype of `Symbol`
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
    pub(super) fn parse_method_definition(&mut self) -> Option<MethodDefinition> {
        let start = self.current_token().span();
        let doc_comment = self.collect_doc_comment();
        let comments = self.collect_comment_attachment();
        let method_kind = MethodKind::Primary;
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
        // infer it (BT-2724).
        let previous_method_selector = self.current_method_selector.take();
        self.current_method_selector = Some(selector.name());
        let body = self.parse_method_body();
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

    /// Checks whether the current position starts a new class/protocol/
    /// type-alias/method/standalone-method declaration, or a `state:`/
    /// `field:`/`classState:` keyword — the set of tokens that always end a
    /// method body no matter how the body was left (a trailing period, a
    /// cast `!`, or error recovery). Factored out of `parse_method_body`
    /// because the same six-way check is needed at three separate exit
    /// points there.
    fn is_at_member_boundary(&self) -> bool {
        self.is_at_end()
            || self.is_at_class_definition()
            || self.is_at_protocol_definition()
            || self.is_at_type_alias_boundary()
            || self.is_at_method_definition()
            || self.is_at_standalone_method_definition()
            || matches!(self.current_kind(), TokenKind::Keyword(k) if k == "state:" || k == "field:" || k == "classState:")
    }

    /// `is_at_type_alias_definition()`, gated by the same indentation guard
    /// `is_at_method_definition()` uses in `parse_method_body` (BT-1294).
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
        (!self.in_class_body
            || self
                .current_token()
                .indentation_after_newline()
                .is_none_or(|col| col <= 2))
            && self.is_at_type_alias_definition()
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
            && !self.is_at_type_alias_boundary()
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
            // BT-2829: a declaration-level `@expect` must end the body of the
            // *previous* method, just like a fresh method/state declaration
            // does — see `is_at_declaration_level_expect`'s doc comment.
            && !self.is_at_declaration_level_expect()
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
                if self.is_at_member_boundary() {
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
    // Type Alias Definition Parsing (ADR 0108, Phase 1)
    // ========================================================================

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
        let comments = self.collect_comment_attachment();

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
            && !self.is_at_type_alias_definition()
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
