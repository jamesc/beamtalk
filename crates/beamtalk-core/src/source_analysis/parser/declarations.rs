// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Declaration parsing for Beamtalk.
//!
//! This module handles parsing of top-level declarations including:
//! - Class definitions with modifiers (`abstract`, `sealed`, `typed`, `internal`)
//! - State (field) declarations with types and default values
//! - Method definitions with optional `sealed` or `internal` modifiers
//!
//! Type-annotation parsing lives in `types.rs`, method parsing in `methods.rs`, and
//! type alias / protocol / native declarations in `protocols.rs`.

use crate::ast::{
    ClassDefinition, ClassModifiers, CommentAttachment, DeclaredKeyword, ExpectCategory,
    Expression, Identifier, Literal, MethodDefinition, ProtocolUse, ShapeVersionDeclaration,
    SlotKind, StateDeclaration, TypeAnnotation, TypeParamDecl,
};
use crate::source_analysis::{Span, TokenKind};
use ecow::EcoString;

use super::types::DoubleColonSkip;
use super::{Diagnostic, Parser};

/// Returns `true` if the token kind is a comma binary selector.
pub(super) fn is_comma(kind: &TokenKind) -> bool {
    matches!(kind, TokenKind::BinarySelector(s) if s.as_str() == ",")
}

/// Returns `true` if the token kind is the `state:` or `field:` keyword that
/// starts a state/field declaration (see [`Parser::parse_state_declaration`]).
fn is_state_or_field_keyword(kind: &TokenKind) -> bool {
    matches!(kind, TokenKind::Keyword(k) if k == "state:" || k == "field:")
}

/// Returns `true` if the token kind is the `classState:` keyword that starts
/// a class-variable declaration (see [`Parser::parse_classvar_declaration`]).
pub(super) fn is_class_state_keyword(kind: &TokenKind) -> bool {
    matches!(kind, TokenKind::Keyword(k) if k == "classState:")
}

/// Returns `true` if the token kind is the `handleScope:` keyword (ADR 0103).
/// Only ever valid as a class-header clause, parsed before the class body
/// loop runs (see [`Parser::parse_optional_handle_scope`]); a `handleScope:`
/// reached *inside* the body loop is a misplaced-clause error.
fn is_handle_scope_keyword(kind: &TokenKind) -> bool {
    matches!(kind, TokenKind::Keyword(k) if k == "handleScope:")
}

/// Returns `true` if the token kind is the `shapeVersion:` keyword
/// (ADR 0123 §1). Like `handleScope:`, only ever valid as a class-header
/// clause parsed before the class body loop runs (see
/// [`Parser::parse_optional_shape_version`]); a `shapeVersion:` reached
/// *inside* the body loop is a misplaced-clause error. Deliberately not
/// added to [`is_state_like_declaration_keyword`]'s set — that set is the
/// single source for where a method body ends, and `shapeVersion:` is a
/// header-only clause with no benefit from joining it (ADR 0123 §1).
fn is_shape_version_keyword(kind: &TokenKind) -> bool {
    matches!(kind, TokenKind::Keyword(k) if k == "shapeVersion:")
}

/// Returns `true` if the token kind is the `uses:` keyword — the trait
/// composition clause reserved at the start of a class-body line (ADR 0127
/// §2), and, only when it has no `=>` following, inside a protocol body too
/// (ADR 0127 §Status 8, §13). Single source for this keyword so
/// [`Parser::is_at_member_boundary`], [`Parser::current_token_could_start_a_declaration`]
/// and the `uses:`-line dispatch in [`Parser::parse_class_body`]/
/// [`Parser::parse_protocol_body`] can't drift apart. Unlike `handleScope:`/
/// `shapeVersion:`, `uses:` *is* added to the member-boundary set: it is a
/// lexer keyword token, never a plain identifier, so it can never
/// legitimately start a bare statement inside a method body — unlike
/// `type`/`declare`, `is_at_member_boundary` needs no indentation gate here.
/// A method or provided method literally named `uses:` (it has a `=>`) is
/// disambiguated by callers checking `is_at_method_definition()` first.
pub(super) fn is_uses_keyword(kind: &TokenKind) -> bool {
    matches!(kind, TokenKind::Keyword(k) if k == "uses:")
}

/// Returns `true` for a `state:`/`field:`/`classState:` keyword — the set of
/// declaration-start keywords that (unlike `handleScope:`) can appear
/// anywhere in a class body, not just its header. Single source for
/// this three-keyword set so [`Parser::is_at_member_boundary`] and
/// [`Parser::parse_method_body`]'s exit condition can't silently drift apart
/// from each other or from [`Parser::current_token_could_start_a_declaration`].
pub(super) fn is_state_like_declaration_keyword(kind: &TokenKind) -> bool {
    is_state_or_field_keyword(kind) || is_class_state_keyword(kind)
}

/// [`Parser::parse_class_body`]'s return shape: `(state, instance methods,
/// class methods, class variables, uses: lines)`.
type ClassBodyParts = (
    Vec<StateDeclaration>,
    Vec<MethodDefinition>,
    Vec<MethodDefinition>,
    Vec<StateDeclaration>,
    Vec<ProtocolUse>,
);

/// A consumed declaration-level `@expect category`,
/// bundled with the doc comment and plain leading comments that sat in its
/// own leading trivia (see [`Parser::parse_pending_declaration_expect`]).
/// `Default` (all `None`/empty) means no `@expect` was present.
#[derive(Default)]
pub(super) struct PendingDeclarationExpect {
    pub(super) expect: Option<(Vec<ExpectCategory>, Option<EcoString>, Span)>,
    doc_comment: Option<String>,
    comments: CommentAttachment,
}

impl PendingDeclarationExpect {
    /// Applies this pending `@expect`/doc comment/leading comments onto the
    /// declaration that follows it in source. `expect` always overwrites (a
    /// declaration can't already carry one — nothing sets it before this
    /// call); `doc_comment`/`comments` only fill in if the declaration's own
    /// token had none of its own (both would otherwise be left dangling on the
    /// `@expect` token's leading trivia — see
    /// `Parser::parse_pending_declaration_expect`).
    pub(super) fn apply_to(
        self,
        expect: &mut Option<(Vec<ExpectCategory>, Option<EcoString>, Span)>,
        doc_comment: &mut Option<String>,
        comments: &mut CommentAttachment,
    ) {
        let had_expect = self.expect.is_some();
        if had_expect {
            *expect = self.expect;
        }
        if doc_comment.is_none() {
            *doc_comment = self.doc_comment;
        }
        // Decide `leading_blank_line` explicitly, independently of
        // whether `leading`/`trailing` get replaced below, rather than
        // piggybacking on the `comments.is_empty()` check below (which only
        // ever looks at `leading`/`trailing`). When an `@expect` was
        // actually present, it — and anything directly above it — is the
        // true start of this declaration's leading trivia, so the
        // blank-line-before-`@expect` signal always wins over the
        // destination's own signal, which only reflects the (usually
        // nonexistent) gap between `@expect` and the declaration keyword.
        // Without this, a declaration with its own leading comment
        // sandwiched between `@expect` and itself would keep its own
        // (typically `false`) `leading_blank_line` and silently drop the
        // fact that a blank line separated the whole `@expect`-annotated
        // declaration from the previous one. No unparser reads
        // `leading_blank_line` on class-member-level `CommentAttachment`s
        // yet (state/class-var/method — only top-level class/protocol/
        // type-alias declarations), but the correct merge is captured now
        // so it doesn't need rediscovering once that changes.
        let expect_leading_blank_line = self.comments.leading_blank_line;
        if comments.is_empty() {
            *comments = self.comments;
        }
        if had_expect {
            comments.leading_blank_line = expect_leading_blank_line;
        }
    }
}

/// Which grammar position a selector is being parsed in.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum SelectorContext {
    /// A method definition in a class body.
    Method,
    /// A method signature in a `Protocol` body (extra guards, newline-separated).
    Protocol,
}

impl Parser {
    /// Reports a "use `::` not `:`" error for type annotations and advances past the `:`.
    pub(super) fn type_annotation_colon_error(&mut self) {
        let span = self.current_token().span();
        self.diagnostics.push(
            Diagnostic::error("Use `::` for type annotations, not `:`", span)
                .with_hint("Replace `:` with `::`"),
        );
        self.advance(); // consume legacy `:`
    }

    // ========================================================================
    // `late` modifier lookahead (ADR 0124 §1)
    // ========================================================================
    //
    // The modifier precedes the declaration keyword (`late state: proc :: Subprocess`),
    // matching the class-header modifier position (the `abstract|sealed|typed|internal`
    // loop at `:205-221`). That loop is header-only; there is no member-level
    // modifier loop, so this reuses its shape, not its code — a plain
    // two-token lookahead extending the single-token dispatch below. `late`
    // is unused as a word anywhere in the stdlib's `.bt` sources, so it is a
    // contextual keyword only in this position; elsewhere (`late := 1`, a
    // method named `late`) it parses as an ordinary identifier/selector.

    /// `true` if the current token is `late` immediately followed by
    /// `state:` or `field:`.
    fn is_at_late_state_or_field_keyword(&self) -> bool {
        matches!(self.current_kind(), TokenKind::Identifier(name) if name == "late")
            && self.peek_at(1).is_some_and(is_state_or_field_keyword)
    }

    /// `true` if the current token is `late` immediately followed by
    /// `classState:`.
    pub(super) fn is_at_late_class_state_keyword(&self) -> bool {
        matches!(self.current_kind(), TokenKind::Identifier(name) if name == "late")
            && self.peek_at(1).is_some_and(is_class_state_keyword)
    }

    /// `true` if the current token is the `late` modifier immediately
    /// followed by any state-like declaration keyword
    /// (`state:`/`field:`/`classState:`). Single source for the combined
    /// check so [`Parser::is_at_member_boundary`],
    /// [`Parser::current_token_could_start_a_declaration`] and
    /// [`Parser::parse_method_body`]'s exit condition can't drift from the
    /// dispatch in [`Parser::parse_class_body`] or from each other — mirrors
    /// [`is_state_like_declaration_keyword`]'s own single-source role for the
    /// unmodified keywords.
    pub(super) fn is_at_late_modifier(&self) -> bool {
        self.is_at_late_state_or_field_keyword() || self.is_at_late_class_state_keyword()
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
        let mut comments = self.collect_comment_attachment();
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

        // Anchor for a trailing comment on the class header line itself
        // (e.g. `Object subclass: Foo  // comment`) — the last token consumed
        // so far (class name, type params, or `native:` module). Captured
        // *before* parsing `handleScope:` because that clause conventionally
        // sits on its own indented line (ADR 0103); if we let
        // `collect_trailing_comment()` look at `current - 1` unconditionally
        // after parsing it, it would inspect the `#symbol` token's own
        // trailing trivia instead of the header line's, silently dropping a
        // header-line comment.
        let header_line_end = self.current.saturating_sub(1);

        // Parse optional `handleScope: #symbol` (ADR 0103) and `shapeVersion:
        // N` (ADR 0123 §1) header clauses — both appear at the head of the
        // class body, like `native:` is a header clause. A `while` loop
        // (mirroring the `abstract`/`sealed`/`typed`/`internal` modifier loop
        // above) accepts either clause in either order, since nothing in
        // either ADR fixes a relative order between them.
        let mut handle_scope = None;
        let mut shape_version: Option<ShapeVersionDeclaration> = None;
        let mut last_header_clause_on_new_line = false;
        loop {
            if handle_scope.is_none() && is_handle_scope_keyword(self.current_kind()) {
                last_header_clause_on_new_line = self.current_token().has_leading_newline();
                handle_scope = self.parse_optional_handle_scope();
            } else if is_shape_version_keyword(self.current_kind()) {
                let on_new_line = self.current_token().has_leading_newline();
                if let Some(sv) = self.parse_one_shape_version_clause(shape_version) {
                    if shape_version.is_none() {
                        last_header_clause_on_new_line = on_new_line;
                    }
                    shape_version = Some(sv);
                }
            } else {
                break;
            }
        }

        // Collect a trailing end-of-line comment on the class header line
        // (after the last header token — class name, type params, or
        // `native:` module — or, when a header clause follows on the same
        // line, its own last token), mirroring the identical handling for
        // type alias/protocol declarations. When the first header clause is on
        // its own line, prefer a comment on the header line itself, but fall
        // back to the post-clause check (the old, only behavior) so a
        // comment trailing that clause's own line is still captured instead
        // of silently dropped.
        //
        // `comments.trailing` is a single slot, so if *both* the header line
        // and a clause's line carry a trailing comment, only the header-line
        // one survives — the clause-line comment is discarded. This is a
        // deliberate choice (the header-line comment is the more prominent of
        // the two), not an oversight; see
        // `parse_handle_scope_on_new_line_prefers_header_over_scope_comment_when_both_present`.
        comments.trailing = if (handle_scope.is_some() || shape_version.is_some())
            && last_header_clause_on_new_line
        {
            self.collect_trailing_comment_at(header_line_end)
                .or_else(|| self.collect_trailing_comment())
        } else {
            self.collect_trailing_comment()
        };

        // Parse class body (uses: lines, state declarations, instance
        // methods, class methods, class variables)
        let (state, methods, class_methods, class_variables, uses) = self.parse_class_body();

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
        // ADR 0123 §1: same rationale as `handleScope:` above — include the
        // `shapeVersion:` clause so a version-only class body still spans its
        // declaration.
        if let Some(ref sv) = shape_version {
            end = end.merge(sv.span);
        }
        // ADR 0127 §2: same rationale — a class whose body is only `uses:`
        // lines (no state/methods yet) still spans its declaration.
        if let Some(u) = uses.last() {
            end = end.merge(u.span);
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
        class_def.shape_version = shape_version;
        // ADR 0127: composing a trait's provisions into a class's own body
        // (flattening) is `semantic_analysis::trait_expansion` (BT-3588),
        // not a parser concern — this node only carries the syntax. The
        // parser used to attach a blanket "not yet supported" placeholder
        // error to any `uses:` line here; that placeholder is gone now that
        // the pass exists.
        class_def.uses = uses;
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
        if !is_handle_scope_keyword(self.current_kind()) {
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

    /// Parses one `shapeVersion: N` clause (ADR 0123 §1), where the caller
    /// has already confirmed `is_shape_version_keyword(self.current_kind())`.
    /// `first` is the already-parsed first occurrence, if any — a second
    /// (or later) occurrence found still adjacent to the header (i.e. before
    /// the class body loop takes over) is a **duplicate** compile error
    /// distinct from `handleScope:`'s generic "must appear in header"
    /// misplaced-clause error, naming the value already recorded; the clause
    /// is still consumed so parsing recovers, but `None` is returned so the
    /// caller keeps the first value. `N` must be a positive integer literal
    /// — the compiler reads it without evaluating code, so anything else
    /// (a non-literal expression, zero, or a negative number) is a compile
    /// error mirroring `handleScope:`'s "expected a symbol" recovery.
    fn parse_one_shape_version_clause(
        &mut self,
        first: Option<ShapeVersionDeclaration>,
    ) -> Option<ShapeVersionDeclaration> {
        self.advance(); // consume `shapeVersion:`
        let TokenKind::Integer(text) = self.current_kind() else {
            self.error("Expected a positive integer literal after 'shapeVersion:'");
            if !self.current_token().has_leading_newline() && !self.is_at_end() {
                self.advance(); // consume the offending token so parsing recovers
            }
            return None;
        };
        let text = text.clone();
        let span = self.current_token().span();
        self.advance(); // consume the integer literal
        let value = match super::expressions::parse_integer(&text) {
            Ok(v) if v > 0 => v,
            Ok(_) => {
                self.diagnostics.push(Diagnostic::error(
                    "'shapeVersion:' expects a positive integer literal (got a non-positive value)",
                    span,
                ));
                return None;
            }
            Err(e) => {
                self.diagnostics.push(Diagnostic::error(e, span));
                return None;
            }
        };
        // SAFETY/CORRECTNESS: `value > 0` was just checked; Beamtalk integer
        // literals parsed here are small class-versioning numbers, so a
        // `u32`-range overflow is not a real-world concern, but guard it
        // anyway rather than truncate silently.
        let Ok(version) = u32::try_from(value) else {
            self.diagnostics.push(Diagnostic::error(
                "'shapeVersion:' value is too large",
                span,
            ));
            return None;
        };
        if let Some(existing) = first {
            self.diagnostics.push(Diagnostic::error(
                format!(
                    "duplicate shapeVersion: declaration (already {})",
                    existing.version
                ),
                span,
            ));
            return None;
        }
        Some(ShapeVersionDeclaration { version, span })
    }

    /// Parses optional type parameters: `(T, E)` or `(T :: Printable, E)`.
    ///
    /// Each type parameter may optionally have a protocol bound after `::`.
    /// Returns an empty `Vec` if no `(` follows the current position.
    ///
    /// **References:** ADR 0068 Phase 2d — type parameter bounds
    pub(super) fn parse_optional_type_params(&mut self) -> Vec<TypeParamDecl> {
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

    /// If the current token is a declaration-level `@expect
    /// category`, consume it (and its own doc comment / leading `//`
    /// comments) for the caller to attach to whichever state/method
    /// declaration follows. Returns `PendingDeclarationExpect::default()`
    /// (all `None`/empty) when the current token isn't `@expect`.
    ///
    /// Both a `/// ...` doc comment and any leading `//`/`/* */`
    /// comments written above a declaration-level `@expect` sit in the
    /// `@expect` token's own leading trivia, not the following
    /// declaration's — `collect_doc_comment()` and
    /// `collect_comment_attachment()` only ever look at the *current* token.
    /// Collect both here, while `@expect` is still current, so neither is
    /// left dangling — the doc comment would otherwise surface as a spurious
    /// "doc comment not attached" warning, and the plain comments would
    /// silently vanish (no such warning exists for them).
    pub(super) fn parse_pending_declaration_expect(&mut self) -> PendingDeclarationExpect {
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

    /// Parses the body of a class (`uses:` lines, state declarations, and
    /// methods).
    ///
    /// State declarations start with `state:`.
    /// Methods are identified by having a `=>` somewhere.
    #[allow(clippy::too_many_lines)] // one dispatch loop over several declaration kinds
    fn parse_class_body(&mut self) -> ClassBodyParts {
        let mut state = Vec::new();
        let mut methods = Vec::new();
        let mut class_methods = Vec::new();
        let mut class_variables = Vec::new();
        let mut uses = Vec::new();

        // Skip any periods/statement terminators
        while self.match_token(&TokenKind::Period) {}

        // Set in_class_body so parse_method_body can use indentation
        // to detect trailing expressions outside the class.
        let was_in_class_body = self.in_class_body;
        self.in_class_body = true;

        while !self.is_at_end()
            && !self.is_at_class_definition()
            && !self.is_at_protocol_definition()
            && !self.is_at_type_alias_definition()
            && !self.is_at_native_declaration()
            && !self.is_at_standalone_method_definition()
        {
            let pending = self.parse_pending_declaration_expect();

            // Check for state/field declaration: `state: fieldName ...` or
            // `field: fieldName ...`, optionally preceded by the `late`
            // modifier (ADR 0124 §1) — `parse_state_declaration` consumes
            // `late` itself so its own leading-comment/doc-comment collection
            // (which must run while `late`, not `state:`/`field:`, is still
            // current) sees it.
            if is_state_or_field_keyword(self.current_kind())
                || self.is_at_late_state_or_field_keyword()
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
            else if is_class_state_keyword(self.current_kind())
                || self.is_at_late_class_state_keyword()
            {
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
                        // flag regardless of how the modifier was tokenised.
                        method.is_class_method = true;
                        class_methods.push(method);
                    } else {
                        methods.push(method);
                    }
                }
            }
            // `uses:` composes a protocol's provided methods into this class
            // (ADR 0127 §2). Checked *after* `is_at_method_definition()` so a
            // keyword method literally named `uses:` (has a `=>` body) is
            // parsed as an ordinary method above, exactly as inside a
            // protocol body (see `parse_protocol_body`) — only a `uses:`
            // line with no `=>` is the reserved composition clause.
            else if is_uses_keyword(self.current_kind()) {
                let has_prior_member = !state.is_empty()
                    || !methods.is_empty()
                    || !class_methods.is_empty()
                    || !class_variables.is_empty();
                let use_line = self.parse_protocol_use_line();
                if has_prior_member {
                    self.diagnostics.push(Diagnostic::error(
                        "'uses:' lines must come before state and method declarations",
                        use_line.span,
                    ));
                } else {
                    uses.push(use_line);
                }
            } else if is_handle_scope_keyword(self.current_kind()) {
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
            } else if is_shape_version_keyword(self.current_kind()) {
                // ADR 0123 §1: `shapeVersion:` is a header clause parsed
                // *before* the body (see `parse_one_shape_version_clause`),
                // same class of error `handleScope:` gets above.
                self.error(
                    "'shapeVersion:' must appear in the class header, before state or method declarations",
                );
                self.advance(); // consume `shapeVersion:`
                if !self.current_token().has_leading_newline()
                    && matches!(self.current_kind(), TokenKind::Integer(_))
                {
                    self.advance(); // consume the integer argument to recover
                }
            } else if let TokenKind::Keyword(kw) = self.current_kind() {
                // An unrecognized keyword line (ADR 0127 §13) — e.g. a typo,
                // or `excluding:`/`overriding:`/`aliasing:` written without a
                // preceding `uses:`. Reported as a targeted error instead of
                // silently ending the class body, which previously let
                // everything after it (including real state/method
                // declarations) go unparsed with no diagnostic at all.
                let kw_text = kw.clone();
                let span = self.current_token().span();
                self.diagnostics.push(Diagnostic::error(
                    format!("unexpected '{kw_text}' in class body"),
                    span,
                ));
                self.advance(); // consume the offending keyword
                // Consume one same-line argument-like token too, mirroring
                // the handleScope:/shapeVersion: misplaced-clause recovery
                // above, so a trailing value isn't itself mistaken for the
                // next declaration.
                if !self.current_token().has_leading_newline() && !self.is_at_end() {
                    self.advance();
                }
            } else {
                // @expect before an invalid position (e.g., end of class body)
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

        // Restore in_class_body flag
        self.in_class_body = was_in_class_body;

        (state, methods, class_methods, class_variables, uses)
    }

    // ========================================================================
    // `uses:` line parsing (ADR 0127 §2)
    // ========================================================================

    /// Parses a `uses:` line: `uses: [package@]ProtocolName[(TypeArgs)]
    /// [excluding: #(#sel, …)] [overriding: #(#sel, …)] [aliasing: #{…}]`.
    ///
    /// Shared by [`Self::parse_class_body`] (where a valid `uses:` line is
    /// kept on `ClassDefinition.uses`) and [`Self::parse_protocol_body`]
    /// (where a `uses:` line is always a "not yet supported" error —
    /// protocol-using-protocol composition, ADR 0127 §Status 8 — and the
    /// parsed result is only used for its span). Callers have already
    /// confirmed the current token is the `uses:` keyword.
    ///
    /// `aliasing:` is reserved but rejected in v1 (ADR 0127 §Status 4): its
    /// keyword and argument are consumed here for parse recovery, but never
    /// stored — there is no `ProtocolUse` field for it yet.
    pub(super) fn parse_protocol_use_line(&mut self) -> ProtocolUse {
        let start = self.current_token().span();
        self.advance(); // consume `uses:`

        let first = self.parse_identifier("Expected protocol name after 'uses:'");
        let (protocol, package) = if matches!(self.current_kind(), TokenKind::At) {
            // Package-qualified protocol: `json @ Parser` (ADR 0070)
            self.advance(); // consume `@`
            let name = self.parse_identifier("Expected protocol name after '@'");
            (name, Some(first))
        } else {
            (first, None)
        };

        // Optional type arguments: `Enumerable(Worker)`. Reuses the same
        // `(TypeArgs)` parser as a class's `Collection(E) subclass: ...`
        // superclass type arguments — identical grammar.
        let type_args = self.parse_optional_superclass_type_args();

        let mut end = type_args.last().map_or(protocol.span, TypeAnnotation::span);
        let mut excluding = Vec::new();
        let mut overriding = Vec::new();

        // `excluding:`/`overriding:`/`aliasing:` may appear in any order —
        // mirrors the `handleScope:`/`shapeVersion:` header-clause loop's
        // own order tolerance, since ADR 0127 §2 does not fix an order
        // between them either.
        loop {
            match self.current_kind() {
                TokenKind::Keyword(k) if k == "excluding:" => {
                    self.advance(); // consume `excluding:`
                    let (selectors, span) = self.parse_selector_symbol_list();
                    excluding = selectors;
                    end = span;
                }
                TokenKind::Keyword(k) if k == "overriding:" => {
                    self.advance(); // consume `overriding:`
                    let (selectors, span) = self.parse_selector_symbol_list();
                    overriding = selectors;
                    end = span;
                }
                TokenKind::Keyword(k) if k == "aliasing:" => {
                    let kw_span = self.current_token().span();
                    self.diagnostics.push(Diagnostic::error(
                        "'aliasing:' is not yet supported",
                        kw_span,
                    ));
                    self.advance(); // consume `aliasing:`
                    end = if matches!(self.current_kind(), TokenKind::MapOpen) {
                        let map = self.with_bracketed_nesting(Self::parse_map_literal);
                        self.bound_expression_depth(map).span()
                    } else {
                        kw_span
                    };
                }
                _ => break,
            }
        }

        // A same-line trailing comma is the classic `uses: A, B` mistake —
        // ADR 0127 §2 explicitly anticipates it ("One trait per `uses:`
        // line; several traits are several lines"). Without this check the
        // comma is neither a recognized clause keyword nor consumed here,
        // so the enclosing class/protocol body would otherwise silently end
        // at it with no diagnostic at all — exactly the failure mode this
        // issue's `unexpected 'foo:' in class body` fix targets, just for a
        // non-keyword token shape.
        if is_comma(self.current_kind()) && !self.current_token().has_leading_newline() {
            let comma_span = self.current_token().span();
            self.diagnostics.push(
                Diagnostic::error("'uses:' takes one protocol per line", comma_span).with_hint(
                    "write each trait on its own 'uses:' line, e.g. 'uses: A' then 'uses: B'",
                ),
            );
            // Skip the rest of the line so the class/protocol body can
            // recover at the next line instead of ending silently.
            while !self.is_at_end() && !self.current_token().has_leading_newline() {
                end = self.current_token().span();
                self.advance();
            }
        }

        ProtocolUse {
            protocol,
            package,
            type_args,
            excluding,
            overriding,
            span: start.merge(end),
        }
    }

    /// Parses a `#(#sel, #sel2, …)` selector-symbol list — the argument of
    /// a `uses:` line's `excluding:`/`overriding:` clause (ADR 0127 §2).
    ///
    /// Reuses the general list-literal parser ([`Self::parse_list_literal`])
    /// rather than duplicating its comma/paren/trailing-comma/error-recovery
    /// handling, then requires each element to be a bare symbol literal —
    /// the "list of symbols" the ADR's grammar calls for, nothing else. A non-symbol
    /// element or a cons tail (`#(#a | rest)`) is reported and skipped;
    /// selector text validation (does the named protocol actually provide
    /// it?) is a semantic-analysis concern (BT-3588), not this parser's.
    ///
    /// Returns the parsed selectors (as `Identifier`s carrying the bare
    /// selector text and the source symbol's span) plus the whole list's
    /// span, for the caller's own span bookkeeping.
    fn parse_selector_symbol_list(&mut self) -> (Vec<Identifier>, Span) {
        let list = self.with_bracketed_nesting(Self::parse_list_literal);
        let expr = self.bound_expression_depth(list);
        let span = expr.span();
        let mut selectors = Vec::new();
        if let Expression::ListLiteral { elements, tail, .. } = expr {
            if let Some(tail) = tail {
                self.diagnostics.push(Diagnostic::error(
                    "a selector list cannot have a cons tail ('|')",
                    tail.span(),
                ));
            }
            for element in elements {
                match element {
                    Expression::Literal(Literal::Symbol(name), sym_span) => {
                        selectors.push(Identifier::new(name, sym_span));
                    }
                    other => {
                        self.diagnostics.push(Diagnostic::error(
                            "expected a symbol selector (e.g. #foo or #at:put:) in this list",
                            other.span(),
                        ));
                    }
                }
            }
        }
        (selectors, span)
    }

    /// Checks whether the current token is a declaration-level
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
    /// of a method) sits deeper than the enclosing method's own header
    /// indentation and is unaffected.
    ///
    /// The boundary column is the *current* method's own header indentation
    /// (`current_method_header_indent`), not a hardcoded `2`: a canonically
    /// nested method has its header at col 2, but `method_source_walker`'s
    /// synthetic-wrap strategy (used for xref/LSP send-collection queries)
    /// puts a bare method's header at col 0, so its body sits at col 2 —
    /// a hardcoded `col <= 2` would misidentify a body-level `@expect` there
    /// as declaration-level and silently truncate the body.
    ///
    /// Unlike `is_at_method_definition()`, this is gated on `in_class_body`
    /// alone (no `!self.in_class_body` fallback): declaration-level `@expect`
    /// is only ever captured by `parse_class_body`'s `pending_expect` loop,
    /// so outside a class body — e.g. a Tonel-style standalone `Class >>
    /// selector => body` method, which has no such capture — an `@expect`
    /// can only ever be the ordinary statement-level directive, never a
    /// boundary, regardless of indentation.
    pub(super) fn is_at_declaration_level_expect(&self) -> bool {
        (self.in_class_body || self.in_protocol_body)
            && self
                .current_token()
                .indentation_after_newline()
                .is_none_or(|col| col <= self.current_method_header_indent.unwrap_or(2))
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
        self.is_method_definition_at(0)
    }

    /// [`is_at_method_definition`](Self::is_at_method_definition) for the
    /// token at `offset`, so lookahead and recovery share one recogniser.
    pub(super) fn is_method_definition_at(&self, start: usize) -> bool {
        let mut offset = start;

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
            // GtGt (`>>`) is also a valid binary method selector.
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
            Some(TokenKind::Keyword(_)) => {
                self.is_keyword_method_at(offset) && !self.is_uses_line_before_next_method(offset)
            }
            _ => false,
        }
    }

    /// BT-3620: `uses: Proto` followed on the next line by a keyword method
    /// (`foo: x =>`) is lexically indistinguishable from the two-part keyword
    /// method header `uses: Proto foo: x =>`. A `uses:`-led header is treated
    /// as a `uses:` line (not a method) when the next keyword part starts on
    /// a new line — a multi-part method literally named `uses:…` keeps all
    /// its selector parts on one line.
    fn is_uses_line_before_next_method(&self, offset: usize) -> bool {
        if !self.peek_at(offset).is_some_and(is_uses_keyword) {
            return false;
        }
        let mut o = offset + 1;
        while let Some(kind) = self.peek_at(o) {
            match kind {
                TokenKind::FatArrow => return false,
                TokenKind::Keyword(_) => {
                    return self
                        .peek_token_at(o)
                        .is_some_and(crate::source_analysis::Token::has_leading_newline);
                }
                _ => o += 1,
            }
        }
        false
    }

    /// Checks if there's a keyword method definition starting at the given offset.
    ///
    /// Pattern: `keyword: param keyword: param ... =>` or with typed params/return type.
    /// Delegates to the shared helper in `mod.rs`.
    fn is_keyword_method_at(&self, start_offset: usize) -> bool {
        self.is_keyword_method_params_at(start_offset)
    }

    /// Parses an `@expect category` or `@expect category "reason"` that
    /// precedes a declaration, delegating to
    /// [`Parser::parse_expect_tail`] for the shared category-list/reason
    /// grammar.
    ///
    /// Unlike the statement-level form (`parse_expect_directive` in
    /// `expressions.rs`), an unrecognised category here has always fallen
    /// back to `ExpectCategory::All` rather than becoming a parse error —
    /// preserved as-is since a declaration always has *some* target to
    /// attach an `@expect` to (there's no "no valid expression" case to
    /// recover from, unlike the statement-level form's `Expression::Error`).
    fn parse_declaration_expect(&mut self) -> (Vec<ExpectCategory>, Option<EcoString>, Span) {
        let start_token = self.advance(); // consume AtExpect
        let (mut categories, reason, span, _) = self.parse_expect_tail(start_token.span());
        if categories.is_empty() {
            categories.push(ExpectCategory::All);
        }
        (categories, reason, span)
    }

    /// Returns `true` if the current token could start a class-body
    /// declaration — i.e. matches one of `parse_class_body`'s own
    /// recognized starts (`state:`/`field:`/`classState:`/`handleScope:`,
    /// or a method definition). Used by [`Parser::parse_expect_tail`] to
    /// avoid ever swallowing a real declaration while discarding garbage
    /// left over from a malformed `@expect`.
    fn current_token_could_start_a_declaration(&self) -> bool {
        is_state_like_declaration_keyword(self.current_kind())
            || self.is_at_late_modifier()
            || is_handle_scope_keyword(self.current_kind())
            || is_shape_version_keyword(self.current_kind())
            || is_uses_keyword(self.current_kind())
            || self.is_at_method_definition()
    }

    /// Parses the comma-separated category list and optional reason string
    /// that follow an already-consumed `@expect` token, shared by
    /// the statement-level (`parse_expect_directive`, `expressions.rs`) and
    /// declaration-level (`parse_declaration_expect`, above) forms.
    ///
    /// `start` is the span of the consumed `@expect` token. A single
    /// category (`@expect dnu`) is still supported — the list form
    /// (`@expect unresolved_ffi, type`) lets one directive suppress several
    /// categories on the same target instead of requiring an artificial
    /// second expression per category. Unknown category names are skipped
    /// with a diagnostic (already pushed here). The returned category list
    /// is empty when nothing valid was found — the last diagnostic message
    /// pushed is also returned so callers that need to fail (rather than
    /// fall back to `All`) can reuse it on an `Expression::Error` node
    /// without pushing a duplicate diagnostic.
    pub(super) fn parse_expect_tail(
        &mut self,
        start: Span,
    ) -> (
        Vec<ExpectCategory>,
        Option<EcoString>,
        Span,
        Option<EcoString>,
    ) {
        let mut categories = Vec::new();
        let mut span = start;
        let mut last_error: Option<EcoString> = None;

        loop {
            if let TokenKind::Identifier(name) = self.current_kind() {
                let name = name.clone();
                let end_token = self.advance();
                span = start.merge(end_token.span());
                if let Some(category) = ExpectCategory::from_name(&name) {
                    categories.push(category);
                } else {
                    let valid = ExpectCategory::valid_names().join(", ");
                    let message: EcoString =
                        format!("unknown @expect category '{name}', valid categories are: {valid}")
                            .into();
                    self.diagnostics
                        .push(Diagnostic::error(message.clone(), span));
                    last_error = Some(message);
                }
            } else {
                let valid = ExpectCategory::valid_names().join(", ");
                let message: EcoString =
                    format!("@expect must be followed by a category name ({valid})").into();
                self.diagnostics
                    .push(Diagnostic::error(message.clone(), span));
                last_error = Some(message);
                // Consume one same-line offending token (e.g. `@expect ,` or
                // `@expect 42`) so it isn't left dangling for the caller —
                // same class-body-truncation risk the dangling-comma fix
                // above addresses. Two things must hold, not just same-line:
                // a token on the *next* line is left alone since that's
                // plausibly the real next declaration/statement (e.g.
                // `@expect` with the category simply forgotten, followed by
                // a normal `state:` on its own line); AND a same-line token
                // that itself could start a class-body declaration (e.g.
                // `@expect state: x = 0`, category forgotten with no
                // separator at all) must ALSO be left alone — otherwise this
                // swallows the real declaration instead of discarding
                // garbage, which is worse than the dangling-token bug being
                // fixed here.
                if !self.is_at_end()
                    && !self.current_token().has_leading_newline()
                    && !self.current_token_could_start_a_declaration()
                {
                    let bad_token = self.advance();
                    span = start.merge(bad_token.span());
                }
                break;
            }

            if !is_comma(self.current_kind()) {
                // No comma at all — the category list ends here, cleanly.
                break;
            }

            // Same-line only, mirroring the reason-string lookahead below:
            // a comma (or the identifier after it) on the next line is not a
            // continuation of this directive's category list — e.g. a
            // trailing comma left over from an aborted edit must not have
            // the following line's leading identifier (which could
            // plausibly name a real category, like a variable called
            // `type`) silently absorbed into the list instead of starting
            // the next statement.
            let continues_same_line = !self.current_token().has_leading_newline()
                && self
                    .peek_token_at(1)
                    .is_some_and(|t| !t.has_leading_newline());
            let comma_token = self.advance();
            span = start.merge(comma_token.span());
            if !continues_same_line {
                // Dangling comma (e.g. a typo or an aborted edit): consume
                // it here, with a diagnostic, rather than leaving it as the
                // current token. At the declaration level in particular,
                // leaving it dangling makes `parse_class_body`'s caller
                // treat the position as "not a valid declaration" (see the
                // comment on the reason-string lookahead below,
                // which this mirrors) and silently drop every subsequent
                // state/method declaration in the class.
                let message: EcoString =
                    "trailing ',' in @expect category list must be followed by another category \
                     on the same line"
                        .into();
                self.diagnostics
                    .push(Diagnostic::error(message.clone(), span));
                last_error = Some(message);
                break;
            }
        }

        // Parse optional reason string after the category list (same line only).
        //
        // Deliberately unconditional — even when nothing valid was found
        // (`categories` is empty) — matching every `@expect`
        // parse-error path, all of which attempted this same lookahead
        // regardless of whether a category identifier was present at all.
        // Skipping it in the "no identifier" case might look more
        // conservative, but it isn't: at the declaration level, leaving a
        // stray trailing token unconsumed there makes `parse_class_body`'s
        // caller treat it as "not a valid declaration", which silently
        // truncates the rest of the class body instead of just discarding
        // one bad reason string — a strictly worse failure mode.
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

        (categories, reason, span, last_error)
    }

    /// Parses a state/field declaration.
    ///
    /// Syntax:
    /// - `state: fieldName` / `field: fieldName`
    /// - `state: fieldName = defaultValue` / `field: fieldName = defaultValue`
    /// - `state: fieldName :: TypeName` / `field: fieldName :: TypeName`
    /// - `state: fieldName :: TypeName = defaultValue` / `field: ...`
    pub(super) fn parse_state_declaration(&mut self) -> Option<StateDeclaration> {
        let start = self.current_token().span();
        let doc_comment = self.collect_doc_comment();
        let comments = self.collect_comment_attachment();

        // ADR 0124 §1: `late state: x :: T` / `late field: x :: T` — consumed
        // here (not by the dispatch loop) so the doc-comment/leading-comment
        // collection above, which reads the *current* token's leading trivia,
        // still sees `late`'s trivia rather than `state:`/`field:`'s (empty).
        let slot_kind = if self.is_at_late_state_or_field_keyword() {
            self.advance(); // consume `late`
            SlotKind::Late
        } else {
            SlotKind::Eager
        };

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

        Some(self.finish_slot_declaration(
            start,
            doc_comment,
            comments,
            slot_kind,
            declared_keyword,
            &format!("Expected field name after '{keyword_label}'"),
        ))
    }

    /// The tail shared by [`Self::parse_state_declaration`] and
    /// [`Self::parse_classvar_declaration`], entered just after the
    /// `state:` / `field:` / `classState:` keyword has been consumed: the slot
    /// name, optional `:: Type` annotation, optional `= default`, the trailing
    /// comment, and the declaration's span.
    ///
    /// `missing_name_message` is the diagnostic when no identifier follows the
    /// keyword.
    fn finish_slot_declaration(
        &mut self,
        start: Span,
        doc_comment: Option<String>,
        mut comments: CommentAttachment,
        slot_kind: SlotKind,
        declared_keyword: DeclaredKeyword,
        missing_name_message: &str,
    ) -> StateDeclaration {
        // Parse slot name and optional type annotation
        let (name, type_annotation) = if let TokenKind::Identifier(_) = self.current_kind() {
            let name_ident = self.parse_identifier(missing_name_message);

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
            self.error(missing_name_message.to_string());
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

        StateDeclaration {
            name,
            type_annotation,
            default_value,
            declared_keyword,
            slot_kind,
            expect: None,
            comments,
            doc_comment,
            span,
        }
    }

    /// Parses a class variable declaration.
    ///
    /// Syntax is identical to state declarations but uses `classState:` keyword:
    /// - `classState: varName`
    /// - `classState: varName = defaultValue`
    /// - `classState: varName :: TypeName`
    /// - `classState: varName :: TypeName = defaultValue`
    /// - `late classState: varName :: TypeName` (ADR 0124 §1)
    pub(super) fn parse_classvar_declaration(&mut self) -> Option<StateDeclaration> {
        let start = self.current_token().span();
        let doc_comment = self.collect_doc_comment();
        let comments = self.collect_comment_attachment();

        // ADR 0124 §1: consumed here, not by the dispatch loop — see the
        // matching comment in `parse_state_declaration`.
        let slot_kind = if self.is_at_late_class_state_keyword() {
            self.advance(); // consume `late`
            SlotKind::Late
        } else {
            SlotKind::Eager
        };

        // Consume `classState:`
        if !is_class_state_keyword(self.current_kind()) {
            return None;
        }
        self.advance();

        Some(self.finish_slot_declaration(
            start,
            doc_comment,
            comments,
            slot_kind,
            DeclaredKeyword::State,
            "Expected variable name after 'classState:'",
        ))
    }
}
