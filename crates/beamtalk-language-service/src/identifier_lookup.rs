// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Method-header, selector and identifier-at-position lookup on
//! [`SimpleLanguageService`] (split from `lib.rs`).

use super::{
    BinarySegment, ByteOffset, CursorBound, EcoString, Expression, Identifier, IdentifierContext,
    Lexer, MessageSelector, MethodDefinition, Module, Pattern, Position, SimpleLanguageService,
    Span, Token, TokenKind, TypeAnnotation, Utf8PathBuf, children_in_source_order,
};

impl SimpleLanguageService {
    /// If the offset falls inside the header of a method *definition*
    /// (the selector area of `bar => ...`, `+ other => ...`, or
    /// `at: i put: v => ...`), returns the method's selector name.
    ///
    /// Returns `None` if the offset is inside a method body, a parameter
    /// name, a parameter/return type annotation, or outside any method.
    ///
    /// This powers "Find All References" (and, indirectly, rename) when the
    /// user's cursor is on the selector of the method definition itself —
    /// not on a call site.
    ///
    /// `source` is the full file text `method`'s spans are offsets into;
    /// see [`Self::offset_in_method_header_selector`] for why it's needed.
    pub(super) fn find_method_header_selector_at_offset(
        module: &Module,
        source: &str,
        offset: u32,
    ) -> Option<EcoString> {
        for class in &module.classes {
            for method in class.methods.iter().chain(class.class_methods.iter()) {
                if Self::offset_in_method_header_selector(method, source, offset) {
                    return Some(method.selector.name());
                }
            }
        }
        for smd in &module.method_definitions {
            if Self::offset_in_method_header_selector(&smd.method, source, offset) {
                return Some(smd.method.selector.name());
            }
        }
        None
    }

    /// Checks whether `offset` lies on the selector of a method definition
    /// header.
    ///
    /// # Precision contract
    ///
    /// For keyword selectors we have precise per-keyword spans and only
    /// accept offsets that fall within one of those keyword parts — note
    /// this deliberately does *not* go through
    /// [`Self::method_header_selector_span`], whose merged first/last span
    /// would also cover the parameter names *between* keyword parts (e.g.
    /// the `i` in `at: i put: v`).
    ///
    /// For unary and binary selectors, `MessageSelector` carries no span for
    /// the selector token itself, so we recover it by scanning the header
    /// text with [`Self::method_header_selector_span`]. The result
    /// is exact: clicks on modifiers (`sealed` / `internal` / `class`),
    /// `->` punctuation, whitespace, parameter names/types, return types,
    /// and body expressions are all rejected — only the selector token
    /// itself matches.
    pub(super) fn offset_in_method_header_selector(
        method: &MethodDefinition,
        source: &str,
        offset: u32,
    ) -> bool {
        if offset < method.span.start() || offset >= method.span.end() {
            return false;
        }

        match &method.selector {
            MessageSelector::Keyword(parts) => parts
                .iter()
                .any(|part| offset >= part.span.start() && offset < part.span.end()),
            MessageSelector::Unary(_) | MessageSelector::Binary(_) => {
                Self::method_header_selector_span(method, source)
                    .is_some_and(|span| offset >= span.start() && offset < span.end())
            }
        }
    }

    /// Computes the exact source span of a method definition's selector
    /// token(s). Used for the `selection_range` on
    /// `CallHierarchyItem` ([`Self::method_header_selection_span`]) and, for
    /// unary/binary selectors, for the offset containment check in
    /// [`Self::offset_in_method_header_selector`].
    ///
    /// For keyword selectors this merges the precise per-part spans already
    /// carried on `MessageSelector::Keyword` — a *display* span suitable for
    /// highlighting the whole selector, not a containment check (see the
    /// doc comment on [`Self::offset_in_method_header_selector`] for why).
    ///
    /// For unary and binary selectors, which carry no span of their own,
    /// this re-tokenizes the header text — from `method.span.start()` up to
    /// the first parameter, return-type annotation, or body element — and
    /// skips leading modifier keyword tokens exactly as the parser's own
    /// modifier loop (`parse_method_definition`) does, returning the span of
    /// the first token that the parser would treat as the selector.
    ///
    /// The skip decision mirrors the parser token-for-token rather than
    /// counting `method`'s modifier flags, because the flags record only
    /// *which* modifiers are present, not *how many* tokens were consumed:
    /// the parser accepts repeated modifiers (`sealed sealed bar => 1`,
    /// `internal internal bar => 1`) — each sets the same boolean once but
    /// consumes a distinct token — so a flag-count-driven skip would stop
    /// short and mis-identify a second modifier keyword as the selector.
    /// Matching the parser's per-token rule instead:
    ///
    /// - `sealed` is *always* a modifier (the parser consumes it
    ///   unconditionally), so a method can never be named `sealed`.
    /// - `class` / `internal` are modifiers *unless* the next token is `=>`,
    ///   `->`, or `::` — the parser's `is_fat_arrow_or_return_type`
    ///   lookahead, which resolves `class => ...` (and `class -> T => ...`,
    ///   `class :: -> T => ...`) in favour of "this is the selector named
    ///   `class`". So `class => 1` returns the `class` token as the selector
    ///   and is never mistaken for the `class` modifier.
    ///
    /// Returns `None` only if the header text can't be tokenized down to a
    /// selector-shaped token, which should not happen for a
    /// `MethodDefinition` produced by parsing this exact `source`.
    pub(super) fn method_header_selector_span(
        method: &MethodDefinition,
        source: &str,
    ) -> Option<Span> {
        match &method.selector {
            MessageSelector::Keyword(parts) => {
                let first = parts.first()?.span;
                let last = parts.last()?.span;
                Some(first.merge(last))
            }
            MessageSelector::Unary(_) | MessageSelector::Binary(_) => {
                let start = method.span.start();

                // The header ends at the first of: first parameter name,
                // parameter type annotation, return type annotation, or
                // first body statement.
                let mut header_end = method.span.end();
                if let Some(first_param) = method.parameters.first() {
                    header_end = header_end.min(first_param.name.span.start());
                    if let Some(ta) = &first_param.type_annotation {
                        header_end = header_end.min(ta.span().start());
                    }
                }
                if let Some(rt) = &method.return_type {
                    header_end = header_end.min(rt.span().start());
                }
                if let Some(first_stmt) = method.body.first() {
                    header_end = header_end.min(first_stmt.expression.span().start());
                }

                let header_text = source.get(start as usize..header_end as usize)?;

                // Re-tokenize the header and walk it exactly as the parser's
                // modifier loop does (see doc comment above). `tokens` holds
                // the meaningful tokens (whitespace is trivia; the iterator
                // excludes EOF), so index `i + 1` peeks the next token.
                let tokens: Vec<_> = Lexer::new(header_text).collect();
                let mut i = 0;
                while let Some(token) = tokens.get(i) {
                    let is_modifier = match token.kind() {
                        // `sealed` is unconditionally a modifier.
                        TokenKind::Identifier(name) if name.as_str() == "sealed" => true,
                        // `class` / `internal` are modifiers unless the next
                        // token marks them as the selector (`=>`, `->`, `::`).
                        TokenKind::Identifier(name)
                            if matches!(name.as_str(), "class" | "internal") =>
                        {
                            !matches!(
                                tokens.get(i + 1).map(Token::kind),
                                Some(
                                    TokenKind::FatArrow | TokenKind::Arrow | TokenKind::DoubleColon
                                )
                            )
                        }
                        _ => false,
                    };
                    if is_modifier {
                        i += 1;
                        continue;
                    }
                    let token_span = token.span();
                    return Some(Span::new(
                        token_span.start() + start,
                        token_span.end() + start,
                    ));
                }
                None
            }
        }
    }

    /// Finds a method selector at a given byte offset in a module.
    ///
    /// Searches expressions, class method bodies, and standalone method definitions.
    pub(super) fn find_selector_at_offset(
        module: &Module,
        offset: u32,
    ) -> Option<crate::queries::definition_provider::SelectorLookup> {
        // Check top-level expressions
        for stmt in &module.expressions {
            if let Some(result) = crate::queries::definition_provider::find_selector_lookup_in_expr(
                &stmt.expression,
                offset,
            ) {
                return Some(result);
            }
        }
        // Check class method bodies
        for class in &module.classes {
            for method in class.methods.iter().chain(class.class_methods.iter()) {
                for stmt in &method.body {
                    if let Some(result) =
                        crate::queries::definition_provider::find_selector_lookup_in_expr(
                            &stmt.expression,
                            offset,
                        )
                    {
                        return Some(result);
                    }
                }
            }
        }
        // Check standalone method bodies
        for smd in &module.method_definitions {
            for stmt in &smd.method.body {
                if let Some(result) =
                    crate::queries::definition_provider::find_selector_lookup_in_expr(
                        &stmt.expression,
                        offset,
                    )
                {
                    return Some(result);
                }
            }
        }
        None
    }

    /// Finds the identifier at a given position.
    pub(super) fn find_identifier_at_position(
        &self,
        file: &Utf8PathBuf,
        position: Position,
    ) -> Option<(Identifier, Span, IdentifierContext)> {
        let file_data = self.get_file(file)?;
        let offset = position.to_byte_offset(&file_data.source)?;
        let offset_val = offset.get();

        for class in &file_data.module.classes {
            if let Some(ident) = Self::find_identifier_in_class(class, offset, offset_val) {
                return Some(ident);
            }
        }

        // Check protocol definitions (name, extending, type-param bounds, method sigs)
        for protocol in &file_data.module.protocols {
            if let Some(ident) = Self::find_identifier_in_protocol(protocol, offset_val) {
                return Some(ident);
            }
        }

        // ADR 0108 Phase 8: Check type alias declarations (the
        // alias name itself, and any name referenced in its RHS
        // annotation) so goto-definition/find-references work when the
        // cursor is on the declaration site, e.g. `type RestartStrategy =
        // ...` or a reference to another alias inside the RHS.
        for alias in &file_data.module.type_aliases {
            if offset_val >= alias.name.span.start() && offset_val < alias.name.span.end() {
                return Some((
                    alias.name.clone(),
                    alias.name.span,
                    IdentifierContext::TypeReference,
                ));
            }
            if let Some(ident) =
                Self::find_identifier_in_type_annotation(&alias.annotation, offset_val)
            {
                return Some(ident);
            }
        }

        for smd in &file_data.module.method_definitions {
            if let Some(ident) = Self::find_identifier_in_standalone_method(smd, offset, offset_val)
            {
                return Some(ident);
            }
        }

        // Walk the top-level expressions
        for stmt in &file_data.module.expressions {
            if let Some(ident) = Self::find_identifier_in_expr(&stmt.expression, offset) {
                return Some(ident);
            }
        }

        None
    }

    /// Walk a class definition looking for an identifier at the given offset.
    ///
    /// Covers: class name, superclass, type-parameter bounds, state /
    /// class-variable type annotations, and method bodies (parameters, return
    /// type, body statements).
    fn find_identifier_in_class(
        class: &beamtalk_core::ast::ClassDefinition,
        offset: ByteOffset,
        offset_val: u32,
    ) -> Option<(Identifier, Span, IdentifierContext)> {
        if offset_val >= class.name.span.start() && offset_val < class.name.span.end() {
            return Some((
                class.name.clone(),
                class.name.span,
                IdentifierContext::TypeReference,
            ));
        }
        if let Some(ref superclass) = class.superclass
            && offset_val >= superclass.span.start()
            && offset_val < superclass.span.end()
        {
            return Some((
                superclass.clone(),
                superclass.span,
                IdentifierContext::TypeReference,
            ));
        }
        if let Some(ident) = Self::find_identifier_in_type_params(&class.type_params, offset_val) {
            return Some(ident);
        }
        for state in class.state.iter().chain(class.class_variables.iter()) {
            if let Some(type_annotation) = &state.type_annotation
                && let Some(ident) =
                    Self::find_identifier_in_type_annotation(type_annotation, offset_val)
            {
                return Some(ident);
            }
        }
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            if let Some(ident) = Self::find_identifier_in_method_signature_and_body(
                &method.parameters,
                method.return_type.as_ref(),
                &method.body,
                offset,
                offset_val,
            ) {
                return Some(ident);
            }
        }
        None
    }

    /// Walk a protocol definition looking for an identifier at the given offset.
    ///
    /// Covers: protocol name, `extending:` target, type-parameter bounds, and
    /// method signature type annotations (both instance and class-side).
    fn find_identifier_in_protocol(
        protocol: &beamtalk_core::ast::ProtocolDefinition,
        offset_val: u32,
    ) -> Option<(Identifier, Span, IdentifierContext)> {
        if offset_val >= protocol.name.span.start() && offset_val < protocol.name.span.end() {
            return Some((
                protocol.name.clone(),
                protocol.name.span,
                IdentifierContext::TypeReference,
            ));
        }
        if let Some(ref extending) = protocol.extending
            && offset_val >= extending.span.start()
            && offset_val < extending.span.end()
        {
            return Some((
                extending.clone(),
                extending.span,
                IdentifierContext::TypeReference,
            ));
        }
        if let Some(ident) = Self::find_identifier_in_type_params(&protocol.type_params, offset_val)
        {
            return Some(ident);
        }
        for sig in protocol
            .method_signatures
            .iter()
            .chain(protocol.class_method_signatures.iter())
        {
            for parameter in &sig.parameters {
                if let Some(type_annotation) = &parameter.type_annotation
                    && let Some(ident) =
                        Self::find_identifier_in_type_annotation(type_annotation, offset_val)
                {
                    return Some(ident);
                }
            }
            if let Some(return_type) = &sig.return_type
                && let Some(ident) =
                    Self::find_identifier_in_type_annotation(return_type, offset_val)
            {
                return Some(ident);
            }
        }
        None
    }

    /// Walk a Tonel-style standalone method definition for an identifier at `offset`.
    fn find_identifier_in_standalone_method(
        smd: &beamtalk_core::ast::StandaloneMethodDefinition,
        offset: ByteOffset,
        offset_val: u32,
    ) -> Option<(Identifier, Span, IdentifierContext)> {
        if offset_val >= smd.class_name.span.start() && offset_val < smd.class_name.span.end() {
            return Some((
                smd.class_name.clone(),
                smd.class_name.span,
                IdentifierContext::TypeReference,
            ));
        }
        Self::find_identifier_in_method_signature_and_body(
            &smd.method.parameters,
            smd.method.return_type.as_ref(),
            &smd.method.body,
            offset,
            offset_val,
        )
    }

    /// Walk a method's parameter types, return type, and body for an identifier
    /// at the given offset. Shared by class methods and standalone methods.
    fn find_identifier_in_method_signature_and_body(
        parameters: &[beamtalk_core::ast::ParameterDefinition],
        return_type: Option<&TypeAnnotation>,
        body: &[beamtalk_core::ast::ExpressionStatement],
        offset: ByteOffset,
        offset_val: u32,
    ) -> Option<(Identifier, Span, IdentifierContext)> {
        for parameter in parameters {
            if let Some(type_annotation) = &parameter.type_annotation
                && let Some(ident) =
                    Self::find_identifier_in_type_annotation(type_annotation, offset_val)
            {
                return Some(ident);
            }
        }
        if let Some(return_type) = return_type
            && let Some(ident) = Self::find_identifier_in_type_annotation(return_type, offset_val)
        {
            return Some(ident);
        }
        for stmt in body {
            if let Some(ident) = Self::find_identifier_in_expr(&stmt.expression, offset) {
                return Some(ident);
            }
        }
        None
    }

    /// Check a list of `TypeParamDecl` for an identifier at the offset.
    ///
    /// Returns the protocol bound identifier when the cursor is on the bound,
    /// e.g., `Printable` in `Logger(T :: Printable)` or `Mapper(T :: Printable)`.
    /// The parameter name itself (e.g., `T`) is intentionally not returned —
    /// type-parameter names are local to their declaration and have no global
    /// definition to navigate to.
    fn find_identifier_in_type_params(
        type_params: &[beamtalk_core::ast::TypeParamDecl],
        offset_val: u32,
    ) -> Option<(Identifier, Span, IdentifierContext)> {
        for param in type_params {
            if let Some(bound) = &param.bound
                && offset_val >= bound.span.start()
                && offset_val < bound.span.end()
            {
                return Some((bound.clone(), bound.span, IdentifierContext::TypeReference));
            }
        }
        None
    }

    fn find_identifier_in_type_annotation(
        annotation: &TypeAnnotation,
        offset_val: u32,
    ) -> Option<(Identifier, Span, IdentifierContext)> {
        match annotation {
            TypeAnnotation::Simple(identifier) => {
                if offset_val >= identifier.span.start() && offset_val < identifier.span.end() {
                    Some((
                        identifier.clone(),
                        identifier.span,
                        IdentifierContext::TypeReference,
                    ))
                } else {
                    None
                }
            }
            TypeAnnotation::Union { types, .. } => types
                .iter()
                .find_map(|ty| Self::find_identifier_in_type_annotation(ty, offset_val)),
            TypeAnnotation::Generic {
                base, parameters, ..
            } => {
                if offset_val >= base.span.start() && offset_val < base.span.end() {
                    return Some((base.clone(), base.span, IdentifierContext::TypeReference));
                }
                parameters
                    .iter()
                    .find_map(|ty| Self::find_identifier_in_type_annotation(ty, offset_val))
            }
            TypeAnnotation::FalseOr { inner, .. } => {
                Self::find_identifier_in_type_annotation(inner, offset_val)
            }
            TypeAnnotation::Difference { base, excluded, .. } => {
                Self::find_identifier_in_type_annotation(base, offset_val)
                    .or_else(|| Self::find_identifier_in_type_annotation(excluded, offset_val))
            }
            TypeAnnotation::Intersection { left, right, .. } => {
                Self::find_identifier_in_type_annotation(left, offset_val)
                    .or_else(|| Self::find_identifier_in_type_annotation(right, offset_val))
            }
            TypeAnnotation::ClassOf { class_name, .. } => {
                if offset_val >= class_name.span.start() && offset_val < class_name.span.end() {
                    Some((
                        class_name.clone(),
                        class_name.span,
                        IdentifierContext::TypeReference,
                    ))
                } else {
                    None
                }
            }
            TypeAnnotation::Shape { fields, .. } => fields
                .iter()
                .find_map(|f| Self::find_identifier_in_type_annotation(&f.value, offset_val)),
            TypeAnnotation::Singleton { .. }
            | TypeAnnotation::SelfType { .. }
            | TypeAnnotation::SelfClass { .. } => None,
        }
    }

    /// Recursively searches for an identifier at the given offset.
    fn find_identifier_in_expr(
        expr: &Expression,
        offset: ByteOffset,
    ) -> Option<(Identifier, Span, IdentifierContext)> {
        let offset_val = offset.get();
        if !CursorBound::Within.contains(expr.span(), offset_val) {
            return None;
        }

        match expr {
            Expression::Identifier(ident) => {
                if offset_val >= ident.span.start() && offset_val < ident.span.end() {
                    Some((ident.clone(), ident.span, IdentifierContext::Expression))
                } else {
                    None
                }
            }
            Expression::ClassReference { name, .. } => {
                if offset_val >= name.span.start() && offset_val < name.span.end() {
                    Some((name.clone(), name.span, IdentifierContext::Expression))
                } else {
                    None
                }
            }
            Expression::FieldAccess { field, .. }
                if offset_val >= field.span.start() && offset_val < field.span.end() =>
            {
                Some((field.clone(), field.span, IdentifierContext::Expression))
            }
            // A match arm also holds a pattern, which is not an expression and
            // so is not among `children_in_source_order`: walk pattern, guard,
            // and body so that goto-definition on a class name inside a
            // constructor pattern (`Result ok: v`) or inside a `when:` guard
            // navigates to the class declaration. Keeps parity with
            // `references_provider::collect_class_refs` which already walks all
            // three.
            Expression::Match { value, arms, .. } => Self::find_identifier_in_expr(value, offset)
                .or_else(|| {
                    arms.iter().find_map(|arm| {
                        Self::find_identifier_in_pattern(&arm.pattern, offset_val)
                            .or_else(|| {
                                arm.guard
                                    .as_ref()
                                    .and_then(|g| Self::find_identifier_in_expr(g, offset))
                            })
                            .or_else(|| Self::find_identifier_in_expr(&arm.body, offset))
                    })
                }),
            // Everything else is a container: descend into whichever child
            // holds the offset.
            _ => children_in_source_order(expr)
                .into_iter()
                .find_map(|child| Self::find_identifier_in_expr(child, offset)),
        }
    }

    /// Recursively searches for a class name identifier inside a destructuring
    /// pattern.
    ///
    /// Only `Pattern::Constructor` carries a class identifier (`Result` in
    /// `Result ok: v`). All other variants just host nested patterns, which we
    /// walk so a cursor on a constructor name nested inside a tuple, list, map,
    /// or rest pattern still finds the class. Wildcards, literals, and plain
    /// variable bindings have no class identifier to navigate to.
    fn find_identifier_in_pattern(
        pattern: &Pattern,
        offset_val: u32,
    ) -> Option<(Identifier, Span, IdentifierContext)> {
        let span = pattern.span();
        if offset_val < span.start() || offset_val >= span.end() {
            return None;
        }
        match pattern {
            Pattern::Constructor {
                class, keywords, ..
            } => {
                if offset_val >= class.span.start() && offset_val < class.span.end() {
                    return Some((class.clone(), class.span, IdentifierContext::TypeReference));
                }
                keywords
                    .iter()
                    .find_map(|(_, p)| Self::find_identifier_in_pattern(p, offset_val))
            }
            Pattern::Tuple { elements, .. } => elements
                .iter()
                .find_map(|p| Self::find_identifier_in_pattern(p, offset_val)),
            Pattern::Array { elements, rest, .. } => elements
                .iter()
                .find_map(|p| Self::find_identifier_in_pattern(p, offset_val))
                .or_else(|| {
                    rest.as_deref()
                        .and_then(|r| Self::find_identifier_in_pattern(r, offset_val))
                }),
            Pattern::List { elements, tail, .. } => elements
                .iter()
                .find_map(|p| Self::find_identifier_in_pattern(p, offset_val))
                .or_else(|| {
                    tail.as_deref()
                        .and_then(|t| Self::find_identifier_in_pattern(t, offset_val))
                }),
            Pattern::Map { pairs, .. } => pairs
                .iter()
                .find_map(|pair| Self::find_identifier_in_pattern(&pair.value, offset_val)),
            Pattern::Binary { segments, .. } => segments
                .iter()
                .find_map(|seg| Self::find_identifier_in_pattern(&seg.value, offset_val)),
            Pattern::Type { class, .. } => {
                if offset_val >= class.span.start() && offset_val < class.span.end() {
                    Some((class.clone(), class.span, IdentifierContext::TypeReference))
                } else {
                    // The binding (e.g. `path` in `path :: String`) is a
                    // local variable, not a class reference — this function
                    // only navigates class identifiers. Go-to-definition on
                    // the binding itself is deferred until it becomes a
                    // fully navigable scope entry.
                    None
                }
            }
            Pattern::Wildcard(_)
            | Pattern::Literal(_, _)
            | Pattern::Variable(_)
            | Pattern::Nil(_) => None,
        }
    }

    /// Collects all identifiers with their spans from an expression.
    pub(super) fn collect_identifiers(expr: &Expression, name: &str, results: &mut Vec<Span>) {
        match expr {
            Expression::Identifier(ident) if ident.name == name => {
                results.push(ident.span);
            }
            Expression::ClassReference {
                name: class_name, ..
            } if class_name.name == name => {
                results.push(class_name.span);
            }
            Expression::Assignment { target, value, .. } => {
                Self::collect_identifiers(target, name, results);
                Self::collect_identifiers(value, name, results);
            }
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } => {
                Self::collect_identifiers(receiver, name, results);
                for arg in arguments {
                    Self::collect_identifiers(arg, name, results);
                }
            }
            Expression::Block(block) => {
                for stmt in &block.body {
                    Self::collect_identifiers(&stmt.expression, name, results);
                }
            }
            Expression::Return { value, .. } => {
                Self::collect_identifiers(value, name, results);
            }
            Expression::ArrayLiteral { elements, .. }
            | Expression::TupleLiteral { elements, .. } => {
                for e in elements {
                    Self::collect_identifiers(e, name, results);
                }
            }
            Expression::Parenthesized { expression, .. } => {
                Self::collect_identifiers(expression, name, results);
            }
            Expression::FieldAccess {
                receiver, field, ..
            } => {
                if field.name == name {
                    results.push(field.span);
                }
                Self::collect_identifiers(receiver, name, results);
            }
            Expression::Cascade {
                receiver, messages, ..
            } => {
                Self::collect_identifiers(receiver, name, results);
                for msg in messages {
                    for arg in &msg.arguments {
                        Self::collect_identifiers(arg, name, results);
                    }
                }
            }
            Expression::Match { value, arms, .. } => {
                Self::collect_identifiers(value, name, results);
                for arm in arms {
                    Self::collect_identifiers(&arm.body, name, results);
                }
            }
            Expression::StringInterpolation { segments, .. } => {
                for segment in segments {
                    if let beamtalk_core::ast::StringSegment::Interpolation(expr) = segment {
                        Self::collect_identifiers(expr, name, results);
                    }
                }
            }
            Expression::BinaryConstruction { segments, .. } => {
                for expr in segments.iter().flat_map(BinarySegment::sub_expressions) {
                    Self::collect_identifiers(expr, name, results);
                }
            }
            _ => {}
        }
    }
}
