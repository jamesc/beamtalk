// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Declaration parsing for Beamtalk.
//!
//! This module handles parsing of top-level declarations including:
//! - Class definitions with modifiers (`abstract`, `sealed`, `typed`)
//! - State (field) declarations with types and default values
//! - Method definitions with optional `sealed` modifier

use crate::ast::{
    ClassDefinition, Expression, ExpressionStatement, Identifier, KeywordPart, MessageSelector,
    MethodDefinition, MethodKind, ParameterDefinition, StandaloneMethodDefinition,
    StateDeclaration, TypeAnnotation,
};
use crate::source_analysis::{Span, TokenKind};

use super::{Diagnostic, Parser};

impl Parser {
    // ========================================================================
    // Class Definition Parsing
    // ========================================================================

    /// Parses a class definition.
    ///
    /// Syntax:
    /// ```text
    /// abstract? sealed? typed? <Superclass> subclass: <ClassName>
    ///   state: fieldName = defaultValue
    ///   state: fieldName: TypeName = defaultValue
    ///
    ///   methodName => body
    ///   sealed methodName => body
    /// ```
    ///
    /// The superclass name determines the class kind:
    /// - `Actor subclass:` → `ClassKind::Actor`
    /// - `Value subclass:` → `ClassKind::Value` (ADR 0042)
    /// - Anything else → `ClassKind::Object`
    pub(super) fn parse_class_definition(&mut self) -> ClassDefinition {
        let start = self.current_token().span();
        let doc_comment = self.collect_doc_comment();
        let comments = self.collect_comment_attachment();
        let mut is_abstract = false;
        let mut is_sealed = false;
        let mut is_typed = false;

        // Parse optional modifiers: abstract, sealed, typed
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
            } else {
                break;
            }
        }

        // Parse superclass name — `nil` means root class (no superclass)
        let superclass_id = self.parse_identifier("Expected superclass name");
        let superclass = if superclass_id.name == "nil" {
            None
        } else {
            Some(superclass_id)
        };

        // Expect `subclass:` keyword
        if !matches!(self.current_kind(), TokenKind::Keyword(k) if k == "subclass:") {
            self.error("Expected 'subclass:' keyword");
            return ClassDefinition::with_modifiers(
                Identifier::new("Error", start),
                superclass,
                is_abstract,
                is_sealed,
                Vec::new(),
                Vec::new(),
                start,
            );
        }
        self.advance(); // consume `subclass:`

        // Parse class name
        let name = self.parse_identifier("Expected class name");

        // Parse class body (state declarations, instance methods, class methods, class variables)
        let (state, methods, class_methods, class_variables) = self.parse_class_body();

        // Determine end span: max of last instance method, class method, state, class var, or name
        let mut end = name.span;
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
            is_abstract,
            is_sealed,
            state,
            methods,
            span,
        );
        class_def.is_typed = is_typed;
        class_def.class_methods = class_methods;
        class_def.class_variables = class_variables;
        class_def.doc_comment = doc_comment;
        class_def.comments = comments;
        class_def
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
            && !self.is_at_standalone_method_definition()
        {
            // Check for state declaration: `state: fieldName ...`
            if matches!(self.current_kind(), TokenKind::Keyword(k) if k == "state:") {
                if let Some(state_decl) = self.parse_state_declaration() {
                    state.push(state_decl);
                }
            }
            // Check for class variable declaration: `classState: varName ...`
            else if matches!(self.current_kind(), TokenKind::Keyword(k) if k == "classState:") {
                if let Some(classvar_decl) = self.parse_classvar_declaration() {
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
                                if matches!(
                                    name.as_str(),
                                    "before" | "after" | "around" | "sealed"
                                ) =>
                            {
                                offset += 1;
                            }
                            Some(TokenKind::Identifier(name)) if name == "class" => {
                                // Check it's not `class => ...` (method named "class")
                                if !matches!(self.peek_at(offset + 1), Some(TokenKind::FatArrow)) {
                                    found = true;
                                }
                                break;
                            }
                            _ => break,
                        }
                    }
                    found
                };
                if let Some(method) = self.parse_method_definition() {
                    if is_class_method {
                        class_methods.push(method);
                    } else {
                        methods.push(method);
                    }
                }
            } else {
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
    /// - `sealed` followed by one of the above
    pub(super) fn is_at_method_definition(&self) -> bool {
        let mut offset = 0;

        // Skip optional modifiers: sealed, class
        // Note: `class` is only a modifier when followed by a method selector,
        // not when followed by `=>` (which makes it a method named `class`).
        while let Some(TokenKind::Identifier(name)) = self.peek_at(offset) {
            if matches!(name.as_str(), "sealed") {
                offset += 1;
            } else if name == "class" {
                // Only treat as modifier if next token is not `=>`
                if matches!(self.peek_at(offset + 1), Some(TokenKind::FatArrow)) {
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
            // Binary method: `+ other =>` or `+ other -> Type =>` or `+ other: Type =>`
            Some(TokenKind::BinarySelector(_)) => {
                // Untyped: `+ other =>` or `+ other -> Type =>`
                (matches!(self.peek_at(offset + 1), Some(TokenKind::Identifier(_)))
                    && (matches!(self.peek_at(offset + 2), Some(TokenKind::FatArrow))
                        || self.is_return_type_then_fat_arrow(offset + 2)
                        // Typed via Colon: `+ other : Type =>` (space before colon)
                        || (matches!(self.peek_at(offset + 2), Some(TokenKind::Colon))
                            && matches!(self.peek_at(offset + 3), Some(TokenKind::Identifier(_)))
                            && (matches!(self.peek_at(offset + 4), Some(TokenKind::FatArrow))
                                || self.is_return_type_then_fat_arrow(offset + 4)))))
                    // Typed via Keyword: `+ other: Type =>` (lexer makes `other:` a Keyword)
                    || (matches!(self.peek_at(offset + 1), Some(TokenKind::Keyword(_)))
                        && matches!(self.peek_at(offset + 2), Some(TokenKind::Identifier(_)))
                        && (matches!(self.peek_at(offset + 3), Some(TokenKind::FatArrow))
                            || self.is_return_type_then_fat_arrow(offset + 3)))
            }
            // Keyword method: `at: index =>` or `at: index put: value =>`
            Some(TokenKind::Keyword(_)) => self.is_keyword_method_at(offset),
            _ => false,
        }
    }

    /// Checks if there's a `-> Type =>` or `-> Type | Type =>` pattern at the given offset.
    ///
    /// Also accepts `-> =>` (missing type) for error recovery — lets `parse_type_annotation`
    /// emit the specific error rather than failing to detect the method definition.
    fn is_return_type_then_fat_arrow(&self, offset: usize) -> bool {
        if !matches!(
            self.peek_at(offset),
            Some(TokenKind::BinarySelector(s)) if s.as_str() == "->"
        ) {
            return false;
        }
        // Skip -> Type (and possible | Type unions)
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
        // Skip union types: `| Type`
        while matches!(self.peek_at(o), Some(TokenKind::Pipe)) {
            o += 1;
            if !matches!(self.peek_at(o), Some(TokenKind::Identifier(_))) {
                return false;
            }
            o += 1;
        }
        matches!(self.peek_at(o), Some(TokenKind::FatArrow))
    }

    /// Checks if there's a keyword method definition starting at the given offset.
    ///
    /// Pattern: `keyword: param keyword: param ... =>` or with typed params/return type
    fn is_keyword_method_at(&self, start_offset: usize) -> bool {
        let mut offset = start_offset;

        // Must have at least one keyword-parameter pair
        loop {
            // Expect keyword
            if !matches!(self.peek_at(offset), Some(TokenKind::Keyword(_))) {
                return false;
            }
            offset += 1;

            // Check for typed parameter: `keyword: paramName: Type`
            // where paramName: is a Keyword token followed by Identifier (type)
            if let Some(TokenKind::Keyword(_)) = self.peek_at(offset) {
                // Could be typed param: paramName: followed by Type (Identifier)
                if matches!(self.peek_at(offset + 1), Some(TokenKind::Identifier(_))) {
                    // Skip paramName: and Type
                    offset += 2;
                    // Check for => or more keywords or return type
                    match self.peek_at(offset) {
                        Some(TokenKind::FatArrow) => return true,
                        Some(TokenKind::Keyword(_)) => continue,
                        Some(TokenKind::BinarySelector(s)) if s.as_str() == "->" => {
                            return self.is_return_type_then_fat_arrow(offset);
                        }
                        _ => return false,
                    }
                }
                // Not a typed param — could be next keyword selector part
                // but that would need an identifier first, so this is invalid
                return false;
            }

            // Expect parameter (identifier) for untyped case
            if !matches!(self.peek_at(offset), Some(TokenKind::Identifier(_))) {
                return false;
            }
            offset += 1;

            // After the parameter name, we may see:
            // - a fat arrow (end of selector)
            // - another keyword (more selector parts)
            // - a return type introducer "->"
            // - or a colon-type pair `: Type` (when param is written as `name : Type`)
            match self.peek_at(offset) {
                Some(TokenKind::FatArrow) => return true,
                Some(TokenKind::Keyword(_)) => {} // More keywords, continue loop
                Some(TokenKind::BinarySelector(s)) if s.as_str() == "->" => {
                    return self.is_return_type_then_fat_arrow(offset);
                }
                Some(TokenKind::Colon) => {
                    // Typed parameter: `paramName : Type`
                    if !matches!(self.peek_at(offset + 1), Some(TokenKind::Identifier(_))) {
                        return false;
                    }
                    offset += 2; // skip `:` and type
                    match self.peek_at(offset) {
                        Some(TokenKind::FatArrow) => return true,
                        Some(TokenKind::Keyword(_)) => {} // More keywords, continue loop
                        Some(TokenKind::BinarySelector(s)) if s.as_str() == "->" => {
                            return self.is_return_type_then_fat_arrow(offset);
                        }
                        _ => return false,
                    }
                }
                _ => return false,
            }
        }
    }

    /// Parses a state declaration.
    ///
    /// Syntax:
    /// - `state: fieldName`
    /// - `state: fieldName = defaultValue`
    /// - `state: fieldName: TypeName`
    /// - `state: fieldName: TypeName = defaultValue`
    fn parse_state_declaration(&mut self) -> Option<StateDeclaration> {
        let start = self.current_token().span();
        let doc_comment = self.collect_doc_comment();
        let mut comments = self.collect_comment_attachment();

        // Consume `state:`
        if !matches!(self.current_kind(), TokenKind::Keyword(k) if k == "state:") {
            return None;
        }
        self.advance();

        // Parse field name and optional type annotation
        // Two cases:
        // 1. `state: fieldName = value` - Identifier followed by = or newline
        // 2. `state: fieldName: Type = value` - lexed as Keyword("fieldName:") + Identifier("Type")
        let (name, type_annotation) = match self.current_kind() {
            // Case 2: field name with type annotation, lexed as keyword
            TokenKind::Keyword(keyword) => {
                // Strip the trailing colon to get the field name
                let field_name = keyword.trim_end_matches(':');
                let span = self.current_token().span();
                let name_ident = Identifier::new(field_name, span);
                self.advance();

                // Parse the type name
                let type_ann = self.parse_type_annotation();
                (name_ident, Some(type_ann))
            }
            // Case 1: simple field name
            TokenKind::Identifier(_) => {
                let name_ident = self.parse_identifier("Expected field name after 'state:'");

                // Check for optional type annotation (: TypeName)
                let type_ann = if self.match_token(&TokenKind::Colon) {
                    Some(self.parse_type_annotation())
                } else {
                    None
                };
                (name_ident, type_ann)
            }
            _ => {
                self.error("Expected field name after 'state:'");
                let span = self.current_token().span();
                (Identifier::new("Error", span), None)
            }
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
    /// - `classState: varName: TypeName`
    /// - `classState: varName: TypeName = defaultValue`
    fn parse_classvar_declaration(&mut self) -> Option<StateDeclaration> {
        let start = self.current_token().span();
        let doc_comment = self.collect_doc_comment();
        let mut comments = self.collect_comment_attachment();

        // Consume `classState:`
        if !matches!(self.current_kind(), TokenKind::Keyword(k) if k == "classState:") {
            return None;
        }
        self.advance();

        let (name, type_annotation) = match self.current_kind() {
            TokenKind::Keyword(keyword) => {
                let field_name = keyword.trim_end_matches(':');
                let span = self.current_token().span();
                let name_ident = Identifier::new(field_name, span);
                self.advance();
                let type_ann = self.parse_type_annotation();
                (name_ident, Some(type_ann))
            }
            TokenKind::Identifier(_) => {
                let name_ident =
                    self.parse_identifier("Expected variable name after 'classState:'");
                let type_ann = if self.match_token(&TokenKind::Colon) {
                    Some(self.parse_type_annotation())
                } else {
                    None
                };
                (name_ident, type_ann)
            }
            _ => {
                self.error("Expected variable name after 'classState:'");
                let span = self.current_token().span();
                (Identifier::new("Error", span), None)
            }
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
    fn parse_single_type_annotation(&mut self) -> TypeAnnotation {
        if let TokenKind::Identifier(name) = self.current_kind() {
            let span = self.current_token().span();
            let ident = Identifier::new(name.clone(), span);
            self.advance();
            TypeAnnotation::Simple(ident)
        } else {
            let span = self.current_token().span();
            self.error("Expected type name");
            TypeAnnotation::Simple(Identifier::new("Error", span))
        }
    }

    /// Parses an optional `-> ReturnType` annotation before `=>`.
    fn parse_optional_return_type(&mut self) -> Option<TypeAnnotation> {
        if matches!(self.current_kind(), TokenKind::BinarySelector(s) if s.as_str() == "->") {
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
    fn parse_method_definition(&mut self) -> Option<MethodDefinition> {
        let start = self.current_token().span();
        let doc_comment = self.collect_doc_comment();
        let comments = self.collect_comment_attachment();
        let method_kind = MethodKind::Primary;
        let mut method_is_sealed = false;
        let mut _is_class_method = false;

        // Parse optional modifiers
        while let TokenKind::Identifier(name) = self.current_kind() {
            match name.as_str() {
                "class" => {
                    // Only treat as modifier if next token is not `=>`
                    // (otherwise it's a method named `class`)
                    if matches!(self.peek_at(1), Some(TokenKind::FatArrow)) {
                        break;
                    }
                    _is_class_method = true;
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
            method_is_sealed,
            method_kind,
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
            // Binary method: `+ other` or `+ other: Type`
            TokenKind::BinarySelector(op) => {
                let selector = MessageSelector::Binary(op.clone());
                self.advance();

                // Parse the single parameter
                // If `other:` is lexed as Keyword (e.g., `+ other: Number`),
                // treat it as a typed parameter: name is `other`, type follows
                if let TokenKind::Keyword(kw) = self.current_kind() {
                    let param_name_str = kw.trim_end_matches(':');
                    let full_span = self.current_token().span();
                    let param_span = Self::span_without_trailing_colon(full_span);
                    let param_name = Identifier::new(param_name_str, param_span);
                    self.advance(); // consume `other:`
                    let type_ann = self.parse_type_annotation();
                    let param = ParameterDefinition::with_type(param_name, type_ann);
                    return Some((selector, vec![param]));
                }

                let param_name =
                    self.parse_identifier("Expected parameter name after binary selector");
                let type_annotation = self.parse_optional_param_type();
                let param = match type_annotation {
                    Some(ta) => ParameterDefinition::with_type(param_name, ta),
                    None => ParameterDefinition::new(param_name),
                };
                Some((selector, vec![param]))
            }
            // Keyword method: `at: index put: value` or `deposit: amount: Integer`
            TokenKind::Keyword(_) => {
                let mut keywords = Vec::new();
                let mut parameters = Vec::new();

                while let TokenKind::Keyword(keyword) = self.current_kind() {
                    let span = self.current_token().span();
                    keywords.push(KeywordPart::new(keyword.clone(), span));
                    self.advance();

                    // Check for typed parameter: `keyword: paramName: Type`
                    // where paramName: is a Keyword token followed by Identifier (type)
                    if let TokenKind::Keyword(param_keyword) = self.current_kind() {
                        // Disambiguate typed param vs next keyword selector part.
                        // If after `paramName:` + Identifier, we see `=>`, `->`, or another
                        // Keyword, the Identifier is a type name (typed parameter).
                        // Otherwise it's the next keyword selector part's parameter.
                        let param_name_str = param_keyword.trim_end_matches(':');
                        if matches!(self.peek_at(1), Some(TokenKind::Identifier(_))) {
                            let is_typed = matches!(
                                self.peek_at(2),
                                Some(
                                    TokenKind::FatArrow
                                        | TokenKind::Keyword(_)
                                        | TokenKind::BinarySelector(_)
                                )
                            );
                            if is_typed {
                                // Typed parameter: `paramName: Type`
                                let full_span = self.current_token().span();
                                let param_span = Self::span_without_trailing_colon(full_span);
                                let param_name = Identifier::new(param_name_str, param_span);
                                self.advance(); // consume `paramName:`

                                let type_ann = self.parse_type_annotation();
                                parameters
                                    .push(ParameterDefinition::with_type(param_name, type_ann));
                                continue;
                            }
                        }
                    }

                    // Parse untyped parameter name
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

    /// Creates a span that excludes the trailing colon from a keyword token span.
    fn span_without_trailing_colon(full_span: Span) -> Span {
        Span::new(full_span.start(), full_span.end() - 1)
    }

    /// Parses an optional `: Type` annotation after a parameter name.
    ///
    /// Used for both binary (`+ other : Number`) and keyword (`deposit: amount : Integer`)
    /// method parameters when the colon is a separate token (space before colon).
    fn parse_optional_param_type(&mut self) -> Option<TypeAnnotation> {
        if matches!(self.current_kind(), TokenKind::Colon) {
            self.advance(); // consume `:`
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
        #[allow(clippy::nonminimal_bool)]
        while !self.is_at_end()
            && !self.is_at_class_definition()
            && !self.is_at_method_definition()
            && !self.is_at_standalone_method_definition()
            && !matches!(self.current_kind(), TokenKind::Keyword(k) if k == "state:")
            && !(self.in_class_body && self.current_token().indentation_after_newline() == Some(0))
        {
            let pos_before = self.current;
            // BT-987: detect blank lines (2+ newlines) before this statement
            let has_blank_line =
                !body.is_empty() && self.current_token().has_preceding_blank_line();
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
                    || self.is_at_method_definition()
                    || self.is_at_standalone_method_definition()
                    || matches!(self.current_kind(), TokenKind::Keyword(k) if k == "state:")
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
                    || self.is_at_method_definition()
                    || self.is_at_standalone_method_definition()
                    || matches!(self.current_kind(), TokenKind::Keyword(k) if k == "state:")
                {
                    // Trailing period at end of method — not needed (BT-948)
                    let mut diag =
                        Diagnostic::lint("unnecessary trailing `.` at end of method", period_span);
                    diag.hint = Some("Remove the trailing `.`".into());
                    self.diagnostics.push(diag);
                    break;
                } else if !self.is_at_end() && self.current_token().has_leading_newline() {
                    // Period immediately before a newline — newline already separates statements
                    let mut diag = Diagnostic::lint(
                        "unnecessary `.` — the following newline already separates statements",
                        period_span,
                    );
                    diag.hint = Some("Remove the `.` and rely on the newline".into());
                    self.diagnostics.push(diag);
                }
                // Otherwise continue parsing more expressions
            } else if self.match_token(&TokenKind::Bang) {
                // Cast terminator — mark the last expression as a cast if it's a MessageSend.
                // If the expression is not a MessageSend (e.g. `x := foo bar!`), emit an error.
                match body.last_mut().map(|s| &mut s.expression) {
                    Some(Expression::MessageSend { is_cast, .. }) => *is_cast = true,
                    Some(last) => {
                        let span = last.span();
                        self.diagnostics.push(Diagnostic::error(
                            "Cast (!) has no return value and cannot be used in an expression. Use . for a synchronous call.",
                            span,
                        ));
                    }
                    None => {}
                }
                // Check if next token starts a new method/state/class (same as period)
                if self.is_at_end()
                    || self.is_at_class_definition()
                    || self.is_at_method_definition()
                    || self.is_at_standalone_method_definition()
                    || matches!(self.current_kind(), TokenKind::Keyword(k) if k == "state:")
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
    pub(super) fn parse_standalone_method_definition(&mut self) -> StandaloneMethodDefinition {
        let start = self.current_token().span();

        // Collect leading comments from the class-name token's leading trivia.
        // parse_identifier() advances past the class name without reading trivia,
        // so we must collect here before the token is consumed.
        let mut class_leading_comments = self.collect_comment_attachment().leading;

        // Parse class name
        let class_name = self.parse_identifier("Expected class name");

        // Check for optional `class` modifier
        let is_class_method = if matches!(self.current_kind(), TokenKind::Identifier(name) if name == "class")
        {
            // Only treat as modifier if next token is `>>`
            if matches!(self.peek_at(1), Some(TokenKind::BinarySelector(s)) if s == ">>") {
                self.advance(); // consume `class`
                true
            } else {
                false
            }
        } else {
            false
        };

        // Consume `>>`
        if !self.match_binary_selector(">>") {
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
            is_class_method,
            method,
            span,
        }
    }
}
