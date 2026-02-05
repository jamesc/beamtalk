// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Declaration parsing for Beamtalk.
//!
//! This module handles parsing of top-level declarations including:
//! - Class definitions with modifiers (`abstract`, `sealed`)
//! - State (field) declarations with types and default values
//! - Method definitions with advice modifiers (`before`, `after`, `around`)

use crate::ast::{
    ClassDefinition, Expression, Identifier, KeywordPart, MessageSelector, MethodDefinition,
    MethodKind, StateDeclaration, TypeAnnotation,
};
use crate::source_analysis::TokenKind;

use super::Parser;

impl Parser {
    // ========================================================================
    // Class Definition Parsing
    // ========================================================================

    /// Parses a class definition.
    ///
    /// Syntax:
    /// ```text
    /// abstract? sealed? <Superclass> subclass: <ClassName>
    ///   state: fieldName = defaultValue
    ///   state: fieldName: TypeName = defaultValue
    ///
    ///   methodName => body
    ///   before methodName => body
    ///   after methodName => body
    ///   around methodName => body
    ///   sealed methodName => body
    /// ```
    pub(super) fn parse_class_definition(&mut self) -> ClassDefinition {
        let start = self.current_token().span();
        let mut is_abstract = false;
        let mut is_sealed = false;

        // Parse optional modifiers: abstract, sealed
        while let TokenKind::Identifier(name) = self.current_kind() {
            if name == "abstract" {
                is_abstract = true;
                self.advance();
            } else if name == "sealed" {
                is_sealed = true;
                self.advance();
            } else {
                break;
            }
        }

        // Parse superclass name
        let superclass = self.parse_identifier("Expected superclass name");

        // Expect `subclass:` keyword
        if !matches!(self.current_kind(), TokenKind::Keyword(k) if k == "subclass:") {
            self.error("Expected 'subclass:' keyword");
            return ClassDefinition::new(
                Identifier::new("Error", start),
                superclass,
                Vec::new(),
                Vec::new(),
                start,
            );
        }
        self.advance(); // consume `subclass:`

        // Parse class name
        let name = self.parse_identifier("Expected class name");

        // Parse class body (state declarations and methods)
        let (state, methods) = self.parse_class_body();

        // Determine end span: prefer methods, fallback to state, then to name
        let end = methods
            .last()
            .map(|m| m.span)
            .or_else(|| state.last().map(|s| s.span))
            .unwrap_or(name.span);
        let span = start.merge(end);

        ClassDefinition::with_modifiers(
            name,
            superclass,
            is_abstract,
            is_sealed,
            state,
            methods,
            span,
        )
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
    fn parse_class_body(&mut self) -> (Vec<StateDeclaration>, Vec<MethodDefinition>) {
        let mut state = Vec::new();
        let mut methods = Vec::new();

        // Skip any periods/statement terminators
        while self.match_token(&TokenKind::Period) {}

        while !self.is_at_end() && !self.is_at_class_definition() {
            // Check for state declaration: `state: fieldName ...`
            if matches!(self.current_kind(), TokenKind::Keyword(k) if k == "state:") {
                if let Some(state_decl) = self.parse_state_declaration() {
                    state.push(state_decl);
                }
            }
            // Check for method definition (with optional modifiers)
            else if self.is_at_method_definition() {
                if let Some(method) = self.parse_method_definition() {
                    methods.push(method);
                }
            } else {
                // Not a state or method - end of class body
                break;
            }

            // Skip any periods/statement terminators
            while self.match_token(&TokenKind::Period) {}
        }

        (state, methods)
    }

    /// Checks if the current position is at the start of a method definition.
    ///
    /// Methods can start with:
    /// - An identifier followed directly by `=>` (unary method)
    /// - A binary selector followed by identifier and `=>` (binary method)
    /// - Keywords followed by identifiers and eventually `=>` (keyword method)
    /// - `before`, `after`, `around`, `sealed` followed by one of the above
    pub(super) fn is_at_method_definition(&self) -> bool {
        let mut offset = 0;

        // Skip optional modifiers: before, after, around, sealed
        while let Some(TokenKind::Identifier(name)) = self.peek_at(offset) {
            if matches!(name.as_str(), "before" | "after" | "around" | "sealed") {
                offset += 1;
            } else {
                break;
            }
        }

        // Check for method selector pattern followed by =>
        match self.peek_at(offset) {
            // Unary method: `identifier =>` (fat arrow must be next token)
            Some(TokenKind::Identifier(_)) => {
                matches!(self.peek_at(offset + 1), Some(TokenKind::FatArrow))
            }
            // Binary method: `+ other =>`
            Some(TokenKind::BinarySelector(_)) => {
                // Binary selector, then parameter name, then =>
                matches!(self.peek_at(offset + 1), Some(TokenKind::Identifier(_)))
                    && matches!(self.peek_at(offset + 2), Some(TokenKind::FatArrow))
            }
            // Keyword method: `at: index =>` or `at: index put: value =>`
            Some(TokenKind::Keyword(_)) => self.is_keyword_method_at(offset),
            _ => false,
        }
    }

    /// Checks if there's a keyword method definition starting at the given offset.
    ///
    /// Pattern: `keyword: param keyword: param ... =>`
    fn is_keyword_method_at(&self, start_offset: usize) -> bool {
        let mut offset = start_offset;

        // Must have at least one keyword-parameter pair
        loop {
            // Expect keyword
            if !matches!(self.peek_at(offset), Some(TokenKind::Keyword(_))) {
                return false;
            }
            offset += 1;

            // Expect parameter (identifier)
            if !matches!(self.peek_at(offset), Some(TokenKind::Identifier(_))) {
                return false;
            }
            offset += 1;

            // Check for => (end of method selector) or another keyword
            match self.peek_at(offset) {
                Some(TokenKind::FatArrow) => return true,
                Some(TokenKind::Keyword(_)) => {} // More keywords, continue loop
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
            span,
        })
    }

    /// Parses a simple type annotation (identifier).
    fn parse_type_annotation(&mut self) -> TypeAnnotation {
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

    /// Parses a method definition.
    ///
    /// Syntax:
    /// - `methodName => body`
    /// - `+ other => body`
    /// - `at: index put: value => body`
    /// - `before methodName => body`
    /// - `after methodName => body`
    /// - `around methodName => body`
    /// - `sealed methodName => body`
    fn parse_method_definition(&mut self) -> Option<MethodDefinition> {
        let start = self.current_token().span();
        let mut method_kind = MethodKind::Primary;
        let mut method_is_sealed = false;

        // Parse optional modifiers
        while let TokenKind::Identifier(name) = self.current_kind() {
            match name.as_str() {
                "before" => {
                    method_kind = MethodKind::Before;
                    self.advance();
                }
                "after" => {
                    method_kind = MethodKind::After;
                    self.advance();
                }
                "around" => {
                    method_kind = MethodKind::Around;
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

        // Expect fat arrow
        if !self.match_token(&TokenKind::FatArrow) {
            self.error("Expected '=>' after method selector");
            return None;
        }

        // Parse method body
        let body = self.parse_method_body();

        let end = body.last().map_or(start, Expression::span);
        let span = start.merge(end);

        Some(MethodDefinition::with_options(
            selector,
            parameters,
            body,
            None, // return_type - could add parsing later
            method_is_sealed,
            method_kind,
            span,
        ))
    }

    /// Parses a method selector and its parameters.
    ///
    /// Returns the selector and parameter names.
    fn parse_method_selector(&mut self) -> Option<(MessageSelector, Vec<Identifier>)> {
        match self.current_kind() {
            // Unary method: `methodName`
            TokenKind::Identifier(name) => {
                let selector = MessageSelector::Unary(name.clone());
                self.advance();
                Some((selector, Vec::new()))
            }
            // Binary method: `+ other`
            TokenKind::BinarySelector(op) => {
                let selector = MessageSelector::Binary(op.clone());
                self.advance();

                // Parse the single parameter
                let param = self.parse_identifier("Expected parameter name after binary selector");
                Some((selector, vec![param]))
            }
            // Keyword method: `at: index put: value`
            TokenKind::Keyword(_) => {
                let mut keywords = Vec::new();
                let mut parameters = Vec::new();

                while let TokenKind::Keyword(keyword) = self.current_kind() {
                    let span = self.current_token().span();
                    keywords.push(KeywordPart::new(keyword.clone(), span));
                    self.advance();

                    // Parse parameter name
                    let param = self.parse_identifier("Expected parameter name after keyword");
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

    /// Parses a method body (expressions until the next method or end of class).
    ///
    /// The body consists of expressions separated by periods.
    pub(super) fn parse_method_body(&mut self) -> Vec<Expression> {
        let mut body = Vec::new();

        // Parse expressions until we hit something that looks like a new method,
        // state declaration, or class definition
        while !self.is_at_end()
            && !self.is_at_class_definition()
            && !self.is_at_method_definition()
            && !matches!(self.current_kind(), TokenKind::Keyword(k) if k == "state:")
        {
            let expr = self.parse_expression();
            let is_error = expr.is_error();
            body.push(expr);

            // If we got an error, try to recover
            if is_error {
                self.synchronize();
                break;
            }

            // Period terminates the expression - check if we should continue
            if self.match_token(&TokenKind::Period) {
                // Check if next token starts a new method/state/class
                if self.is_at_end()
                    || self.is_at_class_definition()
                    || self.is_at_method_definition()
                    || matches!(self.current_kind(), TokenKind::Keyword(k) if k == "state:")
                {
                    break;
                }
                // Otherwise continue parsing more expressions
            } else {
                // No period - this was the last expression in the body
                break;
            }
        }

        body
    }
}
