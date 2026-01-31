// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Recursive descent parser for Beamtalk source code.
//!
//! This parser builds an AST from a stream of tokens. It is designed
//! for IDE use with comprehensive error recovery and diagnostics.
//!
//! # Design Philosophy
//!
//! - **Error recovery is mandatory** - Parser MUST always produce an AST
//! - **Multiple errors** - Report all errors, don't stop at first
//! - **Precise spans** - Every diagnostic points to exact source location
//! - **Synchronization points** - Recover at statement boundaries
//!
//! # Message Precedence
//!
//! Beamtalk follows Smalltalk's three-tier message precedence:
//!
//! 1. **Unary messages**: `object message` (highest precedence)
//! 2. **Binary messages**: `3 + 4` with **standard math precedence**
//!    - `* / %` before `+ -`
//!    - NOT strictly left-to-right like traditional Smalltalk!
//! 3. **Keyword messages**: `array at: 1 put: 'x'` (lowest precedence)
//!
//! # Binary Operator Precedence (Pratt Parsing)
//!
//! Binary operator precedence is handled using Pratt parsing (top-down operator
//! precedence parsing). This approach uses a binding power table to determine
//! precedence declaratively, making it easy to add new operators.
//!
//! | Level | Operators | Associativity |
//! |-------|-----------|---------------|
//! | 10 | `=` `~=` | Left |
//! | 20 | `<` `>` `<=` `>=` | Left |
//! | 30 | `+` `-` | Left |
//! | 40 | `*` `/` `%` | Left |
//!
//! To add a new operator, simply add an entry to [`binary_binding_power`].
//!
//! # Usage
//!
//! ```
//! use beamtalk_core::parse::{lex_with_eof, parse};
//!
//! let tokens = lex_with_eof("x := 3 + 4");
//! let (module, diagnostics) = parse(tokens);
//!
//! assert!(diagnostics.is_empty());
//! assert_eq!(module.expressions.len(), 1);
//! ```

use crate::ast::{
    Block, BlockParameter, CascadeMessage, ClassDefinition, Comment, CommentKind, CompoundOperator,
    Expression, Identifier, KeywordPart, Literal, MapPair, MessageSelector, MethodDefinition,
    MethodKind, Module, StateDeclaration, TypeAnnotation,
};
use crate::parse::{Span, Token, TokenKind};
use ecow::EcoString;

// ============================================================================
// Pratt Parsing for Binary Operator Precedence
// ============================================================================

/// Binding power for binary operators (Pratt parsing).
///
/// Higher values bind tighter. Left and right binding powers differ
/// for associativity:
/// - Left-associative: `left_bp == right_bp - 1` (e.g., `+`, `-`)
/// - Right-associative: `left_bp == right_bp + 1` (e.g., `**`)
#[derive(Debug, Clone, Copy)]
struct BindingPower {
    /// Left binding power (how tightly this operator binds to its left operand).
    left: u8,
    /// Right binding power (how tightly this operator binds to its right operand).
    right: u8,
}

impl BindingPower {
    /// Creates a left-associative binding power.
    const fn left_assoc(precedence: u8) -> Self {
        Self {
            left: precedence,
            right: precedence + 1,
        }
    }

    /// Creates a right-associative binding power.
    #[allow(dead_code)] // Reserved for future operators like `**`
    const fn right_assoc(precedence: u8) -> Self {
        Self {
            left: precedence + 1,
            right: precedence,
        }
    }
}

/// Gets the binding power for a binary operator.
///
/// Returns `None` for unknown operators, allowing the parser to treat
/// them as end of expression (useful for error recovery).
///
/// # Precedence Levels (from lowest to highest)
///
/// | Level | Operators | Associativity |
/// |-------|-----------|---------------|
/// | 10 | `=` `~=` | Left |
/// | 20 | `<` `>` `<=` `>=` | Left |
/// | 30 | `+` `-` | Left |
/// | 40 | `*` `/` `%` | Left |
///
/// To add a new operator, just add an entry here. For example:
/// ```ignore
/// // Bitwise OR (between comparison and additive)
/// "|" => Some(BindingPower::left_assoc(25)),
///
/// // Exponentiation (right-associative, higher than multiplication)
/// "**" => Some(BindingPower::right_assoc(50)),
/// ```
fn binary_binding_power(op: &str) -> Option<BindingPower> {
    match op {
        // Equality (lowest binary precedence)
        // `~=` is the Smalltalk-style not-equal operator
        "=" | "~=" => Some(BindingPower::left_assoc(10)),

        // Comparison
        "<" | ">" | "<=" | ">=" => Some(BindingPower::left_assoc(20)),

        // Additive
        "+" | "-" => Some(BindingPower::left_assoc(30)),

        // Multiplicative
        "*" | "/" | "%" => Some(BindingPower::left_assoc(40)),

        // Exponentiation (right-associative, highest binary precedence)
        // 2 ** 3 ** 2 = 2 ** (3 ** 2) = 2 ** 9 = 512
        "**" => Some(BindingPower::right_assoc(50)),

        // Unknown operator - return None to stop binary expression parsing
        _ => None,
    }
}

/// Parse a sequence of tokens into a module.
///
/// This is the main entry point for parsing. It always returns a [`Module`],
/// even if there are syntax errors. Check the returned diagnostics for errors.
///
/// # Examples
///
/// ```
/// use beamtalk_core::parse::{lex_with_eof, parse};
///
/// let tokens = lex_with_eof("x := 42");
/// let (module, diagnostics) = parse(tokens);
///
/// assert!(diagnostics.is_empty());
/// assert_eq!(module.expressions.len(), 1);
/// ```
#[must_use]
pub fn parse(tokens: Vec<Token>) -> (Module, Vec<Diagnostic>) {
    let mut parser = Parser::new(tokens);
    let module = parser.parse_module();
    (module, parser.diagnostics)
}

/// A diagnostic message (error or warning).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    /// The severity of the diagnostic.
    pub severity: Severity,
    /// The error message.
    pub message: EcoString,
    /// The source location.
    pub span: Span,
}

impl Diagnostic {
    /// Creates a new error diagnostic.
    #[must_use]
    pub fn error(message: impl Into<EcoString>, span: Span) -> Self {
        Self {
            severity: Severity::Error,
            message: message.into(),
            span,
        }
    }

    /// Creates a new warning diagnostic.
    #[must_use]
    pub fn warning(message: impl Into<EcoString>, span: Span) -> Self {
        Self {
            severity: Severity::Warning,
            message: message.into(),
            span,
        }
    }
}

/// Diagnostic severity level.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Severity {
    /// An error that prevents compilation.
    Error,
    /// A warning that should be addressed.
    Warning,
}

/// The parser state.
struct Parser {
    /// The tokens being parsed.
    tokens: Vec<Token>,
    /// Current token index.
    current: usize,
    /// Accumulated diagnostics.
    diagnostics: Vec<Diagnostic>,
}

impl Parser {
    /// Creates a new parser for the given tokens.
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            diagnostics: Vec::new(),
        }
    }

    // ========================================================================
    // Token Management
    // ========================================================================

    /// Returns the current token.
    fn current_token(&self) -> &Token {
        if self.current < self.tokens.len() {
            &self.tokens[self.current]
        } else {
            // If we've advanced past the end of the token stream, fall back to the last token
            // (which should be EOF in well-formed input) rather than panicking.
            self.tokens
                .last()
                .expect("Parser has no tokens; expected at least an EOF token")
        }
    }

    /// Returns the current token kind.
    fn current_kind(&self) -> &TokenKind {
        self.current_token().kind()
    }

    /// Peeks at the next token without consuming.
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current + 1)
    }

    /// Peeks at the next token kind.
    fn peek_kind(&self) -> Option<&TokenKind> {
        self.peek().map(Token::kind)
    }

    /// Checks if we're at the end of input.
    fn is_at_end(&self) -> bool {
        matches!(self.current_kind(), TokenKind::Eof)
    }

    /// Advances to the next token and returns the previous one.
    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.tokens[self.current - 1].clone()
    }

    /// Checks if the current token matches the given kind.
    fn check(&self, kind: &TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }
        std::mem::discriminant(self.current_kind()) == std::mem::discriminant(kind)
    }

    /// Consumes the current token if it matches the given kind.
    fn match_token(&mut self, kind: &TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Expects the current token to match the given kind, advancing if it does.
    ///
    /// If the token doesn't match, reports an error and returns `None`.
    fn expect(&mut self, kind: &TokenKind, message: &str) -> Option<Token> {
        if self.check(kind) {
            Some(self.advance())
        } else {
            let span = self.current_token().span();
            self.diagnostics.push(Diagnostic::error(message, span));
            None
        }
    }

    // ========================================================================
    // Error Handling & Recovery
    // ========================================================================

    /// Reports an error at the current token.
    fn error(&mut self, message: impl Into<EcoString>) {
        let span = self.current_token().span();
        self.diagnostics.push(Diagnostic::error(message, span));
    }

    /// Synchronizes parser to a safe recovery point.
    ///
    /// Advances until we find a statement boundary or synchronization point:
    /// - Period (`.`) - statement terminator
    /// - Right bracket (`]`) - block end
    /// - Right paren (`)`) - expression end
    /// - Right brace (`}`) - tuple end
    /// - Semicolon (`;`) - cascade separator
    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            // Found a statement boundary
            if matches!(
                self.current_kind(),
                TokenKind::Period
                    | TokenKind::RightBracket
                    | TokenKind::RightParen
                    | TokenKind::RightBrace
                    | TokenKind::Semicolon
            ) {
                return;
            }

            // New statement may be starting
            if matches!(self.current_kind(), TokenKind::Caret) {
                return;
            }

            self.advance();
        }
    }

    // ========================================================================
    // Module Parsing
    // ========================================================================

    /// Parses a complete module (top-level).
    fn parse_module(&mut self) -> Module {
        let start = self.current_token().span();
        let mut classes = Vec::new();
        let mut expressions = Vec::new();
        let mut comments = Vec::new();

        // Collect leading comments
        for trivia in self.current_token().leading_trivia() {
            if trivia.is_comment() {
                comments.push(Comment {
                    content: trivia.as_str().into(),
                    span: start, // TODO: track trivia spans
                    kind: match trivia {
                        super::Trivia::LineComment(_) => CommentKind::Line,
                        super::Trivia::BlockComment(_) => CommentKind::Block,
                        super::Trivia::Whitespace(_) => continue,
                    },
                });
            }
        }

        // Parse statements until EOF
        while !self.is_at_end() {
            // Check if this looks like a class definition
            if self.is_at_class_definition() {
                let class = self.parse_class_definition();
                classes.push(class);
            } else {
                let expr = self.parse_expression();
                let is_error = expr.is_error();
                expressions.push(expr);

                // If we got an error, try to recover
                if is_error {
                    self.synchronize();
                } else {
                    // Optional statement terminator
                    self.match_token(&TokenKind::Period);
                }
            }
        }

        // Get end span
        let end = if self.current > 0 {
            self.tokens[self.current - 1].span()
        } else {
            start
        };
        let span = start.merge(end);

        Module {
            classes,
            expressions,
            span,
            leading_comments: comments,
        }
    }

    /// Checks if the current position looks like the start of a class definition.
    ///
    /// Class definitions follow the pattern:
    /// - `abstract? sealed? <Superclass> subclass: <ClassName>`
    ///
    /// We look ahead to detect the `subclass:` keyword.
    fn is_at_class_definition(&self) -> bool {
        let mut offset = 0;

        // Skip optional `abstract` or `sealed` modifiers
        while let Some(kind) = self.peek_at(offset) {
            match kind {
                TokenKind::Identifier(name) if name == "abstract" || name == "sealed" => {
                    offset += 1;
                }
                _ => break,
            }
        }

        // Expect superclass name (identifier)
        if !matches!(self.peek_at(offset), Some(TokenKind::Identifier(_))) {
            return false;
        }
        offset += 1;

        // Expect `subclass:` keyword
        matches!(self.peek_at(offset), Some(TokenKind::Keyword(k)) if k == "subclass:")
    }

    /// Peeks at a token at the given offset from current position.
    fn peek_at(&self, offset: usize) -> Option<&TokenKind> {
        self.tokens.get(self.current + offset).map(Token::kind)
    }

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
    fn parse_class_definition(&mut self) -> ClassDefinition {
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

        let end = if !methods.is_empty() {
            methods.last().unwrap().span
        } else if !state.is_empty() {
            state.last().unwrap().span
        } else {
            name.span
        };
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
    fn parse_identifier(&mut self, error_message: &str) -> Identifier {
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
    fn is_at_method_definition(&self) -> bool {
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
    fn parse_method_body(&mut self) -> Vec<Expression> {
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

    // ========================================================================
    // Expression Parsing
    // ========================================================================

    /// Parses any expression.
    ///
    /// Entry point for expression parsing. Handles all precedence levels.
    fn parse_expression(&mut self) -> Expression {
        // Check for return statement first
        if self.match_token(&TokenKind::Caret) {
            return self.parse_return();
        }

        // Try to parse assignment or regular expression
        self.parse_assignment()
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
    fn parse_assignment(&mut self) -> Expression {
        let expr = self.parse_cascade();

        // Check for assignment operators
        if self.match_token(&TokenKind::Assign) {
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

            let value = Box::new(self.parse_assignment());
            let span = expr.span().merge(value.span());
            return Expression::Assignment {
                target: Box::new(expr),
                value,
                span,
            };
        }

        // Check for compound assignment (+=, -=, etc.)
        if let Some(op) = self.match_compound_operator() {
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

            let value = Box::new(self.parse_assignment());
            let span = expr.span().merge(value.span());
            return Expression::CompoundAssignment {
                target: Box::new(expr),
                operator: op,
                value,
                span,
            };
        }

        expr
    }

    /// Checks for compound assignment operators.
    fn match_compound_operator(&mut self) -> Option<CompoundOperator> {
        let kind = self.current_kind();
        let op = match kind {
            TokenKind::BinarySelector(s) if s.as_str() == "+=" => CompoundOperator::Add,
            TokenKind::BinarySelector(s) if s.as_str() == "-=" => CompoundOperator::Subtract,
            TokenKind::BinarySelector(s) if s.as_str() == "*=" => CompoundOperator::Multiply,
            TokenKind::BinarySelector(s) if s.as_str() == "/=" => CompoundOperator::Divide,
            TokenKind::BinarySelector(s) if s.as_str() == "%=" => CompoundOperator::Remainder,
            _ => return None,
        };
        self.advance();
        Some(op)
    }

    /// Parses a cascade (multiple messages to the same receiver).
    ///
    /// Syntax: `receiver message1; message2; message3`
    fn parse_cascade(&mut self) -> Expression {
        let receiver = self.parse_keyword_message();

        // Check if this is a cascade
        if !self.match_token(&TokenKind::Semicolon) {
            return receiver;
        }

        // This is a cascade - parse additional messages
        let mut messages = Vec::new();

        loop {
            let message = self.parse_cascade_message();
            messages.push(message);

            // Continue if there's another semicolon
            if !self.match_token(&TokenKind::Semicolon) {
                break;
            }
        }

        let last_span = messages.last().map_or(receiver.span(), |m| m.span);
        let span = receiver.span().merge(last_span);

        Expression::Cascade {
            receiver: Box::new(receiver),
            messages,
            span,
        }
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

            while let TokenKind::Keyword(keyword) = self.current_kind() {
                let span = self.current_token().span();
                keywords.push(KeywordPart::new(keyword.clone(), span));
                self.advance();

                // Parse argument (which is a binary message)
                arguments.push(self.parse_binary_message());
            }

            let end_span = arguments.last().map_or(start_span, Expression::span);
            let span = start_span.merge(end_span);

            return CascadeMessage::new(MessageSelector::Keyword(keywords), arguments, span);
        }

        // Try binary message
        if let TokenKind::BinarySelector(op) = self.current_kind() {
            let selector = MessageSelector::Binary(op.clone());
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

    /// Parses a keyword message (lowest precedence).
    fn parse_keyword_message(&mut self) -> Expression {
        let receiver = self.parse_binary_message();

        // Check if this is a keyword message
        // A newline before the keyword indicates a new statement, not a message
        if !matches!(self.current_kind(), TokenKind::Keyword(_))
            || self.current_token().has_leading_newline()
        {
            return receiver;
        }

        // Parse keyword message
        let mut keywords = Vec::new();
        let mut arguments = Vec::new();

        while let TokenKind::Keyword(keyword) = self.current_kind() {
            // Stop if the keyword is on a new line (start of new statement)
            if self.current_token().has_leading_newline() && !keywords.is_empty() {
                break;
            }
            let span = self.current_token().span();
            keywords.push(KeywordPart::new(keyword.clone(), span));
            self.advance();

            // Parse argument (which is a binary message)
            arguments.push(self.parse_binary_message());
        }

        let span = receiver.span().merge(arguments.last().unwrap().span());

        Expression::MessageSend {
            receiver: Box::new(receiver),
            selector: MessageSelector::Keyword(keywords),
            arguments,
            span,
        }
    }

    /// Parses a binary message with standard math precedence using Pratt parsing.
    ///
    /// Unlike traditional Smalltalk (which is strictly left-to-right),
    /// Beamtalk binary messages follow standard operator precedence:
    /// - Multiplicative: `* / %` (higher precedence)
    /// - Additive: `+ -` (lower precedence)
    /// - Comparison: `< > <= >= = ~=` (lowest precedence)
    ///
    /// This implementation uses Pratt parsing (top-down operator precedence)
    /// which makes adding new operators a single-line change in the
    /// [`binary_binding_power`] function.
    fn parse_binary_message(&mut self) -> Expression {
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
        let mut left = self.parse_unary_message();

        while let TokenKind::BinarySelector(op) = self.current_kind() {
            let op = op.clone();

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
                span,
            };
        }

        left
    }

    /// Parses unary messages (highest message precedence).
    fn parse_unary_message(&mut self) -> Expression {
        let mut receiver = self.parse_primary();

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
            | TokenKind::InterpolatedString(_)
            | TokenKind::Symbol(_)
            | TokenKind::Character(_) => self.parse_literal(),

            // Map literal: #{...}
            TokenKind::Hash => {
                // Check if it's a map literal (followed by {)
                if matches!(self.peek_kind(), Some(TokenKind::LeftBrace)) {
                    self.parse_map_literal()
                } else {
                    // Standalone '#' is not a valid primary expression
                    let bad_token = self.advance();
                    let span = bad_token.span();
                    let message: EcoString =
                        "Unexpected '#': expected '#{' for a map literal or a valid expression"
                            .into();
                    self.diagnostics
                        .push(Diagnostic::error(message.clone(), span));
                    Expression::Error { message, span }
                }
            }

            // Identifier or field access
            TokenKind::Identifier(_) => self.parse_identifier_or_field_access(),

            // Block
            TokenKind::LeftBracket => self.parse_block(),

            // Parenthesized expression
            TokenKind::LeftParen => self.parse_parenthesized(),

            // Map literal
            TokenKind::MapOpen => self.parse_map_literal(),

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
                if let Ok(val) = s.parse::<f64>() {
                    Literal::Float(val)
                } else {
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
            TokenKind::String(s) => Literal::String(s),
            TokenKind::InterpolatedString(s) => {
                // For now, treat interpolated strings as regular strings
                // TODO: Implement proper string interpolation in AST
                Literal::String(s)
            }
            TokenKind::Symbol(s) => Literal::Symbol(s),
            TokenKind::Character(c) => Literal::Character(c),
            _ => unreachable!(),
        };

        Expression::Literal(literal, span)
    }

    /// Parses an identifier or field access.
    fn parse_identifier_or_field_access(&mut self) -> Expression {
        let name_token = self.advance();
        let TokenKind::Identifier(name) = name_token.kind() else {
            unreachable!()
        };
        let span = name_token.span();

        let mut expr = Expression::Identifier(Identifier::new(name.clone(), span));

        // Check for field access: identifier.field
        // Only treat a '.' as starting a field access if:
        // 1. The current token is a Period
        // 2. The next token is an Identifier
        // 3. There is no whitespace between the period and the identifier
        //
        // This distinguishes `self.value` (field access) from `n. self` (statement
        // separator followed by new expression).
        //
        // Whitespace detection: The lexer puts trailing same-line whitespace (spaces/tabs)
        // as trailing trivia on the period, and newlines + subsequent whitespace as leading
        // trivia on the next token. We check BOTH to catch all cases:
        // - `n. self` → period has trailing space
        // - `n.\n    self` → identifier has leading newline+spaces
        while self.check(&TokenKind::Period)
            && matches!(self.peek_kind(), Some(TokenKind::Identifier(_)))
            && self.current_token().trailing_trivia().is_empty()
            && self.peek().is_some_and(|t| t.leading_trivia().is_empty())
        {
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

    /// Parses a block: `[:x :y | x + y]`
    fn parse_block(&mut self) -> Expression {
        let start = self
            .expect(&TokenKind::LeftBracket, "Expected '['")
            .unwrap()
            .span();

        let mut parameters = Vec::new();

        // Parse block parameters: [:x :y |
        while self.match_token(&TokenKind::Colon) {
            if let TokenKind::Identifier(name) = self.current_kind() {
                let span = self.current_token().span();
                parameters.push(BlockParameter::new(name.clone(), span));
                self.advance();
            } else {
                self.error("Expected parameter name after ':'");
                break;
            }
        }

        // Expect pipe separator if there are parameters
        if !parameters.is_empty() {
            self.expect(&TokenKind::Pipe, "Expected '|' after block parameters");
        }

        // Parse block body
        let mut body = Vec::new();
        while !self.check(&TokenKind::RightBracket) && !self.is_at_end() {
            let expr = self.parse_expression();
            body.push(expr);

            // Optional statement separator
            if !self.match_token(&TokenKind::Period) {
                break;
            }
        }

        let end = self
            .expect(&TokenKind::RightBracket, "Expected ']' to close block")
            .map_or(start, |t| t.span());

        let span = start.merge(end);
        let block = Block::new(parameters, body, span);
        Expression::Block(block)
    }

    /// Parses a map literal: `#{key => value, ...}`
    ///
    /// Map keys and values are parsed as unary expressions (primaries + unary messages),
    /// which stops at binary operators like `=>` and `,`. This allows nested maps and
    /// simple expressions as keys/values while avoiding ambiguity with the map syntax.
    fn parse_map_literal(&mut self) -> Expression {
        let start_token = self.expect(&TokenKind::MapOpen, "Expected '#{'");
        let start = start_token.map_or_else(|| self.current_token().span(), |t| t.span());

        let mut pairs = Vec::new();

        // Handle empty map: #{}
        if self.check(&TokenKind::RightBrace) {
            let end_token = self.advance();
            let span = start.merge(end_token.span());
            return Expression::MapLiteral { pairs, span };
        }

        // Parse key-value pairs
        loop {
            let pair_start = self.current_token().span();

            // Parse key expression (unary only - stops at `=>`, `,`, `}`)
            let key = self.parse_unary_message();

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

            // Parse value expression (unary only - stops at `,`, `}`)
            let value = self.parse_unary_message();

            let pair_span = pair_start.merge(value.span());
            pairs.push(MapPair::new(key, value, pair_span));

            // Check for comma (continue) or closing brace (end)
            if matches!(self.current_kind(), TokenKind::BinarySelector(s) if s.as_str() == ",") {
                self.advance(); // consume comma
                // Allow trailing comma
                if self.check(&TokenKind::RightBrace) {
                    break;
                }
            } else if self.check(&TokenKind::RightBrace) {
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
        Expression::MapLiteral { pairs, span }
    }

    /// Parses a parenthesized expression.
    fn parse_parenthesized(&mut self) -> Expression {
        let start = self
            .expect(&TokenKind::LeftParen, "Expected '('")
            .unwrap()
            .span();

        let inner = self.parse_expression();

        let end = self
            .expect(&TokenKind::RightParen, "Expected ')' to close parentheses")
            .map_or(start, |t| t.span());

        let span = start.merge(end);

        Expression::Parenthesized {
            expression: Box::new(inner),
            span,
        }
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Parses an integer literal, handling different radix notations.
///
/// Examples:
/// - `42` → 42 (decimal)
/// - `16rFF` → 255 (hexadecimal)
/// - `2r1010` → 10 (binary)
fn parse_integer(s: &str) -> Result<i64, String> {
    // Check for radix notation: NrVALUE
    if let Some(r_pos) = s.find('r') {
        let radix_str = &s[..r_pos];
        let value_str = &s[r_pos + 1..];

        let radix = radix_str
            .parse::<u32>()
            .map_err(|_| format!("Invalid radix: {radix_str}"))?;

        if !(2..=36).contains(&radix) {
            return Err(format!("Radix must be between 2 and 36, got {radix}"));
        }

        i64::from_str_radix(value_str, radix)
            .map_err(|_| format!("Invalid integer value '{value_str}' for radix {radix}"))
    } else {
        // Standard decimal integer
        s.parse::<i64>()
            .map_err(|_| format!("Invalid integer literal: {s}"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::lex_with_eof;

    /// Helper to parse a string and check for errors.
    fn parse_ok(source: &str) -> Module {
        let tokens = lex_with_eof(source);
        let (module, diagnostics) = parse(tokens);
        assert!(
            diagnostics.is_empty(),
            "Expected no errors, got: {diagnostics:?}"
        );
        module
    }

    /// Helper to parse a string expecting errors.
    fn parse_err(source: &str) -> Vec<Diagnostic> {
        let tokens = lex_with_eof(source);
        let (_module, diagnostics) = parse(tokens);
        diagnostics
    }

    #[test]
    fn parse_integer_literal() {
        let module = parse_ok("42");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Literal(Literal::Integer(42), _) => {}
            _ => panic!("Expected integer literal"),
        }
    }

    #[test]
    fn parse_float_literal() {
        let module = parse_ok("2.5");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Literal(Literal::Float(f), _) if (*f - 2.5_f64).abs() < 0.001 => {}
            _ => panic!("Expected float literal"),
        }
    }

    #[test]
    fn parse_string_literal() {
        let module = parse_ok("'hello'");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Literal(Literal::String(s), _) if s == "hello" => {}
            _ => panic!("Expected string literal"),
        }
    }

    #[test]
    fn parse_symbol_literal() {
        let module = parse_ok("#symbol");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Literal(Literal::Symbol(s), _) if s == "symbol" => {}
            _ => panic!("Expected symbol literal"),
        }
    }

    #[test]
    fn parse_identifier() {
        let module = parse_ok("myVar");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Identifier(id) if id.name == "myVar" => {}
            _ => panic!("Expected identifier"),
        }
    }

    #[test]
    fn parse_assignment() {
        let module = parse_ok("x := 42");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Assignment { target, value, .. } => {
                assert!(matches!(**target, Expression::Identifier(_)));
                assert!(matches!(
                    **value,
                    Expression::Literal(Literal::Integer(42), _)
                ));
            }
            _ => panic!("Expected assignment"),
        }
    }

    #[test]
    fn parse_unary_message() {
        let module = parse_ok("3 factorial");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::MessageSend {
                receiver,
                selector: MessageSelector::Unary(name),
                arguments,
                ..
            } => {
                assert!(matches!(
                    **receiver,
                    Expression::Literal(Literal::Integer(3), _)
                ));
                assert_eq!(name.as_str(), "factorial");
                assert!(arguments.is_empty());
            }
            _ => panic!("Expected unary message send"),
        }
    }

    #[test]
    fn parse_binary_message() {
        let module = parse_ok("3 + 4");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::MessageSend {
                receiver,
                selector: MessageSelector::Binary(op),
                arguments,
                ..
            } => {
                assert!(matches!(
                    **receiver,
                    Expression::Literal(Literal::Integer(3), _)
                ));
                assert_eq!(op.as_str(), "+");
                assert_eq!(arguments.len(), 1);
                assert!(matches!(
                    arguments[0],
                    Expression::Literal(Literal::Integer(4), _)
                ));
            }
            _ => panic!("Expected binary message send"),
        }
    }

    #[test]
    fn parse_binary_message_with_precedence() {
        // Test: 2 + 3 * 4 should be 2 + (3 * 4) = 14
        let module = parse_ok("2 + 3 * 4");
        assert_eq!(module.expressions.len(), 1);

        // The AST should be: (2 + (3 * 4))
        match &module.expressions[0] {
            Expression::MessageSend {
                receiver,
                selector: MessageSelector::Binary(op),
                arguments,
                ..
            } => {
                assert!(matches!(
                    **receiver,
                    Expression::Literal(Literal::Integer(2), _)
                ));
                assert_eq!(op.as_str(), "+");
                assert_eq!(arguments.len(), 1);

                // The argument should be (3 * 4)
                match &arguments[0] {
                    Expression::MessageSend {
                        receiver: r2,
                        selector: MessageSelector::Binary(op2),
                        arguments: args2,
                        ..
                    } => {
                        assert!(matches!(**r2, Expression::Literal(Literal::Integer(3), _)));
                        assert_eq!(op2.as_str(), "*");
                        assert_eq!(args2.len(), 1);
                        assert!(matches!(
                            args2[0],
                            Expression::Literal(Literal::Integer(4), _)
                        ));
                    }
                    _ => panic!("Expected multiplication as right operand"),
                }
            }
            _ => panic!("Expected addition at top level"),
        }
    }

    #[test]
    fn parse_keyword_message() {
        let module = parse_ok("array at: 1 put: 'x'");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::MessageSend {
                receiver,
                selector: MessageSelector::Keyword(parts),
                arguments,
                ..
            } => {
                assert!(matches!(**receiver, Expression::Identifier(_)));
                assert_eq!(parts.len(), 2);
                assert_eq!(parts[0].keyword.as_str(), "at:");
                assert_eq!(parts[1].keyword.as_str(), "put:");
                assert_eq!(arguments.len(), 2);
            }
            _ => panic!("Expected keyword message send"),
        }
    }

    #[test]
    fn parse_block_no_params() {
        let module = parse_ok("[42]");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Block(block) => {
                assert!(block.parameters.is_empty());
                assert_eq!(block.body.len(), 1);
            }
            _ => panic!("Expected block"),
        }
    }

    #[test]
    fn parse_block_with_params() {
        let module = parse_ok("[:x :y | x + y]");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Block(block) => {
                assert_eq!(block.parameters.len(), 2);
                assert_eq!(block.parameters[0].name.as_str(), "x");
                assert_eq!(block.parameters[1].name.as_str(), "y");
                assert_eq!(block.body.len(), 1);
            }
            _ => panic!("Expected block"),
        }
    }

    #[test]
    fn parse_return_statement() {
        let module = parse_ok("^42");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Return { value, .. } => {
                assert!(matches!(
                    **value,
                    Expression::Literal(Literal::Integer(42), _)
                ));
            }
            _ => panic!("Expected return statement"),
        }
    }

    #[test]
    fn parse_parenthesized() {
        let module = parse_ok("(3 + 4) * 2");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::MessageSend {
                receiver,
                selector: MessageSelector::Binary(op),
                arguments,
                ..
            } => {
                assert!(matches!(**receiver, Expression::Parenthesized { .. }));
                assert_eq!(op.as_str(), "*");
                assert_eq!(arguments.len(), 1);
            }
            _ => panic!("Expected binary message with parenthesized receiver"),
        }
    }

    #[test]
    fn parse_field_access() {
        let module = parse_ok("self.value");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::FieldAccess {
                receiver, field, ..
            } => {
                assert!(matches!(**receiver, Expression::Identifier(_)));
                assert_eq!(field.name.as_str(), "value");
            }
            _ => panic!("Expected field access"),
        }
    }

    #[test]
    fn parse_compound_assignment() {
        let module = parse_ok("x += 1");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::CompoundAssignment {
                target,
                operator,
                value,
                ..
            } => {
                assert!(matches!(**target, Expression::Identifier(_)));
                assert_eq!(*operator, CompoundOperator::Add);
                assert!(matches!(
                    **value,
                    Expression::Literal(Literal::Integer(1), _)
                ));
            }
            _ => panic!("Expected compound assignment"),
        }
    }

    #[test]
    fn parse_radix_integer() {
        assert_eq!(parse_integer("16rFF").unwrap(), 255);
        assert_eq!(parse_integer("2r1010").unwrap(), 10);
        assert_eq!(parse_integer("8r17").unwrap(), 15);
        assert!(parse_integer("37r10").is_err()); // Invalid radix
        assert!(parse_integer("16rGG").is_err()); // Invalid digits for radix
    }

    #[test]
    fn parse_empty_input() {
        let module = parse_ok("");
        assert!(module.expressions.is_empty());
    }

    #[test]
    fn parse_error_recovery() {
        // Parse with an intentional error
        let diagnostics = parse_err("x := @");
        assert!(!diagnostics.is_empty());
        // Parser should recover and not panic
    }

    #[test]
    fn parse_cascade() {
        let module = parse_ok("Transcript show: 'Hello'; cr; show: 'World'");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Cascade {
                receiver, messages, ..
            } => {
                // Receiver is the first message: Transcript show: 'Hello'
                assert!(matches!(**receiver, Expression::MessageSend { .. }));
                assert_eq!(messages.len(), 2);
                // First message: cr (unary)
                assert!(matches!(
                    &messages[0].selector,
                    MessageSelector::Unary(name) if name == "cr"
                ));
                assert!(messages[0].arguments.is_empty());
                // Second message: show: 'World' (keyword)
                assert!(matches!(&messages[1].selector, MessageSelector::Keyword(_)));
                assert_eq!(messages[1].arguments.len(), 1);
            }
            _ => panic!("Expected cascade expression"),
        }
    }

    #[test]
    fn parse_cascade_simple() {
        let module = parse_ok("x foo; bar; baz");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Cascade {
                receiver, messages, ..
            } => {
                // Receiver is the first unary message: x foo
                assert!(matches!(**receiver, Expression::MessageSend { .. }));
                assert_eq!(messages.len(), 2);
                assert!(matches!(
                    &messages[0].selector,
                    MessageSelector::Unary(name) if name == "bar"
                ));
                assert!(matches!(
                    &messages[1].selector,
                    MessageSelector::Unary(name) if name == "baz"
                ));
            }
            _ => panic!("Expected cascade expression"),
        }
    }

    #[test]
    fn parse_statement_with_period() {
        // Test that unary messages followed by period work correctly
        let module = parse_ok("obj foo.");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::MessageSend {
                receiver,
                selector: MessageSelector::Unary(name),
                ..
            } => {
                assert!(matches!(**receiver, Expression::Identifier(_)));
                assert_eq!(name.as_str(), "foo");
            }
            _ => panic!("Expected unary message send"),
        }
    }

    #[test]
    fn parse_invalid_assignment_target() {
        let diagnostics = parse_err("3 := 4");
        assert!(!diagnostics.is_empty());
        assert!(
            diagnostics[0]
                .message
                .contains("Assignment target must be an identifier or field access")
        );
    }

    #[test]
    fn parse_interpolated_string() {
        let module = parse_ok("\"Hello, {name}!\"");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Literal(Literal::String(s), _) if s == "Hello, {name}!" => {}
            _ => panic!("Expected interpolated string parsed as string literal"),
        }
    }

    #[test]
    fn parse_error_preserved_in_ast() {
        let tokens = lex_with_eof("x := @");
        let (module, diagnostics) = parse(tokens);
        assert!(!diagnostics.is_empty());
        // Error expression should be preserved in the AST
        // We expect 1 expression: the Assignment with an Error value
        // (not the error discarded as before)
        assert_eq!(module.expressions.len(), 1);
        // The assignment should contain the error as its value
        if let Expression::Assignment { value, .. } = &module.expressions[0] {
            assert!(value.is_error());
        }
    }

    #[test]
    fn parse_field_access_with_period() {
        // Test that field access doesn't consume statement terminator
        let module = parse_ok("self.value.");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::FieldAccess { .. } => {}
            _ => panic!("Expected field access"),
        }
    }

    #[test]
    fn parse_block_multiple_statements_with_binary_op() {
        // Regression test: ensure `. ` (period + space) is parsed as statement
        // separator, not field access
        let module = parse_ok("[ 1 + m. y := 1]");
        match &module.expressions[0] {
            Expression::Block(block) => {
                assert_eq!(block.body.len(), 2, "Block should have 2 statements");
            }
            _ => panic!("Expected block"),
        }
    }

    #[test]
    fn parse_empty_map() {
        let module = parse_ok("#{}");
        assert_eq!(module.expressions.len(), 1);
        assert!(matches!(
            &module.expressions[0],
            Expression::MapLiteral { pairs, .. } if pairs.is_empty()
        ));
    }

    #[test]
    fn parse_map_with_atom_keys() {
        let module = parse_ok("#{#name => 'Alice', #age => 30}");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::MapLiteral { pairs, .. } => {
                assert_eq!(pairs.len(), 2);
                // First pair: #name => 'Alice'
                assert!(
                    matches!(&pairs[0].key, Expression::Literal(Literal::Symbol(s), _) if s == "name")
                );
                assert!(
                    matches!(&pairs[0].value, Expression::Literal(Literal::String(s), _) if s == "Alice")
                );
                // Second pair: #age => 30
                assert!(
                    matches!(&pairs[1].key, Expression::Literal(Literal::Symbol(s), _) if s == "age")
                );
                assert!(matches!(
                    &pairs[1].value,
                    Expression::Literal(Literal::Integer(30), _)
                ));
            }
            _ => panic!("Expected MapLiteral"),
        }
    }

    #[test]
    fn parse_block_with_field_assignments() {
        // Regression test: multiple field assignments should parse correctly
        let module = parse_ok("[:n | self.x := self.x + n. self.x := self.x + n. ^self.x]");
        match &module.expressions[0] {
            Expression::Block(block) => {
                assert_eq!(block.body.len(), 3, "Block should have 3 statements");
                assert!(matches!(block.body[0], Expression::Assignment { .. }));
                assert!(matches!(block.body[1], Expression::Assignment { .. }));
                assert!(matches!(block.body[2], Expression::Return { .. }));
            }
            _ => panic!("Expected block"),
        }
    }

    #[test]
    fn parse_map_with_string_keys() {
        let module = parse_ok("#{'host' => 'localhost', 'port' => 8080}");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::MapLiteral { pairs, .. } => {
                assert_eq!(pairs.len(), 2);
                assert!(
                    matches!(&pairs[0].key, Expression::Literal(Literal::String(s), _) if s == "host")
                );
                assert!(
                    matches!(&pairs[0].value, Expression::Literal(Literal::String(s), _) if s == "localhost")
                );
            }
            _ => panic!("Expected MapLiteral"),
        }
    }

    #[test]
    fn parse_block_with_newlines_between_statements() {
        // Regression test: newlines after period should not be treated as field access
        let module = parse_ok(
            "[:n |
    self.value := self.value + n.
    self.value := self.value + n.
    ^self.value
]",
        );
        match &module.expressions[0] {
            Expression::Block(block) => {
                assert_eq!(block.body.len(), 3, "Block should have 3 statements");
            }
            _ => panic!("Expected block"),
        }
    }

    #[test]
    fn parse_map_with_integer_keys() {
        let module = parse_ok("#{1 => 'first', 2 => 'second'}");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::MapLiteral { pairs, .. } => {
                assert_eq!(pairs.len(), 2);
                assert!(matches!(
                    &pairs[0].key,
                    Expression::Literal(Literal::Integer(1), _)
                ));
                assert!(
                    matches!(&pairs[0].value, Expression::Literal(Literal::String(s), _) if s == "first")
                );
            }
            _ => panic!("Expected MapLiteral"),
        }
    }

    #[test]
    fn parse_map_with_trailing_comma() {
        let module = parse_ok("#{#a => 1, #b => 2,}");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::MapLiteral { pairs, .. } => {
                assert_eq!(pairs.len(), 2);
            }
            _ => panic!("Expected MapLiteral"),
        }
    }

    #[test]
    fn parse_nested_maps() {
        let module = parse_ok("#{#outer => #{#inner => 'value'}}");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::MapLiteral { pairs, .. } => {
                assert_eq!(pairs.len(), 1);
                // Outer key is #outer
                assert!(
                    matches!(&pairs[0].key, Expression::Literal(Literal::Symbol(s), _) if s == "outer")
                );
                // Value is a nested map
                match &pairs[0].value {
                    Expression::MapLiteral {
                        pairs: inner_pairs, ..
                    } => {
                        assert_eq!(inner_pairs.len(), 1);
                        assert!(
                            matches!(&inner_pairs[0].key, Expression::Literal(Literal::Symbol(s), _) if s == "inner")
                        );
                        assert!(
                            matches!(&inner_pairs[0].value, Expression::Literal(Literal::String(s), _) if s == "value")
                        );
                    }
                    _ => panic!("Expected nested MapLiteral"),
                }
            }
            _ => panic!("Expected MapLiteral"),
        }
    }

    #[test]
    fn parse_map_assignment() {
        let module = parse_ok("person := #{#name => 'Alice'}");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::Assignment { target, value, .. } => {
                assert!(
                    matches!(target.as_ref(), Expression::Identifier(id) if id.name == "person")
                );
                assert!(
                    matches!(value.as_ref(), Expression::MapLiteral { pairs, .. } if pairs.len() == 1)
                );
            }
            _ => panic!("Expected Assignment"),
        }
    }

    #[test]
    fn parse_multiple_map_assignments() {
        let source = "empty := #{}\nperson := #{#name => 'Alice', #age => 30}\n";
        let module = parse_ok(source);

        assert_eq!(module.expressions.len(), 2, "Expected 2 expressions");

        // First: empty := #{}
        match &module.expressions[0] {
            Expression::Assignment { target, value, .. } => {
                assert!(
                    matches!(target.as_ref(), Expression::Identifier(id) if id.name == "empty")
                );
                assert!(
                    matches!(value.as_ref(), Expression::MapLiteral { pairs, .. } if pairs.is_empty())
                );
            }
            other => panic!("Expected Assignment for first expr, got {other:?}"),
        }

        // Second: person := #{#name => 'Alice', #age => 30}
        match &module.expressions[1] {
            Expression::Assignment { target, value, .. } => {
                assert!(
                    matches!(target.as_ref(), Expression::Identifier(id) if id.name == "person")
                );
                assert!(
                    matches!(value.as_ref(), Expression::MapLiteral { pairs, .. } if pairs.len() == 2)
                );
            }
            other => panic!("Expected Assignment for second expr, got {other:?}"),
        }
    }

    // ========================================================================
    // Class Definition Parsing Tests
    // ========================================================================

    #[test]
    fn parse_basic_class_definition() {
        let module = parse_ok(
            "Actor subclass: Counter
  state: value = 0

  increment => self.value += 1
  getValue => ^self.value",
        );

        assert_eq!(module.classes.len(), 1);
        let class = &module.classes[0];

        assert_eq!(class.name.name, "Counter");
        assert_eq!(class.superclass.name, "Actor");
        assert!(!class.is_abstract);
        assert!(!class.is_sealed);
        assert_eq!(class.state.len(), 1);
        assert_eq!(class.state[0].name.name, "value");
        assert_eq!(class.methods.len(), 2);
        assert_eq!(class.methods[0].selector.name(), "increment");
        assert_eq!(class.methods[1].selector.name(), "getValue");
    }

    #[test]
    fn parse_abstract_class() {
        let module = parse_ok(
            "abstract Actor subclass: Collection
  size => self subclassResponsibility",
        );

        assert_eq!(module.classes.len(), 1);
        let class = &module.classes[0];

        assert!(class.is_abstract);
        assert!(!class.is_sealed);
        assert_eq!(class.name.name, "Collection");
        assert_eq!(class.superclass.name, "Actor");
    }

    #[test]
    fn parse_sealed_class() {
        let module = parse_ok(
            "sealed Actor subclass: Point
  state: x = 0
  state: y = 0",
        );

        assert_eq!(module.classes.len(), 1);
        let class = &module.classes[0];

        assert!(!class.is_abstract);
        assert!(class.is_sealed);
        assert_eq!(class.name.name, "Point");
        assert_eq!(class.state.len(), 2);
    }

    #[test]
    fn parse_state_with_type_annotation() {
        let module = parse_ok(
            "Actor subclass: Person
  state: name: String = 'unnamed'",
        );

        assert_eq!(module.classes.len(), 1);
        let class = &module.classes[0];
        assert_eq!(class.state.len(), 1);

        let state = &class.state[0];
        assert_eq!(state.name.name, "name");
        assert!(state.type_annotation.is_some());
        assert!(state.default_value.is_some());
    }

    #[test]
    fn parse_binary_method() {
        let module = parse_ok(
            "Actor subclass: Number
  + other => self.value + other",
        );

        assert_eq!(module.classes.len(), 1);
        let class = &module.classes[0];
        assert_eq!(class.methods.len(), 1);

        let method = &class.methods[0];
        assert_eq!(method.selector.name(), "+");
        assert_eq!(method.parameters.len(), 1);
        assert_eq!(method.parameters[0].name, "other");
    }

    #[test]
    fn parse_keyword_method() {
        let module = parse_ok(
            "Actor subclass: Array
  at: index put: value => self.storage at: index put: value",
        );

        assert_eq!(module.classes.len(), 1);
        let class = &module.classes[0];
        assert_eq!(class.methods.len(), 1);

        let method = &class.methods[0];
        assert_eq!(method.selector.name(), "at:put:");
        assert_eq!(method.parameters.len(), 2);
        assert_eq!(method.parameters[0].name, "index");
        assert_eq!(method.parameters[1].name, "value");
    }

    #[test]
    fn parse_sealed_method() {
        let module = parse_ok(
            "Actor subclass: Counter
  sealed getValue => ^self.value",
        );

        assert_eq!(module.classes.len(), 1);
        let class = &module.classes[0];
        assert_eq!(class.methods.len(), 1);

        let method = &class.methods[0];
        assert!(method.is_sealed);
        assert_eq!(method.kind, crate::ast::MethodKind::Primary);
    }

    #[test]
    fn parse_before_method() {
        let module = parse_ok(
            "Actor subclass: Agent
  before processMessage => Telemetry log: 'processing'",
        );

        assert_eq!(module.classes.len(), 1);
        let class = &module.classes[0];
        assert_eq!(class.methods.len(), 1);

        let method = &class.methods[0];
        assert_eq!(method.kind, crate::ast::MethodKind::Before);
        assert_eq!(method.selector.name(), "processMessage");
    }

    #[test]
    fn parse_after_method() {
        let module = parse_ok(
            "Actor subclass: Counter
  after increment => self.observers notify",
        );

        assert_eq!(module.classes.len(), 1);
        let class = &module.classes[0];
        assert_eq!(class.methods.len(), 1);

        let method = &class.methods[0];
        assert_eq!(method.kind, crate::ast::MethodKind::After);
    }

    #[test]
    fn parse_around_method() {
        let module = parse_ok(
            "Actor subclass: Cached
  around calculate => self.cache ifNil: [self.cache := self proceed]",
        );

        assert_eq!(module.classes.len(), 1);
        let class = &module.classes[0];
        assert_eq!(class.methods.len(), 1);

        let method = &class.methods[0];
        assert_eq!(method.kind, crate::ast::MethodKind::Around);
    }

    #[test]
    fn parse_class_with_mixed_content() {
        let module = parse_ok(
            "Actor subclass: Counter
  state: value = 0
  state: name: String

  increment => self.value += 1
  decrement => self.value -= 1

  before increment => Telemetry log: 'incrementing'
  after increment => self notifyObservers",
        );

        assert_eq!(module.classes.len(), 1);
        let class = &module.classes[0];

        assert_eq!(class.state.len(), 2);
        assert_eq!(class.methods.len(), 4);

        // Check method kinds
        assert_eq!(class.methods[0].kind, crate::ast::MethodKind::Primary);
        assert_eq!(class.methods[1].kind, crate::ast::MethodKind::Primary);
        assert_eq!(class.methods[2].kind, crate::ast::MethodKind::Before);
        assert_eq!(class.methods[3].kind, crate::ast::MethodKind::After);
    }

    #[test]
    fn parse_multiple_classes() {
        let module = parse_ok(
            "Actor subclass: Point
  state: x = 0
  state: y = 0

Actor subclass: Rectangle
  state: origin
  state: corner",
        );

        assert_eq!(module.classes.len(), 2);
        assert_eq!(module.classes[0].name.name, "Point");
        assert_eq!(module.classes[1].name.name, "Rectangle");
    }

    // ========================================================================
    // Pratt Parsing Tests
    // ========================================================================

    #[test]
    fn pratt_single_operand_expression() {
        // Single operand without any operators should return just the literal
        let module = parse_ok("42");
        assert_eq!(module.expressions.len(), 1);
        assert!(matches!(
            &module.expressions[0],
            Expression::Literal(Literal::Integer(42), _)
        ));
    }

    #[test]
    fn pratt_unknown_operator_stops_parsing() {
        // Unknown operators like `++` should stop binary parsing
        // This parses as: 1 (then `++` is unknown, stops) `++` `2`
        // The `++` gets parsed as two unary `+` messages on `2`, but since
        // there's no identifier after 1, we just get `1` and then a new expression
        // Actually, `++` is a single BinarySelector token, so this tests that
        // unknown operators don't cause errors
        let (module, diagnostics) = {
            let tokens = crate::parse::lex_with_eof("1 ++ 2");
            crate::parse::parse(tokens)
        };
        // The parser should handle this gracefully
        // It may produce diagnostics but shouldn't panic
        assert!(!module.expressions.is_empty());
        // The behavior is that `++` is an unknown operator, so parsing stops at 1
        // Then `++` and `2` might produce errors or be parsed separately
        // The key is that the parser doesn't crash
        let _ = diagnostics; // We don't assert specific diagnostics
    }

    #[test]
    fn pratt_precedence_mul_over_add() {
        // 1 + 2 * 3 should parse as 1 + (2 * 3)
        let module = parse_ok("1 + 2 * 3");
        assert_eq!(module.expressions.len(), 1);

        if let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Binary(op),
            arguments,
            ..
        } = &module.expressions[0]
        {
            // Top level should be `+`
            assert_eq!(op.as_str(), "+");
            assert!(matches!(
                **receiver,
                Expression::Literal(Literal::Integer(1), _)
            ));

            // Right side should be 2 * 3
            if let Expression::MessageSend {
                receiver: inner_recv,
                selector: MessageSelector::Binary(inner_op),
                arguments: inner_args,
                ..
            } = &arguments[0]
            {
                assert_eq!(inner_op.as_str(), "*");
                assert!(matches!(
                    **inner_recv,
                    Expression::Literal(Literal::Integer(2), _)
                ));
                assert!(matches!(
                    inner_args[0],
                    Expression::Literal(Literal::Integer(3), _)
                ));
            } else {
                panic!("Expected nested MessageSend for 2 * 3");
            }
        } else {
            panic!("Expected MessageSend for 1 + ...");
        }
    }

    #[test]
    fn pratt_left_associativity() {
        // 1 - 2 - 3 should parse as (1 - 2) - 3
        let module = parse_ok("1 - 2 - 3");
        assert_eq!(module.expressions.len(), 1);

        if let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Binary(op),
            arguments,
            ..
        } = &module.expressions[0]
        {
            // Top level should be `-` with `3` on right
            assert_eq!(op.as_str(), "-");
            assert!(matches!(
                arguments[0],
                Expression::Literal(Literal::Integer(3), _)
            ));

            // Left side should be 1 - 2
            if let Expression::MessageSend {
                receiver: inner_recv,
                selector: MessageSelector::Binary(inner_op),
                arguments: inner_args,
                ..
            } = &**receiver
            {
                assert_eq!(inner_op.as_str(), "-");
                assert!(matches!(
                    **inner_recv,
                    Expression::Literal(Literal::Integer(1), _)
                ));
                assert!(matches!(
                    inner_args[0],
                    Expression::Literal(Literal::Integer(2), _)
                ));
            } else {
                panic!("Expected nested MessageSend for 1 - 2");
            }
        } else {
            panic!("Expected MessageSend");
        }
    }

    #[test]
    fn pratt_comparison_lowest_precedence() {
        // 1 + 2 < 3 * 4 should parse as (1 + 2) < (3 * 4)
        let module = parse_ok("1 + 2 < 3 * 4");
        assert_eq!(module.expressions.len(), 1);

        if let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Binary(op),
            arguments,
            ..
        } = &module.expressions[0]
        {
            assert_eq!(op.as_str(), "<");

            // Left side: 1 + 2
            if let Expression::MessageSend {
                selector: MessageSelector::Binary(left_op),
                ..
            } = &**receiver
            {
                assert_eq!(left_op.as_str(), "+");
            } else {
                panic!("Expected 1 + 2 on left");
            }

            // Right side: 3 * 4
            if let Expression::MessageSend {
                selector: MessageSelector::Binary(right_op),
                ..
            } = &arguments[0]
            {
                assert_eq!(right_op.as_str(), "*");
            } else {
                panic!("Expected 3 * 4 on right");
            }
        } else {
            panic!("Expected comparison at top level");
        }
    }

    #[test]
    fn pratt_all_operators() {
        // Test all supported operators parse correctly
        // Using `~=` as the not-equal operator (Smalltalk tradition)
        let expressions = vec![
            "1 = 2", "1 ~= 2", // Smalltalk-style not-equal
            "1 < 2", "1 > 2", "1 <= 2", "1 >= 2", "1 + 2", "1 - 2", "1 * 2", "1 / 2", "1 % 2",
        ];

        for expr in expressions {
            let module = parse_ok(expr);
            assert_eq!(module.expressions.len(), 1, "Failed for: {expr}");
            assert!(
                matches!(&module.expressions[0], Expression::MessageSend { .. }),
                "Expected MessageSend for: {expr}"
            );
        }
    }
}
