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
//! use beamtalk_core::source_analysis::{lex_with_eof, parse};
//!
//! let tokens = lex_with_eof("x := 3 + 4");
//! let (module, diagnostics) = parse(tokens);
//!
//! assert!(diagnostics.is_empty());
//! assert_eq!(module.expressions.len(), 1);
//! ```

use crate::ast::{Comment, CommentKind, Module};
#[cfg(test)]
use crate::ast::{Expression, Literal, MessageSelector};
use crate::source_analysis::{Span, Token, TokenKind, Trivia, lex_with_eof};
use ecow::EcoString;

// Submodules with additional impl blocks for Parser
mod declarations;
mod expressions;

// Re-export helper functions for tests
#[cfg(test)]
use expressions::parse_integer;

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
pub(super) struct BindingPower {
    /// Left binding power (how tightly this operator binds to its left operand).
    pub(super) left: u8,
    /// Right binding power (how tightly this operator binds to its right operand).
    pub(super) right: u8,
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
/// | 5  | `>>`                | Left |
/// | 8  | `->`                | Left |
/// | 10 | `=` `~=`            | Left |
/// | 20 | `<` `>` `<=` `>=`   | Left |
/// | 30 | `+` `-`             | Left |
/// | 40 | `*` `/` `%`         | Left |
///
/// To add a new operator, just add an entry here. For example:
/// ```ignore
/// // Bitwise OR (between comparison and additive)
/// "|" => Some(BindingPower::left_assoc(25)),
///
/// // Exponentiation (right-associative, higher than multiplication)
/// "**" => Some(BindingPower::right_assoc(50)),
/// ```
pub(super) fn binary_binding_power(op: &str) -> Option<BindingPower> {
    match op {
        // Method lookup (lowest binary precedence)
        // `Counter >> #increment` returns CompiledMethod object
        ">>" => Some(BindingPower::left_assoc(5)),

        // Association creation (BT-335)
        // `#name -> 'James'` creates an Association key-value pair
        "->" => Some(BindingPower::left_assoc(8)),

        // Equality
        // `~=` is the Smalltalk-style not-equal operator
        "=" | "==" | "~=" => Some(BindingPower::left_assoc(10)),

        // Comparison
        "<" | ">" | "<=" | ">=" => Some(BindingPower::left_assoc(20)),

        // Additive (includes string concatenation ++)
        "+" | "-" | "++" => Some(BindingPower::left_assoc(30)),

        // Multiplicative (highest binary precedence)
        "*" | "/" | "%" => Some(BindingPower::left_assoc(40)),

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
/// use beamtalk_core::source_analysis::{lex_with_eof, parse};
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

/// Checks whether the given source text appears syntactically complete for REPL
/// evaluation.
///
/// This is a heuristic used by the REPL to decide whether to evaluate the
/// current input buffer or show a continuation prompt for multi-line input.
/// It returns `false` (incomplete) when:
///
/// - Delimiters are unclosed: `[`, `(`, `{`, `#{`, `#(`
/// - A string or block comment is unterminated
/// - The last token is a keyword (`at:`), binary operator (`+`), assignment
///   (`:=`), cascade (`;`), or return (`^`) — all of which expect a following
///   expression
///
/// Extra closing delimiters (e.g., `]` alone) are treated as complete so
/// the evaluator can report the syntax error rather than waiting forever.
///
/// # Examples
///
/// ```
/// use beamtalk_core::source_analysis::is_input_complete;
///
/// assert!(is_input_complete("3 + 4"));
/// assert!(!is_input_complete("[:x | x * 2"));  // unclosed block
/// assert!(!is_input_complete("array at:"));     // keyword missing argument
/// ```
#[must_use]
pub fn is_input_complete(source: &str) -> bool {
    if source.trim().is_empty() {
        return true;
    }

    let tokens = lex_with_eof(source);

    let mut bracket_depth: i32 = 0; // [ ]
    let mut paren_depth: i32 = 0; // ( )
    let mut brace_depth: i32 = 0; // { }
    let mut last_meaningful_kind: Option<&TokenKind> = None;

    for token in &tokens {
        let kind = token.kind();

        // Check for unterminated block comments in the EOF token's leading trivia
        if kind.is_eof() {
            for trivia in token.leading_trivia() {
                if let Trivia::BlockComment(text) = trivia {
                    if !text.ends_with("*/") {
                        return false;
                    }
                }
            }
        }

        match kind {
            // Error tokens indicate unterminated strings or other lexer failures
            TokenKind::Error(_) => return false,

            // Opening delimiters
            TokenKind::LeftBracket => bracket_depth += 1,
            TokenKind::LeftParen | TokenKind::ListOpen => paren_depth += 1,
            TokenKind::LeftBrace | TokenKind::MapOpen => brace_depth += 1,

            // Closing delimiters
            TokenKind::RightBracket => bracket_depth -= 1,
            TokenKind::RightParen => paren_depth -= 1,
            TokenKind::RightBrace => brace_depth -= 1,

            TokenKind::Eof => break,
            _ => {}
        }

        if !kind.is_eof() {
            last_meaningful_kind = Some(kind);
        }
    }

    // Unclosed delimiters
    if bracket_depth > 0 || paren_depth > 0 || brace_depth > 0 {
        return false;
    }

    // Trailing keyword missing its argument (e.g., "array at:" or "x ifTrue:")
    if let Some(TokenKind::Keyword(_)) = last_meaningful_kind {
        return false;
    }

    // Trailing binary operator missing its right operand (e.g., "1 +" or "3 *")
    if let Some(TokenKind::BinarySelector(_)) = last_meaningful_kind {
        return false;
    }

    // Trailing assignment missing its value (e.g., "x :=")
    if let Some(TokenKind::Assign) = last_meaningful_kind {
        return false;
    }

    // Trailing cascade separator or return operator (e.g., "x foo;" or "^")
    if matches!(
        last_meaningful_kind,
        Some(TokenKind::Semicolon | TokenKind::Caret)
    ) {
        return false;
    }

    true
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
    /// Optional hint for how to fix the issue.
    pub hint: Option<EcoString>,
}

impl Diagnostic {
    /// Creates a new error diagnostic.
    #[must_use]
    pub fn error(message: impl Into<EcoString>, span: Span) -> Self {
        Self {
            severity: Severity::Error,
            message: message.into(),
            span,
            hint: None,
        }
    }

    /// Creates a new warning diagnostic.
    #[must_use]
    pub fn warning(message: impl Into<EcoString>, span: Span) -> Self {
        Self {
            severity: Severity::Warning,
            message: message.into(),
            span,
            hint: None,
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
pub(super) struct Parser {
    /// The tokens being parsed.
    pub(super) tokens: Vec<Token>,
    /// Current token index.
    pub(super) current: usize,
    /// Accumulated diagnostics.
    pub(super) diagnostics: Vec<Diagnostic>,
    /// Whether the parser is currently inside a method body.
    pub(super) in_method_body: bool,
}

impl Parser {
    /// Creates a new parser for the given tokens.
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            diagnostics: Vec::new(),
            in_method_body: false,
        }
    }

    // ========================================================================
    // Token Management
    // ========================================================================

    /// Returns the current token.
    pub(super) fn current_token(&self) -> &Token {
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
    pub(super) fn current_kind(&self) -> &TokenKind {
        self.current_token().kind()
    }

    /// Peeks at the next token without consuming.
    pub(super) fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current + 1)
    }

    /// Peeks at the next token kind.
    pub(super) fn peek_kind(&self) -> Option<&TokenKind> {
        self.peek().map(Token::kind)
    }

    /// Checks if we're at the end of input.
    pub(super) fn is_at_end(&self) -> bool {
        matches!(self.current_kind(), TokenKind::Eof)
    }

    /// Advances to the next token and returns the previous one.
    pub(super) fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.tokens[self.current - 1].clone()
    }

    /// Checks if the current token matches the given kind.
    pub(super) fn check(&self, kind: &TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }
        std::mem::discriminant(self.current_kind()) == std::mem::discriminant(kind)
    }

    /// Consumes the current token if it matches the given kind.
    pub(super) fn match_token(&mut self, kind: &TokenKind) -> bool {
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
    pub(super) fn expect(&mut self, kind: &TokenKind, message: &str) -> Option<Token> {
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

    /// Extracts a doc comment from the current token's leading trivia.
    ///
    /// Collects consecutive `///` lines immediately preceding the token,
    /// strips the `/// ` or `///` prefix, and joins them with newlines.
    /// A regular comment (`//`) or blank line between doc lines resets
    /// the collection, so only the last consecutive block of `///` lines
    /// is returned.
    pub(super) fn collect_doc_comment(&self) -> Option<String> {
        let mut lines = Vec::new();
        for trivia in self.current_token().leading_trivia() {
            match trivia {
                super::Trivia::DocComment(text) => {
                    let text = text.as_str();
                    // Strip `/// ` (with space) or `///` prefix
                    let stripped = text
                        .strip_prefix("/// ")
                        .unwrap_or_else(|| text.strip_prefix("///").unwrap_or(text));
                    lines.push(stripped.to_string());
                }
                super::Trivia::Whitespace(ws) => {
                    // A blank line (>1 newline) breaks consecutive doc comments
                    if ws.chars().filter(|&c| c == '\n').count() > 1 {
                        lines.clear();
                    }
                }
                _ => lines.clear(), // non-doc comment resets collection
            }
        }
        if lines.is_empty() {
            None
        } else {
            Some(lines.join("\n"))
        }
    }

    /// Reports an error at the current token.
    pub(super) fn error(&mut self, message: impl Into<EcoString>) {
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
    /// - Newline (when inside method body) - implicit statement separator (BT-360)
    pub(super) fn synchronize(&mut self) {
        // BT-368: If we're already at a newline boundary in a method body, don't
        // advance past the first token of the next statement. parse_expression()
        // typically consumes the bad token via advance() in parse_primary, so
        // current may already point to the next statement's first token.
        if !self.is_at_end() && self.in_method_body && self.current_token().has_leading_newline() {
            return;
        }

        self.advance();

        while !self.is_at_end() {
            if self.at_recovery_point() {
                return;
            }

            self.advance();
        }
    }

    /// Returns true if the current token is at a recovery point.
    fn at_recovery_point(&self) -> bool {
        if matches!(
            self.current_kind(),
            TokenKind::Period
                | TokenKind::RightBracket
                | TokenKind::RightParen
                | TokenKind::RightBrace
                | TokenKind::Semicolon
        ) {
            return true;
        }

        if matches!(self.current_kind(), TokenKind::Caret) {
            return true;
        }

        // BT-368: In method bodies, newlines act as implicit statement separators (BT-360)
        // so we can recover at newline boundaries
        if self.in_method_body && self.current_token().has_leading_newline() {
            return true;
        }

        false
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
                        super::Trivia::LineComment(_) | super::Trivia::DocComment(_) => {
                            CommentKind::Line
                        }
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
    pub(super) fn is_at_class_definition(&self) -> bool {
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
    pub(super) fn peek_at(&self, offset: usize) -> Option<&TokenKind> {
        self.tokens.get(self.current + offset).map(Token::kind)
    }

    /// Checks if the current token is a specific binary selector, and consumes it if so.
    pub(super) fn match_binary_selector(&mut self, expected: &str) -> bool {
        if let TokenKind::BinarySelector(s) = self.current_kind() {
            if s.as_str() == expected {
                self.advance();
                return true;
            }
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::MethodKind;
    use crate::source_analysis::lex_with_eof;

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
    fn parse_top_level_error_recovery_unchanged() {
        // BT-368: Top-level parsing currently treats newlines as statement separators.
        // This test ensures that error recovery on the first statement does not
        // regress that behaviour and that we still parse subsequent top-level
        // expressions after a newline.
        let source = "x := @\ny := 3";
        let tokens = lex_with_eof(source);
        let (module, diagnostics) = parse(tokens);

        // Should have exactly one error diagnostic from the invalid '@' token
        assert_eq!(diagnostics.len(), 1, "Expected one error diagnostic");

        // The parser should recover at the newline and continue parsing the
        // second top-level expression (we rely on this behaviour elsewhere,
        // e.g. in `parse_multiple_map_assignments`).
        assert_eq!(
            module.expressions.len(),
            2,
            "Expected both top-level expressions to be parsed despite the first containing an error"
        );
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
    fn parse_block_newline_separated_statements() {
        // BT-360: newlines act as implicit statement separators
        let module = parse_ok("[\n  Transcript show: 'a'\n  Transcript show: 'b'\n  42\n]");
        match &module.expressions[0] {
            Expression::Block(block) => {
                assert_eq!(
                    block.body.len(),
                    3,
                    "Block should have 3 newline-separated statements"
                );
            }
            _ => panic!("Expected block"),
        }
    }

    #[test]
    fn parse_block_mixed_period_and_newline_separators() {
        // BT-360: periods and newlines can be mixed
        let module = parse_ok("[\n  1 + 2.\n  3 + 4\n  5\n]");
        match &module.expressions[0] {
            Expression::Block(block) => {
                assert_eq!(
                    block.body.len(),
                    3,
                    "Block should have 3 statements (mixed separators)"
                );
            }
            _ => panic!("Expected block"),
        }
    }

    #[test]
    fn parse_block_error_recovery_with_newlines() {
        // BT-368: Blocks should continue parsing after errors (implicit newline separation)
        let source = "[\n  x := 1\n  y := @\n  z := 3\n]";
        let tokens = lex_with_eof(source);
        let (module, diagnostics) = parse(tokens);

        // Should have one diagnostic for the error
        assert_eq!(diagnostics.len(), 1, "Expected one error diagnostic");

        // Should still parse the block with all three statements
        assert_eq!(module.expressions.len(), 1, "Should parse the block");
        match &module.expressions[0] {
            Expression::Block(block) => {
                assert_eq!(
                    block.body.len(),
                    3,
                    "Block should have 3 statements (including error)"
                );

                // First statement should be valid
                assert!(!block.body[0].is_error(), "First statement should be valid");

                // Second statement is assignment with error value
                assert!(
                    matches!(&block.body[1], Expression::Assignment { value, .. } if value.is_error()),
                    "Second statement should be assignment with error value"
                );

                // Third statement should be parsed after error
                assert!(
                    !block.body[2].is_error(),
                    "Third statement should be valid (blocks don't break on errors)"
                );
            }
            _ => panic!("Expected block"),
        }
    }

    #[test]
    fn parse_method_body_newline_separated() {
        // BT-360: method bodies parse multiple newline-separated statements
        let module = parse_ok(
            "Object subclass: Chatty\n\n  greet =>\n    Transcript show: 'Hello'\n    Transcript show: 'World'\n    42",
        );
        assert_eq!(module.classes.len(), 1);
        let method = &module.classes[0].methods[0];
        assert_eq!(method.selector.name(), "greet");
        assert_eq!(
            method.body.len(),
            3,
            "Method should have 3 newline-separated statements"
        );
    }

    #[test]
    fn parse_method_body_period_still_works() {
        // BT-360: explicit periods still work (backward compat)
        let module = parse_ok("Object subclass: Test\n\n  go =>\n    1 + 2.\n    3 + 4.\n    5");
        let method = &module.classes[0].methods[0];
        assert_eq!(
            method.body.len(),
            3,
            "Method should have 3 period-separated statements"
        );
    }

    #[test]
    fn parse_method_body_error_recovery_with_newlines() {
        // BT-368: Parser should recover at newline boundaries after errors in method bodies
        let source = "Object subclass: Test\n\n  methodOne =>\n    x := 1\n    y := @\n    z := 3";
        let tokens = lex_with_eof(source);
        let (module, diagnostics) = parse(tokens);

        // Should have one diagnostic for the error
        assert_eq!(diagnostics.len(), 1, "Expected one error diagnostic");
        assert!(
            diagnostics[0].message.contains("Unexpected token"),
            "Expected error about unexpected token"
        );

        // Should still parse the class and method
        assert_eq!(module.classes.len(), 1, "Should parse the class");
        let class = &module.classes[0];
        assert_eq!(class.methods.len(), 1, "Should parse the method");

        // Method should have 3 statements: x := 1, y := @ (error), z := 3
        let method = &class.methods[0];
        assert_eq!(
            method.body.len(),
            3,
            "Method should have 3 statements (including error)"
        );

        // First statement should be valid
        assert!(
            !method.body[0].is_error(),
            "First statement should be valid"
        );

        // Second statement should be an error assignment
        assert!(
            matches!(&method.body[1], Expression::Assignment { value, .. } if value.is_error()),
            "Second statement should be assignment with error value"
        );

        // Third statement should be parsed after recovery
        assert!(
            !method.body[2].is_error(),
            "Third statement should be valid after recovery"
        );
    }

    #[test]
    fn parse_method_body_error_recovery_multiple_methods() {
        // BT-368: Parser should not skip following methods after error recovery
        let source =
            "Object subclass: Test\n\n  methodOne =>\n    x := @\n\n  methodTwo =>\n    y := 42";
        let tokens = lex_with_eof(source);
        let (module, diagnostics) = parse(tokens);

        // Should have one diagnostic for the error
        assert_eq!(diagnostics.len(), 1, "Expected one error diagnostic");

        // Should parse both methods
        assert_eq!(module.classes.len(), 1, "Should parse the class");
        let class = &module.classes[0];
        assert_eq!(
            class.methods.len(),
            2,
            "Should parse both methods despite error in first method"
        );

        // First method should have error statement
        assert_eq!(
            class.methods[0].body.len(),
            1,
            "First method should have 1 statement"
        );

        // Second method should be valid
        assert_eq!(
            class.methods[1].body.len(),
            1,
            "Second method should have 1 statement"
        );
        assert!(
            !class.methods[1].body[0].is_error(),
            "Second method statement should be valid"
        );
    }

    #[test]
    fn parse_method_body_bare_error_recovery_at_newline() {
        // BT-368: A bare error token (not in an assignment) should not cause
        // synchronize() to skip the first token of the next statement.
        // This tests the fix where synchronize() checks recovery points
        // before the initial advance().
        let source = "Object subclass: Test\n\n  go =>\n    x := 1\n    #\n    z := 3";
        let tokens = lex_with_eof(source);
        let (module, diagnostics) = parse(tokens);

        // Should have error diagnostic for the bare '#'
        assert!(!diagnostics.is_empty(), "Expected error diagnostics");

        // Should still parse the class and method
        assert_eq!(module.classes.len(), 1, "Should parse the class");
        let class = &module.classes[0];
        assert_eq!(class.methods.len(), 1, "Should parse the method");

        // Method should recover and parse z := 3 after the error
        let method = &class.methods[0];
        assert!(
            method.body.len() >= 3,
            "Method should have at least 3 statements: x := 1, error, z := 3 (got {})",
            method.body.len()
        );

        // First statement should be valid (x := 1)
        assert!(
            !method.body[0].is_error(),
            "First statement should be valid"
        );

        // Last statement should be valid (z := 3) — recovered after error
        assert!(
            !method.body[method.body.len() - 1].is_error(),
            "Last statement should be valid after recovery"
        );
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

  increment => self.value := self.value + 1
  getValue => ^self.value",
        );

        assert_eq!(module.classes.len(), 1);
        let class = &module.classes[0];

        assert_eq!(class.name.name, "Counter");
        assert_eq!(class.superclass.as_ref().unwrap().name, "Actor");
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
        assert_eq!(class.superclass.as_ref().unwrap().name, "Actor");
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
    fn parse_root_class_nil_superclass() {
        let module = parse_ok(
            "abstract nil subclass: ProtoObject
  class => @primitive classOf",
        );

        assert_eq!(module.classes.len(), 1);
        let class = &module.classes[0];

        assert!(class.is_abstract);
        assert!(class.superclass.is_none());
        assert_eq!(class.name.name, "ProtoObject");
        assert_eq!(class.superclass_name(), "none");
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

  increment => self.value := self.value + 1
  decrement => self.value := self.value - 1

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
            let tokens = crate::source_analysis::lex_with_eof("1 ++ 2");
            crate::source_analysis::parse(tokens)
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

    #[test]
    fn parse_super_unary_message() {
        let module = parse_ok("super increment");
        assert_eq!(module.expressions.len(), 1);

        if let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Unary(name),
            arguments,
            ..
        } = &module.expressions[0]
        {
            assert!(matches!(&**receiver, Expression::Super(_)));
            assert_eq!(name.as_str(), "increment");
            assert_eq!(arguments.len(), 0);
        } else {
            panic!("Expected super message send");
        }
    }

    #[test]
    fn parse_super_keyword_message() {
        let module = parse_ok("super at: 1 put: value");
        assert_eq!(module.expressions.len(), 1);

        if let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = &module.expressions[0]
        {
            assert!(matches!(&**receiver, Expression::Super(_)));
            assert_eq!(parts.len(), 2);
            assert_eq!(parts[0].keyword.as_str(), "at:");
            assert_eq!(parts[1].keyword.as_str(), "put:");
            assert_eq!(arguments.len(), 2);
        } else {
            panic!("Expected super keyword message send");
        }
    }

    #[test]
    fn parse_super_in_method_body() {
        // Test super in a method body
        let module = parse_ok(
            "Actor subclass: Counter
  increment => super increment",
        );

        assert_eq!(module.classes.len(), 1);
        let class = &module.classes[0];
        assert_eq!(class.methods.len(), 1);

        let method = &class.methods[0];
        assert_eq!(method.body.len(), 1, "Method should have 1 statement");

        // Statement should be super increment
        if let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Unary(name),
            ..
        } = &method.body[0]
        {
            assert!(matches!(&**receiver, Expression::Super(_)));
            assert_eq!(name.as_str(), "increment");
        } else {
            panic!("Expected super message send in method body");
        }
    }

    #[test]
    fn parse_super_field_access_is_allowed() {
        // Test that super.field is currently allowed by the parser
        // (This is a known limitation - super should only be used with message sends)
        let module = parse_ok("super.value");
        assert_eq!(module.expressions.len(), 1);

        // Currently parses as FieldAccess with Super receiver
        // This is technically invalid semantically but parser allows it
        if let Expression::FieldAccess { receiver, .. } = &module.expressions[0] {
            assert!(
                matches!(&**receiver, Expression::Super(_)),
                "Parser allows super.field (codegen will reject it)"
            );
        } else {
            panic!("Expected field access with super receiver");
        }
    }

    #[test]
    fn parse_super_with_cascade() {
        // Test that super with cascade parses correctly
        // Note: Codegen currently rejects this (unclear semantics), but parser allows it
        let module = parse_ok("super increment; getValue");
        assert_eq!(module.expressions.len(), 1);

        if let Expression::Cascade {
            receiver, messages, ..
        } = &module.expressions[0]
        {
            // The receiver should be a MessageSend with Super
            if let Expression::MessageSend {
                receiver: inner_receiver,
                selector: MessageSelector::Unary(name),
                ..
            } = &**receiver
            {
                assert!(matches!(&**inner_receiver, Expression::Super(_)));
                assert_eq!(name.as_str(), "increment");
            } else {
                panic!("Expected super increment as cascade receiver");
            }

            assert_eq!(messages.len(), 1);
            assert_eq!(messages[0].selector.name(), "getValue");
        } else {
            panic!("Expected cascade expression");
        }
    }

    // ========================================================================
    // Class Reference Tests
    // ========================================================================

    #[test]
    fn parse_class_reference_spawn() {
        let module = parse_ok("Counter spawn");
        assert_eq!(module.expressions.len(), 1);

        if let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Unary(name),
            arguments,
            ..
        } = &module.expressions[0]
        {
            // Receiver should be ClassReference, not Identifier
            if let Expression::ClassReference {
                name: class_name, ..
            } = &**receiver
            {
                assert_eq!(class_name.name.as_str(), "Counter");
            } else {
                panic!("Expected ClassReference receiver, got {receiver:?}");
            }
            assert_eq!(name.as_str(), "spawn");
            assert_eq!(arguments.len(), 0);
        } else {
            panic!("Expected message send");
        }
    }

    #[test]
    fn parse_class_reference_vs_variable() {
        // Uppercase should parse as ClassReference
        let module1 = parse_ok("Counter");
        if let Expression::ClassReference { name, .. } = &module1.expressions[0] {
            assert_eq!(name.name.as_str(), "Counter");
        } else {
            panic!("Expected ClassReference for 'Counter'");
        }

        // Lowercase should parse as Identifier
        let module2 = parse_ok("counter");
        if let Expression::Identifier(id) = &module2.expressions[0] {
            assert_eq!(id.name.as_str(), "counter");
        } else {
            panic!("Expected Identifier for 'counter'");
        }
    }

    #[test]
    fn parse_class_reference_keyword_message() {
        let module = parse_ok("Counter spawnWith: initialState");
        assert_eq!(module.expressions.len(), 1);

        if let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = &module.expressions[0]
        {
            // Receiver should be ClassReference
            if let Expression::ClassReference {
                name: class_name, ..
            } = &**receiver
            {
                assert_eq!(class_name.name.as_str(), "Counter");
            } else {
                panic!("Expected ClassReference receiver");
            }
            assert_eq!(parts.len(), 1);
            assert_eq!(parts[0].keyword.as_str(), "spawnWith:");
            assert_eq!(arguments.len(), 1);
        } else {
            panic!("Expected keyword message send");
        }
    }

    #[test]
    fn parse_mixed_case_identifier() {
        // camelCase should parse as Identifier (starts with lowercase)
        let module = parse_ok("myVariable");
        if let Expression::Identifier(id) = &module.expressions[0] {
            assert_eq!(id.name.as_str(), "myVariable");
        } else {
            panic!("Expected Identifier for 'myVariable'");
        }

        // PascalCase should parse as ClassReference (starts with uppercase)
        let module = parse_ok("MyClass");
        if let Expression::ClassReference { name, .. } = &module.expressions[0] {
            assert_eq!(name.name.as_str(), "MyClass");
        } else {
            panic!("Expected ClassReference for 'MyClass'");
        }
    }

    // ========================================================================
    // @primitive parsing tests (BT-290)
    // ========================================================================

    #[test]
    fn parse_primitive_quoted_selector() {
        let source = "Object subclass: Foo\n  + other => @primitive '+'";
        let module = parse_ok(source);
        assert_eq!(module.classes.len(), 1);
        let method = &module.classes[0].methods[0];
        assert_eq!(method.body.len(), 1);
        if let Expression::Primitive {
            name, is_quoted, ..
        } = &method.body[0]
        {
            assert_eq!(name.as_str(), "+");
            assert!(is_quoted);
        } else {
            panic!("Expected Primitive, got: {:?}", method.body[0]);
        }
    }

    #[test]
    fn parse_primitive_bare_identifier() {
        let source = "Object subclass: Foo\n  new => @primitive basicNew";
        let module = parse_ok(source);
        assert_eq!(module.classes.len(), 1);
        let method = &module.classes[0].methods[0];
        assert_eq!(method.body.len(), 1);
        if let Expression::Primitive {
            name, is_quoted, ..
        } = &method.body[0]
        {
            assert_eq!(name.as_str(), "basicNew");
            assert!(!is_quoted);
        } else {
            panic!("Expected Primitive, got: {:?}", method.body[0]);
        }
    }

    #[test]
    fn parse_primitive_in_method_body() {
        // @primitive as first expression in a method body
        let source = "Object subclass: MyInt\n  + other => @primitive '+'";
        let module = parse_ok(source);
        assert_eq!(module.classes.len(), 1);
        let method = &module.classes[0].methods[0];
        assert_eq!(method.body.len(), 1);
        assert!(matches!(
            &method.body[0],
            Expression::Primitive {
                is_quoted: true,
                ..
            }
        ));
    }

    #[test]
    fn parse_primitive_with_fallback() {
        // @primitive followed by fallback code in method body
        let source = "Object subclass: MyInt\n  abs => @primitive 'abs'. self negated";
        let module = parse_ok(source);
        let method = &module.classes[0].methods[0];
        assert_eq!(method.body.len(), 2);
        assert!(matches!(&method.body[0], Expression::Primitive { .. }));
    }

    #[test]
    fn parse_primitive_structural_intrinsic() {
        let source = "Object subclass: MyObj\n  new => @primitive basicNew";
        let module = parse_ok(source);
        let method = &module.classes[0].methods[0];
        assert_eq!(method.body.len(), 1);
        if let Expression::Primitive {
            name, is_quoted, ..
        } = &method.body[0]
        {
            assert_eq!(name.as_str(), "basicNew");
            assert!(!is_quoted);
        } else {
            panic!("Expected Primitive");
        }
    }

    #[test]
    fn parse_primitive_missing_name_error() {
        // @primitive without a name should produce an error
        let source = "Object subclass: Foo\n  abs => @primitive";
        let diagnostics = parse_err(source);
        assert!(
            !diagnostics.is_empty(),
            "Expected error for @primitive without name"
        );
        assert!(
            diagnostics[0]
                .message
                .contains("@primitive must be followed by")
        );
    }

    #[test]
    fn parse_primitive_outside_method_body_error() {
        // @primitive at top level (outside method body) should produce an error
        let diagnostics = parse_err("@primitive '+'");
        assert!(
            !diagnostics.is_empty(),
            "Expected error for @primitive outside method body"
        );
        assert!(
            diagnostics[0]
                .message
                .contains("@primitive can only appear inside a method body"),
            "Got: {}",
            diagnostics[0].message
        );
    }

    #[test]
    fn parse_primitive_inside_block_in_method_body() {
        // @primitive inside a block within a method body should still be accepted
        let source = "Object subclass: Foo\n  m => [@primitive '+']";
        let module = parse_ok(source);
        let method = &module.classes[0].methods[0];
        assert_eq!(method.body.len(), 1);
        // The body is a block containing the primitive
        if let Expression::Block(block) = &method.body[0] {
            assert_eq!(block.body.len(), 1);
            assert!(
                matches!(&block.body[0], Expression::Primitive { .. }),
                "Expected Primitive inside block, got: {:?}",
                block.body[0]
            );
        } else {
            panic!("Expected Block, got: {:?}", method.body[0]);
        }
    }

    // @intrinsic parsing tests (BT-484)
    // ========================================================================

    #[test]
    fn parse_intrinsic_bare_identifier() {
        // @intrinsic with bare identifier produces same AST as @primitive
        let source = "Object subclass: Foo\n  blockValue => @intrinsic blockValue";
        let module = parse_ok(source);
        let method = &module.classes[0].methods[0];
        assert_eq!(method.body.len(), 1);
        match &method.body[0] {
            Expression::Primitive {
                name, is_quoted, ..
            } => {
                assert_eq!(name.as_ref(), "blockValue");
                assert!(!is_quoted, "bare @intrinsic should not be quoted");
            }
            other => panic!("Expected Primitive, got: {other:?}"),
        }
    }

    #[test]
    fn parse_intrinsic_quoted_selector() {
        // @intrinsic with quoted selector produces same AST as @primitive
        let source = "Object subclass: Foo\n  size => @intrinsic 'size'";
        let module = parse_ok(source);
        let method = &module.classes[0].methods[0];
        assert_eq!(method.body.len(), 1);
        match &method.body[0] {
            Expression::Primitive {
                name, is_quoted, ..
            } => {
                assert_eq!(name.as_ref(), "size");
                assert!(*is_quoted, "quoted @intrinsic should be quoted");
            }
            other => panic!("Expected Primitive, got: {other:?}"),
        }
    }

    #[test]
    fn parse_intrinsic_outside_method_body_error() {
        let diagnostics = parse_err("@intrinsic blockValue");
        assert!(
            !diagnostics.is_empty(),
            "Expected error for @intrinsic outside method body"
        );
        assert!(
            diagnostics[0]
                .message
                .contains("@intrinsic can only appear inside a method body"),
            "Got: {}",
            diagnostics[0].message
        );
    }

    // BT-285: Consecutive binary method definitions
    #[test]
    fn parse_consecutive_binary_methods() {
        let source = "Object subclass: Foo\n  + other => @primitive '+'\n  - other => @primitive '-'\n  * other => @primitive '*'";
        let module = parse_ok(source);
        assert_eq!(module.classes[0].methods.len(), 3);
        assert_eq!(module.classes[0].methods[0].selector.name(), "+");
        assert_eq!(module.classes[0].methods[1].selector.name(), "-");
        assert_eq!(module.classes[0].methods[2].selector.name(), "*");
    }

    #[test]
    fn parse_binary_methods_followed_by_unary() {
        let source = "Object subclass: Foo\n  + other => @primitive '+'\n  - other => @primitive '-'\n  negated => 0";
        let module = parse_ok(source);
        assert_eq!(module.classes[0].methods.len(), 3);
        assert_eq!(module.classes[0].methods[0].selector.name(), "+");
        assert_eq!(module.classes[0].methods[1].selector.name(), "-");
        assert_eq!(module.classes[0].methods[2].selector.name(), "negated");
    }

    #[test]
    fn parse_binary_continuation_on_same_line() {
        // Binary operator on same line should still work as expression continuation
        let source = "Object subclass: Foo\n  m => 1 + 2 - 3";
        let module = parse_ok(source);
        assert_eq!(module.classes[0].methods.len(), 1);
        assert_eq!(module.classes[0].methods[0].selector.name(), "m");
    }

    #[test]
    fn parse_binary_continuation_on_new_line() {
        // Binary operator on new line that is NOT a method definition should
        // continue the expression (regression test for BT-285)
        let source = "Object subclass: Foo\n  m => 1\n    + 2\n  n => 3";
        let module = parse_ok(source);
        assert_eq!(module.classes[0].methods.len(), 2);
        assert_eq!(module.classes[0].methods[0].selector.name(), "m");
        assert_eq!(module.classes[0].methods[1].selector.name(), "n");
    }

    // ========================================================================
    // List Literal Tests (BT-402)
    // ========================================================================

    #[test]
    fn parse_empty_list() {
        let module = parse_ok("#()");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::ListLiteral { elements, tail, .. } => {
                assert!(elements.is_empty());
                assert!(tail.is_none());
            }
            other => panic!("Expected list literal, got: {other:?}"),
        }
    }

    #[test]
    fn parse_single_element_list() {
        let module = parse_ok("#(42)");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::ListLiteral { elements, tail, .. } => {
                assert_eq!(elements.len(), 1);
                assert!(tail.is_none());
            }
            other => panic!("Expected list literal, got: {other:?}"),
        }
    }

    #[test]
    fn parse_multi_element_list() {
        let module = parse_ok("#(1, 2, 3)");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::ListLiteral { elements, tail, .. } => {
                assert_eq!(elements.len(), 3);
                assert!(tail.is_none());
            }
            other => panic!("Expected list literal, got: {other:?}"),
        }
    }

    #[test]
    fn parse_list_trailing_comma() {
        let module = parse_ok("#(1, 2, 3,)");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::ListLiteral { elements, tail, .. } => {
                assert_eq!(elements.len(), 3);
                assert!(tail.is_none());
            }
            other => panic!("Expected list literal, got: {other:?}"),
        }
    }

    #[test]
    fn parse_list_cons_syntax() {
        let module = parse_ok("#(0 | rest)");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::ListLiteral { elements, tail, .. } => {
                assert_eq!(elements.len(), 1);
                assert!(tail.is_some());
            }
            other => panic!("Expected list literal with cons, got: {other:?}"),
        }
    }

    #[test]
    fn parse_nested_list() {
        let module = parse_ok("#(#(1, 2), #(3, 4))");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::ListLiteral { elements, tail, .. } => {
                assert_eq!(elements.len(), 2);
                assert!(tail.is_none());
                // Each element should be a list literal
                assert!(matches!(&elements[0], Expression::ListLiteral { .. }));
                assert!(matches!(&elements[1], Expression::ListLiteral { .. }));
            }
            other => panic!("Expected nested list literal, got: {other:?}"),
        }
    }

    #[test]
    fn parse_list_with_mixed_types() {
        let module = parse_ok("#(1, 'hello', #ok)");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::ListLiteral { elements, tail, .. } => {
                assert_eq!(elements.len(), 3);
                assert!(tail.is_none());
            }
            other => panic!("Expected list literal, got: {other:?}"),
        }
    }

    #[test]
    fn parse_list_as_message_receiver() {
        // List literal as receiver of a message
        let module = parse_ok("#(1, 2, 3) size");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::MessageSend {
                receiver, selector, ..
            } => {
                assert!(matches!(receiver.as_ref(), Expression::ListLiteral { .. }));
                assert_eq!(selector.name(), "size");
            }
            other => panic!("Expected message send to list, got: {other:?}"),
        }
    }

    #[test]
    fn parse_list_multi_cons() {
        // Multiple elements before cons: #(1, 2 | rest) → [1, 2 | rest]
        let module = parse_ok("#(1, 2 | rest)");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0] {
            Expression::ListLiteral { elements, tail, .. } => {
                assert_eq!(elements.len(), 2);
                assert!(tail.is_some());
            }
            other => panic!("Expected list literal with cons, got: {other:?}"),
        }
    }

    #[test]
    fn parse_list_error_unterminated() {
        // Missing closing paren produces diagnostic
        let diagnostics = parse_err("#(1, 2");
        assert!(
            !diagnostics.is_empty(),
            "Expected error for unterminated list"
        );
    }

    #[test]
    fn parse_doc_comment_on_class() {
        let module = parse_ok(
            "/// A simple counter actor.
/// Maintains a count that can be incremented.
Actor subclass: Counter
  state: value = 0
  increment => self.value := self.value + 1",
        );

        assert_eq!(module.classes.len(), 1);
        let class = &module.classes[0];
        assert_eq!(class.name.name, "Counter");
        assert_eq!(
            class.doc_comment.as_deref(),
            Some("A simple counter actor.\nMaintains a count that can be incremented.")
        );
    }

    #[test]
    fn parse_doc_comment_on_method() {
        let module = parse_ok(
            "Actor subclass: Counter
  state: value = 0

  /// Increments the counter by one.
  increment => self.value := self.value + 1",
        );

        assert_eq!(module.classes.len(), 1);
        let class = &module.classes[0];
        assert_eq!(class.methods.len(), 1);
        assert_eq!(
            class.methods[0].doc_comment.as_deref(),
            Some("Increments the counter by one.")
        );
    }

    #[test]
    fn parse_no_doc_comment_with_regular_comment() {
        let module = parse_ok(
            "// This is a regular comment
Actor subclass: Counter
  state: value = 0
  increment => self.value := self.value + 1",
        );

        assert_eq!(module.classes.len(), 1);
        assert!(module.classes[0].doc_comment.is_none());
    }

    #[test]
    fn parse_four_slashes_not_doc_comment() {
        let module = parse_ok(
            "//// This is NOT a doc comment
Actor subclass: Counter
  state: value = 0
  increment => self.value := self.value + 1",
        );

        assert_eq!(module.classes.len(), 1);
        assert!(module.classes[0].doc_comment.is_none());
    }

    #[test]
    fn parse_doc_comment_strips_prefix() {
        let module = parse_ok(
            "///No space after slashes
Actor subclass: Counter
  increment => 1",
        );

        assert_eq!(module.classes.len(), 1);
        assert_eq!(
            module.classes[0].doc_comment.as_deref(),
            Some("No space after slashes")
        );
    }

    #[test]
    fn parse_doc_comment_on_class_and_method() {
        let module = parse_ok(
            "/// Class doc.
Actor subclass: Counter
  state: value = 0

  /// Method doc.
  increment => self.value := self.value + 1",
        );

        assert_eq!(module.classes.len(), 1);
        let class = &module.classes[0];
        assert_eq!(class.doc_comment.as_deref(), Some("Class doc."));
        assert_eq!(class.methods.len(), 1);
        assert_eq!(class.methods[0].doc_comment.as_deref(), Some("Method doc."));
    }

    #[test]
    fn parse_doc_comment_resets_on_regular_comment() {
        let module = parse_ok(
            "/// Orphaned doc comment.
// Regular comment interrupts.
/// Actual class doc.
Actor subclass: Counter
  increment => 1",
        );

        assert_eq!(module.classes.len(), 1);
        // Only the last consecutive block should be collected
        assert_eq!(
            module.classes[0].doc_comment.as_deref(),
            Some("Actual class doc.")
        );
    }

    #[test]
    fn parse_doc_comment_with_abstract_modifier() {
        let module = parse_ok(
            "/// An abstract collection.
abstract Actor subclass: Collection
  size => self subclassResponsibility",
        );

        assert_eq!(module.classes.len(), 1);
        assert!(module.classes[0].is_abstract);
        assert_eq!(
            module.classes[0].doc_comment.as_deref(),
            Some("An abstract collection.")
        );
    }

    #[test]
    fn parse_doc_comment_blank_line_resets() {
        let module = parse_ok(
            "/// Orphaned doc comment.

/// Actual class doc.
Actor subclass: Counter
  increment => 1",
        );

        assert_eq!(module.classes.len(), 1);
        // Blank line separates the two doc blocks; only the last is attached
        assert_eq!(
            module.classes[0].doc_comment.as_deref(),
            Some("Actual class doc.")
        );
    }

    #[test]
    fn parse_doc_comment_with_sealed_modifier() {
        let module = parse_ok(
            "/// A sealed value type.
sealed Object subclass: Point
  state: x = 0
  state: y = 0
  x => self.x",
        );

        assert_eq!(module.classes.len(), 1);
        assert!(module.classes[0].is_sealed);
        assert_eq!(
            module.classes[0].doc_comment.as_deref(),
            Some("A sealed value type.")
        );
    }

    #[test]
    fn parse_doc_comment_on_before_method() {
        let module = parse_ok(
            "Actor subclass: Counter
  state: value = 0

  /// Logging before increment.
  before increment => Transcript show: 'incrementing'",
        );

        assert_eq!(module.classes.len(), 1);
        let method = &module.classes[0].methods[0];
        assert_eq!(method.kind, MethodKind::Before);
        assert_eq!(
            method.doc_comment.as_deref(),
            Some("Logging before increment.")
        );
    }

    #[test]
    fn parse_doc_comment_on_after_method() {
        let module = parse_ok(
            "Actor subclass: Counter
  state: value = 0

  /// Logging after increment.
  after increment => Transcript show: 'done'",
        );

        assert_eq!(module.classes.len(), 1);
        let method = &module.classes[0].methods[0];
        assert_eq!(method.kind, MethodKind::After);
        assert_eq!(
            method.doc_comment.as_deref(),
            Some("Logging after increment.")
        );
    }

    #[test]
    fn parse_doc_comment_on_around_method() {
        let module = parse_ok(
            "Actor subclass: Counter
  state: value = 0

  /// Wrapping around increment.
  around increment => self.value := self.value + 1",
        );

        assert_eq!(module.classes.len(), 1);
        let method = &module.classes[0].methods[0];
        assert_eq!(method.kind, MethodKind::Around);
        assert_eq!(
            method.doc_comment.as_deref(),
            Some("Wrapping around increment.")
        );
    }

    #[test]
    fn parse_doc_comment_on_sealed_method() {
        let module = parse_ok(
            "Actor subclass: Counter
  state: value = 0

  /// Cannot be overridden.
  sealed getValue => self.value",
        );

        assert_eq!(module.classes.len(), 1);
        let method = &module.classes[0].methods[0];
        assert!(method.is_sealed);
        assert_eq!(method.doc_comment.as_deref(), Some("Cannot be overridden."));
    }

    #[test]
    fn parse_doc_comment_on_keyword_method() {
        let module = parse_ok(
            "Actor subclass: MyCollection
  state: items = #()

  /// Stores a value at the given index.
  at: index put: value => self.items",
        );

        assert_eq!(module.classes.len(), 1);
        let method = &module.classes[0].methods[0];
        assert_eq!(method.selector.name(), "at:put:");
        assert_eq!(
            method.doc_comment.as_deref(),
            Some("Stores a value at the given index.")
        );
    }

    #[test]
    fn parse_doc_comment_on_binary_method() {
        let module = parse_ok(
            "Object subclass: Vector
  state: x = 0

  /// Adds two vectors.
  + other => self.x + other x",
        );

        assert_eq!(module.classes.len(), 1);
        let method = &module.classes[0].methods[0];
        assert_eq!(method.selector.name(), "+");
        assert_eq!(method.doc_comment.as_deref(), Some("Adds two vectors."));
    }

    #[test]
    fn parse_empty_doc_comment_line() {
        let module = parse_ok(
            "/// First line.
///
/// After empty line.
Actor subclass: Counter
  increment => 1",
        );

        assert_eq!(module.classes.len(), 1);
        assert_eq!(
            module.classes[0].doc_comment.as_deref(),
            Some("First line.\n\nAfter empty line.")
        );
    }

    #[test]
    fn parse_doc_comment_on_class_method() {
        let module = parse_ok(
            "Actor subclass: Counter
  state: value = 0

  /// Creates a counter starting at the given value.
  class withValue: v => self new initialize: v",
        );

        assert_eq!(module.classes.len(), 1);
        let method = &module.classes[0].class_methods[0];
        assert_eq!(
            method.doc_comment.as_deref(),
            Some("Creates a counter starting at the given value.")
        );
    }

    // === is_input_complete tests ===

    #[test]
    fn complete_simple_expression() {
        assert!(is_input_complete("3 + 4"));
    }

    #[test]
    fn complete_empty_input() {
        assert!(is_input_complete(""));
        assert!(is_input_complete("   "));
    }

    #[test]
    fn complete_assignment() {
        assert!(is_input_complete("x := 42"));
    }

    #[test]
    fn complete_keyword_message() {
        assert!(is_input_complete("array at: 1 put: 'hello'"));
    }

    #[test]
    fn complete_block() {
        assert!(is_input_complete("[:x | x * 2]"));
    }

    #[test]
    fn complete_nested_blocks() {
        assert!(is_input_complete("[:x | [:y | x + y]]"));
    }

    #[test]
    fn complete_parenthesized() {
        assert!(is_input_complete("(3 + 4) * 2"));
    }

    #[test]
    fn complete_map_literal() {
        assert!(is_input_complete("#{name => 'Alice', age => 30}"));
    }

    #[test]
    fn complete_list_literal() {
        assert!(is_input_complete("#(1, 2, 3)"));
    }

    #[test]
    fn complete_tuple_literal() {
        assert!(is_input_complete("{1, 2, 3}"));
    }

    #[test]
    fn complete_string() {
        assert!(is_input_complete("'hello world'"));
    }

    #[test]
    fn complete_interpolated_string() {
        assert!(is_input_complete("\"hello world\""));
    }

    #[test]
    fn complete_with_period() {
        assert!(is_input_complete("x := 1."));
    }

    #[test]
    fn complete_multi_statement() {
        assert!(is_input_complete("x := 1.\ny := 2"));
    }

    #[test]
    fn incomplete_unclosed_block() {
        assert!(!is_input_complete("["));
        assert!(!is_input_complete("[:x | x * 2"));
        assert!(!is_input_complete("[:x | [:y | x + y]"));
    }

    #[test]
    fn incomplete_unclosed_paren() {
        assert!(!is_input_complete("("));
        assert!(!is_input_complete("(3 + 4"));
    }

    #[test]
    fn incomplete_unclosed_brace() {
        assert!(!is_input_complete("{"));
        assert!(!is_input_complete("{1, 2"));
    }

    #[test]
    fn incomplete_unclosed_map() {
        assert!(!is_input_complete("#{"));
        assert!(!is_input_complete("#{name => 'Alice'"));
    }

    #[test]
    fn incomplete_unclosed_list() {
        assert!(!is_input_complete("#("));
        assert!(!is_input_complete("#(1, 2"));
    }

    #[test]
    fn incomplete_unterminated_string() {
        assert!(!is_input_complete("'hello"));
    }

    #[test]
    fn incomplete_unterminated_interpolated_string() {
        assert!(!is_input_complete("\"hello"));
    }

    #[test]
    fn incomplete_trailing_keyword() {
        assert!(!is_input_complete("array at:"));
        assert!(!is_input_complete("array at: 1 put:"));
        assert!(!is_input_complete("x ifTrue:"));
    }

    #[test]
    fn incomplete_unterminated_block_comment() {
        assert!(!is_input_complete("/* unterminated comment"));
        assert!(!is_input_complete("x := 1 /* still going"));
    }

    #[test]
    fn complete_block_comment() {
        assert!(is_input_complete("/* comment */ x := 1"));
    }

    #[test]
    fn complete_multiline_block() {
        assert!(is_input_complete("[:x |\n  x * 2\n]"));
    }

    #[test]
    fn complete_multiline_map() {
        assert!(is_input_complete("#{\n  name => 'Alice',\n  age => 30\n}"));
    }

    #[test]
    fn incomplete_multiline_block() {
        assert!(!is_input_complete("[:x |\n  x * 2"));
    }

    #[test]
    fn incomplete_trailing_binary_operator() {
        assert!(!is_input_complete("x := 1 +"));
        assert!(!is_input_complete("3 *"));
    }

    #[test]
    fn incomplete_trailing_assign() {
        assert!(!is_input_complete("x :="));
    }

    #[test]
    fn complete_binary_continuation_on_new_line() {
        assert!(is_input_complete("x := 1 +\n  2"));
    }

    #[test]
    fn complete_line_comment_only() {
        assert!(is_input_complete("// just a comment"));
    }

    #[test]
    fn complete_extra_closing_delimiters() {
        // Extra closing delimiters are "complete" — they'll produce syntax errors
        // when evaluated, which is the desired behavior (show the error, don't
        // loop waiting for more input that can never balance them).
        assert!(is_input_complete("]"));
        assert!(is_input_complete(")"));
        assert!(is_input_complete("}"));
    }

    #[test]
    fn incomplete_trailing_cascade() {
        assert!(!is_input_complete("x foo;"));
    }

    #[test]
    fn incomplete_trailing_caret() {
        assert!(!is_input_complete("^"));
    }

    // === Match expression parsing ===

    #[test]
    fn parse_simple_match() {
        let module = parse_ok("42 match: [_ -> 99]");
        assert_eq!(module.expressions.len(), 1);
        assert!(
            matches!(&module.expressions[0], Expression::Match { arms, .. } if arms.len() == 1)
        );
    }

    #[test]
    fn parse_match_with_semicolons() {
        let module = parse_ok("x match: [1 -> 'one'; 2 -> 'two'; _ -> 'other']");
        assert_eq!(module.expressions.len(), 1);
        assert!(
            matches!(&module.expressions[0], Expression::Match { arms, .. } if arms.len() == 3)
        );
    }

    #[test]
    fn parse_match_with_tuple_pattern() {
        let module = parse_ok("r match: [{#ok, v} -> v; {#error, _} -> nil]");
        assert_eq!(module.expressions.len(), 1);
        assert!(
            matches!(&module.expressions[0], Expression::Match { arms, .. } if arms.len() == 2)
        );
    }

    #[test]
    fn parse_match_with_guard() {
        let module = parse_ok("n match: [x when: [x > 0] -> x; _ -> 0]");
        assert_eq!(module.expressions.len(), 1);
        if let Expression::Match { arms, .. } = &module.expressions[0] {
            assert_eq!(arms.len(), 2);
            assert!(arms[0].guard.is_some());
            assert!(arms[1].guard.is_none());
        } else {
            panic!("Expected Match expression");
        }
    }

    #[test]
    fn codegen_simple_match() {
        let module = parse_ok("42 match: [_ -> 99]");
        let expr = &module.expressions[0];
        let result = crate::codegen::core_erlang::generate_test_expression(expr, "test_match");
        assert!(result.is_ok(), "Codegen failed: {:?}", result.err());
        let code = result.unwrap();
        eprintln!("Generated code:\n{code}");
        assert!(code.contains("case"), "Expected case expression in: {code}");
    }

    #[test]
    fn codegen_match_with_arms() {
        let module = parse_ok("1 match: [1 -> 'one'; 2 -> 'two'; _ -> 'other']");
        let expr = &module.expressions[0];
        let result = crate::codegen::core_erlang::generate_test_expression(expr, "test_match");
        assert!(result.is_ok(), "Codegen failed: {:?}", result.err());
        let code = result.unwrap();
        eprintln!("Generated code:\n{code}");
        assert!(code.contains("case"), "Expected case expression in: {code}");
    }

    #[test]
    fn codegen_empty_match_errors() {
        let module = parse_ok("42 match: []");
        let expr = &module.expressions[0];
        let result = crate::codegen::core_erlang::generate_test_expression(expr, "test_match");
        assert!(result.is_err(), "Empty match should fail codegen");
    }

    /// Regression test: unclosed map literal at EOF must not infinite-loop.
    ///
    /// The fuzzer discovered that `#{key` (no `=>`, no `}`) followed by EOF
    /// caused `parse_map_literal`'s outer loop to spin forever because the
    /// error-recovery path reached EOF and `continue`d without breaking.
    #[test]
    fn unclosed_map_literal_at_eof_terminates() {
        // Minimal repro: unclosed map literal inside parens, followed by EOF
        let _diagnostics = parse_err("(#{key value");
        // Parser must terminate (not infinite-loop). Errors are expected.
    }

    /// Variant: bare unclosed map literal without surrounding parens.
    #[test]
    fn unclosed_map_literal_bare_eof_terminates() {
        let _diagnostics = parse_err("#{key");
    }
}
