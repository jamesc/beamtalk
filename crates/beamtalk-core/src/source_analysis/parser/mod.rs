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
//! | 10 | `==` `/=` `=:=` `=/=` | Left |
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

use std::collections::HashSet;

use crate::ast::{Comment, CommentAttachment, Expression, ExpressionStatement, Module};
#[cfg(test)]
use crate::ast::{CommentKind, Literal, MessageSelector};
use crate::source_analysis::{Span, Token, TokenKind, Trivia, lex_with_eof};
use ecow::EcoString;

// Submodules with additional impl blocks for Parser
mod declarations;
mod expressions;

// Property-based tests (ADR 0011 Phase 2)
#[cfg(test)]
mod property_tests;

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
/// | 10 | `==` `/=` `=:=` `=/=` | Left |
/// | 20 | `<` `>` `<=` `>=`   | Left |
/// | 30 | `+` `-`             | Left |
/// | 40 | `*` `/` `%`         | Left |
/// | 50 | `**`                | Right |
///
/// To add a new operator, just add an entry here. For example:
/// ```ignore
/// // Bitwise OR (between comparison and additive)
/// "|" => Some(BindingPower::left_assoc(25)),
/// ```
pub(super) fn binary_binding_power(op: &str) -> Option<BindingPower> {
    match op {
        // Method lookup (lowest binary precedence)
        // `Counter >> #increment` returns CompiledMethod object
        ">>" => Some(BindingPower::left_assoc(5)),

        // Association creation (BT-335)
        // `#name -> 'James'` creates an Association key-value pair
        "->" => Some(BindingPower::left_assoc(8)),

        // Equality (ADR 0002: Erlang comparison operators)
        // `=:=` strict equality, `=/=` strict inequality
        // `/=` loose inequality, `==` loose equality
        // `=` is the legacy strict-equality alias (BT-952: lint warns to use =:= or simplify)
        "==" | "/=" | "=:=" | "=/=" | "=" => Some(BindingPower::left_assoc(10)),

        // Comparison
        "<" | ">" | "<=" | ">=" => Some(BindingPower::left_assoc(20)),

        // Additive (includes string concatenation ++)
        "+" | "-" | "++" => Some(BindingPower::left_assoc(30)),

        // Multiplicative
        "*" | "/" | "%" => Some(BindingPower::left_assoc(40)),

        // Exponentiation (right-associative, highest binary precedence)
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

    // Warn for any doc comments that appeared before non-declaration tokens:
    // collect_doc_comment() was never called for those tokens, so their indices
    // remain in `unattached_doc_comment_indices` after parsing.
    let mut remaining: Vec<usize> = parser.unattached_doc_comment_indices.into_iter().collect();
    remaining.sort_unstable(); // stable diagnostic order
    for idx in remaining {
        if let Some(token) = parser.tokens.get(idx) {
            parser.diagnostics.push(Diagnostic::warning(
                "doc comment not attached to any declaration \
                 (/// only attaches to class, method, or state declarations)",
                token.span(),
            ));
        }
    }

    (module, parser.diagnostics)
}

/// Checks whether the given source text appears syntactically complete for REPL
/// evaluation.
///
/// This is a heuristic used by the REPL to decide whether to evaluate the
/// current input buffer or show a continuation prompt for multi-line input.
/// It returns `false` (incomplete) when:
///
/// - Delimiters are unclosed: `[`, `(`, `{`, `#{`, `#(`, `#[`
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
    let mut has_subclass_keyword = false;
    let mut has_method_arrow = false;

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
            // Error tokens from unterminated literals (e.g., `"hello` without
            // closing quote) indicate incomplete input.  Diagnostic errors with
            // human-readable messages (e.g., "single-quoted strings are no longer
            // supported") are complete-but-invalid — let the compiler report them.
            TokenKind::Error(msg) => {
                if !is_diagnostic_error(msg) {
                    return false;
                }
            }

            // Opening delimiters
            TokenKind::LeftBracket | TokenKind::ArrayOpen => bracket_depth += 1,
            TokenKind::LeftParen | TokenKind::ListOpen => paren_depth += 1,
            TokenKind::LeftBrace | TokenKind::MapOpen => brace_depth += 1,

            // Closing delimiters
            TokenKind::RightBracket => bracket_depth -= 1,
            TokenKind::RightParen => paren_depth -= 1,
            TokenKind::RightBrace => brace_depth -= 1,

            // Track class definition pattern
            TokenKind::Keyword(k) if k == "subclass:" => has_subclass_keyword = true,
            // Only count `=>` as a method arrow when not nested inside delimiters
            // (e.g., `#{key => value}` in state initializers should not count)
            TokenKind::FatArrow if bracket_depth == 0 && paren_depth == 0 && brace_depth == 0 => {
                has_method_arrow = true;
            }

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

    // Class definition: incomplete until at least one method is defined.
    // "Actor subclass: Counter" alone is incomplete — waiting for state/methods.
    // "Actor subclass: Counter\n  state: n = 0" is still incomplete — no methods.
    // "Actor subclass: Counter\n  increment => self.n := self.n + 1" is complete.
    // In the REPL, user presses Enter on a blank line to submit; in E2E tests,
    // the assertion line `// =>` follows immediately after the last method.
    if has_subclass_keyword && !has_method_arrow {
        return false;
    }

    true
}

/// Check if a lexer error message is a human-readable diagnostic (complete but
/// invalid syntax) rather than a raw source fragment from an unterminated literal.
///
/// Unterminated literal errors contain the raw source text (e.g., `"hello` or `$\`),
/// which starts with a delimiter character.  Diagnostic errors are English sentences
/// that start with a lowercase letter (e.g., "single-quoted strings are no longer
/// supported...").
fn is_diagnostic_error(msg: &str) -> bool {
    msg.starts_with(|c: char| c.is_ascii_lowercase())
}

/// The semantic category of a diagnostic, used by `@expect` for suppression.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticCategory {
    /// Does-not-understand (DNU) hint.
    Dnu,
    /// Type-related warning or hint.
    Type,
    /// Unused-variable warning.
    Unused,
    /// Empty-method-body error (BT-859).
    EmptyBody,
    /// Style/redundancy lint (BT-959).
    Lint,
}

/// A diagnostic message (error, warning, or hint).
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
    /// Optional semantic category for `@expect` suppression.
    pub category: Option<DiagnosticCategory>,
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
            category: None,
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
            category: None,
        }
    }

    /// Creates a new hint diagnostic (informational, lower severity than warning).
    #[must_use]
    pub fn hint(message: impl Into<EcoString>, span: Span) -> Self {
        Self {
            severity: Severity::Hint,
            message: message.into(),
            span,
            hint: None,
            category: None,
        }
    }

    /// Creates a new lint diagnostic (style/redundancy check, suppressed during normal compile).
    #[must_use]
    pub fn lint(message: impl Into<EcoString>, span: Span) -> Self {
        Self {
            severity: Severity::Lint,
            message: message.into(),
            span,
            hint: None,
            category: Some(DiagnosticCategory::Lint),
        }
    }

    /// Attaches a semantic category for `@expect` suppression.
    #[must_use]
    pub fn with_category(mut self, category: DiagnosticCategory) -> Self {
        self.category = Some(category);
        self
    }
}

/// Diagnostic severity level.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Severity {
    /// An error that prevents compilation.
    Error,
    /// A warning that should be addressed.
    Warning,
    /// A style/redundancy lint check (suppressed during normal compile, shown by `beamtalk lint`).
    Lint,
    /// A hint or informational note (e.g. DNU that may be intentional).
    Hint,
}

/// Maximum nesting depth for expressions before the parser bails out.
///
/// Prevents stack overflow on deeply nested input (e.g., `(((((...)))))`).
/// Each nesting level uses multiple stack frames through the parser call
/// chain, and ASAN-instrumented builds (fuzzing) have larger frames.
/// 64 is generous enough for any realistic program while staying safe.
///
/// As a second line of defence, `stacker::maybe_grow` is used at the
/// recursive entry point so the stack is extended on the heap if needed.
const MAX_NESTING_DEPTH: usize = 64;

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
    /// Whether the parser is currently inside a class body.
    /// Used to detect trailing expressions via indentation (BT-903).
    pub(super) in_class_body: bool,
    /// Current expression nesting depth (guards against stack overflow).
    nesting_depth: usize,
    /// Indices of tokens whose leading trivia contains at least one `DocComment`.
    ///
    /// Pre-scanned in `Parser::new`. Entries are removed as doc comments are
    /// successfully attached to declarations or warned about during
    /// `collect_doc_comment`. Any indices remaining after `parse_module`
    /// represent doc comments before non-declaration tokens, and a warning is
    /// emitted for each.
    unattached_doc_comment_indices: HashSet<usize>,
}

impl Parser {
    /// Creates a new parser for the given tokens.
    fn new(tokens: Vec<Token>) -> Self {
        // Pre-scan: record indices of tokens that have DocComment trivia.
        // These are tracked so that doc comments never visited by
        // `collect_doc_comment` (e.g. before non-declaration expressions)
        // can still be warned about after parsing completes.
        let unattached_doc_comment_indices = tokens
            .iter()
            .enumerate()
            .filter(|(_, t)| t.leading_trivia().iter().any(Trivia::is_doc_comment))
            .map(|(i, _)| i)
            .collect();
        Self {
            tokens,
            current: 0,
            diagnostics: Vec::new(),
            in_method_body: false,
            in_class_body: false,
            nesting_depth: 0,
            unattached_doc_comment_indices,
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
    ///
    /// When doc comment lines are found and then discarded because a blank
    /// line or a regular `//` comment breaks the attachment, a
    /// [`Severity::Warning`] diagnostic is emitted immediately.
    pub(super) fn collect_doc_comment(&mut self) -> Option<String> {
        // Clone leading trivia to release the shared borrow of `self` so we
        // can push to `self.diagnostics` inside the loop.
        let leading_trivia = self.current_token().leading_trivia().to_vec();
        let current_span = self.current_token().span();
        let current_idx = self.current;

        let mut lines: Vec<String> = Vec::new();
        let mut had_unattached = false;

        for trivia in &leading_trivia {
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
                        if !lines.is_empty() {
                            had_unattached = true;
                        }
                        lines.clear();
                    }
                }
                _ => {
                    // A non-doc-comment (e.g. `//`) resets collection
                    if !lines.is_empty() {
                        had_unattached = true;
                    }
                    lines.clear();
                }
            }
        }

        if had_unattached {
            self.diagnostics.push(Diagnostic::warning(
                "doc comment not attached to any declaration \
                 (blank line or // comment breaks attachment)",
                current_span,
            ));
            // Remove from pre-scan set so the post-parse sweep does not
            // emit a second warning for the same token.
            self.unattached_doc_comment_indices.remove(&current_idx);
        }

        if lines.is_empty() {
            None
        } else {
            // Successfully attached — remove from the pre-scan set.
            self.unattached_doc_comment_indices.remove(&current_idx);
            Some(lines.join("\n"))
        }
    }

    /// Extracts regular (non-doc) comments from the current token's leading trivia.
    ///
    /// Collects `//` line comments and `/* */` block comments into
    /// [`CommentAttachment::leading`].  `///` doc comments are intentionally
    /// **skipped** — they are handled by [`collect_doc_comment`] and must not
    /// be duplicated here.  Call this *after* [`collect_doc_comment`] so both
    /// functions operate on the same trivia snapshot before any token is
    /// consumed.
    ///
    /// # Ordering dependency
    ///
    /// `collect_doc_comment()` must run first, then `collect_comment_attachment()`
    /// on the same leading trivia.  Neither method consumes any tokens, so the
    /// current token is the same for both calls.
    pub(super) fn collect_comment_attachment(&self) -> CommentAttachment {
        let token_span = self.current_token().span();
        let mut leading = Vec::new();
        let mut saw_blank_line = false;
        for trivia in self.current_token().leading_trivia() {
            match trivia {
                super::Trivia::LineComment(text) => {
                    // Strip `// ` (with space) or `//` — Comment.content is
                    // documented as "text without delimiters", consistent with
                    // how collect_doc_comment strips `/// ` from DocComment.
                    let s = text.as_str();
                    let content = s
                        .strip_prefix("// ")
                        .or_else(|| s.strip_prefix("//"))
                        .unwrap_or(s);
                    let mut comment = Comment::line(content, token_span);
                    comment.preceding_blank_line = saw_blank_line;
                    leading.push(comment);
                    saw_blank_line = false;
                }
                super::Trivia::BlockComment(text) => {
                    // Strip `/* ` / ` */` delimiters.
                    let s = text.as_str();
                    let content = s
                        .strip_prefix("/* ")
                        .and_then(|s| s.strip_suffix(" */"))
                        .or_else(|| s.strip_prefix("/*").and_then(|s| s.strip_suffix("*/")))
                        .unwrap_or(s);
                    let mut comment = Comment::block(content, token_span);
                    comment.preceding_blank_line = saw_blank_line;
                    leading.push(comment);
                    saw_blank_line = false;
                }
                super::Trivia::Whitespace(ws) => {
                    // Detect blank lines (2+ newlines) in whitespace between comments
                    if ws.chars().filter(|&c| c == '\n').count() > 1 {
                        saw_blank_line = true;
                    }
                }
                // DocComment is normally handled by collect_doc_comment for
                // declarations.  When it wasn't consumed (e.g. before module-level
                // expressions), preserve it as a regular line comment so the
                // formatter doesn't drop it.
                super::Trivia::DocComment(text) => {
                    if self.unattached_doc_comment_indices.contains(&self.current) {
                        let s = text.as_str();
                        let content = s
                            .strip_prefix("/// ")
                            .unwrap_or_else(|| s.strip_prefix("///").unwrap_or(s));
                        // Prefix with "/" so it round-trips as `/// content`
                        let prefixed = if content.is_empty() {
                            "/".to_string()
                        } else {
                            format!("/ {content}")
                        };
                        let mut comment = Comment::line(&prefixed, token_span);
                        comment.preceding_blank_line = saw_blank_line;
                        leading.push(comment);
                        saw_blank_line = false;
                    }
                }
            }
        }
        CommentAttachment {
            leading,
            trailing: None,
        }
    }

    /// Collects a trailing end-of-line comment from the last consumed token's trailing trivia.
    ///
    /// After parsing a complete expression, call this to check whether there is a
    /// `// comment` on the same source line as the expression's last token.  The
    /// lexer places same-line trailing comments in [`Token::trailing_trivia`], so
    /// checking the previously-consumed token is sufficient — no look-ahead needed.
    ///
    /// Only `//` line comments are considered; doc comments (`///`) and block
    /// comments (`/* */`) are ignored here (block comments can span multiple lines
    /// and are already collected as leading comments by [`collect_comment_attachment`]).
    pub(super) fn collect_trailing_comment(&self) -> Option<Comment> {
        if self.current == 0 {
            return None;
        }
        let last_token = &self.tokens[self.current - 1];
        for trivia in last_token.trailing_trivia() {
            if let super::Trivia::LineComment(text) = trivia {
                let s = text.as_str();
                let content = s
                    .strip_prefix("// ")
                    .or_else(|| s.strip_prefix("//"))
                    .unwrap_or(s);
                return Some(Comment::line(content, last_token.span()));
            }
        }
        None
    }

    /// Reports an error at the current token.
    pub(super) fn error(&mut self, message: impl Into<EcoString>) {
        let span = self.current_token().span();
        self.diagnostics.push(Diagnostic::error(message, span));
    }

    /// Increments the nesting depth and returns `Err(Expression::Error)` if
    /// it exceeds [`MAX_NESTING_DEPTH`].  Call [`leave_nesting`] on every
    /// exit path when this returns `Ok(())`.
    pub(super) fn enter_nesting(&mut self, span: Span) -> Result<(), Expression> {
        self.nesting_depth += 1;
        if self.nesting_depth > MAX_NESTING_DEPTH {
            self.diagnostics.push(Diagnostic::error(
                format!("Expression nesting is too deep (maximum {MAX_NESTING_DEPTH} levels)"),
                span,
            ));
            self.nesting_depth -= 1;
            return Err(Expression::Error {
                message: "Expression nesting too deep".into(),
                span,
            });
        }
        Ok(())
    }

    /// Decrements the nesting depth (pair with [`enter_nesting`]).
    pub(super) fn leave_nesting(&mut self) {
        debug_assert!(
            self.nesting_depth > 0,
            "leave_nesting called without matching enter_nesting"
        );
        self.nesting_depth = self.nesting_depth.saturating_sub(1);
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
                | TokenKind::Bang
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
        let mut method_definitions = Vec::new();
        let mut expressions = Vec::new();

        // Parse statements until EOF.
        // Leading comments on each expression are collected by collect_comment_attachment()
        // immediately before parse_expression().  Class and standalone-method definitions
        // collect their own comments inside parse_class_definition() /
        // parse_standalone_method_definition() via the same helper.
        while !self.is_at_end() {
            // Check if this looks like a class definition
            if self.is_at_class_definition() {
                let class = self.parse_class_definition();
                classes.push(class);
            } else if self.is_at_standalone_method_definition() {
                let method_def = self.parse_standalone_method_definition();
                method_definitions.push(method_def);
            } else {
                let pos_before = self.current;
                // BT-987: detect blank lines (2+ newlines) before this statement
                let has_blank_line =
                    !expressions.is_empty() && self.current_token().has_preceding_blank_line();
                let mut comments = self.collect_comment_attachment();
                let expr = self.parse_expression();
                // Only collect trailing comment if parse_expression consumed tokens;
                // otherwise collect_trailing_comment() reads the previous token's
                // trivia, which belongs to the prior statement.
                if self.current > pos_before {
                    comments.trailing = self.collect_trailing_comment();
                }
                let is_error = expr.is_error();
                expressions.push(ExpressionStatement {
                    comments,
                    expression: expr,
                    preceding_blank_line: has_blank_line,
                });

                // If we got an error, try to recover
                if is_error {
                    self.synchronize();
                } else {
                    // Optional statement terminator: `.` or `!` (cast)
                    if self.match_token(&TokenKind::Period) {
                        // period consumed — nothing else to do
                    } else if self.match_token(&TokenKind::Bang) {
                        // Cast terminator — annotate the last expression as a cast
                        match expressions.last_mut().map(|s| &mut s.expression) {
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
                    }
                }
            }
        }

        // Collect comments from the EOF token.
        // - Empty modules: all comments go to file_leading_comments.
        // - Non-empty modules: leading comments on the first item were already
        //   collected; any remaining comments on the EOF token are trailing comments
        //   that appear after the last item in the file.
        let eof_comments = self.collect_comment_attachment().leading;
        let (file_leading_comments, file_trailing_comments) =
            if classes.is_empty() && method_definitions.is_empty() && expressions.is_empty() {
                (eof_comments, Vec::new())
            } else {
                (Vec::new(), eof_comments)
            };

        // Get end span
        let end = if self.current > 0 {
            self.tokens[self.current - 1].span()
        } else {
            start
        };
        let span = start.merge(end);

        Module {
            classes,
            method_definitions,
            expressions,
            span,
            file_leading_comments,
            file_trailing_comments,
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

        // Skip optional `abstract`, `sealed`, or `typed` modifiers
        while let Some(kind) = self.peek_at(offset) {
            match kind {
                TokenKind::Identifier(name)
                    if name == "abstract" || name == "sealed" || name == "typed" =>
                {
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

    /// Checks if the current position looks like a standalone method definition.
    ///
    /// Standalone method definitions follow the pattern:
    /// - `ClassName >> selector => body` (instance method)
    /// - `ClassName class >> selector => body` (class method)
    ///
    /// We look ahead to detect `Identifier >>` followed by a method selector and `=>`.
    pub(super) fn is_at_standalone_method_definition(&self) -> bool {
        let mut offset = 0;

        // Must start with an uppercase identifier (class name)
        match self.peek_at(offset) {
            Some(TokenKind::Identifier(name)) => {
                if !name.starts_with(|c: char| c.is_uppercase()) {
                    return false;
                }
                offset += 1;
            }
            _ => return false,
        }

        // Optional `class` modifier for class-side methods
        if matches!(self.peek_at(offset), Some(TokenKind::Identifier(name)) if name == "class") {
            offset += 1;
        }

        // Must have `>>` binary selector
        if !matches!(self.peek_at(offset), Some(TokenKind::BinarySelector(s)) if s == ">>") {
            return false;
        }
        offset += 1;

        // After `>>`, must have a method selector followed by `=>`
        self.is_method_selector_at(offset)
    }

    /// Checks if there is a method selector followed by `=>` at the given offset.
    fn is_method_selector_at(&self, offset: usize) -> bool {
        match self.peek_at(offset) {
            // Unary: `identifier =>`
            Some(TokenKind::Identifier(_)) => {
                matches!(self.peek_at(offset + 1), Some(TokenKind::FatArrow))
            }
            // Binary: `+ other =>`
            Some(TokenKind::BinarySelector(_)) => {
                matches!(self.peek_at(offset + 1), Some(TokenKind::Identifier(_)))
                    && matches!(self.peek_at(offset + 2), Some(TokenKind::FatArrow))
            }
            // Keyword: `at: index put: value =>`
            Some(TokenKind::Keyword(_)) => self.is_keyword_method_selector_at(offset),
            _ => false,
        }
    }

    /// Checks if there's a keyword method selector followed by `=>` at the given offset.
    fn is_keyword_method_selector_at(&self, start_offset: usize) -> bool {
        let mut offset = start_offset;
        loop {
            if !matches!(self.peek_at(offset), Some(TokenKind::Keyword(_))) {
                return false;
            }
            offset += 1;
            if !matches!(self.peek_at(offset), Some(TokenKind::Identifier(_))) {
                return false;
            }
            offset += 1;
            match self.peek_at(offset) {
                Some(TokenKind::FatArrow) => return true,
                Some(TokenKind::Keyword(_)) => {}
                _ => return false,
            }
        }
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
    //! Tests for the Beamtalk recursive descent parser.
    use super::*;
    use crate::source_analysis::lex_with_eof;

    /// Helper to parse a string and check for errors.
    ///
    /// Passes if there are no Error or Warning diagnostics. Lint diagnostics
    /// are ignored here since they do not block compilation.
    fn parse_ok(source: &str) -> Module {
        let tokens = lex_with_eof(source);
        let (module, diagnostics) = parse(tokens);
        let non_lint: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.severity != Severity::Lint)
            .collect();
        assert!(non_lint.is_empty(), "Expected no errors, got: {non_lint:?}");
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
        match &module.expressions[0].expression {
            Expression::Literal(Literal::Integer(42), _) => {}
            _ => panic!("Expected integer literal"),
        }
    }

    #[test]
    fn parse_float_literal() {
        let module = parse_ok("2.5");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::Literal(Literal::Float(f), _) if (*f - 2.5_f64).abs() < 0.001 => {}
            _ => panic!("Expected float literal"),
        }
    }

    #[test]
    fn parse_string_literal() {
        let module = parse_ok("\"hello\"");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::Literal(Literal::String(s), _) if s == "hello" => {}
            _ => panic!("Expected string literal"),
        }
    }

    #[test]
    fn parse_symbol_literal() {
        let module = parse_ok("#symbol");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::Literal(Literal::Symbol(s), _) if s == "symbol" => {}
            _ => panic!("Expected symbol literal"),
        }
    }

    #[test]
    fn parse_identifier() {
        let module = parse_ok("myVar");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::Identifier(id) if id.name == "myVar" => {}
            _ => panic!("Expected identifier"),
        }
    }

    #[test]
    fn parse_assignment() {
        let module = parse_ok("x := 42");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
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
        match &module.expressions[0].expression {
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
        match &module.expressions[0].expression {
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
        match &module.expressions[0].expression {
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
        let module = parse_ok("array at: 1 put: \"x\"");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
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
    fn parse_keyword_message_multiline_iftrue_iffalse() {
        let module =
            parse_ok("acc isEmpty ifTrue: [cell]\n            ifFalse: [\"{acc},{cell}\"]");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::MessageSend {
                selector: MessageSelector::Keyword(parts),
                arguments,
                ..
            } => {
                assert_eq!(parts.len(), 2);
                assert_eq!(parts[0].keyword.as_str(), "ifTrue:");
                assert_eq!(parts[1].keyword.as_str(), "ifFalse:");
                assert_eq!(arguments.len(), 2);
            }
            _ => panic!("Expected keyword message send"),
        }
    }

    #[test]
    fn parse_keyword_message_multiline_inject_into() {
        let module = parse_ok("collection inject: 0\n             into: [:acc :each | acc + each]");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::MessageSend {
                selector: MessageSelector::Keyword(parts),
                arguments,
                ..
            } => {
                assert_eq!(parts.len(), 2);
                assert_eq!(parts[0].keyword.as_str(), "inject:");
                assert_eq!(parts[1].keyword.as_str(), "into:");
                assert_eq!(arguments.len(), 2);
            }
            _ => panic!("Expected keyword message send"),
        }
    }

    #[test]
    fn parse_keyword_message_multiline_to_do() {
        let module = parse_ok("1 to: 10\n  do: [:i | i]");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::MessageSend {
                selector: MessageSelector::Keyword(parts),
                arguments,
                ..
            } => {
                assert_eq!(parts.len(), 2);
                assert_eq!(parts[0].keyword.as_str(), "to:");
                assert_eq!(parts[1].keyword.as_str(), "do:");
                assert_eq!(arguments.len(), 2);
            }
            _ => panic!("Expected keyword message send"),
        }
    }

    #[test]
    fn parse_keyword_message_dot_terminates_not_newline() {
        // Two separate statements: `a foo: 1` then `b bar: 2`
        let module = parse_ok("a foo: 1.\nb bar: 2");
        assert_eq!(module.expressions.len(), 2);
    }

    #[test]
    fn parse_multiline_keyword_does_not_consume_method_def() {
        // In a class body, a keyword method on the next line should NOT
        // be consumed as a continuation of the keyword message above.
        let module = parse_ok(
            "Actor subclass: Counter
  state: count = 0
  value => count
  increment: n => n + 1",
        );
        assert_eq!(module.classes.len(), 1);
        let class = &module.classes[0];
        assert_eq!(class.state.len(), 1);
        assert_eq!(class.methods.len(), 2);
    }

    #[test]
    fn parse_block_no_params() {
        let module = parse_ok("[42]");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
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
        match &module.expressions[0].expression {
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
        match &module.expressions[0].expression {
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
        match &module.expressions[0].expression {
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
        match &module.expressions[0].expression {
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
        let module = parse_ok("Transcript show: \"Hello\"; cr; show: \"World\"");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::Cascade {
                receiver, messages, ..
            } => {
                // Receiver is the first message: Transcript show: "Hello"
                assert!(matches!(**receiver, Expression::MessageSend { .. }));
                assert_eq!(messages.len(), 2);
                // First message: cr (unary)
                assert!(matches!(
                    &messages[0].selector,
                    MessageSelector::Unary(name) if name == "cr"
                ));
                assert!(messages[0].arguments.is_empty());
                // Second message: show: "World" (keyword)
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
        match &module.expressions[0].expression {
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
        match &module.expressions[0].expression {
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
        // Plain strings without interpolation still parse as Literal::String
        let module = parse_ok("\"Hello, world!\"");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::Literal(Literal::String(s), _) if s == "Hello, world!" => {}
            _ => panic!("Expected plain string literal"),
        }

        // Interpolated string produces StringInterpolation node
        let tokens = lex_with_eof("\"Hello, {name}!\"");
        let (module, diagnostics) = parse(tokens);
        assert!(
            diagnostics.is_empty(),
            "Expected no diagnostics, got: {diagnostics:?}"
        );
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::StringInterpolation { segments, .. } => {
                assert_eq!(segments.len(), 3);
                match &segments[0] {
                    crate::ast::StringSegment::Literal(s) => assert_eq!(s, "Hello, "),
                    crate::ast::StringSegment::Interpolation(_) => {
                        panic!("Expected literal segment")
                    }
                }
                match &segments[1] {
                    crate::ast::StringSegment::Interpolation(Expression::Identifier(id)) => {
                        assert_eq!(id.name, "name");
                    }
                    crate::ast::StringSegment::Interpolation(_)
                    | crate::ast::StringSegment::Literal(_) => {
                        panic!("Expected interpolation segment with identifier")
                    }
                }
                match &segments[2] {
                    crate::ast::StringSegment::Literal(s) => assert_eq!(s, "!"),
                    crate::ast::StringSegment::Interpolation(_) => {
                        panic!("Expected literal segment")
                    }
                }
            }
            other => panic!("Expected StringInterpolation, got: {other:?}"),
        }
    }

    #[test]
    fn parse_interpolated_string_multiple_segments() {
        // "Name: {firstName} {lastName}"
        let tokens = lex_with_eof("\"Name: {firstName} {lastName}\"");
        let (module, diagnostics) = parse(tokens);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::StringInterpolation { segments, .. } => {
                assert_eq!(segments.len(), 4); // "Name: ", firstName, " ", lastName
            }
            other => panic!("Expected StringInterpolation, got: {other:?}"),
        }
    }

    #[test]
    fn parse_interpolated_string_with_binary_message() {
        // "Result: {x + 1}"
        let tokens = lex_with_eof("\"Result: {x + 1}\"");
        let (module, diagnostics) = parse(tokens);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::StringInterpolation { segments, .. } => {
                assert_eq!(segments.len(), 2); // "Result: ", (x + 1)
                assert!(matches!(
                    &segments[1],
                    crate::ast::StringSegment::Interpolation(Expression::MessageSend { .. })
                ));
            }
            other => panic!("Expected StringInterpolation, got: {other:?}"),
        }
    }

    #[test]
    fn parse_interpolated_string_with_keyword_message() {
        // "Value: {dict at: key}"
        let tokens = lex_with_eof("\"Value: {dict at: key}\"");
        let (module, diagnostics) = parse(tokens);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::StringInterpolation { segments, .. } => {
                assert_eq!(segments.len(), 2); // "Value: ", (dict at: key)
                assert!(matches!(
                    &segments[1],
                    crate::ast::StringSegment::Interpolation(Expression::MessageSend { .. })
                ));
            }
            other => panic!("Expected StringInterpolation, got: {other:?}"),
        }
    }

    #[test]
    fn parse_interpolated_string_no_trailing_literal() {
        // "{name}" — interpolation with no literal segments at start or end
        let tokens = lex_with_eof("\"{name}\"");
        let (module, diagnostics) = parse(tokens);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::StringInterpolation { segments, .. } => {
                // Only the interpolation expression, empty start/end omitted
                assert_eq!(segments.len(), 1);
                assert!(matches!(
                    &segments[0],
                    crate::ast::StringSegment::Interpolation(Expression::Identifier(_))
                ));
            }
            other => panic!("Expected StringInterpolation, got: {other:?}"),
        }
    }

    #[test]
    fn parse_interpolated_string_empty_braces_error() {
        // "{}" produces lexer Error token — parser should include it as error segment
        let tokens = lex_with_eof("\"before {} after\"");
        let (_, diagnostics) = parse(tokens);
        // Empty interpolation produces diagnostics from the lexer error
        assert!(!diagnostics.is_empty());
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
        if let Expression::Assignment { value, .. } = &module.expressions[0].expression {
            assert!(value.is_error());
        }
    }

    #[test]
    fn parse_field_access_with_period() {
        // Test that field access doesn't consume statement terminator
        let module = parse_ok("self.value.");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::FieldAccess { .. } => {}
            _ => panic!("Expected field access"),
        }
    }

    #[test]
    fn parse_block_multiple_statements_with_binary_op() {
        // Regression test: ensure `. ` (period + space) is parsed as statement
        // separator, not field access
        let module = parse_ok("[ 1 + m. y := 1]");
        match &module.expressions[0].expression {
            Expression::Block(block) => {
                assert_eq!(block.body.len(), 2, "Block should have 2 statements");
            }
            _ => panic!("Expected block"),
        }
    }

    #[test]
    fn parse_block_newline_separated_statements() {
        // BT-360: newlines act as implicit statement separators
        let module = parse_ok("[\n  Transcript show: \"a\"\n  Transcript show: \"b\"\n  42\n]");
        match &module.expressions[0].expression {
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
        match &module.expressions[0].expression {
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
        match &module.expressions[0].expression {
            Expression::Block(block) => {
                assert_eq!(
                    block.body.len(),
                    3,
                    "Block should have 3 statements (including error)"
                );

                // First statement should be valid
                assert!(
                    !block.body[0].expression.is_error(),
                    "First statement should be valid"
                );

                // Second statement is assignment with error value
                assert!(
                    matches!(&block.body[1].expression, Expression::Assignment { value, .. } if value.is_error()),
                    "Second statement should be assignment with error value"
                );

                // Third statement should be parsed after error
                assert!(
                    !block.body[2].expression.is_error(),
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
            "Object subclass: Chatty\n\n  greet =>\n    Transcript show: \"Hello\"\n    Transcript show: \"World\"\n    42",
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
            !method.body[0].expression.is_error(),
            "First statement should be valid"
        );

        // Second statement should be an error assignment
        assert!(
            matches!(&method.body[1].expression, Expression::Assignment { value, .. } if value.is_error()),
            "Second statement should be assignment with error value"
        );

        // Third statement should be parsed after recovery
        assert!(
            !method.body[2].expression.is_error(),
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
            !class.methods[1].body[0].expression.is_error(),
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
            !method.body[0].expression.is_error(),
            "First statement should be valid"
        );

        // Last statement should be valid (z := 3) — recovered after error
        assert!(
            !method.body[method.body.len() - 1].expression.is_error(),
            "Last statement should be valid after recovery"
        );
    }

    #[test]
    fn parse_standalone_method_with_field_assignment() {
        let source = "Counter >> foo => self.count := 5";
        let tokens = lex_with_eof(source);
        let (module, diagnostics) = parse(tokens);
        assert!(
            diagnostics.is_empty(),
            "Expected no parse errors, got: {diagnostics:?}"
        );
        assert_eq!(
            module.method_definitions.len(),
            1,
            "Expected 1 standalone method definition"
        );
        let md = &module.method_definitions[0];
        assert_eq!(md.class_name.name, "Counter");
        assert_eq!(md.method.selector.name(), "foo");
        assert!(
            !md.method.body.is_empty(),
            "Method body should not be empty — it contains `self.count := 5`"
        );
        // Also verify no empty-body warning from semantic analysis
        let all_diags = crate::queries::diagnostic_provider::compute_diagnostics(&module, vec![]);
        let empty_body_warnings: Vec<_> = all_diags
            .iter()
            .filter(|d| d.message.contains("empty body"))
            .collect();
        assert!(
            empty_body_warnings.is_empty(),
            "Should not warn about empty body, got: {empty_body_warnings:?}"
        );
    }

    #[test]
    fn parse_combined_class_with_standalone_method_field_assign() {
        // Simulates what the REPL does: concatenates class source + standalone method
        let source = "Actor subclass: Counter\n  state: count = 0\n  increment => self.count := self.count + 1\nCounter >> foo => self.count := 5";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        // The standalone method should have a non-empty body
        assert_eq!(module.method_definitions.len(), 1);
        let md = &module.method_definitions[0];
        assert_eq!(md.method.selector.name(), "foo");
        assert!(
            !md.method.body.is_empty(),
            "Standalone method body should not be empty — contains `self.count := 5`"
        );

        let all_diags =
            crate::queries::diagnostic_provider::compute_diagnostics(&module, parse_diags);
        let empty_body_warnings: Vec<_> = all_diags
            .iter()
            .filter(|d| d.message.contains("empty body"))
            .collect();
        assert!(
            empty_body_warnings.is_empty(),
            "Should not warn about empty body, got: {empty_body_warnings:?}"
        );
    }

    #[test]
    fn parse_empty_map() {
        let module = parse_ok("#{}");
        assert_eq!(module.expressions.len(), 1);
        assert!(matches!(
            &module.expressions[0].expression,
            Expression::MapLiteral { pairs, .. } if pairs.is_empty()
        ));
    }

    #[test]
    fn parse_map_with_atom_keys() {
        let module = parse_ok("#{#name => \"Alice\", #age => 30}");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::MapLiteral { pairs, .. } => {
                assert_eq!(pairs.len(), 2);
                // First pair: #name => "Alice"
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
        match &module.expressions[0].expression {
            Expression::Block(block) => {
                assert_eq!(block.body.len(), 3, "Block should have 3 statements");
                assert!(matches!(
                    block.body[0].expression,
                    Expression::Assignment { .. }
                ));
                assert!(matches!(
                    block.body[1].expression,
                    Expression::Assignment { .. }
                ));
                assert!(matches!(
                    block.body[2].expression,
                    Expression::Return { .. }
                ));
            }
            _ => panic!("Expected block"),
        }
    }

    #[test]
    fn parse_map_with_string_keys() {
        let module = parse_ok("#{\"host\" => \"localhost\", \"port\" => 8080}");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
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
        match &module.expressions[0].expression {
            Expression::Block(block) => {
                assert_eq!(block.body.len(), 3, "Block should have 3 statements");
            }
            _ => panic!("Expected block"),
        }
    }

    #[test]
    fn parse_map_with_integer_keys() {
        let module = parse_ok("#{1 => \"first\", 2 => \"second\"}");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
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
        match &module.expressions[0].expression {
            Expression::MapLiteral { pairs, .. } => {
                assert_eq!(pairs.len(), 2);
            }
            _ => panic!("Expected MapLiteral"),
        }
    }

    #[test]
    fn parse_nested_maps() {
        let module = parse_ok("#{#outer => #{#inner => \"value\"}}");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
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
    fn parse_map_value_binary_expression() {
        // BT-664: Map values should support binary expressions
        let module = parse_ok("#{#x => 1 + 2}");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::MapLiteral { pairs, .. } => {
                assert_eq!(pairs.len(), 1);
                assert!(
                    matches!(&pairs[0].key, Expression::Literal(Literal::Symbol(s), _) if s == "x")
                );
                match &pairs[0].value {
                    Expression::MessageSend {
                        receiver,
                        selector: MessageSelector::Binary(op),
                        arguments,
                        ..
                    } => {
                        assert!(matches!(
                            **receiver,
                            Expression::Literal(Literal::Integer(1), _)
                        ));
                        assert_eq!(op.as_str(), "+");
                        assert_eq!(arguments.len(), 1);
                        assert!(matches!(
                            arguments[0],
                            Expression::Literal(Literal::Integer(2), _)
                        ));
                    }
                    _ => panic!("Expected binary message send as map value"),
                }
            }
            _ => panic!("Expected MapLiteral"),
        }
    }

    #[test]
    fn parse_map_value_multiple_binary_expressions() {
        // BT-664: Multiple map values with binary expressions
        let module = parse_ok("#{#x => 1 + 2, #y => 3 * 4}");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::MapLiteral { pairs, .. } => {
                assert_eq!(pairs.len(), 2);
                assert!(matches!(
                    &pairs[0].value,
                    Expression::MessageSend {
                        selector: MessageSelector::Binary(op),
                        ..
                    } if op.as_str() == "+"
                ));
                assert!(matches!(
                    &pairs[1].value,
                    Expression::MessageSend {
                        selector: MessageSelector::Binary(op),
                        ..
                    } if op.as_str() == "*"
                ));
            }
            _ => panic!("Expected MapLiteral"),
        }
    }

    #[test]
    fn parse_map_value_unary_on_binary() {
        // BT-664: Map values with unary messages on binary results
        let module = parse_ok("#{#x => self x + other x}");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::MapLiteral { pairs, .. } => {
                assert_eq!(pairs.len(), 1);
                // Value should be a binary message send (+)
                assert!(matches!(
                    &pairs[0].value,
                    Expression::MessageSend {
                        selector: MessageSelector::Binary(op),
                        ..
                    } if op.as_str() == "+"
                ));
            }
            _ => panic!("Expected MapLiteral"),
        }
    }

    #[test]
    fn parse_map_assignment() {
        let module = parse_ok("person := #{#name => \"Alice\"}");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
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
        let source = "empty := #{}\nperson := #{#name => \"Alice\", #age => 30}\n";
        let module = parse_ok(source);

        assert_eq!(module.expressions.len(), 2, "Expected 2 expressions");

        // First: empty := #{}
        match &module.expressions[0].expression {
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

        // Second: person := #{#name => "Alice", #age => 30}
        match &module.expressions[1].expression {
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

    #[test]
    fn parse_map_with_bare_identifier_keys() {
        // BT-591: Bare identifiers before `=>` in map literals become implicit symbols
        let module = parse_ok("#{name => \"Alice\", age => 30}");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::MapLiteral { pairs, .. } => {
                assert_eq!(pairs.len(), 2);
                // Bare `name` becomes Symbol("name")
                assert!(
                    matches!(&pairs[0].key, Expression::Literal(Literal::Symbol(s), _) if s == "name")
                );
                assert!(
                    matches!(&pairs[0].value, Expression::Literal(Literal::String(s), _) if s == "Alice")
                );
                // Bare `age` becomes Symbol("age")
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
    fn parse_map_with_mixed_keys() {
        // BT-591: Mixed bare identifier and explicit symbol keys
        let module = parse_ok("#{x => 1, #y => 2}");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::MapLiteral { pairs, .. } => {
                assert_eq!(pairs.len(), 2);
                // Bare `x` becomes Symbol("x")
                assert!(
                    matches!(&pairs[0].key, Expression::Literal(Literal::Symbol(s), _) if s == "x")
                );
                // Explicit `#y` is also Symbol("y")
                assert!(
                    matches!(&pairs[1].key, Expression::Literal(Literal::Symbol(s), _) if s == "y")
                );
            }
            _ => panic!("Expected MapLiteral"),
        }
    }

    #[test]
    fn parse_map_uppercase_key_not_converted() {
        // BT-591: Uppercase identifiers (class references) used as map keys are NOT converted to symbols
        let module = parse_ok("#{Counter => 1}");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::MapLiteral { pairs, .. } => {
                assert_eq!(pairs.len(), 1);
                // Uppercase bare identifier `Counter` remains a ClassReference, not an implicit symbol
                assert!(matches!(&pairs[0].key, Expression::ClassReference { .. }));
                assert!(matches!(
                    &pairs[0].value,
                    Expression::Literal(Literal::Integer(1), _)
                ));
            }
            _ => panic!("Expected MapLiteral"),
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
  state: name: String = \"unnamed\"",
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
        assert_eq!(method.parameters[0].name.name, "other");
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
        assert_eq!(method.parameters[0].name.name, "index");
        assert_eq!(method.parameters[1].name.name, "value");
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
    fn parse_return_type_unary() {
        let module = parse_ok(
            "Actor subclass: Counter
  getBalance -> Integer => self.balance",
        );

        let method = &module.classes[0].methods[0];
        assert_eq!(method.selector.name(), "getBalance");
        assert!(method.return_type.is_some());
        let rt = method.return_type.as_ref().unwrap();
        match rt {
            crate::ast::TypeAnnotation::Simple(id) => assert_eq!(id.name, "Integer"),
            _ => panic!("Expected Simple type annotation"),
        }
    }

    #[test]
    fn parse_return_type_binary() {
        let module = parse_ok(
            "Object subclass: Number
  + other -> Number => self",
        );

        let method = &module.classes[0].methods[0];
        assert_eq!(method.selector.name(), "+");
        assert_eq!(method.parameters.len(), 1);
        assert_eq!(method.parameters[0].name.name, "other");
        assert!(method.return_type.is_some());
        match method.return_type.as_ref().unwrap() {
            crate::ast::TypeAnnotation::Simple(id) => assert_eq!(id.name, "Number"),
            _ => panic!("Expected Simple type annotation"),
        }
    }

    #[test]
    fn parse_return_type_union() {
        let module = parse_ok(
            "Actor subclass: Container
  find -> Integer | Nil => nil",
        );

        let method = &module.classes[0].methods[0];
        assert!(method.return_type.is_some());
        match method.return_type.as_ref().unwrap() {
            crate::ast::TypeAnnotation::Union { types, .. } => {
                assert_eq!(types.len(), 2);
            }
            _ => panic!("Expected Union type annotation"),
        }
    }

    #[test]
    fn parse_typed_keyword_param() {
        let module = parse_ok(
            "Actor subclass: BankAccount
  deposit: amount: Integer => self.balance := self.balance + amount",
        );

        let method = &module.classes[0].methods[0];
        assert_eq!(method.selector.name(), "deposit:");
        assert_eq!(method.parameters.len(), 1);
        assert_eq!(method.parameters[0].name.name, "amount");
        assert!(method.parameters[0].type_annotation.is_some());
        match method.parameters[0].type_annotation.as_ref().unwrap() {
            crate::ast::TypeAnnotation::Simple(id) => assert_eq!(id.name, "Integer"),
            _ => panic!("Expected Simple type annotation"),
        }
    }

    #[test]
    fn parse_typed_keyword_params_multiple() {
        let module = parse_ok(
            "Actor subclass: BankAccount
  transfer: amount: Integer to: target: BankAccount => self",
        );

        let method = &module.classes[0].methods[0];
        assert_eq!(method.selector.name(), "transfer:to:");
        assert_eq!(method.parameters.len(), 2);
        assert_eq!(method.parameters[0].name.name, "amount");
        assert!(method.parameters[0].type_annotation.is_some());
        assert_eq!(method.parameters[1].name.name, "target");
        assert!(method.parameters[1].type_annotation.is_some());
    }

    #[test]
    fn parse_typed_param_with_return_type() {
        let module = parse_ok(
            "Actor subclass: BankAccount
  deposit: amount: Integer -> Integer => self.balance := self.balance + amount",
        );

        let method = &module.classes[0].methods[0];
        assert_eq!(method.selector.name(), "deposit:");
        assert_eq!(method.parameters[0].name.name, "amount");
        assert!(method.parameters[0].type_annotation.is_some());
        assert!(method.return_type.is_some());
    }

    #[test]
    fn parse_untyped_keyword_param_unchanged() {
        // Existing syntax should still work unchanged
        let module = parse_ok(
            "Actor subclass: Counter
  setValue: v => self.value := v",
        );

        let method = &module.classes[0].methods[0];
        assert_eq!(method.selector.name(), "setValue:");
        assert_eq!(method.parameters.len(), 1);
        assert_eq!(method.parameters[0].name.name, "v");
        assert!(method.parameters[0].type_annotation.is_none());
        assert!(method.return_type.is_none());
    }

    #[test]
    fn parse_binary_typed_param() {
        let module = parse_ok(
            "Object subclass: Number
  + other: Number -> Number => self",
        );

        let method = &module.classes[0].methods[0];
        assert_eq!(method.selector.name(), "+");
        assert_eq!(method.parameters.len(), 1);
        assert_eq!(method.parameters[0].name.name, "other");
        assert!(method.parameters[0].type_annotation.is_some());
        assert!(method.return_type.is_some());
    }

    #[test]
    fn parse_mixed_typed_untyped_keyword_params() {
        // First param typed, second untyped
        let module = parse_ok(
            "Actor subclass: BankAccount
  transfer: amount: Integer to: target => self",
        );

        let method = &module.classes[0].methods[0];
        assert_eq!(method.selector.name(), "transfer:to:");
        assert_eq!(method.parameters.len(), 2);
        assert_eq!(method.parameters[0].name.name, "amount");
        assert!(method.parameters[0].type_annotation.is_some());
        assert_eq!(method.parameters[1].name.name, "target");
        assert!(method.parameters[1].type_annotation.is_none());
    }

    #[test]
    fn parse_keyword_typed_param_with_space_colon() {
        // Space around colon: `amount : Integer` instead of `amount: Integer`
        let module = parse_ok(
            "Actor subclass: BankAccount
  deposit: amount : Integer => self",
        );

        let method = &module.classes[0].methods[0];
        assert_eq!(method.selector.name(), "deposit:");
        assert_eq!(method.parameters.len(), 1);
        assert_eq!(method.parameters[0].name.name, "amount");
        assert!(method.parameters[0].type_annotation.is_some());
    }

    #[test]
    fn parse_binary_typed_param_with_space_colon() {
        // Space around colon: `other : Number` instead of `other: Number`
        let module = parse_ok(
            "Actor subclass: Adder
  + other : Number => self",
        );

        let method = &module.classes[0].methods[0];
        assert_eq!(method.selector.name(), "+");
        assert_eq!(method.parameters.len(), 1);
        assert_eq!(method.parameters[0].name.name, "other");
        assert!(method.parameters[0].type_annotation.is_some());
    }

    #[test]
    fn parse_malformed_return_type_recovers() {
        // `-> =>` (missing type name) should still detect the method definition
        // and let parse_type_annotation emit the error, not truncate class parsing
        let tokens = lex_with_eof(
            "Actor subclass: Counter
  increment -> => self",
        );
        let (module, diagnostics) = parse(tokens);
        // Should have an error about missing type name
        assert!(
            !diagnostics.is_empty(),
            "Expected error for missing type name after ->"
        );
        // But the method should still be parsed
        assert_eq!(module.classes.len(), 1);
        assert_eq!(module.classes[0].methods.len(), 1);
        assert_eq!(module.classes[0].methods[0].selector.name(), "increment");
    }

    #[test]
    fn parse_class_with_mixed_content() {
        let module = parse_ok(
            "Actor subclass: Counter
  state: value = 0
  state: name: String

  increment => self.value := self.value + 1
  decrement => self.value := self.value - 1",
        );

        assert_eq!(module.classes.len(), 1);
        let class = &module.classes[0];

        assert_eq!(class.state.len(), 2);
        assert_eq!(class.methods.len(), 2);

        // Check method kinds
        assert_eq!(class.methods[0].kind, crate::ast::MethodKind::Primary);
        assert_eq!(class.methods[1].kind, crate::ast::MethodKind::Primary);
    }

    #[test]
    fn parse_class_with_trailing_expression() {
        let module = parse_ok(
            "Object subclass: Foo
  state: count = 0
  count => self.count
Foo new count",
        );
        assert_eq!(module.classes.len(), 1, "Should have 1 class");
        assert_eq!(
            module.expressions.len(),
            1,
            "Should have 1 trailing expression, got {}",
            module.expressions.len()
        );
    }

    #[test]
    fn parse_class_multiline_method_body_no_false_break() {
        // Ensure indented multi-line method bodies are NOT broken prematurely
        let module = parse_ok(
            "Object subclass: Bar
  doStuff =>
    x := 1
    x + 2",
        );
        assert_eq!(module.classes.len(), 1);
        let class = &module.classes[0];
        assert_eq!(class.methods.len(), 1);
        // Method body should have both statements (x := 1 and x + 2)
        assert_eq!(
            class.methods[0].body.len(),
            2,
            "Multi-line indented method body should have 2 statements"
        );
        // No trailing expressions
        assert_eq!(module.expressions.len(), 0);
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
            &module.expressions[0].expression,
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
        } = &module.expressions[0].expression
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
        } = &module.expressions[0].expression
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
        } = &module.expressions[0].expression
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
        // Using Erlang comparison operators (ADR 0002)
        let expressions = vec![
            "1 =:= 2", "1 /= 2", "1 =/= 2", // ADR 0002: Erlang comparison operators
            "1 < 2", "1 > 2", "1 <= 2", "1 >= 2", "1 + 2", "1 - 2", "1 * 2", "1 / 2", "1 % 2",
        ];

        for expr in expressions {
            let module = parse_ok(expr);
            assert_eq!(module.expressions.len(), 1, "Failed for: {expr}");
            assert!(
                matches!(
                    &module.expressions[0].expression,
                    Expression::MessageSend { .. }
                ),
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
        } = &module.expressions[0].expression
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
        } = &module.expressions[0].expression
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
        } = &method.body[0].expression
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
        if let Expression::FieldAccess { receiver, .. } = &module.expressions[0].expression {
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
        } = &module.expressions[0].expression
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
        } = &module.expressions[0].expression
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
        if let Expression::ClassReference { name, .. } = &module1.expressions[0].expression {
            assert_eq!(name.name.as_str(), "Counter");
        } else {
            panic!("Expected ClassReference for 'Counter'");
        }

        // Lowercase should parse as Identifier
        let module2 = parse_ok("counter");
        if let Expression::Identifier(id) = &module2.expressions[0].expression {
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
        } = &module.expressions[0].expression
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
        if let Expression::Identifier(id) = &module.expressions[0].expression {
            assert_eq!(id.name.as_str(), "myVariable");
        } else {
            panic!("Expected Identifier for 'myVariable'");
        }

        // PascalCase should parse as ClassReference (starts with uppercase)
        let module = parse_ok("MyClass");
        if let Expression::ClassReference { name, .. } = &module.expressions[0].expression {
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
        let source = "Object subclass: Foo\n  + other => @primitive \"+\"";
        let module = parse_ok(source);
        assert_eq!(module.classes.len(), 1);
        let method = &module.classes[0].methods[0];
        assert_eq!(method.body.len(), 1);
        if let Expression::Primitive {
            name, is_quoted, ..
        } = &method.body[0].expression
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
        } = &method.body[0].expression
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
        let source = "Object subclass: MyInt\n  + other => @primitive \"+\"";
        let module = parse_ok(source);
        assert_eq!(module.classes.len(), 1);
        let method = &module.classes[0].methods[0];
        assert_eq!(method.body.len(), 1);
        assert!(matches!(
            &method.body[0].expression,
            Expression::Primitive {
                is_quoted: true,
                ..
            }
        ));
    }

    #[test]
    fn parse_primitive_with_fallback() {
        // @primitive followed by fallback code in method body
        let source = "Object subclass: MyInt\n  abs => @primitive \"abs\". self negated";
        let module = parse_ok(source);
        let method = &module.classes[0].methods[0];
        assert_eq!(method.body.len(), 2);
        assert!(matches!(
            &method.body[0].expression,
            Expression::Primitive { .. }
        ));
    }

    #[test]
    fn parse_primitive_structural_intrinsic() {
        let source = "Object subclass: MyObj\n  new => @primitive basicNew";
        let module = parse_ok(source);
        let method = &module.classes[0].methods[0];
        assert_eq!(method.body.len(), 1);
        if let Expression::Primitive {
            name, is_quoted, ..
        } = &method.body[0].expression
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
        let diagnostics = parse_err("@primitive \"+\"");
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
        let source = "Object subclass: Foo\n  m => [@primitive \"+\"]";
        let module = parse_ok(source);
        let method = &module.classes[0].methods[0];
        assert_eq!(method.body.len(), 1);
        // The body is a block containing the primitive
        if let Expression::Block(block) = &method.body[0].expression {
            assert_eq!(block.body.len(), 1);
            assert!(
                matches!(&block.body[0].expression, Expression::Primitive { .. }),
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
        match &method.body[0].expression {
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
        let source = "Object subclass: Foo\n  size => @intrinsic \"size\"";
        let module = parse_ok(source);
        let method = &module.classes[0].methods[0];
        assert_eq!(method.body.len(), 1);
        match &method.body[0].expression {
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
        let source = "Object subclass: Foo\n  + other => @primitive \"+\"\n  - other => @primitive \"-\"\n  * other => @primitive \"*\"";
        let module = parse_ok(source);
        assert_eq!(module.classes[0].methods.len(), 3);
        assert_eq!(module.classes[0].methods[0].selector.name(), "+");
        assert_eq!(module.classes[0].methods[1].selector.name(), "-");
        assert_eq!(module.classes[0].methods[2].selector.name(), "*");
    }

    #[test]
    fn parse_binary_methods_followed_by_unary() {
        let source = "Object subclass: Foo\n  + other => @primitive \"+\"\n  - other => @primitive \"-\"\n  negated => 0";
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
        match &module.expressions[0].expression {
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
        match &module.expressions[0].expression {
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
        match &module.expressions[0].expression {
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
        match &module.expressions[0].expression {
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
        match &module.expressions[0].expression {
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
        match &module.expressions[0].expression {
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
        let module = parse_ok("#(1, \"hello\", #ok)");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
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
        match &module.expressions[0].expression {
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
        match &module.expressions[0].expression {
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
        // A `//` comment between `///` and the declaration orphans the first
        // `///` block.  We now emit a warning for it.
        let tokens = lex_with_eof(
            "/// Orphaned doc comment.
// Regular comment interrupts.
/// Actual class doc.
Actor subclass: Counter
  increment => 1",
        );
        let (module, diagnostics) = parse(tokens);

        assert_eq!(module.classes.len(), 1);
        // Only the last consecutive block should be collected.
        assert_eq!(
            module.classes[0].doc_comment.as_deref(),
            Some("Actual class doc.")
        );
        // The orphaned `///` block must produce exactly one warning.
        let warnings: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Warning)
            .collect();
        assert_eq!(warnings.len(), 1);
        assert!(
            warnings[0].message.contains("not attached"),
            "unexpected warning message: {}",
            warnings[0].message,
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
        // A blank line between `///` and the declaration orphans the first
        // `///` block.  We now emit a warning for it.
        let tokens = lex_with_eof(
            "/// Orphaned doc comment.

/// Actual class doc.
Actor subclass: Counter
  increment => 1",
        );
        let (module, diagnostics) = parse(tokens);

        assert_eq!(module.classes.len(), 1);
        // Blank line separates the two doc blocks; only the last is attached.
        assert_eq!(
            module.classes[0].doc_comment.as_deref(),
            Some("Actual class doc.")
        );
        // The orphaned `///` block must produce exactly one warning.
        let warnings: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Warning)
            .collect();
        assert_eq!(warnings.len(), 1);
        assert!(
            warnings[0].message.contains("not attached"),
            "unexpected warning message: {}",
            warnings[0].message,
        );
    }

    // ── BT-980: unattached doc comment warnings ──────────────────────────────

    #[test]
    fn unattached_doc_comment_blank_line_before_class_emits_warning() {
        // `///` + blank line + class definition must produce a warning.
        let tokens = lex_with_eof(
            "/// This comment is separated by a blank line.

Object subclass: Foo
  size => 0",
        );
        let (_module, diagnostics) = parse(tokens);
        let warnings: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Warning)
            .collect();
        assert_eq!(
            warnings.len(),
            1,
            "expected exactly one warning, got: {diagnostics:?}"
        );
        assert!(warnings[0].message.contains("not attached"));
    }

    #[test]
    fn attached_doc_comment_no_blank_line_no_warning() {
        // `///` immediately before class definition must NOT produce a warning.
        let module = parse_ok(
            "/// This comment is attached.
Object subclass: Foo
  size => 0",
        );
        assert_eq!(
            module.classes[0].doc_comment.as_deref(),
            Some("This comment is attached.")
        );
    }

    #[test]
    fn unattached_doc_comment_before_expression_emits_warning() {
        // `///` before a non-declaration expression must produce a warning.
        let tokens = lex_with_eof("/// Orphan before assignment.\nx := 42");
        let (_module, diagnostics) = parse(tokens);
        let warnings: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Warning)
            .collect();
        assert_eq!(
            warnings.len(),
            1,
            "expected exactly one warning, got: {diagnostics:?}"
        );
        assert!(warnings[0].message.contains("not attached"));
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
        assert!(is_input_complete("array at: 1 put: \"hello\""));
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
        assert!(is_input_complete("#{name => \"Alice\", age => 30}"));
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
        assert!(is_input_complete("\"hello world\""));
    }

    #[test]
    fn complete_interpolated_string() {
        // Complete interpolated string
        assert!(is_input_complete("\"Hello, {name}!\""));
        // Unterminated interpolation
        assert!(!is_input_complete("\"Hello, {name"));
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
        assert!(!is_input_complete("#{name => \"Alice\""));
    }

    #[test]
    fn incomplete_unclosed_list() {
        assert!(!is_input_complete("#("));
        assert!(!is_input_complete("#(1, 2"));
    }

    #[test]
    fn incomplete_unterminated_string() {
        assert!(!is_input_complete("\"hello"));
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
        assert!(is_input_complete(
            "#{\n  name => \"Alice\",\n  age => 30\n}"
        ));
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

    #[test]
    fn incomplete_class_definition_header_only() {
        // Just the class header — user hasn't typed methods yet
        assert!(!is_input_complete("Actor subclass: Counter"));
    }

    #[test]
    fn incomplete_class_definition_with_state_no_methods() {
        // Class with state but no methods — still incomplete
        assert!(!is_input_complete(
            "Actor subclass: Counter\n  state: value = 0"
        ));
    }

    #[test]
    fn complete_class_definition_with_method() {
        // Class with at least one method — complete
        assert!(is_input_complete(
            "Actor subclass: Counter\n  state: value = 0\n  increment => self.value := self.value + 1"
        ));
    }

    #[test]
    fn complete_class_definition_with_multiple_methods() {
        assert!(is_input_complete(
            "Actor subclass: Counter\n  state: value = 0\n  increment => self.value := self.value + 1\n  getValue => ^self.value"
        ));
    }

    #[test]
    fn incomplete_class_definition_with_map_in_state() {
        // Map literals contain `=>` but should NOT count as method arrows
        assert!(!is_input_complete(
            "Object subclass: Config\n  state: opts = #{verbose => true}"
        ));
    }

    #[test]
    fn single_quoted_string_is_complete() {
        // Single-quoted strings are invalid (ADR 0023) but fully consumed by the
        // lexer — the REPL should NOT wait for more input.
        assert!(is_input_complete("'hello'"));
        assert!(is_input_complete("Transcript show: 'hello'"));
    }

    #[test]
    fn unknown_directive_is_complete() {
        // Unknown directives like @foo are invalid but fully consumed
        assert!(is_input_complete("@foo"));
    }

    // === Match expression parsing ===

    #[test]
    fn parse_simple_match() {
        let module = parse_ok("42 match: [_ -> 99]");
        assert_eq!(module.expressions.len(), 1);
        assert!(
            matches!(&module.expressions[0].expression, Expression::Match { arms, .. } if arms.len() == 1)
        );
    }

    #[test]
    fn parse_match_with_semicolons() {
        let module = parse_ok("x match: [1 -> \"one\"; 2 -> \"two\"; _ -> \"other\"]");
        assert_eq!(module.expressions.len(), 1);
        assert!(
            matches!(&module.expressions[0].expression, Expression::Match { arms, .. } if arms.len() == 3)
        );
    }

    #[test]
    fn parse_match_with_tuple_pattern() {
        let module = parse_ok("r match: [{#ok, v} -> v; {#error, _} -> nil]");
        assert_eq!(module.expressions.len(), 1);
        assert!(
            matches!(&module.expressions[0].expression, Expression::Match { arms, .. } if arms.len() == 2)
        );
    }

    #[test]
    fn parse_match_with_guard() {
        let module = parse_ok("n match: [x when: [x > 0] -> x; _ -> 0]");
        assert_eq!(module.expressions.len(), 1);
        if let Expression::Match { arms, .. } = &module.expressions[0].expression {
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
        let expr = &module.expressions[0].expression;
        let result = crate::codegen::core_erlang::generate_test_expression(expr, "test_match");
        assert!(result.is_ok(), "Codegen failed: {:?}", result.err());
        let code = result.unwrap();
        eprintln!("Generated code:\n{code}");
        assert!(code.contains("case"), "Expected case expression in: {code}");
    }

    #[test]
    fn codegen_match_with_arms() {
        let module = parse_ok("1 match: [1 -> \"one\"; 2 -> \"two\"; _ -> \"other\"]");
        let expr = &module.expressions[0].expression;
        let result = crate::codegen::core_erlang::generate_test_expression(expr, "test_match");
        assert!(result.is_ok(), "Codegen failed: {:?}", result.err());
        let code = result.unwrap();
        eprintln!("Generated code:\n{code}");
        assert!(code.contains("case"), "Expected case expression in: {code}");
    }

    #[test]
    fn codegen_empty_match_errors() {
        let module = parse_ok("42 match: []");
        let expr = &module.expressions[0].expression;
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

    #[test]
    fn parse_exponentiation_operator() {
        // BT-414: `**` is a binary operator
        let module = parse_ok("2 ** 10");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
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
                assert_eq!(op.as_str(), "**");
                assert_eq!(arguments.len(), 1);
                assert!(matches!(
                    arguments[0],
                    Expression::Literal(Literal::Integer(10), _)
                ));
            }
            _ => panic!("Expected binary message send with **"),
        }
    }

    #[test]
    fn parse_exponentiation_higher_precedence_than_multiply() {
        // BT-414: `3 * 2 ** 4` should be `3 * (2 ** 4)`
        let module = parse_ok("3 * 2 ** 4");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
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
                assert_eq!(op.as_str(), "*");
                // The argument should be (2 ** 4)
                match &arguments[0] {
                    Expression::MessageSend {
                        receiver: r2,
                        selector: MessageSelector::Binary(op2),
                        arguments: args2,
                        ..
                    } => {
                        assert!(matches!(**r2, Expression::Literal(Literal::Integer(2), _)));
                        assert_eq!(op2.as_str(), "**");
                        assert!(matches!(
                            args2[0],
                            Expression::Literal(Literal::Integer(4), _)
                        ));
                    }
                    _ => panic!("Expected ** as right operand of *"),
                }
            }
            _ => panic!("Expected * at top level"),
        }
    }

    #[test]
    fn parse_exponentiation_right_associative() {
        // BT-414: `2 ** 3 ** 2` should be `2 ** (3 ** 2)` (right-associative)
        let module = parse_ok("2 ** 3 ** 2");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
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
                assert_eq!(op.as_str(), "**");
                // The argument should be (3 ** 2), NOT ((2 ** 3) ** 2)
                match &arguments[0] {
                    Expression::MessageSend {
                        receiver: r2,
                        selector: MessageSelector::Binary(op2),
                        arguments: args2,
                        ..
                    } => {
                        assert!(matches!(**r2, Expression::Literal(Literal::Integer(3), _)));
                        assert_eq!(op2.as_str(), "**");
                        assert!(matches!(
                            args2[0],
                            Expression::Literal(Literal::Integer(2), _)
                        ));
                    }
                    _ => panic!("Expected ** as right operand (right-associative)"),
                }
            }
            _ => panic!("Expected ** at top level"),
        }
    }

    // ========================================================================
    // BT-571: Standalone Method Definition Tests
    // ========================================================================

    #[test]
    fn standalone_method_definition_unary() {
        let module = parse_ok("Counter >> increment => self.value := self.value + 1");
        assert!(module.classes.is_empty());
        assert!(module.expressions.is_empty());
        assert_eq!(module.method_definitions.len(), 1);
        let method_def = &module.method_definitions[0];
        assert_eq!(method_def.class_name.name.as_str(), "Counter");
        assert!(!method_def.is_class_method);
        assert_eq!(method_def.method.selector.name().as_str(), "increment");
    }

    #[test]
    fn standalone_method_definition_keyword() {
        let module = parse_ok("Counter >> setValue: v => self.value := v");
        assert_eq!(module.method_definitions.len(), 1);
        let method_def = &module.method_definitions[0];
        assert_eq!(method_def.class_name.name.as_str(), "Counter");
        assert_eq!(method_def.method.selector.name().as_str(), "setValue:");
        assert_eq!(method_def.method.parameters.len(), 1);
    }

    #[test]
    fn standalone_method_definition_binary() {
        let module = parse_ok("Point >> + other => self x + (other x)");
        assert_eq!(module.method_definitions.len(), 1);
        let method_def = &module.method_definitions[0];
        assert_eq!(method_def.class_name.name.as_str(), "Point");
        assert_eq!(method_def.method.selector.name().as_str(), "+");
    }

    #[test]
    fn standalone_method_definition_class_side() {
        let module = parse_ok("Counter class >> withInitial: n => self spawnWith: #{value => n}");
        assert_eq!(module.method_definitions.len(), 1);
        let method_def = &module.method_definitions[0];
        assert_eq!(method_def.class_name.name.as_str(), "Counter");
        assert!(method_def.is_class_method);
        assert_eq!(method_def.method.selector.name().as_str(), "withInitial:");
    }

    #[test]
    fn standalone_method_definition_with_class_def() {
        let source = "Actor subclass: Counter\n  state: value = 0\n\nCounter >> increment => self.value := self.value + 1";
        let module = parse_ok(source);
        assert_eq!(module.classes.len(), 1);
        assert_eq!(module.method_definitions.len(), 1);
        assert_eq!(module.classes[0].name.name.as_str(), "Counter");
        assert_eq!(
            module.method_definitions[0].class_name.name.as_str(),
            "Counter"
        );
    }

    #[test]
    fn multiple_standalone_method_definitions() {
        let source = "Counter >> increment => self.value := self.value + 1\nCounter >> getValue => ^self.value";
        let module = parse_ok(source);
        assert_eq!(module.method_definitions.len(), 2);
        assert_eq!(
            module.method_definitions[0].method.selector.name().as_str(),
            "increment"
        );
        assert_eq!(
            module.method_definitions[1].method.selector.name().as_str(),
            "getValue"
        );
    }

    #[test]
    fn method_lookup_not_confused_with_method_definition() {
        // Counter >> #increment is method lookup, not method definition
        let module = parse_ok("Counter >> #increment");
        assert!(module.method_definitions.is_empty());
        assert_eq!(module.expressions.len(), 1);
    }

    #[test]
    fn deeply_nested_parens_does_not_stack_overflow() {
        // 300 levels of nesting exceeds MAX_NESTING_DEPTH (64)
        let source = "(".repeat(300) + "1" + &")".repeat(300);
        let diagnostics = parse_err(&source);
        assert!(
            diagnostics.iter().any(|d| d.message.contains("nesting")),
            "Expected nesting depth error, got: {diagnostics:?}"
        );
    }

    #[test]
    fn deeply_chained_assignments_does_not_stack_overflow() {
        // 300 chained assignments: a := a := a := ... := 1
        let source = "a := ".repeat(300) + "1";
        let diagnostics = parse_err(&source);
        assert!(
            diagnostics.iter().any(|d| d.message.contains("nesting")),
            "Expected nesting depth error, got: {diagnostics:?}"
        );
    }

    #[test]
    fn deeply_nested_blocks_does_not_stack_overflow() {
        // 300 nested blocks: [[[...1...]]]
        let source = "[".repeat(300) + "1" + &"]".repeat(300);
        let diagnostics = parse_err(&source);
        assert!(
            diagnostics.iter().any(|d| d.message.contains("nesting")),
            "Expected nesting depth error, got: {diagnostics:?}"
        );
    }

    #[test]
    fn deeply_nested_match_patterns_does_not_stack_overflow() {
        // Nested tuple patterns in match: x match: [{{{...}}} => 1]
        let source =
            "x match: [".to_string() + &"{".repeat(300) + "a" + &"}".repeat(300) + " => 1]";
        let diagnostics = parse_err(&source);
        assert!(
            diagnostics.iter().any(|d| d.message.contains("nesting")),
            "Expected nesting depth error, got: {diagnostics:?}"
        );
    }

    #[test]
    fn deeply_nested_string_interpolation_does_not_stack_overflow() {
        // Each nesting level needs a real string-in-interpolation: "x {"y {"z {1} z"} y"} x"
        // The lexer only produces StringStart inside {..."..."...} sequences.
        // Build: "{"{"{ ... 1 ... "}"}"}"
        let mut source = String::new();
        for _ in 0..300 {
            source.push_str("\"a{");
        }
        source.push('1');
        for _ in 0..300 {
            source.push_str("}a\"");
        }
        // This input may or may not trigger the nesting guard depending on
        // lexer behavior, but it must never stack overflow.
        let tokens = crate::source_analysis::lex_with_eof(&source);
        let (_module, _diagnostics) = crate::source_analysis::parse(tokens);
        // Success = no panic/stack overflow
    }

    #[test]
    fn unclosed_deeply_nested_blocks_does_not_oom() {
        // Fuzz regression: 158 unclosed '[' caused OOM because the
        // nesting guard returned Error without consuming tokens and the
        // parse_block body loop spun infinitely.
        let source = "[".repeat(158);
        let diagnostics = parse_err(&source);
        assert!(
            diagnostics.iter().any(|d| d.message.contains("nesting")),
            "Expected nesting depth error, got: {diagnostics:?}"
        );
    }

    #[test]
    fn hash_newline_brace_does_not_stack_overflow() {
        // Fuzz regression: "#\n{" caused infinite mutual recursion
        // between parse_primary (Hash+LeftBrace lookahead) and
        // parse_map_literal (expected MapOpen, got Hash).
        let diagnostics = parse_err("#\n{");
        assert!(
            diagnostics
                .iter()
                .any(|d| d.message.contains("Unexpected '#'")),
            "Expected unexpected '#' error, got: {diagnostics:?}"
        );
    }

    #[test]
    fn unclosed_nested_blocks_in_method_body_does_not_oom() {
        // Fuzz regression: deeply nested unclosed '[' inside a method
        // body could spin the parse_method_body loop infinitely because
        // synchronize() can return without advancing when in_method_body.
        let source = format!("Object subclass: Foo\n  bar => {}", "[".repeat(100));
        let diagnostics = parse_err(&source);
        assert!(
            diagnostics.iter().any(|d| d.message.contains("nesting")),
            "Expected nesting depth error, got: {diagnostics:?}"
        );
    }

    #[test]
    fn parse_typed_class() {
        let module = parse_ok(
            "typed Actor subclass: StrictCounter
  state: value: Integer = 0

  increment -> Integer => self.value := self.value + 1",
        );

        assert_eq!(module.classes.len(), 1);
        let class = &module.classes[0];

        assert!(!class.is_abstract);
        assert!(!class.is_sealed);
        assert!(class.is_typed);
        assert_eq!(class.name.name, "StrictCounter");
        assert_eq!(class.state.len(), 1);
        assert_eq!(class.methods.len(), 1);
    }

    #[test]
    fn parse_typed_sealed_class() {
        let module = parse_ok(
            "typed sealed Actor subclass: ImmutablePoint
  state: x: Integer = 0
  state: y: Integer = 0",
        );

        let class = &module.classes[0];
        assert!(class.is_typed);
        assert!(class.is_sealed);
        assert_eq!(class.name.name, "ImmutablePoint");
    }

    #[test]
    fn parse_abstract_typed_class() {
        let module = parse_ok(
            "abstract typed Actor subclass: Shape
  area -> Float => ^0.0",
        );

        let class = &module.classes[0];
        assert!(class.is_abstract);
        assert!(class.is_typed);
        assert_eq!(class.name.name, "Shape");
    }

    #[test]
    fn parse_non_typed_class_defaults_false() {
        let module = parse_ok(
            "Actor subclass: Counter
  state: value = 0",
        );

        let class = &module.classes[0];
        assert!(!class.is_typed);
    }

    // ========================================================================
    // BT-919: Cast (!) statement terminator tests
    // ========================================================================

    #[test]
    fn parse_bang_marks_message_send_as_cast() {
        // `foo bar!` should parse as a MessageSend with is_cast = true
        let module = parse_ok("foo bar!");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::MessageSend {
                is_cast,
                selector: MessageSelector::Unary(name),
                ..
            } => {
                assert_eq!(name.as_str(), "bar");
                assert!(
                    is_cast,
                    "Expected is_cast = true for bang-terminated message"
                );
            }
            other => panic!("Expected MessageSend, got: {other:?}"),
        }
    }

    #[test]
    fn parse_period_keeps_message_send_as_call() {
        // `foo bar.` should parse as a MessageSend with is_cast = false
        let module = parse_ok("foo bar.");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::MessageSend { is_cast, .. } => {
                assert!(
                    !is_cast,
                    "Expected is_cast = false for period-terminated message"
                );
            }
            other => panic!("Expected MessageSend, got: {other:?}"),
        }
    }

    #[test]
    fn parse_bang_keyword_message_is_cast() {
        // `obj doSomething: 42!` should mark the keyword send as cast
        let module = parse_ok("obj doSomething: 42!");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::MessageSend {
                is_cast,
                selector: MessageSelector::Keyword(_),
                ..
            } => {
                assert!(is_cast, "Expected keyword message with ! to be a cast");
            }
            other => panic!("Expected keyword MessageSend, got: {other:?}"),
        }
    }

    #[test]
    fn parse_bang_in_expression_context_is_error() {
        // `x := foo bar!` — cast has no return value, can't be used in an expression
        let diagnostics = parse_err("x := foo bar!");
        assert!(
            !diagnostics.is_empty(),
            "Expected error for cast in expression context"
        );
        assert!(
            diagnostics[0]
                .message
                .contains("Cast (!) has no return value"),
            "Expected cast-in-expression error, got: {}",
            diagnostics[0].message
        );
    }

    #[test]
    fn parse_multiple_statements_with_bang() {
        // `foo bar! baz qux.` — two statements: first cast, second call
        let module = parse_ok("foo bar!\nbaz qux.");
        assert_eq!(module.expressions.len(), 2);
        match &module.expressions[0].expression {
            Expression::MessageSend { is_cast, .. } => assert!(is_cast),
            other => panic!("Expected cast MessageSend, got: {other:?}"),
        }
        match &module.expressions[1].expression {
            Expression::MessageSend { is_cast, .. } => assert!(!is_cast),
            other => panic!("Expected call MessageSend, got: {other:?}"),
        }
    }

    #[test]
    fn parse_bang_binary_message_is_cast() {
        // `3 + 4!` — binary message send with cast
        let module = parse_ok("3 + 4!");
        assert_eq!(module.expressions.len(), 1);
        match &module.expressions[0].expression {
            Expression::MessageSend {
                is_cast,
                selector: MessageSelector::Binary(op),
                ..
            } => {
                assert_eq!(op.as_str(), "+");
                assert!(is_cast, "Expected binary message with ! to be a cast");
            }
            other => panic!("Expected binary MessageSend, got: {other:?}"),
        }
    }

    #[test]
    fn parse_bang_in_block_body() {
        // `[foo bar!]` — cast inside a block body
        let module = parse_ok("[foo bar!]");
        assert_eq!(module.expressions.len(), 1);
        if let Expression::Block(block) = &module.expressions[0].expression {
            assert_eq!(block.body.len(), 1);
            match &block.body[0].expression {
                Expression::MessageSend { is_cast, .. } => {
                    assert!(is_cast, "Expected cast inside block body");
                }
                other => panic!("Expected MessageSend in block, got: {other:?}"),
            }
        } else {
            panic!("Expected Block expression");
        }
    }

    #[test]
    fn parse_return_with_bang_is_error() {
        // `^foo bar!` — can't return a cast (no return value)
        let diagnostics = parse_err("^foo bar!");
        assert!(!diagnostics.is_empty(), "Expected error for return of cast");
        assert!(
            diagnostics[0]
                .message
                .contains("Cast (!) has no return value"),
            "Expected cast error for return, got: {}",
            diagnostics[0].message
        );
    }

    #[test]
    fn parse_cascade_with_bang_is_error() {
        // `obj msg1; msg2!` — cascade is not a MessageSend, so ! produces error
        // (Cascade + cast semantics are deferred to a future issue)
        let diagnostics = parse_err("obj msg1; msg2!");
        assert!(
            !diagnostics.is_empty(),
            "Expected error for cascade with bang"
        );
    }

    // =========================================================================
    // BT-948: Unnecessary period separator warnings
    // =========================================================================

    /// Helper to extract only lint diagnostics from a parse.
    fn parse_lints(source: &str) -> Vec<Diagnostic> {
        let tokens = lex_with_eof(source);
        let (_module, diagnostics) = parse(tokens);
        diagnostics
            .into_iter()
            .filter(|d| d.severity == Severity::Lint)
            .collect()
    }

    #[test]
    fn trailing_period_before_bracket_in_block_emits_lint() {
        // `[ foo bar. ]` — trailing period before `]` is redundant
        let lints = parse_lints("x := [ foo bar. ]");
        assert_eq!(lints.len(), 1, "expected one lint, got: {lints:?}");
        assert!(
            lints[0].message.contains("trailing"),
            "message: {}",
            lints[0].message
        );
        assert_eq!(lints[0].severity, Severity::Lint);
        assert!(lints[0].hint.is_some());
    }

    #[test]
    fn period_before_newline_in_block_emits_lint() {
        // `[ foo bar.\n  baz ]` — period before newline is redundant
        let lints = parse_lints("x := [\n  foo bar.\n  baz\n]");
        assert_eq!(lints.len(), 1, "expected one lint, got: {lints:?}");
        assert!(
            lints[0].message.contains("newline"),
            "message: {}",
            lints[0].message
        );
        assert_eq!(lints[0].severity, Severity::Lint);
        assert!(lints[0].hint.is_some());
    }

    #[test]
    fn period_between_statements_same_line_in_block_no_lint() {
        // `[ foo bar. baz quux ]` — period on same line is the separator, no lint
        let lints = parse_lints("x := [ foo bar. baz quux ]");
        assert!(lints.is_empty(), "expected no lints, got: {lints:?}");
    }

    #[test]
    fn trailing_period_at_end_of_method_emits_lint() {
        // `increment => self.n := self.n + 1.` — trailing period after last statement
        let lints = parse_lints(
            "Object subclass: Counter\n  state: n = 0\n  increment => self.n := self.n + 1.\n",
        );
        assert_eq!(lints.len(), 1, "expected one lint, got: {lints:?}");
        assert!(
            lints[0].message.contains("end of method"),
            "message: {}",
            lints[0].message
        );
        assert_eq!(lints[0].severity, Severity::Lint);
    }

    #[test]
    fn period_before_newline_in_method_body_emits_lint() {
        // Two statements in a method body with an explicit period before newline
        let lints = parse_lints("Object subclass: Foo\n  go =>\n    x := 1.\n    x + 2\n");
        assert_eq!(lints.len(), 1, "expected one lint, got: {lints:?}");
        assert!(
            lints[0].message.contains("newline"),
            "message: {}",
            lints[0].message
        );
    }

    #[test]
    fn period_between_same_line_statements_in_method_no_lint() {
        // `go => foo bar. baz quux` — same-line separator, needed
        let lints = parse_lints("Object subclass: Foo\n  go => foo bar. baz quux\n");
        assert!(lints.is_empty(), "expected no lints, got: {lints:?}");
    }

    #[test]
    fn module_level_period_does_not_lint() {
        // At module level, periods are optional statement terminators — no lint
        let lints = parse_lints("foo bar.\nbaz quux.");
        assert!(
            lints.is_empty(),
            "expected no lints at module level, got: {lints:?}"
        );
    }

    // ========================================================================
    // Comment attachment tests (BT-975)
    // ========================================================================

    #[test]
    fn class_line_comment_attached_to_leading() {
        // A `//` comment immediately before a class definition should appear in
        // `ClassDefinition.comments.leading`.
        let module = parse_ok("// A useful class\nObject subclass: Foo\n");
        assert_eq!(module.classes.len(), 1);
        let class = &module.classes[0];
        assert_eq!(
            class.comments.leading.len(),
            1,
            "expected one leading comment"
        );
        assert_eq!(class.comments.leading[0].kind, CommentKind::Line);
        assert!(
            class.comments.leading[0].content.contains("A useful class"),
            "content: {}",
            class.comments.leading[0].content
        );
    }

    #[test]
    fn class_doc_comment_not_duplicated_in_attachment() {
        // A `///` doc comment must NOT appear in `comments.leading`; it goes to
        // `doc_comment` only.
        let module = parse_ok("/// Doc text\nObject subclass: Foo\n");
        assert_eq!(module.classes.len(), 1);
        let class = &module.classes[0];
        assert!(
            class.doc_comment.is_some(),
            "expected doc_comment to be populated"
        );
        assert!(
            class.comments.leading.is_empty(),
            "doc comment must not be duplicated into leading comments"
        );
    }

    #[test]
    fn method_mixed_doc_and_line_comment_separated() {
        // `//` goes to `comments.leading`, `///` goes to `doc_comment`.
        // The `//` must appear BEFORE `///` so the doc comment collection is
        // not reset (a `//` after `///` would reset the doc-comment buffer).
        let source = "Object subclass: Foo\n  // Line comment\n  /// Doc comment\n  go => 42\n";
        let module = parse_ok(source);
        assert_eq!(module.classes.len(), 1);
        let method = &module.classes[0].methods[0];
        assert!(
            method.doc_comment.is_some(),
            "expected doc_comment on method"
        );
        assert_eq!(
            method.comments.leading.len(),
            1,
            "expected one leading comment on method"
        );
        assert_eq!(method.comments.leading[0].kind, CommentKind::Line);
    }

    #[test]
    fn state_declaration_doc_comment_populated() {
        // A `///` doc comment before a state declaration is captured in `doc_comment`.
        let source = "Object subclass: Foo\n  /// The count\n  state: count = 0\n";
        let module = parse_ok(source);
        assert_eq!(module.classes.len(), 1);
        let state = &module.classes[0].state[0];
        assert!(
            state.doc_comment.is_some(),
            "expected doc_comment on state declaration"
        );
        assert!(
            state
                .doc_comment
                .as_deref()
                .unwrap_or("")
                .contains("The count"),
            "doc_comment: {:?}",
            state.doc_comment
        );
    }

    #[test]
    fn state_declaration_line_comment_attached() {
        // A `//` comment before `state:` is attached to `comments.leading`.
        let source = "Object subclass: Foo\n  // The x field\n  state: x = 1\n";
        let module = parse_ok(source);
        assert_eq!(module.classes.len(), 1);
        let state = &module.classes[0].state[0];
        assert_eq!(
            state.comments.leading.len(),
            1,
            "expected one leading comment on state declaration"
        );
        assert!(
            state.comments.leading[0].content.contains("The x field"),
            "content: {}",
            state.comments.leading[0].content
        );
    }

    // ========================================================================
    // ExpressionStatement comment attachment tests (BT-976)
    // ========================================================================

    #[test]
    fn expression_statement_leading_comment_in_method_body() {
        // A `//` comment between two statements in a method body must appear as a
        // leading comment on the second ExpressionStatement.
        let source = "Object subclass: Foo\n  go =>\n    x := 1.\n    // Step two\n    y := 2\n";
        let module = parse_ok(source);
        assert_eq!(module.classes.len(), 1);
        let body = &module.classes[0].methods[0].body;
        assert_eq!(body.len(), 2, "expected 2 statements");
        // First statement has no leading comment
        assert!(
            body[0].comments.leading.is_empty(),
            "first statement should have no leading comment"
        );
        // Second statement has the `// Step two` comment as leading
        assert_eq!(
            body[1].comments.leading.len(),
            1,
            "expected one leading comment on second statement"
        );
        assert!(
            body[1].comments.leading[0].content.contains("Step two"),
            "content: {}",
            body[1].comments.leading[0].content
        );
    }

    #[test]
    fn expression_statement_trailing_comment() {
        // A `// comment` on the same line as a statement must appear as `trailing`
        // on that ExpressionStatement.
        let source = "Object subclass: Foo\n  go => x := 1 // inline note\n";
        let module = parse_ok(source);
        assert_eq!(module.classes.len(), 1);
        let body = &module.classes[0].methods[0].body;
        assert_eq!(body.len(), 1, "expected 1 statement");
        assert!(
            body[0].comments.trailing.is_some(),
            "expected a trailing comment"
        );
        assert!(
            body[0]
                .comments
                .trailing
                .as_ref()
                .unwrap()
                .content
                .contains("inline note"),
            "trailing content: {:?}",
            body[0].comments.trailing
        );
    }

    #[test]
    fn empty_module_file_leading_comments_populated() {
        // A `.bt` file containing only comments and no items must populate
        // `Module.file_leading_comments`.
        let source = "// First comment\n// Second comment\n";
        let (module, diagnostics) = parse(lex_with_eof(source));
        assert!(diagnostics.is_empty(), "expected no diagnostics");
        assert!(module.classes.is_empty());
        assert!(module.expressions.is_empty());
        assert_eq!(
            module.file_leading_comments.len(),
            2,
            "expected 2 file-level comments"
        );
        assert!(
            module.file_leading_comments[0].content.contains("First"),
            "content: {}",
            module.file_leading_comments[0].content
        );
        assert!(
            module.file_leading_comments[1].content.contains("Second"),
            "content: {}",
            module.file_leading_comments[1].content
        );
    }

    #[test]
    fn non_empty_module_file_leading_comments_empty() {
        // In a non-empty module, file-level leading comments attach to the first
        // item — `file_leading_comments` must remain empty.
        let source = "// File comment\nx := 42\n";
        let (module, diagnostics) = parse(lex_with_eof(source));
        assert!(diagnostics.is_empty(), "expected no diagnostics");
        assert!(
            module.file_leading_comments.is_empty(),
            "file_leading_comments should be empty for non-empty module"
        );
        assert_eq!(module.expressions.len(), 1);
        // The comment attaches to the first expression's ExpressionStatement
        assert_eq!(
            module.expressions[0].comments.leading.len(),
            1,
            "expected leading comment on first expression"
        );
        assert!(
            module.expressions[0].comments.leading[0]
                .content
                .contains("File comment"),
            "content: {}",
            module.expressions[0].comments.leading[0].content
        );
    }

    #[test]
    fn block_body_expression_statement_leading_comment() {
        // A `//` comment between statements in a block body attaches as leading
        // on the following ExpressionStatement in the block.
        let source = "Object subclass: Foo\n  go =>\n    [:each |\n      // Transform\n      each asUppercase]\n";
        let module = parse_ok(source);
        assert_eq!(module.classes.len(), 1);
        let body = &module.classes[0].methods[0].body;
        assert_eq!(body.len(), 1, "expected 1 statement in method body");
        // The statement is a block expression
        let Expression::Block(block) = &body[0].expression else {
            panic!("expected Block expression, got {:?}", body[0].expression);
        };
        assert_eq!(block.body.len(), 1, "expected 1 statement in block");
        assert_eq!(
            block.body[0].comments.leading.len(),
            1,
            "expected leading comment on block statement"
        );
        assert!(
            block.body[0].comments.leading[0]
                .content
                .contains("Transform"),
            "content: {}",
            block.body[0].comments.leading[0].content
        );
    }

    #[test]
    fn standalone_method_leading_comment_attached() {
        // A `//` comment before a standalone method definition (`ClassName >> ...`)
        // must appear as a leading comment on the MethodDefinition.
        // Regression test: previously parse_standalone_method_definition() dropped
        // comments from the class-name token's leading trivia.
        let source = "// Note about Counter\nCounter >> increment => self.n := self.n + 1\n";
        let (module, diagnostics) = parse(lex_with_eof(source));
        assert!(
            diagnostics.is_empty(),
            "expected no diagnostics: {diagnostics:?}"
        );
        assert_eq!(module.method_definitions.len(), 1);
        let method = &module.method_definitions[0].method;
        assert_eq!(
            method.comments.leading.len(),
            1,
            "expected one leading comment on standalone method"
        );
        assert!(
            method.comments.leading[0]
                .content
                .contains("Note about Counter"),
            "content: {}",
            method.comments.leading[0].content
        );
        assert!(module.file_leading_comments.is_empty());
    }
}
