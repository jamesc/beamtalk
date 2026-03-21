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
use crate::ast::{CommentKind, Literal};
use crate::source_analysis::{Span, Token, TokenKind, Trivia, lex_with_eof};
use ecow::EcoString;

// Submodules with additional impl blocks for Parser
mod declarations;
mod expressions;

use declarations::DoubleColonSkip;

// Property-based tests (ADR 0011 Phase 2)
#[cfg(test)]
mod property_tests;

// Unit tests (extracted from mod.rs for maintainability)
#[cfg(test)]
mod tests;

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
/// | 3  | `->`                | Left |
/// | 5  | `>>`                | Left |
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
        // Association creation: `key -> value` (ADR 0047, lowest binary precedence)
        "->" => Some(BindingPower::left_assoc(3)),

        // Method lookup
        // `Counter >> #increment` returns CompiledMethod object
        ">>" => Some(BindingPower::left_assoc(5)),

        // Equality (ADR 0002: Erlang comparison operators)
        // `=:=` strict equality, `=/=` strict inequality
        // `/=` loose inequality, `==` loose equality
        "==" | "/=" | "=:=" | "=/=" => Some(BindingPower::left_assoc(10)),

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
    let mut binary_depth: i32 = 0; // << >>
    let mut last_meaningful_kind: Option<&TokenKind> = None;
    let mut has_subclass_keyword = false;
    let mut has_protocol_define = false;
    let mut has_method_arrow = false;
    let mut has_thin_arrow = false;

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
            TokenKind::LtLt => binary_depth += 1,

            // Closing delimiters
            TokenKind::RightBracket => bracket_depth -= 1,
            TokenKind::RightParen => paren_depth -= 1,
            TokenKind::RightBrace => brace_depth -= 1,
            TokenKind::GtGt if binary_depth > 0 => binary_depth -= 1,

            // Track class definition pattern
            TokenKind::Keyword(k) if k == "subclass:" => has_subclass_keyword = true,
            // Track protocol definition pattern (Protocol define: Name ...)
            TokenKind::Keyword(k) if k == "define:" => {
                if matches!(last_meaningful_kind, Some(TokenKind::Identifier(name)) if name == "Protocol")
                {
                    has_protocol_define = true;
                }
            }
            // Only count `=>` as a method arrow when not nested inside delimiters
            // (e.g., `#{key => value}` in state initializers should not count)
            TokenKind::FatArrow if bracket_depth == 0 && paren_depth == 0 && brace_depth == 0 => {
                has_method_arrow = true;
            }
            // Track `->` for protocol method signatures (e.g., `greet -> String`)
            TokenKind::Arrow if bracket_depth == 0 && paren_depth == 0 && brace_depth == 0 => {
                has_thin_arrow = true;
            }

            TokenKind::Eof => break,
            _ => {}
        }

        if !kind.is_eof() {
            last_meaningful_kind = Some(kind);
        }
    }

    // Unclosed delimiters
    if bracket_depth > 0 || paren_depth > 0 || brace_depth > 0 || binary_depth > 0 {
        return false;
    }

    // Trailing keyword missing its argument (e.g., "array at:" or "x ifTrue:")
    if let Some(TokenKind::Keyword(_)) = last_meaningful_kind {
        return false;
    }

    // Trailing binary operator missing its right operand (e.g., "1 +" or "3 *" or "x ->")
    // GtGt trailing: "Counter >>" is incomplete (method lookup or binary op)
    if matches!(
        last_meaningful_kind,
        Some(TokenKind::BinarySelector(_) | TokenKind::Arrow | TokenKind::GtGt)
    ) {
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

    // Protocol definition: incomplete until at least one method signature is defined.
    // "Protocol define: Greetable" alone is incomplete — waiting for method sigs.
    // "Protocol define: Greetable\n  greet -> String" is complete.
    if has_protocol_define && !has_thin_arrow {
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
    /// Dead assignment in block closure (BT-1476).
    DeadAssignment,
    /// Duplicate extension method conflict (BT-1516).
    ExtensionConflict,
    /// Deprecation warning (BT-1529) — wrong keyword/class-kind combination.
    ///
    /// Excluded from `--warnings-as-errors` to allow gradual migration.
    Deprecation,
    /// Actor-new error (BT-1559) — using `new`/`new:` on an Actor subclass.
    ActorNew,
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

    /// Attaches an optional hint for how to fix the issue.
    #[must_use]
    pub fn with_hint(mut self, hint: impl Into<EcoString>) -> Self {
        self.hint = Some(hint.into());
        self
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
        let mut protocols = Vec::new();
        let mut expressions = Vec::new();

        // Parse statements until EOF.
        // Leading comments on each expression are collected by collect_comment_attachment()
        // immediately before parse_expression().  Class and standalone-method definitions
        // collect their own comments inside parse_class_definition() /
        // parse_standalone_method_definition() via the same helper.
        while !self.is_at_end() {
            // Check if this looks like a protocol definition
            if self.is_at_protocol_definition() {
                let protocol = self.parse_protocol_definition();
                protocols.push(protocol);
            }
            // Check if this looks like a class definition
            else if self.is_at_class_definition() {
                let class = self.parse_class_definition();
                classes.push(class);
            } else if self.is_at_standalone_method_definition() {
                let method_def = self.parse_standalone_method_definition();
                method_definitions.push(method_def);
            } else {
                let pos_before = self.current;
                // BT-987: detect blank lines (2+ newlines) before this statement
                let has_blank_line = !expressions.is_empty()
                    && self.current_token().has_blank_line_before_first_comment();
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
        let (file_leading_comments, file_trailing_comments) = if classes.is_empty()
            && method_definitions.is_empty()
            && protocols.is_empty()
            && expressions.is_empty()
        {
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
            protocols,
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

        // Skip optional superclass type arguments: `Collection(E)`, `Mapping(K, V)`
        if matches!(self.peek_at(offset), Some(TokenKind::LeftParen)) {
            if let Some(after) = self.skip_paren_type_params(offset) {
                offset = after;
            }
        }

        // Expect `subclass:` keyword
        matches!(self.peek_at(offset), Some(TokenKind::Keyword(k)) if k == "subclass:")
    }

    /// Checks if the current position looks like a protocol definition.
    ///
    /// Protocol definitions follow the pattern:
    /// - `Protocol define: <Name>` optionally followed by `(T, E)` type params
    ///
    /// We look ahead for `Identifier("Protocol")` followed by `Keyword("define:")`.
    pub(super) fn is_at_protocol_definition(&self) -> bool {
        matches!(self.peek_at(0), Some(TokenKind::Identifier(name)) if name == "Protocol")
            && matches!(self.peek_at(1), Some(TokenKind::Keyword(k)) if k == "define:")
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

        // Must have `>>` token (GtGt since BT-663)
        if !matches!(self.peek_at(offset), Some(TokenKind::GtGt)) {
            return false;
        }
        offset += 1;

        // After `>>`, must have a method selector followed by `=>`
        self.is_method_selector_at(offset)
    }

    /// Checks if there is a method selector followed by `=>` (or `-> Type =>`) at the given offset.
    fn is_method_selector_at(&self, offset: usize) -> bool {
        match self.peek_at(offset) {
            // Unary: `identifier =>` or `identifier -> Type =>`
            Some(TokenKind::Identifier(_)) => self.is_fat_arrow_or_return_type(offset + 1),
            // Binary: `+ other =>` or `+ other :: Type (| Type)* =>` or `+ other -> Type =>`
            Some(TokenKind::BinarySelector(_)) => {
                if !matches!(self.peek_at(offset + 1), Some(TokenKind::Identifier(_))) {
                    return false;
                }
                let after_param = match self.skip_double_colon_type(offset + 2) {
                    DoubleColonSkip::Valid(o) | DoubleColonSkip::Malformed(o) => o,
                    DoubleColonSkip::NotPresent => offset + 2,
                };
                self.is_fat_arrow_or_return_type(after_param)
            }
            // Keyword: `at: index put: value =>` or `at: index :: Type =>`
            Some(TokenKind::Keyword(_)) => self.is_keyword_method_selector_at(offset),
            _ => false,
        }
    }

    /// Checks if there's a keyword method selector followed by `=>` (or `-> Type =>`) at the given offset.
    /// Delegates to the shared helper.
    fn is_keyword_method_selector_at(&self, start_offset: usize) -> bool {
        self.is_keyword_method_params_at(start_offset)
    }

    /// Shared lookahead helper for keyword method definitions.
    ///
    /// Handles all keyword param forms:
    /// - `keyword: param =>` (untyped)
    /// - `keyword: paramName: Type =>` (typed via `Keyword` token, i.e. `name:`)
    /// - `keyword: paramName: Type | Type =>` (union type)
    /// - `keyword: param : Type =>` (typed via separate `Colon` token)
    /// - `keyword: param : Type | Type =>` (union type)
    /// - Any of the above followed by `-> ReturnType =>`
    pub(super) fn is_keyword_method_params_at(&self, start_offset: usize) -> bool {
        let mut offset = start_offset;

        loop {
            // Expect keyword selector part
            if !matches!(self.peek_at(offset), Some(TokenKind::Keyword(_))) {
                return false;
            }
            offset += 1;

            // Check for typed parameter written as `paramName: Type`
            // where `paramName:` is lexed as a Keyword token followed by an Identifier (the type)
            if let Some(TokenKind::Keyword(_)) = self.peek_at(offset) {
                // Skip `paramName:`
                offset += 1;
                // Must be followed by a type Identifier
                if !matches!(self.peek_at(offset), Some(TokenKind::Identifier(_))) {
                    return false;
                }
                offset += 1;
                // Skip generic type params: `Type(T, E)`
                if matches!(self.peek_at(offset), Some(TokenKind::LeftParen)) {
                    if let Some(after) = self.skip_paren_type_params(offset) {
                        offset = after;
                    }
                }
                // Skip union tail: `| Type | Type ...`
                while matches!(self.peek_at(offset), Some(TokenKind::Pipe)) {
                    offset += 1;
                    if !matches!(self.peek_at(offset), Some(TokenKind::Identifier(_))) {
                        return false;
                    }
                    offset += 1;
                    // Skip generic type params on union member
                    if matches!(self.peek_at(offset), Some(TokenKind::LeftParen)) {
                        if let Some(after) = self.skip_paren_type_params(offset) {
                            offset = after;
                        }
                    }
                }
                match self.peek_at(offset) {
                    Some(TokenKind::FatArrow) => return true,
                    Some(TokenKind::Keyword(_)) => continue,
                    Some(TokenKind::Arrow) => {
                        return self.is_return_type_then_fat_arrow(offset);
                    }
                    // ADR 0066 Phase 4: `:: -> Type =>` extension type annotation
                    Some(TokenKind::DoubleColon) => {
                        return self.is_return_type_then_fat_arrow(offset + 1);
                    }
                    _ => return false,
                }
            }

            // Expect parameter name (untyped or `param : Type` form)
            if !matches!(self.peek_at(offset), Some(TokenKind::Identifier(_))) {
                return false;
            }
            offset += 1;
            // Optional `:: Type (| Type)*` annotation on this parameter
            if let DoubleColonSkip::Valid(after) | DoubleColonSkip::Malformed(after) =
                self.skip_double_colon_type(offset)
            {
                offset = after;
            }
            match self.peek_at(offset) {
                Some(TokenKind::FatArrow) => return true,
                Some(TokenKind::Keyword(_)) => {} // More keyword parts, continue loop
                Some(TokenKind::Arrow) => {
                    return self.is_return_type_then_fat_arrow(offset);
                }
                // ADR 0066 Phase 4: `:: -> Type =>` extension type annotation
                Some(TokenKind::DoubleColon) => {
                    return self.is_return_type_then_fat_arrow(offset + 1);
                }
                Some(TokenKind::Colon) => {
                    // Typed parameter: `paramName : Type`
                    offset += 1; // skip `:`
                    if !matches!(self.peek_at(offset), Some(TokenKind::Identifier(_))) {
                        return false;
                    }
                    offset += 1; // skip type name
                    // Skip generic type params: `Type(T, E)`
                    if matches!(self.peek_at(offset), Some(TokenKind::LeftParen)) {
                        if let Some(after) = self.skip_paren_type_params(offset) {
                            offset = after;
                        }
                    }
                    // Skip union tail: `| Type | Type ...`
                    while matches!(self.peek_at(offset), Some(TokenKind::Pipe)) {
                        offset += 1;
                        if !matches!(self.peek_at(offset), Some(TokenKind::Identifier(_))) {
                            return false;
                        }
                        offset += 1;
                        // Skip generic type params on union member
                        if matches!(self.peek_at(offset), Some(TokenKind::LeftParen)) {
                            if let Some(after) = self.skip_paren_type_params(offset) {
                                offset = after;
                            }
                        }
                    }
                    match self.peek_at(offset) {
                        Some(TokenKind::FatArrow) => return true,
                        Some(TokenKind::Keyword(_)) => {} // More keyword parts, continue loop
                        Some(TokenKind::Arrow) => {
                            return self.is_return_type_then_fat_arrow(offset);
                        }
                        // ADR 0066 Phase 4: `:: -> Type =>` extension type annotation
                        Some(TokenKind::DoubleColon) => {
                            return self.is_return_type_then_fat_arrow(offset + 1);
                        }
                        _ => return false,
                    }
                }
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
