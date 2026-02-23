// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Token types for Beamtalk lexical analysis.
//!
//! This module defines the token types produced by the lexer, including
//! support for trivia (whitespace and comments) to enable formatting tools.
//!
//! # Token Structure
//!
//! Each token consists of:
//! - A [`TokenKind`] indicating the type of token
//! - A [`Span`] indicating its location in source
//! - Leading and trailing [`Trivia`] for accurate source reconstruction
//!
//! # Smalltalk Syntax Coverage
//!
//! Beamtalk tokens cover the full Smalltalk message syntax:
//! - Unary messages: `object message`
//! - Binary messages: `3 + 4`
//! - Keyword messages: `array at: 1 put: value`
//! - Blocks: `[:x | x + 1]`
//! - Cascades: `stream nextPut: 'a'; nextPut: 'b'`

use ecow::EcoString;

use super::Span;

/// The kind of token, not including source location or trivia.
///
/// This enum represents all syntactic elements that can appear in Beamtalk
/// source code. Tokens are designed to be cheap to clone (using [`EcoString`]
/// for string data).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // === Literals ===
    /// An identifier: `foo`, `myVariable`, `Array`
    Identifier(EcoString),

    /// An integer literal: `42`, `-17`, `16rFF` (base 16)
    Integer(EcoString),

    /// A floating-point literal: `3.14`, `2.5e10`
    Float(EcoString),

    /// A double-quoted string: `"hello world"` (no interpolation)
    String(EcoString),

    /// Start of an interpolated string: `"Hello, ` in `"Hello, {name}!"`
    /// Contains the literal text from the opening `"` up to the first `{`.
    StringStart(EcoString),

    /// Middle segment of an interpolated string: ` and ` in `"a{x} and {y}b"`
    /// Contains the literal text between a `}` and the next `{`.
    StringSegment(EcoString),

    /// End of an interpolated string: `!` in `"Hello, {name}!"`
    /// Contains the literal text from the last `}` to the closing `"`.
    StringEnd(EcoString),

    /// A symbol literal: `#foo`, `#'hello world'`
    Symbol(EcoString),

    /// A character literal: `$a`, `$\n`
    Character(char),

    // === Message Selectors ===
    /// A keyword selector part (ends with colon): `at:`, `put:`, `ifTrue:`
    Keyword(EcoString),

    /// A binary selector: `+`, `-`, `*`, `/`, `<`, `>`, `=`, `~`, etc.
    BinarySelector(EcoString),

    // === Delimiters ===
    /// Left parenthesis: `(`
    LeftParen,

    /// Right parenthesis: `)`
    RightParen,

    /// Left bracket (block start): `[`
    LeftBracket,

    /// Right bracket (block end): `]`
    RightBracket,

    /// Left brace (tuple literal): `{`
    LeftBrace,

    /// Right brace: `}`
    RightBrace,

    /// Map open (map literal start): `#{`
    MapOpen,

    /// List open (list literal start): `#(`
    ListOpen,

    // === Punctuation ===
    /// Assignment operator: `:=`
    Assign,

    /// Return operator: `^`
    Caret,

    /// Cascade separator: `;`
    Semicolon,

    /// Statement terminator: `.`
    Period,

    /// Block argument separator: `|`
    Pipe,

    /// Colon for block argument declaration (not keyword selectors): `:`
    Colon,

    /// Standalone hash character: `#`
    ///
    /// Note: complete symbol literals are represented by [`TokenKind::Symbol`],
    /// which stores the symbol name without the leading `#`.
    Hash,

    /// Fat arrow for method definitions: `=>`
    FatArrow,

    // === Pragmas ===
    /// The `@primitive` directive for primitive method injection (ADR 0007)
    AtPrimitive,
    /// The `@intrinsic` directive, synonym for `@primitive` (ADR 0007 Amendment)
    AtIntrinsic,
    /// The `@expect` directive for suppressing specific diagnostic categories
    AtExpect,

    // === Special ===
    /// End of file
    Eof,

    /// Invalid/error token (preserves unparseable text for error recovery)
    Error(EcoString),
}

impl TokenKind {
    /// Returns `true` if this token is a literal value.
    ///
    /// Note: identifiers are not considered literals as they are names
    /// that reference values, not direct value representations.
    #[must_use]
    pub const fn is_literal(&self) -> bool {
        matches!(
            self,
            Self::Integer(_)
                | Self::Float(_)
                | Self::String(_)
                | Self::StringStart(_)
                | Self::Symbol(_)
                | Self::Character(_)
        )
    }

    /// Returns `true` if this token is an identifier.
    #[must_use]
    pub const fn is_identifier(&self) -> bool {
        matches!(self, Self::Identifier(_))
    }

    /// Returns `true` if this token is a message selector component.
    #[must_use]
    pub const fn is_selector(&self) -> bool {
        matches!(self, Self::Keyword(_) | Self::BinarySelector(_))
    }

    /// Returns `true` if this token is a delimiter.
    #[must_use]
    pub const fn is_delimiter(&self) -> bool {
        matches!(
            self,
            Self::LeftParen
                | Self::RightParen
                | Self::LeftBracket
                | Self::RightBracket
                | Self::LeftBrace
                | Self::RightBrace
                | Self::MapOpen
                | Self::ListOpen
        )
    }

    /// Returns `true` if this is the end-of-file marker.
    #[must_use]
    pub const fn is_eof(&self) -> bool {
        matches!(self, Self::Eof)
    }

    /// Returns `true` if this is an error token.
    #[must_use]
    pub const fn is_error(&self) -> bool {
        matches!(self, Self::Error(_))
    }

    /// Returns the string content if this token carries one.
    #[must_use]
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Self::Identifier(s)
            | Self::Integer(s)
            | Self::Float(s)
            | Self::String(s)
            | Self::StringStart(s)
            | Self::StringSegment(s)
            | Self::StringEnd(s)
            | Self::Symbol(s)
            | Self::Keyword(s)
            | Self::BinarySelector(s)
            | Self::Error(s) => Some(s),
            Self::Character(_)
            | Self::LeftParen
            | Self::RightParen
            | Self::LeftBracket
            | Self::RightBracket
            | Self::LeftBrace
            | Self::RightBrace
            | Self::MapOpen
            | Self::ListOpen
            | Self::Assign
            | Self::Caret
            | Self::Semicolon
            | Self::Period
            | Self::Pipe
            | Self::Colon
            | Self::Hash
            | Self::FatArrow
            | Self::AtPrimitive
            | Self::AtIntrinsic
            | Self::AtExpect
            | Self::Eof => None,
        }
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // Tokens that display their string content directly
            Self::Identifier(s)
            | Self::Integer(s)
            | Self::Float(s)
            | Self::Keyword(s)
            | Self::BinarySelector(s) => write!(f, "{s}"),
            // Tokens with delimiters around content
            Self::String(s) => write!(f, "\"{s}\""),
            Self::StringStart(s) => write!(f, "\"{s}{{"),
            Self::StringSegment(s) => write!(f, "}}{s}{{"),
            Self::StringEnd(s) => write!(f, "}}{s}\""),
            Self::Symbol(s) => write!(f, "#{s}"),
            Self::Character(c) => write!(f, "${c}"),
            Self::Error(s) => write!(f, "<error: {s}>"),
            // Fixed-text tokens
            Self::LeftParen => write!(f, "("),
            Self::RightParen => write!(f, ")"),
            Self::LeftBracket => write!(f, "["),
            Self::RightBracket => write!(f, "]"),
            Self::LeftBrace => write!(f, "{{"),
            Self::RightBrace => write!(f, "}}"),
            Self::MapOpen => write!(f, "#{{"),
            Self::ListOpen => write!(f, "#("),
            Self::Assign => write!(f, ":="),
            Self::Caret => write!(f, "^"),
            Self::Semicolon => write!(f, ";"),
            Self::Period => write!(f, "."),
            Self::Pipe => write!(f, "|"),
            Self::Colon => write!(f, ":"),
            Self::Hash => write!(f, "#"),
            Self::FatArrow => write!(f, "=>"),
            Self::AtPrimitive => write!(f, "@primitive"),
            Self::AtIntrinsic => write!(f, "@intrinsic"),
            Self::AtExpect => write!(f, "@expect"),
            Self::Eof => write!(f, "<eof>"),
        }
    }
}

/// Trivia represents non-semantic content like whitespace and comments.
///
/// Preserving trivia enables formatting tools to reconstruct source code
/// exactly, and allows comments to be associated with adjacent tokens.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Trivia {
    /// Whitespace (spaces, tabs, newlines)
    Whitespace(EcoString),

    /// A line comment: `// comment text`
    LineComment(EcoString),

    /// A block comment: `/* comment text */`
    BlockComment(EcoString),

    /// A doc comment: `/// doc text`
    DocComment(EcoString),
}

impl Trivia {
    /// Returns the text content of this trivia.
    #[must_use]
    pub fn as_str(&self) -> &str {
        match self {
            Self::Whitespace(s)
            | Self::LineComment(s)
            | Self::BlockComment(s)
            | Self::DocComment(s) => s,
        }
    }

    /// Returns `true` if this trivia contains a newline.
    #[must_use]
    pub fn contains_newline(&self) -> bool {
        self.as_str().contains('\n')
    }

    /// Returns `true` if this is whitespace.
    #[must_use]
    pub const fn is_whitespace(&self) -> bool {
        matches!(self, Self::Whitespace(_))
    }

    /// Returns `true` if this is a comment.
    #[must_use]
    pub const fn is_comment(&self) -> bool {
        matches!(
            self,
            Self::LineComment(_) | Self::BlockComment(_) | Self::DocComment(_)
        )
    }

    /// Returns `true` if this is a doc comment.
    #[must_use]
    pub const fn is_doc_comment(&self) -> bool {
        matches!(self, Self::DocComment(_))
    }
}

/// A token with its source location and surrounding trivia.
///
/// Tokens carry their position in the source file ([`Span`]) and any
/// whitespace or comments that precede or follow them. This enables:
///
/// - Precise error reporting with source locations
/// - Code formatting that preserves comments
/// - IDE features like syntax highlighting
///
/// # Examples
///
/// ```
/// use beamtalk_core::source_analysis::{Token, TokenKind, Span};
///
/// let token = Token::new(TokenKind::Identifier("foo".into()), Span::new(0, 3));
/// assert!(matches!(token.kind(), TokenKind::Identifier(_)));
/// assert_eq!(token.span().len(), 3);
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    kind: TokenKind,
    span: Span,
    leading_trivia: Vec<Trivia>,
    trailing_trivia: Vec<Trivia>,
}

impl Token {
    /// Creates a new token with no trivia.
    #[must_use]
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self {
            kind,
            span,
            leading_trivia: Vec::new(),
            trailing_trivia: Vec::new(),
        }
    }

    /// Creates a new token with trivia.
    #[must_use]
    pub fn with_trivia(
        kind: TokenKind,
        span: Span,
        leading_trivia: Vec<Trivia>,
        trailing_trivia: Vec<Trivia>,
    ) -> Self {
        Self {
            kind,
            span,
            leading_trivia,
            trailing_trivia,
        }
    }

    /// Returns the kind of this token.
    #[must_use]
    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    /// Consumes the token and returns its kind.
    #[must_use]
    pub fn into_kind(self) -> TokenKind {
        self.kind
    }

    /// Returns the source span of this token (excluding trivia).
    #[must_use]
    pub fn span(&self) -> Span {
        self.span
    }

    /// Returns the full span including leading and trailing trivia.
    ///
    /// This is useful for formatting operations that need to preserve
    /// all source text.
    #[must_use]
    pub fn full_span(&self) -> Span {
        // For now, just return the token span
        // When trivia has spans, this will merge them
        self.span
    }

    /// Returns the trivia that precedes this token.
    #[must_use]
    pub fn leading_trivia(&self) -> &[Trivia] {
        &self.leading_trivia
    }

    /// Returns the trivia that follows this token.
    #[must_use]
    pub fn trailing_trivia(&self) -> &[Trivia] {
        &self.trailing_trivia
    }

    /// Sets the leading trivia for this token.
    pub fn set_leading_trivia(&mut self, trivia: Vec<Trivia>) {
        self.leading_trivia = trivia;
    }

    /// Sets the trailing trivia for this token.
    pub fn set_trailing_trivia(&mut self, trivia: Vec<Trivia>) {
        self.trailing_trivia = trivia;
    }

    /// Returns `true` if this token has any leading comments.
    #[must_use]
    pub fn has_leading_comment(&self) -> bool {
        self.leading_trivia.iter().any(Trivia::is_comment)
    }

    /// Returns `true` if this token has any trailing comments.
    #[must_use]
    pub fn has_trailing_comment(&self) -> bool {
        self.trailing_trivia.iter().any(Trivia::is_comment)
    }

    /// Returns `true` if there's a newline in the leading trivia.
    ///
    /// This is useful for determining statement boundaries.
    #[must_use]
    pub fn has_leading_newline(&self) -> bool {
        self.leading_trivia.iter().any(Trivia::contains_newline)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn token_kind_display() {
        assert_eq!(TokenKind::Identifier("foo".into()).to_string(), "foo");
        assert_eq!(TokenKind::Integer("42".into()).to_string(), "42");
        assert_eq!(TokenKind::String("hello".into()).to_string(), "\"hello\"");
        assert_eq!(TokenKind::Symbol("sym".into()).to_string(), "#sym");
        assert_eq!(TokenKind::Keyword("at:".into()).to_string(), "at:");
        assert_eq!(TokenKind::BinarySelector("+".into()).to_string(), "+");
        assert_eq!(TokenKind::Assign.to_string(), ":=");
        assert_eq!(TokenKind::Caret.to_string(), "^");
        assert_eq!(TokenKind::AtPrimitive.to_string(), "@primitive");
        assert_eq!(TokenKind::AtIntrinsic.to_string(), "@intrinsic");
    }

    #[test]
    fn token_kind_predicates() {
        // is_literal: excludes identifiers (they reference values, not literals)
        assert!(!TokenKind::Identifier("x".into()).is_literal());
        assert!(TokenKind::Integer("1".into()).is_literal());
        assert!(TokenKind::Float("3.14".into()).is_literal());
        assert!(TokenKind::String("hello".into()).is_literal());
        assert!(TokenKind::Symbol("sym".into()).is_literal());
        assert!(TokenKind::Character('a').is_literal());
        assert!(!TokenKind::Keyword("at:".into()).is_literal());

        // is_identifier
        assert!(TokenKind::Identifier("foo".into()).is_identifier());
        assert!(!TokenKind::Integer("1".into()).is_identifier());

        // is_selector
        assert!(TokenKind::Keyword("at:".into()).is_selector());
        assert!(TokenKind::BinarySelector("+".into()).is_selector());
        assert!(!TokenKind::Identifier("x".into()).is_selector());

        // is_delimiter
        assert!(TokenKind::LeftParen.is_delimiter());
        assert!(TokenKind::RightBracket.is_delimiter());
        assert!(!TokenKind::Semicolon.is_delimiter());

        // is_eof, is_error
        assert!(TokenKind::Eof.is_eof());
        assert!(TokenKind::Error("bad".into()).is_error());
    }

    #[test]
    fn token_creation_and_accessors() {
        let token = Token::new(TokenKind::Identifier("foo".into()), Span::new(0, 3));

        assert!(matches!(token.kind(), TokenKind::Identifier(s) if s == "foo"));
        assert_eq!(token.span().start(), 0);
        assert_eq!(token.span().end(), 3);
        assert!(token.leading_trivia().is_empty());
        assert!(token.trailing_trivia().is_empty());
    }

    #[test]
    fn token_with_trivia() {
        let leading = vec![Trivia::Whitespace("  ".into())];
        let trailing = vec![Trivia::LineComment("// comment".into())];

        let token = Token::with_trivia(
            TokenKind::Integer("42".into()),
            Span::new(2, 4),
            leading,
            trailing,
        );

        assert_eq!(token.leading_trivia().len(), 1);
        assert_eq!(token.trailing_trivia().len(), 1);
        assert!(!token.has_leading_comment());
        assert!(token.has_trailing_comment());
    }

    #[test]
    fn trivia_predicates() {
        let ws = Trivia::Whitespace("  \n  ".into());
        let comment = Trivia::LineComment("// note".into());

        assert!(ws.is_whitespace());
        assert!(!ws.is_comment());
        assert!(ws.contains_newline());

        assert!(!comment.is_whitespace());
        assert!(comment.is_comment());
        assert!(!comment.contains_newline());
    }

    #[test]
    fn token_leading_newline_detection() {
        let no_newline = Token::with_trivia(
            TokenKind::Identifier("x".into()),
            Span::new(2, 3),
            vec![Trivia::Whitespace("  ".into())],
            vec![],
        );
        assert!(!no_newline.has_leading_newline());

        let with_newline = Token::with_trivia(
            TokenKind::Identifier("x".into()),
            Span::new(5, 6),
            vec![Trivia::Whitespace("\n  ".into())],
            vec![],
        );
        assert!(with_newline.has_leading_newline());
    }

    #[test]
    fn token_kind_display_complete() {
        // Additional display tests for full coverage
        assert_eq!(TokenKind::Float("3.14".into()).to_string(), "3.14");
        assert_eq!(
            TokenKind::String("Hello, {name}!".into()).to_string(),
            "\"Hello, {name}!\""
        );
        assert_eq!(TokenKind::Character('a').to_string(), "$a");
        assert_eq!(
            TokenKind::Error("unexpected".into()).to_string(),
            "<error: unexpected>"
        );
        assert_eq!(TokenKind::LeftBrace.to_string(), "{");
        assert_eq!(TokenKind::RightBrace.to_string(), "}");
        assert_eq!(TokenKind::Pipe.to_string(), "|");
        assert_eq!(TokenKind::Colon.to_string(), ":");
        assert_eq!(TokenKind::Hash.to_string(), "#");
        assert_eq!(TokenKind::Eof.to_string(), "<eof>");
    }

    #[test]
    fn token_kind_as_str() {
        // Tokens with string content
        assert_eq!(TokenKind::Identifier("foo".into()).as_str(), Some("foo"));
        assert_eq!(TokenKind::Integer("42".into()).as_str(), Some("42"));
        assert_eq!(TokenKind::Float("3.14".into()).as_str(), Some("3.14"));
        assert_eq!(TokenKind::String("hello".into()).as_str(), Some("hello"));
        assert_eq!(TokenKind::Symbol("sym".into()).as_str(), Some("sym"));
        assert_eq!(TokenKind::Keyword("at:".into()).as_str(), Some("at:"));
        assert_eq!(TokenKind::BinarySelector("+".into()).as_str(), Some("+"));
        assert_eq!(TokenKind::Error("bad".into()).as_str(), Some("bad"));

        // Tokens without string content
        assert_eq!(TokenKind::Character('x').as_str(), None);
        assert_eq!(TokenKind::LeftParen.as_str(), None);
        assert_eq!(TokenKind::Assign.as_str(), None);
        assert_eq!(TokenKind::Eof.as_str(), None);
    }

    #[test]
    fn token_into_kind() {
        let token = Token::new(TokenKind::Integer("42".into()), Span::new(0, 2));
        let kind = token.into_kind();
        assert!(matches!(kind, TokenKind::Integer(s) if s == "42"));
    }

    #[test]
    fn token_full_span() {
        let token = Token::new(TokenKind::Identifier("x".into()), Span::new(5, 6));
        // Currently full_span equals span (trivia spans not yet tracked)
        assert_eq!(token.full_span().start(), 5);
        assert_eq!(token.full_span().end(), 6);
    }

    #[test]
    fn token_set_trivia() {
        let mut token = Token::new(TokenKind::Identifier("x".into()), Span::new(0, 1));
        assert!(token.leading_trivia().is_empty());
        assert!(token.trailing_trivia().is_empty());

        token.set_leading_trivia(vec![Trivia::Whitespace("  ".into())]);
        token.set_trailing_trivia(vec![Trivia::LineComment("// note".into())]);

        assert_eq!(token.leading_trivia().len(), 1);
        assert_eq!(token.trailing_trivia().len(), 1);
    }
}
