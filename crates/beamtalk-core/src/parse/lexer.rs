// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lexical analysis for Beamtalk source code.
//!
//! This module converts source text into a stream of [`Token`]s. The lexer
//! is hand-written for maximum control over error recovery and IDE features.
//!
//! # Design Principles
//!
//! - **Error recovery**: Never panic on malformed input; emit [`TokenKind::Error`]
//! - **Trivia preservation**: Track whitespace and comments for formatting tools
//! - **Precise spans**: Every token carries its exact source location
//!
//! # Example
//!
//! ```
//! use beamtalk_core::parse::{Lexer, TokenKind};
//!
//! let tokens: Vec<_> = Lexer::new("x + 1").collect();
//! assert_eq!(tokens.len(), 3); // x, +, 1 (EOF excluded from iterator)
//! ```

use std::iter::Peekable;
use std::str::CharIndices;

use ecow::EcoString;

use super::{Span, Token, TokenKind, Trivia};

/// A lexer that tokenizes Beamtalk source code.
///
/// The lexer produces tokens with their source spans and attached trivia
/// (whitespace and comments). It implements [`Iterator`] for easy consumption.
///
/// # Error Recovery
///
/// The lexer never fails completely. Unknown characters and unterminated
/// strings produce [`TokenKind::Error`] tokens, allowing parsing to continue.
pub struct Lexer<'src> {
    /// The source text being lexed.
    source: &'src str,
    /// Character iterator with byte positions.
    chars: Peekable<CharIndices<'src>>,
    /// Current byte position in source.
    position: usize,
    /// Pending trivia to attach to the next token.
    pending_trivia: Vec<Trivia>,
}

impl std::fmt::Debug for Lexer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Lexer")
            .field("position", &self.position)
            .field("remaining", &self.source.get(self.position..).unwrap_or(""))
            .finish()
    }
}

impl<'src> Lexer<'src> {
    /// Creates a new lexer for the given source text.
    #[must_use]
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
            position: 0,
            pending_trivia: Vec::new(),
        }
    }

    /// Peeks at the next character without consuming it.
    fn peek_char(&mut self) -> Option<char> {
        self.chars.peek().map(|&(_, c)| c)
    }

    /// Peeks at the character after the next one.
    fn peek_char_second(&self) -> Option<char> {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next().map(|(_, c)| c)
    }

    /// Consumes the next character and returns it.
    fn advance(&mut self) -> Option<char> {
        let (pos, c) = self.chars.next()?;
        self.position = pos + c.len_utf8();
        Some(c)
    }

    /// Consumes characters while the predicate is true.
    fn advance_while(&mut self, predicate: impl Fn(char) -> bool) {
        while self.peek_char().is_some_and(&predicate) {
            self.advance();
        }
    }

    /// Returns the current byte position.
    #[expect(
        clippy::cast_possible_truncation,
        reason = "source files over 4GB are not supported"
    )]
    fn current_position(&self) -> u32 {
        self.position as u32
    }

    /// Creates a span from start to current position.
    fn span_from(&self, start: u32) -> Span {
        Span::new(start, self.current_position())
    }

    /// Extracts source text for a span.
    fn text_for(&self, span: Span) -> &'src str {
        &self.source[span.as_range()]
    }

    /// Skips whitespace and comments, collecting them as trivia.
    fn skip_trivia(&mut self) {
        loop {
            match self.peek_char() {
                Some(' ' | '\t' | '\r' | '\n') => {
                    let start = self.current_position();
                    self.advance_while(|c| matches!(c, ' ' | '\t' | '\r' | '\n'));
                    let text = self.text_for(self.span_from(start));
                    self.pending_trivia
                        .push(Trivia::Whitespace(EcoString::from(text)));
                }
                Some('/') if self.peek_char_second() == Some('/') => {
                    self.lex_line_comment();
                }
                Some('/') if self.peek_char_second() == Some('*') => {
                    self.lex_block_comment();
                }
                _ => break,
            }
        }
    }

    /// Lexes a line comment: `// ...`
    fn lex_line_comment(&mut self) {
        let start = self.current_position();
        self.advance(); // /
        self.advance(); // /
        self.advance_while(|c| c != '\n');
        let text = self.text_for(self.span_from(start));
        self.pending_trivia
            .push(Trivia::LineComment(EcoString::from(text)));
    }

    /// Lexes a block comment: `/* ... */`
    fn lex_block_comment(&mut self) {
        let start = self.current_position();
        self.advance(); // /
        self.advance(); // *

        loop {
            match self.peek_char() {
                None => break, // Unterminated - recover gracefully
                Some('*') if self.peek_char_second() == Some('/') => {
                    self.advance(); // *
                    self.advance(); // /
                    break;
                }
                _ => {
                    self.advance();
                }
            }
        }

        let text = self.text_for(self.span_from(start));
        self.pending_trivia
            .push(Trivia::BlockComment(EcoString::from(text)));
    }

    /// Lexes the next token.
    fn lex_token(&mut self) -> Token {
        self.skip_trivia();
        let leading_trivia = std::mem::take(&mut self.pending_trivia);

        let start = self.current_position();

        let kind = match self.peek_char() {
            None => TokenKind::Eof,
            Some(c) => self.lex_token_kind(c, start),
        };

        let span = self.span_from(start);

        // Collect trailing trivia (whitespace on same line, up to newline)
        self.collect_trailing_trivia();
        let trailing_trivia = std::mem::take(&mut self.pending_trivia);

        Token::with_trivia(kind, span, leading_trivia, trailing_trivia)
    }

    /// Collects trailing trivia (same-line whitespace and comments).
    fn collect_trailing_trivia(&mut self) {
        loop {
            match self.peek_char() {
                Some(' ' | '\t') => {
                    let start = self.current_position();
                    self.advance_while(|c| matches!(c, ' ' | '\t'));
                    let text = self.text_for(self.span_from(start));
                    self.pending_trivia
                        .push(Trivia::Whitespace(EcoString::from(text)));
                }
                Some('/') if self.peek_char_second() == Some('/') => {
                    self.lex_line_comment();
                    break; // Line comment ends trailing trivia
                }
                _ => break,
            }
        }
    }

    /// Lexes a token kind based on the first character.
    fn lex_token_kind(&mut self, c: char, start: u32) -> TokenKind {
        match c {
            // Identifiers and keywords
            'a'..='z' | 'A'..='Z' | '_' => self.lex_identifier_or_keyword(),

            // Numbers
            '0'..='9' => self.lex_number(),

            // Strings
            '\'' => self.lex_string(),
            '"' => self.lex_interpolated_string(),

            // Character literals
            '$' => self.lex_character(),

            // Symbols
            '#' => self.lex_symbol_or_hash(),

            // Single-character tokens
            '(' => {
                self.advance();
                TokenKind::LeftParen
            }
            ')' => {
                self.advance();
                TokenKind::RightParen
            }
            '[' => {
                self.advance();
                TokenKind::LeftBracket
            }
            ']' => {
                self.advance();
                TokenKind::RightBracket
            }
            '{' => {
                self.advance();
                TokenKind::LeftBrace
            }
            '}' => {
                self.advance();
                TokenKind::RightBrace
            }
            '^' => {
                self.advance();
                TokenKind::Caret
            }
            ';' => {
                self.advance();
                TokenKind::Semicolon
            }
            '.' => {
                self.advance();
                TokenKind::Period
            }
            '|' => {
                self.advance();
                TokenKind::Pipe
            }

            // Colon or assignment
            ':' => self.lex_colon_or_assign(),

            // Binary operators
            '+' | '-' | '*' | '/' | '<' | '>' | '=' | '~' | '@' | '%' | '&' | '?' | ',' | '\\' => {
                self.lex_binary_selector()
            }

            // Unknown character - error recovery
            _ => {
                self.advance();
                let text = self.text_for(self.span_from(start));
                TokenKind::Error(EcoString::from(text))
            }
        }
    }

    /// Lexes an identifier or keyword selector.
    fn lex_identifier_or_keyword(&mut self) -> TokenKind {
        let start = self.current_position();
        self.advance_while(|c| c.is_ascii_alphanumeric() || c == '_');

        // Check if followed by colon (keyword selector)
        if self.peek_char() == Some(':') && self.peek_char_second() != Some('=') {
            self.advance(); // consume the colon
            let text = self.text_for(self.span_from(start));
            TokenKind::Keyword(EcoString::from(text))
        } else {
            let text = self.text_for(self.span_from(start));
            TokenKind::Identifier(EcoString::from(text))
        }
    }

    /// Lexes an integer or float literal.
    fn lex_number(&mut self) -> TokenKind {
        let start = self.current_position();

        // Integer part
        self.advance_while(|c| c.is_ascii_digit());

        // Check for radix notation: 16rFF
        if self.peek_char() == Some('r') {
            self.advance(); // consume 'r'
            self.advance_while(|c| c.is_ascii_alphanumeric());
            let text = self.text_for(self.span_from(start));
            return TokenKind::Integer(EcoString::from(text));
        }

        // Check for float: decimal point followed by digit
        let is_float = if self.peek_char() == Some('.')
            && self.peek_char_second().is_some_and(|c| c.is_ascii_digit())
        {
            self.advance(); // consume '.'
            self.advance_while(|c| c.is_ascii_digit());
            true
        } else {
            false
        };

        // Check for exponent: e or E
        if matches!(self.peek_char(), Some('e' | 'E')) {
            self.advance(); // consume 'e' or 'E'
            if matches!(self.peek_char(), Some('+' | '-')) {
                self.advance(); // consume sign
            }
            self.advance_while(|c| c.is_ascii_digit());
            let text = self.text_for(self.span_from(start));
            return TokenKind::Float(EcoString::from(text));
        }

        let text = self.text_for(self.span_from(start));
        if is_float {
            TokenKind::Float(EcoString::from(text))
        } else {
            TokenKind::Integer(EcoString::from(text))
        }
    }

    /// Lexes a single-quoted string literal.
    fn lex_string(&mut self) -> TokenKind {
        let start = self.current_position();
        self.advance(); // opening quote

        loop {
            match self.peek_char() {
                None => {
                    // Unterminated string - error recovery
                    let text = self.text_for(self.span_from(start));
                    return TokenKind::Error(EcoString::from(text));
                }
                Some('\'') => {
                    // Check for escaped quote ('')
                    if self.peek_char_second() == Some('\'') {
                        self.advance();
                        self.advance();
                    } else {
                        self.advance(); // closing quote
                        break;
                    }
                }
                _ => {
                    self.advance();
                }
            }
        }

        // Extract content without quotes
        let full_text = self.text_for(self.span_from(start));
        let content = &full_text[1..full_text.len() - 1];
        // Unescape doubled quotes
        let content = content.replace("''", "'");
        TokenKind::String(EcoString::from(content))
    }

    /// Lexes a double-quoted interpolated string.
    fn lex_interpolated_string(&mut self) -> TokenKind {
        let start = self.current_position();
        self.advance(); // opening quote

        loop {
            match self.peek_char() {
                None => {
                    // Unterminated string - error recovery
                    let text = self.text_for(self.span_from(start));
                    return TokenKind::Error(EcoString::from(text));
                }
                Some('"') => {
                    self.advance(); // closing quote
                    break;
                }
                Some('\\') => {
                    self.advance(); // backslash
                    self.advance(); // escaped char
                }
                _ => {
                    self.advance();
                }
            }
        }

        // Extract content without quotes
        let full_text = self.text_for(self.span_from(start));
        let content = &full_text[1..full_text.len() - 1];
        TokenKind::InterpolatedString(EcoString::from(content))
    }

    /// Lexes a character literal: `$a`, `$\n`
    fn lex_character(&mut self) -> TokenKind {
        let start = self.current_position();
        self.advance(); // $

        match self.peek_char() {
            None => {
                let text = self.text_for(self.span_from(start));
                TokenKind::Error(EcoString::from(text))
            }
            Some('\\') => {
                self.advance(); // backslash
                match self.advance() {
                    Some('n') => TokenKind::Character('\n'),
                    Some('t') => TokenKind::Character('\t'),
                    Some('r') => TokenKind::Character('\r'),
                    Some('\\') => TokenKind::Character('\\'),
                    Some('\'') => TokenKind::Character('\''),
                    Some(c) => TokenKind::Character(c),
                    None => {
                        let text = self.text_for(self.span_from(start));
                        TokenKind::Error(EcoString::from(text))
                    }
                }
            }
            Some(c) => {
                self.advance();
                TokenKind::Character(c)
            }
        }
    }

    /// Lexes a symbol literal or standalone hash.
    fn lex_symbol_or_hash(&mut self) -> TokenKind {
        let start = self.current_position();
        self.advance(); // #

        match self.peek_char() {
            // Quoted symbol: #'hello world'
            Some('\'') => {
                self.advance(); // opening quote
                loop {
                    match self.peek_char() {
                        None => {
                            let text = self.text_for(self.span_from(start));
                            return TokenKind::Error(EcoString::from(text));
                        }
                        Some('\'') => {
                            if self.peek_char_second() == Some('\'') {
                                self.advance();
                                self.advance();
                            } else {
                                self.advance(); // closing quote
                                break;
                            }
                        }
                        _ => {
                            self.advance();
                        }
                    }
                }
                let full_text = self.text_for(self.span_from(start));
                // Extract symbol name: #'foo' -> foo
                let content = &full_text[2..full_text.len() - 1];
                let content = content.replace("''", "'");
                TokenKind::Symbol(EcoString::from(content))
            }
            // Identifier symbol: #foo
            Some(c) if c.is_ascii_alphabetic() || c == '_' => {
                self.advance_while(|c| c.is_ascii_alphanumeric() || c == '_');
                let full_text = self.text_for(self.span_from(start));
                // Extract symbol name without #
                let name = &full_text[1..];
                TokenKind::Symbol(EcoString::from(name))
            }
            // Standalone hash
            _ => TokenKind::Hash,
        }
    }

    /// Lexes colon or assignment operator.
    fn lex_colon_or_assign(&mut self) -> TokenKind {
        self.advance(); // :
        if self.peek_char() == Some('=') {
            self.advance(); // =
            TokenKind::Assign
        } else {
            TokenKind::Colon
        }
    }

    /// Lexes a binary selector (one or more operator characters).
    fn lex_binary_selector(&mut self) -> TokenKind {
        let start = self.current_position();

        // Handle potential negative number: - followed by digit
        if self.peek_char() == Some('-')
            && self.peek_char_second().is_some_and(|c| c.is_ascii_digit())
        {
            self.advance(); // -
            return self.lex_number_with_prefix(start, "-");
        }

        self.advance_while(|c| {
            matches!(
                c,
                '+' | '-' | '*' | '/' | '<' | '>' | '=' | '~' | '@' | '%' | '&' | '?' | ',' | '\\'
            )
        });

        let text = self.text_for(self.span_from(start));
        TokenKind::BinarySelector(EcoString::from(text))
    }

    /// Lexes a number with a prefix already consumed (e.g., negative sign).
    fn lex_number_with_prefix(&mut self, start: u32, _prefix: &str) -> TokenKind {
        // Integer part
        self.advance_while(|c| c.is_ascii_digit());

        // Check for float
        let is_float = if self.peek_char() == Some('.')
            && self.peek_char_second().is_some_and(|c| c.is_ascii_digit())
        {
            self.advance();
            self.advance_while(|c| c.is_ascii_digit());
            true
        } else {
            false
        };

        // Check for exponent
        if matches!(self.peek_char(), Some('e' | 'E')) {
            self.advance();
            if matches!(self.peek_char(), Some('+' | '-')) {
                self.advance();
            }
            self.advance_while(|c| c.is_ascii_digit());
            let text = self.text_for(self.span_from(start));
            return TokenKind::Float(EcoString::from(text));
        }

        let text = self.text_for(self.span_from(start));
        if is_float {
            TokenKind::Float(EcoString::from(text))
        } else {
            TokenKind::Integer(EcoString::from(text))
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.lex_token();
        if token.kind().is_eof() {
            None
        } else {
            Some(token)
        }
    }
}

/// Convenience function to lex source into a vector of tokens (excluding EOF).
///
/// For most use cases, prefer using the `Lexer` iterator directly.
#[must_use]
pub fn lex(source: &str) -> Vec<Token> {
    Lexer::new(source).collect()
}

/// Convenience function to lex source into a vector of tokens including EOF.
#[must_use]
pub fn lex_with_eof(source: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(source);
    let mut tokens = Vec::new();
    loop {
        let token = lexer.lex_token();
        let is_eof = token.kind().is_eof();
        tokens.push(token);
        if is_eof {
            break;
        }
    }
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper to lex and extract just the token kinds.
    fn lex_kinds(source: &str) -> Vec<TokenKind> {
        lex(source).into_iter().map(Token::into_kind).collect()
    }

    #[test]
    fn lex_empty() {
        assert!(lex("").is_empty());
        assert!(lex("   ").is_empty());
        assert!(lex("// comment").is_empty());
    }

    #[test]
    fn lex_identifiers() {
        assert_eq!(
            lex_kinds("foo bar Baz _private x1"),
            vec![
                TokenKind::Identifier("foo".into()),
                TokenKind::Identifier("bar".into()),
                TokenKind::Identifier("Baz".into()),
                TokenKind::Identifier("_private".into()),
                TokenKind::Identifier("x1".into()),
            ]
        );
    }

    #[test]
    fn lex_keywords() {
        assert_eq!(
            lex_kinds("at: put: ifTrue:"),
            vec![
                TokenKind::Keyword("at:".into()),
                TokenKind::Keyword("put:".into()),
                TokenKind::Keyword("ifTrue:".into()),
            ]
        );
    }

    #[test]
    fn lex_integers() {
        assert_eq!(
            lex_kinds("42 0 123 16rFF 2r1010"),
            vec![
                TokenKind::Integer("42".into()),
                TokenKind::Integer("0".into()),
                TokenKind::Integer("123".into()),
                TokenKind::Integer("16rFF".into()),
                TokenKind::Integer("2r1010".into()),
            ]
        );
    }

    #[test]
    fn lex_floats() {
        assert_eq!(
            lex_kinds("3.14 0.5 1e10 2.5e-3"),
            vec![
                TokenKind::Float("3.14".into()),
                TokenKind::Float("0.5".into()),
                TokenKind::Float("1e10".into()),
                TokenKind::Float("2.5e-3".into()),
            ]
        );
    }

    #[test]
    fn lex_strings() {
        assert_eq!(
            lex_kinds("'hello' 'world' ''"),
            vec![
                TokenKind::String("hello".into()),
                TokenKind::String("world".into()),
                TokenKind::String("".into()),
            ]
        );
    }

    #[test]
    fn lex_string_with_escaped_quote() {
        assert_eq!(lex_kinds("'it''s'"), vec![TokenKind::String("it's".into())]);
    }

    #[test]
    fn lex_interpolated_strings() {
        assert_eq!(
            lex_kinds(r#""hello" "Hello, {name}!""#),
            vec![
                TokenKind::InterpolatedString("hello".into()),
                TokenKind::InterpolatedString("Hello, {name}!".into()),
            ]
        );
    }

    #[test]
    fn lex_characters() {
        assert_eq!(
            lex_kinds("$a $Z $1 $\\n $\\t"),
            vec![
                TokenKind::Character('a'),
                TokenKind::Character('Z'),
                TokenKind::Character('1'),
                TokenKind::Character('\n'),
                TokenKind::Character('\t'),
            ]
        );
    }

    #[test]
    fn lex_symbols() {
        assert_eq!(
            lex_kinds("#foo #bar #'hello world'"),
            vec![
                TokenKind::Symbol("foo".into()),
                TokenKind::Symbol("bar".into()),
                TokenKind::Symbol("hello world".into()),
            ]
        );
    }

    #[test]
    fn lex_delimiters() {
        assert_eq!(
            lex_kinds("()[]{}"),
            vec![
                TokenKind::LeftParen,
                TokenKind::RightParen,
                TokenKind::LeftBracket,
                TokenKind::RightBracket,
                TokenKind::LeftBrace,
                TokenKind::RightBrace,
            ]
        );
    }

    #[test]
    fn lex_punctuation() {
        assert_eq!(
            lex_kinds(":= ^ ; . | : #"),
            vec![
                TokenKind::Assign,
                TokenKind::Caret,
                TokenKind::Semicolon,
                TokenKind::Period,
                TokenKind::Pipe,
                TokenKind::Colon,
                TokenKind::Hash,
            ]
        );
    }

    #[test]
    fn lex_binary_selectors() {
        assert_eq!(
            lex_kinds("+ - * / < > = ~"),
            vec![
                TokenKind::BinarySelector("+".into()),
                TokenKind::BinarySelector("-".into()),
                TokenKind::BinarySelector("*".into()),
                TokenKind::BinarySelector("/".into()),
                TokenKind::BinarySelector("<".into()),
                TokenKind::BinarySelector(">".into()),
                TokenKind::BinarySelector("=".into()),
                TokenKind::BinarySelector("~".into()),
            ]
        );
    }

    #[test]
    fn lex_compound_binary_selectors() {
        assert_eq!(
            lex_kinds("<= >= == ~="),
            vec![
                TokenKind::BinarySelector("<=".into()),
                TokenKind::BinarySelector(">=".into()),
                TokenKind::BinarySelector("==".into()),
                TokenKind::BinarySelector("~=".into()),
            ]
        );
    }

    #[test]
    fn lex_negative_numbers() {
        assert_eq!(
            lex_kinds("-42 -3.14"),
            vec![
                TokenKind::Integer("-42".into()),
                TokenKind::Float("-3.14".into()),
            ]
        );
    }

    #[test]
    fn lex_message_send() {
        assert_eq!(
            lex_kinds("array at: 1 put: value"),
            vec![
                TokenKind::Identifier("array".into()),
                TokenKind::Keyword("at:".into()),
                TokenKind::Integer("1".into()),
                TokenKind::Keyword("put:".into()),
                TokenKind::Identifier("value".into()),
            ]
        );
    }

    #[test]
    fn lex_block() {
        assert_eq!(
            lex_kinds("[:x | x + 1]"),
            vec![
                TokenKind::LeftBracket,
                TokenKind::Colon,
                TokenKind::Identifier("x".into()),
                TokenKind::Pipe,
                TokenKind::Identifier("x".into()),
                TokenKind::BinarySelector("+".into()),
                TokenKind::Integer("1".into()),
                TokenKind::RightBracket,
            ]
        );
    }

    #[test]
    fn lex_preserves_trivia() {
        let tokens = lex("  x  ");
        assert_eq!(tokens.len(), 1);
        let token = &tokens[0];
        assert!(matches!(token.kind(), TokenKind::Identifier(_)));
        assert_eq!(token.leading_trivia().len(), 1);
        assert_eq!(token.trailing_trivia().len(), 1);
    }

    #[test]
    fn lex_line_comment_as_trivia() {
        let tokens = lex("x // comment\ny");
        assert_eq!(tokens.len(), 2);
        assert!(tokens[0].has_trailing_comment());
    }

    #[test]
    fn lex_block_comment_as_trivia() {
        let tokens = lex("/* comment */ x");
        assert_eq!(tokens.len(), 1);
        assert!(tokens[0].has_leading_comment());
    }

    #[test]
    fn lex_spans_are_correct() {
        let tokens = lex("foo bar");
        assert_eq!(tokens[0].span().start(), 0);
        assert_eq!(tokens[0].span().end(), 3);
        assert_eq!(tokens[1].span().start(), 4);
        assert_eq!(tokens[1].span().end(), 7);
    }

    #[test]
    fn lex_error_recovery_unknown_char() {
        let kinds = lex_kinds("x § y");
        assert_eq!(kinds.len(), 3);
        assert!(matches!(kinds[0], TokenKind::Identifier(_)));
        assert!(matches!(kinds[1], TokenKind::Error(_)));
        assert!(matches!(kinds[2], TokenKind::Identifier(_)));
    }

    #[test]
    fn lex_error_recovery_unterminated_string() {
        let kinds = lex_kinds("'unterminated");
        assert_eq!(kinds.len(), 1);
        assert!(matches!(kinds[0], TokenKind::Error(_)));
    }
}
