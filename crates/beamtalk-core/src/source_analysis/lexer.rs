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
//! use beamtalk_core::source_analysis::{Lexer, TokenKind};
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
    /// Buffered tokens from string interpolation (drained before lexing more).
    pending_tokens: Vec<Token>,
    /// Override span for the current token (used for `StringStart`).
    override_span: Option<Span>,
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
            pending_tokens: Vec::new(),
            override_span: None,
        }
    }

    /// Peeks at the next character without consuming it.
    fn peek_char(&mut self) -> Option<char> {
        self.chars.peek().map(|&(_, c)| c)
    }

    /// Peeks `n+1` characters ahead without consuming (n=0 is same as `peek_char`,
    /// n=1 returns the second character, n=2 the third, etc.).
    fn peek_char_n(&self, n: usize) -> Option<char> {
        let mut iter = self.chars.clone();
        for _ in 0..n {
            iter.next();
        }
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
                Some('/')
                    if self.peek_char_n(1) == Some('/')
                        && self.peek_char_n(2) == Some('/')
                        && self.peek_char_n(3) != Some('/') =>
                {
                    self.lex_doc_comment();
                }
                Some('/') if self.peek_char_n(1) == Some('/') => {
                    self.lex_line_comment();
                }
                Some('/') if self.peek_char_n(1) == Some('*') => {
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

    /// Lexes a doc comment: `/// ...`
    ///
    /// Only exactly three slashes count as a doc comment.
    /// Four or more slashes (`////...`) are regular line comments.
    fn lex_doc_comment(&mut self) {
        let start = self.current_position();
        self.advance(); // /
        self.advance(); // /
        self.advance(); // /
        self.advance_while(|c| c != '\n');
        let text = self.text_for(self.span_from(start));
        self.pending_trivia
            .push(Trivia::DocComment(EcoString::from(text)));
    }

    /// Lexes a block comment: `/* ... */`
    fn lex_block_comment(&mut self) {
        let start = self.current_position();
        self.advance(); // /
        self.advance(); // *

        loop {
            match self.peek_char() {
                None => break, // Unterminated - recover gracefully
                Some('*') if self.peek_char_n(1) == Some('/') => {
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
        // Drain buffered tokens from string interpolation first
        if let Some(token) = self.pending_tokens.pop() {
            return token;
        }

        self.skip_trivia();
        let leading_trivia = std::mem::take(&mut self.pending_trivia);

        let start = self.current_position();

        let kind = match self.peek_char() {
            None => TokenKind::Eof,
            Some(c) => self.lex_token_kind(c, start),
        };

        let span = self
            .override_span
            .take()
            .unwrap_or_else(|| self.span_from(start));

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
                Some('/')
                    if self.peek_char_n(1) == Some('/')
                        && self.peek_char_n(2) == Some('/')
                        && self.peek_char_n(3) != Some('/') =>
                {
                    self.lex_doc_comment();
                    break; // Doc comment ends trailing trivia
                }
                Some('/') if self.peek_char_n(1) == Some('/') => {
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
            '"' => self.lex_string(),

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
            '!' => {
                self.advance();
                TokenKind::Bang
            }
            '|' => {
                self.advance();
                TokenKind::Pipe
            }

            // Colon or assignment
            ':' => self.lex_colon_or_assign(),

            // Fat arrow (=>) for method definitions, or Erlang comparison operators (ADR 0002)
            '=' => {
                if self.peek_char_n(1) == Some('>') {
                    // Check for `=>/=` — should this be `=> /=`? No, `=>` is only at
                    // method definitions where `/=` wouldn't follow. So `=>` is safe.
                    self.advance(); // =
                    self.advance(); // >
                    TokenKind::FatArrow
                } else if self.peek_char_n(1) == Some(':') && self.peek_char_n(2) == Some('=') {
                    // `=:=` — strict equality (ADR 0002)
                    self.advance(); // =
                    self.advance(); // :
                    self.advance(); // =
                    TokenKind::BinarySelector(EcoString::from("=:="))
                } else if self.peek_char_n(1) == Some('/') && self.peek_char_n(2) == Some('=') {
                    // `=/=` — strict inequality (ADR 0002)
                    self.advance(); // =
                    self.advance(); // /
                    self.advance(); // =
                    TokenKind::BinarySelector(EcoString::from("=/="))
                } else {
                    self.lex_binary_selector()
                }
            }

            // Pragma directives
            '@' => self.lex_at_directive(start),

            // Binary operators
            '+' | '-' | '*' | '/' | '<' | '>' | '~' | '%' | '&' | '?' | ',' | '\\' => {
                self.lex_binary_selector()
            }

            // Single quote — no longer valid string syntax (ADR 0023)
            '\'' => self.lex_single_quoted_string_error(),

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
        if self.peek_char() == Some(':') && self.peek_char_n(1) != Some('=') {
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
            && self.peek_char_n(1).is_some_and(|c| c.is_ascii_digit())
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

    /// Lexes a double-quoted string literal, detecting `{expr}` interpolation segments.
    ///
    /// Plain strings without `{expr}` produce a single `TokenKind::String`.
    /// Interpolated strings produce `StringStart`, expression tokens, optional
    /// `StringSegment`s, and `StringEnd`.
    fn lex_string(&mut self) -> TokenKind {
        let string_start = self.current_position();
        self.advance(); // opening quote

        let mut seg_start = self.current_position();
        let mut content_buf = String::new();
        let mut segments: Vec<(EcoString, u32, u32)> = Vec::new();
        let mut interp_ranges: Vec<(u32, u32)> = Vec::new();
        let mut has_interpolation = false;

        loop {
            match self.peek_char() {
                None => {
                    let text = self.text_for(self.span_from(string_start));
                    return TokenKind::Error(EcoString::from(text));
                }
                Some('"') if self.peek_char_n(1) == Some('"') => {
                    // Doubled delimiter: "" → literal "
                    self.advance(); // first "
                    self.advance(); // second "
                    content_buf.push('"');
                }
                Some('"') => {
                    // Closing delimiter
                    let seg_end = self.current_position();
                    segments.push((EcoString::from(content_buf.as_str()), seg_start, seg_end));
                    self.advance(); // closing quote
                    break;
                }
                Some('\\') => {
                    self.advance();
                    if self.peek_char().is_none() {
                        let text = self.text_for(self.span_from(string_start));
                        return TokenKind::Error(EcoString::from(text));
                    }
                    content_buf.push('\\');
                    if let Some(c) = self.advance() {
                        content_buf.push(c);
                    }
                }
                Some('{') => {
                    has_interpolation = true;
                    let seg_end = self.current_position();
                    segments.push((EcoString::from(content_buf.as_str()), seg_start, seg_end));
                    content_buf.clear();
                    self.advance(); // consume `{`

                    match self.lex_interpolation_body(string_start) {
                        Ok((interp_start, interp_end)) => {
                            interp_ranges.push((interp_start, interp_end));
                            seg_start = self.current_position();
                        }
                        Err(error_kind) => return error_kind,
                    }
                }
                Some(c) => {
                    self.advance();
                    content_buf.push(c);
                }
            }
        }

        if !has_interpolation {
            return TokenKind::String(segments[0].0.clone());
        }

        // Set the span override for StringStart to cover opening `"` through first `{`
        let first_seg = &segments[0];
        self.override_span = Some(Span::new(string_start, first_seg.2));

        self.build_interpolation_tokens(&segments, &interp_ranges);
        TokenKind::StringStart(first_seg.0.clone())
    }

    /// Scans the body of a `{...}` interpolation, tracking brace depth.
    /// Returns `Ok((start, end))` of the expression range, or `Err` on unterminated input.
    fn lex_interpolation_body(&mut self, string_start: u32) -> Result<(u32, u32), TokenKind> {
        let interp_start = self.current_position();

        // Check for empty braces (only whitespace between { and })
        if self.is_empty_interpolation() {
            // Skip to closing `}`
            while self.peek_char() != Some('}') {
                self.advance();
            }
            self.advance(); // consume `}`
            let pos = self.current_position();
            return Ok((pos, pos)); // empty range signals error token
        }

        let mut depth: u32 = 1;
        while depth > 0 {
            match self.peek_char() {
                None => {
                    let text = self.text_for(self.span_from(string_start));
                    return Err(TokenKind::Error(EcoString::from(text)));
                }
                Some('"') => {
                    self.skip_nested_string();
                }
                Some('{') => {
                    depth += 1;
                    self.advance();
                }
                Some('/') if self.peek_char_n(1) == Some('/') => {
                    // Line comment — skip to newline
                    while self.peek_char().is_some_and(|c| c != '\n') {
                        self.advance();
                    }
                }
                Some('/') if self.peek_char_n(1) == Some('*') => {
                    // Block comment — skip to closing */
                    self.advance(); // /
                    self.advance(); // *
                    loop {
                        match self.peek_char() {
                            None => {
                                let text = self.text_for(self.span_from(string_start));
                                return Err(TokenKind::Error(EcoString::from(text)));
                            }
                            Some('*') if self.peek_char_n(1) == Some('/') => {
                                self.advance(); // *
                                self.advance(); // /
                                break;
                            }
                            _ => {
                                self.advance();
                            }
                        }
                    }
                }
                Some('$') => {
                    // Character literal — skip the $ and the next char
                    self.advance(); // $
                    if self.peek_char() == Some('\\') {
                        self.advance(); // backslash
                        if self.peek_char().is_some() {
                            self.advance(); // escaped char
                        }
                    } else if self.peek_char().is_some() {
                        self.advance(); // literal char
                    }
                }
                Some('}') => {
                    depth -= 1;
                    if depth == 0 {
                        let interp_end = self.current_position();
                        self.advance();
                        return Ok((interp_start, interp_end));
                    }
                    self.advance();
                }
                Some('\\') => {
                    self.advance();
                    if self.peek_char().is_some() {
                        self.advance();
                    }
                }
                _ => {
                    self.advance();
                }
            }
        }
        unreachable!()
    }

    /// Builds buffered tokens for an interpolated string's expression segments.
    fn build_interpolation_tokens(
        &mut self,
        segments: &[(EcoString, u32, u32)],
        interp_ranges: &[(u32, u32)],
    ) {
        let string_end = self.current_position();
        let mut tokens: Vec<Token> = Vec::new();

        for (i, interp) in interp_ranges.iter().enumerate() {
            if i > 0 {
                let seg = &segments[i];
                tokens.push(Token::new(
                    TokenKind::StringSegment(seg.0.clone()),
                    Span::new(seg.1, seg.2),
                ));
            }

            if interp.0 == interp.1 {
                // Empty interpolation — span covers the `{}` delimiters.
                // segments[i].2 is the position of `{`, interp.0 is after `}`.
                let brace_start = segments[i].2;
                tokens.push(Token::new(
                    TokenKind::Error(EcoString::from(
                        "empty interpolation {} is not allowed, use \\{ for literal brace",
                    )),
                    Span::new(brace_start, interp.0),
                ));
            } else {
                let expr_source = self.text_for(Span::new(interp.0, interp.1));
                let sub_lexer = Lexer::new(expr_source);
                for sub_token in sub_lexer {
                    let adjusted_span = Span::new(
                        sub_token.span().start() + interp.0,
                        sub_token.span().end() + interp.0,
                    );
                    let leading = sub_token.leading_trivia().to_vec();
                    let trailing = sub_token.trailing_trivia().to_vec();
                    tokens.push(Token::with_trivia(
                        sub_token.into_kind(),
                        adjusted_span,
                        leading,
                        trailing,
                    ));
                }
            }
        }

        let last_seg = segments.last().unwrap();
        tokens.push(Token::new(
            TokenKind::StringEnd(last_seg.0.clone()),
            Span::new(last_seg.1, string_end),
        ));

        tokens.reverse();
        self.pending_tokens = tokens;
    }

    /// Checks if the current interpolation is empty (only whitespace before `}`).
    /// Does not consume any characters.
    fn is_empty_interpolation(&self) -> bool {
        let mut offset = 0;
        loop {
            let ch = self.source[self.position + offset..].chars().next();
            match ch {
                Some(' ' | '\t' | '\r' | '\n') => offset += 1,
                Some('}') => return true,
                _ => return false,
            }
        }
    }

    /// Skips a nested string literal inside an interpolation expression.
    fn skip_nested_string(&mut self) {
        self.advance(); // opening quote
        loop {
            match self.peek_char() {
                None => break,
                Some('"') if self.peek_char_n(1) == Some('"') => {
                    // Doubled delimiter — skip both quotes
                    self.advance(); // first "
                    self.advance(); // second "
                }
                Some('"') => {
                    self.advance(); // closing quote
                    break;
                }
                Some('\\') => {
                    self.advance();
                    if self.peek_char().is_some() {
                        self.advance();
                    }
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    /// Consumes a single-quoted string and produces a helpful error (ADR 0023).
    fn lex_single_quoted_string_error(&mut self) -> TokenKind {
        self.advance(); // opening quote
        loop {
            match self.peek_char() {
                None => break,
                Some('\'') => {
                    self.advance(); // closing quote
                    break;
                }
                Some('\\') => {
                    self.advance(); // backslash
                    if self.peek_char().is_some() {
                        self.advance(); // escaped char
                    }
                }
                _ => {
                    self.advance();
                }
            }
        }
        TokenKind::Error(EcoString::from(
            "single-quoted strings are no longer supported, use double quotes: \"...\"",
        ))
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
            // Map literal: #{
            Some('{') => {
                self.advance(); // consume '{'
                TokenKind::MapOpen
            }
            // List literal: #(
            Some('(') => {
                self.advance(); // consume '('
                TokenKind::ListOpen
            }
            // Array literal: #[
            Some('[') => {
                self.advance(); // consume '['
                TokenKind::ArrayOpen
            }
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
                            if self.peek_char_n(1) == Some('\'') {
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
            // Identifier symbol: #foo or keyword symbol: #setValue: or #at:put:
            Some(c) if c.is_ascii_alphabetic() || c == '_' => {
                self.advance_while(|c| c.is_ascii_alphanumeric() || c == '_');
                // Check for trailing colon (keyword symbol like #setValue:)
                // Also handle multi-keyword symbols like #at:put:
                if self.peek_char() == Some(':') && self.peek_char_n(1) != Some('=') {
                    self.advance(); // consume first colon
                    // Continue consuming additional keyword parts (e.g., put: in #at:put:)
                    while self
                        .peek_char()
                        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
                    {
                        self.advance_while(|c| c.is_ascii_alphanumeric() || c == '_');
                        if self.peek_char() == Some(':') && self.peek_char_n(1) != Some('=') {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
                let full_text = self.text_for(self.span_from(start));
                // Extract symbol name without #
                let name = &full_text[1..];
                TokenKind::Symbol(EcoString::from(name))
            }
            // Binary operator symbol: #+, #>=, #/=
            Some(c) if Self::is_binary_selector_char(c) => {
                self.advance_while(Self::is_binary_selector_char);
                let full_text = self.text_for(self.span_from(start));
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

    /// Lexes an `@` directive: `@primitive` or error for unknown directives.
    fn lex_at_directive(&mut self, start: u32) -> TokenKind {
        self.advance(); // @

        // Check if followed by an identifier
        let ident_start = self.current_position();
        if self
            .peek_char()
            .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
        {
            self.advance_while(|c| c.is_ascii_alphanumeric() || c == '_');
            let ident = self.text_for(self.span_from(ident_start));
            if ident == "primitive" {
                return TokenKind::AtPrimitive;
            }
            if ident == "intrinsic" {
                return TokenKind::AtIntrinsic;
            }
            if ident == "expect" {
                return TokenKind::AtExpect;
            }
            let text = self.text_for(self.span_from(start));
            return TokenKind::Error(EcoString::from(format!(
                "unknown directive '{text}', only '@primitive', '@intrinsic', and '@expect' are supported"
            )));
        }

        TokenKind::Error(EcoString::from(
            "expected directive name after '@', only '@primitive', '@intrinsic', and '@expect' are supported",
        ))
    }

    /// Returns true if the character is a binary selector (operator) character.
    ///
    /// Shared between `lex_binary_selector()` and symbol operator lexing
    /// in `lex_symbol_or_hash()` to keep the character sets in sync.
    fn is_binary_selector_char(c: char) -> bool {
        matches!(
            c,
            '+' | '-' | '*' | '/' | '<' | '>' | '=' | '~' | '%' | '&' | '?' | ',' | '\\'
        )
    }

    /// Lexes a binary selector (one or more operator characters).
    fn lex_binary_selector(&mut self) -> TokenKind {
        let start = self.current_position();

        // Handle potential negative number: - followed by digit
        if self.peek_char() == Some('-') && self.peek_char_n(1).is_some_and(|c| c.is_ascii_digit())
        {
            self.advance(); // -
            return self.lex_number_with_prefix(start);
        }

        self.advance_while(Self::is_binary_selector_char);

        let text = self.text_for(self.span_from(start));
        TokenKind::BinarySelector(EcoString::from(text))
    }

    /// Lexes a number with a prefix already consumed (e.g., negative sign).
    fn lex_number_with_prefix(&mut self, start: u32) -> TokenKind {
        // Integer part
        self.advance_while(|c| c.is_ascii_digit());

        // Check for float
        let is_float = if self.peek_char() == Some('.')
            && self.peek_char_n(1).is_some_and(|c| c.is_ascii_digit())
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
            lex_kinds(r#""hello" "world" """#),
            vec![
                TokenKind::String("hello".into()),
                TokenKind::String("world".into()),
                TokenKind::String("".into()),
            ]
        );
    }

    #[test]
    fn lex_string_with_escaped_quote() {
        assert_eq!(
            lex_kinds(r#""it\"s""#),
            vec![TokenKind::String("it\\\"s".into())]
        );
    }

    #[test]
    fn lex_string_doubled_delimiter_double_quote() {
        // Beamtalk `""""` = opening-", doubled "" escape, closing-" → String containing `"`
        assert_eq!(lex_kinds("\"\"\"\""), vec![TokenKind::String("\"".into())]);
    }

    #[test]
    fn lex_string_doubled_delimiter_multiple() {
        // Beamtalk `"say ""hello"" please"` → String `say "hello" please`
        assert_eq!(
            lex_kinds("\"say \"\"hello\"\" please\""),
            vec![TokenKind::String("say \"hello\" please".into())]
        );
    }

    #[test]
    fn lex_string_doubled_delimiter_at_boundaries() {
        // Beamtalk `""" hi """` → String `" hi "`
        assert_eq!(
            lex_kinds("\"\"\" hi \"\"\""),
            vec![TokenKind::String("\" hi \"".into())]
        );
    }

    #[test]
    fn lex_string_doubled_delimiter_with_interpolation() {
        // Doubled delimiter combined with interpolation:
        // `"pre "" {x} "" post"` → StringStart(`pre " `), Identifier(x), StringEnd(` " post`)
        assert_eq!(
            lex_kinds("\"pre \"\" {x} \"\" post\""),
            vec![
                TokenKind::StringStart("pre \" ".into()),
                TokenKind::Identifier("x".into()),
                TokenKind::StringEnd(" \" post".into()),
            ]
        );
    }

    #[test]
    fn lex_string_with_backslash_escapes() {
        // Escaped braces produce plain string (no interpolation)
        assert_eq!(
            lex_kinds(r#""hello" "Hello, \{name\}!""#),
            vec![
                TokenKind::String("hello".into()),
                TokenKind::String("Hello, \\{name\\}!".into()),
            ]
        );
    }

    #[test]
    fn lex_string_interpolation_simple() {
        // Unescaped braces trigger interpolation
        assert_eq!(
            lex_kinds(r#""Hello, {name}!""#),
            vec![
                TokenKind::StringStart("Hello, ".into()),
                TokenKind::Identifier("name".into()),
                TokenKind::StringEnd("!".into()),
            ]
        );
    }

    #[test]
    fn lex_string_interpolation_multiple_segments() {
        assert_eq!(
            lex_kinds(r#""{x} and {y}""#),
            vec![
                TokenKind::StringStart("".into()),
                TokenKind::Identifier("x".into()),
                TokenKind::StringSegment(" and ".into()),
                TokenKind::Identifier("y".into()),
                TokenKind::StringEnd("".into()),
            ]
        );
    }

    #[test]
    fn lex_string_interpolation_complex_expression() {
        assert_eq!(
            lex_kinds(r#""result: {x + 1}""#),
            vec![
                TokenKind::StringStart("result: ".into()),
                TokenKind::Identifier("x".into()),
                TokenKind::BinarySelector("+".into()),
                TokenKind::Integer("1".into()),
                TokenKind::StringEnd("".into()),
            ]
        );
    }

    #[test]
    fn lex_string_interpolation_nested_braces() {
        // Nested braces (e.g., tuple literal inside interpolation)
        assert_eq!(
            lex_kinds(r#""value: {#{1 => 2}}""#),
            vec![
                TokenKind::StringStart("value: ".into()),
                TokenKind::MapOpen,
                TokenKind::Integer("1".into()),
                TokenKind::FatArrow,
                TokenKind::Integer("2".into()),
                TokenKind::RightBrace,
                TokenKind::StringEnd("".into()),
            ]
        );
    }

    #[test]
    fn lex_string_interpolation_escaped_braces() {
        // Escaped braces are literal, no interpolation
        assert_eq!(
            lex_kinds(r#""\{not interpolated\}""#),
            vec![TokenKind::String("\\{not interpolated\\}".into())]
        );
    }

    #[test]
    fn lex_string_interpolation_mixed_escaped_and_real() {
        assert_eq!(
            lex_kinds(r#""\{literal\} {x}""#),
            vec![
                TokenKind::StringStart("\\{literal\\} ".into()),
                TokenKind::Identifier("x".into()),
                TokenKind::StringEnd("".into()),
            ]
        );
    }

    #[test]
    fn lex_string_interpolation_empty_braces_error() {
        let tokens = lex(r#""empty: {}""#);
        assert_eq!(tokens.len(), 3); // StringStart, Error, StringEnd
        assert!(matches!(tokens[0].kind(), TokenKind::StringStart(_)));
        assert!(matches!(tokens[1].kind(), TokenKind::Error(_)));
        // Error span covers the `{}` delimiters (positions 8-10)
        assert_eq!(tokens[1].span().start(), 8);
        assert_eq!(tokens[1].span().end(), 10);
        assert!(matches!(tokens[2].kind(), TokenKind::StringEnd(_)));
    }

    #[test]
    fn lex_string_interpolation_at_start() {
        assert_eq!(
            lex_kinds(r#""{x} world""#),
            vec![
                TokenKind::StringStart("".into()),
                TokenKind::Identifier("x".into()),
                TokenKind::StringEnd(" world".into()),
            ]
        );
    }

    #[test]
    fn lex_string_interpolation_at_end() {
        assert_eq!(
            lex_kinds(r#""hello {x}""#),
            vec![
                TokenKind::StringStart("hello ".into()),
                TokenKind::Identifier("x".into()),
                TokenKind::StringEnd("".into()),
            ]
        );
    }

    #[test]
    fn lex_string_interpolation_only() {
        assert_eq!(
            lex_kinds(r#""{x}""#),
            vec![
                TokenKind::StringStart("".into()),
                TokenKind::Identifier("x".into()),
                TokenKind::StringEnd("".into()),
            ]
        );
    }

    #[test]
    fn lex_string_interpolation_keyword_message() {
        assert_eq!(
            lex_kinds(r#""{x printString}""#),
            vec![
                TokenKind::StringStart("".into()),
                TokenKind::Identifier("x".into()),
                TokenKind::Identifier("printString".into()),
                TokenKind::StringEnd("".into()),
            ]
        );
    }

    #[test]
    fn lex_string_no_interpolation_plain() {
        // Plain strings without braces are unchanged
        assert_eq!(
            lex_kinds(r#""hello world""#),
            vec![TokenKind::String("hello world".into())]
        );
    }

    #[test]
    fn lex_string_interpolation_multiline() {
        let source = "\"line1\n{x}\nline2\"";
        assert_eq!(
            lex_kinds(source),
            vec![
                TokenKind::StringStart("line1\n".into()),
                TokenKind::Identifier("x".into()),
                TokenKind::StringEnd("\nline2".into()),
            ]
        );
    }

    #[test]
    fn lex_string_interpolation_three_segments() {
        assert_eq!(
            lex_kinds(r#""{a} + {b} = {c}""#),
            vec![
                TokenKind::StringStart("".into()),
                TokenKind::Identifier("a".into()),
                TokenKind::StringSegment(" + ".into()),
                TokenKind::Identifier("b".into()),
                TokenKind::StringSegment(" = ".into()),
                TokenKind::Identifier("c".into()),
                TokenKind::StringEnd("".into()),
            ]
        );
    }

    #[test]
    fn lex_string_interpolation_followed_by_more_code() {
        // Ensure lexer continues normally after interpolated string
        assert_eq!(
            lex_kinds(r#""hi {x}" printString"#),
            vec![
                TokenKind::StringStart("hi ".into()),
                TokenKind::Identifier("x".into()),
                TokenKind::StringEnd("".into()),
                TokenKind::Identifier("printString".into()),
            ]
        );
    }

    #[test]
    fn lex_string_interpolation_nested_string() {
        // String literal inside interpolation expression
        assert_eq!(
            lex_kinds(r#""msg: {"hello"}""#),
            vec![
                TokenKind::StringStart("msg: ".into()),
                TokenKind::String("hello".into()),
                TokenKind::StringEnd("".into()),
            ]
        );
    }

    #[test]
    fn lex_string_interpolation_nested_string_with_braces() {
        // String containing braces inside interpolation
        assert_eq!(
            lex_kinds(r#""msg: {"a{b}c"}""#),
            vec![
                TokenKind::StringStart("msg: ".into()),
                TokenKind::StringStart("a".into()),
                TokenKind::Identifier("b".into()),
                TokenKind::StringEnd("c".into()),
                TokenKind::StringEnd("".into()),
            ]
        );
    }

    #[test]
    fn lex_string_interpolation_block_in_expr() {
        // Block expression inside interpolation
        assert_eq!(
            lex_kinds(r#""result: {[:x | x + 1] value: 5}""#),
            vec![
                TokenKind::StringStart("result: ".into()),
                TokenKind::LeftBracket,
                TokenKind::Colon,
                TokenKind::Identifier("x".into()),
                TokenKind::Pipe,
                TokenKind::Identifier("x".into()),
                TokenKind::BinarySelector("+".into()),
                TokenKind::Integer("1".into()),
                TokenKind::RightBracket,
                TokenKind::Keyword("value:".into()),
                TokenKind::Integer("5".into()),
                TokenKind::StringEnd("".into()),
            ]
        );
    }

    #[test]
    fn lex_string_interpolation_spans_are_correct() {
        // "Hello, {name}!" — verify each token has correct span
        let tokens = lex(r#""Hello, {name}!""#);
        assert_eq!(tokens.len(), 3);

        // StringStart: "Hello,  — spans from opening " to {
        assert!(matches!(tokens[0].kind(), TokenKind::StringStart(_)));
        assert_eq!(tokens[0].span().start(), 0);
        assert_eq!(tokens[0].span().end(), 8); // "Hello, {

        // Identifier: name — spans the variable name (inside {})
        assert!(matches!(tokens[1].kind(), TokenKind::Identifier(_)));
        assert_eq!(tokens[1].span().start(), 9); // 'n' of name
        assert_eq!(tokens[1].span().end(), 13); // after 'e'

        // StringEnd: ! — spans from after } to closing "
        assert!(matches!(tokens[2].kind(), TokenKind::StringEnd(_)));
        assert_eq!(tokens[2].span().start(), 14); // '!' after }
        assert_eq!(tokens[2].span().end(), 16); // after closing "
    }

    #[test]
    fn lex_string_interpolation_preserves_trivia() {
        // "{ x + y }" — whitespace around expression should be trivia
        let tokens = lex(r#""{ x + y }""#);
        assert_eq!(tokens.len(), 5);
        // StringStart, Identifier(x), BinarySelector(+), Identifier(y), StringEnd
        assert!(matches!(tokens[0].kind(), TokenKind::StringStart(_)));
        assert!(matches!(tokens[1].kind(), TokenKind::Identifier(_)));
        // x should have leading whitespace trivia from the space after {
        assert!(!tokens[1].leading_trivia().is_empty());
        assert!(matches!(tokens[2].kind(), TokenKind::BinarySelector(_)));
        assert!(matches!(tokens[3].kind(), TokenKind::Identifier(_)));
        assert!(matches!(tokens[4].kind(), TokenKind::StringEnd(_)));
    }

    #[test]
    fn lex_string_interpolation_comment_with_brace() {
        // Comment containing } should not close the interpolation
        let kinds = lex_kinds("\"result: {x // } comment\n+ y}\"");
        assert_eq!(
            kinds,
            vec![
                TokenKind::StringStart("result: ".into()),
                TokenKind::Identifier("x".into()),
                TokenKind::BinarySelector("+".into()),
                TokenKind::Identifier("y".into()),
                TokenKind::StringEnd("".into()),
            ]
        );
    }

    #[test]
    fn lex_string_interpolation_block_comment_with_brace() {
        // Block comment containing } should not close the interpolation
        let kinds = lex_kinds("\"val: {x /* } */ + 1}\"");
        assert_eq!(
            kinds,
            vec![
                TokenKind::StringStart("val: ".into()),
                TokenKind::Identifier("x".into()),
                TokenKind::BinarySelector("+".into()),
                TokenKind::Integer("1".into()),
                TokenKind::StringEnd("".into()),
            ]
        );
    }

    #[test]
    fn lex_string_interpolation_char_literal_brace() {
        // Character literal $} should not close the interpolation
        let kinds = lex_kinds("\"char: {$}}\"");
        assert_eq!(
            kinds,
            vec![
                TokenKind::StringStart("char: ".into()),
                TokenKind::Character('}'),
                TokenKind::StringEnd("".into()),
            ]
        );
    }

    #[test]
    fn lex_string_interpolation_empty_with_newline() {
        // Empty braces with newline should still be an error
        let kinds = lex_kinds("\"empty: {\n}\"");
        assert_eq!(kinds.len(), 3);
        assert!(matches!(kinds[0], TokenKind::StringStart(_)));
        assert!(matches!(kinds[1], TokenKind::Error(_)));
        assert!(matches!(kinds[2], TokenKind::StringEnd(_)));
    }

    #[test]
    fn lex_single_quote_produces_helpful_error() {
        let tokens = lex_kinds("'hello'");
        assert_eq!(tokens.len(), 1); // single error token consuming entire literal
        assert_eq!(
            tokens[0],
            TokenKind::Error(
                "single-quoted strings are no longer supported, use double quotes: \"...\"".into()
            )
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
    fn lex_keyword_symbols() {
        assert_eq!(
            lex_kinds("#setValue: #at:put:"),
            vec![
                TokenKind::Symbol("setValue:".into()),
                TokenKind::Symbol("at:put:".into()),
            ]
        );
    }

    #[test]
    fn lex_keyword_symbol_before_assignment() {
        // #foo:= should NOT consume the colon (it's part of :=)
        assert_eq!(
            lex_kinds("#foo:="),
            vec![TokenKind::Symbol("foo".into()), TokenKind::Assign,]
        );
    }

    #[test]
    fn lex_binary_operator_symbols() {
        assert_eq!(
            lex_kinds("#+ #- #>= #/="),
            vec![
                TokenKind::Symbol("+".into()),
                TokenKind::Symbol("-".into()),
                TokenKind::Symbol(">=".into()),
                TokenKind::Symbol("/=".into()),
            ]
        );
    }

    #[test]
    fn lex_map_open() {
        assert_eq!(lex_kinds("#{"), vec![TokenKind::MapOpen]);

        // Test that #{ is recognized as MapOpen, not Hash followed by LeftBrace
        let tokens = lex("#{");
        assert_eq!(tokens.len(), 1);
        assert!(matches!(tokens[0].kind(), TokenKind::MapOpen));
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
            lex_kinds(":= ^ ; . ! | : #"),
            vec![
                TokenKind::Assign,
                TokenKind::Caret,
                TokenKind::Semicolon,
                TokenKind::Period,
                TokenKind::Bang,
                TokenKind::Pipe,
                TokenKind::Colon,
                TokenKind::Hash,
            ]
        );
    }

    #[test]
    fn lex_bang_as_cast_terminator() {
        // BT-919: `!` is a cast (fire-and-forget) statement terminator
        assert_eq!(
            lex_kinds("foo bar!"),
            vec![
                TokenKind::Identifier("foo".into()),
                TokenKind::Identifier("bar".into()),
                TokenKind::Bang,
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
            lex_kinds("<= >= == /= ** =:= =/="),
            vec![
                TokenKind::BinarySelector("<=".into()),
                TokenKind::BinarySelector(">=".into()),
                TokenKind::BinarySelector("==".into()),
                TokenKind::BinarySelector("/=".into()),
                TokenKind::BinarySelector("**".into()),
                TokenKind::BinarySelector("=:=".into()),
                TokenKind::BinarySelector("=/=".into()),
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
        let kinds = lex_kinds("\"unterminated");
        assert_eq!(kinds.len(), 1);
        assert!(matches!(kinds[0], TokenKind::Error(_)));
    }

    #[test]
    fn lex_at_primitive() {
        assert_eq!(lex_kinds("@primitive"), vec![TokenKind::AtPrimitive]);
    }

    #[test]
    fn lex_at_primitive_followed_by_string() {
        assert_eq!(
            lex_kinds("@primitive \"add\""),
            vec![TokenKind::AtPrimitive, TokenKind::String("add".into())]
        );
    }

    #[test]
    fn lex_at_primitive_followed_by_identifier() {
        assert_eq!(
            lex_kinds("@primitive foo"),
            vec![TokenKind::AtPrimitive, TokenKind::Identifier("foo".into()),]
        );
    }

    #[test]
    fn lex_at_intrinsic() {
        assert_eq!(lex_kinds("@intrinsic"), vec![TokenKind::AtIntrinsic]);
    }

    #[test]
    fn lex_at_intrinsic_followed_by_identifier() {
        assert_eq!(
            lex_kinds("@intrinsic blockValue"),
            vec![
                TokenKind::AtIntrinsic,
                TokenKind::Identifier("blockValue".into()),
            ]
        );
    }

    #[test]
    fn lex_at_intrinsic_followed_by_string() {
        assert_eq!(
            lex_kinds("@intrinsic \"size\""),
            vec![TokenKind::AtIntrinsic, TokenKind::String("size".into())]
        );
    }

    #[test]
    fn lex_at_expect() {
        assert_eq!(lex_kinds("@expect"), vec![TokenKind::AtExpect]);
    }

    #[test]
    fn lex_at_expect_followed_by_identifier() {
        assert_eq!(
            lex_kinds("@expect dnu"),
            vec![TokenKind::AtExpect, TokenKind::Identifier("dnu".into())]
        );
    }

    #[test]
    fn lex_at_expect_followed_by_all_category() {
        assert_eq!(
            lex_kinds("@expect all"),
            vec![TokenKind::AtExpect, TokenKind::Identifier("all".into())]
        );
    }

    #[test]
    fn lex_at_unknown_directive_is_error() {
        let kinds = lex_kinds("@unknown");
        assert_eq!(kinds.len(), 1);
        assert!(matches!(kinds[0], TokenKind::Error(_)));
    }

    #[test]
    fn lex_at_bare_is_error() {
        let kinds = lex_kinds("@ ");
        assert_eq!(kinds.len(), 1);
        assert!(matches!(kinds[0], TokenKind::Error(_)));
    }

    #[test]
    fn lex_at_bare_at_eof_is_error() {
        let kinds = lex_kinds("@");
        assert_eq!(kinds.len(), 1);
        assert!(matches!(kinds[0], TokenKind::Error(_)));
    }

    #[test]
    fn lex_at_followed_by_number_is_error() {
        let kinds = lex_kinds("@123");
        assert_eq!(kinds.len(), 2);
        assert!(matches!(kinds[0], TokenKind::Error(_)));
        assert!(matches!(kinds[1], TokenKind::Integer(_)));
    }

    #[test]
    fn lex_at_primitive_span_is_correct() {
        let tokens = lex("@primitive");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].span().start(), 0);
        assert_eq!(tokens[0].span().end(), 10);
    }

    #[test]
    fn lex_doc_comment_as_trivia() {
        let tokens = lex("/// doc comment\nx");
        assert_eq!(tokens.len(), 1);
        assert!(tokens[0].has_leading_comment());
        let doc_trivia: Vec<_> = tokens[0]
            .leading_trivia()
            .iter()
            .filter(|t| t.is_doc_comment())
            .collect();
        assert_eq!(doc_trivia.len(), 1);
        assert_eq!(doc_trivia[0].as_str(), "/// doc comment");
    }

    #[test]
    fn lex_triple_slash_is_doc_comment() {
        let tokens = lex("/// this is a doc comment\nx");
        assert_eq!(tokens.len(), 1);
        let doc_trivia: Vec<_> = tokens[0]
            .leading_trivia()
            .iter()
            .filter(|t| t.is_doc_comment())
            .collect();
        assert_eq!(doc_trivia.len(), 1);
        assert_eq!(doc_trivia[0].as_str(), "/// this is a doc comment");
    }

    #[test]
    fn lex_four_slashes_is_line_comment() {
        let tokens = lex("//// not a doc comment\nx");
        assert_eq!(tokens.len(), 1);
        let doc_trivia: Vec<_> = tokens[0]
            .leading_trivia()
            .iter()
            .filter(|t| t.is_doc_comment())
            .collect();
        assert_eq!(doc_trivia.len(), 0);
        // Should be a regular line comment instead
        let line_trivia: Vec<_> = tokens[0]
            .leading_trivia()
            .iter()
            .filter(|t| matches!(t, Trivia::LineComment(_)))
            .collect();
        assert_eq!(line_trivia.len(), 1);
    }

    #[test]
    fn lex_double_slash_is_line_comment_not_doc() {
        let tokens = lex("// regular comment\nx");
        assert_eq!(tokens.len(), 1);
        let doc_trivia: Vec<_> = tokens[0]
            .leading_trivia()
            .iter()
            .filter(|t| t.is_doc_comment())
            .collect();
        assert_eq!(doc_trivia.len(), 0);
    }

    #[test]
    fn lex_consecutive_doc_comments() {
        let tokens = lex("/// line one\n/// line two\nx");
        assert_eq!(tokens.len(), 1);
        let doc_trivia: Vec<_> = tokens[0]
            .leading_trivia()
            .iter()
            .filter(|t| t.is_doc_comment())
            .collect();
        assert_eq!(doc_trivia.len(), 2);
        assert_eq!(doc_trivia[0].as_str(), "/// line one");
        assert_eq!(doc_trivia[1].as_str(), "/// line two");
    }

    #[test]
    fn lex_empty_doc_comment() {
        let tokens = lex("///\nx");
        assert_eq!(tokens.len(), 1);
        let doc_trivia: Vec<_> = tokens[0]
            .leading_trivia()
            .iter()
            .filter(|t| t.is_doc_comment())
            .collect();
        assert_eq!(doc_trivia.len(), 1);
        assert_eq!(doc_trivia[0].as_str(), "///");
    }

    #[test]
    fn lex_five_slashes_is_line_comment() {
        let tokens = lex("///// five slashes\nx");
        assert_eq!(tokens.len(), 1);
        let doc_trivia: Vec<_> = tokens[0]
            .leading_trivia()
            .iter()
            .filter(|t| t.is_doc_comment())
            .collect();
        assert_eq!(doc_trivia.len(), 0);
    }
}
