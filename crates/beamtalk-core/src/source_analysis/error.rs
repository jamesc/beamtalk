// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Error types for the Beamtalk compiler.
//!
//! Errors carry source locations ([`Span`]) for precise diagnostics.
//! They integrate with [`miette`] for beautiful error reporting.

// Spurious warnings from miette derive macro expansion
#![allow(unused_assignments)]

use miette::Diagnostic;
use thiserror::Error;

use super::Span;

/// A lexical error encountered during tokenization.
///
/// The lexer uses error recovery, so lexical errors don't stop parsing.
/// Lexical problems are reported via error tokens in the token stream,
/// which can be inspected and converted into [`LexError`] values for diagnostics.
#[derive(Debug, Clone, PartialEq, Eq, Error, Diagnostic)]
#[error("{kind}")]
#[diagnostic()]
pub struct LexError {
    /// The kind of lexical error.
    #[source]
    pub kind: LexErrorKind,
    /// The source location of the error.
    #[label("here")]
    pub span: Span,
}

impl LexError {
    /// Creates a new lexical error.
    #[must_use]
    pub fn new(kind: LexErrorKind, span: Span) -> Self {
        Self { kind, span }
    }

    #[cfg(test)]
    /// Creates an "unexpected character" error.
    #[must_use]
    pub fn unexpected_char(c: char, span: Span) -> Self {
        Self::new(LexErrorKind::UnexpectedCharacter(c), span)
    }

    #[cfg(test)]
    /// Creates an "unterminated string" error.
    #[must_use]
    pub fn unterminated_string(span: Span) -> Self {
        Self::new(LexErrorKind::UnterminatedString, span)
    }
}

/// The kind of lexical error.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum LexErrorKind {
    /// An unexpected character was encountered.
    #[error("unexpected character '{0}'")]
    UnexpectedCharacter(char),

    /// A string literal was not terminated.
    #[error("unterminated string literal")]
    UnterminatedString,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_error_display() {
        let err = LexError::unexpected_char('§', Span::new(0, 2));
        assert_eq!(err.to_string(), "unexpected character '§'");

        let err = LexError::unterminated_string(Span::new(0, 10));
        assert_eq!(err.to_string(), "unterminated string literal");
    }

    #[test]
    fn lex_error_span() {
        let err = LexError::new(LexErrorKind::UnterminatedString, Span::new(5, 15));
        assert_eq!(err.span.start(), 5);
        assert_eq!(err.span.end(), 15);
    }
}
