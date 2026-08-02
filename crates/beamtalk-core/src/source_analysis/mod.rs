// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Parsing infrastructure for Beamtalk source code.
//!
//! **DDD Context:** Source Analysis
//!
//! This module contains the lexer, parser, and AST definitions.
//!
//! # Lexical Analysis
//!
//! The [`Lexer`] converts source text into a stream of [`Token`]s. Each token
//! carries its source location via [`Span`] and supports trivia tracking
//! (whitespace and comments) for formatting tools.
//!
//! ```
//! use beamtalk_core::source_analysis::{Lexer, TokenKind};
//!
//! let tokens: Vec<_> = Lexer::new("x + 1").collect();
//! assert_eq!(tokens.len(), 3); // x, +, 1
//! ```
//!
//! See [`TokenKind`] for all supported syntactic elements.
//!
//! # Parsing
//!
//! The [`parse`] function converts tokens into a [`Module`](crate::ast::Module) AST.
//! Binary operator precedence uses Pratt parsing for correct associativity and
//! easy extensibility (see [`parser`] module for details).
//!
//! # Error Handling
//!
//! The lexer uses error recovery: invalid input is converted into
//! [`TokenKind::Error`] tokens rather than stopping. These tokens carry all the
//! information needed for diagnostics, so downstream code should inspect
//! `TokenKind::Error` variants when reporting lexing problems.
//!
//! Use [`LexError`] to construct structured diagnostics with miette integration.

mod error;
mod lexer;
pub mod method_span;
mod parser;
mod span;
pub mod summary;
mod token;

// Property-based tests for the lexer (ADR 0011 Phase 2)
#[cfg(test)]
mod lexer_property_tests;

// Corpus round-trip validation for the byte-span resolver (ADR 0082, Phase 0).
#[cfg(test)]
mod method_span_corpus_tests;

pub use error::{LexError, LexErrorKind};
pub use lexer::{Lexer, lex, lex_with_eof};
pub use method_span::{MethodSide, SpanResolveError, resolve_method_span};
pub use parser::{
    Diagnostic, DiagnosticCategory, DiagnosticNote, Severity, is_input_complete,
    needs_blank_line_to_complete, parse, parse_method,
};
pub use span::Span;
pub use summary::{DiagnosticSummary, SeverityCounts, category_name};
pub use token::{Token, TokenKind, Trivia};

/// Returns `true` if `name` is a valid Beamtalk class name.
///
/// A valid class name:
/// - is non-empty
/// - starts with an ASCII uppercase letter
/// - contains only ASCII alphanumeric characters and underscores
///
/// This is the canonical definition; tools that validate user-supplied class
/// names (LSP, MCP, CLI) must delegate their boolean check here so the rule
/// stays in one place.
pub fn is_valid_class_name(name: &str) -> bool {
    !name.is_empty()
        && name.starts_with(|c: char| c.is_ascii_uppercase())
        && name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
}

/// Returns `true` if `sel` is a valid Beamtalk selector.
///
/// Three shapes are accepted:
/// - **Unary / keyword**: non-empty, all characters are ASCII alphanumeric,
///   underscore, or `:` (e.g. `increment`, `at:put:`).
/// - **Binary operator**: non-empty, all characters are operator characters
///   from the set `+ - * / < > = ~ % & ? , \` (e.g. `+`, `>=`, `**`).
///
/// Mixed shapes (e.g. a string that starts with an operator char but also
/// contains alphanumerics) are rejected.
///
/// This is the canonical definition; tools that validate user-supplied
/// selectors (LSP, MCP) must delegate their boolean check here so the rule
/// stays in one place.
pub fn is_valid_selector(sel: &str) -> bool {
    fn is_binary_selector_char(c: char) -> bool {
        matches!(
            c,
            '+' | '-' | '*' | '/' | '<' | '>' | '=' | '~' | '%' | '&' | '?' | ',' | '\\'
        )
    }

    let mut chars = sel.chars();
    match chars.next() {
        None => false,
        Some(first) if is_binary_selector_char(first) => chars.all(is_binary_selector_char),
        Some(_) => sel
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == ':'),
    }
}

#[cfg(test)]
mod naming_tests {
    use super::*;

    #[test]
    fn valid_simple() {
        assert!(is_valid_class_name("Counter"));
        assert!(is_valid_class_name("FooBarBaz"));
        assert!(is_valid_class_name("X123"));
        assert!(is_valid_class_name("X_y"));
        assert!(is_valid_class_name("A"));
    }

    #[test]
    fn invalid_empty() {
        assert!(!is_valid_class_name(""));
    }

    #[test]
    fn invalid_lowercase_start() {
        assert!(!is_valid_class_name("counter"));
        assert!(!is_valid_class_name("myClass"));
    }

    #[test]
    fn invalid_bad_chars() {
        assert!(!is_valid_class_name("With Space"));
        assert!(!is_valid_class_name("Bad!"));
        assert!(!is_valid_class_name("Has-Hyphen"));
        assert!(!is_valid_class_name("Has.Dot"));
    }

    #[test]
    fn invalid_digit_start() {
        assert!(!is_valid_class_name("123Foo"));
    }

    #[test]
    fn selector_unary() {
        assert!(is_valid_selector("increment"));
        assert!(is_valid_selector("size"));
        assert!(is_valid_selector("printString"));
    }

    #[test]
    fn selector_keyword() {
        assert!(is_valid_selector("at:"));
        assert!(is_valid_selector("at:put:"));
        assert!(is_valid_selector("do:separatedBy:"));
    }

    #[test]
    fn selector_binary() {
        assert!(is_valid_selector("+"));
        assert!(is_valid_selector(">="));
        assert!(is_valid_selector("**"));
        assert!(is_valid_selector("~="));
    }

    #[test]
    fn selector_empty_is_invalid() {
        assert!(!is_valid_selector(""));
    }

    #[test]
    fn selector_mixed_shapes_invalid() {
        assert!(!is_valid_selector("+foo"));
        assert!(!is_valid_selector("foo+"));
        assert!(!is_valid_selector("has space"));
        assert!(!is_valid_selector("bad!char"));
    }
}
