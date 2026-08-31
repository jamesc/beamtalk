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

pub mod class_span;
mod error;
mod lexer;
pub mod method_category;
pub mod method_span;
mod parser;
pub mod summary;
mod token;

// Property-based tests for the lexer (ADR 0011 Phase 2)
#[cfg(test)]
mod lexer_property_tests;

// Shared `.bt` corpus-walking helpers for the corpus test suites below, and
// for `crate::unparse`'s corpus conformance tests (BT-3346) — `pub(crate)`
// rather than private so both can reach it.
#[cfg(test)]
pub(crate) mod corpus_test_support;

// Corpus-wide divider validation for method categories (BT-2601 recognizer,
// BT-2626 stdlib-wide curation).
#[cfg(test)]
mod method_category_corpus_tests;

// Corpus round-trip validation for the byte-span resolver (ADR 0082, Phase 0).
#[cfg(test)]
mod method_span_corpus_tests;

pub use class_span::{ClassSpanResolveError, class_state_field_defaults, resolve_class_span};
pub use error::{LexError, LexErrorKind};
pub use lexer::{Lexer, lex, lex_with_eof};
pub use method_category::{
    CategorizeMethodsError, CategorizedMethod, MethodCategory, categorize_methods,
    categorize_methods_in_source, parse_divider_name,
};
pub use method_span::{MethodSide, SpanResolveError, resolve_method_span};
pub use parser::{
    Diagnostic, DiagnosticCategory, DiagnosticNote, Severity, is_input_complete,
    needs_blank_line_to_complete, parse, parse_method,
};
// `Span` is defined in the shared leaf module `crate::span` (ADR 0117, Decision
// step 4) — beneath both `ast` and `source_analysis` — and re-exported here so
// existing `source_analysis::Span` call sites keep working unchanged.
pub use crate::span::Span;
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
/// selectors (LSP, MCP) must use [`validate_selector_input`] (which delegates
/// here) so both the boolean rule and its error messages stay in one place.
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

/// Validates a user-supplied selector string, returning an error description
/// on failure.
///
/// Returns `Ok(())` for valid selectors. Returns `Err(message)` with a
/// human-readable description for two failure modes:
/// - empty input
/// - a non-empty string that fails the shape rules of [`is_valid_selector`]
///
/// This is the single source of truth for selector validation error messages.
/// Callers (LSP, MCP) convert the `String` error into their own error type:
///
/// ```text
/// // LSP (returns Result<(), String>):
/// beamtalk_core::source_analysis::validate_selector_input(sel)?;
///
/// // MCP (returns Result<(), rmcp::ErrorData>):
/// beamtalk_core::source_analysis::validate_selector_input(sel)
///     .map_err(|e| rmcp::ErrorData::invalid_params(e, None))?;
/// ```
///
/// # Errors
///
/// Returns `Err(message)` when `sel` is empty or fails [`is_valid_selector`].
pub fn validate_selector_input(sel: &str) -> Result<(), String> {
    if sel.is_empty() {
        return Err("selector must not be empty".to_string());
    }
    if !is_valid_selector(sel) {
        return Err(format!("invalid selector: '{sel}'"));
    }
    Ok(())
}

/// Returns `true` if `c` should be treated as part of a completion "word" —
/// the token tab-completion / hover word-boundary scanners use to find the
/// prefix currently being typed.
///
/// Deliberately broader than a strict lexical identifier: `:` is included so
/// keyword selectors like `ifTrue:` and `ifTrue:ifFalse:` complete as a
/// single unit, and `@` so package-qualified names like `json@Parser` do too
/// (BT-1659). This is the canonical Rust definition — the CLI REPL's
/// tab-completer (`crates/beamtalk-cli/src/commands/repl/helper.rs`) and the
/// LSP's static completion provider (`language_service::completion_provider`) both
/// delegate here rather than keeping their own copies (BT-3083).
///
/// The live REPL/MCP completion engine (`beamtalk_repl_ops_dev:is_identifier_char/1`
/// in `runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_dev.erl`) cannot
/// depend on this Rust crate, so it re-implements the identical rule in
/// Erlang; a shared corpus fixture
/// (`runtime/apps/beamtalk_workspace/test/fixtures/completion_word_boundary_corpus.json`)
/// pins both implementations to the same cases so the two can't silently
/// drift apart — see `completion_word_char_matches_shared_corpus` below and
/// `beamtalk_repl_ops_dev_tests:is_identifier_char_matches_shared_corpus_test/0`.
pub fn is_completion_word_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_' || c == ':' || c == '@'
}

/// Finds the byte offset where the completion "word" ending at `text` began,
/// scanning backward from the end of `text` for the first character that
/// isn't [`is_completion_word_char`].
///
/// Returns `0` if `text` is a single word (or empty) — the whole string is
/// the word.
pub fn completion_word_start(text: &str) -> usize {
    text.char_indices()
        .rev()
        .find(|&(_, c)| !is_completion_word_char(c))
        .map_or(0, |(i, c)| i + c.len_utf8())
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

#[cfg(test)]
mod completion_word_boundary_tests {
    use super::*;

    #[test]
    fn selector_completes_as_one_word() {
        assert_eq!(completion_word_start("ifTrue:"), 0);
        assert_eq!(completion_word_start("Integer ifT:"), 8);
    }

    #[test]
    fn qualified_name_completes_as_one_word() {
        assert_eq!(completion_word_start("json@Parser"), 0);
        assert_eq!(completion_word_start("json@Parser pa"), 12);
    }

    #[test]
    fn plain_identifier_boundary() {
        assert_eq!(completion_word_start("obj message"), 4);
        assert_eq!(completion_word_start(""), 0);
    }

    /// BT-3083 conformance: every case in the shared corpus must classify
    /// identically here and in the Erlang REPL/MCP completion engine's
    /// `is_identifier_char/1`. The corpus is the single source of truth both
    /// implementations are pinned to; the Erlang side asserts the identical
    /// cases in
    /// `beamtalk_repl_ops_dev_tests:is_identifier_char_matches_shared_corpus_test/0`.
    #[test]
    fn completion_word_char_matches_shared_corpus() {
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("crates/")
            .parent()
            .expect("repo root")
            .join(
                "runtime/apps/beamtalk_workspace/test/fixtures/completion_word_boundary_corpus.json",
            );
        let raw = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("read corpus {}: {e}", path.display()));
        let cases: Vec<serde_json::Value> =
            serde_json::from_str(&raw).expect("corpus is a JSON array");
        assert!(!cases.is_empty(), "corpus must have cases");
        for case in &cases {
            let ch_str = case["char"].as_str().expect("case.char is a string");
            let ch = ch_str.chars().next().expect("case.char is non-empty");
            let expected = case["is_word_char"]
                .as_bool()
                .expect("case.is_word_char is a bool");
            let why = case["why"].as_str().unwrap_or("");
            assert_eq!(
                is_completion_word_char(ch),
                expected,
                "corpus mismatch for char {ch:?} ({why})"
            );
        }
    }
}
