// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Property-based tests for the Beamtalk lexer.
//!
//! These tests use `proptest` to verify lexer invariants over generated inputs:
//!
//! 1. **Lexer never panics** — arbitrary string input always produces tokens
//! 2. **Token spans within input** — all token spans satisfy `end <= input.len()`
//! 3. **Token spans are non-overlapping** — token spans don't overlap
//! 4. **EOF is always last** — `lex_with_eof` always ends with EOF
//! 5. **Lexer is deterministic** — same input always produces same tokens
//! 6. **Valid fragments produce no errors** — known-valid inputs lex cleanly
//!
//! **DDD Context:** Source Analysis
//!
//! ADR 0011 Phase 2 (extended).

use proptest::prelude::*;

use super::lexer::{lex, lex_with_eof};
use super::token::TokenKind;

// ============================================================================
// Generators
// ============================================================================

/// Known-valid single-token fragments that should lex without errors.
const VALID_SINGLE_TOKENS: &[&str] = &[
    "42",
    "3.14",
    "\"hello\"",
    "true",
    "false",
    "nil",
    "x",
    "myVariable",
    "+",
    "-",
    "*",
    "(",
    ")",
    "[",
    "]",
    "$A",
    "#symbol",
    "at:",
    "ifTrue:",
    ":=",
    "^",
    ";",
    "=>",
    "@primitive",
    "@intrinsic",
];

/// Multi-token valid expressions that should lex cleanly.
const VALID_EXPRESSIONS: &[&str] = &[
    "x + 1",
    "arr at: 1",
    "obj at: 1 put: 2",
    "[:x | x + 1]",
    "(3 + 4)",
    "^42",
    "self",
    "#(1, 2, 3)",
    "#{a => 1}",
    "x := 42",
    "3 timesRepeat: [x := x + 1]",
];

fn valid_single_token() -> impl Strategy<Value = String> {
    prop::sample::select(VALID_SINGLE_TOKENS).prop_map(std::string::ToString::to_string)
}

fn valid_expression() -> impl Strategy<Value = String> {
    prop::sample::select(VALID_EXPRESSIONS).prop_map(std::string::ToString::to_string)
}

// ============================================================================
// Property tests
// ============================================================================

/// Default is 512 cases; override via `PROPTEST_CASES` env var for nightly runs.
fn proptest_config() -> ProptestConfig {
    let default = ProptestConfig::default();
    ProptestConfig {
        cases: default.cases.max(512),
        ..default
    }
}

proptest! {
    #![proptest_config(proptest_config())]

    /// Property 1: Lexer never panics on arbitrary string input.
    #[test]
    fn lexer_never_panics(input in "\\PC{0,500}") {
        let _tokens = lex(&input);
    }

    /// Property 1b: Lexer never panics with lex_with_eof on arbitrary input.
    #[test]
    fn lexer_with_eof_never_panics(input in "\\PC{0,500}") {
        let _tokens = lex_with_eof(&input);
    }

    /// Property 2: All token spans are within input bounds.
    #[test]
    fn token_spans_within_input(input in "\\PC{0,500}") {
        let tokens = lex_with_eof(&input);
        let input_len = u32::try_from(input.len()).unwrap_or(u32::MAX);
        for token in &tokens {
            let span = token.span();
            prop_assert!(
                span.end() <= input_len,
                "Token {:?} span end {} exceeds input length {} for input {:?}",
                token.kind(),
                span.end(),
                input_len,
                input,
            );
            prop_assert!(
                span.start() <= span.end(),
                "Token {:?} span start {} > end {} for input {:?}",
                token.kind(),
                span.start(),
                span.end(),
                input,
            );
        }
    }

    /// Property 3: Token spans are non-overlapping and ordered.
    ///
    /// Non-trivia token spans should be monotonically increasing (each token's
    /// start >= previous token's end). We skip the EOF token which has a
    /// zero-length span at the end.
    #[test]
    fn token_spans_non_overlapping(input in "\\PC{0,500}") {
        let tokens = lex(&input);
        for window in tokens.windows(2) {
            let prev = &window[0];
            let next = &window[1];
            prop_assert!(
                next.span().start() >= prev.span().end(),
                "Overlapping spans: {:?} at {:?} and {:?} at {:?} for input {:?}",
                prev.kind(),
                prev.span(),
                next.kind(),
                next.span(),
                input,
            );
        }
    }

    /// Property 4: lex_with_eof always ends with EOF.
    #[test]
    fn eof_always_last(input in "\\PC{0,500}") {
        let tokens = lex_with_eof(&input);
        prop_assert!(!tokens.is_empty(), "lex_with_eof should never return empty");
        prop_assert!(
            tokens.last().unwrap().kind().is_eof(),
            "Last token should be EOF, got {:?} for input {:?}",
            tokens.last().unwrap().kind(),
            input,
        );
    }

    /// Property 5: Lexer is deterministic — same input, same tokens.
    #[test]
    fn lexer_deterministic(input in "\\PC{0,200}") {
        let tokens1 = lex_with_eof(&input);
        let tokens2 = lex_with_eof(&input);
        prop_assert_eq!(
            tokens1.len(),
            tokens2.len(),
            "Different token counts for same input {:?}",
            input,
        );
        for (i, (t1, t2)) in tokens1.iter().zip(tokens2.iter()).enumerate() {
            prop_assert_eq!(
                t1.kind(),
                t2.kind(),
                "Token {} differs: {:?} vs {:?} for input {:?}",
                i,
                t1.kind(),
                t2.kind(),
                input,
            );
            prop_assert_eq!(
                t1.span(),
                t2.span(),
                "Token {} span differs for input {:?}",
                i,
                input,
            );
        }
    }

    /// Property 6: Known-valid single tokens produce no Error tokens.
    #[test]
    fn valid_tokens_no_errors(input in valid_single_token()) {
        let tokens = lex(&input);
        for token in &tokens {
            prop_assert!(
                !token.kind().is_error(),
                "Valid input {:?} produced error token {:?}",
                input,
                token.kind(),
            );
        }
    }

    /// Property 7: Known-valid expressions produce no Error tokens.
    #[test]
    fn valid_expressions_no_errors(input in valid_expression()) {
        let tokens = lex(&input);
        for token in &tokens {
            prop_assert!(
                !token.kind().is_error(),
                "Valid expression {:?} produced error token {:?}",
                input,
                token.kind(),
            );
        }
    }

    /// Property 8: Non-empty input produces at least one token.
    ///
    /// For any non-empty, non-whitespace-only, non-comment input, the lexer
    /// should produce at least one non-EOF token (even if it's an Error token).
    /// Comments and whitespace are stored as trivia, not tokens, so input that
    /// is purely comment/whitespace legitimately produces zero tokens.
    #[test]
    fn nonempty_input_produces_tokens(input in "[^ \t\n\r]{1,100}") {
        // Skip inputs that are purely comments (line or block)
        if input.starts_with("//") || input.starts_with("/*") {
            return Ok(());
        }
        let tokens = lex(&input);
        prop_assert!(
            !tokens.is_empty(),
            "Non-whitespace non-comment input {:?} produced zero tokens (excluding EOF)",
            input,
        );
    }

    /// Property 9: String interpolation tokens are well-formed.
    ///
    /// If a StringStart token appears, we should eventually see a StringEnd
    /// token (or an Error/EOF). The sequence should be:
    /// StringStart, (expr tokens, StringSegment)*, expr tokens, StringEnd
    #[test]
    fn string_interpolation_well_formed(input in "\"[a-z ]{0,10}\\{[a-z0-9]{1,5}\\}[a-z ]{0,10}\"") {
        let tokens = lex_with_eof(&input);
        let mut saw_start = false;
        let mut saw_end = false;
        for token in &tokens {
            match token.kind() {
                TokenKind::StringStart(_) => {
                    prop_assert!(!saw_start, "Multiple StringStart tokens");
                    saw_start = true;
                }
                TokenKind::StringEnd(_) => {
                    prop_assert!(saw_start, "StringEnd without StringStart");
                    saw_end = true;
                }
                _ => {}
            }
        }
        if saw_start {
            prop_assert!(
                saw_end,
                "StringStart without StringEnd for input {:?}",
                input,
            );
        }
    }
}
