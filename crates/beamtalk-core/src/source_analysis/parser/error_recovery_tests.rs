// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Property-based tests for parser error recovery.
//!
//! These tests verify that the parser recovers gracefully from all errors:
//!
//! 1. **No silent failures** — non-whitespace input always produces AST nodes
//!    or diagnostics (never both empty)
//! 2. **Error injection recovery** — inserting garbage into valid code still
//!    produces some AST nodes plus error diagnostics
//! 3. **Error messages are non-empty** — all Error-severity diagnostics carry
//!    a meaningful message
//! 4. **Error count is bounded** — prevents diagnostic explosion on short input
//!
//! **DDD Context:** Source Analysis — Parsing
//!
//! ADR 0011 Phase 2 (extended).

use proptest::prelude::*;

use super::super::{Severity, lex_with_eof, parse};

// ============================================================================
// Generators
// ============================================================================

/// Known-valid Beamtalk fragments for error injection testing.
const VALID_FRAGMENTS: &[&str] = &[
    "42",
    "\"hello\"",
    "true",
    "false",
    "nil",
    "x := 42",
    "x + y",
    "[:x | x + 1]",
    "Object subclass: Foo\n  state: x = 0\n  bar => x",
    "Actor subclass: Counter\n  state: count = 0\n  increment => count := count + 1",
    "#(1, 2, 3)",
    "#{#a => 1}",
    "self",
    "^42",
    "3 timesRepeat: [x := x + 1]",
    "#[first, ...rest] := #[1, 2, 3]",
    "[1] ensure: [nil]",
    "x match: { 1 => \"one\", _ => \"other\" }",
    "Object subclass: Bar\n  greet => \"hello\"",
    "x > 0 ifTrue: ['positive'] ifFalse: ['non-positive']",
];

/// Garbage strings to inject into valid code.
///
/// These are chosen to be syntactically invalid in any Beamtalk context —
/// they cannot be parsed as identifiers, operators, or keyword messages.
const GARBAGE: &[&str] = &[
    "@@@ $$$ @@@",
    "))))",
    "}}}}",
    ">>><<<",
    "!@#$%^&*",
    "=>=>=>",
    ":::::",
    "``~~~``",
    "\\\\\\\\",
];

fn valid_fragment() -> impl Strategy<Value = String> {
    prop::sample::select(VALID_FRAGMENTS).prop_map(std::string::ToString::to_string)
}

fn garbage_string() -> impl Strategy<Value = String> {
    prop::sample::select(GARBAGE).prop_map(std::string::ToString::to_string)
}

/// Insert garbage between two valid fragments (not inside string literals).
///
/// Injecting *inside* a fragment can land inside a string literal where the
/// garbage becomes valid content. Concatenating garbage *between* fragments
/// avoids this and reliably triggers parse errors.
fn inject_error() -> impl Strategy<Value = String> {
    (valid_fragment(), garbage_string(), valid_fragment()).prop_map(|(before, garbage, after)| {
        format!("{before}\n{garbage}\n{after}")
    })
}

// ============================================================================
// Property tests
// ============================================================================

fn proptest_config() -> ProptestConfig {
    let default = ProptestConfig::default();
    ProptestConfig {
        cases: default.cases.max(512),
        ..default
    }
}

proptest! {
    #![proptest_config(proptest_config())]

    /// Property 1: Non-whitespace input always produces AST nodes or diagnostics.
    ///
    /// The parser must never silently produce an empty module with no diagnostics
    /// for non-trivial input. Either some AST was recovered, or errors were reported.
    #[test]
    fn parse_always_produces_result(input in "\\PC{1,300}") {
        // Skip whitespace-only and comment-only input — these legitimately
        // produce empty modules with no diagnostics.
        let trimmed = input.trim();
        if trimmed.is_empty() || trimmed.starts_with("//") {
            return Ok(());
        }

        let tokens = lex_with_eof(&input);
        let (module, diags) = parse(tokens);

        let has_ast = !module.classes.is_empty()
            || !module.expressions.is_empty()
            || !module.method_definitions.is_empty()
            || !module.protocols.is_empty();

        // Either we got some AST, or we got diagnostics (or both).
        // It's fine for very short garbage to produce only error tokens that
        // the parser skips — but there should at least be diagnostics.
        prop_assert!(
            has_ast || !diags.is_empty(),
            "Parser produced empty module with no diagnostics for non-whitespace input {:?}",
            input,
        );
    }

    /// Property 2: Injecting garbage into valid code produces error diagnostics.
    ///
    /// The parser must detect the injected error and report it.
    #[test]
    fn injecting_error_into_valid_code_produces_diagnostics(input in inject_error()) {
        let tokens = lex_with_eof(&input);
        let (_, diags) = parse(tokens);

        let error_count = diags.iter()
            .filter(|d| d.severity == Severity::Error)
            .count();

        prop_assert!(
            error_count > 0,
            "Parser produced no error diagnostics for garbage-injected input {:?}",
            input,
        );
    }

    /// Property 3: All Error-severity diagnostics have non-empty messages.
    #[test]
    fn error_diagnostics_have_nonempty_messages(input in "\\PC{0,300}") {
        let tokens = lex_with_eof(&input);
        let (_, diags) = parse(tokens);

        for diag in &diags {
            if diag.severity == Severity::Error {
                prop_assert!(
                    !diag.message.is_empty(),
                    "Error diagnostic with empty message for input {:?}",
                    input,
                );
            }
        }
    }

    /// Property 4: Error count is bounded for short inputs.
    ///
    /// For inputs under 300 characters, the number of Error diagnostics should
    /// not explode (< 100). This prevents pathological diagnostic cascading.
    #[test]
    fn error_count_bounded(input in "\\PC{0,300}") {
        let tokens = lex_with_eof(&input);
        let (_, diags) = parse(tokens);

        let error_count = diags.iter()
            .filter(|d| d.severity == Severity::Error)
            .count();

        prop_assert!(
            error_count < 100,
            "Parser produced {} error diagnostics for {} byte input {:?} — possible cascade",
            error_count,
            input.len(),
            input,
        );
    }
}
