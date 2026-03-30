// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Property-based tests for the Beamtalk unparser (round-trip / idempotency).
//!
//! These tests verify that the unparser faithfully round-trips parsed ASTs:
//!
//! 1. **`format_source` never panics** — arbitrary input returns `None` or `Some`
//! 2. **`format_source` is idempotent** — formatting twice yields the same output
//! 3. **Round-trip preserves structure** — parse → unparse → re-parse → unparse
//!    produces the same string on both passes
//!
//! **DDD Context:** Language Service — Formatting / Unparse
//!
//! ADR 0011 Phase 2 (extended).

use proptest::prelude::*;

use super::{format_source, unparse_module};
use crate::source_analysis::{lex_with_eof, parse};

// ============================================================================
// Generators
// ============================================================================

/// Near-valid Beamtalk fragments for round-trip testing.
const FRAGMENTS: &[&str] = &[
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
    "Object subclass: Baz\n  foo: x => x + 1",
    "sealed Object subclass: Shape",
];

fn valid_fragment() -> impl Strategy<Value = String> {
    prop::sample::select(FRAGMENTS).prop_map(std::string::ToString::to_string)
}

fn near_valid_beamtalk() -> impl Strategy<Value = String> {
    prop_oneof![
        valid_fragment(),
        // Truncated
        valid_fragment().prop_flat_map(|s| {
            let len = s.len();
            if len <= 1 {
                Just(s).boxed()
            } else {
                (1..len)
                    .prop_map(move |cut| {
                        let safe_cut = s.floor_char_boundary(cut);
                        if safe_cut == 0 {
                            s.clone()
                        } else {
                            s[..safe_cut].to_string()
                        }
                    })
                    .boxed()
            }
        }),
        // Multiple fragments
        (valid_fragment(), valid_fragment()).prop_map(|(a, b)| format!("{a}\n{b}")),
    ]
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

    /// Property 1: `format_source` never panics on arbitrary input.
    ///
    /// It may return `None` (parse errors) or `Some(formatted)`, but must not panic.
    #[test]
    fn format_source_never_panics(input in "\\PC{0,300}") {
        let _result = format_source(&input);
    }

    /// Property 1b: `format_source` never panics on near-valid input.
    #[test]
    fn format_source_never_panics_near_valid(input in near_valid_beamtalk()) {
        let _result = format_source(&input);
    }

    /// Property 2: `format_source` is idempotent.
    ///
    /// If `format_source(s)` returns `Some(f)`, then `format_source(f)` must also
    /// return `Some` with the same output.
    #[test]
    fn format_source_idempotent(input in near_valid_beamtalk()) {
        if let Some(pass1) = format_source(&input) {
            let pass2 = format_source(&pass1);
            prop_assert!(
                pass2.is_some(),
                "format_source returned None on already-formatted output for input {:?}",
                input,
            );
            prop_assert_eq!(
                &pass1,
                pass2.as_ref().unwrap(),
                "format_source is not idempotent for input {:?}",
                input,
            );
        }
    }

    /// Property 3: Round-trip preserves structure.
    ///
    /// parse → unparse → re-parse → unparse must produce the same string on
    /// both unparse passes (we compare at string level since AST spans differ).
    #[test]
    fn unparse_roundtrip_preserves_structure(input in near_valid_beamtalk()) {
        let tokens1 = lex_with_eof(&input);
        let (module1, diags1) = parse(tokens1);

        // Only test round-trip on error-free parses — errors lose information.
        let has_errors = diags1.iter().any(|d| d.severity == crate::source_analysis::Severity::Error);
        prop_assume!(!has_errors);

        let unparsed1 = unparse_module(&module1);

        let tokens2 = lex_with_eof(&unparsed1);
        let (module2, diags2) = parse(tokens2);

        // The re-parse should also be error-free.
        let has_errors2 = diags2.iter().any(|d| d.severity == crate::source_analysis::Severity::Error);
        prop_assert!(
            !has_errors2,
            "Re-parsing unparsed output produced errors for input {:?}:\nunparsed: {:?}\ndiags: {:?}",
            input,
            unparsed1,
            diags2.iter().map(|d| d.message.as_str()).collect::<Vec<_>>(),
        );

        let unparsed2 = unparse_module(&module2);
        prop_assert_eq!(
            unparsed1,
            unparsed2,
            "Round-trip unparse mismatch for input {:?}",
            input,
        );
    }
}
