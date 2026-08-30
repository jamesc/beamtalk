// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Property-based tests for REPL code generation.
//!
//! These tests verify that `beamtalk-repl`'s codegen entry points handle
//! all parsed ASTs safely:
//!
//! 1. **`generate_repl_expression` never panics** — REPL codegen is safe
//!    on arbitrary and near-valid input.
//! 2. **Successful REPL codegen produces non-empty output** — no silent
//!    empty results.
//!
//! **DDD Context:** REPL
//!
//! BT-3344 (ADR 0117 Decision step 4): moved here from `beamtalk-core`'s
//! `tests/codegen_property_tests.rs`, where these REPL-specific properties
//! were the last remaining edge from `codegen`'s test tree into `repl`
//! (test-only; see BT-3340, ADR 0117 Decision step 2, for the production-
//! code split). The `generate_module`-only properties stayed behind in
//! `beamtalk-core`, since they don't touch `beamtalk-repl` at all.
//!
//! Kept as a Cargo integration test (not a unit test embedded in
//! `beamtalk-repl::src`), matching `beamtalk-repl/tests/repl_codegen_smoke.rs`
//! — see that file's doc comment for why.

use beamtalk_core::ast::Module;
use beamtalk_core::source_analysis::{lex_with_eof, parse};
use beamtalk_repl::codegen::generate_repl_expression;
use proptest::prelude::*;

// ============================================================================
// Generators
// ============================================================================

/// Near-valid Beamtalk fragments for REPL codegen testing.
///
/// Deliberately duplicated from `beamtalk-core/tests/codegen_property_tests.rs`
/// rather than shared: the two files are integration tests of different
/// crates (`beamtalk-repl` vs `beamtalk-core`), so sharing this small,
/// test-only fixture generator would require a new shared test-support
/// crate — disproportionate for a handful of literal fragments.
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
                        // MSRV-1.85-compatible stand-in for `str::floor_char_boundary`
                        // (stable since 1.91, past this crate's pinned MSRV).
                        let mut safe_cut = cut;
                        while safe_cut > 0 && !s.is_char_boundary(safe_cut) {
                            safe_cut -= 1;
                        }
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
// Helpers
// ============================================================================

fn parse_source(source: &str) -> Module {
    let tokens = lex_with_eof(source);
    let (module, _) = parse(tokens);
    module
}

/// Standard proptest configuration for this suite: at least 512 cases
/// (overridable via `PROPTEST_CASES`), matching
/// `beamtalk_core::test_helpers::test_support::proptest_config_default` —
/// that helper is `#[cfg(test)]`-gated (deliberately unavailable to
/// dependent crates, including this integration test binary), so it's
/// reproduced here rather than imported.
fn proptest_config() -> ProptestConfig {
    let default = ProptestConfig::default();
    ProptestConfig {
        cases: default.cases.max(512),
        ..default
    }
}

// ============================================================================
// Property tests
// ============================================================================

proptest! {
    #![proptest_config(proptest_config())]

    /// Property 1: `generate_repl_expression` never panics.
    ///
    /// Each top-level expression in the parsed module is tried individually.
    #[test]
    fn generate_repl_expression_never_panics(input in "\\PC{0,300}") {
        let module = parse_source(&input);
        for expr in &module.expressions {
            let _result = generate_repl_expression(&expr.expression, "prop_test_repl");
        }
    }

    /// Property 1b: `generate_repl_expression` never panics on near-valid input.
    #[test]
    fn generate_repl_expression_never_panics_near_valid(input in near_valid_beamtalk()) {
        let module = parse_source(&input);
        for expr in &module.expressions {
            let _result = generate_repl_expression(&expr.expression, "prop_test_repl");
        }
    }

    /// Property 2: REPL expression codegen output contains module structure.
    #[test]
    fn repl_codegen_output_structure(input in near_valid_beamtalk()) {
        let module = parse_source(&input);
        for expr in &module.expressions {
            if let Ok(output) = generate_repl_expression(&expr.expression, "prop_test_repl") {
                prop_assert!(
                    !output.is_empty(),
                    "generate_repl_expression returned Ok with empty output for input {:?}",
                    input,
                );
            }
        }
    }
}
