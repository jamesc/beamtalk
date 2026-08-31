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
//!
//! The near-valid-input generator and proptest config (below, via
//! `beamtalk_core::test_helpers::test_support`) are shared with
//! `beamtalk-core`'s sibling file rather than duplicated locally — this
//! crate's `[dev-dependencies]` opts into `beamtalk-core`'s `test` feature
//! for exactly that (code review on BT-3344's PR; same mechanism BT-3100
//! established for `test_support::arb_declared_type`).

use beamtalk_core::ast::Module;
use beamtalk_core::source_analysis::{lex_with_eof, parse};
use beamtalk_core::test_helpers::test_support::{near_valid_beamtalk, proptest_config_default};
use beamtalk_repl::codegen::generate_repl_expression;
use proptest::prelude::*;

// ============================================================================
// Helpers
// ============================================================================

fn parse_source(source: &str) -> Module {
    let tokens = lex_with_eof(source);
    let (module, _) = parse(tokens);
    module
}

// ============================================================================
// Property tests
// ============================================================================

proptest! {
    #![proptest_config(proptest_config_default())]

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
