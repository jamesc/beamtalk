// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Property-based tests for Beamtalk code generation.
//!
//! These tests verify that the code generator handles all parsed ASTs safely:
//!
//! 1. **`generate_module` never panics** — codegen returns Ok or Err, never panics
//! 2. **Generated output is valid UTF-8** — output is always a valid String
//! 3. **Successful codegen produces non-empty output** — no silent empty results
//!
//! **DDD Context:** Code Generation
//!
//! ADR 0011 Phase 2 (extended).
//!
//! BT-3344 (ADR 0117 Decision step 4): the REPL-specific properties that
//! used to live here (`generate_repl_expression` never panics / produces
//! non-empty output) moved to
//! `beamtalk-repl/tests/codegen_property_tests.rs` — they exercised only
//! `beamtalk-repl::codegen`'s public API, the last remaining edge from
//! `codegen`'s test tree into `repl` (test-only; see BT-3340, ADR 0117
//! Decision step 2, for the production-code split). What's left here needs
//! no such cross-crate care: it exercises only `beamtalk-core`'s own
//! `generate_module`, so this could be a plain unit test, but it stays a
//! Cargo integration test for consistency with its sibling file above.
//!
//! The near-valid-input generator and proptest config (below, via
//! `test_helpers::test_support`) are shared with that sibling file rather
//! than duplicated — see `test_support::near_valid_beamtalk`'s own doc
//! comment for why (code review on BT-3344's PR).

use beamtalk_codegen::core_erlang::{CodegenOptions, generate_module};
use beamtalk_core::ast::Module;
use beamtalk_core::source_analysis::{lex_with_eof, parse};
use beamtalk_core::test_helpers::test_support::{near_valid_beamtalk, proptest_config_default};
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

    /// Property 1: `generate_module` never panics on arbitrary parsed input.
    ///
    /// The code generator may return Ok or Err, but it must never panic.
    #[test]
    fn generate_module_never_panics(input in "\\PC{0,300}") {
        let module = parse_source(&input);
        let options = CodegenOptions::new("prop_test_module");
        let _result = generate_module(&module, options);
    }

    /// Property 1b: `generate_module` never panics on near-valid input.
    #[test]
    fn generate_module_never_panics_near_valid(input in near_valid_beamtalk()) {
        let module = parse_source(&input);
        let options = CodegenOptions::new("prop_test_module");
        let _result = generate_module(&module, options);
    }

    /// Property 2: Successful codegen always produces valid, non-empty output.
    #[test]
    fn successful_codegen_produces_output(input in near_valid_beamtalk()) {
        let module = parse_source(&input);
        let options = CodegenOptions::new("prop_test_module");
        if let Ok(output) = generate_module(&module, options) {
            prop_assert!(
                !output.is_empty(),
                "generate_module returned Ok with empty output for input {:?}",
                input,
            );
            // Output is already a String, so it's valid UTF-8 by construction.
            // But verify it contains the expected module header.
            let snippet_end = output.floor_char_boundary(200);
            prop_assert!(
                output.contains("module") || output.contains("'prop_test_module'"),
                "Generated output doesn't look like Core Erlang for input {:?}: {}",
                input,
                &output[..snippet_end],
            );
        }
    }
}
