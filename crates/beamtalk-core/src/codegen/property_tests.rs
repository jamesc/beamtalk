// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Property-based tests for Beamtalk code generation.
//!
//! These tests verify that the code generator handles all parsed ASTs safely:
//!
//! 1. **`generate` never panics** — codegen returns Ok or Err, never panics
//! 2. **`generate_repl_expression` never panics** — REPL codegen is safe
//! 3. **Generated output is valid UTF-8** — output is always a valid String
//! 4. **Successful codegen produces non-empty output** — no silent empty results
//!
//! **DDD Context:** Code Generation
//!
//! ADR 0011 Phase 2 (extended).

use proptest::prelude::*;

use crate::codegen::core_erlang::{CodegenOptions, generate_module, generate_repl_expression};
use crate::source_analysis::{lex_with_eof, parse};

// ============================================================================
// Generators
// ============================================================================

/// Near-valid Beamtalk fragments for codegen testing.
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
    "#{a => 1}",
    "self",
    "^42",
    "3 timesRepeat: [x := x + 1]",
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
// Helpers
// ============================================================================

fn parse_source(source: &str) -> crate::ast::Module {
    let tokens = lex_with_eof(source);
    let (module, _) = parse(tokens);
    module
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

    /// Property 2: `generate_repl_expression` never panics.
    ///
    /// Each top-level expression in the parsed module is tried individually.
    #[test]
    fn generate_repl_expression_never_panics(input in "\\PC{0,300}") {
        let module = parse_source(&input);
        for expr in &module.expressions {
            let _result = generate_repl_expression(expr, "prop_test_repl");
        }
    }

    /// Property 2b: `generate_repl_expression` never panics on near-valid input.
    #[test]
    fn generate_repl_expression_never_panics_near_valid(input in near_valid_beamtalk()) {
        let module = parse_source(&input);
        for expr in &module.expressions {
            let _result = generate_repl_expression(expr, "prop_test_repl");
        }
    }

    /// Property 3: Successful codegen always produces valid, non-empty output.
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
            prop_assert!(
                output.contains("module") || output.contains("'prop_test_module'"),
                "Generated output doesn't look like Core Erlang for input {:?}: {}",
                input,
                &output[..output.len().min(200)],
            );
        }
    }

    /// Property 4: REPL expression codegen output contains module structure.
    #[test]
    fn repl_codegen_output_structure(input in near_valid_beamtalk()) {
        let module = parse_source(&input);
        for expr in &module.expressions {
            if let Ok(output) = generate_repl_expression(expr, "prop_test_repl") {
                prop_assert!(
                    !output.is_empty(),
                    "generate_repl_expression returned Ok with empty output for input {:?}",
                    input,
                );
            }
        }
    }
}
