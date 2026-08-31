// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Property-based tests for Core Erlang output validity.
//!
//! These tests go beyond "never panics" to verify structural properties of
//! successfully generated Core Erlang:
//!
//! 1. **Parseable structure** — output starts with `module`, ends with `end`,
//!    and has balanced parentheses/brackets
//! 2. **Module name matches** — the module name in output matches `CodegenOptions`
//! 3. **No format artifacts** — output contains no `{:?}`, `Document::`, or other
//!    Rust debug/display leaks (guards against BT-875 class of bugs)
//!
//! **DDD Context:** Code Generation
//!
//! ADR 0011 Phase 2 (extended).

use proptest::prelude::*;

use crate::core_erlang::{CodegenOptions, generate_module};
use beamtalk_core::source_analysis::{lex_with_eof, parse};

// ============================================================================
// Generators
// ============================================================================

/// Near-valid Beamtalk fragments for codegen validity testing.
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

fn parse_source(source: &str) -> beamtalk_core::ast::Module {
    let tokens = lex_with_eof(source);
    let (module, _) = parse(tokens);
    module
}

// `has_balanced_delimiters`/`FORMAT_ARTIFACT_PATTERNS` moved to
// `beamtalk_core::test_helpers::test_support` (BT-3124) so the `compile_pipeline`
// fuzz target can share the same structural-validity checks instead of
// duplicating them.
use beamtalk_core::test_helpers::test_support::{
    CORE_ERLANG_FORMAT_ARTIFACT_PATTERNS as FORMAT_ARTIFACT_PATTERNS, arb_program,
    core_erlang_has_balanced_delimiters as has_balanced_delimiters, core_erlang_structural_issues,
};
use beamtalk_core::unparse::unparse_module;

// ============================================================================
// Property tests
// ============================================================================

use beamtalk_core::test_helpers::test_support::proptest_config_default as proptest_config;

proptest! {
    #![proptest_config(proptest_config())]

    /// Property 1: Successful codegen produces structurally valid Core Erlang.
    ///
    /// When `generate_module` returns `Ok`, the output must start with `module`,
    /// end with `end`, and have balanced delimiters.
    #[test]
    fn successful_codegen_produces_parseable_core_erlang(input in near_valid_beamtalk()) {
        let module = parse_source(&input);
        let options = CodegenOptions::new("prop_validity_test");
        if let Ok(output) = generate_module(&module, options) {
            let trimmed = output.trim();

            prop_assert!(
                trimmed.starts_with("module"),
                "Core Erlang output does not start with 'module' for input {:?}:\n{}",
                input,
                &trimmed[..trimmed.floor_char_boundary(200)],
            );

            prop_assert!(
                trimmed.ends_with("end"),
                "Core Erlang output does not end with 'end' for input {:?}:\n...{}",
                input,
                &trimmed[trimmed.ceil_char_boundary(trimmed.len().saturating_sub(200))..],
            );

            prop_assert!(
                has_balanced_delimiters(&output),
                "Core Erlang output has unbalanced delimiters for input {:?}",
                input,
            );
        }
    }

    /// Property 2: Module name in output matches `CodegenOptions`.
    #[test]
    fn successful_codegen_module_name_matches(input in near_valid_beamtalk()) {
        let module = parse_source(&input);
        let options = CodegenOptions::new("prop_name_test");
        if let Ok(output) = generate_module(&module, options) {
            prop_assert!(
                output.contains("'prop_name_test'"),
                "Core Erlang output missing module name 'prop_name_test' for input {:?}:\n{}",
                input,
                &output[..output.floor_char_boundary(300)],
            );
        }
    }

    /// Property 3: Successful codegen contains no Rust format/debug artifacts.
    ///
    /// Guards against the BT-875 class of bugs where `format!()` or `Debug`
    /// implementations leak Rust type names into Core Erlang output.
    #[test]
    fn successful_codegen_no_format_artifacts(input in near_valid_beamtalk()) {
        let module = parse_source(&input);
        let options = CodegenOptions::new("prop_artifact_test");
        if let Ok(output) = generate_module(&module, options) {
            for pattern in FORMAT_ARTIFACT_PATTERNS {
                prop_assert!(
                    !output.contains(pattern),
                    "Core Erlang output contains format artifact {:?} for input {:?}:\n{}",
                    pattern,
                    input,
                    &output[..output.floor_char_boundary(500)],
                );
            }
        }
    }
}

// ============================================================================
// Grammar-driven program generator properties (BT-3116)
//
// `near_valid_beamtalk()` above builds inputs from a small hand-curated
// FRAGMENTS array plus truncation/concatenation -- useful for "never
// panics" robustness, but shallow: it can't reach nested blocks with
// captures, `^` inside nested closures, or multi-statement bodies
// threading local state. `arb_program` (test_helpers::test_support)
// generates well-formed programs as typed AST values instead, so
// shrinking works structurally on the tree rather than by truncating
// strings. These properties are *additional* coverage -- the FRAGMENTS-
// based properties above stay, since they intentionally also cover
// ill-formed/truncated input this generator never produces.
// ============================================================================

proptest! {
    #![proptest_config(proptest_config())]

    /// Round-trip: a generated program renders via `unparse` to source text
    /// that parses back with zero diagnostics (BT-3116 acceptance
    /// criterion). Mirrors `unparse::property_tests::
    /// unparse_roundtrip_preserves_structure`'s guarantee, specialised to
    /// this generator's shape space.
    #[test]
    fn program_gen_round_trip(module in arb_program("GenRoundTrip")) {
        let source = unparse_module(&module);
        let tokens = lex_with_eof(&source);
        let (_reparsed, diagnostics) = parse(tokens);
        let errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.severity == beamtalk_core::source_analysis::Severity::Error)
            .collect();
        prop_assert!(
            errors.is_empty(),
            "generated program did not round-trip cleanly: {:?}\n\nSource:\n{}",
            errors,
            source,
        );
    }

    /// Codegen validity: whenever `generate_module` accepts a generated
    /// program, its output passes the same structural-validity checks
    /// (balanced delimiters, `module`/`end` framing, no Rust format-artifact
    /// leaks) as `successful_codegen_produces_parseable_core_erlang` above
    /// (BT-3116 acceptance criterion).
    #[test]
    fn program_gen_codegen_validity(module in arb_program("GenCodegenValidity")) {
        let options = CodegenOptions::new("prop_program_gen_test");
        if let Ok(output) = generate_module(&module, options) {
            let issues = core_erlang_structural_issues(&output);
            prop_assert!(
                issues.is_empty(),
                "generated program produced structurally invalid Core Erlang:\n{}\n\nSource:\n{}",
                issues.join("\n"),
                unparse_module(&module),
            );
        }
    }
}
