// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Property-based tests for Beamtalk semantic analysis.
//!
//! These tests verify that the semantic analysis pipeline never panics on
//! arbitrary parsed AST, and that diagnostics have valid spans:
//!
//! 1. **`analyse` never panics** — any parsed module can be analysed
//! 2. **`analyse_with_known_vars` never panics** — REPL context analysis is safe
//! 3. **Semantic diagnostic spans within input** — all span bounds are valid
//! 4. **Class hierarchy never panics** — building hierarchy from any AST is safe
//! 5. **Type inference never panics** — type checking any parsed AST is safe
//!
//! **DDD Context:** Semantic Analysis
//!
//! ADR 0011 Phase 2 (extended).

use proptest::prelude::*;

use crate::source_analysis::{lex_with_eof, parse};

use super::{ClassHierarchy, analyse, analyse_with_known_vars, infer_types};

// ============================================================================
// Generators
// ============================================================================

/// Near-valid Beamtalk fragments for semantic analysis testing.
///
/// These are structurally richer than pure random strings to exercise
/// more semantic analysis paths (class definitions, message sends, etc.).
const FRAGMENTS: &[&str] = &[
    "42",
    "x := 42",
    "x + y",
    "[:x | x + 1]",
    "true ifTrue: [1] ifFalse: [0]",
    "Object subclass: Foo\n  state: x = 0\n  bar => x",
    "Object subclass: Bar\n  greet => \"hello\"",
    "self foo",
    "super bar",
    "^42",
    "#(1, 2, 3)",
    "#{a => 1, b => 2}",
    "x match: { 1 => \"one\", _ => \"other\" }",
    "Actor subclass: Counter\n  state: count = 0\n  increment => count := count + 1",
    "3 timesRepeat: [x := x + 1]",
    "x > 0 ifTrue: ['positive'] ifFalse: ['non-positive']",
];

fn valid_fragment() -> impl Strategy<Value = String> {
    prop::sample::select(FRAGMENTS).prop_map(std::string::ToString::to_string)
}

/// Generates near-valid Beamtalk with mutations.
fn near_valid_beamtalk() -> impl Strategy<Value = String> {
    prop_oneof![
        // Valid fragments
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
        // Multiple fragments concatenated
        (valid_fragment(), valid_fragment()).prop_map(|(a, b)| format!("{a}\n{b}")),
    ]
}

/// Known variable names for REPL context testing.
fn known_var_names() -> impl Strategy<Value = Vec<String>> {
    prop::collection::vec("[a-z][a-zA-Z0-9]{0,8}", 0..5)
}

// ============================================================================
// Helpers
// ============================================================================

/// Parse source into (Module, parse diagnostics).
fn parse_source(source: &str) -> (crate::ast::Module, Vec<crate::source_analysis::Diagnostic>) {
    let tokens = lex_with_eof(source);
    parse(tokens)
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

    /// Property 1: `analyse` never panics on arbitrary string input.
    ///
    /// The full semantic analysis pipeline (name resolution, type checking,
    /// block analysis, validators) must handle any parsed AST without panicking.
    #[test]
    fn analyse_never_panics(input in "\\PC{0,500}") {
        let (module, _) = parse_source(&input);
        let _result = analyse(&module);
    }

    /// Property 1b: `analyse` never panics on near-valid structured input.
    #[test]
    fn analyse_never_panics_near_valid(input in near_valid_beamtalk()) {
        let (module, _) = parse_source(&input);
        let _result = analyse(&module);
    }

    /// Property 2: `analyse_with_known_vars` never panics.
    ///
    /// REPL-style analysis with pre-defined variables must be safe.
    #[test]
    fn analyse_with_vars_never_panics(
        input in near_valid_beamtalk(),
        vars in known_var_names(),
    ) {
        let (module, _) = parse_source(&input);
        let var_refs: Vec<&str> = vars.iter().map(String::as_str).collect();
        let _result = analyse_with_known_vars(&module, &var_refs);
    }

    /// Property 3: Semantic diagnostic spans are within input bounds.
    #[test]
    fn semantic_diagnostic_spans_within_input(input in "\\PC{0,500}") {
        let (module, _) = parse_source(&input);
        let result = analyse(&module);
        let input_len = u32::try_from(input.len()).unwrap_or(u32::MAX);
        for diag in &result.diagnostics {
            prop_assert!(
                diag.span.end() <= input_len,
                "Semantic diagnostic span end {} exceeds input length {} for input {:?}: {}",
                diag.span.end(),
                input_len,
                input,
                diag.message,
            );
            prop_assert!(
                diag.span.start() <= diag.span.end(),
                "Semantic diagnostic span start {} > end {} for input {:?}: {}",
                diag.span.start(),
                diag.span.end(),
                input,
                diag.message,
            );
        }
    }

    /// Property 4: Class hierarchy building never panics.
    #[test]
    fn class_hierarchy_never_panics(input in "\\PC{0,500}") {
        let (module, _) = parse_source(&input);
        let (_hierarchy, _diags) = ClassHierarchy::build(&module);
    }

    /// Property 4b: Class hierarchy building never panics on near-valid input.
    #[test]
    fn class_hierarchy_never_panics_near_valid(input in near_valid_beamtalk()) {
        let (module, _) = parse_source(&input);
        let (_hierarchy, _diags) = ClassHierarchy::build(&module);
    }

    /// Property 5: Type inference never panics.
    #[test]
    fn type_inference_never_panics(input in "\\PC{0,500}") {
        let (module, _) = parse_source(&input);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let _type_map = infer_types(&module, &hierarchy);
    }

    /// Property 5b: Type inference never panics on near-valid input.
    #[test]
    fn type_inference_never_panics_near_valid(input in near_valid_beamtalk()) {
        let (module, _) = parse_source(&input);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let _type_map = infer_types(&module, &hierarchy);
    }
}
