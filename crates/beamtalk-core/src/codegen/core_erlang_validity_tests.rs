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

use crate::codegen::core_erlang::{CodegenOptions, generate_module};
use crate::source_analysis::{lex_with_eof, parse};

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

fn parse_source(source: &str) -> crate::ast::Module {
    let tokens = lex_with_eof(source);
    let (module, _) = parse(tokens);
    module
}

/// Check that parentheses, brackets, and braces are balanced in Core Erlang output.
fn has_balanced_delimiters(s: &str) -> bool {
    let mut stack = Vec::new();
    // Skip string literals (single-quoted atoms and double-quoted strings)
    let mut chars = s.chars().peekable();
    let mut in_single_quote = false;
    let mut in_double_quote = false;

    while let Some(ch) = chars.next() {
        match ch {
            '\'' if !in_double_quote => in_single_quote = !in_single_quote,
            '"' if !in_single_quote => in_double_quote = !in_double_quote,
            '\\' if in_single_quote || in_double_quote => {
                // Skip escaped character
                chars.next();
            }
            _ if in_single_quote || in_double_quote => {}
            '(' => stack.push(')'),
            '[' => stack.push(']'),
            '{' => stack.push('}'),
            ')' | ']' | '}' => {
                if stack.pop() != Some(ch) {
                    return false;
                }
            }
            _ => {}
        }
    }
    stack.is_empty() && !in_single_quote && !in_double_quote
}

/// Patterns that should never appear in valid Core Erlang output.
/// These indicate Rust Debug/Display format leaks (BT-875).
const FORMAT_ARTIFACT_PATTERNS: &[&str] = &[
    "{:?}",
    "Document::",
    "BinaryOp(",
    "Expression::",
    "Literal::",
    "MessageSelector::",
    "Pattern::",
    "TokenKind::",
];

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
