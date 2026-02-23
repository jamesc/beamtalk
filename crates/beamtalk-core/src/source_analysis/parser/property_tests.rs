// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Property-based tests for the Beamtalk parser.
//!
//! These tests use `proptest` to verify parser invariants over generated inputs:
//!
//! 1. **Parser never panics** — arbitrary string input always returns a result
//! 2. **Diagnostic spans within input** — all spans have `end <= input.len()`
//! 3. **Error nodes produce diagnostics** — `Expression::Error` ⟹ non-empty diagnostics
//! 4. **Error messages are user-facing** — no internal type names in diagnostics
//!
//! **DDD Context:** Compilation
//!
//! ADR 0011 Phase 2.

use proptest::prelude::*;

use crate::ast::{Expression, Module};
use crate::source_analysis::{lex_with_eof, parse};

// ============================================================================
// Near-valid Beamtalk generators
// ============================================================================

/// Beamtalk expression fragments for composing near-valid inputs.
///
/// Most are valid Beamtalk; a few are intentionally invalid (e.g., single-quoted
/// strings) to exercise error recovery paths when mutated by generators.
const FRAGMENTS: &[&str] = &[
    "42",
    "3.14",
    "\"hello\"",
    "\"world\"",
    "true",
    "false",
    "nil",
    "x",
    "x := 42",
    "x + y",
    "arr at: 1",
    "obj at: 1 put: 2",
    "[:x | x + 1]",
    "(3 + 4)",
    "^42",
    "self",
    "super foo",
    "#(1, 2, 3)",
    "#{a => 1}",
    "#symbol",
    "$A",
    "x foo bar",
    "Object subclass: Counter\n  state: count = 0\n  increment => count := count + 1",
    "3 timesRepeat: [x := x + 1]",
    "x > 0 ifTrue: ['positive'] ifFalse: ['non-positive']",
];

/// Generates a Beamtalk fragment from the seed corpus.
fn valid_fragment() -> impl Strategy<Value = String> {
    prop::sample::select(FRAGMENTS).prop_map(std::string::ToString::to_string)
}

/// Generates a truncated valid expression (cut at a random point).
fn truncated_expression() -> impl Strategy<Value = String> {
    valid_fragment().prop_flat_map(|s| {
        let len = s.len();
        if len <= 1 {
            Just(s).boxed()
        } else {
            (1..len)
                .prop_map(move |cut| {
                    // Find nearest char boundary to avoid panicking on multi-byte chars
                    let safe_cut = s.floor_char_boundary(cut);
                    if safe_cut == 0 {
                        s.clone()
                    } else {
                        s[..safe_cut].to_string()
                    }
                })
                .boxed()
        }
    })
}

/// Generates input with mismatched brackets via single-pass char mapping.
fn mismatched_brackets() -> impl Strategy<Value = String> {
    valid_fragment().prop_map(|s| {
        let mut result = String::with_capacity(s.len());
        for ch in s.chars() {
            let mapped = match ch {
                '[' => '(',
                ']' => '}',
                '(' => '[',
                _ => ch,
            };
            result.push(mapped);
        }
        result
    })
}

/// Generates input with missing keyword colons.
fn missing_keyword_colons() -> impl Strategy<Value = String> {
    valid_fragment().prop_map(|s| {
        // Remove colons that follow identifiers (keyword selectors)
        let mut result = String::new();
        let chars: Vec<char> = s.chars().collect();
        let mut i = 0;
        while i < chars.len() {
            if chars[i] == ':'
                && i > 0
                && chars[i - 1].is_alphanumeric()
                && (i + 1 >= chars.len() || chars[i + 1] != '=')
            {
                // Skip the colon (removes keyword selector colon)
            } else {
                result.push(chars[i]);
            }
            i += 1;
        }
        result
    })
}

/// Generates input with duplicated operators.
fn duplicated_operators() -> impl Strategy<Value = String> {
    valid_fragment().prop_map(|s| s.replace('+', "+ +").replace('*', "* *"))
}

/// Generates a near-valid Beamtalk input using one of several mutation strategies.
fn near_valid_beamtalk() -> impl Strategy<Value = String> {
    prop_oneof![
        valid_fragment(),
        truncated_expression(),
        mismatched_brackets(),
        missing_keyword_colons(),
        duplicated_operators(),
    ]
}

// ============================================================================
// AST helpers
// ============================================================================

/// Recursively checks if an AST contains any `Expression::Error` nodes.
fn has_error_node(expr: &Expression) -> bool {
    match expr {
        Expression::Error { .. } => true,
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => has_error_node(receiver) || arguments.iter().any(has_error_node),
        Expression::FieldAccess { receiver, .. } => has_error_node(receiver),
        Expression::Assignment { target, value, .. } => {
            has_error_node(target) || has_error_node(value)
        }
        Expression::Return { value, .. } => has_error_node(value),
        Expression::Parenthesized { expression, .. } => has_error_node(expression),
        Expression::Block(block) => block.body.iter().any(has_error_node),
        Expression::Cascade {
            receiver, messages, ..
        } => {
            has_error_node(receiver)
                || messages
                    .iter()
                    .any(|m| m.arguments.iter().any(has_error_node))
        }
        Expression::Match { value, arms, .. } => {
            has_error_node(value)
                || arms.iter().any(|arm| {
                    has_error_node(&arm.body) || arm.guard.as_ref().is_some_and(has_error_node)
                })
        }
        Expression::MapLiteral { pairs, .. } => pairs
            .iter()
            .any(|p| has_error_node(&p.key) || has_error_node(&p.value)),
        Expression::ListLiteral { elements, tail, .. } => {
            elements.iter().any(has_error_node) || tail.as_ref().is_some_and(|t| has_error_node(t))
        }
        Expression::StringInterpolation { segments, .. } => segments.iter().any(|seg| {
            if let crate::ast::StringSegment::Interpolation(expr) = seg {
                has_error_node(expr)
            } else {
                false
            }
        }),
        // Leaf nodes
        Expression::Literal(..)
        | Expression::Identifier(..)
        | Expression::ClassReference { .. }
        | Expression::Super(..)
        | Expression::Primitive { .. }
        | Expression::ExpectDirective { .. } => false,
    }
}

/// Checks if a module's AST contains any `Expression::Error` nodes.
fn module_has_error_nodes(module: &Module) -> bool {
    module.expressions.iter().any(has_error_node)
        || module.classes.iter().any(|cls| {
            cls.methods
                .iter()
                .any(|m| m.body.iter().any(has_error_node))
                || cls
                    .class_methods
                    .iter()
                    .any(|m| m.body.iter().any(has_error_node))
                || cls
                    .state
                    .iter()
                    .any(|s| s.default_value.as_ref().is_some_and(has_error_node))
                || cls
                    .class_variables
                    .iter()
                    .any(|s| s.default_value.as_ref().is_some_and(has_error_node))
        })
        || module
            .method_definitions
            .iter()
            .any(|m| m.method.body.iter().any(has_error_node))
}

/// Internal type names that should never appear in user-facing diagnostics.
const INTERNAL_NAMES: &[&str] = &[
    "TokenKind",
    "unwrap()",
    "panic!",
    "unreachable!",
    "Expression::",
    "Literal::",
    "ParseError::",
    "internal error",
];

// ============================================================================
// Property tests
// ============================================================================

/// Default is 512 cases for standard CI; override via `PROPTEST_CASES` env var
/// for nightly extended runs (e.g., `PROPTEST_CASES=10000`).
fn proptest_config() -> ProptestConfig {
    let default = ProptestConfig::default();
    ProptestConfig {
        // Use at least 512 cases, but allow PROPTEST_CASES to increase beyond that
        cases: default.cases.max(512),
        ..default
    }
}

proptest! {
    #![proptest_config(proptest_config())]

    /// Property 1: Parser never panics on arbitrary string input.
    ///
    /// The parser must always return a (Module, Vec<Diagnostic>) pair,
    /// even for completely invalid input.
    #[test]
    fn parser_never_panics(input in "\\PC{0,500}") {
        let tokens = lex_with_eof(&input);
        let (_module, _diagnostics) = parse(tokens);
        // If we get here without panicking, the property holds.
    }

    /// Property 2: All diagnostic spans are within the input bounds.
    ///
    /// Every diagnostic's span must satisfy `end <= input.len()` (byte-level).
    #[test]
    fn diagnostic_spans_within_input(input in "\\PC{0,500}") {
        let tokens = lex_with_eof(&input);
        let (_module, diagnostics) = parse(tokens);
        let input_len = u32::try_from(input.len()).unwrap_or(u32::MAX);
        for diag in &diagnostics {
            prop_assert!(
                diag.span.end() <= input_len,
                "Diagnostic span end {} exceeds input length {} for input {:?}: {}",
                diag.span.end(),
                input_len,
                input,
                diag.message,
            );
            prop_assert!(
                diag.span.start() <= diag.span.end(),
                "Diagnostic span start {} > end {} for input {:?}: {}",
                diag.span.start(),
                diag.span.end(),
                input,
                diag.message,
            );
        }
    }

    /// Property 3: Error AST nodes always produce diagnostics.
    ///
    /// If the AST contains any `Expression::Error` node, the diagnostics
    /// vector must be non-empty.
    #[test]
    fn error_nodes_produce_diagnostics(input in near_valid_beamtalk()) {
        let tokens = lex_with_eof(&input);
        let (module, diagnostics) = parse(tokens);
        if module_has_error_nodes(&module) {
            prop_assert!(
                !diagnostics.is_empty(),
                "AST contains Error node(s) but diagnostics is empty for input: {:?}",
                input,
            );
        }
    }

    /// Property 4: Error messages are user-facing (no internal type names).
    ///
    /// No diagnostic message should contain internal Rust type names or
    /// panic-related strings that would confuse end users.
    #[test]
    fn error_messages_are_user_facing(input in near_valid_beamtalk()) {
        let tokens = lex_with_eof(&input);
        let (_module, diagnostics) = parse(tokens);
        for diag in &diagnostics {
            for internal in INTERNAL_NAMES {
                prop_assert!(
                    !diag.message.contains(internal),
                    "Diagnostic message contains internal name {:?}: {:?} (input: {:?})",
                    internal,
                    diag.message,
                    input,
                );
            }
        }
    }

    /// Property 1b: Parser never panics on near-valid structured input.
    ///
    /// Uses near-valid generators that exercise error recovery more deeply.
    #[test]
    fn parser_never_panics_near_valid(input in near_valid_beamtalk()) {
        let tokens = lex_with_eof(&input);
        let (_module, _diagnostics) = parse(tokens);
    }
}
