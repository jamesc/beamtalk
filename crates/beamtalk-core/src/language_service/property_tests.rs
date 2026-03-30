// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Property-based tests for the Beamtalk language service.
//!
//! These tests verify that all language service operations handle arbitrary
//! source text and positions without panicking:
//!
//! 1. **`update_file` never panics** — any source text is accepted
//! 2. **`diagnostics` never panics** — diagnostics are always produced safely
//! 3. **`completions` never panics** — arbitrary positions handled gracefully
//! 4. **`hover` never panics** — arbitrary positions handled gracefully
//! 5. **`goto_definition` never panics** — arbitrary positions handled gracefully
//!
//! **DDD Context:** Language Service
//!
//! ADR 0011 Phase 2 (extended).

use proptest::prelude::*;

use super::{LanguageService, Position, SimpleLanguageService};
use camino::Utf8PathBuf;

// ============================================================================
// Generators
// ============================================================================

/// Near-valid Beamtalk fragments for language service testing.
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
    "self foo: 1 bar: 2",
    "x > 0 ifTrue: ['positive'] ifFalse: ['non-positive']",
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

/// Generate a Position from a source string at a random byte offset.
///
/// Converts a byte offset to line/character coordinates.
fn position_from_source(source: &str, byte_offset: usize) -> Position {
    let clamped = byte_offset.min(source.len());
    let prefix = &source[..clamped];
    let line = u32::try_from(prefix.matches('\n').count()).unwrap_or(u32::MAX);
    let last_newline = prefix.rfind('\n').map_or(0, |pos| pos + 1);
    let character = u32::try_from(clamped - last_newline).unwrap_or(u32::MAX);
    Position {
        line,
        column: character,
    }
}

// ============================================================================
// Helpers
// ============================================================================

fn file_id() -> Utf8PathBuf {
    Utf8PathBuf::from("proptest.bt")
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

    /// Property 1: `update_file` + `diagnostics` never panics on arbitrary input.
    #[test]
    fn update_file_and_diagnostics_never_panics(input in "\\PC{0,300}") {
        let mut service = SimpleLanguageService::new();
        service.update_file(file_id(), input);
        let _diags = service.diagnostics(&file_id());
    }

    /// Property 1b: `update_file` + `diagnostics` never panics on near-valid input.
    #[test]
    fn update_file_and_diagnostics_never_panics_near_valid(input in near_valid_beamtalk()) {
        let mut service = SimpleLanguageService::new();
        service.update_file(file_id(), input);
        let _diags = service.diagnostics(&file_id());
    }

    /// Property 2: `completions` never panics at arbitrary positions.
    #[test]
    fn completions_never_panics(
        input in near_valid_beamtalk(),
        offset in 0..500usize,
    ) {
        let mut service = SimpleLanguageService::new();
        service.update_file(file_id(), input.clone());
        let pos = position_from_source(&input, offset);
        let _completions = service.completions(&file_id(), pos);
    }

    /// Property 3: `hover` never panics at arbitrary positions.
    #[test]
    fn hover_never_panics(
        input in near_valid_beamtalk(),
        offset in 0..500usize,
    ) {
        let mut service = SimpleLanguageService::new();
        service.update_file(file_id(), input.clone());
        let pos = position_from_source(&input, offset);
        let _hover = service.hover(&file_id(), pos);
    }

    /// Property 4: `goto_definition` never panics at arbitrary positions.
    #[test]
    fn goto_definition_never_panics(
        input in near_valid_beamtalk(),
        offset in 0..500usize,
    ) {
        let mut service = SimpleLanguageService::new();
        service.update_file(file_id(), input.clone());
        let pos = position_from_source(&input, offset);
        let _def = service.goto_definition(&file_id(), pos);
    }
}
