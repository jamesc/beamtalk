// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0011 Phase 4 property-based tests: `handle_compile()` / `handle_compile_expression()` must never panic and must always return a well-formed ETF response for near-valid Beamtalk input.

use super::*;

/// Near-valid Beamtalk source fragments (reuses patterns from parser property tests).
const FRAGMENTS: &[&str] = &[
    "42",
    "3.14",
    "\"hello\"",
    "true",
    "false",
    "nil",
    "x := 42",
    "x + y",
    "arr at: 1",
    "[:x | x + 1]",
    "(3 + 4)",
    "^42",
    "self",
    "#(1, 2, 3)",
    "#{#a => 1}",
    "Object subclass: Counter\n  state: count = 0\n  increment => count := count + 1",
    "3 timesRepeat: [x := x + 1]",
    "#[first, ...rest] := #[1, 2, 3]",
    "[1] ensure: [nil]",
    "x match: { 1 => \"one\", _ => \"other\" }",
];

/// Generates a near-valid Beamtalk input using one of several mutation strategies.
fn near_valid_beamtalk() -> impl Strategy<Value = String> {
    prop_oneof![
        // Valid fragments
        prop::sample::select(FRAGMENTS).prop_map(std::string::ToString::to_string),
        // Truncated valid expressions
        prop::sample::select(FRAGMENTS).prop_flat_map(|s| {
            let len = s.len();
            if len <= 1 {
                Just(s.to_string()).boxed()
            } else {
                (1..len)
                    .prop_map(move |cut| {
                        let safe_cut = s.floor_char_boundary(cut);
                        if safe_cut == 0 {
                            s.to_string()
                        } else {
                            s[..safe_cut].to_string()
                        }
                    })
                    .boxed()
            }
        }),
        // Mismatched brackets
        prop::sample::select(FRAGMENTS).prop_map(|s| {
            s.chars()
                .map(|ch| match ch {
                    '[' => '(',
                    ']' => '}',
                    '(' => '[',
                    _ => ch,
                })
                .collect()
        }),
    ]
}

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

    /// Property 1a: `handle_compile` never panics on arbitrary string input.
    #[test]
    fn compile_never_panics(input in "\\PC{0,500}") {
        let request = compile_request(&input);
        let response = handle_compile(&request);
        let status = response_status(&response);
        prop_assert!(
            status.is_some(),
            "Response must have a status field for input: {:?}",
            input,
        );
        let status = status.unwrap();
        prop_assert!(
            status == "ok" || status == "error",
            "Status must be 'ok' or 'error', got {:?} for input: {:?}",
            status,
            input,
        );
    }

    /// Property 1b: `handle_compile_expression` never panics on arbitrary string input.
    #[test]
    fn compile_expression_never_panics(input in "\\PC{0,500}") {
        let request = compile_expression_request(&input);
        let response = handle_compile_expression(&request);
        let status = response_status(&response);
        prop_assert!(
            status.is_some(),
            "Response must have a status field for input: {:?}",
            input,
        );
        let status = status.unwrap();
        prop_assert!(
            status == "ok" || status == "error",
            "Status must be 'ok' or 'error', got {:?} for input: {:?}",
            status,
            input,
        );
    }

    /// Property 1c: `handle_compile` never panics on near-valid structured input.
    #[test]
    fn compile_never_panics_near_valid(input in near_valid_beamtalk()) {
        let request = compile_request(&input);
        let response = handle_compile(&request);
        let status = response_status(&response);
        prop_assert!(status.is_some());
    }

    /// Property 1d: `handle_compile_expression` never panics on near-valid structured input.
    #[test]
    fn compile_expression_never_panics_near_valid(input in near_valid_beamtalk()) {
        let request = compile_expression_request(&input);
        let response = handle_compile_expression(&request);
        let status = response_status(&response);
        prop_assert!(status.is_some());
    }

    /// Property 2: Error responses have non-empty diagnostics.
    ///
    /// When status is "error", the diagnostics list must be non-empty.
    #[test]
    fn error_responses_have_diagnostics(input in "\\PC{0,500}") {
        // Test both compile paths
        for response in [
            handle_compile(&compile_request(&input)),
            handle_compile_expression(&compile_expression_request(&input)),
        ] {
            if response_status(&response).as_deref() == Some("error") {
                let diags = response_diagnostics(&response);
                prop_assert!(
                    diags.is_some(),
                    "Error response must have 'diagnostics' field for input: {:?}",
                    input,
                );
                prop_assert!(
                    !diags.unwrap().elements.is_empty(),
                    "Error diagnostics must be non-empty for input: {:?}",
                    input,
                );
            }
        }
    }

    /// Property 3: Diagnostic entries are structured maps with a non-empty message.
    ///
    /// BT-1235: Every diagnostic in an error response must be a Map with a
    /// non-empty `message` binary field (and optionally `line` and `hint`).
    #[test]
    fn diagnostics_are_nonempty_strings(input in "\\PC{0,500}") {
        for response in [
            handle_compile(&compile_request(&input)),
            handle_compile_expression(&compile_expression_request(&input)),
        ] {
            if response_status(&response).as_deref() == Some("error") {
                if let Some(diags) = response_diagnostics(&response) {
                    for (i, diag_term) in diags.elements.iter().enumerate() {
                        let Term::Map(diag_map) = diag_term else {
                            prop_assert!(
                                false,
                                "Diagnostic {} is not a Map term for input: {:?}",
                                i,
                                input,
                            );
                            continue;
                        };
                        let msg_term = diag_map.map.get(&atom("message"));
                        prop_assert!(
                            msg_term.is_some(),
                            "Diagnostic {} has no 'message' field for input: {:?}",
                            i,
                            input,
                        );
                        if let Some(Term::Binary(b)) = msg_term {
                            let text = String::from_utf8(b.bytes.clone());
                            prop_assert!(
                                text.is_ok(),
                                "Diagnostic {} message is not valid UTF-8 for input: {:?}",
                                i,
                                input,
                            );
                            prop_assert!(
                                !text.unwrap().is_empty(),
                                "Diagnostic {} message is empty for input: {:?}",
                                i,
                                input,
                            );
                        } else {
                            prop_assert!(
                                false,
                                "Diagnostic {} message is not a Binary for input: {:?}",
                                i,
                                input,
                            );
                        }
                    }
                }
            }
        }
    }
}
