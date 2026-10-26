// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared escape-mapping conformance: lex-value ↔ `printString` ↔ `fmt`
//! round-trips over every ADR 0138 escape (BT-4164).
//!
//! The three emitters of Beamtalk string spellings (Rust lexer values,
//! Beamtalk `String printString`, Rust `unparse_literal_display` / `fmt`)
//! cannot literally share one table, so the mapping is pinned by the shared
//! corpus `stdlib/test/fixtures/string_escape_corpus.json` instead. Each case
//! carries the string `value` plus its canonical `display` spelling (with
//! surrounding quotes).
//!
//! This module pins the Rust legs over every case:
//!
//! - `display` re-lexes to exactly `value` (proves the spelling round-trips);
//! - `unparse_literal_display` renders `value` byte-identically to `display`
//!   (the `fmt` leg — `string_content` escaping landed in BT-4163);
//! - `escape_string_literal` output re-lexes to `value` (the generated-code
//!   leg — value equality, not byte equality: it deliberately passes control
//!   characters through verbatim since Beamtalk strings are multi-line);
//! - `format_source` over a snippet embedding `display` is byte-identical and
//!   idempotent (formatter round-trip).
//!
//! The Beamtalk leg — `value printString` equals `display` — is asserted by
//! `stdlib/test/string_printstring_escape_test.bt` over the same file. The two
//! suites together prove `printString` output re-parses to the identical
//! string; neither side keeps a second copy of the table.

use super::common::*;
use crate::ast::Literal;
use crate::source_analysis::{TokenKind, lex};

/// One corpus entry: the string value plus its canonical display spelling.
struct EscapeCase {
    value: String,
    display: String,
    why: String,
}

fn escape_corpus() -> Vec<EscapeCase> {
    let path = beamtalk_workspace::cargo_workspace_root!()
        .join("stdlib/test/fixtures/string_escape_corpus.json");
    let raw = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("read corpus {}: {e}", path.display()));
    let cases: Vec<serde_json::Value> = serde_json::from_str(&raw).expect("corpus is a JSON array");
    assert!(!cases.is_empty(), "corpus must have cases");
    cases
        .iter()
        .map(|case| EscapeCase {
            value: case["value"]
                .as_str()
                .expect("case.value is a string")
                .to_string(),
            display: case["display"]
                .as_str()
                .expect("case.display is a string")
                .to_string(),
            why: case["why"].as_str().unwrap_or("").to_string(),
        })
        .collect()
}

/// Lex `source` and return the value of its single string token.
///
/// Panics unless the source is exactly one plain (non-interpolated) string
/// literal — an unescaped `{` in a display spelling would lex as
/// `StringStart` and fail here rather than silently testing the wrong shape.
#[track_caller]
fn lex_single_string_value(source: &str, why: &str) -> String {
    let tokens = lex(source);
    assert_eq!(
        tokens.len(),
        1,
        "expected a single token for {source:?} ({why}), got {}",
        tokens.len()
    );
    match tokens[0].kind() {
        TokenKind::String(s) => s.to_string(),
        kind => panic!("expected a String token for {source:?} ({why}), got {kind:?}"),
    }
}

#[test]
fn escape_corpus_display_relexes_to_value() {
    // The re-parse leg: every display spelling lexes back to its value.
    for case in escape_corpus() {
        assert_eq!(
            lex_single_string_value(&case.display, &case.why),
            case.value,
            "re-lex mismatch for {:?} ({})",
            case.display,
            case.why
        );
    }
}

#[test]
fn escape_corpus_unparse_literal_display_matches() {
    // The fmt leg: the Rust display hub renders each value byte-identically
    // to the corpus spelling — the same bytes `printString` must produce.
    for case in escape_corpus() {
        let lit = Literal::String(case.value.clone().into());
        assert_eq!(
            unparse_literal_display(&lit),
            case.display,
            "unparse_literal_display mismatch for {:?} ({})",
            case.value,
            case.why
        );
    }
}

#[test]
fn escape_corpus_generated_code_relexes_to_value() {
    // The generated-code leg: `escape_string_literal` output re-parses to the
    // value. Value equality only — control characters pass through verbatim
    // (multi-line generated expressions), unlike the `\n`-style spellings in
    // `display`.
    for case in escape_corpus() {
        let generated = format!("\"{}\"", escape_string_literal(&case.value));
        assert_eq!(
            lex_single_string_value(&generated, &case.why),
            case.value,
            "generated-code re-lex mismatch for {:?} ({})",
            case.value,
            case.why
        );
    }
}

#[test]
fn escape_corpus_format_is_identity_and_idempotent() {
    // The formatter round-trip: a snippet embedding each display spelling is
    // already canonical (fmt emits exactly these bytes) and stable.
    for case in escape_corpus() {
        let snippet = format!("x := {}\n", case.display);
        let pass1 = format_source(&snippet).unwrap_or_else(|| {
            panic!(
                "format_source refused {:?} ({}) — the display spelling must parse cleanly",
                case.display, case.why
            )
        });
        assert_eq!(
            pass1, snippet,
            "fmt changed canonical snippet for {:?} ({})",
            case.display, case.why
        );
        let pass2 = format_source(&pass1).expect("fmt must accept its own output");
        assert_eq!(
            pass2, pass1,
            "fmt not idempotent for {:?} ({})",
            case.display, case.why
        );
    }
}

// ============================================================================
// Property tests: `lex(print(s)) == s` for arbitrary strings (BT-4201)
// ============================================================================
//
// ADR 0138 requires the round-trip for *all* strings, not just the fixed
// corpus. These generate arbitrary Unicode (every scalar value incl. controls,
// DEL, U+2028, astral planes) biased toward the characters that carry escape
// meaning: `"`, `\`, `{`, `}`, and the control characters with spellings.

mod escape_properties {
    use super::*;
    use proptest::prelude::*;

    fn arb_string_value() -> impl Strategy<Value = String> {
        let special = prop_oneof![
            Just('"'),
            Just('\\'),
            Just('{'),
            Just('}'),
            Just('\r'),
            Just('\n'),
            Just('\t'),
            Just('\0'),
            Just('\u{7f}'),
            Just('\u{2028}'),
            Just('n'),
            Just('0'),
        ];
        let ch = prop_oneof![3 => special, 2 => any::<char>()];
        proptest::collection::vec(ch, 0..24).prop_map(|cs| cs.into_iter().collect())
    }

    /// Lex a source and return every string-part token value in order.
    fn string_parts(source: &str) -> Vec<String> {
        lex(source)
            .iter()
            .filter_map(|t| match t.kind() {
                TokenKind::String(s)
                | TokenKind::StringStart(s)
                | TokenKind::StringSegment(s)
                | TokenKind::StringEnd(s) => Some(s.to_string()),
                _ => None,
            })
            .collect()
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(512))]

        #[test]
        fn display_spelling_relexes_to_value(s in arb_string_value()) {
            let display = unparse_literal_display(&Literal::String(s.clone().into()));
            prop_assert_eq!(lex_single_string_value(&display, "proptest display"), s);
        }

        #[test]
        fn generated_code_relexes_to_value(s in arb_string_value()) {
            let generated = format!("\"{}\"", escape_string_literal(&s));
            prop_assert_eq!(lex_single_string_value(&generated, "proptest generated"), s);
        }

        #[test]
        fn display_spelling_is_fmt_stable(s in arb_string_value()) {
            let display = unparse_literal_display(&Literal::String(s.into()));
            let snippet = format!("x := {display}\n");
            let pass1 = format_source(&snippet);
            prop_assert_eq!(pass1.as_deref(), Some(snippet.as_str()));
        }

        #[test]
        fn escaped_segments_in_interpolated_string_relex_to_value(s in arb_string_value()) {
            // The same escaping is used for literal segments of an
            // interpolated string: leading, middle and trailing positions,
            // including a `}` right after an escaped `{`.
            let esc = escape_string_literal(&s);
            let parts = string_parts(&format!("\"{esc}{{x}}\""));
            prop_assert_eq!(parts, vec![s.clone(), String::new()]);
            let parts = string_parts(&format!("\"{{x}}{esc}\""));
            prop_assert_eq!(parts, vec![String::new(), s.clone()]);
            let parts = string_parts(&format!("\"{{x}}{esc}{{y}}\""));
            prop_assert_eq!(parts, vec![String::new(), s, String::new()]);
        }
    }
}
