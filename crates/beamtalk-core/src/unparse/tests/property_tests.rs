// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Property-based tests for the Beamtalk unparser (round-trip / idempotency).
//!
//! These tests verify that the unparser faithfully round-trips parsed ASTs:
//!
//! 1. **`format_source` never panics** — arbitrary input returns `None` or `Some`
//! 2. **`format_source` is idempotent** — formatting twice yields the same output
//! 3. **Round-trip preserves structure** — parse → unparse → re-parse → unparse
//!    produces the same string on both passes
//! 4. **Formatting preserves the source** (BT-5183) — for every input that
//!    parses cleanly, the output keeps every comment, every number spelling
//!    and the same AST as the *input* (not just as pass 1), in LF and CRLF
//!    files alike, over fragments with comments in every position, cascades,
//!    negative and radix numbers, symbols and Unicode
//!
//! **DDD Context:** Language Service — Formatting / Unparse
//!
//! ADR 0011 Phase 2 (extended).

use proptest::prelude::*;

use crate::test_helpers::test_support::near_valid_from;

use super::super::round_trip::{ast_fingerprint, comment_multiset, number_spellings};
use super::super::{FormatError, format_source, unparse_module};
use crate::source_analysis::{Severity, lex_with_eof, parse};

// ============================================================================
// Generators
// ============================================================================

/// Near-valid Beamtalk fragments for round-trip testing.
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
    "sealed Object subclass: Shape",
];

/// Fragments for the fidelity property (BT-5183): each exercises source the
/// formatter must not lose or change — comments in every position the issue
/// listed, cascades, negative, radix and exponent numbers, quoted and
/// keyword symbols, and Unicode.
const FIDELITY_FRAGMENTS: &[&str] = &[
    "x := 16rFF",
    "x := 2r1010 + 36rZZ",
    "x := 1e10",
    "x := 2.5e-3",
    "x := -1.5e3",
    "x := -7",
    "x := 1.50",
    "x := #(16rFF, -3, 1e3)",
    "x := <<16rFF:8, 1e3/float>>",
    "x match: [- 16rFF -> 1; - 5 -> 2; _ -> 0]",
    "x := #'café'",
    "x := #'it''s'",
    "x := #'a:b'",
    "x := #at:put:",
    "x := #'+'",
    "x := #'with space'",
    "x := \"héllo wörld\"",
    "x := $é",
    "// leading comment\nx := 1",
    "x := 1  // trailing comment",
    "/* block */ x := 1",
    "/* multi\n   line */\nx := 1",
    "/// doc\nObject subclass: Foo\n  bar => 1",
    "x foo: 1 // between keywords\n  bar: 2",
    "Transcript show: 1; // after cascade\n  show: 2",
    "Transcript show: 1; show: 2; cr",
    "x := (1 // inside parens\n  + 2)",
    "x := [:a // after param\n  | a]",
    "x := [:a | a\n  // before close\n]",
    "x := 1 /// trailing doc",
    "x := #(1, // in list\n  2)",
    "x\n  |> foo  // pipeline\n  |> bar",
];

// ============================================================================
// Property tests
// ============================================================================

fn proptest_config() -> ProptestConfig {
    let mut config = crate::test_helpers::test_support::proptest_config_default();
    let cases = config.cases;
    // The round-trip test filters out inputs with parse errors via
    // `prop_assume!`, so the rejection rate can be high. Scale the
    // global-reject cap with the case count to avoid "too many rejects"
    // failures when running with large PROPTEST_CASES values, while
    // still respecting any higher env/default `max_global_rejects`.
    config.max_global_rejects = config.max_global_rejects.max(cases * 2);
    config
}

proptest! {
    #![proptest_config(proptest_config())]

    /// Property 1: `format_source` never panics on arbitrary input.
    ///
    /// It may return `None` (parse errors) or `Some(formatted)`, but must not panic.
    #[test]
    fn format_source_never_panics(input in "\\PC{0,300}") {
        let _result = format_source(&input);
    }

    /// Property 1b: `format_source` never panics on near-valid input.
    #[test]
    fn format_source_never_panics_near_valid(input in near_valid_from(FRAGMENTS)) {
        let _result = format_source(&input);
    }

    /// Property 2: `format_source` is idempotent.
    ///
    /// If `format_source(s)` returns `Some(f)`, then `format_source(f)` must also
    /// return `Some` with the same output.
    #[test]
    fn format_source_idempotent(input in near_valid_from(FRAGMENTS)) {
        if let Ok(pass1) = format_source(&input) {
            let pass2 = format_source(&pass1);
            prop_assert!(
                pass2.is_ok(),
                "format_source refused on already-formatted output for input {:?}",
                input,
            );
            prop_assert_eq!(
                &pass1,
                pass2.as_ref().unwrap(),
                "format_source is not idempotent for input {:?}",
                input,
            );
        }
    }

    /// Property 4 (BT-5183): formatting preserves the source.
    ///
    /// For every input (LF or CRLF) that parses cleanly, `format_source`
    /// either returns text with the input's comments, number spellings and
    /// AST, or refuses with [`FormatError::CommentsChanged`] (a comment in a
    /// position the parser does not attach yet). Never a lossy result,
    /// never another refusal. Checked here directly, not through the
    /// formatter's own gate.
    #[test]
    fn format_source_preserves_the_source(
        input in near_valid_from(FIDELITY_FRAGMENTS),
        crlf in any::<bool>(),
    ) {
        // A trailing newline gives every input a line ending to keep.
        let input = format!("{input}\n");
        let input = if crlf { input.replace('\n', "\r\n") } else { input };
        let (module, diags) = parse(lex_with_eof(&input));
        prop_assume!(!diags.iter().any(|d| d.severity == Severity::Error));
        match format_source(&input) {
            Ok(out) => {
                prop_assert_eq!(comment_multiset(&out), comment_multiset(&input), "comments of {:?}", input);
                prop_assert_eq!(number_spellings(&out), number_spellings(&input), "numbers of {:?}", input);
                let (reparsed, _) = parse(lex_with_eof(&out));
                prop_assert_eq!(ast_fingerprint(&reparsed), ast_fingerprint(&module), "AST of {:?}", input);
                let stripped = if crlf { out.replace("\r\n", "") } else { out.clone() };
                prop_assert!(
                    !stripped.contains(if crlf { '\n' } else { '\r' }),
                    "mixed line endings for {:?}: {:?}", input, out
                );
                let pass2 = format_source(&out).ok();
                prop_assert_eq!(pass2.as_deref(), Some(out.as_str()), "not idempotent");
            }
            Err(FormatError::CommentsChanged { .. }) => {}
            Err(e) => prop_assert!(false, "{:?} refused: {}", input, e),
        }
    }

    /// Property 3: Round-trip preserves structure.
    ///
    /// parse → unparse → re-parse → unparse must produce the same string on
    /// both unparse passes (we compare at string level since AST spans differ).
    #[test]
    fn unparse_roundtrip_preserves_structure(input in near_valid_from(FRAGMENTS)) {
        let tokens1 = lex_with_eof(&input);
        let (module1, diags1) = parse(tokens1);

        // Only test round-trip on error-free parses — errors lose information.
        let has_errors = diags1.iter().any(|d| d.severity == crate::source_analysis::Severity::Error);
        prop_assume!(!has_errors);

        let unparsed1 = unparse_module(&module1);

        let tokens2 = lex_with_eof(&unparsed1);
        let (module2, diags2) = parse(tokens2);

        // The re-parse should also be error-free.
        let has_errors2 = diags2.iter().any(|d| d.severity == crate::source_analysis::Severity::Error);
        prop_assert!(
            !has_errors2,
            "Re-parsing unparsed output produced errors for input {:?}:\nunparsed: {:?}\ndiags: {:?}",
            input,
            unparsed1,
            diags2.iter().map(|d| d.message.as_str()).collect::<Vec<_>>(),
        );

        let unparsed2 = unparse_module(&module2);
        prop_assert_eq!(
            unparsed1,
            unparsed2,
            "Round-trip unparse mismatch for input {:?}",
            input,
        );
    }
}
