// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `format_source` must not lose or change source (BT-5183).
//!
//! One test per reported loss: symbols the lexer cannot read back, radix and
//! exponent literals rewritten as decimal, mixed line endings in CRLF files,
//! raw multi-line strings re-indented by the per-method fallback, and
//! comments in positions the parser does not attach. Each test reproduces
//! the loss through the public entry point and asserts that the source
//! survives, or (for losses not fixed here) that the safety net refuses
//! the output instead of writing it.

use super::super::round_trip::verify_round_trip;
use super::common::*;

/// `format_source` with the error discarded, so each assertion reads as
/// "this is the text `fmt` writes" (`None`: `fmt` writes nothing).
fn fmt(source: &str) -> Option<String> {
    format_source(source).ok()
}

/// Asserts that `source` is already canonical and survives `fmt` unchanged.
#[track_caller]
fn assert_kept(source: &str) {
    assert_eq!(
        fmt(source).as_deref(),
        Some(source),
        "fmt changed {source:?}"
    );
}

// --- Safety net ---

fn verify(source: &str, formatted: &str) -> Result<(), FormatError> {
    let (module, diags) = crate::source_analysis::parse(lex_with_eof(source));
    assert!(diags.is_empty(), "fixture must parse: {diags:?}");
    verify_round_trip(source, &module, formatted)
}

#[test]
fn safety_net_rejects_a_lost_comment() {
    assert_eq!(
        verify("x := 1 // keep me\n", "x := 1\n"),
        Err(FormatError::CommentsChanged {
            missing: vec!["// keep me".to_string()],
            added: Vec::new(),
        })
    );
}

#[test]
fn safety_net_rejects_an_added_comment() {
    assert!(matches!(
        verify("x := 1\n", "x := 1 // new\n"),
        Err(FormatError::CommentsChanged { missing, added })
            if missing.is_empty() && added == ["// new"]
    ));
}

#[test]
fn safety_net_rejects_a_respelled_number() {
    assert_eq!(
        verify("x := 16rFF\n", "x := 255\n"),
        Err(FormatError::NumberSpellingChanged {
            missing: vec!["16rFF".to_string()],
        })
    );
}

#[test]
fn safety_net_rejects_a_changed_program() {
    assert!(matches!(
        verify("x := 1 + 2\n", "x := 1 - 2\n"),
        Err(FormatError::AstChanged { .. })
    ));
    assert!(matches!(
        verify("x := #(1, 2)\n", "x := #(2, 1)\n"),
        Err(FormatError::AstChanged { .. })
    ));
}

#[test]
fn safety_net_rejects_output_that_does_not_parse() {
    assert_eq!(
        verify("x := #'café'\n", "x := #café\n"),
        Err(FormatError::OutputDoesNotParse)
    );
}

#[test]
fn safety_net_accepts_layout_only_changes() {
    assert_eq!(verify("x   :=   1   //c\n", "x := 1  // c\n"), Ok(()));
    assert_eq!(
        verify(
            "x := 1\n/* a\n     b */\ny := 2\n",
            "x := 1\n/* a\n b */\ny := 2\n"
        ),
        Ok(())
    );
}

#[test]
fn format_error_messages_say_the_file_was_left_unchanged() {
    let err = FormatError::CommentsChanged {
        missing: vec!["// a".to_string(), "// b".to_string()],
        added: Vec::new(),
    };
    let text = err.to_string();
    assert!(text.contains("left unchanged"), "{text}");
    assert!(text.contains("// a and 1 more"), "{text}");
}

// --- Symbols ---

#[test]
fn symbol_outside_the_lexers_ascii_identifiers_stays_quoted() {
    // The lexer's bare symbols are ASCII-only: `#café` does not re-lex.
    assert_kept("x := #'café'\n");
    assert_kept("x := #'ñ'\n");
    assert_kept("x := #'a::b'\n");
    assert_kept("x := #'a:='\n");
    assert_kept("x := #'1abc'\n");
}

#[test]
fn quoted_symbol_quotes_are_re_escaped() {
    assert_kept("x := #'it''s'\n");
    assert_kept("x := #''''\n");
}

#[test]
fn identifier_and_keyword_symbols_print_bare() {
    assert_eq!(fmt("x := #'foo'\n").as_deref(), Some("x := #foo\n"));
    assert_eq!(fmt("x := #'at:put:'\n").as_deref(), Some("x := #at:put:\n"));
    // The lexer reads `#a:b` back as the one symbol `a:b`.
    assert_eq!(fmt("x := #'a:b'\n").as_deref(), Some("x := #a:b\n"));
    assert_kept("x := #'+'\n");
}

#[test]
fn symbol_display_matches_the_lexer() {
    // `unparse_literal_display` feeds doc comments and hover: same rules.
    assert_eq!(
        unparse_literal_display(&Literal::Symbol("it's".into())),
        "#'it''s'"
    );
    assert_eq!(
        unparse_literal_display(&Literal::Symbol("café".into())),
        "#'café'"
    );
}

// --- Number spellings ---

#[test]
fn radix_integers_keep_their_spelling() {
    assert_kept("x := 16rFF\n");
    assert_kept("x := 2r1010\n");
    assert_kept("x := 36rZZ\n");
    assert_kept("x := 16rff\n");
}

#[test]
fn float_exponents_keep_their_spelling() {
    assert_kept("x := 1e10\n");
    assert_kept("x := 2.5e-3\n");
    assert_kept("x := 1.50\n");
    assert_kept("x := -1.5e3\n");
}

#[test]
fn number_spellings_survive_in_every_literal_position() {
    assert_kept("x := #(16rFF, 1e3, -1.5e3)\n");
    assert_kept("x := #[16rFF, 1e3]\n");
    assert_kept("x := {16rFF, 1e3}\n");
    assert_kept("x := #{#a => 16rFF}\n");
    assert_kept("x := <<16rFF:8, 1e3/float>>\n");
    assert_kept("x := 16rFF + 1e3\n");
    assert_kept("x foo: 16rFF bar: 2r1\n");
}

#[test]
fn negated_pattern_numbers_keep_their_spelling() {
    assert_kept("x match: [\n  16rFF -> 1;\n  1e3 -> 2;\n  _ -> 0\n]\n");
    // `-16rFF` would lex as `-16 rFF`, so a negated radix keeps its `- `.
    assert_kept("x match: [\n  - 16rFF -> 1;\n  _ -> 0\n]\n");
    assert_kept("x := <<- 16rFF:8>>\n");
    assert_eq!(
        fmt("x match: [- 5 -> 1; - 1e3 -> 2; _ -> 0]\n").as_deref(),
        Some("x match: [\n  -5 -> 1;\n  -1e3 -> 2;\n  _ -> 0\n]\n")
    );
}

#[test]
fn shape_version_keeps_its_spelling() {
    assert_kept("Object subclass: Foo\n  shapeVersion: 16r2\n\n  bar => 1\n");
}

#[test]
fn method_source_keeps_number_spellings() {
    // `unparse_method` builds `CompiledMethod.source` and the live-edit
    // stored source: it must not respell either.
    let module = parse_source("Object subclass: Foo\n  bar => 16rFF + 1e3\n");
    let method = &module.classes[0].methods[0];
    assert_eq!(unparse_method(method), "bar => 16rFF + 1e3");
}

// --- Line endings ---

#[test]
fn crlf_file_keeps_crlf_line_endings() {
    let source = "// head\r\nx := 1  // c\r\n\r\ny := \"a\r\nb\"\r\n";
    let out = fmt(source).expect("CRLF source formats");
    assert!(
        !out.replace("\r\n", "").contains('\n'),
        "mixed line endings: {out:?}"
    );
    assert!(!out.replace("\r\n", "").contains('\r'), "stray CR: {out:?}");
    assert_eq!(out, "// head\r\nx := 1  // c\r\n\r\ny := \"a\\r\\nb\"\r\n");
    assert_eq!(fmt(&out).as_deref(), Some(out.as_str()), "not idempotent");
}

#[test]
fn crlf_block_comment_keeps_crlf_line_endings() {
    let source = "/* one\r\n   two */\r\nx := 1\r\n";
    let out = fmt(source).expect("CRLF source formats");
    assert!(!out.replace("\r\n", "").contains('\n'), "{out:?}");
}

#[test]
fn lf_file_keeps_lf_line_endings() {
    let out = fmt("x := 1  // c\ny := 2\n").expect("formats");
    assert!(!out.contains('\r'), "{out:?}");
}

// --- Per-method re-indent fallback ---

#[test]
fn reindent_fallback_leaves_raw_multi_line_strings_alone() {
    // Does not parse as a method (unclosed paren), so the fallback shift
    // runs. The string's continuation lines are part of its value.
    let source = "foo =>\n  x := \"line one\n    line two\n  line three\"\n  (\n";
    let out = reindent_method_source("    ", source);
    assert!(
        out.contains("\"line one\n    line two\n  line three\""),
        "string value changed: {out:?}"
    );
}

#[test]
fn reindent_fallback_treats_crlf_blank_lines_as_blank() {
    let source = "foo =>\r\n  1\r\n\r\n  (\r\n";
    let out = reindent_method_source("  ", source);
    assert_eq!(out, "  foo =>\r\n    1\r\n\r\n    (\r\n");
}

// --- Comments the parser does not attach yet ---
//
// Each of these positions lost its comment before BT-5183. Until the parser
// attaches them, the safety net must refuse to format rather than drop them.

#[test]
fn comments_in_unattached_positions_are_never_dropped() {
    for source in [
        "x foo: 1 // c\n  bar: 2\n",
        "x foo; // c\n  bar\n",
        "x := (1 // c\n  + 2)\n",
        "x := [:a // c\n  | a]\n",
        "x := [:a | a\n  // c\n]\n",
        "x := 1 /// d\n",
    ] {
        match format_source(source) {
            Ok(out) => assert_eq!(
                crate::unparse::round_trip::comment_multiset(&out),
                crate::unparse::round_trip::comment_multiset(source),
                "comment lost from {source:?}: {out:?}"
            ),
            Err(FormatError::CommentsChanged { .. }) => {}
            Err(e) => panic!("{source:?}: unexpected {e}"),
        }
    }
}
