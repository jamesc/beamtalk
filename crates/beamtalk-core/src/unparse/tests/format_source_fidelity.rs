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
            added: vec!["255".to_string()],
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
        verify("x := 1\n//   a\ny := 2\n", "x := 1\n// a\ny := 2\n"),
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
    // `-16rFF` is one literal (BT-5182), so a negated radix joins its `-`
    // like any other negated number.
    assert_kept("x match: [\n  -16rFF -> 1;\n  _ -> 0\n]\n");
    assert_eq!(
        fmt("x match: [- 16rFF -> 1; _ -> 0]\n").as_deref(),
        Some("x match: [\n  -16rFF -> 1;\n  _ -> 0\n]\n")
    );
    assert_kept("x := <<-16rFF:8>>\n");
    assert_eq!(
        fmt("x := <<- 16rFF:8>>\n").as_deref(),
        Some("x := <<-16rFF:8>>\n")
    );
    assert_eq!(
        fmt("x match: [- 5 -> 1; - 1e3 -> 2; _ -> 0]\n").as_deref(),
        Some("x match: [\n  -5 -> 1;\n  -1e3 -> 2;\n  _ -> 0\n]\n")
    );
}

#[test]
fn big_integer_numbers_keep_their_spelling() {
    // BT-5182: literals past `i64` are `Literal::BigInteger`; their radix
    // spelling survives like any other number's.
    assert_kept("x := 16rFFFFFFFFFFFFFFFFFF\n");
    assert_kept("x := -36rZZZZZZZZZZZZZZZZ + 100000000000000000000\n");
    assert_kept("x match: [\n  -16rFFFFFFFFFFFFFFFFFF -> 1;\n  _ -> 0\n]\n");
}

#[test]
fn binary_unit_keeps_its_spelling() {
    assert_kept("x := <<y:2/unit:16r8>>\n");
    assert_kept("x := <<y:16r2/integer-unit:2r1000>>\n");
    assert_kept("x match: [\n  <<a:2/unit:16r8>> -> a;\n  _ -> 0\n]\n");
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
fn line_break_inside_a_quoted_symbol_is_part_of_its_value() {
    // The symbol's raw line break is its value, not layout: it is kept as
    // written whatever the file's line endings are.
    assert_kept("x := #'a\r\nb'\n");
    assert_kept("x := #'a\nb'\r\ny := 1\r\n");
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
fn reindent_fallback_still_indents_code_after_an_unterminated_string() {
    // A half-edited body: the stray `"` lexes as an error token running to
    // the end, but the lines after it are still code.
    let source = "foo =>\n  x := \"abc\n  y bar\n  z baz";
    assert_eq!(
        reindent_method_source("  ", source),
        "  foo =>\n    x := \"abc\n    y bar\n    z baz"
    );
}

#[test]
fn reindent_fallback_treats_crlf_blank_lines_as_blank() {
    let source = "foo =>\r\n  1\r\n\r\n  (\r\n";
    let out = reindent_method_source("  ", source);
    assert_eq!(out, "  foo =>\r\n    1\r\n\r\n    (\r\n");
}

// --- Comments between an expression's tokens ---
//
// Each of these positions lost its comment before BT-5183: the parser read
// comments only at statement, pipeline, match-arm and literal-item
// positions. Each must now come back where it was written, and the output
// must be canonical (formatting it again changes nothing).

/// Asserts `fmt(source) == expected` and that `expected` is canonical.
#[track_caller]
fn assert_formats_to(source: &str, expected: &str) {
    assert_eq!(fmt(source).as_deref(), Some(expected), "fmt of {source:?}");
    assert_kept(expected);
}

#[test]
fn comment_between_keyword_parts_is_kept() {
    assert_formats_to("x foo: 1 // c\n  bar: 2\n", "x\n  foo: 1  // c\n  bar: 2\n");
    assert_formats_to(
        "x foo: 1\n  // own line\n  bar: 2\n",
        "x\n  foo: 1\n  // own line\n  bar: 2\n",
    );
    assert_formats_to("x // r\n  foo: 1\n", "x  // r\n  foo: 1\n");
    assert_formats_to(
        "Object subclass: Foo\n  bar => x foo: 1 // c\n    bar: 2\n",
        "Object subclass: Foo\n\n  bar =>\n    x\n      foo: 1  // c\n      bar: 2\n",
    );
    assert_kept("x := y\n  |> foo: 1  // c\n     bar: 2\n");
}

#[test]
fn comment_around_a_cascade_semicolon_is_kept() {
    assert_formats_to("x foo; // c\n  bar\n", "x foo;  // c\n  bar\n");
    assert_formats_to("x foo // before\n  ; bar\n", "x foo;  // before\n  bar\n");
    assert_formats_to("x foo; bar; // c\n  baz\n", "x foo;\n  bar;  // c\n  baz\n");
    assert_formats_to(
        "x foo; at: 1 // c\n  put: 2\n",
        "x foo; at: 1  // c\n  put: 2\n",
    );
}

#[test]
fn comment_inside_parentheses_is_kept() {
    assert_formats_to("x := (1 // c\n  + 2)\n", "x := (1  // c\n  + 2)\n");
    assert_formats_to("x := ( // open\n  1 + 2)\n", "x := (  // open\n 1 + 2)\n");
    // The `)` after a close comment goes back to the enclosing indentation.
    assert_formats_to("x := (1 + 2 // close\n)\n", "x := (1 + 2  // close\n)\n");
    assert_formats_to(
        "x := 1 + // after op\n  2\n",
        "x := 1  // after op\n  + 2\n",
    );
}

#[test]
fn comment_between_block_parameters_and_bar_is_kept() {
    assert_formats_to("x := [:a // c\n  | a]\n", "x := [:a  // c\n  |\n  a\n]\n");
}

#[test]
fn own_line_comment_before_block_close_is_kept() {
    assert_formats_to("x := [:a | a\n  // c\n]\n", "x := [:a |\n  a\n  // c\n]\n");
    assert_formats_to(
        "Object subclass: Foo\n  bar => [:a | a\n    // c\n  ]\n",
        "Object subclass: Foo\n\n  bar =>\n    [:a |\n      a\n      // c\n    ]\n",
    );
    assert_formats_to("x := [ // c\n]\n", "x := [  // c\n]\n");
}

#[test]
fn comment_after_block_opener_is_kept() {
    assert_formats_to("x := [ // c\n  a]\n", "x := [\n  // c\n  a\n]\n");
    assert_formats_to("x := [:a | // c\n  a]\n", "x := [:a |\n  // c\n  a\n]\n");
}

#[test]
fn trailing_doc_comment_after_code_is_kept() {
    assert_formats_to("x := 1 /// d\n", "x := 1  /// d\n");
}

#[test]
fn comment_after_assignment_or_return_is_kept() {
    assert_formats_to("x := // c\n  1\n", "x :=  // c\n  1\n");
    assert_formats_to("^ // c\n  1\n", "^  // c\n  1\n");
}

#[test]
fn gap_comments_survive_per_method_source() {
    // `unparse_method` (CompiledMethod.source, live edits) reads the same
    // per-method table.
    let module = parse_source("Object subclass: Foo\n  bar => x foo: 1 // c\n    bar: 2\n");
    let method = &module.classes[0].methods[0];
    assert_eq!(
        unparse_method(method),
        "bar =>\n  x\n    foo: 1  // c\n    bar: 2"
    );
}

#[test]
fn line_comments_on_both_sides_of_an_operator_stay_on_their_own_lines() {
    // Two `//` comments must never be joined onto one line.
    assert_formats_to(
        "x := 1 // a\n  + // b\n  2\n",
        "x := 1  // a\n  // b\n  + 2\n",
    );
    assert_formats_to(
        "x foo // a\n  ; // b\n  bar\n",
        "x foo;  // a\n  // b\n  bar\n",
    );
}

#[test]
fn trailing_doc_comment_in_a_one_statement_block_does_not_swallow_the_bracket() {
    assert_formats_to("x := [:a | a /// d\n]\n", "x := [:a |\n  a  /// d\n]\n");
    assert_formats_to(
        "x foo: [a /// d\n] bar: 1\n",
        "x\n  foo: [\n    a  /// d\n  ]\n  bar: 1\n",
    );
}

#[test]
fn comment_around_a_concatenation_is_kept() {
    assert_formats_to(
        "x := \"a\" ++ // c\n  \"b\" ++ \"c\"\n",
        "x := \"a\"  // c\n  ++ \"b\"\n  ++ \"c\"\n",
    );
    assert_formats_to(
        "x := \"a\" // c\n  ++ \"b\"\n",
        "x := \"a\"  // c\n  ++ \"b\"\n",
    );
}

#[test]
fn comment_around_a_binary_cascade_message_is_kept() {
    assert_formats_to("x foo; + // c\n  2\n", "x foo; +  // c\n  2\n");
    // A comment after the `;` belongs to the cascade gap only; reading it
    // again at the operator would print it twice.
    assert_formats_to("x foo; // c\n  + 2\n", "x foo;  // c\n  + 2\n");
    assert_formats_to("x foo // c\n  ; + 2\n", "x foo;  // c\n  + 2\n");
}

#[test]
fn comment_after_destructure_or_state_default_is_kept() {
    assert_formats_to("{a, b} := // c\n  foo\n", "{a, b} :=  // c\n  foo\n");
    assert_formats_to("#[a, b] := // c\n  foo\n", "#[a, b] :=  // c\n  foo\n");
    assert_formats_to(
        "#{#k => v} := // c\n  foo\n",
        "#{#k => v} :=  // c\n  foo\n",
    );
    assert_formats_to(
        "Object subclass: Foo\n  state: x = // c\n    1\n",
        "Object subclass: Foo\n  state: x =  // c\n    1\n",
    );
}

#[test]
fn comment_between_keyword_and_argument_is_kept() {
    assert_formats_to("x foo: // c\n  1\n", "x\n  foo:  // c\n    1\n");
    assert_formats_to(
        "x foo: 1 bar: // c\n  2\n",
        "x\n  foo: 1\n  bar:  // c\n    2\n",
    );
    assert_formats_to(
        "x foo; at: // c\n  1 put: 2\n",
        "x foo; at:  // c\n  1 put: 2\n",
    );
}

#[test]
fn comments_around_match_arms_are_kept() {
    assert_formats_to(
        "y := x // c\n  match: [1 -> 2]\n",
        "y := x  // c\n  match: [\n    1 -> 2\n  ]\n",
    );
    assert_formats_to(
        "x match: [1 -> 2 /// d\n ; _ -> 3]\n",
        "x match: [\n  1 -> 2;  /// d\n  _ -> 3\n]\n",
    );
    assert_formats_to(
        "x match: [\n  1 -> 2  // d\n  _ -> 3  // e\n  // f\n]\n",
        "x match: [\n  1 -> 2;  // d\n  _ -> 3  // e\n  // f\n]\n",
    );
    assert_formats_to(
        "x match: [1 -> 2 // d\n]\n",
        "x match: [\n  1 -> 2  // d\n]\n",
    );
}

#[test]
fn value_after_a_gap_comment_indents_its_own_continuation_lines() {
    assert_formats_to(
        "^ // c\n  x foo: 1 // d\n    bar: 2\n",
        "^  // c\n  x\n    foo: 1  // d\n    bar: 2\n",
    );
}

#[test]
fn blank_line_before_a_gap_comment_is_kept() {
    assert_formats_to(
        "x := [:a | a\n\n  // c\n]\n",
        "x := [:a |\n  a\n\n  // c\n]\n",
    );
}

#[test]
fn comment_in_an_empty_block_has_no_stray_spaces() {
    assert_formats_to("x := [:a | // c\n]\n", "x := [:a |  // c\n]\n");
}

#[test]
fn comment_before_a_block_parameter_is_kept() {
    assert_formats_to(
        "x := [:a // c\n  :b | a]\n",
        "x := [:a  // c\n  :b |\n  a\n]\n",
    );
    assert_formats_to("x := [ // c\n  :a | a]\n", "x := [  // c\n  :a |\n  a\n]\n");
}

#[test]
fn comment_before_the_next_statement_is_not_taken_by_a_missing_argument() {
    // `bar:` has no argument: the `at:` on the next line cannot be one. The
    // comment before it leads that next statement and must not also be
    // filed as `bar:`'s argument gap.
    let source = "object foo: 1 bar:\n\n// next\nat: 42\n";
    let module = parse_source(source);
    let anchor = u32::try_from(source.find("at:").expect("at:")).expect("offset");
    assert_eq!(
        module
            .literal_comments
            .gap(anchor, crate::ast::GapSlot::AfterKeyword),
        None
    );
}
