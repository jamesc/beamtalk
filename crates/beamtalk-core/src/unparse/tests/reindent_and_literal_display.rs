// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `escape_string_literal`, `reindent_method_source` (BT-2584), the
//! `unparse_method` round-trip, and `unparse_literal_display` (BT-3088)
//! (moved from `unparse/mod.rs` per BT-3450).

use super::common::*;

// --- escape_string_literal ---

#[test]
fn escape_string_literal_plain_string_unchanged() {
    assert_eq!(escape_string_literal("hello"), "hello");
}

#[test]
fn escape_string_literal_escapes_double_quote() {
    assert_eq!(escape_string_literal("a\"b"), "a\\\"b");
}

#[test]
fn escape_string_literal_escapes_backslash() {
    assert_eq!(escape_string_literal("a\\b"), "a\\\\b");
}

#[test]
fn escape_string_literal_escapes_open_brace() {
    assert_eq!(escape_string_literal("a{b}"), "a\\{b}");
}

#[test]
fn escape_string_literal_backslash_before_brace_both_escaped() {
    // Backslash must be escaped before brace so "\{x}" → "\\\{x}" (escaped
    // backslash `\\` + escaped brace `\{`), not "\\{x}" (escaped backslash
    // followed by an unescaped `{` that would start interpolation).
    assert_eq!(escape_string_literal("\\{x}"), "\\\\\\{x}");
}

#[test]
fn escape_string_literal_mixed_backslash_quote_brace() {
    // Verifies all three escape rules fire correctly in one input and that
    // replacement order does not corrupt any of them.
    assert_eq!(escape_string_literal("\\\"\\{"), "\\\\\\\"\\\\\\{");
}

// --- reindent_method_source (BT-2584) ---

#[test]
fn reindent_empty_base_is_identity_for_canonical() {
    // Canonical (column-0) input + empty base indent is a no-op.
    let src = "foo => 1\n";
    assert_eq!(reindent_method_source("", src), src);
}

#[test]
fn reindent_shifts_canonical_body_to_base() {
    // A column-0 single-line method shifted to 2-space class-body indent.
    assert_eq!(reindent_method_source("  ", "foo => 1\n"), "  foo => 1\n");
}

#[test]
fn reindent_preserves_relative_indentation() {
    // A multi-statement body keeps its 2-space relative step under the new
    // base (the body stays multi-line because it is genuinely > 1 statement).
    assert_eq!(
        reindent_method_source("  ", "foo =>\n  a\n  b\n"),
        "  foo =>\n    a\n    b\n"
    );
}

#[test]
fn reindent_rebreaks_line_too_wide_at_indent() {
    // BT-2594: the core re-layout property. A single-expression body that
    // fits inline at column 0 (exactly 80 cols) must break to its own line
    // once indented, because the indent steals from the width budget — a pure
    // whitespace shift could not do this.
    let src = "compute => self firstThing + self secondThing + self thirdThing + moreStuffHereX\n";
    // At column 0 it fits and stays inline.
    assert_eq!(reindent_method_source("", src), src);
    // Indented two spaces, the same body must break to the next line.
    assert_eq!(
        reindent_method_source("  ", src),
        "  compute =>\n    self firstThing + self secondThing + self thirdThing + moreStuffHereX\n"
    );
}

#[test]
fn reindent_preserves_blank_line_between_statements() {
    // A blank line the author left between two body statements (BT-987) is
    // preserved by the re-layout and shifted to the new base (blank stays
    // empty — no indent, no trailing space).
    assert_eq!(
        reindent_method_source("  ", "foo =>\n  a\n\n  b\n"),
        "  foo =>\n    a\n\n    b\n"
    );
}

#[test]
fn reindent_inline_comment_preserved() {
    // A trailing `//` comment rides along with its statement.
    let src = "foo => 1  // bump\n";
    assert_eq!(reindent_method_source("  ", src), "  foo => 1  // bump\n");
}

#[test]
fn reindent_doc_comment_and_multiline_body() {
    // A `///` doc comment sits directly above the signature, and a
    // multi-statement body keeps its relative indentation.
    let src = "/// doc\nfoo =>\n  a\n  b\n";
    assert_eq!(
        reindent_method_source("  ", src),
        "  /// doc\n  foo =>\n    a\n    b\n"
    );
}

#[test]
fn reindent_tab_base_indent() {
    // A tab base indent is prepended verbatim to every non-blank line.
    assert_eq!(
        reindent_method_source("\t", "foo =>\n  a\n  b\n"),
        "\tfoo =>\n\t  a\n\t  b\n"
    );
}

#[test]
fn reindent_no_trailing_newline_is_preserved() {
    // The disk slice clamps to EOF when the final body line has no trailing
    // newline; reindent must not add one.
    assert_eq!(reindent_method_source("  ", "foo => 1"), "  foo => 1");
}

#[test]
fn reindent_is_idempotent_on_canonical_disk_shape() {
    // Re-indenting an already-canonical, already-disk-shaped method to the
    // same base is a no-op.
    let disk = "  /// doc\n  foo =>\n    a\n    b\n";
    assert_eq!(reindent_method_source("  ", disk), disk);
}

#[test]
fn reindent_emits_class_prefix_for_class_side_method() {
    // BT-2594: the `class ` modifier is recovered from the re-parsed method
    // and re-emitted, so a class-side method's stored source keeps its prefix.
    assert_eq!(
        reindent_method_source("  ", "class spawn => self new\n"),
        "  class spawn => self new\n"
    );
}

// --- unparse_method round-trip (moved from source_analysis's
// `parser::tests::method_tests`, BT-3346 / ADR 0117 Phase 4) ---

/// Parses a single bare method definition (`Class >> ` header not
/// required — the live-image "compile one method" idiom), failing loudly
/// on any parse error.
fn parse_method_ok(source: &str) -> crate::ast::MethodDefinition {
    let (method, diagnostics) = parse_method(lex_with_eof(source));
    let errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert!(errors.is_empty(), "unexpected parse errors: {errors:?}");
    method.expect("expected a single method definition")
}

#[test]
fn parse_method_unparse_drops_banner_keeps_doc() {
    // unparse_method is what gets STORED as the live method `__source__`, and it
    // must match the method's byte span — which starts at the `///` doc block and
    // excludes a leading `//` section banner (the banner is inter-method file
    // structure, not part of the method). So unparse_method drops the banner but
    // keeps the doc comment; whole-file unparse preserves the banner in place
    // (BT-2594; banners become first-class categories in BT-2601).
    let src = "// --- Execution CRUD ---\n\
               \n\
               /// Store a new workflow execution.\n\
               /// Raises if the workflowId already exists.\n\
               createExecution: execution :: Object -> Object =>\n\
               \x20\x20execution";
    let m = parse_method_ok(src);
    let out = unparse_method(&m);
    assert!(
        !out.contains("--- Execution CRUD ---"),
        "unparse_method should drop the leading section banner so the stored \
         source matches the method's byte span:\n{out}"
    );
    assert!(
        out.contains("/// Store a new workflow execution."),
        "unparse dropped the first doc line:\n{out}"
    );
    assert!(
        out.contains("/// Raises if the workflowId already exists."),
        "unparse dropped a doc line:\n{out}"
    );
}

#[test]
fn parse_method_roundtrip_is_idempotent() {
    // parse -> unparse -> parse -> unparse must be stable: this is the property
    // that a method surviving N saves keeps its source intact (no per-save
    // erosion of the leading comment block).
    let src = "// --- Execution CRUD ---\n\
               \n\
               /// Store a new workflow execution.\n\
               createExecution: execution :: Object -> Object =>\n\
               \x20\x20execution";
    let once = unparse_method(&parse_method_ok(src));
    let twice = unparse_method(&parse_method_ok(&once));
    let thrice = unparse_method(&parse_method_ok(&twice));
    assert_eq!(once, twice, "method source not idempotent after 2nd save");
    assert_eq!(twice, thrice, "method source not idempotent after 3rd save");
}

// --- unparse_literal_display (BT-3088) ---
//
// These are the single source of truth for literal-to-source rendering;
// `format_default_value` (stdlib-metadata path) and hover's literal
// rendering both delegate here, so any case fixed here fixes them too.

#[test]
fn unparse_literal_display_string_with_embedded_quote() {
    // A `"` inside a string literal is doubled per Beamtalk convention,
    // not backslash-escaped like Rust's `{s:?}` would render it.
    let lit = Literal::String("say \"hi\"".into());
    assert_eq!(unparse_literal_display(&lit), "\"say \"\"hi\"\"\"");
}

#[test]
fn unparse_literal_display_symbol_with_space_is_quoted() {
    // A symbol containing a space must be rendered as `#'...'`, never as
    // an unquoted `#with space` (which isn't valid Beamtalk syntax).
    let lit = Literal::Symbol("with space".into());
    assert_eq!(unparse_literal_display(&lit), "#'with space'");
}

#[test]
fn unparse_literal_display_newline_character() {
    // `$\n` uses Beamtalk's own escape rules (via `leaf::char_lit`), not
    // Rust's `char::escape_default`.
    let lit = Literal::Character('\n');
    assert_eq!(unparse_literal_display(&lit), "$\\n");
}

#[test]
fn unparse_literal_display_integral_float_keeps_decimal_point() {
    // `1.0` must round-trip as `1.0`, not collapse to the integer `1`.
    let lit = Literal::Float(1.0);
    assert_eq!(unparse_literal_display(&lit), "1.0");
}
