// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Escaping helpers for Core Erlang atom and string leaves (BT-875, BT-3089).
//!
//! **DDD Context:** Compilation — Code Generation (shared leaf, ADR 0117 step 4)

/// Escapes a string for use inside a Core Erlang double-quoted string literal.
///
/// Replaces `\` with `\\` and `"` with `\"` so the result is safe to embed
/// between `"..."` in generated `.core` source.
#[must_use]
pub fn escape_core_erlang_string(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"")
}

/// Escapes special characters in an atom name for Core Erlang.
///
/// This is the single canonical funnel for atom escaping (BT-875, BT-3089):
/// every atom emitted through [`leaf::atom`](crate::leaf::atom)
/// passes through here, and other atom-formatting call sites (e.g.
/// `beamtalk-cli`'s generated-EUnit-source atom formatter) should reuse this
/// function rather than hand-rolling their own escape table.
///
/// Escapes:
/// - `\` → `\\`, `'` → `\'` — required to keep the surrounding `'...'`
///   delimiters and any embedded backslash unambiguous.
/// - `\n`, `\r`, `\t`, `\0` — **necessary, not just cosmetic** (BT-3089):
///   Beamtalk quoted-symbol literals (`#'foo bar'`) have no backslash-escape
///   syntax of their own — the lexer (`lex_symbol_or_hash`) accepts *any*
///   literal character up to the closing `'`, including a raw newline typed
///   directly in the source. A `Literal::Symbol` carrying one of these
///   characters reaches `leaf::atom` unchanged; without escaping them here,
///   the generated `.core` source would contain a literal control character
///   embedded inside a quoted atom, breaking the "one term per line"
///   assumption the rest of the pipeline (and any downstream tooling) relies
///   on. Mirrors [`escape_core_erlang_string`]'s equivalent table.
#[must_use]
pub fn escape_atom_chars(name: &str) -> String {
    let mut result = String::with_capacity(name.len());
    for c in name.chars() {
        match c {
            '\'' => result.push_str("\\'"),
            '\\' => result.push_str("\\\\"),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            '\0' => result.push_str("\\0"),
            _ => result.push(c),
        }
    }
    result
}

/// Escapes special characters for embedding in an Erlang string literal.
///
/// Handles `\`, `"`, `\n`, `\r`, `\t`, and `\0` so the result is safe to
/// embed between `"..."` in generated Erlang source (e.g. `-eval` arguments,
/// path strings passed to `erlc`, `.app` descriptions).
///
/// This is distinct from [`escape_core_erlang_string`], which is a lighter
/// variant for Core Erlang `.core` output.
#[must_use]
pub fn escape_erlang_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\\' => result.push_str("\\\\"),
            '"' => result.push_str("\\\""),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            '\0' => result.push_str("\\0"),
            _ => result.push(c),
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_escape_atom_chars_normal() {
        assert_eq!(escape_atom_chars("normal"), "normal");
    }

    #[test]
    fn test_escape_atom_chars_quote() {
        assert_eq!(escape_atom_chars("it's"), "it\\'s");
    }

    #[test]
    fn test_escape_atom_chars_backslash() {
        assert_eq!(escape_atom_chars("back\\slash"), "back\\\\slash");
    }

    /// BT-3089: a quoted symbol literal (`#'foo\nbar'`, real newline typed
    /// in source — Beamtalk quoted symbols have no backslash-escape syntax
    /// of their own) must not reach the generated `.core` source as a raw
    /// embedded control character.
    #[test]
    fn test_escape_atom_chars_control_characters() {
        assert_eq!(escape_atom_chars("foo\nbar"), "foo\\nbar");
        assert_eq!(escape_atom_chars("foo\rbar"), "foo\\rbar");
        assert_eq!(escape_atom_chars("foo\tbar"), "foo\\tbar");
        assert_eq!(escape_atom_chars("foo\0bar"), "foo\\0bar");
    }

    #[test]
    fn test_escape_atom_chars_mixed() {
        assert_eq!(escape_atom_chars("it's\na\\b"), "it\\'s\\na\\\\b");
    }

    #[test]
    fn test_escape_core_erlang_string_plain() {
        assert_eq!(escape_core_erlang_string("hello"), "hello");
    }

    #[test]
    fn test_escape_core_erlang_string_backslash() {
        assert_eq!(escape_core_erlang_string("a\\b"), "a\\\\b");
    }

    #[test]
    fn test_escape_core_erlang_string_double_quote() {
        assert_eq!(escape_core_erlang_string("say \"hi\""), "say \\\"hi\\\"");
    }

    #[test]
    fn test_escape_core_erlang_string_windows_path() {
        assert_eq!(
            escape_core_erlang_string("C:\\Users\\foo\\bar.bt"),
            "C:\\\\Users\\\\foo\\\\bar.bt"
        );
    }

    #[test]
    fn test_escape_erlang_string_empty() {
        assert_eq!(escape_erlang_string(""), "");
    }

    #[test]
    fn test_escape_erlang_string_no_special_chars() {
        assert_eq!(escape_erlang_string("hello"), "hello");
        assert_eq!(escape_erlang_string("foo_bar"), "foo_bar");
        assert_eq!(escape_erlang_string("path/to/file"), "path/to/file");
    }

    #[test]
    fn test_escape_erlang_string_backslashes() {
        assert_eq!(escape_erlang_string("a\\b"), "a\\\\b");
        assert_eq!(escape_erlang_string("\\\\"), "\\\\\\\\");
    }

    #[test]
    fn test_escape_erlang_string_quotes() {
        assert_eq!(escape_erlang_string("a\"b"), "a\\\"b");
        assert_eq!(escape_erlang_string("\"test\""), "\\\"test\\\"");
    }

    #[test]
    fn test_escape_erlang_string_newlines() {
        assert_eq!(escape_erlang_string("line1\nline2"), "line1\\nline2");
        assert_eq!(escape_erlang_string("\r\n"), "\\r\\n");
    }

    #[test]
    fn test_escape_erlang_string_tabs() {
        assert_eq!(escape_erlang_string("col1\tcol2"), "col1\\tcol2");
    }

    #[test]
    fn test_escape_erlang_string_null_byte() {
        assert_eq!(escape_erlang_string("\0"), "\\0");
    }

    #[test]
    fn test_escape_erlang_string_combined() {
        assert_eq!(
            escape_erlang_string("path\\to\\\"file\"\n"),
            "path\\\\to\\\\\\\"file\\\"\\n"
        );
    }
}
