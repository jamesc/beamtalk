// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Typed Core Erlang leaf constructors (ADR 0089 Phase 1).
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Historically the open `Document::String(arbitrary_string)` leaf was the
//! BT-875 recurrence vector: any author could stuff an unquoted atom, a
//! misformatted variable name, or a hand-rendered `format!()` fragment into a
//! leaf and the compiler accepted it. This module replaces that open leaf with
//! a small set of *typed* leaf constructors — "what kind of leaf is this?" —
//! that do the Core Erlang punctuation and escaping for you.
//!
//! Each helper returns a `Document<'static>` directly: typed leaves live inside
//! the existing [`Document`] pretty-printer rather than building a separate
//! Core Erlang AST. After ADR 0089 Phase 3 (the flag-day removal), the public
//! `Document::String` / `Document::Eco` variants no longer exist; these helpers
//! are the only sanctioned way to introduce a runtime-derived leaf, wrapping
//! the crate-internal [`Document::Owned`] backing variant.
//!
//! # Helper map
//!
//! | Helper           | Renders                    | Replaces call sites that build…           |
//! |------------------|----------------------------|-------------------------------------------|
//! | [`atom`]         | `'escaped_name'`           | `docvec!["'", Document::String(a), "'"]`  |
//! | [`var`]          | `VarName`                  | `Document::String(var_name)`              |
//! | [`string_lit`]   | `"escaped string"`         | `docvec!["\"", Document::String(e), "\""]`|
//! | [`int_lit`]      | `42`                       | `Document::String(n.to_string())`         |
//! | [`float_lit`]    | `3.14` / `5.0`             | the `Literal::Float` arm of codegen       |
//! | [`fname`]        | `'escaped_name'/arity`     | `docvec!["'", Document::String(f), "'/", …]` |
//! | [`binary_lit`]   | `#{#<…>(…), …}#`            | `Document::String(Self::binary_string_literal(s))` |

use super::Document;
use crate::codegen::core_erlang::CoreErlangGenerator;
use crate::codegen::core_erlang::util::{escape_atom_chars, escape_core_erlang_string};
use crate::docvec;

/// `'escaped_name'` — a quoted Core Erlang atom.
///
/// The atom name is escaped via
/// [`escape_atom_chars`]
/// (apostrophes and backslashes), then wrapped in single quotes. The
/// atom-quote punctuation is the entire BT-875 recurrence vector this helper
/// closes.
///
/// Replaces hand-rolled `docvec!["'", Document::String(name), "'"]` call sites
/// — symbol literals, class/selector atoms, `maps:put` keys, logger metadata
/// keys, and module/function atoms throughout codegen.
///
/// ```text
/// atom("foo")  => 'foo'
/// atom("it's") => 'it\'s'
/// ```
#[must_use]
pub fn atom(name: impl Into<String>) -> Document<'static> {
    let escaped = escape_atom_chars(&name.into());
    docvec!["'", Document::Owned(escaped), "'"]
}

/// `VarName` — a bare Core Erlang variable name.
///
/// No escaping is applied: Core Erlang variable names are unquoted identifiers
/// that codegen already produces in valid form (capitalised, mangled). The
/// typed `var()` helper makes the intent explicit so authors no longer reach
/// for `Document::String(...)` directly for variables.
///
/// Replaces `Document::String(var_name)` call sites — `let`-bound temporaries,
/// threaded state variables, and function parameters.
///
/// ```text
/// var("State")   => State
/// var("Packed1") => Packed1
/// ```
#[must_use]
pub fn var(name: impl Into<String>) -> Document<'static> {
    Document::Owned(name.into())
}

/// `"escaped string"` — a Core Erlang double-quoted string literal.
///
/// The contents are escaped via `util::escape_core_erlang_string`
/// (backslashes and double quotes), then wrapped in double quotes.
///
/// Replaces hand-rolled `docvec!["\"", Document::String(escaped), "\""]` call
/// sites — `'file'` module attributes and other quoted-string fragments. Note
/// this is the Core Erlang *char-list* string syntax; for Beamtalk `String`
/// values (binaries) use [`binary_lit`].
///
/// ```text
/// string_lit("hello")       => "hello"
/// string_lit("hello\"world") => "hello\"world"
/// ```
#[must_use]
pub fn string_lit(s: impl AsRef<str>) -> Document<'static> {
    let escaped = escape_core_erlang_string(s.as_ref());
    docvec!["\"", Document::Owned(escaped), "\""]
}

/// `42` — a Core Erlang integer literal.
///
/// Replaces `Document::String(n.to_string())` call sites — the
/// `Literal::Integer` and `Literal::Character` arms of expression codegen and
/// any inline integer (arities are handled by [`fname`]).
///
/// ```text
/// int_lit(42)  => 42
/// int_lit(-7)  => -7
/// ```
#[must_use]
pub fn int_lit(i: i64) -> Document<'static> {
    Document::Owned(i.to_string())
}

/// `3.14` / `5.0` — a Core Erlang float literal.
///
/// Always renders a decimal point: Core Erlang interprets a bare `5` as an
/// integer, so a value like `5.0` whose `to_string()` is `"5"` is rendered as
/// `5.0`. Mirrors the `Literal::Float` arm of expression codegen.
///
/// Replaces the inline float-formatting logic in expression codegen.
///
/// ```text
/// float_lit(2.5) => 2.5
/// float_lit(5.0) => 5.0
/// ```
#[must_use]
pub fn float_lit(f: f64) -> Document<'static> {
    let s = f.to_string();
    // Ensure Core Erlang float literals always contain a decimal point,
    // otherwise Erlang interprets them as integers (e.g. 5.0 -> "5" -> int 5).
    if s.contains('.') || s.contains('e') || s.contains('E') {
        Document::Owned(s)
    } else {
        docvec![Document::Owned(s), ".0"]
    }
}

/// `'escaped_name'/arity` — a Core Erlang function name (atom plus arity).
///
/// The function name is escaped exactly like [`atom`] and suffixed with
/// `/arity`. Replaces hand-rolled `docvec!["'", Document::String(name),
/// "'/", Document::String(arity.to_string())]` call sites — function
/// definitions, exports, and `fun` references.
///
/// ```text
/// fname("foo", 2)  => 'foo'/2
/// fname("init", 1) => 'init'/1
/// ```
#[must_use]
pub fn fname(name: impl Into<String>, arity: usize) -> Document<'static> {
    let escaped = escape_atom_chars(&name.into());
    docvec![
        "'",
        Document::Owned(escaped),
        "'/",
        Document::Owned(arity.to_string())
    ]
}

/// `#{#<byte>(8,1,'integer',['unsigned'|['big']]), …}#` — a Core Erlang binary
/// literal holding the UTF-8 bytes of `s`.
///
/// This is the wire form for Beamtalk `String` values. The bytes are rendered
/// by the canonical `CoreErlangGenerator::binary_string_literal` builder, so
/// the output is byte-for-byte identical to the call sites it replaces.
///
/// Replaces `Document::String(Self::binary_string_literal(s))` call sites —
/// string-literal codegen, error hints, and REPL source binaries.
///
/// ```text
/// binary_lit("hi") => #{#<104>(8,1,'integer',['unsigned'|['big']]),#<105>(...)}#
/// ```
#[must_use]
pub fn binary_lit(s: impl AsRef<str>) -> Document<'static> {
    Document::Owned(CoreErlangGenerator::binary_string_literal(s.as_ref()))
}

/// `#<104>(8,1,'integer',['unsigned'|['big']]),#<105>(…)` — the *unwrapped*
/// inner byte segments of a Core Erlang binary literal, with **no** surrounding
/// `#{…}#`.
///
/// Used by string-interpolation codegen, where literal segments are spliced
/// between interpolated expression segments inside a single enclosing `#{…}#`.
/// [`binary_lit`] would add its own wrapper and corrupt that construction, so
/// this helper is the typed-leaf entry point for the unwrapped form. The bytes
/// are rendered by the canonical
/// `CoreErlangGenerator::binary_byte_segments` builder, so the output is
/// byte-for-byte identical to the call site it replaces.
///
/// ```text
/// binary_segments("hi") => #<104>(8,1,'integer',['unsigned'|['big']]),#<105>(...)
/// ```
#[must_use]
pub fn binary_segments(s: impl AsRef<str>) -> Document<'static> {
    Document::Owned(CoreErlangGenerator::binary_byte_segments(s.as_ref()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn atom_trivial() {
        assert_eq!(atom("foo").to_pretty_string(), "'foo'");
    }

    #[test]
    fn atom_escapes_apostrophe() {
        // 'it\'s' — the apostrophe is backslash-escaped inside the quotes.
        assert_eq!(atom("it's").to_pretty_string(), "'it\\'s'");
    }

    #[test]
    fn atom_escapes_backslash() {
        // 'a\\b' — a literal backslash is doubled inside the quotes.
        assert_eq!(atom("a\\b").to_pretty_string(), "'a\\\\b'");
    }

    #[test]
    fn var_is_bare() {
        assert_eq!(var("State").to_pretty_string(), "State");
        assert_eq!(var("Packed1").to_pretty_string(), "Packed1");
    }

    #[test]
    fn string_lit_trivial() {
        assert_eq!(string_lit("hello").to_pretty_string(), "\"hello\"");
    }

    #[test]
    fn string_lit_escapes_double_quote() {
        // "hello\"world" — the embedded double quote is backslash-escaped.
        assert_eq!(
            string_lit("hello\"world").to_pretty_string(),
            "\"hello\\\"world\""
        );
    }

    #[test]
    fn string_lit_escapes_backslash() {
        assert_eq!(string_lit("a\\b").to_pretty_string(), "\"a\\\\b\"");
    }

    #[test]
    fn int_lit_renders() {
        assert_eq!(int_lit(42).to_pretty_string(), "42");
        assert_eq!(int_lit(-7).to_pretty_string(), "-7");
        assert_eq!(int_lit(0).to_pretty_string(), "0");
    }

    #[test]
    fn float_lit_with_decimal() {
        assert_eq!(float_lit(2.5).to_pretty_string(), "2.5");
        assert_eq!(float_lit(0.125).to_pretty_string(), "0.125");
    }

    #[test]
    fn float_lit_whole_number_gets_decimal_point() {
        // 5.0.to_string() is "5"; Core Erlang needs the decimal point.
        assert_eq!(float_lit(5.0).to_pretty_string(), "5.0");
    }

    #[test]
    fn fname_renders_name_and_arity() {
        assert_eq!(fname("foo", 2).to_pretty_string(), "'foo'/2");
        assert_eq!(fname("init", 1).to_pretty_string(), "'init'/1");
    }

    #[test]
    fn fname_escapes_name() {
        assert_eq!(fname("it's", 0).to_pretty_string(), "'it\\'s'/0");
    }

    #[test]
    fn binary_lit_matches_canonical_builder() {
        // Byte-equivalent to the existing binary_string_literal builder.
        let expected = CoreErlangGenerator::binary_string_literal("hi");
        assert_eq!(binary_lit("hi").to_pretty_string(), expected);
    }

    #[test]
    fn binary_lit_empty_string() {
        let expected = CoreErlangGenerator::binary_string_literal("");
        assert_eq!(binary_lit("").to_pretty_string(), expected);
    }
}
