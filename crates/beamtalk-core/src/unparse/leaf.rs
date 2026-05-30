// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Typed leaf constructors for the Beamtalk *source* unparser (ADR 0089).
//!
//! **DDD Context:** Language Service — Formatting / Unparse
//!
//! The unparser in [`super`] turns AST nodes back into Beamtalk source text.
//! Every text-shaped [`Document`] leaf it emits used to be built with the open
//! [`Document::String`] escape hatch, which accepts arbitrary text and carries
//! no intent. ADR 0089 replaces those open leaves with this small parallel API
//! so each call site declares *what kind* of leaf it produces.
//!
//! # Why this is separate from `document::leaf`
//!
//! These helpers produce **Beamtalk syntax**, not Core Erlang. The escaping
//! rules differ entirely:
//!
//! - No atom-quote ceremony (`'...'`) — that's a Core Erlang concept.
//! - No Core Erlang string escaping — Beamtalk string literals use the
//!   doubled-delimiter convention (`"` → `""`) and escape `{` to avoid
//!   triggering interpolation.
//!
//! Sharing the Core Erlang leaf API here would be incorrect, so this module is
//! intentionally a sibling rather than a reuse of `document::leaf`.
//!
//! # The single chokepoint
//!
//! Every helper wraps [`Document::String`] internally. When Phase 3 of ADR 0089
//! removes the `Document::String` variant, this module is the *only* place in
//! the unparser that needs to switch to the replacement owned-string
//! constructor.

use crate::codegen::core_erlang::document::Document;

/// A Beamtalk identifier, operator, keyword part, or other syntactic name.
///
/// Identifiers are emitted verbatim — Beamtalk source identifiers, binary
/// operators (`+`, `++`), keyword selectors (`at:`), type names, field names,
/// and class names need no escaping.
#[must_use]
pub(super) fn ident(name: impl AsRef<str>) -> Document<'static> {
    Document::String(name.as_ref().to_string())
}

/// An integer literal (`42`, `-7`).
#[must_use]
pub(super) fn int_lit(value: i64) -> Document<'static> {
    Document::String(value.to_string())
}

/// A non-negative integer rendered in decimal — e.g. a binary-segment unit size
/// (`:8`). Distinct from [`int_lit`] only in the source type it accepts.
#[must_use]
pub(super) fn nat_lit(value: usize) -> Document<'static> {
    Document::String(value.to_string())
}

/// A float literal, rendered so it always reads back as a float (`2.0`, not `2`).
///
/// Integral floats gain a `.0` suffix; values already containing `.` or `e`
/// are emitted as Rust's default `f64` formatting produces them.
#[must_use]
pub(super) fn float_lit(value: f64) -> Document<'static> {
    let s = format!("{value}");
    let rendered = if s.contains('.') || s.contains('e') {
        s
    } else {
        format!("{value}.0")
    };
    Document::String(rendered)
}

/// A character literal (`$a`, `$\n`).
///
/// Control characters and the backslash are re-escaped; all other characters
/// are emitted directly after the `$` sigil.
#[must_use]
pub(super) fn char_lit(c: char) -> Document<'static> {
    let repr = match c {
        '\n' => "\\n".to_string(),
        '\t' => "\\t".to_string(),
        '\r' => "\\r".to_string(),
        '\\' => "\\\\".to_string(),
        _ => c.to_string(),
    };
    Document::String(format!("${repr}"))
}

/// The *inner* content of a Beamtalk string literal, with delimiters escaped.
///
/// The caller is responsible for emitting the surrounding `"` delimiters; this
/// helper escapes only the content so the same routine works for both plain
/// string literals and the literal segments of an interpolated string.
///
/// Escaping rules (Beamtalk source, not Core Erlang):
///
/// - `"` is doubled (`""`) — the doubled-delimiter convention.
/// - `{` becomes `\{` — otherwise it would start an interpolation.
#[must_use]
pub(super) fn string_content(s: impl AsRef<str>) -> Document<'static> {
    let s = s.as_ref();
    let mut result = String::with_capacity(s.len() + 4);
    for c in s.chars() {
        match c {
            '"' => {
                result.push('"');
                result.push('"');
            }
            '{' => {
                result.push('\\');
                result.push('{');
            }
            _ => result.push(c),
        }
    }
    Document::String(result)
}

/// The *inner* content of a symbol literal (`#name`, `#'with spaces'`).
///
/// The caller emits the `#` sigil and any surrounding quotes; the symbol name
/// itself is emitted verbatim (no escaping).
#[must_use]
pub(super) fn symbol_content(s: impl AsRef<str>) -> Document<'static> {
    Document::String(s.as_ref().to_string())
}

/// A class reference, optionally package-qualified (`Foo` or `pkg@Foo`).
#[must_use]
pub(super) fn class_ref(package: Option<&str>, name: &str) -> Document<'static> {
    match package {
        Some(pkg) => Document::String(format!("{pkg}@{name}")),
        None => Document::String(name.to_string()),
    }
}

/// Raw, unescaped text such as comment bodies, doc-comment lines, and error
/// messages.
///
/// The caller is responsible for any surrounding syntax (e.g. `// `, `/* */`)
/// and for any content sanitisation it requires (such as neutralising `*/`
/// inside a block comment).
#[must_use]
pub(super) fn raw_text(s: impl AsRef<str>) -> Document<'static> {
    Document::String(s.as_ref().to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn render(doc: &Document<'static>) -> String {
        doc.to_pretty_string()
    }

    #[test]
    fn ident_is_verbatim() {
        assert_eq!(render(&ident("fooBar")), "fooBar");
        assert_eq!(render(&ident("++")), "++");
        assert_eq!(render(&ident("at:")), "at:");
        assert_eq!(render(&ident("")), "");
    }

    #[test]
    fn int_lit_renders_decimal() {
        assert_eq!(render(&int_lit(0)), "0");
        assert_eq!(render(&int_lit(42)), "42");
        assert_eq!(render(&int_lit(-7)), "-7");
        assert_eq!(render(&int_lit(i64::MAX)), i64::MAX.to_string());
        assert_eq!(render(&int_lit(i64::MIN)), i64::MIN.to_string());
    }

    #[test]
    fn nat_lit_renders_decimal() {
        assert_eq!(render(&nat_lit(0)), "0");
        assert_eq!(render(&nat_lit(8)), "8");
        assert_eq!(render(&nat_lit(usize::MAX)), usize::MAX.to_string());
    }

    #[test]
    fn float_lit_adds_point_zero_for_integral_values() {
        assert_eq!(render(&float_lit(2.0)), "2.0");
        assert_eq!(render(&float_lit(0.0)), "0.0");
        assert_eq!(render(&float_lit(-5.0)), "-5.0");
    }

    #[test]
    fn float_lit_preserves_fractional_and_exponent_forms() {
        assert_eq!(render(&float_lit(2.5)), "2.5");
        assert_eq!(render(&float_lit(3.15)), "3.15");
        // Very large magnitudes render in exponent form; the `.0` branch must
        // not fire for those.
        let big = render(&float_lit(1e300));
        assert!(big.contains('e') || big.contains('.'), "got {big}");
    }

    #[test]
    fn char_lit_escapes_control_chars() {
        assert_eq!(render(&char_lit('a')), "$a");
        assert_eq!(render(&char_lit('\n')), "$\\n");
        assert_eq!(render(&char_lit('\t')), "$\\t");
        assert_eq!(render(&char_lit('\r')), "$\\r");
        assert_eq!(render(&char_lit('\\')), "$\\\\");
        assert_eq!(render(&char_lit('$')), "$$");
    }

    #[test]
    fn string_content_doubles_quotes() {
        assert_eq!(render(&string_content("plain")), "plain");
        assert_eq!(render(&string_content("say \"hi\"")), "say \"\"hi\"\"");
    }

    #[test]
    fn string_content_escapes_interpolation_brace() {
        assert_eq!(render(&string_content("a{b")), "a\\{b");
        assert_eq!(render(&string_content("{")), "\\{");
    }

    #[test]
    fn string_content_leaves_other_chars_untouched() {
        // Closing brace and backslash are not special to the unparser's
        // string-content escaping.
        assert_eq!(render(&string_content("a}b")), "a}b");
        assert_eq!(render(&string_content("a\\b")), "a\\b");
    }

    #[test]
    fn symbol_content_is_verbatim() {
        assert_eq!(render(&symbol_content("foo")), "foo");
        assert_eq!(render(&symbol_content("with spaces")), "with spaces");
    }

    #[test]
    fn class_ref_qualifies_with_package() {
        assert_eq!(render(&class_ref(None, "Foo")), "Foo");
        assert_eq!(render(&class_ref(Some("pkg"), "Foo")), "pkg@Foo");
    }

    #[test]
    fn text_helpers_accept_any_str_like_source() {
        // The unparser passes `&str`, `&String`, and `&EcoString` AST fields;
        // `AsRef<str>` must accept all of them.
        let owned = String::from("x");
        let eco = ecow::EcoString::from("y");
        assert_eq!(render(&ident("x")), "x");
        assert_eq!(render(&ident(&owned)), "x");
        assert_eq!(render(&ident(&eco)), "y");
    }

    #[test]
    fn raw_text_is_verbatim() {
        assert_eq!(render(&raw_text("a comment")), "a comment");
        assert_eq!(
            render(&raw_text("has \" and { chars")),
            "has \" and { chars"
        );
    }
}
