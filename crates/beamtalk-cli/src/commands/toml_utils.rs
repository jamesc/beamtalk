// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared TOML-formatting utilities used by multiple CLI commands.

use std::fmt::Write as _;

/// Escape a value for embedding in a TOML basic string.
///
/// TOML basic strings prohibit unescaped control characters (U+0000–U+001F,
/// U+007F) as well as unescaped backslashes and double-quotes. Leaving any
/// of those characters bare either produces invalid TOML or, in the worst
/// case, injects extra key-value pairs into the surrounding document.
pub(crate) fn escape_toml_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 || c == '\x7f' => {
                let _ = write!(out, "\\u{:04X}", c as u32);
            }
            c => out.push(c),
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_escape_toml_string_escapes_quotes_and_backslashes() {
        assert_eq!(escape_toml_string(r#"say "hi""#), r#"say \"hi\""#);
        assert_eq!(escape_toml_string(r"a\b"), r"a\\b");
    }

    /// Covers the full TOML-prohibited control-character range (U+0000–U+001F,
    /// U+007F), not just the named short escapes.
    #[test]
    fn test_escape_toml_string_escapes_full_control_character_range() {
        assert_eq!(escape_toml_string("a\nb"), r"a\nb");
        assert_eq!(escape_toml_string("a\rb"), r"a\rb");
        assert_eq!(escape_toml_string("a\tb"), r"a\tb");
        // NUL, VT (U+000B, no short escape), a mid-range control (U+001F),
        // and DEL (U+007F) — all fall back to the `\uXXXX` form.
        assert_eq!(
            escape_toml_string("a\u{0}\u{b}\u{1f}\u{7f}b"),
            "a\\u0000\\u000B\\u001F\\u007Fb"
        );
    }
}
