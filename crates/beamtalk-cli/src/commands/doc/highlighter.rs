// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Syntax highlighting for Beamtalk source code.
//!
//! **DDD Context:** CLI / Documentation

use std::fmt::Write as _;

use super::renderer::html_escape;

/// Syntax-highlight Beamtalk source code.
///
/// Applies HTML span tags for keywords, strings, numbers, comments,
/// selectors, symbols, class names, and `self`.
pub(super) fn highlight_beamtalk(code: &str) -> String {
    let mut result = String::new();
    let chars: Vec<char> = code.chars().collect();
    let len = chars.len();
    let mut i = 0;

    while i < len {
        if let Some(new_i) = hl_comment(&chars, i, &mut result) {
            i = new_i;
        } else if let Some(new_i) = hl_string(&chars, i, &mut result) {
            i = new_i;
        } else if let Some(new_i) = hl_symbol(&chars, i, &mut result) {
            i = new_i;
        } else if let Some(new_i) = hl_number(&chars, i, &mut result) {
            i = new_i;
        } else if let Some(new_i) = hl_word(&chars, i, &mut result) {
            i = new_i;
        } else {
            match chars[i] {
                '&' => result.push_str("&amp;"),
                '<' => result.push_str("&lt;"),
                '>' => result.push_str("&gt;"),
                '"' => result.push_str("&quot;"),
                c => result.push(c),
            }
            i += 1;
        }
    }

    result
}

/// Highlight line or block comments. Returns new index if matched.
fn hl_comment(chars: &[char], i: usize, out: &mut String) -> Option<usize> {
    let len = chars.len();
    if i + 1 >= len || chars[i] != '/' {
        return None;
    }

    let mut j = i;
    if chars[i + 1] == '/' {
        while j < len && chars[j] != '\n' {
            j += 1;
        }
    } else if chars[i + 1] == '*' {
        j += 2;
        while j + 1 < len && !(chars[j] == '*' && chars[j + 1] == '/') {
            j += 1;
        }
        if j + 1 < len && chars[j] == '*' && chars[j + 1] == '/' {
            j += 2;
        } else {
            j = len;
        }
    } else {
        return None;
    }

    let text: String = chars[i..j].iter().collect();
    let _ = write!(
        out,
        "<span class=\"hl-comment\">{}</span>",
        html_escape(&text)
    );
    Some(j)
}

/// Highlight string literals. Returns new index if matched.
fn hl_string(chars: &[char], i: usize, out: &mut String) -> Option<usize> {
    if chars[i] != '\'' && chars[i] != '"' {
        return None;
    }
    let quote = chars[i];
    let len = chars.len();
    let mut j = i + 1;
    while j < len && chars[j] != quote {
        if chars[j] == '\\' {
            j += 1;
            if j >= len {
                break;
            }
        }
        j += 1;
    }
    if j < len {
        j += 1;
    }
    let s: String = chars[i..j].iter().collect();
    let _ = write!(out, "<span class=\"hl-string\">{}</span>", html_escape(&s));
    Some(j)
}

/// Highlight symbol literals (#name). Returns new index if matched.
fn hl_symbol(chars: &[char], i: usize, out: &mut String) -> Option<usize> {
    let len = chars.len();
    if chars[i] != '#' || i + 1 >= len || !(chars[i + 1].is_alphabetic() || chars[i + 1] == '_') {
        return None;
    }
    let mut j = i + 1;
    while j < len && (chars[j].is_alphanumeric() || chars[j] == '_' || chars[j] == ':') {
        j += 1;
    }
    let sym: String = chars[i..j].iter().collect();
    let _ = write!(
        out,
        "<span class=\"hl-symbol\">{}</span>",
        html_escape(&sym)
    );
    Some(j)
}

/// Highlight numeric literals. Returns new index if matched.
fn hl_number(chars: &[char], i: usize, out: &mut String) -> Option<usize> {
    let len = chars.len();
    let is_neg = chars[i] == '-'
        && i + 1 < len
        && chars[i + 1].is_ascii_digit()
        && (i == 0 || !chars[i - 1].is_alphanumeric());

    if !chars[i].is_ascii_digit() && !is_neg {
        return None;
    }

    let mut j = i;
    if chars[j] == '-' {
        j += 1;
    }
    while j < len && (chars[j].is_ascii_digit() || chars[j] == '.') {
        j += 1;
    }
    let num: String = chars[i..j].iter().collect();
    let _ = write!(
        out,
        "<span class=\"hl-number\">{}</span>",
        html_escape(&num)
    );
    Some(j)
}

/// Highlight identifiers, keywords, and class names. Returns new index if matched.
fn hl_word(chars: &[char], i: usize, out: &mut String) -> Option<usize> {
    if !chars[i].is_alphabetic() && chars[i] != '_' {
        return None;
    }
    let len = chars.len();
    let mut j = i;
    while j < len && (chars[j].is_alphanumeric() || chars[j] == '_') {
        j += 1;
    }
    let word: String = chars[i..j].iter().collect();

    match word.as_str() {
        "self" => {
            let _ = write!(out, "<span class=\"hl-self\">{word}</span>");
        }
        "true" | "false" | "nil" | "super" | "sealed" => {
            let _ = write!(
                out,
                "<span class=\"hl-keyword\">{}</span>",
                html_escape(&word)
            );
        }
        "subclass" | "state" | "ifTrue" | "ifFalse" | "ifNil" | "ifNotNil" | "whileTrue"
        | "whileFalse" | "timesRepeat" => {
            let _ = write!(
                out,
                "<span class=\"hl-selector\">{}</span>",
                html_escape(&word)
            );
        }
        _ if word.starts_with(|c: char| c.is_uppercase()) => {
            let _ = write!(
                out,
                "<span class=\"hl-class\">{}</span>",
                html_escape(&word)
            );
        }
        _ => out.push_str(&html_escape(&word)),
    }
    Some(j)
}

#[cfg(test)]
mod tests {
    use super::*;

    // `highlight_beamtalk` is exercised indirectly by `renderer.rs`/`site.rs`
    // (rendering full doc pages), but none of those tests target this
    // module's individual highlighting branches directly, so every arm below
    // gets its own focused case.

    #[test]
    fn highlights_line_comment_to_end_of_line() {
        let out = highlight_beamtalk("// a comment\nfoo");
        assert!(out.contains("<span class=\"hl-comment\">// a comment</span>"));
        assert!(out.ends_with("foo"));
    }

    #[test]
    fn highlights_line_comment_at_end_of_input_with_no_trailing_newline() {
        let out = highlight_beamtalk("// trailing");
        assert_eq!(
            out,
            "<span class=\"hl-comment\">// trailing</span>".to_string()
        );
    }

    #[test]
    fn highlights_closed_block_comment() {
        let out = highlight_beamtalk("/* block */ x");
        assert!(out.starts_with("<span class=\"hl-comment\">/* block */</span>"));
    }

    #[test]
    fn highlights_unterminated_block_comment_to_end_of_input() {
        // No closing `*/`: `hl_comment` must still consume to end-of-input
        // rather than looping or under-consuming.
        let out = highlight_beamtalk("/* never closed");
        assert_eq!(
            out,
            "<span class=\"hl-comment\">/* never closed</span>".to_string()
        );
    }

    #[test]
    fn does_not_treat_lone_slash_as_comment() {
        // A single `/` (binary division selector) must fall through to the
        // default escape branch, not be swallowed by `hl_comment`. Uses
        // letters rather than digits around it so `hl_number` doesn't also
        // wrap the operands, keeping this test focused on `hl_comment`.
        let out = highlight_beamtalk("a / b");
        assert_eq!(out, "a / b");
    }

    #[test]
    fn highlights_single_quoted_string() {
        // `html_escape` does not escape single quotes, so the delimiters
        // pass through verbatim inside the span.
        let out = highlight_beamtalk("'hello'");
        assert_eq!(out, "<span class=\"hl-string\">'hello'</span>".to_string());
    }

    #[test]
    fn highlights_double_quoted_string() {
        // Double quotes ARE escaped by `html_escape`, unlike single quotes.
        let out = highlight_beamtalk("\"hi\"");
        assert_eq!(
            out,
            "<span class=\"hl-string\">&quot;hi&quot;</span>".to_string()
        );
    }

    #[test]
    fn highlights_string_with_escaped_quote() {
        // The escape-skip (`\\` -> skip next char) must not end the string
        // early on the escaped quote.
        let out = highlight_beamtalk(r"'it\'s'");
        assert!(
            out.contains("hl-string"),
            "escaped quote must stay inside the string span: {out}"
        );
    }

    #[test]
    fn highlights_unterminated_string_to_end_of_input() {
        let out = highlight_beamtalk("'never closed");
        assert!(out.starts_with("<span class=\"hl-string\">"));
        assert!(out.contains("never closed"));
    }

    #[test]
    fn highlights_symbol_literal() {
        let out = highlight_beamtalk("#foo:bar:");
        assert_eq!(
            out,
            "<span class=\"hl-symbol\">#foo:bar:</span>".to_string()
        );
    }

    #[test]
    fn does_not_treat_bare_hash_as_symbol() {
        // `#` not followed by an alphabetic char/underscore falls through to
        // the default escape branch. (`:` is chosen over a digit so the
        // following char isn't itself picked up by `hl_number`.)
        let out = highlight_beamtalk("#:");
        assert_eq!(out, "#:");
    }

    #[test]
    fn highlights_underscore_led_symbol() {
        let out = highlight_beamtalk("#_priv");
        assert!(out.starts_with("<span class=\"hl-symbol\">"));
    }

    #[test]
    fn highlights_positive_number() {
        let out = highlight_beamtalk("42");
        assert_eq!(out, "<span class=\"hl-number\">42</span>".to_string());
    }

    #[test]
    fn highlights_decimal_number() {
        let out = highlight_beamtalk("3.14");
        assert_eq!(out, "<span class=\"hl-number\">3.14</span>".to_string());
    }

    #[test]
    fn highlights_negative_number_at_start_of_input() {
        // `i == 0` arm of the `is_neg` check — no preceding char to inspect.
        let out = highlight_beamtalk("-5");
        assert_eq!(out, "<span class=\"hl-number\">-5</span>".to_string());
    }

    #[test]
    fn highlights_negative_number_after_non_alphanumeric() {
        let out = highlight_beamtalk("(-5)");
        assert!(out.contains("<span class=\"hl-number\">-5</span>"));
    }

    #[test]
    fn does_not_treat_minus_after_identifier_as_negative_number() {
        // `x - 5`: the `-` follows an alphanumeric char, so `is_neg` is false
        // and the `-` itself is not swallowed into the number span.
        let out = highlight_beamtalk("x-5");
        assert!(
            !out.contains("hl-number\">-5"),
            "minus after an identifier must not start a negative-number span: {out}"
        );
    }

    #[test]
    fn highlights_self_keyword() {
        let out = highlight_beamtalk("self");
        assert_eq!(out, "<span class=\"hl-self\">self</span>".to_string());
    }

    #[test]
    fn highlights_literal_keywords() {
        for word in ["true", "false", "nil", "super", "sealed"] {
            let out = highlight_beamtalk(word);
            assert_eq!(
                out,
                format!("<span class=\"hl-keyword\">{word}</span>"),
                "keyword: {word}"
            );
        }
    }

    #[test]
    fn highlights_control_flow_selectors() {
        for word in [
            "subclass",
            "state",
            "ifTrue",
            "ifFalse",
            "ifNil",
            "ifNotNil",
            "whileTrue",
            "whileFalse",
            "timesRepeat",
        ] {
            let out = highlight_beamtalk(word);
            assert_eq!(
                out,
                format!("<span class=\"hl-selector\">{word}</span>"),
                "selector: {word}"
            );
        }
    }

    #[test]
    fn highlights_uppercase_leading_identifier_as_class() {
        let out = highlight_beamtalk("Counter");
        assert_eq!(out, "<span class=\"hl-class\">Counter</span>".to_string());
    }

    #[test]
    fn plain_lowercase_identifier_is_not_wrapped() {
        let out = highlight_beamtalk("increment");
        assert_eq!(out, "increment");
    }

    #[test]
    fn identifier_may_contain_digits_and_underscores() {
        let out = highlight_beamtalk("value_1");
        assert_eq!(out, "value_1");
    }

    #[test]
    fn escapes_raw_html_special_characters_outside_any_token() {
        // `"` is deliberately excluded here: it would instead trigger
        // `hl_string`, which is covered separately above.
        let out = highlight_beamtalk("a < b & c > d");
        assert_eq!(out, "a &lt; b &amp; c &gt; d");
    }

    #[test]
    fn empty_input_produces_empty_output() {
        assert_eq!(highlight_beamtalk(""), "");
    }
}
