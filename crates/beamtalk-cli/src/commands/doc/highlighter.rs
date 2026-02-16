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
