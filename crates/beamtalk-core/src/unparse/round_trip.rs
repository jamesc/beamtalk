// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The formatter's round-trip safety net (BT-5183).
//!
//! `beamtalk fmt` overwrites a file in place, so a printer bug that drops a
//! comment or changes a literal is silent data loss. [`verify_round_trip`]
//! is the one check that stands between the printer and the disk: it
//! re-parses the formatted text and refuses the result unless
//!
//! 1. the formatted text parses without an error diagnostic,
//! 2. the multiset of comments (kind + text) is unchanged,
//! 3. every number literal keeps its source spelling (`16rFF`, `1e10`), and
//! 4. the program is unchanged: the two ASTs agree once source positions and
//!    comment placement are erased ([`ast_fingerprint`]).
//!
//! The comment and spelling checks read straight from the lexer's tokens
//! rather than from the AST, so a comment the parser never attached anywhere
//! (the original BT-5183 failure mode), or a number the AST holds only as a
//! value, is still counted.

use std::fmt::Write as _;

use crate::ast::Module;
use crate::source_analysis::{Severity, TokenKind, Trivia, lex_with_eof, parse};

/// Why [`super::format_source`] refused to produce output.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FormatError {
    /// The input has a parse error. Formatting a broken file could corrupt
    /// it, so the formatter does not try.
    ParseErrors,
    /// The formatted text does not parse cleanly. This is a formatter bug.
    OutputDoesNotParse,
    /// The formatted text lost, gained or changed comments. This is a
    /// formatter bug.
    CommentsChanged {
        /// Comments in the input that are missing from the output.
        missing: Vec<String>,
        /// Comments in the output that are not in the input.
        added: Vec<String>,
    },
    /// The formatted text respells a number literal (`16rFF` as `255`). This
    /// is a formatter bug.
    NumberSpellingChanged {
        /// Spellings in the input that are missing from the output.
        missing: Vec<String>,
    },
    /// The formatted text parses to a different program. This is a formatter
    /// bug.
    AstChanged {
        /// The first line where the two AST fingerprints differ, for the bug
        /// report.
        first_difference: String,
    },
}

impl std::fmt::Display for FormatError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ParseErrors => write!(f, "the source has parse errors"),
            Self::OutputDoesNotParse => write!(
                f,
                "formatter bug: the formatted output does not parse; the file was left unchanged"
            ),
            Self::CommentsChanged { missing, added } => {
                write!(
                    f,
                    "formatter bug: formatting would change the file's comments; the file was left unchanged"
                )?;
                if let Some(first) = missing.first() {
                    write!(f, " (lost: {first}")?;
                    if missing.len() > 1 {
                        write!(f, " and {} more", missing.len() - 1)?;
                    }
                    write!(f, ")")?;
                }
                if let Some(first) = added.first() {
                    write!(f, " (added: {first})")?;
                }
                Ok(())
            }
            Self::NumberSpellingChanged { missing } => write!(
                f,
                "formatter bug: formatting would respell number literals ({}); the file was left unchanged",
                missing.join(", ")
            ),
            Self::AstChanged { first_difference } => write!(
                f,
                "formatter bug: formatting would change the program; the file was left unchanged (first difference: {first_difference})"
            ),
        }
    }
}

impl std::error::Error for FormatError {}

/// Checks that `formatted` is a faithful rewrite of `source`, whose parse is
/// `module`. See the module docs for what "faithful" means.
pub(super) fn verify_round_trip(
    source: &str,
    module: &Module,
    formatted: &str,
) -> Result<(), FormatError> {
    let (reparsed, diags) = parse(lex_with_eof(formatted));
    if diags.iter().any(|d| d.severity == Severity::Error) {
        return Err(FormatError::OutputDoesNotParse);
    }

    let before = comment_multiset(source);
    let after = comment_multiset(formatted);
    if before != after {
        return Err(FormatError::CommentsChanged {
            missing: multiset_difference(&before, &after),
            added: multiset_difference(&after, &before),
        });
    }

    let before = number_spellings(source);
    let after = number_spellings(formatted);
    if before != after {
        return Err(FormatError::NumberSpellingChanged {
            missing: multiset_difference(&before, &after),
        });
    }

    let before = ast_fingerprint(module);
    let after = ast_fingerprint(&reparsed);
    if before != after {
        let first_difference = first_difference(&before, &after);
        return Err(FormatError::AstChanged { first_difference });
    }
    Ok(())
}

/// A short window of `before` and `after` around their first difference.
fn first_difference(before: &str, after: &str) -> String {
    let at = before
        .bytes()
        .zip(after.bytes())
        .position(|(a, b)| a != b)
        .unwrap_or_else(|| before.len().min(after.len()));
    let window = |s: &str| {
        let mut start = at.saturating_sub(40);
        while !s.is_char_boundary(start) {
            start -= 1;
        }
        let mut end = (at + 40).min(s.len());
        while !s.is_char_boundary(end) {
            end += 1;
        }
        s[start..end].to_string()
    };
    let mut out = String::new();
    let _ = write!(out, "`…{}…` became `…{}…`", window(before), window(after));
    out
}

/// Every comment in `source`, as its kind marker plus its trimmed text, in
/// sorted order. Read from the lexer's trivia so that a comment no AST node
/// holds still counts.
///
/// Comment text is compared trimmed because the printer normalises the space
/// after `//` and around a block comment's body; the words themselves must
/// survive. Line endings are stripped for the same reason (a CRLF file's
/// comments carry a trailing `\r` in the trivia).
pub(crate) fn comment_multiset(source: &str) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    for token in lex_with_eof(source) {
        for trivia in token.leading_trivia().iter().chain(token.trailing_trivia()) {
            let (marker, text) = match trivia {
                Trivia::Whitespace(_) => continue,
                Trivia::LineComment(s) => ("//", s.as_str()),
                Trivia::BlockComment(s) => ("/*", s.as_str()),
                Trivia::DocComment(s) => ("///", s.as_str()),
            };
            out.push(normalise_comment(marker, text));
        }
    }
    out.sort();
    out
}

/// The spelling of every number literal token in `source`, without a
/// leading `-`, in sorted order.
///
/// The sign is dropped because the printer may legitimately move it between
/// tokens (a pattern's `- 5` prints as `-5`); the value, sign included, is
/// checked by the AST comparison.
pub(crate) fn number_spellings(source: &str) -> Vec<String> {
    let mut out: Vec<String> = lex_with_eof(source)
        .into_iter()
        .filter_map(|token| match token.kind() {
            TokenKind::Integer(s) | TokenKind::Float(s) => {
                Some(s.strip_prefix('-').unwrap_or(s).to_string())
            }
            _ => None,
        })
        .collect();
    out.sort();
    out
}

/// `marker` followed by the comment body with its delimiters and surrounding
/// whitespace removed.
fn normalise_comment(marker: &str, text: &str) -> String {
    let body = match marker {
        "///" => text.strip_prefix("///").unwrap_or(text),
        "//" => text.strip_prefix("//").unwrap_or(text),
        _ => {
            let t = text.strip_prefix("/*").unwrap_or(text);
            t.strip_suffix("*/").unwrap_or(t)
        }
    };
    let mut out = String::from(marker);
    out.push(' ');
    // Inner lines of a block comment are compared line by line, trimmed, so
    // that re-indenting a multi-line block comment is not a change.
    let mut first = true;
    for line in body.trim().lines() {
        if !first {
            out.push('\n');
        }
        first = false;
        out.push_str(line.trim());
    }
    out
}

/// The elements of `a` not matched by an element of `b` (both sorted).
fn multiset_difference(a: &[String], b: &[String]) -> Vec<String> {
    let mut out = Vec::new();
    let mut j = 0;
    for item in a {
        while j < b.len() && b[j] < *item {
            j += 1;
        }
        if j < b.len() && b[j] == *item {
            j += 1;
        } else {
            out.push(item.clone());
        }
    }
    out
}

/// Fields of the AST's `Debug` form that hold comments or comment layout.
/// [`ast_fingerprint`] drops them; comments are checked separately, by
/// [`comment_multiset`], because the printer may legitimately move a comment
/// between attachment points (a statement's trailing comment becomes the
/// next statement's leading one).
const COMMENT_FIELDS: &[&str] = &[
    "comments:",
    "doc_comment:",
    "file_leading_comments:",
    "file_trailing_comments:",
    "literal_comments:",
    "preceding_blank_line:",
];

/// A rendering of `module` that is equal for two modules exactly when they
/// are the same program: [`debug_without_layout`] with the comment fields
/// dropped too.
pub(crate) fn ast_fingerprint(module: &Module) -> String {
    debug_without_layout(module, COMMENT_FIELDS)
}

/// `value`'s `Debug` form with every span erased and every field named in
/// `dropped_fields` (each written `name:`) removed, so two trees that differ
/// only in source positions (and in those fields) render alike.
///
/// One left-to-right scan over the compact `{:?}` form: string and char
/// literals are copied through untouched, `Span { start: .., end: .. }`
/// becomes `Span` whatever field holds it (`span`, `key_span`, a tuple
/// position), and a dropped field is removed with its whole value by
/// bracket matching. This is the one span-eraser:
/// `test_helpers::test_support::without_spans` delegates here.
pub(crate) fn debug_without_layout(
    value: &impl std::fmt::Debug,
    dropped_fields: &[&str],
) -> String {
    const SPAN: &str = "Span { start: ";
    let debug = format!("{value:?}");
    let mut out = String::with_capacity(debug.len());
    let mut rest = debug.as_str();
    while let Some(c) = rest.chars().next() {
        if c == '"' || c == '\'' {
            let len = quoted_len(rest, c);
            out.push_str(&rest[..len]);
            rest = &rest[len..];
            continue;
        }
        if rest.starts_with(SPAN) {
            out.push_str("Span");
            let close = rest.find('}').map_or(rest.len(), |i| i + 1);
            rest = &rest[close..];
            continue;
        }
        let at_field_start = out.ends_with("{ ") || out.ends_with(", ");
        if at_field_start
            && let Some(field) = dropped_fields.iter().find(|f| {
                rest.strip_prefix(**f)
                    .is_some_and(|after| after.starts_with(' '))
            })
        {
            let value_start = field.len() + 1;
            let value_len = value_len(&rest[value_start..]);
            rest = &rest[value_start + value_len..];
            if let Some(after) = rest.strip_prefix(", ") {
                // More fields follow: the `{ ` / `, ` already written leads
                // into the next one.
                rest = after;
            } else {
                // The dropped field was the last: take back its separator.
                out.truncate(out.len() - 2);
            }
            continue;
        }
        out.push(c);
        rest = &rest[c.len_utf8()..];
    }
    out
}

/// The byte length of the `Debug` string or char literal at the start of
/// `s`, opened by `quote`, including both quotes.
fn quoted_len(s: &str, quote: char) -> usize {
    let mut escaped = false;
    for (i, c) in s.char_indices().skip(1) {
        if escaped {
            escaped = false;
        } else if c == '\\' {
            escaped = true;
        } else if c == quote {
            return i + 1;
        }
    }
    s.len()
}

/// The byte length of the `Debug` value at the start of `s`: up to the first
/// `,` or closing bracket at nesting depth zero.
fn value_len(s: &str) -> usize {
    let mut depth = 0usize;
    let mut i = 0;
    while let Some(c) = s[i..].chars().next() {
        match c {
            '"' | '\'' => {
                i += quoted_len(&s[i..], c);
                continue;
            }
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' | ',' if depth == 0 => return i,
            ')' | ']' | '}' => depth -= 1,
            _ => {}
        }
        i += c.len_utf8();
    }
    s.len()
}
