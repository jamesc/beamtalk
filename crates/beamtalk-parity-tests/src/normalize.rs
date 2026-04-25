// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Output normalization helpers used by every surface driver.
//!
//! Different surfaces format the same logical value slightly differently — the
//! REPL strips quotes from string values, MCP tool results wrap the payload in
//! a JSON content array, and CLI commands print human-readable text. This
//! module collapses those superficial differences so a parity assertion can
//! compare the underlying value.

use std::sync::OnceLock;

/// Normalize a surface's textual output for a value comparison.
///
/// * Trims surrounding whitespace.
/// * Strips a single pair of matching ASCII quotes.
/// * Replaces every BEAM-style pid (`<0.123.0>`) with `<pid>`.
/// * Collapses runs of whitespace to a single space.
pub fn value(text: &str) -> String {
    let trimmed = text.trim();
    let unquoted = strip_outer_quotes(trimmed);
    let pid_replaced = replace_pids(&unquoted);
    collapse_whitespace(&pid_replaced)
}

fn strip_outer_quotes(s: &str) -> String {
    let bytes = s.as_bytes();
    if bytes.len() >= 2 {
        let first = bytes[0];
        let last = bytes[bytes.len() - 1];
        if (first == b'"' && last == b'"') || (first == b'\'' && last == b'\'') {
            // Safety: ASCII byte slice; safe to take inner string.
            return s[1..s.len() - 1].to_string();
        }
    }
    s.to_string()
}

fn replace_pids(s: &str) -> String {
    // Hand-rolled to avoid pulling in `regex`.
    let mut out = String::with_capacity(s.len());
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'<' {
            if let Some(end) = find_pid_end(&bytes[i..]) {
                out.push_str("<pid>");
                i += end;
                continue;
            }
        }
        // Push UTF-8 char starting at i.
        let ch = s[i..].chars().next().expect("valid utf-8");
        out.push(ch);
        i += ch.len_utf8();
    }
    out
}

/// If `slice` starts with `<digits.digits.digits>` return the byte length of
/// that prefix, otherwise None. Used for normalising Erlang pid renderings.
fn find_pid_end(slice: &[u8]) -> Option<usize> {
    if slice.first()? != &b'<' {
        return None;
    }
    let mut idx = 1;
    let mut dots = 0;
    let mut digits_in_segment = 0;
    while idx < slice.len() {
        let c = slice[idx];
        if c.is_ascii_digit() {
            digits_in_segment += 1;
            idx += 1;
            continue;
        }
        if c == b'.' {
            if digits_in_segment == 0 {
                return None;
            }
            dots += 1;
            digits_in_segment = 0;
            idx += 1;
            continue;
        }
        if c == b'>' && dots == 2 && digits_in_segment > 0 {
            return Some(idx + 1);
        }
        return None;
    }
    None
}

fn collapse_whitespace(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut last_space = false;
    for ch in s.chars() {
        if ch.is_whitespace() {
            if !last_space {
                out.push(' ');
            }
            last_space = true;
        } else {
            out.push(ch);
            last_space = false;
        }
    }
    out.trim().to_string()
}

/// Cached project root (the directory containing `Cargo.toml` workspace
/// manifest). Used by drivers when resolving binary paths.
pub fn project_root() -> &'static std::path::Path {
    static ROOT: OnceLock<std::path::PathBuf> = OnceLock::new();
    ROOT.get_or_init(|| {
        // CARGO_MANIFEST_DIR for this crate is `crates/beamtalk-parity-tests`.
        let manifest =
            std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set during test");
        std::path::PathBuf::from(manifest)
            .parent()
            .and_then(|p| p.parent())
            .expect("workspace root has at least two ancestors")
            .to_path_buf()
    })
    .as_path()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn value_trims_and_unquotes() {
        assert_eq!(value("  \"hello\"  "), "hello");
        assert_eq!(value("'7'"), "7");
        assert_eq!(value("42\n"), "42");
    }

    #[test]
    fn value_replaces_pids() {
        assert_eq!(value("<0.123.0> ready"), "<pid> ready");
        assert_eq!(value("two: <0.1.0> and <0.2.0>"), "two: <pid> and <pid>");
    }

    #[test]
    fn value_keeps_angle_text_that_is_not_a_pid() {
        assert_eq!(value("<not a pid>"), "<not a pid>");
        assert_eq!(value("<0.x.0>"), "<0.x.0>");
    }

    #[test]
    fn value_collapses_internal_whitespace() {
        assert_eq!(value("a   b\tc"), "a b c");
    }
}
