// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Source-location translation for runtime-delegated navigation (BT-2239).
//!
//! **DDD Context:** Language Service
//!
//! When the LSP delegates a navigation query to a running runtime (the
//! static-first, live-augmented model of ADR 0024), `SystemNavigation`
//! answers with sites shaped as `{class, selector, line}`, where `line` is
//! 1-based **relative to the containing method's source** — the runtime does
//! not store absolute file lines. This module turns such a site into a
//! [`Location`] (file + span) that the LSP can render as `{uri, range}`.
//!
//! The relative line is anchored at the method's *header* line, because the
//! runtime's `method_source` is produced by the unparser starting at the
//! method signature (see `extract_method_source` in codegen). One known
//! imprecision: the unparser includes leading doc comments that sit *above*
//! `method.span.start()`, so a site that lands inside a leading comment can be
//! off by the number of comment lines. Call sites and references — the common
//! case — live in the body and resolve correctly. Per-method parity tests in
//! the BT-2215 children pin this down further.

use crate::source_analysis::Span;

use super::value_objects::{ByteOffset, Position};

/// Compute the [`Span`] of a method-relative line within a file.
///
/// `method_span` is the span of the method *definition* (its `start()` is the
/// header line). `relative_line` is 1-based, relative to the method's source.
/// The returned span covers the full resolved line (from its first byte to the
/// end of the line, excluding the trailing newline).
///
/// Returns `None` if `method_span.start()` is out of bounds for `source` or the
/// resolved line lies past the end of `source`.
#[must_use]
pub fn method_relative_line_span(
    source: &str,
    method_span: Span,
    relative_line: u32,
) -> Option<Span> {
    let header_line =
        Position::from_byte_offset(source, ByteOffset::new(method_span.start()))?.line;
    let target_line = header_line + relative_line.saturating_sub(1);
    line_span(source, target_line)
}

/// Return the byte span covering the given 0-based line, excluding the trailing
/// newline. Returns `None` if the line index is beyond the last line.
fn line_span(source: &str, line: u32) -> Option<Span> {
    let mut current_line: u32 = 0;
    let mut line_start: usize = 0;

    for (i, ch) in source.char_indices() {
        if ch == '\n' {
            if current_line == line {
                return Some(Span::new(to_u32(line_start), to_u32(i)));
            }
            current_line += 1;
            line_start = i + 1;
        }
    }

    // Last line (no trailing newline).
    if current_line == line {
        return Some(Span::new(to_u32(line_start), to_u32(source.len())));
    }
    None
}

#[expect(
    clippy::cast_possible_truncation,
    reason = "source files over 4GB are not supported"
)]
const fn to_u32(value: usize) -> u32 {
    value as u32
}

#[cfg(test)]
mod tests {
    use super::*;

    // Method header on line 2 (0-based line 1). Body sends on lines 3-4.
    const SRC: &str = "\
class
  increment =>
    self bump
    self log
other";

    fn span_at(line_one_based: u32) -> (u32, u32) {
        // method_span starts at the header `increment =>` which begins at the
        // 'i' after the two-space indent on line index 1.
        let header_offset = u32::try_from(SRC.find("increment").unwrap()).unwrap();
        let method_span = Span::new(header_offset, header_offset + 5);
        let span = method_relative_line_span(SRC, method_span, line_one_based).unwrap();
        (span.start(), span.end())
    }

    #[test]
    fn relative_line_one_is_method_header() {
        // Relative line 1 → the header line itself.
        let (start, end) = span_at(1);
        let text = &SRC[start as usize..end as usize];
        assert_eq!(text, "  increment =>");
    }

    #[test]
    fn relative_line_two_is_first_body_line() {
        let (start, end) = span_at(2);
        let text = &SRC[start as usize..end as usize];
        assert_eq!(text, "    self bump");
    }

    #[test]
    fn relative_line_three_is_second_body_line() {
        let (start, end) = span_at(3);
        let text = &SRC[start as usize..end as usize];
        assert_eq!(text, "    self log");
    }

    #[test]
    fn out_of_range_relative_line_returns_none() {
        let header_offset = u32::try_from(SRC.find("increment").unwrap()).unwrap();
        let method_span = Span::new(header_offset, header_offset + 5);
        // Header is on line index 1; +99 overshoots the 5-line source.
        assert!(method_relative_line_span(SRC, method_span, 99).is_none());
    }

    #[test]
    fn last_line_without_trailing_newline() {
        let src = "a =>\n  b";
        let method_span = Span::new(0, 1);
        let span = method_relative_line_span(src, method_span, 2).unwrap();
        assert_eq!(&src[span.start() as usize..span.end() as usize], "  b");
    }
}
