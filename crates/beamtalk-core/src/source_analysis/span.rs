// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Source location tracking.
//!
//! Every AST node carries a `Span` indicating its position in the source file.
//! This enables accurate error messages and IDE features like go-to-definition.

use std::ops::Range;

/// A span of source code, represented as a byte offset range.
///
/// Spans are used throughout the compiler to track the source location
/// of tokens, AST nodes, and error messages.
///
/// # Examples
///
/// ```
/// use beamtalk_core::source_analysis::Span;
///
/// let span = Span::new(0, 10);
/// assert_eq!(span.start(), 0);
/// assert_eq!(span.end(), 10);
/// assert_eq!(span.len(), 10);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Span {
    start: u32,
    end: u32,
}

impl Span {
    /// Creates a new span from start and end byte offsets.
    #[must_use]
    pub const fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    /// Returns the start byte offset.
    #[must_use]
    pub const fn start(self) -> u32 {
        self.start
    }

    /// Returns the end byte offset (exclusive).
    #[must_use]
    pub const fn end(self) -> u32 {
        self.end
    }

    /// Returns the length of the span in bytes.
    #[must_use]
    pub const fn len(self) -> u32 {
        self.end - self.start
    }

    /// Returns true if the span is empty.
    #[must_use]
    pub const fn is_empty(self) -> bool {
        self.start == self.end
    }

    /// Returns true if `other` is fully contained within `self`.
    #[must_use]
    pub const fn contains(self, other: Self) -> bool {
        self.start <= other.start && other.end <= self.end
    }

    /// Creates a span that covers both `self` and `other`.
    #[must_use]
    pub const fn merge(self, other: Self) -> Self {
        let start = if self.start < other.start {
            self.start
        } else {
            other.start
        };
        let end = if self.end > other.end {
            self.end
        } else {
            other.end
        };
        Self { start, end }
    }

    /// Converts to a `Range<usize>` for indexing into source text.
    #[must_use]
    pub const fn as_range(self) -> Range<usize> {
        self.start as usize..self.end as usize
    }
}

impl From<Range<u32>> for Span {
    fn from(range: Range<u32>) -> Self {
        Self::new(range.start, range.end)
    }
}

impl From<Range<usize>> for Span {
    #[expect(
        clippy::cast_possible_truncation,
        reason = "source files over 4GB are not supported"
    )]
    fn from(range: Range<usize>) -> Self {
        Self::new(range.start as u32, range.end as u32)
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.as_range()
    }
}

impl From<Span> for miette::SourceSpan {
    fn from(span: Span) -> Self {
        (span.start as usize, span.len() as usize).into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn span_new_and_accessors() {
        let span = Span::new(5, 15);
        assert_eq!(span.start(), 5);
        assert_eq!(span.end(), 15);
        assert_eq!(span.len(), 10);
        assert!(!span.is_empty());
    }

    #[test]
    fn span_empty() {
        let span = Span::new(5, 5);
        assert!(span.is_empty());
        assert_eq!(span.len(), 0);
    }

    #[test]
    fn span_merge() {
        let a = Span::new(5, 10);
        let b = Span::new(15, 20);
        let merged = a.merge(b);
        assert_eq!(merged.start(), 5);
        assert_eq!(merged.end(), 20);
    }

    #[test]
    fn span_from_range() {
        let span: Span = (0u32..10u32).into();
        assert_eq!(span.start(), 0);
        assert_eq!(span.end(), 10);

        let span: Span = (0usize..10usize).into();
        assert_eq!(span.start(), 0);
        assert_eq!(span.end(), 10);
    }

    #[test]
    fn span_as_range() {
        let span = Span::new(5, 15);
        let range: Range<usize> = span.into();
        assert_eq!(range, 5..15);
    }
}
