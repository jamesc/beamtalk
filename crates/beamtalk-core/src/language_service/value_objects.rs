// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Value objects for the language service.
//!
//! **DDD Context:** Language Service
//!
//! Value objects are immutable types defined by their attributes.
//! These types represent domain concepts in the language service context:
//!
//! - **`ByteOffset`** - A position in source text as a byte offset
//! - **`Position`** - A line/column position in source text
//! - **`Location`** - A file path and span combination
//! - **`Completion`** - A code completion suggestion
//! - **`HoverInfo`** - Information to display on hover
//! - **`Diagnostic`** - An error or warning message

use crate::source_analysis::{Diagnostic as ParseDiagnostic, Span};
use camino::Utf8PathBuf;
use ecow::EcoString;

/// A byte offset in a source file (0-indexed).
///
/// This newtype provides type safety to prevent mixing positions and offsets.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ByteOffset(pub u32);

impl ByteOffset {
    /// Creates a new byte offset.
    #[must_use]
    pub const fn new(offset: u32) -> Self {
        Self(offset)
    }

    /// Returns the raw byte offset value.
    #[must_use]
    pub const fn get(self) -> u32 {
        self.0
    }
}

/// A position in a source file (line and column, both 0-indexed).
///
/// The `column` field is a **byte offset within the line**, not a character
/// count. Callers must ensure that it always lies on a valid UTF-8 character
/// boundary in the corresponding source line.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Position {
    /// Line number (0-indexed).
    pub line: u32,
    /// Column offset in bytes from the start of the line (0-indexed).
    /// Must be at a valid UTF-8 character boundary.
    pub column: u32,
}

impl Position {
    /// Creates a new position.
    #[must_use]
    pub const fn new(line: u32, column: u32) -> Self {
        Self { line, column }
    }

    /// Converts a byte offset to a position given source text.
    ///
    /// Returns `None` if the offset is out of bounds.
    #[must_use]
    #[expect(
        clippy::cast_possible_truncation,
        reason = "source files over 4GB are not supported"
    )]
    pub fn from_byte_offset(source: &str, offset: ByteOffset) -> Option<Self> {
        let offset_val = offset.get() as usize;
        if offset_val > source.len() {
            return None;
        }

        let mut line = 0;
        let mut line_start = 0;

        for (i, ch) in source.char_indices() {
            if i >= offset_val {
                return Some(Self::new(line, (offset_val - line_start) as u32));
            }
            if ch == '\n' {
                line += 1;
                line_start = i + 1;
            }
        }

        Some(Self::new(line, (offset_val - line_start) as u32))
    }

    /// Converts a byte offset to a position given source text (legacy).
    ///
    /// Returns `None` if the offset is out of bounds.
    ///
    /// **Deprecated:** Use `from_byte_offset` for type safety.
    #[must_use]
    #[expect(
        clippy::cast_possible_truncation,
        reason = "source files over 4GB are not supported"
    )]
    pub fn from_offset(source: &str, offset: usize) -> Option<Self> {
        Self::from_byte_offset(source, ByteOffset::new(offset as u32))
    }

    /// Converts a position to a byte offset given source text.
    ///
    /// Returns `None` if the position is out of bounds.
    #[must_use]
    #[expect(
        clippy::cast_possible_truncation,
        reason = "source files over 4GB are not supported"
    )]
    pub fn to_byte_offset(self, source: &str) -> Option<ByteOffset> {
        self.to_offset(source)
            .map(|off| ByteOffset::new(off as u32))
    }

    /// Converts a position to a byte offset given source text (legacy).
    ///
    /// Returns `None` if the position is out of bounds.
    ///
    /// **Deprecated:** Use `to_byte_offset` for type safety.
    #[must_use]
    #[expect(
        clippy::cast_possible_truncation,
        reason = "source files over 4GB are not supported"
    )]
    pub fn to_offset(self, source: &str) -> Option<usize> {
        let mut current_line = 0;
        let mut line_start = 0;

        for (i, ch) in source.char_indices() {
            if current_line == self.line {
                let col = (i - line_start) as u32;
                if col == self.column {
                    return Some(i);
                }
            }
            if ch == '\n' {
                if current_line == self.line {
                    // We've passed the line without finding the column
                    return None;
                }
                current_line += 1;
                line_start = i + 1;
            }
        }

        // Handle position at end of last line
        if current_line == self.line {
            let col = (source.len() - line_start) as u32;
            if col == self.column {
                return Some(source.len());
            }
        }

        None
    }
}

/// A location in a source file (file path and span).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Location {
    /// The file path.
    pub file: Utf8PathBuf,
    /// The source span.
    pub span: Span,
}

impl Location {
    /// Creates a new location.
    #[must_use]
    pub fn new(file: Utf8PathBuf, span: Span) -> Self {
        Self { file, span }
    }
}

/// A code completion suggestion.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Completion {
    /// The text to insert.
    pub label: EcoString,
    /// The kind of completion (function, variable, etc.).
    pub kind: CompletionKind,
    /// Optional documentation.
    pub detail: Option<EcoString>,
    /// Optional longer documentation.
    pub documentation: Option<EcoString>,
}

/// The kind of a completion item.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompletionKind {
    /// A function or method.
    Function,
    /// A variable or parameter.
    Variable,
    /// A class or type.
    Class,
    /// A module.
    Module,
    /// A keyword.
    Keyword,
    /// A field.
    Field,
}

impl Completion {
    /// Creates a new completion.
    #[must_use]
    pub fn new(label: impl Into<EcoString>, kind: CompletionKind) -> Self {
        Self {
            label: label.into(),
            kind,
            detail: None,
            documentation: None,
        }
    }

    /// Adds detail text.
    #[must_use]
    pub fn with_detail(mut self, detail: impl Into<EcoString>) -> Self {
        self.detail = Some(detail.into());
        self
    }

    /// Adds documentation.
    #[must_use]
    pub fn with_documentation(mut self, documentation: impl Into<EcoString>) -> Self {
        self.documentation = Some(documentation.into());
        self
    }
}

/// Information to display on hover.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HoverInfo {
    /// The main content (e.g., type signature).
    pub contents: EcoString,
    /// Optional documentation.
    pub documentation: Option<EcoString>,
    /// The span this hover applies to.
    pub span: Span,
}

impl HoverInfo {
    /// Creates new hover info.
    #[must_use]
    pub fn new(contents: impl Into<EcoString>, span: Span) -> Self {
        Self {
            contents: contents.into(),
            documentation: None,
            span,
        }
    }

    /// Adds documentation.
    #[must_use]
    pub fn with_documentation(mut self, documentation: impl Into<EcoString>) -> Self {
        self.documentation = Some(documentation.into());
        self
    }
}

/// A diagnostic message (error or warning).
///
/// This is a re-export of the parse diagnostic with the same structure
/// for consistency across the language service API.
pub type Diagnostic = ParseDiagnostic;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn position_from_offset() {
        let source = "hello\nworld\n!";
        assert_eq!(Position::from_offset(source, 0), Some(Position::new(0, 0)));
        assert_eq!(Position::from_offset(source, 5), Some(Position::new(0, 5)));
        assert_eq!(Position::from_offset(source, 6), Some(Position::new(1, 0)));
        assert_eq!(Position::from_offset(source, 11), Some(Position::new(1, 5)));
        assert_eq!(Position::from_offset(source, 12), Some(Position::new(2, 0)));
    }

    #[test]
    fn position_to_offset() {
        let source = "hello\nworld\n!";
        assert_eq!(Position::new(0, 0).to_offset(source), Some(0));
        assert_eq!(Position::new(0, 5).to_offset(source), Some(5));
        assert_eq!(Position::new(1, 0).to_offset(source), Some(6));
        assert_eq!(Position::new(1, 5).to_offset(source), Some(11));
        assert_eq!(Position::new(2, 0).to_offset(source), Some(12));
    }

    // UTF-8 multi-byte character tests
    #[test]
    fn position_from_offset_with_multibyte_utf8() {
        // "héllo" has é as 2 bytes (0xC3 0xA9), total 6 bytes
        let source = "héllo";
        assert_eq!(Position::from_offset(source, 0), Some(Position::new(0, 0))); // h
        assert_eq!(Position::from_offset(source, 1), Some(Position::new(0, 1))); // start of é
        assert_eq!(Position::from_offset(source, 3), Some(Position::new(0, 3))); // l (after 2-byte é)
        assert_eq!(Position::from_offset(source, 6), Some(Position::new(0, 6))); // end of string
    }

    #[test]
    fn position_from_offset_with_emoji() {
        // "a🎉b" has 🎉 as 4 bytes, total 6 bytes
        let source = "a🎉b";
        assert_eq!(Position::from_offset(source, 0), Some(Position::new(0, 0))); // a
        assert_eq!(Position::from_offset(source, 1), Some(Position::new(0, 1))); // start of 🎉
        assert_eq!(Position::from_offset(source, 5), Some(Position::new(0, 5))); // b (after 4-byte emoji)
        assert_eq!(Position::from_offset(source, 6), Some(Position::new(0, 6))); // end
    }

    #[test]
    fn position_from_offset_with_cjk() {
        // "日本" has 2 CJK chars, each 3 bytes, total 6 bytes
        let source = "日本";
        assert_eq!(Position::from_offset(source, 0), Some(Position::new(0, 0))); // 日
        assert_eq!(Position::from_offset(source, 3), Some(Position::new(0, 3))); // 本
        assert_eq!(Position::from_offset(source, 6), Some(Position::new(0, 6))); // end
    }

    #[test]
    fn position_to_offset_with_multibyte_utf8() {
        let source = "héllo";
        assert_eq!(Position::new(0, 0).to_offset(source), Some(0)); // h
        assert_eq!(Position::new(0, 1).to_offset(source), Some(1)); // start of é
        assert_eq!(Position::new(0, 3).to_offset(source), Some(3)); // l
        assert_eq!(Position::new(0, 6).to_offset(source), Some(6)); // end
    }

    #[test]
    fn position_from_offset_multiline_with_utf8() {
        // Test multiline with UTF-8
        let source = "héllo\nwörld";
        assert_eq!(Position::from_offset(source, 0), Some(Position::new(0, 0))); // h
        assert_eq!(Position::from_offset(source, 7), Some(Position::new(1, 0))); // w (after "héllo\n" which is 7 bytes)
        assert_eq!(Position::from_offset(source, 8), Some(Position::new(1, 1))); // start of ö
    }

    #[test]
    fn position_out_of_bounds() {
        let source = "hello";
        assert_eq!(Position::from_offset(source, 100), None);
        assert_eq!(Position::new(10, 0).to_offset(source), None);
        assert_eq!(Position::new(0, 100).to_offset(source), None);
    }
}
