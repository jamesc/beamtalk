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

    /// Converts a position to a byte offset given source text.
    ///
    /// Returns `None` if the position is out of bounds.
    #[must_use]
    #[expect(
        clippy::cast_possible_truncation,
        reason = "source files over 4GB are not supported"
    )]
    pub fn to_byte_offset(self, source: &str) -> Option<ByteOffset> {
        let mut current_line = 0;
        let mut line_start = 0;

        for (i, ch) in source.char_indices() {
            if current_line == self.line {
                let col = (i - line_start) as u32;
                if col == self.column {
                    return Some(ByteOffset::new(i as u32));
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
                return Some(ByteOffset::new(source.len() as u32));
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

/// A document symbol (class, method, field, etc.) for outline views.
///
/// Follows LSP `DocumentSymbol` naming conventions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DocumentSymbol {
    /// Display name (e.g., "Counter (class)", "increment").
    pub name: EcoString,
    /// Symbol kind.
    pub kind: DocumentSymbolKind,
    /// Source span of the full symbol (used for `range` in LSP).
    pub span: Span,
    /// Source span of the symbol name only (used for `selection_range` in LSP).
    /// Defaults to `span` when not set.
    pub name_span: Option<Span>,
    /// Children (e.g., methods inside a class).
    pub children: Vec<DocumentSymbol>,
}

/// Signature help information for a method being called.
///
/// **DDD Context:** Language Service — Value Object
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignatureHelp {
    /// The matching signatures (usually one, but could be multiple if overloaded).
    pub signatures: Vec<SignatureInfo>,
    /// The index of the active signature.
    pub active_signature: u32,
    /// The index of the active parameter within the active signature.
    pub active_parameter: u32,
}

/// Information about a single method signature.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignatureInfo {
    /// The human-readable label for this signature (e.g., `transfer: amount: Integer to: target: Account -> Boolean`).
    pub label: EcoString,
    /// Optional documentation for the signature.
    pub documentation: Option<EcoString>,
    /// The parameters of the signature.
    pub parameters: Vec<ParameterInfo>,
}

/// Information about a single parameter in a signature.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParameterInfo {
    /// The label for this parameter (e.g., `amount: Integer`).
    pub label: EcoString,
    /// Optional documentation for the parameter.
    pub documentation: Option<EcoString>,
}

/// A suggested code action (e.g. "Add annotation: -> Integer").
///
/// **DDD Context:** Language Service — Value Object
///
/// Used to surface inferred return-type annotations as VS Code quick-fixes
/// (BT-1067, ADR 0045 Phase 1b). Each action describes a single text
/// insertion: `new_text` inserted at byte offset `insert_at` in the file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeAction {
    /// Human-readable title shown in the VS Code lightbulb menu.
    /// Example: `"Add annotation: -> Integer"`
    pub title: EcoString,
    /// The text to insert at `insert_at`.
    /// Example: `"-> Integer "`
    pub new_text: EcoString,
    /// The byte offset (in the source file) at which `new_text` is inserted.
    /// This is a zero-length insertion point — existing text is not replaced.
    pub insert_at: u32,
}

impl CodeAction {
    /// Creates a new code action.
    #[must_use]
    pub fn new(
        title: impl Into<EcoString>,
        new_text: impl Into<EcoString>,
        insert_at: u32,
    ) -> Self {
        Self {
            title: title.into(),
            new_text: new_text.into(),
            insert_at,
        }
    }
}

/// A call-hierarchy target — the method-level symbol surfaced by
/// `textDocument/prepareCallHierarchy` (BT-2243).
///
/// **DDD Context:** Language Service — Value Object
///
/// `prepareCallHierarchy` answers "what method does the cursor refer to?"
/// before the editor follows up with `callHierarchy/incomingCalls` or
/// `callHierarchy/outgoingCalls`. The LSP layer translates this target into
/// an LSP `CallHierarchyItem`; we keep the translation in the LSP crate so
/// `beamtalk-core` stays free of `lsp_types` symbols.
///
/// The cold-file classifier produces one target per cursor hit: the call
/// site's selector, the enclosing method's class (when the cursor is on a
/// method-definition header), or no target at all when the cursor sits on a
/// local identifier the LSP can't resolve to a method.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallHierarchyTarget {
    /// The selector of the method the cursor points at (e.g. `increment`,
    /// `at:put:`, `+`). Used as the display name on the LSP item and as the
    /// senders-of argument when the editor asks for incoming calls.
    pub selector: EcoString,
    /// The class that defines (or contains the call site referencing) the
    /// method, when the cursor is on a method-definition header in a known
    /// class. `None` when the cursor is on a call site whose receiver class
    /// can't be statically resolved — incoming-call queries still work
    /// because `sendersOf:` is class-agnostic.
    pub class_name: Option<EcoString>,
    /// `true` when the method is defined on the metaclass (a class-side
    /// method), `false` for instance-side. Always `false` when `class_name`
    /// is `None`.
    pub class_side: bool,
    /// The file the prepare hit landed in. Used to retain anchoring
    /// information for the follow-up `outgoingCalls` walk, which needs to
    /// re-locate the method's body in this file.
    pub file: Utf8PathBuf,
    /// The span covering the full method definition when the cursor was on a
    /// method-definition header, or just the selector token when on a call
    /// site. Maps to the LSP `range` on `CallHierarchyItem`.
    pub range: Span,
    /// The span of the selector token (sub-range of `range`). Maps to the
    /// LSP `selection_range`. When the selector span is not available (unary
    /// / binary method headers don't carry one), falls back to `range`.
    pub selection_range: Span,
}

impl CallHierarchyTarget {
    /// Creates a new call-hierarchy target.
    #[must_use]
    pub fn new(
        selector: impl Into<EcoString>,
        class_name: Option<EcoString>,
        class_side: bool,
        file: Utf8PathBuf,
        range: Span,
        selection_range: Span,
    ) -> Self {
        Self {
            selector: selector.into(),
            class_name,
            class_side,
            file,
            range,
            selection_range,
        }
    }
}

/// The kind of a document symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DocumentSymbolKind {
    /// A class definition.
    Class,
    /// An instance method.
    Method,
    /// A class-side method.
    ClassMethod,
    /// A state variable (field).
    Field,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn position_from_byte_offset() {
        let source = "hello\nworld\n!";
        assert_eq!(
            Position::from_byte_offset(source, ByteOffset::new(0)),
            Some(Position::new(0, 0))
        );
        assert_eq!(
            Position::from_byte_offset(source, ByteOffset::new(5)),
            Some(Position::new(0, 5))
        );
        assert_eq!(
            Position::from_byte_offset(source, ByteOffset::new(6)),
            Some(Position::new(1, 0))
        );
        assert_eq!(
            Position::from_byte_offset(source, ByteOffset::new(11)),
            Some(Position::new(1, 5))
        );
        assert_eq!(
            Position::from_byte_offset(source, ByteOffset::new(12)),
            Some(Position::new(2, 0))
        );
    }

    #[test]
    fn position_to_byte_offset() {
        let source = "hello\nworld\n!";
        assert_eq!(
            Position::new(0, 0).to_byte_offset(source),
            Some(ByteOffset::new(0))
        );
        assert_eq!(
            Position::new(0, 5).to_byte_offset(source),
            Some(ByteOffset::new(5))
        );
        assert_eq!(
            Position::new(1, 0).to_byte_offset(source),
            Some(ByteOffset::new(6))
        );
        assert_eq!(
            Position::new(1, 5).to_byte_offset(source),
            Some(ByteOffset::new(11))
        );
        assert_eq!(
            Position::new(2, 0).to_byte_offset(source),
            Some(ByteOffset::new(12))
        );
    }

    // UTF-8 multi-byte character tests
    #[test]
    fn position_from_byte_offset_with_multibyte_utf8() {
        // "héllo" has é as 2 bytes (0xC3 0xA9), total 6 bytes
        let source = "héllo";
        assert_eq!(
            Position::from_byte_offset(source, ByteOffset::new(0)),
            Some(Position::new(0, 0))
        ); // h
        assert_eq!(
            Position::from_byte_offset(source, ByteOffset::new(1)),
            Some(Position::new(0, 1))
        ); // start of é
        assert_eq!(
            Position::from_byte_offset(source, ByteOffset::new(3)),
            Some(Position::new(0, 3))
        ); // l (after 2-byte é)
        assert_eq!(
            Position::from_byte_offset(source, ByteOffset::new(6)),
            Some(Position::new(0, 6))
        ); // end of string
    }

    #[test]
    fn position_from_byte_offset_with_emoji() {
        // "a🎉b" has 🎉 as 4 bytes, total 6 bytes
        let source = "a🎉b";
        assert_eq!(
            Position::from_byte_offset(source, ByteOffset::new(0)),
            Some(Position::new(0, 0))
        ); // a
        assert_eq!(
            Position::from_byte_offset(source, ByteOffset::new(1)),
            Some(Position::new(0, 1))
        ); // start of 🎉
        assert_eq!(
            Position::from_byte_offset(source, ByteOffset::new(5)),
            Some(Position::new(0, 5))
        ); // b (after 4-byte emoji)
        assert_eq!(
            Position::from_byte_offset(source, ByteOffset::new(6)),
            Some(Position::new(0, 6))
        ); // end
    }

    #[test]
    fn position_from_byte_offset_with_cjk() {
        // "日本" has 2 CJK chars, each 3 bytes, total 6 bytes
        let source = "日本";
        assert_eq!(
            Position::from_byte_offset(source, ByteOffset::new(0)),
            Some(Position::new(0, 0))
        ); // 日
        assert_eq!(
            Position::from_byte_offset(source, ByteOffset::new(3)),
            Some(Position::new(0, 3))
        ); // 本
        assert_eq!(
            Position::from_byte_offset(source, ByteOffset::new(6)),
            Some(Position::new(0, 6))
        ); // end
    }

    #[test]
    fn position_to_byte_offset_with_multibyte_utf8() {
        let source = "héllo";
        assert_eq!(
            Position::new(0, 0).to_byte_offset(source),
            Some(ByteOffset::new(0))
        ); // h
        assert_eq!(
            Position::new(0, 1).to_byte_offset(source),
            Some(ByteOffset::new(1))
        ); // start of é
        assert_eq!(
            Position::new(0, 3).to_byte_offset(source),
            Some(ByteOffset::new(3))
        ); // l
        assert_eq!(
            Position::new(0, 6).to_byte_offset(source),
            Some(ByteOffset::new(6))
        ); // end
    }

    #[test]
    fn position_from_byte_offset_multiline_with_utf8() {
        // Test multiline with UTF-8
        let source = "héllo\nwörld";
        assert_eq!(
            Position::from_byte_offset(source, ByteOffset::new(0)),
            Some(Position::new(0, 0))
        ); // h
        assert_eq!(
            Position::from_byte_offset(source, ByteOffset::new(7)),
            Some(Position::new(1, 0))
        ); // w (after "héllo\n" which is 7 bytes)
        assert_eq!(
            Position::from_byte_offset(source, ByteOffset::new(8)),
            Some(Position::new(1, 1))
        ); // start of ö
    }

    #[test]
    fn position_out_of_bounds() {
        let source = "hello";
        assert_eq!(
            Position::from_byte_offset(source, ByteOffset::new(100)),
            None
        );
        assert_eq!(Position::new(10, 0).to_byte_offset(source), None);
        assert_eq!(Position::new(0, 100).to_byte_offset(source), None);
    }
}
