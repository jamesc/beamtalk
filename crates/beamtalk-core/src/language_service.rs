// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Language service API for IDE integration.
//!
//! Following the TypeScript approach: the compiler IS the language service.
//! All compiler phases are designed to answer IDE queries efficiently.
//!
//! # Architecture
//!
//! The language service provides a query-based interface to compiler internals:
//!
//! - **Diagnostics** - Syntax errors with precise spans (semantic analysis planned)
//! - **Completions** - Code completion suggestions with keywords, identifiers, and messages
//! - **Hover** - Symbol information on hover (type information planned)
//! - **Go to Definition** - Navigate to symbol definitions
//! - **Find References** - Locate all usages of a symbol
//!
//! # Performance Requirements
//!
//! From `docs/beamtalk-architecture.md`:
//!
//! | Operation | Target | Notes |
//! |-----------|--------|-------|
//! | Keystroke to diagnostics | <50ms | LSP responsiveness |
//! | Completions | <50ms | Typing feel |
//! | Hover info | <50ms | Instant feedback |
//! | Go to definition | <100ms | Navigation |
//! | Find references | <500ms | Project-wide search |
//!
//! # Usage
//!
//! ```
//! use beamtalk_core::language_service::{LanguageService, SimpleLanguageService};
//! use camino::Utf8PathBuf;
//!
//! // Create a language service instance
//! let mut service = SimpleLanguageService::new();
//!
//! // Parse and index a file
//! let source = "x := 42";
//! let file_id = Utf8PathBuf::from("example.bt");
//! service.update_file(file_id.clone(), source.to_string());
//!
//! // Get diagnostics
//! let diagnostics = service.diagnostics(&file_id);
//! assert!(diagnostics.is_empty());
//! ```

use crate::ast::{Expression, Identifier, Module};
use crate::parse::{Diagnostic as ParseDiagnostic, Span};
use camino::Utf8PathBuf;
use ecow::EcoString;
use std::collections::HashMap;

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

/// The language service trait.
///
/// This trait defines the core query interface for IDE features.
/// Implementations cache parse results and provide incremental updates.
pub trait LanguageService {
    /// Updates the content of a file.
    ///
    /// This invalidates cached results for the file and triggers reparsing.
    fn update_file(&mut self, file: Utf8PathBuf, content: String);

    /// Removes a file from the language service.
    fn remove_file(&mut self, file: &Utf8PathBuf);

    /// Returns diagnostics for a file.
    ///
    /// This includes syntax errors, type errors, and other issues.
    /// Should respond in <50ms for typical file sizes.
    fn diagnostics(&self, file: &Utf8PathBuf) -> Vec<Diagnostic>;

    /// Returns code completions at a position.
    ///
    /// Should respond in <50ms for typical file sizes.
    fn completions(&self, file: &Utf8PathBuf, position: Position) -> Vec<Completion>;

    /// Returns hover information at a position.
    ///
    /// Should respond in <50ms for typical file sizes.
    fn hover(&self, file: &Utf8PathBuf, position: Position) -> Option<HoverInfo>;

    /// Returns the definition location of the symbol at the given position.
    ///
    /// Should respond in <100ms for typical file sizes.
    fn goto_definition(&self, file: &Utf8PathBuf, position: Position) -> Option<Location>;

    /// Finds all references to the symbol at the given position.
    ///
    /// Should respond in <500ms for project-wide search.
    fn find_references(&self, file: &Utf8PathBuf, position: Position) -> Vec<Location>;
}

/// A simple in-memory language service implementation.
///
/// This implementation stores parsed files in memory and provides
/// basic language service features without full semantic analysis.
#[derive(Debug, Default)]
pub struct SimpleLanguageService {
    /// Cached file contents.
    files: HashMap<Utf8PathBuf, FileData>,
}

#[derive(Debug, Clone)]
struct FileData {
    /// The source text.
    source: String,
    /// The parsed AST.
    module: Module,
    /// Parse diagnostics.
    diagnostics: Vec<Diagnostic>,
}

impl SimpleLanguageService {
    /// Creates a new language service.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Gets file data if it exists.
    fn get_file(&self, file: &Utf8PathBuf) -> Option<&FileData> {
        self.files.get(file)
    }

    /// Finds the identifier at a given position.
    /// Finds the identifier at a given position.
    fn find_identifier_at_position(
        &self,
        file: &Utf8PathBuf,
        position: Position,
    ) -> Option<(Identifier, Span)> {
        let file_data = self.get_file(file)?;
        let offset = position.to_byte_offset(&file_data.source)?;

        // Walk the AST to find the identifier at this position
        for expr in &file_data.module.expressions {
            if let Some(ident) = Self::find_identifier_in_expr(expr, offset) {
                return Some(ident);
            }
        }

        None
    }

    /// Recursively searches for an identifier at the given offset.
    fn find_identifier_in_expr(
        expr: &Expression,
        offset: ByteOffset,
    ) -> Option<(Identifier, Span)> {
        let offset_val = offset.get();
        let span = expr.span();
        if offset_val < span.start() || offset_val >= span.end() {
            return None;
        }

        match expr {
            Expression::Identifier(ident) => {
                if offset_val >= ident.span.start() && offset_val < ident.span.end() {
                    Some((ident.clone(), ident.span))
                } else {
                    None
                }
            }
            Expression::Assignment { target, value, .. } => {
                Self::find_identifier_in_expr(target, offset)
                    .or_else(|| Self::find_identifier_in_expr(value, offset))
            }
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } => Self::find_identifier_in_expr(receiver, offset).or_else(|| {
                arguments
                    .iter()
                    .find_map(|arg| Self::find_identifier_in_expr(arg, offset))
            }),
            Expression::Block(block) => block
                .body
                .iter()
                .find_map(|expr| Self::find_identifier_in_expr(expr, offset)),
            Expression::Return { value, .. } => Self::find_identifier_in_expr(value, offset),
            Expression::Parenthesized { expression, .. } => {
                Self::find_identifier_in_expr(expression, offset)
            }
            Expression::FieldAccess {
                receiver, field, ..
            } => {
                if offset_val >= field.span.start() && offset_val < field.span.end() {
                    Some((field.clone(), field.span))
                } else {
                    Self::find_identifier_in_expr(receiver, offset)
                }
            }
            Expression::CompoundAssignment { target, value, .. } => {
                Self::find_identifier_in_expr(target, offset)
                    .or_else(|| Self::find_identifier_in_expr(value, offset))
            }
            Expression::Cascade {
                receiver, messages, ..
            } => Self::find_identifier_in_expr(receiver, offset).or_else(|| {
                messages.iter().find_map(|msg| {
                    msg.arguments
                        .iter()
                        .find_map(|arg| Self::find_identifier_in_expr(arg, offset))
                })
            }),
            Expression::Pipe { value, target, .. } => Self::find_identifier_in_expr(value, offset)
                .or_else(|| Self::find_identifier_in_expr(target, offset)),
            Expression::Match { value, arms, .. } => Self::find_identifier_in_expr(value, offset)
                .or_else(|| {
                    arms.iter()
                        .find_map(|arm| Self::find_identifier_in_expr(&arm.body, offset))
                }),
            _ => None,
        }
    }

    /// Collects all identifiers with their spans from an expression.
    fn collect_identifiers(expr: &Expression, name: &str, results: &mut Vec<Span>) {
        match expr {
            Expression::Identifier(ident) if ident.name == name => {
                results.push(ident.span);
            }
            Expression::Assignment { target, value, .. }
            | Expression::CompoundAssignment { target, value, .. } => {
                Self::collect_identifiers(target, name, results);
                Self::collect_identifiers(value, name, results);
            }
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } => {
                Self::collect_identifiers(receiver, name, results);
                for arg in arguments {
                    Self::collect_identifiers(arg, name, results);
                }
            }
            Expression::Block(block) => {
                for expr in &block.body {
                    Self::collect_identifiers(expr, name, results);
                }
            }
            Expression::Return { value, .. } => {
                Self::collect_identifiers(value, name, results);
            }
            Expression::Parenthesized { expression, .. } => {
                Self::collect_identifiers(expression, name, results);
            }
            Expression::FieldAccess {
                receiver, field, ..
            } => {
                if field.name == name {
                    results.push(field.span);
                }
                Self::collect_identifiers(receiver, name, results);
            }
            Expression::Cascade {
                receiver, messages, ..
            } => {
                Self::collect_identifiers(receiver, name, results);
                for msg in messages {
                    for arg in &msg.arguments {
                        Self::collect_identifiers(arg, name, results);
                    }
                }
            }
            Expression::Pipe { value, target, .. } => {
                Self::collect_identifiers(value, name, results);
                Self::collect_identifiers(target, name, results);
            }
            Expression::Match { value, arms, .. } => {
                Self::collect_identifiers(value, name, results);
                for arm in arms {
                    Self::collect_identifiers(&arm.body, name, results);
                }
            }
            _ => {}
        }
    }
}

impl LanguageService for SimpleLanguageService {
    fn update_file(&mut self, file: Utf8PathBuf, content: String) {
        use crate::parse::{lex_with_eof, parse};

        let tokens = lex_with_eof(&content);
        let (module, diagnostics) = parse(tokens);

        self.files.insert(
            file,
            FileData {
                source: content,
                module,
                diagnostics,
            },
        );
    }

    fn remove_file(&mut self, file: &Utf8PathBuf) {
        self.files.remove(file);
    }

    fn diagnostics(&self, file: &Utf8PathBuf) -> Vec<Diagnostic> {
        self.get_file(file)
            .map(|data| {
                crate::queries::diagnostic_provider::compute_diagnostics(
                    &data.module,
                    data.diagnostics.clone(),
                )
            })
            .unwrap_or_default()
    }

    fn completions(&self, file: &Utf8PathBuf, position: Position) -> Vec<Completion> {
        let Some(file_data) = self.get_file(file) else {
            return Vec::new();
        };

        crate::queries::completion_provider::compute_completions(
            &file_data.module,
            &file_data.source,
            position,
        )
    }

    fn hover(&self, file: &Utf8PathBuf, position: Position) -> Option<HoverInfo> {
        let file_data = self.get_file(file)?;

        crate::queries::hover_provider::compute_hover(
            &file_data.module,
            &file_data.source,
            position,
        )
    }

    fn goto_definition(&self, file: &Utf8PathBuf, position: Position) -> Option<Location> {
        let (ident, _span) = self.find_identifier_at_position(file, position)?;
        let file_data = self.get_file(file)?;

        // Find the first assignment to this identifier (simple heuristic)
        for expr in &file_data.module.expressions {
            if let Some(def_span) = Self::find_definition(expr, &ident.name) {
                return Some(Location::new(file.clone(), def_span));
            }
        }

        None
    }

    fn find_references(&self, file: &Utf8PathBuf, position: Position) -> Vec<Location> {
        let Some((ident, _span)) = self.find_identifier_at_position(file, position) else {
            return Vec::new();
        };

        let Some(file_data) = self.get_file(file) else {
            return Vec::new();
        };

        let mut spans = Vec::new();
        for expr in &file_data.module.expressions {
            Self::collect_identifiers(expr, &ident.name, &mut spans);
        }

        spans
            .into_iter()
            .map(|span| Location::new(file.clone(), span))
            .collect()
    }
}

impl SimpleLanguageService {
    /// Finds the definition of a variable (first assignment).
    fn find_definition(expr: &Expression, name: &str) -> Option<Span> {
        match expr {
            Expression::Assignment { target, .. } => {
                if let Expression::Identifier(ident) = target.as_ref() {
                    if ident.name == name {
                        return Some(ident.span);
                    }
                }
                None
            }
            Expression::Block(block) => block
                .body
                .iter()
                .find_map(|expr| Self::find_definition(expr, name)),
            _ => None,
        }
    }
}

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

    #[test]
    fn simple_language_service_update_and_diagnostics() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        service.update_file(file.clone(), "x := 42".to_string());
        let diagnostics = service.diagnostics(&file);
        assert!(diagnostics.is_empty());

        // Invalid syntax should produce diagnostics
        service.update_file(file.clone(), "x := :=".to_string());
        let diagnostics = service.diagnostics(&file);
        assert!(!diagnostics.is_empty());
    }

    #[test]
    fn simple_language_service_completions() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        service.update_file(file.clone(), "x := 42".to_string());
        let completions = service.completions(&file, Position::new(0, 0));
        assert!(!completions.is_empty());

        // Should have keyword completions
        assert!(
            completions
                .iter()
                .any(|c| c.label == "self" && c.kind == CompletionKind::Keyword)
        );

        // Should have identifier completions from source
        assert!(
            completions
                .iter()
                .any(|c| c.label == "x" && c.kind == CompletionKind::Variable)
        );

        // Should have message completions
        assert!(
            completions
                .iter()
                .any(|c| c.label == "at:" && c.kind == CompletionKind::Function)
        );
    }

    #[test]
    fn simple_language_service_hover() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        service.update_file(file.clone(), "x := 42".to_string());

        // Hover on 'x' at position 0
        let hover = service.hover(&file, Position::new(0, 0));
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(hover.contents.contains('x'));

        // Hover on literal '42' at position 5
        let hover_literal = service.hover(&file, Position::new(0, 5));
        assert!(hover_literal.is_some());
        let hover_literal = hover_literal.unwrap();
        assert!(hover_literal.contents.contains("42"));
    }

    #[test]
    fn simple_language_service_goto_definition() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        service.update_file(file.clone(), "x := 42.\ny := x".to_string());

        // Go to definition of 'x' at position (1, 5) should find assignment at (0, 0)
        let def = service.goto_definition(&file, Position::new(1, 5));
        assert!(def.is_some());
        let loc = def.unwrap();
        assert_eq!(loc.file, file);
        assert_eq!(loc.span.start(), 0);
    }

    #[test]
    fn simple_language_service_find_references() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        service.update_file(file.clone(), "x := 42.\ny := x".to_string());

        // Find references to 'x'
        let refs = service.find_references(&file, Position::new(0, 0));
        assert_eq!(refs.len(), 2); // Assignment and usage
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
