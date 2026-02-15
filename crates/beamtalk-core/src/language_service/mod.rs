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

mod project_index;
mod value_objects;

// Re-export value objects at the module level
pub use project_index::ProjectIndex;
pub use value_objects::{
    ByteOffset, Completion, CompletionKind, Diagnostic, HoverInfo, Location, Position,
};

use crate::ast::{Expression, Identifier, Module};
use crate::source_analysis::Span;
use camino::Utf8PathBuf;
use std::collections::HashMap;

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
/// language service features with cross-file class awareness via `ProjectIndex`.
#[derive(Debug, Clone)]
pub struct SimpleLanguageService {
    /// Cached file contents.
    files: HashMap<Utf8PathBuf, FileData>,
    /// Cross-file project index (merged class hierarchy).
    project_index: ProjectIndex,
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
    /// Creates a new language service with an empty `ProjectIndex`.
    #[must_use]
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
            project_index: ProjectIndex::new(),
        }
    }

    /// Creates a new language service with a pre-populated `ProjectIndex`.
    ///
    /// Use this to pre-index stdlib classes or other project-wide class
    /// definitions before opening individual files.
    #[must_use]
    pub fn with_project_index(project_index: ProjectIndex) -> Self {
        Self {
            files: HashMap::new(),
            project_index,
        }
    }

    /// Returns a reference to the project index.
    #[must_use]
    pub fn project_index(&self) -> &ProjectIndex {
        &self.project_index
    }

    /// Gets file data if it exists.
    fn get_file(&self, file: &Utf8PathBuf) -> Option<&FileData> {
        self.files.get(file)
    }

    /// Returns the cached parsed Module for a file, if available.
    pub fn module(&self, file: &Utf8PathBuf) -> Option<&Module> {
        self.files.get(file).map(|data| &data.module)
    }

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
            Expression::StringInterpolation { segments, .. } => segments.iter().find_map(|seg| {
                if let crate::ast::StringSegment::Interpolation(expr) = seg {
                    Self::find_identifier_in_expr(expr, offset)
                } else {
                    None
                }
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
            Expression::Assignment { target, value, .. } => {
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
            Expression::StringInterpolation { segments, .. } => {
                for segment in segments {
                    if let crate::ast::StringSegment::Interpolation(expr) = segment {
                        Self::collect_identifiers(expr, name, results);
                    }
                }
            }
            _ => {}
        }
    }
}

impl LanguageService for SimpleLanguageService {
    fn update_file(&mut self, file: Utf8PathBuf, content: String) {
        use crate::source_analysis::{lex_with_eof, parse};

        let tokens = lex_with_eof(&content);
        let (module, diagnostics) = parse(tokens);

        // Build class hierarchy for the project index.
        // This is intentionally lightweight (ClassHierarchy::build only, not full
        // analyse()) since diagnostic_provider lazily runs full semantic analysis
        // when diagnostics are requested, avoiding duplicate work.
        let (class_hierarchy, _) = crate::semantic_analysis::ClassHierarchy::build(&module);

        // Update the project-wide index with this file's class hierarchy
        self.project_index
            .update_file(file.clone(), &class_hierarchy);

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
        self.project_index.remove_file(file);
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

        // Use project-wide hierarchy for completions (cross-file class awareness)
        crate::queries::completion_provider::compute_completions(
            &file_data.module,
            &file_data.source,
            position,
            self.project_index.hierarchy(),
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

        // Cross-file definition lookup via definition provider (zero-copy)
        crate::queries::definition_provider::find_definition_cross_file(
            &ident.name,
            file,
            &file_data.module,
            &self.project_index,
            self.files.iter().map(|(path, data)| (path, &data.module)),
        )
    }

    fn find_references(&self, file: &Utf8PathBuf, position: Position) -> Vec<Location> {
        let Some((ident, _span)) = self.find_identifier_at_position(file, position) else {
            return Vec::new();
        };

        // Search across all indexed files for references
        let mut results = Vec::new();
        for (file_path, file_data) in &self.files {
            let mut spans = Vec::new();
            for expr in &file_data.module.expressions {
                Self::collect_identifiers(expr, &ident.name, &mut spans);
            }
            results.extend(
                spans
                    .into_iter()
                    .map(|span| Location::new(file_path.clone(), span)),
            );
        }

        results
    }
}

impl Default for SimpleLanguageService {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

        // Should have message completions (from class hierarchy)
        assert!(
            completions
                .iter()
                .any(|c| c.label == "isNil" && c.kind == CompletionKind::Function)
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
}
