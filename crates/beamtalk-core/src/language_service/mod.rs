// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Language service API for IDE integration.
//!
//! **DDD Context:** Language Service
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
    ByteOffset, CodeAction, Completion, CompletionKind, Diagnostic, DocumentSymbol,
    DocumentSymbolKind, HoverInfo, Location, ParameterInfo, Position, SignatureHelp, SignatureInfo,
};

// Property-based tests for language service operations (ADR 0011 Phase 2)
#[cfg(test)]
mod property_tests;

use crate::ast::{Expression, Identifier, Module, TypeAnnotation};
use crate::semantic_analysis::type_checker::NativeTypeRegistry;
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

    /// Returns signature help at a position.
    ///
    /// Should respond in <50ms for typical file sizes.
    fn signature_help(&self, file: &Utf8PathBuf, position: Position) -> Option<SignatureHelp>;

    /// Returns the definition location of the symbol at the given position.
    ///
    /// Should respond in <100ms for typical file sizes.
    fn goto_definition(&self, file: &Utf8PathBuf, position: Position) -> Option<Location>;

    /// Finds all references to the symbol at the given position.
    ///
    /// Should respond in <500ms for project-wide search.
    fn find_references(&self, file: &Utf8PathBuf, position: Position) -> Vec<Location>;

    /// Returns document symbols (outline) for a file.
    ///
    /// Should respond in <50ms for typical file sizes.
    fn document_symbols(&self, file: &Utf8PathBuf) -> Vec<DocumentSymbol>;

    /// Returns code actions available at the given byte range in a file.
    ///
    /// Returns "Add annotation: -> `ClassName`" quick-fixes for unannotated
    /// methods whose return type can be inferred by the `TypeChecker` (BT-1067).
    /// Should respond in <50ms for typical file sizes.
    fn code_actions(&self, file: &Utf8PathBuf, start: u32, end: u32) -> Vec<CodeAction>;
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
    /// Native type registry for Erlang FFI typed completions (ADR 0075).
    native_types: Option<NativeTypeRegistry>,
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
            native_types: None,
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
            native_types: None,
        }
    }

    /// Sets the native type registry for Erlang FFI typed completions.
    ///
    /// ADR 0075 Phase 1: When set, `Erlang <module>` completions display
    /// typed signatures (e.g., `reverse: list :: List -> List`) instead of
    /// just arity information.
    pub fn set_native_types(&mut self, registry: NativeTypeRegistry) {
        self.native_types = Some(registry);
    }

    /// Returns a reference to the native type registry, if set.
    #[must_use]
    pub fn native_types(&self) -> Option<&NativeTypeRegistry> {
        self.native_types.as_ref()
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

    /// Checks if a goto-definition result points to a `self delegate` method
    /// in a native class, returning the backing Erlang module name if so.
    ///
    /// Used by the LSP to redirect navigation to the backing `.erl` file.
    #[must_use]
    pub fn check_native_delegate(
        &self,
        location: &Location,
    ) -> Option<crate::queries::definition_provider::NativeDelegateInfo> {
        crate::queries::definition_provider::check_native_delegate(
            location,
            self.files.iter().map(|(path, data)| (path, &data.module)),
        )
    }

    /// Check if a position is on an Erlang FFI call, returning module/function info.
    ///
    /// Used by the LSP to redirect goto-definition to the backing `.erl` source file
    /// when the cursor is on `Erlang <module> <function>:`.
    #[must_use]
    pub fn check_ffi_call(
        &self,
        file: &Utf8PathBuf,
        position: Position,
    ) -> Option<crate::queries::definition_provider::FfiCallInfo> {
        let file_data = self.get_file(file)?;
        let offset = position.to_byte_offset(&file_data.source)?;
        let mut info =
            crate::queries::definition_provider::check_ffi_call(&file_data.module, offset.get())?;

        // Enrich with line number from the native type registry if available.
        if !info.function_name.is_empty() {
            if let Some(sig) = self
                .native_types
                .as_ref()
                .and_then(|reg| reg.lookup(&info.module_name, &info.function_name, info.arity))
            {
                info.line = sig.line;
            }
        }

        Some(info)
    }

    /// Returns the cached source text for a file, if available.
    #[must_use]
    pub fn file_source(&self, file: &Utf8PathBuf) -> Option<String> {
        self.files.get(file).map(|data| data.source.clone())
    }

    /// Finds a method selector at a given byte offset in a module.
    ///
    /// Searches expressions, class method bodies, and standalone method definitions.
    fn find_selector_at_offset(
        module: &Module,
        offset: u32,
    ) -> Option<crate::queries::definition_provider::SelectorLookup> {
        // Check top-level expressions
        for stmt in &module.expressions {
            if let Some(result) = crate::queries::definition_provider::find_selector_lookup_in_expr(
                &stmt.expression,
                offset,
            ) {
                return Some(result);
            }
        }
        // Check class method bodies
        for class in &module.classes {
            for method in class.methods.iter().chain(class.class_methods.iter()) {
                for stmt in &method.body {
                    if let Some(result) =
                        crate::queries::definition_provider::find_selector_lookup_in_expr(
                            &stmt.expression,
                            offset,
                        )
                    {
                        return Some(result);
                    }
                }
            }
        }
        // Check standalone method bodies
        for smd in &module.method_definitions {
            for stmt in &smd.method.body {
                if let Some(result) =
                    crate::queries::definition_provider::find_selector_lookup_in_expr(
                        &stmt.expression,
                        offset,
                    )
                {
                    return Some(result);
                }
            }
        }
        None
    }

    /// Finds the identifier at a given position.
    fn find_identifier_at_position(
        &self,
        file: &Utf8PathBuf,
        position: Position,
    ) -> Option<(Identifier, Span)> {
        let file_data = self.get_file(file)?;
        let offset = position.to_byte_offset(&file_data.source)?;
        let offset_val = offset.get();

        // Check class definitions (name, superclass, method bodies)
        for class in &file_data.module.classes {
            if offset_val >= class.name.span.start() && offset_val < class.name.span.end() {
                return Some((class.name.clone(), class.name.span));
            }
            if let Some(ref superclass) = class.superclass {
                if offset_val >= superclass.span.start() && offset_val < superclass.span.end() {
                    return Some((superclass.clone(), superclass.span));
                }
            }

            for state in class.state.iter().chain(class.class_variables.iter()) {
                if let Some(type_annotation) = &state.type_annotation {
                    if let Some(ident) =
                        Self::find_identifier_in_type_annotation(type_annotation, offset_val)
                    {
                        return Some(ident);
                    }
                }
            }

            // Search method bodies
            for method in class.methods.iter().chain(class.class_methods.iter()) {
                for parameter in &method.parameters {
                    if let Some(type_annotation) = &parameter.type_annotation {
                        if let Some(ident) =
                            Self::find_identifier_in_type_annotation(type_annotation, offset_val)
                        {
                            return Some(ident);
                        }
                    }
                }
                if let Some(return_type) = &method.return_type {
                    if let Some(ident) =
                        Self::find_identifier_in_type_annotation(return_type, offset_val)
                    {
                        return Some(ident);
                    }
                }
                for stmt in &method.body {
                    if let Some(ident) = Self::find_identifier_in_expr(&stmt.expression, offset) {
                        return Some(ident);
                    }
                }
            }
        }

        // Check standalone method definitions
        for smd in &file_data.module.method_definitions {
            if offset_val >= smd.class_name.span.start() && offset_val < smd.class_name.span.end() {
                return Some((smd.class_name.clone(), smd.class_name.span));
            }

            for parameter in &smd.method.parameters {
                if let Some(type_annotation) = &parameter.type_annotation {
                    if let Some(ident) =
                        Self::find_identifier_in_type_annotation(type_annotation, offset_val)
                    {
                        return Some(ident);
                    }
                }
            }
            if let Some(return_type) = &smd.method.return_type {
                if let Some(ident) =
                    Self::find_identifier_in_type_annotation(return_type, offset_val)
                {
                    return Some(ident);
                }
            }

            for stmt in &smd.method.body {
                if let Some(ident) = Self::find_identifier_in_expr(&stmt.expression, offset) {
                    return Some(ident);
                }
            }
        }

        // Walk the top-level expressions
        for stmt in &file_data.module.expressions {
            if let Some(ident) = Self::find_identifier_in_expr(&stmt.expression, offset) {
                return Some(ident);
            }
        }

        None
    }

    fn find_identifier_in_type_annotation(
        annotation: &TypeAnnotation,
        offset_val: u32,
    ) -> Option<(Identifier, Span)> {
        match annotation {
            TypeAnnotation::Simple(identifier) => {
                if offset_val >= identifier.span.start() && offset_val < identifier.span.end() {
                    Some((identifier.clone(), identifier.span))
                } else {
                    None
                }
            }
            TypeAnnotation::Union { types, .. } => types
                .iter()
                .find_map(|ty| Self::find_identifier_in_type_annotation(ty, offset_val)),
            TypeAnnotation::Generic {
                base, parameters, ..
            } => {
                if offset_val >= base.span.start() && offset_val < base.span.end() {
                    return Some((base.clone(), base.span));
                }
                parameters
                    .iter()
                    .find_map(|ty| Self::find_identifier_in_type_annotation(ty, offset_val))
            }
            TypeAnnotation::FalseOr { inner, .. } => {
                Self::find_identifier_in_type_annotation(inner, offset_val)
            }
            TypeAnnotation::Singleton { .. } | TypeAnnotation::SelfType { .. } => None,
        }
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
            Expression::ClassReference { name, .. } => {
                if offset_val >= name.span.start() && offset_val < name.span.end() {
                    Some((name.clone(), name.span))
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
                .find_map(|stmt| Self::find_identifier_in_expr(&stmt.expression, offset)),
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
            Expression::ClassReference {
                name: class_name, ..
            } if class_name.name == name => {
                results.push(class_name.span);
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
                for stmt in &block.body {
                    Self::collect_identifiers(&stmt.expression, name, results);
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
        let (class_hierarchy_result, hierarchy_diags) =
            crate::semantic_analysis::ClassHierarchy::build(&module);
        if let Ok(class_hierarchy) = class_hierarchy_result {
            // Update the project-wide index with this file's class hierarchy
            self.project_index
                .update_file(file.clone(), &class_hierarchy);
        } else {
            // Hierarchy build failed: store the file with merged diagnostics
            // but do not update the project index for this file.
            let mut all_diagnostics = diagnostics;
            all_diagnostics.extend(hierarchy_diags);
            self.files.insert(
                file,
                FileData {
                    source: content,
                    module,
                    diagnostics: all_diagnostics,
                },
            );
            return;
        }

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
                // ADR 0075 Phase 4: Pass NativeTypeRegistry so FFI type warnings
                // surface in the editor as diagnostics.
                crate::queries::diagnostic_provider::compute_diagnostics_with_native_types(
                    &data.module,
                    data.diagnostics.clone(),
                    self.native_types.clone(),
                )
            })
            .unwrap_or_default()
    }

    fn completions(&self, file: &Utf8PathBuf, position: Position) -> Vec<Completion> {
        let Some(file_data) = self.get_file(file) else {
            return Vec::new();
        };

        // Determine the current file's package for cross-package visibility filtering
        // (ADR 0071, BT-1703).
        let current_package = self.project_index.package_for_file(file);

        // Use project-wide hierarchy for completions (cross-file class awareness)
        // ADR 0075: Pass native type registry for typed Erlang FFI completions
        crate::queries::completion_provider::compute_completions(
            &file_data.module,
            &file_data.source,
            position,
            self.project_index.hierarchy(),
            current_package.as_deref(),
            self.native_types.as_ref(),
        )
    }

    fn hover(&self, file: &Utf8PathBuf, position: Position) -> Option<HoverInfo> {
        let file_data = self.get_file(file)?;

        crate::queries::hover_provider::compute_hover(
            &file_data.module,
            &file_data.source,
            position,
            self.project_index.hierarchy(),
            self.native_types.as_ref(),
        )
    }

    fn signature_help(&self, file: &Utf8PathBuf, position: Position) -> Option<SignatureHelp> {
        let file_data = self.get_file(file)?;

        crate::queries::signature_help_provider::compute_signature_help(
            &file_data.module,
            &file_data.source,
            position,
            self.project_index.hierarchy(),
            self.native_types.as_ref(),
        )
    }

    fn goto_definition(&self, file: &Utf8PathBuf, position: Position) -> Option<Location> {
        let file_data = self.get_file(file)?;
        let offset = position.to_byte_offset(&file_data.source)?;

        // 1. Try selector-based go-to-definition (cursor on a method keyword)
        if let Some(selector_lookup) =
            Self::find_selector_at_offset(&file_data.module, offset.get())
        {
            let receiver_context =
                crate::queries::definition_provider::resolve_receiver_class_context(
                    &file_data.module,
                    offset.get(),
                    &selector_lookup,
                    self.project_index.hierarchy(),
                );
            return crate::queries::definition_provider::find_method_definition_cross_file_with_receiver(
                selector_lookup.selector_name.as_str(),
                receiver_context.as_ref(),
                &self.project_index,
                self.files.iter().map(|(path, data)| (path, &data.module)),
            );
        }

        // 2. Try identifier-based go-to-definition (cursor on a name)
        let (ident, _span) = self.find_identifier_at_position(file, position)?;

        // Cross-file definition lookup via definition provider
        crate::queries::definition_provider::find_definition_cross_file(
            &ident.name,
            file,
            &file_data.module,
            &self.project_index,
            self.files.iter().map(|(path, data)| (path, &data.module)),
        )
    }

    fn find_references(&self, file: &Utf8PathBuf, position: Position) -> Vec<Location> {
        let file_data = self.get_file(file);
        let Some(file_data) = file_data else {
            return Vec::new();
        };
        let Some(offset) = position.to_byte_offset(&file_data.source) else {
            return Vec::new();
        };

        // 1. Try selector-based references (cursor on a method keyword)
        if let Some(selector_lookup) =
            Self::find_selector_at_offset(&file_data.module, offset.get())
        {
            return crate::queries::references_provider::find_selector_references(
                selector_lookup.selector_name.as_str(),
                self.files.iter().map(|(path, data)| (path, &data.module)),
            );
        }

        // 2. Try identifier-based references (cursor on a name)
        let Some((ident, _span)) = self.find_identifier_at_position(file, position) else {
            return Vec::new();
        };

        // If the identifier is a class name, use class-aware references
        if self.project_index.hierarchy().has_class(&ident.name) {
            return crate::queries::references_provider::find_class_references(
                &ident.name,
                self.files.iter().map(|(path, data)| (path, &data.module)),
            );
        }

        // Fall back to identifier-based references across all files
        let mut results = Vec::new();
        for (file_path, fd) in &self.files {
            let mut spans = Vec::new();
            for stmt in &fd.module.expressions {
                Self::collect_identifiers(&stmt.expression, &ident.name, &mut spans);
            }
            // Also search class method bodies
            for class in &fd.module.classes {
                for method in class.methods.iter().chain(class.class_methods.iter()) {
                    for stmt in &method.body {
                        Self::collect_identifiers(&stmt.expression, &ident.name, &mut spans);
                    }
                }
            }
            // And standalone method bodies
            for smd in &fd.module.method_definitions {
                for stmt in &smd.method.body {
                    Self::collect_identifiers(&stmt.expression, &ident.name, &mut spans);
                }
            }
            results.extend(
                spans
                    .into_iter()
                    .map(|span| Location::new(file_path.clone(), span)),
            );
        }

        results
    }

    fn document_symbols(&self, file: &Utf8PathBuf) -> Vec<DocumentSymbol> {
        let Some(file_data) = self.get_file(file) else {
            return Vec::new();
        };

        crate::queries::document_symbols_provider::compute_document_symbols(&file_data.module)
    }

    fn code_actions(&self, file: &Utf8PathBuf, start: u32, end: u32) -> Vec<CodeAction> {
        let Some(file_data) = self.get_file(file) else {
            return Vec::new();
        };

        let inferred = crate::semantic_analysis::type_checker::infer_method_return_types(
            &file_data.module,
            self.project_index.hierarchy(),
        );

        if inferred.is_empty() {
            return Vec::new();
        }

        let source = &file_data.source;
        let mut actions = Vec::new();

        for class in &file_data.module.classes {
            for (method, is_class_method) in class
                .methods
                .iter()
                .map(|m| (m, false))
                .chain(class.class_methods.iter().map(|m| (m, true)))
            {
                if method.return_type.is_some() {
                    continue;
                }
                // Only offer for methods whose span overlaps the requested range.
                // Distinguish cursor requests (start == end, point-in-span check) from
                // selection requests (half-open interval overlap).
                let overlaps = if start == end {
                    method.span.start() <= start && start < method.span.end()
                } else {
                    method.span.start() < end && start < method.span.end()
                };
                if !overlaps {
                    continue;
                }
                let key = (
                    class.name.name.clone(),
                    method.selector.name(),
                    is_class_method,
                );
                if let Some(class_name) = inferred.get(&key) {
                    if let Some(offset) = find_body_open_offset(source, method.span) {
                        actions.push(CodeAction::new(
                            format!("Add annotation: -> {class_name}"),
                            format!("-> {class_name} "),
                            offset,
                        ));
                    }
                }
            }
        }

        for standalone in &file_data.module.method_definitions {
            let method = &standalone.method;
            if method.return_type.is_some() {
                continue;
            }
            let overlaps = if start == end {
                method.span.start() <= start && start < method.span.end()
            } else {
                method.span.start() < end && start < method.span.end()
            };
            if !overlaps {
                continue;
            }
            let key = (
                standalone.class_name.name.clone(),
                method.selector.name(),
                standalone.is_class_method,
            );
            if let Some(class_name) = inferred.get(&key) {
                if let Some(offset) = find_body_open_offset(source, method.span) {
                    actions.push(CodeAction::new(
                        format!("Add annotation: -> {class_name}"),
                        format!("-> {class_name} "),
                        offset,
                    ));
                }
            }
        }

        actions
    }
}

/// Finds the byte offset of the `=>` body opener of a method definition.
///
/// Scans forward from `method_span.start()` in `source` looking for the first
/// `=>` token.  Inserting text at this offset places a return-type annotation
/// just before the body opener, e.g. turning `count => 42` into
/// `count -> Integer => 42`.
///
/// Returns `None` when the span is out of bounds or no `=>` is found.
#[expect(
    clippy::cast_possible_truncation,
    reason = "source files over 4GB are not supported"
)]
fn find_body_open_offset(source: &str, method_span: Span) -> Option<u32> {
    let start = method_span.start() as usize;
    let end = method_span.end() as usize;
    let src = source.get(start..end.min(source.len()))?;
    let bytes = src.as_bytes();
    let mut i = 0;
    while i < bytes.len().saturating_sub(1) {
        if bytes[i] == b'=' && bytes[i + 1] == b'>' {
            return Some((start + i) as u32);
        }
        i += 1;
    }
    None
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

    #[test]
    fn goto_definition_cross_file_class() {
        let mut service = SimpleLanguageService::new();
        let file_a = Utf8PathBuf::from("a.bt");
        let file_b = Utf8PathBuf::from("b.bt");

        service.update_file(
            file_a.clone(),
            "Object subclass: Foo\n  bar => 1".to_string(),
        );
        service.update_file(file_b.clone(), "x := Foo new".to_string());

        // Go to definition of 'Foo' from file_b should find class in file_a
        let def = service.goto_definition(&file_b, Position::new(0, 5));
        assert!(def.is_some());
        let loc = def.unwrap();
        assert_eq!(loc.file, file_a);
    }

    #[test]
    fn goto_definition_cross_file_method_keyword() {
        let mut service = SimpleLanguageService::new();
        let file_a = Utf8PathBuf::from("a.bt");
        let file_b = Utf8PathBuf::from("b.bt");

        service.update_file(
            file_a.clone(),
            "Object subclass: Foo\n  at: i put: v => v".to_string(),
        );
        // "x at: 1 put: 2" — cursor on "at:" keyword
        service.update_file(file_b.clone(), "x at: 1 put: 2".to_string());

        // Position 2 is on "at:" — should navigate to method definition in file_a
        let def = service.goto_definition(&file_b, Position::new(0, 2));
        assert!(def.is_some());
        let loc = def.unwrap();
        assert_eq!(loc.file, file_a);
    }

    #[test]
    fn goto_definition_stdlib_class() {
        let stdlib = vec![(
            Utf8PathBuf::from("stdlib/src/Counter.bt"),
            "Object subclass: Counter\n  increment => 1".to_string(),
        )];
        let index = ProjectIndex::with_stdlib(&stdlib).0.unwrap();
        let mut service = SimpleLanguageService::with_project_index(index);

        // Add the stdlib file as an open file too so cross-file lookup can find it
        service.update_file(
            Utf8PathBuf::from("stdlib/src/Counter.bt"),
            "Object subclass: Counter\n  increment => 1".to_string(),
        );

        let user_file = Utf8PathBuf::from("user.bt");
        service.update_file(user_file.clone(), "x := Counter new".to_string());

        // Go to definition of 'Counter' from user.bt should find in lib/Counter.bt
        let def = service.goto_definition(&user_file, Position::new(0, 5));
        assert!(def.is_some());
        let loc = def.unwrap();
        assert_eq!(loc.file, Utf8PathBuf::from("stdlib/src/Counter.bt"));
    }

    #[test]
    fn goto_definition_superclass_in_class_header() {
        let mut service = SimpleLanguageService::new();

        let collection_file = Utf8PathBuf::from("stdlib/src/Collection.bt");
        let set_file = Utf8PathBuf::from("stdlib/src/Set.bt");

        service.update_file(
            collection_file.clone(),
            "abstract Object subclass: Collection".to_string(),
        );
        service.update_file(
            set_file.clone(),
            "sealed Collection subclass: Set".to_string(),
        );

        // Cursor on "Collection" in class header.
        let def = service.goto_definition(&set_file, Position::new(0, 8));
        assert!(def.is_some());
        let loc = def.unwrap();
        assert_eq!(loc.file, collection_file);
    }

    #[test]
    fn goto_definition_type_annotation_identifier() {
        let mut service = SimpleLanguageService::new();
        let integer_file = Utf8PathBuf::from("stdlib/src/Integer.bt");
        let tuple_file = Utf8PathBuf::from("stdlib/src/Tuple.bt");

        service.update_file(
            integer_file.clone(),
            "sealed Number subclass: Integer".to_string(),
        );
        service.update_file(
            tuple_file.clone(),
            "sealed Collection subclass: Tuple\n  size -> Integer => 0".to_string(),
        );

        let def = service.goto_definition(&tuple_file, Position::new(1, 10));
        assert!(def.is_some());
        let loc = def.unwrap();
        assert_eq!(loc.file, integer_file);
    }

    #[test]
    fn find_references_class_cross_file() {
        let mut service = SimpleLanguageService::new();
        let file_a = Utf8PathBuf::from("a.bt");
        let file_b = Utf8PathBuf::from("b.bt");

        service.update_file(
            file_a.clone(),
            "Object subclass: Foo\n  bar => 1".to_string(),
        );
        service.update_file(file_b.clone(), "x := Foo new".to_string());

        // Find references to 'Foo' from file_a — should find in both files
        let refs = service.find_references(&file_a, Position::new(0, 17));
        assert!(refs.len() >= 2);
        assert!(refs.iter().any(|r| r.file == file_a));
        assert!(refs.iter().any(|r| r.file == file_b));
    }

    #[test]
    fn find_references_selector_cross_file() {
        let mut service = SimpleLanguageService::new();
        let file_a = Utf8PathBuf::from("a.bt");
        let file_b = Utf8PathBuf::from("b.bt");

        service.update_file(
            file_a.clone(),
            "Object subclass: Foo\n  at: i put: v => v".to_string(),
        );
        // "x at: 1 put: 2" — cursor on "at:" keyword
        service.update_file(file_b.clone(), "x at: 1 put: 2".to_string());

        // Find references to 'at:put:' from file_b
        let refs = service.find_references(&file_b, Position::new(0, 2));
        assert!(refs.len() >= 2);
        assert!(refs.iter().any(|r| r.file == file_a));
        assert!(refs.iter().any(|r| r.file == file_b));
    }

    #[test]
    fn goto_definition_cross_file_method_unary() {
        let mut service = SimpleLanguageService::new();
        let file_a = Utf8PathBuf::from("a.bt");
        let file_b = Utf8PathBuf::from("b.bt");

        service.update_file(
            file_a.clone(),
            "Object subclass: Foo\n  bar => 1".to_string(),
        );
        // Cursor on unary selector "bar"
        service.update_file(file_b.clone(), "x bar".to_string());

        let def = service.goto_definition(&file_b, Position::new(0, 2));
        assert!(def.is_some());
        let loc = def.unwrap();
        assert_eq!(loc.file, file_a);
    }

    #[test]
    fn goto_definition_method_uses_inferred_receiver_class_context() {
        let mut service = SimpleLanguageService::new();
        let file_foo = Utf8PathBuf::from("foo.bt");
        let file_bar = Utf8PathBuf::from("bar.bt");
        let file_calls = Utf8PathBuf::from("calls.bt");

        service.update_file(
            file_foo.clone(),
            "Object subclass: Foo\n  ping => 1".to_string(),
        );
        service.update_file(
            file_bar.clone(),
            "Object subclass: Bar\n  ping => 2".to_string(),
        );
        service.update_file(
            file_calls.clone(),
            "x := Foo new\nx ping\ny := Bar new\ny ping".to_string(),
        );

        let first = service.goto_definition(&file_calls, Position::new(1, 2));
        assert!(first.is_some());
        assert_eq!(first.unwrap().file, file_foo);

        let second = service.goto_definition(&file_calls, Position::new(3, 2));
        assert!(second.is_some());
        assert_eq!(second.unwrap().file, file_bar);
    }

    #[test]
    fn goto_definition_method_uses_receiver_class_side_context() {
        let mut service = SimpleLanguageService::new();
        let file_foo = Utf8PathBuf::from("foo.bt");
        let file_bar = Utf8PathBuf::from("bar.bt");
        let file_calls = Utf8PathBuf::from("calls.bt");

        service.update_file(
            file_foo.clone(),
            "Object subclass: Foo\n  class ping => 1".to_string(),
        );
        service.update_file(
            file_bar.clone(),
            "Object subclass: Bar\n  class ping => 2".to_string(),
        );
        service.update_file(file_calls.clone(), "Foo ping\nBar ping".to_string());

        let first = service.goto_definition(&file_calls, Position::new(0, 4));
        assert!(first.is_some());
        assert_eq!(first.unwrap().file, file_foo);

        let second = service.goto_definition(&file_calls, Position::new(1, 4));
        assert!(second.is_some());
        assert_eq!(second.unwrap().file, file_bar);
    }

    #[test]
    fn goto_definition_method_ambiguous_without_receiver_is_deterministic() {
        let mut service = SimpleLanguageService::new();
        let file_alpha = Utf8PathBuf::from("alpha.bt");
        let file_zed = Utf8PathBuf::from("zed.bt");
        let file_calls = Utf8PathBuf::from("calls.bt");

        service.update_file(
            file_alpha.clone(),
            "Object subclass: Alpha\n  ping => 1".to_string(),
        );
        service.update_file(
            file_zed.clone(),
            "Object subclass: Zed\n  ping => 2".to_string(),
        );
        service.update_file(file_calls.clone(), "unknown ping".to_string());

        let def = service.goto_definition(&file_calls, Position::new(0, 8));
        assert!(def.is_some());
        assert_eq!(def.unwrap().file, file_alpha);
    }

    #[test]
    fn goto_definition_method_with_self_receiver_resolves_in_class() {
        let mut service = SimpleLanguageService::new();
        let file_foo = Utf8PathBuf::from("foo.bt");

        service.update_file(
            file_foo.clone(),
            "Object subclass: Foo\n  ping => self other\n  other => 1".to_string(),
        );

        let def = service.goto_definition(&file_foo, Position::new(1, 15));
        assert!(def.is_some());
        assert_eq!(def.unwrap().file, file_foo);
    }

    #[test]
    fn goto_definition_method_with_super_receiver_resolves_in_superclass() {
        let mut service = SimpleLanguageService::new();
        let file_hierarchy = Utf8PathBuf::from("hierarchy.bt");

        service.update_file(
            file_hierarchy.clone(),
            "Object subclass: Foo\n  other => 1\nFoo subclass: Bar\n  ping => super other"
                .to_string(),
        );

        let def = service.goto_definition(&file_hierarchy, Position::new(3, 16));
        assert!(def.is_some());
        assert_eq!(def.unwrap().file, file_hierarchy);
    }

    // ── native delegate tests (BT-1215) ────────────────────────────────────

    #[test]
    fn check_native_delegate_on_self_delegate_method() {
        let mut service = SimpleLanguageService::new();
        let file_def = Utf8PathBuf::from("native.bt");
        service.update_file(
            file_def.clone(),
            "Actor subclass: Proc native: beamtalk_proc\n  run => self delegate".to_string(),
        );
        let file_call = Utf8PathBuf::from("caller.bt");
        service.update_file(file_call.clone(), "p := Proc spawn\np run".to_string());

        // Go-to-definition on the `run` call navigates to the method definition
        let def = service.goto_definition(&file_call, Position::new(1, 2));
        assert!(def.is_some(), "should find the method definition");
        let loc = def.unwrap();
        assert_eq!(loc.file, file_def);

        let delegate_info = service.check_native_delegate(&loc);
        assert!(delegate_info.is_some(), "should detect native delegate");
        assert_eq!(delegate_info.unwrap().backing_module, "beamtalk_proc");
    }

    #[test]
    fn check_native_delegate_returns_none_for_normal_method() {
        let mut service = SimpleLanguageService::new();
        let file_def = Utf8PathBuf::from("normal.bt");
        service.update_file(
            file_def.clone(),
            "Object subclass: Foo\n  bar => 42".to_string(),
        );
        let file_call = Utf8PathBuf::from("caller.bt");
        service.update_file(file_call.clone(), "f := Foo new\nf bar".to_string());

        let def = service.goto_definition(&file_call, Position::new(1, 2));
        assert!(def.is_some());
        let loc = def.unwrap();
        let delegate_info = service.check_native_delegate(&loc);
        assert!(delegate_info.is_none());
    }

    // ── FFI goto-definition tests ─────────────────────────────────────────

    #[test]
    fn check_ffi_call_on_selector() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("ffi.bt");
        service.update_file(file.clone(), "Erlang lists reverse: items".to_string());

        // Cursor on `reverse:` — should detect FFI call
        let ffi = service.check_ffi_call(&file, Position::new(0, 20));
        assert!(ffi.is_some(), "should detect FFI call on selector");
        let info = ffi.unwrap();
        assert_eq!(info.module_name, "lists");
        assert_eq!(info.function_name, "reverse");
    }

    #[test]
    fn check_ffi_call_on_module_name() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("ffi.bt");
        service.update_file(file.clone(), "Erlang lists reverse: items".to_string());

        // Cursor on `lists` — should detect FFI call with module but no function
        let ffi = service.check_ffi_call(&file, Position::new(0, 8));
        assert!(ffi.is_some(), "should detect FFI call on module name");
        let info = ffi.unwrap();
        assert_eq!(info.module_name, "lists");
        assert!(info.function_name.is_empty());
    }

    #[test]
    fn check_ffi_call_returns_none_for_normal_send() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("normal.bt");
        service.update_file(file.clone(), "items reverse".to_string());

        let ffi = service.check_ffi_call(&file, Position::new(0, 7));
        assert!(
            ffi.is_none(),
            "should not detect FFI on normal message send"
        );
    }

    #[test]
    fn check_ffi_call_in_method_body() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("method.bt");
        service.update_file(
            file.clone(),
            "Object subclass: Foo\n  bar => Erlang io format: \"hello\"".to_string(),
        );

        // Cursor on `format:` inside method body
        let ffi = service.check_ffi_call(&file, Position::new(1, 20));
        assert!(ffi.is_some(), "should detect FFI in method body");
        let info = ffi.unwrap();
        assert_eq!(info.module_name, "io");
        assert_eq!(info.function_name, "format");
    }

    #[test]
    fn check_ffi_call_enriches_line_from_registry() {
        use crate::semantic_analysis::InferredType;
        use crate::semantic_analysis::type_checker::{
            FunctionSignature, NativeTypeRegistry, ParamType, TypeProvenance,
        };

        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("ffi.bt");
        service.update_file(file.clone(), "Erlang lists reverse: items".to_string());

        // Set up registry with a line number for lists:reverse/1
        let mut registry = NativeTypeRegistry::new();
        registry.register_module(
            "lists",
            vec![FunctionSignature {
                name: "reverse".to_string(),
                arity: 1,
                params: vec![ParamType {
                    keyword: Some(ecow::EcoString::from("list")),
                    type_: InferredType::known("List"),
                }],
                return_type: InferredType::known("List"),
                provenance: TypeProvenance::Extracted,
                line: Some(42),
            }],
        );
        service.set_native_types(registry);

        let ffi = service.check_ffi_call(&file, Position::new(0, 20));
        assert!(ffi.is_some());
        let info = ffi.unwrap();
        assert_eq!(info.module_name, "lists");
        assert_eq!(info.function_name, "reverse");
        assert_eq!(info.line, Some(42));
    }

    #[test]
    fn check_ffi_call_line_is_none_without_registry() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("ffi.bt");
        service.update_file(file.clone(), "Erlang lists reverse: items".to_string());

        // No registry set — line should be None
        let ffi = service.check_ffi_call(&file, Position::new(0, 20));
        assert!(ffi.is_some());
        assert!(ffi.unwrap().line.is_none());
    }

    // ── code_actions tests (BT-1067) ────────────────────────────────────────

    /// Helper: byte length of a source string as `u32` (test-only).
    #[expect(
        clippy::cast_possible_truncation,
        reason = "test strings are trivially small"
    )]
    fn len32(s: &str) -> u32 {
        s.len() as u32
    }

    #[test]
    fn code_actions_returns_annotation_suggestion_for_unary_method() {
        // `count => 42` — inferred Integer; action inserts `-> Integer ` before `=>`
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");
        let source = "Object subclass: Counter\n  count => 42";
        service.update_file(file.clone(), source.to_string());

        // Request code actions spanning the whole file
        let actions = service.code_actions(&file, 0, len32(source));
        assert_eq!(actions.len(), 1, "expected exactly one code action");

        let action = &actions[0];
        assert!(
            action.title.contains("Integer"),
            "title should mention inferred type: {title}",
            title = action.title
        );
        assert_eq!(action.new_text, "-> Integer ", "wrong inserted text");

        // Verify insertion point is at the `=>` of the method
        let before_body = &source[..action.insert_at as usize];
        assert!(
            before_body.ends_with("count "),
            "insertion should be before `=>`, got prefix: {before_body:?}"
        );
    }

    #[test]
    fn code_actions_returns_annotation_suggestion_for_early_return_method() {
        // `count => ^ 42` — inferred Integer via early return; same insertion logic
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");
        let source = "Object subclass: Counter\n  count => ^ 42";
        service.update_file(file.clone(), source.to_string());

        let actions = service.code_actions(&file, 0, len32(source));
        assert_eq!(actions.len(), 1, "expected exactly one code action");

        let action = &actions[0];
        assert!(action.title.contains("Integer"));
        assert_eq!(action.new_text, "-> Integer ");
    }

    #[test]
    fn code_actions_skips_already_annotated_methods() {
        // Explicit annotation — no code action expected
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");
        let source = "Object subclass: Counter\n  count -> Integer => 42";
        service.update_file(file.clone(), source.to_string());

        let actions = service.code_actions(&file, 0, len32(source));
        assert!(
            actions.is_empty(),
            "annotated method should produce no action"
        );
    }

    #[test]
    fn code_actions_skips_methods_outside_requested_range() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");
        let source = "Object subclass: Counter\n  count => 42";
        service.update_file(file.clone(), source.to_string());

        // Request code actions for a range that doesn't overlap the method
        let actions = service.code_actions(&file, 0, 5);
        assert!(
            actions.is_empty(),
            "method outside range should produce no action"
        );
    }

    #[test]
    fn code_actions_cursor_at_method_span_start_matches() {
        // Cursor (start == end) placed exactly at the first byte of the method should match.
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");
        let source = "Object subclass: Counter\n  count => 42";
        service.update_file(file.clone(), source.to_string());

        // "  count => 42" starts at byte 27 (after "Object subclass: Counter\n")
        let method_start = len32("Object subclass: Counter\n  "); // 27
        let actions = service.code_actions(&file, method_start, method_start);
        assert!(
            !actions.is_empty(),
            "cursor at method span start should match (start == end boundary)"
        );
    }

    #[test]
    fn code_actions_cursor_at_method_span_end_does_not_match() {
        // Cursor placed exactly at span.end() (exclusive) should NOT match.
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");
        let source = "Object subclass: Counter\n  count => 42";
        service.update_file(file.clone(), source.to_string());

        let method_end = len32(source); // end of source == end of method span
        let actions = service.code_actions(&file, method_end, method_end);
        assert!(
            actions.is_empty(),
            "cursor at exclusive span end should NOT match"
        );
    }

    #[test]
    fn code_actions_empty_for_unknown_file() {
        let service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("nonexistent.bt");
        let actions = service.code_actions(&file, 0, 100);
        assert!(
            actions.is_empty(),
            "unknown file should return empty actions"
        );
    }

    #[test]
    fn code_actions_returns_annotation_for_keyword_method() {
        // `greet: name => "hello"` — keyword method with known String return
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");
        let source = "Object subclass: Calc\n  greet: name => \"hello\"";
        service.update_file(file.clone(), source.to_string());

        let actions = service.code_actions(&file, 0, len32(source));
        assert_eq!(
            actions.len(),
            1,
            "expected one code action for keyword method"
        );
        assert!(actions[0].title.contains("String"));
        assert_eq!(actions[0].new_text, "-> String ");
    }

    #[test]
    fn code_actions_returns_annotation_for_class_method() {
        // `class answer => 42` — class-side method inferred Integer
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");
        let source = "Object subclass: Calc\n  class answer => 42";
        service.update_file(file.clone(), source.to_string());

        let actions = service.code_actions(&file, 0, len32(source));
        assert_eq!(
            actions.len(),
            1,
            "expected one code action for class method"
        );
        assert!(actions[0].title.contains("Integer"));
    }

    #[test]
    fn find_body_open_offset_finds_fat_arrow() {
        use crate::source_analysis::Span;
        // `count => 42` — the `=>` is at byte 6 within "count => 42"
        let source = "count => 42";
        let span = Span::new(0, len32(source));
        let offset = find_body_open_offset(source, span).expect("should find `=>`");
        assert_eq!(offset, 6, "should point to `=>`");
    }

    #[test]
    fn find_body_open_offset_with_keyword_selector() {
        use crate::source_analysis::Span;
        // keyword method: `add: x => x + 1`
        let source = "add: x => x + 1";
        let span = Span::new(0, len32(source));
        let offset = find_body_open_offset(source, span).expect("should find `=>`");
        assert_eq!(offset, 7, "should point to `=>`");
    }

    #[test]
    fn find_body_open_offset_returns_none_without_fat_arrow() {
        use crate::source_analysis::Span;
        let source = "no body opener here";
        let span = Span::new(0, len32(source));
        assert!(find_body_open_offset(source, span).is_none());
    }

    #[test]
    fn find_references_selector_cross_file_unary() {
        let mut service = SimpleLanguageService::new();
        let file_a = Utf8PathBuf::from("a.bt");
        let file_b = Utf8PathBuf::from("b.bt");

        service.update_file(
            file_a.clone(),
            "Object subclass: Foo\n  bar => 1".to_string(),
        );
        // Cursor on unary selector "bar"
        service.update_file(file_b.clone(), "x bar".to_string());

        let refs = service.find_references(&file_b, Position::new(0, 2));
        assert!(refs.len() >= 2);
        assert!(refs.iter().any(|r| r.file == file_a));
        assert!(refs.iter().any(|r| r.file == file_b));
    }

    // -----------------------------------------------------------------------
    // ADR 0075: NativeTypeRegistry integration
    // -----------------------------------------------------------------------

    #[test]
    fn completions_use_native_type_registry() {
        use crate::semantic_analysis::type_checker::{
            FunctionSignature, InferredType, ParamType, TypeProvenance,
        };

        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");
        service.update_file(file.clone(), "Erlang lists ".to_string());

        // Without registry: completions should still work (untyped)
        let completions = service.completions(&file, Position::new(0, 13));
        let reverse_untyped = completions.iter().find(|c| c.label == "reverse:");
        assert!(
            reverse_untyped.is_some(),
            "Should find reverse: without registry"
        );

        // With registry: completions should show typed signatures
        let mut registry = NativeTypeRegistry::new();
        registry.register_module(
            "lists",
            vec![FunctionSignature {
                name: "reverse".to_string(),
                arity: 1,
                params: vec![ParamType {
                    keyword: Some(ecow::EcoString::from("list")),
                    type_: InferredType::known("List"),
                }],
                return_type: InferredType::known("List"),
                provenance: TypeProvenance::Extracted,
                line: None,
            }],
        );
        service.set_native_types(registry);

        let completions = service.completions(&file, Position::new(0, 13));
        let reverse_typed = completions.iter().find(|c| c.label == "reverse:");
        assert!(
            reverse_typed.is_some(),
            "Should find reverse: with registry"
        );
        let detail = reverse_typed.unwrap().detail.as_deref().unwrap_or("");
        assert_eq!(
            detail, "reverse: list :: List -> List",
            "Should show typed signature from NativeTypeRegistry"
        );
    }

    #[test]
    fn set_and_get_native_types() {
        let mut service = SimpleLanguageService::new();
        assert!(service.native_types().is_none());

        service.set_native_types(NativeTypeRegistry::new());
        assert!(service.native_types().is_some());
    }
}
