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
pub mod runtime_delegate;
mod value_objects;

// Re-export value objects at the module level
pub use project_index::ProjectIndex;
pub use runtime_delegate::{
    NavQuery, NavQueryResponse, NavSite, NavSymbolClass, NavSymbolMethod, NavSymbolsResponse,
    RuntimeLocation, line_to_position, nav_site_to_location,
};
pub use value_objects::{
    ByteOffset, CallHierarchyTarget, CodeAction, Completion, CompletionKind, Diagnostic,
    DocumentSymbol, DocumentSymbolKind, HoverInfo, Location, ParameterInfo, Position,
    SignatureHelp, SignatureInfo,
};

// Property-based tests for language service operations (ADR 0011 Phase 2)
#[cfg(test)]
mod property_tests;

use crate::ast::{
    Expression, Identifier, MessageSelector, MethodDefinition, Module, Pattern, TypeAnnotation,
};
use crate::semantic_analysis::type_checker::NativeTypeRegistry;
use crate::source_analysis::{Lexer, Span, Token, TokenKind};
use camino::Utf8PathBuf;
use ecow::EcoString;
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
    native_types: Option<std::sync::Arc<NativeTypeRegistry>>,
    /// Whether workspace preload has completed with full coverage (BT-2796).
    ///
    /// When `true`, diagnostics are computed with
    /// `KnowledgeScope::ProjectComplete` — the `ProjectIndex` holds every
    /// project file's classes, so the receiver-knowledge classifier may
    /// treat missing parents as genuinely unresolved rather than
    /// not-yet-seen. Set by the LSP server after `preload_workspace_source_files`
    /// finishes within its file budget; stays `false` if the budget was
    /// exhausted (coverage would be partial).
    project_complete: bool,
    /// Whether the workspace has package dependencies (BT-2794 pre-WS3 guard).
    has_package_dependencies: bool,
    /// Per-category diagnostic severity overrides from the workspace's
    /// `beamtalk.toml` `[diagnostics]` section (ADR 0100 Rule 3, BT-2800).
    /// Empty (the default) preserves today's Rule 1 completeness-ladder
    /// defaults — the same behaviour as before this field existed. Set by
    /// the LSP server after loading `beamtalk.toml` from each workspace
    /// root, so the LSP agrees with `beamtalk build` on diagnostic severity.
    diagnostics_overrides: crate::compilation::diagnostics_policy::DiagnosticsTable,
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
            project_complete: false,
            has_package_dependencies: false,
            diagnostics_overrides: crate::compilation::diagnostics_policy::DiagnosticsTable::new(),
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
            project_complete: false,
            has_package_dependencies: false,
            diagnostics_overrides: crate::compilation::diagnostics_policy::DiagnosticsTable::new(),
        }
    }

    /// Declare that workspace preload completed with full coverage (BT-2796).
    ///
    /// After this, diagnostics run with `KnowledgeScope::ProjectComplete`.
    /// Only call when the preload walked every project source file — a
    /// budget-exhausted preload must NOT claim completeness, or the
    /// receiver-knowledge classifier would mistake unseen files for
    /// genuinely-unresolved classes.
    pub fn set_project_complete(&mut self, complete: bool) {
        self.project_complete = complete;
    }

    /// Declare whether the workspace has package dependencies (BT-2794).
    ///
    /// Pre-WS3, dependency extension contributions are invisible, so when
    /// true (and the project is complete) the receiver-knowledge classifier
    /// keeps every receiver `Open`.
    pub fn set_has_package_dependencies(&mut self, has_deps: bool) {
        self.has_package_dependencies = has_deps;
    }

    /// Sets the `[diagnostics]` severity-override table loaded from the
    /// workspace's `beamtalk.toml` (ADR 0100 Rule 3, BT-2800).
    ///
    /// Called by the LSP server once per workspace root after loading and
    /// parsing `beamtalk.toml`. An empty table (the default) is a no-op —
    /// diagnostics keep today's Rule 1 completeness-ladder severities.
    pub fn set_diagnostics_overrides(
        &mut self,
        table: crate::compilation::diagnostics_policy::DiagnosticsTable,
    ) {
        self.diagnostics_overrides = table;
    }

    /// Sets the native type registry for Erlang FFI typed completions.
    ///
    /// ADR 0075 Phase 1: When set, `Erlang <module>` completions display
    /// typed signatures (e.g., `reverse: list :: List -> List`) instead of
    /// just arity information.
    pub fn set_native_types(&mut self, registry: NativeTypeRegistry) {
        self.native_types = Some(std::sync::Arc::new(registry));
    }

    /// Returns a reference to the native type registry, if set.
    #[must_use]
    pub fn native_types(&self) -> Option<&NativeTypeRegistry> {
        self.native_types.as_deref()
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
                .as_deref()
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

    /// BT-2243: Classify the cursor for a
    /// `textDocument/prepareCallHierarchy` request and return a
    /// [`CallHierarchyTarget`], or `None` when the cursor is not on a
    /// recognisable method symbol.
    ///
    /// The classifier is intentionally cold-file: it walks only the open
    /// document's AST. The LSP layer turns the target into a
    /// `CallHierarchyItem`; the `incomingCalls` / `outgoingCalls` follow-ups
    /// use that item to dispatch — incoming via the existing `nav-query`
    /// `SendersOf` path, outgoing via a body walk anchored on this hit.
    ///
    /// Returns:
    /// * `Some(CallHierarchyTarget)` when the cursor is on a selector token
    ///   (call site or method-definition header). When the hit lands on a
    ///   method-definition header we additionally fill in the enclosing
    ///   class name and class-side flag so outgoing-calls can locate the
    ///   method body deterministically.
    /// * `None` when the cursor is on a local identifier, whitespace, or
    ///   any non-selector shape — the editor will fall back to no call
    ///   hierarchy.
    #[must_use]
    pub fn call_hierarchy_prepare_at(
        &self,
        file: &Utf8PathBuf,
        position: Position,
    ) -> Option<CallHierarchyTarget> {
        let file_data = self.files.get(file)?;
        let offset = position.to_byte_offset(&file_data.source)?;
        let offset_val = offset.get();

        // 1. Method-definition header hit — most informative because we can
        //    fill in the enclosing class and class-side flag.
        for class in &file_data.module.classes {
            for method in &class.methods {
                if Self::offset_in_method_header_selector(method, &file_data.source, offset_val) {
                    let selection = Self::method_header_selection_span(method, &file_data.source);
                    return Some(CallHierarchyTarget::new(
                        method.selector.name(),
                        Some(class.name.name.clone()),
                        false,
                        file.clone(),
                        method.span,
                        selection,
                    ));
                }
            }
            for method in &class.class_methods {
                if Self::offset_in_method_header_selector(method, &file_data.source, offset_val) {
                    let selection = Self::method_header_selection_span(method, &file_data.source);
                    return Some(CallHierarchyTarget::new(
                        method.selector.name(),
                        Some(class.name.name.clone()),
                        true,
                        file.clone(),
                        method.span,
                        selection,
                    ));
                }
            }
        }
        for smd in &file_data.module.method_definitions {
            if Self::offset_in_method_header_selector(&smd.method, &file_data.source, offset_val) {
                let selection = Self::method_header_selection_span(&smd.method, &file_data.source);
                return Some(CallHierarchyTarget::new(
                    smd.method.selector.name(),
                    Some(smd.class_name.name.clone()),
                    smd.is_class_method,
                    file.clone(),
                    smd.method.span,
                    selection,
                ));
            }
        }

        // 2. Call-site selector hit — no enclosing class on the *send*
        //    (the receiver class is dynamic), but the selector alone is
        //    enough to drive `sendersOf:` for incoming calls. Outgoing
        //    calls from a call-site target are not meaningful (we have no
        //    method body to walk), so the LSP layer returns `[]` in that
        //    case rather than picking the wrong method.
        if let Some(selector_lookup) = Self::find_selector_at_offset(&file_data.module, offset_val)
        {
            return Some(CallHierarchyTarget::new(
                selector_lookup.selector_name,
                None,
                false,
                file.clone(),
                selector_lookup.selector_span,
                selector_lookup.selector_span,
            ));
        }

        None
    }

    /// BT-2243: find every *call site* of `selector_name` across every
    /// indexed file — sends only, no method-definition headers.
    ///
    /// Used by the LSP `callHierarchy/incomingCalls` cold-file fallback
    /// when no runtime is attached or the `delegateToRuntime` flag is off.
    /// The runtime-attached path goes through `nav-query` `senders` and
    /// the `beamtalk_xref` index instead.
    ///
    /// Unlike [`LanguageService::find_references`] (which intentionally
    /// also surfaces method-definition headers for the
    /// `textDocument/references` UI), this walker excludes definitions so
    /// the editor doesn't list the method's own header as an incoming
    /// call to itself.
    #[must_use]
    pub fn find_selector_send_sites_across_files(&self, selector_name: &str) -> Vec<Location> {
        crate::queries::references_provider::find_selector_send_sites(
            selector_name,
            self.files.iter().map(|(path, data)| (path, &data.module)),
        )
    }

    /// The selector-token span of a method-definition header, used as the
    /// `selection_range` on `CallHierarchyItem`. Delegates to
    /// [`Self::method_header_selector_span`] for the exact span, falling
    /// back to the whole `method.span` in the (should-not-happen) case
    /// where the header text can't be tokenized.
    fn method_header_selection_span(method: &MethodDefinition, source: &str) -> Span {
        Self::method_header_selector_span(method, source).unwrap_or(method.span)
    }

    /// BT-2239: Classify the cursor for a Find-References request and
    /// return the equivalent [`NavQuery`], or `None` when the cursor is
    /// on a local identifier that can't be expressed as a runtime
    /// navigation query.
    ///
    /// Public so `beamtalk-lsp` can build runtime-delegate requests with
    /// the *same* classification logic the AST walker uses — keeping the
    /// two modes in lockstep is a foundational invariant of the epic
    /// (BT-2215).
    ///
    /// Returns:
    /// * `Some(NavQuery::SendersOf(selector))` when the cursor is on a
    ///   selector token (call site or method header).
    /// * `Some(NavQuery::ReferencesTo(class_name))` when the cursor is on
    ///   a class-name identifier known to the project hierarchy.
    /// * `None` for local-identifier references (parameters, locals) and
    ///   any other shape the runtime can't resolve.
    #[must_use]
    pub fn references_query_at(&self, file: &Utf8PathBuf, position: Position) -> Option<NavQuery> {
        let file_data = self.files.get(file)?;
        let offset = position.to_byte_offset(&file_data.source)?;

        if let Some(selector_lookup) =
            Self::find_selector_at_offset(&file_data.module, offset.get())
        {
            return Some(NavQuery::SendersOf(selector_lookup.selector_name));
        }
        if let Some(selector_name) = Self::find_method_header_selector_at_offset(
            &file_data.module,
            &file_data.source,
            offset.get(),
        ) {
            return Some(NavQuery::SendersOf(selector_name));
        }
        let (ident, _span) = self.find_identifier_at_position(file, position)?;
        if self.project_index.hierarchy().has_class(&ident.name) {
            return Some(NavQuery::ReferencesTo(ident.name));
        }
        None
    }

    /// BT-2240: Find the **declaration sites** (method-definition headers)
    /// for the given selector across every indexed file.
    ///
    /// Used by the LSP to overlay declarations onto runtime-attached
    /// `textDocument/references` results when `includeDeclaration = true`.
    /// The runtime path (`SystemNavigation sendersOf:`) returns call sites
    /// only; merging this helper's output back in keeps the runtime path
    /// at parity with the cold-file AST walker, which has always returned
    /// both.
    ///
    /// Polymorphic selectors (defined by multiple classes) return one
    /// `Location` per defining class.
    #[must_use]
    pub fn find_selector_declarations(&self, selector_name: &str) -> Vec<Location> {
        crate::queries::references_provider::find_selector_declarations(
            selector_name,
            self.files.iter().map(|(path, data)| (path, &data.module)),
        )
    }

    /// BT-2240: Find the **class declaration sites** (the class-name token
    /// at the definition) for the given class or protocol name across every
    /// indexed file.
    ///
    /// Used by the LSP to overlay class declarations onto runtime-attached
    /// `textDocument/references` results when `includeDeclaration = true`.
    /// The runtime path (`SystemNavigation referencesTo:`) returns sites
    /// that *use* the class (type annotations, class literals, …) but
    /// never the declaration site itself; merging this helper's output
    /// back in keeps the runtime path at parity with the cold-file walker.
    ///
    /// In normal projects each class is declared once. The result is a
    /// `Vec` to cover legitimate cases where the same name is declared in
    /// multiple files (test fixtures, overlapping workspace roots).
    #[must_use]
    pub fn find_class_declarations(&self, class_name: &str) -> Vec<Location> {
        crate::queries::references_provider::find_class_declarations(
            class_name,
            self.files.iter().map(|(path, data)| (path, &data.module)),
        )
    }

    /// BT-2241: Classify the cursor for a `textDocument/implementation`
    /// request and return the equivalent [`NavQuery::ImplementorsOf`], or
    /// `None` when the cursor is not on a selector.
    ///
    /// Public so `beamtalk-lsp` builds the runtime-delegate request with
    /// the *same* classification logic the AST walker uses — keeping the
    /// runtime and cold-file modes in lockstep is a foundational invariant
    /// of the epic (BT-2215).
    ///
    /// Unlike [`Self::references_query_at`], goto-implementation only
    /// resolves selectors (not class names) — "implementations of a class"
    /// would mean its subclasses, which `SystemNavigation` exposes via a
    /// different selector (`subclassesOf:`) and is out of scope for this
    /// LSP capability.
    ///
    /// Returns:
    /// * `Some(NavQuery::ImplementorsOf(selector))` when the cursor is on
    ///   a selector token (call site or method header).
    /// * `None` for any other shape (class names, locals, parameters, ...).
    #[must_use]
    pub fn implementors_query_at(
        &self,
        file: &Utf8PathBuf,
        position: Position,
    ) -> Option<NavQuery> {
        let file_data = self.files.get(file)?;
        let offset = position.to_byte_offset(&file_data.source)?;

        if let Some(selector_lookup) =
            Self::find_selector_at_offset(&file_data.module, offset.get())
        {
            return Some(NavQuery::ImplementorsOf(selector_lookup.selector_name));
        }
        if let Some(selector_name) = Self::find_method_header_selector_at_offset(
            &file_data.module,
            &file_data.source,
            offset.get(),
        ) {
            return Some(NavQuery::ImplementorsOf(selector_name));
        }
        None
    }

    /// BT-2241: Find every class that defines `selector_name` across all
    /// indexed files. Mirrors `SystemNavigation default implementorsOf:`
    /// semantics — local definitions only, both instance- and class-side.
    ///
    /// This is the cold-file AST fallback for `textDocument/implementation`.
    /// The LSP wraps it in [`Backend::delegate_nav_query`] so the runtime
    /// path is preferred when attached (and sees live patches, stdlib
    /// classes, ADR-0066 extension methods the walker can't index).
    ///
    /// Returns method-definition locations whose `span` is the full
    /// `MethodDefinition::span` — LSP callers that want only the header
    /// line collapse it to `span.start()`.
    #[must_use]
    pub fn find_implementors(&self, selector_name: &str) -> Vec<Location> {
        crate::queries::implementors_provider::find_implementors(
            selector_name,
            self.files.iter().map(|(path, data)| (path, &data.module)),
        )
    }

    /// BT-2242: Classify the cursor for a `textDocument/prepareTypeHierarchy`
    /// request and return the class name plus its declaration site, or
    /// `None` when the cursor is not on a known class name.
    ///
    /// "Class name" means any identifier whose token matches a class in the
    /// merged project hierarchy (`ProjectIndex::hierarchy`). That picks up
    /// the class header (`Foo subclass: Bar` — both names), superclass
    /// references in other classes, type annotations, and class-literal
    /// expressions inside method bodies.
    ///
    /// Returns the `(class_name, declaration_location)` pair. The
    /// declaration location is the *class header span* — the name of the
    /// declared class in its own definition (matches `definition_provider`'s
    /// class-name lookup). When no source file backs the class (built-in /
    /// stdlib precompiled, or an injected `add_from_beam_meta` entry), the
    /// location is `None` and consumers fall back to "name only" presentation.
    ///
    /// The classification is selector-aware: clicks on a method selector
    /// token that happens to be capitalised do not match here — selector
    /// classification ([`Self::implementors_query_at`]) already covers
    /// those.
    #[must_use]
    pub fn type_hierarchy_prepare_at(
        &self,
        file: &Utf8PathBuf,
        position: Position,
    ) -> Option<(EcoString, Option<Location>)> {
        // Re-use the identifier walker the references handler uses — it
        // already rejects selectors and parameter names, and returns the
        // identifier *and* its span at the cursor.
        let (ident, _span) = self.find_identifier_at_position(file, position)?;
        if !self.project_index.hierarchy().has_class(&ident.name) {
            return None;
        }
        let class_name = ident.name.clone();
        let declaration = self.find_class_declaration_location(class_name.as_str());
        Some((class_name, declaration))
    }

    /// BT-2242: Find the declaration site of `class_name`, scanning every
    /// indexed file for a `ClassDefinition` whose name matches.
    ///
    /// The returned [`Location::span`] is the *class-name span* (e.g. the
    /// `Counter` token in `Actor subclass: Counter`), matching
    /// `definition_provider` and what LSP `selectionRange` / `range`
    /// expects for `TypeHierarchyItem`.
    ///
    /// Returns `None` for builtin / runtime-only classes whose source file
    /// the language service hasn't indexed (the editor still shows the
    /// item via the name returned from
    /// [`Self::type_hierarchy_prepare_at`]; navigation just doesn't have a
    /// jump target).
    ///
    /// BT-2317: resolution delegates to
    /// [`definition_provider::find_class_or_protocol_declaration`], the same
    /// helper goto-definition uses, so the BT-1933 protocol/class shadowing
    /// rule applies on this path too. When a name is defined as both a real
    /// class and a synthetic protocol (across separate files), the real class
    /// wins; a protocol declaration is only returned when the hierarchy marks
    /// the name as a protocol class — so `prepareTypeHierarchy` on a protocol
    /// name still lands on the protocol header.
    fn find_class_declaration_location(&self, class_name: &str) -> Option<Location> {
        crate::queries::definition_provider::find_class_or_protocol_declaration(
            class_name,
            &self.project_index,
            self.files.iter().map(|(path, data)| (path, &data.module)),
        )
    }

    /// BT-2242: Resolve the supertype chain of `class_name` to declaration
    /// locations. Mirrors `Behaviour superclassChain` (the stdlib query
    /// `typeHierarchy/supertypes` is wired to) — returns the names paired
    /// with their declaration site when one exists in the indexed corpus.
    ///
    /// `class_name` is excluded (consistent with `superclass_chain` itself).
    /// Entries whose source file is not indexed return `(name, None)` so
    /// the LSP layer can still emit a `TypeHierarchyItem` for the name and
    /// let the editor present "no jump target" rather than dropping the row.
    #[must_use]
    pub fn supertypes_of(&self, class_name: &str) -> Vec<(EcoString, Option<Location>)> {
        self.project_index
            .hierarchy()
            .superclass_chain(class_name)
            .into_iter()
            .map(|name| {
                let loc = self.find_class_declaration_location(name.as_str());
                (name, loc)
            })
            .collect()
    }

    /// BT-2242: Resolve all transitive subtypes of `class_name`. Mirrors
    /// `Behaviour allSubclasses` (the stdlib query
    /// `typeHierarchy/subtypes` is wired to) — returns the names paired
    /// with their declaration site when one exists in the indexed corpus.
    ///
    /// Order matches `ClassHierarchy::all_subclasses`: BFS over the
    /// inheritance tree, so direct children precede grandchildren. The
    /// receiver itself is not included.
    #[must_use]
    pub fn subtypes_of(&self, class_name: &str) -> Vec<(EcoString, Option<Location>)> {
        self.project_index
            .hierarchy()
            .all_subclasses(class_name)
            .into_iter()
            .map(|name| {
                let loc = self.find_class_declaration_location(name.as_str());
                (name, loc)
            })
            .collect()
    }

    /// If the offset falls inside the header of a method *definition*
    /// (the selector area of `bar => ...`, `+ other => ...`, or
    /// `at: i put: v => ...`), returns the method's selector name.
    ///
    /// Returns `None` if the offset is inside a method body, a parameter
    /// name, a parameter/return type annotation, or outside any method.
    ///
    /// This powers "Find All References" (and, indirectly, rename) when the
    /// user's cursor is on the selector of the method definition itself —
    /// not on a call site.
    ///
    /// `source` is the full file text `method`'s spans are offsets into;
    /// see [`Self::offset_in_method_header_selector`] for why it's needed.
    fn find_method_header_selector_at_offset(
        module: &Module,
        source: &str,
        offset: u32,
    ) -> Option<EcoString> {
        for class in &module.classes {
            for method in class.methods.iter().chain(class.class_methods.iter()) {
                if Self::offset_in_method_header_selector(method, source, offset) {
                    return Some(method.selector.name());
                }
            }
        }
        for smd in &module.method_definitions {
            if Self::offset_in_method_header_selector(&smd.method, source, offset) {
                return Some(smd.method.selector.name());
            }
        }
        None
    }

    /// Checks whether `offset` lies on the selector of a method definition
    /// header.
    ///
    /// # Precision contract
    ///
    /// For keyword selectors we have precise per-keyword spans and only
    /// accept offsets that fall within one of those keyword parts — note
    /// this deliberately does *not* go through
    /// [`Self::method_header_selector_span`], whose merged first/last span
    /// would also cover the parameter names *between* keyword parts (e.g.
    /// the `i` in `at: i put: v`).
    ///
    /// For unary and binary selectors, `MessageSelector` carries no span for
    /// the selector token itself, so we recover it by scanning the header
    /// text with [`Self::method_header_selector_span`] (BT-1941). The result
    /// is exact: clicks on modifiers (`sealed` / `internal` / `class`),
    /// `->` punctuation, whitespace, parameter names/types, return types,
    /// and body expressions are all rejected — only the selector token
    /// itself matches.
    fn offset_in_method_header_selector(
        method: &MethodDefinition,
        source: &str,
        offset: u32,
    ) -> bool {
        if offset < method.span.start() || offset >= method.span.end() {
            return false;
        }

        match &method.selector {
            MessageSelector::Keyword(parts) => parts
                .iter()
                .any(|part| offset >= part.span.start() && offset < part.span.end()),
            MessageSelector::Unary(_) | MessageSelector::Binary(_) => {
                Self::method_header_selector_span(method, source)
                    .is_some_and(|span| offset >= span.start() && offset < span.end())
            }
        }
    }

    /// Computes the exact source span of a method definition's selector
    /// token(s) (BT-1941). Used for the `selection_range` on
    /// `CallHierarchyItem` ([`Self::method_header_selection_span`]) and, for
    /// unary/binary selectors, for the offset containment check in
    /// [`Self::offset_in_method_header_selector`].
    ///
    /// For keyword selectors this merges the precise per-part spans already
    /// carried on `MessageSelector::Keyword` — a *display* span suitable for
    /// highlighting the whole selector, not a containment check (see the
    /// doc comment on [`Self::offset_in_method_header_selector`] for why).
    ///
    /// For unary and binary selectors, which carry no span of their own,
    /// this re-tokenizes the header text — from `method.span.start()` up to
    /// the first parameter, return-type annotation, or body element — and
    /// skips leading modifier keyword tokens exactly as the parser's own
    /// modifier loop (`parse_method_definition`) does, returning the span of
    /// the first token that the parser would treat as the selector.
    ///
    /// The skip decision mirrors the parser token-for-token rather than
    /// counting `method`'s modifier flags, because the flags record only
    /// *which* modifiers are present, not *how many* tokens were consumed:
    /// the parser accepts repeated modifiers (`sealed sealed bar => 1`,
    /// `internal internal bar => 1`) — each sets the same boolean once but
    /// consumes a distinct token — so a flag-count-driven skip would stop
    /// short and mis-identify a second modifier keyword as the selector.
    /// Matching the parser's per-token rule instead:
    ///
    /// - `sealed` is *always* a modifier (the parser consumes it
    ///   unconditionally), so a method can never be named `sealed`.
    /// - `class` / `internal` are modifiers *unless* the next token is `=>`,
    ///   `->`, or `::` — the parser's `is_fat_arrow_or_return_type`
    ///   lookahead, which resolves `class => ...` (and `class -> T => ...`,
    ///   `class :: -> T => ...`) in favour of "this is the selector named
    ///   `class`". So `class => 1` returns the `class` token as the selector
    ///   and is never mistaken for the `class` modifier.
    ///
    /// Returns `None` only if the header text can't be tokenized down to a
    /// selector-shaped token, which should not happen for a
    /// `MethodDefinition` produced by parsing this exact `source`.
    fn method_header_selector_span(method: &MethodDefinition, source: &str) -> Option<Span> {
        match &method.selector {
            MessageSelector::Keyword(parts) => {
                let first = parts.first()?.span;
                let last = parts.last()?.span;
                Some(first.merge(last))
            }
            MessageSelector::Unary(_) | MessageSelector::Binary(_) => {
                let start = method.span.start();

                // The header ends at the first of: first parameter name,
                // parameter type annotation, return type annotation, or
                // first body statement.
                let mut header_end = method.span.end();
                if let Some(first_param) = method.parameters.first() {
                    header_end = header_end.min(first_param.name.span.start());
                    if let Some(ta) = &first_param.type_annotation {
                        header_end = header_end.min(ta.span().start());
                    }
                }
                if let Some(rt) = &method.return_type {
                    header_end = header_end.min(rt.span().start());
                }
                if let Some(first_stmt) = method.body.first() {
                    header_end = header_end.min(first_stmt.expression.span().start());
                }

                let header_text = source.get(start as usize..header_end as usize)?;

                // Re-tokenize the header and walk it exactly as the parser's
                // modifier loop does (see doc comment above). `tokens` holds
                // the meaningful tokens (whitespace is trivia; the iterator
                // excludes EOF), so index `i + 1` peeks the next token.
                let tokens: Vec<_> = Lexer::new(header_text).collect();
                let mut i = 0;
                while let Some(token) = tokens.get(i) {
                    let is_modifier = match token.kind() {
                        // `sealed` is unconditionally a modifier.
                        TokenKind::Identifier(name) if name.as_str() == "sealed" => true,
                        // `class` / `internal` are modifiers unless the next
                        // token marks them as the selector (`=>`, `->`, `::`).
                        TokenKind::Identifier(name)
                            if matches!(name.as_str(), "class" | "internal") =>
                        {
                            !matches!(
                                tokens.get(i + 1).map(Token::kind),
                                Some(
                                    TokenKind::FatArrow | TokenKind::Arrow | TokenKind::DoubleColon
                                )
                            )
                        }
                        _ => false,
                    };
                    if is_modifier {
                        i += 1;
                        continue;
                    }
                    let token_span = token.span();
                    return Some(Span::new(
                        token_span.start() + start,
                        token_span.end() + start,
                    ));
                }
                None
            }
        }
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

        for class in &file_data.module.classes {
            if let Some(ident) = Self::find_identifier_in_class(class, offset, offset_val) {
                return Some(ident);
            }
        }

        // BT-1936: Check protocol definitions (name, extending, type-param bounds, method sigs)
        for protocol in &file_data.module.protocols {
            if let Some(ident) = Self::find_identifier_in_protocol(protocol, offset_val) {
                return Some(ident);
            }
        }

        for smd in &file_data.module.method_definitions {
            if let Some(ident) = Self::find_identifier_in_standalone_method(smd, offset, offset_val)
            {
                return Some(ident);
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

    /// Walk a class definition looking for an identifier at the given offset.
    ///
    /// Covers: class name, superclass, type-parameter bounds (BT-1936), state /
    /// class-variable type annotations, and method bodies (parameters, return
    /// type, body statements).
    fn find_identifier_in_class(
        class: &crate::ast::ClassDefinition,
        offset: ByteOffset,
        offset_val: u32,
    ) -> Option<(Identifier, Span)> {
        if offset_val >= class.name.span.start() && offset_val < class.name.span.end() {
            return Some((class.name.clone(), class.name.span));
        }
        if let Some(ref superclass) = class.superclass {
            if offset_val >= superclass.span.start() && offset_val < superclass.span.end() {
                return Some((superclass.clone(), superclass.span));
            }
        }
        if let Some(ident) = Self::find_identifier_in_type_params(&class.type_params, offset_val) {
            return Some(ident);
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
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            if let Some(ident) = Self::find_identifier_in_method_signature_and_body(
                &method.parameters,
                method.return_type.as_ref(),
                &method.body,
                offset,
                offset_val,
            ) {
                return Some(ident);
            }
        }
        None
    }

    /// Walk a protocol definition looking for an identifier at the given offset.
    ///
    /// Covers: protocol name, `extending:` target, type-parameter bounds, and
    /// method signature type annotations (both instance and class-side). (BT-1936)
    fn find_identifier_in_protocol(
        protocol: &crate::ast::ProtocolDefinition,
        offset_val: u32,
    ) -> Option<(Identifier, Span)> {
        if offset_val >= protocol.name.span.start() && offset_val < protocol.name.span.end() {
            return Some((protocol.name.clone(), protocol.name.span));
        }
        if let Some(ref extending) = protocol.extending {
            if offset_val >= extending.span.start() && offset_val < extending.span.end() {
                return Some((extending.clone(), extending.span));
            }
        }
        if let Some(ident) = Self::find_identifier_in_type_params(&protocol.type_params, offset_val)
        {
            return Some(ident);
        }
        for sig in protocol
            .method_signatures
            .iter()
            .chain(protocol.class_method_signatures.iter())
        {
            for parameter in &sig.parameters {
                if let Some(type_annotation) = &parameter.type_annotation {
                    if let Some(ident) =
                        Self::find_identifier_in_type_annotation(type_annotation, offset_val)
                    {
                        return Some(ident);
                    }
                }
            }
            if let Some(return_type) = &sig.return_type {
                if let Some(ident) =
                    Self::find_identifier_in_type_annotation(return_type, offset_val)
                {
                    return Some(ident);
                }
            }
        }
        None
    }

    /// Walk a Tonel-style standalone method definition for an identifier at `offset`.
    fn find_identifier_in_standalone_method(
        smd: &crate::ast::StandaloneMethodDefinition,
        offset: ByteOffset,
        offset_val: u32,
    ) -> Option<(Identifier, Span)> {
        if offset_val >= smd.class_name.span.start() && offset_val < smd.class_name.span.end() {
            return Some((smd.class_name.clone(), smd.class_name.span));
        }
        Self::find_identifier_in_method_signature_and_body(
            &smd.method.parameters,
            smd.method.return_type.as_ref(),
            &smd.method.body,
            offset,
            offset_val,
        )
    }

    /// Walk a method's parameter types, return type, and body for an identifier
    /// at the given offset. Shared by class methods and standalone methods.
    fn find_identifier_in_method_signature_and_body(
        parameters: &[crate::ast::ParameterDefinition],
        return_type: Option<&TypeAnnotation>,
        body: &[crate::ast::ExpressionStatement],
        offset: ByteOffset,
        offset_val: u32,
    ) -> Option<(Identifier, Span)> {
        for parameter in parameters {
            if let Some(type_annotation) = &parameter.type_annotation {
                if let Some(ident) =
                    Self::find_identifier_in_type_annotation(type_annotation, offset_val)
                {
                    return Some(ident);
                }
            }
        }
        if let Some(return_type) = return_type {
            if let Some(ident) = Self::find_identifier_in_type_annotation(return_type, offset_val) {
                return Some(ident);
            }
        }
        for stmt in body {
            if let Some(ident) = Self::find_identifier_in_expr(&stmt.expression, offset) {
                return Some(ident);
            }
        }
        None
    }

    /// Check a list of `TypeParamDecl` for an identifier at the offset.
    ///
    /// Returns the protocol bound identifier when the cursor is on the bound,
    /// e.g., `Printable` in `Logger(T :: Printable)` or `Mapper(T :: Printable)`.
    /// The parameter name itself (e.g., `T`) is intentionally not returned —
    /// type-parameter names are local to their declaration and have no global
    /// definition to navigate to. (BT-1936)
    fn find_identifier_in_type_params(
        type_params: &[crate::ast::TypeParamDecl],
        offset_val: u32,
    ) -> Option<(Identifier, Span)> {
        for param in type_params {
            if let Some(bound) = &param.bound {
                if offset_val >= bound.span.start() && offset_val < bound.span.end() {
                    return Some((bound.clone(), bound.span));
                }
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
            TypeAnnotation::Difference { base, excluded, .. } => {
                Self::find_identifier_in_type_annotation(base, offset_val)
                    .or_else(|| Self::find_identifier_in_type_annotation(excluded, offset_val))
            }
            TypeAnnotation::Intersection { left, right, .. } => {
                Self::find_identifier_in_type_annotation(left, offset_val)
                    .or_else(|| Self::find_identifier_in_type_annotation(right, offset_val))
            }
            TypeAnnotation::ClassOf { class_name, .. } => {
                if offset_val >= class_name.span.start() && offset_val < class_name.span.end() {
                    Some((class_name.clone(), class_name.span))
                } else {
                    None
                }
            }
            TypeAnnotation::Singleton { .. }
            | TypeAnnotation::SelfType { .. }
            | TypeAnnotation::SelfClass { .. } => None,
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
                    arms.iter().find_map(|arm| {
                        // BT-1940: walk pattern, guard, and body so that
                        // goto-definition on a class name inside a constructor
                        // pattern (`Result ok: v`) or inside a `when:` guard
                        // navigates to the class declaration. Keeps parity with
                        // `references_provider::collect_class_refs` which
                        // already walks all three.
                        Self::find_identifier_in_pattern(&arm.pattern, offset_val)
                            .or_else(|| {
                                arm.guard
                                    .as_ref()
                                    .and_then(|g| Self::find_identifier_in_expr(g, offset))
                            })
                            .or_else(|| Self::find_identifier_in_expr(&arm.body, offset))
                    })
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

    /// Recursively searches for a class name identifier inside a destructuring
    /// pattern. (BT-1940)
    ///
    /// Only `Pattern::Constructor` carries a class identifier (`Result` in
    /// `Result ok: v`). All other variants just host nested patterns, which we
    /// walk so a cursor on a constructor name nested inside a tuple, list, map,
    /// or rest pattern still finds the class. Wildcards, literals, and plain
    /// variable bindings have no class identifier to navigate to.
    fn find_identifier_in_pattern(
        pattern: &Pattern,
        offset_val: u32,
    ) -> Option<(Identifier, Span)> {
        let span = pattern.span();
        if offset_val < span.start() || offset_val >= span.end() {
            return None;
        }
        match pattern {
            Pattern::Constructor {
                class, keywords, ..
            } => {
                if offset_val >= class.span.start() && offset_val < class.span.end() {
                    return Some((class.clone(), class.span));
                }
                keywords
                    .iter()
                    .find_map(|(_, p)| Self::find_identifier_in_pattern(p, offset_val))
            }
            Pattern::Tuple { elements, .. } => elements
                .iter()
                .find_map(|p| Self::find_identifier_in_pattern(p, offset_val)),
            Pattern::Array { elements, rest, .. } => elements
                .iter()
                .find_map(|p| Self::find_identifier_in_pattern(p, offset_val))
                .or_else(|| {
                    rest.as_deref()
                        .and_then(|r| Self::find_identifier_in_pattern(r, offset_val))
                }),
            Pattern::List { elements, tail, .. } => elements
                .iter()
                .find_map(|p| Self::find_identifier_in_pattern(p, offset_val))
                .or_else(|| {
                    tail.as_deref()
                        .and_then(|t| Self::find_identifier_in_pattern(t, offset_val))
                }),
            Pattern::Map { pairs, .. } => pairs
                .iter()
                .find_map(|pair| Self::find_identifier_in_pattern(&pair.value, offset_val)),
            Pattern::Binary { segments, .. } => segments
                .iter()
                .find_map(|seg| Self::find_identifier_in_pattern(&seg.value, offset_val)),
            Pattern::Type { class, .. } => {
                if offset_val >= class.span.start() && offset_val < class.span.end() {
                    Some((class.clone(), class.span))
                } else {
                    // The binding (e.g. `path` in `path :: String`) is a
                    // local variable, not a class reference — this function
                    // only navigates class identifiers. Go-to-definition on
                    // the binding itself is deferred to BT-2855, which is
                    // when it becomes a fully navigable scope entry.
                    None
                }
            }
            Pattern::Wildcard(_)
            | Pattern::Literal(_, _)
            | Pattern::Variable(_)
            | Pattern::Nil(_) => None,
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
        if let Ok(mut class_hierarchy) = class_hierarchy_result {
            // BT-1933: Register protocol definitions as synthetic class entries
            // so LSP features (completions, has_class) work with protocol names.
            class_hierarchy.register_protocol_classes(&module);

            // BT-2796: A file with parse errors may have an under-recovered
            // method surface (error recovery can drop method definitions).
            // Mark its classes so cross-file consumers of this file's
            // hierarchy never emit unresolved-selector hints against a
            // surface the parser never fully saw.
            let has_parse_errors = diagnostics
                .iter()
                .any(|d| d.severity == crate::source_analysis::Severity::Error);
            if has_parse_errors {
                class_hierarchy.mark_module_classes_surface_incomplete(&module);
            }

            // Update the project-wide index with this file's class hierarchy
            self.project_index
                .update_file(file.clone(), &class_hierarchy);

            // BT-2795: Track this file's standalone extension definitions so
            // other files' diagnostics see them (cross-file extension
            // visibility, ADR 0066 / ADR 0100 Rule 2 WS1).
            let mut extensions = crate::compilation::extension_index::ExtensionIndex::new();
            extensions.add_module(&module, file.as_std_path());
            self.project_index
                .set_file_extensions(file.clone(), extensions);
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
                // BT-2009: Use the unified diagnostic pipeline so that LSP
                // diagnostics match CLI diagnostics. Cross-file classes from
                // the ProjectIndex are passed so type checking, @expect
                // directives, and all post-analysis passes run identically.
                let cross_file_classes = self.project_index.cross_file_class_infos_for(file);
                // BT-2027: Stdlib source files must be analysed with
                // `stdlib_mode = true` so the "conflicts with a stdlib class"
                // shadowing check (BT-738) doesn't flag every class the file
                // legitimately defines.
                let mut options = crate::CompilerOptions::default();
                if self.project_index.is_stdlib_file(file) {
                    options.stdlib_mode = true;
                }
                // BT-2796: After a full-coverage workspace preload the
                // ProjectIndex holds every project file's classes, so the
                // injected knowledge is project-complete.
                if self.project_complete {
                    options.knowledge_scope =
                        crate::semantic_analysis::KnowledgeScope::ProjectComplete;
                }
                options.has_package_dependencies = self.has_package_dependencies;
                // BT-2795: Cross-file extensions from the ProjectIndex are
                // passed so a same-project `ClassName >> selector` defined in
                // another file resolves instead of producing a false Dnu hint.
                let cross_file_extensions = self.project_index.cross_file_extensions_for(file);
                let ctx = crate::queries::diagnostic_provider::ProjectDiagnosticContext {
                    options,
                    cross_file_classes,
                    cross_file_extensions,
                    native_type_registry: self.native_types.clone(),
                    // BT-2800: apply the same `beamtalk.toml` `[diagnostics]`
                    // severity-override table `beamtalk build` uses, so the
                    // LSP never disagrees with the CLI about a diagnostic's
                    // severity.
                    diagnostics_overrides: self.diagnostics_overrides.clone(),
                    ..Default::default()
                };
                crate::queries::diagnostic_provider::compute_project_diagnostics(
                    &data.module,
                    data.diagnostics.clone(),
                    &ctx,
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
            self.native_types.as_deref(),
        )
    }

    fn hover(&self, file: &Utf8PathBuf, position: Position) -> Option<HoverInfo> {
        let file_data = self.get_file(file)?;

        // BT-2897 / ADR 0108: `ProjectIndex` does not yet track a
        // project-wide `AliasRegistry` (that's LSP integration, deferred to
        // the later ADR 0108 phase, BT-2901) — `None` here means an
        // alias-typed value's hover falls back to its bare structural
        // expansion rather than `AliasName (expansion)`, matching pre-BT-2897
        // behaviour until that phase lands.
        crate::queries::hover_provider::compute_hover(
            &file_data.module,
            &file_data.source,
            position,
            self.project_index.hierarchy(),
            self.native_types.as_deref(),
            None,
        )
    }

    fn signature_help(&self, file: &Utf8PathBuf, position: Position) -> Option<SignatureHelp> {
        let file_data = self.get_file(file)?;

        crate::queries::signature_help_provider::compute_signature_help(
            &file_data.module,
            &file_data.source,
            position,
            self.project_index.hierarchy(),
            self.native_types.as_deref(),
        )
    }

    fn goto_definition(&self, file: &Utf8PathBuf, position: Position) -> Option<Location> {
        let file_data = self.get_file(file)?;
        let offset = position.to_byte_offset(&file_data.source)?;

        // 1. Try selector-based go-to-definition (cursor on a method keyword
        //    at a call site — e.g. `x bar`, `a + b`, `x at: 1 put: 2`)
        if let Some(selector_lookup) =
            Self::find_selector_at_offset(&file_data.module, offset.get())
        {
            let receiver_context =
                crate::queries::definition_provider::resolve_receiver_class_context(
                    &file_data.module,
                    offset.get(),
                    &selector_lookup,
                    self.project_index.hierarchy(),
                    self.native_types.as_deref(),
                );
            return crate::queries::definition_provider::find_method_definition_cross_file_with_receiver(
                selector_lookup.selector_name.as_str(),
                receiver_context.as_ref(),
                &self.project_index,
                self.files.iter().map(|(path, data)| (path, &data.module)),
            );
        }

        // 2. Try selector-based go-to-definition at a method *definition
        //    header* (BT-1939): cursor on the selector in `bar => ...`,
        //    `+ other => ...`, or `at: i put: v => ...`. Navigate to the
        //    nearest overridden parent method. This mirrors the header path
        //    added to `find_references` in BT-1938 and reuses its helper.
        //
        //    We use `find_overridden_method_definition` rather than the
        //    general-purpose receiver lookup because the latter has a
        //    global-search fallback that would navigate back to the current
        //    class's own method when no ancestor defines the selector. For
        //    "go to parent" semantics we want the strict MRO-only walk: if
        //    nothing in the ancestors defines this selector, the result is
        //    `None` (matches the no-regression scope in the issue).
        if let Some(selector_name) = Self::find_method_header_selector_at_offset(
            &file_data.module,
            &file_data.source,
            offset.get(),
        ) {
            let receiver_context =
                crate::queries::definition_provider::resolve_enclosing_superclass_context(
                    &file_data.module,
                    offset.get(),
                    self.project_index.hierarchy(),
                )?;
            return crate::queries::definition_provider::find_overridden_method_definition(
                selector_name.as_str(),
                &receiver_context,
                &self.project_index,
                self.files.iter().map(|(path, data)| (path, &data.module)),
            );
        }

        // 3. Try identifier-based go-to-definition (cursor on a name)
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

        // 1. Try selector-based references (cursor on a method keyword in a
        //    call site — e.g. `x bar`, `a + b`, `x at: 1 put: 2`)
        if let Some(selector_lookup) =
            Self::find_selector_at_offset(&file_data.module, offset.get())
        {
            return crate::queries::references_provider::find_selector_references(
                selector_lookup.selector_name.as_str(),
                self.files.iter().map(|(path, data)| (path, &data.module)),
            );
        }

        // 2. Try selector-based references at a method *definition header*
        //    (cursor on the selector in `bar => ...`, `+ other => ...`, or
        //    `at: i put: v => ...`). Without this check, clicking on the name
        //    where a method is defined would fall through to the identifier
        //    path and return nothing.
        if let Some(selector_name) = Self::find_method_header_selector_at_offset(
            &file_data.module,
            &file_data.source,
            offset.get(),
        ) {
            return crate::queries::references_provider::find_selector_references(
                selector_name.as_str(),
                self.files.iter().map(|(path, data)| (path, &data.module)),
            );
        }

        // 3. Try identifier-based references (cursor on a name)
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
            self.native_types.as_deref(),
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
                // BT-2022: inferred map stores InferredType; use
                // `display_for_diagnostic()` so user-facing annotations render
                // source-friendly names (e.g., `Nil` instead of `UndefinedObject`).
                if let Some(inferred_ty) = inferred.get(&key) {
                    let display = inferred_ty
                        .display_for_diagnostic()
                        .unwrap_or_else(|| ecow::EcoString::from("Dynamic"));
                    if let Some(offset) = find_body_open_offset(source, method.span) {
                        actions.push(CodeAction::new(
                            format!("Add annotation: -> {display}"),
                            format!("-> {display} "),
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
            if let Some(inferred_ty) = inferred.get(&key) {
                let display = inferred_ty
                    .display_for_diagnostic()
                    .unwrap_or_else(|| ecow::EcoString::from("Dynamic"));
                if let Some(offset) = find_body_open_offset(source, method.span) {
                    actions.push(CodeAction::new(
                        format!("Add annotation: -> {display}"),
                        format!("-> {display} "),
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

    // --- BT-2243: call hierarchy prepare classifier ---

    /// Cursor on a method-definition header (instance side) populates
    /// selector + class + `class_side=false`.
    #[test]
    fn call_hierarchy_prepare_at_method_header_instance() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("counter.bt");
        // Line 2, column 2: `increment` selector token in a method header.
        let source = "Object subclass: Counter\n  increment => 1\n";
        service.update_file(file.clone(), source.to_string());

        let target = service
            .call_hierarchy_prepare_at(&file, Position::new(1, 2))
            .expect("prepare hit on method header");
        assert_eq!(target.selector, "increment");
        assert_eq!(target.class_name.as_deref(), Some("Counter"));
        assert!(!target.class_side);
        assert_eq!(target.file, file);
    }

    /// Cursor on a class-method header populates `class_side=true`.
    #[test]
    fn call_hierarchy_prepare_at_method_header_class_side() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("counter.bt");
        let source = "Object subclass: Counter\n  class make => self new\n";
        service.update_file(file.clone(), source.to_string());

        // Column 8: inside the `make` selector on the class-method header.
        let target = service
            .call_hierarchy_prepare_at(&file, Position::new(1, 8))
            .expect("prepare hit on class-method header");
        assert_eq!(target.selector, "make");
        assert_eq!(target.class_name.as_deref(), Some("Counter"));
        assert!(target.class_side);
    }

    /// Cursor on a call-site selector populates selector but leaves
    /// `class_name` empty — the receiver class is dynamic.
    #[test]
    fn call_hierarchy_prepare_at_call_site_has_no_class_context() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("counter.bt");
        let source = "Object subclass: Counter\n  greet => self name asString\n";
        service.update_file(file.clone(), source.to_string());

        // Column 23: inside `asString` at the call site
        // (2-space indent + `greet => self name ` = 21 chars; column 23
        // lands on the third letter of `asString`).
        let target = service
            .call_hierarchy_prepare_at(&file, Position::new(1, 23))
            .expect("prepare hit on call site");
        assert_eq!(target.selector, "asString");
        assert!(target.class_name.is_none());
    }

    /// Cursor on whitespace / non-selector returns None.
    #[test]
    fn call_hierarchy_prepare_at_returns_none_on_local_identifier() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("counter.bt");
        let source = "x := 42\n".to_string();
        service.update_file(file.clone(), source);

        // Column 0 is the bare local variable `x` — not a selector.
        let target = service.call_hierarchy_prepare_at(&file, Position::new(0, 0));
        assert!(target.is_none());
    }

    /// BT-2243: cold-file incoming-calls fallback walks every file and
    /// returns *only* sender sites — method-definition headers must be
    /// excluded (otherwise the editor would list the method itself as
    /// "calling itself"). Compare with `find_references`, which also
    /// emits the definition for the `textDocument/references` UI.
    #[test]
    fn find_selector_send_sites_across_files_excludes_definitions() {
        let mut service = SimpleLanguageService::new();
        let file_a = Utf8PathBuf::from("a.bt");
        let file_b = Utf8PathBuf::from("b.bt");
        // File A defines `increment` but does not send it.
        service.update_file(
            file_a.clone(),
            "Object subclass: A\n  increment => 1\n".to_string(),
        );
        // File B sends `increment` from another method.
        service.update_file(
            file_b.clone(),
            "Object subclass: B\n  use => a increment\n".to_string(),
        );

        let sites = service.find_selector_send_sites_across_files("increment");
        // The sender lives in B, and only in B — the definition header in
        // A must not appear, even though A is the implementor.
        assert!(
            sites.iter().any(|loc| loc.file == file_b),
            "expected call site in B, got {sites:?}"
        );
        assert!(
            !sites.iter().any(|loc| loc.file == file_a),
            "definition in A leaked into senders: {sites:?}"
        );
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
    fn find_references_from_method_definition_header_unary() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        // Method `bar` defined in class Foo, and called via `x bar`.
        service.update_file(
            file.clone(),
            "Object subclass: Foo\n  bar => 1\nx := Foo new\nx bar".to_string(),
        );

        // Cursor ON the unary method definition header (the `bar` on line 1 col 2).
        // Expected: 1 definition + 1 call site = 2 refs.
        let refs = service.find_references(&file, Position::new(1, 2));
        assert_eq!(
            refs.len(),
            2,
            "expected exactly 2 refs (definition + call site) from unary header, got {}",
            refs.len()
        );
    }

    #[test]
    fn find_references_from_method_definition_header_keyword() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        service.update_file(
            file.clone(),
            "Object subclass: Foo\n  at: i put: v => v\nx at: 1 put: 2".to_string(),
        );

        // Cursor on the `at:` keyword in the method header (line 1, col 2).
        // Expected: 1 definition + 1 call site = 2 refs.
        let refs = service.find_references(&file, Position::new(1, 2));
        assert_eq!(
            refs.len(),
            2,
            "expected exactly 2 refs (definition + call site) from `at:` header, got {}",
            refs.len()
        );

        // Cursor on the `put:` keyword in the method header (line 1, col 8) —
        // must return the same full `at:put:` reference set.
        let refs_put = service.find_references(&file, Position::new(1, 8));
        assert_eq!(
            refs_put.len(),
            2,
            "expected exactly 2 refs (definition + call site) from `put:` header, got {}",
            refs_put.len()
        );
    }

    #[test]
    fn find_references_from_method_definition_header_binary() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        service.update_file(
            file.clone(),
            "Object subclass: Vec\n  + other => other\nx + y".to_string(),
        );

        // Cursor on the `+` binary selector in the method header (line 1, col 2).
        // Expected: 1 definition + 1 call site = 2 refs.
        let refs = service.find_references(&file, Position::new(1, 2));
        assert_eq!(
            refs.len(),
            2,
            "expected exactly 2 refs (definition + call site) from binary header `+`, got {}",
            refs.len()
        );
    }

    #[test]
    fn find_references_from_method_definition_header_cross_file() {
        // The definition-header path must integrate with cross-file search:
        // clicking on a method name in its definition should find every call
        // site in every indexed file.
        let mut service = SimpleLanguageService::new();
        let file_a = Utf8PathBuf::from("a.bt");
        let file_b = Utf8PathBuf::from("b.bt");

        service.update_file(
            file_a.clone(),
            "Object subclass: Foo\n  bar => 1".to_string(),
        );
        service.update_file(file_b.clone(), "x := Foo new\nx bar".to_string());

        // Cursor on the `bar` in the definition in file_a.
        // Expected: 1 definition in file_a + 1 call site in file_b = 2 refs.
        let refs = service.find_references(&file_a, Position::new(1, 2));
        assert_eq!(
            refs.len(),
            2,
            "expected exactly 2 cross-file refs (def in file_a + call in file_b), got {}: {refs:?}",
            refs.len()
        );
        assert!(
            refs.iter().any(|r| r.file == file_a),
            "missing definition in file_a: {refs:?}"
        );
        assert!(
            refs.iter().any(|r| r.file == file_b),
            "missing call site in file_b: {refs:?}"
        );
    }

    #[test]
    fn find_references_rejects_click_on_parameter_name_in_header() {
        // Clicking on a parameter name must NOT be treated as "on the
        // selector". Here we click on `i` in `at: i put: v`. The parameter
        // name has no uses in the body (body is just `v`), so the identifier
        // fallback should also return no cross-file matches. The point is
        // that the click must not be mistaken for a click on the `at:put:`
        // selector (which would return 2 refs).
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        service.update_file(
            file.clone(),
            "Object subclass: Foo\n  at: i put: v => v\nx at: 1 put: 2".to_string(),
        );

        // Column 6 = position of `i` on line 1 (`  at: i put: v => v`)
        let refs_on_param = service.find_references(&file, Position::new(1, 6));
        // Sanity-check that a click on the selector would return 2, so an
        // over-matching bug in the header heuristic would surface as 2 here.
        let refs_on_selector = service.find_references(&file, Position::new(1, 2));
        assert_eq!(refs_on_selector.len(), 2);
        assert_ne!(
            refs_on_param.len(),
            2,
            "param-name click was mistaken for `at:put:` selector click"
        );
    }

    // ── BT-1941: tightened unary/binary selector-span precision ────────

    #[test]
    fn find_references_rejects_click_on_sealed_modifier_in_header() {
        // Clicking on the `sealed` modifier keyword must NOT be treated as
        // a click on the selector `bar`. Before BT-1941 the coarse
        // "inside method.span, before the first param/return-type/body"
        // rule was permissive here because `method.span.start()` is
        // captured before modifiers are consumed.
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        service.update_file(
            file.clone(),
            "Object subclass: Foo\n  sealed bar => 1\nx := Foo new\nx bar".to_string(),
        );

        // Line 1: `  sealed bar => 1` — `sealed` spans columns 2-7, `bar`
        // spans columns 9-11.
        let refs_on_selector = service.find_references(&file, Position::new(1, 9));
        assert_eq!(
            refs_on_selector.len(),
            2,
            "expected exactly 2 refs (definition + call site) from `bar` selector click, got {}",
            refs_on_selector.len()
        );

        let refs_on_modifier = service.find_references(&file, Position::new(1, 4));
        assert_ne!(
            refs_on_modifier.len(),
            2,
            "click on `sealed` modifier was mistaken for a click on the `bar` selector"
        );
    }

    #[test]
    fn find_references_rejects_click_on_arrow_in_header() {
        // Clicking on the `->` punctuation between a unary selector and its
        // return type annotation must NOT be treated as a click on the
        // selector. Before BT-1941 this was permissive because `->` falls
        // inside `method.span` but outside the return-type annotation's own
        // span.
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        service.update_file(
            file.clone(),
            "Object subclass: Foo\n  bar -> Integer => 1\nx := Foo new\nx bar".to_string(),
        );

        // Line 1: `  bar -> Integer => 1` — `bar` spans columns 2-4, `->`
        // spans columns 6-7.
        let refs_on_selector = service.find_references(&file, Position::new(1, 2));
        assert_eq!(
            refs_on_selector.len(),
            2,
            "expected exactly 2 refs (definition + call site) from `bar` selector click, got {}",
            refs_on_selector.len()
        );

        let refs_on_arrow = service.find_references(&file, Position::new(1, 6));
        assert_ne!(
            refs_on_arrow.len(),
            2,
            "click on `->` punctuation was mistaken for a click on the `bar` selector"
        );
    }

    #[test]
    fn find_references_rejects_click_on_whitespace_in_header() {
        // Clicking on whitespace between the selector and the `=>` must NOT
        // be treated as a click on the selector.
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        service.update_file(
            file.clone(),
            "Object subclass: Foo\n  bar => 1\nx := Foo new\nx bar".to_string(),
        );

        // Line 1: `  bar => 1` — `bar` spans columns 2-4, whitespace at
        // column 5.
        let refs_on_selector = service.find_references(&file, Position::new(1, 2));
        assert_eq!(
            refs_on_selector.len(),
            2,
            "expected exactly 2 refs (definition + call site) from `bar` selector click, got {}",
            refs_on_selector.len()
        );

        let refs_on_whitespace = service.find_references(&file, Position::new(1, 5));
        assert_ne!(
            refs_on_whitespace.len(),
            2,
            "click on whitespace was mistaken for a click on the `bar` selector"
        );
    }

    #[test]
    fn find_references_rejects_click_on_class_and_internal_modifiers_in_header() {
        // Both `class` and `internal` modifiers, combined on one header,
        // must be rejected — and the skip-count logic must correctly walk
        // past *both* of them to land on the real selector `bar`.
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        service.update_file(
            file.clone(),
            "Object subclass: Foo\n  internal class bar => 1\nFoo bar".to_string(),
        );

        // Line 1: `  internal class bar => 1` — `internal` spans columns
        // 2-9, `class` spans columns 11-15, `bar` spans columns 17-19.
        let refs_on_selector = service.find_references(&file, Position::new(1, 17));
        assert_eq!(
            refs_on_selector.len(),
            2,
            "expected exactly 2 refs (definition + call site) from `bar` selector click, got {}",
            refs_on_selector.len()
        );

        let refs_on_internal = service.find_references(&file, Position::new(1, 5));
        assert_ne!(
            refs_on_internal.len(),
            2,
            "click on `internal` modifier was mistaken for a click on the `bar` selector"
        );

        let refs_on_class = service.find_references(&file, Position::new(1, 13));
        assert_ne!(
            refs_on_class.len(),
            2,
            "click on `class` modifier was mistaken for a click on the `bar` selector"
        );
    }

    #[test]
    fn find_references_rejects_click_on_duplicate_sealed_modifiers_in_header() {
        // The parser consumes `sealed` unconditionally and does not dedupe,
        // so `sealed sealed bar => 1` parses with `is_sealed == true` but two
        // `sealed` tokens consumed. The header scan must skip *both* tokens
        // (mirroring the parser token-for-token) rather than skipping a fixed
        // count derived from the single `is_sealed` flag — otherwise the
        // second `sealed` would be mis-identified as the `bar` selector.
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        service.update_file(
            file.clone(),
            "Object subclass: Foo\n  sealed sealed bar => 1\nx := Foo new\nx bar".to_string(),
        );

        // Line 1: `  sealed sealed bar => 1` — first `sealed` cols 2-7,
        // second `sealed` cols 9-14, `bar` cols 16-18.
        let refs_on_selector = service.find_references(&file, Position::new(1, 16));
        assert_eq!(
            refs_on_selector.len(),
            2,
            "expected exactly 2 refs (definition + call site) from `bar` selector click, got {}",
            refs_on_selector.len()
        );

        let refs_on_second_sealed = service.find_references(&file, Position::new(1, 11));
        assert_ne!(
            refs_on_second_sealed.len(),
            2,
            "click on the second `sealed` modifier was mistaken for a click on the `bar` selector"
        );
    }

    #[test]
    fn find_references_rejects_click_on_duplicate_class_modifiers_in_header() {
        // `class` is a modifier when its lookahead is not `=>`/`->`/`::`, so
        // `class class bar => 1` consumes *two* `class` tokens as modifiers
        // (each keeping `is_class_method == true`) before the `bar` selector.
        // The scan must walk past both.
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        service.update_file(
            file.clone(),
            "Object subclass: Foo\n  class class bar => 1\nFoo bar".to_string(),
        );

        // Line 1: `  class class bar => 1` — first `class` cols 2-7, second
        // `class` cols 8-13, `bar` cols 14-17.
        let refs_on_selector = service.find_references(&file, Position::new(1, 14));
        assert_eq!(
            refs_on_selector.len(),
            2,
            "expected exactly 2 refs (definition + call site) from `bar` selector click, got {}",
            refs_on_selector.len()
        );

        let refs_on_second_class = service.find_references(&file, Position::new(1, 9));
        assert_ne!(
            refs_on_second_class.len(),
            2,
            "click on the second `class` modifier was mistaken for a click on the `bar` selector"
        );
    }

    #[test]
    fn find_references_from_binary_method_header_with_modifier() {
        // Binary selector (`+`) with a leading modifier: the scan must skip
        // `sealed` and land on the `+` selector token (a `BinarySelector`
        // token kind, exercising the non-`Identifier` selector path).
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        service.update_file(
            file.clone(),
            "Object subclass: Foo\n  sealed + other => other\nx := Foo new\nx + 1".to_string(),
        );

        // Line 1: `  sealed + other => other` — `sealed` cols 2-7, `+` col 9.
        let refs_on_selector = service.find_references(&file, Position::new(1, 9));
        assert_eq!(
            refs_on_selector.len(),
            2,
            "expected exactly 2 refs (definition + call site) from `+` selector click, got {}",
            refs_on_selector.len()
        );

        let refs_on_sealed = service.find_references(&file, Position::new(1, 4));
        assert_ne!(
            refs_on_sealed.len(),
            2,
            "click on `sealed` modifier was mistaken for a click on the `+` selector"
        );
    }

    #[test]
    fn find_references_from_method_header_named_after_modifier_keyword() {
        // Edge case called out in BT-1941: a unary method whose selector
        // name is itself a modifier keyword. `class => ...` parses with
        // `is_class_method == false` because the parser's own lookahead
        // (`is_fat_arrow_or_return_type`) resolves the ambiguity in favour
        // of "this is the selector" when `class` is immediately followed by
        // `=>`. The header scan must drive its skip count off that flag —
        // not off pattern-matching the literal word `class` — so it must
        // still recognize `class` as the selector rather than skipping it
        // as a modifier and then finding nothing.
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        service.update_file(
            file.clone(),
            "Object subclass: Foo\n  class => 1\nx := Foo new\nx class".to_string(),
        );

        // Line 1: `  class => 1` — `class` spans columns 2-6.
        let refs = service.find_references(&file, Position::new(1, 4));
        assert_eq!(
            refs.len(),
            2,
            "expected exactly 2 refs (definition + call site) from `class`-named selector click, got {}",
            refs.len()
        );
    }

    #[test]
    fn find_references_from_class_method_definition_header() {
        // The helper walks both `class.methods` and `class.class_methods`;
        // pin coverage for the class-side path so it doesn't regress.
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        service.update_file(
            file.clone(),
            "Object subclass: Foo\n  class ping => 1\nFoo ping".to_string(),
        );

        // Cursor on `ping` in the class-method definition header (line 1 col 8
        // — after `  class `).
        let refs = service.find_references(&file, Position::new(1, 8));
        assert_eq!(
            refs.len(),
            2,
            "expected exactly 2 refs (class-method def + `Foo ping` call), got {}: {refs:?}",
            refs.len()
        );
    }

    #[test]
    fn find_references_from_standalone_method_definition_header() {
        // The helper walks `module.method_definitions` (Tonel-style `Foo >>
        // bar => ...`); pin coverage for that path too.
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        // Line layout:
        //   0: Object subclass: Foo
        //   1: Foo >> bar => 1
        //   2: x := Foo new
        //   3: x bar
        service.update_file(
            file.clone(),
            "Object subclass: Foo\nFoo >> bar => 1\nx := Foo new\nx bar".to_string(),
        );

        // Cursor on `bar` in the standalone definition header: line 1,
        // column 7 (columns: F=0, o=1, o=2, ' '=3, >=4, >=5, ' '=6, b=7).
        let refs = service.find_references(&file, Position::new(1, 7));
        assert_eq!(
            refs.len(),
            2,
            "expected exactly 2 refs (standalone def + `x bar` call), got {}: {refs:?}",
            refs.len()
        );
    }

    #[test]
    fn find_references_rejects_click_on_body_expression_in_method() {
        // Clicking on a body expression must not be treated as "on the
        // selector" — otherwise every expression in the method body would
        // falsely return the full set of call sites for the method.
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        service.update_file(
            file.clone(),
            "Object subclass: Foo\n  bar => myVar\nx bar\ny bar".to_string(),
        );

        // Line 1, col 9 = `myVar` in the body (not the selector `bar`)
        let refs_on_body = service.find_references(&file, Position::new(1, 9));
        // Line 1, col 2 = `bar` selector in the header
        let refs_on_header = service.find_references(&file, Position::new(1, 2));

        // The header click finds `bar` refs: 1 def + 2 call sites = 3.
        assert_eq!(
            refs_on_header.len(),
            3,
            "header click should return def + two call sites, got {}",
            refs_on_header.len()
        );
        // The body click finds `myVar` refs: just the one occurrence in the body.
        assert_eq!(
            refs_on_body.len(),
            1,
            "body click should only return the single `myVar` occurrence, got {}",
            refs_on_body.len()
        );
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

    // BT-1940: goto-definition on the class name inside a constructor
    // pattern (e.g. `Result` in `Result ok: v`) must navigate to the class
    // declaration. Prior to the pattern walker the cursor returned None.
    #[test]
    fn goto_definition_constructor_pattern_class_name() {
        let mut service = SimpleLanguageService::new();
        let result_file = Utf8PathBuf::from("Result.bt");
        let classify_file = Utf8PathBuf::from("classify.bt");

        service.update_file(
            result_file.clone(),
            "Value subclass: Result\n  ok: v\n  error: e".to_string(),
        );
        // The match arm `Result ok: v -> v` starts on line 2, col 4.
        // `Result` occupies columns 4..10, so column 5 lands on the 'e' of
        // "Result" — inside the class identifier.
        service.update_file(
            classify_file.clone(),
            "Object subclass: Classifier\n  \
             classify: r =>\n    \
             r match: [\n      \
             Result ok: v -> v;\n      \
             Result error: _ -> 0\n    ]"
                .to_string(),
        );

        // Line 3 (0-indexed) is `      Result ok: v -> v;`; column 8
        // lands inside "Result".
        let def = service.goto_definition(&classify_file, Position::new(3, 8));
        assert!(
            def.is_some(),
            "goto-definition on constructor pattern class name returned None"
        );
        let loc = def.unwrap();
        assert_eq!(loc.file, result_file);
    }

    // BT-1940: goto-definition must also reach identifiers inside a `when:`
    // guard, not just the pattern and the arm body. `references_provider`
    // already walks guards; this test pins the parity in `find_identifier_in_expr`.
    #[test]
    fn goto_definition_inside_match_arm_guard() {
        let mut service = SimpleLanguageService::new();
        let threshold_file = Utf8PathBuf::from("Threshold.bt");
        let guarded_file = Utf8PathBuf::from("guarded.bt");

        service.update_file(
            threshold_file.clone(),
            "Object subclass: Threshold\n  limit -> Integer => 10".to_string(),
        );
        // Match arm with a guard referencing `Threshold` from the class body:
        //   r match: [
        //     v when: v > Threshold limit -> v;
        //     _ -> 0
        //   ]
        service.update_file(
            guarded_file.clone(),
            "Object subclass: Guarded\n  \
             check: r =>\n    \
             r match: [\n      \
             v when: v > Threshold limit -> v;\n      \
             _ -> 0\n    ]"
                .to_string(),
        );

        // Line 3 (0-indexed) is `      v when: v > Threshold limit -> v;`.
        // `Threshold` starts at column 17 ("      v when: v > " = 18 chars, so
        // column 18 lands on 'T').
        let def = service.goto_definition(&guarded_file, Position::new(3, 20));
        assert!(
            def.is_some(),
            "goto-definition on identifier inside match arm guard returned None"
        );
        let loc = def.unwrap();
        assert_eq!(loc.file, threshold_file);
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

    // ── BT-1939: Go to Definition on method definition headers ─────────────

    #[test]
    fn goto_definition_from_method_header_unary_navigates_to_parent() {
        // BT-1939: clicking Go to Definition on the selector in a method's
        // own definition header should navigate to the overridden parent
        // method, mirroring the BT-1938 header path added for find_references.
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        //  0: Object subclass: Parent
        //  1:   greet => 1
        //  2: Parent subclass: Child
        //  3:   greet => 2
        let source = "Object subclass: Parent\n  greet => 1\nParent subclass: Child\n  greet => 2"
            .to_string();
        service.update_file(file.clone(), source.clone());

        // Cursor on `greet` in Child's header: line 3, col 2 (after the two
        // leading spaces).
        let def = service.goto_definition(&file, Position::new(3, 2));
        assert!(
            def.is_some(),
            "expected navigation to parent's greet, got None"
        );
        let loc = def.unwrap();
        assert_eq!(loc.file, file);
        // Parent's method body lives before the `Parent subclass: Child`
        // declaration, so any span inside Parent must start before that
        // declaration's offset.
        let child_decl_offset = source.find("Parent subclass: Child").unwrap();
        assert!(
            (loc.span.start() as usize) < child_decl_offset,
            "expected Parent's method span (< {child_decl_offset}), got {}",
            loc.span.start()
        );
    }

    #[test]
    fn goto_definition_from_method_header_binary_navigates_to_parent() {
        // BT-1939: binary selectors (`+`, `-`, etc.) must also resolve.
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        //  0: Object subclass: Vec
        //  1:   + other => other
        //  2: Vec subclass: Vec3
        //  3:   + other => other
        let source =
            "Object subclass: Vec\n  + other => other\nVec subclass: Vec3\n  + other => other"
                .to_string();
        service.update_file(file.clone(), source.clone());

        // Cursor on `+` in Vec3's header: line 3, col 2.
        let def = service.goto_definition(&file, Position::new(3, 2));
        assert!(
            def.is_some(),
            "expected navigation to parent's `+`, got None"
        );
        let loc = def.unwrap();
        assert_eq!(loc.file, file);
        let child_decl_offset = source.find("Vec subclass: Vec3").unwrap();
        assert!(
            (loc.span.start() as usize) < child_decl_offset,
            "expected Vec's `+` span (< {child_decl_offset}), got {}",
            loc.span.start()
        );
    }

    #[test]
    fn goto_definition_from_method_header_keyword_navigates_to_parent() {
        // BT-1939: keyword selectors — both keyword parts must navigate to
        // the parent definition.
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        //  0: Object subclass: Dict
        //  1:   at: k put: v => v
        //  2: Dict subclass: OrderedDict
        //  3:   at: k put: v => v
        let source = "Object subclass: Dict\n  at: k put: v => v\nDict subclass: OrderedDict\n  at: k put: v => v"
            .to_string();
        service.update_file(file.clone(), source.clone());

        let child_decl_offset = source.find("Dict subclass: OrderedDict").unwrap();

        // Cursor on `at:` in OrderedDict's header: line 3, col 2.
        let def_at = service.goto_definition(&file, Position::new(3, 2));
        assert!(def_at.is_some(), "expected navigation from `at:`, got None");
        let loc_at = def_at.unwrap();
        assert_eq!(loc_at.file, file);
        assert!(
            (loc_at.span.start() as usize) < child_decl_offset,
            "expected Dict's at:put: span (< {child_decl_offset}), got {}",
            loc_at.span.start()
        );

        // Cursor on `put:` in the same header: line 3, col 8.
        let def_put = service.goto_definition(&file, Position::new(3, 8));
        assert!(
            def_put.is_some(),
            "expected navigation from `put:`, got None"
        );
        let loc_put = def_put.unwrap();
        assert_eq!(loc_put.file, file);
        assert!(
            (loc_put.span.start() as usize) < child_decl_offset,
            "expected Dict's at:put: span (< {child_decl_offset}), got {}",
            loc_put.span.start()
        );

        // Both keyword parts must navigate to the same definition.
        assert_eq!(
            loc_at.span.start(),
            loc_put.span.start(),
            "`at:` and `put:` keyword clicks should resolve to the same definition"
        );
    }

    #[test]
    fn goto_definition_from_method_header_no_override_returns_none() {
        // BT-1939 no-regression: if the method does not override anything in
        // any ancestor, Go to Definition on the header returns None rather
        // than navigating elsewhere.
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        //  0: Object subclass: Foo
        //  1:   bar => 1
        // Object defines no `bar` (in a stripped hierarchy), so `bar` does
        // not override anything in Foo.
        service.update_file(file.clone(), "Object subclass: Foo\n  bar => 1".to_string());

        // Cursor on `bar` in Foo's header: line 1, col 2.
        let def = service.goto_definition(&file, Position::new(1, 2));
        assert!(
            def.is_none(),
            "expected None for non-overriding method header, got {def:?}"
        );
    }

    #[test]
    fn goto_definition_from_class_method_header_navigates_to_parent() {
        // BT-1939: class-side methods (`class foo => ...`) must resolve the
        // same way as instance methods. The `class_side` flag on the
        // receiver context threads through `find_method_in_module` so the
        // MRO walk matches only class-side methods on each ancestor.
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        //  0: Object subclass: Parent
        //  1:   class ping => 1
        //  2: Parent subclass: Child
        //  3:   class ping => 2
        let source =
            "Object subclass: Parent\n  class ping => 1\nParent subclass: Child\n  class ping => 2"
                .to_string();
        service.update_file(file.clone(), source.clone());

        // Cursor on `ping` in Child's class-method header: line 3, col 8
        // (after `  class `).
        let def = service.goto_definition(&file, Position::new(3, 8));
        assert!(
            def.is_some(),
            "expected navigation to parent's class-side ping, got None"
        );
        let loc = def.unwrap();
        assert_eq!(loc.file, file);
        let child_decl_offset = source.find("Parent subclass: Child").unwrap();
        assert!(
            (loc.span.start() as usize) < child_decl_offset,
            "expected Parent's class ping span (< {child_decl_offset}), got {}",
            loc.span.start()
        );
    }

    #[test]
    fn goto_definition_from_standalone_method_header_navigates_to_parent() {
        // BT-1939: standalone (Tonel-style) method definitions
        // (`Foo >> bar => ...`) must also resolve correctly. These live in
        // `module.method_definitions` rather than on `Class::methods`.
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        //  0: Object subclass: Parent
        //  1: Parent subclass: Child
        //  2: Parent >> greet => 1
        //  3: Child >> greet => 2
        let source =
            "Object subclass: Parent\nParent subclass: Child\nParent >> greet => 1\nChild >> greet => 2"
                .to_string();
        service.update_file(file.clone(), source.clone());

        // Cursor on `greet` in Child's standalone header: line 3, col 9
        // (C=0, h=1, i=2, l=3, d=4, ' '=5, >=6, >=7, ' '=8, g=9).
        let def = service.goto_definition(&file, Position::new(3, 9));
        assert!(
            def.is_some(),
            "expected navigation to parent's standalone greet, got None"
        );
        let loc = def.unwrap();
        assert_eq!(loc.file, file);
        // Parent's standalone `greet` lives before `Child >> greet`, so the
        // returned span must start before that marker.
        let child_standalone_offset = source.find("Child >> greet").unwrap();
        assert!(
            (loc.span.start() as usize) < child_standalone_offset,
            "expected Parent's standalone greet span (< {child_standalone_offset}), got {}",
            loc.span.start()
        );
    }

    #[test]
    fn goto_definition_from_method_header_skips_non_overriding_intermediate() {
        // BT-1939: walk MRO order correctly. Given
        //   Grandparent defines greet, Middle does NOT, Leaf overrides greet
        // clicking on Leaf's greet header should navigate to Grandparent
        // (skipping Middle). This pins the MRO walk order guarantee — a
        // naive implementation that only checks the immediate superclass
        // would return None here, which would be wrong.
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("test.bt");

        //  0: Object subclass: Grandparent
        //  1:   greet => 1
        //  2: Grandparent subclass: Middle
        //  3: Middle subclass: Leaf
        //  4:   greet => 3
        let source = "Object subclass: Grandparent\n  greet => 1\nGrandparent subclass: Middle\nMiddle subclass: Leaf\n  greet => 3"
            .to_string();
        service.update_file(file.clone(), source.clone());

        // Cursor on `greet` in Leaf's header: line 4, col 2.
        let def = service.goto_definition(&file, Position::new(4, 2));
        assert!(
            def.is_some(),
            "expected MRO walk to find Grandparent's greet, got None"
        );
        let loc = def.unwrap();
        assert_eq!(loc.file, file);
        // Grandparent's greet lives before the `Grandparent subclass: Middle`
        // declaration, so any span inside Grandparent must start before that
        // marker.
        let middle_decl_offset = source.find("Grandparent subclass: Middle").unwrap();
        assert!(
            (loc.span.start() as usize) < middle_decl_offset,
            "expected Grandparent's greet span (< {middle_decl_offset}), got {}",
            loc.span.start()
        );
    }

    #[test]
    fn goto_definition_from_method_header_navigates_cross_file() {
        // BT-1939: the parent definition can live in a different file from
        // the overriding child. The cross-file walker in
        // `find_overridden_method_definition` should handle it.
        let mut service = SimpleLanguageService::new();
        let file_parent = Utf8PathBuf::from("parent.bt");
        let file_child = Utf8PathBuf::from("child.bt");

        service.update_file(
            file_parent.clone(),
            "Object subclass: Parent\n  greet => 1".to_string(),
        );
        service.update_file(
            file_child.clone(),
            "Parent subclass: Child\n  greet => 2".to_string(),
        );

        // Cursor on `greet` in Child's header (in child.bt): line 1, col 2.
        let def = service.goto_definition(&file_child, Position::new(1, 2));
        assert!(
            def.is_some(),
            "expected cross-file navigation to parent.bt, got None"
        );
        let loc = def.unwrap();
        assert_eq!(
            loc.file, file_parent,
            "expected parent.bt, got {:?}",
            loc.file
        );
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

    // -----------------------------------------------------------------------
    // BT-1933: Protocol class object completions via SimpleLanguageService
    // -----------------------------------------------------------------------

    #[test]
    fn protocol_registered_in_project_index() {
        let mut service = SimpleLanguageService::new();

        let file = Utf8PathBuf::from("proto.bt");
        let source = "Protocol define: Printable\n  asString -> String";
        service.update_file(file.clone(), source.to_string());

        // Protocol should be visible in project index as a class
        assert!(
            service.project_index.hierarchy().has_class("Printable"),
            "Protocol should be registered in project hierarchy"
        );

        // Protocol class should have the expected class methods
        let class = service
            .project_index
            .hierarchy()
            .get_class("Printable")
            .expect("Printable should be a class in the hierarchy");
        assert_eq!(class.superclass.as_deref(), Some("Protocol"));
        assert!(class.is_sealed);
        assert!(class.is_abstract);
        let selectors: Vec<_> = class
            .class_methods
            .iter()
            .map(|m| m.selector.as_str())
            .collect();
        assert!(
            selectors.contains(&"requiredMethods"),
            "Protocol class should have requiredMethods, got: {selectors:?}"
        );
        assert!(
            selectors.contains(&"conformingClasses"),
            "Protocol class should have conformingClasses, got: {selectors:?}"
        );
    }

    #[test]
    fn protocol_visible_cross_file_in_project_index() {
        let mut service = SimpleLanguageService::new();

        // File A defines a protocol
        service.update_file(
            Utf8PathBuf::from("a.bt"),
            "Protocol define: Printable\n  asString -> String".to_string(),
        );

        // File B defines a class
        service.update_file(
            Utf8PathBuf::from("b.bt"),
            "Object subclass: Foo\n  bar => 1".to_string(),
        );

        // Both should be visible in merged hierarchy
        assert!(
            service.project_index.hierarchy().has_class("Printable"),
            "Protocol from file A should be visible in merged hierarchy"
        );
        assert!(
            service.project_index.hierarchy().has_class("Foo"),
            "Class from file B should be visible in merged hierarchy"
        );
    }

    // -----------------------------------------------------------------------
    // BT-1936: Goto-definition and find-references for protocol names
    // -----------------------------------------------------------------------

    #[test]
    fn goto_definition_protocol_from_class_type_param_bound() {
        // Click on `Printable` inside `Logger(T :: Printable)` in another file.
        let mut service = SimpleLanguageService::new();
        let file_a = Utf8PathBuf::from("printable.bt");
        let file_b = Utf8PathBuf::from("logger.bt");

        service.update_file(
            file_a.clone(),
            "Protocol define: Printable\n  asString -> String".to_string(),
        );
        service.update_file(
            file_b.clone(),
            "Actor subclass: Logger(T :: Printable)\n  log: msg => self".to_string(),
        );

        // `Actor subclass: Logger(T :: Printable)`
        //  0         1         2         3
        //  0123456789012345678901234567890123456789
        // "Printable" starts at column 29; click at column 32.
        let def = service.goto_definition(&file_b, Position::new(0, 32));
        let loc = def.expect("goto-def should navigate to protocol declaration");
        assert_eq!(loc.file, file_a);
    }

    #[test]
    fn goto_definition_protocol_from_extending_clause() {
        // Click on `Comparable` inside `extending: Comparable`.
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("protocols.bt");
        service.update_file(
            file.clone(),
            "Protocol define: Comparable\n  < other :: Self -> Boolean\n\n\
             Protocol define: Sortable\n  extending: Comparable\n  sortKey -> Object"
                .to_string(),
        );

        // Line 4 (0-indexed) is `  extending: Comparable`
        //   0         1         2
        //   01234567890123456789012345
        //   "  extending: Comparable"  — "Comparable" starts at col 13.
        let def = service.goto_definition(&file, Position::new(4, 15));
        let loc = def.expect("goto-def should find Comparable protocol");
        assert_eq!(loc.file, file);
    }

    #[test]
    fn find_references_protocol_from_definition_site() {
        // Click on the protocol definition name and find: the definition itself
        // plus all usages (extending target, type-param bounds).
        let mut service = SimpleLanguageService::new();
        let file_proto = Utf8PathBuf::from("printable.bt");
        let file_logger = Utf8PathBuf::from("logger.bt");
        let file_sortable = Utf8PathBuf::from("sortable.bt");

        service.update_file(
            file_proto.clone(),
            "Protocol define: Printable\n  asString -> String".to_string(),
        );
        service.update_file(
            file_logger.clone(),
            "Actor subclass: Logger(T :: Printable)\n  log: msg => self".to_string(),
        );
        service.update_file(
            file_sortable.clone(),
            "Protocol define: Sortable\n  extending: Printable\n  sortKey -> Object".to_string(),
        );

        // "Protocol define: Printable" — `Printable` starts at column 17.
        let refs = service.find_references(&file_proto, Position::new(0, 20));
        // Expect: definition (file_proto) + bound (file_logger) + extending (file_sortable) = 3.
        assert_eq!(
            refs.len(),
            3,
            "expected 3 protocol references, got {refs:?}"
        );
        assert!(refs.iter().any(|r| r.file == file_proto));
        assert!(refs.iter().any(|r| r.file == file_logger));
        assert!(refs.iter().any(|r| r.file == file_sortable));
    }

    /// BT-2027: `SimpleLanguageService::diagnostics` must hand off the
    /// cross-file class set from the `ProjectIndex` to the unified diagnostic
    /// pipeline, so that a file referencing a class defined elsewhere does
    /// not produce a spurious `UnresolvedClass` diagnostic.
    #[test]
    fn diagnostics_resolve_cross_file_class_via_project_index() {
        use crate::source_analysis::DiagnosticCategory;

        let mut service = SimpleLanguageService::new();
        let src_file = Utf8PathBuf::from("src/Foo.bt");
        let test_file = Utf8PathBuf::from("test/FooTest.bt");

        service.update_file(
            src_file.clone(),
            "Object subclass: Foo\n  class demo => 42\n".to_string(),
        );
        service.update_file(
            test_file.clone(),
            "Object subclass: FooTest\n  class run =>\n    Foo demo\n".to_string(),
        );

        let diags = service.diagnostics(&test_file);
        let unresolved: Vec<_> = diags
            .iter()
            .filter(|d| d.category == Some(DiagnosticCategory::UnresolvedClass))
            .collect();
        assert!(
            unresolved.is_empty(),
            "cross-file class `Foo` should resolve via ProjectIndex, got: {unresolved:?}"
        );
    }

    /// BT-2800 (ADR 0100 Rule 3 surface-parity gap): `SimpleLanguageService`
    /// must apply the `[diagnostics]` table set via `set_diagnostics_overrides`
    /// exactly like `beamtalk build` does — a `dnu = "error"` override
    /// promotes the default `Hint` on an unresolved selector to `Error`.
    #[test]
    fn diagnostics_applies_severity_overrides() {
        use crate::compilation::diagnostics_policy::{
            DiagnosticSeverityOverride, DiagnosticsTable,
        };
        use crate::source_analysis::{DiagnosticCategory, Severity};

        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("src/Dnu.bt");
        service.update_file(file.clone(), "\"hello\" frobnicate".to_string());

        // Baseline: no overrides set, Rule 1 default is Hint.
        let baseline = service.diagnostics(&file);
        assert!(
            baseline.iter().any(
                |d| d.category == Some(DiagnosticCategory::Dnu) && d.severity == Severity::Hint
            ),
            "expected a Dnu Hint before any override: {baseline:?}"
        );

        let mut table = DiagnosticsTable::new();
        table.insert(DiagnosticCategory::Dnu, DiagnosticSeverityOverride::Error);
        service.set_diagnostics_overrides(table);

        let overridden = service.diagnostics(&file);
        assert!(
            overridden
                .iter()
                .any(|d| d.category == Some(DiagnosticCategory::Dnu)
                    && d.severity == Severity::Error),
            "dnu = \"error\" override must promote the Dnu diagnostic to Error: {overridden:?}"
        );
    }

    /// BT-2795 (ADR 0100 Rule 2 WS1): a standalone extension defined in one
    /// file must be visible to another file's diagnostics — the false `Dnu`
    /// hint on a same-project cross-file extension disappears.
    #[test]
    fn diagnostics_resolve_cross_file_extension_via_project_index() {
        let mut service = SimpleLanguageService::new();
        let ext_file = Utf8PathBuf::from("src/StringShout.bt");
        let use_file = Utf8PathBuf::from("src/UseShout.bt");

        service.update_file(
            ext_file.clone(),
            "String >> shoutLouder => self\n".to_string(),
        );
        service.update_file(
            use_file.clone(),
            "Object subclass: UseShout\n  class demo =>\n    \"abc\" shoutLouder\n".to_string(),
        );

        let diags = service.diagnostics(&use_file);
        let dnu: Vec<_> = diags
            .iter()
            .filter(|d| d.message.contains("shoutLouder"))
            .collect();
        assert!(
            dnu.is_empty(),
            "cross-file extension `String >> shoutLouder` should resolve, got: {dnu:?}"
        );
    }

    /// BT-2795: removing the defining file makes the extension unresolved again.
    #[test]
    fn diagnostics_cross_file_extension_gone_after_remove() {
        let mut service = SimpleLanguageService::new();
        let ext_file = Utf8PathBuf::from("src/StringShout.bt");
        let use_file = Utf8PathBuf::from("src/UseShout.bt");

        service.update_file(
            ext_file.clone(),
            "String >> shoutLouder => self\n".to_string(),
        );
        service.update_file(
            use_file.clone(),
            "Object subclass: UseShout\n  class demo =>\n    \"abc\" shoutLouder\n".to_string(),
        );
        service.remove_file(&ext_file);

        let diags = service.diagnostics(&use_file);
        let dnu: Vec<_> = diags
            .iter()
            .filter(|d| d.message.contains("shoutLouder"))
            .collect();
        assert!(
            !dnu.is_empty(),
            "after removing the defining file the extension should be unresolved again"
        );
    }

    /// BT-2027: Opening a stdlib source file must not emit "conflicts with
    /// stdlib class" diagnostics for every class the file defines. The
    /// language service now sets `stdlib_mode = true` for files tracked as
    /// stdlib in the `ProjectIndex`.
    #[test]
    fn diagnostics_skip_stdlib_shadowing_for_stdlib_files() {
        use crate::language_service::project_index::ProjectIndex;

        // Pre-index a stdlib file defining `Counter`.
        let stdlib_path = Utf8PathBuf::from("stdlib/src/Counter.bt");
        let stdlib_source = "Object subclass: Counter\n  class zero => 0\n".to_string();
        let (index_result, _) =
            ProjectIndex::with_stdlib(&[(stdlib_path.clone(), stdlib_source.clone())]);
        let index = index_result.unwrap();

        let mut service = SimpleLanguageService::with_project_index(index);
        // Re-register through update_file so `files` has an entry for it.
        service.update_file(stdlib_path.clone(), stdlib_source);

        let diags = service.diagnostics(&stdlib_path);
        let shadowing: Vec<_> = diags
            .iter()
            .filter(|d| d.message.contains("conflicts with a stdlib class"))
            .collect();
        assert!(
            shadowing.is_empty(),
            "stdlib files should not emit stdlib-shadowing diagnostics, got: {shadowing:?}"
        );
    }

    // ---------- BT-2242: type hierarchy ----------

    #[test]
    fn type_hierarchy_prepare_at_resolves_class_name_on_subclass_clause() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("hierarchy.bt");
        service.update_file(
            file.clone(),
            "Object subclass: Foo\nFoo subclass: Bar\n".to_string(),
        );

        // Line 1 = "Foo subclass: Bar"; col 1 is on the `Foo` token.
        let result = service.type_hierarchy_prepare_at(&file, Position::new(1, 1));
        let (name, loc) = result.expect("Some for cursor on class reference");
        assert_eq!(name.as_str(), "Foo");
        let loc = loc.expect("Foo is declared in the same file");
        assert_eq!(loc.file, file);
        // The declaration span is the `Foo` token on line 0 (col 17 inside
        // "Object subclass: Foo"). We only assert the file here; the span
        // exact-offset is exercised by `find_class_declaration_location`'s
        // direct call site (the LSP handler test).
    }

    #[test]
    fn type_hierarchy_prepare_at_returns_none_for_selector_cursor() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("class_with_method.bt");
        service.update_file(
            file.clone(),
            "Object subclass: Foo\n  bar => 42\n".to_string(),
        );

        // Cursor on the `bar` selector token (line 1, col 2..5). The
        // identifier walker rejects this (selector, not a known class
        // name), so prepare returns None.
        let result = service.type_hierarchy_prepare_at(&file, Position::new(1, 4));
        assert!(
            result.is_none(),
            "expected None for selector cursor, got {result:?}"
        );
    }

    #[test]
    fn supertypes_of_returns_chain_for_user_class() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("chain.bt");
        service.update_file(
            file.clone(),
            "Object subclass: Foo\nFoo subclass: Bar\n".to_string(),
        );

        let supers = service.supertypes_of("Bar");
        let names: Vec<String> = supers.iter().map(|(n, _)| n.to_string()).collect();
        assert_eq!(names, vec!["Foo", "Object", "ProtoObject"]);

        // `Foo` is declared in this file → should resolve.
        let foo_row = supers
            .iter()
            .find(|(n, _)| n.as_str() == "Foo")
            .expect("Foo row present");
        assert!(foo_row.1.is_some(), "Foo's declaration must be indexed");
        // Object / ProtoObject are builtins with no indexed source → None.
        let object_row = supers
            .iter()
            .find(|(n, _)| n.as_str() == "Object")
            .expect("Object row present");
        assert!(
            object_row.1.is_none(),
            "Object should have no indexed declaration in this test setup"
        );
    }

    #[test]
    fn subtypes_of_returns_descendants_in_bfs_order() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("descendants.bt");
        service.update_file(
            file.clone(),
            "Object subclass: Foo\n\
             Foo subclass: Bar\n\
             Foo subclass: Baz\n\
             Bar subclass: Qux\n"
                .to_string(),
        );

        let subs = service.subtypes_of("Foo");
        let names: Vec<&str> = subs.iter().map(|(n, _)| n.as_str()).collect();
        assert_eq!(names.len(), 3, "expected 3 descendants, got {names:?}");
        assert!(names.contains(&"Bar"));
        assert!(names.contains(&"Baz"));
        assert!(names.contains(&"Qux"));
        // BFS: Qux (grandchild) must come after Bar (its parent).
        let bar_pos = names.iter().position(|n| *n == "Bar").unwrap();
        let qux_pos = names.iter().position(|n| *n == "Qux").unwrap();
        assert!(qux_pos > bar_pos);
        // The receiver itself is not included.
        assert!(!names.contains(&"Foo"));
    }

    #[test]
    fn subtypes_of_returns_empty_for_class_with_no_children() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("childless.bt");
        service.update_file(file.clone(), "Object subclass: Foo\n".to_string());
        let subs = service.subtypes_of("Foo");
        assert!(subs.is_empty());
    }

    // ---------- BT-2317: protocol/class shadowing on the type-hierarchy path ----------

    #[test]
    fn type_hierarchy_prepare_at_prefers_class_over_same_named_protocol() {
        // BT-2317: a name defined as both a real class and a synthetic
        // protocol (BT-1933) across separate files must resolve to the class
        // on the type-hierarchy path, mirroring goto-definition. Before the
        // fix this depended on `HashMap` iteration order and could land on the
        // protocol header.
        let mut service = SimpleLanguageService::new();
        let class_file = Utf8PathBuf::from("real_class.bt");
        let protocol_file = Utf8PathBuf::from("protocol_foo.bt");
        let ref_file = Utf8PathBuf::from("uses_foo.bt");

        service.update_file(
            class_file.clone(),
            "Object subclass: Foo\n  bar => 1\n".to_string(),
        );
        service.update_file(
            protocol_file.clone(),
            "Protocol define: Foo\n  baz -> Integer\n".to_string(),
        );
        // `Foo subclass: Sub` gives us a `Foo` reference token to put the
        // cursor on (col 1 lands inside the `Foo` identifier).
        service.update_file(ref_file.clone(), "Foo subclass: Sub\n".to_string());

        let (name, loc) = service
            .type_hierarchy_prepare_at(&ref_file, Position::new(0, 1))
            .expect("Some for cursor on the Foo class reference");
        assert_eq!(name.as_str(), "Foo");
        let loc = loc.expect("Foo's real class declaration is indexed");
        assert_eq!(
            loc.file, class_file,
            "type hierarchy must resolve to the real class, not the protocol header"
        );
    }
}
