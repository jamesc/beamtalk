// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Language service API for IDE integration.
//!
//! **DDD Context:** Language Service (ADR 0117 step 5, BT-3361 — its own
//! crate, depending on `beamtalk-core`'s Compilation context, never the
//! reverse; see `docs/development/architecture-principles.md` §1)
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
//! use beamtalk_language_service::{LanguageService, SimpleLanguageService};
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
// BT-3342 (ADR 0117 Decision step 3): the query-provider modules
// (`completion_provider`, `definition_provider`, `hover_provider`, ...) that
// used to be the sibling top-level `queries` module — merged in here because
// both were the single Language Service DDD context split into two Rust
// modules with a two-way, previously-unenforced cycle between them (this
// orchestrator called into the providers for query behavior; the providers
// imported result/protocol types — `Position`, `Location`, `Completion`, ...
// — defined below). BT-3361 (ADR 0117 Decision step 5) moved this whole
// module tree — `language_service` and its `queries` submodule — out of
// `beamtalk-core` verbatim into this crate; call sites across the workspace
// were updated from `beamtalk_core::language_service::`/
// `beamtalk_core::queries::` to `beamtalk_language_service::`/
// `beamtalk_language_service::queries::`.
pub mod queries;
pub mod runtime_delegate;
// All test code lives in `tests/`, split by feature (BT-3450): per-feature
// modules plus the property-based (`property_tests`) suite.
#[cfg(test)]
mod tests;
mod value_objects;

// Re-export value objects at the module level
pub use project_index::{ProjectIndex, STDLIB_PACKAGE_MARKER};
pub use runtime_delegate::{
    NavQuery, NavQueryResponse, NavSite, NavSymbolClass, NavSymbolMethod, NavSymbolsResponse,
    RuntimeLocation, line_to_position, nav_site_to_location,
};
pub use value_objects::{
    ByteOffset, CallHierarchyTarget, CodeAction, Completion, CompletionKind, Diagnostic,
    DocumentSymbol, DocumentSymbolKind, HoverInfo, Location, ParameterInfo, Position,
    SignatureHelp, SignatureInfo,
};

use beamtalk_core::ast::{
    Expression, Identifier, MessageSelector, MethodDefinition, Module, Pattern, TypeAnnotation,
};
use beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry;
use beamtalk_core::source_analysis::{Lexer, Span, Token, TokenKind};
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

    /// Returns folding ranges for a file: one per `// === Name ===` section
    /// divider category (BT-3237), plus one per class body and one per
    /// method body (BT-3260) — see
    /// [`crate::queries::folding_range_provider`] for why the latter exist
    /// (indentation-equivalent folding, so registering this provider at all
    /// doesn't regress a divider-less file's fold arrows). Empty only for a
    /// file with no classes, or none with anything multi-line to fold.
    ///
    /// Should respond in <50ms for typical file sizes.
    fn folding_ranges(&self, file: &Utf8PathBuf) -> Vec<Span>;

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
    diagnostics_overrides: beamtalk_core::compilation::diagnostics_policy::DiagnosticsTable,
    /// Whether the LSP server's startup workspace preload is currently
    /// in-flight (BT-3433).
    ///
    /// A `didOpen`/`didChange` for a file racing preload can compute and
    /// publish diagnostics against a `ProjectIndex` that is only partially
    /// populated (e.g. a sibling class not yet indexed), producing a false
    /// `Unresolved class` warning. `preload_workspace_source_files`'s own
    /// startup-sequence caller sets this `true` before preload starts and
    /// `false` only after the project index is fully populated, and
    /// `Backend::publish_diagnostics` (used by `did_open`/`did_change`/
    /// `did_save`/`republish_open_diagnostics` — every caller that records
    /// the file's path into the LSP's open-file map before publishing, not
    /// the shared `publish_diagnostics_impl` those all funnel into, whose
    /// other two callers target URIs that need not be open at all) skips
    /// sending whenever it's `true` — the self-healing
    /// `republish_open_diagnostics` pass that follows is then the only
    /// publish for that file during the race window, so at most one,
    /// correctly-computed notification ever reaches the client instead of
    /// two racing to be last. Both flips happen under the same lock this
    /// flag itself lives behind (`Backend::service`), so a concurrent
    /// `didOpen`'s read of this flag is never reordered relative to the
    /// startup sequence's own flip.
    preload_in_progress: bool,
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

/// Tags whether an identifier match from [`SimpleLanguageService::find_identifier_at_position`]
/// came from a syntactic type-reference position or a plain expression.
///
/// This is a purely syntactic classification — it doesn't require the name to
/// resolve to anything. `TypeReference` covers every AST position where the
/// grammar guarantees a name can only denote a class, protocol, or alias: type
/// annotations (state/class-var/param/return, alias RHS), superclass clauses,
/// `extending:` targets, type-parameter bounds, and constructor/type-pattern
/// class names. `Expression` covers everything else (locals, message args,
/// fields, class-literal references, declaration names). BT-2919: this lets
/// callers like [`SimpleLanguageService::unresolved_type_reference_at`] detect
/// "cursor is on a name that must be a class/alias, but isn't a known one yet"
/// without waiting for the name to resolve.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IdentifierContext {
    TypeReference,
    Expression,
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
            diagnostics_overrides:
                beamtalk_core::compilation::diagnostics_policy::DiagnosticsTable::new(),
            preload_in_progress: false,
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
            diagnostics_overrides:
                beamtalk_core::compilation::diagnostics_policy::DiagnosticsTable::new(),
            preload_in_progress: false,
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

    /// Returns whether workspace preload completed with full coverage (BT-2796).
    ///
    /// ADR 0108 Phase 8 (BT-2901): the LSP server consults this before
    /// answering `textDocument/references` for a type-alias name — when
    /// `false`, files outside the current build graph haven't contributed
    /// their alias reference sites yet, so the response may be incomplete
    /// (see [`Self::is_alias_reference_query`]).
    #[must_use]
    pub fn is_project_complete(&self) -> bool {
        self.project_complete
    }

    /// Declare whether the workspace has package dependencies (BT-2794).
    ///
    /// Pre-WS3, dependency extension contributions are invisible, so when
    /// true (and the project is complete) the receiver-knowledge classifier
    /// keeps every receiver `Open`.
    pub fn set_has_package_dependencies(&mut self, has_deps: bool) {
        self.has_package_dependencies = has_deps;
    }

    /// Declare whether the LSP server's startup workspace preload is
    /// currently in-flight (BT-3433). See the `preload_in_progress` field
    /// doc for why `Backend::publish_diagnostics` consults this.
    pub fn set_preload_in_progress(&mut self, in_progress: bool) {
        self.preload_in_progress = in_progress;
    }

    /// Returns whether the LSP server's startup workspace preload is
    /// currently in-flight (BT-3433).
    #[must_use]
    pub fn is_preload_in_progress(&self) -> bool {
        self.preload_in_progress
    }

    /// Sets the `[diagnostics]` severity-override table loaded from the
    /// workspace's `beamtalk.toml` (ADR 0100 Rule 3, BT-2800).
    ///
    /// Called by the LSP server once per workspace root after loading and
    /// parsing `beamtalk.toml`. An empty table (the default) is a no-op —
    /// diagnostics keep today's Rule 1 completeness-ladder severities.
    pub fn set_diagnostics_overrides(
        &mut self,
        table: beamtalk_core::compilation::diagnostics_policy::DiagnosticsTable,
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

    /// Marks `file` as a stdlib source (BT-2959) so its aliases are stamped
    /// with the stdlib package marker instead of the same-project marker.
    /// Safe to call before or after [`Self::update_file`] — a file indexed
    /// first is re-stamped in place (BT-2961) — see
    /// [`ProjectIndex::mark_stdlib_file`].
    pub fn mark_stdlib_file(&mut self, file: Utf8PathBuf) {
        self.project_index.mark_stdlib_file(file);
    }

    /// Sets the real package name for each known workspace root (BT-2960).
    /// Safe to call before or after files under a root are indexed —
    /// already-stamped aliases are re-stamped against the new root map
    /// (BT-2961) — see [`ProjectIndex::set_root_packages`].
    pub fn set_root_packages(&mut self, root_packages: Vec<(Utf8PathBuf, EcoString)>) {
        self.project_index.set_root_packages(root_packages);
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
        let (ident, _span, _ctx) = self.find_identifier_at_position(file, position)?;
        if self.project_index.hierarchy().has_class(&ident.name) {
            return Some(NavQuery::ReferencesTo(ident.name));
        }
        // ADR 0108 Phase 8 (BT-2901): deliberately `None` for a type-alias
        // name, even though `find_references` (the cold-file AST path)
        // does resolve aliases — type aliases erase entirely at compile
        // time (no BEAM artifact carries the name), so the live runtime's
        // `SystemNavigation referencesTo:` has nothing to answer with for
        // one. Returning `None` here routes the caller (the LSP
        // `references` handler) straight to its `ast_fallback`, which is
        // the only path that can see alias reference sites.
        None
    }

    /// Returns the type alias name at `position`, if the identifier there is
    /// a known alias (ADR 0108 Phase 8, BT-2901).
    ///
    /// Used by the LSP server for two things: (1) deciding whether a
    /// `textDocument/references` response needs the "coverage may be
    /// incomplete" `window/showMessage` warning — find-references coverage
    /// is explicitly scoped to files compiled into the current build graph,
    /// so when [`Self::is_project_complete`] is `false`, a short or empty
    /// alias reference list must not read as exhaustive, since a caller
    /// trusting it could delete an alias still used by an uncompiled file;
    /// and (2) finding the alias's own declaration site (via
    /// [`Self::find_class_declarations`]) to correctly exclude it from a
    /// cold-file `include_declaration = false` request — unlike a class or
    /// protocol name, [`Self::references_query_at`] is always `None` for an
    /// alias (aliases have no runtime representation for
    /// `NavQuery::ReferencesTo` to target), so the usual
    /// `ast_declarations_for_cursor` route never fires for one.
    #[must_use]
    pub fn alias_name_at(&self, file: &Utf8PathBuf, position: Position) -> Option<EcoString> {
        let (ident, _span, _ctx) = self.find_identifier_at_position(file, position)?;
        self.project_index
            .alias_registry()
            .has_alias(&ident.name)
            .then_some(ident.name)
    }

    /// Returns `true` when the cursor at `position` sits on an identifier that
    /// either resolves to a known alias, or is in a syntactic type-reference
    /// position that doesn't resolve to any known class, protocol, or alias.
    ///
    /// Single-AST-walk combination of [`Self::alias_name_at`] and
    /// [`Self::unresolved_type_reference_at`] for the LSP `references()`
    /// incompleteness-warning gate (BT-2919).
    ///
    /// The two checks are mutually exclusive at a given cursor — an alias
    /// name that already resolves can't also count as "unresolved" — so
    /// calling both separately runs [`Self::find_identifier_at_position`]'s
    /// full AST walk twice per request for no benefit. This does the walk
    /// once and evaluates both conditions against the same match.
    #[must_use]
    pub fn has_incomplete_reference_coverage_at(
        &self,
        file: &Utf8PathBuf,
        position: Position,
    ) -> bool {
        let Some((ident, _span, ctx)) = self.find_identifier_at_position(file, position) else {
            return false;
        };
        if self.project_index.alias_registry().has_alias(&ident.name) {
            return true;
        }
        ctx == IdentifierContext::TypeReference
            && !self.project_index.hierarchy().has_class(&ident.name)
    }

    /// Returns the name at `position` when the cursor sits on an identifier in
    /// a syntactic type-reference position (a type annotation, superclass
    /// clause, `extending:` target, type-param bound, or constructor/type
    /// pattern class name) that does **not** resolve to any known class,
    /// protocol, or alias (BT-2919).
    ///
    /// This is the counterpart to [`Self::alias_name_at`] for the gap BT-2919
    /// identified: `alias_name_at` (and the `has_class`/`has_alias` routing
    /// gate in [`Self::find_references`]) can only recognize a name that's
    /// *already* registered in the project index — but if the name's own
    /// declaring file hasn't been indexed yet (preload hasn't reached it, or
    /// the preload budget ran out first), the name looks like a plain unknown
    /// identifier even though the cursor position proves it can only be a
    /// class, protocol, or alias reference. The LSP genuinely cannot tell
    /// whether this is a typo or an as-yet-unindexed name, so callers use this
    /// to decide whether an empty or short find-references result needs the
    /// same "coverage may be incomplete" warning that a *resolved* alias gets.
    #[must_use]
    pub fn unresolved_type_reference_at(
        &self,
        file: &Utf8PathBuf,
        position: Position,
    ) -> Option<EcoString> {
        let (ident, _span, ctx) = self.find_identifier_at_position(file, position)?;
        if ctx != IdentifierContext::TypeReference {
            return None;
        }
        let known = self.project_index.hierarchy().has_class(&ident.name)
            || self.project_index.alias_registry().has_alias(&ident.name);
        (!known).then_some(ident.name)
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
        let (ident, _span, _ctx) = self.find_identifier_at_position(file, position)?;
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
    ) -> Option<(Identifier, Span, IdentifierContext)> {
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

        // ADR 0108 Phase 8 (BT-2901): Check type alias declarations (the
        // alias name itself, and any name referenced in its RHS
        // annotation) so goto-definition/find-references work when the
        // cursor is on the declaration site, e.g. `type RestartStrategy =
        // ...` or a reference to another alias inside the RHS.
        for alias in &file_data.module.type_aliases {
            if offset_val >= alias.name.span.start() && offset_val < alias.name.span.end() {
                return Some((
                    alias.name.clone(),
                    alias.name.span,
                    IdentifierContext::TypeReference,
                ));
            }
            if let Some(ident) =
                Self::find_identifier_in_type_annotation(&alias.annotation, offset_val)
            {
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
        class: &beamtalk_core::ast::ClassDefinition,
        offset: ByteOffset,
        offset_val: u32,
    ) -> Option<(Identifier, Span, IdentifierContext)> {
        if offset_val >= class.name.span.start() && offset_val < class.name.span.end() {
            return Some((
                class.name.clone(),
                class.name.span,
                IdentifierContext::TypeReference,
            ));
        }
        if let Some(ref superclass) = class.superclass {
            if offset_val >= superclass.span.start() && offset_val < superclass.span.end() {
                return Some((
                    superclass.clone(),
                    superclass.span,
                    IdentifierContext::TypeReference,
                ));
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
        protocol: &beamtalk_core::ast::ProtocolDefinition,
        offset_val: u32,
    ) -> Option<(Identifier, Span, IdentifierContext)> {
        if offset_val >= protocol.name.span.start() && offset_val < protocol.name.span.end() {
            return Some((
                protocol.name.clone(),
                protocol.name.span,
                IdentifierContext::TypeReference,
            ));
        }
        if let Some(ref extending) = protocol.extending {
            if offset_val >= extending.span.start() && offset_val < extending.span.end() {
                return Some((
                    extending.clone(),
                    extending.span,
                    IdentifierContext::TypeReference,
                ));
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
        smd: &beamtalk_core::ast::StandaloneMethodDefinition,
        offset: ByteOffset,
        offset_val: u32,
    ) -> Option<(Identifier, Span, IdentifierContext)> {
        if offset_val >= smd.class_name.span.start() && offset_val < smd.class_name.span.end() {
            return Some((
                smd.class_name.clone(),
                smd.class_name.span,
                IdentifierContext::TypeReference,
            ));
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
        parameters: &[beamtalk_core::ast::ParameterDefinition],
        return_type: Option<&TypeAnnotation>,
        body: &[beamtalk_core::ast::ExpressionStatement],
        offset: ByteOffset,
        offset_val: u32,
    ) -> Option<(Identifier, Span, IdentifierContext)> {
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
        type_params: &[beamtalk_core::ast::TypeParamDecl],
        offset_val: u32,
    ) -> Option<(Identifier, Span, IdentifierContext)> {
        for param in type_params {
            if let Some(bound) = &param.bound {
                if offset_val >= bound.span.start() && offset_val < bound.span.end() {
                    return Some((bound.clone(), bound.span, IdentifierContext::TypeReference));
                }
            }
        }
        None
    }

    fn find_identifier_in_type_annotation(
        annotation: &TypeAnnotation,
        offset_val: u32,
    ) -> Option<(Identifier, Span, IdentifierContext)> {
        match annotation {
            TypeAnnotation::Simple(identifier) => {
                if offset_val >= identifier.span.start() && offset_val < identifier.span.end() {
                    Some((
                        identifier.clone(),
                        identifier.span,
                        IdentifierContext::TypeReference,
                    ))
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
                    return Some((base.clone(), base.span, IdentifierContext::TypeReference));
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
                    Some((
                        class_name.clone(),
                        class_name.span,
                        IdentifierContext::TypeReference,
                    ))
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
    ) -> Option<(Identifier, Span, IdentifierContext)> {
        let offset_val = offset.get();
        let span = expr.span();
        if offset_val < span.start() || offset_val >= span.end() {
            return None;
        }

        match expr {
            Expression::Identifier(ident) => {
                if offset_val >= ident.span.start() && offset_val < ident.span.end() {
                    Some((ident.clone(), ident.span, IdentifierContext::Expression))
                } else {
                    None
                }
            }
            Expression::ClassReference { name, .. } => {
                if offset_val >= name.span.start() && offset_val < name.span.end() {
                    Some((name.clone(), name.span, IdentifierContext::Expression))
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
                    Some((field.clone(), field.span, IdentifierContext::Expression))
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
                if let beamtalk_core::ast::StringSegment::Interpolation(expr) = seg {
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
    ) -> Option<(Identifier, Span, IdentifierContext)> {
        let span = pattern.span();
        if offset_val < span.start() || offset_val >= span.end() {
            return None;
        }
        match pattern {
            Pattern::Constructor {
                class, keywords, ..
            } => {
                if offset_val >= class.span.start() && offset_val < class.span.end() {
                    return Some((class.clone(), class.span, IdentifierContext::TypeReference));
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
                    Some((class.clone(), class.span, IdentifierContext::TypeReference))
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
                    if let beamtalk_core::ast::StringSegment::Interpolation(expr) = segment {
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
        use beamtalk_core::source_analysis::{lex_with_eof, parse};

        let tokens = lex_with_eof(&content);
        let (module, diagnostics) = parse(tokens);

        // Build class hierarchy for the project index.
        // This is intentionally lightweight (ClassHierarchy::build only, not full
        // analyse()) since diagnostic_provider lazily runs full semantic analysis
        // when diagnostics are requested, avoiding duplicate work.
        let (class_hierarchy_result, hierarchy_diags) =
            beamtalk_core::semantic_analysis::ClassHierarchy::build(&module);
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
                .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error);
            if has_parse_errors {
                class_hierarchy.mark_module_classes_surface_incomplete(&module);
            }

            // Update the project-wide index with this file's class hierarchy
            self.project_index
                .update_file(file.clone(), &class_hierarchy);

            // BT-2795: Track this file's standalone extension definitions so
            // other files' diagnostics see them (cross-file extension
            // visibility, ADR 0066 / ADR 0100 Rule 2 WS1).
            let mut extensions = beamtalk_core::compilation::extension_index::ExtensionIndex::new();
            extensions.add_module(&module, file.as_std_path());
            self.project_index
                .set_file_extensions(file.clone(), extensions);

            // ADR 0108 Phase 8 (BT-2901): track this file's `type`
            // declarations in the project-wide alias registry, so
            // completions/goto-definition/find-references/hover can resolve
            // alias names cross-file the same way they resolve classes.
            let alias_infos =
                beamtalk_core::semantic_analysis::AliasRegistry::extract_alias_infos(&module);
            self.project_index
                .update_file_aliases(file.clone(), alias_infos);

            // BT-2950: track this file's `Protocol define: ...` declarations
            // too, so `extending:`/conformance checks against a protocol
            // declared in a different project file or indexed dependency
            // resolve during LSP diagnostics — mirrors the alias tracking
            // immediately above (BT-2910 already wired the CLI `build`/`lint`
            // side of this; this closes the LSP parity gap).
            let protocol_infos =
                beamtalk_core::semantic_analysis::ProtocolRegistry::extract_protocol_infos(&module);
            self.project_index
                .update_file_protocols(file.clone(), protocol_infos);
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
                let mut options = beamtalk_core::CompilerOptions::default();
                if self.project_index.is_stdlib_file(file) {
                    options.stdlib_mode = true;
                }
                // BT-2796: After a full-coverage workspace preload the
                // ProjectIndex holds every project file's classes, so the
                // injected knowledge is project-complete.
                if self.project_complete {
                    options.knowledge_scope =
                        beamtalk_core::semantic_analysis::KnowledgeScope::ProjectComplete;
                }
                options.has_package_dependencies = self.has_package_dependencies;
                // BT-2951: `current_package` so `AliasRegistry::add_pre_loaded`'s
                // seeding-boundary exclusion (ADR 0108 Phase 5) actually has
                // real package data to filter `pre_loaded_aliases` on below —
                // without this, every entry's `package` looks unset from the
                // exclusion check's point of view and a dependency's
                // `internal type Foo = ...` leaks into every file's
                // diagnostics. Must match `cross_file_alias_infos_for`'s
                // entries' own stamping exactly — see
                // `ProjectIndex::alias_package_for_file`'s doc.
                options.current_package =
                    Some(self.project_index.alias_package_for_file(file).to_string());
                // BT-2795: Cross-file extensions from the ProjectIndex are
                // passed so a same-project `ClassName >> selector` defined in
                // another file resolves instead of producing a false Dnu hint.
                let cross_file_extensions = self.project_index.cross_file_extensions_for(file);
                // BT-2928: Cross-file type aliases from the ProjectIndex, so a
                // `type Name = ...` declared in another project file resolves
                // instead of leaving `Dynamic (dynamic receiver)` behind —
                // mirrors `cross_file_classes` immediately above.
                let pre_loaded_aliases = self.project_index.cross_file_alias_infos_for(file);
                // BT-2950: Cross-file protocol declarations from the
                // ProjectIndex, so `extending:`/conformance checks against a
                // `Protocol define: Name ...` declared in another project
                // file or dependency resolve instead of degrading to
                // "unknown protocol" — parity with the CLI's `build`/`lint`
                // wiring (BT-2910), mirrors `cross_file_classes` above.
                let pre_loaded_protocols = self.project_index.cross_file_protocol_infos_for(file);
                // BT-1846/BT-1847: a `stubs/lists.bt` opened directly in an
                // editor must not be diagnosed as if it were an ordinary
                // src/ file — `declare native:` is only legal there. See
                // `ProjectIndex::is_stub_file`'s doc for why this can't be a
                // tracked-membership check like `is_stdlib_file`.
                let is_stub_file = self.project_index.is_stub_file(file);
                // BT-3431: file basename (without extension), so the shared
                // pipeline can validate it agrees with the class declared
                // here — see `ProjectDiagnosticContext::source_file_stem`'s doc.
                let source_file_stem = file.file_stem().map(std::string::ToString::to_string);
                let ctx = crate::queries::diagnostic_provider::ProjectDiagnosticContext {
                    options,
                    cross_file_classes,
                    pre_loaded_protocols,
                    pre_loaded_aliases,
                    cross_file_extensions,
                    native_type_registry: self.native_types.clone(),
                    // BT-2800: apply the same `beamtalk.toml` `[diagnostics]`
                    // severity-override table `beamtalk build` uses, so the
                    // LSP never disagrees with the CLI about a diagnostic's
                    // severity.
                    diagnostics_overrides: self.diagnostics_overrides.clone(),
                    is_stub_file,
                    source_file_stem,
                    ..Default::default()
                };
                crate::queries::diagnostic_provider::compute_project_diagnostics(
                    &data.module,
                    &data.source,
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
        // ADR 0108 Phase 8 (BT-2901): Pass the project-wide alias registry so
        // alias names are offered alongside class/protocol names in
        // type-annotation position.
        crate::queries::completion_provider::compute_completions_with_aliases(
            &file_data.module,
            &file_data.source,
            position,
            self.project_index.hierarchy(),
            current_package.as_deref(),
            self.native_types.as_deref(),
            Some(self.project_index.alias_registry()),
        )
    }

    fn hover(&self, file: &Utf8PathBuf, position: Position) -> Option<HoverInfo> {
        let file_data = self.get_file(file)?;

        // ADR 0108 Phase 8 (BT-2901): `ProjectIndex` now tracks a
        // project-wide `AliasRegistry` (see `update_file`), so an
        // alias-typed value's hover resolves and renders
        // `AliasName (expansion)` instead of falling back to its bare
        // structural expansion.
        crate::queries::hover_provider::compute_hover(
            &file_data.module,
            &file_data.source,
            position,
            self.project_index.hierarchy(),
            self.native_types.as_deref(),
            Some(self.project_index.alias_registry()),
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
        let (ident, _span, _ctx) = self.find_identifier_at_position(file, position)?;

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
        let Some((ident, _span, ctx)) = self.find_identifier_at_position(file, position) else {
            return Vec::new();
        };

        // If the identifier is a class, protocol, or type-alias name, use
        // class-aware references. ADR 0108 Phase 8 (BT-2901): aliases are
        // never registered into `ClassHierarchy` (they're a peer namespace,
        // collision-checked against it instead), so `has_class` alone can't
        // see them — the `alias_registry().has_alias` check closes that gap.
        // `find_class_references` itself walks `module.type_aliases` (both
        // the declaration site and RHS references), so it already handles
        // an alias name correctly once routed here.
        //
        // BT-2919: also route here when the cursor is in a syntactic
        // type-reference position (annotation, superclass, `extending:`,
        // type-param bound, constructor/type pattern) even if the name
        // *doesn't* resolve to a known class/protocol/alias yet — e.g. the
        // name's declaring file hasn't been indexed. `find_class_references`
        // matches by name text across every indexed file rather than
        // requiring the name to be pre-registered, so it's always a superset
        // of what the identifier-only fallback below could find for this
        // position; that fallback only walks `Expression` nodes and never
        // looks at type annotations at all, so it was structurally
        // guaranteed to return nothing here.
        if self.project_index.hierarchy().has_class(&ident.name)
            || self.project_index.alias_registry().has_alias(&ident.name)
            || ctx == IdentifierContext::TypeReference
        {
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

        crate::queries::document_symbols_provider::compute_document_symbols(
            &file_data.module,
            &file_data.source,
        )
    }

    fn folding_ranges(&self, file: &Utf8PathBuf) -> Vec<Span> {
        let Some(file_data) = self.get_file(file) else {
            return Vec::new();
        };

        crate::queries::folding_range_provider::compute_folding_ranges(
            &file_data.module,
            &file_data.source,
        )
    }

    fn code_actions(&self, file: &Utf8PathBuf, start: u32, end: u32) -> Vec<CodeAction> {
        let Some(file_data) = self.get_file(file) else {
            return Vec::new();
        };

        let inferred = beamtalk_core::semantic_analysis::type_checker::infer_method_return_types(
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
