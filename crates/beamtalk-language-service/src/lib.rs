// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Language service API for IDE integration.
//!
//! **DDD Context:** Language Service (ADR 0117 step 5 — its own
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

mod hierarchy;
mod identifier_lookup;
mod project_index;
mod rename;
// ADR 0117 Decision step 3: the query-provider modules
// (`completion_provider`, `definition_provider`, `hover_provider`, ...) that
// used to be the sibling top-level `queries` module — merged in here because
// both were the single Language Service DDD context split into two Rust
// modules with a two-way, previously-unenforced cycle between them (this
// orchestrator called into the providers for query behavior; the providers
// imported result/protocol types — `Position`, `Location`, `Completion`, ...
// — defined below). ADR 0117 Decision step 5 moved this whole
// module tree — `language_service` and its `queries` submodule — out of
// `beamtalk-core` verbatim into this crate; call sites across the workspace
// were updated from `beamtalk_core::language_service::`/
// `beamtalk_core::queries::` to `beamtalk_language_service::`/
// `beamtalk_language_service::queries::`.
pub mod queries;
pub mod runtime_delegate;
// All test code lives in `tests/`, split by feature: per-feature modules
// plus the property-based (`property_tests`) suite.
#[cfg(test)]
mod tests;
mod value_objects;

// Re-export value objects at the module level
pub use project_index::{FileInterface, ProjectIndex, STDLIB_PACKAGE_MARKER};
pub use rename::{FileMove, PreparedRename, RenameEdit, RenameKind, RenamePlan, RenameRefusal};
pub use runtime_delegate::{
    NavQuery, NavQueryResponse, NavSite, NavSymbolClass, NavSymbolMethod, NavSymbolsResponse,
    RuntimeLocation, line_to_position, nav_site_to_location,
};
pub use value_objects::{
    ByteOffset, CallHierarchyTarget, CodeAction, Completion, CompletionKind, Diagnostic,
    DocumentSymbol, DocumentSymbolKind, HoverInfo, Location, ParameterInfo, Position,
    SignatureHelp, SignatureInfo, SourceEdit,
};

use beamtalk_core::ast::{
    BinarySegment, Expression, Identifier, MessageSelector, MethodDefinition, Module, Pattern,
    TypeAnnotation,
};
use beamtalk_core::ast_walker::{CursorBound, children_in_source_order};
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
    /// divider category, plus one per class body and one per
    /// method body — see
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
    /// methods whose return type can be inferred by the `TypeChecker`, and
    /// the ADR 0133 "return the value instead" fix on an escaping-block
    /// diagnostic (`queries::return_value_fix_provider`).
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
    /// Per-file FFI registry that replaces `native_types` for diagnostics of
    /// that file (BT-4967): a file under the monorepo's `stdlib/` tree has no
    /// `beamtalk.toml`, so no root contributes a registry for it. The LSP
    /// fills this from the registry `build`/`test`/`lint` share
    /// (`ffi_type_registry::lint_type_registry`); this crate does not decide
    /// which files qualify.
    file_native_types: HashMap<Utf8PathBuf, std::sync::Arc<NativeTypeRegistry>>,
    /// Project-known Erlang modules for the `unresolved_ffi` check (BT-4022):
    /// hex-dep/native ebin modules plus own `native/*.erl`, unioned across
    /// workspace roots by the LSP server at startup (mirrors `native_types`).
    /// Default (unset) disables the check — no unresolved-module diagnostics.
    known_erlang_modules: beamtalk_core::ffi_type_specs::KnownErlangModules,
    /// Per-workspace-root package names a `pkg@Class` qualifier may name
    /// (ADR 0070 Phase 2, BT-4928): each root's manifest `[dependencies]`,
    /// the package itself and `stdlib`, built by
    /// [`beamtalk_core::semantic_analysis::known_qualifier_packages`]. A file
    /// is checked against the set of the most specific root containing it; a
    /// file under no registered root (no manifest, unreadable manifest) is
    /// never checked.
    root_qualifier_packages: Vec<(Utf8PathBuf, std::collections::HashSet<String>)>,
    /// Whether workspace preload has completed with full coverage.
    ///
    /// When `true`, diagnostics are computed with
    /// `KnowledgeScope::ProjectComplete` — the `ProjectIndex` holds every
    /// project file's classes, so the receiver-knowledge classifier may
    /// treat missing parents as genuinely unresolved rather than
    /// not-yet-seen. Set by the LSP server after `preload_workspace_source_files`
    /// finishes within its file budget; stays `false` if the budget was
    /// exhausted (coverage would be partial).
    project_complete: bool,
    /// Per-category diagnostic severity overrides from the workspace's
    /// `beamtalk.toml` `[diagnostics]` section (ADR 0100 Rule 3).
    /// Empty (the default) preserves today's Rule 1 completeness-ladder
    /// defaults — the same behaviour as before this field existed. Set by
    /// the LSP server after loading `beamtalk.toml` from each workspace
    /// root, so the LSP agrees with `beamtalk build` on diagnostic severity.
    diagnostics_overrides: beamtalk_core::compilation::diagnostics_policy::DiagnosticsTable,
    /// Whether the LSP server's startup workspace preload is currently
    /// in-flight.
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
    /// The parser reported an error, so recovery may have dropped part of
    /// the file's declarations (BT-5188).
    has_parse_errors: bool,
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
/// fields, class-literal references, declaration names). This lets
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
            file_native_types: HashMap::new(),
            known_erlang_modules: beamtalk_core::ffi_type_specs::KnownErlangModules::default(),
            root_qualifier_packages: Vec::new(),
            project_complete: false,
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
            file_native_types: HashMap::new(),
            known_erlang_modules: beamtalk_core::ffi_type_specs::KnownErlangModules::default(),
            root_qualifier_packages: Vec::new(),
            project_complete: false,
            diagnostics_overrides:
                beamtalk_core::compilation::diagnostics_policy::DiagnosticsTable::new(),
            preload_in_progress: false,
        }
    }

    /// Declare that workspace preload completed with full coverage.
    ///
    /// After this, diagnostics run with `KnowledgeScope::ProjectComplete`.
    /// Only call when the preload walked every project source file — a
    /// budget-exhausted preload must NOT claim completeness, or the
    /// receiver-knowledge classifier would mistake unseen files for
    /// genuinely-unresolved classes.
    pub fn set_project_complete(&mut self, complete: bool) {
        self.project_complete = complete;
    }

    /// Returns whether workspace preload completed with full coverage.
    ///
    /// ADR 0108 Phase 8: the LSP server consults this before
    /// answering `textDocument/references` for a type-alias name — when
    /// `false`, files outside the current build graph haven't contributed
    /// their alias reference sites yet, so the response may be incomplete
    /// (see [`Self::is_alias_reference_query`]).
    #[must_use]
    pub fn is_project_complete(&self) -> bool {
        self.project_complete
    }

    /// Declare whether the LSP server's startup workspace preload is
    /// currently in-flight. See the `preload_in_progress` field
    /// doc for why `Backend::publish_diagnostics` consults this.
    pub fn set_preload_in_progress(&mut self, in_progress: bool) {
        self.preload_in_progress = in_progress;
    }

    /// Returns whether the LSP server's startup workspace preload is
    /// currently in-flight.
    #[must_use]
    pub fn is_preload_in_progress(&self) -> bool {
        self.preload_in_progress
    }

    /// Sets the `[diagnostics]` severity-override table loaded from the
    /// workspace's `beamtalk.toml` (ADR 0100 Rule 3).
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

    /// Sets (`Some`) or clears (`None`) the FFI registry used for `file`'s
    /// diagnostics in place of the session-wide one (BT-4967).
    pub fn set_file_native_types(
        &mut self,
        file: &Utf8PathBuf,
        registry: Option<NativeTypeRegistry>,
    ) {
        match registry {
            Some(registry) => {
                self.file_native_types
                    .insert(file.clone(), std::sync::Arc::new(registry));
            }
            None => {
                self.file_native_types.remove(file);
            }
        }
    }

    /// Returns a reference to the native type registry, if set.
    #[must_use]
    pub fn native_types(&self) -> Option<&NativeTypeRegistry> {
        self.native_types.as_deref()
    }

    /// Sets the project-known Erlang modules for the `unresolved_ffi` check
    /// (BT-4022) — hex-dep/native ebin modules plus own `native/*.erl`.
    ///
    /// Called by the LSP server at startup with the union across workspace
    /// roots (mirrors [`set_native_types`](Self::set_native_types)); like
    /// that setter, this is a whole-session simplification loaded once —
    /// modules added by a later `beamtalk build` need an LSP restart.
    pub fn set_known_erlang_modules(
        &mut self,
        modules: beamtalk_core::ffi_type_specs::KnownErlangModules,
    ) {
        self.known_erlang_modules = modules;
    }

    /// Returns the project-known Erlang modules for the `unresolved_ffi`
    /// check (BT-4022), if any were set.
    #[must_use]
    pub fn known_erlang_modules(&self) -> &beamtalk_core::ffi_type_specs::KnownErlangModules {
        &self.known_erlang_modules
    }

    /// Sets the `pkg@Class` qualifier package set for each workspace root
    /// (ADR 0070 Phase 2, BT-4928).
    ///
    /// Replaces the whole map — pass every root whose manifest yielded a set
    /// (see [`beamtalk_core::semantic_analysis::known_qualifier_packages`]);
    /// a root with a missing or unreadable manifest is omitted, so its files
    /// are not checked rather than falsely flagged.
    ///
    /// Loaded once at startup (and when a new root is first opened), like
    /// [`set_known_erlang_modules`](Self::set_known_erlang_modules): a later
    /// edit to a manifest's `[dependencies]` needs an LSP restart, until
    /// then a newly declared package still shows "Unknown package".
    pub fn set_root_qualifier_packages(
        &mut self,
        root_packages: Vec<(Utf8PathBuf, std::collections::HashSet<String>)>,
    ) {
        self.root_qualifier_packages = root_packages;
    }

    /// The qualifier package set of the most specific registered root
    /// containing `file`, or `None` when no root does. A file in a fetched
    /// dependency checkout (`_build/deps/<name>/…`) is checked only against
    /// its own checkout's set - never inherited from the consumer root (the
    /// CLI's `dependency_compile_options` rule) - so a checkout that is not
    /// registered (stale, renamed, non-UTF-8) is skipped.
    fn known_qualifier_packages_for(
        &self,
        file: &camino::Utf8Path,
    ) -> Option<std::collections::HashSet<String>> {
        let (root, packages) =
            project_index::most_specific_root(&self.root_qualifier_packages, file)?;
        if project_index::is_dependency_checkout_file(file)
            && !project_index::is_dependency_checkout_file(root)
        {
            return None;
        }
        Some(packages.clone())
    }

    /// Returns a reference to the project index.
    #[must_use]
    pub fn project_index(&self) -> &ProjectIndex {
        &self.project_index
    }

    /// Marks `file` as a stdlib source so its aliases are stamped
    /// with the stdlib package marker instead of the same-project marker.
    /// Safe to call before or after [`Self::update_file`] — a file indexed
    /// first is re-stamped in place — see
    /// [`ProjectIndex::mark_stdlib_file`].
    pub fn mark_stdlib_file(&mut self, file: Utf8PathBuf) {
        self.project_index.mark_stdlib_file(file);
    }

    /// Sets the real package name for each known workspace root.
    /// Safe to call before or after files under a root are indexed —
    /// already-stamped aliases are re-stamped against the new root map
    /// — see [`ProjectIndex::set_root_packages`].
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
        if !info.function_name.is_empty()
            && let Some(sig) = self
                .native_types
                .as_deref()
                .and_then(|reg| reg.lookup(&info.module_name, &info.function_name, info.arity))
        {
            info.line = sig.line;
        }

        Some(info)
    }

    /// Whether the indexed text of `file` has a parse error, so error
    /// recovery may have dropped some of its declarations (BT-5188).
    /// `false` for a file that is not indexed.
    #[must_use]
    pub fn has_parse_errors(&self, file: &Utf8PathBuf) -> bool {
        self.files
            .get(file)
            .is_some_and(|data| data.has_parse_errors)
    }

    /// Returns the cached source text for a file, if available.
    #[must_use]
    pub fn file_source(&self, file: &Utf8PathBuf) -> Option<String> {
        self.files.get(file).map(|data| data.source.clone())
    }

    /// Classify the cursor for a
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

    /// Find every *call site* of `selector_name` across every
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

    /// Classify the cursor for a Find-References request and
    /// return the equivalent [`NavQuery`], or `None` when the cursor is
    /// on a local identifier that can't be expressed as a runtime
    /// navigation query.
    ///
    /// Public so `beamtalk-lsp` can build runtime-delegate requests with
    /// the *same* classification logic the AST walker uses — keeping the
    /// two modes in lockstep is a foundational invariant of the epic.
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
        // ADR 0108 Phase 8: deliberately `None` for a type-alias
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
    /// a known alias (ADR 0108 Phase 8).
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
    /// incompleteness-warning gate.
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
    /// protocol, or alias.
    ///
    /// This is the counterpart to [`Self::alias_name_at`] for the gap
    /// this fills: `alias_name_at` (and the `has_class`/`has_alias` routing
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

    /// Find the **declaration sites** (method-definition headers)
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

    /// Find the **class declaration sites** (the class-name token
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

    /// Classify the cursor for a `textDocument/implementation`
    /// request and return the equivalent [`NavQuery::ImplementorsOf`], or
    /// `None` when the cursor is not on a selector.
    ///
    /// Public so `beamtalk-lsp` builds the runtime-delegate request with
    /// the *same* classification logic the AST walker uses — keeping the
    /// runtime and cold-file modes in lockstep is a foundational invariant
    /// of the epic.
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

    /// Find every class that defines `selector_name` across all
    /// indexed files. Mirrors `SystemNavigation implementorsOf:`
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
}

impl SimpleLanguageService {
    /// Indexes one file. `reindex_siblings` lets a dependency file whose
    /// `internal type` aliases changed re-index its package siblings, whose
    /// exported signatures were expanded against the old alias set (BT-4778).
    ///
    /// Returns whether this was a dependency file whose `internal` aliases
    /// changed, i.e. whether its package siblings are now stale — a caller
    /// passing `reindex_siblings = false` must deal with that itself.
    fn index_file(&mut self, file: Utf8PathBuf, content: String, reindex_siblings: bool) -> bool {
        use beamtalk_core::source_analysis::{lex_with_eof, parse};

        let tokens = lex_with_eof(&content);
        let (mut module, mut diagnostics) = parse(tokens);
        let has_parse_errors = diagnostics
            .iter()
            .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error);
        // A file is one compilation unit for its class (BT-4721): the project
        // index sees same-file extensions as methods of their class. The stored
        // module is folded, so the fold's own diagnostics (an instance-side
        // extension on a `native:` actor) are kept with the parse diagnostics:
        // analysis of the folded module cannot rediscover them.
        diagnostics.extend(fold_and_check_extensions(&mut module));

        // Build class hierarchy for the project index.
        // This is intentionally lightweight (ClassHierarchy::build only, not full
        // analyse()) since diagnostic_provider lazily runs full semantic analysis
        // when diagnostics are requested, avoiding duplicate work.
        //
        // Trait provenance (ADR 0127, BT-3655): flatten `uses:` and stamp
        // `MethodInfo::origin` here, at the source that builds `MethodInfo`
        // for the LSP path, so hover needn't re-derive it.
        let (class_hierarchy_result, hierarchy_diags) =
            crate::project_index::build_hierarchy_with_trait_origins(
                &module,
                Some(self.project_index.alias_package_for_file(&file).as_str()),
                || {
                    // First definition wins, as in analysis (BT-3915).
                    beamtalk_core::semantic_analysis::trait_expansion::first_wins_protocol_map(
                        self.project_index.cross_file_protocol_defs_for(&file),
                    )
                },
            );
        let aliases_changed;
        if let Ok(mut class_hierarchy) = class_hierarchy_result {
            // Register protocol definitions as synthetic class entries
            // so LSP features (completions, has_class) work with protocol names.
            class_hierarchy.register_protocol_classes(&module);

            // A file with parse errors may have an under-recovered
            // method surface (error recovery can drop method definitions).
            // Mark its classes so cross-file consumers of this file's
            // hierarchy never emit unresolved-selector hints against a
            // surface the parser never fully saw.
            if has_parse_errors {
                class_hierarchy.mark_module_classes_surface_incomplete(&module);
            }

            // A dependency's own `internal type` aliases are never seeded
            // into a consumer's alias scope (ADR 0108), so expand them in the
            // signatures this file exports, as the CLI does (BT-4778 /
            // BT-4615 / BT-4707). Done after trait flattening above, which
            // reads the unexpanded ASTs in the dependency's own scope.
            let alias_infos =
                beamtalk_core::semantic_analysis::AliasRegistry::extract_alias_infos(&module);
            let mut protocol_infos =
                beamtalk_core::semantic_analysis::ProtocolRegistry::extract_protocol_infos(&module);
            let mut protocol_defs: Vec<_> = module
                .protocols
                .iter()
                .filter(|p| {
                    beamtalk_core::semantic_analysis::trait_expansion::is_carried_protocol(p)
                })
                .cloned()
                .collect();
            let is_dependency = crate::project_index::is_dependency_file(&file);
            let old_internal_aliases = self.project_index.internal_aliases_of(&file);
            if is_dependency {
                // `internal` is package-private (ADR 0108): expand against
                // every indexed file of this dependency, not just this one.
                let mut package_aliases = self.project_index.dependency_sibling_aliases(&file);
                package_aliases.extend(alias_infos.iter().cloned());
                let expander =
                    beamtalk_core::semantic_analysis::alias_expansion::InternalAliasExpander::from_alias_infos(
                        &package_aliases,
                    );
                class_hierarchy.map_class_infos(|info| expander.expand_class_info(info));
                for info in &mut protocol_infos {
                    expander.expand_protocol_info(info);
                }
                beamtalk_project::dependency_aliases::expand_dependency_protocol_defs(
                    &package_aliases,
                    &mut protocol_defs,
                );
            }

            // Update the project-wide index with this file's class hierarchy
            self.project_index
                .update_file(file.clone(), &class_hierarchy);

            // ADR 0108 Phase 8: track this file's `type`
            // declarations in the project-wide alias registry, so
            // completions/goto-definition/find-references/hover can resolve
            // alias names cross-file the same way they resolve classes.
            self.project_index
                .update_file_aliases(file.clone(), alias_infos);
            aliases_changed = is_dependency
                && self.project_index.internal_aliases_of(&file) != old_internal_aliases;

            // Track this file's `Protocol define: ...` declarations
            // too, so `extending:`/conformance checks against a protocol
            // declared in a different project file or indexed dependency
            // resolve during LSP diagnostics — mirrors the alias tracking
            // immediately above (the CLI `build`/`lint` side of this is
            // already wired; this closes the LSP parity gap).
            self.project_index
                .update_file_protocols(file.clone(), protocol_infos);

            // Track this file's carried protocols' full ASTs
            // (BT-3591) so a cross-file/cross-package `uses:` flattens
            // during LSP diagnostics the same way `beamtalk build` already
            // does, and record which protocols this file's own classes
            // `uses:` (the protocol → users edge — see
            // `ProjectIndex::users_of_protocol`'s doc), so an editor/LSP
            // handler can re-analyse a protocol's open users after it changes.
            self.project_index
                .update_file_protocol_defs(file.clone(), protocol_defs);
            self.project_index
                .update_file_protocol_uses(file.clone(), &module);
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
                    has_parse_errors,
                },
            );
            return false;
        }

        self.files.insert(
            file.clone(),
            FileData {
                source: content,
                module,
                diagnostics,
                has_parse_errors,
            },
        );

        // This dependency file's `internal` aliases changed: its siblings'
        // exported signatures were expanded against the old set (or, during a
        // preload, before this file was indexed), so re-index them (BT-4778).
        if reindex_siblings && aliases_changed {
            self.reindex_dependency_siblings(&file, false);
        }
        aliases_changed
    }
}

impl SimpleLanguageService {
    /// Indexes many files at once — a workspace preload. Equivalent to
    /// [`LanguageService::update_file`] on each, but a dependency package
    /// whose `internal` aliases changed has its files re-indexed once after
    /// the whole batch, not once per alias-bearing file (quadratic in the
    /// package's size for a large dependency).
    pub fn update_files(&mut self, files: impl IntoIterator<Item = (Utf8PathBuf, String)>) {
        let mut stale_packages: Vec<Utf8PathBuf> = Vec::new();
        for (file, content) in files {
            if self.index_file(file.clone(), content, false)
                && !stale_packages
                    .iter()
                    .any(|seen| crate::project_index::same_dependency_package(seen, &file))
            {
                stale_packages.push(file);
            }
        }
        // Every file's aliases are indexed now, so one more pass expands each
        // file against its package's complete alias set.
        for file in &stale_packages {
            self.reindex_dependency_siblings(file, true);
        }
    }

    /// Re-indexes the other indexed files of `file`'s dependency package, and
    /// `file` itself when `include_file`.
    fn reindex_dependency_siblings(&mut self, file: &Utf8PathBuf, include_file: bool) {
        let siblings: Vec<(Utf8PathBuf, String)> = self
            .files
            .iter()
            .filter(|(other, _)| {
                (include_file || *other != file)
                    && crate::project_index::same_dependency_package(other, file)
            })
            .map(|(other, data)| (other.clone(), data.source.clone()))
            .collect();
        for (other, source) in siblings {
            self.index_file(other, source, false);
        }
    }
}

impl LanguageService for SimpleLanguageService {
    fn update_file(&mut self, file: Utf8PathBuf, content: String) {
        self.index_file(file, content, true);
    }

    fn remove_file(&mut self, file: &Utf8PathBuf) {
        let had_internal_aliases = !self.project_index.internal_aliases_of(file).is_empty();
        self.project_index.remove_file(file);
        self.files.remove(file);
        self.file_native_types.remove(file);
        // Siblings were expanded against this file's aliases (BT-4778).
        if had_internal_aliases && crate::project_index::is_dependency_file(file) {
            self.reindex_dependency_siblings(file, false);
        }
    }

    fn diagnostics(&self, file: &Utf8PathBuf) -> Vec<Diagnostic> {
        self.get_file(file)
            .map(|data| {
                // Use the unified diagnostic pipeline so that LSP
                // diagnostics match CLI diagnostics. Cross-file classes from
                // the ProjectIndex are passed so type checking, @expect
                // directives, and all post-analysis passes run identically.
                let cross_file_classes = self.project_index.cross_file_class_infos_for(file);
                // Stdlib source files must be analysed with
                // `stdlib_mode = true` so the "conflicts with a stdlib class"
                // shadowing check doesn't flag every class the file
                // legitimately defines.
                let mut options = beamtalk_core::CompilerOptions::default();
                if self.project_index.is_stdlib_file(file) {
                    options.stdlib_mode = true;
                }
                // After a full-coverage workspace preload the
                // ProjectIndex holds every project file's classes, so the
                // injected knowledge is project-complete.
                if self.project_complete {
                    options.knowledge_scope =
                        beamtalk_core::semantic_analysis::KnowledgeScope::ProjectComplete;
                }
                // BT-4022: project-known Erlang modules (hex-dep/native
                // ebins + own `native/*.erl`, set by the LSP server at
                // startup) so the `unresolved_ffi` check agrees with
                // `beamtalk build`.
                options
                    .known_erlang_modules
                    .clone_from(&self.known_erlang_modules);
                // `current_package` so `AliasRegistry::add_pre_loaded`'s
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
                // ADR 0070 Phase 2 (BT-4928): the file's root's manifest
                // package set, so an undeclared `pkg@Class` qualifier shows
                // the squiggle `beamtalk build` would. `None` skips the check.
                options.known_packages = self.known_qualifier_packages_for(file);
                // Cross-file type aliases from the ProjectIndex, so a
                // `type Name = ...` declared in another project file resolves
                // instead of leaving `Dynamic (dynamic receiver)` behind —
                // mirrors `cross_file_classes` immediately above.
                let pre_loaded_aliases = self.project_index.cross_file_alias_infos_for(file);
                // Cross-file protocol declarations from the
                // ProjectIndex, so `extending:`/conformance checks against a
                // `Protocol define: Name ...` declared in another project
                // file or dependency resolve instead of degrading to
                // "unknown protocol" — parity with the CLI's `build`/`lint`
                // wiring, mirrors `cross_file_classes` above.
                let pre_loaded_protocols = self.project_index.cross_file_protocol_infos_for(file);
                // Cross-file/cross-package carried protocol ASTs
                // (BT-3591), so a cross-file `uses:` flattens during LSP
                // diagnostics instead of reporting "unknown protocol" —
                // parity with the CLI's `build`/`lint` wiring, mirrors
                // `pre_loaded_protocols` immediately above.
                let pre_loaded_protocol_defs =
                    self.project_index.cross_file_protocol_defs_for(file);
                // A `stubs/lists.bt` opened directly in an
                // editor must not be diagnosed as if it were an ordinary
                // src/ file — `declare native:` is only legal there. See
                // `ProjectIndex::is_stub_file`'s doc for why this can't be a
                // tracked-membership check like `is_stdlib_file`.
                let is_stub_file = self.project_index.is_stub_file(file);
                // File basename (without extension), so the shared
                // pipeline can validate it agrees with the class declared
                // here — see `ProjectDiagnosticContext::source_file_stem`'s doc.
                let source_file_stem = file.file_stem().map(std::string::ToString::to_string);
                let ctx = crate::queries::diagnostic_provider::ProjectDiagnosticContext {
                    options,
                    cross_file_classes,
                    pre_loaded_protocols,
                    pre_loaded_protocol_defs,
                    pre_loaded_aliases,
                    native_type_registry: self
                        .file_native_types
                        .get(file)
                        .cloned()
                        .or_else(|| self.native_types.clone()),
                    // Apply the same `beamtalk.toml` `[diagnostics]`
                    // severity-override table `beamtalk build` uses, so the
                    // LSP never disagrees with the CLI about a diagnostic's
                    // severity.
                    diagnostics_overrides: self.diagnostics_overrides.clone(),
                    is_stub_file,
                    source_file_stem,
                    ..Default::default()
                };
                let mut diags = crate::queries::diagnostic_provider::compute_project_diagnostics(
                    &data.module,
                    &data.source,
                    data.diagnostics.clone(),
                    &ctx,
                );
                // BT-5080: the same class in two project files. The sorted-first
                // path wins deterministically; say so on the losing/winning
                // definition so the conflict is visible instead of silent.
                for (name, other, winner) in self.project_index.duplicate_classes_of(file) {
                    if let Some(class) = data.module.classes.iter().find(|c| c.name.name == name) {
                        diags.push(
                            beamtalk_core::source_analysis::Diagnostic::warning(
                                format!("class `{name}` is also defined in `{other}`"),
                                class.name.span,
                            )
                            .with_hint(format!(
                                "`{winner}` takes precedence in the project index; rename or remove one definition."
                            )),
                        );
                    }
                }
                diags
            })
            .unwrap_or_default()
    }

    fn completions(&self, file: &Utf8PathBuf, position: Position) -> Vec<Completion> {
        let Some(file_data) = self.get_file(file) else {
            return Vec::new();
        };

        // Determine the current file's package for cross-package visibility filtering
        // (ADR 0071).
        let current_package = self.project_index.package_for_file(file);

        // Use project-wide hierarchy for completions (cross-file class awareness)
        // ADR 0075: Pass native type registry for typed Erlang FFI completions
        // ADR 0108 Phase 8: Pass the project-wide alias registry so
        // alias names are offered alongside class/protocol names in
        // type-annotation position.
        // ADR 0144: the file's protocol registry, so a `P class` receiver
        // completes `P`'s `class` requirements.
        let protocol_registry = self
            .project_index
            .protocol_registry_for(file, &file_data.module);
        crate::queries::completion_provider::compute_completions_with_registries(
            &file_data.module,
            &file_data.source,
            position,
            self.project_index.hierarchy(),
            current_package.as_deref(),
            self.native_types.as_ref(),
            Some(self.project_index.alias_registry_arc()),
            Some(protocol_registry),
        )
    }

    fn hover(&self, file: &Utf8PathBuf, position: Position) -> Option<HoverInfo> {
        let file_data = self.get_file(file)?;

        // ADR 0108 Phase 8: `ProjectIndex` tracks a
        // project-wide `AliasRegistry` (see `update_file`), so an
        // alias-typed value's hover resolves and renders
        // `AliasName (expansion)` instead of falling back to its bare
        // structural expansion. ADR 0144: likewise the file's protocol
        // registry, so a `P class` value hovers as the protocol metatype.
        let protocol_registry = self
            .project_index
            .protocol_registry_for(file, &file_data.module);
        crate::queries::hover_provider::compute_hover(
            &file_data.module,
            &file_data.source,
            position,
            self.project_index.hierarchy(),
            self.native_types.as_ref(),
            Some(self.project_index.alias_registry_arc()),
            Some(protocol_registry),
        )
    }

    fn signature_help(&self, file: &Utf8PathBuf, position: Position) -> Option<SignatureHelp> {
        let file_data = self.get_file(file)?;

        // ADR 0144: the file's protocol registry, as for completion and
        // hover, so a `P class` receiver shows its `class` requirement.
        let protocol_registry = self
            .project_index
            .protocol_registry_for(file, &file_data.module);
        crate::queries::signature_help_provider::compute_signature_help(
            &file_data.module,
            &file_data.source,
            position,
            self.project_index.hierarchy(),
            self.native_types.as_ref(),
            Some(protocol_registry),
            Some(self.project_index.alias_registry_arc()),
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
        //    header*: cursor on the selector in `bar => ...`,
        //    `+ other => ...`, or `at: i put: v => ...`. Navigate to the
        //    nearest overridden parent method. This mirrors the header path
        //    added to `find_references` and reuses its helper.
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
        // class-aware references. ADR 0108 Phase 8: aliases are
        // never registered into `ClassHierarchy` (they're a peer namespace,
        // collision-checked against it instead), so `has_class` alone can't
        // see them — the `alias_registry().has_alias` check closes that gap.
        // `find_class_references` itself walks `module.type_aliases` (both
        // the declaration site and RHS references), so it already handles
        // an alias name correctly once routed here.
        //
        // Also route here when the cursor is in a syntactic
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
            let mut refs = crate::queries::references_provider::find_class_references(
                &ident.name,
                self.files.iter().map(|(path, data)| (path, &data.module)),
            );
            // `pkg@Name` is `Name` in package `pkg` (BT-5079): keep it only
            // when `pkg` is the declaring package, as class rename does.
            let own_package = self.declaring_package(&ident.name);
            refs.retain(|loc| {
                self.qualifier_of(loc, &ident.name)
                    .is_none_or(|q| own_package.as_deref() == Some(q))
            });
            return refs;
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

        let source = &file_data.source;
        // ADR 0133 (BT-3914): "return the value instead" on the escaping-block
        // diagnostic, keyed off the diagnostic itself (no escape analysis here).
        let mut actions = crate::queries::return_value_fix_provider::compute_return_value_actions(
            &file_data.module,
            source,
            || self.diagnostics(file),
            start,
            end,
        );

        // Collect the unannotated methods overlapping the requested range
        // *before* running inference: editors request code actions on every
        // cursor move, and inference is the expensive part (BT-5080).
        // Cursor requests (start == end) use a point-in-span check; selections
        // use half-open interval overlap.
        let overlaps = |span: Span| {
            if start == end {
                span.start() <= start && start < span.end()
            } else {
                span.start() < end && start < span.end()
            }
        };
        let mut candidates: Vec<((ecow::EcoString, ecow::EcoString, bool), Span)> = Vec::new();
        for class in &file_data.module.classes {
            for (method, is_class_method) in class
                .methods
                .iter()
                .map(|m| (m, false))
                .chain(class.class_methods.iter().map(|m| (m, true)))
            {
                if method.return_type.is_none() && overlaps(method.span) {
                    candidates.push((
                        (
                            class.name.name.clone(),
                            method.selector.name(),
                            is_class_method,
                        ),
                        method.span,
                    ));
                }
            }
        }
        for standalone in &file_data.module.method_definitions {
            let method = &standalone.method;
            if method.return_type.is_none() && overlaps(method.span) {
                candidates.push((
                    (
                        standalone.class_name.name.clone(),
                        method.selector.name(),
                        standalone.is_class_method,
                    ),
                    method.span,
                ));
            }
        }
        if candidates.is_empty() {
            return actions;
        }

        let inferred = beamtalk_core::semantic_analysis::type_checker::infer_method_return_types(
            &file_data.module,
            self.project_index.hierarchy(),
            self.native_types.as_deref(),
        );

        for (key, span) in candidates {
            // The inferred map stores InferredType; use
            // `display_for_diagnostic()` so user-facing annotations render
            // source-friendly names (e.g., `Nil` instead of `UndefinedObject`).
            if let Some(inferred_ty) = inferred.get(&key) {
                let display = inferred_ty
                    .display_for_diagnostic()
                    .unwrap_or_else(|| ecow::EcoString::from("Dynamic"));
                if let Some(offset) = find_body_open_offset(source, span) {
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
/// Lexes the method's span and returns the first `=>` *token*. Comments are
/// trivia, so a `=>` inside one is never mistaken for the opener (BT-5079).
/// Returns `None` when the span is out of bounds or no `=>` is found.
fn find_body_open_offset(source: &str, method_span: Span) -> Option<u32> {
    let start = method_span.start();
    let text = source.get(start as usize..(method_span.end() as usize).min(source.len()))?;
    beamtalk_core::source_analysis::Lexer::new(text)
        .find(|t| {
            matches!(
                t.kind(),
                beamtalk_core::source_analysis::TokenKind::FatArrow
            )
        })
        .map(|t| start + t.span().start())
}

impl Default for SimpleLanguageService {
    fn default() -> Self {
        Self::new()
    }
}

/// The same-file fold (BT-4721) followed by the ADR 0152 cross-file check: the
/// file-compile rule the language service shares with the build.
fn fold_and_check_extensions(
    module: &mut beamtalk_core::ast::Module,
) -> Vec<beamtalk_core::source_analysis::Diagnostic> {
    let mut diagnostics = beamtalk_core::compilation::fold_same_file_extensions(module);
    diagnostics.extend(beamtalk_core::compilation::reject_cross_file_extensions(
        module,
    ));
    diagnostics
}
