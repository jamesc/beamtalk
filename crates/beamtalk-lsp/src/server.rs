// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! LSP server implementation.
//!
//! **DDD Context:** Language Service
//!
//! Delegates all IDE operations to `SimpleLanguageService` + `ProjectIndex`.
//! Maps between LSP protocol types and beamtalk language service types.

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::time::Duration;

use serde::{Deserialize, Serialize};

use crate::runtime::{
    ClassChangedEvent, FlushEvent, ReloadCheckEvent, ReloadFinding, RuntimeClient, RuntimeError,
};

use beamtalk_core::language_service::{
    CallHierarchyTarget, CompletionKind, DocumentSymbolKind, LanguageService,
    Location as BtLocation, NavQuery, NavSite, NavSymbolClass, Position as BtPosition,
    RuntimeLocation, SimpleLanguageService, nav_site_to_location,
};
use beamtalk_core::queries::all_sends_query::{ReceiverKind, find_all_sends_in_source};
use beamtalk_core::semantic_analysis::ClassHierarchy;
use beamtalk_core::source_analysis::{Severity, Span};
use beamtalk_core::unparse::{escape_string_literal, format_source};
use camino::Utf8PathBuf;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CallHierarchyIncomingCall, CallHierarchyIncomingCallsParams, CallHierarchyItem,
    CallHierarchyOptions, CallHierarchyOutgoingCall, CallHierarchyOutgoingCallsParams,
    CallHierarchyPrepareParams, CallHierarchyServerCapability, CodeAction, CodeActionKind,
    CodeActionOrCommand, CodeActionParams, CodeActionProviderCapability, CodeActionResponse,
    CompletionItem, CompletionItemKind, CompletionOptions, CompletionParams, CompletionResponse,
    DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFormattingParams,
    DocumentRangeFormattingParams, DocumentSymbolParams, DocumentSymbolResponse, Documentation,
    ExecuteCommandOptions, ExecuteCommandParams, GotoDefinitionParams, GotoDefinitionResponse,
    Hover, HoverContents, HoverParams, HoverProviderCapability, ImplementationProviderCapability,
    InitializeParams, InitializeResult, InitializedParams, MarkupContent, MarkupKind, MessageType,
    OneOf, ParameterInformation, ParameterLabel, Position, Range, ReferenceParams,
    ServerCapabilities, SignatureHelp, SignatureHelpOptions, SignatureHelpParams,
    SignatureInformation, SymbolInformation, SymbolKind, TextDocumentSyncCapability,
    TextDocumentSyncKind, TextDocumentSyncOptions, TextDocumentSyncSaveOptions, TextEdit,
    TypeHierarchyItem, TypeHierarchyPrepareParams, TypeHierarchySubtypesParams,
    TypeHierarchySupertypesParams, Url, WorkDoneProgressOptions, WorkspaceEdit,
    WorkspaceSymbolParams,
};
use tower_lsp::{Client, LanguageServer};
use tracing::debug;

const DIAGNOSTIC_DEBOUNCE_DURATION: Duration = Duration::from_millis(150);
const PRELOAD_MAX_FILES: usize = 5000;

/// ADR 0082 Phase 3 (BT-2289): LSP `workspace/executeCommand` identifiers
/// surfaced to clients. Each command compiles to a Beamtalk expression
/// submitted via the workspace's existing `evaluate` REPL op — no new
/// workspace-side dispatch is added (per ADR 0082 "Rationale: why no new REPL
/// ops"). Keep the names stable; editors bind to them by string match.
pub(crate) const CMD_FLUSH: &str = "beamtalk.flush";
pub(crate) const CMD_FLUSH_CLASS: &str = "beamtalk.flush.class";
pub(crate) const CMD_FLUSH_FILE: &str = "beamtalk.flush.file";
pub(crate) const CMD_FLUSH_KIND: &str = "beamtalk.flush.kind";
pub(crate) const CMD_SAVE_CLASS: &str = "beamtalk.saveClass";
/// ADR 0105 Phase 3 (BT-2782): the editor's "check before save" pre-save
/// advisory hook — compiles a pending method edit and reports would-be-stale
/// dependents without installing it. Non-blocking; the caller decides
/// whether/when to follow up with the real save (`compile:source:` via
/// whichever surface the editor uses for that).
pub(crate) const CMD_PRECHECK_METHOD: &str = "beamtalk.precheckMethod";
/// ADR 0105 Phase 3 (BT-2782): the explicit whole-image re-check
/// (`Workspace recheckImage` / REPL `:recheck image`).
pub(crate) const CMD_RECHECK_IMAGE: &str = "beamtalk.recheckImage";

/// All commands surfaced via `executeCommand`. Wired into
/// `ServerCapabilities::execute_command_provider` and used by the LSP→runtime
/// dispatch in [`Backend::execute_command`].
pub(crate) const BEAMTALK_LSP_COMMANDS: &[&str] = &[
    CMD_FLUSH,
    CMD_FLUSH_CLASS,
    CMD_FLUSH_FILE,
    CMD_FLUSH_KIND,
    CMD_SAVE_CLASS,
    CMD_PRECHECK_METHOD,
    CMD_RECHECK_IMAGE,
];

#[derive(Clone)]
struct PreloadConfig {
    roots: Vec<PathBuf>,
    stdlib_dirs: Vec<PathBuf>,
}

/// Live reload-induced diagnostics (ADR 0105 Phase 1, BT-2779), double-keyed
/// by document URI and then by `(owner class name, changed class name)`.
/// Both levels of keying are load-bearing, not cosmetic:
///
/// * **Owner**: Beamtalk supports multiple classes defined in one `.bt` file
///   (e.g. `Behaviour.bt`, `WorkspaceInterface.bt`), so two different caller
///   classes' reload-induced diagnostics can legitimately share a URI. A
///   flat `HashMap<Url, Vec<Diagnostic>>` would have one owner's `put`/clear
///   silently clobber a sibling owner's diagnostics in the same file.
/// * **Changed class**: a single caller can be broken by two *independently
///   reloading* classes (`Dashboard` calls both `Counter>>getCount` and
///   `Widget>>size`; each can go stale and get fixed on its own schedule).
///   Keying only by owner would have a later reload's `insert`/`remove`
///   clobber an earlier, still-valid finding from a *different* changed
///   class — mirrors `beamtalk_workspace_findings_store`'s `origin_key()`
///   (`{Owner, ChangedClass}`) server-side.
type ReloadDiagnosticsByUriAndOrigin =
    HashMap<Url, HashMap<(String, String), Vec<tower_lsp::lsp_types::Diagnostic>>>;

#[derive(Default)]
struct PreloadedFiles {
    user_files: Vec<(PathBuf, String)>,
    stdlib_files: Vec<(PathBuf, String)>,
    /// Whether the preload file budget (`PRELOAD_MAX_FILES`) was exhausted
    /// mid-walk (BT-2796). When true, workspace coverage may be partial and
    /// the language service must NOT claim `KnowledgeScope::ProjectComplete`.
    budget_exhausted: bool,
    /// Whether any workspace root has fetched package dependencies
    /// (`_build/deps/*/src` present, BT-2794). Pre-WS3, dependency extension
    /// contributions are invisible, so diagnostics must stay conservative.
    deps_present: bool,
}

/// Params for the `beamtalk-lsp/fetchContent` custom request.
#[derive(Deserialize)]
pub struct FetchContentParams {
    /// A `beamtalk-stdlib:///ClassName.bt` URI identifying the file to fetch.
    pub uri: String,
}

/// Response for the `beamtalk-lsp/fetchContent` custom request.
#[derive(Debug, Serialize)]
pub struct FetchContentResult {
    /// The source content of the requested file.
    pub content: String,
}

/// LSP backend wrapping `SimpleLanguageService`.
pub struct Backend {
    /// LSP client handle for sending notifications and responses.
    client: Client,
    /// The underlying language service, protected by a mutex for concurrent
    /// access. `Arc`-wrapped (ADR 0105 Phase 1, BT-2779) so the detached
    /// reload-check listener task (spawned once, outlives any single
    /// request handler's `&self` borrow) can recompute *static* diagnostics
    /// and merge them with reload-induced ones when it republishes — the
    /// same reason [`Self::versions`] and [`Self::nav_cache`] are
    /// `Arc`-wrapped. `self.service.lock()` is unaffected: `Arc<Mutex<T>>`
    /// derefs to `Mutex<T>`.
    service: Arc<Mutex<SimpleLanguageService>>,
    /// Last known LSP document version by file path.
    versions: Arc<Mutex<HashMap<Utf8PathBuf, i32>>>,
    /// Paths of documents that have received `didChange` notifications since
    /// their last `didSave` / `didOpen`. The editor's in-memory copy is the
    /// source of truth for these — the on-disk bytes are stale and so is the
    /// runtime's class registry (which only sees compiled / flushed source).
    /// Used by [`Backend::document_symbol`] to bypass the runtime path when
    /// answering the outline for an unsaved buffer (BT-2244 review fix).
    dirty_files: Mutex<HashSet<Utf8PathBuf>>,
    /// Monotonic generation counter used to debounce `didChange` diagnostics per URI.
    diagnostic_generation: Mutex<HashMap<Url, u64>>,
    /// Deferred preload config captured at initialize and consumed after handshake.
    preload_config: Mutex<Option<PreloadConfig>>,
    /// Paths of stdlib files loaded during preload; used to emit `beamtalk-stdlib://` URIs.
    stdlib_paths: Mutex<HashSet<Utf8PathBuf>>,
    /// Workspace roots discovered at initialization; used for native delegate `.erl` lookup.
    workspace_roots: Mutex<Vec<PathBuf>>,
    /// Cached OTP lib directory (e.g., `/usr/lib/erlang/lib`); resolved once at startup.
    otp_lib_dir: Mutex<Option<PathBuf>>,
    /// ADR 0082 Phase 3 (BT-2289): lazy-attached WebSocket client to the
    /// running workspace, used for `workspace/executeCommand` dispatch and
    /// `flush_completed` push subscription. Lives behind a `tokio` mutex so
    /// async `executeCommand` handlers can attach on demand without blocking
    /// the LSP service thread.
    runtime: tokio::sync::Mutex<Option<RuntimeClient>>,
    /// ADR 0082 Phase 3 (BT-2289): handle to the flush-event listener task
    /// that consumes `FlushEvent`s from the runtime client and emits
    /// `workspace/applyEdit` per touched file.
    flush_listener: tokio::sync::Mutex<Option<tokio::task::JoinHandle<()>>>,
    /// BT-2239: handle to the class-changed-event listener task that
    /// consumes `ClassChangedEvent`s and invalidates [`Self::nav_cache`].
    class_changed_listener: tokio::sync::Mutex<Option<tokio::task::JoinHandle<()>>>,
    /// BT-2239: feature flag — when true and a runtime is attached, the
    /// LSP delegates navigation queries (find-references / implementation
    /// / call-hierarchy / type-hierarchy) to the workspace via
    /// `nav-query`. When false (the default), all nav queries use the
    /// in-process AST walker. Read once from the `initialize` params'
    /// `initializationOptions.delegateToRuntime` field and never mutated
    /// after that — atomic for cheap concurrent reads from nav handlers.
    delegate_to_runtime: std::sync::atomic::AtomicBool,
    /// BT-2239: per-query cache of runtime-attached nav results, keyed by
    /// query string. Invalidated wholesale on every `ClassChangedEvent`
    /// (coarse but correct for the foundation issue — per-method children
    /// can refine to per-class buckets if needed).
    ///
    /// Shared `Arc` so the class-changed listener task can hold a clone
    /// without keeping the `Backend` itself alive (the listener is aborted
    /// on `Drop` via `class_changed_listener`'s `JoinHandle`).
    nav_cache: Arc<std::sync::Mutex<NavCache>>,
    /// ADR 0105 Phase 1 (BT-2779): handle to the reload-check-event listener
    /// task that consumes `ReloadCheckEvent`s and publishes/clears
    /// reload-induced diagnostics.
    reload_check_listener: tokio::sync::Mutex<Option<tokio::task::JoinHandle<()>>>,
    /// ADR 0105 Phase 1 (BT-2779): currently-live reload-induced diagnostics,
    /// keyed by document URI and then by owner class name (see
    /// [`ReloadDiagnosticsByUriAndOrigin`] for why the owner keying matters).
    /// Populated/replaced wholesale per owner by [`reload_check_listener`]
    /// (clearing-by-replacement — an owner with no current findings has its
    /// entry removed, not left stale), and merged into every
    /// [`Backend::publish_diagnostics`] call so a subsequent
    /// `didChange`-triggered republish doesn't silently drop them.
    reload_diagnostics: Arc<std::sync::Mutex<ReloadDiagnosticsByUriAndOrigin>>,
}

/// Coarse cache for runtime-attached navigation results (BT-2239).
///
/// The foundation issue uses a single generation counter — every
/// `ClassChangedEvent` increments it, and cache entries are tagged with
/// the generation they were computed under. A reader treats any entry
/// whose generation predates the current one as stale.
///
/// Per-method children (BT-2240..2244) can keep the same shape and add
/// payloads keyed by `(NavQuery, generation)` if they want memoisation;
/// the foundation issue ships the invariant (any class change ⇒ caches
/// are stale) without committing to a memoisation policy.
#[derive(Debug, Default)]
pub(crate) struct NavCache {
    /// Monotonic generation. Starts at 0; bumped on every class change.
    generation: u64,
}

impl NavCache {
    /// Read the current generation. Cache consumers stash this with each
    /// cached entry and compare on read.
    ///
    /// Foundation issue exposes the API but does not consume it directly;
    /// per-method children (BT-2240..2244) call this when they cache
    /// runtime results. Allowed-dead so the foundation PR doesn't have
    /// to ship a consumer.
    #[allow(dead_code, reason = "per-method children consume this API")]
    pub(crate) fn generation(&self) -> u64 {
        self.generation
    }

    /// Bump the generation — invalidates every cached entry.
    pub(crate) fn invalidate(&mut self) {
        self.generation = self.generation.saturating_add(1);
    }
}

impl Backend {
    /// Creates a new `Backend` with the given LSP client handle.
    pub fn new(client: Client) -> Self {
        Self {
            client,
            service: Arc::new(Mutex::new(SimpleLanguageService::new())),
            versions: Arc::new(Mutex::new(HashMap::new())),
            dirty_files: Mutex::new(HashSet::new()),
            diagnostic_generation: Mutex::new(HashMap::new()),
            preload_config: Mutex::new(None),
            stdlib_paths: Mutex::new(HashSet::new()),
            workspace_roots: Mutex::new(Vec::new()),
            otp_lib_dir: Mutex::new(None),
            runtime: tokio::sync::Mutex::new(None),
            flush_listener: tokio::sync::Mutex::new(None),
            class_changed_listener: tokio::sync::Mutex::new(None),
            delegate_to_runtime: std::sync::atomic::AtomicBool::new(false),
            nav_cache: Arc::new(std::sync::Mutex::new(NavCache::default())),
            reload_check_listener: tokio::sync::Mutex::new(None),
            reload_diagnostics: Arc::new(std::sync::Mutex::new(HashMap::new())),
        }
    }

    /// Read the current nav-cache generation (BT-2239). Foundation
    /// consumers store this with each cached entry and compare on read.
    #[allow(dead_code, reason = "per-method children consume this API")]
    pub(crate) fn nav_cache_generation(&self) -> u64 {
        self.nav_cache
            .lock()
            .expect("nav_cache lock poisoned")
            .generation()
    }

    /// BT-2239: two-mode dispatch seam for navigation queries.
    ///
    /// When the `delegateToRuntime` flag is on **and** a runtime is
    /// reachable (the workspace is running), forwards `query` to the
    /// attached runtime via [`RuntimeClient::nav_query`] and converts the
    /// resulting [`NavSite`]s to LSP `Location`s through the per-call
    /// `to_lsp` mapper. Otherwise — flag off, no running workspace,
    /// runtime error, or empty runtime result — falls back to
    /// `ast_fallback`.
    ///
    /// Per-method children (BT-2240..2244) implement one nav query each
    /// by calling this helper with:
    ///
    /// * `query` — a [`NavQuery`] built from the cursor symbol
    /// * `to_lsp` — turns a `NavSite` into the LSP type the caller needs
    ///   (`Location` for references / implementation, `CallHierarchyItem`
    ///   for call hierarchy, etc.)
    /// * `ast_fallback` — a sync closure that runs the in-process AST
    ///   walker (current behaviour)
    ///
    /// The helper keeps two contracts the issue's acceptance criteria
    /// require:
    /// 1. **No behaviour change when the flag is off.** The runtime path
    ///    is never taken, no eval is submitted, no cache is consulted.
    ///    Per-method children rely on this for byte-for-byte parity with
    ///    today.
    /// 2. **Strict cold-file fallback.** A runtime path that returns an
    ///    error (workspace disconnected, malformed reply) falls through
    ///    to `ast_fallback` rather than surfacing the error to the
    ///    editor. The runtime-attached mode is a *better* answer, not a
    ///    *different* one.
    pub(crate) async fn delegate_nav_query<T, F, A>(
        &self,
        query: NavQuery,
        to_lsp: F,
        ast_fallback: A,
    ) -> Vec<T>
    where
        F: Fn(&NavSite, &[PathBuf]) -> Option<T> + Send + Sync,
        A: FnOnce() -> Vec<T>,
    {
        if !self.delegate_to_runtime() {
            return ast_fallback();
        }
        let runtime = match self.ensure_runtime_attached().await {
            Ok(client) => client,
            Err(e) => {
                tracing::debug!(
                    %e,
                    kind = query.kind(),
                    "delegate_nav_query: runtime unreachable, falling back to AST"
                );
                return ast_fallback();
            }
        };
        let sites = match runtime.nav_query(&query).await {
            Ok(sites) => sites,
            Err(e) => {
                tracing::warn!(
                    %e,
                    kind = query.kind(),
                    "delegate_nav_query: runtime error, falling back to AST"
                );
                return ast_fallback();
            }
        };
        if sites.is_empty() {
            // The runtime knows the query but has no matches. Trust the
            // runtime — it sees live patches and stdlib classes the AST
            // walker can't index — and return an empty result. (Falling
            // back to AST here would mask legitimate "no matches" with
            // stale results.)
            return Vec::new();
        }
        let roots = {
            let guard = self
                .workspace_roots
                .lock()
                .expect("workspace_roots lock poisoned");
            guard.clone()
        };
        sites
            .iter()
            .filter_map(|site| to_lsp(site, &roots))
            .collect()
    }

    /// BT-2244: two-mode dispatch seam for the **bulk symbol outline**
    /// (`textDocument/documentSymbol`, `workspace/symbol`).
    ///
    /// Sibling of [`Self::delegate_nav_query`]. Both follow the same
    /// flag/runtime contract — runtime path wins when the flag is on **and**
    /// a runtime is reachable; otherwise falls back to `ast_fallback`.
    /// The difference is the payload shape: `nav-symbols` returns a list of
    /// classes-with-methods instead of a flat list of sites, so the helper
    /// hands the typed `Vec<NavSymbolClass>` to the caller's `to_lsp`
    /// mapper rather than a per-row converter.
    ///
    /// * `scope` — `Some("user")` for source-backed only (LSP
    ///   `documentSymbol`), `Some("all")` / `None` for every loaded class
    ///   (LSP `workspace/symbol`)
    /// * `to_lsp` — converts the typed payload to the LSP result shape; sees
    ///   the workspace roots so it can resolve `source_file` paths the same
    ///   way [`runtime_site_to_lsp_location`] does for nav queries
    /// * `ast_fallback` — sync closure that runs today's AST/glob path; the
    ///   sole code path when the flag is off, the runtime is unreachable,
    ///   or the runtime returns an error
    ///
    /// Contracts match [`Self::delegate_nav_query`]:
    /// 1. **No behaviour change when the flag is off.**
    /// 2. **Strict cold-file fallback on runtime error** — a transport or
    ///    decoding failure falls through to AST, never surfaces to the
    ///    editor.
    /// 3. **Trust an empty runtime answer.** When the runtime returns zero
    ///    classes (a *valid* "no symbols" answer — e.g. a project with no
    ///    user classes loaded yet) the helper does **not** fall back; an
    ///    empty list is what the editor wants.
    pub(crate) async fn delegate_nav_symbols<T, F, A>(
        &self,
        scope: Option<&'static str>,
        to_lsp: F,
        ast_fallback: A,
    ) -> Vec<T>
    where
        F: FnOnce(Vec<NavSymbolClass>, &[PathBuf]) -> Vec<T>,
        A: FnOnce() -> Vec<T>,
    {
        if !self.delegate_to_runtime() {
            return ast_fallback();
        }
        let runtime = match self.ensure_runtime_attached().await {
            Ok(client) => client,
            Err(e) => {
                tracing::debug!(
                    %e,
                    "delegate_nav_symbols: runtime unreachable, falling back to AST"
                );
                return ast_fallback();
            }
        };
        let classes = match runtime.nav_symbols(scope).await {
            Ok(classes) => classes,
            Err(e) => {
                tracing::warn!(
                    %e,
                    "delegate_nav_symbols: runtime error, falling back to AST"
                );
                return ast_fallback();
            }
        };
        let roots = {
            let guard = self
                .workspace_roots
                .lock()
                .expect("workspace_roots lock poisoned");
            guard.clone()
        };
        to_lsp(classes, &roots)
    }

    /// BT-2240: Resolve the **declaration sites** (LSP `Location`s) for a
    /// [`NavQuery`].
    ///
    /// * [`NavQuery::SendersOf`] (selector) → runtime
    ///   `SystemNavigation implementorsOf:` via [`Self::delegate_nav_query`]
    ///   when the flag is on; falls back to the in-process
    ///   [`SimpleLanguageService::find_selector_declarations`] walker
    ///   otherwise. The runtime path picks up live-edited methods (ADR
    ///   0082, `ChangeLog` patches) and extension methods (ADR 0066) that the
    ///   AST walker can't see.
    /// * [`NavQuery::ReferencesTo`] (class) → always the AST
    ///   [`SimpleLanguageService::find_class_declarations`] walker.
    ///   `beamtalk_xref` does not currently expose a "declaration site for
    ///   class `Foo`" query — class declarations come from cold-file
    ///   indexing, which is always available for indexed files.
    /// * [`NavQuery::ImplementorsOf`] — not used by `textDocument/references`;
    ///   callers should not pass it. Returns an empty vector if they do.
    ///
    /// Used by the `textDocument/references` handler to overlay declaration
    /// sites onto runtime-attached results when `includeDeclaration = true`.
    async fn declaration_sites_for_query(
        &self,
        query: &NavQuery,
    ) -> Vec<tower_lsp::lsp_types::Location> {
        match query {
            NavQuery::SendersOf(selector) => {
                let selector_name = selector.clone();
                let ast_fallback = || -> Vec<tower_lsp::lsp_types::Location> {
                    let svc = self.service.lock().expect("service lock poisoned");
                    let locs = svc.find_selector_declarations(selector_name.as_str());
                    bt_locations_to_lsp(&svc, locs)
                };
                let implementors_query = NavQuery::ImplementorsOf(selector.clone());
                self.delegate_nav_query(
                    implementors_query,
                    runtime_site_to_lsp_location,
                    ast_fallback,
                )
                .await
            }
            NavQuery::ReferencesTo(class_name) => {
                let svc = self.service.lock().expect("service lock poisoned");
                let locs = svc.find_class_declarations(class_name.as_str());
                bt_locations_to_lsp(&svc, locs)
            }
            NavQuery::ImplementorsOf(_) => Vec::new(),
        }
    }

    /// Read the `delegateToRuntime` flag (BT-2239).
    pub(crate) fn delegate_to_runtime(&self) -> bool {
        self.delegate_to_runtime
            .load(std::sync::atomic::Ordering::Relaxed)
    }

    /// Set the `delegateToRuntime` flag from `initialize` params.
    pub(crate) fn set_delegate_to_runtime(&self, value: bool) {
        self.delegate_to_runtime
            .store(value, std::sync::atomic::Ordering::Relaxed);
    }

    /// Returns true if `path` has unsaved edits in the editor — i.e. a
    /// `didChange` arrived since the last `didOpen` / `didSave`. The
    /// runtime's view of this file (on-disk bytes / last compiled module)
    /// is stale for as long as this flag is set, so any query that needs
    /// per-file source ordering (notably `document_symbol`) must use the
    /// LSP-side AST for dirty files. (BT-2244 review fix.)
    pub(crate) fn is_dirty(&self, path: &Utf8PathBuf) -> bool {
        let guard = self.dirty_files.lock().expect("dirty_files lock poisoned");
        guard.contains(path)
    }

    /// Mark `path` as having unsaved edits. Called from `did_change`.
    fn mark_dirty(&self, path: Utf8PathBuf) {
        let mut guard = self.dirty_files.lock().expect("dirty_files lock poisoned");
        guard.insert(path);
    }

    /// Clear the dirty bit for `path`. Called from `did_save` and
    /// `did_close` — `did_save` because the on-disk bytes now match the
    /// editor buffer, `did_close` because the editor no longer owns a
    /// modified copy.
    fn clear_dirty(&self, path: &Utf8PathBuf) {
        let mut guard = self.dirty_files.lock().expect("dirty_files lock poisoned");
        guard.remove(path);
    }

    fn file_version_for_uri(&self, uri: &Url) -> Option<i32> {
        let path = self.resolve_path_for_uri(uri)?;
        let versions = self.versions.lock().expect("versions lock poisoned");
        versions.get(&path).copied()
    }

    /// Resolves a URI to an internal path key used by the language service.
    ///
    /// - For `file://` URIs: returns the real filesystem path.
    /// - For `untitled:` URIs: returns a synthetic `__untitled__/` path key.
    /// - For `beamtalk-stdlib:///ClassName.bt` URIs: looks up the real path
    ///   from `stdlib_paths`. Returns `None` for unknown class names, invalid
    ///   URI form, or ambiguous filenames.
    fn resolve_path_for_uri(&self, uri: &Url) -> Option<Utf8PathBuf> {
        if uri.scheme() == "beamtalk-stdlib" {
            self.stdlib_uri_to_path(uri)
        } else {
            uri_to_path(uri)
        }
    }

    /// Looks up the real filesystem path for a `beamtalk-stdlib:///ClassName.bt` URI.
    ///
    /// Returns `None` for invalid URI form, unknown class names, or ambiguous filenames.
    fn stdlib_uri_to_path(&self, uri: &Url) -> Option<Utf8PathBuf> {
        // Only canonical form: no host, no query, no fragment.
        if uri.host().is_some() || uri.query().is_some() || uri.fragment().is_some() {
            return None;
        }
        let path = uri.path().trim_start_matches('/');
        // Require non-empty, no sub-paths, .bt extension only.
        if path.is_empty()
            || path.contains('/')
            || !std::path::Path::new(path)
                .extension()
                .is_some_and(|ext| ext.eq_ignore_ascii_case("bt"))
        {
            return None;
        }

        let stdlib_paths = self
            .stdlib_paths
            .lock()
            .expect("stdlib_paths lock poisoned");
        let mut matches = stdlib_paths.iter().filter(|p| p.file_name() == Some(path));
        let first = matches.next()?.clone();
        // Ambiguous: multiple files with the same name → None.
        if matches.next().is_some() {
            return None;
        }
        Some(first)
    }

    /// Searches workspace roots, OTP lib dirs, and dependency paths for an Erlang
    /// source file matching a module name.
    ///
    /// Search order:
    /// 1. `runtime/apps/*/src/<module>.erl` (project runtime)
    /// 2. `src/<module>.erl` (flat project layout)
    /// 3. `runtime/_build/default/lib/*/src/<module>.erl` (rebar3 dependencies)
    /// 4. `<otp_lib_dir>/*/src/<module>.erl` (OTP installation)
    fn find_erlang_source_file(&self, module_name: &str) -> Option<PathBuf> {
        // Reject module names containing path separators to prevent directory traversal.
        if module_name.contains('/') || module_name.contains('\\') || module_name.contains("..") {
            return None;
        }
        let filename = format!("{module_name}.erl");
        let roots = self
            .workspace_roots
            .lock()
            .expect("workspace_roots lock poisoned")
            .clone();
        for root in &roots {
            // Check runtime/apps/*/src/<module>.erl (OTP app layout)
            let runtime_apps = root.join("runtime").join("apps");
            if let Ok(entries) = std::fs::read_dir(&runtime_apps) {
                for entry in entries.flatten() {
                    let candidate = entry.path().join("src").join(&filename);
                    if candidate.is_file() {
                        return Some(candidate);
                    }
                }
            }
            // Check src/<module>.erl (flat layout)
            let flat = root.join("src").join(&filename);
            if flat.is_file() {
                return Some(flat);
            }
            // Check _build/default/lib/*/src/<module>.erl (rebar3 hex dependencies)
            let rebar3_lib = root
                .join("runtime")
                .join("_build")
                .join("default")
                .join("lib");
            if let Ok(entries) = std::fs::read_dir(&rebar3_lib) {
                for entry in entries.flatten() {
                    let candidate = entry.path().join("src").join(&filename);
                    if candidate.is_file() {
                        return Some(candidate);
                    }
                }
            }
        }

        // Check OTP lib dir (e.g., /usr/lib/erlang/lib/stdlib-6.2/src/lists.erl)
        let otp_dir = self
            .otp_lib_dir
            .lock()
            .expect("otp_lib_dir lock poisoned")
            .clone();
        if let Some(lib_dir) = otp_dir {
            if let Ok(entries) = std::fs::read_dir(&lib_dir) {
                for entry in entries.flatten() {
                    let candidate = entry.path().join("src").join(&filename);
                    if candidate.is_file() {
                        return Some(candidate);
                    }
                }
            }
        }

        None
    }

    /// Resolves and caches the OTP lib directory.
    ///
    /// Runs `erl -noshell -eval 'io:format("~s", [code:lib_dir()]), halt().'`
    /// once and stores the result. Subsequent calls return the cached value.
    async fn resolve_otp_lib_dir(&self) {
        let result = match tokio::time::timeout(
            std::time::Duration::from_secs(5),
            tokio::task::spawn_blocking(|| {
                std::process::Command::new("erl")
                    .args([
                        "-noshell",
                        "-eval",
                        "io:format(\"~s\", [code:lib_dir()]), halt().",
                    ])
                    .output()
                    .ok()
                    .and_then(|output| {
                        if output.status.success() {
                            String::from_utf8(output.stdout).ok().map(PathBuf::from)
                        } else {
                            None
                        }
                    })
            }),
        )
        .await
        {
            Ok(Ok(path)) => path,
            _ => None,
        };

        if let Some(ref dir) = result {
            debug!("Resolved OTP lib dir: {}", dir.display());
        }
        *self.otp_lib_dir.lock().expect("otp_lib_dir lock poisoned") = result;
    }

    /// BT-2242: dynamically register the type-hierarchy capability so
    /// clients that ignore the `experimental` channel still pick it up.
    ///
    /// `lsp-types` 0.94.1 doesn't expose a typed
    /// `ServerCapabilities::type_hierarchy_provider` field (added upstream
    /// in 0.95+), so static advertisement falls back to the typed
    /// `experimental` blob set in [`Self::initialize`]. Some clients —
    /// notably older `VSCode` + a strict tower-lsp install — read only the
    /// typed fields and ignore `experimental`. Dynamic registration via
    /// `client/registerCapability` is the LSP-3.17 way of telling those
    /// clients about the three methods regardless of the typed-field
    /// availability.
    ///
    /// Errors here are logged at debug level and otherwise swallowed:
    /// pre-3.17 clients reject dynamic registration entirely, and we don't
    /// want to gate startup on a non-essential feature.
    async fn register_type_hierarchy_capability(&self) {
        use tower_lsp::lsp_types::{Registration, TextDocumentRegistrationOptions};
        let opts = TextDocumentRegistrationOptions {
            document_selector: None,
        };
        let registrations = vec![Registration {
            id: "beamtalk-type-hierarchy".to_string(),
            method: "textDocument/prepareTypeHierarchy".to_string(),
            register_options: serde_json::to_value(&opts).ok(),
        }];
        if let Err(e) = self.client.register_capability(registrations).await {
            debug!(
                "type-hierarchy dynamic capability registration rejected: {e}; clients that read `experimental.typeHierarchyProvider` still pick it up"
            );
        }
    }

    /// Loads the type cache from `_build/type_cache/` in workspace roots.
    ///
    /// ADR 0075 Phase 1: Reads cached spec protocol lines and populates the
    /// `NativeTypeRegistry` in the language service. This provides typed
    /// completions for Erlang FFI calls (e.g., `reverse: list :: List -> List`).
    async fn load_type_cache(&self, roots: &[PathBuf]) {
        use beamtalk_core::semantic_analysis::type_checker::{
            NativeTypeRegistry, parse_specs_line,
        };

        let roots_owned: Vec<PathBuf> = roots.to_vec();
        let registry = tokio::task::spawn_blocking(move || {
            let mut registry = NativeTypeRegistry::new();
            for root in &roots_owned {
                let cache_dir = root.join("_build").join("type_cache");
                if !cache_dir.is_dir() {
                    continue;
                }
                let Ok(entries) = std::fs::read_dir(&cache_dir) else {
                    continue;
                };
                for entry in entries.flatten() {
                    let path = entry.path();
                    if path.extension().and_then(|e| e.to_str()) != Some("json") {
                        continue;
                    }
                    let Ok(content) = std::fs::read_to_string(&path) else {
                        continue;
                    };
                    // Extract specs_line from the JSON cache entry.
                    // Format: {"beam_mtime_secs":N,"specs_line":"..."}
                    if let Some(line) = extract_specs_line_from_cache(&content) {
                        if !line.is_empty() {
                            parse_specs_line(&line, &mut registry);
                        }
                    }
                }
            }
            registry
        })
        .await
        .unwrap_or_default();

        if registry.module_count() > 0 {
            debug!(
                "Loaded {} modules ({} functions) from type cache",
                registry.module_count(),
                registry.function_count()
            );
            let mut svc = self.service.lock().expect("service lock poisoned");
            svc.set_native_types(registry);
        }
    }

    async fn preload_workspace_source_files(&self, config: PreloadConfig) {
        let loaded = tokio::task::spawn_blocking(move || collect_preload_files(config))
            .await
            .unwrap_or_default();

        // Register stdlib paths before indexing so they are available immediately.
        let stdlib_utf8: Vec<Utf8PathBuf> = loaded
            .stdlib_files
            .iter()
            .filter_map(|(p, _)| Utf8PathBuf::from_path_buf(p.clone()).ok())
            .collect();
        {
            let mut stdlib_paths = self
                .stdlib_paths
                .lock()
                .expect("stdlib_paths lock poisoned");
            for path in &stdlib_utf8 {
                stdlib_paths.insert(path.clone());
            }
        }

        let mut svc = self.service.lock().expect("service lock poisoned");
        let budget_exhausted = loaded.budget_exhausted;
        let deps_present = loaded.deps_present;
        for (path, content) in loaded.user_files.into_iter().chain(loaded.stdlib_files) {
            let Ok(utf8_path) = Utf8PathBuf::from_path_buf(path) else {
                continue;
            };
            svc.update_file(utf8_path, content);
        }
        // BT-2796: With every workspace source file indexed, the ProjectIndex
        // is project-complete and diagnostics may say so (ADR 0100 Rule 2
        // sequencing guard). A budget-exhausted preload has partial coverage
        // and must keep the conservative ModuleOnly default.
        //
        // "Complete" here means the conventional source layout (`src/`,
        // `test/`, fetched dep sources, stdlib) was fully walked. Files
        // outside those directories are not preloaded and are only indexed
        // when opened — same coverage the ProjectIndex has always had for
        // classes. Scope claims must stay tied to this walk; do not claim
        // completeness from any weaker signal.
        svc.set_project_complete(!budget_exhausted);
        // BT-2794 (pre-WS3): fetched deps mean dependency extensions may
        // exist that the checker cannot see. (Declared-but-unfetched deps are
        // invisible here — the LSP deliberately avoids parsing beamtalk.toml —
        // but such a workspace cannot resolve dep classes at all, so hint
        // noise is the lesser concern.)
        svc.set_has_package_dependencies(deps_present);
    }

    /// Handles the `beamtalk-lsp/fetchContent` custom request.
    ///
    /// Returns the source content for a `beamtalk-stdlib:///ClassName.bt` virtual URI.
    /// Responds with an error if the URI scheme is unsupported or the file is not available.
    #[expect(
        clippy::unused_async,
        reason = "tower-lsp custom_method requires async fn signature"
    )]
    pub async fn fetch_content(
        &self,
        params: FetchContentParams,
    ) -> tower_lsp::jsonrpc::Result<FetchContentResult> {
        let uri = Url::parse(&params.uri).map_err(|_| {
            tower_lsp::jsonrpc::Error::invalid_params(format!("invalid URI: {}", params.uri))
        })?;

        if uri.scheme() != "beamtalk-stdlib" {
            return Err(tower_lsp::jsonrpc::Error::invalid_params(format!(
                "unsupported URI scheme: {}",
                uri.scheme()
            )));
        }

        // Only the canonical form `beamtalk-stdlib:///ClassName.bt` (empty authority, no query/fragment) is accepted.
        if uri.host().is_some() || uri.query().is_some() || uri.fragment().is_some() {
            return Err(tower_lsp::jsonrpc::Error::invalid_params(format!(
                "invalid stdlib URI `{}` (expected beamtalk-stdlib:///ClassName.bt)",
                params.uri
            )));
        }

        let path = uri.path().trim_start_matches('/');
        if path.is_empty()
            || path.contains('/')
            || !std::path::Path::new(path)
                .extension()
                .is_some_and(|ext| ext.eq_ignore_ascii_case("bt"))
        {
            return Err(tower_lsp::jsonrpc::Error::invalid_params(format!(
                "invalid stdlib URI path `{}` (expected beamtalk-stdlib:///ClassName.bt)",
                params.uri
            )));
        }
        let filename = path.to_string();

        let matching_paths: Vec<Utf8PathBuf> = {
            let stdlib_paths = self
                .stdlib_paths
                .lock()
                .expect("stdlib_paths lock poisoned");
            stdlib_paths
                .iter()
                .filter(|p| p.file_name() == Some(filename.as_str()))
                .cloned()
                .collect()
        };

        let path = match matching_paths.as_slice() {
            [single] => single.clone(),
            [] => {
                return Err(tower_lsp::jsonrpc::Error {
                    code: tower_lsp::jsonrpc::ErrorCode::ServerError(-32_001),
                    message: format!("stdlib source not available: {filename}").into(),
                    data: None,
                });
            }
            _ => {
                return Err(tower_lsp::jsonrpc::Error::invalid_params(format!(
                    "ambiguous stdlib URI `{}`: multiple files named `{filename}`",
                    params.uri
                )));
            }
        };

        let content = {
            let svc = self.service.lock().expect("service lock poisoned");
            svc.file_source(&path)
        };

        content
            .map(|c| FetchContentResult { content: c })
            .ok_or_else(|| tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ServerError(-32_001),
                message: format!("stdlib source not available: {filename}").into(),
                data: None,
            })
    }

    /// Formats a document identified by URI, returning whole-document edits.
    fn format_document(&self, uri: &Url) -> Option<Vec<TextEdit>> {
        // Stdlib virtual documents are read-only; return no edits.
        if uri.scheme() == "beamtalk-stdlib" {
            return None;
        }
        let path = uri_to_path(uri)?;
        let source = {
            let svc = self.service.lock().expect("service lock poisoned");
            svc.file_source(&path)?
        };

        let formatted = format_source(&source)?;

        if formatted == source {
            return Some(vec![]);
        }

        let end = offset_to_position(source.len(), &source);
        Some(vec![TextEdit {
            range: Range {
                start: tower_lsp::lsp_types::Position::new(0, 0),
                end,
            },
            new_text: formatted,
        }])
    }

    /// Republishes diagnostics for every currently-open file.
    ///
    /// BT-2027: Called once preload completes so that any file opened before
    /// the project index was fully populated has its diagnostics recomputed
    /// against the complete hierarchy. Stale `unresolved_class` warnings
    /// against now-indexed classes self-heal without user intervention.
    ///
    /// `versions` is keyed by the internal path returned by
    /// `resolve_path_for_uri`, so paths may represent `file://`, `untitled:`
    /// (via the `__untitled__/` prefix), or `beamtalk-stdlib://` virtual docs
    /// (real stdlib paths stored in `stdlib_paths`). The URI reconstructed
    /// here must match the original scheme so `publish_diagnostics` routes
    /// correctly — in particular, stdlib docs must not be republished under
    /// `file://`, which would bypass the stdlib early-return and leak
    /// diagnostics for sources the user never opened.
    async fn republish_open_diagnostics(&self) {
        let paths: Vec<Utf8PathBuf> = {
            let versions = self.versions.lock().expect("versions lock poisoned");
            versions.keys().cloned().collect()
        };
        for path in paths {
            let is_stdlib = {
                let stdlib_paths = self
                    .stdlib_paths
                    .lock()
                    .expect("stdlib_paths lock poisoned");
                stdlib_paths.contains(&path)
            };
            let uri = if is_stdlib {
                path_to_stdlib_uri(&path)
            } else {
                path_to_uri(&path)
            };
            if let Some(uri) = uri {
                self.publish_diagnostics(&uri).await;
            }
        }
    }

    /// Publishes diagnostics for a file after every change.
    /// ADR 0082 Phase 3 (BT-2289): Attach to a running workspace lazily.
    ///
    /// Returns a cloneable [`RuntimeClient`] handle on success. The handle
    /// is cached on `Backend::runtime`; subsequent calls reuse it. A
    /// background task is spawned the first time to consume `FlushEvent`s
    /// from the runtime and emit `workspace/applyEdit` per flushed file.
    ///
    /// Discovery uses the first workspace root captured at `initialize` —
    /// this matches the LSP convention "the editor told us which project to
    /// attach to" and avoids guessing across multi-root workspaces. If the
    /// workspace is not running this returns `RuntimeError::WorkspaceNotFound`
    /// and the LSP layer surfaces a friendly message to the editor.
    ///
    /// Concurrency: two parallel `executeCommand` calls may both pass the
    /// initial cache check before either finishes connecting. Both will run
    /// `RuntimeClient::connect` in parallel; whichever finishes first wins
    /// (stored in the cache + its listener spawned). The loser's client is
    /// dropped (its background tasks abort on drop via `JoinHandle::abort`
    /// when the `RuntimeInner` is freed) — no leaked tasks, but the loser's
    /// WebSocket session counts against the workspace until OS cleanup.
    /// This is acceptable for the LSP's low call rate; if it ever matters,
    /// switch to `tokio::sync::OnceCell`-style single-flight.
    async fn ensure_runtime_attached(&self) -> std::result::Result<RuntimeClient, RuntimeError> {
        {
            let guard = self.runtime.lock().await;
            if let Some(client) = guard.as_ref() {
                return Ok(client.clone());
            }
        }

        let project_root = {
            let roots = self
                .workspace_roots
                .lock()
                .expect("workspace_roots lock poisoned");
            roots
                .first()
                .cloned()
                .ok_or_else(|| RuntimeError::WorkspaceNotFound {
                    project_path: "<no workspace root>".to_string(),
                    reason: "LSP initialize did not provide a workspace folder".to_string(),
                })?
        };

        // Unbounded so a slow `applyEdit` task can't backpressure the runtime
        // listener — flushes are infrequent and small relative to typical
        // LSP traffic.
        let (flush_tx, flush_rx) = tokio::sync::mpsc::unbounded_channel::<FlushEvent>();
        // BT-2239: class-loaded / method-installed push frames so the LSP
        // can invalidate runtime-attached nav caches. Today the listener
        // just logs and drops — the per-method children (BT-2240..2244)
        // attach the real cache to it.
        let (class_changed_tx, class_changed_rx) =
            tokio::sync::mpsc::unbounded_channel::<ClassChangedEvent>();
        // ADR 0105 Phase 1 (BT-2779): reload-induced re-check outcomes, so
        // the LSP can publish/clear diagnostics on the affected callers.
        let (reload_check_tx, reload_check_rx) =
            tokio::sync::mpsc::unbounded_channel::<ReloadCheckEvent>();
        let client =
            RuntimeClient::connect(&project_root, flush_tx, class_changed_tx, reload_check_tx)
                .await?;

        // Re-check the cache under the lock before installing. If a parallel
        // call beat us to it, drop our freshly-connected client (its
        // listener/writer tasks will be aborted on drop since the only
        // strong refs are inside the soon-to-be-dropped `RuntimeInner`) and
        // return the winner's client. This narrows the race window from
        // "always leak on parallel attach" to "rare, OS-cleaned" — see the
        // doc comment above for the trade-off rationale.
        let runtime_guard_first = self.runtime.lock().await;
        if let Some(existing) = runtime_guard_first.as_ref() {
            let existing = existing.clone();
            drop(runtime_guard_first);
            // Explicit close on the loser so the workspace doesn't see a
            // dangling authenticated WS session until the OS reclaims it.
            client.close().await;
            return Ok(existing);
        }
        drop(runtime_guard_first);

        // Spawn the listener that consumes FlushEvents and emits
        // workspace/applyEdit per touched file. The listener queries the
        // *live* `versions` map on each event so files opened after attach
        // get refreshed too; we still gather the roots once because
        // workspace roots are fixed at `initialize` and never change.
        let listener_client = self.client.clone();
        let listener_roots = {
            let roots = self
                .workspace_roots
                .lock()
                .expect("workspace_roots lock poisoned");
            roots.clone()
        };
        let open_paths_handle = OpenPathsHandle {
            versions: Arc::clone(&self.versions),
        };
        let listener_handle = tokio::spawn(flush_event_listener(
            listener_client,
            listener_roots,
            open_paths_handle,
            flush_rx,
        ));

        // BT-2239: cache-invalidation listener for class-loaded /
        // method-installed push events. The listener holds a clone of the
        // shared `Arc<Mutex<NavCache>>` so it can bump the generation
        // counter as events arrive — `Backend::nav_cache_generation` reads
        // through the same lock.
        let class_changed_handle = tokio::spawn(class_changed_listener(
            Arc::clone(&self.nav_cache),
            class_changed_rx,
        ));

        let reload_check_handle = self.spawn_reload_check_listener(client.clone(), reload_check_rx);

        {
            let mut runtime_guard = self.runtime.lock().await;
            *runtime_guard = Some(client.clone());
        }
        {
            let mut listener_guard = self.flush_listener.lock().await;
            // Cancel any prior listener (shouldn't happen, but be defensive).
            if let Some(prev) = listener_guard.take() {
                prev.abort();
            }
            *listener_guard = Some(listener_handle);
        }
        {
            let mut guard = self.class_changed_listener.lock().await;
            if let Some(prev) = guard.take() {
                prev.abort();
            }
            *guard = Some(class_changed_handle);
        }
        {
            let mut guard = self.reload_check_listener.lock().await;
            if let Some(prev) = guard.take() {
                prev.abort();
            }
            *guard = Some(reload_check_handle);
        }

        // BT-2801 (ADR 0105 surface-parity gap): seed `reload_diagnostics`
        // with any findings that already existed in
        // `beamtalk_workspace_findings_store` before this attach — the
        // `reload_check_handle` listener above only ever delivers *new*
        // outcomes, so without this a fresh LSP session (or one reconnecting
        // after a crash) would show nothing for a caller until the next
        // reload happens to touch it again. Best-effort; see
        // `seed_reload_diagnostics`'s doc for the accepted narrow race with
        // a concurrently-arriving push. Re-reads `workspace_roots` (rather
        // than reusing `listener_roots`, moved into `flush_event_listener`
        // above) — same pattern as `spawn_reload_check_listener`'s own
        // fresh clone.
        let seed_roots = {
            let roots = self
                .workspace_roots
                .lock()
                .expect("workspace_roots lock poisoned");
            roots.clone()
        };
        seed_reload_diagnostics(
            &self.client,
            &client,
            &seed_roots,
            &self.service,
            &self.reload_diagnostics,
        )
        .await;

        Ok(client)
    }

    /// Spawn the reload-check listener task (ADR 0105 Phase 1, BT-2779).
    /// Extracted out of `ensure_runtime_attached` purely to keep that
    /// function under the lint's line-count limit — needs a `RuntimeClient`
    /// clone (to resolve owner class -> URI via `nav-symbols`) plus `Arc`
    /// clones of `service` and `reload_diagnostics` so it can
    /// merge-and-republish, mirroring the flush listener's pattern of
    /// holding only the specific pieces it needs rather than a `Backend`
    /// back-reference.
    fn spawn_reload_check_listener(
        &self,
        runtime_client: RuntimeClient,
        reload_check_rx: tokio::sync::mpsc::UnboundedReceiver<ReloadCheckEvent>,
    ) -> tokio::task::JoinHandle<()> {
        let roots = {
            let roots = self
                .workspace_roots
                .lock()
                .expect("workspace_roots lock poisoned");
            roots.clone()
        };
        tokio::spawn(reload_check_listener(
            self.client.clone(),
            runtime_client,
            roots,
            Arc::clone(&self.service),
            Arc::clone(&self.reload_diagnostics),
            reload_check_rx,
        ))
    }

    async fn publish_diagnostics(&self, uri: &Url) {
        let version = self.file_version_for_uri(uri);
        publish_diagnostics_impl(
            &self.client,
            &self.service,
            &self.reload_diagnostics,
            uri,
            version,
        )
        .await;
    }
}

/// Recompute static diagnostics for `uri`, merge in any live reload-induced
/// diagnostics (ADR 0105 Phase 1, BT-2779), and publish the combined set.
///
/// A free function (not a `&self` method) so both [`Backend::publish_diagnostics`]
/// and the detached `reload_check_listener` task — which only holds `Arc`
/// clones of the pieces it needs, not a `Backend` reference, since it
/// outlives any single request handler's borrow — can share one
/// implementation. LSP's `publishDiagnostics` fully replaces what the editor
/// shows for a URI (no incremental-append notification), so every call,
/// whether triggered by a normal edit/save or by a reload-check push, must
/// include both sources or one silently clobbers the other.
async fn publish_diagnostics_impl(
    client: &Client,
    service: &Mutex<SimpleLanguageService>,
    reload_diagnostics: &std::sync::Mutex<ReloadDiagnosticsByUriAndOrigin>,
    uri: &Url,
    version: Option<i32>,
) {
    // Stdlib virtual documents have no user-facing diagnostics.
    if uri.scheme() == "beamtalk-stdlib" {
        return;
    }
    let Some(path) = uri_to_path(uri) else {
        return;
    };
    let mut diagnostics: Vec<tower_lsp::lsp_types::Diagnostic> = {
        let svc = service.lock().expect("service lock poisoned");
        let source = svc.file_source(&path);
        svc.diagnostics(&path)
            .into_iter()
            .map(|d| to_lsp_diagnostic(&d, source.as_deref()))
            .collect()
    };
    {
        let reload = reload_diagnostics
            .lock()
            .expect("reload_diagnostics lock poisoned");
        // Flatten every owner's diagnostics for this URI — a file can
        // define more than one class (see `ReloadDiagnosticsByUriAndOrigin`'s
        // doc), each with its own independently clearing entry.
        if let Some(by_owner) = reload.get(uri) {
            diagnostics.extend(by_owner.values().flatten().cloned());
        }
    }
    client
        .publish_diagnostics(uri.clone(), diagnostics, version)
        .await;
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    /// Reports server capabilities to the client during handshake.
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        let roots = workspace_roots(&params);
        let configured_stdlib = configured_stdlib_source_dir(&params);
        let stdlib_dirs = configured_stdlib_source_dirs(configured_stdlib.as_deref(), &roots);
        // BT-2239: read the `delegateToRuntime` flag from
        // `initializationOptions`. Default is `false` (foundation issue
        // keeps current behaviour; per-method children opt the flag on as
        // they're rolled out).
        let delegate = configured_delegate_to_runtime(&params);
        self.set_delegate_to_runtime(delegate);
        {
            let mut stored_roots = self
                .workspace_roots
                .lock()
                .expect("workspace_roots lock poisoned");
            (*stored_roots).clone_from(&roots);
        }
        {
            let mut preload_config = self
                .preload_config
                .lock()
                .expect("preload_config lock poisoned");
            *preload_config = Some(PreloadConfig { roots, stdlib_dirs });
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        save: Some(TextDocumentSyncSaveOptions::Supported(true)),
                        ..Default::default()
                    },
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".into(), ":".into()]),
                    ..Default::default()
                }),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec![":".into()]),
                    retrigger_characters: Some(vec![" ".into()]),
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                // BT-2241: `textDocument/implementation` — selector under
                // the cursor → `SystemNavigation implementorsOf:` via the
                // BT-2239 runtime-delegate seam (cold-file fallback walks
                // the AST). Wired alongside `references_provider` because
                // both go through the same `Backend::delegate_nav_query`
                // helper and the same `nav-query` REPL op.
                implementation_provider: Some(ImplementationProviderCapability::Simple(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                // BT-2242: `textDocument/prepareTypeHierarchy` +
                // `typeHierarchy/{supertypes,subtypes}` — the class under
                // the cursor → `Behaviour superclassChain` / `allSubclasses`.
                // `lsp-types` 0.94.1 does not yet expose a typed
                // `type_hierarchy_provider` field on `ServerCapabilities`
                // (added upstream in 0.95+), so we advertise via the typed
                // `experimental` escape hatch. VSCode and Helix both accept
                // the capability through this channel; the surface-drift
                // checker recognises the `experimental` JSON value so the
                // surface-parity doc keeps verifying.
                experimental: Some(serde_json::json!({
                    "typeHierarchyProvider": true,
                })),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                document_range_formatting_provider: Some(OneOf::Left(true)),
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                // ADR 0082 Phase 3 (BT-2289): workspace/executeCommand for
                // `beamtalk.flush` and `beamtalk.saveClass` — the LSP-surface
                // wrappers that dispatch to `Workspace flush` /
                // `Workspace newClass:at:` on the attached runtime. Keep the
                // command identifiers stable; the surface-parity drift checker
                // hashes them.
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: BEAMTALK_LSP_COMMANDS
                        .iter()
                        .map(|s| (*s).to_string())
                        .collect(),
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                }),
                // BT-2243: `textDocument/prepareCallHierarchy` plus
                // `callHierarchy/{incomingCalls,outgoingCalls}` (prepare
                // lives under `textDocument/` per the LSP spec because
                // it takes a text-document position; the follow-ups live
                // under `callHierarchy/` because they take a
                // `CallHierarchyItem`). Incoming calls are answered via
                // the existing `nav-query` `senders` kind through
                // `Backend::delegate_nav_query`; outgoing calls walk the
                // method body's AST in-process (the cold-file path is the
                // only correct answer because the body lives in the open
                // file). The advertised capability is wired regardless of
                // the `delegateToRuntime` flag — runtime delegation flips
                // only the incoming-calls fallback path between the AST
                // walker and the runtime's `senders_of` xref index.
                call_hierarchy_provider: Some(CallHierarchyServerCapability::Options(
                    CallHierarchyOptions {
                        work_done_progress_options: WorkDoneProgressOptions::default(),
                    },
                )),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    /// Called after the client acknowledges initialization.
    async fn initialized(&self, _: InitializedParams) {
        let preload_config = {
            let mut preload_config = self
                .preload_config
                .lock()
                .expect("preload_config lock poisoned");
            preload_config.take()
        };
        if let Some(ref config) = preload_config {
            self.preload_workspace_source_files(config.clone()).await;
            // ADR 0075: Load type cache from _build/type_cache/ for typed completions.
            self.load_type_cache(&config.roots).await;

            // BT-2027: Re-publish diagnostics for every open file after preload
            // completes. If a file was opened via `did_open` before preload
            // finished (or against an incomplete project index), its initial
            // diagnostics may contain stale `unresolved_class` warnings against
            // classes that have since been indexed. Republishing self-heals
            // those without requiring the user to touch the file.
            self.republish_open_diagnostics().await;
        }

        // Resolve OTP lib dir for FFI goto-definition.
        self.resolve_otp_lib_dir().await;

        // BT-2242: dynamically register the type-hierarchy capability so
        // clients that ignore the `experimental` channel still pick it up.
        // `lsp-types` 0.94.1 doesn't expose `type_hierarchy_provider` on
        // `ServerCapabilities`, but `client/registerCapability` works for
        // every LSP-3.17 client regardless of the typed-field availability.
        // Failures here are non-fatal — older clients may reject dynamic
        // registration entirely, in which case the `experimental` field
        // remains the advertisement channel.
        self.register_type_hierarchy_capability().await;

        debug!("beamtalk-lsp initialized");
        self.client
            .log_message(
                tower_lsp::lsp_types::MessageType::INFO,
                "Beamtalk language server ready",
            )
            .await;
    }

    /// Handles a graceful shutdown request from the client.
    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    /// Indexes a newly opened document and publishes diagnostics.
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(path) = self.resolve_path_for_uri(&uri) {
            if uri.scheme() != "beamtalk-stdlib" {
                // Stdlib files are pre-loaded at startup; skip re-indexing.
                let mut svc = self.service.lock().expect("service lock poisoned");
                svc.update_file(path.clone(), params.text_document.text);
            }
            {
                let mut versions = self.versions.lock().expect("versions lock poisoned");
                versions.insert(path, params.text_document.version);
            }
            self.publish_diagnostics(&uri).await;
        }
    }

    /// Re-indexes a document after edits and republishes diagnostics.
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        // Stdlib virtual documents are read-only; ignore all change events.
        if uri.scheme() == "beamtalk-stdlib" {
            return;
        }
        if let (Some(path), Some(change)) =
            (uri_to_path(&uri), params.content_changes.into_iter().last())
        {
            {
                let mut svc = self.service.lock().expect("service lock poisoned");
                svc.update_file(path.clone(), change.text);
            }
            {
                let mut versions = self.versions.lock().expect("versions lock poisoned");
                versions.insert(path.clone(), params.text_document.version);
            }
            // Buffer now diverges from the on-disk bytes (and the
            // runtime's compiled module) until the next `didSave`.
            self.mark_dirty(path);

            let generation = {
                let mut generations = self
                    .diagnostic_generation
                    .lock()
                    .expect("diagnostic_generation lock poisoned");
                let entry = generations.entry(uri.clone()).or_insert(0);
                *entry += 1;
                *entry
            };

            tokio::time::sleep(DIAGNOSTIC_DEBOUNCE_DURATION).await;

            let is_latest = {
                let generations = self
                    .diagnostic_generation
                    .lock()
                    .expect("diagnostic_generation lock poisoned");
                generations.get(&uri).copied() == Some(generation)
            };

            if is_latest {
                self.publish_diagnostics(&uri).await;
            }
        }
    }

    /// Removes a closed document from the index and clears its diagnostics.
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(path) = self.resolve_path_for_uri(&uri) {
            if uri.scheme() != "beamtalk-stdlib" {
                // Keep stdlib files indexed across editor open/close cycles.
                let mut svc = self.service.lock().expect("service lock poisoned");
                svc.remove_file(&path);
            }
            {
                let mut versions = self.versions.lock().expect("versions lock poisoned");
                versions.remove(&path);
            }
            self.clear_dirty(&path);
            {
                let mut generations = self
                    .diagnostic_generation
                    .lock()
                    .expect("diagnostic_generation lock poisoned");
                generations.remove(&uri);
            }
            if uri.scheme() != "beamtalk-stdlib" {
                self.client.publish_diagnostics(uri, Vec::new(), None).await;
            }
        }
    }

    /// Handles save notifications and republishes diagnostics.
    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri;
        debug!(uri = %uri, "did_save");
        // Editor buffer now matches the on-disk bytes again. The runtime
        // will still be stale until the workspace recompiles the module
        // (a separate flush cycle), but the AST path is no longer
        // *strictly* required for outline correctness.
        if let Some(path) = self.resolve_path_for_uri(&uri) {
            self.clear_dirty(&path);
        }
        self.publish_diagnostics(&uri).await;
    }

    /// Returns completion items for the cursor position.
    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };

        let items: Vec<CompletionItem> = {
            let svc = self.service.lock().expect("service lock poisoned");
            let Some(source) = svc.file_source(&path) else {
                return Ok(None);
            };
            let pos = to_bt_position(params.text_document_position.position, &source);
            svc.completions(&path, pos)
                .into_iter()
                .map(|c| CompletionItem {
                    label: c.label.to_string(),
                    kind: Some(match c.kind {
                        CompletionKind::Function => CompletionItemKind::FUNCTION,
                        CompletionKind::Variable => CompletionItemKind::VARIABLE,
                        CompletionKind::Class => CompletionItemKind::CLASS,
                        CompletionKind::Module => CompletionItemKind::MODULE,
                        CompletionKind::Keyword => CompletionItemKind::KEYWORD,
                        CompletionKind::Field => CompletionItemKind::FIELD,
                    }),
                    detail: c.detail.map(|d| d.to_string()),
                    documentation: c.documentation.map(|d| {
                        Documentation::MarkupContent(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: d.to_string(),
                        })
                    }),
                    ..Default::default()
                })
                .collect()
        };

        if items.is_empty() {
            Ok(None)
        } else {
            Ok(Some(CompletionResponse::Array(items)))
        }
    }

    /// Returns hover information (type/docs) for the symbol at the cursor.
    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };

        let svc = self.service.lock().expect("service lock poisoned");
        let source = svc.file_source(&path);
        let Some(src) = source.as_deref() else {
            return Ok(None);
        };
        let pos = to_bt_position(params.text_document_position_params.position, src);
        let hover = svc.hover(&path, pos);

        Ok(hover.map(|h| {
            let mut value = h.contents.to_string();
            if let Some(doc) = h.documentation {
                value.push_str("\n\n");
                value.push_str(&format_hover_documentation(&doc));
            }
            if let Some(stdlib_note) = stdlib_hover_policy_note(&svc, &value) {
                value.push_str("\n\n");
                value.push_str(&stdlib_note);
            }
            Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value,
                }),
                range: Some(span_to_range(h.span, src)),
            }
        }))
    }

    /// Returns signature help for the method being called at the cursor.
    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };

        let svc = self.service.lock().expect("service lock poisoned");
        let Some(source) = svc.file_source(&path) else {
            return Ok(None);
        };
        let pos = to_bt_position(params.text_document_position_params.position, &source);
        let help = svc.signature_help(&path, pos);

        Ok(help.map(|h| {
            let signatures = h
                .signatures
                .into_iter()
                .map(|sig| {
                    let parameters = Some(
                        sig.parameters
                            .into_iter()
                            .map(|p| ParameterInformation {
                                label: ParameterLabel::Simple(p.label.to_string()),
                                documentation: p.documentation.map(|d| {
                                    Documentation::MarkupContent(MarkupContent {
                                        kind: MarkupKind::Markdown,
                                        value: d.to_string(),
                                    })
                                }),
                            })
                            .collect(),
                    );
                    SignatureInformation {
                        label: sig.label.to_string(),
                        documentation: sig.documentation.map(|d| {
                            Documentation::MarkupContent(MarkupContent {
                                kind: MarkupKind::Markdown,
                                value: d.to_string(),
                            })
                        }),
                        parameters,
                        active_parameter: None,
                    }
                })
                .collect();
            SignatureHelp {
                signatures,
                active_signature: Some(h.active_signature),
                active_parameter: Some(h.active_parameter),
            }
        }))
    }

    /// Navigates to the definition of the symbol at the cursor.
    ///
    /// Returns a `beamtalk-stdlib:///ClassName.bt` virtual URI for stdlib definitions,
    /// or a `file://` URI for user-defined symbols.
    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };

        // Hold svc lock only for the definition lookup; release before checking stdlib_paths.
        let (resolved, native_delegate, ffi_call) = {
            let svc = self.service.lock().expect("service lock poisoned");
            let Some(source) = svc.file_source(&path) else {
                return Ok(None);
            };
            let pos = to_bt_position(params.text_document_position_params.position, &source);
            let location = svc.goto_definition(&path, pos);
            if let Some(loc) = location {
                let delegate_info = svc.check_native_delegate(&loc);
                let target_source = svc.file_source(&loc.file);
                let resolved = target_source.map(|src| {
                    let range = span_to_range(loc.span, &src);
                    (loc.file.clone(), range)
                });
                (resolved, delegate_info, None)
            } else {
                // No Beamtalk definition found — check if cursor is on an FFI call.
                let ffi = svc.check_ffi_call(&path, pos);
                (None, None, ffi)
            }
        };

        // If this is a native delegate method, try to navigate to the backing .erl file.
        if let Some(delegate_info) = native_delegate {
            if let Some(erl_path) = self.find_erlang_source_file(&delegate_info.backing_module) {
                if let Ok(target_uri) = Url::from_file_path(&erl_path) {
                    // Resolve the matching `handle_call` clause line so the jump
                    // lands on the same clause the System Browser's native-source
                    // jump does (BT-2582). If the `.erl` is unreadable or the
                    // selector has no `handle_call` clause (e.g. a delegate that
                    // replies from `handle_info`), fall back to the file top.
                    let range = std::fs::read_to_string(&erl_path)
                        .ok()
                        .and_then(|content| {
                            beamtalk_core::queries::definition_provider::handle_call_clause_line(
                                &content,
                                &delegate_info.selector,
                            )
                        })
                        .map_or_else(tower_lsp::lsp_types::Range::default, |line| {
                            // Clause lines are 1-based; LSP lines are 0-based.
                            let lsp_line = line.saturating_sub(1);
                            tower_lsp::lsp_types::Range {
                                start: tower_lsp::lsp_types::Position::new(lsp_line, 0),
                                end: tower_lsp::lsp_types::Position::new(lsp_line, 0),
                            }
                        });
                    return Ok(Some(GotoDefinitionResponse::Scalar(
                        tower_lsp::lsp_types::Location {
                            uri: target_uri,
                            range,
                        },
                    )));
                }
            }
            // Fall through to .bt location if .erl file not found.
        }

        // If this is an Erlang FFI call, try to navigate to the .erl source file.
        if let Some(ffi_info) = ffi_call {
            if let Some(erl_path) = self.find_erlang_source_file(&ffi_info.module_name) {
                if let Ok(target_uri) = Url::from_file_path(&erl_path) {
                    // Use the function's source line from .beam abstract_code if available.
                    // LSP lines are 0-based; abstract_code lines are 1-based.
                    let range = ffi_info
                        .line
                        .map(|line| {
                            let lsp_line = line.saturating_sub(1);
                            tower_lsp::lsp_types::Range {
                                start: tower_lsp::lsp_types::Position::new(lsp_line, 0),
                                end: tower_lsp::lsp_types::Position::new(lsp_line, 0),
                            }
                        })
                        .unwrap_or_default();
                    return Ok(Some(GotoDefinitionResponse::Scalar(
                        tower_lsp::lsp_types::Location {
                            uri: target_uri,
                            range,
                        },
                    )));
                }
            }
        }

        Ok(resolved.and_then(|(file, range)| {
            let is_stdlib = {
                let stdlib_paths = self
                    .stdlib_paths
                    .lock()
                    .expect("stdlib_paths lock poisoned");
                stdlib_paths.contains(&file)
            };
            let target_uri = if is_stdlib {
                path_to_stdlib_uri(&file)?
            } else {
                path_to_uri(&file)?
            };
            Some(GotoDefinitionResponse::Scalar(
                tower_lsp::lsp_types::Location {
                    uri: target_uri,
                    range,
                },
            ))
        }))
    }

    /// Finds all references to the symbol at the cursor.
    ///
    /// BT-2239 wired this through [`Backend::delegate_nav_query`] as the
    /// first per-method consumer of the runtime-attached navigation seam.
    /// BT-2240 closes the **declaration-merge gap** left there:
    ///
    /// * `senders_of/1` (the runtime backing for selectors) returns call
    ///   sites only — it never returns the method-definition headers.
    /// * `references_to/1` (the runtime backing for classes) returns use
    ///   sites only — it never returns the class-declaration name span.
    ///
    /// The AST walker has always returned both definitions and call sites
    /// merged into one list. So when the LSP `context.includeDeclaration`
    /// flag is `true` (the LSP default) and the runtime path is active,
    /// we overlay the AST-known declaration sites onto the runtime result.
    /// When `includeDeclaration` is `false`, we strip declarations out of
    /// the AST result so the cold-file path obeys the flag too.
    ///
    /// Behaviour matrix:
    ///
    /// | cursor on | `include_declaration` | result |
    /// |-----------|-----------------------|--------|
    /// | selector  | `true`  | runtime `senders` + AST/runtime declarations |
    /// | selector  | `false` | runtime `senders` only |
    /// | class     | `true`  | runtime `references` + AST class-decl sites |
    /// | class     | `false` | runtime `references` only |
    /// | local id  | any      | AST walker (locals have no declaration overlay — `include_declaration` is a no-op for them) |
    async fn references(
        &self,
        params: ReferenceParams,
    ) -> Result<Option<Vec<tower_lsp::lsp_types::Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };
        let include_declaration = params.context.include_declaration;

        // Compute everything that depends on `svc` up front so the lock is
        // released before any async runtime call (delegate_nav_query awaits
        // a runtime round-trip when the flag is on).
        let (pos, runtime_query) = {
            let svc = self.service.lock().expect("service lock poisoned");
            let Some(source) = svc.file_source(&path) else {
                return Ok(None);
            };
            let pos = to_bt_position(params.text_document_position.position, &source);
            let runtime_query = if self.delegate_to_runtime() {
                svc.references_query_at(&path, pos)
            } else {
                None
            };
            (pos, runtime_query)
        };

        let backend_self = self;
        let path_for_ast = path.clone();
        // Cold-file fallback. Builds an LSP location list from the AST
        // walker. The walker always returns definitions+calls; strip
        // declarations out when `include_declaration` is false so the
        // cold-file path mirrors the runtime-path behaviour.
        let ast_fallback = || -> Vec<tower_lsp::lsp_types::Location> {
            let svc = backend_self.service.lock().expect("service lock poisoned");
            let refs = svc.find_references(&path_for_ast, pos);
            // When the caller wants senders/uses only, subtract the AST
            // declaration set for this cursor from the merged AST results.
            let decl_set: HashSet<(Utf8PathBuf, Span)> = if include_declaration {
                HashSet::new()
            } else {
                ast_declarations_for_cursor(&svc, &path_for_ast, pos)
                    .into_iter()
                    .map(|loc| (loc.file, loc.span))
                    .collect()
            };
            refs.into_iter()
                .filter(|loc| {
                    include_declaration || !decl_set.contains(&(loc.file.clone(), loc.span))
                })
                .filter_map(|loc| {
                    let source = svc.file_source(&loc.file)?;
                    let range = span_to_range(loc.span, &source);
                    Some(tower_lsp::lsp_types::Location {
                        uri: path_to_uri(&loc.file)?,
                        range,
                    })
                })
                .collect()
        };

        let locations = match runtime_query {
            Some(query) => {
                let mut sites = self
                    .delegate_nav_query(query.clone(), runtime_site_to_lsp_location, ast_fallback)
                    .await;
                if include_declaration {
                    let decl_locs = self.declaration_sites_for_query(&query).await;
                    merge_locations(&mut sites, decl_locs);
                }
                sites
            }
            None => {
                // Cursor isn't on a selector or class name (local
                // identifier, parameter, etc.) — the runtime can't answer
                // this, so go straight to the AST path. `ast_fallback`
                // already honours `include_declaration`.
                ast_fallback()
            }
        };

        if locations.is_empty() {
            Ok(None)
        } else {
            Ok(Some(locations))
        }
    }

    /// BT-2241: Finds every class that implements the selector under the
    /// cursor (`textDocument/implementation`).
    ///
    /// When `delegateToRuntime` is on and a runtime is attached, classifies
    /// the cursor with [`SimpleLanguageService::implementors_query_at`] and
    /// forwards an `ImplementorsOf` [`NavQuery`] through
    /// [`Backend::delegate_nav_query`]; the runtime answers via
    /// `beamtalk_xref` (one site per implementing class, instance- and
    /// class-side both reported) and includes ADR-0066 extension methods
    /// the AST walker can't see.
    ///
    /// Otherwise (flag off, no workspace, runtime error), falls back to the
    /// in-process AST walker via [`SimpleLanguageService::find_implementors`].
    ///
    /// Returns `None` (LSP "no result") when the cursor isn't on a selector
    /// or when no class defines it. LSP technically distinguishes `null`
    /// from `[]` on the wire, but most editors treat both as "no jump
    /// target"; collapsing both cases to `None` keeps the handler simple
    /// and matches the existing `goto_definition` shape.
    async fn goto_implementation(
        &self,
        params: tower_lsp::lsp_types::request::GotoImplementationParams,
    ) -> Result<Option<tower_lsp::lsp_types::request::GotoImplementationResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };

        // Compute everything that depends on `svc` up front so the lock is
        // released before any async runtime call (delegate_nav_query awaits
        // a runtime round-trip when the flag is on). Mirrors the `references`
        // handler structure (BT-2239 reference impl).
        let (pos, runtime_query) = {
            let svc = self.service.lock().expect("service lock poisoned");
            let Some(source) = svc.file_source(&path) else {
                return Ok(None);
            };
            let pos = to_bt_position(params.text_document_position_params.position, &source);
            let runtime_query = if self.delegate_to_runtime() {
                svc.implementors_query_at(&path, pos)
            } else {
                None
            };
            (pos, runtime_query)
        };

        let backend_self = self;
        let path_for_ast = path.clone();
        let ast_fallback = || -> Vec<tower_lsp::lsp_types::Location> {
            let svc = backend_self.service.lock().expect("service lock poisoned");
            // Resolve the selector locally so the AST fallback works when
            // the runtime flag is off (in which case `runtime_query` above
            // is `None` and the same `implementors_query_at` classification
            // is needed here). Reusing the classifier keeps cold-file and
            // runtime-attached modes in lockstep.
            let Some(query) = svc.implementors_query_at(&path_for_ast, pos) else {
                return Vec::new();
            };
            let Some(selector) = query.selector() else {
                return Vec::new();
            };
            let impls = svc.find_implementors(selector);
            impls
                .into_iter()
                .filter_map(|loc| {
                    let source = svc.file_source(&loc.file)?;
                    // Collapse to a zero-width range at the start of the
                    // method-header line, matching `runtime_site_to_lsp_location`
                    // so goto-impl selection looks identical in cold-file and
                    // runtime-attached modes (BT-2241 review).
                    let start = offset_to_position(loc.span.start() as usize, &source);
                    let line_start = tower_lsp::lsp_types::Position::new(start.line, 0);
                    let range = tower_lsp::lsp_types::Range {
                        start: line_start,
                        end: line_start,
                    };
                    Some(tower_lsp::lsp_types::Location {
                        uri: path_to_uri(&loc.file)?,
                        range,
                    })
                })
                .collect()
        };

        let locations = if let Some(query) = runtime_query {
            self.delegate_nav_query(query, runtime_site_to_lsp_location, ast_fallback)
                .await
        } else {
            // Cursor isn't on a selector — `implementorsOf:` has no answer
            // for non-selector tokens. Skip the runtime hop and run the
            // fallback (which also returns empty for the same reason).
            ast_fallback()
        };

        if locations.is_empty() {
            Ok(None)
        } else {
            // `GotoImplementationResponse` is an alias for `GotoDefinitionResponse`
            // in lsp-types; using the underlying type avoids the
            // `request::` path everywhere we construct the response.
            Ok(Some(GotoDefinitionResponse::Array(locations)))
        }
    }

    /// BT-2243: `textDocument/prepareCallHierarchy` — resolve the cursor to
    /// a method-level `CallHierarchyItem` the editor can pass back to
    /// `callHierarchy/{incomingCalls,outgoingCalls}`.
    ///
    /// Cold-file classification only — `SimpleLanguageService::call_hierarchy_prepare_at`
    /// walks the open document's AST. When the hit lands on a method-
    /// definition header we record the enclosing class and class-side flag
    /// in `data` (via [`SerializedCallTarget`]) so outgoing calls can
    /// reliably locate the body to walk; call-site hits omit those fields
    /// because the receiver class is dynamic.
    ///
    /// Returns `None` (`null` to the editor) when the cursor is on a local
    /// identifier, whitespace, or any non-selector shape — VS Code falls
    /// back to no call hierarchy in that case.
    async fn prepare_call_hierarchy(
        &self,
        params: CallHierarchyPrepareParams,
    ) -> Result<Option<Vec<CallHierarchyItem>>> {
        let uri = params.text_document_position_params.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(&uri) else {
            return Ok(None);
        };

        let item = {
            let svc = self.service.lock().expect("service lock poisoned");
            let Some(source) = svc.file_source(&path) else {
                return Ok(None);
            };
            let pos = to_bt_position(params.text_document_position_params.position, &source);
            let Some(target) = svc.call_hierarchy_prepare_at(&path, pos) else {
                return Ok(None);
            };
            target_to_lsp_item(&target, &uri, &source)
        };

        match item {
            Some(item) => Ok(Some(vec![item])),
            None => Ok(None),
        }
    }

    /// BT-2242: `textDocument/prepareTypeHierarchy` — resolves the class
    /// under the cursor to a single [`TypeHierarchyItem`] the editor can
    /// then pass to [`Self::supertypes`] / [`Self::subtypes`].
    ///
    /// Cold-file only: type-hierarchy classification (selectors vs. class
    /// names vs. locals) lives in [`SimpleLanguageService::type_hierarchy_prepare_at`].
    /// The query channel (`nav-query`) is deliberately kept locked to the
    /// three navigation kinds it ships today (`senders` / `implementors` /
    /// `references`); the class-hierarchy data needed here is structural
    /// (parent edges in `ClassHierarchy`), so the AST-walker answer is
    /// strictly more complete than a runtime query would be in cold-file
    /// mode. The runtime-attached path falls back to the same AST walker
    /// when the flag is off, matching BT-2243's "outgoing calls" choice.
    ///
    /// Returns `None` when the cursor is not on a known class name (a
    /// selector, a local identifier, whitespace, ...). LSP's
    /// `prepareTypeHierarchy` semantics use `null` to mean "no item" — the
    /// editor then suppresses the supertypes/subtypes follow-up.
    async fn prepare_type_hierarchy(
        &self,
        params: TypeHierarchyPrepareParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };

        let prepared = {
            let svc = self.service.lock().expect("service lock poisoned");
            let Some(source) = svc.file_source(&path) else {
                return Ok(None);
            };
            let pos = to_bt_position(params.text_document_position_params.position, &source);
            let Some((class_name, declaration)) = svc.type_hierarchy_prepare_at(&path, pos) else {
                return Ok(None);
            };
            // The clicked-on file's URI is the authoritative fallback when
            // the class itself has no indexed declaration (built-in or
            // runtime-only class). Tests rely on this — `prepareTypeHierarchy`
            // on `Object` in a user file returns an item the editor can
            // still annotate, even though `Object`'s source isn't in the
            // workspace.
            type_hierarchy_item(class_name.as_str(), declaration.as_ref(), &svc, uri)
        };

        Ok(prepared.map(|item| vec![item]))
    }

    /// BT-2243: `callHierarchy/incomingCalls` — who calls this method.
    ///
    /// Decodes the [`SerializedCallTarget`] from the item's `data` field
    /// to recover the selector, then dispatches through the existing
    /// `nav-query` `SendersOf` channel via [`Backend::delegate_nav_query`].
    /// The runtime path uses `beamtalk_xref:senders_of/1` (live, sees
    /// extension methods + `ChangeLog` patches); the AST fallback uses
    /// the in-process `references_provider::find_selector_references`
    /// walker — the same path used by `textDocument/references`.
    ///
    /// Each sender site becomes one `CallHierarchyIncomingCall`. Sites with
    /// no backing source file (stdlib without `source_file` metadata,
    /// dynamic / bootstrap classes) are dropped — the editor cannot
    /// navigate to them anyway.
    async fn incoming_calls(
        &self,
        params: CallHierarchyIncomingCallsParams,
    ) -> Result<Option<Vec<CallHierarchyIncomingCall>>> {
        let Some(serialized) = SerializedCallTarget::from_item(&params.item) else {
            return Ok(None);
        };
        let selector = serialized.selector.clone();

        let backend_self = self;
        let display_name = serialized.selector.clone();
        let ast_fallback = move || -> Vec<CallHierarchyIncomingCall> {
            let svc = backend_self.service.lock().expect("service lock poisoned");
            // Sender sites only — `find_selector_send_sites_across_files`
            // intentionally excludes method-definition headers (see its
            // docstring) so the editor doesn't list the method itself as
            // an incoming call. This mirrors the runtime-attached path,
            // where `nav-query` `senders` returns send sites only.
            let refs = svc.find_selector_send_sites_across_files(&selector);
            refs.into_iter()
                .filter_map(|loc| {
                    let source = svc.file_source(&loc.file)?;
                    let range = span_to_range(loc.span, &source);
                    let uri = path_to_uri(&loc.file)?;
                    let from = CallHierarchyItem {
                        name: display_name.clone(),
                        kind: SymbolKind::METHOD,
                        tags: None,
                        detail: None,
                        uri,
                        range,
                        selection_range: range,
                        data: None,
                    };
                    Some(CallHierarchyIncomingCall {
                        from,
                        from_ranges: vec![range],
                    })
                })
                .collect()
        };

        let query = NavQuery::SendersOf(serialized.selector.clone().into());
        let to_lsp =
            move |site: &NavSite, roots: &[PathBuf]| -> Option<CallHierarchyIncomingCall> {
                let resolved = nav_site_to_location(site, roots)?;
                let uri = path_to_uri(&resolved.file)?;
                let line = resolved.line.checked_sub(1)?;
                let range = Range {
                    start: Position::new(line, 0),
                    end: Position::new(line, 0),
                };
                // Name the item after the *containing* method (the caller),
                // because the editor renders this as "X calls our method".
                let from = CallHierarchyItem {
                    name: site.method.to_string(),
                    kind: SymbolKind::METHOD,
                    tags: None,
                    detail: Some(format_class_detail(&site.class, site.class_side)),
                    uri,
                    range,
                    selection_range: range,
                    data: None,
                };
                Some(CallHierarchyIncomingCall {
                    from,
                    from_ranges: vec![range],
                })
            };

        let calls = self.delegate_nav_query(query, to_lsp, ast_fallback).await;
        if calls.is_empty() {
            Ok(None)
        } else {
            Ok(Some(calls))
        }
    }

    /// BT-2243: `callHierarchy/outgoingCalls` — what does this method call.
    ///
    /// Walks the method body's AST in-process via
    /// [`find_all_sends_in_source`]: the body source slice comes from the
    /// item's recorded file + range (the file the editor opened, the range
    /// covering the method definition). One [`CallHierarchyOutgoingCall`]
    /// per send is emitted, with the selector as the item's name and the
    /// 1-based line (translated to LSP 0-based) as the call range.
    ///
    /// Erlang FFI sends (`Erlang foo` / `(Erlang foo) bar:`) are skipped —
    /// these are Erlang function invocations through the `ErlangModule`
    /// DNU bridge, not Beamtalk message sends, so showing them as
    /// outgoing calls would shadow Beamtalk selectors with Erlang
    /// module/function names. Mirrors the same exclusion in the stdlib
    /// `unusedSelectors` query (BT-2212).
    ///
    /// Returns `None` when:
    /// * The item is a call-site target (no enclosing method body to walk)
    /// * The file isn't open / has no cached source
    /// * The body slice contains no sends
    async fn outgoing_calls(
        &self,
        params: CallHierarchyOutgoingCallsParams,
    ) -> Result<Option<Vec<CallHierarchyOutgoingCall>>> {
        let Some(serialized) = SerializedCallTarget::from_item(&params.item) else {
            return Ok(None);
        };
        // A call-site hit (no class context) has no body to walk.
        if serialized.class_name.is_none() {
            return Ok(None);
        }
        let Ok(path) = Utf8PathBuf::try_from(PathBuf::from(serialized.file.as_str())) else {
            return Ok(None);
        };

        let calls = {
            let svc = self.service.lock().expect("service lock poisoned");
            let Some(source) = svc.file_source(&path) else {
                return Ok(None);
            };
            let Some(body_slice) =
                source.get(serialized.range_start as usize..serialized.range_end as usize)
            else {
                return Ok(None);
            };
            // Re-derive the LSP line offset for the body slice so per-hit
            // ranges land at the right absolute line in the file. Method
            // definitions start at column 0 in practice (the unparser
            // emits them that way), so a column anchor is not needed —
            // the LSP item's range carries the precise span.
            let body_start_pos = offset_to_position(serialized.range_start as usize, &source);
            let Some(uri) = path_to_uri(&path) else {
                return Ok(None);
            };
            outgoing_calls_for_body(body_slice, body_start_pos, &uri)
        };

        if calls.is_empty() {
            Ok(None)
        } else {
            Ok(Some(calls))
        }
    }

    /// BT-2242: `typeHierarchy/supertypes` — answers "what does this class
    /// inherit from, transitively?" via [`SimpleLanguageService::supertypes_of`]
    /// (which delegates to [`ClassHierarchy::superclass_chain`]).
    ///
    /// Returns one [`TypeHierarchyItem`] per ancestor, in order from
    /// nearest parent to root. Names whose declaration site isn't indexed
    /// (built-in classes the language service hasn't loaded) still appear
    /// in the list — they carry a synthetic zero-range and the URI of the
    /// originating item, so editors can show the name even if they can't
    /// navigate to it. See [`type_hierarchy_item_for_undeclared`] for the
    /// rationale.
    async fn supertypes(
        &self,
        params: TypeHierarchySupertypesParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        let parent_uri = params.item.uri.clone();
        let class_name = params.item.name.clone();
        let items = {
            let svc = self.service.lock().expect("service lock poisoned");
            collect_hierarchy_items(svc.supertypes_of(class_name.as_str()), &svc, &parent_uri)
        };
        Ok(Some(items))
    }

    /// BT-2242: `typeHierarchy/subtypes` — answers "who inherits from this
    /// class, transitively?" via [`SimpleLanguageService::subtypes_of`]
    /// (which delegates to [`ClassHierarchy::all_subclasses`]).
    ///
    /// Order is the BFS order of `all_subclasses` — direct children first,
    /// then grandchildren, etc. Names whose declaration site isn't indexed
    /// are surfaced with [`type_hierarchy_item_for_undeclared`], same as
    /// supertypes.
    async fn subtypes(
        &self,
        params: TypeHierarchySubtypesParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        let parent_uri = params.item.uri.clone();
        let class_name = params.item.name.clone();
        let items = {
            let svc = self.service.lock().expect("service lock poisoned");
            collect_hierarchy_items(svc.subtypes_of(class_name.as_str()), &svc, &parent_uri)
        };
        Ok(Some(items))
    }

    /// Returns the document symbol outline (classes, methods, fields).
    ///
    /// BT-2244 wired this through [`Backend::delegate_nav_symbols`] as the
    /// per-file dispatcher: when `delegateToRuntime` is on and a workspace
    /// is attached, the runtime answers via `nav-symbols` (the live class
    /// registry — picks up REPL-loaded and live-edited classes the AST
    /// walker can't see) and we filter to classes whose `source_file`
    /// resolves to the requested URI's path. Otherwise we fall back to
    /// the in-process AST walker, byte-for-byte identical to the
    /// pre-BT-2244 behaviour.
    ///
    /// **Bypasses the runtime path for buffers the runtime can't answer
    /// correctly** (BT-2244 review fix). The runtime path needs a stable
    /// `source_file` correspondence and the latest source bytes — neither
    /// holds for:
    /// * `untitled:` URIs — no `source_file` exists in the class registry
    ///   (the buffer has never been saved), so a `scope = "user"` query
    ///   filters every class out and returns an empty outline.
    /// * `beamtalk-stdlib:` URIs — virtual stdlib documents. `scope =
    ///   "user"` filters out stdlib classes by construction, so the outline
    ///   would be empty here too.
    /// * Dirty (unsaved) `file://` documents — `did_change` updates the
    ///   in-memory AST cache but never re-pushes source to the runtime, so
    ///   the runtime's reply reflects stale (last-saved / last-compiled)
    ///   bytes. The AST path uses the latest in-memory copy.
    ///
    /// For any of those, we drop straight into the AST fallback rather
    /// than asking the runtime.
    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };

        // AST fallback — preserves the pre-BT-2244 behaviour byte-for-byte.
        // Used when the flag is off, the runtime is unreachable, the
        // runtime returns an error, or the request is for a buffer the
        // runtime can't answer correctly (see the doc comment above).
        let ast_fallback = || -> Vec<tower_lsp::lsp_types::DocumentSymbol> {
            let svc = self.service.lock().expect("service lock poisoned");
            let Some(src) = svc.file_source(&path) else {
                return Vec::new();
            };
            let symbols = svc.document_symbols(&path);
            symbols
                .into_iter()
                .map(|s| to_lsp_symbol(s, &src))
                .collect()
        };

        // Runtime path needs (a) a `file://` URI so `source_file`
        // correspondence is meaningful, and (b) a clean buffer so the
        // runtime's view of the source matches the editor's. If either
        // fails, take the AST path directly.
        let runtime_path_ok = uri.scheme() == "file" && !self.is_dirty(&path);
        if !runtime_path_ok {
            let lsp_symbols = ast_fallback();
            return if lsp_symbols.is_empty() {
                Ok(None)
            } else {
                Ok(Some(DocumentSymbolResponse::Nested(lsp_symbols)))
            };
        }

        let target_path = path.clone();
        let to_lsp = move |classes: Vec<NavSymbolClass>,
                           roots: &[PathBuf]|
              -> Vec<tower_lsp::lsp_types::DocumentSymbol> {
            classes
                .into_iter()
                .filter_map(|c| runtime_class_to_document_symbol(c, &target_path, roots))
                .collect()
        };

        // `scope = "user"` — `document_symbol` is per-file, so only
        // source-backed classes are reachable here. Reduces the wire
        // payload and avoids touching stdlib's huge class set.
        let lsp_symbols = self
            .delegate_nav_symbols(Some("user"), to_lsp, ast_fallback)
            .await;

        if lsp_symbols.is_empty() {
            Ok(None)
        } else {
            Ok(Some(DocumentSymbolResponse::Nested(lsp_symbols)))
        }
    }

    /// Returns workspace-wide class symbols matching the query (BT-2081).
    ///
    /// BT-2244 wired this through [`Backend::delegate_nav_symbols`]:
    /// * when `delegateToRuntime` is on and a workspace is attached, the
    ///   runtime answers via `nav-symbols` against the live class
    ///   registry — the headline win is that REPL-loaded classes with
    ///   no source file appear here (impossible with the AST/glob path
    ///   which only sees indexed `.bt` files on disk). Classes without
    ///   a `source_file` are attached to the first workspace root with
    ///   a zero-width range at (0, 0); the symbol detail carries
    ///   `(no source file)` so editors render them visibly distinct.
    /// * Otherwise — flag off, runtime unreachable, runtime error — the
    ///   fallback iterates every indexed user file and emits one
    ///   `SymbolInformation` per top-level class whose name contains the
    ///   query string (case-insensitive, substring match). Stdlib files
    ///   are excluded so the result mirrors the MCP `list_classes`
    ///   "user" scope.
    ///
    /// An empty query returns every user class; this matches MCP
    /// behaviour and the editor's "Ctrl-T with empty filter" UX.
    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let query_lower = params.query.to_ascii_lowercase();

        // AST fallback path — kept verbatim from the pre-BT-2244
        // implementation so cold-file mode is byte-for-byte identical.
        let query_for_fallback = query_lower.clone();
        let ast_fallback = || -> Vec<SymbolInformation> {
            let svc = self.service.lock().expect("service lock poisoned");
            let mut out: Vec<SymbolInformation> = Vec::new();
            let files: Vec<Utf8PathBuf> = svc
                .project_index()
                .indexed_files()
                .into_iter()
                .filter(|f| !svc.project_index().is_stdlib_file(f))
                .cloned()
                .collect();
            for file in files {
                let Some(source) = svc.file_source(&file) else {
                    continue;
                };
                let symbols = svc.document_symbols(&file);
                let Some(uri) = path_to_uri(&file) else {
                    continue;
                };
                for sym in symbols {
                    if !matches!(sym.kind, DocumentSymbolKind::Class) {
                        continue;
                    }
                    let name = sym
                        .name
                        .as_str()
                        .strip_suffix(" (class)")
                        .unwrap_or(sym.name.as_str())
                        .to_string();
                    if !query_for_fallback.is_empty()
                        && !name.to_ascii_lowercase().contains(&query_for_fallback)
                    {
                        continue;
                    }
                    let range = span_to_range(sym.span, &source);
                    #[expect(
                        deprecated,
                        reason = "LSP SymbolInformation requires deprecated field"
                    )]
                    let info = SymbolInformation {
                        name,
                        kind: SymbolKind::CLASS,
                        tags: None,
                        deprecated: None,
                        location: tower_lsp::lsp_types::Location {
                            uri: uri.clone(),
                            range,
                        },
                        container_name: None,
                    };
                    out.push(info);
                }
            }
            out
        };

        // Workspace-root URI fallback used when the runtime reports a
        // class with no `source_file`. Editors that get this row see
        // the symbol but a zero-width range; clicking opens the project
        // root (or stays put if the editor declines the navigation).
        let workspace_root_uri = {
            let roots = self
                .workspace_roots
                .lock()
                .expect("workspace_roots lock poisoned");
            roots.first().and_then(|p| Url::from_file_path(p).ok())
        };
        // Stdlib paths to exclude from the runtime path — mirrors the
        // AST fallback at line ~2126 which skips `is_stdlib_file`. The
        // runtime's `scope=all` returns every loaded class including
        // stdlib (`Integer`, `String`, ...); without this filter the
        // editor's Ctrl-T picker would balloon with stdlib classes that
        // were not in the cold-file result.
        let stdlib_paths: HashSet<Utf8PathBuf> = {
            let guard = self
                .stdlib_paths
                .lock()
                .expect("stdlib_paths lock poisoned");
            guard.clone()
        };

        let to_lsp =
            move |classes: Vec<NavSymbolClass>, roots: &[PathBuf]| -> Vec<SymbolInformation> {
                classes
                    .iter()
                    .filter(|c| !class_source_is_stdlib(c, roots, &stdlib_paths))
                    .filter_map(|c| {
                        runtime_class_to_workspace_symbol(
                            c,
                            &query_lower,
                            roots,
                            workspace_root_uri.as_ref(),
                        )
                    })
                    .collect()
            };

        // `scope = "all"` — workspace/symbol's headline win is showing
        // source-less classes (REPL-loaded, dynamically built). Letting
        // the runtime return its full class list and filtering on the
        // LSP side keeps the query string locally-applied for
        // case-insensitivity parity with the AST path.
        let out = self
            .delegate_nav_symbols(Some("all"), to_lsp, ast_fallback)
            .await;

        if out.is_empty() {
            Ok(None)
        } else {
            Ok(Some(out))
        }
    }

    /// Formats the entire document using the Beamtalk unparser.
    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        Ok(self.format_document(&params.text_document.uri))
    }

    /// Formats the document for a selected range.
    ///
    /// Beamtalk formatting is a whole-file operation (the unparser works on the
    /// full module AST), so this method formats the entire document and returns
    /// a single edit even when only a range is selected. `VSCode` accepts
    /// whole-document edits from `rangeFormatting` without issue.
    async fn range_formatting(
        &self,
        params: DocumentRangeFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        Ok(self.format_document(&params.text_document.uri))
    }

    /// Returns code actions available at the requested range.
    ///
    /// Currently surfaces "Add annotation: -> `ClassName`" quick-fixes for
    /// unannotated methods whose return type the `TypeChecker` can infer
    /// (BT-1067, ADR 0045 Phase 1b).
    #[expect(
        clippy::cast_possible_truncation,
        reason = "source files over 4GB are not supported"
    )]
    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let uri = &params.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };
        // Stdlib files are read-only; no code actions.
        if uri.scheme() == "beamtalk-stdlib" {
            return Ok(None);
        }

        let (source, actions) = {
            let svc = self.service.lock().expect("service lock poisoned");
            let Some(source) = svc.file_source(&path) else {
                return Ok(None);
            };
            let range = &params.range;
            let start_offset = position_to_offset(range.start, &source) as u32;
            let end_offset = position_to_offset(range.end, &source) as u32;
            let actions = svc.code_actions(&path, start_offset, end_offset);
            (source, actions)
        };

        if actions.is_empty() {
            return Ok(Some(vec![]));
        }

        let lsp_actions: CodeActionResponse = actions
            .into_iter()
            .map(|action| {
                let insert_pos = offset_to_position(action.insert_at as usize, &source);
                let edit_range = Range {
                    start: insert_pos,
                    end: insert_pos,
                };
                let text_edit = TextEdit {
                    range: edit_range,
                    new_text: action.new_text.to_string(),
                };
                let mut changes = HashMap::new();
                changes.insert(uri.clone(), vec![text_edit]);
                let lsp_action = CodeAction {
                    title: action.title.to_string(),
                    kind: Some(CodeActionKind::QUICKFIX),
                    edit: Some(WorkspaceEdit {
                        changes: Some(changes),
                        ..Default::default()
                    }),
                    is_preferred: Some(false),
                    ..Default::default()
                };
                CodeActionOrCommand::CodeAction(lsp_action)
            })
            .collect();

        Ok(Some(lsp_actions))
    }

    /// ADR 0082 Phase 3 (BT-2289): dispatch `workspace/executeCommand`.
    ///
    /// Each LSP command compiles to a Beamtalk expression submitted via the
    /// existing `evaluate` REPL op on the attached workspace. Per ADR 0082
    /// the language is the API — there are no new workspace-side ops. This
    /// handler:
    ///
    /// 1. Looks up the command and builds the Beamtalk expression (see
    ///    [`build_command_expression`]).
    /// 2. Attaches to a running workspace lazily if not already attached (see
    ///    [`Backend::ensure_runtime_attached`]).
    /// 3. Submits the expression and returns the result value to the editor.
    ///
    /// A missing workspace (`beamtalk run .` not active) surfaces a friendly
    /// `MessageType::WARNING` log to the client and an `internal_error`
    /// JSON-RPC response so the editor's progress UI ends cleanly. Structured
    /// runtime errors (`#beamtalk_error{}`) come back through `ReplResponse`
    /// and are forwarded as the command result so editors can surface the
    /// detail to the user.
    async fn execute_command(
        &self,
        params: ExecuteCommandParams,
    ) -> Result<Option<serde_json::Value>> {
        let expr = match build_command_expression(&params.command, &params.arguments) {
            Ok(expr) => expr,
            Err(msg) => {
                self.client
                    .log_message(
                        MessageType::WARNING,
                        format!("executeCommand {}: {msg}", params.command),
                    )
                    .await;
                return Err(tower_lsp::jsonrpc::Error::invalid_params(msg));
            }
        };

        let runtime = match self.ensure_runtime_attached().await {
            Ok(client) => client,
            Err(e) => {
                let detail = format!(
                    "Beamtalk LSP could not reach a running workspace for executeCommand `{}`. \
                     Start `beamtalk run .` (or `beamtalk repl`) against this project and retry. \
                     ({e})",
                    params.command
                );
                self.client.log_message(MessageType::WARNING, &detail).await;
                let mut err = tower_lsp::jsonrpc::Error::internal_error();
                err.message = detail.into();
                return Err(err);
            }
        };

        match runtime.evaluate(&expr).await {
            Ok(response) => {
                if response.is_error() {
                    let msg = response
                        .error_message()
                        .unwrap_or("workspace evaluator returned an error");
                    self.client
                        .log_message(
                            MessageType::WARNING,
                            format!("executeCommand {}: {msg}", params.command),
                        )
                        .await;
                    // Forward structured Beamtalk evaluator errors as the
                    // command result so editors can surface the rich payload
                    // (matches the docstring above). Reserve JSON-RPC errors
                    // for transport/parameter failures.
                    let value = response.value.clone().unwrap_or_else(|| {
                        serde_json::json!({
                            "error": msg,
                            "status": response.status,
                        })
                    });
                    return Ok(Some(value));
                }
                let pretty = response.value_string();
                let value = response.value.unwrap_or(serde_json::Value::Null);
                if !pretty.is_empty() {
                    self.client
                        .log_message(
                            MessageType::INFO,
                            format!("executeCommand {}: {pretty}", params.command),
                        )
                        .await;
                }
                Ok(Some(value))
            }
            Err(e) => {
                let detail = format!("executeCommand {}: runtime error: {e}", params.command);
                self.client.log_message(MessageType::WARNING, &detail).await;
                let mut err = tower_lsp::jsonrpc::Error::internal_error();
                err.message = detail.into();
                Err(err)
            }
        }
    }
}

/// Shared handle to the LSP `Backend::versions` map, used by the flush
/// listener task to ask "is this file currently open?" on each `FlushEvent`
/// without copying the whole map up-front. Wrapping rather than passing
/// `Arc<Mutex<HashMap<...>>>` directly keeps the listener's API narrow:
/// it can only check membership, not mutate.
#[derive(Clone)]
pub(crate) struct OpenPathsHandle {
    versions: Arc<Mutex<HashMap<Utf8PathBuf, i32>>>,
}

impl OpenPathsHandle {
    /// Returns true if `path` is currently registered as open in the LSP
    /// document table. Locks the underlying mutex briefly; the lock is
    /// released before the caller awaits, so no deadlock risk against the
    /// LSP's `std::sync::Mutex`.
    fn contains(&self, path: &Utf8PathBuf) -> bool {
        let guard = self.versions.lock().expect("versions lock poisoned");
        guard.contains_key(path)
    }
}

/// BT-2239: consume `ClassChangedEvent`s from the runtime listener and
/// bump the shared nav-cache generation.
///
/// The foundation issue uses a coarse single-counter invalidation:
/// any class load / reload / method install bumps the generation, and
/// readers compare entries against the current counter. The per-method
/// children (BT-2240..2244) can keep the same shape if they add their
/// own per-class buckets — the listener stays the same.
///
/// Holds an `Arc` to `Backend::nav_cache` rather than a back-reference to
/// the `Backend` so the task does not keep the backend alive on its own.
/// When `ensure_runtime_attached` stores the `JoinHandle`, the task ends
/// when the handle is aborted (during a subsequent attach or backend
/// drop) or when the `class_changed_rx` channel closes (`RuntimeClient`
/// disconnect).
async fn class_changed_listener(
    nav_cache: Arc<Mutex<NavCache>>,
    mut class_changed_rx: tokio::sync::mpsc::UnboundedReceiver<ClassChangedEvent>,
) {
    while let Some(event) = class_changed_rx.recv().await {
        tracing::debug!(
            class_name = %event.class_name,
            "class_changed_listener: invalidating nav cache"
        );
        let mut guard = nav_cache.lock().expect("nav_cache lock poisoned");
        guard.invalidate();
    }
    tracing::debug!("class_changed_listener: channel closed, exiting");
}

/// ADR 0105 Phase 1 (BT-2779): consume `ReloadCheckEvent`s from the runtime
/// listener and publish/clear reload-induced diagnostics on the affected
/// caller classes' documents.
///
/// For every owner in the event's `checked_owners` (the clearing-by-
/// replacement set — see [`ReloadCheckEvent`]'s doc), this:
/// 1. Resolves the owner class name to a document URI via `nav-symbols`
///    (one round-trip per event, not per owner — `nav-symbols` already
///    returns every user class).
/// 2. Builds LSP diagnostics from the event's findings restricted to that
///    owner (`reload_finding_to_lsp_diagnostics`), one per call site so a
///    finding with several sends in the same method surfaces at each line.
/// 3. Replaces (never merges) that `(owner, changed_class)` origin's entry
///    within that URI's bucket in `reload_diagnostics` — an origin with no
///    current findings gets its entry removed, which is exactly how a
///    clean re-check clears a stale diagnostic, without touching a
///    *different* class's entry that happens to share the same file, NOR a
///    *different changed class*'s still-valid findings for the *same*
///    owner (`ReloadDiagnosticsByUriAndOrigin`) — a caller broken by two
///    independently-reloading classes must not have one reload's
///    replacement silently discard the other's still-valid finding.
/// 4. Republishes the merged (static + every origin's reload) diagnostic
///    set for that URI.
///
/// An owner with no resolvable source file (a REPL-only / dynamically
/// defined class, or one `nav-symbols` doesn't know about) is skipped — the
/// LSP has nothing to attach a `publishDiagnostics` notification to.
/// Silent: this is a normal, expected case (surface-parity is preserved by
/// the REPL notice and workspace UI, which don't need a `.bt` file to
/// attribute a finding to).
async fn reload_check_listener(
    client: Client,
    runtime: RuntimeClient,
    workspace_roots: Vec<PathBuf>,
    service: Arc<Mutex<SimpleLanguageService>>,
    reload_diagnostics: Arc<std::sync::Mutex<ReloadDiagnosticsByUriAndOrigin>>,
    mut reload_check_rx: tokio::sync::mpsc::UnboundedReceiver<ReloadCheckEvent>,
) {
    while let Some(event) = reload_check_rx.recv().await {
        if event.checked_owners.is_empty() {
            continue;
        }
        // Echo the summary line every other surface shows (REPL notice,
        // workspace UI header — "N callers re-checked, M stale") to the LSP
        // client's output channel. Squiggles alone don't carry the
        // clean-recheck count, and `window/logMessage` is the LSP's own
        // best-effort notice channel (not a `publishDiagnostics` — this
        // never affects the diagnostic set).
        let cap_suffix = event
            .cap_note
            .as_deref()
            .map(|n| format!(" ({n})"))
            .unwrap_or_default();
        client
            .log_message(
                MessageType::INFO,
                format!(
                    "reload check: {}>>{} {}; {} checked, {} not checked{cap_suffix}",
                    event.changed_class,
                    event.changed_selector,
                    event.classification,
                    event.checked,
                    event.not_checked
                ),
            )
            .await;
        let classes = match runtime.nav_symbols(Some("user")).await {
            Ok(classes) => classes,
            Err(e) => {
                tracing::warn!(error = %e, "reload_check_listener: nav_symbols failed");
                continue;
            }
        };
        for owner in &event.checked_owners {
            let Some(class) = classes.iter().find(|c| c.name.as_str() == owner.as_str()) else {
                tracing::debug!(
                    owner,
                    "reload_check_listener: owner has no known source file, skipping"
                );
                continue;
            };
            let Some(uri) = resolve_class_uri(class, &workspace_roots) else {
                tracing::debug!(owner, "reload_check_listener: could not resolve class URI");
                continue;
            };
            let diagnostics: Vec<tower_lsp::lsp_types::Diagnostic> = event
                .findings
                .iter()
                .filter(|f| &f.owner == owner)
                .flat_map(reload_finding_to_lsp_diagnostics)
                .collect();
            {
                let mut guard = reload_diagnostics
                    .lock()
                    .expect("reload_diagnostics lock poisoned");
                // `owner == changed_class` means this owner's OWN source
                // just changed — the server unconditionally full-wipes it
                // (`beamtalk_workspace_findings_store:clear_owner/1`) before
                // any scoped replace, so every origin bucket for this owner
                // is stale, not just the one keyed to `changed_class`. Every
                // *other* owner only had its `(owner, changed_class)` origin
                // scoped-replaced server-side (`put_owner_origin/3`), so a
                // different changed class's still-valid finding for the
                // same owner must survive.
                if owner == &event.changed_class {
                    if let Some(by_origin) = guard.get_mut(&uri) {
                        by_origin.retain(|(o, _cc), _| o != owner);
                        if by_origin.is_empty() {
                            guard.remove(&uri);
                        }
                    }
                    if !diagnostics.is_empty() {
                        guard
                            .entry(uri.clone())
                            .or_default()
                            .insert((owner.clone(), event.changed_class.clone()), diagnostics);
                    }
                } else {
                    let origin_key = (owner.clone(), event.changed_class.clone());
                    if diagnostics.is_empty() {
                        // An empty `diagnostics` list is ambiguous: it means
                        // either a genuinely clean re-check (no finding at
                        // all for this owner — safe to clear), or a finding
                        // that exists but is siteless (no xref call-site
                        // line to anchor a `Diagnostic` to). Only the first
                        // case should clear the origin; conflating the two
                        // would silently drop a real finding whenever it
                        // happens to have no placeable site.
                        let has_finding_for_owner =
                            event.findings.iter().any(|f| &f.owner == owner);
                        if has_finding_for_owner {
                            tracing::warn!(
                                owner,
                                changed_class = %event.changed_class,
                                "reload_check_listener: finding present but produced no \
                                 placeable diagnostics (siteless); leaving prior diagnostics \
                                 for this origin untouched"
                            );
                        } else if let Some(by_origin) = guard.get_mut(&uri) {
                            by_origin.remove(&origin_key);
                            if by_origin.is_empty() {
                                guard.remove(&uri);
                            }
                        }
                    } else {
                        guard
                            .entry(uri.clone())
                            .or_default()
                            .insert(origin_key, diagnostics);
                    }
                }
            }
            // No tracked document version for a URI the editor may not even
            // have open — `publishDiagnostics`' `version` field is optional
            // per the LSP spec, so omitting it is correct here (unlike the
            // flush listener, which only touches already-open buffers).
            publish_diagnostics_impl(&client, &service, &reload_diagnostics, &uri, None).await;
        }
    }
    tracing::debug!("reload_check_listener: channel closed, exiting");
}

/// Seed `reload_diagnostics` with any reload-induced findings that already
/// existed in `beamtalk_workspace_findings_store` before this client
/// attached (BT-2801, ADR 0105 surface-parity gap) — the `reload_check`
/// push channel [`reload_check_listener`] consumes only ever delivers *new*
/// outcomes, so a fresh LSP process attaching for the first time (e.g. the
/// editor just started, or restarted the language server after a crash)
/// would otherwise show nothing for a caller until the next reload happens
/// to touch it again.
///
/// Called once from [`Backend::ensure_runtime_attached`], after the push
/// listener is spawned but before the runtime client is handed back to
/// callers, so the first [`Backend::publish_diagnostics`] for any
/// already-open document picks the snapshot up through the normal merge
/// path (`publish_diagnostics_impl`). Findings are grouped by `(owner,
/// changed_class)` — the same origin key `reload_diagnostics` uses — so each
/// independent contribution to an owner's diagnostics stays an
/// independently-clearing entry once live pushes start arriving, exactly
/// mirroring [`reload_check_listener`]'s per-origin bucketing.
///
/// A document already open when the seed completes would otherwise have to
/// wait for an unrelated edit to surface its pre-existing findings, so any
/// URI touched by the seed is republished immediately.
///
/// Best-effort: a transport failure here only means the LSP starts cold —
/// exactly the behaviour before this feature existed — so it must not fail
/// the attach itself, only log and return.
///
/// **Additive-only, and only correct because it runs at most once per
/// `Backend`:** this never *clears* `reload_diagnostics`, it only inserts.
/// That is sound today because `ensure_runtime_attached` caches `self.runtime`
/// forever once set (nothing ever resets it back to `None`), so this
/// function's single call site only ever runs against an empty
/// `reload_diagnostics` map — there is no live LSP *process* reconnect path
/// today, only a fresh process attaching once. If a same-process reconnect
/// path is ever added, this must change to clear stale entries for origins
/// no longer in the fresh snapshot (not a blanket clear — a concurrent
/// `reload_check` push landing first, see the race note below, must not be
/// wiped) rather than staying purely additive, or a finding cleared while
/// disconnected could remain stuck forever. Two concurrent
/// `ensure_runtime_attached` callers *can* both pass the attach-cache's
/// `None` check and both reach this function (the existing "loser client"
/// race), but that is harmless here: both compute the same snapshot and the
/// inserts are idempotent over the same keys.
///
/// **Known narrow race, accepted:** the `reload-findings` RPC and the
/// `reload_check` push listener are two independent round-trips against the
/// same live store, so a real reload that clears an origin can have its
/// `ReloadCheckCompleted` push processed by [`reload_check_listener`]
/// *before* this function's own (slightly earlier) snapshot finishes being
/// written — in which case this seed re-inserts the origin the push had
/// already correctly cleared. This mirrors the "loser client" race
/// [`Backend::ensure_runtime_attached`] already documents and accepts for
/// the same reason: it needs a reload to land in the exact window between
/// attach and seed completion. It is not as fully self-healing as it may
/// first look: a re-inserted *clearing* finding only disappears the next
/// time `changed_class` (not just any reload touching `owner`) is reloaded
/// again — which may not happen again in the session — so the practical
/// effect is a stale squiggle that behaves exactly like the pre-BT-2801
/// baseline (nothing seeded) for that one origin, not a regression beyond
/// it. A fully race-free version would need the findings store to expose a
/// generation/version the client could compare against, which is out of
/// scope here.
async fn seed_reload_diagnostics(
    client: &Client,
    runtime_client: &RuntimeClient,
    workspace_roots: &[PathBuf],
    service: &Mutex<SimpleLanguageService>,
    reload_diagnostics: &std::sync::Mutex<ReloadDiagnosticsByUriAndOrigin>,
) {
    // Awaited inline in the attach path (not `tokio::spawn`ed) so that by the
    // time `ensure_runtime_attached` returns, any already-open document has
    // already been republished with the seeded findings — a spawned version
    // would race the caller's own next `publish_diagnostics` call for no
    // real benefit, since first-attach latency here is bounded by two RPC
    // round-trips (`reload-findings` + `nav-symbols`) against a workspace
    // already proven reachable by the connect this immediately follows.
    let findings = match runtime_client.reload_findings().await {
        Ok(findings) => findings,
        Err(e) => {
            tracing::warn!(error = %e, "seed_reload_diagnostics: reload-findings failed");
            return;
        }
    };
    if findings.is_empty() {
        return;
    }
    let classes = match runtime_client.nav_symbols(Some("user")).await {
        Ok(classes) => classes,
        Err(e) => {
            tracing::warn!(error = %e, "seed_reload_diagnostics: nav_symbols failed");
            return;
        }
    };
    let by_origin = group_findings_by_origin(findings);
    let mut touched_uris: HashSet<Url> = HashSet::new();
    for ((owner, changed_class), owner_findings) in by_origin {
        let Some(class) = classes.iter().find(|c| c.name.as_str() == owner.as_str()) else {
            tracing::debug!(
                owner,
                "seed_reload_diagnostics: owner has no known source file, skipping"
            );
            continue;
        };
        let Some(uri) = resolve_class_uri(class, workspace_roots) else {
            tracing::debug!(
                owner,
                "seed_reload_diagnostics: could not resolve class URI"
            );
            continue;
        };
        let diagnostics: Vec<tower_lsp::lsp_types::Diagnostic> = owner_findings
            .iter()
            .flat_map(reload_finding_to_lsp_diagnostics)
            .collect();
        if diagnostics.is_empty() {
            continue;
        }
        {
            let mut guard = reload_diagnostics
                .lock()
                .expect("reload_diagnostics lock poisoned");
            guard
                .entry(uri.clone())
                .or_default()
                .insert((owner, changed_class), diagnostics);
        }
        touched_uris.insert(uri);
    }
    for uri in touched_uris {
        publish_diagnostics_impl(client, service, reload_diagnostics, &uri, None).await;
    }
}

/// Group a flat findings snapshot by `(owner, changed_class)` — the same
/// origin key [`ReloadDiagnosticsByUriAndOrigin`] uses — so
/// [`seed_reload_diagnostics`] can seed each independent contribution to an
/// owner's diagnostics as its own independently-clearing entry, exactly
/// mirroring [`reload_check_listener`]'s per-origin bucketing. Pulled out as
/// a pure function (no I/O) so this grouping — the one piece of genuinely
/// new logic `seed_reload_diagnostics` adds beyond what
/// [`reload_check_listener`] already does per-event — is unit-testable
/// without a live `RuntimeClient`.
fn group_findings_by_origin(
    findings: Vec<ReloadFinding>,
) -> HashMap<(String, String), Vec<ReloadFinding>> {
    let mut by_origin: HashMap<(String, String), Vec<ReloadFinding>> = HashMap::new();
    for finding in findings {
        by_origin
            .entry((finding.owner.clone(), finding.changed_class.clone()))
            .or_default()
            .push(finding);
    }
    by_origin
}

/// Resolve a `NavSymbolClass`'s `source_file` to a document `Url`, the same
/// way [`runtime_class_to_document_symbol`] does — reusing
/// `nav_site_to_location` for the workspace-root canonicalisation via a
/// synthetic single-site `NavSite`.
fn resolve_class_uri(class: &NavSymbolClass, workspace_roots: &[PathBuf]) -> Option<Url> {
    let source_file = class.source_file.as_deref()?;
    if source_file.is_empty() {
        return None;
    }
    let resolved = nav_site_to_location(
        &NavSite {
            class: class.name.clone(),
            class_side: false,
            method: class.name.clone(),
            line: class.line.unwrap_or(1),
            source_file: Some(source_file.to_string()),
        },
        workspace_roots,
    )?;
    path_to_uri(&resolved.file)
}

/// Build one LSP `Diagnostic` per call site in a reload-induced finding
/// (ADR 0105 Phase 1, BT-2779). Uses the site's xref-recorded line number —
/// not the finding's `start`/`end` byte-offset span, since those are
/// offsets into the *live combined class source* the compiler re-checked
/// against, and there is no existing machinery to map that back onto an
/// on-disk position the way `nav_site_to_location`/`line_to_position`
/// already do for a line number (see `ReloadFinding::start`'s doc).
fn reload_finding_to_lsp_diagnostics(
    finding: &ReloadFinding,
) -> Vec<tower_lsp::lsp_types::Diagnostic> {
    let severity = match finding.severity.as_str() {
        "error" => DiagnosticSeverity::ERROR,
        "warning" => DiagnosticSeverity::WARNING,
        _ => DiagnosticSeverity::HINT,
    };
    // `reload check (<classification> of <ChangedClass>>><selector>): …`
    // attributes the finding the same way the REPL notice / ADR demo do
    // (`format_reload_check_notice` in `beamtalk-cli`), so a squiggle's
    // hover text answers "why is this here" without cross-referencing
    // another surface.
    let mut message = format!(
        "reload check ({} of {}>>{}): {}",
        finding.classification, finding.changed_class, finding.selector, finding.message
    );
    if let Some(note) = &finding.note {
        message.push_str("\n  = ");
        message.push_str(note);
    }
    let code = finding
        .category
        .clone()
        .map(tower_lsp::lsp_types::NumberOrString::String);
    finding
        .sites
        .iter()
        .map(|site| {
            let row = site.line.saturating_sub(1);
            // Highlight the whole line: `character: u32::MAX` is the LSP
            // convention for "end of line" (the flush listener's
            // `workspace/applyEdit` uses the same trick for "end of file")
            // — clients clamp to the line's actual length. Byte-precise
            // spans aren't available here (see the doc comment above).
            let range = Range {
                start: Position::new(row, 0),
                end: Position::new(row, u32::MAX),
            };
            tower_lsp::lsp_types::Diagnostic {
                range,
                severity: Some(severity),
                code: code.clone(),
                source: Some("beamtalk (reload)".into()),
                message: format!("{message} (in {})", site.method),
                ..Default::default()
            }
        })
        .collect()
}

/// ADR 0082 Phase 3 (BT-2289): consume `FlushEvent`s from the runtime
/// listener and emit `workspace/applyEdit` per flushed file.
///
/// For each file in the event, the listener:
/// 1. Resolves the runtime-reported path against the LSP workspace roots to
///    canonicalise into an absolute filesystem path.
/// 2. Checks the *live* open-paths handle to see whether the path is
///    currently open in the editor. Files that aren't open are skipped —
///    `VSCode` reads them fresh on next `did_open`. The check happens per
///    event so files opened after the listener started are still picked up.
/// 3. Reads the new on-disk content and issues `apply_edit` with a single
///    `TextEdit` covering the whole document, so the open buffer realigns
///    with the post-flush bytes. `VSCode`'s conflict UX handles unsaved local
///    edits per the LSP spec.
async fn flush_event_listener(
    client: Client,
    workspace_roots: Vec<PathBuf>,
    open_paths: OpenPathsHandle,
    mut flush_rx: tokio::sync::mpsc::UnboundedReceiver<FlushEvent>,
) {
    while let Some(event) = flush_rx.recv().await {
        for raw_path in event.files {
            let canonical = resolve_flushed_path(&raw_path, &workspace_roots);
            let Some(abs_path) = canonical else {
                tracing::debug!(
                    raw_path,
                    "flush_event_listener: could not resolve runtime path against workspace roots"
                );
                continue;
            };
            let Ok(utf8_path) = Utf8PathBuf::try_from(abs_path.clone()) else {
                tracing::debug!(raw_path, "flush_event_listener: resolved path is not UTF-8");
                continue;
            };
            if !open_paths.contains(&utf8_path) {
                tracing::debug!(
                    %utf8_path,
                    "flush_event_listener: skipping closed file"
                );
                continue;
            }
            let Ok(content) = tokio::fs::read_to_string(&abs_path).await else {
                tracing::warn!(
                    %utf8_path,
                    "flush_event_listener: failed to read flushed file from disk"
                );
                continue;
            };
            let Ok(uri) = Url::from_file_path(&abs_path) else {
                tracing::debug!(
                    %utf8_path,
                    "flush_event_listener: could not build file:// URI"
                );
                continue;
            };
            let edit = WorkspaceEdit {
                changes: Some({
                    let mut changes = HashMap::new();
                    changes.insert(
                        uri.clone(),
                        vec![TextEdit {
                            range: Range {
                                start: Position {
                                    line: 0,
                                    character: 0,
                                },
                                // u32::MAX/u32::MAX is the LSP convention for "end
                                // of file" — any line longer than this is unrealistic
                                // for source code and clients clamp to actual EOF.
                                end: Position {
                                    line: u32::MAX,
                                    character: u32::MAX,
                                },
                            },
                            new_text: content,
                        }],
                    );
                    changes
                }),
                ..Default::default()
            };
            match client.apply_edit(edit).await {
                Ok(resp) if resp.applied => {
                    tracing::debug!(%uri, "flush_event_listener: applied edit");
                }
                Ok(resp) => {
                    tracing::info!(
                        %uri,
                        failure_reason = ?resp.failure_reason,
                        "flush_event_listener: client declined applyEdit"
                    );
                }
                Err(e) => {
                    tracing::warn!(%uri, error = %e, "flush_event_listener: applyEdit failed");
                }
            }
        }
    }
}

/// Resolve a path reported by the runtime against the LSP workspace roots.
///
/// The runtime stores `ChangeEntry.sourceFile` as whatever was passed at
/// `compile:source:` hook time — typically a workspace-relative path
/// (`"src/counter.bt"`) when the workspace was started in the project root.
/// We try, in order:
///
/// 1. Absolute → use as-is if it exists.
/// 2. For each workspace root, join + canonicalise.
///
/// Returns the canonicalised absolute path on success, or `None` if we can't
/// find a real file matching `raw`. Canonicalisation matters because the LSP
/// stores open documents under canonical paths (`uri_to_path` runs
/// `canonicalize`); a non-canonical lookup would always miss.
pub(crate) fn resolve_flushed_path(raw: &str, roots: &[PathBuf]) -> Option<PathBuf> {
    let candidate = PathBuf::from(raw);
    if candidate.is_absolute() {
        return candidate.canonicalize().ok();
    }
    for root in roots {
        let joined = root.join(&candidate);
        if let Ok(canon) = joined.canonicalize() {
            return Some(canon);
        }
    }
    None
}

/// ADR 0082 Phase 3 (BT-2289): map a `workspace/executeCommand` invocation
/// to the Beamtalk expression that compiles to the same effect on the live
/// workspace. Mirrors `beamtalk-mcp`'s expression builders one-for-one so
/// surface parity is preserved (REPL `:flush` ≡ MCP `flush` ≡ LSP
/// `beamtalk.flush`).
///
/// Returns the expression string on success or a human-readable parameter
/// error on failure (which the LSP layer surfaces as `invalid_params`).
pub(crate) fn build_command_expression(
    command: &str,
    arguments: &[serde_json::Value],
) -> std::result::Result<String, String> {
    match command {
        CMD_FLUSH => {
            // No arguments — `Workspace flush` on the whole pending set.
            if !arguments.is_empty() {
                return Err(format!(
                    "{CMD_FLUSH}: expected no arguments, got {}",
                    arguments.len()
                ));
            }
            Ok("Workspace flush".to_string())
        }
        CMD_FLUSH_CLASS => {
            let class = expect_string_arg(arguments, 0, "class")?;
            validate_class_name(&class)?;
            // `Workspace flush: ClassName` — the class is named literally in
            // the expression (no escaping); validation above prevents any
            // shape that could parse differently.
            Ok(format!("Workspace flush: {class}"))
        }
        CMD_FLUSH_FILE => {
            let file = expect_string_arg(arguments, 0, "file")?;
            // `Workspace flush: #{ #file => "path" }` — Symbol-keyed
            // dictionary; the path is passed as a String value.
            Ok(format!(
                "Workspace flush: #{{ #file => \"{}\" }}",
                escape_string_literal(&file)
            ))
        }
        CMD_FLUSH_KIND => {
            let kind = expect_string_arg(arguments, 0, "kind")?;
            // Allow either bare `new-class` or `#'new-class'`. Emit the
            // quoted-symbol form so hyphenated kinds parse cleanly.
            let bare = kind.strip_prefix('#').unwrap_or(&kind);
            let bare = bare.trim_matches('\'');
            if bare.is_empty()
                || !bare
                    .chars()
                    .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
            {
                return Err(format!(
                    "{CMD_FLUSH_KIND}: 'kind' must be an identifier (letters, digits, '-' or '_'); got '{kind}'"
                ));
            }
            Ok(format!("Workspace flush: #'{bare}'"))
        }
        CMD_SAVE_CLASS => {
            let source = expect_string_arg(arguments, 0, "source")?;
            let path = expect_string_arg(arguments, 1, "path")?;
            if source.is_empty() {
                return Err(format!("{CMD_SAVE_CLASS}: 'source' must not be empty"));
            }
            if path.is_empty() {
                return Err(format!("{CMD_SAVE_CLASS}: 'path' must not be empty"));
            }
            // Mirrors `beamtalk-mcp::save_class_expr`: pass body + path as
            // String values; the workspace primitive takes them as values
            // rather than re-parsed Beamtalk source.
            Ok(format!(
                "Workspace newClass: \"{}\" at: \"{}\"",
                escape_string_literal(&source),
                escape_string_literal(&path)
            ))
        }
        CMD_PRECHECK_METHOD => {
            let class = expect_string_arg(arguments, 0, "class")?;
            validate_class_name(&class)?;
            let selector = expect_string_arg(arguments, 1, "selector")?;
            // Accepted with or without a leading '#', mirroring MCP's
            // `precheck_method` tool so both surfaces agree on input shape.
            let selector = selector.strip_prefix('#').unwrap_or(&selector);
            validate_selector(selector)?;
            let source = expect_string_arg(arguments, 2, "source")?;
            if source.is_empty() {
                return Err(format!("{CMD_PRECHECK_METHOD}: 'source' must not be empty"));
            }
            // Mirrors `CMD_SAVE_CLASS`'s / `save_method_expr`'s shape:
            // `ClassName precheckCompile: #selector source: "body"`.
            Ok(format!(
                "{class} precheckCompile: #{selector} source: \"{}\"",
                escape_string_literal(&source)
            ))
        }
        CMD_RECHECK_IMAGE => {
            if !arguments.is_empty() {
                return Err(format!(
                    "{CMD_RECHECK_IMAGE}: expected no arguments, got {}",
                    arguments.len()
                ));
            }
            Ok("Workspace recheckImage".to_string())
        }
        _ => Err(format!("unknown LSP command: {command}")),
    }
}

/// Extract the i-th argument from an `executeCommand` invocation as a string.
/// LSP clients pack arguments as a `Vec<Value>`; we accept either a bare
/// JSON string or an object whose `name` field is the value the parameter
/// expects, to be friendly to both raw JSON-RPC callers and editors that wrap
/// arguments in objects.
fn expect_string_arg(
    arguments: &[serde_json::Value],
    index: usize,
    name: &str,
) -> std::result::Result<String, String> {
    let value = arguments
        .get(index)
        .ok_or_else(|| format!("missing argument {index} ({name})"))?;
    match value {
        serde_json::Value::String(s) => Ok(s.clone()),
        serde_json::Value::Object(map) => {
            if let Some(serde_json::Value::String(s)) = map.get(name) {
                Ok(s.clone())
            } else {
                Err(format!(
                    "argument {index} must be a string or an object with a '{name}' string field"
                ))
            }
        }
        _ => Err(format!(
            "argument {index} ({name}) must be a string, got {value}"
        )),
    }
}

/// Validate that a class name argument is a Beamtalk identifier
/// (`PascalCase`, ASCII-only, underscores OK). Prevents an injection-shaped
/// argument like `"X. delete Workspace; X"` from being concatenated into the
/// flush expression. Tight on purpose — Beamtalk class names are a closed
/// alphabet.
fn validate_class_name(name: &str) -> std::result::Result<(), String> {
    if beamtalk_core::source_analysis::is_valid_class_name(name) {
        return Ok(());
    }
    if name.is_empty() {
        return Err("class name must not be empty".to_string());
    }
    if !name.starts_with(|c: char| c.is_ascii_uppercase()) {
        return Err(format!(
            "class name '{name}' must start with an uppercase letter"
        ));
    }
    // is_valid_class_name returned false, name is non-empty with uppercase start,
    // so there must be at least one disallowed character.
    let c = name
        .chars()
        .find(|c| !(c.is_ascii_alphanumeric() || *c == '_'))
        .expect(
            "invariant: is_valid_class_name=false, non-empty, starts-uppercase → must have bad char",
        );
    Err(format!(
        "class name '{name}' contains invalid character '{c}' (allowed: letters, digits, underscore)"
    ))
}

/// Validate a Beamtalk selector (unary, keyword, or binary) before splicing
/// it unescaped into a `#selector` literal in a built expression (ADR 0105
/// Phase 3, BT-2782's `CMD_PRECHECK_METHOD`) — mirrors `beamtalk-mcp`'s
/// `validate_selector` (there is no shared crate for this; both surfaces
/// need the same shape check before string-building an expression).
fn validate_selector(sel: &str) -> std::result::Result<(), String> {
    fn is_binary_selector_char(c: char) -> bool {
        matches!(
            c,
            '+' | '-' | '*' | '/' | '<' | '>' | '=' | '~' | '%' | '&' | '?' | ',' | '\\'
        )
    }

    if sel.is_empty() {
        return Err("selector must not be empty".to_string());
    }

    let first = sel.chars().next().expect("checked non-empty above");
    if is_binary_selector_char(first) {
        return if sel.chars().all(is_binary_selector_char) {
            Ok(())
        } else {
            Err(format!("invalid binary selector: '{sel}'"))
        };
    }

    if sel
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == ':')
    {
        return Ok(());
    }

    Err(format!("invalid selector: '{sel}'"))
}

fn workspace_roots(params: &InitializeParams) -> Vec<PathBuf> {
    let mut roots = Vec::new();

    if let Some(workspace_folders) = &params.workspace_folders {
        for folder in workspace_folders {
            if let Ok(path) = folder.uri.to_file_path() {
                roots.push(path);
            }
        }
    }

    if let Some(root_uri) = &params.root_uri {
        if let Ok(path) = root_uri.to_file_path() {
            roots.push(path);
        }
    }

    roots = roots
        .into_iter()
        .map(|root| beamtalk_core::project::discover_project_root(&root))
        .collect();

    roots.sort_unstable();
    roots.dedup();
    roots
}

fn configured_stdlib_source_dir(params: &InitializeParams) -> Option<String> {
    params
        .initialization_options
        .as_ref()
        .and_then(|value| value.get("stdlibSourceDir"))
        .and_then(|value| value.as_str())
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .map(ToString::to_string)
}

/// BT-2239: Read the `delegateToRuntime` flag from `initializationOptions`.
///
/// Defaults to `false`: foundation issue keeps all navigation on the
/// AST walker so behaviour is byte-for-byte identical to today. The
/// per-method children (BT-2240..2244) flip individual queries over and
/// rely on the editor / user enabling this flag once the runtime path is
/// stable.
fn configured_delegate_to_runtime(params: &InitializeParams) -> bool {
    params
        .initialization_options
        .as_ref()
        .and_then(|value| value.get("delegateToRuntime"))
        .and_then(serde_json::Value::as_bool)
        .unwrap_or(false)
}

/// Returns the stdlib source directory auto-discovered from the LSP binary's sysroot.
///
/// Derives sysroot as `parent(parent(current_exe()))` — the same convention used
/// by `beamtalk --print-sysroot` — then looks for `share/beamtalk/stdlib/src/`
/// under that prefix.
fn sysroot_stdlib_source_dir() -> Option<PathBuf> {
    let exe = std::env::current_exe().ok()?;
    let sysroot = exe.parent()?.parent()?;
    let candidate = sysroot.join("share/beamtalk/stdlib/src");
    canonicalize_existing_dir(&candidate)
}

fn configured_stdlib_source_dirs(
    configured: Option<&str>,
    project_roots: &[PathBuf],
) -> Vec<PathBuf> {
    let Some(configured) = configured else {
        // No explicit config — fall back to sysroot auto-discovery.
        return sysroot_stdlib_source_dir().into_iter().collect();
    };

    let configured_path = PathBuf::from(configured);
    let canonical_roots: Vec<PathBuf> = project_roots
        .iter()
        .filter_map(|root| canonicalize_existing_dir(root))
        .collect();
    let mut dirs = Vec::new();

    if configured_path.is_absolute() {
        if let Some(candidate) = canonicalize_existing_dir(&configured_path) {
            dirs.push(candidate);
        }
    } else {
        for root in project_roots {
            let candidate = root.join(&configured_path);
            if let Some(canonical_candidate) = canonicalize_existing_dir(&candidate)
                && path_within_any_root(&canonical_candidate, &canonical_roots)
            {
                dirs.push(canonical_candidate);
            }
        }
    }

    dirs.sort_unstable();
    dirs.dedup();
    dirs
}

fn canonicalize_existing_dir(path: &Path) -> Option<PathBuf> {
    if !path.is_dir() {
        return None;
    }
    fs::canonicalize(path).ok()
}

fn path_within_any_root(path: &Path, roots: &[PathBuf]) -> bool {
    roots.iter().any(|root| path.starts_with(root))
}

/// Enumerate `_build/deps/<name>/src/` directories for every fetched
/// dependency under `root`.
///
/// Filesystem-driven rather than manifest-driven: any directory under
/// `_build/deps/` with a `src/` subdirectory is treated as a dep. This avoids
/// adding a `beamtalk-cli` dependency on the LSP just to parse `beamtalk.toml`,
/// and matches the layout the build system writes.
fn dependency_src_dirs(root: &Path) -> Vec<PathBuf> {
    let deps_dir = root.join("_build").join("deps");
    let Ok(entries) = fs::read_dir(&deps_dir) else {
        return Vec::new();
    };
    let mut out = Vec::new();
    for entry in entries.flatten() {
        let dep_path = entry.path();
        if !dep_path.is_dir() {
            continue;
        }
        let src = dep_path.join("src");
        if src.is_dir() {
            out.push(src);
        }
    }
    out.sort_unstable();
    out
}

fn collect_preload_files(config: PreloadConfig) -> PreloadedFiles {
    use beamtalk_core::file_walker::FileWalker;

    let PreloadConfig { roots, stdlib_dirs } = config;
    let mut user_paths = Vec::new();
    let mut remaining_budget = PRELOAD_MAX_FILES;

    let preload_walker = FileWalker::preload_files(remaining_budget);

    // BT-2027: Preload both `src/` and `test/` so that opening a file in
    // `test/` immediately sees classes defined in `src/` (and vice versa).
    // Before this, the LSP would report spurious `Unresolved class` for every
    // test-to-src reference until the user touched the src file manually.
    //
    // BT-2137: Also preload `_build/deps/<name>/src/` for each fetched
    // dependency so references to classes from declared `beamtalk.toml`
    // dependencies (e.g. `HTTPClient` from the `http` package) resolve
    // without spurious `Unresolved class` warnings. Dep dirs are walked
    // *after* every workspace root's `src/`/`test/` so that in a multi-root
    // workspace one root's deps cannot exhaust the shared preload budget
    // before later roots' user files are considered.
    for root in &roots {
        for subdir in ["src", "test"] {
            if remaining_budget == 0 {
                break;
            }
            let dir = root.join(subdir);
            if dir.is_dir() {
                if let Ok(found) = preload_walker
                    .clone()
                    .max_files(remaining_budget)
                    .walk_pathbuf(&dir)
                {
                    remaining_budget = remaining_budget.saturating_sub(found.len());
                    user_paths.extend(found);
                }
            }
        }
    }

    let mut deps_present = false;
    for root in &roots {
        for dep_src in dependency_src_dirs(root) {
            deps_present = true;
            if remaining_budget == 0 {
                break;
            }
            if let Ok(found) = preload_walker
                .clone()
                .max_files(remaining_budget)
                .walk_pathbuf(&dep_src)
            {
                remaining_budget = remaining_budget.saturating_sub(found.len());
                user_paths.extend(found);
            }
        }
    }

    let mut stdlib_path_list = Vec::new();
    for dir in &stdlib_dirs {
        if remaining_budget == 0 {
            break;
        }
        if let Ok(found) = preload_walker
            .clone()
            .max_files(remaining_budget)
            .walk_pathbuf(dir)
        {
            remaining_budget = remaining_budget.saturating_sub(found.len());
            stdlib_path_list.extend(found);
        }
    }

    // Build the stdlib set first so user files that overlap with stdlib are
    // classified as stdlib (preserving the beamtalk-stdlib:// URI route).
    let stdlib_path_set: HashSet<PathBuf> = stdlib_path_list.iter().cloned().collect();

    let mut seen_user_files = HashSet::new();
    let mut user_files = Vec::new();
    for path in user_paths {
        // Skip paths that appear in stdlib: they must stay in the stdlib bucket.
        if stdlib_path_set.contains(&path) || !seen_user_files.insert(path.clone()) {
            continue;
        }
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        user_files.push((path, content));
    }

    let mut seen_stdlib_files = HashSet::new();
    let mut stdlib_files = Vec::new();
    for path in stdlib_path_list {
        if !seen_stdlib_files.insert(path.clone()) {
            continue;
        }
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        stdlib_files.push((path, content));
    }

    PreloadedFiles {
        user_files,
        stdlib_files,
        budget_exhausted: remaining_budget == 0,
        deps_present,
    }
}

/// Formats hover documentation for compact display in editor hovers.
///
/// VS Code controls hover font sizes, so we demote markdown heading levels
/// to reduce visual dominance of large section titles like `## Examples`.
fn format_hover_documentation(doc: &str) -> String {
    doc.lines()
        .map(|line| {
            if let Some(rest) = line.strip_prefix("### ") {
                return format!("##### {rest}");
            }
            if let Some(rest) = line.strip_prefix("## ") {
                return format!("#### {rest}");
            }
            if let Some(rest) = line.strip_prefix("# ") {
                return format!("### {rest}");
            }
            line.to_string()
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Adds an LSP-only stdlib profile note for class/method hovers.
///
/// This is intentionally editor policy (not compiler behavior): when hover
/// resolves to a built-in/stdlib class, show sealed/abstract traits so users
/// can better understand completion/diagnostic confidence.
fn stdlib_hover_policy_note(
    service: &SimpleLanguageService,
    hover_markdown: &str,
) -> Option<String> {
    let class_name = extract_hover_class_name(hover_markdown)?;
    if !ClassHierarchy::is_builtin_class(class_name) {
        return None;
    }

    let class = service.project_index().hierarchy().get_class(class_name)?;
    let mut traits = Vec::new();
    if class.is_sealed {
        traits.push("sealed");
    }
    if class.is_abstract {
        traits.push("abstract");
    }

    if traits.is_empty() {
        return None;
    }

    let mut lines = vec![format!(
        "**Stdlib Profile:** `{class_name}` is {}.",
        traits.join(" + ")
    )];
    if class.is_sealed {
        lines.push("- Method surface is closed to subclass overrides.".to_string());
    }
    if class.is_abstract {
        lines.push("- Defines protocol to be implemented by concrete subclasses.".to_string());
    }
    if class.is_sealed && hover_markdown.contains("Resolved on `") {
        lines.push("- Confidence: high (static, sealed stdlib dispatch).".to_string());
    }

    Some(lines.join("\n"))
}

/// Extract class name from hover markdown generated by beamtalk-core.
///
/// Supports:
/// - Class hovers: `Class: `Integer` ...`
/// - Resolved method hovers: `Resolved on `Integer` (defined in ... )`
fn extract_hover_class_name(hover_markdown: &str) -> Option<&str> {
    if let Some(name) = extract_backticked_after(hover_markdown, "Class: `") {
        return Some(name);
    }
    extract_backticked_after(hover_markdown, "Resolved on `")
}

/// Returns the first backticked segment after a marker prefix.
fn extract_backticked_after<'a>(text: &'a str, marker: &str) -> Option<&'a str> {
    let start = text.find(marker)? + marker.len();
    let rest = &text[start..];
    let end = rest.find('`')?;
    Some(&rest[..end])
}

// --- Type conversion helpers ---

/// Converts an LSP URI to a `Utf8PathBuf`.
fn uri_to_path(uri: &Url) -> Option<Utf8PathBuf> {
    match uri.scheme() {
        "file" => uri
            .to_file_path()
            .ok()
            .and_then(|p| Utf8PathBuf::try_from(p).ok()),
        "untitled" => {
            let name = uri.path().trim_start_matches('/');
            Some(Utf8PathBuf::from(format!("__untitled__/{name}")))
        }
        _ => None,
    }
}

/// Converts a `Utf8PathBuf` to an LSP URI.
fn path_to_uri(path: &Utf8PathBuf) -> Option<Url> {
    if let Some(name) = path.as_str().strip_prefix("__untitled__/") {
        Url::parse(&format!("untitled:{name}")).ok()
    } else {
        Url::from_file_path(path.as_str()).ok()
    }
}

/// Converts a stdlib file path to a `beamtalk-stdlib:///ClassName.bt` virtual URI.
fn path_to_stdlib_uri(path: &Utf8PathBuf) -> Option<Url> {
    let filename = path.file_name()?;
    Url::parse(&format!("beamtalk-stdlib:///{filename}")).ok()
}

/// Converts an LSP `Position` (UTF-16 code units) to a beamtalk `Position` (byte offsets).
///
/// LSP positions use UTF-16 code units for the character field.
/// Beamtalk positions use byte offsets within the line.
fn to_bt_position(pos: tower_lsp::lsp_types::Position, source: &str) -> BtPosition {
    let target_line = pos.line;
    let target_utf16_col = pos.character;

    let mut current_line = 0u32;
    let mut line_start = 0usize;

    // Find the start of the target line
    for (i, ch) in source.char_indices() {
        if current_line == target_line {
            break;
        }
        if ch == '\n' {
            current_line += 1;
            line_start = i + 1;
        }
    }

    // Walk the target line, counting UTF-16 code units until we reach the target column
    let mut utf16_col = 0u32;
    let mut byte_col = 0u32;
    for ch in source[line_start..].chars() {
        if ch == '\n' || utf16_col >= target_utf16_col {
            break;
        }
        // UTF-16 len is always 1 or 2, safe to truncate
        #[expect(
            clippy::cast_possible_truncation,
            reason = "char::len_utf16() is always 1 or 2"
        )]
        {
            utf16_col += ch.len_utf16() as u32;
        }
        // len_utf8 is always 1-4, safe to truncate
        #[expect(
            clippy::cast_possible_truncation,
            reason = "char::len_utf8() is always 1 to 4"
        )]
        {
            byte_col += ch.len_utf8() as u32;
        }
    }

    BtPosition::new(target_line, byte_col)
}

/// Converts a beamtalk `Span` to an LSP `Range` using source text.
fn span_to_range(span: Span, source: &str) -> Range {
    let start = offset_to_position(span.start() as usize, source);
    let end = offset_to_position(span.end() as usize, source);
    Range { start, end }
}

/// BT-2243: serializable shape of a [`CallHierarchyTarget`] stored on the
/// `data` field of a [`CallHierarchyItem`] so the incoming/outgoing
/// follow-up requests can re-construct enough context to dispatch.
///
/// The LSP spec is explicit that `data` is the channel for preserving
/// prepare-side state across the three-call flow (prepare → incoming /
/// outgoing): `data` is opaque to the editor, round-tripped verbatim.
/// We encode everything we need to dispatch incoming (selector) and
/// outgoing (selector + file + body range to walk) calls.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct SerializedCallTarget {
    /// Selector of the method this item refers to. Drives the senders-of
    /// dispatch for incoming calls and serves as the displayable name.
    selector: String,
    /// Enclosing class name when the prepare hit was on a method-definition
    /// header, `None` for a call-site hit. Outgoing-calls dispatch refuses
    /// to walk a body when this is `None` (no method context to anchor on).
    #[serde(default)]
    class_name: Option<String>,
    /// Whether the method is class-side. Mirrored to the runtime-attached
    /// senders query; the AST walker is class-agnostic.
    #[serde(default)]
    class_side: bool,
    /// File path the prepare hit landed in — used by outgoing-calls to
    /// re-locate the method body in the open document set.
    file: String,
    /// Start byte offset of the method-definition span in `file`. Combined
    /// with `range_end` to extract the body slice for the outgoing AST
    /// walk.
    range_start: u32,
    /// End byte offset of the method-definition span in `file`.
    range_end: u32,
}

impl SerializedCallTarget {
    /// Build from a [`CallHierarchyTarget`] produced by the prepare
    /// classifier.
    fn from_target(target: &CallHierarchyTarget) -> Self {
        Self {
            selector: target.selector.to_string(),
            class_name: target
                .class_name
                .as_ref()
                .map(std::string::ToString::to_string),
            class_side: target.class_side,
            file: target.file.as_str().to_string(),
            range_start: target.range.start(),
            range_end: target.range.end(),
        }
    }

    /// Decode from a [`CallHierarchyItem`]'s `data` field, returning `None`
    /// when the field is absent or malformed. The LSP spec does not
    /// guarantee `data` survives every editor round-trip (some clients
    /// scrub it), so callers should treat `None` as "the editor didn't
    /// preserve our context — answer with no calls" rather than as an
    /// error condition.
    fn from_item(item: &CallHierarchyItem) -> Option<Self> {
        let data = item.data.as_ref()?;
        serde_json::from_value(data.clone()).ok()
    }
}

/// Build a [`CallHierarchyItem`] from a prepare-side target, embedding
/// the [`SerializedCallTarget`] payload on `data` so the follow-up
/// incoming/outgoing requests can dispatch without re-classifying.
///
/// `kind` is always [`SymbolKind::METHOD`] — call hierarchy items are
/// methods by definition. `detail` includes the class name (with a
/// `class` suffix for class-side methods) when known, mirroring the
/// document-symbol convention.
fn target_to_lsp_item(
    target: &CallHierarchyTarget,
    uri: &Url,
    source: &str,
) -> Option<CallHierarchyItem> {
    let range = span_to_range(target.range, source);
    let selection_range = span_to_range(target.selection_range, source);
    let detail = target
        .class_name
        .as_ref()
        .map(|cn| format_class_detail(cn.as_str(), target.class_side));
    let data = serde_json::to_value(SerializedCallTarget::from_target(target)).ok()?;
    Some(CallHierarchyItem {
        name: target.selector.to_string(),
        kind: SymbolKind::METHOD,
        tags: None,
        detail,
        uri: uri.clone(),
        range,
        selection_range,
        data: Some(data),
    })
}

/// Format a class-name detail string for the `detail` field on a
/// `CallHierarchyItem`. Mirrors the workspace-symbol convention of
/// rendering class-side methods with a `class` suffix so the editor
/// shows `Counter` vs `Counter class` in the hover.
fn format_class_detail(class: &str, class_side: bool) -> String {
    if class_side {
        format!("{class} class")
    } else {
        class.to_string()
    }
}

/// BT-2243: walk a method body slice for outgoing calls.
///
/// Reads sends via [`find_all_sends_in_source`] (the same query that backs
/// the stdlib `SystemNavigation messagesSentBy:`), drops Erlang FFI sends,
/// and emits one [`CallHierarchyOutgoingCall`] per remaining send. Lines
/// are 1-based relative to `body_slice`; the LSP layer adds
/// `body_start_pos.line` (zero-based) so per-call ranges land at the right
/// absolute line in the file. Column is approximated to 0 because the
/// underlying query records only line granularity for selector tokens — a
/// precise column would require re-lexing the slice for each hit.
fn outgoing_calls_for_body(
    body_slice: &str,
    body_start_pos: tower_lsp::lsp_types::Position,
    uri: &Url,
) -> Vec<CallHierarchyOutgoingCall> {
    let hits = find_all_sends_in_source(body_slice);
    let mut calls = Vec::new();
    for hit in hits {
        if hit.receiver == ReceiverKind::ErlangFfi {
            continue;
        }
        // 1-based line within the body slice, offset by the body's
        // starting line within the file. `find_all_sends_in_source`
        // returns at least 1 for any real hit (the `.max(1)` in its
        // implementation), so subtracting 1 is safe.
        let abs_line = body_start_pos.line + hit.line.saturating_sub(1);
        let range = Range {
            start: Position::new(abs_line, 0),
            end: Position::new(abs_line, 0),
        };
        let to = CallHierarchyItem {
            name: hit.selector.clone(),
            kind: SymbolKind::METHOD,
            tags: None,
            detail: None,
            uri: uri.clone(),
            range,
            selection_range: range,
            data: None,
        };
        calls.push(CallHierarchyOutgoingCall {
            to,
            from_ranges: vec![range],
        });
    }
    calls
}

/// BT-2240: Compute the AST-known declaration sites that correspond to the
/// symbol the cursor is sitting on, in the same classification order
/// [`SimpleLanguageService::references_query_at`] uses.
///
/// * Cursor on a selector (call site or method header) →
///   [`SimpleLanguageService::find_selector_declarations`].
/// * Cursor on a class-name identifier known to the hierarchy →
///   [`SimpleLanguageService::find_class_declarations`].
/// * Cursor on a local identifier (parameter, local variable) → empty —
///   locals are scope-bound declarations, but the cold-file
///   `find_references` walker emits all matching identifier spans without
///   distinguishing the binding site, so we have nothing to subtract.
///
/// Caller uses the returned `(file, span)` pairs as a "decl set" to filter
/// out of the AST `find_references` result when the LSP
/// `context.includeDeclaration` flag is `false`.
fn ast_declarations_for_cursor(
    svc: &SimpleLanguageService,
    file: &Utf8PathBuf,
    position: BtPosition,
) -> Vec<BtLocation> {
    let Some(query) = svc.references_query_at(file, position) else {
        return Vec::new();
    };
    match query {
        NavQuery::SendersOf(selector) | NavQuery::ImplementorsOf(selector) => {
            svc.find_selector_declarations(selector.as_str())
        }
        NavQuery::ReferencesTo(class_name) => svc.find_class_declarations(class_name.as_str()),
    }
}

/// BT-2240: Translate a list of `beamtalk-core` `Location` values to LSP
/// `Location`s by re-reading each file's cached source for the column
/// math.
///
/// Drops entries whose file source isn't cached (deleted file, unloaded
/// dependency) or whose path can't be converted to a `file://` URI.
fn bt_locations_to_lsp(
    svc: &SimpleLanguageService,
    locations: Vec<BtLocation>,
) -> Vec<tower_lsp::lsp_types::Location> {
    locations
        .into_iter()
        .filter_map(|loc| {
            let source = svc.file_source(&loc.file)?;
            let range = span_to_range(loc.span, &source);
            Some(tower_lsp::lsp_types::Location {
                uri: path_to_uri(&loc.file)?,
                range,
            })
        })
        .collect()
}

/// BT-2240: Append `extras` to `base`, skipping any LSP `Location` already
/// present (matched by `(uri, range)`). Used to overlay declaration sites
/// onto runtime-attached `textDocument/references` results without
/// duplicating entries the runtime already returned.
///
/// `lsp_types::Range` does not implement `Hash`, so the key flattens it
/// to a 4-tuple of `(start.line, start.character, end.line, end.character)`
/// alongside the URI.
fn merge_locations(
    base: &mut Vec<tower_lsp::lsp_types::Location>,
    extras: Vec<tower_lsp::lsp_types::Location>,
) {
    fn key(loc: &tower_lsp::lsp_types::Location) -> (Url, u32, u32, u32, u32) {
        (
            loc.uri.clone(),
            loc.range.start.line,
            loc.range.start.character,
            loc.range.end.line,
            loc.range.end.character,
        )
    }
    let mut seen: HashSet<(Url, u32, u32, u32, u32)> = base.iter().map(key).collect();
    for loc in extras {
        if seen.insert(key(&loc)) {
            base.push(loc);
        }
    }
}

/// BT-2239: Convert a runtime-supplied `NavSite` into an LSP `Location`.
///
/// Steps:
/// 1. Use [`nav_site_to_location`] (in `beamtalk-core`) to resolve the
///    runtime's `source_file` path against workspace roots.
/// 2. Translate the 1-based runtime line to a zero-width LSP `Range`
///    anchored at the start of the line. (The runtime tracks line, not
///    column, so a finer `Range` would require re-reading the file —
///    deferred to per-method consumers if they need it.)
/// 3. Build the `file://` URI from the resolved path.
///
/// Returns `None` when:
/// * The site has no backing `.bt` file (`source_file` is null — stdlib,
///   dynamic, bootstrap class), or
/// * The runtime reported line 0 (defensive — the runtime shouldn't).
/// * The resolved path is not file-URI-able.
fn runtime_site_to_lsp_location(
    site: &NavSite,
    workspace_roots: &[PathBuf],
) -> Option<tower_lsp::lsp_types::Location> {
    let resolved: RuntimeLocation = nav_site_to_location(site, workspace_roots)?;
    let uri = path_to_uri(&resolved.file)?;
    let line = resolved.line.checked_sub(1)?;
    let range = Range {
        start: tower_lsp::lsp_types::Position::new(line, 0),
        end: tower_lsp::lsp_types::Position::new(line, 0),
    };
    Some(tower_lsp::lsp_types::Location { uri, range })
}

/// BT-2244: Convert a runtime [`NavSymbolClass`] to an LSP [`DocumentSymbol`]
/// for `textDocument/documentSymbol`.
///
/// Drops the row when:
/// * the class has no `source_file` (REPL-only / dynamic — these surface
///   in `workspace/symbol`, not the per-file outline), or
/// * the resolved `source_file` doesn't match the requested file (a
///   different class also lives in `nav-symbols`'s reply — we filter
///   here to match the AST-walker's per-file scope).
///
/// The class entry is tagged `Counter (class)` (BT-2244 preserves the
/// ADR 0013 outline disambiguator the cold-file path uses) and the
/// children are the class's locally-defined instance + class-side
/// method headers. Field children are omitted: the live class registry
/// doesn't expose field declarations as xref rows, so the runtime path
/// is method-only by construction. (When the AST fallback runs, fields
/// still appear — the two paths intentionally differ in detail because
/// the runtime carries the *current* class shape, not its source-text
/// declaration.)
#[expect(deprecated, reason = "LSP DocumentSymbol requires deprecated field")]
fn runtime_class_to_document_symbol(
    class: NavSymbolClass,
    requested_path: &Utf8PathBuf,
    workspace_roots: &[PathBuf],
) -> Option<tower_lsp::lsp_types::DocumentSymbol> {
    let source_file = class.source_file.as_deref()?;
    if source_file.is_empty() {
        return None;
    }
    // Canonicalise the runtime-reported `source_file` against the
    // workspace roots the same way nav-query results are canonicalised.
    // The path must equal the file the editor asked about; otherwise the
    // class belongs to a different file in the same reply.
    let resolved = nav_site_to_location(
        &NavSite {
            class: class.name.clone(),
            class_side: false,
            // `nav_site_to_location` only reads `source_file` + `line`;
            // we reuse the class name here to satisfy the struct shape
            // without pulling `ecow` into this crate.
            method: class.name.clone(),
            line: class.line.unwrap_or(1),
            source_file: Some(source_file.to_string()),
        },
        workspace_roots,
    )?;
    if resolved.file != *requested_path {
        return None;
    }

    let class_range = zero_width_range_for_line(class.line.unwrap_or(1));
    let mut children = Vec::with_capacity(class.methods.len());
    for m in class.methods {
        // Runtime-mode methods carry `line: None` when xref has no
        // method_info entry (primitives whose source is `nil`, or a
        // method that hasn't re-registered after a hot reload). Render
        // them at row 0 — better than dropping the row entirely, which
        // would hide selectors the user knows exist.
        let line = m.line.unwrap_or(0);
        let range = zero_width_range_for_line(line);
        // Class-side methods get a `(class)` detail string so editors
        // can disambiguate `Counter >> #foo` from `Counter class >> #foo`
        // in the outline. Instance-side methods carry no detail (parity
        // with the cold-file path, which leaves detail unset).
        let detail = if m.class_side {
            Some("(class)".to_string())
        } else {
            None
        };
        children.push(tower_lsp::lsp_types::DocumentSymbol {
            name: m.selector.to_string(),
            detail,
            kind: SymbolKind::METHOD,
            tags: None,
            deprecated: None,
            range,
            selection_range: range,
            children: None,
        });
    }

    Some(tower_lsp::lsp_types::DocumentSymbol {
        // ADR 0013 — class outline rows carry a ` (class)` suffix so the
        // editor disambiguates them from same-named selectors. Cold-file
        // path matches; we mirror it to keep the editor outline stable
        // across modes.
        name: format!("{} (class)", class.name),
        detail: None,
        kind: SymbolKind::CLASS,
        tags: None,
        deprecated: None,
        range: class_range,
        selection_range: class_range,
        children: if children.is_empty() {
            None
        } else {
            Some(children)
        },
    })
}

/// BT-2244: Convert a runtime [`NavSymbolClass`] to an LSP
/// [`SymbolInformation`] for `workspace/symbol`.
///
/// Applies the `query` substring filter (case-insensitive, empty matches
/// everything) and produces:
/// * a normal source-anchored row when the class has a backing
///   `source_file` resolvable against the workspace roots, or
/// * a zero-width row anchored to the workspace root URI with the
///   detail `(no source file)` when the class has no source — the
///   *headline win* of BT-2244 — so REPL-loaded classes still surface
///   in the editor's Ctrl-T picker.
///
/// Returns `None` when the class fails the substring filter, or when
/// the class has no source AND no workspace root URI is configured (no
/// safe URI to attach the row to).
fn runtime_class_to_workspace_symbol(
    class: &NavSymbolClass,
    query_lower: &str,
    workspace_roots: &[PathBuf],
    workspace_root_uri: Option<&Url>,
) -> Option<SymbolInformation> {
    let name = class.name.to_string();
    if !query_lower.is_empty() && !name.to_ascii_lowercase().contains(query_lower) {
        return None;
    }

    let (uri, range, detail) = match class.source_file.as_deref() {
        Some(source_file) if !source_file.is_empty() => {
            let resolved = nav_site_to_location(
                &NavSite {
                    class: class.name.clone(),
                    class_side: false,
                    // `nav_site_to_location` only reads `source_file` + `line`;
                    // we reuse the class name here to satisfy the struct shape
                    // without pulling `ecow` into this crate.
                    method: class.name.clone(),
                    line: class.line.unwrap_or(1),
                    source_file: Some(source_file.to_string()),
                },
                workspace_roots,
            )?;
            let uri = path_to_uri(&resolved.file)?;
            (uri, zero_width_range_for_line(resolved.line), None)
        }
        _ => {
            // Headline win: surface source-less classes (REPL-loaded,
            // ClassBuilder, stdlib if the consumer asked for scope=all).
            // Attach to the workspace root URI with a (0, 0) range so
            // the editor renders the symbol; clicking opens the root.
            let uri = workspace_root_uri.cloned()?;
            (
                uri,
                Range {
                    start: Position::new(0, 0),
                    end: Position::new(0, 0),
                },
                Some("(no source file)".to_string()),
            )
        }
    };

    #[expect(deprecated, reason = "LSP SymbolInformation requires deprecated field")]
    let info = SymbolInformation {
        name,
        kind: SymbolKind::CLASS,
        tags: None,
        deprecated: None,
        location: tower_lsp::lsp_types::Location { uri, range },
        container_name: detail,
    };
    Some(info)
}

/// BT-2244: 1-based runtime line → zero-width LSP `Range`. Defends
/// against the runtime emitting line 0 (which it shouldn't) by clamping
/// to row 0.
fn zero_width_range_for_line(line: u32) -> Range {
    let row = line.saturating_sub(1);
    Range {
        start: Position::new(row, 0),
        end: Position::new(row, 0),
    }
}

/// BT-2244: Detect whether a runtime [`NavSymbolClass`] is backed by a
/// stdlib source file. Used by the `workspace/symbol` handler to exclude
/// stdlib classes from the runtime-attached result set, mirroring the
/// `ProjectIndex::is_stdlib_file` filter the AST-fallback path applies.
///
/// Returns `false` (i.e. *keep the row*) when:
/// * the class has no `source_file` at all (REPL-loaded / dynamic — the
///   *headline win*; these aren't stdlib by construction), or
/// * the class's `source_file` doesn't resolve against any workspace
///   root, or
/// * the resolved path isn't in the LSP's stdlib set.
fn class_source_is_stdlib(
    class: &NavSymbolClass,
    workspace_roots: &[PathBuf],
    stdlib_paths: &HashSet<Utf8PathBuf>,
) -> bool {
    let Some(source_file) = class.source_file.as_deref() else {
        return false;
    };
    if source_file.is_empty() {
        return false;
    }
    let Some(resolved) = nav_site_to_location(
        &NavSite {
            class: class.name.clone(),
            class_side: false,
            method: class.name.clone(),
            line: class.line.unwrap_or(1),
            source_file: Some(source_file.to_string()),
        },
        workspace_roots,
    ) else {
        return false;
    };
    stdlib_paths.contains(&resolved.file)
}

/// BT-2242: Build a `TypeHierarchyItem` for `class_name` when its declaration
/// site is known. The selection range is collapsed to the class-name token
/// (the `span` field on the declaration `Location`), and the surrounding
/// `range` covers the same span — tower-lsp clients accept identical
/// `range`/`selectionRange` for symbols whose body the server doesn't
/// model (Beamtalk class bodies are method-level; the symbol itself is
/// the name).
///
/// `fallback_uri` is only consulted in the `declaration == None` branch —
/// when there is no indexed declaration at all, we still want to render
/// the symbol row, so we attach the originating URI with a zero-width
/// range. In the `Some(loc)` branch we never fall back: if either
/// `file_source(&loc.file)` or `path_to_uri(&loc.file)` fails the function
/// returns `None`. Returning `None` for a known-but-unloadable declaration
/// is the conservative choice — pointing the editor at the wrong file
/// would be worse than dropping the row.
fn type_hierarchy_item(
    class_name: &str,
    declaration: Option<&beamtalk_core::language_service::Location>,
    svc: &SimpleLanguageService,
    fallback_uri: &Url,
) -> Option<TypeHierarchyItem> {
    let (uri, range) = if let Some(loc) = declaration {
        let source = svc.file_source(&loc.file)?;
        let range = span_to_range(loc.span, &source);
        let uri = path_to_uri(&loc.file)?;
        (uri, range)
    } else {
        // No indexed declaration — the class is real (the project index
        // knows about it; e.g. a stdlib class compiled without source) but
        // we don't have a file to point at. Attach the originating URI
        // with a zero-width range at (0, 0) so editors render the name.
        let zero = Range {
            start: tower_lsp::lsp_types::Position::new(0, 0),
            end: tower_lsp::lsp_types::Position::new(0, 0),
        };
        (fallback_uri.clone(), zero)
    };
    Some(TypeHierarchyItem {
        name: class_name.to_string(),
        kind: SymbolKind::CLASS,
        tags: None,
        detail: None,
        uri,
        range,
        selection_range: range,
        data: None,
    })
}

/// BT-2242: Build a `TypeHierarchyItem` for a class whose declaration site
/// is not indexed in the language service (e.g. ancestor classes
/// compiled into the runtime without a `.bt` source mapping). Falls back
/// to the parent item's URI so the editor still renders the row.
///
/// Kept separate from [`type_hierarchy_item`] so the parent-URI fallback
/// is explicit at the call site — the `prepare` path passes the open
/// document, the `super-/subtypes` paths pass the parent item's URI from
/// the request.
fn type_hierarchy_item_for_undeclared(class_name: &str, parent_uri: Url) -> TypeHierarchyItem {
    let zero = Range {
        start: tower_lsp::lsp_types::Position::new(0, 0),
        end: tower_lsp::lsp_types::Position::new(0, 0),
    };
    TypeHierarchyItem {
        name: class_name.to_string(),
        kind: SymbolKind::CLASS,
        tags: None,
        detail: None,
        uri: parent_uri,
        range: zero,
        selection_range: zero,
        data: None,
    }
}

/// BT-2242: Convert a `Vec<(name, Option<Location>)>` (the shape
/// `supertypes_of` / `subtypes_of` return) into LSP items, falling back
/// to the parent item's URI for any class without an indexed declaration.
fn collect_hierarchy_items<S>(
    rows: Vec<(S, Option<beamtalk_core::language_service::Location>)>,
    svc: &SimpleLanguageService,
    parent_uri: &Url,
) -> Vec<TypeHierarchyItem>
where
    S: AsRef<str>,
{
    rows.into_iter()
        .map(|(name, loc)| {
            let resolved = loc
                .as_ref()
                .and_then(|_| type_hierarchy_item(name.as_ref(), loc.as_ref(), svc, parent_uri));
            resolved.unwrap_or_else(|| {
                type_hierarchy_item_for_undeclared(name.as_ref(), parent_uri.clone())
            })
        })
        .collect()
}

/// Converts a byte offset to an LSP `Position` (0-based line/character in UTF-16 code units).
fn offset_to_position(offset: usize, source: &str) -> tower_lsp::lsp_types::Position {
    let offset = offset.min(source.len());
    let mut line = 0u32;
    let mut col = 0u32;
    for (i, ch) in source.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            // UTF-16 len is always 1 or 2, safe to truncate
            #[expect(
                clippy::cast_possible_truncation,
                reason = "char::len_utf16() is always 1 or 2"
            )]
            {
                col += ch.len_utf16() as u32;
            }
        }
    }
    tower_lsp::lsp_types::Position::new(line, col)
}

/// Converts an LSP `Position` (line, UTF-16 column) to a byte offset in `source`.
///
/// Returns `source.len()` when the position is beyond the end of the file.
fn position_to_offset(pos: tower_lsp::lsp_types::Position, source: &str) -> usize {
    let mut line = 0u32;
    let mut line_start = 0usize;
    for (i, ch) in source.char_indices() {
        if line == pos.line {
            // Walk UTF-16 columns within this line
            let mut col_utf16 = 0u32;
            let mut byte_offset = line_start;
            for (j, c) in source[line_start..].char_indices() {
                if col_utf16 >= pos.character {
                    return line_start + j;
                }
                if c == '\n' {
                    break;
                }
                #[expect(
                    clippy::cast_possible_truncation,
                    reason = "char::len_utf16() is always 1 or 2"
                )]
                {
                    col_utf16 += c.len_utf16() as u32;
                }
                byte_offset = line_start + j + c.len_utf8();
            }
            return byte_offset;
        }
        if ch == '\n' {
            line += 1;
            line_start = i + 1;
        }
    }
    source.len()
}

/// Extracts the `specs_line` value from a type cache JSON entry.
///
/// The cache format is `{"beam_mtime_secs":N,"specs_line":"..."}`.
/// This is a simple key-value extraction — we don't need a full JSON parser.
fn extract_specs_line_from_cache(json: &str) -> Option<String> {
    // Look for "specs_line":" and extract until the closing unescaped "
    let key = "\"specs_line\":\"";
    let start = json.find(key)? + key.len();
    let remaining = &json[start..];

    // Find the end of the JSON string value, handling escaped quotes
    let mut result = String::new();
    let mut chars = remaining.chars();
    loop {
        match chars.next()? {
            '\\' => {
                // Handle escape sequences
                match chars.next()? {
                    '"' => result.push('"'),
                    '\\' => result.push('\\'),
                    'n' => result.push('\n'),
                    'r' => result.push('\r'),
                    't' => result.push('\t'),
                    other => {
                        result.push('\\');
                        result.push(other);
                    }
                }
            }
            '"' => break, // End of string value
            c => result.push(c),
        }
    }

    Some(result)
}

/// Converts a beamtalk `Diagnostic` to an LSP `Diagnostic`.
fn to_lsp_diagnostic(
    diag: &beamtalk_core::language_service::Diagnostic,
    source: Option<&str>,
) -> tower_lsp::lsp_types::Diagnostic {
    let range = source
        .map(|src| span_to_range(diag.span, src))
        .unwrap_or_default();

    tower_lsp::lsp_types::Diagnostic {
        range,
        severity: Some(match diag.severity {
            Severity::Error => DiagnosticSeverity::ERROR,
            Severity::Warning => DiagnosticSeverity::WARNING,
            // Lint and Hint map to LSP HINT (informational)
            Severity::Lint | Severity::Hint => DiagnosticSeverity::HINT,
        }),
        source: Some("beamtalk".into()),
        message: {
            use std::fmt::Write;
            let mut msg = diag.message.to_string();
            // BT-1588: Append notes for origin tracing
            for note in &diag.notes {
                let _ = write!(msg, "\n  = {}", note.message);
            }
            if let Some(ref hint) = diag.hint {
                let _ = write!(msg, "\nHint: {hint}");
            }
            msg
        },
        ..Default::default()
    }
}

/// Converts a beamtalk `DocumentSymbol` to an LSP `DocumentSymbol`.
#[expect(deprecated, reason = "LSP DocumentSymbol requires deprecated field")]
fn to_lsp_symbol(
    sym: beamtalk_core::language_service::DocumentSymbol,
    source: &str,
) -> tower_lsp::lsp_types::DocumentSymbol {
    let range = span_to_range(sym.span, source);
    let children = sym
        .children
        .into_iter()
        .map(|c| to_lsp_symbol(c, source))
        .collect();

    let selection_range = sym.name_span.map_or(range, |s| span_to_range(s, source));
    tower_lsp::lsp_types::DocumentSymbol {
        name: sym.name.to_string(),
        kind: match sym.kind {
            DocumentSymbolKind::Class => SymbolKind::CLASS,
            DocumentSymbolKind::Method | DocumentSymbolKind::ClassMethod => SymbolKind::METHOD,
            DocumentSymbolKind::Field => SymbolKind::FIELD,
        },
        detail: None,
        tags: None,
        deprecated: None,
        range,
        selection_range,
        children: Some(children),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_core::language_service::HoverInfo;
    use beamtalk_core::test_helpers::unique_temp_dir;
    use camino::Utf8PathBuf;
    use std::fs;

    #[test]
    fn configured_stdlib_source_dirs_rejects_relative_traversal_outside_root() {
        let temp = unique_temp_dir("beamtalk_lsp_stdlib_traversal");
        let project_root = temp.join("project");
        let outside = temp.join("outside");
        fs::create_dir_all(project_root.join("src")).expect("create project dirs");
        fs::create_dir_all(outside.join("lib")).expect("create outside dirs");

        let dirs = configured_stdlib_source_dirs(Some("../outside/lib"), &[project_root]);
        assert!(dirs.is_empty());

        let _ = fs::remove_dir_all(&temp);
    }

    #[test]
    fn configured_stdlib_source_dirs_accepts_in_root_absolute_path() {
        let temp = unique_temp_dir("beamtalk_lsp_stdlib_in_root");
        let project_root = temp.join("project");
        let stdlib = project_root.join("stdlib/src");
        fs::create_dir_all(&stdlib).expect("create stdlib dir");

        let dirs = configured_stdlib_source_dirs(
            Some(stdlib.to_str().expect("utf8 path")),
            std::slice::from_ref(&project_root),
        );
        assert_eq!(dirs.len(), 1);

        let _ = fs::remove_dir_all(&temp);
    }

    #[test]
    fn configured_stdlib_source_dirs_accepts_outside_root_absolute_path() {
        let temp = unique_temp_dir("beamtalk_lsp_stdlib_outside_root");
        let project_root = temp.join("project");
        let stdlib = temp.join("shared-stdlib");
        fs::create_dir_all(project_root.join("src")).expect("create project dir");
        fs::create_dir_all(&stdlib).expect("create stdlib dir");

        let dirs = configured_stdlib_source_dirs(
            Some(stdlib.to_str().expect("utf8 path")),
            std::slice::from_ref(&project_root),
        );
        assert_eq!(
            dirs,
            vec![fs::canonicalize(&stdlib).expect("canonical stdlib")]
        );

        let _ = fs::remove_dir_all(&temp);
    }

    #[test]
    fn configured_stdlib_source_dir_reads_initialize_option() {
        let params = InitializeParams {
            initialization_options: Some(serde_json::json!({
                "stdlibSourceDir": "stdlib/src"
            })),
            ..Default::default()
        };

        let configured = configured_stdlib_source_dir(&params);
        assert_eq!(configured.as_deref(), Some("stdlib/src"));
    }

    #[test]
    fn extract_hover_class_name_from_class_hover() {
        let text = "Class: `Integer`";
        assert_eq!(extract_hover_class_name(text), Some("Integer"));
    }

    #[test]
    fn extract_hover_class_name_from_package_qualified_hover() {
        // BT-1658: class name extraction must work with package provenance suffix
        let text = "Class: `Parser` (from package `json`)";
        assert_eq!(extract_hover_class_name(text), Some("Parser"));
    }

    #[test]
    fn extract_hover_class_name_from_resolved_method_hover() {
        let text = "```beamtalk\n+\n```\n\nResolved on `Integer` (defined in `Integer`)\n\n_instance-side, sealed_";
        assert_eq!(extract_hover_class_name(text), Some("Integer"));
    }

    #[test]
    fn stdlib_policy_note_for_sealed_class() {
        let service = SimpleLanguageService::new();
        let note = stdlib_hover_policy_note(&service, "Class: `Integer`");
        assert!(note.is_some());
        let note = note.unwrap();
        assert!(note.contains("Stdlib Profile"));
        assert!(note.contains("sealed"));
    }

    #[test]
    fn stdlib_policy_note_ignores_non_builtin_classes() {
        let mut service = SimpleLanguageService::new();
        let file = Utf8PathBuf::from("user.bt");
        service.update_file(
            file,
            "Object subclass: Counter\n  increment => 1".to_string(),
        );

        let note = stdlib_hover_policy_note(&service, "Class: `Counter`");
        assert!(note.is_none());
    }

    #[test]
    fn stdlib_policy_note_for_abstract_class() {
        let service = SimpleLanguageService::new();
        let note = stdlib_hover_policy_note(&service, "Class: `Boolean`");
        assert!(note.is_some());
        let note = note.unwrap();
        assert!(note.contains("abstract"));
    }

    #[test]
    fn stdlib_policy_note_marks_high_confidence_for_sealed_method_hover() {
        let service = SimpleLanguageService::new();
        let hover = "```beamtalk\n+\n```\n\nResolved on `Integer` (defined in `Integer`)\n\n_instance-side, sealed_";
        let note = stdlib_hover_policy_note(&service, hover);
        assert!(note.is_some());
        let note = note.unwrap();
        assert!(note.contains("Confidence: high"));
        assert!(note.contains("sealed stdlib dispatch"));
    }

    #[test]
    fn stdlib_policy_note_does_not_mark_high_confidence_for_class_hover() {
        let service = SimpleLanguageService::new();
        let note = stdlib_hover_policy_note(&service, "Class: `Integer`");
        assert!(note.is_some());
        assert!(!note.unwrap().contains("Confidence: high"));
    }

    #[test]
    fn policy_note_works_for_resolved_method_hover_markdown() {
        let service = SimpleLanguageService::new();
        let hover = HoverInfo::new("```beamtalk\n+\n```", Span::new(0, 1)).with_documentation(
            "Resolved on `Integer` (defined in `Integer`)\n\n_instance-side, sealed_",
        );
        let markdown = format!(
            "{}\n\n{}",
            hover.contents,
            format_hover_documentation(hover.documentation.as_deref().unwrap_or_default())
        );

        let note = stdlib_hover_policy_note(&service, &markdown);
        assert!(note.is_some());
        assert!(note.unwrap().contains("Integer"));
    }

    #[test]
    fn configured_stdlib_falls_back_to_sysroot_when_none() {
        // When no explicit stdlibSourceDir is configured, configured_stdlib_source_dirs
        // falls back to sysroot auto-discovery. Assert against the actual sysroot
        // result so the test is deterministic whether or not the binary is installed.
        let expected: Vec<PathBuf> = sysroot_stdlib_source_dir().into_iter().collect();
        let dirs = configured_stdlib_source_dirs(None, &[PathBuf::from("/workspace/project")]);
        assert_eq!(dirs, expected);
    }

    #[test]
    fn sysroot_stdlib_source_dir_smoke_test() {
        // Verify sysroot_stdlib_source_dir doesn't panic regardless of
        // the environment — the result depends on the test runner's
        // installation layout and may be Some or None.
        let _ = sysroot_stdlib_source_dir();
    }

    #[test]
    fn path_to_stdlib_uri_produces_beamtalk_stdlib_scheme() {
        let path = Utf8PathBuf::from("/usr/share/beamtalk/stdlib/src/Integer.bt");
        let uri = path_to_stdlib_uri(&path).expect("should produce URI");
        assert_eq!(uri.scheme(), "beamtalk-stdlib");
        assert_eq!(uri.path(), "/Integer.bt");
    }

    #[test]
    fn path_to_stdlib_uri_handles_nested_path() {
        let path = Utf8PathBuf::from("/some/deep/path/to/Collection.bt");
        let uri = path_to_stdlib_uri(&path).expect("should produce URI");
        assert_eq!(uri.scheme(), "beamtalk-stdlib");
        assert_eq!(uri.path(), "/Collection.bt");
    }

    #[tokio::test]
    async fn fetch_content_returns_error_for_unsupported_scheme() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();
        let result: tower_lsp::jsonrpc::Result<FetchContentResult> = backend
            .fetch_content(FetchContentParams {
                uri: "file:///some/file.bt".to_string(),
            })
            .await;
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.message.contains("unsupported URI scheme"));
    }

    #[tokio::test]
    async fn fetch_content_returns_error_for_missing_stdlib_file() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();
        let result: tower_lsp::jsonrpc::Result<FetchContentResult> = backend
            .fetch_content(FetchContentParams {
                uri: "beamtalk-stdlib:///NonExistent.bt".to_string(),
            })
            .await;
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.message.contains("stdlib source not available"));
        assert!(err.message.contains("NonExistent.bt"));
    }

    #[tokio::test]
    async fn fetch_content_rejects_malformed_stdlib_uri_path() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        for bad_uri in &[
            "beamtalk-stdlib:///",
            "beamtalk-stdlib:///sub/Integer.bt",
            "beamtalk-stdlib:///Integer.erl",
        ] {
            let result: tower_lsp::jsonrpc::Result<FetchContentResult> = backend
                .fetch_content(FetchContentParams {
                    uri: bad_uri.to_string(),
                })
                .await;
            assert!(result.is_err(), "expected error for {bad_uri}");
            let err = result.unwrap_err();
            assert!(
                err.message.contains("invalid stdlib URI path"),
                "expected 'invalid stdlib URI path' in error for {bad_uri}, got: {}",
                err.message
            );
        }
    }

    #[tokio::test]
    async fn fetch_content_rejects_authority_bearing_stdlib_uri() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();
        let result: tower_lsp::jsonrpc::Result<FetchContentResult> = backend
            .fetch_content(FetchContentParams {
                uri: "beamtalk-stdlib://host/Integer.bt".to_string(),
            })
            .await;
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.message.contains("invalid stdlib URI"),
            "expected 'invalid stdlib URI' but got: {}",
            err.message
        );
    }

    #[tokio::test]
    async fn fetch_content_rejects_stdlib_uri_with_query_or_fragment() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        for bad_uri in &[
            "beamtalk-stdlib:///Integer.bt?x=1",
            "beamtalk-stdlib:///Integer.bt#section",
        ] {
            let result: tower_lsp::jsonrpc::Result<FetchContentResult> = backend
                .fetch_content(FetchContentParams {
                    uri: bad_uri.to_string(),
                })
                .await;
            assert!(result.is_err(), "expected error for {bad_uri}");
            let err = result.unwrap_err();
            assert!(
                err.message.contains("invalid stdlib URI"),
                "expected 'invalid stdlib URI' for {bad_uri}, got: {}",
                err.message
            );
        }
    }

    #[tokio::test]
    async fn fetch_content_returns_error_for_ambiguous_stdlib_filename() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        let path1 = Utf8PathBuf::from("/fake/stdlib/a/Integer.bt");
        let path2 = Utf8PathBuf::from("/fake/stdlib/b/Integer.bt");
        let content = "Object subclass: Integer".to_string();

        {
            let mut stdlib_paths = backend
                .stdlib_paths
                .lock()
                .expect("stdlib_paths lock poisoned");
            stdlib_paths.insert(path1.clone());
            stdlib_paths.insert(path2.clone());
        }
        {
            let mut svc = backend.service.lock().expect("service lock poisoned");
            svc.update_file(path1, content.clone());
            svc.update_file(path2, content);
        }

        let result: tower_lsp::jsonrpc::Result<FetchContentResult> = backend
            .fetch_content(FetchContentParams {
                uri: "beamtalk-stdlib:///Integer.bt".to_string(),
            })
            .await;
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            err.message.contains("ambiguous"),
            "expected 'ambiguous' but got: {}",
            err.message
        );
    }

    #[tokio::test]
    async fn fetch_content_returns_content_for_registered_stdlib_file() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        let stdlib_path = Utf8PathBuf::from("/fake/stdlib/Integer.bt");
        let content = "Object subclass: Integer\n  + other => 0".to_string();

        {
            let mut stdlib_paths = backend
                .stdlib_paths
                .lock()
                .expect("stdlib_paths lock poisoned");
            stdlib_paths.insert(stdlib_path.clone());
        }
        {
            let mut svc = backend.service.lock().expect("service lock poisoned");
            svc.update_file(stdlib_path, content.clone());
        }

        let result: FetchContentResult = backend
            .fetch_content(FetchContentParams {
                uri: "beamtalk-stdlib:///Integer.bt".to_string(),
            })
            .await
            .expect("should return content");
        assert_eq!(result.content, content);
    }

    #[test]
    fn collect_preload_files_classifies_overlapping_path_as_stdlib() {
        // When the same .bt file appears in both a workspace src/ dir and a
        // stdlib dir, it must end up in stdlib_files (not user_files) so that
        // goto_definition emits a beamtalk-stdlib:// URI rather than file://.
        let temp = unique_temp_dir("beamtalk_lsp_preload_overlap");
        let project_root = temp.join("project");
        let src_dir = project_root.join("src");
        let stdlib_dir = temp.join("stdlib");

        fs::create_dir_all(&src_dir).expect("create src dir");
        fs::create_dir_all(&stdlib_dir).expect("create stdlib dir");

        // Write the same path into both dirs by using a shared physical file
        // path via the stdlib_dir pointing into the src dir is not realistic;
        // instead, create the same filename in both so we can test the set logic.
        // The realistic case is a symlinked or canonicalized path appearing twice.
        let shared_path = src_dir.join("Integer.bt");
        fs::write(&shared_path, "Object subclass: Integer").expect("write Integer");
        let stdlib_integer = stdlib_dir.join("Integer.bt");
        fs::write(&stdlib_integer, "Object subclass: Integer").expect("write stdlib Integer");

        // The paths are different files with the same name — test that the stdlib
        // path deduplication is independent of the user path deduplication.
        let config = PreloadConfig {
            roots: vec![project_root.clone()],
            stdlib_dirs: vec![stdlib_dir.clone()],
        };
        let loaded = collect_preload_files(config);

        // The two files have the same name but different paths, so both should
        // appear in their respective buckets.
        assert_eq!(loaded.user_files.len(), 1);
        assert_eq!(loaded.stdlib_files.len(), 1);
        assert!(loaded.user_files[0].0.ends_with("Integer.bt"));
        assert!(loaded.stdlib_files[0].0.ends_with("Integer.bt"));
        // The user file must NOT be the stdlib path.
        assert_ne!(loaded.user_files[0].0, stdlib_integer);
        assert_eq!(loaded.stdlib_files[0].0, stdlib_integer);

        let _ = fs::remove_dir_all(&temp);
    }

    #[test]
    fn collect_preload_files_separates_user_and_stdlib_files() {
        let temp = unique_temp_dir("beamtalk_lsp_preload_separation");
        let project_root = temp.join("project");
        let src_dir = project_root.join("src");
        let stdlib_dir = temp.join("stdlib");

        fs::create_dir_all(&src_dir).expect("create src dir");
        fs::create_dir_all(&stdlib_dir).expect("create stdlib dir");

        fs::write(src_dir.join("User.bt"), "Object subclass: User").expect("write user file");
        fs::write(stdlib_dir.join("Integer.bt"), "Object subclass: Integer")
            .expect("write stdlib file");

        let config = PreloadConfig {
            roots: vec![project_root],
            stdlib_dirs: vec![stdlib_dir],
        };
        let loaded = collect_preload_files(config);

        assert_eq!(loaded.user_files.len(), 1);
        assert_eq!(loaded.stdlib_files.len(), 1);
        assert!(loaded.user_files[0].0.ends_with("User.bt"));
        assert!(loaded.stdlib_files[0].0.ends_with("Integer.bt"));

        let _ = fs::remove_dir_all(&temp);
    }

    /// BT-2027: Preload must walk both `src/` and `test/`. Before this fix,
    /// opening a `test/` file in the LSP reported spurious `Unresolved class`
    /// for every reference to a `src/` class.
    #[test]
    fn collect_preload_files_walks_src_and_test() {
        let temp = unique_temp_dir("beamtalk_lsp_preload_test_dir");
        let project_root = temp.join("project");
        let src_dir = project_root.join("src");
        let test_dir = project_root.join("test");

        fs::create_dir_all(&src_dir).expect("create src dir");
        fs::create_dir_all(&test_dir).expect("create test dir");

        fs::write(src_dir.join("Foo.bt"), "Object subclass: Foo").expect("write src file");
        fs::write(test_dir.join("FooTest.bt"), "Object subclass: FooTest")
            .expect("write test file");

        let config = PreloadConfig {
            roots: vec![project_root],
            stdlib_dirs: vec![],
        };
        let loaded = collect_preload_files(config);

        assert_eq!(loaded.user_files.len(), 2);
        assert!(loaded.user_files.iter().any(|(p, _)| p.ends_with("Foo.bt")));
        assert!(
            loaded
                .user_files
                .iter()
                .any(|(p, _)| p.ends_with("FooTest.bt"))
        );

        let _ = fs::remove_dir_all(&temp);
    }

    /// BT-2137: Preload must walk every `_build/deps/<name>/src/` so classes
    /// from declared `beamtalk.toml` dependencies resolve without spurious
    /// `Unresolved class` warnings.
    #[test]
    fn collect_preload_files_walks_dependency_src_dirs() {
        let temp = unique_temp_dir("beamtalk_lsp_preload_deps");
        let project_root = temp.join("project");
        let src_dir = project_root.join("src");
        let dep_src = project_root
            .join("_build")
            .join("deps")
            .join("http")
            .join("src");
        let other_dep_src = project_root
            .join("_build")
            .join("deps")
            .join("json")
            .join("src");

        fs::create_dir_all(&src_dir).expect("create src dir");
        fs::create_dir_all(&dep_src).expect("create dep src dir");
        fs::create_dir_all(&other_dep_src).expect("create other dep src dir");

        fs::write(src_dir.join("App.bt"), "Object subclass: App").expect("write src file");
        fs::write(dep_src.join("HTTPClient.bt"), "Object subclass: HTTPClient")
            .expect("write dep file");
        fs::write(
            other_dep_src.join("JSONParser.bt"),
            "Object subclass: JSONParser",
        )
        .expect("write other dep file");

        let config = PreloadConfig {
            roots: vec![project_root],
            stdlib_dirs: vec![],
        };
        let loaded = collect_preload_files(config);

        assert_eq!(loaded.user_files.len(), 3);
        assert!(loaded.user_files.iter().any(|(p, _)| p.ends_with("App.bt")));
        assert!(
            loaded
                .user_files
                .iter()
                .any(|(p, _)| p.ends_with("HTTPClient.bt")),
            "dependency class HTTPClient must be preloaded, got {:?}",
            loaded.user_files
        );
        assert!(
            loaded
                .user_files
                .iter()
                .any(|(p, _)| p.ends_with("JSONParser.bt")),
            "dependency class JSONParser must be preloaded, got {:?}",
            loaded.user_files
        );

        let _ = fs::remove_dir_all(&temp);
    }

    /// BT-2137: A dep checkout without a `src/` subdirectory must be skipped
    /// silently rather than causing a walk failure.
    #[test]
    fn collect_preload_files_tolerates_dep_without_src_dir() {
        let temp = unique_temp_dir("beamtalk_lsp_preload_deps_no_src");
        let project_root = temp.join("project");
        let src_dir = project_root.join("src");
        let dep_dir = project_root.join("_build").join("deps").join("oddball");

        fs::create_dir_all(&src_dir).expect("create src dir");
        fs::create_dir_all(&dep_dir).expect("create bare dep dir");
        fs::write(src_dir.join("App.bt"), "Object subclass: App").expect("write src file");

        let config = PreloadConfig {
            roots: vec![project_root],
            stdlib_dirs: vec![],
        };
        let loaded = collect_preload_files(config);

        assert_eq!(loaded.user_files.len(), 1);
        assert!(loaded.user_files[0].0.ends_with("App.bt"));

        let _ = fs::remove_dir_all(&temp);
    }

    /// BT-2137: In a multi-root workspace, every root's own `src/`/`test/`
    /// must be preloaded before *any* root's dependency `src/` directories.
    /// Otherwise deps from an earlier root could exhaust the shared
    /// `PRELOAD_MAX_FILES` budget and leave a later root's user files
    /// unindexed — exactly the unresolved-class noise this change fights.
    #[test]
    fn collect_preload_files_prioritizes_user_files_across_multi_root_workspace() {
        let temp = unique_temp_dir("beamtalk_lsp_preload_multi_root");
        let root_a = temp.join("root_a");
        let root_b = temp.join("root_b");
        let src_a = root_a.join("src");
        let src_b = root_b.join("src");
        let dep_a_src = root_a.join("_build").join("deps").join("dep_a").join("src");

        fs::create_dir_all(&src_a).expect("create root_a src");
        fs::create_dir_all(&src_b).expect("create root_b src");
        fs::create_dir_all(&dep_a_src).expect("create root_a dep src");

        fs::write(src_a.join("AppA.bt"), "Object subclass: AppA").expect("write src_a file");
        fs::write(src_b.join("AppB.bt"), "Object subclass: AppB").expect("write src_b file");
        fs::write(dep_a_src.join("DepA.bt"), "Object subclass: DepA").expect("write dep file");

        let config = PreloadConfig {
            roots: vec![root_a, root_b],
            stdlib_dirs: vec![],
        };
        let loaded = collect_preload_files(config);

        // The ordering contract: every workspace user file appears in
        // `user_paths` *before* any dependency file. We can observe that by
        // confirming root_b's `AppB.bt` is loaded before root_a's `DepA.bt`.
        let app_b_pos = loaded
            .user_files
            .iter()
            .position(|(p, _)| p.ends_with("AppB.bt"))
            .expect("AppB.bt must be preloaded");
        let dep_a_pos = loaded
            .user_files
            .iter()
            .position(|(p, _)| p.ends_with("DepA.bt"))
            .expect("DepA.bt must be preloaded");
        assert!(
            app_b_pos < dep_a_pos,
            "root_b user file must be walked before root_a dep file (got AppB at {app_b_pos}, DepA at {dep_a_pos})"
        );

        let _ = fs::remove_dir_all(&temp);
    }

    #[test]
    fn format_source_returns_none_for_parse_errors() {
        let result = format_source("@@@invalid beamtalk@@@");
        assert!(result.is_none(), "parse errors must suppress formatting");
    }

    #[test]
    fn format_source_returns_formatted_string() {
        // An unformatted source must produce non-empty output, and that output
        // must be stable (formatting the result again returns the same string).
        let source = "Object subclass: Foo\n  bar => 42\n";
        let pass1 = format_source(source).expect("valid source must produce output");
        assert!(!pass1.is_empty(), "formatted output must not be empty");
        let pass2 = format_source(&pass1).expect("formatted output must be valid");
        assert_eq!(pass1, pass2, "output must be canonical (idempotent)");
    }

    #[test]
    fn format_source_ensures_trailing_newline() {
        let source = "x := 42";
        let result = format_source(source).expect("valid source");
        assert!(
            result.ends_with('\n'),
            "formatted output must end with newline"
        );
    }

    #[test]
    fn format_source_idempotent() {
        let source = "Object subclass: Foo\n  bar => 42\n";
        let pass1 = format_source(source).expect("pass 1");
        let pass2 = format_source(&pass1).expect("pass 2");
        assert_eq!(pass1, pass2, "formatting must be idempotent");
    }

    #[test]
    fn resolve_path_for_uri_stdlib_uri_resolves_to_real_path() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        let real_path = Utf8PathBuf::from("/fake/stdlib/Integer.bt");
        {
            let mut stdlib_paths = backend
                .stdlib_paths
                .lock()
                .expect("stdlib_paths lock poisoned");
            stdlib_paths.insert(real_path.clone());
        }

        let uri = Url::parse("beamtalk-stdlib:///Integer.bt").expect("valid URI");
        let result = backend.resolve_path_for_uri(&uri);
        assert_eq!(result, Some(real_path));
    }

    #[test]
    fn resolve_path_for_uri_unknown_class_returns_none() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        let uri = Url::parse("beamtalk-stdlib:///NonExistent.bt").expect("valid URI");
        let result = backend.resolve_path_for_uri(&uri);
        assert!(result.is_none());
    }

    #[test]
    fn resolve_path_for_uri_invalid_stdlib_form_returns_none() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        let real_path = Utf8PathBuf::from("/fake/stdlib/Integer.bt");
        {
            let mut stdlib_paths = backend
                .stdlib_paths
                .lock()
                .expect("stdlib_paths lock poisoned");
            stdlib_paths.insert(real_path);
        }

        for bad_uri in &[
            "beamtalk-stdlib:///",
            "beamtalk-stdlib:///sub/Integer.bt",
            "beamtalk-stdlib:///Integer.erl",
            "beamtalk-stdlib://host/Integer.bt",
            "beamtalk-stdlib:///Integer.bt?x=1",
            "beamtalk-stdlib:///Integer.bt#section",
        ] {
            let uri = Url::parse(bad_uri).expect("parseable URI");
            let result = backend.resolve_path_for_uri(&uri);
            assert!(result.is_none(), "expected None for {bad_uri}");
        }
    }

    #[test]
    fn resolve_path_for_uri_ambiguous_stdlib_returns_none() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        {
            let mut stdlib_paths = backend
                .stdlib_paths
                .lock()
                .expect("stdlib_paths lock poisoned");
            stdlib_paths.insert(Utf8PathBuf::from("/fake/stdlib/a/Integer.bt"));
            stdlib_paths.insert(Utf8PathBuf::from("/fake/stdlib/b/Integer.bt"));
        }

        let uri = Url::parse("beamtalk-stdlib:///Integer.bt").expect("valid URI");
        let result = backend.resolve_path_for_uri(&uri);
        assert!(result.is_none(), "ambiguous filename must return None");
    }

    #[test]
    #[cfg(unix)]
    fn resolve_path_for_uri_file_uri_still_works() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        let uri = Url::parse("file:///some/project/main.bt").expect("valid URI");
        let result = backend.resolve_path_for_uri(&uri);
        assert_eq!(result, Some(Utf8PathBuf::from("/some/project/main.bt")));
    }

    #[test]
    #[cfg(windows)]
    fn resolve_path_for_uri_file_uri_still_works() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        let uri = Url::parse("file:///C:/project/main.bt").expect("valid URI");
        let result = backend.resolve_path_for_uri(&uri);
        assert_eq!(result, Some(Utf8PathBuf::from("C:/project/main.bt")));
    }

    // ── position_to_offset tests (BT-1067) ─────────────────────────────

    #[test]
    fn position_to_offset_first_line() {
        let source = "hello world";
        let pos = tower_lsp::lsp_types::Position::new(0, 6);
        assert_eq!(position_to_offset(pos, source), 6);
    }

    #[test]
    fn position_to_offset_second_line() {
        let source = "hello\nworld";
        let pos = tower_lsp::lsp_types::Position::new(1, 3);
        assert_eq!(position_to_offset(pos, source), 9); // 'l' in "world"
    }

    #[test]
    fn position_to_offset_beyond_eof() {
        let source = "hi";
        let pos = tower_lsp::lsp_types::Position::new(5, 0);
        assert_eq!(position_to_offset(pos, source), source.len());
    }

    #[test]
    fn position_to_offset_roundtrip_with_offset_to_position() {
        let source = "Object subclass: Counter\n  count => 42";
        for byte_offset in [0, 5, 24, 25, 30, 38] {
            let pos = offset_to_position(byte_offset, source);
            let back = position_to_offset(pos, source);
            assert_eq!(
                back, byte_offset,
                "roundtrip failed for offset {byte_offset}"
            );
        }
    }

    // -----------------------------------------------------------------------
    // Type cache JSON parsing (ADR 0075)
    // -----------------------------------------------------------------------

    #[test]
    fn extract_specs_line_from_cache_basic() {
        let json = r#"{"beam_mtime_secs":12345,"specs_line":"beamtalk-specs-module:lists:[#{arity => 1}]"}"#;
        let result = extract_specs_line_from_cache(json);
        assert_eq!(
            result.as_deref(),
            Some("beamtalk-specs-module:lists:[#{arity => 1}]")
        );
    }

    #[test]
    fn extract_specs_line_from_cache_empty() {
        let json = r#"{"beam_mtime_secs":100,"specs_line":""}"#;
        let result = extract_specs_line_from_cache(json);
        assert_eq!(result.as_deref(), Some(""));
    }

    #[test]
    fn extract_specs_line_from_cache_with_escaped_quotes() {
        let json = r#"{"beam_mtime_secs":100,"specs_line":"name => <<\"reverse\">>"}"#;
        let result = extract_specs_line_from_cache(json);
        assert_eq!(result.as_deref(), Some(r#"name => <<"reverse">>"#));
    }

    #[test]
    fn extract_specs_line_from_cache_missing_key() {
        let json = r#"{"beam_mtime_secs":100}"#;
        let result = extract_specs_line_from_cache(json);
        assert!(result.is_none());
    }

    // ----------------------------------------------------------------------
    // ADR 0082 Phase 3 (BT-2289): executeCommand expression builder + helpers
    // ----------------------------------------------------------------------

    #[test]
    fn build_flush_with_no_arguments_returns_workspace_flush() {
        let expr = build_command_expression(CMD_FLUSH, &[]).expect("ok");
        assert_eq!(expr, "Workspace flush");
    }

    #[test]
    fn build_flush_rejects_extra_arguments() {
        let err =
            build_command_expression(CMD_FLUSH, &[serde_json::json!("extra")]).expect_err("err");
        assert!(err.contains("expected no arguments"));
    }

    #[test]
    fn build_flush_class_with_valid_name_returns_class_filter() {
        let expr =
            build_command_expression(CMD_FLUSH_CLASS, &[serde_json::json!("Counter")]).expect("ok");
        assert_eq!(expr, "Workspace flush: Counter");
    }

    #[test]
    fn build_flush_class_rejects_lowercase_first_letter() {
        let err = build_command_expression(CMD_FLUSH_CLASS, &[serde_json::json!("counter")])
            .expect_err("err");
        assert!(err.contains("must start with an uppercase letter"));
    }

    #[test]
    fn build_flush_class_rejects_special_chars() {
        let err =
            build_command_expression(CMD_FLUSH_CLASS, &[serde_json::json!("Counter; rm -rf /")])
                .expect_err("err");
        assert!(err.contains("invalid character"));
    }

    #[test]
    fn build_flush_class_rejects_empty() {
        let err =
            build_command_expression(CMD_FLUSH_CLASS, &[serde_json::json!("")]).expect_err("err");
        assert!(err.contains("must not be empty"));
    }

    #[test]
    fn build_flush_file_emits_symbol_keyed_dictionary() {
        let expr = build_command_expression(CMD_FLUSH_FILE, &[serde_json::json!("src/counter.bt")])
            .expect("ok");
        assert_eq!(expr, "Workspace flush: #{ #file => \"src/counter.bt\" }");
    }

    #[test]
    fn build_flush_file_escapes_quotes_and_braces() {
        // A pathological path with `"`, `\`, and `{` to confirm the
        // Beamtalk string escape rules match `beamtalk-mcp`.
        let expr = build_command_expression(CMD_FLUSH_FILE, &[serde_json::json!("a\\b\"c{d")])
            .expect("ok");
        assert_eq!(expr, "Workspace flush: #{ #file => \"a\\\\b\\\"c\\{d\" }");
    }

    #[test]
    fn build_flush_kind_accepts_bare_kind() {
        let expr = build_command_expression(CMD_FLUSH_KIND, &[serde_json::json!("new-class")])
            .expect("ok");
        assert_eq!(expr, "Workspace flush: #'new-class'");
    }

    #[test]
    fn build_flush_kind_accepts_quoted_symbol() {
        let expr = build_command_expression(CMD_FLUSH_KIND, &[serde_json::json!("#'new-class'")])
            .expect("ok");
        assert_eq!(expr, "Workspace flush: #'new-class'");
    }

    #[test]
    fn build_flush_kind_rejects_invalid_chars() {
        let err = build_command_expression(CMD_FLUSH_KIND, &[serde_json::json!("new class")])
            .expect_err("err");
        assert!(err.contains("must be an identifier"));
    }

    #[test]
    fn build_save_class_emits_workspace_newclass_expression() {
        let source = "Object subclass: Greeter\n  instanceMethods\n  greet => 'hi'";
        let path = "src/greeter.bt";
        let expr = build_command_expression(
            CMD_SAVE_CLASS,
            &[serde_json::json!(source), serde_json::json!(path)],
        )
        .expect("ok");
        // Roundtrip the source through the Beamtalk escaper.
        let expected_source = escape_string_literal(source);
        let expected_path = escape_string_literal(path);
        assert_eq!(
            expr,
            format!("Workspace newClass: \"{expected_source}\" at: \"{expected_path}\"")
        );
    }

    #[test]
    fn build_save_class_rejects_empty_source() {
        let err = build_command_expression(
            CMD_SAVE_CLASS,
            &[serde_json::json!(""), serde_json::json!("src/x.bt")],
        )
        .expect_err("err");
        assert!(err.contains("source"));
    }

    #[test]
    fn build_save_class_rejects_empty_path() {
        let err = build_command_expression(
            CMD_SAVE_CLASS,
            &[
                serde_json::json!("Object subclass: X"),
                serde_json::json!(""),
            ],
        )
        .expect_err("err");
        assert!(err.contains("path"));
    }

    #[test]
    fn build_save_class_accepts_object_wrapped_arguments() {
        // LSP clients sometimes pack arguments as `{"source": "...", "path":
        // "..."}` instead of a positional `[source, path]`. The handler
        // accepts either shape.
        let expr = build_command_expression(
            CMD_SAVE_CLASS,
            &[
                serde_json::json!({"source": "Object subclass: X"}),
                serde_json::json!({"path": "src/x.bt"}),
            ],
        )
        .expect("ok");
        assert!(expr.starts_with("Workspace newClass: \"Object subclass: X\""));
        assert!(expr.ends_with("at: \"src/x.bt\""));
    }

    #[test]
    fn build_precheck_method_emits_precheck_compile_source_expression() {
        let expr = build_command_expression(
            CMD_PRECHECK_METHOD,
            &[
                serde_json::json!("Counter"),
                serde_json::json!("getCount"),
                serde_json::json!("getCount => \"nope\""),
            ],
        )
        .expect("ok");
        assert_eq!(
            expr,
            "Counter precheckCompile: #getCount source: \"getCount => \\\"nope\\\"\""
        );
    }

    #[test]
    fn build_precheck_method_accepts_leading_hash_selector() {
        // Mirrors MCP's `precheck_method`: selectors are accepted with or
        // without a leading '#'.
        let expr = build_command_expression(
            CMD_PRECHECK_METHOD,
            &[
                serde_json::json!("Counter"),
                serde_json::json!("#getCount"),
                serde_json::json!("getCount => \"nope\""),
            ],
        )
        .expect("ok");
        assert_eq!(
            expr,
            "Counter precheckCompile: #getCount source: \"getCount => \\\"nope\\\"\""
        );
    }

    #[test]
    fn build_precheck_method_rejects_bad_class_name() {
        let err = build_command_expression(
            CMD_PRECHECK_METHOD,
            &[
                serde_json::json!("not a class"),
                serde_json::json!("getCount"),
                serde_json::json!("getCount => 1"),
            ],
        )
        .expect_err("err");
        assert!(err.contains("class name"));
    }

    #[test]
    fn build_precheck_method_rejects_bad_selector() {
        let err = build_command_expression(
            CMD_PRECHECK_METHOD,
            &[
                serde_json::json!("Counter"),
                serde_json::json!("bad selector!"),
                serde_json::json!("getCount => 1"),
            ],
        )
        .expect_err("err");
        assert!(err.contains("selector"));
    }

    #[test]
    fn build_precheck_method_rejects_empty_source() {
        let err = build_command_expression(
            CMD_PRECHECK_METHOD,
            &[
                serde_json::json!("Counter"),
                serde_json::json!("getCount"),
                serde_json::json!(""),
            ],
        )
        .expect_err("err");
        assert!(err.contains("source"));
    }

    #[test]
    fn build_recheck_image_emits_workspace_recheckimage_expression() {
        let expr = build_command_expression(CMD_RECHECK_IMAGE, &[]).expect("ok");
        assert_eq!(expr, "Workspace recheckImage");
    }

    #[test]
    fn build_recheck_image_rejects_arguments() {
        let err = build_command_expression(CMD_RECHECK_IMAGE, &[serde_json::json!("x")])
            .expect_err("err");
        assert!(err.contains("expected no arguments"));
    }

    #[test]
    fn build_unknown_command_returns_error() {
        let err = build_command_expression("not.a.real.command", &[]).expect_err("err");
        assert!(err.contains("unknown LSP command"));
    }

    #[test]
    fn validate_class_name_accepts_pascal_case() {
        assert!(validate_class_name("Counter").is_ok());
        assert!(validate_class_name("FooBarBaz").is_ok());
        assert!(validate_class_name("X123").is_ok());
        assert!(validate_class_name("X_y").is_ok());
    }

    #[test]
    fn validate_class_name_rejects_bad_shapes() {
        assert!(validate_class_name("").is_err());
        assert!(validate_class_name("lowercase").is_err());
        assert!(validate_class_name("With Space").is_err());
        assert!(validate_class_name("Bad!").is_err());
        assert!(validate_class_name("123Foo").is_err());
    }

    #[test]
    fn beamtalk_lsp_commands_list_matches_constants() {
        // Surface-parity drift check fodder: the constants drive the
        // capability list. Verify there are no accidental omissions.
        let listed: HashSet<&str> = BEAMTALK_LSP_COMMANDS.iter().copied().collect();
        assert!(listed.contains(CMD_FLUSH));
        assert!(listed.contains(CMD_FLUSH_CLASS));
        assert!(listed.contains(CMD_FLUSH_FILE));
        assert!(listed.contains(CMD_FLUSH_KIND));
        assert!(listed.contains(CMD_SAVE_CLASS));
        assert!(listed.contains(CMD_PRECHECK_METHOD));
        assert!(listed.contains(CMD_RECHECK_IMAGE));
        assert_eq!(listed.len(), 7);
    }

    #[test]
    fn validate_selector_accepts_unary_keyword_and_binary() {
        assert!(validate_selector("size").is_ok());
        assert!(validate_selector("at:put:").is_ok());
        assert!(validate_selector("+").is_ok());
        assert!(validate_selector(">=").is_ok());
    }

    #[test]
    fn validate_selector_rejects_bad_shapes() {
        assert!(validate_selector("").is_err());
        assert!(validate_selector("bad selector!").is_err());
        assert!(validate_selector("+foo").is_err());
    }

    #[test]
    fn resolve_flushed_path_canonicalises_absolute_paths_against_existing_file() {
        let temp = unique_temp_dir("beamtalk_lsp_resolve_abs");
        fs::create_dir_all(&temp).expect("create temp");
        let bt = temp.join("counter.bt");
        fs::write(&bt, "src").expect("write");
        let raw = bt.to_str().unwrap();
        let resolved = resolve_flushed_path(raw, &[]).expect("resolved");
        // canonicalize may resolve symlinks; just verify equality with the
        // canonical form of the same input.
        let canon = bt.canonicalize().expect("canonicalize");
        assert_eq!(resolved, canon);
        let _ = fs::remove_dir_all(&temp);
    }

    #[test]
    fn resolve_flushed_path_joins_against_first_matching_workspace_root() {
        let temp = unique_temp_dir("beamtalk_lsp_resolve_root");
        let root = temp.join("project");
        fs::create_dir_all(root.join("src")).expect("dirs");
        let target = root.join("src/counter.bt");
        fs::write(&target, "src").expect("write");

        let resolved =
            resolve_flushed_path("src/counter.bt", std::slice::from_ref(&root)).expect("resolved");
        assert_eq!(resolved, target.canonicalize().expect("canonicalize"));
        let _ = fs::remove_dir_all(&temp);
    }

    #[test]
    fn resolve_flushed_path_returns_none_when_nothing_matches() {
        // A nonexistent relative path with no roots — nothing to find.
        assert!(resolve_flushed_path("does/not/exist.bt", &[]).is_none());
    }

    #[test]
    fn open_paths_handle_observes_late_inserts() {
        // ADR 0082 Phase 3 (BT-2289): the listener must see files opened
        // *after* the runtime client attached. Verify the handle reads the
        // live map, not a captured snapshot, by inserting after handle
        // creation and confirming `contains` flips from false to true.
        let versions: Arc<Mutex<HashMap<Utf8PathBuf, i32>>> = Arc::new(Mutex::new(HashMap::new()));
        let handle = OpenPathsHandle {
            versions: Arc::clone(&versions),
        };
        let path = Utf8PathBuf::from("/workspace/counter.bt");
        assert!(!handle.contains(&path));
        versions
            .lock()
            .expect("versions lock")
            .insert(path.clone(), 1);
        assert!(handle.contains(&path));
    }

    #[test]
    fn open_paths_handle_observes_late_removes() {
        // Symmetric to the late-inserts case: closing a file in the editor
        // should immediately flip `contains` back to false.
        let versions: Arc<Mutex<HashMap<Utf8PathBuf, i32>>> = Arc::new(Mutex::new(HashMap::new()));
        let handle = OpenPathsHandle {
            versions: Arc::clone(&versions),
        };
        let path = Utf8PathBuf::from("/workspace/counter.bt");
        versions
            .lock()
            .expect("versions lock")
            .insert(path.clone(), 1);
        assert!(handle.contains(&path));
        versions.lock().expect("versions lock").remove(&path);
        assert!(!handle.contains(&path));
    }

    // --- BT-2243: callHierarchy helpers ---

    /// `target_to_lsp_item` round-trips through `SerializedCallTarget::from_item`
    /// so the prepare → outgoing flow preserves enough context to walk the
    /// method body in the open document.
    #[test]
    fn call_target_round_trips_through_item_data() {
        use beamtalk_core::source_analysis::Span;
        // CLAUDE.md forbids hardcoded `/tmp/`. `unique_temp_dir` keeps
        // the path Windows-safe and platform-portable; `path_to_uri`
        // turns it into a `file://` URI without a hand-written prefix.
        let temp = unique_temp_dir("beamtalk_lsp_callhier_roundtrip");
        let path = Utf8PathBuf::try_from(temp.join("counter.bt")).expect("utf8 path");
        let target = CallHierarchyTarget::new(
            "increment",
            Some("Counter".into()),
            false,
            path.clone(),
            Span::new(10, 50),
            Span::new(10, 19),
        );
        let uri = path_to_uri(&path).expect("uri");
        // The slice covers up to byte 50 — make the synthetic source at
        // least that long so `span_to_range` doesn't clamp inside our
        // expected range.
        let source = " ".repeat(60);
        let item = target_to_lsp_item(&target, &uri, &source).expect("item built");
        assert_eq!(item.name, "increment");
        assert_eq!(item.kind, SymbolKind::METHOD);
        assert_eq!(item.detail.as_deref(), Some("Counter"));
        let decoded = SerializedCallTarget::from_item(&item).expect("decoded");
        assert_eq!(decoded.selector, "increment");
        assert_eq!(decoded.class_name.as_deref(), Some("Counter"));
        assert!(!decoded.class_side);
        assert_eq!(decoded.range_start, 10);
        assert_eq!(decoded.range_end, 50);
        // Round-trip the file field against the temp path rather than a
        // hardcoded string; the specific bytes are irrelevant — only
        // that the path survived the encode/decode round trip.
        assert_eq!(decoded.file, path.as_str());
        let _ = std::fs::remove_dir_all(&temp);
    }

    /// Class-side targets surface a `Counter class` detail string so the
    /// editor distinguishes instance from class methods in the hover.
    #[test]
    fn call_target_class_side_detail_uses_class_suffix() {
        use beamtalk_core::source_analysis::Span;
        let temp = unique_temp_dir("beamtalk_lsp_callhier_classside");
        let path = Utf8PathBuf::try_from(temp.join("c.bt")).expect("utf8 path");
        let target = CallHierarchyTarget::new(
            "make",
            Some("Counter".into()),
            true,
            path.clone(),
            Span::new(0, 10),
            Span::new(0, 4),
        );
        let uri = path_to_uri(&path).expect("uri");
        let source = "x".repeat(20);
        let item = target_to_lsp_item(&target, &uri, &source).expect("item");
        assert_eq!(item.detail.as_deref(), Some("Counter class"));
        let decoded = SerializedCallTarget::from_item(&item).expect("decoded");
        assert!(decoded.class_side);
        let _ = std::fs::remove_dir_all(&temp);
    }

    /// `from_item` returns None on a call-site item that has no recorded
    /// data — defensive against editors that scrub the `data` field on
    /// round-trip.
    #[test]
    fn serialized_call_target_returns_none_when_data_missing() {
        // The URI value is irrelevant to this test (we only check the
        // `data` field), so use a non-platform-specific synthetic URI
        // rather than a `/tmp/` literal.
        let item = CallHierarchyItem {
            name: "foo".into(),
            kind: SymbolKind::METHOD,
            tags: None,
            detail: None,
            uri: Url::parse("file:///workspace/x.bt").expect("url"),
            range: Range {
                start: Position::new(0, 0),
                end: Position::new(0, 3),
            },
            selection_range: Range {
                start: Position::new(0, 0),
                end: Position::new(0, 3),
            },
            data: None,
        };
        assert!(SerializedCallTarget::from_item(&item).is_none());
    }

    /// `outgoing_calls_for_body` walks the slice and emits one
    /// `CallHierarchyOutgoingCall` per non-FFI send. Lines are offset by
    /// the body's starting line so absolute file lines come out right.
    #[test]
    fn outgoing_calls_for_body_emits_one_per_send_offset_by_body_start() {
        // URI is opaque to the call-hierarchy walker — the test only
        // verifies the returned items, so a generic non-`/tmp/` URI is
        // sufficient (CLAUDE.md cross-platform temp-path rule).
        let uri = Url::parse("file:///workspace/counter.bt").expect("url");
        // A simple method body with two sends on the same line, plus one
        // FFI call which must be filtered out.
        let body = "report =>\n  self show: \"hi\"\n  Erlang lists reverse: x";
        // Body starts on line 5 of the synthetic file (0-based).
        let body_start = Position::new(5, 0);
        let calls = outgoing_calls_for_body(body, body_start, &uri);
        // `find_all_sends_in_source` finds `show:` and (likely) one of
        // the Erlang sends; the FFI one must not appear. Verify at least
        // the show: send is present and at the right absolute line, and
        // no Erlang/reverse send leaks through.
        assert!(
            calls.iter().any(|c| c.to.name == "show:"),
            "expected show: in {calls:?}"
        );
        assert!(
            calls.iter().all(|c| c.to.name != "reverse:"),
            "Erlang reverse: leaked: {calls:?}"
        );
        // `show:` is on body-relative line 2 → absolute file line 6.
        let show = calls.iter().find(|c| c.to.name == "show:").unwrap();
        assert_eq!(show.to.range.start.line, 6);
    }

    /// An empty body produces no outgoing calls (defensive — the editor
    /// should get `None` from the handler in this case).
    #[test]
    fn outgoing_calls_for_body_empty_returns_no_calls() {
        let uri = Url::parse("file:///workspace/counter.bt").expect("url");
        let calls = outgoing_calls_for_body("answer => 42", Position::new(0, 0), &uri);
        assert!(calls.is_empty(), "got {calls:?}");
    }

    // -----------------------------------------------------------------
    // BT-2240: declaration-merge in `textDocument/references`.
    //
    // These tests exercise the cold-file (AST-fallback) path because the
    // runtime path requires a live `beamtalk_workspace` WebSocket and a
    // populated `beamtalk_xref` table — which the BT-2241 sister PR's
    // `delegate_nav_query` test fixture also avoids. The acceptance criterion
    // we cover here is the `includeDeclaration` round-trip semantics: with
    // the flag on, the result must include method-definition headers; with
    // the flag off, declarations must be filtered out and only call sites
    // remain. The runtime path inherits the same merge logic by construction
    // (`declaration_sites_for_query` always falls back to the AST helpers
    // when `delegate_to_runtime` is off, which is the default in tests).
    // -----------------------------------------------------------------

    /// Helper: open a single file in a `Backend` test fixture by directly
    /// updating the service and versions maps. Skips the full LSP lifecycle
    /// (`publish_diagnostics`, etc.) so handler logic can be exercised in
    /// isolation. Returns the URI that `textDocument/references` requests
    /// should target.
    fn open_test_file(backend: &Backend, path: &Utf8PathBuf, source: &str) -> Url {
        {
            let mut svc = backend.service.lock().expect("service lock");
            svc.update_file(path.clone(), source.to_string());
        }
        {
            let mut versions = backend.versions.lock().expect("versions lock");
            versions.insert(path.clone(), 1);
        }
        Url::from_file_path(path.as_std_path()).expect("path → uri")
    }

    fn references_params(
        uri: Url,
        line: u32,
        character: u32,
        include_declaration: bool,
    ) -> ReferenceParams {
        ReferenceParams {
            text_document_position: tower_lsp::lsp_types::TextDocumentPositionParams {
                text_document: tower_lsp::lsp_types::TextDocumentIdentifier { uri },
                position: tower_lsp::lsp_types::Position::new(line, character),
            },
            work_done_progress_params: tower_lsp::lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: tower_lsp::lsp_types::PartialResultParams::default(),
            context: tower_lsp::lsp_types::ReferenceContext {
                include_declaration,
            },
        }
    }

    #[tokio::test]
    async fn references_with_declaration_merges_method_definition_and_call_sites() {
        // Cold-file path: cursor on a selector at a call site. With
        // `includeDeclaration = true` the result must include the method
        // definition header **and** every call site (one of each here).
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();
        let path =
            Utf8PathBuf::from_path_buf(unique_temp_dir("bt-2240-with-decl").with_extension("bt"))
                .expect("temp path is UTF-8");
        let source = "Object subclass: Foo\n  bar => 1\nx bar\n";
        let uri = open_test_file(backend, &path, source);

        // Cursor on the `bar` call at line 2 (0-based), col 2.
        let result = backend
            .references(references_params(uri.clone(), 2, 2, true))
            .await
            .expect("rpc ok")
            .expect("some locations");

        // Expect at least 2 sites: the method-definition header on line 1
        // and the call on line 2.
        assert!(
            result.len() >= 2,
            "expected def + call (>= 2 sites), got {result:?}"
        );
        let lines: HashSet<u32> = result.iter().map(|l| l.range.start.line).collect();
        assert!(
            lines.contains(&1),
            "expected definition line 1, got {lines:?}"
        );
        assert!(
            lines.contains(&2),
            "expected call site line 2, got {lines:?}"
        );
    }

    #[tokio::test]
    async fn references_without_declaration_strips_method_definition() {
        // Same setup as above, but `includeDeclaration = false`: the
        // method-definition header must be filtered out and only the call
        // site should remain.
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();
        let path =
            Utf8PathBuf::from_path_buf(unique_temp_dir("bt-2240-no-decl").with_extension("bt"))
                .expect("temp path is UTF-8");
        let source = "Object subclass: Foo\n  bar => 1\nx bar\n";
        let uri = open_test_file(backend, &path, source);

        let result = backend
            .references(references_params(uri.clone(), 2, 2, false))
            .await
            .expect("rpc ok")
            .expect("some locations");

        let lines: HashSet<u32> = result.iter().map(|l| l.range.start.line).collect();
        assert!(
            !lines.contains(&1),
            "method-definition header (line 1) must be stripped when \
             includeDeclaration = false, got lines {lines:?}"
        );
        assert!(
            lines.contains(&2),
            "call site line 2 must remain, got {lines:?}"
        );
    }

    #[tokio::test]
    async fn references_with_declaration_includes_polymorphic_definitions() {
        // Two classes both define `ping`. From a call site, the merge
        // path must surface both definition headers (declaration-merge for
        // polymorphic selectors).
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();
        let path_foo =
            Utf8PathBuf::from_path_buf(unique_temp_dir("bt-2240-foo").with_extension("bt"))
                .expect("temp path is UTF-8");
        let path_bar =
            Utf8PathBuf::from_path_buf(unique_temp_dir("bt-2240-bar").with_extension("bt"))
                .expect("temp path is UTF-8");
        let path_call =
            Utf8PathBuf::from_path_buf(unique_temp_dir("bt-2240-call").with_extension("bt"))
                .expect("temp path is UTF-8");
        open_test_file(backend, &path_foo, "Object subclass: Foo\n  ping => 1\n");
        open_test_file(backend, &path_bar, "Object subclass: Bar\n  ping => 2\n");
        let uri_call = open_test_file(backend, &path_call, "x ping\n");

        let result = backend
            .references(references_params(uri_call, 0, 2, true))
            .await
            .expect("rpc ok")
            .expect("some locations");

        // Expect: Foo definition + Bar definition + the call site.
        let file_uris: HashSet<String> = result.iter().map(|l| l.uri.to_string()).collect();
        let foo_uri = Url::from_file_path(path_foo.as_std_path())
            .unwrap()
            .to_string();
        let bar_uri = Url::from_file_path(path_bar.as_std_path())
            .unwrap()
            .to_string();
        assert!(
            file_uris.contains(&foo_uri),
            "expected Foo definition, got {file_uris:?}"
        );
        assert!(
            file_uris.contains(&bar_uri),
            "expected Bar definition, got {file_uris:?}"
        );
    }

    #[test]
    fn merge_locations_dedupes_overlapping_runtime_and_decl_sites() {
        // The runtime path may legitimately return a site that the
        // declaration-overlay path would also produce (e.g. a method that
        // is its own only call site, or a future change that has
        // `senders_of/1` include definition rows). Verify the merge
        // helper does not duplicate them.
        let uri = Url::parse("file:///foo.bt").unwrap();
        let r = Range {
            start: tower_lsp::lsp_types::Position::new(1, 2),
            end: tower_lsp::lsp_types::Position::new(1, 5),
        };
        let mut base = vec![tower_lsp::lsp_types::Location {
            uri: uri.clone(),
            range: r,
        }];
        let extras = vec![tower_lsp::lsp_types::Location {
            uri: uri.clone(),
            range: r,
        }];
        merge_locations(&mut base, extras);
        assert_eq!(base.len(), 1, "duplicate site must collapse, got {base:?}");
    }

    #[test]
    fn merge_locations_appends_disjoint_decl_sites() {
        let uri = Url::parse("file:///foo.bt").unwrap();
        let r1 = Range {
            start: tower_lsp::lsp_types::Position::new(1, 0),
            end: tower_lsp::lsp_types::Position::new(1, 3),
        };
        let r2 = Range {
            start: tower_lsp::lsp_types::Position::new(7, 0),
            end: tower_lsp::lsp_types::Position::new(7, 3),
        };
        let mut base = vec![tower_lsp::lsp_types::Location {
            uri: uri.clone(),
            range: r1,
        }];
        let extras = vec![tower_lsp::lsp_types::Location {
            uri: uri.clone(),
            range: r2,
        }];
        merge_locations(&mut base, extras);
        assert_eq!(base.len(), 2);
        assert!(base.iter().any(|l| l.range == r1));
        assert!(base.iter().any(|l| l.range == r2));
    }

    /// BT-2241: `initialize` must advertise `implementation_provider` so
    /// clients enable goto-implementation. Pins the capability registration
    /// so a future refactor of the capabilities literal can't silently
    /// drop the binding (the surface-parity drift checker would catch
    /// the drop via `extract_lsp_caps`, but a dedicated unit test gives a
    /// faster signal at the change site).
    #[tokio::test]
    async fn initialize_advertises_implementation_provider() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();
        let result = backend
            .initialize(InitializeParams::default())
            .await
            .expect("initialize ok");
        assert!(
            matches!(
                result.capabilities.implementation_provider,
                Some(ImplementationProviderCapability::Simple(true))
            ),
            "expected implementation_provider = Simple(true), got {:?}",
            result.capabilities.implementation_provider
        );
    }

    /// BT-2241: AST-fallback path of `goto_implementation`. The runtime
    /// flag defaults to off in tests (no `initialize` with
    /// `delegateToRuntime`), so this exercises the cold-file walker via
    /// `SimpleLanguageService::find_implementors`. Two classes in the same
    /// in-memory file define `bar`; both method-header locations must come
    /// back when the cursor is on a call site for `bar`.
    #[tokio::test]
    async fn goto_implementation_returns_all_implementors_via_ast_fallback() {
        use tower_lsp::lsp_types::{
            PartialResultParams, TextDocumentIdentifier, TextDocumentPositionParams,
            WorkDoneProgressParams,
        };

        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        // Seed an in-memory file with two implementors of `bar` plus a call
        // site for `bar`. Use a `file://` URI so `resolve_path_for_uri`
        // returns the path key the service stores.
        let path = Utf8PathBuf::from_path_buf(
            unique_temp_dir("bt-2241-goto-impl-test").with_extension("bt"),
        )
        .expect("temp path is UTF-8");
        let source = "Object subclass: Foo\n  \
                      bar => 1\n\
                      Object subclass: Baz\n  \
                      bar => 2\n\
                      Object subclass: User\n  \
                      go => self bar\n";
        {
            let mut svc = backend.service.lock().expect("service lock poisoned");
            svc.update_file(path.clone(), source.to_string());
        }

        // Cursor on the `bar` call site (line 5, column 12 in the User
        // method body). Line/column are 0-based for LSP.
        let uri = Url::from_file_path(path.as_std_path()).expect("file URI");
        let params = tower_lsp::lsp_types::request::GotoImplementationParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: tower_lsp::lsp_types::Position::new(5, 12),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };

        let response = backend
            .goto_implementation(params)
            .await
            .expect("goto_implementation ok");

        let locations = match response {
            Some(GotoDefinitionResponse::Array(v)) => v,
            other => panic!("expected Array response, got {other:?}"),
        };
        assert_eq!(
            locations.len(),
            2,
            "expected one location per implementor (Foo + Baz), got {locations:?}"
        );
        // Both locations should be in the seeded file (the only one
        // indexed) and both ranges should be zero-width at column 0
        // (matching the runtime path's header-line anchor, BT-2241 review).
        let expected_uri = Url::from_file_path(path.as_std_path()).expect("file URI");
        for loc in &locations {
            assert_eq!(loc.uri, expected_uri, "unexpected URI in result");
            assert_eq!(
                loc.range.start, loc.range.end,
                "AST fallback should emit zero-width ranges to match runtime path"
            );
            assert_eq!(
                loc.range.start.character, 0,
                "AST fallback should anchor at column 0 of the header line"
            );
        }
    }

    /// BT-2241: cursor on a non-selector token (e.g. a `state:` declaration
    /// name) yields `None`. The runtime-attached mode would skip the hop
    /// (the classifier returns `None`), and the AST fallback would do the
    /// same — `goto_implementation` is selector-scoped by design.
    #[tokio::test]
    async fn goto_implementation_returns_none_for_non_selector_cursor() {
        use tower_lsp::lsp_types::{
            PartialResultParams, TextDocumentIdentifier, TextDocumentPositionParams,
            WorkDoneProgressParams,
        };

        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        let path = Utf8PathBuf::from_path_buf(
            unique_temp_dir("bt-2241-goto-impl-none").with_extension("bt"),
        )
        .expect("temp path is UTF-8");
        // A class body with a state declaration; cursor will land on the
        // state-variable name, which is neither a selector nor a class.
        let source = "Object subclass: Foo\n  \
                      state: counter = 0\n  \
                      bar => 1\n";
        {
            let mut svc = backend.service.lock().expect("service lock poisoned");
            svc.update_file(path.clone(), source.to_string());
        }

        let uri = Url::from_file_path(path.as_std_path()).expect("file URI");
        let params = tower_lsp::lsp_types::request::GotoImplementationParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                // Line 1 ("  state: counter = 0"), column 11 lands on the
                // `counter` identifier.
                position: tower_lsp::lsp_types::Position::new(1, 11),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };

        let response = backend
            .goto_implementation(params)
            .await
            .expect("goto_implementation ok");
        assert!(
            response.is_none(),
            "expected None for non-selector cursor, got {response:?}"
        );
    }

    /// BT-2242: `initialize` must advertise type-hierarchy support via the
    /// `experimental` channel (lsp-types 0.94.1 does not yet expose a typed
    /// `type_hierarchy_provider` field on `ServerCapabilities`).
    ///
    /// Pins the JSON shape so editors that look for
    /// `experimental.typeHierarchyProvider == true` continue to detect the
    /// capability after refactors of the capabilities literal.
    #[tokio::test]
    async fn initialize_advertises_type_hierarchy_provider_via_experimental() {
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();
        let result = backend
            .initialize(InitializeParams::default())
            .await
            .expect("initialize ok");
        let experimental = result
            .capabilities
            .experimental
            .expect("experimental field set");
        let provider = experimental
            .get("typeHierarchyProvider")
            .expect("typeHierarchyProvider key present");
        assert_eq!(
            provider,
            &serde_json::Value::Bool(true),
            "typeHierarchyProvider should be `true`, got {provider:?}"
        );
    }

    /// BT-2242: `prepare_type_hierarchy` on a known class name returns a
    /// single item carrying the class-name span. Exercises the AST-walker
    /// classifier (`type_hierarchy_prepare_at`) plus the LSP-side item
    /// construction.
    #[tokio::test]
    async fn prepare_type_hierarchy_resolves_class_under_cursor() {
        use tower_lsp::lsp_types::{
            TextDocumentIdentifier, TextDocumentPositionParams, WorkDoneProgressParams,
        };

        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        let path =
            Utf8PathBuf::from_path_buf(unique_temp_dir("bt-2242-prepare-th").with_extension("bt"))
                .expect("temp path is UTF-8");
        // Two user classes — Bar's superclass reference to Foo is the
        // cursor target. Click on the `Foo` token (line 2, col 0).
        let source = "Object subclass: Foo\n\
                      \n\
                      Foo subclass: Bar\n";
        {
            let mut svc = backend.service.lock().expect("service lock poisoned");
            svc.update_file(path.clone(), source.to_string());
        }
        let uri = Url::from_file_path(path.as_std_path()).expect("file URI");
        let params = TypeHierarchyPrepareParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                // Line 2 = "Foo subclass: Bar", column 1 lands on "Foo".
                position: tower_lsp::lsp_types::Position::new(2, 1),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
        };

        let items = backend
            .prepare_type_hierarchy(params)
            .await
            .expect("prepare ok")
            .expect("Some(items) for cursor on class name");

        assert_eq!(
            items.len(),
            1,
            "expected exactly one prepared item, got {items:?}"
        );
        assert_eq!(items[0].name, "Foo");
        assert_eq!(items[0].kind, SymbolKind::CLASS);
        // The URI must be the file containing the `Foo` declaration —
        // since the seeded file declares Foo on line 0, the item points
        // back at the seeded path.
        assert_eq!(items[0].uri, uri);
    }

    /// BT-2242: cursor not on a class name (e.g. inside the `subclass:`
    /// selector token) yields `None`. Selector tokens are owned by the
    /// implementors / senders queries, not type hierarchy.
    #[tokio::test]
    async fn prepare_type_hierarchy_returns_none_for_non_class_cursor() {
        use tower_lsp::lsp_types::{
            TextDocumentIdentifier, TextDocumentPositionParams, WorkDoneProgressParams,
        };

        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        let path = Utf8PathBuf::from_path_buf(
            unique_temp_dir("bt-2242-prepare-th-none").with_extension("bt"),
        )
        .expect("temp path is UTF-8");
        let source = "Object subclass: Foo\n  \
                      bar => 42\n";
        {
            let mut svc = backend.service.lock().expect("service lock poisoned");
            svc.update_file(path.clone(), source.to_string());
        }

        let uri = Url::from_file_path(path.as_std_path()).expect("file URI");
        // Line 1 ("  bar => 42"), column 4 — squarely on `bar` (a selector,
        // not a class name).
        let params = TypeHierarchyPrepareParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: tower_lsp::lsp_types::Position::new(1, 4),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
        };

        let response = backend
            .prepare_type_hierarchy(params)
            .await
            .expect("prepare ok");
        assert!(
            response.is_none(),
            "expected None for selector cursor, got {response:?}"
        );
    }

    /// BT-2242: `typeHierarchy/supertypes` returns the receiver's ancestor
    /// chain, in `Behaviour superclassChain` order. Two-level chain
    /// `Bar -> Foo -> Object -> ProtoObject` exercises the BFS-like
    /// ordering (direct parent first).
    #[tokio::test]
    async fn supertypes_returns_chain_in_inheritance_order() {
        use tower_lsp::lsp_types::{PartialResultParams, WorkDoneProgressParams};

        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        let path =
            Utf8PathBuf::from_path_buf(unique_temp_dir("bt-2242-supertypes").with_extension("bt"))
                .expect("temp path is UTF-8");
        let source = "Object subclass: Foo\n\
                      Foo subclass: Bar\n";
        {
            let mut svc = backend.service.lock().expect("service lock poisoned");
            svc.update_file(path.clone(), source.to_string());
        }

        // Build a synthetic TypeHierarchyItem for `Bar` (the editor would
        // get this from `prepare_type_hierarchy`; we hand-roll it here so
        // the test focuses on the supertypes resolver).
        let uri = Url::from_file_path(path.as_std_path()).expect("file URI");
        let zero_range = Range {
            start: tower_lsp::lsp_types::Position::new(0, 0),
            end: tower_lsp::lsp_types::Position::new(0, 0),
        };
        let item = TypeHierarchyItem {
            name: "Bar".to_string(),
            kind: SymbolKind::CLASS,
            tags: None,
            detail: None,
            uri,
            range: zero_range,
            selection_range: zero_range,
            data: None,
        };
        let params = TypeHierarchySupertypesParams {
            item,
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };

        let supers = backend
            .supertypes(params)
            .await
            .expect("supertypes ok")
            .expect("Some(items)");

        // `Bar` => Foo, Object, ProtoObject. The user file only declares
        // Foo; Object / ProtoObject come from the builtin hierarchy and
        // have no indexed declaration site, but the LSP layer still emits
        // a row for them (parent-URI fallback).
        let names: Vec<&str> = supers.iter().map(|s| s.name.as_str()).collect();
        assert_eq!(
            names,
            vec!["Foo", "Object", "ProtoObject"],
            "expected ordered ancestor chain"
        );
    }

    /// BT-2242: `typeHierarchy/subtypes` returns transitive descendants
    /// (BFS order — direct children before grandchildren) via
    /// `ClassHierarchy::all_subclasses`. Two-level tree exercises both
    /// levels and confirms the receiver itself is excluded.
    #[tokio::test]
    async fn subtypes_returns_transitive_descendants_in_bfs_order() {
        use tower_lsp::lsp_types::{PartialResultParams, WorkDoneProgressParams};

        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();

        let path =
            Utf8PathBuf::from_path_buf(unique_temp_dir("bt-2242-subtypes").with_extension("bt"))
                .expect("temp path is UTF-8");
        // Two-level tree: Foo -> {Bar, Baz}; Bar -> Qux.
        let source = "Object subclass: Foo\n\
                      Foo subclass: Bar\n\
                      Foo subclass: Baz\n\
                      Bar subclass: Qux\n";
        {
            let mut svc = backend.service.lock().expect("service lock poisoned");
            svc.update_file(path.clone(), source.to_string());
        }

        let uri = Url::from_file_path(path.as_std_path()).expect("file URI");
        let zero_range = Range {
            start: tower_lsp::lsp_types::Position::new(0, 0),
            end: tower_lsp::lsp_types::Position::new(0, 0),
        };
        let item = TypeHierarchyItem {
            name: "Foo".to_string(),
            kind: SymbolKind::CLASS,
            tags: None,
            detail: None,
            uri,
            range: zero_range,
            selection_range: zero_range,
            data: None,
        };
        let params = TypeHierarchySubtypesParams {
            item,
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };

        let subs = backend
            .subtypes(params)
            .await
            .expect("subtypes ok")
            .expect("Some(items)");

        let names: Vec<&str> = subs.iter().map(|s| s.name.as_str()).collect();
        // `all_subclasses` iteration order depends on hash-map iteration
        // for siblings at the same level — assert as a set for the direct
        // children and pin Qux's position after both direct children.
        assert_eq!(names.len(), 3, "expected 3 descendants, got {names:?}");
        assert!(
            names.contains(&"Bar"),
            "Bar should be in subtypes, got {names:?}"
        );
        assert!(
            names.contains(&"Baz"),
            "Baz should be in subtypes, got {names:?}"
        );
        let qux_pos = names.iter().position(|n| *n == "Qux").expect("Qux present");
        let bar_pos = names.iter().position(|n| *n == "Bar").expect("Bar present");
        assert!(
            qux_pos > bar_pos,
            "Qux (grandchild) must come after Bar (its parent) in BFS order, got {names:?}"
        );
        assert!(
            !names.contains(&"Foo"),
            "receiver Foo must not be in its own subtypes, got {names:?}"
        );
    }

    // -----------------------------------------------------------------
    // BT-2244: workspace/document symbol unification.
    //
    // The runtime path (`nav-symbols` over WebSocket) needs a live
    // workspace, which the LSP test harness doesn't spin up. These
    // tests target the pure conversion helpers
    // (`runtime_class_to_document_symbol`,
    // `runtime_class_to_workspace_symbol`, `zero_width_range_for_line`)
    // and the AST-fallback behaviour of the `document_symbol` and
    // `symbol` handlers — together they pin the parts of the dispatch
    // contract that don't require runtime attachment. (The runtime
    // dispatch itself is exercised end-to-end by the surface-drift +
    // Erlang EUnit tests in `beamtalk_repl_ops_nav_symbols_tests.erl`.)
    // -----------------------------------------------------------------

    use beamtalk_core::language_service::{NavSymbolClass as NSClass, NavSymbolMethod as NSMethod};

    #[test]
    fn zero_width_range_for_line_clamps_zero_to_row_zero() {
        // Defensive: the runtime should never emit line 0, but if it
        // does we render the symbol at row 0 rather than panicking on
        // a `0 - 1` underflow.
        let r = zero_width_range_for_line(0);
        assert_eq!(r.start, Position::new(0, 0));
        assert_eq!(r.end, Position::new(0, 0));
    }

    #[test]
    fn zero_width_range_for_line_converts_one_based_runtime_lines() {
        let r = zero_width_range_for_line(7);
        assert_eq!(r.start, Position::new(6, 0));
        assert_eq!(r.end, Position::new(6, 0));
    }

    // -----------------------------------------------------------------
    // ADR 0105 Phase 1 (BT-2779): reload-induced diagnostics
    // -----------------------------------------------------------------

    fn sample_reload_finding() -> crate::runtime::ReloadFinding {
        crate::runtime::ReloadFinding {
            owner: "Dashboard".to_string(),
            changed_class: "Counter".to_string(),
            selector: "getCount".to_string(),
            classification: "signature_change".to_string(),
            severity: "warning".to_string(),
            category: Some("Dnu".to_string()),
            message: "String does not understand '+'".to_string(),
            note: None,
            sites: vec![crate::runtime::ReloadSite {
                method: "refresh".to_string(),
                line: 14,
            }],
            start: 0,
            end: 5,
        }
    }

    #[test]
    fn reload_finding_to_lsp_diagnostics_one_per_site() {
        let mut finding = sample_reload_finding();
        finding.sites.push(crate::runtime::ReloadSite {
            method: "render".to_string(),
            line: 20,
        });
        let diags = reload_finding_to_lsp_diagnostics(&finding);
        assert_eq!(diags.len(), 2);
        assert_eq!(diags[0].range.start, Position::new(13, 0));
        assert_eq!(diags[0].range.end, Position::new(13, u32::MAX));
        assert_eq!(diags[1].range.start, Position::new(19, 0));
        assert!(diags[0].message.contains("getCount"));
        assert!(diags[0].message.contains("refresh"));
        assert!(diags[1].message.contains("render"));
    }

    #[test]
    fn reload_finding_to_lsp_diagnostics_maps_severity() {
        let mut finding = sample_reload_finding();
        finding.severity = "hint".to_string();
        let diags = reload_finding_to_lsp_diagnostics(&finding);
        assert_eq!(diags[0].severity, Some(DiagnosticSeverity::HINT));

        finding.severity = "warning".to_string();
        let diags = reload_finding_to_lsp_diagnostics(&finding);
        assert_eq!(diags[0].severity, Some(DiagnosticSeverity::WARNING));
    }

    #[test]
    fn reload_finding_to_lsp_diagnostics_appends_note() {
        let mut finding = sample_reload_finding();
        finding.classification = "removal".to_string();
        finding.note = Some("removed by the reload of Counter".to_string());
        let diags = reload_finding_to_lsp_diagnostics(&finding);
        assert!(
            diags[0]
                .message
                .contains("removed by the reload of Counter")
        );
    }

    #[test]
    fn reload_finding_to_lsp_diagnostics_carries_category_as_code() {
        let finding = sample_reload_finding();
        let diags = reload_finding_to_lsp_diagnostics(&finding);
        assert_eq!(
            diags[0].code,
            Some(tower_lsp::lsp_types::NumberOrString::String(
                "Dnu".to_string()
            ))
        );
    }

    #[test]
    fn group_findings_by_origin_splits_same_owner_different_changed_class() {
        // BT-2801: `seed_reload_diagnostics` must seed independently-clearing
        // entries — two findings attributed to the same owner but from
        // *different* reloaded classes are two distinct origins, not one
        // merged bucket, exactly mirroring `reload_check_listener`'s
        // per-origin bucketing (`ReloadDiagnosticsByUriAndOrigin`'s doc).
        let mut from_counter = sample_reload_finding();
        from_counter.owner = "Dashboard".to_string();
        from_counter.changed_class = "Counter".to_string();
        let mut from_widget = sample_reload_finding();
        from_widget.owner = "Dashboard".to_string();
        from_widget.changed_class = "Widget".to_string();

        let by_origin = group_findings_by_origin(vec![from_counter.clone(), from_widget.clone()]);

        assert_eq!(by_origin.len(), 2);
        assert_eq!(
            by_origin[&("Dashboard".to_string(), "Counter".to_string())],
            vec![from_counter]
        );
        assert_eq!(
            by_origin[&("Dashboard".to_string(), "Widget".to_string())],
            vec![from_widget]
        );
    }

    #[test]
    fn group_findings_by_origin_merges_same_owner_and_changed_class() {
        // Two findings sharing the exact same origin (e.g. two removed
        // selectors on the same reloaded class) must land in one bucket, so
        // `seed_reload_diagnostics` seeds one origin entry covering both —
        // matching `reload_check_listener`'s per-event grouping, which never
        // splits a single `(owner, changed_class)` origin across entries.
        let mut first = sample_reload_finding();
        first.selector = "getCount".to_string();
        let mut second = sample_reload_finding();
        second.selector = "reset".to_string();

        let by_origin = group_findings_by_origin(vec![first.clone(), second.clone()]);

        assert_eq!(by_origin.len(), 1);
        let bucket = &by_origin[&("Dashboard".to_string(), "Counter".to_string())];
        assert_eq!(bucket.len(), 2);
        assert_eq!(bucket[0].selector, "getCount");
        assert_eq!(bucket[1].selector, "reset");
    }

    #[test]
    fn group_findings_by_origin_empty_input_returns_empty_map() {
        assert!(group_findings_by_origin(vec![]).is_empty());
    }

    #[test]
    fn resolve_class_uri_resolves_absolute_source_file() {
        let temp = unique_temp_dir("bt-2779-resolve-class-uri");
        fs::create_dir_all(&temp).expect("create temp dir");
        let file_path = temp.join("dashboard.bt");
        fs::write(&file_path, "").expect("write file");

        let class = NSClass::new(
            "Dashboard",
            Some(file_path.to_str().expect("utf8").to_string()),
            Some(1),
            vec![],
        );
        let uri = resolve_class_uri(&class, &[]).expect("resolved uri");
        assert_eq!(uri.scheme(), "file");
        assert!(uri.path().ends_with("dashboard.bt"));

        let _ = fs::remove_dir_all(&temp);
    }

    #[test]
    fn resolve_class_uri_returns_none_without_source_file() {
        let class = NSClass::new("Dashboard", None, None, vec![]);
        assert!(resolve_class_uri(&class, &[]).is_none());
    }

    #[test]
    fn runtime_class_to_document_symbol_filters_classes_in_other_files() {
        // A `nav-symbols` reply lists every loaded class. The per-file
        // `documentSymbol` handler must drop classes that belong to a
        // different `source_file` than the requested URI.
        let tmp = beamtalk_core::test_helpers::unique_temp_dir("bt-2244-doc-sym-filter");
        std::fs::create_dir_all(&tmp).expect("create temp dir");
        let a_path = tmp.join("a.bt");
        let b_path = tmp.join("b.bt");
        std::fs::write(&a_path, "").expect("write a.bt");
        std::fs::write(&b_path, "").expect("write b.bt");

        let class_in_b = NSClass::new(
            "B",
            Some(b_path.to_str().unwrap().to_string()),
            Some(1),
            vec![],
        );
        let requested = Utf8PathBuf::from_path_buf(a_path).expect("utf8");
        let workspace_roots = vec![tmp.clone()];
        let out = runtime_class_to_document_symbol(class_in_b, &requested, &workspace_roots);
        assert!(out.is_none(), "class from b.bt must be filtered out");
    }

    #[test]
    fn runtime_class_to_document_symbol_drops_classes_without_source_file() {
        // Classes without a backing source file (REPL-loaded,
        // ClassBuilder) belong to `workspace/symbol`, not the per-file
        // outline.
        let tmp = beamtalk_core::test_helpers::unique_temp_dir("bt-2244-doc-sym-no-src");
        std::fs::create_dir_all(&tmp).expect("create temp dir");
        let path = tmp.join("counter.bt");
        std::fs::write(&path, "").expect("write counter.bt");

        let class = NSClass::new("ReplOnly", None, None, vec![]);
        let requested = Utf8PathBuf::from_path_buf(path).expect("utf8");
        let out = runtime_class_to_document_symbol(class, &requested, std::slice::from_ref(&tmp));
        assert!(out.is_none(), "source-less class must be filtered out");
    }

    #[test]
    fn runtime_class_to_document_symbol_emits_class_with_methods() {
        let tmp = beamtalk_core::test_helpers::unique_temp_dir("bt-2244-doc-sym-emit");
        std::fs::create_dir_all(&tmp).expect("create temp dir");
        let path = tmp.join("counter.bt");
        std::fs::write(&path, "").expect("write counter.bt");
        let path_str = path.to_str().unwrap().to_string();

        let class = NSClass::new(
            "Counter",
            Some(path_str),
            Some(1),
            vec![
                NSMethod::new("increment", false, Some(7)),
                NSMethod::new("withInitial:", true, Some(3)),
            ],
        );
        let requested = Utf8PathBuf::from_path_buf(path).expect("utf8");
        let sym = runtime_class_to_document_symbol(class, &requested, std::slice::from_ref(&tmp))
            .expect("class with matching source file should produce a symbol");

        assert_eq!(sym.name, "Counter (class)");
        assert_eq!(sym.kind, SymbolKind::CLASS);
        let children = sym.children.expect("Counter has methods");
        assert_eq!(children.len(), 2);
        let increment = children.iter().find(|c| c.name == "increment").unwrap();
        assert!(increment.detail.is_none());
        assert_eq!(increment.range.start, Position::new(6, 0));
        let with_initial = children.iter().find(|c| c.name == "withInitial:").unwrap();
        assert_eq!(with_initial.detail.as_deref(), Some("(class)"));
        assert_eq!(with_initial.range.start, Position::new(2, 0));
    }

    #[test]
    fn runtime_class_to_workspace_symbol_applies_query_filter() {
        let tmp = beamtalk_core::test_helpers::unique_temp_dir("bt-2244-ws-sym-filter");
        std::fs::create_dir_all(&tmp).expect("create temp dir");
        let path = tmp.join("counter.bt");
        std::fs::write(&path, "").expect("write counter.bt");
        let path_str = path.to_str().unwrap().to_string();

        let class = NSClass::new("Counter", Some(path_str), Some(1), vec![]);

        // Empty query — everything matches.
        let root_uri = Url::from_file_path(&tmp).ok();
        let s = runtime_class_to_workspace_symbol(
            &class,
            "",
            std::slice::from_ref(&tmp),
            root_uri.as_ref(),
        )
        .expect("empty query matches");
        assert_eq!(s.name, "Counter");

        // Case-insensitive substring — `count` matches `Counter`.
        let s2 = runtime_class_to_workspace_symbol(
            &class,
            "count",
            std::slice::from_ref(&tmp),
            root_uri.as_ref(),
        )
        .expect("substring matches");
        assert_eq!(s2.name, "Counter");

        // Non-matching query.
        let s3 = runtime_class_to_workspace_symbol(
            &class,
            "zzzz",
            std::slice::from_ref(&tmp),
            root_uri.as_ref(),
        );
        assert!(s3.is_none(), "non-matching query must drop the row");
    }

    #[test]
    fn runtime_class_to_workspace_symbol_surfaces_source_less_classes() {
        // The headline win of BT-2244: classes with no backing source
        // file (REPL-loaded, ClassBuilder) still appear in
        // `workspace/symbol`, anchored to the workspace-root URI with a
        // `(no source file)` detail string.
        let tmp = beamtalk_core::test_helpers::unique_temp_dir("bt-2244-ws-sym-nosrc");
        std::fs::create_dir_all(&tmp).expect("create temp dir");
        let root_uri = Url::from_file_path(&tmp).expect("root → uri");

        let class = NSClass::new("MyRunner", None, None, vec![]);
        let s = runtime_class_to_workspace_symbol(
            &class,
            "",
            std::slice::from_ref(&tmp),
            Some(&root_uri),
        )
        .expect("source-less class still surfaces");
        assert_eq!(s.name, "MyRunner");
        assert_eq!(s.location.range.start, Position::new(0, 0));
        // `container_name` carries the `(no source file)` marker (we
        // overload the field to render visibly distinct rows; the
        // alternative — `detail`, which isn't on `SymbolInformation`
        // — is unavailable in the LSP type).
        let container = s.container_name.as_deref();
        assert_eq!(container, Some("(no source file)"));
    }

    #[test]
    fn class_source_is_stdlib_keeps_source_less_classes() {
        // Source-less REPL/dynamic classes are *not* stdlib — they must
        // pass the filter so the headline win still works.
        let class = NSClass::new("MyRunner", None, None, vec![]);
        let stdlib_paths: HashSet<Utf8PathBuf> = HashSet::new();
        assert!(!class_source_is_stdlib(&class, &[], &stdlib_paths));
    }

    #[test]
    fn class_source_is_stdlib_keeps_user_classes() {
        // A class whose resolved source file is *not* in the stdlib set
        // is kept (this is the typical user-code path).
        let tmp = beamtalk_core::test_helpers::unique_temp_dir("bt-2244-stdlib-keep-user");
        std::fs::create_dir_all(&tmp).expect("create temp dir");
        let user_path = tmp.join("user.bt");
        std::fs::write(&user_path, "").expect("write user.bt");
        let class = NSClass::new(
            "UserClass",
            Some(user_path.to_str().unwrap().to_string()),
            Some(1),
            vec![],
        );
        let stdlib_paths: HashSet<Utf8PathBuf> = HashSet::new();
        assert!(!class_source_is_stdlib(
            &class,
            std::slice::from_ref(&tmp),
            &stdlib_paths
        ));
    }

    #[test]
    fn class_source_is_stdlib_filters_stdlib_classes() {
        // A class whose resolved source file *is* in the stdlib set is
        // filtered out — the runtime-attached `workspace/symbol`
        // consumer relies on this to keep its result set matching the
        // cold-file fallback (which only sees user files).
        let tmp = beamtalk_core::test_helpers::unique_temp_dir("bt-2244-stdlib-filter");
        std::fs::create_dir_all(&tmp).expect("create temp dir");
        let stdlib_path = tmp.join("Integer.bt");
        std::fs::write(&stdlib_path, "").expect("write Integer.bt");
        let utf8 = Utf8PathBuf::from_path_buf(stdlib_path.clone()).expect("utf8");
        let stdlib_paths: HashSet<Utf8PathBuf> = std::iter::once(utf8).collect();
        let class = NSClass::new(
            "Integer",
            Some(stdlib_path.to_str().unwrap().to_string()),
            Some(1),
            vec![],
        );
        assert!(class_source_is_stdlib(
            &class,
            std::slice::from_ref(&tmp),
            &stdlib_paths
        ));
    }

    #[test]
    fn runtime_class_to_workspace_symbol_drops_source_less_without_root() {
        // No workspace root configured → nowhere safe to anchor the
        // source-less row, so drop it rather than emit an invalid URI.
        let class = NSClass::new("Orphan", None, None, vec![]);
        let s = runtime_class_to_workspace_symbol(&class, "", &[], None);
        assert!(
            s.is_none(),
            "source-less class without a root URI must drop the row"
        );
    }

    #[tokio::test]
    async fn document_symbol_uses_ast_for_untitled_uri() {
        // BT-2244 review fix: `document_symbol` must bypass the runtime
        // (`delegate_nav_symbols`) path whenever the URI is not a clean
        // `file://` document.  An `untitled:` buffer has no `source_file`
        // in the live class registry, so a `scope = "user"` runtime
        // query would filter every class out and return an empty
        // outline. The handler should take the AST fallback directly
        // and return the buffer's parsed symbols, even when
        // `delegateToRuntime` is on.
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();
        // Mimic what `did_open` does for an `untitled:` URI: the path
        // key is `__untitled__/<name>`.
        let path = Utf8PathBuf::from("__untitled__/scratch.bt");
        let source = "Object subclass: Counter\n  increment => 1\n  value => 2";
        {
            let mut svc = backend.service.lock().expect("service lock");
            svc.update_file(path.clone(), source.to_string());
        }
        {
            let mut versions = backend.versions.lock().expect("versions lock");
            versions.insert(path.clone(), 1);
        }
        // Flip the flag on so the bypass guard is the *only* reason the
        // runtime path isn't taken. (Without the bypass, a delegate
        // path with no attached runtime would still fall back to AST,
        // so the assertion below couldn't distinguish — but the flag
        // being on makes the guard observable as the early-return.)
        backend.set_delegate_to_runtime(true);

        let untitled_uri = Url::parse("untitled:scratch.bt").expect("untitled uri parses");
        let params = DocumentSymbolParams {
            text_document: tower_lsp::lsp_types::TextDocumentIdentifier { uri: untitled_uri },
            work_done_progress_params: tower_lsp::lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: tower_lsp::lsp_types::PartialResultParams::default(),
        };
        let response = backend.document_symbol(params).await.expect("rpc ok");
        let DocumentSymbolResponse::Nested(symbols) = response.expect("Some(symbols)") else {
            panic!("expected nested response");
        };
        assert_eq!(
            symbols.len(),
            1,
            "untitled buffer should yield the AST outline"
        );
        assert_eq!(symbols[0].name, "Counter (class)");
        let children = symbols[0].children.as_ref().expect("methods present");
        let names: Vec<&str> = children.iter().map(|c| c.name.as_str()).collect();
        assert!(names.contains(&"increment"), "got {names:?}");
        assert!(names.contains(&"value"), "got {names:?}");
    }

    #[tokio::test]
    async fn document_symbol_falls_back_to_ast_when_flag_off() {
        // With `delegateToRuntime` false (the default), `document_symbol`
        // returns the AST walker's outline byte-for-byte — same shape
        // the pre-BT-2244 implementation produced. This pins the
        // "no behaviour change when off" contract.
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();
        let path = Utf8PathBuf::from_path_buf(
            unique_temp_dir("bt-2244-doc-sym-fallback").with_extension("bt"),
        )
        .expect("utf8 path");
        let source = "Object subclass: Counter\n  increment => 1\n  value => 2";
        let uri = open_test_file(backend, &path, source);
        assert!(!backend.delegate_to_runtime());

        let params = DocumentSymbolParams {
            text_document: tower_lsp::lsp_types::TextDocumentIdentifier { uri },
            work_done_progress_params: tower_lsp::lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: tower_lsp::lsp_types::PartialResultParams::default(),
        };
        let response = backend.document_symbol(params).await.expect("rpc ok");
        let DocumentSymbolResponse::Nested(symbols) = response.expect("Some(symbols)") else {
            panic!("expected nested response");
        };
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "Counter (class)");
        let children = symbols[0].children.as_ref().expect("methods present");
        let names: Vec<&str> = children.iter().map(|c| c.name.as_str()).collect();
        assert!(names.contains(&"increment"), "got {names:?}");
        assert!(names.contains(&"value"), "got {names:?}");
    }

    #[tokio::test]
    async fn workspace_symbol_falls_back_to_ast_when_flag_off() {
        // Cold-file path matches the BT-2081 behaviour exactly — one
        // SymbolInformation per top-level class whose name matches the
        // (case-insensitive substring) query, drawn from the indexed
        // user files.
        let (service, _socket) = tower_lsp::LspService::new(Backend::new);
        let backend: &Backend = service.inner();
        let path = Utf8PathBuf::from_path_buf(
            unique_temp_dir("bt-2244-ws-sym-fallback").with_extension("bt"),
        )
        .expect("utf8 path");
        let source = "Object subclass: Counter\n  increment => 1";
        let _uri = open_test_file(backend, &path, source);
        assert!(!backend.delegate_to_runtime());

        // Empty query — matches every user class.
        let params = WorkspaceSymbolParams {
            query: String::new(),
            work_done_progress_params: tower_lsp::lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: tower_lsp::lsp_types::PartialResultParams::default(),
        };
        let response = backend.symbol(params).await.expect("rpc ok");
        let syms = response.expect("Some(symbols)");
        let names: Vec<&str> = syms.iter().map(|s| s.name.as_str()).collect();
        assert!(
            names.contains(&"Counter"),
            "fallback should find Counter, got {names:?}"
        );

        // Case-insensitive substring filter.
        let params2 = WorkspaceSymbolParams {
            query: "count".to_string(),
            work_done_progress_params: tower_lsp::lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: tower_lsp::lsp_types::PartialResultParams::default(),
        };
        let response2 = backend.symbol(params2).await.expect("rpc ok");
        let syms2 = response2.expect("Some(symbols)");
        assert_eq!(syms2.len(), 1);
        assert_eq!(syms2[0].name, "Counter");

        // Non-matching query.
        let params3 = WorkspaceSymbolParams {
            query: "zzzz".to_string(),
            work_done_progress_params: tower_lsp::lsp_types::WorkDoneProgressParams::default(),
            partial_result_params: tower_lsp::lsp_types::PartialResultParams::default(),
        };
        let response3 = backend.symbol(params3).await.expect("rpc ok");
        assert!(response3.is_none(), "non-matching query → None");
    }
}
