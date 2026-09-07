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
    ClassChangedEvent, FlushEvent, FlushFileKind, FlushedFile, ReloadCheckEvent, ReloadFinding,
    RuntimeClient, RuntimeError,
};

use beamtalk_core::semantic_analysis::ClassHierarchy;
use beamtalk_core::source_analysis::{Severity, Span};
use beamtalk_core::tool_expr::{
    FlushFilter, flush_expr, precheck_method_expr, remove_method_expr,
    remove_method_if_absent_expr, save_class_expr,
};
use beamtalk_core::unparse::format_source;
use beamtalk_language_service::queries::all_sends_query::{ReceiverKind, find_all_sends_in_source};
use beamtalk_language_service::{
    CallHierarchyTarget, CompletionKind, DocumentSymbolKind, LanguageService,
    Location as BtLocation, NavQuery, NavSite, NavSymbolClass, Position as BtPosition,
    RuntimeLocation, SimpleLanguageService, nav_site_to_location,
};
use camino::Utf8PathBuf;
use ecow::EcoString;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::notification::Notification as LspNotification;
use tower_lsp::lsp_types::{
    CallHierarchyIncomingCall, CallHierarchyIncomingCallsParams, CallHierarchyItem,
    CallHierarchyOptions, CallHierarchyOutgoingCall, CallHierarchyOutgoingCallsParams,
    CallHierarchyPrepareParams, CallHierarchyServerCapability, CodeAction, CodeActionKind,
    CodeActionOrCommand, CodeActionParams, CodeActionProviderCapability, CodeActionResponse,
    CompletionItem, CompletionItemKind, CompletionOptions, CompletionParams, CompletionResponse,
    CreateFile, CreateFileOptions, DeleteFile, DeleteFileOptions, DiagnosticSeverity,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, DocumentChangeOperation, DocumentChanges, DocumentFormattingParams,
    DocumentRangeFormattingParams, DocumentSymbolParams, DocumentSymbolResponse, Documentation,
    ExecuteCommandOptions, ExecuteCommandParams, FoldingRange, FoldingRangeKind,
    FoldingRangeParams, FoldingRangeProviderCapability, GotoDefinitionParams,
    GotoDefinitionResponse, Hover, HoverContents, HoverParams, HoverProviderCapability,
    ImplementationProviderCapability, InitializeParams, InitializeResult, InitializedParams,
    MarkupContent, MarkupKind, MessageType, OneOf, OptionalVersionedTextDocumentIdentifier,
    ParameterInformation, ParameterLabel, Position, Range, ReferenceParams, ResourceOp,
    ServerCapabilities, SignatureHelp, SignatureHelpOptions, SignatureHelpParams,
    SignatureInformation, SymbolInformation, SymbolKind, TextDocumentEdit,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    TextDocumentSyncSaveOptions, TextEdit, TypeHierarchyItem, TypeHierarchyPrepareParams,
    TypeHierarchySubtypesParams, TypeHierarchySupertypesParams, Url, WorkDoneProgressOptions,
    WorkspaceEdit, WorkspaceSymbolParams,
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
/// ADR 0112 Phase 4 (BT-3188): remove a method from a class
/// (`Behaviour>>removeSelector:` / `removeSelector:ifAbsent:`). Its
/// expression shape is shared with MCP's `remove_method` tool via
/// `beamtalk_core::tool_expr::remove_method_expr` (BT-3193) — the two can't
/// drift, since both call the same function.
pub(crate) const CMD_REMOVE_METHOD: &str = "beamtalk.removeMethod";

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
    CMD_REMOVE_METHOD,
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
///   (e.g. `behaviour.bt`, `workspace_interface.bt`), so two different caller
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

/// Params for the `beamtalk-lsp/documentMoved` custom notification (ADR 0114
/// LSP follow-up, BT-3285) — see [`DocumentMoved`] for why it exists.
/// `#[serde(rename_all = "camelCase")]` matches every other LSP payload
/// (e.g. `lsp_types::RenameFile`'s `oldUri`/`newUri`), even though this type
/// isn't itself part of the upstream `lsp_types` crate.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DocumentMovedParams {
    /// The `file://` URI the class's declaration lived at before the
    /// `renameTo:`/`moveClass:to:` flush moved it. Already gone from disk by
    /// the time this notification is sent — see [`apply_rename_class_move`].
    pub old_uri: Url,
    /// The `file://` URI the class's declaration now lives at.
    pub new_uri: Url,
}

/// A custom server-to-client LSP notification (ADR 0114 LSP follow-up,
/// BT-3285): `beamtalk-lsp/documentMoved`, `{oldUri, newUri}`.
///
/// **Why this exists:** a `renameTo:`/`moveClass:to:` flush's
/// `workspace/applyEdit` used to include a typed `RenameFile` resource
/// operation (`old_uri -> new_uri`), so an open editor tab would follow the
/// move. By the time that edit reaches the client, though, the runtime has
/// already renamed the file on disk and unlinked `old_uri` — and VS Code's
/// own `RenameOperation.perform()` treats "target already exists, source
/// already gone" as "nothing to do" and silently skips the move step
/// entirely (no error, but no editor-state retargeting either). See BT-3285
/// for the investigation and docs/ADR/0114-class-and-method-rename.md's LSP
/// section for the resulting design.
///
/// This notification is this project's first-party VS Code extension's
/// replacement mechanism: it doesn't ask the client to perform a filesystem
/// rename at all (there is nothing left to rename by the time it fires),
/// just tells a listening client which editor URI to retarget. The
/// extension (`editors/vscode/src/extension.ts`'s `handleDocumentMoved`,
/// mirroring the existing `beamtalk-lsp/fetchContent` custom-request
/// precedent used by `StdlibContentProvider`) closes any open tab at
/// `old_uri` and reopens `new_uri` in its place. Any other LSP client
/// ignores an unrecognised notification per the LSP spec, so it sees the
/// documented degraded (no-crash, no-retarget) outcome — the same one the
/// dropped `RenameFile` op silently produced in VS Code, just without the
/// misleading implication that a real filesystem rename request was made.
enum DocumentMoved {}

impl LspNotification for DocumentMoved {
    type Params = DocumentMovedParams;

    const METHOD: &'static str = "beamtalk-lsp/documentMoved";
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
    /// Per-path generation, bumped only by `did_open` (never by `did_change`)
    /// and read by `did_close` to detect a same-path reopen racing its own
    /// (I/O-bearing) disk re-index. Deliberately *not* `versions`: an
    /// ordinary `did_change` also bumps that map's value, and a version
    /// snapshot/recheck against it can't tell "this path was reopened —
    /// back off" apart from "this path was merely edited while its close was
    /// in flight — proceed, the edit is moot, we're closing regardless". A
    /// dedicated counter that only a genuine reopen touches makes that
    /// distinction unambiguous. See `did_close`'s doc for the full race.
    open_generation: Mutex<HashMap<Utf8PathBuf, u64>>,
    /// Source for `open_generation`'s values — monotonic and global (not
    /// per-path) is sufficient: `did_close` only ever compares one path's
    /// before/after value against itself, never across paths.
    next_open_generation: std::sync::atomic::AtomicU64,
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
            open_generation: Mutex::new(HashMap::new()),
            next_open_generation: std::sync::atomic::AtomicU64::new(0),
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

    /// Loads Erlang FFI type specs for each workspace root, live-extracting
    /// from OTP/dependency `.beam` files when `_build/type_cache/` is
    /// missing or stale rather than only reading whatever it happens to
    /// hold.
    ///
    /// ADR 0075 Phase 1 / BT-2859: Calls `beamtalk_core::ffi_type_specs::
    /// extract_type_specs` — the same single source of truth `beamtalk
    /// build`/`beamtalk lint` and the MCP `lint`/`diagnostic_summary` tools
    /// (BT-2858) use — instead of hand-parsing whatever JSON cache entries
    /// happen to be on disk. Before this, a workspace opened before any
    /// `beamtalk build` had run (or after `beamtalk clean`) got an
    /// empty `NativeTypeRegistry` for the rest of the session: no FFI
    /// argument-type diagnostics, and any `@expect type` suppressing one
    /// shown as stale. Runs on a blocking task since it may spawn a
    /// `beamtalk_build_worker` BEAM node on a cold/stale cache.
    async fn load_type_cache(&self, roots: &[PathBuf]) {
        use beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry;

        let roots_owned: Vec<PathBuf> = roots.to_vec();
        let registry = tokio::task::spawn_blocking(move || {
            let mut registry = NativeTypeRegistry::new();
            for root in &roots_owned {
                let Ok(root) = Utf8PathBuf::from_path_buf(root.clone()) else {
                    continue;
                };
                let cache_dir = root.join("_build").join("type_cache");
                let dependency_ebin_dirs =
                    beamtalk_core::ffi_type_specs::collect_project_dependency_ebin_dirs(&root);
                if let Some(root_registry) = beamtalk_core::ffi_type_specs::extract_type_specs(
                    &cache_dir,
                    &dependency_ebin_dirs,
                ) {
                    registry.merge(root_registry);
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

    /// Loads each workspace root's `beamtalk.toml` `[diagnostics]`
    /// severity-override table (ADR 0100 Rule 3, BT-2800) and installs it in
    /// the language service so `beamtalk build` and the LSP agree on
    /// diagnostic severity.
    ///
    /// Before this, the LSP's diagnostics path never consulted the table at
    /// all — a package with `dnu = "error"` failed `beamtalk build` while the
    /// editor kept showing the same site as a soft `Hint`. Parsing lives in
    /// `beamtalk-core` (`beamtalk_core::compilation::diagnostics_policy`),
    /// not `beamtalk-cli`, specifically so the LSP can read `beamtalk.toml`
    /// without a `beamtalk-lsp -> beamtalk-cli` dependency (forbidden — see
    /// `docs/development/architecture-principles.md`).
    ///
    /// Lenient by design: a root with no `beamtalk.toml`, an I/O error reading
    /// it (permissions, EISDIR, etc.), or one that fails to parse, contributes
    /// an empty table for that root (Rule 1 defaults) rather than blocking
    /// diagnostics entirely — a malformed manifest already fails loudly at
    /// `beamtalk build` time, and the LSP must keep publishing diagnostics for
    /// open files regardless. Non-`NotFound` I/O errors and parse failures are
    /// logged as `WARN` so the mismatch is discoverable. A multi-root workspace merges
    /// every root's table into one (later roots win on category collisions);
    /// like `set_has_package_dependencies`, this is a whole-session
    /// simplification, not a per-file lookup.
    ///
    /// Loaded once at startup (mirrors [`Self::load_type_cache`]) —
    /// `beamtalk.toml` edits made while the server is running require an LSP
    /// restart to take effect.
    async fn load_diagnostics_table(&self, roots: &[PathBuf]) {
        let roots_owned: Vec<PathBuf> = roots.to_vec();
        let table = tokio::task::spawn_blocking(move || {
            let mut merged = beamtalk_core::compilation::DiagnosticsTable::new();
            for root in &roots_owned {
                // Missing or unreadable manifest → empty table → no-op merge.
                // Parse errors are logged inside load_diagnostics_table_for_root.
                let root_table = beamtalk_core::compilation::load_diagnostics_table_for_root(root);
                for (category, severity) in root_table {
                    if let Some(previous) = merged.get(&category) {
                        if *previous != severity {
                            tracing::warn!(
                                root = %root.display(),
                                category = ?category,
                                previous = ?previous,
                                new = ?severity,
                                "[diagnostics] category set differently by multiple \
                                 workspace roots; this root's value wins for the \
                                 whole session"
                            );
                        }
                    }
                    merged.insert(category, severity);
                }
            }
            merged
        })
        .await
        .unwrap_or_default();

        if !table.is_empty() {
            debug!(
                "Loaded {} [diagnostics] severity override(s) from beamtalk.toml",
                table.len()
            );
        }
        let mut svc = self.service.lock().expect("service lock poisoned");
        svc.set_diagnostics_overrides(table);
    }

    /// Reads each workspace root's real `beamtalk.toml` `[package] name` and
    /// registers it with the `ProjectIndex` (BT-2960), so two distinct real
    /// packages opened as sibling workspace roots get distinct alias-package
    /// stamps instead of colliding on the same-project marker (BT-2951).
    ///
    /// A root with no manifest, an unparseable manifest, or no `[package]
    /// name` is simply omitted — [`ProjectIndex::package_for_alias_stamping`]
    /// falls back to the same-project marker for any file under an
    /// unregistered root, matching the pre-BT-2960 behavior for that root.
    ///
    /// Runs *before* [`Self::preload_workspace_source_files`] so preloaded
    /// files stamp correctly on first indexing, but the ordering is a
    /// fast-path optimisation, not a correctness requirement (BT-2961):
    /// `set_root_packages` re-stamps any already-indexed file's aliases, so
    /// a `didOpen`/`didChange` notification that races this call (tower-lsp
    /// does not serialize `initialized()` against notification handlers)
    /// is corrected here instead of keeping a stale same-project stamp
    /// until its next edit.
    async fn load_root_packages(&self, roots: &[PathBuf]) {
        use beamtalk_core::compilation::parse_package_name_from_manifest_toml;

        let roots_owned: Vec<PathBuf> = roots.to_vec();
        let root_packages: Vec<(Utf8PathBuf, EcoString)> = tokio::task::spawn_blocking(move || {
            roots_owned
                .into_iter()
                .filter_map(|root| {
                    let manifest_path = root.join("beamtalk.toml");
                    let content = std::fs::read_to_string(&manifest_path).ok()?;
                    let name = parse_package_name_from_manifest_toml(&content)?;
                    let utf8_root = Utf8PathBuf::from_path_buf(root).ok()?;
                    Some((utf8_root, EcoString::from(name)))
                })
                .collect()
        })
        .await
        .unwrap_or_default();

        if !root_packages.is_empty() {
            debug!(
                "Loaded {} workspace root package name(s) from beamtalk.toml",
                root_packages.len()
            );
        }
        let mut svc = self.service.lock().expect("service lock poisoned");
        svc.set_root_packages(root_packages);
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
        for (path, content) in loaded.user_files {
            let Ok(utf8_path) = Utf8PathBuf::from_path_buf(path) else {
                continue;
            };
            svc.update_file(utf8_path, content);
        }
        // BT-2959: stdlib files must be marked in the ProjectIndex before
        // indexing, not chained into the same loop as user_files above —
        // otherwise `is_stdlib_file` never returns true for them in the real
        // running LSP (only `ProjectIndex::with_stdlib`, a separate
        // constructor used by beamtalk-cli's build pipeline, did this), and
        // BT-2951's package stamping mis-tags stdlib aliases as same-project.
        for (path, content) in loaded.stdlib_files {
            let Ok(utf8_path) = Utf8PathBuf::from_path_buf(path) else {
                continue;
            };
            svc.mark_stdlib_file(utf8_path.clone());
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
            open_paths_handle.clone(),
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

        let reload_check_handle = self.spawn_reload_check_listener(
            client.clone(),
            reload_check_rx,
            open_paths_handle.clone(),
        );

        {
            let mut runtime_guard = self.runtime.lock().await;
            *runtime_guard = Some(client.clone());
        }
        // Cancel each prior listener (shouldn't happen, but be defensive)
        // before storing its replacement.
        Self::store_listener_handle(&self.flush_listener, listener_handle).await;
        Self::store_listener_handle(&self.class_changed_listener, class_changed_handle).await;
        Self::store_listener_handle(&self.reload_check_listener, reload_check_handle).await;

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
            &open_paths_handle,
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
        open_paths: OpenPathsHandle,
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
            open_paths,
        ))
    }

    /// Abort a previous listener task, if any, and store its replacement.
    /// Extracted out of `ensure_runtime_attached` (which repeats this for
    /// three listeners) purely to keep that function under the lint's
    /// line-count limit.
    async fn store_listener_handle(
        guard: &tokio::sync::Mutex<Option<tokio::task::JoinHandle<()>>>,
        handle: tokio::task::JoinHandle<()>,
    ) {
        let mut guard = guard.lock().await;
        if let Some(prev) = guard.take() {
            prev.abort();
        }
        *guard = Some(handle);
    }

    async fn publish_diagnostics(&self, uri: &Url) {
        // BT-3433: a didOpen/didChange/didSave racing the LSP's startup
        // workspace preload can compute diagnostics against a partially-
        // populated `ProjectIndex` (e.g. a sibling class not yet indexed),
        // producing a false `Unresolved class` warning. Sending it would
        // race `republish_open_diagnostics` — the self-healing pass the
        // startup sequence runs once preload completes — to be the last
        // `publishDiagnostics` notification the client sees for this URI,
        // and that race is not guaranteed to resolve in the correct
        // notification's favor. Skip the send entirely: every caller of
        // this method (`did_open`/`did_change`/`did_save`, plus
        // `republish_open_diagnostics` itself) has already recorded this
        // path as open (in `Backend::versions`) before calling here, so
        // `republish_open_diagnostics` is guaranteed to (re)publish it
        // once, correctly, after preload finishes — see
        // `is_preload_in_progress`'s doc for why this check and that
        // recording never race each other.
        //
        // This check lives here, not in the shared `publish_diagnostics_impl`
        // below, precisely because that invariant does *not* unconditionally
        // hold for its other two callers, `reload_check_listener` and
        // `seed_reload_diagnostics` — both target URIs that need not be open
        // in the editor (see their own comments), and unconditionally gating
        // their sends here would silently and permanently drop a
        // reload-induced diagnostic for a *closed* file, with nothing left to
        // resend it. Those two callers instead gate themselves conditionally
        // via `should_defer_reload_publish_for_preload` (BT-3433 follow-up):
        // deferring only when the target URI is *also* open, so the same
        // guarantee this method relies on — `republish_open_diagnostics`
        // resends every open path once preload completes — covers them too.
        {
            let svc = self.service.lock().expect("service lock poisoned");
            if svc.is_preload_in_progress() {
                return;
            }
        }
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
                // BT-3237: `textDocument/foldingRange` — one range per
                // `// === Name ===` section divider, AST-only (see
                // `Backend::folding_range`'s doc comment).
                folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
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
            // BT-3433: mark preload in-flight so a didOpen/didChange racing
            // this sequence defers its own `publish_diagnostics` to the
            // self-healing republish below instead of racing it to be the
            // last notification sent — see `is_preload_in_progress`'s doc.
            {
                let mut svc = self.service.lock().expect("service lock poisoned");
                svc.set_preload_in_progress(true);
            }
            // BT-2960: load each workspace root's real beamtalk.toml
            // [package] name before preload indexes files under that root,
            // so first-time stamping is already correct. BT-2961: this
            // ordering is best-effort, not a guarantee — a didOpen/didChange
            // racing this sequence still stamps eagerly with the fallback
            // marker, and set_root_packages/mark_stdlib_file re-stamp those
            // files when they run. (The [diagnostics] table below needs no
            // such care: it is applied per-request at diagnostic-computation
            // time.)
            self.load_root_packages(&config.roots).await;
            self.preload_workspace_source_files(config.clone()).await;
            // ADR 0075: Load type cache from _build/type_cache/ for typed completions.
            self.load_type_cache(&config.roots).await;
            // ADR 0100 Rule 3 / BT-2800: load beamtalk.toml's [diagnostics]
            // severity-override table so the LSP agrees with `beamtalk build`.
            self.load_diagnostics_table(&config.roots).await;
            // BT-3433: the project index is now fully populated (and won't
            // change again from this startup sequence) — clear the flag
            // *before* republishing below, so republish's own
            // `publish_diagnostics` calls actually send.
            {
                let mut svc = self.service.lock().expect("service lock poisoned");
                svc.set_preload_in_progress(false);
            }

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
            // Recorded *before* `update_file` so a concurrently dispatched
            // `did_close` for the same path (a close immediately followed by
            // a reopen) can observe "this path is open again" as early as
            // possible — see `did_close`'s own comment for why this specific
            // ordering, paired with the lock it holds there, is what makes
            // the reopen race-proof rather than just less likely.
            {
                let generation = self
                    .next_open_generation
                    .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                let mut open_generation = self
                    .open_generation
                    .lock()
                    .expect("open_generation lock poisoned");
                open_generation.insert(path.clone(), generation);
            }
            {
                let mut versions = self.versions.lock().expect("versions lock poisoned");
                versions.insert(path.clone(), params.text_document.version);
            }
            if uri.scheme() != "beamtalk-stdlib" {
                // Stdlib files are pre-loaded at startup; skip re-indexing.
                let mut svc = self.service.lock().expect("service lock poisoned");
                svc.update_file(path, params.text_document.text);
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
    ///
    /// The non-stdlib path below does a disk read that is not instantaneous,
    /// so a `did_open` for the same path (a close immediately followed by a
    /// reopen) can be dispatched concurrently and complete while this is
    /// still reading — applying the now-stale disk snapshot afterwards would
    /// silently clobber that fresher in-memory content. Guarded via
    /// [`Self::open_generation`]: snapshotted before the I/O-bearing work,
    /// rechecked inside the critical section that applies the update. A
    /// mismatch means a genuine reopen raced in and won, so this whole
    /// handler backs off entirely — leaving `svc`, `versions`, and the
    /// reopened document's diagnostics to it.
    ///
    /// Deliberately keyed on `open_generation`, not `versions`: an ordinary
    /// `did_change` racing the same close (e.g. a final keystroke, or a
    /// format-on-save edit, landing right before the tab closes) also bumps
    /// `versions`, but is not a reopen — the document is still closing
    /// either way, so its edit is moot and must not make this handler back
    /// off *permanently* (which would leak the exact eviction bug this
    /// method exists to fix, plus skip the dirty/diagnostic-generation
    /// cleanup and the empty-diagnostics publish below, forever). Only
    /// `did_open` bumps `open_generation`, so it alone can trigger the
    /// back-off.
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(path) = self.resolve_path_for_uri(&uri) {
            // Snapshotted *before* any of the (I/O-bearing, for the
            // non-stdlib branch) work below, so it reflects this document's
            // open-generation at the instant we started closing it — not a
            // later one a racing `did_open` on the same path might install.
            let generation_before = {
                let open_generation = self
                    .open_generation
                    .lock()
                    .expect("open_generation lock poisoned");
                open_generation.get(&path).copied()
            };
            let disk_content = if uri.scheme() == "beamtalk-stdlib" {
                None
            } else {
                // Startup preload (BT-2027) indexes every file under each
                // root's `src/`/`test/`/`_build/deps/*/src` regardless of
                // whether it is open, so the index is workspace-wide, not
                // "currently open files". Closing a tab therefore must not
                // evict the file — that silently removed its classes from
                // the merged hierarchy, and every other file referencing them
                // reported `Unresolved class` until the next restart. Revert
                // to the on-disk content instead (unsaved edits are discarded
                // on close, so disk is the truth). Files preload never
                // covered — scratch files outside those dirs, untitled
                // buffers, files deleted from disk — are still removed.
                let covered = {
                    let roots = self
                        .workspace_roots
                        .lock()
                        .expect("workspace_roots lock poisoned");
                    preload_covers(path.as_std_path(), &roots)
                };
                let is_stdlib = {
                    let svc = self.service.lock().expect("service lock poisoned");
                    svc.project_index().is_stdlib_file(&path)
                };
                let keep = covered || is_stdlib;
                // Read with no lock held, per this method's own doc.
                keep.then(|| fs::read_to_string(&path).ok()).flatten()
            };

            // Guard against the reopen race: hold `open_generation` across
            // both the check *and* the `svc`/`versions` mutation below, not
            // just the check — releasing it in between would reopen the
            // exact same window under a different name (a reopen landing
            // after we decide "safe to write" but before we actually write
            // would still get silently clobbered by our stale disk
            // snapshot). `did_open` records its path here *before* touching
            // `svc`/`versions` (see its own comment), so if a reopen's
            // `did_open` acquired this lock first, the generation check
            // below is guaranteed to see it and we back off entirely,
            // touching nothing. If we acquire this lock first instead,
            // `did_open`'s insert simply blocks until we release it (after
            // our own write below completes), so its own `update_file` call
            // is guaranteed to run after — and therefore win over — ours.
            let reopened = {
                let mut open_generation = self
                    .open_generation
                    .lock()
                    .expect("open_generation lock poisoned");
                let reopened = open_generation.get(&path).copied() != generation_before;
                if !reopened {
                    if uri.scheme() == "beamtalk-stdlib" {
                        let mut versions = self.versions.lock().expect("versions lock poisoned");
                        versions.remove(&path);
                    } else {
                        let mut svc = self.service.lock().expect("service lock poisoned");
                        match disk_content {
                            Some(content) => svc.update_file(path.clone(), content),
                            None => svc.remove_file(&path),
                        }
                        let mut versions = self.versions.lock().expect("versions lock poisoned");
                        versions.remove(&path);
                    }
                    open_generation.remove(&path);
                }
                reopened
            };
            if reopened {
                // Leave `svc`, `versions`, and this entry alone — they
                // belong to the reopen now.
                return;
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
                            beamtalk_language_service::queries::definition_provider::handle_call_clause_line(
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
        let (pos, runtime_query, incomplete_coverage_warning) = {
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
            // ADR 0108 Phase 8 (BT-2901): find-references coverage for a
            // type alias is explicitly scoped to files compiled into the
            // current build graph — an uncompiled or never-opened file
            // contributes no reference edges until it's compiled. When
            // workspace preload hasn't finished (`!is_project_complete()`),
            // a short or empty result must not silently read as exhaustive:
            // a caller trusting it could delete an alias a not-yet-compiled
            // file still uses. Surface that via `window/showMessage` below
            // rather than staying silent.
            //
            // BT-2919: a plain `alias_name_at` check alone misses the case
            // where the *alias's own declaring file* hasn't been indexed yet
            // — `Foo` in `policy :: Foo` doesn't look like a known alias if
            // `type Foo = ...` hasn't been compiled, even though the cursor
            // position proves it can only be a class/protocol/alias
            // reference. `has_incomplete_reference_coverage_at` closes that
            // gap (and the equivalent one for not-yet-indexed class names)
            // without needing the name to resolve first, in a single AST
            // walk of the cursor position.
            let incomplete_coverage_warning =
                !svc.is_project_complete() && svc.has_incomplete_reference_coverage_at(&path, pos);
            (pos, runtime_query, incomplete_coverage_warning)
        };

        if incomplete_coverage_warning {
            self.client
                .show_message(
                    MessageType::WARNING,
                    "Find-references for this name may be incomplete: not all workspace files \
                     have been compiled yet, so references in uncompiled files aren't included \
                     in this result.",
                )
                .await;
        }

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

    /// Returns folding ranges for a file: one per `// === Name ===` section
    /// divider category (BT-3237), plus one per class body and one per
    /// method body, instance- and class-side (BT-3260).
    ///
    /// AST-only — unlike `document_symbol`, there is no runtime-delegation
    /// path here (see `docs/development/surface-parity.md`'s `nav-symbols`
    /// row, which documents BT-2601's outline nesting as AST-only for the
    /// same reason: folding ranges are inter-method file structure, not a
    /// property surfaced by a loaded class with no source in hand).
    ///
    /// **BT-3260:** per the LSP folding spec's provider-registration model,
    /// registering `folding_range_provider` at all opts every `.bt` file
    /// out of VS Code's default indentation-based folding strategy once
    /// `editor.foldingStrategy` is `"auto"` (the default) — there is no
    /// per-region merge/fallback, and a proposed `disablesIndentation`
    /// provider opt-out flag exists upstream (microsoft/vscode#265661) but
    /// has not shipped. A divider-only provider (BT-3237's original scope)
    /// would therefore have silently regressed every divider-less file's
    /// per-class/per-method fold arrows the moment this capability was
    /// registered. To avoid that, `compute_folding_ranges` also emits a
    /// class-body range and one range per method body for **every** class,
    /// independent of whether it uses dividers — the indentation-equivalent
    /// fold points VS Code's built-in strategy would otherwise have
    /// offered. A class with dividers gets both the divider-category ranges
    /// and these body ranges (nested folding: class -> category -> method);
    /// a divider-less class gets only the body ranges, but still folds at
    /// every class/method exactly as indentation folding used to. A
    /// single-line class or method contributes no range of its own (nothing
    /// to collapse), matching indentation folding's own behavior.
    ///
    /// Returning `Ok(None)` (JSON `null`) rather than `Ok(Some(vec![]))` is
    /// load-bearing, not a style choice, for the one case that still
    /// produces zero ranges — a file with no classes, or none with anything
    /// multi-line to fold: `null` is how a `textDocument/foldingRange`
    /// response tells VS Code "this provider has nothing to say," which is
    /// what lets the editor fall back to its own indentation-based folding
    /// for that request. An empty array is a *real, if vacuous, answer* and
    /// editors are not obligated to fall back on one. Do not simplify away
    /// the `is_empty()` branch below.
    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        let uri = &params.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };

        let ranges = {
            let svc = self.service.lock().expect("service lock poisoned");
            let Some(src) = svc.file_source(&path) else {
                return Ok(None);
            };
            svc.folding_ranges(&path)
                .into_iter()
                .map(|span| span_to_folding_range(span, &src))
                .collect::<Vec<_>>()
        };

        if ranges.is_empty() {
            Ok(None)
        } else {
            Ok(Some(ranges))
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
    open_paths: OpenPathsHandle,
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
            //
            // BT-3433 (follow-up): if the URI *is* open and startup preload
            // is still in-flight, `publish_diagnostics_impl` would recompute
            // `svc.diagnostics()` against the same partially-populated
            // `ProjectIndex` that caused the original false `Unresolved
            // class` positive, and its send could still race
            // `republish_open_diagnostics`'s later, correct one — the same
            // channel-ordering race `Backend::publish_diagnostics` was fixed
            // to avoid, just reached through this listener instead of
            // `did_open`/`did_change`/`did_save`. Skip the send in that case:
            // the `reload_diagnostics` map above is already updated, and
            // `republish_open_diagnostics` is guaranteed to pick it up in its
            // one, correct publish once preload completes.
            if !should_defer_reload_publish_for_preload(&service, &open_paths, &uri) {
                publish_diagnostics_impl(&client, &service, &reload_diagnostics, &uri, None).await;
            }
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
    open_paths: &OpenPathsHandle,
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
        // BT-3433 (follow-up): same deferral as `reload_check_listener` —
        // `ensure_runtime_attached` (this function's only caller) runs
        // on-demand, independently of the startup preload sequence, so it
        // can race it. Skip the send for an open URI while preload is
        // in-flight; `reload_diagnostics` above is already updated, so
        // `republish_open_diagnostics` picks this seed up correctly once
        // preload completes.
        if !should_defer_reload_publish_for_preload(service, open_paths, &uri) {
            publish_diagnostics_impl(client, service, reload_diagnostics, &uri, None).await;
        }
    }
}

/// BT-3433 (follow-up): true if publishing diagnostics for `uri` right now
/// would risk the same startup-preload notification race
/// [`Backend::publish_diagnostics`] was fixed to avoid for
/// `did_open`/`did_change`/`did_save` — i.e. startup preload is still
/// in-flight *and* `uri` is currently open in the editor.
///
/// A closed-file URI is always safe to publish immediately: nothing else
/// (`republish_open_diagnostics` only iterates open files) will ever
/// resend it, so deferring would drop the diagnostic forever — matching
/// why `publish_diagnostics_impl` itself isn't gated unconditionally. An
/// open-file URI is safe to defer: `republish_open_diagnostics` is
/// guaranteed to (re)publish it, correctly, once preload finishes.
fn should_defer_reload_publish_for_preload(
    service: &Mutex<SimpleLanguageService>,
    open_paths: &OpenPathsHandle,
    uri: &Url,
) -> bool {
    let Some(path) = uri_to_path(uri) else {
        return false;
    };
    if !open_paths.contains(&path) {
        return false;
    }
    let svc = service.lock().expect("service lock poisoned");
    svc.is_preload_in_progress()
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

/// ADR 0082 Phase 3 (BT-2289); ADR 0113 Phase 4a (BT-3209) and LSP follow-up
/// (BT-3212); ADR 0114 LSP follow-up (BT-3275): consume `FlushEvent`s from
/// the runtime listener and emit `workspace/applyEdit` per flushed file.
///
/// For each file in the event, the listener:
/// 1. Resolves the runtime-reported path against the LSP workspace roots to
///    an absolute filesystem path ([`resolve_flushed_path`]), which also
///    reports whether the leaf still exists on disk — the fallback signal
///    used only when the wire carried no per-file `kind` (see below).
/// 2. Dispatches on the wire's per-file `kind` (BT-3212) directly when
///    present, before any filesystem probing:
///    - `rename-class` **with** an `oldFile` companion (BT-3275) — the one
///      file among a `'rename-class'` flush's touched files that IS the
///      moved declaration — routes to [`apply_rename_class_move`], never
///      through [`classify_flush_action`].
///    - `rename-class` **without** `oldFile` (an ordinary same-batch
///      reference rewrite in a file that did not itself move) and
///      `rename-method` (BT-3275, a definition or confirmed-sender site)
///      each route to their own branch below, likewise bypassing
///      [`classify_flush_action`].
///    - Everything else goes through [`classify_flush_action`]: `new-class`
///      -> `CreateFile`, `remove-class` -> `DeleteFile`, anything else ->
///      an ordinary patch. A producer that predates BT-3212 sends no `kind`
///      for a path; that case still falls back to BT-3209's original
///      existence heuristic (gone -> `DeleteFile`, still there -> patch), so
///      `CreateFile` is only ever reachable via an explicit wire `kind` —
///      the existence check alone can never tell "freshly created" from
///      "patched in place" (both leave the leaf present).
/// 3. **`DeleteFile`**: emits a [`DocumentChangeOperation::Op`] `DeleteFile`
///    resource operation ([`delete_file_edit`]) — unconditionally, not gated
///    on the open-paths check below, since a deletion is project-wide state
///    (an open tab that must close, stale diagnostics) rather than something
///    only an open buffer cares about.
/// 4. **`CreateFile`**: reads the freshly-written file (it already exists on
///    disk by the time this fires — Phase B already committed) and emits a
///    `CreateFile` resource operation paired with a `TextDocumentEdit`
///    carrying the full content ([`create_file_edit`]) — also unconditional,
///    since a brand-new file was by definition never open before this flush.
/// 5. **Rename-class move** (BT-3275; revised BT-3285): sends the custom
///    `beamtalk-lsp/documentMoved` notification ([`DocumentMoved`]) carrying
///    `{old_uri, new_uri}` — unconditional, mirroring `CreateFile`/
///    `DeleteFile`: a file move is project-wide state, not something only an
///    open buffer cares about. No `workspace/applyEdit` `RenameFile` op is
///    sent any more; see [`DocumentMoved`]'s doc for why.
/// 6. **Rename-method site** (BT-3275): checks the *live* open-paths handle
///    exactly like an ordinary patch (step 7) — every file this reaches was
///    an explicitly CONFIRMED site the caller already approved via
///    `confirmDestructive` (never a `candidate_sites` entry, which is never
///    staged/written and so never reaches this listener at all), but
///    "was this open" still gates whether refreshing the *editor buffer* is
///    worth doing — the on-disk bytes are already correct either way. Emits
///    a `TextDocumentEdit` via `documentChanges` ([`rename_method_site_edit`])
///    rather than the plain-`changes`-map shape ordinary patches use,
///    matching the ADR's "`TextDocumentEdit` per confirmed site" wording.
/// 7. **Patch** (an ordinary content edit, unchanged since ADR 0082 Phase 3):
///    checks the *live* open-paths handle to see whether the path is
///    currently open in the editor. Files that aren't open are skipped —
///    `VSCode` reads them fresh on next `did_open`. The check happens per
///    event so files opened after the listener started are still picked up.
///    Reads the new on-disk content and issues `apply_edit` with a single
///    `TextEdit` covering the whole document ([`change_file_edit`]), so the
///    open buffer realigns with the post-flush bytes. `VSCode`'s conflict UX
///    handles unsaved local edits per the LSP spec.
async fn flush_event_listener(
    client: Client,
    workspace_roots: Vec<PathBuf>,
    open_paths: OpenPathsHandle,
    mut flush_rx: tokio::sync::mpsc::UnboundedReceiver<FlushEvent>,
) {
    while let Some(event) = flush_rx.recv().await {
        for FlushedFile {
            path: raw_path,
            kind,
            old_path,
        } in event.files
        {
            let resolved = resolve_flushed_path(&raw_path, &workspace_roots);
            let Some((abs_path, existed)) = resolved else {
                tracing::debug!(
                    raw_path,
                    "flush_event_listener: could not resolve runtime path against workspace roots"
                );
                continue;
            };
            let Ok(uri) = Url::from_file_path(&abs_path) else {
                tracing::debug!(
                    ?abs_path,
                    "flush_event_listener: could not build file:// URI"
                );
                continue;
            };

            match (kind, old_path.as_deref()) {
                (Some(FlushFileKind::RenameClass), Some(old_raw)) => {
                    apply_rename_class_move(&client, &workspace_roots, uri, old_raw).await;
                }
                (Some(FlushFileKind::RenameMethod), _) => {
                    apply_rename_method_site(&client, &abs_path, &raw_path, uri, &open_paths).await;
                }
                _ => match classify_flush_action(kind, existed) {
                    FlushAction::Delete => apply_delete_file(&client, uri).await,
                    FlushAction::Create => apply_create_file(&client, &abs_path, uri).await,
                    FlushAction::Patch => {
                        apply_patch_file(&client, &abs_path, &raw_path, uri, &open_paths).await;
                    }
                },
            }
        }
    }
}

/// Send `edit` via `client.apply_edit` and log the outcome uniformly — shared
/// by the three `flush_event_listener` branches (`Delete`/`Create`/`Patch`,
/// ADR 0113 LSP follow-up BT-3212) so the three near-identical response-match
/// arms exist in exactly one place.
async fn apply_flush_edit(client: &Client, uri: &Url, edit: WorkspaceEdit, op_name: &str) {
    match client.apply_edit(edit).await {
        Ok(resp) if resp.applied => {
            tracing::debug!(%uri, op_name, "flush_event_listener: applied");
        }
        Ok(resp) => {
            tracing::info!(
                %uri,
                op_name,
                failure_reason = ?resp.failure_reason,
                "flush_event_listener: client declined applyEdit"
            );
        }
        Err(e) => {
            tracing::warn!(%uri, op_name, error = %e, "flush_event_listener: applyEdit failed");
        }
    }
}

/// `FlushAction::Delete` (BT-3209): unconditional — a deletion is
/// project-wide state, not gated on the open-paths check the patch branch
/// uses.
async fn apply_delete_file(client: &Client, uri: Url) {
    let edit = delete_file_edit(uri.clone());
    apply_flush_edit(client, &uri, edit, "DeleteFile").await;
}

/// `FlushAction::Create` (ADR 0113 LSP follow-up, BT-3212): also
/// unconditional — a brand-new file was by definition never open before
/// this flush. The file already exists on disk (Phase B already committed
/// by the time this event fires) so the read is expected to succeed.
async fn apply_create_file(client: &Client, abs_path: &Path, uri: Url) {
    let Ok(content) = tokio::fs::read_to_string(abs_path).await else {
        tracing::warn!(
            %uri,
            "flush_event_listener: failed to read newly-created file from disk"
        );
        return;
    };
    let edit = create_file_edit(uri.clone(), content);
    apply_flush_edit(client, &uri, edit, "CreateFile").await;
}

/// Resolves a `'rename-class'` flush's `oldFile` companion (`old_raw`)
/// against the workspace roots via [`resolve_flushed_path`] — tolerating its
/// already-deleted state on disk (Phase B already unlinked it by the time
/// the flush event fires), the same way that function already tolerates a
/// `DeleteFile` target being gone — into the `old_uri` [`apply_rename_class_move`]
/// sends in its [`DocumentMoved`] notification.
///
/// Split out of `apply_rename_class_move` purely so this resolution step is
/// unit-testable on its own: it needs no `Client`, whereas exercising
/// `apply_rename_class_move` itself through a real `LspService`/socket pair
/// would additionally require driving the service through a full LSP
/// `initialize` handshake first — `Client::send_notification` (unlike
/// `show_message`/`log_message`, which use its `_unchecked` sibling) only
/// actually sends once `ServerState` has reached `Initialized`, and silently
/// no-ops (never touching the socket) otherwise, so a socket-side test
/// without that handshake would hang waiting for a message that never
/// arrives.
fn resolve_rename_class_old_uri(workspace_roots: &[PathBuf], old_raw: &str) -> Option<Url> {
    let Some((old_abs, _existed)) = resolve_flushed_path(old_raw, workspace_roots) else {
        tracing::debug!(
            old_raw,
            "flush_event_listener: could not resolve rename-class old path against workspace roots"
        );
        return None;
    };
    let Ok(old_uri) = Url::from_file_path(&old_abs) else {
        tracing::debug!(
            ?old_abs,
            "flush_event_listener: could not build file:// URI for rename-class old path"
        );
        return None;
    };
    Some(old_uri)
}

/// `FlushFileKind::RenameClass` with `oldFile` present (ADR 0114 LSP
/// follow-up, BT-3275; revised BT-3285): the one file among a
/// `'rename-class'` flush's touched files that IS the moved declaration.
/// Unconditional, like `Create`/`Delete`: a file move is project-wide state,
/// not something only an open buffer cares about.
///
/// Sends the custom [`DocumentMoved`] notification rather than a
/// `workspace/applyEdit` `RenameFile` op (BT-3275's original approach, which
/// this replaces): `old_uri` is *always* already gone from disk by the time
/// this runs, which is exactly the state VS Code's own `RenameOperation`
/// treats as "already done" and silently skips — no error, but no
/// editor-state retargeting either, so an open tab at `old_uri` never
/// actually followed the rename in VS Code (BT-3285's investigation). No
/// file content needs reading here any more, since the notification carries
/// no content — the receiving client reopens `new_uri` itself and reads its
/// already-correct on-disk bytes fresh.
async fn apply_rename_class_move(
    client: &Client,
    workspace_roots: &[PathBuf],
    new_uri: Url,
    old_raw: &str,
) {
    let Some(old_uri) = resolve_rename_class_old_uri(workspace_roots, old_raw) else {
        return;
    };
    client
        .send_notification::<DocumentMoved>(DocumentMovedParams {
            old_uri,
            new_uri: new_uri.clone(),
        })
        .await;
    tracing::debug!(
        %new_uri,
        "flush_event_listener: sent beamtalk-lsp/documentMoved notification"
    );
}

/// `FlushFileKind::RenameMethod` (ADR 0114 LSP follow-up, BT-3275): the
/// definition site or a confirmed sender site of a `'rename-method'` flush
/// (never a `candidate_sites` entry — those are never staged/written, so
/// they never reach this listener at all). Gated on the file being open,
/// exactly like [`apply_patch_file`] — the on-disk bytes are already correct
/// either way (Phase B already committed), so this check is purely about
/// whether an open editor buffer is worth refreshing, same as an ordinary
/// patch. Emits a `TextDocumentEdit` via `documentChanges`
/// ([`rename_method_site_edit`]) rather than the plain-`changes`-map shape
/// [`change_file_edit`] uses, matching the ADR's "`TextDocumentEdit` per
/// confirmed site" wording.
async fn apply_rename_method_site(
    client: &Client,
    abs_path: &Path,
    raw_path: &str,
    uri: Url,
    open_paths: &OpenPathsHandle,
) {
    let Ok(utf8_path) = Utf8PathBuf::try_from(abs_path.to_path_buf()) else {
        tracing::debug!(raw_path, "flush_event_listener: resolved path is not UTF-8");
        return;
    };
    if !open_paths.contains(&utf8_path) {
        tracing::debug!(%utf8_path, "flush_event_listener: skipping closed rename-method site");
        return;
    }
    let Ok(content) = tokio::fs::read_to_string(abs_path).await else {
        tracing::warn!(
            %utf8_path,
            "flush_event_listener: failed to read rename-method site from disk"
        );
        return;
    };
    let edit = rename_method_site_edit(uri.clone(), content);
    apply_flush_edit(client, &uri, edit, "RenameMethodSite").await;
}

/// `FlushAction::Patch` (an ordinary content edit, unchanged since ADR 0082
/// Phase 3): gated on the file being open — `VSCode` reads a closed file fresh
/// on next `did_open`.
async fn apply_patch_file(
    client: &Client,
    abs_path: &Path,
    raw_path: &str,
    uri: Url,
    open_paths: &OpenPathsHandle,
) {
    let Ok(utf8_path) = Utf8PathBuf::try_from(abs_path.to_path_buf()) else {
        tracing::debug!(raw_path, "flush_event_listener: resolved path is not UTF-8");
        return;
    };
    if !open_paths.contains(&utf8_path) {
        tracing::debug!(%utf8_path, "flush_event_listener: skipping closed file");
        return;
    }
    let Ok(content) = tokio::fs::read_to_string(abs_path).await else {
        tracing::warn!(
            %utf8_path,
            "flush_event_listener: failed to read flushed file from disk"
        );
        return;
    };
    let edit = change_file_edit(uri.clone(), content);
    apply_flush_edit(client, &uri, edit, "Change").await;
}

/// The `workspace/applyEdit` shape to emit for one flushed file (ADR 0113
/// LSP follow-up, BT-3212).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FlushAction {
    /// `CreateFile` + a `TextDocumentEdit` carrying the new content.
    Create,
    /// `DeleteFile` (BT-3209).
    Delete,
    /// A whole-document `TextEdit`, gated on the file being open.
    Patch,
}

/// Decide the [`FlushAction`] for one flushed file (ADR 0113 LSP follow-up,
/// BT-3212). When the wire reported a `kind`, it drives the decision
/// directly — no filesystem probing needed. When `kind` is `None` (a
/// producer that predates BT-3212, or omits this one path), falls back to
/// BT-3209's original existence heuristic: gone -> `Delete`, still there ->
/// `Patch`. That fallback can never produce `Create` — a `new-class` file on
/// a pre-BT-3212 producer degrades to `Patch` (its exact previous
/// behaviour), since post-flush existence alone cannot distinguish "freshly
/// created" from "patched in place".
///
/// `RenameClass`/`RenameMethod` (ADR 0114 LSP follow-up, BT-3275) are
/// dispatched to their own dedicated branches in `flush_event_listener`
/// *before* this function is ever called for them — `RenameClass` only
/// reaches here when the wire carried no `oldFile` (an ordinary same-batch
/// reference rewrite, not the moved file itself), and `RenameMethod` never
/// reaches here at all. Both bucket to `Patch` here defensively, matching
/// what the pre-BT-3275 fallback would have done for an unrecognised kind.
fn classify_flush_action(kind: Option<FlushFileKind>, existed: bool) -> FlushAction {
    match kind {
        Some(FlushFileKind::NewClass) => FlushAction::Create,
        Some(FlushFileKind::Patch | FlushFileKind::RenameClass | FlushFileKind::RenameMethod) => {
            FlushAction::Patch
        }
        None if existed => FlushAction::Patch,
        Some(FlushFileKind::RemoveClass) | None => FlushAction::Delete,
    }
}

/// Build a single `TextEdit` that replaces an entire document's content —
/// the `changes`/`documentChanges` payload shape every flush-driven edit
/// builder below sends (`change_file_edit`, `create_file_edit`,
/// `rename_method_site_edit`): the flush already
/// spliced the on-disk bytes server-side (no incremental diff is computed),
/// so the client is always handed the whole new content rather than a
/// localized range. `u32::MAX`/`u32::MAX` is the LSP convention for "end of
/// file" — any line longer than this is unrealistic for source code and
/// clients clamp to actual EOF.
fn whole_document_text_edit(content: String) -> TextEdit {
    TextEdit {
        range: Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: u32::MAX,
                character: u32::MAX,
            },
        },
        new_text: content,
    }
}

/// Build the `workspace/applyEdit` payload for a file that still exists on
/// disk after the flush (patch / `new-class` / `remove-method` — Tier 1,
/// unchanged from the behaviour that shipped in ADR 0082 Phase 3): a single
/// `TextEdit` covering the whole document with the new on-disk content.
fn change_file_edit(uri: Url, content: String) -> WorkspaceEdit {
    WorkspaceEdit {
        changes: Some({
            let mut changes = HashMap::new();
            changes.insert(uri, vec![whole_document_text_edit(content)]);
            changes
        }),
        ..Default::default()
    }
}

/// Build the `workspace/applyEdit` payload for a file the flush already
/// deleted from disk (ADR 0113 Phase 4a, BT-3209: a Tier 2 destructive flush
/// `remove-class` entry) — a typed `DeleteFile` resource operation via
/// `documentChanges`, not a text edit, since there is no content left to
/// send. `ignoreIfNotExists: true` is defensive: some further time has
/// passed between the existence check that classified this as a deletion
/// and the client actually receiving this request, so the client's own view
/// might already agree the file is gone.
fn delete_file_edit(uri: Url) -> WorkspaceEdit {
    WorkspaceEdit {
        document_changes: Some(DocumentChanges::Operations(vec![
            DocumentChangeOperation::Op(ResourceOp::Delete(DeleteFile {
                uri,
                options: Some(DeleteFileOptions {
                    recursive: Some(false),
                    ignore_if_not_exists: Some(true),
                    annotation_id: None,
                }),
            })),
        ])),
        ..Default::default()
    }
}

/// Build the `workspace/applyEdit` payload for a file the flush just wrote
/// to disk for the first time (ADR 0113 LSP follow-up, BT-3212: a
/// `new-class` entry) — a typed `CreateFile` resource operation via
/// `documentChanges`, paired with a `TextDocumentEdit` carrying the new
/// file's full content (ADR 0113's LSP section: "`Workspace newClass:at:`
/// flush should also switch from the generic `Change` shape to
/// `CreateFile`"). Both operations live in the same `documentChanges` array
/// (the LSP spec does not allow mixing the plain `changes` map with
/// `documentChanges` in one edit) so a client applies the create and the
/// content atomically as one workspace edit.
///
/// The physical file already exists on disk by the time this fires
/// (`beamtalk_workspace_flush:complete_flush/5` announces after Phase B
/// commits) — `ignoreIfExists: true` is defensive for exactly that reason:
/// the client's own `CreateFile` step must not fail just because the flush
/// beat it to the write, and the paired `TextDocumentEdit` still supplies
/// the correct content for the client's in-memory buffer either way.
fn create_file_edit(uri: Url, content: String) -> WorkspaceEdit {
    WorkspaceEdit {
        document_changes: Some(DocumentChanges::Operations(vec![
            DocumentChangeOperation::Op(ResourceOp::Create(CreateFile {
                uri: uri.clone(),
                options: Some(CreateFileOptions {
                    overwrite: None,
                    ignore_if_exists: Some(true),
                }),
                annotation_id: None,
            })),
            DocumentChangeOperation::Edit(TextDocumentEdit {
                text_document: OptionalVersionedTextDocumentIdentifier { uri, version: None },
                edits: vec![OneOf::Left(whole_document_text_edit(content))],
            }),
        ])),
        ..Default::default()
    }
}

/// Build the `workspace/applyEdit` payload for one confirmed site of a
/// `'rename-method'` flush (ADR 0114 LSP follow-up, BT-3275:
/// `renameSelector:to:`) — a single `TextDocumentEdit` via `documentChanges`
/// carrying the file's new content (a whole-document replacement, same
/// convention as [`change_file_edit`]/[`create_file_edit`]'s content edits).
/// Deliberately the typed `documentChanges`/`TextDocumentEdit` shape rather
/// than [`change_file_edit`]'s plain `changes` map — the ADR's LSP section
/// calls for "a `TextDocumentEdit` per confirmed site", not a generic patch,
/// even though both end up replacing the whole document the same way.
fn rename_method_site_edit(uri: Url, content: String) -> WorkspaceEdit {
    WorkspaceEdit {
        document_changes: Some(DocumentChanges::Operations(vec![
            DocumentChangeOperation::Edit(TextDocumentEdit {
                text_document: OptionalVersionedTextDocumentIdentifier { uri, version: None },
                edits: vec![OneOf::Left(whole_document_text_edit(content))],
            }),
        ])),
        ..Default::default()
    }
}

/// Resolve a path reported by the runtime against the LSP workspace roots,
/// tolerating an already-deleted target (ADR 0113 Phase 4a, BT-3209): a
/// Tier 2 destructive flush (`remove-class`) has already unlinked its file
/// by the time the `flush_completed` push fires
/// (`beamtalk_workspace_flush:complete_flush/5` announces after Phase B
/// commits), so `canonicalize()` — which requires the leaf to exist — can't
/// validate that case the way it does for an ordinary write.
///
/// The runtime stores `ChangeEntry.sourceFile` as whatever was passed at
/// `compile:source:` hook time — typically a workspace-relative path
/// (`"src/counter.bt"`) when the workspace was started in the project root.
/// We try, in order:
///
/// 1. Absolute → use as-is.
/// 2. For each workspace root, join.
///
/// For each candidate: canonicalise if the leaf exists (`existed = true`);
/// otherwise fall back to the literal candidate path if its *parent*
/// directory is real (`existed = false`) — enough to build a `file://` URI
/// for a `DeleteFile` resource operation without being able to canonicalise
/// a path that no longer exists. Canonicalisation matters for the
/// `existed = true` case because the LSP stores open documents under
/// canonical paths (`uri_to_path` runs `canonicalize`); a non-canonical
/// lookup would always miss the open-paths check.
///
/// Returns `None` if no root (nor the absolute-path case) finds even a real
/// parent directory for `raw`.
///
/// **Multi-root ambiguity for a deleted leaf:** with `existed = true` the
/// per-root loop picks the (necessarily unique) root that actually contains
/// the file. With `existed = false` there is no such tiebreaker — the first
/// root whose parent directory is real wins, even if a sibling root's same
/// relative path would *also* have a real parent. Worst case this reports
/// the deletion against the wrong root's copy of the path; the resulting
/// `DeleteFile` targets a path the editor never had open, which is a no-op
/// there, not a wrong deletion (the flush already deleted the *real* file
/// before this event fired — this function only decides which URI to name
/// in the notification). Relevant only for multi-root workspaces with a
/// same-relative-path collision across roots.
pub(crate) fn resolve_flushed_path(raw: &str, roots: &[PathBuf]) -> Option<(PathBuf, bool)> {
    let candidate = PathBuf::from(raw);
    if candidate.is_absolute() {
        return resolve_candidate(candidate);
    }
    for root in roots {
        if let Some(resolved) = resolve_candidate(root.join(&candidate)) {
            return Some(resolved);
        }
    }
    None
}

/// Resolve one absolute candidate path — see [`resolve_flushed_path`] for
/// the existed/deleted classification this implements.
fn resolve_candidate(candidate: PathBuf) -> Option<(PathBuf, bool)> {
    match candidate.canonicalize() {
        Ok(canon) => return Some((canon, true)),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
        // A non-NotFound failure (permission denied, symlink cycle, transient
        // I/O error, non-directory path component) does not mean the file was
        // deleted — treating it as "deleted" would fire a DeleteFile applyEdit
        // for a file that's still there, possibly discarding a client's open,
        // unsaved buffer for it.
        Err(_) => return None,
    }
    let parent = candidate.parent()?;
    if parent.exists() {
        Some((candidate, false))
    } else {
        None
    }
}

/// ADR 0082 Phase 3 (BT-2289): map a `workspace/executeCommand` invocation
/// to the Beamtalk expression that compiles to the same effect on the live
/// workspace. Delegates the actual expression construction to
/// `beamtalk_core::tool_expr`, the same shared builders `beamtalk-mcp`'s
/// typed tools call — a single implementation both surfaces call into, so
/// REPL `:flush` ≡ MCP `flush` ≡ LSP `beamtalk.flush` can't drift apart
/// (BT-3193; see `beamtalk_core::tool_expr`'s module docs and unit tests for
/// the enforcing conformance suite).
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
            Ok(flush_expr(FlushFilter::None))
        }
        CMD_FLUSH_CLASS => {
            let class = expect_string_arg(arguments, 0, "class")?;
            validate_class_name(&class)?;
            // The class is named literally in the expression (no escaping);
            // validation above prevents any shape that could parse
            // differently.
            Ok(flush_expr(FlushFilter::Class(&class)))
        }
        CMD_FLUSH_FILE => {
            let file = expect_string_arg(arguments, 0, "file")?;
            Ok(flush_expr(FlushFilter::File(&file)))
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
            Ok(flush_expr(FlushFilter::Kind(bare)))
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
            Ok(save_class_expr(&source, &path))
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
            Ok(precheck_method_expr(&class, selector, &source))
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
        CMD_REMOVE_METHOD => {
            let class = expect_string_arg(arguments, 0, "class")?;
            validate_class_name(&class)?;
            let selector = expect_string_arg(arguments, 1, "selector")?;
            // Accepted with or without a leading '#', mirroring MCP's
            // `remove_method` tool so both surfaces agree on input shape.
            let selector = selector.strip_prefix('#').unwrap_or(&selector);
            validate_selector(selector)?;
            // Optional third argument: an `ifAbsent:` fallback. Unlike
            // `CMD_PRECHECK_METHOD`'s `source`, this is raw Beamtalk
            // expression code embedded as the fallback block's body, not a
            // String value passed to a `compile:source:`-style primitive.
            // Missing and explicit `null` are both treated as "no fallback"
            // — some JSON-RPC clients pad positional arguments with `null`
            // rather than omitting the trailing slot.
            if matches!(arguments.get(2), None | Some(serde_json::Value::Null)) {
                return Ok(remove_method_expr(&class, selector));
            }
            let if_absent = expect_string_arg(arguments, 2, "ifAbsent")?;
            Ok(remove_method_if_absent_expr(&class, selector, &if_absent))
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
/// Phase 3, BT-2782's `CMD_PRECHECK_METHOD`). Delegates to the canonical
/// implementation in `beamtalk_core::source_analysis::validate_selector_input`.
fn validate_selector(sel: &str) -> std::result::Result<(), String> {
    beamtalk_core::source_analysis::validate_selector_input(sel)
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
        .map(|root| beamtalk_project::discover_project_root(&root))
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
/// Derives the sysroot via the shared `beamtalk_sysroot` leaf crate — the
/// same convention used by `beamtalk --print-sysroot` — then looks for
/// `share/beamtalk/stdlib/src/` under that prefix.
fn sysroot_stdlib_source_dir() -> Option<PathBuf> {
    let sysroot = beamtalk_sysroot::current_sysroot()?;
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

/// Whether `path` lies in a directory [`collect_preload_files`] walks for
/// some workspace root — `src/`, `test/`, or a fetched dependency's `src/`.
/// Must stay in lockstep with that walk: it decides which closed files keep
/// their on-disk index entry in `did_close`.
fn preload_covers(path: &Path, roots: &[PathBuf]) -> bool {
    roots.iter().any(|root| {
        path.starts_with(root.join("src"))
            || path.starts_with(root.join("test"))
            || dependency_src_dirs(root)
                .iter()
                .any(|dep_src| path.starts_with(dep_src))
    })
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

/// Converts a [`Span`] to an LSP [`FoldingRange`] (BT-3237).
///
/// Line-only, matching typical LSP folding-range providers: `start_line` is
/// the divider's own banner line, `end_line` is the line of the category's
/// last method. Character offsets are left unset (defaults to the full
/// line), and `kind` is `Region` — VS Code's default fold-gutter affordance,
/// distinct from `Comment`/`Imports`.
fn span_to_folding_range(span: Span, source: &str) -> FoldingRange {
    let range = span_to_range(span, source);
    FoldingRange {
        start_line: range.start.line,
        start_character: None,
        end_line: range.end.line,
        end_character: None,
        kind: Some(FoldingRangeKind::Region),
        collapsed_text: None,
    }
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
        // ADR 0108 Phase 8 (BT-2901): `references_query_at` is always
        // `None` for a type-alias name (see its doc — aliases have no
        // runtime representation for `NavQuery::ReferencesTo` to target),
        // so the usual match below never sees one. Check the alias
        // namespace directly so `include_declaration = false` still
        // excludes the alias's own declaration site from a cold-file
        // `references` response.
        return svc
            .alias_name_at(file, position)
            .map(|name| svc.find_class_declarations(name.as_str()))
            .unwrap_or_default();
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
        let (kind, detail) = method_symbol_kind_and_detail(m.class_side);
        children.push(tower_lsp::lsp_types::DocumentSymbol {
            name: m.selector.to_string(),
            detail,
            kind,
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
    declaration: Option<&beamtalk_language_service::Location>,
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
    rows: Vec<(S, Option<beamtalk_language_service::Location>)>,
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

/// Converts a beamtalk `Diagnostic` to an LSP `Diagnostic`.
fn to_lsp_diagnostic(
    diag: &beamtalk_language_service::Diagnostic,
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

/// BT-3442: the `SymbolKind`/`detail` pair for a method `DocumentSymbol`,
/// shared by every conversion path that builds one (currently the
/// AST-fallback `to_lsp_symbol` and the runtime `runtime_class_to_document_symbol`)
/// so they can't silently disagree on how VS Code's Outline, breadcrumbs,
/// and Go to Symbol in File distinguish a class-side method from an
/// instance-side method sharing the same selector. `FUNCTION` isn't used
/// elsewhere in either path's `SymbolKind` mapping, so it's free to
/// repurpose for "static-ish member" — the closest standard fit.
fn method_symbol_kind_and_detail(class_side: bool) -> (SymbolKind, Option<String>) {
    if class_side {
        (SymbolKind::FUNCTION, Some("class method".to_string()))
    } else {
        (SymbolKind::METHOD, None)
    }
}

/// Converts a beamtalk `DocumentSymbol` to an LSP `DocumentSymbol`.
#[expect(deprecated, reason = "LSP DocumentSymbol requires deprecated field")]
fn to_lsp_symbol(
    sym: beamtalk_language_service::DocumentSymbol,
    source: &str,
) -> tower_lsp::lsp_types::DocumentSymbol {
    let range = span_to_range(sym.span, source);
    let children = sym
        .children
        .into_iter()
        .map(|c| to_lsp_symbol(c, source))
        .collect();

    let selection_range = sym.name_span.map_or(range, |s| span_to_range(s, source));
    let (kind, detail) = match sym.kind {
        DocumentSymbolKind::Class => (SymbolKind::CLASS, None),
        DocumentSymbolKind::Method => method_symbol_kind_and_detail(false),
        DocumentSymbolKind::ClassMethod => method_symbol_kind_and_detail(true),
        DocumentSymbolKind::Field => (SymbolKind::FIELD, None),
        // BT-2601: a `// === Name ===` divider's method-category
        // container. NAMESPACE is the closest standard LSP `SymbolKind`
        // for "a named grouping of members that isn't itself a
        // type/function" — VS Code renders it with a distinct icon from
        // Method/Class, which is all that's needed here (nesting,
        // breadcrumbs, and sticky-scroll come from the tree shape, not
        // the icon choice).
        DocumentSymbolKind::Category => (SymbolKind::NAMESPACE, None),
    };
    tower_lsp::lsp_types::DocumentSymbol {
        name: sym.name.to_string(),
        kind,
        detail,
        tags: None,
        deprecated: None,
        range,
        selection_range,
        children: Some(children),
    }
}

#[cfg(test)]
mod tests;
