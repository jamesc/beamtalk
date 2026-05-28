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

use crate::runtime::{ClassChangedEvent, FlushEvent, RuntimeClient, RuntimeError};

use beamtalk_core::language_service::{
    CompletionKind, DocumentSymbolKind, LanguageService, NavQuery, NavSite, Position as BtPosition,
    RuntimeLocation, SimpleLanguageService, nav_site_to_location,
};
use beamtalk_core::semantic_analysis::ClassHierarchy;
use beamtalk_core::source_analysis::{Severity, Span};
use beamtalk_core::unparse::format_source;
use camino::Utf8PathBuf;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams,
    CodeActionProviderCapability, CodeActionResponse, CompletionItem, CompletionItemKind,
    CompletionOptions, CompletionParams, CompletionResponse, DiagnosticSeverity,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, DocumentFormattingParams, DocumentRangeFormattingParams,
    DocumentSymbolParams, DocumentSymbolResponse, Documentation, ExecuteCommandOptions,
    ExecuteCommandParams, GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents,
    HoverParams, HoverProviderCapability, ImplementationProviderCapability, InitializeParams,
    InitializeResult, InitializedParams, MarkupContent, MarkupKind, MessageType, OneOf,
    ParameterInformation, ParameterLabel, Position, Range, ReferenceParams, ServerCapabilities,
    SignatureHelp, SignatureHelpOptions, SignatureHelpParams, SignatureInformation,
    SymbolInformation, SymbolKind, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextDocumentSyncOptions, TextDocumentSyncSaveOptions, TextEdit, Url, WorkDoneProgressOptions,
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

/// All commands surfaced via `executeCommand`. Wired into
/// `ServerCapabilities::execute_command_provider` and used by the LSP→runtime
/// dispatch in [`Backend::execute_command`].
pub(crate) const BEAMTALK_LSP_COMMANDS: &[&str] = &[
    CMD_FLUSH,
    CMD_FLUSH_CLASS,
    CMD_FLUSH_FILE,
    CMD_FLUSH_KIND,
    CMD_SAVE_CLASS,
];

#[derive(Clone)]
struct PreloadConfig {
    roots: Vec<PathBuf>,
    stdlib_dirs: Vec<PathBuf>,
}

#[derive(Default)]
struct PreloadedFiles {
    user_files: Vec<(PathBuf, String)>,
    stdlib_files: Vec<(PathBuf, String)>,
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
    /// The underlying language service, protected by a mutex for concurrent access.
    service: Mutex<SimpleLanguageService>,
    /// Last known LSP document version by file path.
    versions: Arc<Mutex<HashMap<Utf8PathBuf, i32>>>,
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
            service: Mutex::new(SimpleLanguageService::new()),
            versions: Arc::new(Mutex::new(HashMap::new())),
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
        for (path, content) in loaded.user_files.into_iter().chain(loaded.stdlib_files) {
            let Ok(utf8_path) = Utf8PathBuf::from_path_buf(path) else {
                continue;
            };
            svc.update_file(utf8_path, content);
        }
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
        let client = RuntimeClient::connect(&project_root, flush_tx, class_changed_tx).await?;

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

        Ok(client)
    }

    async fn publish_diagnostics(&self, uri: &Url) {
        // Stdlib virtual documents have no user-facing diagnostics.
        if uri.scheme() == "beamtalk-stdlib" {
            return;
        }
        let Some(path) = uri_to_path(uri) else {
            return;
        };
        let diagnostics = {
            let svc = self.service.lock().expect("service lock poisoned");
            let source = svc.file_source(&path);
            svc.diagnostics(&path)
                .into_iter()
                .map(|d| to_lsp_diagnostic(&d, source.as_deref()))
                .collect()
        };
        let version = self.file_version_for_uri(uri);
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, version)
            .await;
    }
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
                svc.update_file(path, change.text);
            }
            {
                let mut versions = self.versions.lock().expect("versions lock poisoned");
                versions.insert(
                    uri_to_path(&uri).expect("path already checked"),
                    params.text_document.version,
                );
            }

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
                    return Ok(Some(GotoDefinitionResponse::Scalar(
                        tower_lsp::lsp_types::Location {
                            uri: target_uri,
                            range: tower_lsp::lsp_types::Range::default(),
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
    /// BT-2239 worked example: when `delegateToRuntime` is on and a runtime
    /// is attached, classify the cursor with
    /// [`SimpleLanguageService::references_query_at`] and forward to the
    /// runtime via [`Backend::delegate_nav_query`]. Local-identifier hits
    /// (parameters, locals) — which the runtime can't see — fall back to
    /// the AST walker unconditionally.
    async fn references(
        &self,
        params: ReferenceParams,
    ) -> Result<Option<Vec<tower_lsp::lsp_types::Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };

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
        let ast_fallback = || -> Vec<tower_lsp::lsp_types::Location> {
            let svc = backend_self.service.lock().expect("service lock poisoned");
            let refs = svc.find_references(&path_for_ast, pos);
            refs.into_iter()
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

        let locations = if let Some(query) = runtime_query {
            self.delegate_nav_query(query, runtime_site_to_lsp_location, ast_fallback)
                .await
        } else {
            // Cursor isn't on a selector or class name (local identifier,
            // parameter, etc.) — the runtime can't answer this, so go
            // straight to the AST path.
            ast_fallback()
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

    /// Returns the document symbol outline (classes, methods, fields).
    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;
        let Some(path) = self.resolve_path_for_uri(uri) else {
            return Ok(None);
        };

        let svc = self.service.lock().expect("service lock poisoned");
        let source = svc.file_source(&path);
        let symbols = svc.document_symbols(&path);

        let Some(src) = source.as_deref() else {
            return Ok(None);
        };

        let lsp_symbols: Vec<tower_lsp::lsp_types::DocumentSymbol> =
            symbols.into_iter().map(|s| to_lsp_symbol(s, src)).collect();

        if lsp_symbols.is_empty() {
            Ok(None)
        } else {
            Ok(Some(DocumentSymbolResponse::Nested(lsp_symbols)))
        }
    }

    /// Returns workspace-wide class symbols matching the query (BT-2081).
    ///
    /// Iterates over every indexed user file and emits one `SymbolInformation`
    /// per top-level class whose name contains the query string (case-insensitive,
    /// substring match). Stdlib files are excluded so the result mirrors the
    /// MCP `list_classes` "user" scope.
    ///
    /// An empty query returns every user class; this matches MCP behaviour.
    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let query_lower = params.query.to_ascii_lowercase();
        let svc = self.service.lock().expect("service lock poisoned");
        let mut out: Vec<SymbolInformation> = Vec::new();
        // Snapshot indexed file paths first; `document_symbols` does not
        // require a separate borrow of the project index.
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
                // The document-symbol provider tags class entries with a
                // trailing " (class)" disambiguator (so editors can render
                // `Counter (class)` in document outlines). Strip it here so
                // workspace/symbol surfaces a bare class name — that
                // matches the MCP `list_classes` semantics and is the
                // string editors typically expect for symbol queries.
                let name = sym
                    .name
                    .as_str()
                    .strip_suffix(" (class)")
                    .unwrap_or(sym.name.as_str())
                    .to_string();
                if !query_lower.is_empty() && !name.to_ascii_lowercase().contains(&query_lower) {
                    continue;
                }
                let range = span_to_range(sym.span, &source);
                #[expect(deprecated, reason = "LSP SymbolInformation requires deprecated field")]
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
                escape_beamtalk_string(&file)
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
                escape_beamtalk_string(&source),
                escape_beamtalk_string(&path)
            ))
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
    if name.is_empty() {
        return Err("class name must not be empty".to_string());
    }
    let mut chars = name.chars();
    match chars.next() {
        Some(c) if c.is_ascii_uppercase() => {}
        _ => {
            return Err(format!(
                "class name '{name}' must start with an uppercase letter"
            ));
        }
    }
    for c in chars {
        if !(c.is_ascii_alphanumeric() || c == '_') {
            return Err(format!(
                "class name '{name}' contains invalid character '{c}' (allowed: letters, digits, underscore)"
            ));
        }
    }
    Ok(())
}

/// Escape an arbitrary Rust string for embedding inside a Beamtalk double-
/// quoted string literal. Mirrors `beamtalk-mcp::escape_beamtalk_string`:
/// Beamtalk strings use `\` and `"` like Rust plus `{` triggers
/// interpolation (ADR 0023), so `\{` is required for a literal `{`.
fn escape_beamtalk_string(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('{', "\\{")
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

    for root in &roots {
        for dep_src in dependency_src_dirs(root) {
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
        let dirs = configured_stdlib_source_dirs(None, &[PathBuf::from("/tmp/project")]);
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
        let expected_source = escape_beamtalk_string(source);
        let expected_path = escape_beamtalk_string(path);
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
    fn build_unknown_command_returns_error() {
        let err = build_command_expression("not.a.real.command", &[]).expect_err("err");
        assert!(err.contains("unknown LSP command"));
    }

    #[test]
    fn escape_beamtalk_string_handles_quotes_backslashes_and_braces() {
        assert_eq!(escape_beamtalk_string("hello"), "hello");
        assert_eq!(escape_beamtalk_string("\"q\""), "\\\"q\\\"");
        assert_eq!(escape_beamtalk_string("a\\b"), "a\\\\b");
        assert_eq!(escape_beamtalk_string("{x}"), "\\{x}");
        assert_eq!(escape_beamtalk_string("\\\"{"), "\\\\\\\"\\{");
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
        assert_eq!(listed.len(), 5);
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
        let path = Utf8PathBuf::from("/tmp/counter.bt");
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
        let path = Utf8PathBuf::from("/tmp/counter.bt");
        versions
            .lock()
            .expect("versions lock")
            .insert(path.clone(), 1);
        assert!(handle.contains(&path));
        versions.lock().expect("versions lock").remove(&path);
        assert!(!handle.contains(&path));
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
}
