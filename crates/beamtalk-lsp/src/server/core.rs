// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `Backend`'s core support methods: construction, dirty-file tracking,
//! runtime attachment, diagnostics publication, and the reload/flush
//! listener lifecycle.

use super::config::PreloadConfig;
use super::config::collect_preload_files;
use super::convert::{offset_to_position, path_to_stdlib_uri, path_to_uri, uri_to_path};
use super::flush::flush_event_listener;
use super::nav::{bt_locations_to_lsp, runtime_site_to_lsp_location};
use super::reload::{
    OpenPathsHandle, class_changed_listener, publish_diagnostics_impl, reload_check_listener,
    seed_reload_diagnostics,
};
use super::{Backend, FetchContentParams, FetchContentResult, NavCache};
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use crate::runtime::{
    ClassChangedEvent, FlushEvent, ReloadCheckEvent, RuntimeClient, RuntimeError,
};

use beamtalk_core::unparse::format_source;
use beamtalk_language_service::{
    LanguageService, NavQuery, NavSite, NavSymbolClass, SimpleLanguageService,
};
use camino::Utf8PathBuf;
use ecow::EcoString;
use tower_lsp::Client;
use tower_lsp::lsp_types::{Range, TextEdit, Url};
use tracing::debug;

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

    /// Read the current nav-cache generation. Foundation
    /// consumers store this with each cached entry and compare on read.
    #[allow(dead_code, reason = "per-method children consume this API")]
    pub(crate) fn nav_cache_generation(&self) -> u64 {
        self.nav_cache
            .lock()
            .expect("nav_cache lock poisoned")
            .generation()
    }

    /// two-mode dispatch seam for navigation queries.
    ///
    /// When the `delegateToRuntime` flag is on **and** a runtime is
    /// reachable (the workspace is running), forwards `query` to the
    /// attached runtime via [`RuntimeClient::nav_query`] and converts the
    /// resulting [`NavSite`]s to LSP `Location`s through the per-call
    /// `to_lsp` mapper. Otherwise — flag off, no running workspace,
    /// runtime error, or empty runtime result — falls back to
    /// `ast_fallback`.
    ///
    /// Per-method children implement one nav query each
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

    /// two-mode dispatch seam for the **bulk symbol outline**
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

    /// Resolve the **declaration sites** (LSP `Location`s) for a
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
    pub(in crate::server) async fn declaration_sites_for_query(
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

    /// Read the `delegateToRuntime` flag.
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
    /// LSP-side AST for dirty files.
    pub(crate) fn is_dirty(&self, path: &Utf8PathBuf) -> bool {
        let guard = self.dirty_files.lock().expect("dirty_files lock poisoned");
        guard.contains(path)
    }

    /// Mark `path` as having unsaved edits. Called from `did_change`.
    pub(in crate::server) fn mark_dirty(&self, path: Utf8PathBuf) {
        let mut guard = self.dirty_files.lock().expect("dirty_files lock poisoned");
        guard.insert(path);
    }

    /// Clear the dirty bit for `path`. Called from `did_save` and
    /// `did_close` — `did_save` because the on-disk bytes now match the
    /// editor buffer, `did_close` because the editor no longer owns a
    /// modified copy.
    pub(in crate::server) fn clear_dirty(&self, path: &Utf8PathBuf) {
        let mut guard = self.dirty_files.lock().expect("dirty_files lock poisoned");
        guard.remove(path);
    }

    pub(in crate::server) fn file_version_for_uri(&self, uri: &Url) -> Option<i32> {
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
    pub(in crate::server) fn resolve_path_for_uri(&self, uri: &Url) -> Option<Utf8PathBuf> {
        if uri.scheme() == "beamtalk-stdlib" {
            self.stdlib_uri_to_path(uri)
        } else {
            uri_to_path(uri)
        }
    }

    /// Looks up the real filesystem path for a `beamtalk-stdlib:///ClassName.bt` URI.
    ///
    /// Returns `None` for invalid URI form, unknown class names, or ambiguous filenames.
    pub(in crate::server) fn stdlib_uri_to_path(&self, uri: &Url) -> Option<Utf8PathBuf> {
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
    pub(in crate::server) fn find_erlang_source_file(&self, module_name: &str) -> Option<PathBuf> {
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
    pub(in crate::server) async fn resolve_otp_lib_dir(&self) {
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

    /// dynamically register the type-hierarchy capability so
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
    pub(in crate::server) async fn register_type_hierarchy_capability(&self) {
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
    /// ADR 0075 Phase 1: Calls `beamtalk_core::ffi_type_specs::
    /// extract_type_specs` — the same single source of truth `beamtalk
    /// build`/`beamtalk lint` and the MCP `lint`/`diagnostic_summary` tools
    /// use — instead of hand-parsing whatever JSON cache entries
    /// happen to be on disk. Before this, a workspace opened before any
    /// `beamtalk build` had run (or after `beamtalk clean`) got an
    /// empty `NativeTypeRegistry` for the rest of the session: no FFI
    /// argument-type diagnostics, and any `@expect type` suppressing one
    /// shown as stale. Runs on a blocking task since it may spawn a
    /// `beamtalk_build_worker` BEAM node on a cold/stale cache.
    pub(in crate::server) async fn load_type_cache(&self, roots: &[PathBuf]) {
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
    /// severity-override table (ADR 0100 Rule 3) and installs it in
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
    pub(in crate::server) async fn load_diagnostics_table(&self, roots: &[PathBuf]) {
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
    /// registers it with the `ProjectIndex`, so two distinct real
    /// packages opened as sibling workspace roots get distinct alias-package
    /// stamps instead of colliding on the same-project marker.
    ///
    /// A root with no manifest, an unparseable manifest, or no `[package]
    /// name` is simply omitted — [`ProjectIndex::package_for_alias_stamping`]
    /// falls back to the same-project marker for any file under an
    /// unregistered root, matching prior behavior for that root.
    ///
    /// Runs *before* [`Self::preload_workspace_source_files`] so preloaded
    /// files stamp correctly on first indexing, but the ordering is a
    /// fast-path optimisation, not a correctness requirement:
    /// `set_root_packages` re-stamps any already-indexed file's aliases, so
    /// a `didOpen`/`didChange` notification that races this call (tower-lsp
    /// does not serialize `initialized()` against notification handlers)
    /// is corrected here instead of keeping a stale same-project stamp
    /// until its next edit.
    pub(in crate::server) async fn load_root_packages(&self, roots: &[PathBuf]) {
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

    pub(in crate::server) async fn preload_workspace_source_files(&self, config: PreloadConfig) {
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
        // stdlib files must be marked in the ProjectIndex before
        // indexing, not chained into the same loop as user_files above —
        // otherwise `is_stdlib_file` never returns true for them in the real
        // running LSP (only `ProjectIndex::with_stdlib`, a separate
        // constructor used by beamtalk-cli's build pipeline, did this), and
        // package stamping otherwise mis-tags stdlib aliases as same-project.
        for (path, content) in loaded.stdlib_files {
            let Ok(utf8_path) = Utf8PathBuf::from_path_buf(path) else {
                continue;
            };
            svc.mark_stdlib_file(utf8_path.clone());
            svc.update_file(utf8_path, content);
        }
        // With every workspace source file indexed, the ProjectIndex
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
        // Fetched deps mean dependency extensions may exist that the checker
        // cannot see. (Declared-but-unfetched deps are
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
    pub(in crate::server) fn format_document(&self, uri: &Url) -> Option<Vec<TextEdit>> {
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
    /// Called once preload completes so that any file opened before
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
    pub(in crate::server) async fn republish_open_diagnostics(&self) {
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
    /// ADR 0082 Phase 3: Attach to a running workspace lazily.
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
    pub(in crate::server) async fn ensure_runtime_attached(
        &self,
    ) -> std::result::Result<RuntimeClient, RuntimeError> {
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
        // class-loaded / method-installed push frames so the LSP
        // can invalidate runtime-attached nav caches. Today the listener
        // just logs and drops — the per-method children
        // attach the real cache to it.
        let (class_changed_tx, class_changed_rx) =
            tokio::sync::mpsc::unbounded_channel::<ClassChangedEvent>();
        // ADR 0105 Phase 1: reload-induced re-check outcomes, so
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

        // cache-invalidation listener for class-loaded /
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

        // ADR 0105 surface-parity gap: seed `reload_diagnostics`
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

    /// Spawn the reload-check listener task (ADR 0105 Phase 1).
    /// Extracted out of `ensure_runtime_attached` purely to keep that
    /// function under the lint's line-count limit — needs a `RuntimeClient`
    /// clone (to resolve owner class -> URI via `nav-symbols`) plus `Arc`
    /// clones of `service` and `reload_diagnostics` so it can
    /// merge-and-republish, mirroring the flush listener's pattern of
    /// holding only the specific pieces it needs rather than a `Backend`
    /// back-reference.
    pub(in crate::server) fn spawn_reload_check_listener(
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
    pub(in crate::server) async fn store_listener_handle(
        guard: &tokio::sync::Mutex<Option<tokio::task::JoinHandle<()>>>,
        handle: tokio::task::JoinHandle<()>,
    ) {
        let mut guard = guard.lock().await;
        if let Some(prev) = guard.take() {
            prev.abort();
        }
        *guard = Some(handle);
    }

    pub(in crate::server) async fn publish_diagnostics(&self, uri: &Url) {
        // a didOpen/didChange/didSave racing the LSP's startup
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
        // via `should_defer_reload_publish_for_preload`: deferring only when
        // the target URI is *also* open, so the same
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
