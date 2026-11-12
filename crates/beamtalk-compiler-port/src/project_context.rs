// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Per-project analysis context for file-based `compile` requests (BT-4322).
//!
//! **DDD Context:** Compilation
//!
//! The workspace `load_project`/`load_file` path compiles one file per
//! `compile` request, while `beamtalk build` typechecks every file with the
//! whole package in scope. A `compile` that sees none of that package scope
//! reports diagnostics the build never does: an FFI call with no registry
//! behind it infers `Dynamic (untyped FFI)`, and a `type` alias declared in
//! another file reads as an unknown name (so a `field: x :: Scheme = #http`
//! default mismatches). Under the project's own `[diagnostics] type =
//! "error"` those warnings fail the load of a package the build accepts.
//!
//! This module derives the compiled file's project from its `source_path`
//! (walk up to the enclosing `beamtalk.toml`, exactly the manifest that
//! scopes a `beamtalk build`) and assembles the three pieces of package
//! scope the build threads into every file's analysis, reusing the same
//! core loaders the build itself uses:
//!
//! - the project's own `[diagnostics]` table
//!   ([`load_diagnostics_table_for_root`]),
//! - the project's native Erlang FFI registry, replayed from its
//!   `_build/type_cache/` ([`load_native_type_registry_from`]) with the
//!   project-local `stubs/` tier applied on top
//!   ([`load_project_stub_registry`], the loader `beamtalk build` uses;
//!   BT-4378),
//! - the project's cross-file `type` aliases plus its resolved dependencies'
//!   aliases, stamped by
//!   [`extract_package_alias_infos`](AliasRegistry::extract_package_alias_infos)
//!   — the same extraction, project-first ordering and package stamping as
//!   the build's Pass 1 (`collect_project_protocol_and_alias_infos`, and
//!   `extract_dep_protocol_alias_metadata` for dependencies). The dependency
//!   set is the build's resolved graph (`_build/deps/.dep-graph.json`)
//!   pruned to what `beamtalk.toml` still declares
//!   ([`resolved_dependency_names`]), not a listing of `_build/deps/`,
//!   which keeps the checkouts of removed dependencies (BT-4378). The
//!   listing is used only when the snapshot is missing or unusable
//!   (BT-4394).
//!
//! The alias scan and the registry are memoized per project root (exact
//! path→(mtime, length) fingerprints, so any source, stub, manifest or cache
//! change re-resolves — including a same-mtime edit that changes the size,
//! BT-4365). A `compile` request carrying a `project_batch` token — one
//! per workspace `load_project` — verifies those fingerprints only on the
//! batch's first request for that root; the rest of the batch reuses the
//! verified scope, so a load stats the project once rather than once per
//! file. A file with no enclosing manifest keeps the legacy behaviour
//! (port-process cwd table, no registry, session aliases only).
//!
//! Deliberately out of scope: the build's distribution and package-bundled
//! stub tiers (`resolve_package_type_registry`), which need each
//! dependency's resolved manifest, and the build's live spec extraction (the
//! tiered cache's cold path). A project that was never built — no
//! `_build/type_cache/` — gets only its `stubs/` tier, like the port's
//! completion path and the LSP's cache replay get none. Path
//! dependencies are in scope (BT-4326): their sources live at the declared
//! path, outside `_build/deps/`, found by walking manifests like the build's
//! discovery, and dependencies merge in the build's compilation order.
//!
//! The editor `diagnostics/3` entry point resolves the same scope as
//! `compile` when its request carries the buffer's `source_path`
//! ([`RequestScope`], BT-4325); without one — a scratch buffer or an
//! expression typed at the REPL — it keeps the legacy scope, as does
//! `compile_expression` (REPL input belongs to no file's project). On the
//! keystroke path the memo above means a keystroke re-stats the project but
//! never re-parses it unless a fingerprint changed.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::time::SystemTime;

use beamtalk_core::compilation::dep_graph::{
    DEP_GRAPH_SNAPSHOT_FILE, DepGraphSnapshotError, declared_dependency_names_from_manifest_toml,
    declared_path_dependencies_from_manifest_toml, read_dep_graph_snapshot,
    resolved_dependency_names,
};
use beamtalk_core::compilation::dep_order::topological_order;
use beamtalk_core::compilation::diagnostics_policy::DiagnosticsTable;
use beamtalk_core::compilation::ffi_stubs::{PROJECT_STUBS_DIR, load_project_stub_registry};
use beamtalk_core::compilation::load_diagnostics_table_for_root;
use beamtalk_core::semantic_analysis::ProjectAnalysisContext;
use beamtalk_core::semantic_analysis::alias_registry::{AliasInfo, AliasRegistry};
use beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry;
use camino::{Utf8Path, Utf8PathBuf};

use crate::registry::load_native_type_registry_from;

/// The project scope a single `compile` request resolves to.
#[derive(Clone)]
pub(crate) struct FileProjectContext {
    /// The project's own `[diagnostics]` table.
    pub diagnostics_table: DiagnosticsTable,
    /// Registry + known modules + package name for
    /// `compute_diagnostics_and_analysis`.
    pub analysis_project: ProjectAnalysisContext,
    /// Cross-file aliases: the project's own first, then each resolved
    /// dependency's, mirroring the build's merge order.
    pub aliases: Vec<ProjectAlias>,
}

/// One project or dependency `type` alias, with the file declaring it — so
/// a request whose own source *is* that file (an editor buffer, possibly
/// unsaved) can drop the on-disk copy's aliases in favour of its own
/// (BT-4325).
#[derive(Clone)]
pub(crate) struct ProjectAlias {
    /// The declaring file, as the project source walk produced it.
    pub file: PathBuf,
    /// The alias itself, stamped with its package.
    pub info: AliasInfo,
}

/// Merge session-carried aliases with a project's: a name the session
/// already defines keeps the session's declaration. The REPL's "current
/// turn wins" rule applies — a same-named project alias would otherwise be
/// reported as a hard alias/alias collision the moment the project
/// reloads, breaking the redefine-in-session flow that works today.
/// Everything else appends project-first, mirroring the build's order.
fn merge_aliases<'a>(
    session_aliases: Vec<AliasInfo>,
    project_aliases: impl Iterator<Item = &'a AliasInfo>,
) -> Vec<AliasInfo> {
    let session_names: HashSet<String> = session_aliases
        .iter()
        .map(|info| info.name.to_string())
        .collect();
    let mut merged = session_aliases;
    merged.extend(
        project_aliases
            .filter(|info| !session_names.contains(info.name.as_str()))
            .cloned(),
    );
    merged
}

/// The analysis scope one request runs under: its file's project when
/// `source_path` sits under a manifest, otherwise the legacy port-process
/// scope (cwd `[diagnostics]` table, no registry, session aliases only).
///
/// The one place `compile`, `compile_method` and `diagnostics/3` pick their
/// scope, so the editor's diagnostics and a `load_project` compile of the
/// same file can never drift apart again (BT-4325).
pub(crate) struct RequestScope {
    project: Option<FileProjectContext>,
    legacy_project: ProjectAnalysisContext,
    /// The request's own file, canonicalized — `None` without a path or
    /// when the file does not exist on disk (an unsaved new buffer).
    own_file: Option<PathBuf>,
}

impl RequestScope {
    /// Resolve the scope for a request carrying `source_path` (and, for a
    /// `load_project` compile, its `project_batch` token). `None` — no path,
    /// a manifest-less file, or a stdlib compile the caller already
    /// excluded — is the legacy scope.
    pub(crate) fn for_source(source_path: Option<&str>, batch: Option<&str>) -> Self {
        Self {
            project: source_path.and_then(|path| project_context_for_source(path, batch)),
            legacy_project: ProjectAnalysisContext::default(),
            own_file: source_path.and_then(|path| std::fs::canonicalize(path).ok()),
        }
    }

    /// The `[diagnostics]` table: the project's own, else the port cwd's.
    pub(crate) fn diagnostics_table(&self) -> &DiagnosticsTable {
        match &self.project {
            Some(project) => &project.diagnostics_table,
            None => crate::registry::diagnostics_overrides(),
        }
    }

    /// Registry + known modules + package name for
    /// `compute_diagnostics_and_analysis`.
    pub(crate) fn analysis_project(&self) -> &ProjectAnalysisContext {
        match &self.project {
            Some(project) => &project.analysis_project,
            None => &self.legacy_project,
        }
    }

    /// The request's session aliases merged with every project alias
    /// ([`merge_aliases`]: session wins, then project-first). For a request
    /// whose source is only *part* of its file (a `compile_method` patch
    /// carries the class, not the file's `type` declarations), so the
    /// file's own on-disk aliases must stay in scope.
    pub(crate) fn aliases(&self, session_aliases: Vec<AliasInfo>) -> Vec<AliasInfo> {
        match &self.project {
            Some(project) => merge_aliases(
                session_aliases,
                project.aliases.iter().map(|alias| &alias.info),
            ),
            None => session_aliases,
        }
    }

    /// [`Self::aliases`] for a request whose source is its *whole* file
    /// (`compile`, an editor buffer for `diagnostics`): the on-disk copy's
    /// own aliases are dropped, since the request's source declares the
    /// file's aliases itself. Otherwise an alias deleted or renamed in an
    /// unsaved buffer would stay in scope from the saved copy, and the
    /// editor would accept code `load_project` rejects once saved
    /// (BT-4325). For a `load_project` compile the source *is* the saved
    /// file, so this is the same scope the build gives it.
    pub(crate) fn whole_file_aliases(&self, session_aliases: Vec<AliasInfo>) -> Vec<AliasInfo> {
        match &self.project {
            Some(project) => merge_aliases(
                session_aliases,
                project
                    .aliases
                    .iter()
                    .filter(|alias| self.own_file.as_deref() != Some(alias.file.as_path()))
                    .map(|alias| &alias.info),
            ),
            None => session_aliases,
        }
    }
}

/// Memoized per-root scope. Fingerprints are exact path→(mtime, length)
/// maps, so any added/removed/modified file re-resolves on the next verified
/// compile.
struct CachedProject {
    alias_files: Vec<Fingerprint>,
    registry_files: Vec<Fingerprint>,
    /// The scope last resolved for this root.
    context: FileProjectContext,
    /// The `project_batch` whose request last verified the fingerprints
    /// above: later requests of that batch reuse `context` without stats.
    verified_batch: Option<String>,
}

/// Upper bound on memoized project scopes: a long-lived port compiling many
/// distinct checkouts (e.g. one tempdir per test) must not pin a native
/// registry per root forever. Overflow evicts everything; the only cost is
/// a rescan.
const MAX_CACHED_PROJECTS: usize = 64;

/// One file's memo key: its path plus `(mtime, length)`, or `None` when the
/// file is missing or unstatable.
///
/// The length is free (the same `metadata()` call returns it) and catches an
/// edit that keeps the mtime — two writes within one coarse filesystem tick
/// (NTFS stamps from a ~15.6 ms clock), or `cp -p`/`rsync -t` — as long as
/// the size changes (BT-4365). A same-size, same-mtime edit is the accepted
/// residual: hashing contents would read every source on every verified
/// compile, which is the cost this memo exists to avoid.
type Fingerprint = (PathBuf, Option<(SystemTime, u64)>);

/// The `(mtime, length)` stamp of a stat result, or `None` when the
/// platform cannot report an mtime.
fn metadata_stamp(metadata: &std::fs::Metadata) -> Option<(SystemTime, u64)> {
    Some((metadata.modified().ok()?, metadata.len()))
}

/// Fingerprint of a single file (a missing file fingerprints as absent, so
/// creating or deleting it re-resolves).
fn file_fingerprint(path: &Path) -> Fingerprint {
    let stamp = std::fs::metadata(path)
        .ok()
        .and_then(|m| metadata_stamp(&m));
    (path.to_path_buf(), stamp)
}

static PROJECT_CACHE: std::sync::LazyLock<Mutex<HashMap<PathBuf, CachedProject>>> =
    std::sync::LazyLock::new(|| Mutex::new(HashMap::new()));

fn cache_lock() -> std::sync::MutexGuard<'static, HashMap<PathBuf, CachedProject>> {
    PROJECT_CACHE
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
}

/// Derive the file's project root: the nearest ancestor directory of
/// `source_path` holding a `beamtalk.toml`. Returns `None` for a manifest-less
/// file (stdlib-style single-file compiles), preserving legacy behaviour.
fn find_project_root(source_path: &str) -> Option<PathBuf> {
    let path = Path::new(source_path);
    let absolute = if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir().ok()?.join(path)
    };
    let mut dir = absolute.parent()?.to_path_buf();
    loop {
        if dir.join("beamtalk.toml").is_file() {
            return std::fs::canonicalize(&dir).ok();
        }
        if !dir.pop() {
            return None;
        }
    }
}

/// The project's source files for alias extraction: the one shared
/// [`collect_project_source_files`](beamtalk_core::file_walker::collect_project_source_files)
/// rule the build itself uses, so a layout change there applies here too.
/// A walk failure degrades to no project aliases (with a warning) rather
/// than failing the compile — the sibling file is unreadable, not the file
/// being compiled.
fn project_source_files(root: &Utf8Path) -> Vec<Utf8PathBuf> {
    match beamtalk_core::file_walker::collect_project_source_files(root) {
        Ok(files) => files,
        Err(error) => {
            tracing::warn!(
                root = %root,
                %error,
                "project alias scan: failed to walk source dir, continuing with no project aliases"
            );
            Vec::new()
        }
    }
}

/// Parse every file and extract its `type` aliases, stamped with `package`
/// by the build's own
/// [`extract_package_alias_infos`](AliasRegistry::extract_package_alias_infos).
/// Unreadable files are skipped, like the build's scan.
fn extract_aliases(files: &[Utf8PathBuf], package: &str) -> Vec<ProjectAlias> {
    let mut aliases = Vec::new();
    for file in files {
        let Ok(source) = std::fs::read_to_string(file.as_std_path()) else {
            continue;
        };
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
        let (module, _diagnostics) = beamtalk_core::source_analysis::parse(tokens);
        aliases.extend(
            AliasRegistry::extract_package_alias_infos(&module, Some(package))
                .into_iter()
                .map(|info| ProjectAlias {
                    file: file.as_std_path().to_path_buf(),
                    info,
                }),
        );
    }
    aliases
}

/// Exact path→(mtime, length) fingerprint of a file set (sorted, so
/// comparison is a single `==`). Any add/remove/modify changes it.
/// Unstatable files are carried as `None` rather than dropped, so a file
/// flipping between readable and unreadable still re-resolves.
fn fingerprint(files: &[Utf8PathBuf]) -> Vec<Fingerprint> {
    let mut entries: Vec<Fingerprint> = files
        .iter()
        .map(|f| file_fingerprint(f.as_std_path()))
        .collect();
    entries.sort();
    entries
}

/// Fingerprint of a directory's immediate entries: paths plus
/// `(mtime, length)`, with an unstatable entry carried as `None` (see
/// [`fingerprint`]).
fn dir_file_fingerprint(dir: &Path) -> Vec<Fingerprint> {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return Vec::new();
    };
    let mut files: Vec<Fingerprint> = entries
        .flatten()
        .map(|e| {
            let stamp = e.metadata().ok().and_then(|m| metadata_stamp(&m));
            (e.path(), stamp)
        })
        .collect();
    files.sort();
    files
}

/// Fingerprint of the project-local stub tier: every stub file the shared
/// loader reads (it walks `stubs/` recursively), so adding, editing or
/// deleting a stub re-resolves the registry. Like [`fingerprint`], a stub
/// that cannot be stat'ed is carried as `None`, not dropped.
fn stub_file_fingerprint(root: &Utf8Path) -> Vec<Fingerprint> {
    let stubs_dir = root.join(PROJECT_STUBS_DIR);
    if !stubs_dir.is_dir() {
        return Vec::new();
    }
    let files = beamtalk_core::file_walker::FileWalker::source_files()
        .walk(&stubs_dir)
        .unwrap_or_default();
    fingerprint(&files)
}

/// Fingerprint for the memoized registry: the type-cache entries, the
/// project-local stub files, and the native inputs the extraction reads —
/// dependency `.beam` files and the project's own `native/*.erl` sources.
/// The replay itself re-validates each entry against its beam mtime, but the
/// memo would bypass that check, so the fingerprint must cover the inputs
/// too: otherwise a dependency rebuild without a fresh `beamtalk build`
/// would keep serving the old specs until the port restarts. (An OTP upgrade
/// mid-session is the accepted residual — it warrants a restart anyway.)
fn registry_fingerprint(root: &Utf8Path, cache_dir: &Path) -> Vec<Fingerprint> {
    let mut entries = dir_file_fingerprint(cache_dir);
    for ebin in beamtalk_core::ffi_type_specs::collect_project_dependency_ebin_dirs(root) {
        entries.extend(dir_file_fingerprint(ebin.as_std_path()));
    }
    entries.extend(dir_file_fingerprint(root.join("native").as_std_path()));
    entries.extend(stub_file_fingerprint(root));
    entries.sort();
    entries
}

/// The project's resolved dependencies with a source root on disk — unpacked
/// under `_build/deps/` (hex/git) or at the declared path (path dependencies,
/// BT-4326) — each with its package name, in the build's compilation order.
///
/// The set is the build's resolved graph (`_build/deps/.dep-graph.json`)
/// pruned to what `manifest_toml`'s `[dependencies]` still declares
/// ([`resolved_dependency_names`]) — not a listing of `_build/deps/`, which
/// keeps the checkout of a dependency since removed from `beamtalk.toml`.
///
/// A missing or unusable snapshot (never written, written by an older
/// toolchain or schema, a best-effort write that failed, or corrupt) falls
/// back to the manifest's directly declared dependencies plus every
/// `_build/deps/` checkout that has a manifest
/// ([`unresolved_graph_dependency_names`]), so the REPL does not reject code
/// `beamtalk build` accepts — transitive dependencies included — until the
/// next build rewrites the snapshot. The listing is only a fallback: with a
/// usable snapshot, a checkout outside the resolved graph never contributes.
fn dependency_roots(root: &Utf8Path, manifest_toml: &str) -> Vec<(Utf8PathBuf, String)> {
    let deps_dir = root.join("_build/deps");
    let declared = declared_dependency_names_from_manifest_toml(manifest_toml);
    let (names, graph_is_resolved) =
        match read_dep_graph_snapshot(deps_dir.join(DEP_GRAPH_SNAPSHOT_FILE).as_std_path()) {
            Ok(snapshot) => (resolved_dependency_names(&snapshot, &declared), true),
            Err(reason) => {
                if reason != DepGraphSnapshotError::Missing {
                    tracing::debug!(
                        root = %root,
                        %reason,
                        "dependency-graph snapshot unusable; falling back to _build/deps/ checkouts"
                    );
                }
                (
                    unresolved_graph_dependency_names(&deps_dir, declared),
                    false,
                )
            }
        };
    locate_dependencies(root, manifest_toml, &deps_dir, &names, graph_is_resolved)
}

/// A resolved dependency's on-disk root and the names its own manifest
/// declares.
struct LocatedDependency {
    root: Utf8PathBuf,
    declared: Vec<String>,
}

/// Find each of `names` on disk and order them as the build does.
///
/// Where a dependency lives depends on how its *declaring* manifest asks for
/// it (BT-4326): a path dependency's sources stay at the declared path,
/// relative to that manifest's directory (the build's
/// `canonicalize_dep_path`); hex/git/registry dependencies are unpacked to
/// `_build/deps/<name>/`. So this walks manifests from the project's, the way
/// the build's discovery does, instead of guessing a layout. A name no
/// manifest reaches (the unusable-snapshot fallback listing) is looked for
/// under `_build/deps/`. With a usable snapshot (`graph_is_resolved`) only
/// `names` are located; without one every dependency a manifest reaches is.
///
/// The result is in the build's compilation order
/// ([`topological_order`]: leaves first, ties by name) — the order
/// `dep_ctx.resolved_deps` carries, so a same-named alias declared by two
/// dependencies has the same first-wins winner here as in the build.
fn locate_dependencies(
    root: &Utf8Path,
    manifest_toml: &str,
    deps_dir: &Utf8Path,
    names: &BTreeSet<String>,
    graph_is_resolved: bool,
) -> Vec<(Utf8PathBuf, String)> {
    let mut located: BTreeMap<String, LocatedDependency> = BTreeMap::new();
    let mut queue = VecDeque::from([(root.to_path_buf(), manifest_toml.to_string())]);
    while let Some((dir, toml)) = queue.pop_front() {
        let paths: HashMap<String, String> = declared_path_dependencies_from_manifest_toml(&toml)
            .into_iter()
            .collect();
        for name in declared_dependency_names_from_manifest_toml(&toml) {
            // Without a usable snapshot `names` is only a guess (declared
            // plus checkouts), so every dependency a manifest reaches counts:
            // a transitive path dependency has no checkout to list.
            if (graph_is_resolved && !names.contains(&name)) || located.contains_key(&name) {
                continue;
            }
            let dep_root = match paths.get(&name) {
                Some(path) => std::fs::canonicalize(dir.join(path).as_std_path())
                    .ok()
                    .and_then(|p| Utf8PathBuf::from_path_buf(p).ok()),
                None => Some(deps_dir.join(&name)),
            };
            if let Some(dep) = dep_root.and_then(read_dependency) {
                queue.push_back((dep.root.clone(), dep.manifest.clone()));
                located.insert(name, dep.located);
            }
        }
    }
    for name in names {
        if !located.contains_key(name)
            && let Some(dep) = read_dependency(deps_dir.join(name))
        {
            located.insert(name.clone(), dep.located);
        }
    }

    let edges: BTreeMap<String, Vec<String>> = located
        .iter()
        .map(|(name, dep)| (name.clone(), dep.declared.clone()))
        .collect();
    // A cycle is a build error, so a project with one never has a snapshot
    // worth ordering; fall back to name order rather than failing a compile.
    let order = topological_order(&edges, "")
        .unwrap_or_else(|_| located.keys().cloned().collect::<Vec<_>>());
    order
        .into_iter()
        .filter_map(|name| located.remove(&name).map(|dep| (dep.root, name)))
        .collect()
}

/// A dependency checkout read from disk: its manifest text plus its
/// [`LocatedDependency`] record.
struct ReadDependency {
    root: Utf8PathBuf,
    manifest: String,
    located: LocatedDependency,
}

/// Read `dep_root`'s manifest, or `None` when it has none (a dependency with
/// no manifest cannot be built, so it contributes nothing).
fn read_dependency(dep_root: Utf8PathBuf) -> Option<ReadDependency> {
    let manifest = std::fs::read_to_string(dep_root.join("beamtalk.toml").as_std_path()).ok()?;
    let declared = declared_dependency_names_from_manifest_toml(&manifest);
    Some(ReadDependency {
        root: dep_root.clone(),
        manifest,
        located: LocatedDependency {
            root: dep_root,
            declared,
        },
    })
}

/// The dependency set when there is no usable resolved graph: the directly
/// declared names plus every `_build/deps/` checkout directory holding a
/// `beamtalk.toml`. A checkout left over from a removed dependency is
/// included too; without the snapshot nothing tells it apart from a
/// transitive dependency, and over-including only risks accepting an alias
/// the build would reject, while under-including rejects code the build
/// accepts.
fn unresolved_graph_dependency_names(
    deps_dir: &Utf8Path,
    declared: Vec<String>,
) -> BTreeSet<String> {
    let mut names: BTreeSet<String> = declared.into_iter().collect();
    if let Ok(entries) = deps_dir.read_dir_utf8() {
        names.extend(
            entries
                .flatten()
                .filter(|entry| entry.path().join("beamtalk.toml").is_file())
                .map(|entry| entry.file_name().to_string()),
        );
    }
    names
}

/// The project's FFI registry: its `_build/type_cache/` replay with the
/// project-local `stubs/` tier applied on top (project stubs win per
/// function/arity, as in the build's resolution chain). `None` when neither
/// contributes anything.
fn load_project_registry(utf8_root: &Utf8Path) -> Option<Arc<NativeTypeRegistry>> {
    let mut registry = load_native_type_registry_from(utf8_root.as_std_path());
    if let Some(stubs) = load_project_stub_registry(utf8_root, None) {
        registry.apply_overrides(stubs.registry);
    }
    (registry.module_count() > 0).then(|| Arc::new(registry))
}

/// Assemble the [`FileProjectContext`] for the project enclosing
/// `source_path`, or `None` when no manifest encloses it.
///
/// `batch` is the request's `project_batch` token, when it carries one. The
/// first request of a batch for a given root verifies the fingerprints (and
/// re-reads the manifest); every later request of the same batch reuses
/// that verified scope without re-statting the project. A `load_project`
/// does not edit the project mid-batch, and the next batch — or any request
/// without a token — verifies afresh.
fn project_context_for_source(
    source_path: &str,
    batch: Option<&str>,
) -> Option<FileProjectContext> {
    let root = find_project_root(source_path)?;
    if let Some(batch) = batch {
        let cache = cache_lock();
        if let Some(entry) = cache.get(&root)
            && entry.verified_batch.as_deref() == Some(batch)
        {
            return Some(entry.context.clone());
        }
    }
    resolve_project_context(root, batch)
}

/// Resolve `root`'s scope against its current fingerprints — reusing the
/// memoized scan when they are unchanged — and record it as verified for
/// `batch`.
fn resolve_project_context(root: PathBuf, batch: Option<&str>) -> Option<FileProjectContext> {
    let utf8_root = Utf8PathBuf::from_path_buf(root.clone()).ok()?;
    let manifest_path = root.join("beamtalk.toml");

    let manifest = std::fs::read_to_string(&manifest_path).unwrap_or_default();
    let package_name = beamtalk_core::compilation::parse_package_name_from_manifest_toml(&manifest)
        .unwrap_or_default();
    let diagnostics_table = load_diagnostics_table_for_root(root.as_path());

    // Alias scan inputs: project sources plus every resolved dependency's
    // sources, in build merge order (project first).
    let mut alias_inputs: Vec<(Vec<Utf8PathBuf>, String)> =
        vec![(project_source_files(&utf8_root), package_name.clone())];
    let dep_roots = dependency_roots(&utf8_root, &manifest);
    for (dep_root, dep_name) in &dep_roots {
        alias_inputs.push((project_source_files(dep_root), dep_name.clone()));
    }
    let alias_files: Vec<Utf8PathBuf> = alias_inputs.iter().flat_map(|(f, _)| f.clone()).collect();
    // The manifests and the dependency-graph snapshot decide the package
    // stamps and the dependency set, so a change to either — a `[package]
    // name` rename, a removed dependency, a re-resolve — re-resolves too.
    let mut alias_fingerprint = fingerprint(&alias_files);
    alias_fingerprint.push(file_fingerprint(&manifest_path));
    alias_fingerprint.push(file_fingerprint(
        utf8_root
            .join("_build/deps")
            .join(DEP_GRAPH_SNAPSHOT_FILE)
            .as_std_path(),
    ));
    for (dep_root, _) in &dep_roots {
        alias_fingerprint.push(file_fingerprint(
            dep_root.join("beamtalk.toml").as_std_path(),
        ));
    }
    alias_fingerprint.sort();

    let registry_files = registry_fingerprint(&utf8_root, &root.join("_build/type_cache"));

    let mut cache = cache_lock();
    // The known Erlang modules ride the registry's memo: their inputs
    // (dependency `.beam` files, `native/*.erl`) are exactly the registry
    // fingerprint's.
    let (aliases, registry, known_erlang_modules) = match cache.get(&root) {
        Some(entry)
            if entry.alias_files == alias_fingerprint
                && entry.registry_files == registry_files
                // A disabled set means the OTP probe failed (e.g. no `erl`
                // on PATH); failures are not memoized, so retry (BT-4724).
                && entry
                    .context
                    .analysis_project
                    .known_erlang_modules
                    .check_enabled() =>
        {
            (
                entry.context.aliases.clone(),
                entry.context.analysis_project.native_type_registry.clone(),
                entry.context.analysis_project.known_erlang_modules.clone(),
            )
        }
        _ => (
            alias_inputs
                .iter()
                .flat_map(|(files, package)| extract_aliases(files, package))
                .collect(),
            load_project_registry(&utf8_root),
            beamtalk_core::ffi_type_specs::collect_known_erlang_modules(&utf8_root),
        ),
    };

    let context = FileProjectContext {
        diagnostics_table,
        analysis_project: ProjectAnalysisContext {
            package_name: (!package_name.is_empty()).then_some(package_name),
            native_type_registry: registry,
            known_erlang_modules,
        },
        aliases,
    };
    if !cache.contains_key(&root) && cache.len() >= MAX_CACHED_PROJECTS {
        cache.clear();
    }
    cache.insert(
        root,
        CachedProject {
            alias_files: alias_fingerprint,
            registry_files,
            context: context.clone(),
            verified_batch: batch.map(str::to_string),
        },
    );
    Some(context)
}
