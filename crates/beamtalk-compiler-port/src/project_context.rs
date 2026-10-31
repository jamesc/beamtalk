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
//!   ([`load_diagnostics_table_for_root`](beamtalk_core::compilation::load_diagnostics_table_for_root)),
//! - the project's native Erlang FFI registry, replayed from its
//!   `_build/type_cache/`
//!   ([`load_native_type_registry_from`](crate::registry::load_native_type_registry_from)),
//! - the project's cross-file `type` aliases plus its unpacked
//!   dependencies' (`_build/deps/*/`) aliases, extracted with the same
//!   parse-and-[`extract_alias_infos`](beamtalk_core::semantic_analysis::alias_registry::AliasRegistry::extract_alias_infos)
//!   scan — and the same project-first ordering and package stamping — as
//!   the build's Pass 1 (`collect_project_protocol_and_alias_infos`, and
//!   `extract_dep_protocol_alias_metadata` for dependencies).
//!
//! The alias scan and the type-cache replay are memoized per project root
//! (exact path→mtime fingerprints, so any source or cache change
//! re-resolves); the manifest reads stay uncached — one small TOML parse
//! per compile, negligible next to a full analysis. A file with no
//! enclosing manifest keeps the legacy behaviour (port-process cwd table,
//! no registry, session aliases only).
//!
//! Deliberately out of scope: the build's stub tiers
//! (`resolve_package_type_registry`'s distribution/dependency/project-local
//! `stubs/`) need dependency resolution the port does not do, and the
//! build's live spec extraction (the tiered cache's cold path). A project
//! that was never built — no `_build/type_cache/` — stays registry-blind
//! exactly as before, same as the port's completion path and the LSP's
//! cache replay.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::time::SystemTime;

use beamtalk_core::compilation::diagnostics_policy::DiagnosticsTable;
use beamtalk_core::file_walker::FileWalker;
use beamtalk_core::semantic_analysis::ProjectAnalysisContext;
use beamtalk_core::semantic_analysis::alias_registry::{AliasInfo, AliasRegistry};
use beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry;
use camino::{Utf8Path, Utf8PathBuf};

use beamtalk_core::compilation::load_diagnostics_table_for_root;

use crate::registry::load_native_type_registry_from;

/// The project scope a single `compile` request resolves to.
pub(crate) struct FileProjectContext {
    /// The project's own `[diagnostics]` table.
    pub diagnostics_table: DiagnosticsTable,
    /// Registry + known modules + package name for
    /// `compute_diagnostics_and_analysis`.
    pub analysis_project: ProjectAnalysisContext,
    /// Cross-file aliases: the project's own first, then each unpacked
    /// dependency's, mirroring the build's merge order.
    pub aliases: Vec<AliasInfo>,
}

/// Memoized per-root scan results. Fingerprints are exact path→mtime maps,
/// so any added/removed/modified file re-resolves on the next compile.
struct CachedProject {
    alias_files: Vec<(PathBuf, SystemTime)>,
    aliases: Vec<AliasInfo>,
    cache_files: Vec<(PathBuf, SystemTime)>,
    registry: Arc<NativeTypeRegistry>,
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

/// The project's source files for alias extraction: `src/` when it exists,
/// else the root itself — excluding `stubs/`, which the build scans
/// separately and never compiles as ordinary source. Mirrors
/// `collect_project_source_files`.
fn project_source_files(root: &Utf8Path) -> Vec<Utf8PathBuf> {
    let src_dir = root.join("src");
    let search_dir = if src_dir.exists() {
        src_dir
    } else {
        root.to_path_buf()
    };
    let stubs_dir = root.join("stubs");
    FileWalker::source_files()
        .walk(&search_dir)
        .unwrap_or_default()
        .into_iter()
        .filter(|f| !f.starts_with(&stubs_dir))
        .collect()
}

/// Parse every file and extract its `type` aliases, stamped with `package`.
/// Unreadable or unparseable files are skipped, like the build's scan.
fn extract_aliases(files: &[Utf8PathBuf], package: &str) -> Vec<AliasInfo> {
    let mut aliases = Vec::new();
    for file in files {
        let Ok(source) = std::fs::read_to_string(file.as_std_path()) else {
            continue;
        };
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
        let (module, _diagnostics) = beamtalk_core::source_analysis::parse(tokens);
        let mut infos = AliasRegistry::extract_alias_infos(&module);
        for info in &mut infos {
            info.package = Some(package.into());
        }
        aliases.extend(infos);
    }
    aliases
}

/// Exact path→mtime fingerprint of a file set (sorted, so comparison is a
/// single `==`). Any add/remove/modify changes it.
fn fingerprint(files: &[Utf8PathBuf]) -> Vec<(PathBuf, SystemTime)> {
    let mut entries: Vec<(PathBuf, SystemTime)> = files
        .iter()
        .filter_map(|f| {
            let mtime = std::fs::metadata(f.as_std_path()).ok()?.modified().ok()?;
            Some((f.as_std_path().to_path_buf(), mtime))
        })
        .collect();
    entries.sort();
    entries
}

/// Fingerprint of a type-cache dir: the entry filenames plus mtimes.
fn cache_dir_fingerprint(cache_dir: &Path) -> Vec<(PathBuf, SystemTime)> {
    let Ok(entries) = std::fs::read_dir(cache_dir) else {
        return Vec::new();
    };
    let mut files: Vec<(PathBuf, SystemTime)> = entries
        .flatten()
        .filter_map(|e| {
            let mtime = e.metadata().ok()?.modified().ok()?;
            Some((e.path(), mtime))
        })
        .collect();
    files.sort();
    files
}

/// The project's dependency source roots: unpacked packages under
/// `_build/deps/` (hex/git deps land there with their sources, which is
/// what the build's `build_dep_class_index` scans for their aliases).
fn dependency_roots(root: &Utf8Path) -> Vec<(Utf8PathBuf, String)> {
    let deps_dir = root.join("_build/deps");
    let Ok(entries) = std::fs::read_dir(deps_dir.as_std_path()) else {
        return Vec::new();
    };
    let mut deps = Vec::new();
    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_dir() || !path.join("beamtalk.toml").is_file() {
            continue;
        }
        let Ok(utf8) = Utf8PathBuf::from_path_buf(path) else {
            continue;
        };
        let name = std::fs::read_to_string(utf8.join("beamtalk.toml"))
            .ok()
            .and_then(|manifest| {
                beamtalk_core::compilation::parse_package_name_from_manifest_toml(&manifest)
            })
            .or_else(|| utf8.file_name().map(ToString::to_string))
            .unwrap_or_default();
        deps.push((utf8, name));
    }
    deps.sort();
    deps
}

/// Assemble the [`FileProjectContext`] for the project enclosing
/// `source_path`, or `None` when no manifest encloses it.
pub(crate) fn project_context_for_source(source_path: &str) -> Option<FileProjectContext> {
    let root = find_project_root(source_path)?;
    let utf8_root = Utf8PathBuf::from_path_buf(root.clone()).ok()?;

    let manifest = std::fs::read_to_string(root.join("beamtalk.toml")).unwrap_or_default();
    let package_name = beamtalk_core::compilation::parse_package_name_from_manifest_toml(&manifest)
        .unwrap_or_default();
    let diagnostics_table = load_diagnostics_table_for_root(root.as_path());

    // Alias scan inputs: project sources plus every unpacked dependency's
    // sources, in build merge order (project first).
    let mut alias_inputs: Vec<(Vec<Utf8PathBuf>, String)> =
        vec![(project_source_files(&utf8_root), package_name.clone())];
    for (dep_root, dep_name) in dependency_roots(&utf8_root) {
        alias_inputs.push((project_source_files(&dep_root), dep_name));
    }
    let alias_files: Vec<Utf8PathBuf> = alias_inputs.iter().flat_map(|(f, _)| f.clone()).collect();
    let alias_fingerprint = fingerprint(&alias_files);

    let cache_dir = root.join("_build/type_cache");
    let registry_fingerprint = cache_dir_fingerprint(&cache_dir);

    // Memoized resolve: exact fingerprints, so any source or cache change
    // re-resolves on the next compile.
    let (aliases, registry) = {
        let mut cache = cache_lock();
        match cache.get(&root) {
            Some(entry)
                if entry.alias_files == alias_fingerprint
                    && entry.cache_files == registry_fingerprint =>
            {
                (entry.aliases.clone(), Arc::clone(&entry.registry))
            }
            _ => {
                let aliases = alias_inputs
                    .iter()
                    .flat_map(|(files, package)| extract_aliases(files, package))
                    .collect::<Vec<_>>();
                let registry = Arc::new(load_native_type_registry_from(root.as_path()));
                cache.insert(
                    root.clone(),
                    CachedProject {
                        alias_files: alias_fingerprint,
                        aliases: aliases.clone(),
                        cache_files: registry_fingerprint,
                        registry: Arc::clone(&registry),
                    },
                );
                (aliases, registry)
            }
        }
    };

    let known_erlang_modules: HashSet<String> =
        beamtalk_core::ffi_type_specs::collect_project_known_erlang_modules(&utf8_root);

    Some(FileProjectContext {
        diagnostics_table,
        analysis_project: ProjectAnalysisContext {
            package_name: if package_name.is_empty() {
                None
            } else {
                Some(package_name)
            },
            native_type_registry: if registry.module_count() == 0 {
                None
            } else {
                Some(registry)
            },
            known_erlang_modules,
        },
        aliases,
    })
}
