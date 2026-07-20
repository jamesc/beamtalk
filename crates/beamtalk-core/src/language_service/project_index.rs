// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Cross-file project index for the language service.
//!
//! **DDD Context:** Language Service — Aggregate Root
//!
//! `ProjectIndex` manages a merged [`ClassHierarchy`] across all open files,
//! pre-indexed with stdlib classes from `lib/*.bt`. It supports incremental
//! updates (add/remove/update file) and provides the foundation for cross-file
//! go-to-definition, find-references, and class-aware completions.
//!
//! # Architecture (ADR 0024, Phase 1)
//!
//! ```text
//! ProjectIndex (Aggregate Root)
//! ├── merged ClassHierarchy (builtins + stdlib + all open files)
//! ├── per-file class names (for incremental updates)
//! └── stdlib class names (pre-indexed from lib/*.bt)
//! ```

use crate::compilation::extension_index::ExtensionIndex;
use crate::semantic_analysis::{
    AliasInfo, AliasRegistry, ClassHierarchy, ProtocolInfo, ProtocolRegistry, SemanticError,
};
use crate::source_analysis::{Diagnostic, lex_with_eof, parse};
use camino::{Utf8Path, Utf8PathBuf};
use ecow::EcoString;
use std::collections::{HashMap, HashSet};

/// Package stamp used for a same-project (non-dependency, non-stdlib) file's
/// `AliasInfo.package` (BT-2951).
///
/// The language service has no access to a workspace's `beamtalk.toml`
/// package name — parsing manifests is deliberately `beamtalk-lsp`'s concern,
/// not `beamtalk-core`'s (mirrors why `beamtalk-lsp/src/server.rs`'s
/// dependency preload is filesystem-driven rather than manifest-driven — see
/// that module's `dependency_src_dirs` doc). This fixed marker stands in for
/// "this project's own package": not a real package name, but stable and
/// consistent within one [`ProjectIndex`], which is all
/// [`AliasRegistry::add_pre_loaded`]'s internal/cross-package exclusion
/// needs to tell a same-project alias apart from a dependency's. `$` is not
/// a valid character in a Hex/`beamtalk.toml` package name, so this can
/// never collide with a real dependency's directory-derived stamp (see
/// [`dependency_package_for_path`]).
const CURRENT_PROJECT_PACKAGE_MARKER: &str = "$project";

/// Package stamp for a stdlib file's `AliasInfo.package`, mirroring the CLI
/// build pipeline's convention (`build_stdlib.rs`'s `generate_app_file`,
/// `class_hierarchy/builtins.rs`'s `ClassInfo.package` stamping) — stdlib is
/// always its own package, distinct from both a project's own aliases and
/// any fetched dependency's.
const STDLIB_PACKAGE_MARKER: &str = "stdlib";

/// Derives the dependency package name for `file` from its path (BT-2951):
/// mirrors `beamtalk-lsp/src/server.rs`'s `dependency_src_dirs` filesystem
/// convention — any file under a `_build/deps/<name>/src/` directory belongs
/// to dependency `<name>`. `None` for a file with no such path segment
/// (a same-project file).
fn dependency_package_for_path(file: &Utf8Path) -> Option<EcoString> {
    let components: Vec<&str> = file.as_str().split('/').collect();
    components
        .windows(2)
        .position(|w| w == ["_build", "deps"])
        .and_then(|i| components.get(i + 2))
        .map(|name| EcoString::from(*name))
}

/// The package to stamp on every [`AliasInfo`] extracted from `file`
/// (BT-2951): the dependency name if `file` is under a
/// `_build/deps/<name>/src/` directory (see [`dependency_package_for_path`]),
/// otherwise [`CURRENT_PROJECT_PACKAGE_MARKER`].
fn package_for_alias_stamping(file: &Utf8Path) -> EcoString {
    dependency_package_for_path(file)
        .unwrap_or_else(|| EcoString::from(CURRENT_PROJECT_PACKAGE_MARKER))
}

/// Cross-file project index holding a merged class hierarchy.
///
/// **DDD Context:** Language Service — Aggregate Root
///
/// The `ProjectIndex` aggregates class information from all open files
/// and the stdlib, providing a project-wide view used by providers
/// (definition, completion, diagnostics).
#[derive(Debug, Clone)]
pub struct ProjectIndex {
    /// Merged class hierarchy (builtins + stdlib + all open files).
    merged_hierarchy: ClassHierarchy,
    /// Per-file tracking of which class names came from which file.
    file_classes: HashMap<Utf8PathBuf, Vec<EcoString>>,
    /// Per-file class hierarchies for correct rebuild on conflicts.
    file_hierarchies: HashMap<Utf8PathBuf, ClassHierarchy>,
    /// Class names from stdlib (never removed during file updates).
    stdlib_class_names: HashSet<EcoString>,
    /// Tracks which files were loaded as stdlib sources.
    stdlib_files: HashSet<Utf8PathBuf>,
    /// Per-file standalone extension definitions (BT-2795, ADR 0066).
    ///
    /// Tracked per file so incremental updates can drop a file's stale
    /// extensions; merged on demand by [`Self::cross_file_extensions_for`].
    file_extensions: HashMap<Utf8PathBuf, ExtensionIndex>,
    /// Merged type alias registry across all indexed files (ADR 0108 Phase
    /// 8, BT-2901) — the alias-namespace counterpart to `merged_hierarchy`.
    /// Rebuilt from [`Self::file_aliases`] on every [`Self::update_file_aliases`]
    /// / [`Self::remove_file`] call; see [`Self::rebuild_alias_registry`]'s
    /// doc for why a full rebuild (rather than incremental removal) is used.
    merged_aliases: AliasRegistry,
    /// Per-file tracking of which [`AliasInfo`] entries came from which file
    /// (mirrors `file_classes`), so [`Self::rebuild_alias_registry`] can
    /// recompute `merged_aliases` deterministically after an incremental
    /// update or removal.
    file_aliases: HashMap<Utf8PathBuf, Vec<AliasInfo>>,
    /// Per-file tracking of which [`ProtocolInfo`] declarations came from
    /// which file (BT-2950) — the protocol-namespace counterpart to
    /// `file_classes`/`file_aliases`, feeding
    /// [`Self::cross_file_protocol_infos_for`]. Unlike `file_aliases`, there
    /// is no merged registry to rebuild: `ProtocolInfo` carries no `package`
    /// field and protocols have no `internal` modifier at the AST level (see
    /// `beamtalk-cli`'s `collect_project_protocol_and_alias_infos` doc), so
    /// every declared protocol is exported project-wide with no
    /// collision/visibility bookkeeping needed here.
    file_protocols: HashMap<Utf8PathBuf, Vec<ProtocolInfo>>,
}

impl ProjectIndex {
    /// Create a new empty `ProjectIndex` with only built-in classes.
    #[must_use]
    pub fn new() -> Self {
        Self {
            merged_hierarchy: ClassHierarchy::with_builtins(),
            file_classes: HashMap::new(),
            file_hierarchies: HashMap::new(),
            stdlib_class_names: HashSet::new(),
            stdlib_files: HashSet::new(),
            file_extensions: HashMap::new(),
            merged_aliases: AliasRegistry::new(),
            file_aliases: HashMap::new(),
            file_protocols: HashMap::new(),
        }
    }

    /// Create a `ProjectIndex` pre-indexed with stdlib classes from source files.
    ///
    /// Each `(path, source)` pair is parsed and its class definitions are
    /// merged into the project-wide hierarchy.
    ///
    /// Returns `(Result<Self, SemanticError>, Vec<Diagnostic>)` following the
    /// project convention for fallible operations, even though in practice
    /// `ClassHierarchy::build` is infallible and will always return `Ok`.
    ///
    /// # Errors
    ///
    /// Returns [`SemanticError`] if `ClassHierarchy::build` fails for any
    /// stdlib source file (cannot happen in practice).
    pub fn with_stdlib(
        stdlib_sources: &[(Utf8PathBuf, String)],
    ) -> (Result<Self, SemanticError>, Vec<Diagnostic>) {
        let mut index = Self::new();
        let mut all_diagnostics = Vec::new();
        for (path, source) in stdlib_sources {
            let tokens = lex_with_eof(source);
            let (module, _parse_diagnostics) = parse(tokens);
            let (file_hierarchy_result, hierarchy_diags) = ClassHierarchy::build(&module);
            all_diagnostics.extend(hierarchy_diags);
            let mut file_hierarchy = match file_hierarchy_result {
                Ok(h) => h,
                Err(e) => return (Err(e), all_diagnostics),
            };

            // BT-1933: Register protocol definitions as synthetic class entries
            file_hierarchy.register_protocol_classes(&module);

            // Track which classes came from this stdlib file
            let class_names: Vec<EcoString> = file_hierarchy
                .class_names()
                .filter(|name| !ClassHierarchy::is_builtin_class(name))
                .cloned()
                .collect();
            index.stdlib_class_names.extend(class_names.iter().cloned());
            index.stdlib_files.insert(path.clone());
            index.file_classes.insert(path.clone(), class_names);
            index
                .file_hierarchies
                .insert(path.clone(), file_hierarchy.clone());
            index.merged_hierarchy.merge(&file_hierarchy);

            // BT-2795: Track standalone extensions defined in stdlib sources
            // so they are visible to cross-file diagnostics like any other
            // indexed file's extensions.
            let mut extensions = ExtensionIndex::new();
            extensions.add_module(&module, path.as_std_path());
            index.set_file_extensions(path.clone(), extensions);

            // ADR 0108 Phase 8 (BT-2901): track stdlib `type` declarations
            // too, so a stdlib-defined alias is offered in completions and
            // resolves go-to-definition/hover/find-references the same as
            // any project-file alias. Collision detection against
            // `merged_hierarchy` runs once at the end via
            // `rebuild_alias_registry` (after every stdlib file's classes
            // have been merged), not per-file here, so a stdlib alias
            // colliding with a class declared in a *later* stdlib file is
            // still caught.
            // BT-2951: stdlib aliases are stamped `stdlib`, not the
            // same-project marker — see `STDLIB_PACKAGE_MARKER`'s doc.
            let mut alias_infos = AliasRegistry::extract_alias_infos(&module);
            for info in &mut alias_infos {
                info.package = Some(EcoString::from(STDLIB_PACKAGE_MARKER));
            }
            if !alias_infos.is_empty() {
                index.file_aliases.insert(path.clone(), alias_infos);
            }

            // BT-2950: track stdlib `Protocol define: ...` declarations too,
            // so a stdlib-defined protocol resolves cross-file (`extending:`/
            // conformance checks) the same as any project-file protocol —
            // mirrors the alias tracking immediately above.
            let protocol_infos = ProtocolRegistry::extract_protocol_infos(&module);
            if !protocol_infos.is_empty() {
                index.file_protocols.insert(path.clone(), protocol_infos);
            }
        }
        index.rebuild_alias_registry();
        (Ok(index), all_diagnostics)
    }

    /// Returns the merged class hierarchy across all files.
    #[must_use]
    pub fn hierarchy(&self) -> &ClassHierarchy {
        &self.merged_hierarchy
    }

    /// Returns the merged type alias registry across all indexed files
    /// (ADR 0108 Phase 8, BT-2901) — the alias-namespace counterpart to
    /// [`Self::hierarchy`]. Used by completion/definition/references/hover
    /// providers to resolve alias names project-wide.
    #[must_use]
    pub fn alias_registry(&self) -> &AliasRegistry {
        &self.merged_aliases
    }

    /// Add or update a file in the index.
    ///
    /// If the file was previously indexed, its old class definitions are
    /// removed and then re-merged from remaining files to handle conflicts
    /// correctly (two files defining the same class name).
    pub fn update_file(&mut self, file: Utf8PathBuf, hierarchy: &ClassHierarchy) {
        // Remove old classes from this file (if any)
        if let Some(old_names) = self.file_classes.remove(&file) {
            self.merged_hierarchy.remove_classes(&old_names);
            self.file_hierarchies.remove(&file);

            // Re-merge classes from other files that may have been shadowed
            self.remerge_classes(&old_names);
        }

        // Track new class names from this file
        let new_names: Vec<EcoString> = hierarchy
            .class_names()
            .filter(|name| !ClassHierarchy::is_builtin_class(name))
            .cloned()
            .collect();

        self.file_classes.insert(file.clone(), new_names);
        self.file_hierarchies.insert(file, hierarchy.clone());
        self.merged_hierarchy.merge(hierarchy);

        // ADR 0108 Phase 8 (BT-2901): a class/protocol change in *this* file
        // can create or resolve a namespace collision with an alias
        // declared in a *different* file — `rebuild_alias_registry`'s
        // collision check runs against `merged_hierarchy`, not just
        // same-file siblings, so e.g. this file newly defining `class Foo`
        // must evict an already-registered `type Foo = ...` alias from
        // elsewhere in the project (the class wins the namespace), and
        // removing/renaming a colliding class must let a previously-shadowed
        // alias re-register. Skipped when no file has any tracked alias
        // (`file_aliases.is_empty()`) — the common case for a project that
        // doesn't use type aliases at all — so an alias-less project's class
        // edits pay no extra cost from this check.
        if !self.file_aliases.is_empty() {
            self.rebuild_alias_registry();
        }
    }

    /// Add or update a file's type alias declarations in the project index
    /// (ADR 0108 Phase 8, BT-2901) — the alias-namespace counterpart to
    /// [`Self::update_file`]. Call after [`Self::update_file`] so the merged
    /// `ClassHierarchy` used for alias/class collision detection already
    /// reflects this call's class/protocol changes.
    ///
    /// An empty `aliases` clears the file's prior entry (mirrors
    /// [`Self::set_file_extensions`]).
    ///
    /// No-ops (skips [`Self::rebuild_alias_registry`] entirely) when `file`
    /// had no tracked aliases before *and* `aliases` is empty now — this
    /// method is called on every [`Self::update_file`]-adjacent LSP
    /// `update_file`/keystroke, and the overwhelming majority of files in a
    /// project declare no `type` aliases at all, so the common case must not
    /// pay for an O(every-file-with-aliases) rebuild it can't possibly
    /// change the outcome of (unlike [`Self::update_file`]'s
    /// `ClassHierarchy::merge`, which is bounded by the *incoming* file's
    /// own class count, `rebuild_alias_registry` has no incremental API and
    /// must re-walk every alias-bearing file from scratch — see its doc).
    ///
    /// BT-2951: stamps each entry's `AliasInfo.package` from `file`'s own
    /// path (overwriting whatever the caller passed in, if anything) — see
    /// [`package_for_alias_stamping`] — so [`Self::cross_file_alias_infos_for`]
    /// and [`Self::rebuild_alias_registry`] both have real package data for
    /// [`AliasRegistry::add_pre_loaded`]'s internal/cross-package exclusion
    /// to act on, instead of every entry looking package-less.
    pub fn update_file_aliases(&mut self, file: Utf8PathBuf, aliases: Vec<AliasInfo>) {
        let had_aliases = self.file_aliases.contains_key(&file);
        if aliases.is_empty() {
            if !had_aliases {
                return;
            }
            self.file_aliases.remove(&file);
        } else {
            let package = package_for_alias_stamping(&file);
            let stamped: Vec<AliasInfo> = aliases
                .into_iter()
                .map(|mut info| {
                    info.package = Some(package.clone());
                    info
                })
                .collect();
            self.file_aliases.insert(file, stamped);
        }
        self.rebuild_alias_registry();
    }

    /// Add or update a file's protocol declarations in the project index
    /// (BT-2950) — the protocol-namespace counterpart to
    /// [`Self::update_file_aliases`]. Call after [`Self::update_file`], same
    /// ordering rationale.
    ///
    /// An empty `protocols` clears the file's prior entry (mirrors
    /// [`Self::update_file_aliases`]). No merged-registry rebuild needed
    /// here — see [`Self::file_protocols`]'s doc for why.
    pub fn update_file_protocols(&mut self, file: Utf8PathBuf, protocols: Vec<ProtocolInfo>) {
        if protocols.is_empty() {
            self.file_protocols.remove(&file);
        } else {
            self.file_protocols.insert(file, protocols);
        }
    }

    /// Rebuilds [`Self::merged_aliases`] from scratch across every indexed
    /// file's tracked [`AliasInfo`] entries, in deterministic (sorted-path)
    /// iteration order — mirrors [`Self::remerge_classes`]'s determinism
    /// rationale.
    ///
    /// [`AliasRegistry`] has no incremental removal API (unlike
    /// [`ClassHierarchy::remove_classes`]), so a full rebuild is the
    /// simplest correct way to keep the merged view in sync with
    /// [`Self::update_file_aliases`] / [`Self::remove_file`]. Alias counts
    /// are small relative to class counts (one entry per `type`
    /// declaration in a project), so this is not a hot-path performance
    /// concern. Collision diagnostics from [`AliasRegistry::add_pre_loaded`]
    /// are discarded here — they were already surfaced once, per file, by
    /// the full semantic-analysis pipeline behind `diagnostics()`; this
    /// rebuild only needs the resulting table, not a second round of
    /// diagnostics.
    ///
    /// BT-2951: seeds with `current_package =
    /// Some(`[`CURRENT_PROJECT_PACKAGE_MARKER`]`)` — every same-project
    /// file's aliases were stamped with that same marker by
    /// [`Self::update_file_aliases`]/[`Self::with_stdlib`], so
    /// [`AliasRegistry::add_pre_loaded`]'s exclusion correctly drops an
    /// `internal` alias stamped with a *different* package (a dependency's
    /// directory-derived name, or [`STDLIB_PACKAGE_MARKER`]) from this
    /// project-wide merged view, while keeping every same-project `internal`
    /// alias visible throughout the project.
    fn rebuild_alias_registry(&mut self) {
        let mut registry = AliasRegistry::new();
        let mut sorted_paths: Vec<&Utf8PathBuf> = self.file_aliases.keys().collect();
        sorted_paths.sort();
        let empty_protocols = ProtocolRegistry::new();
        for path in sorted_paths {
            let infos = self.file_aliases[path].clone();
            let _ = registry.add_pre_loaded(
                infos,
                &self.merged_hierarchy,
                &empty_protocols,
                Some(CURRENT_PROJECT_PACKAGE_MARKER),
            );
        }
        self.merged_aliases = registry;
    }

    /// Record the standalone extension definitions contributed by `file`
    /// (BT-2795). An empty index clears the file's entry. Call after
    /// [`Self::update_file`] whenever the file is (re)indexed.
    pub fn set_file_extensions(&mut self, file: Utf8PathBuf, extensions: ExtensionIndex) {
        if extensions.is_empty() {
            self.file_extensions.remove(&file);
        } else {
            self.file_extensions.insert(file, extensions);
        }
    }

    /// Returns a merged extension index over every indexed file except
    /// `file` (BT-2795) — the extension analogue of
    /// [`Self::cross_file_class_infos_for`].
    #[must_use]
    pub fn cross_file_extensions_for(&self, file: &Utf8PathBuf) -> ExtensionIndex {
        let mut merged = ExtensionIndex::new();
        for (path, index) in &self.file_extensions {
            if path != file {
                merged.merge(index);
            }
        }
        merged
    }

    /// Remove a file from the index.
    ///
    /// Classes defined only in this file are removed from the merged hierarchy.
    /// Classes also defined in other files are preserved (re-merged).
    pub fn remove_file(&mut self, file: &Utf8PathBuf) {
        if self.stdlib_files.contains(file) {
            // Stdlib files are never truly removed — preserve their hierarchy
            return;
        }
        self.file_extensions.remove(file);
        let mut classes_changed = false;
        if let Some(old_names) = self.file_classes.remove(file) {
            self.file_hierarchies.remove(file);

            // Remove all classes from this file (including stdlib overrides)
            self.merged_hierarchy.remove_classes(&old_names);

            // Re-merge classes from remaining files that shared a name
            // (this restores stdlib definitions if they were shadowed)
            self.remerge_classes(&old_names);
            classes_changed = true;
        }
        // ADR 0108 Phase 8 (BT-2901): drop this file's aliases too, mirroring
        // the class-removal handling above. A rebuild is also needed (once,
        // not twice) when this file's *classes* changed and some other
        // file's alias might have been shadowed by (or is now unshadowed
        // from) one of them — the same cross-file collision case
        // `update_file` now guards against; see its doc.
        let had_aliases = self.file_aliases.remove(file).is_some();
        if had_aliases || (classes_changed && !self.file_aliases.is_empty()) {
            self.rebuild_alias_registry();
        }
        // BT-2950: drop this file's protocols too, mirroring the alias
        // removal above — no rebuild needed (no merged registry; see
        // `file_protocols`'s doc).
        self.file_protocols.remove(file);
    }

    /// Re-merge class definitions from remaining files for the given class names.
    ///
    /// When a file is removed/updated, classes it defined may also exist in
    /// other files. This method restores the first definition found, using
    /// deterministic (sorted by path) iteration order.
    fn remerge_classes(&mut self, names: &[EcoString]) {
        // Sort file paths for deterministic conflict resolution
        let mut sorted_paths: Vec<&Utf8PathBuf> = self.file_hierarchies.keys().collect();
        sorted_paths.sort();

        let mut changed = false;
        for name in names {
            for path in &sorted_paths {
                if let Some(info) = self.file_hierarchies[*path].classes().get(name) {
                    self.merged_hierarchy
                        .classes_mut()
                        .insert(name.clone(), info.clone());
                    changed = true;
                    break;
                }
            }
        }
        if changed {
            self.merged_hierarchy.rebuild_all_indexes();
        }
    }

    /// Returns the set of files currently indexed.
    #[must_use]
    pub fn indexed_files(&self) -> Vec<&Utf8PathBuf> {
        self.file_classes.keys().collect()
    }

    /// Returns the class names contributed by a specific file.
    #[must_use]
    pub fn classes_in_file(&self, file: &Utf8PathBuf) -> Option<&[EcoString]> {
        self.file_classes.get(file).map(Vec::as_slice)
    }

    /// Returns `true` if `file` was loaded as part of the stdlib source set.
    ///
    /// BT-2027: `SimpleLanguageService::diagnostics` uses this to suppress the
    /// "conflicts with a stdlib class" shadowing check for stdlib files
    /// themselves (which would otherwise flag every class they define).
    #[must_use]
    pub fn is_stdlib_file(&self, file: &Utf8PathBuf) -> bool {
        self.stdlib_files.contains(file)
    }

    /// Returns cross-file `ClassInfo` entries for diagnostic computation (BT-2009).
    ///
    /// Returns all `ClassInfo` entries from the merged hierarchy that were NOT
    /// contributed by the given `file`. This is the LSP equivalent of the CLI's
    /// `ClassHierarchy::cross_file_class_infos` filtering.
    #[must_use]
    pub fn cross_file_class_infos_for(
        &self,
        file: &Utf8PathBuf,
    ) -> Vec<crate::semantic_analysis::class_hierarchy::ClassInfo> {
        let current_file_classes: std::collections::HashSet<&EcoString> = self
            .file_classes
            .get(file)
            .map(|names| names.iter().collect())
            .unwrap_or_default();

        self.merged_hierarchy
            .classes()
            .iter()
            .filter(|(name, _)| {
                !ClassHierarchy::is_builtin_class(name) && !current_file_classes.contains(name)
            })
            .map(|(_, info)| info.clone())
            .collect()
    }

    /// Returns cross-file `AliasInfo` entries for diagnostic computation
    /// (BT-2928) — the alias-namespace analogue of
    /// [`Self::cross_file_class_infos_for`]. Returns every tracked alias
    /// declaration from files other than `file`, for seeding into
    /// `ProjectDiagnosticContext::pre_loaded_aliases` so a `type Name = ...`
    /// declared in a different project file resolves during LSP diagnostics
    /// the same way a cross-file class reference already does.
    ///
    /// BT-2951: every returned entry's `package` is populated (by
    /// [`Self::update_file_aliases`]/[`Self::with_stdlib`] at insertion time,
    /// not here) — an `internal` entry from a different package than `file`'s
    /// own is *not* filtered out by this method; the caller must pass
    /// [`Self::alias_package_for_file`]`(file)` as `current_package` to
    /// [`AliasRegistry::add_pre_loaded`] (via `CompilerOptions::current_package`)
    /// so that existing seeding-boundary exclusion — which already runs
    /// downstream wherever these entries are seeded — has real package data
    /// to filter on, mirroring how the file-classes analogue,
    /// [`Self::cross_file_class_infos_for`], relies on the type checker's own
    /// downstream `internal` visibility check rather than filtering here.
    #[must_use]
    pub fn cross_file_alias_infos_for(&self, file: &Utf8PathBuf) -> Vec<AliasInfo> {
        self.file_aliases
            .iter()
            .filter(|(path, _)| *path != file)
            .flat_map(|(_, infos)| infos.iter().cloned())
            .collect()
    }

    /// Returns cross-file `ProtocolInfo` entries for diagnostic computation
    /// (BT-2950) — the protocol-namespace analogue of
    /// [`Self::cross_file_alias_infos_for`]. Returns every tracked protocol
    /// declaration from files other than `file` (same-project or indexed
    /// `_build/deps/*/src/` dependency files), for seeding into
    /// `ProjectDiagnosticContext::pre_loaded_protocols` so a `Protocol
    /// define: Name ...` declared in a different project file or dependency
    /// resolves during LSP diagnostics (`extending:`/conformance checks) the
    /// same way `beamtalk build`/`beamtalk lint` already do (BT-2910). No
    /// package filtering — protocols have no `internal` modifier, so every
    /// declared protocol is exported project-wide (see [`Self::file_protocols`]'s
    /// doc).
    #[must_use]
    pub fn cross_file_protocol_infos_for(&self, file: &Utf8PathBuf) -> Vec<ProtocolInfo> {
        self.file_protocols
            .iter()
            .filter(|(path, _)| *path != file)
            .flat_map(|(_, infos)| infos.iter().cloned())
            .collect()
    }

    /// The package identity to pass as `current_package` when seeding `file`'s
    /// diagnostics/analysis with cross-file aliases (BT-2951) — pairs with
    /// [`Self::cross_file_alias_infos_for`]'s entries, which were stamped
    /// with the exact same derivation at insertion time (see
    /// [`package_for_alias_stamping`]), so `AliasRegistry::add_pre_loaded`'s
    /// `info.package.as_deref() != current_package` comparison lines up.
    ///
    /// Checks [`Self::is_stdlib_file`] first (like [`Self::with_stdlib`]
    /// stamps stdlib aliases [`STDLIB_PACKAGE_MARKER`] regardless of path
    /// shape) before falling back to the same path-based derivation
    /// [`Self::update_file_aliases`] uses for every other file.
    #[must_use]
    pub fn alias_package_for_file(&self, file: &Utf8PathBuf) -> EcoString {
        if self.is_stdlib_file(file) {
            EcoString::from(STDLIB_PACKAGE_MARKER)
        } else {
            package_for_alias_stamping(file)
        }
    }

    /// Returns the package name for a file, if determinable.
    ///
    /// Uses the file-local hierarchy (not the merged hierarchy) so that
    /// two files defining the same class name in different packages don't
    /// contaminate each other's package resolution. Only checks classes
    /// contributed by this file (from `file_classes`), ignoring builtins
    /// that are always present in every hierarchy.
    ///
    /// Returns `None` for REPL files, scripts, or files whose classes have
    /// no package set (e.g., not yet compiled with a package context).
    #[must_use]
    pub fn package_for_file(&self, file: &Utf8PathBuf) -> Option<EcoString> {
        let class_names = self.file_classes.get(file)?;
        let file_hierarchy = self.file_hierarchies.get(file)?;
        for name in class_names {
            if let Some(info) = file_hierarchy.classes().get(name.as_str()) {
                if let Some(ref pkg) = info.package {
                    return Some(pkg.clone());
                }
            }
        }
        None
    }
}

impl Default for ProjectIndex {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_has_only_builtins() {
        let index = ProjectIndex::new();
        // Should have built-in classes (Object, Integer, etc.)
        assert!(index.hierarchy().has_class("Object"));
        assert!(index.hierarchy().has_class("Integer"));
        assert!(index.file_classes.is_empty());
    }

    #[test]
    fn with_stdlib_indexes_classes() {
        let stdlib = vec![(
            Utf8PathBuf::from("stdlib/src/Counter.bt"),
            "Object subclass: Counter\n  increment => 1".to_string(),
        )];
        let (index_result, _) = ProjectIndex::with_stdlib(&stdlib);
        let index = index_result.unwrap();
        assert!(index.hierarchy().has_class("Counter"));
        assert!(
            index
                .stdlib_class_names
                .contains(&EcoString::from("Counter"))
        );
    }

    #[test]
    fn update_file_adds_classes() {
        let mut index = ProjectIndex::new();
        let file = Utf8PathBuf::from("test.bt");

        let tokens = lex_with_eof("Object subclass: Foo\n  bar => 1");
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        index.update_file(file.clone(), &hierarchy);

        assert!(index.hierarchy().has_class("Foo"));
        assert_eq!(
            index.classes_in_file(&file).unwrap(),
            &[EcoString::from("Foo")]
        );
    }

    #[test]
    fn update_file_replaces_old_classes() {
        let mut index = ProjectIndex::new();
        let file = Utf8PathBuf::from("test.bt");

        // First version defines Foo
        let tokens = lex_with_eof("Object subclass: Foo\n  bar => 1");
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        index.update_file(file.clone(), &hierarchy);
        assert!(index.hierarchy().has_class("Foo"));

        // Second version defines Bar instead
        let tokens = lex_with_eof("Object subclass: Bar\n  baz => 2");
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        index.update_file(file.clone(), &hierarchy);
        assert!(index.hierarchy().has_class("Bar"));
        assert!(!index.hierarchy().has_class("Foo"));
    }

    #[test]
    fn remove_file_removes_classes() {
        let mut index = ProjectIndex::new();
        let file = Utf8PathBuf::from("test.bt");

        let tokens = lex_with_eof("Object subclass: Foo\n  bar => 1");
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        index.update_file(file.clone(), &hierarchy);
        assert!(index.hierarchy().has_class("Foo"));

        index.remove_file(&file);
        assert!(!index.hierarchy().has_class("Foo"));
    }

    #[test]
    fn remove_file_preserves_stdlib() {
        let stdlib = vec![(
            Utf8PathBuf::from("stdlib/src/Counter.bt"),
            "Object subclass: Counter\n  increment => 1".to_string(),
        )];
        let mut index = ProjectIndex::with_stdlib(&stdlib).0.unwrap();
        assert!(index.hierarchy().has_class("Counter"));

        // Remove the stdlib file — Counter should persist because it's stdlib
        index.remove_file(&Utf8PathBuf::from("stdlib/src/Counter.bt"));
        // Stdlib classes are tracked separately
        assert!(
            index
                .stdlib_class_names
                .contains(&EcoString::from("Counter"))
        );
    }

    #[test]
    fn cross_file_classes_visible() {
        let mut index = ProjectIndex::new();

        let tokens_a = lex_with_eof("Object subclass: ClassA\n  methodA => 1");
        let (module_a, _) = parse(tokens_a);
        let hierarchy_a = ClassHierarchy::build(&module_a).0.unwrap();
        index.update_file(Utf8PathBuf::from("a.bt"), &hierarchy_a);

        let tokens_b = lex_with_eof("Object subclass: ClassB\n  methodB => 2");
        let (module_b, _) = parse(tokens_b);
        let hierarchy_b = ClassHierarchy::build(&module_b).0.unwrap();
        index.update_file(Utf8PathBuf::from("b.bt"), &hierarchy_b);

        // Both classes visible in merged hierarchy
        assert!(index.hierarchy().has_class("ClassA"));
        assert!(index.hierarchy().has_class("ClassB"));
    }

    #[test]
    fn remerge_deterministic_on_conflict() {
        // Two files define the same class name with different methods.
        // After removing one, the surviving definition should be deterministic
        // (alphabetically first file wins).
        let mut index = ProjectIndex::new();

        // a.bt defines Dup with methodA
        let tokens_a = lex_with_eof("Object subclass: Dup\n  methodA => 1");
        let (module_a, _) = parse(tokens_a);
        let hierarchy_a = ClassHierarchy::build(&module_a).0.unwrap();
        index.update_file(Utf8PathBuf::from("a.bt"), &hierarchy_a);

        // b.bt defines Dup with methodB
        let tokens_b = lex_with_eof("Object subclass: Dup\n  methodB => 2");
        let (module_b, _) = parse(tokens_b);
        let hierarchy_b = ClassHierarchy::build(&module_b).0.unwrap();
        index.update_file(Utf8PathBuf::from("b.bt"), &hierarchy_b);

        // b.bt was last to merge, so Dup currently has methodB
        let class = index.hierarchy().get_class("Dup").unwrap();
        assert!(class.methods.iter().any(|m| m.selector == "methodB"));

        // Remove b.bt — a.bt's definition should survive (deterministic)
        index.remove_file(&Utf8PathBuf::from("b.bt"));
        assert!(index.hierarchy().has_class("Dup"));
        let class = index.hierarchy().get_class("Dup").unwrap();
        assert!(class.methods.iter().any(|m| m.selector == "methodA"));
    }

    #[test]
    fn stdlib_restored_after_user_override_removed() {
        // Stdlib defines Counter with increment.
        // User file shadows Counter with decrement.
        // Removing the user file should restore the stdlib Counter.
        let stdlib = vec![(
            Utf8PathBuf::from("stdlib/src/Counter.bt"),
            "Object subclass: Counter\n  increment => 1".to_string(),
        )];
        let mut index = ProjectIndex::with_stdlib(&stdlib).0.unwrap();
        assert!(index.hierarchy().has_class("Counter"));

        // User file shadows stdlib Counter
        let user_file = Utf8PathBuf::from("user/counter.bt");
        let tokens = lex_with_eof("Object subclass: Counter\n  decrement => 1");
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        index.update_file(user_file.clone(), &hierarchy);

        // User's Counter (with decrement) is now the active definition
        let class = index.hierarchy().get_class("Counter").unwrap();
        assert!(class.methods.iter().any(|m| m.selector == "decrement"));

        // Remove the user file — stdlib Counter should be restored
        index.remove_file(&user_file);
        assert!(index.hierarchy().has_class("Counter"));
        let class = index.hierarchy().get_class("Counter").unwrap();
        assert!(class.methods.iter().any(|m| m.selector == "increment"));
    }

    #[test]
    fn package_for_file_returns_package_from_hierarchy() {
        let mut index = ProjectIndex::new();
        let file = Utf8PathBuf::from("packages/my_app/src/MyClass.bt");

        let tokens = lex_with_eof("Object subclass: MyClass\n  doStuff => 1");
        let (module, _) = parse(tokens);
        let mut hierarchy = ClassHierarchy::build(&module).0.unwrap();
        // Stamp the package on the hierarchy (simulating build pipeline behaviour)
        hierarchy.stamp_package("my_app");
        index.update_file(file.clone(), &hierarchy);

        assert_eq!(
            index.package_for_file(&file).as_deref(),
            Some("my_app"),
            "package_for_file should return the package from the hierarchy"
        );
    }

    #[test]
    fn package_for_file_returns_none_without_package() {
        let mut index = ProjectIndex::new();
        let file = Utf8PathBuf::from("scratch.bt");

        let tokens = lex_with_eof("Object subclass: Scratch\n  run => 1");
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        index.update_file(file.clone(), &hierarchy);

        assert_eq!(
            index.package_for_file(&file),
            None,
            "package_for_file should return None when no package is set"
        );
    }

    #[test]
    fn package_for_file_returns_none_for_unknown_file() {
        let index = ProjectIndex::new();
        let file = Utf8PathBuf::from("nonexistent.bt");
        assert_eq!(index.package_for_file(&file), None);
    }

    // BT-1933: Protocol class objects in project index

    #[test]
    fn protocol_classes_visible_cross_file() {
        let mut index = ProjectIndex::new();

        // File A defines a protocol
        let tokens_a = lex_with_eof("Protocol define: Printable\n  asString -> String");
        let (module_a, _) = parse(tokens_a);
        let mut hierarchy_a = ClassHierarchy::build(&module_a).0.unwrap();
        hierarchy_a.register_protocol_classes(&module_a);
        index.update_file(Utf8PathBuf::from("a.bt"), &hierarchy_a);

        // File B defines a class
        let tokens_b = lex_with_eof("Object subclass: Foo\n  bar => 1");
        let (module_b, _) = parse(tokens_b);
        let hierarchy_b = ClassHierarchy::build(&module_b).0.unwrap();
        index.update_file(Utf8PathBuf::from("b.bt"), &hierarchy_b);

        // Both should be visible in merged hierarchy
        assert!(
            index.hierarchy().has_class("Printable"),
            "Protocol from file A should be visible in merged hierarchy"
        );
        assert!(
            index.hierarchy().has_class("Foo"),
            "Class from file B should be visible in merged hierarchy"
        );
    }

    #[test]
    fn protocol_class_removed_with_file() {
        let mut index = ProjectIndex::new();

        let tokens = lex_with_eof("Protocol define: Printable\n  asString -> String");
        let (module, _) = parse(tokens);
        let mut hierarchy = ClassHierarchy::build(&module).0.unwrap();
        hierarchy.register_protocol_classes(&module);
        index.update_file(Utf8PathBuf::from("proto.bt"), &hierarchy);
        assert!(index.hierarchy().has_class("Printable"));

        index.remove_file(&Utf8PathBuf::from("proto.bt"));
        assert!(
            !index.hierarchy().has_class("Printable"),
            "Protocol class should be removed when file is removed"
        );
    }

    #[test]
    fn real_class_not_overwritten_by_protocol_cross_file() {
        // If file A defines class Foo and file B defines protocol Foo,
        // the real class should win regardless of indexing order.
        let mut index = ProjectIndex::new();

        // File A: real class
        let tokens_a = lex_with_eof("Object subclass: Foo\n  bar => 1");
        let (module_a, _) = parse(tokens_a);
        let hierarchy_a = ClassHierarchy::build(&module_a).0.unwrap();
        index.update_file(Utf8PathBuf::from("a.bt"), &hierarchy_a);

        // File B: protocol with same name (indexed after)
        let tokens_b = lex_with_eof("Protocol define: Foo\n  baz");
        let (module_b, _) = parse(tokens_b);
        let mut hierarchy_b = ClassHierarchy::build(&module_b).0.unwrap();
        hierarchy_b.register_protocol_classes(&module_b);
        index.update_file(Utf8PathBuf::from("b.bt"), &hierarchy_b);

        // Real class should still be the active definition
        let class = index.hierarchy().get_class("Foo").unwrap();
        assert_eq!(
            class.superclass.as_deref(),
            Some("Object"),
            "Real class Foo should not be overwritten by protocol Foo from another file"
        );
    }

    // ---- ADR 0108 Phase 8 (BT-2901): project-wide alias registry ----

    #[test]
    fn update_file_aliases_no_op_for_alias_less_file_preserves_other_files_aliases() {
        // Perf guard: a file with zero aliases (the common case) must not
        // trigger a registry rebuild that could accidentally disturb
        // aliases tracked for *other* files — and repeatedly calling
        // `update_file_aliases([])` for it (as every keystroke-triggered
        // `SimpleLanguageService::update_file` does) must stay a true no-op.
        let mut index = ProjectIndex::new();

        let alias_tokens = lex_with_eof("type RestartStrategy = #temporary | #transient");
        let (alias_module, _) = parse(alias_tokens);
        let alias_hierarchy = ClassHierarchy::build(&alias_module).0.unwrap();
        index.update_file(Utf8PathBuf::from("aliases.bt"), &alias_hierarchy);
        index.update_file_aliases(
            Utf8PathBuf::from("aliases.bt"),
            crate::semantic_analysis::AliasRegistry::extract_alias_infos(&alias_module),
        );
        assert!(index.alias_registry().has_alias("RestartStrategy"));

        // Now repeatedly "edit" an unrelated, alias-less file.
        let plain_tokens = lex_with_eof("Object subclass: Foo\n  bar => 1");
        let (plain_module, _) = parse(plain_tokens);
        let plain_hierarchy = ClassHierarchy::build(&plain_module).0.unwrap();
        for _ in 0..3 {
            index.update_file(Utf8PathBuf::from("foo.bt"), &plain_hierarchy);
            index.update_file_aliases(Utf8PathBuf::from("foo.bt"), Vec::new());
        }

        assert!(
            index.alias_registry().has_alias("RestartStrategy"),
            "an unrelated alias-less file's updates must not disturb other files' aliases"
        );
    }

    #[test]
    fn update_file_aliases_adds_alias_to_merged_registry() {
        let mut index = ProjectIndex::new();
        let file = Utf8PathBuf::from("aliases.bt");

        let tokens = lex_with_eof("type RestartStrategy = #temporary | #transient | #permanent");
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        index.update_file(file.clone(), &hierarchy);
        let infos = crate::semantic_analysis::AliasRegistry::extract_alias_infos(&module);
        index.update_file_aliases(file, infos);

        assert!(index.alias_registry().has_alias("RestartStrategy"));
    }

    #[test]
    fn update_file_aliases_replaces_old_aliases() {
        let mut index = ProjectIndex::new();
        let file = Utf8PathBuf::from("aliases.bt");

        let tokens = lex_with_eof("type Foo = Integer");
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        index.update_file(file.clone(), &hierarchy);
        let infos = crate::semantic_analysis::AliasRegistry::extract_alias_infos(&module);
        index.update_file_aliases(file.clone(), infos);
        assert!(index.alias_registry().has_alias("Foo"));

        // Re-index the same file with a differently-named alias.
        let tokens = lex_with_eof("type Bar = Symbol");
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        index.update_file(file.clone(), &hierarchy);
        let infos = crate::semantic_analysis::AliasRegistry::extract_alias_infos(&module);
        index.update_file_aliases(file, infos);

        assert!(index.alias_registry().has_alias("Bar"));
        assert!(
            !index.alias_registry().has_alias("Foo"),
            "stale alias from the previous version of the file must be dropped"
        );
    }

    /// BT-2951: `update_file_aliases` must stamp `AliasInfo.package` from the
    /// file's own path — a dependency file (under `_build/deps/<name>/src/`)
    /// gets `<name>`, a same-project file gets the same-project marker.
    /// `AliasRegistry::add_pre_loaded`'s seeding-boundary exclusion (ADR 0108
    /// Phase 5) has nothing to filter on without this — every entry looked
    /// package-less before this fix.
    #[test]
    fn update_file_aliases_stamps_package_from_file_path() {
        let mut index = ProjectIndex::new();
        let dep_file = Utf8PathBuf::from("_build/deps/http/src/Types.bt");
        let project_file = Utf8PathBuf::from("src/Types.bt");

        let tokens = lex_with_eof("type Foo = Integer");
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        index.update_file(dep_file.clone(), &hierarchy);
        let infos = crate::semantic_analysis::AliasRegistry::extract_alias_infos(&module);
        index.update_file_aliases(dep_file.clone(), infos);

        let dep_infos = index.cross_file_alias_infos_for(&Utf8PathBuf::from("elsewhere.bt"));
        assert_eq!(
            dep_infos.iter().find(|i| i.name == "Foo").unwrap().package,
            Some(EcoString::from("http")),
            "a dependency file's alias should be stamped with its directory-derived package name"
        );

        index.remove_file(&dep_file);
        let tokens = lex_with_eof("type Foo = Integer");
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        index.update_file(project_file.clone(), &hierarchy);
        let infos = crate::semantic_analysis::AliasRegistry::extract_alias_infos(&module);
        index.update_file_aliases(project_file, infos);

        let project_infos = index.cross_file_alias_infos_for(&Utf8PathBuf::from("elsewhere.bt"));
        assert_eq!(
            project_infos
                .iter()
                .find(|i| i.name == "Foo")
                .unwrap()
                .package,
            Some(EcoString::from(CURRENT_PROJECT_PACKAGE_MARKER)),
            "a same-project file's alias should be stamped with the same-project marker"
        );
    }

    #[test]
    fn remove_file_drops_its_aliases() {
        let mut index = ProjectIndex::new();
        let file = Utf8PathBuf::from("aliases.bt");

        let tokens = lex_with_eof("type RestartStrategy = #temporary | #transient | #permanent");
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        index.update_file(file.clone(), &hierarchy);
        let infos = crate::semantic_analysis::AliasRegistry::extract_alias_infos(&module);
        index.update_file_aliases(file.clone(), infos);
        assert!(index.alias_registry().has_alias("RestartStrategy"));

        index.remove_file(&file);
        assert!(!index.alias_registry().has_alias("RestartStrategy"));
    }

    #[test]
    fn alias_colliding_with_cross_file_class_is_not_registered() {
        // Mirrors `AliasRegistry::add_pre_loaded`'s collision check — a
        // cross-file alias whose name collides with a real class must be
        // excluded from the merged registry entirely (the class wins the
        // namespace, per ADR 0108).
        let mut index = ProjectIndex::new();

        let class_tokens = lex_with_eof("Object subclass: Foo\n  bar => 1");
        let (class_module, _) = parse(class_tokens);
        let class_hierarchy = ClassHierarchy::build(&class_module).0.unwrap();
        index.update_file(Utf8PathBuf::from("foo.bt"), &class_hierarchy);

        let alias_tokens = lex_with_eof("type Foo = Integer");
        let (alias_module, _) = parse(alias_tokens);
        let alias_hierarchy = ClassHierarchy::build(&alias_module).0.unwrap();
        index.update_file(Utf8PathBuf::from("alias_foo.bt"), &alias_hierarchy);
        let infos = crate::semantic_analysis::AliasRegistry::extract_alias_infos(&alias_module);
        index.update_file_aliases(Utf8PathBuf::from("alias_foo.bt"), infos);

        assert!(
            !index.alias_registry().has_alias("Foo"),
            "alias colliding with a real class must not be registered"
        );
    }

    #[test]
    fn alias_evicted_when_a_later_alias_less_file_defines_a_colliding_class() {
        // Adversarial-review regression: the collision test above only
        // covers the *alias* file's own `update_file`/`update_file_aliases`
        // call triggering the collision check. This pins the other
        // direction — a plain class-only file (`update_file` with no
        // `update_file_aliases` call at all, the common shape for the vast
        // majority of files in a project) must *also* evict an
        // already-registered alias it collides with, not just leave the
        // stale (invalid) alias sitting in the merged registry until some
        // unrelated alias-file edit happens to trigger a rebuild.
        let mut index = ProjectIndex::new();

        // File A: alias `Foo` registers cleanly (no collision yet).
        let alias_tokens = lex_with_eof("type Foo = Integer");
        let (alias_module, _) = parse(alias_tokens);
        let alias_hierarchy = ClassHierarchy::build(&alias_module).0.unwrap();
        index.update_file(Utf8PathBuf::from("alias_foo.bt"), &alias_hierarchy);
        let infos = crate::semantic_analysis::AliasRegistry::extract_alias_infos(&alias_module);
        index.update_file_aliases(Utf8PathBuf::from("alias_foo.bt"), infos);
        assert!(index.alias_registry().has_alias("Foo"));

        // File B: a plain class-only file (never calls `update_file_aliases`
        // at all) later defines a colliding class `Foo`.
        let class_tokens = lex_with_eof("Object subclass: Foo\n  bar => 1");
        let (class_module, _) = parse(class_tokens);
        let class_hierarchy = ClassHierarchy::build(&class_module).0.unwrap();
        index.update_file(Utf8PathBuf::from("foo.bt"), &class_hierarchy);

        assert!(
            !index.alias_registry().has_alias("Foo"),
            "a colliding class defined in an alias-less file's update_file call must evict \
             the already-registered alias, not leave it stale"
        );

        // Removing the colliding class (file B) must let the alias
        // re-register — the inverse direction, exercised via `remove_file`.
        index.remove_file(&Utf8PathBuf::from("foo.bt"));
        assert!(
            index.alias_registry().has_alias("Foo"),
            "removing the colliding class should let the previously-shadowed alias \
             re-register"
        );
    }
}

#[cfg(test)]
mod extension_tests {
    use super::*;

    fn parse_and_index(index: &mut ProjectIndex, path: &str, source: &str) {
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        index.update_file(Utf8PathBuf::from(path), &hierarchy);
        let mut extensions = ExtensionIndex::new();
        extensions.add_module(&module, std::path::Path::new(path));
        index.set_file_extensions(Utf8PathBuf::from(path), extensions);
    }

    #[test]
    fn cross_file_extensions_exclude_own_file() {
        let mut index = ProjectIndex::new();
        parse_and_index(&mut index, "a.bt", "String >> fromA => self\n");
        parse_and_index(&mut index, "b.bt", "String >> fromB => self\n");

        let for_a = index.cross_file_extensions_for(&Utf8PathBuf::from("a.bt"));
        assert_eq!(for_a.len(), 1, "a.bt should see only b.bt's extension");
        let for_b = index.cross_file_extensions_for(&Utf8PathBuf::from("b.bt"));
        assert_eq!(for_b.len(), 1, "b.bt should see only a.bt's extension");
    }

    #[test]
    fn removed_file_extensions_disappear() {
        let mut index = ProjectIndex::new();
        parse_and_index(&mut index, "a.bt", "String >> fromA => self\n");
        parse_and_index(&mut index, "b.bt", "Object subclass: B\n  m => 1\n");

        index.remove_file(&Utf8PathBuf::from("a.bt"));
        let for_b = index.cross_file_extensions_for(&Utf8PathBuf::from("b.bt"));
        assert!(
            for_b.is_empty(),
            "removed file's extensions must not linger"
        );
    }

    #[test]
    fn empty_extension_set_clears_entry() {
        let mut index = ProjectIndex::new();
        parse_and_index(&mut index, "a.bt", "String >> fromA => self\n");
        // Re-index the same file without extensions.
        parse_and_index(&mut index, "a.bt", "Object subclass: A\n  m => 1\n");

        let for_other = index.cross_file_extensions_for(&Utf8PathBuf::from("other.bt"));
        assert!(
            for_other.is_empty(),
            "re-indexing without extensions must clear the stale entry"
        );
    }
}
