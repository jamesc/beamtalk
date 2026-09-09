// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Pass 1: class-to-module index construction, dependency-index merging, and
//! cross-file protocol/alias collection.

use camino::{Utf8Path, Utf8PathBuf};
use miette::Result;
use std::collections::HashMap;
use std::fs;
use tracing::{debug, warn};

use super::environment::{BuildEnvironment, DependencyContext, package_identity};

/// Cached AST from Pass 1 — holds the source text, parsed `Module`, and any
/// parse diagnostics so Pass 2 can skip re-reading and re-parsing the same file.
#[derive(Debug)]
pub(crate) struct CachedAst {
    pub(crate) source: String,
    pub(crate) module: beamtalk_core::ast::Module,
    pub(crate) diagnostics: Vec<beamtalk_core::source_analysis::Diagnostic>,
}

/// Results from Pass 1: class index building and dependency merging.
pub(crate) struct ClassIndexResult {
    /// Merged class-to-module index (source + dependency classes).
    pub(crate) class_module_index: HashMap<String, String>,
    /// Class-to-superclass index.
    pub(crate) class_superclass_index: HashMap<String, String>,
    /// Unified collection of all `ClassInfo` from source and dependency classes.
    pub(crate) all_class_infos: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    /// Type alias declarations (`type Name = ...`) collected from
    /// every project source file, for cross-file/package alias resolution
    /// during Pass 2. See `collect_project_alias_infos`'s doc for why this
    /// is a plain full scan rather than threaded through the incremental
    /// Pass 1 cache the way `all_class_infos` is.
    ///
    /// Merged with dependency-exported alias infos
    /// (`ResolvedDependency.alias_infos`), so cross-package aliases (e.g.
    /// stdlib's `JsonValue`) resolve the same way cross-package classes do.
    pub(crate) all_alias_infos: Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo>,
    /// Unified collection of all `ProtocolInfo` from same-package
    /// cross-file protocol definitions and dependency-exported protocols,
    /// mirroring `all_alias_infos`/`all_class_infos`. Protocols have no
    /// `internal` modifier at the AST level, so every declared protocol
    /// project-wide (and every dependency's) is included unconditionally.
    pub(crate) all_protocol_infos:
        Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo>,
    /// Project-wide standalone extension index from Pass 1.
    pub(crate) extension_index: beamtalk_core::compilation::extension_index::ExtensionIndex,
    /// Dependency registry for cross-package collision detection.
    pub(crate) dep_registry: beamtalk_core::semantic_analysis::DependencyRegistry,
    /// Cached ASTs from incremental Pass 1, keyed by source file path.
    pub(crate) cached_asts: HashMap<Utf8PathBuf, CachedAst>,
    /// Whether the manifest changed, forcing Pass 2 recompilation.
    pub(crate) force_pass2: bool,
    /// Content hash of every Pass-1-scanned source file, keyed by path
    /// string — empty for manifest-less builds, where Pass 1 never
    /// runs. Reused by `detect_changes` (Pass 2) so it doesn't re-hash a file
    /// whose content Pass 1 already hashed this same build.
    pub(crate) source_hashes: HashMap<String, String>,
}

/// Phase 5-6: Build the class index (Pass 1) and merge dependency indexes.
///
/// Computes module names for all source files, builds the class-to-module and
/// class-to-superclass indexes, merges dependency class indexes, and validates
/// stdlib reservation violations.
#[allow(clippy::too_many_lines)] // linear merge pipeline (source + N deps) — split adds indirection, not clarity
pub(crate) fn build_class_index(
    env: &BuildEnvironment,
    dep_ctx: &DependencyContext,
    options: &beamtalk_core::CompilerOptions,
    force: bool,
) -> Result<ClassIndexResult> {
    let pkg_manifest = env.pkg_manifest();

    // Pass 1: compute module names and build the class → module index.
    // This allows later files to resolve cross-file class references (including
    // classes in package subdirectories) during code generation.
    // Use incremental cache to skip unchanged files in Pass 1.
    let manifest_path = if pkg_manifest.is_some() {
        let p = env.project_root.join("beamtalk.toml");
        if p.exists() { Some(p) } else { None }
    } else {
        None
    };
    let (
        mut class_module_index,
        class_superclass_index,
        source_class_infos,
        extension_index,
        cached_asts,
        force_pass2,
        source_hashes,
    ) = if let Some(pkg) = pkg_manifest {
        let result = crate::commands::build_cache::incremental_build_class_module_index(
            &env.source_files,
            env.source_root.as_deref(),
            &pkg.name,
            &env.build_dir,
            manifest_path.as_deref(),
            force,
        )?;
        // If the manifest changed, force Pass 2 recompilation too —
        // .bt files may be unchanged but semantics depend on the manifest.
        (
            result.class_module_index,
            result.class_superclass_index,
            result.all_class_infos,
            result.extension_index,
            result.cached_asts,
            result.manifest_invalidated,
            result.source_hashes,
        )
    } else {
        (
            HashMap::new(),
            HashMap::new(),
            Vec::new(),
            beamtalk_core::compilation::extension_index::ExtensionIndex::new(),
            HashMap::new(),
            false,
            HashMap::new(),
        )
    };

    // ADR 0070: Merge dependency class indexes into the main package's indexes
    // so cross-package class references resolve during compilation.
    // Also build a DependencyRegistry for collision detection (Phase 3).
    // Track direct vs transitive dependencies for W0302 warnings.
    let dep_infos: Vec<beamtalk_core::semantic_analysis::DepInfo> = dep_ctx
        .resolved_deps
        .iter()
        .map(|dep| beamtalk_core::semantic_analysis::DepInfo {
            name: dep.name.clone(),
            class_module_index: dep.class_module_index.clone(),
            is_direct: dep.is_direct,
            via_chain: dep.via_chain.clone(),
        })
        .collect();
    let dep_registry =
        beamtalk_core::semantic_analysis::build_dependency_registry_with_graph(&dep_infos);

    // Collect dependency ClassInfo/ProtocolInfo/AliasInfo separately, then
    // merge with source-side infos via the collect_all_* helpers.
    let mut dep_class_infos = Vec::new();
    let mut dep_protocol_infos = Vec::new();
    let mut dep_alias_infos = Vec::new();
    for dep in &dep_ctx.resolved_deps {
        for (class_name, module_name) in &dep.class_module_index {
            debug!(
                dep = %dep.name,
                class = %class_name,
                module = %module_name,
                "Adding dependency class to index"
            );
            class_module_index.insert(class_name.clone(), module_name.clone());
        }
        dep_class_infos.extend(dep.class_infos.clone());
        dep_protocol_infos.extend(dep.protocol_infos.clone());
        dep_alias_infos.extend(dep.alias_infos.clone());
    }

    // Single unified collection of all ClassInfo from all sources.
    // To add a new .bt source location, add its ClassInfo slice here.
    let all_class_infos = collect_all_class_infos(&[&source_class_infos, &dep_class_infos]);

    // ADR 0070 Phase 3: Eagerly check stdlib reservation violations.
    // Dependencies must not export classes with stdlib-reserved names.
    if !dep_registry.is_empty() && !options.stdlib_mode {
        check_stdlib_reservations(&dep_registry)?;
    }

    // Same-package cross-file protocol and type-alias
    // resolution, merged with dependency-exported protocols/aliases. Only
    // runs for package builds (matching `pre_loaded_classes`'s existing
    // scope) — a manifest-less directory/single-file build has no package
    // boundary and keeps today's same-file-only resolution.
    //
    // `--stdlib-mode` is the exception (see `package_identity`).
    // `beamtalk build --stdlib-mode <dir>` — what `just dialyzer-specs` runs,
    // over a flat copy of `stdlib/src/*.bt` in a bare temp dir — has no
    // manifest but *is* one coherent package. Without it, `field: restart ::
    // RestartStrategy = #temporary` in `supervision_spec.bt` couldn't see
    // `type RestartStrategy = ...` declared over in `actor.bt`, and
    // `check_state_defaults` reported a false "declared as RestartStrategy,
    // default is #temporary" mismatch against the unexpanded alias name.
    // `just build-stdlib` never hits this: `build_stdlib.rs` seeds its own
    // `pre_loaded_aliases` from a live same-run pre-pass. There are
    // no dependencies to merge in stdlib mode, so the dep-side vectors are
    // empty and the merge helpers are pass-throughs.
    //
    // Protocols and aliases are extracted together in one uncached scan
    // (`collect_project_protocol_and_alias_infos`) rather than two separate
    // ones — a build review finding: scanning the project source set twice
    // here, on top of `build_class_module_index`'s own (incrementally
    // cached) scan for classes, was doing up to three full parses of every
    // file per build.
    let (all_protocol_infos, all_alias_infos) =
        match package_identity(pkg_manifest, options.stdlib_mode) {
            Some(name) => {
                let (source_protocol_infos, source_alias_infos) =
                    collect_project_protocol_and_alias_infos(&env.source_files, name);
                (
                    collect_all_protocol_infos(&[&source_protocol_infos, &dep_protocol_infos]),
                    collect_all_alias_infos(&[&source_alias_infos, &dep_alias_infos]),
                )
            }
            None => (Vec::new(), Vec::new()),
        };

    Ok(ClassIndexResult {
        class_module_index,
        class_superclass_index,
        all_class_infos,
        all_alias_infos,
        all_protocol_infos,
        extension_index,
        dep_registry,
        cached_asts,
        force_pass2,
        source_hashes,
    })
}

/// Check that no dependency exports classes with stdlib-reserved names.
fn check_stdlib_reservations(
    dep_registry: &beamtalk_core::semantic_analysis::DependencyRegistry,
) -> Result<()> {
    let mut reservation_diags = Vec::new();
    beamtalk_core::semantic_analysis::check_stdlib_reservation(
        dep_registry,
        &mut reservation_diags,
    );
    if reservation_diags.is_empty() {
        return Ok(());
    }
    let has_errors = reservation_diags
        .iter()
        .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error);
    if has_errors {
        let messages: Vec<String> = reservation_diags
            .iter()
            .filter(|d| d.severity == beamtalk_core::source_analysis::Severity::Error)
            .map(|d| {
                if let Some(ref hint) = d.hint {
                    format!("{}\n  help: {}", d.message, hint)
                } else {
                    d.message.to_string()
                }
            })
            .collect();
        miette::bail!("{}", messages.join("\n"));
    }
    Ok(())
}

/// Build class indexes from a set of source files.
///
/// Returns four items:
/// 1. **Class module index:** Maps class names to compiled module names
///    (e.g. `"SchemeEnv"` → `"bt@sicp_example@scheme@env"`).
/// 2. **Class superclass index:** Maps class names to their direct superclass names
///    (e.g. `"MyChild"` → `"MyParent"`). Used to resolve cross-file
///    inheritance so the compiler can determine value-object vs actor codegen.
/// 3. **Class infos:** Full `ClassInfo` entries extracted from all source files.
///    Injected into the type checker's hierarchy during Pass 2 so
///    cross-file method resolution works without reading BEAM files.
/// 4. **Cached ASTs:** Maps file paths to their parsed `Module` + source
///    text so Pass 2 can reuse them instead of re-reading and re-parsing.
#[allow(clippy::type_complexity)]
pub(crate) fn build_class_module_index(
    source_files: &[Utf8PathBuf],
    source_root: Option<&Utf8Path>,
    pkg_name: &str,
) -> Result<(
    HashMap<String, String>,
    HashMap<String, String>,
    Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    beamtalk_core::compilation::extension_index::ExtensionIndex,
    HashMap<Utf8PathBuf, CachedAst>,
)> {
    let mut module_index = HashMap::new();
    let mut superclass_index = HashMap::new();
    let mut all_class_infos = Vec::new();
    let mut extension_index = beamtalk_core::compilation::extension_index::ExtensionIndex::new();
    let mut cached_asts: HashMap<Utf8PathBuf, CachedAst> = HashMap::new();

    for file in source_files {
        let relative_module = super::sources::compute_relative_module(file, source_root)?;
        let module_name = format!("bt@{pkg_name}@{relative_module}");

        let source = match fs::read_to_string(file) {
            Ok(s) => s,
            Err(e) => {
                warn!(
                    file = %file,
                    error = %e,
                    "Cannot read source file during index pass; class resolution from this file will be skipped"
                );
                continue;
            }
        };
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
        let (module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);
        if !diagnostics.is_empty() {
            warn!(
                file = %file,
                diagnostic_count = diagnostics.len(),
                "Source file has parse errors during index pass; class resolution from this file may be incomplete"
            );
        }

        // Extract full ClassInfo for cross-file hierarchy resolution.
        //
        // A file with parse *errors* may have an under-recovered
        // method surface (error recovery can drop method definitions), so its
        // classes are marked `surface_incomplete`. The receiver-knowledge
        // classifier downgrades receivers whose superclass chain contains a
        // marked class to `Open`, preventing false unresolved-selector hints
        // against a surface Pass 1 never fully saw.
        let has_parse_errors = diagnostics
            .iter()
            .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error);
        let mut class_infos =
            beamtalk_core::semantic_analysis::ClassHierarchy::extract_class_infos(&module);
        if has_parse_errors {
            for info in &mut class_infos {
                info.surface_incomplete = true;
            }
        }
        // Stamp the package now, while each file's classes are still
        // isolated — `analyse_full`'s `stamp_package` only reaches classes
        // built from the *current* module's own AST, never the cross-file
        // `ClassInfo` this Pass 1 index injects into every other file's
        // compilation. Without this, E0401/E0402 treat a same-package class
        // from a sibling file as package-less and never flag it as a leak.
        beamtalk_core::semantic_analysis::ClassHierarchy::stamp_package_on_infos(
            &mut class_infos,
            pkg_name,
        );
        all_class_infos.extend(class_infos);

        // Collect standalone extension definitions
        // (`ClassName >> selector => ...`) project-wide so Pass 2 can
        // register them into every file's class hierarchy — a same-project
        // cross-file extension then resolves instead of producing a false
        // `Dnu` hint (ADR 0066 / ADR 0100 Rule 2 WS1).
        extension_index.add_module(&module, file.as_std_path());

        for class in &module.classes {
            let class_name = class.name.name.to_string();
            if let Some(existing) = module_index.get(&class_name) {
                if existing != &module_name {
                    eprintln!(
                        "Warning: class '{class_name}' is defined in both '{existing}' and \
                         '{module_name}'; using '{module_name}' for cross-file dispatch"
                    );
                }
            }
            module_index.insert(class_name.clone(), module_name.clone());
            // Record direct superclass for cross-file hierarchy resolution.
            // When a duplicate class overwrites a prior entry, keep the superclass
            // index consistent by removing any stale mapping if the new definition
            // has no explicit superclass.
            if let Some(ref superclass) = class.superclass {
                superclass_index.insert(class_name.clone(), superclass.name.to_string());
            } else {
                superclass_index.remove(&class_name);
            }
        }

        // Cache the parsed AST so Pass 2 doesn't re-read/re-parse.
        cached_asts.insert(
            file.clone(),
            CachedAst {
                source,
                module,
                diagnostics,
            },
        );
    }

    Ok((
        module_index,
        superclass_index,
        all_class_infos,
        extension_index,
        cached_asts,
    ))
}

/// Extracts type-alias declarations (`type Name = ...`) from every project
/// source file, for cross-file/package alias resolution during Pass 2.
///
/// Unlike `ClassInfo`, alias metadata is *not* threaded through the
/// incremental Pass 1 cache (`build_cache.rs`'s `.beamtalk-pass1-cache.json`):
/// `AliasInfo` embeds an unresolved `TypeAnnotation`, which (unlike
/// `ClassInfo`'s stringly-typed method signatures) has no `serde` support —
/// adding it would mean threading `Serialize`/`Deserialize` through the AST's
/// `TypeAnnotation` tree purely to satisfy the cache format, a disproportionate
/// blast radius (tracked as a follow-up for anyone who
/// wants the incremental-cache perf win later). Lexing and parsing every
/// project source file just to pull out `type` declarations is cheap relative
/// to the rest of a build, so this always does a full, uncached scan
/// regardless of per-file change detection — every call re-derives the
/// project's alias table from scratch rather than risking staleness.
///
/// Parse errors on an individual file are non-fatal here: that file simply
/// contributes no aliases to the merged set, and the same parse error is
/// already reported through the normal Pass 1 diagnostics path.
pub(crate) fn collect_project_alias_infos(
    source_files: &[Utf8PathBuf],
    pkg_name: &str,
) -> Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo> {
    collect_project_protocol_and_alias_infos(source_files, pkg_name).1
}

/// Merge alias infos from multiple sources (same-package cross-file +
/// dependency-exported) into one collection, mirroring `collect_all_class_infos`.
/// A plain concatenation — collision diagnostics are handled by
/// `AliasRegistry::add_pre_loaded`, not here.
pub(crate) fn collect_all_alias_infos(
    sources: &[&[beamtalk_core::semantic_analysis::alias_registry::AliasInfo]],
) -> Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo> {
    let total_len: usize = sources.iter().map(|s| s.len()).sum();
    let mut result = Vec::with_capacity(total_len);
    for source in sources {
        result.extend_from_slice(source);
    }
    result
}

/// Extracts protocol declarations (`Protocol define: Name ...`) from every
/// project source file, for same-package cross-file protocol resolution
/// during Pass 2.
///
/// Mirrors `collect_project_alias_infos`: a plain, uncached full scan rather
/// than threaded through the incremental Pass 1 cache. Unlike `AliasInfo`,
/// `ProtocolInfo` carries no `package` field to stamp — protocols have no
/// `internal` modifier at the AST level, so every declared protocol is
/// exported project-wide.
///
/// Parse errors on an individual file are non-fatal here: that file simply
/// contributes no protocols to the merged set, and the same parse error is
/// already reported through the normal Pass 1 diagnostics path.
///
/// `build_class_index`'s production path calls
/// `collect_project_protocol_and_alias_infos` directly to extract both kinds
/// in one scan; this standalone single-purpose wrapper is kept for tests
/// that only care about protocols (mirrors `collect_project_alias_infos`,
/// which stays in production use via `compile_dependency_with_context`).
#[allow(dead_code)] // Used by tests; production path uses the combined extractor above
pub(crate) fn collect_project_protocol_infos(
    source_files: &[Utf8PathBuf],
) -> Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo> {
    collect_project_protocol_and_alias_infos(source_files, "").0
}

/// Extracts both protocol and type-alias declarations from every project
/// source file in a single lex/parse pass per file.
///
/// Extracting protocols and aliases each with their own independent
/// re-parse of the full project source set would mean a manifest-based
/// build does up to three full scans of every source file: once
/// (incrementally cached) in `build_class_module_index` for classes, then
/// two more *uncached* scans here for protocols and aliases. This combines
/// those last two into one uncached scan, mirroring how
/// `build_dep_class_index` already extracts both kinds from a single pass
/// over its cached ASTs. The class-index scan stays separate — it's
/// incrementally cached and produces different data
/// (`class_module_index`/`class_superclass_index`), not a natural fit for
/// this merge.
///
/// `pkg_name` is only used to stamp `AliasInfo.package`; pass `""` when only
/// the protocol half of the result is needed (see
/// `collect_project_protocol_infos`, which never stamps a package anyway).
fn collect_project_protocol_and_alias_infos(
    source_files: &[Utf8PathBuf],
    pkg_name: &str,
) -> (
    Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo>,
    Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo>,
) {
    let mut all_protocols = Vec::new();
    let mut all_aliases = Vec::new();
    for file in source_files {
        let Ok(source) = fs::read_to_string(file) else {
            continue;
        };
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
        let (module, _diagnostics) = beamtalk_core::source_analysis::parse(tokens);

        all_protocols.extend(
            beamtalk_core::semantic_analysis::protocol_registry::ProtocolRegistry::extract_protocol_infos(
                &module,
            ),
        );

        let mut infos =
            beamtalk_core::semantic_analysis::alias_registry::AliasRegistry::extract_alias_infos(
                &module,
            );
        for info in &mut infos {
            info.package = Some(pkg_name.into());
        }
        all_aliases.extend(infos);
    }
    (all_protocols, all_aliases)
}

/// Merge protocol infos from multiple sources (same-package cross-file +
/// dependency-exported) into one collection, mirroring `collect_all_class_infos`
/// / `collect_all_alias_infos`.
pub(crate) fn collect_all_protocol_infos(
    sources: &[&[beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo]],
) -> Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo> {
    let total_len: usize = sources.iter().map(|s| s.len()).sum();
    let mut result = Vec::with_capacity(total_len);
    for source in sources {
        result.extend_from_slice(source);
    }
    result
}

/// Collect `ClassInfo` from multiple sources into a single unified vector.
///
/// This is the single entry point for gathering all `ClassInfo`
/// metadata across the project. Callers provide `ClassInfo` slices from each
/// source (package sources, dependencies, fixtures, etc.) and get back a
/// merged vector suitable for the type checker and structural validator.
///
/// Adding a new `.bt` source location only requires adding its `ClassInfo`
/// slice to the input list — no separate wiring needed.
pub(crate) fn collect_all_class_infos(
    sources: &[&[beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo]],
) -> Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo> {
    let total_len: usize = sources.iter().map(|s| s.len()).sum();
    let mut result = Vec::with_capacity(total_len);
    for source in sources {
        result.extend_from_slice(source);
    }
    result
}
