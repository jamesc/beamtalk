// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build beamtalk projects.
//!
//! This module is the `build` command's entry point ([`build`]) and pass
//! orchestration ([`execute_build_passes`]). The individual build stages live
//! in sibling modules, split by concern (BT-3455):
//!
//! - [`changes`] — per-file change detection and stale-artifact cleanup
//! - [`environment`] — build-environment setup and dependency resolution
//! - [`class_index`] — Pass 1 class/protocol/alias index construction
//! - [`stubs`] — ADR 0075 native FFI type-spec and stub-registry resolution
//! - [`native`] — ADR 0072 native Erlang compilation (rebar3 + erlc)
//! - [`outputs`] — OTP application packaging (`.app` file, corpus files)
//! - [`sources`] — source-file discovery and per-file Core Erlang/BEAM compilation
//!
//! Items consumed outside this module (by `build_stdlib.rs`, `test.rs`,
//! `lint.rs`, `deps/path.rs`, `fmt.rs`, `type_coverage.rs`, and
//! `beam_compiler.rs`) are re-exported below so `crate::commands::build::X`
//! paths keep working unchanged.

use std::collections::{HashMap, HashSet};
use std::fs;

use camino::{Utf8Path, Utf8PathBuf};
use miette::Result;
use tracing::{debug, info, instrument};

use crate::beam_compiler::{ClassHierarchyContext, CompileContext};

use super::OutputFormat;

mod changes;
mod class_index;
mod environment;
mod native;
mod outputs;
mod sources;
mod stubs;

// Re-exports so `crate::commands::build::X` paths used by `build_stdlib.rs`,
// `test.rs`, `lint.rs`, `fmt.rs`, `type_coverage.rs`, `beam_compiler.rs`, and
// `deps/path.rs` keep working unchanged (BT-3455).
pub(crate) use changes::{clean_stale_artifacts, detect_changes};
pub(crate) use class_index::{
    CachedAst, build_class_index, build_class_module_index, collect_all_class_infos,
    collect_project_alias_infos,
};
pub(crate) use environment::{
    BuildEnvironment, DependencyContext, package_identity, resolve_and_validate_dependencies,
    setup_build_environment,
};
pub(crate) use native::{Rebar3Result, collect_rebar3_ebin_paths, compile_native_sources};
pub(crate) use outputs::{
    PackageBuildOutputs, build_alias_metadata, build_class_metadata, generate_package_corpus,
    generate_package_outputs,
};
pub use sources::{collect_formattable_files_from_dir, collect_source_files_from_dir};
pub(crate) use sources::{
    collect_project_source_files, compile_file, compile_to_beam, compute_file_module_pairs,
    compute_relative_module,
};
pub(crate) use stubs::{
    distribution_stubs_dir, extract_type_specs, load_dependency_stub_registries,
    load_project_stub_registry,
};

// Re-exported only for `build::tests` (which relies on `super::*` to see
// every build-stage helper as if this were still one flat file, per
// BT-3451's split test tree). None of these has a production caller outside
// its own defining submodule, so re-exporting them unconditionally would be
// an unused-import warning under `-D warnings` in a non-test build.
#[cfg(test)]
pub(crate) use class_index::{
    collect_all_alias_infos, collect_all_protocol_infos, collect_project_protocol_infos,
};
#[cfg(test)]
pub(crate) use native::{
    aggregate_native_dependencies, check_native_module_collisions, collect_erl_files,
    compile_native_erlang_with_deps, generate_class_header, generate_rebar_config, rebar3_path,
    validate_native_class_references,
};
#[cfg(test)]
pub(crate) use outputs::to_forward_slash;
#[cfg(test)]
pub(crate) use sources::find_source_files;

/// Results from the two compilation passes (class index + source compilation).
/// Produced by [`execute_build_passes`].
struct BuildPassesResult {
    /// All module names (both compiled and unchanged), needed for .app generation.
    module_names: Vec<String>,
    /// File-to-module mapping for all source files.
    file_module_pairs: Vec<(Utf8PathBuf, String, Utf8PathBuf)>,
    /// Class hierarchy context used for post-processing (app file generation, etc.).
    hierarchy: ClassHierarchyContext,
    /// Native compilation result, if native Erlang sources were compiled.
    native_result: Option<Rebar3Result>,
    /// BT-2014: Diagnostic summary aggregated from all compiled files.
    diagnostic_summary: beamtalk_core::source_analysis::DiagnosticSummary,
}

/// Build beamtalk source files.
///
/// This command compiles .bt files to .beam bytecode via Core Erlang.
///
/// When `force` is false, per-file change detection compares each `.bt`
/// source's content hash against the hash recorded for its `.beam` output
/// (BT-3120) and only recompiles changed files.
#[instrument(skip_all, fields(path = %path))]
pub fn build(path: &str, options: &beamtalk_core::CompilerOptions, force: bool) -> Result<()> {
    info!("Starting build");

    let env = setup_build_environment(path)?;

    // BT-2796: A package directory build walks every project source file
    // (Pass 1) before any per-file analysis runs (Pass 2), so the injected
    // cross-file knowledge is project-complete. Declare that to the
    // receiver-knowledge classifier (ADR 0100 Rule 2 sequencing guard).
    // A single-file build (`beamtalk build foo.bt`) sees only that file, and
    // a manifest-less directory build skips Pass 1 entirely — both keep the
    // conservative `ModuleOnly` default.
    let mut options = options.clone();
    if Utf8Path::new(path).is_dir() && env.full_manifest.is_some() {
        options.knowledge_scope = beamtalk_core::semantic_analysis::KnowledgeScope::ProjectComplete;
    }
    // BT-2794 (pre-WS3 guard): dependency extension contributions are not
    // loaded until WS3 (ADR 0070 amendment), so declaring dependencies means
    // no receiver's method surface is provably complete.
    options.has_package_dependencies = env
        .full_manifest
        .as_ref()
        .is_some_and(|m| !m.dependencies.is_empty());
    // BT-2920: Set the current package so E0401/E0402 visibility checks
    // (`check_class_visibility`/`check_alias_leaked_visibility`) actually run
    // — they're gated on `current_package: Some(_)` and silently emit zero
    // diagnostics otherwise. `None` for single-file/manifest-less builds,
    // which have no package boundary to enforce.
    options.current_package =
        package_identity(env.pkg_manifest(), options.stdlib_mode).map(str::to_owned);
    let options = &options;

    let dep_ctx = resolve_and_validate_dependencies(&env, options)?;
    let passes = execute_build_passes(&env, options, &dep_ctx, force)?;

    // BT-2014: Print diagnostic summary at end of successful build.
    // Suppressed when --no-warnings is set (suppress_warnings), matching
    // the behaviour of individual warning suppression during compilation.
    if !options.suppress_warnings && !passes.diagnostic_summary.is_empty() {
        eprintln!();
        eprintln!("{}", passes.diagnostic_summary);
    }

    post_process_package_artifacts(&env, &dep_ctx, &passes)?;

    Ok(())
}

/// Phase 5-8: Build the class index (Pass 1), merge dependency indexes,
/// compile changed source files (Pass 2), and compile Core Erlang to BEAM.
#[allow(clippy::too_many_lines)] // orchestration function — one call per build phase
fn execute_build_passes(
    env: &BuildEnvironment,
    options: &beamtalk_core::CompilerOptions,
    dep_ctx: &DependencyContext,
    force: bool,
) -> Result<BuildPassesResult> {
    // ADR 0098 Phase 1: gate the project build on provenance before Pass 1, so a
    // toolchain-version mismatch forces a full rebuild regardless of mtime.
    let force = force || provenance_requires_rebuild(env);

    let index = build_class_index(env, dep_ctx, options, force)?;
    let force = if index.force_pass2 { true } else { force };

    let native_result = compile_native_sources(env, dep_ctx, &index.class_module_index)?;

    // ADR 0075: Extract type specs from OTP and dependency .beam files after
    // native compilation so freshly compiled native modules are included.
    // Results are cached in _build/type_cache/ — incremental builds read zero
    // .beam files when the cache is fresh.
    let auto_extract_registry = extract_type_specs(
        &env.layout,
        env.pkg_manifest().is_some(),
        options.stdlib_mode,
    );
    // ADR 0075 full resolution chain (BT-1847, BT-3394): each stub tier
    // overrides auto-extract at the function/arity level, applied
    // lowest-precedence first so project-local always wins —
    // extracted -> distribution -> package-bundled -> project-local. Every
    // tier is checked for drift against the pre-merge auto-extract registry
    // so the drift check has real ground truth.
    let dependency_stubs = load_dependency_stub_registries(
        &dep_ctx.resolved_deps,
        distribution_stubs_dir().as_deref(),
        auto_extract_registry.as_ref(),
        OutputFormat::Text,
    );
    let project_stubs = load_project_stub_registry(
        &env.project_root,
        auto_extract_registry.as_ref(),
        OutputFormat::Text,
    );

    let (native_type_registry, stub_diagnostics) =
        if dependency_stubs.is_none() && project_stubs.is_none() {
            (auto_extract_registry, Vec::new())
        } else {
            let mut merged = auto_extract_registry.unwrap_or_default();
            let mut diagnostics = Vec::new();
            if let Some((dep_registry, dep_diags)) = dependency_stubs {
                merged.apply_overrides(dep_registry);
                diagnostics.extend(dep_diags);
            }
            if let Some((stub_registry, diags)) = project_stubs {
                merged.apply_overrides(stub_registry);
                diagnostics.extend(diags);
            }
            (Some(merged), diagnostics)
        };
    let native_type_registry = native_type_registry.map(std::sync::Arc::new);

    let file_module_pairs = compute_file_module_pairs(env)?;

    // BT-1682: Per-file change detection — only recompile files whose source
    // is newer than the corresponding .beam output. Pass 1's already-computed
    // content hashes (`index.source_hashes`, BT-3120) let this skip re-hashing
    // any file Pass 1 already hashed this build.
    let changes = detect_changes(
        &env.source_files,
        &env.build_dir,
        &file_module_pairs,
        force,
        &index.source_hashes,
    );

    // Warn about orphaned .beam files (source deleted but .beam remains)
    for orphan in &changes.orphaned_beam_files {
        eprintln!(
            "warning: orphaned .beam file {orphan} has no corresponding .bt source (source may have been deleted)"
        );
    }

    // Collect ALL module names for .app generation (both changed and unchanged),
    // but only compile changed files.
    let changed_set: HashSet<&Utf8Path> = changes
        .changed_files
        .iter()
        .map(Utf8PathBuf::as_path)
        .collect();

    // Pass 2: compile each file with the full class → module index.
    // BT-1544: Reuse cached ASTs from Pass 1 to avoid re-reading and re-parsing.
    // BT-1682: Only compile files that have changed since last build.
    let mut cached_asts = index.cached_asts;
    let mut core_files = Vec::new();
    let mut module_names = Vec::new();
    let strict_deps = env
        .full_manifest
        .as_ref()
        .is_some_and(|m| m.package.strict_deps);
    // ADR 0100 Rule 3 (BT-2793): the package's `[diagnostics]` severity-override
    // table, empty when there's no manifest or no `[diagnostics]` section.
    let diagnostics_overrides = env
        .full_manifest
        .as_ref()
        .map(|m| m.diagnostics.clone())
        .unwrap_or_default();

    let registry_ref = if index.dep_registry.is_empty() {
        None
    } else {
        Some(&index.dep_registry)
    };
    let compile_ctx = CompileContext {
        hierarchy: ClassHierarchyContext {
            class_module_index: index.class_module_index.clone(),
            class_superclass_index: index.class_superclass_index.clone(),
            pre_loaded_classes: index.all_class_infos.clone(),
            pre_loaded_protocols: index.all_protocol_infos.clone(),
            pre_loaded_aliases: index.all_alias_infos.clone(),
            extension_index: index.extension_index.clone(),
        },
        dep_registry: registry_ref,
        strict_deps,
        native_type_registry,
        diagnostics_overrides,
    };
    // BT-2014: Collect diagnostics from all compiled files for the summary.
    // BT-1847: seed with stubs/ diagnostics (skipped signatures, version drift).
    let mut all_build_diags: Vec<beamtalk_core::source_analysis::Diagnostic> = stub_diagnostics;

    // BT-3410: an unchanged file's diagnostics from its last actual compile,
    // so they can be replayed (shown and counted) instead of silently
    // vanishing the moment the file stops being recompiled. Rebuilt from
    // scratch below (rather than mutated in place) so a file removed from
    // the project, or one whose diagnostics changed, never leaves a stale
    // entry in the sidecar this build writes back out.
    let previous_diagnostics_cache =
        crate::commands::build_cache::load_diagnostics_cache(&env.build_dir);
    let mut new_diagnostics_cache: HashMap<
        String,
        Vec<beamtalk_core::source_analysis::Diagnostic>,
    > = HashMap::with_capacity(file_module_pairs.len());
    // BT-3410: every changed file (always known) plus every unchanged file
    // whose diagnostics were actually found in the cache and replayed, or
    // recompiled below because they weren't.
    let mut files_with_known_diagnostics = 0usize;
    // BT-3410: unchanged files recompiled this build solely because the
    // diagnostics sidecar had no entry for them (a fresh sidecar right after
    // upgrading to this feature, corruption, or a cache-version bump) — their
    // .beam is already current, only their diagnostics were unknown. Counted
    // separately so the "Compiling N of M files" summary below still
    // reflects what `core_files` actually holds.
    let mut diagnostics_cache_miss_recompiles = 0usize;

    for (file, module_name, core_file) in &file_module_pairs {
        module_names.push(module_name.clone());

        if changed_set.contains(file.as_path()) {
            let cached = cached_asts.remove(file);
            let file_diags =
                compile_file(file, module_name, core_file, options, &compile_ctx, cached)?;
            files_with_known_diagnostics += 1;
            new_diagnostics_cache.insert(file.as_str().to_string(), file_diags.clone());
            all_build_diags.extend(file_diags);
            core_files.push(core_file.clone());
            continue;
        }

        if let Some(cached_diags) = previous_diagnostics_cache.get(file.as_str()) {
            debug!(file = %file, "Skipping unchanged file (diagnostics replayed from cache)");
            if !cached_diags.is_empty() {
                // A cached diagnostic's span is only valid against the exact
                // content that produced it — `changed_set` already
                // guarantees this file's content hash hasn't moved since
                // then, so re-reading it now is safe to render against.
                if let Ok(source) = fs::read_to_string(file) {
                    crate::diagnostic::print_diagnostics_text(
                        cached_diags,
                        file.as_str(),
                        &source,
                        options,
                    );
                }
            }
            files_with_known_diagnostics += 1;
            all_build_diags.extend(cached_diags.iter().cloned());
            new_diagnostics_cache.insert(file.as_str().to_string(), cached_diags.clone());
            continue;
        }

        // BT-3410: unchanged, but no diagnostics cache entry — recompile now
        // so the gap closes within this one build instead of leaving the
        // file silently unreported until its content next changes or
        // `--force` is used. `compile_file` transparently falls back to
        // reading and parsing from disk when Pass 1 didn't cache this file's
        // AST either (its own, separate incremental cache), so this is safe
        // even though the file was never in `cached_asts`.
        debug!(file = %file, "Unchanged file has no diagnostics cache entry — recompiling");
        let cached = cached_asts.remove(file);
        let file_diags = compile_file(file, module_name, core_file, options, &compile_ctx, cached)?;
        files_with_known_diagnostics += 1;
        diagnostics_cache_miss_recompiles += 1;
        new_diagnostics_cache.insert(file.as_str().to_string(), file_diags.clone());
        all_build_diags.extend(file_diags);
        core_files.push(core_file.clone());
    }

    compile_to_beam(
        &env.build_dir,
        &core_files,
        changes.changed_files.len() + diagnostics_cache_miss_recompiles,
        changes.unchanged_files.len() - diagnostics_cache_miss_recompiles,
        file_module_pairs.len(),
    )?;

    // BT-3410: persist diagnostics before the beam-hash sidecar. If the
    // process dies between these two writes, the hash cache must be the one
    // left stale — an unchanged-looking file whose diagnostics entry is
    // missing/superseded falls through to "unknown" and gets skipped safely,
    // whereas writing the hash cache first would let a crash leave it
    // already matching the new content while the diagnostics cache still
    // held the superseded file's diagnostics, which would then get replayed
    // as if it were current.
    crate::commands::build_cache::save_diagnostics_cache(&env.build_dir, &new_diagnostics_cache);
    // BT-3120: only persist the beam-hash sidecar once compilation has
    // actually succeeded (the `?` above already returned on failure) — the
    // saved hashes assert "this content produced the `.beam` now on disk".
    crate::commands::build_cache::save_beam_hash_cache(&env.build_dir, &changes.source_hashes);

    let diagnostic_summary = beamtalk_core::source_analysis::DiagnosticSummary::from_diagnostics(
        &all_build_diags,
        files_with_known_diagnostics,
    );

    Ok(BuildPassesResult {
        module_names,
        file_module_pairs,
        hierarchy: compile_ctx.hierarchy,
        native_result,
        diagnostic_summary,
    })
}

/// ADR 0098 Phase 1: project provenance gate.
///
/// Returns `true` when the project's build scope was produced by a different
/// toolchain (a `beamtalk_version` or OTP-version mismatch) or the stamp is
/// missing/corrupt/unknown-schema — meaning every module must be recompiled
/// regardless of mtime. On a miss the version-sensitive Pass 1 metadata cache is
/// discarded too (its `ClassInfo` is compiler-derived). No-op (returns `false`)
/// for single-file builds, which have no `_build/dev/` scope or stamp.
fn provenance_requires_rebuild(env: &BuildEnvironment) -> bool {
    if env.pkg_manifest().is_none() {
        return false;
    }
    let current_otp = crate::commands::build_stamp::current_otp_version();
    match crate::commands::build_stamp::read_stamp_status(&env.layout.stamp_path(), current_otp) {
        crate::commands::build_stamp::StampStatus::Fresh => false,
        crate::commands::build_stamp::StampStatus::Stale(reason) => {
            info!("Build provenance miss ({reason}) — forcing full rebuild");
            crate::commands::build_cache::discard_pass1_cache(&env.build_dir);
            true
        }
    }
}

/// Phase 9: Clean stale artifacts and generate OTP application outputs
/// (.app file, corpus, supervisor callback) for package builds.
fn post_process_package_artifacts(
    env: &BuildEnvironment,
    dep_ctx: &DependencyContext,
    passes: &BuildPassesResult,
) -> Result<()> {
    let Some(pkg) = env.pkg_manifest() else {
        return Ok(());
    };

    // BT-1682: For stale artifact cleanup, we need ALL expected .beam files
    // (both newly compiled and unchanged), not just the ones from this build.
    let all_expected_beams: Vec<Utf8PathBuf> = passes
        .file_module_pairs
        .iter()
        .map(|(_, module_name, _)| env.build_dir.join(format!("{module_name}.beam")))
        .collect();
    clean_stale_artifacts(&env.build_dir, &all_expected_beams, &pkg.name)?;
    let native_module_names: Vec<String> = passes
        .native_result
        .as_ref()
        .map(|r| r.module_names.clone())
        .unwrap_or_default();
    // ADR 0072 §7: Direct hex dep names for the {applications, [...]} list
    let hex_dep_names: Vec<String> = env
        .full_manifest
        .as_ref()
        .map(|m| m.native_dependencies.keys().cloned().collect())
        .unwrap_or_default();
    let bt_dep_names: Vec<String> = dep_ctx
        .resolved_deps
        .iter()
        .filter(|d| d.is_direct)
        .map(|d| d.name.clone())
        .collect();
    generate_package_outputs(
        &env.build_dir,
        &env.project_root,
        pkg,
        &passes.hierarchy,
        &PackageBuildOutputs {
            module_names: &passes.module_names,
            native_module_names: &native_module_names,
            bt_dep_names: &bt_dep_names,
            hex_dep_names: &hex_dep_names,
            source_files: &env.source_files,
        },
    )?;

    // ADR 0098 Phase 1: write the provenance stamp LAST, after every artifact in
    // the scope has landed, so a crash mid-build never leaves a stamp claiming
    // freshness for incomplete output. Best-effort (a write failure just means
    // the next build sees no stamp and rebuilds).
    crate::commands::build_stamp::write_stamp(
        &env.layout.stamp_path(),
        crate::commands::build_stamp::current_otp_version(),
    );

    Ok(())
}

#[cfg(test)]
mod tests;
