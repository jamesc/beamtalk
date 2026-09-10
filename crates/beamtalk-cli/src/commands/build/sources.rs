// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Source-file discovery, module-name computation, and per-file Core Erlang /
//! BEAM compilation (Pass 2).

use crate::beam_compiler::{BeamCompiler, CompileContext, compile_source_with_bindings};
use beamtalk_core::file_walker::FileWalker;
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use tracing::{debug, info};

use super::class_index::CachedAst;
use super::environment::BuildEnvironment;

/// Find all `.bt` source files at the given path.
///
/// If `path` is a file, returns it (must have `.bt` extension).
/// If `path` is a directory, searches `src/` subdirectory first, falling back
/// to the directory itself.
pub(crate) fn find_source_files(path: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    if path.is_file() {
        return FileWalker::source_files().walk(path);
    }

    if !path.exists() {
        miette::bail!("Path '{}' does not exist", path);
    }

    collect_project_source_files(path)
}

/// Finds a project's `.bt` source files: `src/` if it exists, else
/// `project_root` itself — excluding `project_root/stubs/`, which
/// `load_project_stub_registry` (ADR 0075) scans separately and
/// never compiles as ordinary source (a `declare native:` block there is a
/// hard error everywhere else).
///
/// Without the exclusion, a project with no `src/` directory would fall back
/// to searching the whole project root, sweeping `stubs/*.bt` into the
/// normal compile/lint pipeline. Shared by `find_source_files` (this file),
/// `type_coverage.rs`, and `deps/path.rs`'s path-dependency compilation — all
/// three previously duplicated the same `src/`-or-fallback logic without
/// this exclusion.
pub(crate) fn collect_project_source_files(project_root: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    let src_dir = project_root.join("src");
    let search_dir = if src_dir.exists() {
        src_dir
    } else {
        project_root.to_path_buf()
    };

    let files = collect_source_files_from_dir(&search_dir)?;
    let stubs_dir = project_root.join("stubs");
    Ok(files
        .into_iter()
        .filter(|f| !f.starts_with(&stubs_dir))
        .collect())
}

/// Collect all `.bt` source files from a directory tree.
///
/// Returns an error if the directory does not exist or cannot be read.
pub fn collect_source_files_from_dir(dir: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    FileWalker::source_files().walk(dir)
}

/// Collect all `.bt` and `.btscript` source files from a directory tree.
///
/// Used by `beamtalk fmt` to find all formattable files when given a directory
/// path. Returns an error if the directory does not exist or cannot be read.
pub fn collect_formattable_files_from_dir(dir: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    FileWalker::format_files().walk(dir)
}

/// Compute the relative module path from a source file.
///
/// When `source_root` is provided (i.e., a `src/` directory exists), computes
/// the path relative to it. Subdirectories become `@` segments.
///
/// Examples:
/// - `src/counter.bt` → `counter`
/// - `src/util/math.bt` → `util@math`
///
/// Falls back to the file stem when no source root is available.
///
/// ADR 0119 step 0: the segment-validation and per-segment
/// `to_module_name` conversion delegate to
/// `beamtalk_core::semantic_analysis::relative_module_segments` — the one
/// implementation of "how a file path becomes module-name segments" shared
/// with the new `ClassModuleRegistry`'s Pass-1 construction (CLAUDE.md's "No
/// duplicate implementations" rule) — rather than re-implementing the loop
/// here.
pub(crate) fn compute_relative_module(
    file: &Utf8Path,
    source_root: Option<&Utf8Path>,
) -> Result<String> {
    if let Some(root) = source_root {
        if let Ok(relative) = file.strip_prefix(root) {
            let segments = beamtalk_core::semantic_analysis::relative_module_segments(relative)
                .into_diagnostic()?;
            return Ok(segments.join("@"));
        }
    }
    // Fallback: use file stem
    let stem = file.file_stem().unwrap_or("unknown");
    Ok(beamtalk_codegen::core_erlang::to_module_name(stem))
}

/// Compute file-module-core triples for all source files.
///
/// For each `.bt` source file, computes the Erlang module name (using the
/// package naming convention for package builds) and the corresponding
/// `.core` output path.
pub(crate) fn compute_file_module_pairs(
    env: &BuildEnvironment,
) -> Result<Vec<(Utf8PathBuf, String, Utf8PathBuf)>> {
    let pkg_manifest = env.pkg_manifest();
    let mut pairs = Vec::new();

    for file in &env.source_files {
        let stem = file
            .file_stem()
            .ok_or_else(|| miette::miette!("File '{}' has no name", file))?;

        // Validate module name contains only safe characters
        if !stem.chars().all(|c| c == '_' || c.is_ascii_alphanumeric()) {
            miette::bail!(
                "Invalid module name '{}': must contain only alphanumeric characters and underscores",
                stem
            );
        }

        // ADR 0026: Package mode uses bt@{package}@{relative_path} naming
        // ADR 0016: Single-file mode uses bt@{module} naming
        let module_name = if let Some(pkg) = pkg_manifest {
            let relative_module = compute_relative_module(file, env.source_root.as_deref())?;
            format!("bt@{}@{}", pkg.name, relative_module)
        } else {
            format!("bt@{}", beamtalk_codegen::core_erlang::to_module_name(stem))
        };
        let core_file = env.build_dir.join(format!("{module_name}.core"));
        pairs.push((file.clone(), module_name, core_file));
    }

    Ok(pairs)
}

/// Compile a single `.bt` source file to Core Erlang, printing progress.
///
/// When `cached_ast` is `Some`, reuses the pre-parsed source and `Module` from
/// Pass 1 instead of re-reading and re-parsing the file.
pub(crate) fn compile_file(
    path: &Utf8Path,
    module_name: &str,
    core_file: &Utf8Path,
    options: &beamtalk_core::CompilerOptions,
    ctx: &CompileContext<'_>,
    cached_ast: Option<CachedAst>,
) -> Result<Vec<beamtalk_core::source_analysis::Diagnostic>> {
    debug!("Compiling {path}");

    let diags = compile_source_with_bindings(
        path,
        module_name,
        core_file,
        options,
        &beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable::new(),
        ctx,
        cached_ast,
    )?;

    debug!("Generated Core Erlang: {core_file}");

    Ok(diags)
}

/// Compile Core Erlang files to BEAM bytecode, reporting incremental build
/// status to the user.
pub(crate) fn compile_to_beam(
    build_dir: &Utf8Path,
    core_files: &[Utf8PathBuf],
    changed_count: usize,
    unchanged_count: usize,
    total_files: usize,
) -> Result<()> {
    // Report incremental build status.
    if core_files.is_empty() {
        eprintln!("All {total_files} files unchanged — nothing to compile");
        info!("All files up-to-date — nothing to compile");
    } else if unchanged_count > 0 {
        eprintln!("Compiling {changed_count} of {total_files} files ({unchanged_count} unchanged)");
        info!(count = core_files.len(), "Compiling changed files to BEAM");
    } else {
        eprintln!("Compiling {total_files} files");
        info!(count = core_files.len(), "Compiling changed files to BEAM");
    }

    // Compile Core Erlang to BEAM (no-op if nothing changed).
    let beam_count = if core_files.is_empty() {
        0
    } else {
        let compiler = BeamCompiler::new(build_dir.to_path_buf());
        let beam_files = compiler
            .compile_batch(core_files)
            .wrap_err("Failed to compile Core Erlang to BEAM")?;
        beam_files.len()
    };

    info!(
        beam_count,
        skipped = unchanged_count,
        "Build completed successfully"
    );
    Ok(())
}
