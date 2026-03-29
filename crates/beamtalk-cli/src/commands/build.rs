// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build beamtalk projects.

use crate::beam_compiler::{BeamCompiler, compile_source_with_bindings};
use crate::commands::util;
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::time::SystemTime;
use tracing::{debug, error, info, instrument, warn};

use super::app_file;
use super::manifest;

/// Result of per-file change detection.
///
/// Compares each `.bt` source file's modification time against its corresponding
/// `.beam` output to determine which files need recompilation.
#[derive(Debug)]
#[allow(clippy::struct_field_names)] // `_files` postfix is clearer for this domain struct
pub(crate) struct ChangeDetectionResult {
    /// Source files that need (re)compilation — either modified since last build,
    /// newly added (no `.beam` exists), or included because of `--force`.
    pub changed_files: Vec<Utf8PathBuf>,
    /// Source files whose `.beam` output is up-to-date.
    pub unchanged_files: Vec<Utf8PathBuf>,
    /// `.beam` files in the build directory with no corresponding `.bt` source.
    /// These are reported as warnings but not deleted (users may have manually
    /// placed files or renamed sources intentionally).
    pub orphaned_beam_files: Vec<Utf8PathBuf>,
}

/// Detect which source files have changed relative to their compiled `.beam` output.
///
/// For each `.bt` source file, computes the expected `.beam` filename in `build_dir`
/// using the same module naming scheme as the build pipeline. A file is considered
/// changed if:
/// - Its `.beam` does not exist (new file or first build)
/// - Its mtime is newer than the `.beam` mtime
///
/// Also detects orphaned `.beam` files (no corresponding `.bt` source) and reports
/// them as warnings.
///
/// When `force` is true, all source files are treated as changed regardless of mtime.
pub(crate) fn detect_changes(
    source_files: &[Utf8PathBuf],
    build_dir: &Utf8Path,
    file_module_pairs: &[(Utf8PathBuf, String, Utf8PathBuf)],
    force: bool,
) -> ChangeDetectionResult {
    if force {
        info!("Force build requested — all files will be recompiled");
        // Still detect orphaned .beam files so users get consistent warnings
        let expected_beam_filenames: HashSet<String> = file_module_pairs
            .iter()
            .map(|(_, module_name, _)| format!("{module_name}.beam"))
            .collect();
        let orphaned_beam_files = detect_orphaned_beams(build_dir, &expected_beam_filenames);
        return ChangeDetectionResult {
            changed_files: source_files.to_vec(),
            unchanged_files: Vec::new(),
            orphaned_beam_files,
        };
    }

    let mut changed_files = Vec::new();
    let mut unchanged_files = Vec::new();

    // Build a set of expected .beam filenames from source files
    let mut expected_beam_stems: HashSet<String> = HashSet::new();

    for (source_file, module_name, _core_file) in file_module_pairs {
        let beam_file = build_dir.join(format!("{module_name}.beam"));
        expected_beam_stems.insert(format!("{module_name}.beam"));

        if !beam_file.exists() {
            debug!(file = %source_file, "No .beam output — needs compilation");
            changed_files.push(source_file.clone());
            continue;
        }

        // Compare mtimes: source newer than beam → needs recompilation
        let source_mtime = mtime_of(source_file);
        let beam_mtime = mtime_of(&beam_file);

        match (source_mtime, beam_mtime) {
            (Some(src_t), Some(beam_t)) if src_t > beam_t => {
                debug!(file = %source_file, "Source newer than .beam — needs recompilation");
                changed_files.push(source_file.clone());
            }
            (Some(_), Some(_)) => {
                debug!(file = %source_file, "Up-to-date");
                unchanged_files.push(source_file.clone());
            }
            _ => {
                // If we can't read mtimes, err on the side of recompilation
                debug!(file = %source_file, "Cannot read mtime — needs recompilation");
                changed_files.push(source_file.clone());
            }
        }
    }

    // Detect orphaned .beam files (exist in build dir but no corresponding source)
    let orphaned_beam_files = detect_orphaned_beams(build_dir, &expected_beam_stems);

    let total = changed_files.len() + unchanged_files.len();
    info!(
        changed = changed_files.len(),
        unchanged = unchanged_files.len(),
        orphaned = orphaned_beam_files.len(),
        total,
        "Change detection complete"
    );

    ChangeDetectionResult {
        changed_files,
        unchanged_files,
        orphaned_beam_files,
    }
}

/// Read the modification time of a file, returning `None` on any error.
fn mtime_of(path: &Utf8Path) -> Option<SystemTime> {
    fs::metadata(path).ok()?.modified().ok()
}

/// Find `bt@*` `.beam` files in the build directory that are not in the expected set.
fn detect_orphaned_beams(
    build_dir: &Utf8Path,
    expected_beam_filenames: &HashSet<String>,
) -> Vec<Utf8PathBuf> {
    let Ok(entries) = fs::read_dir(build_dir) else {
        return Vec::new();
    };

    let mut orphaned = Vec::new();
    for entry in entries.flatten() {
        let path = entry.path();
        let Some(name) = path.file_name().and_then(|n| n.to_str()) else {
            continue;
        };

        // Only consider bt@* beam files (user/package modules)
        if name.starts_with("bt@")
            && path.extension().is_some_and(|ext| ext == "beam")
            && !expected_beam_filenames.contains(name)
        {
            if let Ok(utf8) = Utf8PathBuf::from_path_buf(path) {
                orphaned.push(utf8);
            }
        }
    }

    orphaned
}

/// Build beamtalk source files.
///
/// This command compiles .bt files to .beam bytecode via Core Erlang.
///
/// When `force` is false, per-file change detection compares `.bt` source mtimes
/// against `.beam` output mtimes and only recompiles changed files.
#[allow(clippy::too_many_lines)]
#[instrument(skip_all, fields(path = %path))]
pub fn build(path: &str, options: &beamtalk_core::CompilerOptions, mut force: bool) -> Result<()> {
    info!("Starting build");
    let source_path = Utf8PathBuf::from(path);

    // Find source files
    let source_files = find_source_files(&source_path)?;

    if source_files.is_empty() {
        error!("No .bt source files found in '{}'", path);
        miette::bail!("No .bt source files found in '{path}'");
    }

    info!(count = source_files.len(), "Found source files");
    debug!("Building {} file(s)", source_files.len());

    // Determine project root (for directory input, use the path; for file input, use parent)
    let project_root = if source_path.is_dir() {
        source_path.clone()
    } else {
        source_path
            .parent()
            .map_or_else(|| Utf8PathBuf::from("."), Utf8Path::to_path_buf)
    };

    // Look for package manifest (full parse includes dependencies for
    // transitive dep tracking — ADR 0070 Phase 3)
    let full_manifest = manifest::find_manifest_full(&project_root)?;
    let pkg_manifest = full_manifest.as_ref().map(|m| &m.package);
    if let Some(pkg) = pkg_manifest {
        info!(name = %pkg.name, version = %pkg.version, "Found package manifest");
        debug!(?pkg, "Package manifest details");
    } else {
        debug!("No beamtalk.toml found, using default behavior");
    }

    // Create build directory relative to project root
    // ADR 0026 §5: Package mode outputs to _build/dev/ebin/, single-file mode keeps build/
    let build_dir = if pkg_manifest.is_some() {
        project_root.join("_build").join("dev").join("ebin")
    } else {
        project_root.join("build")
    };

    debug!("Creating build directory: {}", build_dir);
    std::fs::create_dir_all(&build_dir)
        .into_diagnostic()
        .wrap_err("Failed to create build directory")?;

    // ADR 0070 Phase 1: Resolve and compile dependencies when needed.
    // Uses staleness detection: no-op when lockfile is fresh and deps are compiled.
    // Otherwise resolves the full transitive graph in topological order.
    let resolved_deps = if pkg_manifest.is_some() {
        super::deps::ensure_deps_resolved(&project_root, options)?
    } else {
        Vec::new()
    };

    // ADR 0072 Phase 1 (Path A): Compile native Erlang sources before .bt files.
    // Native modules must be compiled first because .bt files may reference them
    // via `(Erlang module)` FFI or `native:` annotations.
    let _native_result = if pkg_manifest.is_some() {
        use crate::beam_compiler::compile_native_erlang;
        match compile_native_erlang(&project_root)? {
            Some(result) => {
                eprintln!(
                    "Compiling {} native Erlang file(s)",
                    result.beam_files.len()
                );
                Some(result)
            }
            None => None,
        }
    } else {
        None
    };

    // Determine the source root for computing relative module paths
    let src_dir = project_root.join("src");
    let source_root = if src_dir.exists() {
        Some(src_dir)
    } else {
        None
    };

    // Pass 1: compute module names and build the class → module index.
    // This allows later files to resolve cross-file class references (including
    // classes in package subdirectories) during code generation.
    // BT-1683: Use incremental cache to skip unchanged files in Pass 1.
    let mut file_module_pairs: Vec<(Utf8PathBuf, String, Utf8PathBuf)> = Vec::new();
    let manifest_path = if pkg_manifest.is_some() {
        let p = project_root.join("beamtalk.toml");
        if p.exists() { Some(p) } else { None }
    } else {
        None
    };
    let (mut class_module_index, class_superclass_index, all_class_infos, cached_asts) =
        if let Some(pkg) = pkg_manifest {
            let result = super::build_cache::incremental_build_class_module_index(
                &source_files,
                source_root.as_deref(),
                &pkg.name,
                &build_dir,
                manifest_path.as_deref(),
                force,
            )?;
            // If the manifest changed, force Pass 2 recompilation too —
            // .bt files may be unchanged but semantics depend on the manifest.
            if result.manifest_invalidated {
                force = true;
            }
            (
                result.class_module_index,
                result.class_superclass_index,
                result.all_class_infos,
                result.cached_asts,
            )
        } else {
            (HashMap::new(), HashMap::new(), Vec::new(), HashMap::new())
        };

    // ADR 0070: Merge dependency class indexes into the main package's indexes
    // so cross-package class references resolve during compilation.
    // Also build a DependencyRegistry for collision detection (Phase 3).
    // BT-1654: Track direct vs transitive dependencies for W0302 warnings.
    let dep_infos: Vec<beamtalk_core::semantic_analysis::DepInfo> = resolved_deps
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

    for dep in &resolved_deps {
        for (class_name, module_name) in &dep.class_module_index {
            debug!(
                dep = %dep.name,
                class = %class_name,
                module = %module_name,
                "Adding dependency class to index"
            );
            class_module_index.insert(class_name.clone(), module_name.clone());
        }
    }

    // BT-1653 / ADR 0070 Phase 3: Eagerly check stdlib reservation violations.
    // Dependencies must not export classes with stdlib-reserved names.
    if !dep_registry.is_empty() && !options.stdlib_mode {
        let mut reservation_diags = Vec::new();
        beamtalk_core::semantic_analysis::check_stdlib_reservation(
            &dep_registry,
            &mut reservation_diags,
        );
        if !reservation_diags.is_empty() {
            let has_errors = reservation_diags
                .iter()
                .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error);
            if has_errors {
                // Collect all error messages for reporting
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
        }
    }

    for file in &source_files {
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
            let relative_module = compute_relative_module(file, source_root.as_deref())?;
            format!("bt@{}@{}", pkg.name, relative_module)
        } else {
            format!(
                "bt@{}",
                beamtalk_core::codegen::core_erlang::to_module_name(stem)
            )
        };
        let core_file = build_dir.join(format!("{module_name}.core"));
        file_module_pairs.push((file.clone(), module_name, core_file));
    }

    // BT-1682: Per-file change detection — only recompile files whose source
    // is newer than the corresponding .beam output.
    let changes = detect_changes(&source_files, &build_dir, &file_module_pairs, force);

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
    let mut cached_asts = cached_asts;
    let mut core_files = Vec::new();
    let mut module_names = Vec::new();
    let registry_ref = if dep_registry.is_empty() {
        None
    } else {
        Some(&dep_registry)
    };
    let strict_deps = full_manifest
        .as_ref()
        .is_some_and(|m| m.package.strict_deps);
    for (file, module_name, core_file) in &file_module_pairs {
        // Always track module names for .app generation
        module_names.push(module_name.clone());

        if !changed_set.contains(file.as_path()) {
            debug!(file = %file, "Skipping unchanged file");
            continue;
        }

        let cached = cached_asts.remove(file);
        compile_file(
            file,
            module_name,
            core_file,
            options,
            &class_module_index,
            &class_superclass_index,
            &all_class_infos,
            cached,
            registry_ref,
            strict_deps,
        )?;
        core_files.push(core_file.clone());
    }

    // Report incremental build status to the user.
    let total_files = file_module_pairs.len();
    let changed_count = changes.changed_files.len();
    let unchanged_count = changes.unchanged_files.len();

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

    let beam_files = if core_files.is_empty() {
        Vec::new()
    } else {
        let compiler = BeamCompiler::new(build_dir.clone());
        compiler
            .compile_batch(&core_files)
            .wrap_err("Failed to compile Core Erlang to BEAM")?
    };

    info!(
        beam_count = beam_files.len(),
        skipped = changes.unchanged_files.len(),
        "Build completed successfully"
    );

    // ADR 0026 §3: Package mode post-processing — clean stale artifacts and generate .app
    if let Some(pkg) = pkg_manifest {
        // BT-1682: For stale artifact cleanup, we need ALL expected .beam files
        // (both newly compiled and unchanged), not just the ones from this build.
        let all_expected_beams: Vec<Utf8PathBuf> = file_module_pairs
            .iter()
            .map(|(_, module_name, _)| build_dir.join(format!("{module_name}.beam")))
            .collect();
        clean_stale_artifacts(&build_dir, &all_expected_beams, &pkg.name)?;
        generate_package_outputs(
            &build_dir,
            &project_root,
            pkg,
            &module_names,
            &class_module_index,
        )?;
    }

    Ok(())
}

/// Generate OTP application artefacts for a package build.
///
/// Emits the `.app` file and, when `[application] supervisor` is set, the
/// OTP application callback module (`beamtalk_{appname}_app.erl` + `.beam`).
///
/// `class_module_index` maps Beamtalk class names to their compiled Erlang module
/// names (e.g. `"AppSup"` → `"bt@my_app@supervision@app_sup"`). Used to resolve
/// the supervisor class's actual module regardless of source file path.
fn generate_package_outputs(
    build_dir: &Utf8Path,
    project_root: &Utf8PathBuf,
    pkg: &manifest::PackageManifest,
    module_names: &[String],
    class_module_index: &HashMap<String, String>,
) -> Result<()> {
    // TODO: Extract class metadata from compiled modules in a future issue.
    let class_metadata: Vec<app_file::ClassMetadata> = Vec::new();

    // BT-1191: Generate OTP application callback when [application] supervisor is set.
    let app_callback_module =
        if let Some(ref app_config) = manifest::find_application_config(project_root)? {
            let cb_module_name = format!("beamtalk_{}_app", pkg.name);
            // Resolve the supervisor's actual Erlang module via the class index.
            // This correctly handles classes in subdirectories (e.g. src/app/app_sup.bt).
            let sup_module = class_module_index
                .get(&app_config.supervisor)
                .ok_or_else(|| {
                    miette::miette!(
                        "Cannot find compiled module for supervisor class '{}'. \
                         Ensure the class is defined in a .bt source file in this package.",
                        app_config.supervisor
                    )
                })?;
            generate_otp_app_callback(
                build_dir,
                &app_config.supervisor,
                sup_module,
                &cb_module_name,
            )?;
            info!(
                supervisor = %app_config.supervisor,
                module = %cb_module_name,
                "Generated OTP application callback"
            );
            Some(cb_module_name)
        } else {
            None
        };

    // Include the generated callback module in the .app modules list so release
    // tooling (appup generation, etc.) can account for it.
    let all_modules: Vec<String> = if let Some(ref cb) = app_callback_module {
        let mut v = module_names.to_vec();
        v.push(cb.clone());
        v
    } else {
        module_names.to_vec()
    };

    app_file::generate_app_file(
        build_dir,
        pkg,
        &all_modules,
        &class_metadata,
        app_callback_module.as_deref(),
    )?;
    info!(name = %pkg.name, "Generated .app file");
    Ok(())
}

/// Generate an OTP application callback module (`beamtalk_{appname}_app.erl`).
///
/// The generated module implements the OTP `application` behaviour, calling
/// the Beamtalk supervisor's `start_link` from `start/2`. It is compiled to
/// BEAM and placed alongside the other package modules in the build directory.
fn generate_otp_app_callback(
    build_dir: &Utf8Path,
    supervisor_class: &str,
    sup_module: &str,
    cb_module_name: &str,
) -> Result<()> {
    let src = format!(
        "%% Copyright 2026 James Casey\n\
         %% SPDX-License-Identifier: Apache-2.0\n\
         %%\n\
         %% Generated OTP application callback.\n\
         %% Do not edit — regenerated by `beamtalk build`.\n\
         -module({cb_module_name}).\n\
         -behaviour(application).\n\
         -export([start/2, stop/1]).\n\
         \n\
         start(_Type, _Args) ->\n\
             case '{sup_module}':'start_link'() of\n\
                 {{ok, Pid}} = Ok ->\n\
                     SupTuple = {{beamtalk_supervisor, '{supervisor_class}', '{sup_module}', Pid}},\n\
                     beamtalk_supervisor:register_root(SupTuple),\n\
                     Ok;\n\
                 Err ->\n\
                     Err\n\
             end.\n\
         \n\
         stop(_State) -> ok.\n"
    );

    // Write the .erl source next to the .core files so erlc can pick it up
    let erl_path = build_dir.join(format!("{cb_module_name}.erl"));
    fs::write(&erl_path, src)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write OTP app callback '{erl_path}'"))?;

    // Compile the generated .erl with erlc
    let status = std::process::Command::new("erlc")
        .arg("-o")
        .arg(build_dir.as_str())
        .arg(erl_path.as_str())
        .status()
        .into_diagnostic()
        .wrap_err("Failed to run erlc for OTP app callback")?;

    if !status.success() {
        miette::bail!(
            "erlc failed to compile OTP app callback '{}'. Is Erlang/OTP installed?",
            cb_module_name
        );
    }

    Ok(())
}

/// Remove stale build artifacts from the build directory.
///
/// After a successful build, compares the set of `.beam` files just produced
/// against all `bt@*.beam` files on disk. Any on-disk file not in the produced
/// set is stale (from a deleted source file, renamed class, or package rename)
/// and is removed along with its corresponding `.core` file.
///
/// Also removes `.app` files that don't match the current package name (from
/// package renames).
fn clean_stale_artifacts(
    build_dir: &Utf8Path,
    produced_beams: &[Utf8PathBuf],
    pkg_name: &str,
) -> Result<()> {
    let produced: std::collections::HashSet<&Utf8Path> =
        produced_beams.iter().map(Utf8PathBuf::as_path).collect();

    let current_app_file = format!("{pkg_name}.app");

    let entries = fs::read_dir(build_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read build directory '{build_dir}'"))?;

    for entry in entries {
        let entry = entry.into_diagnostic()?;
        let path = entry.path();
        let Some(name) = path.file_name().and_then(|n| n.to_str()) else {
            continue;
        };

        let ext = path.extension().and_then(|e| e.to_str());

        // Clean stale .app files from package renames
        if ext == Some("app") && name != current_app_file {
            let utf8 = Utf8PathBuf::from_path_buf(path.clone())
                .map_err(|_| miette::miette!("Non-UTF-8 path in build dir"))?;
            debug!(file = %utf8, "Removing stale .app artifact");
            fs::remove_file(&utf8).into_diagnostic()?;
            continue;
        }

        // Only consider bt@* files (user/package modules) for beam/core cleanup
        if !name.starts_with("bt@") {
            continue;
        }

        match ext {
            Some("beam") => {
                let utf8 = Utf8PathBuf::from_path_buf(path.clone())
                    .map_err(|_| miette::miette!("Non-UTF-8 path in build dir"))?;
                if !produced.contains(utf8.as_path()) {
                    debug!(file = %utf8, "Removing stale .beam artifact");
                    fs::remove_file(&utf8).into_diagnostic()?;
                    // Also remove the companion .core file if present
                    let core = utf8.with_extension("core");
                    if core.exists() {
                        fs::remove_file(&core).into_diagnostic()?;
                    }
                }
            }
            Some("core") => {
                // Stale .core files whose .beam was already removed (or never produced).
                // Tolerates NotFound because the .beam branch may have already removed
                // the companion .core in the same cleanup pass.
                let beam_path = path.with_extension("beam");
                if !beam_path.exists() {
                    let utf8 = Utf8PathBuf::from_path_buf(path.clone())
                        .map_err(|_| miette::miette!("Non-UTF-8 path in build dir"))?;
                    match fs::remove_file(&utf8) {
                        Ok(()) => debug!(file = %utf8, "Removing orphaned .core artifact"),
                        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
                        Err(e) => return Err(e).into_diagnostic(),
                    }
                }
            }
            _ => {}
        }
    }

    Ok(())
}

/// Find all `.bt` source files at the given path.
///
/// If `path` is a file, returns it (must have `.bt` extension).
/// If `path` is a directory, searches `src/` subdirectory first, falling back
/// to the directory itself.
fn find_source_files(path: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    if path.is_file() {
        if path.extension() == Some("bt") {
            return Ok(vec![path.to_path_buf()]);
        }
        miette::bail!("File '{}' is not a .bt source file", path);
    }

    if !path.exists() {
        miette::bail!("Path '{}' does not exist", path);
    }

    // Look for src directory or .bt files in current directory
    let src_dir = path.join("src");
    let search_dir = if src_dir.exists() {
        src_dir
    } else {
        path.to_path_buf()
    };

    let mut files = Vec::new();
    util::collect_files_recursive(&search_dir, &["bt"], &mut files)?;
    files.sort();
    Ok(files)
}

/// Collect all `.bt` source files from a directory tree.
///
/// Returns an error if the directory does not exist or cannot be read.
pub fn collect_source_files_from_dir(dir: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    let mut files = Vec::new();
    util::collect_files_recursive(dir, &["bt"], &mut files)?;
    files.sort();
    Ok(files)
}

/// Collect all `.bt` and `.btscript` source files from a directory tree.
///
/// Used by `beamtalk fmt` to find all formattable files when given a directory
/// path. Returns an error if the directory does not exist or cannot be read.
pub fn collect_formattable_files_from_dir(dir: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    let mut files = Vec::new();
    util::collect_files_recursive(dir, &["bt", "btscript"], &mut files)?;
    files.sort();
    Ok(files)
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
pub(crate) fn compute_relative_module(
    file: &Utf8Path,
    source_root: Option<&Utf8Path>,
) -> Result<String> {
    if let Some(root) = source_root {
        if let Ok(relative) = file.strip_prefix(root) {
            let without_ext = relative.with_extension("");
            let segments: Vec<String> = without_ext
                .components()
                .map(|c| {
                    let segment = c.as_str();
                    // Validate each path segment
                    if !segment
                        .chars()
                        .all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
                    {
                        miette::bail!(
                            "Invalid directory name '{}' in source path '{}': must contain only alphanumeric characters and underscores",
                            segment,
                            file
                        );
                    }
                    Ok(beamtalk_core::codegen::core_erlang::to_module_name(segment))
                })
                .collect::<Result<_>>()?;
            return Ok(segments.join("@"));
        }
    }
    // Fallback: use file stem
    let stem = file.file_stem().unwrap_or("unknown");
    Ok(beamtalk_core::codegen::core_erlang::to_module_name(stem))
}

/// Compile a single `.bt` source file to Core Erlang, printing progress.
///
/// When `cached_ast` is `Some`, reuses the pre-parsed source and `Module` from
/// Pass 1 instead of re-reading and re-parsing the file (BT-1544).
#[allow(clippy::too_many_arguments)]
fn compile_file(
    path: &Utf8Path,
    module_name: &str,
    core_file: &Utf8Path,
    options: &beamtalk_core::CompilerOptions,
    class_module_index: &HashMap<String, String>,
    class_superclass_index: &HashMap<String, String>,
    pre_loaded_classes: &[beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo],
    cached_ast: Option<CachedAst>,
    dep_registry: Option<&beamtalk_core::semantic_analysis::DependencyRegistry>,
    strict_deps: bool,
) -> Result<()> {
    debug!("Compiling {path}");

    compile_source_with_bindings(
        path,
        module_name,
        core_file,
        options,
        &beamtalk_core::erlang::primitive_bindings::PrimitiveBindingTable::new(),
        class_module_index,
        class_superclass_index,
        pre_loaded_classes,
        cached_ast,
        dep_registry,
        strict_deps,
    )?;

    debug!("Generated Core Erlang: {core_file}");

    Ok(())
}

/// Cached AST from Pass 1 — holds the source text, parsed `Module`, and any
/// parse diagnostics so Pass 2 can skip re-reading and re-parsing the same file.
#[derive(Debug)]
pub(crate) struct CachedAst {
    pub(crate) source: String,
    pub(crate) module: beamtalk_core::ast::Module,
    pub(crate) diagnostics: Vec<beamtalk_core::source_analysis::Diagnostic>,
}

/// Build class indexes from a set of source files.
///
/// Returns four items:
/// 1. **Class module index:** Maps class names to compiled module names
///    (e.g. `"SchemeEnv"` → `"bt@sicp_example@scheme@env"`).
/// 2. **Class superclass index:** Maps class names to their direct superclass names
///    (e.g. `"MyChild"` → `"MyParent"`). Used by BT-894 to resolve cross-file
///    inheritance so the compiler can determine value-object vs actor codegen.
/// 3. **Class infos:** Full `ClassInfo` entries extracted from all source files.
///    BT-1523: Injected into the type checker's hierarchy during Pass 2 so
///    cross-file method resolution works without reading BEAM files.
/// 4. **Cached ASTs:** BT-1544: Maps file paths to their parsed `Module` + source
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
    HashMap<Utf8PathBuf, CachedAst>,
)> {
    let mut module_index = HashMap::new();
    let mut superclass_index = HashMap::new();
    let mut all_class_infos = Vec::new();
    let mut cached_asts: HashMap<Utf8PathBuf, CachedAst> = HashMap::new();

    for file in source_files {
        let relative_module = compute_relative_module(file, source_root)?;
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

        // BT-1523: Extract full ClassInfo for cross-file hierarchy resolution.
        let class_infos =
            beamtalk_core::semantic_analysis::ClassHierarchy::extract_class_infos(&module);
        all_class_infos.extend(class_infos);

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
            // BT-894: Record direct superclass for cross-file hierarchy resolution.
            // When a duplicate class overwrites a prior entry, keep the superclass
            // index consistent by removing any stale mapping if the new definition
            // has no explicit superclass.
            if let Some(ref superclass) = class.superclass {
                superclass_index.insert(class_name.clone(), superclass.name.to_string());
            } else {
                superclass_index.remove(&class_name);
            }
        }

        // BT-1544: Cache the parsed AST so Pass 2 doesn't re-read/re-parse.
        cached_asts.insert(
            file.clone(),
            CachedAst {
                source,
                module,
                diagnostics,
            },
        );
    }

    Ok((module_index, superclass_index, all_class_infos, cached_asts))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn create_test_project(temp: &TempDir) -> Utf8PathBuf {
        let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_path = project_path.join("src");
        fs::create_dir_all(&src_path).unwrap();
        project_path
    }

    fn write_test_file(path: &Utf8Path, content: &str) {
        fs::write(path, content).unwrap();
    }

    fn default_options() -> beamtalk_core::CompilerOptions {
        beamtalk_core::CompilerOptions::default()
    }

    #[test]
    fn test_find_source_files_single_file() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let test_file = project_path.join("test.bt");
        write_test_file(&test_file, "test := [1].");

        let files = find_source_files(&test_file).unwrap();
        assert_eq!(files.len(), 1);
        assert_eq!(files[0], test_file);
    }

    #[test]
    fn test_find_source_files_in_directory() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");

        write_test_file(&src_path.join("file1.bt"), "test := [1].");
        write_test_file(&src_path.join("file2.bt"), "test := [2].");
        write_test_file(&src_path.join("other.txt"), "not beamtalk");

        let files = find_source_files(&project_path).unwrap();
        assert_eq!(files.len(), 2);
        assert!(files.iter().any(|f| f.file_name() == Some("file1.bt")));
        assert!(files.iter().any(|f| f.file_name() == Some("file2.bt")));
    }

    #[test]
    fn test_find_source_files_no_src_directory() {
        let temp = TempDir::new().unwrap();
        let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        write_test_file(&project_path.join("test.bt"), "test := [1].");

        let files = find_source_files(&project_path).unwrap();
        assert_eq!(files.len(), 1);
    }

    #[test]
    fn test_find_source_files_non_bt_file_error() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let test_file = project_path.join("test.txt");
        write_test_file(&test_file, "not beamtalk");

        let result = find_source_files(&test_file);
        assert!(result.is_err());
    }

    #[test]
    fn test_find_source_files_nonexistent_path() {
        let path = Utf8PathBuf::from("/nonexistent/path");
        let result = find_source_files(&path);
        assert!(result.is_err());
    }

    #[test]
    fn test_compile_valid_file() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let test_file = project_path.join("test.bt");
        let core_file = project_path.join("test.core");
        write_test_file(&test_file, "test := [1 + 2].");

        let result = compile_file(
            &test_file,
            "test",
            &core_file,
            &default_options(),
            &HashMap::new(),
            &HashMap::new(),
            &[],
            None,
            None,
            false,
        );
        assert!(result.is_ok());
        assert!(core_file.exists());
    }

    #[test]
    fn test_compile_file_with_syntax_error() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let test_file = project_path.join("test.bt");
        let core_file = project_path.join("test.core");
        write_test_file(&test_file, "test := [1 + ]."); // Syntax error

        let result = compile_file(
            &test_file,
            "test",
            &core_file,
            &default_options(),
            &HashMap::new(),
            &HashMap::new(),
            &[],
            None,
            None,
            false,
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_build_empty_directory() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);

        let result = build(project_path.as_str(), &default_options(), false);
        assert!(result.is_err());
    }

    #[test]
    fn test_build_single_file() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("main.bt"), "main := [42].");

        let result = build(project_path.as_str(), &default_options(), false);

        // If escript is not available, the test should fail at the BEAM compilation stage
        // We allow this in CI environments
        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                println!("Skipping test - escript not installed in CI environment");
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_multiple_files() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");

        write_test_file(&src_path.join("file1.bt"), "test1 := [1].");
        write_test_file(&src_path.join("file2.bt"), "test2 := [2].");

        let result = build(project_path.as_str(), &default_options(), false);

        // If escript is not available, the test should fail at the BEAM compilation stage
        // We allow this in CI environments
        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                println!("Skipping test - escript not installed in CI environment");
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_with_manifest() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("main.bt"), "main := [42].");
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
        );

        let result = build(project_path.as_str(), &default_options(), false);

        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                eprintln!("Skipping test - escript not installed in CI environment");
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_with_malformed_manifest() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("main.bt"), "main := [42].");
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "this is not valid toml {{{{",
        );

        let result = build(project_path.as_str(), &default_options(), false);
        let err = result.expect_err("Expected build to fail due to malformed manifest");
        let error_msg = format!("{err:?}");
        assert!(
            error_msg.contains("Failed to parse manifest"),
            "Expected error to mention manifest parse failure, got: {error_msg}"
        );
    }

    #[test]
    fn test_find_source_files_recursive() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        let util_path = src_path.join("util");
        fs::create_dir_all(&util_path).unwrap();

        write_test_file(&src_path.join("main.bt"), "main := [1].");
        write_test_file(&util_path.join("math.bt"), "math := [2].");

        let files = find_source_files(&project_path).unwrap();
        assert_eq!(files.len(), 2);
        assert!(files.iter().any(|f| f.file_name() == Some("main.bt")));
        assert!(files.iter().any(|f| f.file_name() == Some("math.bt")));
    }

    #[test]
    fn test_find_source_files_deeply_nested() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        let deep_path = src_path.join("a").join("b").join("c");
        fs::create_dir_all(&deep_path).unwrap();

        write_test_file(&deep_path.join("deep.bt"), "deep := [42].");

        let files = find_source_files(&project_path).unwrap();
        assert_eq!(files.len(), 1);
        assert!(files[0].as_str().contains('a'));
    }

    #[test]
    fn test_compute_relative_module_flat() {
        let root = Utf8Path::new("/project/src");
        let file = Utf8Path::new("/project/src/counter.bt");
        assert_eq!(
            compute_relative_module(file, Some(root)).unwrap(),
            "counter"
        );
    }

    #[test]
    fn test_compute_relative_module_subdirectory() {
        let root = Utf8Path::new("/project/src");
        let file = Utf8Path::new("/project/src/util/math.bt");
        assert_eq!(
            compute_relative_module(file, Some(root)).unwrap(),
            "util@math"
        );
    }

    #[test]
    fn test_compute_relative_module_deep_subdirectory() {
        let root = Utf8Path::new("/project/src");
        let file = Utf8Path::new("/project/src/a/b/c.bt");
        assert_eq!(compute_relative_module(file, Some(root)).unwrap(), "a@b@c");
    }

    #[test]
    fn test_compute_relative_module_camel_case() {
        let root = Utf8Path::new("/project/src");
        let file = Utf8Path::new("/project/src/MyCounter.bt");
        assert_eq!(
            compute_relative_module(file, Some(root)).unwrap(),
            "my_counter"
        );
    }

    #[test]
    fn test_compute_relative_module_no_root() {
        let file = Utf8Path::new("/project/counter.bt");
        assert_eq!(compute_relative_module(file, None).unwrap(), "counter");
    }

    #[test]
    fn test_compute_relative_module_invalid_dir_name() {
        let root = Utf8Path::new("/project/src");
        let file = Utf8Path::new("/project/src/my-dir/counter.bt");
        let result = compute_relative_module(file, Some(root));
        assert!(result.is_err());
    }

    #[test]
    fn test_build_with_manifest_generates_package_module_name() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("counter.bt"), "counter := [42].");
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
        );

        let result = build(project_path.as_str(), &default_options(), false);

        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                // Verify the .core file was generated with package-qualified name
                let core_file = project_path.join("_build/dev/ebin/bt@my_app@counter.core");
                assert!(
                    core_file.exists(),
                    "Expected package-qualified .core file at {core_file}"
                );
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_without_manifest_preserves_bt_prefix() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("counter.bt"), "counter := [42].");
        // No beamtalk.toml

        let result = build(project_path.as_str(), &default_options(), false);

        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                let core_file = project_path.join("build/bt@counter.core");
                assert!(
                    core_file.exists(),
                    "Expected bt@counter.core file at {core_file}"
                );
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_with_manifest_subdirectory() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        let util_path = src_path.join("util");
        fs::create_dir_all(&util_path).unwrap();
        write_test_file(&util_path.join("math.bt"), "math := [42].");
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
        );

        let result = build(project_path.as_str(), &default_options(), false);

        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                let core_file = project_path.join("_build/dev/ebin/bt@my_app@util@math.core");
                assert!(
                    core_file.exists(),
                    "Expected package-qualified .core file at {core_file}"
                );
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_with_manifest_creates_build_dev_ebin() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("main.bt"), "main := [42].");
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n",
        );

        let result = build(project_path.as_str(), &default_options(), false);

        // Verify _build/dev/ebin/ directory structure exists
        let ebin_dir = project_path.join("_build/dev/ebin");
        assert!(ebin_dir.exists(), "Expected _build/dev/ebin/ directory");

        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_without_manifest_uses_build_dir() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("main.bt"), "main := [42].");
        // No beamtalk.toml

        let result = build(project_path.as_str(), &default_options(), false);

        // Verify build/ directory is used (not _build/)
        let build_dir = project_path.join("build");
        assert!(
            build_dir.exists(),
            "Expected build/ directory for single-file mode"
        );
        let ebin_dir = project_path.join("_build");
        assert!(
            !ebin_dir.exists(),
            "_build/ should not exist in single-file mode"
        );

        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }
    }

    #[test]
    fn test_build_with_manifest_generates_app_file() {
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("counter.bt"), "counter := [42].");
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "[package]\nname = \"my_app\"\nversion = \"0.1.0\"\ndescription = \"Test app\"\n",
        );

        let result = build(project_path.as_str(), &default_options(), false);

        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                // .app is generated after BEAM compile — may not exist if escript missing
                // but the .core file should exist
                let core_file = project_path.join("_build/dev/ebin/bt@my_app@counter.core");
                assert!(core_file.exists(), "Expected .core file at {core_file}");
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }

        // If BEAM compilation succeeded, .app file should exist
        let app_file = project_path.join("_build/dev/ebin/my_app.app");
        assert!(app_file.exists(), "Expected .app file at {app_file}");
        let content = fs::read_to_string(&app_file).unwrap();
        assert!(content.contains("{application, my_app, ["));
        assert!(content.contains("{description, \"Test app\"}"));
        assert!(content.contains("{vsn, \"0.1.0\"}"));
        assert!(content.contains("'bt@my_app@counter'"));
    }

    #[test]
    fn test_build_class_module_index_skips_unreadable_file() {
        // A path that does not exist on disk — simulates an unreadable file.
        // The function should succeed (not error) and return an empty index.
        let nonexistent = Utf8PathBuf::from("/nonexistent/no_such_file.bt");
        let result = build_class_module_index(&[nonexistent], None, "my_app");
        assert!(result.is_ok());
        let (module_index, superclass_index, _class_infos, _cached_asts) = result.unwrap();
        assert!(module_index.is_empty());
        assert!(superclass_index.is_empty());
    }

    #[test]
    fn test_build_class_module_index_handles_parse_errors_gracefully() {
        let temp = TempDir::new().unwrap();
        let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let bad_file = project_path.join("bad.bt");
        // Write a file with a syntax error — parse diagnostics should be non-empty.
        write_test_file(&bad_file, "class Foo { @@@@invalid syntax }");

        // Should succeed and return whatever classes (if any) were parsed before the error.
        let result = build_class_module_index(&[bad_file], None, "my_app");
        assert!(result.is_ok());
    }

    /// BT-906: Verify that `build_class_module_index` correctly maps actor classes
    /// defined in subdirectories to their full subdirectory-qualified module names.
    ///
    /// When a package has `src/observer/event_bus.bt` defining `EventBus`, the index
    /// must map `EventBus → bt@gang_of_four@observer@event_bus` (not the heuristic
    /// fallback `bt@gang_of_four@event_bus` which drops the `observer@` segment).
    #[test]
    fn test_build_class_module_index_subdirectory_actor_class() {
        let temp = TempDir::new().unwrap();
        let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_path = project_path.join("src");
        let observer_path = src_path.join("observer");
        fs::create_dir_all(&observer_path).unwrap();

        write_test_file(
            &observer_path.join("event_bus.bt"),
            "Actor subclass: EventBus\n  notify: e => nil\n",
        );

        let source_files = vec![observer_path.join("event_bus.bt")];
        let (index, _superclass, _class_infos, _cached_asts) =
            build_class_module_index(&source_files, Some(&src_path), "gang_of_four").unwrap();

        assert_eq!(
            index.get("EventBus").map(String::as_str),
            Some("bt@gang_of_four@observer@event_bus"),
            "EventBus in observer/ subdirectory must map to full subdirectory-qualified module name"
        );
    }

    /// BT-906: Verify end-to-end that cross-file actor spawn uses the correct
    /// subdirectory-qualified module path in the generated Core Erlang.
    ///
    /// When `src/main.bt` calls `EventBus spawn` and `EventBus` is defined in
    /// `src/observer/event_bus.bt`, the generated Core Erlang must reference
    /// `bt@gang_of_four@observer@event_bus` (not `bt@gang_of_four@event_bus`).
    #[test]
    fn test_cross_file_subdirectory_actor_spawn_uses_correct_module() {
        let temp = TempDir::new().unwrap();
        let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_path = project_path.join("src");
        let observer_path = src_path.join("observer");
        fs::create_dir_all(&observer_path).unwrap();

        // File 1: EventBus actor in observer/ subdirectory
        write_test_file(
            &observer_path.join("event_bus.bt"),
            "Actor subclass: EventBus\n  notify: e => nil\n",
        );

        // File 2: main.bt that references EventBus spawn
        write_test_file(
            &src_path.join("main.bt"),
            "Actor subclass: Main\n  run => EventBus spawn\n",
        );

        let build_dir = project_path.join("_build/dev/ebin");
        fs::create_dir_all(&build_dir).unwrap();

        let source_files = vec![observer_path.join("event_bus.bt"), src_path.join("main.bt")];
        let (class_module_index, class_superclass_index, all_class_infos, _cached_asts) =
            build_class_module_index(&source_files, Some(&src_path), "gang_of_four").unwrap();

        // Build the class index — EventBus should map to observer subdir module
        assert_eq!(
            class_module_index.get("EventBus").map(String::as_str),
            Some("bt@gang_of_four@observer@event_bus"),
            "Index pass should map EventBus to subdirectory module"
        );

        // Compile main.bt with the index
        let core_file = build_dir.join("bt@gang_of_four@main.core");
        let options = default_options();
        compile_file(
            &src_path.join("main.bt"),
            "bt@gang_of_four@main",
            &core_file,
            &options,
            &class_module_index,
            &class_superclass_index,
            &all_class_infos,
            None,
            None,
            false,
        )
        .unwrap();

        let core_content = fs::read_to_string(&core_file).unwrap();

        // The spawn call must reference the full subdirectory-qualified module name
        assert!(
            core_content.contains("bt@gang_of_four@observer@event_bus"),
            "Spawn call should use observer@ subdirectory module. Generated Core Erlang:\n{core_content}"
        );
        assert!(
            !core_content.contains("'bt@gang_of_four@event_bus'"),
            "Should NOT use heuristic module name that drops observer@ segment. Generated:\n{core_content}"
        );
    }

    /// BT-1523: Verify cross-file class hierarchy resolution.
    ///
    /// When `InheritingCounter` (file 2) subclasses `Counter` (file 1) and calls
    /// `self getValue`, the type checker should NOT emit a DNU warning because
    /// `getValue` is inherited from `Counter` via the cross-file `ClassInfo` injection.
    #[test]
    fn test_cross_file_inheritance_no_false_dnu_warning() {
        let temp = TempDir::new().unwrap();
        let project_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_path = project_path.join("src");
        fs::create_dir_all(&src_path).unwrap();

        // File 1: Counter with getValue method and count state
        write_test_file(
            &src_path.join("counter.bt"),
            "Actor subclass: Counter\n  state: count = 0\n  getValue => self.count\n  increment => self.count := self.count + 1\n",
        );

        // File 2: InheritingCounter that calls inherited getValue
        write_test_file(
            &src_path.join("inheriting_counter.bt"),
            "Counter subclass: InheritingCounter\n  getDoubled => self getValue * 2\n",
        );

        let build_dir = project_path.join("_build/dev/ebin");
        fs::create_dir_all(&build_dir).unwrap();

        let source_files = vec![
            src_path.join("counter.bt"),
            src_path.join("inheriting_counter.bt"),
        ];
        let (class_module_index, class_superclass_index, all_class_infos, _cached_asts) =
            build_class_module_index(&source_files, Some(&src_path), "test_pkg").unwrap();

        // Verify ClassInfo extraction captured Counter's methods
        assert!(
            all_class_infos.iter().any(|ci| ci.name == "Counter"),
            "Pass 1 should extract ClassInfo for Counter"
        );
        let counter_info = all_class_infos
            .iter()
            .find(|ci| ci.name == "Counter")
            .unwrap();
        assert!(
            counter_info
                .methods
                .iter()
                .any(|m| m.selector == "getValue"),
            "Counter ClassInfo should include getValue method"
        );

        // Compile InheritingCounter with cross-file class infos — should succeed
        // (no false DNU error for getValue which is inherited from Counter)
        let core_file = build_dir.join("bt@test_pkg@inheriting_counter.core");
        let options = default_options();
        compile_file(
            &src_path.join("inheriting_counter.bt"),
            "bt@test_pkg@inheriting_counter",
            &core_file,
            &options,
            &class_module_index,
            &class_superclass_index,
            &all_class_infos,
            None,
            None,
            false,
        )
        .expect("Cross-file inheritance should compile without errors");

        assert!(core_file.exists());
    }

    #[test]
    fn test_clean_stale_artifacts_removes_orphaned_beam() {
        let temp = TempDir::new().unwrap();
        let build_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create a stale .beam and .core from a previous build
        write_test_file(&build_dir.join("bt@old_pkg@counter.beam"), "stale");
        write_test_file(&build_dir.join("bt@old_pkg@counter.core"), "stale");
        // Create the current build's .beam
        let current = build_dir.join("bt@new_pkg@counter.beam");
        write_test_file(&current, "current");
        write_test_file(&build_dir.join("bt@new_pkg@counter.core"), "current");

        clean_stale_artifacts(&build_dir, std::slice::from_ref(&current), "new_pkg").unwrap();

        // Stale files should be removed
        assert!(!build_dir.join("bt@old_pkg@counter.beam").exists());
        assert!(!build_dir.join("bt@old_pkg@counter.core").exists());
        // Current files should remain
        assert!(current.exists());
        assert!(build_dir.join("bt@new_pkg@counter.core").exists());
    }

    #[test]
    fn test_clean_stale_artifacts_removes_old_app_file() {
        let temp = TempDir::new().unwrap();
        let build_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        write_test_file(&build_dir.join("old_name.app"), "stale");
        write_test_file(&build_dir.join("new_name.app"), "current");
        let beam = build_dir.join("bt@new_name@main.beam");
        write_test_file(&beam, "current");

        clean_stale_artifacts(&build_dir, &[beam], "new_name").unwrap();

        assert!(!build_dir.join("old_name.app").exists());
        assert!(build_dir.join("new_name.app").exists());
    }

    #[test]
    fn test_clean_stale_artifacts_preserves_non_bt_files() {
        let temp = TempDir::new().unwrap();
        let build_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Non-bt@ beam files (like OTP callback modules) should be preserved
        write_test_file(&build_dir.join("beamtalk_myapp_app.beam"), "callback");
        write_test_file(&build_dir.join("beamtalk_myapp_app.erl"), "source");
        let beam = build_dir.join("bt@myapp@main.beam");
        write_test_file(&beam, "current");

        clean_stale_artifacts(&build_dir, &[beam], "myapp").unwrap();

        assert!(build_dir.join("beamtalk_myapp_app.beam").exists());
        assert!(build_dir.join("beamtalk_myapp_app.erl").exists());
    }

    // ── BT-1682: Change detection tests ──────────────────────────────

    /// Helper: create `file_module_pairs` matching the format used by the build pipeline.
    fn make_pairs(
        source_files: &[Utf8PathBuf],
        build_dir: &Utf8Path,
    ) -> Vec<(Utf8PathBuf, String, Utf8PathBuf)> {
        source_files
            .iter()
            .map(|f| {
                let stem = f.file_stem().unwrap_or("unknown");
                let module_name = format!("bt@{stem}");
                let core_file = build_dir.join(format!("{module_name}.core"));
                (f.clone(), module_name, core_file)
            })
            .collect()
    }

    #[test]
    fn test_detect_changes_new_files_no_build_dir() {
        let temp = TempDir::new().unwrap();
        let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_dir = project.join("src");
        fs::create_dir_all(&src_dir).unwrap();
        write_test_file(&src_dir.join("counter.bt"), "counter := [0].");

        // Build dir doesn't exist yet — all files should be changed
        let build_dir = project.join("build");
        let source_files = vec![src_dir.join("counter.bt")];
        let pairs = make_pairs(&source_files, &build_dir);

        let result = detect_changes(&source_files, &build_dir, &pairs, false);
        assert_eq!(result.changed_files.len(), 1);
        assert!(result.unchanged_files.is_empty());
        assert!(result.orphaned_beam_files.is_empty());
    }

    #[test]
    fn test_detect_changes_no_beam_exists() {
        let temp = TempDir::new().unwrap();
        let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_dir = project.join("src");
        let build_dir = project.join("build");
        fs::create_dir_all(&src_dir).unwrap();
        fs::create_dir_all(&build_dir).unwrap();

        write_test_file(&src_dir.join("counter.bt"), "counter := [0].");

        let source_files = vec![src_dir.join("counter.bt")];
        let pairs = make_pairs(&source_files, &build_dir);

        let result = detect_changes(&source_files, &build_dir, &pairs, false);
        assert_eq!(
            result.changed_files.len(),
            1,
            "New file with no .beam should be changed"
        );
        assert!(result.unchanged_files.is_empty());
    }

    #[test]
    fn test_detect_changes_up_to_date() {
        let temp = TempDir::new().unwrap();
        let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_dir = project.join("src");
        let build_dir = project.join("build");
        fs::create_dir_all(&src_dir).unwrap();
        fs::create_dir_all(&build_dir).unwrap();

        // Create source FIRST (older mtime)
        write_test_file(&src_dir.join("counter.bt"), "counter := [0].");
        // Small delay to ensure mtime differs
        std::thread::sleep(std::time::Duration::from_millis(50));
        // Then create beam (newer mtime)
        write_test_file(&build_dir.join("bt@counter.beam"), "BEAM");

        let source_files = vec![src_dir.join("counter.bt")];
        let pairs = make_pairs(&source_files, &build_dir);

        let result = detect_changes(&source_files, &build_dir, &pairs, false);
        assert!(
            result.changed_files.is_empty(),
            "Up-to-date file should not be changed"
        );
        assert_eq!(result.unchanged_files.len(), 1);
    }

    #[test]
    fn test_detect_changes_source_modified() {
        let temp = TempDir::new().unwrap();
        let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_dir = project.join("src");
        let build_dir = project.join("build");
        fs::create_dir_all(&src_dir).unwrap();
        fs::create_dir_all(&build_dir).unwrap();

        // Create beam FIRST (older mtime)
        write_test_file(&build_dir.join("bt@counter.beam"), "BEAM");
        std::thread::sleep(std::time::Duration::from_millis(50));
        // Then modify source (newer mtime)
        write_test_file(&src_dir.join("counter.bt"), "counter := [1].");

        let source_files = vec![src_dir.join("counter.bt")];
        let pairs = make_pairs(&source_files, &build_dir);

        let result = detect_changes(&source_files, &build_dir, &pairs, false);
        assert_eq!(
            result.changed_files.len(),
            1,
            "Modified source should be changed"
        );
        assert!(result.unchanged_files.is_empty());
    }

    #[test]
    fn test_detect_changes_force_flag() {
        let temp = TempDir::new().unwrap();
        let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_dir = project.join("src");
        let build_dir = project.join("build");
        fs::create_dir_all(&src_dir).unwrap();
        fs::create_dir_all(&build_dir).unwrap();

        // Source older than beam — normally unchanged
        write_test_file(&src_dir.join("counter.bt"), "counter := [0].");
        std::thread::sleep(std::time::Duration::from_millis(50));
        write_test_file(&build_dir.join("bt@counter.beam"), "BEAM");

        let source_files = vec![src_dir.join("counter.bt")];
        let pairs = make_pairs(&source_files, &build_dir);

        let result = detect_changes(&source_files, &build_dir, &pairs, true);
        assert_eq!(
            result.changed_files.len(),
            1,
            "--force should mark all files as changed"
        );
        assert!(result.unchanged_files.is_empty());
    }

    #[test]
    fn test_detect_changes_orphaned_beam() {
        let temp = TempDir::new().unwrap();
        let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_dir = project.join("src");
        let build_dir = project.join("build");
        fs::create_dir_all(&src_dir).unwrap();
        fs::create_dir_all(&build_dir).unwrap();

        // Source file exists for counter but not for deleted_module
        write_test_file(&src_dir.join("counter.bt"), "counter := [0].");
        std::thread::sleep(std::time::Duration::from_millis(50));
        write_test_file(&build_dir.join("bt@counter.beam"), "BEAM");
        // Orphaned beam — no corresponding .bt
        write_test_file(&build_dir.join("bt@deleted_module.beam"), "BEAM");

        let source_files = vec![src_dir.join("counter.bt")];
        let pairs = make_pairs(&source_files, &build_dir);

        let result = detect_changes(&source_files, &build_dir, &pairs, false);
        assert_eq!(
            result.orphaned_beam_files.len(),
            1,
            "Should detect orphaned .beam"
        );
        assert!(
            result.orphaned_beam_files[0]
                .as_str()
                .contains("deleted_module"),
            "Orphaned file should be the deleted_module beam"
        );
    }

    #[test]
    fn test_detect_changes_mixed_states() {
        let temp = TempDir::new().unwrap();
        let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src_dir = project.join("src");
        let build_dir = project.join("build");
        fs::create_dir_all(&src_dir).unwrap();
        fs::create_dir_all(&build_dir).unwrap();

        // File 1: up-to-date (source older than beam)
        write_test_file(&src_dir.join("stable.bt"), "stable := [1].");
        std::thread::sleep(std::time::Duration::from_millis(50));
        write_test_file(&build_dir.join("bt@stable.beam"), "BEAM");

        // File 2: modified (beam older than source)
        write_test_file(&build_dir.join("bt@changed.beam"), "BEAM");
        std::thread::sleep(std::time::Duration::from_millis(50));
        write_test_file(&src_dir.join("changed.bt"), "changed := [2].");

        // File 3: new (no beam exists)
        write_test_file(&src_dir.join("new_file.bt"), "new_file := [3].");

        let source_files = vec![
            src_dir.join("changed.bt"),
            src_dir.join("new_file.bt"),
            src_dir.join("stable.bt"),
        ];
        let pairs = make_pairs(&source_files, &build_dir);

        let result = detect_changes(&source_files, &build_dir, &pairs, false);
        assert_eq!(
            result.changed_files.len(),
            2,
            "changed + new_file should need compilation"
        );
        assert_eq!(
            result.unchanged_files.len(),
            1,
            "stable should be unchanged"
        );
    }

    #[test]
    fn test_detect_changes_non_bt_beams_ignored() {
        let temp = TempDir::new().unwrap();
        let project = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let build_dir = project.join("build");
        fs::create_dir_all(&build_dir).unwrap();

        // Non-bt@ beam files should NOT appear as orphaned
        write_test_file(&build_dir.join("beamtalk_app.beam"), "callback");
        write_test_file(&build_dir.join("some_otp_module.beam"), "otp");

        let source_files: Vec<Utf8PathBuf> = Vec::new();
        let pairs: Vec<(Utf8PathBuf, String, Utf8PathBuf)> = Vec::new();

        let result = detect_changes(&source_files, &build_dir, &pairs, false);
        assert!(
            result.orphaned_beam_files.is_empty(),
            "Non-bt@ beam files should not be flagged as orphaned"
        );
    }

    // ---- ADR 0072 Phase 1: native Erlang compilation in build ----

    #[test]
    fn test_build_without_native_dir_is_unaffected() {
        // Packages without native/ should build exactly as before.
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");
        write_test_file(&src_path.join("main.bt"), "main := [42].");
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "[package]\nname = \"no_native\"\nversion = \"0.1.0\"\n",
        );

        let result = build(project_path.as_str(), &default_options(), false);

        if let Err(e) = result {
            let error_msg = format!("{e:?}");
            if error_msg.contains("escript not found") {
                eprintln!("Skipping test - escript not installed in CI environment");
                return;
            }
            panic!("Build failed with unexpected error: {e:?}");
        }

        // native ebin should NOT be created
        let native_ebin = project_path
            .join("_build")
            .join("dev")
            .join("native")
            .join("ebin");
        assert!(
            !native_ebin.exists(),
            "native ebin should not be created when no native/ directory exists"
        );
    }

    #[test]
    #[ignore = "requires erlc"]
    fn test_build_with_native_erlang() {
        // ADR 0072 Phase 1: build should discover and compile native/*.erl files
        let temp = TempDir::new().unwrap();
        let project_path = create_test_project(&temp);
        let src_path = project_path.join("src");

        // Create beamtalk.toml
        write_test_file(
            &project_path.join("beamtalk.toml"),
            "[package]\nname = \"with_native\"\nversion = \"0.1.0\"\n",
        );

        // Create a .bt source file
        write_test_file(&src_path.join("main.bt"), "main := [42].");

        // Create native/ with an Erlang module
        let native_dir = project_path.join("native");
        fs::create_dir(&native_dir).unwrap();
        write_test_file(
            &native_dir.join("hello_native.erl"),
            "-module(hello_native).\n-export([greet/0]).\ngreet() -> <<\"hello\">>.\n",
        );

        let result = build(project_path.as_str(), &default_options(), false);
        assert!(result.is_ok(), "Build should succeed: {result:?}");

        // Native BEAM file should exist in _build/dev/native/ebin/
        let native_beam = project_path
            .join("_build")
            .join("dev")
            .join("native")
            .join("ebin")
            .join("hello_native.beam");
        assert!(
            native_beam.exists(),
            "Native BEAM file should be produced at {native_beam}"
        );

        // Regular .bt BEAM should also exist
        let bt_beam = project_path
            .join("_build")
            .join("dev")
            .join("ebin")
            .join("bt@with_native@main.beam");
        assert!(
            bt_beam.exists(),
            "Beamtalk BEAM file should be produced at {bt_beam}"
        );
    }
}
