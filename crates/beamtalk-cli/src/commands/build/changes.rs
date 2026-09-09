// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Per-file change detection and stale-artifact cleanup.
//!
//! Compares each `.bt` source file's content hash against the hash recorded
//! for its corresponding `.beam` output (BT-3120) to decide which files need
//! recompilation, and removes `.beam`/`.core`/`.app` artifacts that no longer
//! correspond to a current source file or package name.

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::collections::{HashMap, HashSet};
use std::fs;
use tracing::{debug, info};

use crate::commands::util::content_hash_of;

/// Result of per-file change detection.
///
/// Compares each `.bt` source file's content hash against the hash recorded
/// for its corresponding `.beam` output (BT-3120) to determine which files
/// need recompilation.
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
    /// Content hash of every source file considered in this pass, keyed by
    /// path string (BT-3120). Callers persist this via
    /// `build_cache::save_beam_hash_cache` after a successful build, so the
    /// next `detect_changes` call has something to compare against.
    pub source_hashes: HashMap<String, String>,
}

/// Detect which source files have changed relative to their compiled `.beam` output.
///
/// For each `.bt` source file, computes the expected `.beam` filename in `build_dir`
/// using the same module naming scheme as the build pipeline. A file is considered
/// changed if:
/// - Its `.beam` does not exist (new file or first build)
/// - Its content hash differs from the hash recorded for it in the beam-hash
///   sidecar (BT-3120), including when no hash was recorded at all
///
/// mtime is deliberately not used for this decision: it lies under git
/// operations (branch switches restore old content under a fresh mtime) and
/// under tools that preserve or backdate mtimes on write, either of which can
/// make an mtime-keyed check serve a stale `.beam`.
///
/// Also detects orphaned `.beam` files (no corresponding `.bt` source) and reports
/// them as warnings.
///
/// When `force` is true, all source files are treated as changed regardless of
/// their recorded hash.
///
/// `known_hashes` is an optional set of already-computed content hashes,
/// keyed by path string — for a manifest-based package build, `build_rs`'s
/// Pass 1 (`build_cache::incremental_build_class_module_index`) has already
/// hashed every source file's content this same build (BT-3120), so a hit
/// here skips reading and hashing that file's content a second time. A miss
/// (manifest-less builds, where Pass 1 never runs; or any file Pass 1
/// couldn't read) falls back to hashing it directly here, same as before.
/// `known_hashes` is mainly an optimization, but not purely one: a file
/// edited in the window between Pass 1 hashing it and this call reuses the
/// now-stale Pass-1-time hash for this build cycle. That's narrow and
/// self-healing (Pass 1 always re-reads from disk on the next build), never
/// a permanent stale skip, but it means a same-build edit isn't caught as
/// immediately as the old mtime-based check (which re-read mtime fresh at
/// this call site).
pub(crate) fn detect_changes(
    source_files: &[Utf8PathBuf],
    build_dir: &Utf8Path,
    file_module_pairs: &[(Utf8PathBuf, String, Utf8PathBuf)],
    force: bool,
    known_hashes: &HashMap<String, String>,
) -> ChangeDetectionResult {
    // BT-3120: content hash of each source file as of the last successful
    // `.beam` build, keyed by path string.
    let previous_hashes = crate::commands::build_cache::load_beam_hash_cache(build_dir);

    // Hash every source file up front — both to decide staleness below and
    // to hand back to the caller for persisting after a successful build.
    // Reuse Pass 1's hash when we already have one (see `known_hashes`'s doc)
    // rather than reading and hashing the file's content again.
    let mut source_hashes: HashMap<String, String> = HashMap::new();
    for (source_file, _module_name, _core_file) in file_module_pairs {
        if let Some(hash) = known_hashes
            .get(source_file.as_str())
            .cloned()
            .or_else(|| content_hash_of(source_file))
        {
            source_hashes.insert(source_file.as_str().to_string(), hash);
        }
    }

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
            source_hashes,
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

        // Compare content hashes: no hash, or a hash mismatch, needs recompilation.
        match source_hashes.get(source_file.as_str()) {
            Some(current_hash)
                if previous_hashes.get(source_file.as_str()) == Some(current_hash) =>
            {
                debug!(file = %source_file, "Up-to-date (content hash unchanged)");
                unchanged_files.push(source_file.clone());
            }
            Some(_) => {
                debug!(file = %source_file, "Content hash changed — needs recompilation");
                changed_files.push(source_file.clone());
            }
            None => {
                // If we can't read the source, err on the side of recompilation.
                debug!(file = %source_file, "Cannot read source — needs recompilation");
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
        source_hashes,
    }
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

/// Remove stale build artifacts from the build directory.
///
/// After a successful build, compares the set of `.beam` files just produced
/// against all `bt@*.beam` files on disk. Any on-disk file not in the produced
/// set is stale (from a deleted source file, renamed class, or package rename)
/// and is removed along with its corresponding `.core` file.
///
/// Also removes `.app` files that don't match the current package name (from
/// package renames).
pub(crate) fn clean_stale_artifacts(
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
