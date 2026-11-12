// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Per-file change detection and stale-artifact cleanup.
//!
//! Compares each `.bt` source file's content hash against the hash recorded
//! for its corresponding `.beam` output to decide which files need
//! recompilation, and removes `.beam`/`.core`/`.app` artifacts that no longer
//! correspond to a current source file or package name.

use camino::{Utf8Path, Utf8PathBuf};
use ecow::EcoString;
use miette::{Context, IntoDiagnostic, Result};
use std::collections::{HashMap, HashSet};
use std::fs;
use tracing::{debug, info, warn};

use super::surface_key::{SurfaceKeys, consulted_key, decode_surface, encode_surface};
use crate::commands::util::{content_hash_of, sha256_hex};
use beamtalk_core::semantic_analysis::consulted::ConsultedSurface;

/// Result of per-file change detection.
///
/// Compares each `.bt` source file's content hash against the hash recorded
/// for its corresponding `.beam` output to determine which files
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
    /// path string. Callers persist this via
    /// `build_cache::save_beam_hash_cache` after a successful build, so the
    /// next `detect_changes` call has something to compare against.
    pub source_hashes: HashMap<String, String>,
}

/// Re-key `source_file` in `source_hashes` after it was compiled this build:
/// its new cache key folds in the slice of the surface that compile just
/// consulted, and the consulted surface is stored for the next build's
/// decision (BT-4690). A no-op when the build has no per-name surface keys
/// (manifest-less builds).
pub(crate) fn record_compiled_surface(
    source_hashes: &mut HashMap<String, String>,
    edges: &BuildGraphEdges,
    source_file: &Utf8Path,
    surface: &ConsultedSurface,
) {
    if edges.surface_keys.is_none() {
        return;
    }
    let Some(own_hash) = source_hashes.get(&own_hash_key(source_file)).cloned() else {
        return;
    };
    let consulted = edges.consulted_part(Some(surface));
    let surface_part = consulted.as_deref().map_or(
        SurfacePart::Full(&edges.analysis_surface_hash),
        SurfacePart::Consulted,
    );
    let used_protocols = edges
        .file_protocol_uses
        .get(source_file)
        .map_or(&[][..], Vec::as_slice);
    let combined = combined_content_hash(
        &own_hash,
        used_protocols,
        &edges.protocol_hashes,
        surface_part,
        &edges.stub_inputs_hash,
    );
    source_hashes.insert(source_file.as_str().to_string(), combined);
    source_hashes.insert(consulted_key(source_file), encode_surface(surface));
}

/// Cross-file build-graph inputs to `detect_changes`' per-file cache key
/// (ADR 0127 §10a; BT-3591, BT-3674) — see its doc. `Default` is "no edges":
/// every file's key reduces to its own content hash.
#[derive(Debug, Default, Clone)]
pub(crate) struct BuildGraphEdges {
    /// Protocol names each file's classes `uses:`.
    pub file_protocol_uses: HashMap<Utf8PathBuf, Vec<EcoString>>,
    /// Content hash of each provision-bearing protocol, by bare name.
    pub protocol_hashes: HashMap<EcoString, String>,
    /// Hash of the cross-file analysis surface: every pre-loaded class,
    /// protocol and alias signature (empty = none; BT-3674, BT-4336).
    pub analysis_surface_hash: String,
    /// Hash of every FFI stub file the package's type registry was merged
    /// from, across all tiers (empty = no stubs; BT-4393) — see
    /// `stubs::ResolvedTypeRegistry::stub_inputs_hash`.
    pub stub_inputs_hash: String,
    /// Per-name hashes of the same surface (BT-4690). With these, a file whose
    /// last build stored a [`ConsultedSurface`] keys on just the slice of the
    /// surface it consulted; without them (or without a stored surface) it
    /// keys on `analysis_surface_hash`.
    pub surface_keys: Option<SurfaceKeys>,
}

/// The surface component of a file's cache key.
#[derive(Debug, Clone, Copy)]
enum SurfacePart<'a> {
    /// The package-wide `analysis_surface_hash` (empty = nothing cross-file).
    Full(&'a str),
    /// The hash of the slice of the surface the file consulted.
    Consulted(&'a str),
}

impl BuildGraphEdges {
    /// The surface component for a file that last consulted `stored`, or the
    /// full hash when it cannot be keyed on its consulted slice. Returns the
    /// consulted-slice text by value, so the caller owns it.
    fn consulted_part(&self, stored: Option<&ConsultedSurface>) -> Option<String> {
        self.surface_keys.as_ref()?.key_part(stored?)
    }
}

/// Log why a file whose own content is unchanged is being rebuilt. A file
/// with no protocols only moved because a package-wide input (the analysis
/// surface or the FFI stubs) changed (routine, `info!`); one that uses
/// protocols gets the ADR 0127 §10a warning.
fn log_rebuild_reason(source_file: &Utf8Path, used_protocols: &[EcoString]) {
    if used_protocols.is_empty() {
        info!(
            file = %source_file,
            "rebuilding '{source_file}': unchanged itself, but a class, protocol \
             or type-alias signature it may type-check against, or an FFI stub, \
             changed since it was last built"
        );
    } else {
        warn!(
            file = %source_file,
            protocols = ?used_protocols,
            "rebuilding '{source_file}': unchanged itself, but a protocol it uses \
             (one of {used_protocols:?}) or a class, protocol or type-alias \
             signature it may type-check against changed since it was last built"
        );
    }
}

/// Detect which source files have changed relative to their compiled `.beam` output.
///
/// For each `.bt` source file, computes the expected `.beam` filename in `build_dir`
/// using the same module naming scheme as the build pipeline. A file is considered
/// changed if:
/// - Its `.beam` does not exist (new file or first build)
/// - Its content hash differs from the hash recorded for it in the beam-hash
///   sidecar, including when no hash was recorded at all
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
/// hashed every source file's content this same build, so a hit
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
///
/// `edges.file_protocol_uses`/`edges.protocol_hashes` are ADR 0127 §10a's (BT-3591)
/// build-graph edge: a class's `uses:` line makes its file's cache key
/// depend on that protocol's content too, not just the file's own. For each
/// source file, [`combined_content_hash`] folds the hashes of every
/// provision-bearing protocol its classes `uses:` (bare name;
/// `file_protocol_uses` — built by the caller from the same parsed `Module`s
/// Pass 1 already has — resolves a package-qualified `uses: pkg@Name` under
/// its bare `Name`, mirroring `trait_expansion`'s own resolution) into that
/// file's stored/compared hash, so editing a protocol changes its users'
/// cache keys and rebuilds them, even when their own source is untouched.
/// `edges.analysis_surface_hash` (BT-3674, generalised in BT-4336) covers
/// every *other* cross-file input: a file type-checks against the
/// `ClassInfo` of classes in other files and dependencies (trait provisions
/// flattened in), and against their protocols and type aliases. Changing a
/// return type, dropping a method or renaming a provision changes that
/// surface without touching the caller's own source, so the package-wide
/// hash of it is folded into every file's key — otherwise the caller's
/// `.beam` and cached diagnostics would be replayed against the old
/// signature and `build` would pass code a REPL `load-project` rejects.
/// Cost: a signature change anywhere rebuilds the whole package (not just
/// referencing files); edits that leave every signature equal (method
/// bodies, doc comments, line shifts) keep it stable.
///
/// `edges.stub_inputs_hash` (BT-4393) does the same for the FFI stub tiers
/// (project-local `stubs/`, package-bundled, distribution): every file
/// type-checks its FFI calls against the registry merged from them, so a
/// stub add/edit/remove rebuilds the whole package. A package without stubs
/// has an empty hash and pays nothing.
///
/// Both are empty for a caller with no protocol data to offer (manifest-less
/// builds, `--stdlib-mode`'s own separate pre-pass, tests), in which case
/// every file's combined hash reduces to its own content hash — unchanged
/// behaviour.
#[allow(clippy::too_many_lines)] // one linear pass: hash, key, then decide
pub(crate) fn detect_changes(
    source_files: &[Utf8PathBuf],
    build_dir: &Utf8Path,
    file_module_pairs: &[(Utf8PathBuf, String, Utf8PathBuf)],
    force: bool,
    known_hashes: &HashMap<String, String>,
    edges: &BuildGraphEdges,
) -> ChangeDetectionResult {
    let BuildGraphEdges {
        file_protocol_uses,
        protocol_hashes,
        analysis_surface_hash,
        stub_inputs_hash,
        surface_keys: _,
    } = edges;
    // Content hash of each source file as of the last successful
    // `.beam` build, keyed by path string. Also carries each file's
    // own-content-only hash under a synthetic `<path>\0own` key (never a
    // real path — `\0` cannot appear in one), stashed in the very same
    // sidecar so a protocol-driven cache-key change can be told apart from
    // an own-content change on the *next* build without a second cache file
    // (see the rebuild-reason `warn!` below).
    let previous_hashes = crate::commands::build_cache::load_beam_hash_cache(build_dir);

    // Hash every source file up front — both to decide staleness below and
    // to hand back to the caller for persisting after a successful build.
    // Reuse Pass 1's hash when we already have one (see `known_hashes`'s doc)
    // rather than reading and hashing the file's content again.
    let mut source_hashes: HashMap<String, String> = HashMap::new();
    let empty_uses: Vec<EcoString> = Vec::new();
    for (source_file, _module_name, _core_file) in file_module_pairs {
        let Some(own_hash) = known_hashes
            .get(source_file.as_str())
            .cloned()
            .or_else(|| content_hash_of(source_file))
        else {
            continue;
        };
        let used_protocols = file_protocol_uses.get(source_file).unwrap_or(&empty_uses);
        // BT-4690: key on the slice of the surface the file consulted when it
        // was last built; no stored surface means the full package-wide hash.
        let stored_text = previous_hashes.get(&consulted_key(source_file));
        let stored = stored_text.and_then(|text| decode_surface(text));
        let consulted = edges.consulted_part(stored.as_ref());
        let surface_part = consulted.as_deref().map_or(
            SurfacePart::Full(analysis_surface_hash),
            SurfacePart::Consulted,
        );
        let combined_hash = combined_content_hash(
            &own_hash,
            used_protocols,
            protocol_hashes,
            surface_part,
            stub_inputs_hash,
        );

        // A protocol-driven rebuild (own content unchanged, but the
        // combined key differs because a used protocol's hash changed) is
        // exactly what ADR 0127 §10a asks `beamtalk build` to warn about —
        // surfaced here, at the point the distinction is cheaply knowable,
        // rather than left indistinguishable from an ordinary content edit.
        if combined_hash != own_hash
            && previous_hashes.get(&own_hash_key(source_file)) == Some(&own_hash)
            && previous_hashes.get(source_file.as_str()) != Some(&combined_hash)
        {
            log_rebuild_reason(source_file, used_protocols);
        }

        source_hashes.insert(source_file.as_str().to_string(), combined_hash);
        source_hashes.insert(own_hash_key(source_file), own_hash);
        // Carry the stored surface forward: a skipped file keeps it, a rebuilt
        // one is overwritten by `record_compiled_surface`.
        if let (Some(text), Some(_)) = (stored_text, &stored) {
            source_hashes.insert(consulted_key(source_file), text.clone());
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

/// The synthetic sidecar key stashing `source_file`'s own-content-only hash
/// (as opposed to its combined-with-protocols hash, stored under the file's
/// own path) — see `detect_changes`'s doc for why this shares the existing
/// beam-hash sidecar rather than needing a second cache file. `\0` can never
/// appear in a real path, so this can never collide with one.
fn own_hash_key(source_file: &Utf8Path) -> String {
    format!("{source_file}\u{0}own")
}

/// Folds `used_protocols`' hashes into `own_hash` to produce the combined
/// cache key a source file's classes depend on (ADR 0127 §10a; BT-3591) —
/// see `detect_changes`'s doc. A protocol name with no entry in
/// `protocol_hashes` (not provision-bearing, or not resolved by any
/// currently-known compile path) contributes nothing, matching
/// `trait_expansion::expand_module`'s own "protocols with no provisions need
/// no AST" distinction. Sorted before folding so the combined hash is
/// independent of `used_protocols`' original (parse) order.
fn combined_content_hash(
    own_hash: &str,
    used_protocols: &[EcoString],
    protocol_hashes: &HashMap<EcoString, String>,
    surface: SurfacePart<'_>,
    stub_inputs_hash: &str,
) -> String {
    let mut protocol_hash_parts: Vec<&str> = used_protocols
        .iter()
        .filter_map(|name| protocol_hashes.get(name))
        .map(String::as_str)
        .collect();
    let surface_is_empty = matches!(surface, SurfacePart::Full(hash) if hash.is_empty());
    if protocol_hash_parts.is_empty() && surface_is_empty && stub_inputs_hash.is_empty() {
        return own_hash.to_string();
    }
    protocol_hash_parts.sort_unstable();

    let mut buf = String::from(own_hash);
    for part in protocol_hash_parts {
        buf.push('\n');
        buf.push_str(part);
    }
    match surface {
        SurfacePart::Full(hash) if !hash.is_empty() => {
            buf.push_str("\nsurface:");
            buf.push_str(hash);
        }
        SurfacePart::Full(_) => {}
        SurfacePart::Consulted(hash) => {
            buf.push_str("\nconsulted:");
            buf.push_str(hash);
        }
    }
    if !stub_inputs_hash.is_empty() {
        buf.push_str("\nstubs:");
        buf.push_str(stub_inputs_hash);
    }
    sha256_hex(buf.as_bytes())
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
