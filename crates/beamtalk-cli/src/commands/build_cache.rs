// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Persistent metadata cache for incremental Pass 1 builds (BT-1683).
//!
//! Serialises the class indexes and `ClassInfo` vectors that Pass 1 produces so
//! that unchanged files can be skipped on the next build. The cache stores
//! per-file metadata keyed by source path, together with a SHA-256 hash of the
//! file's contents so staleness can be detected correctly (BT-3120) — mtime is
//! not used for this decision because it lies under git operations (branch
//! switches restore old content under a fresh mtime) and under tools that
//! preserve or backdate mtimes on write, either of which can make an
//! mtime-keyed cache serve stale (incorrect) results.
//!
//! The cache is invalidated entirely when `beamtalk.toml` changes (mtime check
//! — the manifest itself isn't content-hashed, only source files are) or when
//! `--force` is used.
//!
//! This module also owns the sibling "beam hash" sidecar
//! ([`load_beam_hash_cache`]/[`save_beam_hash_cache`]) that backs
//! `build.rs`'s `detect_changes`: the content hash of each source file as of
//! its last successful `.beam` compilation. It's a separate cache from
//! [`Pass1Cache`] because `detect_changes` runs for every build — including
//! manifest-less single-file builds, where `Pass1Cache` never applies.

use beamtalk_core::compilation::extension_index::{
    ExtensionIndex, ExtensionKey, ExtensionLocation,
};
use beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo;
use camino::{Utf8Path, Utf8PathBuf};
use miette::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::time::SystemTime;
use tracing::{debug, info, warn};

#[cfg(test)]
use super::util::content_hash_of;
use super::util::{content_hashes_of, mtime_of};

/// Name of the cache file stored inside the build directory.
const CACHE_FILENAME: &str = ".beamtalk-pass1-cache.json";

/// Current cache format version. Bump when the serialised layout changes.
/// v2: `ClassInfo` gained `surface_incomplete` (BT-2796); entries gained
/// per-file extension definitions (BT-2795).
/// v3: `CacheEntry.mtime` replaced by `CacheEntry.content_hash` — staleness
/// is now keyed on file content, not filesystem mtime (BT-3120).
const CACHE_VERSION: u32 = 3;

/// On-disk representation of the Pass 1 metadata cache.
///
/// File paths are stored as `String` keys (not `Utf8PathBuf`) because camino
/// does not have serde enabled in the workspace, and adding it for just this
/// cache would be an unnecessarily broad change.
#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct Pass1Cache {
    /// Format version — if this doesn't match `CACHE_VERSION` the cache is
    /// discarded silently.
    version: u32,

    /// Modification time of `beamtalk.toml` when the cache was written.
    /// If the manifest has changed since, the entire cache is invalidated.
    #[serde(with = "system_time_serde")]
    manifest_mtime: Option<SystemTime>,

    /// Per-file metadata entries, keyed by the canonical source path (as string).
    entries: HashMap<String, CacheEntry>,
}

/// Cached metadata for a single source file.
#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct CacheEntry {
    /// SHA-256 content hash of the source file when this entry was recorded
    /// (BT-3120). The source of truth for staleness — see the module doc for
    /// why mtime isn't used.
    content_hash: String,

    /// Classes defined in this file, mapped to their compiled module name.
    /// E.g. `"Counter" → "bt@my_app@counter"`.
    class_module_index: HashMap<String, String>,

    /// Superclass relationships for classes in this file.
    /// E.g. `"MyChild" → "MyParent"`.
    class_superclass_index: HashMap<String, String>,

    /// Full `ClassInfo` entries extracted from this file.
    class_infos: Vec<ClassInfo>,

    /// Standalone extension definitions in this file (BT-2795),
    /// keyed by `(class, side, selector)`.
    #[serde(default)]
    extensions: Vec<(ExtensionKey, Vec<ExtensionLocation>)>,
}

/// Result of loading the cache and merging with fresh data.
pub(crate) struct IncrementalPass1Result {
    /// Merged class → module index (cached + freshly-scanned).
    pub class_module_index: HashMap<String, String>,
    /// Merged class → superclass index.
    pub class_superclass_index: HashMap<String, String>,
    /// Merged `ClassInfo` vector.
    pub all_class_infos: Vec<ClassInfo>,
    /// Merged project-wide extension index (BT-2795).
    pub extension_index: ExtensionIndex,
    /// Cached ASTs for files that were re-scanned in this build.
    pub cached_asts: HashMap<Utf8PathBuf, super::build::CachedAst>,
    /// Whether the manifest changed and forced a full cache invalidation.
    /// When true, Pass 2 should also force-recompile all files.
    pub manifest_invalidated: bool,
    /// Content hash of every file in `source_files`, keyed by path string
    /// (BT-3120) — computed once for this Pass 1 pass and handed back so
    /// Pass 2's `detect_changes` can reuse them instead of re-hashing every
    /// file's content a second time. See [`super::util::content_hashes_of`].
    pub source_hashes: HashMap<String, String>,
}

/// Result of loading the cache — distinguishes "no cache" from "manifest invalidated".
enum CacheLoadResult {
    /// Cache loaded successfully.
    Hit(Pass1Cache),
    /// No cache found, or cache was corrupt/version-mismatched.
    Miss,
    /// Cache existed but manifest changed — all files must be recompiled.
    ManifestInvalidated,
}

/// Load the Pass 1 cache from `build_dir`, if it exists and is valid.
///
/// Returns `Miss` if the cache doesn't exist, has a version mismatch,
/// or is corrupt. Returns `ManifestInvalidated` if the manifest has changed.
fn load_cache(build_dir: &Utf8Path, manifest_path: Option<&Utf8Path>) -> CacheLoadResult {
    let cache_path = build_dir.join(CACHE_FILENAME);
    let Ok(data) = fs::read_to_string(&cache_path) else {
        debug!("No Pass 1 cache found at {cache_path}");
        return CacheLoadResult::Miss;
    };

    let Ok(cache) = serde_json::from_str::<Pass1Cache>(&data) else {
        warn!("Pass 1 cache is corrupt or unreadable — rebuilding");
        return CacheLoadResult::Miss;
    };

    // Version check
    if cache.version != CACHE_VERSION {
        info!(
            cached = cache.version,
            current = CACHE_VERSION,
            "Pass 1 cache version mismatch — rebuilding"
        );
        return CacheLoadResult::Miss;
    }

    // Manifest mtime check: if beamtalk.toml has changed, invalidate everything
    if let Some(mp) = manifest_path {
        let current_mtime = mtime_of(mp);
        if current_mtime != cache.manifest_mtime {
            info!("beamtalk.toml has changed — invalidating Pass 1 cache");
            return CacheLoadResult::ManifestInvalidated;
        }
    } else if cache.manifest_mtime.is_some() {
        // Cache was written with a manifest that no longer exists
        info!("beamtalk.toml removed — invalidating Pass 1 cache");
        return CacheLoadResult::ManifestInvalidated;
    }

    debug!(
        entries = cache.entries.len(),
        "Loaded Pass 1 cache from {cache_path}"
    );
    CacheLoadResult::Hit(cache)
}

/// Save the Pass 1 cache to `build_dir`.
///
/// Cache persistence is best-effort — serialization or I/O failures are logged
/// as warnings but never fail the build.
fn save_cache(
    build_dir: &Utf8Path,
    manifest_path: Option<&Utf8Path>,
    file_entries: HashMap<String, CacheEntry>,
) {
    let manifest_mtime = manifest_path.and_then(mtime_of);

    let cache = Pass1Cache {
        version: CACHE_VERSION,
        manifest_mtime,
        entries: file_entries,
    };

    let cache_path = build_dir.join(CACHE_FILENAME);
    let data = match serde_json::to_string(&cache) {
        Ok(d) => d,
        Err(e) => {
            warn!(error = %e, "Failed to serialise Pass 1 cache");
            return;
        }
    };

    if let Err(e) = fs::write(&cache_path, &data) {
        warn!(error = %e, "Failed to write Pass 1 cache to {cache_path}");
        return;
    }

    debug!(
        entries = cache.entries.len(),
        "Saved Pass 1 cache to {cache_path}"
    );
}

/// Discard the Pass 1 metadata cache from `build_dir` (ADR 0098).
///
/// Called on a project-scope provenance miss. A forced rebuild already ignores
/// and overwrites the cache, but the cached `ClassInfo` / class-index entries
/// are compiler-*derived* from the source AST and just as version-sensitive as
/// the `.beam` output, so we remove the stale file outright rather than trust
/// that the rebuild reaches every entry. Best-effort: a removal failure is
/// logged, never fatal.
pub(crate) fn discard_pass1_cache(build_dir: &Utf8Path) {
    let cache_path = build_dir.join(CACHE_FILENAME);
    match fs::remove_file(&cache_path) {
        Ok(()) => debug!("Discarded Pass 1 cache at {cache_path} (provenance miss)"),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
        Err(e) => warn!(error = %e, "Failed to discard Pass 1 cache at {cache_path}"),
    }
}

/// Name of the sidecar file recording, for each source file, the content
/// hash that produced its currently-compiled `.beam` (BT-3120).
const BEAM_HASH_CACHE_FILENAME: &str = ".beamtalk-beam-hashes.json";

/// Format version for the beam-hash sidecar. Bump when the layout changes.
const BEAM_HASH_CACHE_VERSION: u32 = 1;

/// On-disk representation of the beam-hash sidecar.
#[derive(Debug, Default, Serialize, Deserialize)]
struct BeamHashCache {
    /// Format version — if this doesn't match `BEAM_HASH_CACHE_VERSION` the
    /// cache is discarded silently (every file is then treated as changed
    /// until the next successful build repopulates it).
    version: u32,
    /// Source path (as string) → content hash of the source that produced
    /// the `.beam` currently on disk for it.
    hashes: HashMap<String, String>,
}

/// Load the beam-hash sidecar for `build_dir`.
///
/// Returns an empty map on any miss — no file, corrupt JSON, or a version
/// mismatch. `detect_changes` treats a file with no recorded hash as changed,
/// so a miss just costs one extra rebuild per affected file, never a stale
/// skip.
pub(crate) fn load_beam_hash_cache(build_dir: &Utf8Path) -> HashMap<String, String> {
    let cache_path = build_dir.join(BEAM_HASH_CACHE_FILENAME);
    let Ok(data) = fs::read_to_string(&cache_path) else {
        debug!("No beam-hash cache found at {cache_path}");
        return HashMap::new();
    };

    let Ok(cache) = serde_json::from_str::<BeamHashCache>(&data) else {
        warn!("Beam-hash cache is corrupt or unreadable — treating all files as changed");
        return HashMap::new();
    };

    if cache.version != BEAM_HASH_CACHE_VERSION {
        info!(
            cached = cache.version,
            current = BEAM_HASH_CACHE_VERSION,
            "Beam-hash cache version mismatch — treating all files as changed"
        );
        return HashMap::new();
    }

    debug!(
        entries = cache.hashes.len(),
        "Loaded beam-hash cache from {cache_path}"
    );
    cache.hashes
}

/// Save the beam-hash sidecar to `build_dir`.
///
/// Call only after a successful `.beam` compile — the saved hashes assert
/// "this content produced the `.beam` now on disk". Best-effort like
/// [`save_cache`]: serialization or I/O failures are logged, never fatal.
pub(crate) fn save_beam_hash_cache(build_dir: &Utf8Path, hashes: &HashMap<String, String>) {
    let cache = BeamHashCache {
        version: BEAM_HASH_CACHE_VERSION,
        hashes: hashes.clone(),
    };

    let cache_path = build_dir.join(BEAM_HASH_CACHE_FILENAME);
    let data = match serde_json::to_string(&cache) {
        Ok(d) => d,
        Err(e) => {
            warn!(error = %e, "Failed to serialise beam-hash cache");
            return;
        }
    };

    if let Err(e) = fs::write(&cache_path, &data) {
        warn!(error = %e, "Failed to write beam-hash cache to {cache_path}");
        return;
    }

    debug!(
        entries = cache.hashes.len(),
        "Saved beam-hash cache to {cache_path}"
    );
}

/// Name of the sidecar file recording, for each source file, the diagnostics
/// produced the last time it was actually compiled (BT-3410).
const DIAGNOSTICS_CACHE_FILENAME: &str = ".beamtalk-diagnostics-cache.json";

/// Format version for the diagnostics sidecar. Bump when the layout changes.
const DIAGNOSTICS_CACHE_VERSION: u32 = 1;

/// On-disk representation of the diagnostics sidecar.
#[derive(Debug, Default, Serialize, Deserialize)]
struct DiagnosticsCache {
    /// Format version — if this doesn't match `DIAGNOSTICS_CACHE_VERSION` the
    /// cache is discarded silently (every unchanged file then has no cache
    /// entry, which `build.rs`'s incremental-skip path treats as a signal to
    /// recompile it this build — see `load_diagnostics_cache`'s doc).
    version: u32,
    /// Source path (as string) → the diagnostics produced compiling it, as of
    /// the content hash recorded for it in the beam-hash sidecar. Kept as a
    /// separate file (rather than folded into `BeamHashCache`) so a build
    /// that only touches hashes never has to rewrite every file's
    /// diagnostics, and vice versa.
    diagnostics: HashMap<String, Vec<beamtalk_core::source_analysis::Diagnostic>>,
}

/// Load the diagnostics sidecar for `build_dir` (BT-3410).
///
/// Returns an empty map on any miss — no file, corrupt JSON, or a version
/// mismatch. Unlike the beam-hash sidecar (where a miss means "assume
/// changed"), a missing diagnostics entry for an otherwise-unchanged file is
/// not silently treated as "no diagnostics": `build.rs`'s incremental-skip
/// path looks the file up in this map, and on a miss falls back to actually
/// recompiling that one file this build (its `.beam` is untouched — only its
/// diagnostics were unknown) rather than reporting nothing for it. So a
/// version bump, corruption, or a fresh sidecar right after upgrading to
/// this feature self-heals within the *same* build, not a later one — every
/// file's diagnostics are known by the time this build finishes, and the
/// sidecar it writes back out (rebuilt from scratch, never mutated in
/// place) reflects that.
pub(crate) fn load_diagnostics_cache(
    build_dir: &Utf8Path,
) -> HashMap<String, Vec<beamtalk_core::source_analysis::Diagnostic>> {
    let cache_path = build_dir.join(DIAGNOSTICS_CACHE_FILENAME);
    let Ok(data) = fs::read_to_string(&cache_path) else {
        debug!("No diagnostics cache found at {cache_path}");
        return HashMap::new();
    };

    let Ok(cache) = serde_json::from_str::<DiagnosticsCache>(&data) else {
        warn!("Diagnostics cache is corrupt or unreadable — treating all files as having none");
        return HashMap::new();
    };

    if cache.version != DIAGNOSTICS_CACHE_VERSION {
        info!(
            cached = cache.version,
            current = DIAGNOSTICS_CACHE_VERSION,
            "Diagnostics cache version mismatch — treating all files as having none"
        );
        return HashMap::new();
    }

    debug!(
        entries = cache.diagnostics.len(),
        "Loaded diagnostics cache from {cache_path}"
    );
    cache.diagnostics
}

/// Save the diagnostics sidecar to `build_dir` (BT-3410).
///
/// Call with the diagnostics for every file in this build's `file_module_pairs`
/// (both freshly compiled and unchanged-and-replayed) so a file removed from
/// the project — or one that stops producing a diagnostic — doesn't leave a
/// stale entry behind. Best-effort like [`save_beam_hash_cache`]:
/// serialization or I/O failures are logged, never fatal.
pub(crate) fn save_diagnostics_cache(
    build_dir: &Utf8Path,
    diagnostics: &HashMap<String, Vec<beamtalk_core::source_analysis::Diagnostic>>,
) {
    let cache = DiagnosticsCache {
        version: DIAGNOSTICS_CACHE_VERSION,
        diagnostics: diagnostics.clone(),
    };

    let cache_path = build_dir.join(DIAGNOSTICS_CACHE_FILENAME);
    let data = match serde_json::to_string(&cache) {
        Ok(d) => d,
        Err(e) => {
            warn!(error = %e, "Failed to serialise diagnostics cache");
            return;
        }
    };

    if let Err(e) = fs::write(&cache_path, &data) {
        warn!(error = %e, "Failed to write diagnostics cache to {cache_path}");
        return;
    }

    debug!(
        entries = cache.diagnostics.len(),
        "Saved diagnostics cache to {cache_path}"
    );
}

/// Perform an incremental Pass 1 scan.
///
/// Loads the existing cache (if any), determines which files are stale,
/// re-scans only those, and merges the results. Returns the merged indexes
/// and the set of files that were actually re-scanned.
///
/// When `force` is true, the cache is ignored entirely.
#[allow(clippy::too_many_lines)] // linear merge pipeline — split adds indirection, not clarity
pub(crate) fn incremental_build_class_module_index(
    source_files: &[Utf8PathBuf],
    source_root: Option<&Utf8Path>,
    pkg_name: &str,
    build_dir: &Utf8Path,
    manifest_path: Option<&Utf8Path>,
    force: bool,
) -> Result<IncrementalPass1Result> {
    // BT-3120: hash every source file's content exactly once for this Pass 1
    // pass. `partition_files` (staleness) and `build_cache_entries` (the
    // updated cache) both need every file's hash; computing it once here and
    // passing it into both — instead of each calling `content_hash_of`
    // independently — halves Pass 1's own content-hashing work, and the
    // result is handed back to the caller so Pass 2 doesn't hash a third
    // time. See `content_hashes_of`'s doc for why this matters.
    let source_hashes = content_hashes_of(source_files);

    // If forced, skip cache entirely
    if force {
        info!("Force build — ignoring Pass 1 cache");
        let (
            class_module_index,
            class_superclass_index,
            all_class_infos,
            extension_index,
            cached_asts,
        ) = super::build::build_class_module_index(source_files, source_root, pkg_name)?;

        // Build cache entries for saving later
        let file_entries = build_cache_entries(
            source_files,
            source_root,
            pkg_name,
            &Pass1Indexes {
                class_module_index: &class_module_index,
                class_superclass_index: &class_superclass_index,
                all_class_infos: &all_class_infos,
                extension_index: &extension_index,
            },
            &source_hashes,
        );
        save_cache(build_dir, manifest_path, file_entries);

        return Ok(IncrementalPass1Result {
            class_module_index,
            class_superclass_index,
            all_class_infos,
            extension_index,
            cached_asts,
            manifest_invalidated: false,
            source_hashes,
        });
    }

    let cache_result = load_cache(build_dir, manifest_path);
    let manifest_invalidated = matches!(cache_result, CacheLoadResult::ManifestInvalidated);

    // Extract cache if we got a hit
    let cache = match cache_result {
        CacheLoadResult::Hit(c) => Some(c),
        _ => None,
    };

    // Determine which files need re-scanning
    let (stale_files, fresh_files) = match &cache {
        Some(c) => partition_files(source_files, c, &source_hashes),
        None => (source_files.to_vec(), Vec::new()),
    };

    if stale_files.is_empty() {
        info!(
            "Pass 1 cache hit — all {} files up-to-date",
            source_files.len()
        );
    } else {
        info!(
            stale = stale_files.len(),
            cached = fresh_files.len(),
            "Incremental Pass 1: re-scanning {} of {} files",
            stale_files.len(),
            source_files.len()
        );
    }

    // Collect cached data for fresh files
    let mut class_module_index = HashMap::new();
    let mut class_superclass_index = HashMap::new();
    let mut all_class_infos = Vec::new();
    let mut extension_index = ExtensionIndex::new();

    if let Some(ref c) = cache {
        for file in &fresh_files {
            if let Some(entry) = c.entries.get(file.as_str()) {
                for (class_name, module_name) in &entry.class_module_index {
                    class_module_index.insert(class_name.clone(), module_name.clone());
                }
                for (class_name, superclass) in &entry.class_superclass_index {
                    class_superclass_index.insert(class_name.clone(), superclass.clone());
                }
                all_class_infos.extend(entry.class_infos.clone());
                extension_index.add_entries(entry.extensions.iter().cloned());
            }
        }
    }

    // Re-scan stale files
    let (
        stale_module_index,
        stale_superclass_index,
        stale_class_infos,
        stale_extensions,
        cached_asts,
    ) = if stale_files.is_empty() {
        (
            HashMap::new(),
            HashMap::new(),
            Vec::new(),
            ExtensionIndex::new(),
            HashMap::new(),
        )
    } else {
        super::build::build_class_module_index(&stale_files, source_root, pkg_name)?
    };

    // Merge stale results
    class_module_index.extend(stale_module_index);
    class_superclass_index.extend(stale_superclass_index);
    all_class_infos.extend(stale_class_infos);
    extension_index.merge(&stale_extensions);

    // Build updated cache entries and save
    let file_entries = build_cache_entries(
        source_files,
        source_root,
        pkg_name,
        &Pass1Indexes {
            class_module_index: &class_module_index,
            class_superclass_index: &class_superclass_index,
            all_class_infos: &all_class_infos,
            extension_index: &extension_index,
        },
        &source_hashes,
    );
    save_cache(build_dir, manifest_path, file_entries);

    Ok(IncrementalPass1Result {
        class_module_index,
        class_superclass_index,
        all_class_infos,
        extension_index,
        cached_asts,
        manifest_invalidated,
        source_hashes,
    })
}

/// Partition source files into stale (need re-scan) and fresh (cache hit).
///
/// `hashes` is `source_files`' content hashes, keyed by path string —
/// precomputed once by the caller via [`content_hashes_of`] rather than
/// hashed again per file here (BT-3120).
///
/// A file is considered stale if:
/// - It has no cache entry
/// - Its content hash differs from the cached content hash (BT-3120)
/// - Its content cannot be read (err on the side of re-scanning)
fn partition_files(
    source_files: &[Utf8PathBuf],
    cache: &Pass1Cache,
    hashes: &HashMap<String, String>,
) -> (Vec<Utf8PathBuf>, Vec<Utf8PathBuf>) {
    let mut stale = Vec::new();
    let mut fresh = Vec::new();

    for file in source_files {
        if let Some(entry) = cache.entries.get(file.as_str()) {
            match hashes.get(file.as_str()) {
                Some(hash) if *hash == entry.content_hash => fresh.push(file.clone()),
                Some(_) => {
                    debug!(file = %file, "Cache stale — content changed");
                    stale.push(file.clone());
                }
                None => {
                    debug!(file = %file, "Cannot read file content — treating as stale");
                    stale.push(file.clone());
                }
            }
        } else {
            debug!(file = %file, "No cache entry — new file");
            stale.push(file.clone());
        }
    }

    // Also check for files that were in the cache but no longer exist in source_files.
    // These represent deleted files whose classes should NOT appear in the merged result.
    // Since we only iterate source_files above, deleted files are naturally excluded from
    // both stale and fresh, so their cached data won't be merged. No extra handling needed.

    (stale, fresh)
}

/// The merged Pass 1 indexes `build_cache_entries` reads from — bundled into
/// one borrow so the function stays under clippy's argument-count limit
/// (BT-3120 added the `hashes` parameter, which would otherwise push it over).
/// Mirrors the corresponding fields of [`IncrementalPass1Result`].
struct Pass1Indexes<'a> {
    class_module_index: &'a HashMap<String, String>,
    class_superclass_index: &'a HashMap<String, String>,
    all_class_infos: &'a [ClassInfo],
    extension_index: &'a ExtensionIndex,
}

/// Build cache entries from the current Pass 1 results.
///
/// Each source file gets an entry with its current content hash (BT-3120,
/// taken from the precomputed `hashes` map — see [`content_hashes_of`] —
/// rather than re-hashed here) and the subset of class/superclass indexes
/// that belong to it (determined by module name prefix matching).
fn build_cache_entries(
    source_files: &[Utf8PathBuf],
    source_root: Option<&Utf8Path>,
    pkg_name: &str,
    indexes: &Pass1Indexes<'_>,
    hashes: &HashMap<String, String>,
) -> HashMap<String, CacheEntry> {
    let Pass1Indexes {
        class_module_index,
        class_superclass_index,
        all_class_infos,
        extension_index,
    } = *indexes;

    // Build a reverse index: module_name → Vec<(class_name, module_name)>
    // This avoids O(files * classes) iteration in the loop below.
    let mut module_to_classes: HashMap<&str, Vec<&str>> = HashMap::new();
    for (class_name, mod_name) in class_module_index {
        module_to_classes
            .entry(mod_name.as_str())
            .or_default()
            .push(class_name.as_str());
    }

    // Build a name → ClassInfo index for fast lookup
    let class_info_by_name: HashMap<&str, &ClassInfo> = all_class_infos
        .iter()
        .map(|ci| (ci.name.as_str(), ci))
        .collect();

    let mut entries = HashMap::new();

    for file in source_files {
        let Some(content_hash) = hashes.get(file.as_str()).cloned() else {
            continue;
        };

        // Compute the expected module name for this file
        let module_name = match super::build::compute_relative_module(file, source_root) {
            Ok(rel) => format!("bt@{pkg_name}@{rel}"),
            Err(_) => continue,
        };

        // Collect classes that belong to this file's module via reverse index
        let mut file_class_module_index = HashMap::new();
        let mut file_superclass_index = HashMap::new();
        let mut file_class_infos = Vec::new();

        if let Some(class_names) = module_to_classes.get(module_name.as_str()) {
            for &class_name in class_names {
                file_class_module_index.insert(class_name.to_string(), module_name.clone());
                if let Some(superclass) = class_superclass_index.get(class_name) {
                    file_superclass_index.insert(class_name.to_string(), superclass.clone());
                }
                if let Some(ci) = class_info_by_name.get(class_name) {
                    file_class_infos.push((*ci).clone());
                }
            }
        }

        entries.insert(
            file.as_str().to_string(),
            CacheEntry {
                content_hash,
                class_module_index: file_class_module_index,
                class_superclass_index: file_superclass_index,
                class_infos: file_class_infos,
                extensions: extension_index.entries_for_file(file.as_std_path()),
            },
        );
    }

    entries
}

/// Serde support for `Option<SystemTime>` via duration-since-epoch.
///
/// `SystemTime` does not implement `Serialize`/`Deserialize` by default,
/// so we convert to/from seconds + nanos since `UNIX_EPOCH`.
#[allow(clippy::ref_option)] // serde `with` attribute requires `&Option<T>` signature
mod system_time_serde {
    use serde::{Deserialize, Deserializer, Serialize, Serializer};
    use std::time::{Duration, SystemTime, UNIX_EPOCH};

    #[derive(Serialize, Deserialize)]
    struct Epoch {
        secs: u64,
        nanos: u32,
    }

    pub fn serialize<S: Serializer>(
        time: &Option<SystemTime>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        match time {
            Some(t) => {
                let dur = t.duration_since(UNIX_EPOCH).unwrap_or(Duration::ZERO);
                let epoch = Epoch {
                    secs: dur.as_secs(),
                    nanos: dur.subsec_nanos(),
                };
                Some(epoch).serialize(serializer)
            }
            None => Option::<Epoch>::None.serialize(serializer),
        }
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(
        deserializer: D,
    ) -> Result<Option<SystemTime>, D::Error> {
        let opt: Option<Epoch> = Option::deserialize(deserializer)?;
        Ok(opt.map(|e| UNIX_EPOCH + Duration::new(e.secs, e.nanos)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_cache_round_trip() {
        let temp = TempDir::new().unwrap();
        let build_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let mut entries = HashMap::new();
        let mut cmi = HashMap::new();
        cmi.insert("Counter".to_string(), "bt@app@counter".to_string());
        let mut csi = HashMap::new();
        csi.insert("Counter".to_string(), "Object".to_string());

        entries.insert(
            "/src/counter.bt".to_string(),
            CacheEntry {
                extensions: Vec::new(),
                content_hash: "deadbeef".to_string(),
                class_module_index: cmi,
                class_superclass_index: csi,
                class_infos: Vec::new(),
            },
        );

        save_cache(&build_dir, None, entries);

        let loaded = load_cache(&build_dir, None);
        let CacheLoadResult::Hit(loaded) = loaded else {
            panic!("expected cache hit");
        };
        assert_eq!(loaded.version, CACHE_VERSION);
        assert_eq!(loaded.entries.len(), 1);
        assert!(loaded.entries.contains_key("/src/counter.bt"));
    }

    #[test]
    fn test_cache_version_mismatch_returns_none() {
        let temp = TempDir::new().unwrap();
        let build_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let cache = Pass1Cache {
            version: 999,
            manifest_mtime: None,
            entries: HashMap::new(),
        };
        let data = serde_json::to_string(&cache).unwrap();
        fs::write(build_dir.join(CACHE_FILENAME), data).unwrap();

        assert!(matches!(
            load_cache(&build_dir, None),
            CacheLoadResult::Miss
        ));
    }

    #[test]
    fn test_cache_invalidated_when_manifest_changes() {
        let temp = TempDir::new().unwrap();
        let build_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let manifest_path = build_dir.join("beamtalk.toml");
        fs::write(&manifest_path, "initial").unwrap();

        // Save cache with initial manifest mtime
        save_cache(&build_dir, Some(&manifest_path), HashMap::new());

        // Verify cache loads OK with same manifest
        assert!(matches!(
            load_cache(&build_dir, Some(&manifest_path)),
            CacheLoadResult::Hit(_)
        ));

        // Touch the manifest until the filesystem reports a different mtime.
        // Some filesystems have 1-second mtime granularity, so we retry.
        let original_mtime = mtime_of(&manifest_path);
        for attempt in 0..20 {
            std::thread::sleep(std::time::Duration::from_millis(100));
            fs::write(&manifest_path, format!("changed-{attempt}")).unwrap();
            if mtime_of(&manifest_path) != original_mtime {
                break;
            }
        }
        assert_ne!(
            mtime_of(&manifest_path),
            original_mtime,
            "filesystem mtime did not change after retries"
        );

        // Cache should now be invalidated due to manifest change
        assert!(matches!(
            load_cache(&build_dir, Some(&manifest_path)),
            CacheLoadResult::ManifestInvalidated
        ));
    }

    #[test]
    fn test_partition_files_detects_new_and_stale() {
        let temp = TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create two source files
        let file_a = dir.join("a.bt");
        let file_b = dir.join("b.bt");
        let file_c = dir.join("c.bt");
        fs::write(&file_a, "a").unwrap();
        fs::write(&file_b, "b").unwrap();
        fs::write(&file_c, "c").unwrap();

        let hash_a = content_hash_of(&file_a).unwrap();
        let hash_b = content_hash_of(&file_b).unwrap();

        // Build a cache with a and b (but not c)
        let mut entries = HashMap::new();
        entries.insert(
            file_a.as_str().to_string(),
            CacheEntry {
                extensions: Vec::new(),
                content_hash: hash_a,
                class_module_index: HashMap::new(),
                class_superclass_index: HashMap::new(),
                class_infos: Vec::new(),
            },
        );
        entries.insert(
            file_b.as_str().to_string(),
            CacheEntry {
                extensions: Vec::new(),
                content_hash: hash_b,
                class_module_index: HashMap::new(),
                class_superclass_index: HashMap::new(),
                class_infos: Vec::new(),
            },
        );

        let cache = Pass1Cache {
            version: CACHE_VERSION,
            manifest_mtime: None,
            entries,
        };

        // file_a: fresh, file_b: fresh, file_c: new (stale)
        let source_files = vec![file_a.clone(), file_b.clone(), file_c.clone()];
        let hashes = content_hashes_of(&source_files);
        let (stale, fresh) = partition_files(&source_files, &cache, &hashes);
        assert_eq!(fresh, vec![file_a, file_b]);
        assert_eq!(stale, vec![file_c]);
    }

    /// BT-3120: a file whose content changed but whose mtime was backdated
    /// (or preserved) by the writing tool must still be detected as stale —
    /// staleness comes from the content hash, never from mtime.
    #[test]
    fn test_partition_files_stale_on_content_change_with_backdated_mtime() {
        let temp = TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let file = dir.join("a.bt");

        fs::write(&file, "counter := [0].").unwrap();
        let old_hash = content_hash_of(&file).unwrap();
        let old_mtime = mtime_of(&file).unwrap();

        let mut entries = HashMap::new();
        entries.insert(
            file.as_str().to_string(),
            CacheEntry {
                extensions: Vec::new(),
                content_hash: old_hash,
                class_module_index: HashMap::new(),
                class_superclass_index: HashMap::new(),
                class_infos: Vec::new(),
            },
        );
        let cache = Pass1Cache {
            version: CACHE_VERSION,
            manifest_mtime: None,
            entries,
        };

        // Change the content, then force the mtime back to what it was
        // before — simulating a tool that preserves or backdates mtime on
        // write (or a same-second write on a coarse-grained filesystem).
        fs::write(&file, "counter := [1].").unwrap();
        fs::OpenOptions::new()
            .write(true)
            .open(&file)
            .unwrap()
            .set_modified(old_mtime)
            .unwrap();
        assert_eq!(
            mtime_of(&file),
            Some(old_mtime),
            "mtime must be back to its old value for this test to be meaningful"
        );

        let hashes = content_hashes_of(std::slice::from_ref(&file));
        let (stale, fresh) = partition_files(std::slice::from_ref(&file), &cache, &hashes);
        assert_eq!(
            stale,
            vec![file],
            "content changed under an unchanged mtime must still be detected as stale"
        );
        assert!(fresh.is_empty());
    }

    /// BT-3120: rewriting identical content (e.g. a `touch`, or a build tool
    /// that always rewrites its output) bumps mtime but must not be treated
    /// as stale — only a content change should trigger re-scanning.
    #[test]
    fn test_partition_files_fresh_on_touch_without_content_change() {
        let temp = TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let file = dir.join("a.bt");

        fs::write(&file, "counter := [0].").unwrap();
        let hash = content_hash_of(&file).unwrap();

        let mut entries = HashMap::new();
        entries.insert(
            file.as_str().to_string(),
            CacheEntry {
                extensions: Vec::new(),
                content_hash: hash,
                class_module_index: HashMap::new(),
                class_superclass_index: HashMap::new(),
                class_infos: Vec::new(),
            },
        );
        let cache = Pass1Cache {
            version: CACHE_VERSION,
            manifest_mtime: None,
            entries,
        };

        // Rewrite the exact same content — this changes mtime but not bytes.
        std::thread::sleep(std::time::Duration::from_millis(10));
        fs::write(&file, "counter := [0].").unwrap();

        let hashes = content_hashes_of(std::slice::from_ref(&file));
        let (stale, fresh) = partition_files(std::slice::from_ref(&file), &cache, &hashes);
        assert_eq!(
            fresh,
            vec![file],
            "touch-without-change must not be treated as stale"
        );
        assert!(stale.is_empty());
    }

    /// BT-3120 acceptance criterion: build with content A, then simulate a
    /// `git checkout` that restores older content B under a *newer* mtime
    /// (git always sets mtime to the checkout time, regardless of the
    /// content's age) — the cache must still detect the content change and
    /// mark the file stale. A subsequent touch-without-change (mtime bump,
    /// same content) must not cause another rebuild.
    #[test]
    fn test_partition_files_branch_switch_scenario() {
        let temp = TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let file = dir.join("a.bt");

        // "main" branch content, built once.
        fs::write(&file, "counter := [0].").unwrap();
        let hash_main = content_hash_of(&file).unwrap();
        let mut entries = HashMap::new();
        entries.insert(
            file.as_str().to_string(),
            CacheEntry {
                extensions: Vec::new(),
                content_hash: hash_main,
                class_module_index: HashMap::new(),
                class_superclass_index: HashMap::new(),
                class_infos: Vec::new(),
            },
        );
        let cache = Pass1Cache {
            version: CACHE_VERSION,
            manifest_mtime: None,
            entries,
        };

        // `git checkout old-branch`: older (different) content, fresh mtime.
        std::thread::sleep(std::time::Duration::from_millis(10));
        fs::write(&file, "counter := [-1]. \"older branch content\"").unwrap();

        let hashes = content_hashes_of(std::slice::from_ref(&file));
        let (stale, fresh) = partition_files(std::slice::from_ref(&file), &cache, &hashes);
        assert_eq!(
            stale,
            vec![file.clone()],
            "checked-out content differs from the cached hash — must rebuild"
        );
        assert!(fresh.is_empty());

        // Re-cache as if the rebuild happened, then touch without changing
        // content (e.g. a second `git checkout` back to the same content, or
        // an editor save-without-edit).
        let hash_old_branch = content_hash_of(&file).unwrap();
        let mut entries2 = HashMap::new();
        entries2.insert(
            file.as_str().to_string(),
            CacheEntry {
                extensions: Vec::new(),
                content_hash: hash_old_branch,
                class_module_index: HashMap::new(),
                class_superclass_index: HashMap::new(),
                class_infos: Vec::new(),
            },
        );
        let cache2 = Pass1Cache {
            version: CACHE_VERSION,
            manifest_mtime: None,
            entries: entries2,
        };

        std::thread::sleep(std::time::Duration::from_millis(10));
        fs::write(&file, "counter := [-1]. \"older branch content\"").unwrap();

        let hashes2 = content_hashes_of(std::slice::from_ref(&file));
        let (stale2, fresh2) = partition_files(std::slice::from_ref(&file), &cache2, &hashes2);
        assert_eq!(
            fresh2,
            vec![file],
            "touch-without-change after the rebuild must not recompile"
        );
        assert!(stale2.is_empty());
    }

    #[test]
    fn test_no_cache_file_returns_miss() {
        let temp = TempDir::new().unwrap();
        let build_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        assert!(matches!(
            load_cache(&build_dir, None),
            CacheLoadResult::Miss
        ));
    }

    #[test]
    fn test_discard_pass1_cache_removes_file() {
        let temp = TempDir::new().unwrap();
        let build_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Write a cache, then discard it.
        save_cache(&build_dir, None, HashMap::new());
        assert!(build_dir.join(CACHE_FILENAME).exists());

        discard_pass1_cache(&build_dir);
        assert!(!build_dir.join(CACHE_FILENAME).exists());

        // Discarding a missing cache is a no-op, not an error.
        discard_pass1_cache(&build_dir);
    }

    #[test]
    fn test_corrupt_cache_file_returns_miss() {
        let temp = TempDir::new().unwrap();
        let build_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        fs::write(build_dir.join(CACHE_FILENAME), "not valid json!!!").unwrap();

        assert!(matches!(
            load_cache(&build_dir, None),
            CacheLoadResult::Miss
        ));
    }
}

#[cfg(test)]
mod extension_cache_tests {
    use super::*;
    use tempfile::TempDir;

    /// BT-2795: extensions must survive the Pass 1 cache round-trip — first
    /// build scans and saves them; a second build with unchanged files must
    /// restore the same project-wide extension index from cache alone, and a
    /// deleted file's cached extensions must disappear.
    #[test]
    fn extensions_survive_incremental_cache_round_trip() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src = root.join("src");
        std::fs::create_dir_all(&src).unwrap();
        let build_dir = root.join("build");
        std::fs::create_dir_all(&build_dir).unwrap();

        let ext_file = src.join("string_shout.bt");
        std::fs::write(&ext_file, "String >> shoutLouder => self\n").unwrap();
        let plain_file = src.join("plain.bt");
        std::fs::write(&plain_file, "Object subclass: Plain\n  m => 1\n").unwrap();

        let source_files = vec![ext_file.clone(), plain_file.clone()];

        // First build: cache miss, everything scanned.
        let first = incremental_build_class_module_index(
            &source_files,
            Some(&src),
            "pkg",
            &build_dir,
            None,
            false,
        )
        .unwrap();
        assert_eq!(first.extension_index.len(), 1, "extension scanned");
        // BT-3120: Pass 1 hands back every scanned file's content hash so
        // Pass 2 (`detect_changes`) can reuse it instead of re-hashing.
        assert_eq!(
            first.source_hashes.len(),
            2,
            "source_hashes covers every source file, not just stale ones"
        );
        assert_eq!(
            first.source_hashes.get(ext_file.as_str()),
            content_hash_of(&ext_file).as_ref(),
            "returned hash must match the file's actual content hash"
        );

        // Second build: all files fresh — extensions must come from the cache.
        let second = incremental_build_class_module_index(
            &source_files,
            Some(&src),
            "pkg",
            &build_dir,
            None,
            false,
        )
        .unwrap();
        assert_eq!(
            second.extension_index.len(),
            1,
            "extension restored from cache on a fully-fresh build"
        );
        // Regrouping by file must find the entry under its original path
        // (byte-exact path identity — see ExtensionIndex::entries_for_file).
        assert_eq!(
            second
                .extension_index
                .entries_for_file(ext_file.as_std_path())
                .len(),
            1,
            "cached extension regroups under its defining file"
        );
        // BT-3120: even on an all-fresh (cache-hit) build, every file is
        // still hashed once so `source_hashes` stays complete for Pass 2 —
        // the fresh/stale split only affects re-scanning, not hashing.
        assert_eq!(
            second.source_hashes.len(),
            2,
            "source_hashes stays complete on an all-fresh build"
        );

        // Third build: the defining file is deleted — its cached extensions
        // must not linger.
        std::fs::remove_file(&ext_file).unwrap();
        let remaining = vec![plain_file];
        let third = incremental_build_class_module_index(
            &remaining,
            Some(&src),
            "pkg",
            &build_dir,
            None,
            false,
        )
        .unwrap();
        assert!(
            third.extension_index.is_empty(),
            "deleted file's cached extensions must disappear"
        );
    }
}
