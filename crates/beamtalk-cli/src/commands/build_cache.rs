// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Persistent metadata cache for incremental Pass 1 builds (BT-1683).
//!
//! Serialises the class indexes and `ClassInfo` vectors that Pass 1 produces so
//! that unchanged files can be skipped on the next build. The cache stores
//! per-file metadata keyed by source path, together with the file's modification
//! time so staleness can be detected cheaply.
//!
//! The cache is invalidated entirely when `beamtalk.toml` changes (mtime check)
//! or when `--force` is used.

use beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo;
use camino::{Utf8Path, Utf8PathBuf};
use miette::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::time::SystemTime;
use tracing::{debug, info, warn};

use super::util::mtime_of;

/// Name of the cache file stored inside the build directory.
const CACHE_FILENAME: &str = ".beamtalk-pass1-cache.json";

/// Current cache format version. Bump when the serialised layout changes.
const CACHE_VERSION: u32 = 1;

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
    /// Source file modification time when this entry was recorded.
    #[serde(with = "system_time_required_serde")]
    mtime: SystemTime,

    /// Classes defined in this file, mapped to their compiled module name.
    /// E.g. `"Counter" → "bt@my_app@counter"`.
    class_module_index: HashMap<String, String>,

    /// Superclass relationships for classes in this file.
    /// E.g. `"MyChild" → "MyParent"`.
    class_superclass_index: HashMap<String, String>,

    /// Full `ClassInfo` entries extracted from this file.
    class_infos: Vec<ClassInfo>,
}

/// Result of loading the cache and merging with fresh data.
pub(crate) struct IncrementalPass1Result {
    /// Merged class → module index (cached + freshly-scanned).
    pub class_module_index: HashMap<String, String>,
    /// Merged class → superclass index.
    pub class_superclass_index: HashMap<String, String>,
    /// Merged `ClassInfo` vector.
    pub all_class_infos: Vec<ClassInfo>,
    /// Cached ASTs for files that were re-scanned in this build.
    pub cached_asts: HashMap<Utf8PathBuf, super::build::CachedAst>,
    /// Whether the manifest changed and forced a full cache invalidation.
    /// When true, Pass 2 should also force-recompile all files.
    pub manifest_invalidated: bool,
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

/// Perform an incremental Pass 1 scan.
///
/// Loads the existing cache (if any), determines which files are stale,
/// re-scans only those, and merges the results. Returns the merged indexes
/// and the set of files that were actually re-scanned.
///
/// When `force` is true, the cache is ignored entirely.
pub(crate) fn incremental_build_class_module_index(
    source_files: &[Utf8PathBuf],
    source_root: Option<&Utf8Path>,
    pkg_name: &str,
    build_dir: &Utf8Path,
    manifest_path: Option<&Utf8Path>,
    force: bool,
) -> Result<IncrementalPass1Result> {
    // If forced, skip cache entirely
    if force {
        info!("Force build — ignoring Pass 1 cache");
        let (class_module_index, class_superclass_index, all_class_infos, cached_asts) =
            super::build::build_class_module_index(source_files, source_root, pkg_name)?;

        // Build cache entries for saving later
        let file_entries = build_cache_entries(
            source_files,
            source_root,
            pkg_name,
            &class_module_index,
            &class_superclass_index,
            &all_class_infos,
        );
        save_cache(build_dir, manifest_path, file_entries);

        return Ok(IncrementalPass1Result {
            class_module_index,
            class_superclass_index,
            all_class_infos,
            cached_asts,
            manifest_invalidated: false,
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
        Some(c) => partition_files(source_files, c),
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
            }
        }
    }

    // Re-scan stale files
    let (stale_module_index, stale_superclass_index, stale_class_infos, cached_asts) =
        if stale_files.is_empty() {
            (HashMap::new(), HashMap::new(), Vec::new(), HashMap::new())
        } else {
            super::build::build_class_module_index(&stale_files, source_root, pkg_name)?
        };

    // Merge stale results
    class_module_index.extend(stale_module_index);
    class_superclass_index.extend(stale_superclass_index);
    all_class_infos.extend(stale_class_infos);

    // Build updated cache entries and save
    let file_entries = build_cache_entries(
        source_files,
        source_root,
        pkg_name,
        &class_module_index,
        &class_superclass_index,
        &all_class_infos,
    );
    save_cache(build_dir, manifest_path, file_entries);

    Ok(IncrementalPass1Result {
        class_module_index,
        class_superclass_index,
        all_class_infos,
        cached_asts,
        manifest_invalidated,
    })
}

/// Partition source files into stale (need re-scan) and fresh (cache hit).
///
/// A file is considered stale if:
/// - It has no cache entry
/// - Its mtime is different from the cached mtime
/// - Its mtime cannot be read (err on the side of re-scanning)
fn partition_files(
    source_files: &[Utf8PathBuf],
    cache: &Pass1Cache,
) -> (Vec<Utf8PathBuf>, Vec<Utf8PathBuf>) {
    let mut stale = Vec::new();
    let mut fresh = Vec::new();

    for file in source_files {
        if let Some(entry) = cache.entries.get(file.as_str()) {
            let current_mtime = mtime_of(file);
            if current_mtime == Some(entry.mtime) {
                fresh.push(file.clone());
            } else {
                debug!(file = %file, "Cache stale — mtime changed");
                stale.push(file.clone());
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

/// Build cache entries from the current Pass 1 results.
///
/// Each source file gets an entry with its current mtime and the subset of
/// class/superclass indexes that belong to it (determined by module name
/// prefix matching).
fn build_cache_entries(
    source_files: &[Utf8PathBuf],
    source_root: Option<&Utf8Path>,
    pkg_name: &str,
    class_module_index: &HashMap<String, String>,
    class_superclass_index: &HashMap<String, String>,
    all_class_infos: &[ClassInfo],
) -> HashMap<String, CacheEntry> {
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
        let Some(mtime) = mtime_of(file) else {
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
                mtime,
                class_module_index: file_class_module_index,
                class_superclass_index: file_superclass_index,
                class_infos: file_class_infos,
            },
        );
    }

    entries
}

/// Serde support for `SystemTime` (required field) via duration-since-epoch.
mod system_time_required_serde {
    use serde::{Deserialize, Deserializer, Serialize, Serializer};
    use std::time::{Duration, SystemTime, UNIX_EPOCH};

    #[derive(Serialize, Deserialize)]
    struct Epoch {
        secs: u64,
        nanos: u32,
    }

    #[allow(clippy::trivially_copy_pass_by_ref)] // serde serialize_with requires &T
    pub fn serialize<S: Serializer>(time: &SystemTime, serializer: S) -> Result<S::Ok, S::Error> {
        let dur = time.duration_since(UNIX_EPOCH).unwrap_or(Duration::ZERO);
        let epoch = Epoch {
            secs: dur.as_secs(),
            nanos: dur.subsec_nanos(),
        };
        epoch.serialize(serializer)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(deserializer: D) -> Result<SystemTime, D::Error> {
        let epoch = Epoch::deserialize(deserializer)?;
        Ok(UNIX_EPOCH + Duration::new(epoch.secs, epoch.nanos))
    }
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
                mtime: SystemTime::now(),
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

        let mtime_a = mtime_of(&file_a).unwrap();
        let mtime_b = mtime_of(&file_b).unwrap();

        // Build a cache with a and b (but not c)
        let mut entries = HashMap::new();
        entries.insert(
            file_a.as_str().to_string(),
            CacheEntry {
                mtime: mtime_a,
                class_module_index: HashMap::new(),
                class_superclass_index: HashMap::new(),
                class_infos: Vec::new(),
            },
        );
        entries.insert(
            file_b.as_str().to_string(),
            CacheEntry {
                mtime: mtime_b,
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
        let (stale, fresh) = partition_files(&source_files, &cache);
        assert_eq!(fresh, vec![file_a, file_b]);
        assert_eq!(stale, vec![file_c]);
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
