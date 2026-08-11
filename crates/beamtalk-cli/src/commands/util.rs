// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared helpers for CLI commands.
//!
//! **DDD Context:** CLI

use beamtalk_core::file_walker::FileWalker;
use beamtalk_workspace::hex_encode;
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use sha2::{Digest, Sha256};
use std::fs;
use std::time::SystemTime;

/// Build the BEAM module name for a user-code file stem (`bt@<normalised-stem>`).
///
/// ADR 0016: all user-code modules use the `bt@` prefix. The stem is normalised
/// via [`beamtalk_core::codegen::core_erlang::to_module_name`] so that e.g.
/// `my-class.bt` → `bt@my_class`.
pub(crate) fn bt_module_name_from_stem(stem: &str) -> String {
    format!(
        "bt@{}",
        beamtalk_core::codegen::core_erlang::to_module_name(stem)
    )
}

/// What a test assertion expects: a value or an error.
///
/// Shared between stdlib tests (`test_stdlib`) and doc tests (`doc_tests`).
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expected {
    /// Match formatted result string (`_` for wildcard).
    Value(String),
    /// Match `#beamtalk_error{kind = Kind}` on error.
    Error { kind: String },
}

/// Read the modification time of a file, returning `None` on any error.
pub(super) fn mtime_of(path: &Utf8Path) -> Option<SystemTime> {
    fs::metadata(path).ok()?.modified().ok()
}

/// SHA-256 content hash of a file, as a lowercase hex string, or `None` if
/// the file cannot be read.
///
/// BT-3120: the source of truth for batch-build staleness checks
/// (`Pass1Cache` and `detect_changes`'s `.bt`-vs-`.beam` decision). Unlike
/// mtime, a content hash cannot be fooled by git branch switches (which
/// restore old content under a fresh mtime) or by tools that preserve or
/// backdate mtimes on write — both of which make mtime-only staleness checks
/// either spuriously rebuild (benign) or, worse, incorrectly skip a rebuild
/// (a stale `.beam`). Reuses `sha2`, already a dependency of this crate and
/// of `beamtalk-workspace`, and that crate's [`beamtalk_workspace::hex_encode`]
/// leaf for the digest-to-hex-string step — no new hash crate, and no
/// second copy of the hex-encoding loop, is introduced.
pub(super) fn content_hash_of(path: &Utf8Path) -> Option<String> {
    let bytes = fs::read(path).ok()?;
    let mut hasher = Sha256::new();
    hasher.update(&bytes);
    let digest = hasher.finalize();
    Some(hex_encode(&digest))
}

/// [`content_hash_of`] for every file in `paths`, keyed by path string.
///
/// BT-3120: a single build hashes each source file's content in more than
/// one place — Pass 1's staleness check and cache-entry rebuild
/// (`build_cache::partition_files` / `build_cache::build_cache_entries`),
/// and Pass 2's `.beam` staleness check (`detect_changes`). Computing every
/// file's hash once here and threading the map through those call sites
/// (rather than each calling [`content_hash_of`] independently) keeps a
/// build to one content-hash pass per file instead of two or three — the
/// difference between a `stat()` and hashing full file contents is real, and
/// BT-3120's acceptance criteria specifically calls out "no measurable
/// regression in warm no-op build time". Files that can't be read are
/// omitted, matching [`content_hash_of`]'s `None`-on-error behaviour —
/// callers already treat a missing hash as "must recompute".
pub(super) fn content_hashes_of(
    paths: &[Utf8PathBuf],
) -> std::collections::HashMap<String, String> {
    paths
        .iter()
        .filter_map(|p| content_hash_of(p).map(|h| (p.as_str().to_string(), h)))
        .collect()
}

/// Find the project root by requiring a `beamtalk.toml` in the current directory.
///
/// Returns the current working directory as a [`Utf8PathBuf`] if a manifest is
/// present, or an error telling the user to run from a Beamtalk project root.
pub(crate) fn find_project_root() -> Result<Utf8PathBuf> {
    let cwd = std::env::current_dir()
        .into_diagnostic()
        .wrap_err("Failed to determine current directory")?;

    let project_root = Utf8PathBuf::from_path_buf(cwd).map_err(|p| {
        miette::miette!("Current directory path is not valid UTF-8: {}", p.display())
    })?;

    let manifest_path = project_root.join("beamtalk.toml");
    if !manifest_path.exists() {
        miette::bail!(
            "No beamtalk.toml found in current directory.\n  \
             Run this command from a Beamtalk project root, or create one with `beamtalk new`."
        );
    }

    Ok(project_root)
}

/// Find files matching the given extensions in a path.
///
/// - If `path` is a file, validates it has one of the given extensions and returns it.
/// - If `path` is a directory, returns all matching files directly inside it (sorted).
/// - If `path` does not exist, returns an error.
///
/// This is a **non-recursive** scan. For recursive directory walking, use
/// [`FileWalker`](beamtalk_core::file_walker::FileWalker) directly.
pub fn find_files(path: &Utf8Path, extensions: &[&str]) -> Result<Vec<Utf8PathBuf>> {
    FileWalker::new()
        .extensions(extensions)
        .recursive(false)
        .walk(path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_content_hash_of_stable_for_same_content() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        let file = dir_path.join("a.bt");
        fs::write(&file, "counter := [0].").unwrap();

        let hash1 = content_hash_of(&file).unwrap();
        let hash2 = content_hash_of(&file).unwrap();
        assert_eq!(
            hash1, hash2,
            "hashing the same content must be deterministic"
        );
        assert_eq!(hash1.len(), 64, "SHA-256 hex digest is 64 chars");
    }

    #[test]
    fn test_content_hash_of_changes_with_content() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        let file = dir_path.join("a.bt");

        fs::write(&file, "counter := [0].").unwrap();
        let hash_a = content_hash_of(&file).unwrap();

        fs::write(&file, "counter := [1].").unwrap();
        let hash_b = content_hash_of(&file).unwrap();

        assert_ne!(hash_a, hash_b, "different content must hash differently");
    }

    #[test]
    fn test_content_hash_of_same_content_different_mtime() {
        // BT-3120: the hash must depend only on bytes, not on filesystem
        // metadata — rewriting identical content (which bumps mtime) must
        // produce the same hash.
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        let file = dir_path.join("a.bt");

        fs::write(&file, "counter := [0].").unwrap();
        let hash_a = content_hash_of(&file).unwrap();

        std::thread::sleep(std::time::Duration::from_millis(10));
        fs::write(&file, "counter := [0].").unwrap();
        let hash_b = content_hash_of(&file).unwrap();

        assert_eq!(
            hash_a, hash_b,
            "identical content must hash the same regardless of mtime"
        );
    }

    #[test]
    fn test_content_hash_of_missing_file_returns_none() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        let missing = dir_path.join("does-not-exist.bt");
        assert!(content_hash_of(&missing).is_none());
    }

    #[test]
    fn test_find_files_single_file() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        let file = dir_path.join("test.bt");
        fs::write(&file, "").unwrap();

        let files = find_files(&file, &["bt"]).unwrap();
        assert_eq!(files, vec![file]);
    }

    #[test]
    fn test_find_files_wrong_extension() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        let file = dir_path.join("test.txt");
        fs::write(&file, "").unwrap();

        let result = find_files(&file, &["bt"]);
        assert!(result.is_err());
    }

    #[test]
    fn test_find_files_directory() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        fs::write(dir_path.join("a.bt"), "").unwrap();
        fs::write(dir_path.join("b.bt"), "").unwrap();
        fs::write(dir_path.join("c.txt"), "").unwrap();

        let files = find_files(dir_path, &["bt"]).unwrap();
        assert_eq!(files.len(), 2);
        assert!(files[0].file_name() == Some("a.bt"));
        assert!(files[1].file_name() == Some("b.bt"));
    }

    #[test]
    fn test_find_files_nonexistent() {
        let result = find_files(Utf8Path::new("/nonexistent"), &["bt"]);
        assert!(result.is_err());
    }

    #[test]
    fn test_find_files_multiple_extensions() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        fs::write(dir_path.join("a.bt"), "").unwrap();
        fs::write(dir_path.join("b.btscript"), "").unwrap();
        fs::write(dir_path.join("c.txt"), "").unwrap();

        let files = find_files(dir_path, &["bt", "btscript"]).unwrap();
        assert_eq!(files.len(), 2);
    }
}
