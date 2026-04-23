// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Package-root discovery and source-file collection.
//!
//! **DDD Context:** Build System
//!
//! Shared helpers for tooling that needs to locate a Beamtalk package
//! (identified by `beamtalk.toml`) and enumerate its conventional source
//! directories (`src/` and `test/`).
//!
//! Both the CLI (`beamtalk lint` / `beamtalk fmt`) and the MCP server
//! (`run_lint_structured`, `compute_diagnostic_summary`) use these helpers so
//! lint diagnostics stay consistent between the two entry points
//! (BT-2052, BT-2060).

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use crate::file_walker::FileWalker;

/// Walk ancestors from `start` to find the package root — the first ancestor
/// directory containing a `beamtalk.toml` manifest.
///
/// `start` is canonicalized before the ancestor walk so that single-component
/// relative paths (e.g. `test/` invoked from the package root) still reach the
/// real package root instead of running out of parents (BT-2027).
///
/// Returns `None` if no `beamtalk.toml` is found in any ancestor directory.
#[must_use]
pub fn find_package_root(start: &Path) -> Option<PathBuf> {
    let canonical = std::fs::canonicalize(start).unwrap_or_else(|_| start.to_path_buf());
    let start_dir = if canonical.is_file() {
        canonical.parent()?.to_path_buf()
    } else {
        canonical
    };

    let mut dir = start_dir.as_path();
    loop {
        // Guard against empty paths produced when walking up from a
        // single-component relative path — `"".join("beamtalk.toml")` would
        // otherwise resolve relative to the CWD and report a spurious hit.
        if dir.as_os_str().is_empty() {
            return None;
        }
        if dir.join("beamtalk.toml").exists() {
            return Some(dir.to_path_buf());
        }
        dir = dir.parent()?;
    }
}

/// Collect all `.bt` files from a package's conventional source directories
/// (`src/` and `test/`) for cross-file class resolution.
///
/// Duplicates are removed by canonical path, so a file reached via both
/// directories (or via an explicit target and a walked directory) appears only
/// once.
///
/// Walk failures on individual directories are returned via `walk_errors` so
/// the caller can surface them through whatever logging stack it uses. The
/// remaining directory is still scanned — one missing subdir should not
/// silently drop the other.
#[must_use]
pub fn collect_package_source_files(package_root: &Path) -> Vec<PathBuf> {
    collect_package_source_files_with_errors(package_root).0
}

/// Same as [`collect_package_source_files`] but also returns any per-directory
/// walk errors encountered. Callers that want to log walk failures use this
/// variant; callers that prefer silent best-effort behaviour use the plain
/// `collect_package_source_files`.
#[must_use]
pub fn collect_package_source_files_with_errors(
    package_root: &Path,
) -> (Vec<PathBuf>, Vec<(PathBuf, miette::Report)>) {
    let mut seen: HashSet<PathBuf> = HashSet::new();
    let mut out: Vec<PathBuf> = Vec::new();
    let mut errors: Vec<(PathBuf, miette::Report)> = Vec::new();

    for subdir in ["src", "test"] {
        let dir = package_root.join(subdir);
        if dir.is_dir() {
            match FileWalker::source_files().walk_pathbuf(&dir) {
                Ok(files) => {
                    for f in files {
                        let key = std::fs::canonicalize(&f).unwrap_or_else(|_| f.clone());
                        if seen.insert(key) {
                            out.push(f);
                        }
                    }
                }
                Err(e) => {
                    errors.push((dir, e));
                }
            }
        }
    }

    (out, errors)
}

/// Determine the full set of files to parse for cross-file class extraction,
/// and build a canonical set of target files for matching.
///
/// When `source_path` lives inside a package (`beamtalk.toml` is reachable via
/// ancestor walk), extraction covers the whole package source set (`src/` +
/// `test/`) so cross-file class references resolve correctly even when the
/// lint target is a subset (e.g. `test/` or a single file). Target files that
/// live outside the conventional directories are always appended so they are
/// never dropped.
///
/// Returns `(extraction_files, target_set)` where `target_set` contains the
/// canonical paths of the original `source_files` for filtering parsed results
/// back down to the user-requested targets.
#[must_use]
pub fn resolve_extraction_files(
    source_path: &Path,
    source_files: &[PathBuf],
) -> (Vec<PathBuf>, HashSet<PathBuf>) {
    let package_root = find_package_root(source_path);

    let target_set: HashSet<PathBuf> = source_files
        .iter()
        .map(|f| std::fs::canonicalize(f).unwrap_or_else(|_| f.clone()))
        .collect();

    let extraction_files = match &package_root {
        Some(root) => {
            let mut pkg_files = collect_package_source_files(root);
            // Build a canonical set of already-included package files so the
            // dedup lookup is O(1) per target rather than O(n) linear.
            let pkg_canonical: HashSet<PathBuf> = pkg_files
                .iter()
                .map(|p| std::fs::canonicalize(p).unwrap_or_else(|_| p.clone()))
                .collect();
            // Ensure explicitly-targeted files are always included even when
            // they live outside the conventional src/ and test/ directories.
            for f in source_files {
                let key = std::fs::canonicalize(f).unwrap_or_else(|_| f.clone());
                if !pkg_canonical.contains(&key) {
                    pkg_files.push(f.clone());
                }
            }
            pkg_files
        }
        None => source_files.to_vec(),
    };

    (extraction_files, target_set)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn write_manifest(dir: &Path) {
        fs::write(
            dir.join("beamtalk.toml"),
            "[package]\nname = \"t\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
    }

    #[test]
    fn find_package_root_finds_manifest_from_subdir() {
        let tmp = TempDir::new().unwrap();
        let dir = tmp.path();
        let subdir = dir.join("src");
        fs::create_dir_all(&subdir).unwrap();
        write_manifest(dir);

        let found = find_package_root(&subdir);
        let expected = fs::canonicalize(dir).unwrap();
        assert_eq!(found.as_deref(), Some(expected.as_path()));
    }

    #[test]
    fn find_package_root_returns_none_without_manifest() {
        let tmp = TempDir::new().unwrap();
        let found = find_package_root(tmp.path());
        assert!(found.is_none(), "expected None, got {found:?}");
    }

    #[test]
    fn find_package_root_from_file() {
        let tmp = TempDir::new().unwrap();
        let dir = tmp.path();
        let src = dir.join("src");
        fs::create_dir_all(&src).unwrap();
        write_manifest(dir);
        let file = src.join("foo.bt");
        fs::write(&file, "Object subclass: Foo\n").unwrap();

        let found = find_package_root(&file);
        let expected = fs::canonicalize(dir).unwrap();
        assert_eq!(found.as_deref(), Some(expected.as_path()));
    }

    #[test]
    fn collect_package_source_files_covers_src_and_test() {
        let tmp = TempDir::new().unwrap();
        let dir = tmp.path();
        let src = dir.join("src");
        let test = dir.join("test");
        fs::create_dir_all(&src).unwrap();
        fs::create_dir_all(&test).unwrap();
        fs::write(src.join("a.bt"), "Object subclass: A\n").unwrap();
        fs::write(test.join("b.bt"), "Object subclass: B\n").unwrap();

        let files = collect_package_source_files(dir);
        assert_eq!(files.len(), 2, "expected 2 files, got {files:?}");
    }

    #[test]
    fn resolve_extraction_files_adds_target_outside_conventional_dirs() {
        let tmp = TempDir::new().unwrap();
        let dir = tmp.path();
        let src = dir.join("src");
        fs::create_dir_all(&src).unwrap();
        write_manifest(dir);
        fs::write(src.join("a.bt"), "Object subclass: A\n").unwrap();

        // A target outside src/ and test/ must be appended to extraction files.
        let outside = dir.join("root_only.bt");
        fs::write(&outside, "Object subclass: RootOnly\n").unwrap();

        let (extraction, target_set) =
            resolve_extraction_files(&outside, std::slice::from_ref(&outside));
        assert!(
            extraction
                .iter()
                .any(|f| fs::canonicalize(f).unwrap() == fs::canonicalize(&outside).unwrap()),
            "target file must appear in extraction set, got {extraction:?}"
        );
        assert!(
            extraction
                .iter()
                .any(|f| f.file_name().is_some_and(|n| n == "a.bt")),
            "sibling src/a.bt must appear in extraction set, got {extraction:?}"
        );
        assert_eq!(target_set.len(), 1);
    }

    #[test]
    fn resolve_extraction_files_without_manifest_returns_source_files() {
        let tmp = TempDir::new().unwrap();
        let file = tmp.path().join("loose.bt");
        fs::write(&file, "Object subclass: Loose\n").unwrap();

        let (extraction, target_set) = resolve_extraction_files(&file, std::slice::from_ref(&file));
        assert_eq!(extraction.len(), 1);
        assert_eq!(target_set.len(), 1);
    }
}
