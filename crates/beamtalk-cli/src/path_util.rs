// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared path utilities for the Beamtalk CLI.

use beamtalk_core::file_walker::FileWalker;
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};

/// Render a path string as forward-slash-separated text regardless of host OS.
///
/// `Utf8PathBuf`'s `Display`/`ToString` preserves native separators (backslash
/// on Windows), which is wrong for values embedded in generated or protocol
/// strings that must be byte-identical regardless of the developer's OS.
/// Backslash cannot appear as a legitimate path separator on Unix, so a plain
/// byte-level replace is safe on all supported platforms.
///
/// Pass `.as_str()` for a [`camino::Utf8Path`], `.to_string_lossy().as_ref()`
/// for a [`std::path::Path`], or `&s` for a `String` or `&str` directly.
///
/// This is the single shared leaf for path-separator normalisation inside
/// `beamtalk-cli`; do not inline `.replace('\\', "/")` at call sites.
pub fn to_forward_slash(s: &str) -> String {
    s.replace('\\', "/")
}

/// Whether `path` itself is a symlink — `false`, not an error, if it
/// doesn't exist. Uses `symlink_metadata` (not `exists`/`metadata`, which
/// follow symlinks), so this is the answer a caller about to
/// `remove_dir_all` a computed path actually needs: `starts_with`-style
/// containment checks are purely lexical and cannot see that a directory
/// in the chain is a symlink, so a caller that needs to refuse rather than
/// silently follow a link into unrelated storage checks this too. The
/// single shared leaf for that check inside `beamtalk-cli` — `clean.rs`
/// and `commands::release::mod`'s `ensure_clean_release_dir` both call it
/// rather than keeping their own copies.
///
/// # Errors
///
/// Returns an error if `path` exists but cannot be stat'd (e.g. a
/// permissions problem on an ancestor directory).
pub fn is_symlink(path: &Utf8Path) -> Result<bool> {
    match std::fs::symlink_metadata(path) {
        Ok(meta) => Ok(meta.file_type().is_symlink()),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(false),
        Err(e) => Err(e)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to stat '{path}'")),
    }
}

/// Whether `path`, or any existing ancestor of it down to and including
/// `boundary`, is a symlink. `starts_with(boundary)` containment checks are
/// lexical and can't see this, so a caller about to remove a sub-path
/// under some root directory (`boundary`) uses this to refuse rather than
/// silently follow a link partway down the chain into unrelated storage —
/// `_build` itself being replaced with a symlink is exactly the case
/// `clean.rs`'s own `is_symlink(&build_root)` check exists for; this
/// generalises it to every component between `boundary` and `path`, for
/// callers (like a computed `_build/release/<name>-<vsn>/`) where more than
/// one new path segment sits below the boundary.
///
/// `path` must have `boundary` as a prefix; if it doesn't, only `path`
/// itself is checked (the walk stops as soon as it can't produce a
/// shorter-but-still-`boundary`-prefixed ancestor).
///
/// # Errors
///
/// Returns an error if any checked component exists but cannot be stat'd.
pub fn has_symlink_in_chain(path: &Utf8Path, boundary: &Utf8Path) -> Result<bool> {
    let mut current = Some(path);
    while let Some(p) = current {
        if is_symlink(p)? {
            return Ok(true);
        }
        if p == boundary {
            break;
        }
        current = p.parent().filter(|parent| parent.starts_with(boundary));
    }
    Ok(false)
}

// `normalize_path` moved to `beamtalk-project` with the dependency-graph walk
// that uses it (BT-4611); re-exported for existing call sites.
pub use beamtalk_project::dependency_graph::normalize_path;

/// The `.erl` files directly inside `dir` (not recursive), sorted, as UTF-8
/// paths — the one lister for a package's `native/`, `native/test/` and the
/// stdlib's Erlang `src/` directories.
///
/// # Errors
///
/// Returns an error if `dir` does not exist or cannot be read.
pub fn erl_files_in(dir: &Utf8Path) -> Result<Vec<Utf8PathBuf>> {
    FileWalker::new()
        .extensions(&["erl"])
        .recursive(false)
        .walk(dir)
        .wrap_err_with(|| format!("Failed to list Erlang files in '{dir}'"))
}

/// The module names (file stems) of [`erl_files_in`], sorted.
///
/// # Errors
///
/// Same as [`erl_files_in`].
pub fn erl_module_names_in(dir: &Utf8Path) -> Result<Vec<String>> {
    Ok(erl_files_in(dir)?
        .iter()
        .filter_map(|p| p.file_stem().map(str::to_owned))
        .collect())
}

/// Delete every `.beam` directly in `ebin` whose module is not in `keep`, and
/// return the modules removed (BT-5074).
///
/// `erlc` writes beams but never removes one whose source is gone, so a
/// deleted `native/old.erl` would otherwise stay on the code path, in spec
/// extraction and in the known-module set. A missing `ebin` is a no-op.
///
/// # Errors
///
/// Returns an error if the directory cannot be read or a beam cannot be removed.
pub fn remove_beams_not_in<S: std::hash::BuildHasher>(
    ebin: &Utf8Path,
    keep: &std::collections::HashSet<String, S>,
) -> Result<Vec<String>> {
    let Ok(entries) = std::fs::read_dir(ebin) else {
        return Ok(Vec::new());
    };
    let mut removed = Vec::new();
    for entry in entries {
        let path = entry
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to list '{ebin}'"))?
            .path();
        if path.extension().and_then(|e| e.to_str()) != Some("beam") {
            continue;
        }
        let Some(module) = path.file_stem().and_then(|s| s.to_str()) else {
            continue;
        };
        if !keep.contains(module) {
            std::fs::remove_file(&path)
                .into_diagnostic()
                .wrap_err_with(|| format!("Failed to remove stale beam '{}'", path.display()))?;
            removed.push(module.to_string());
        }
    }
    removed.sort();
    Ok(removed)
}

#[cfg(test)]
mod erl_listing_tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn dir_with(files: &[&str]) -> (TempDir, Utf8PathBuf) {
        let temp = TempDir::new().unwrap();
        let dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        for f in files {
            fs::write(dir.join(f), "").unwrap();
        }
        (temp, dir)
    }

    #[test]
    fn erl_files_in_is_sorted_filters_extensions_and_is_not_recursive() {
        let (_t, dir) = dir_with(&["foo.erl", "bar.erl", "readme.md", "data.json"]);
        fs::create_dir(dir.join("test")).unwrap();
        fs::write(dir.join("test/nested.erl"), "").unwrap();

        let files = erl_files_in(&dir).unwrap();
        assert_eq!(files, vec![dir.join("bar.erl"), dir.join("foo.erl")]);
    }

    #[test]
    fn erl_module_names_in_returns_sorted_stems() {
        let (_t, dir) = dir_with(&["zeta.erl", "alpha.erl", "notes.txt"]);
        assert_eq!(erl_module_names_in(&dir).unwrap(), ["alpha", "zeta"]);
    }

    #[test]
    fn erl_files_in_missing_directory_errors() {
        let (_t, dir) = dir_with(&[]);
        assert!(erl_files_in(&dir.join("missing")).is_err());
    }
}
