// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared path utilities for the Beamtalk CLI.

use camino::Utf8Path;
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
