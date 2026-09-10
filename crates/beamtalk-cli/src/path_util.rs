// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared path utilities for the Beamtalk CLI.

use camino::{Utf8Component, Utf8Path, Utf8PathBuf};

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

/// Normalize a path by resolving `.` and `..` components without filesystem access.
///
/// Unlike `std::fs::canonicalize`, this does not require the path to exist and
/// does not resolve symlinks.
pub fn normalize_path(path: &Utf8Path) -> Utf8PathBuf {
    let mut components = Vec::new();
    for component in path.components() {
        match component {
            Utf8Component::CurDir => {
                // Skip `.`
            }
            Utf8Component::ParentDir => match components.last().copied() {
                // Already at the filesystem root — an extra `..` is a no-op
                // rather than something to pop or accumulate: popping
                // `RootDir` here would turn `/foo/../..` into `.` instead of
                // `/`.
                Some(Utf8Component::RootDir) => {}
                // Nothing to pop yet, or a run of leading `..`s in a
                // relative path — accumulate.
                None | Some(Utf8Component::ParentDir) => components.push(component),
                // A real component precedes it — cancel the two out.
                Some(_) => {
                    components.pop();
                }
            },
            _ => {
                components.push(component);
            }
        }
    }

    if components.is_empty() {
        return Utf8PathBuf::from(".");
    }

    let mut result = Utf8PathBuf::new();
    for component in components {
        result.push(component.as_str());
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn normalize_path_resolves_parent() {
        let path = Utf8PathBuf::from("/home/user/project/../dep");
        assert_eq!(normalize_path(&path), Utf8PathBuf::from("/home/user/dep"));
    }

    #[test]
    fn normalize_path_resolves_dot() {
        let path = Utf8PathBuf::from("/home/user/./project");
        assert_eq!(
            normalize_path(&path),
            Utf8PathBuf::from("/home/user/project")
        );
    }

    #[test]
    fn normalize_path_multiple_parents() {
        let path = Utf8PathBuf::from("/home/user/project/../../dep");
        assert_eq!(normalize_path(&path), Utf8PathBuf::from("/home/dep"));
    }

    #[test]
    fn normalize_path_parent_at_root() {
        // A `..` chain that consumes every real component and then hits
        // root must not pop `RootDir` itself — `/foo/../..` is still `/`,
        // not `.`.
        let path = Utf8PathBuf::from("/foo/../..");
        assert_eq!(normalize_path(&path), Utf8PathBuf::from("/"));
    }
}
