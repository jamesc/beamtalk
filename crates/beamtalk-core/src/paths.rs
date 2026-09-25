// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared leaf: absolutizing a filesystem path without resolving symlinks.
//!
//! **DDD Context:** Infrastructure (shared leaf, no domain logic)
//!
//! Both [`crate::ffi_type_specs::find_runtime_dir_with_layout`] (this crate)
//! and `beamtalk-cli`'s release assembly (`release::assembly::absolutize`)
//! need to turn a possibly-relative path into an absolute one *without*
//! canonicalizing it — canonicalizing resolves symlinks, which breaks a
//! consumer that compares the result against another string built the same
//! way (BT-3619's `RELEASE_DIR` literal-prefix match) or that spawns a
//! program whose path contains a `/` from a different working directory
//! (BT-3624: `Command::current_dir` plus a relative program path resolves
//! the program *after* the chdir, not before).

use std::path::{Path, PathBuf};

/// Make `path` absolute by joining it onto the current directory when it
/// isn't already, then lexically normalize the result the way Erlang's own
/// `filename:join/1` normalizes paths: `.` segments, repeated separators and
/// a trailing separator are dropped, `..` segments are kept verbatim
/// (resolving them lexically would be wrong across a symlink).
/// `Path::components()` already implements exactly these rules.
///
/// Deliberately does **not** canonicalize — see the module doc comment for
/// why that distinction matters to callers.
///
/// # Errors
///
/// Returns an error if `path` is relative and the current directory cannot
/// be read.
pub fn absolutize(path: &Path) -> std::io::Result<PathBuf> {
    let absolute = if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir()?.join(path)
    };
    Ok(absolute.components().collect())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn absolutize_drops_cur_dir_segments_from_a_relative_path() {
        let cwd = std::env::current_dir().unwrap();
        let abs = absolutize(Path::new("./_build/release/app-0.1.0")).unwrap();
        assert_eq!(abs, cwd.join("_build").join("release").join("app-0.1.0"));
    }

    #[test]
    fn absolutize_normalizes_an_absolute_path_like_erlang_filename_join() {
        let tmp = tempfile::TempDir::new().unwrap();
        let root = tmp.path();
        let messy = PathBuf::from(format!("{}/./a//b/./", root.display()));
        assert_eq!(absolutize(&messy).unwrap(), root.join("a").join("b"));
    }

    #[test]
    fn absolutize_keeps_parent_dir_segments() {
        let tmp = tempfile::TempDir::new().unwrap();
        let root = tmp.path();
        let with_parent = root.join("a").join("..").join("b");
        assert_eq!(absolutize(&with_parent).unwrap(), with_parent);
    }

    #[test]
    fn absolutize_leaves_an_already_clean_absolute_path_untouched() {
        let tmp = tempfile::TempDir::new().unwrap();
        let root = tmp.path().join("a").join("b");
        assert_eq!(absolutize(&root).unwrap(), root);
    }
}
