// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared helpers for CLI commands.
//!
//! **DDD Context:** CLI

use beamtalk_core::file_walker::FileWalker;
use camino::{Utf8Path, Utf8PathBuf};
use miette::Result;

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
