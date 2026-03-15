// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared helpers for CLI commands.
//!
//! **DDD Context:** CLI

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::fs;

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
/// [`collect_files_recursive`].
pub fn find_files(path: &Utf8Path, extensions: &[&str]) -> Result<Vec<Utf8PathBuf>> {
    if path.is_file() {
        if path
            .extension()
            .is_some_and(|ext| extensions.contains(&ext))
        {
            return Ok(vec![path.to_path_buf()]);
        }
        let ext_list = extensions.join(", .");
        miette::bail!("Expected a .{ext_list} file, got '{path}'");
    }

    if !path.exists() {
        miette::bail!("Path '{path}' does not exist");
    }

    let mut files = Vec::new();

    for entry in fs::read_dir(path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read directory '{path}'"))?
    {
        let entry = entry.into_diagnostic()?;
        let entry_path = Utf8PathBuf::from_path_buf(entry.path())
            .map_err(|_| miette::miette!("Non-UTF-8 path in '{}'", path))?;

        if entry_path.is_file()
            && entry_path
                .extension()
                .is_some_and(|ext| extensions.contains(&ext))
        {
            files.push(entry_path);
        }
    }

    files.sort();
    Ok(files)
}

/// Recursively collect all files with the given extensions from a directory tree.
///
/// Symlinks are skipped to avoid potential infinite recursion from circular links.
/// Results are sorted after collection.
pub fn collect_files_recursive(
    dir: &Utf8Path,
    extensions: &[&str],
    files: &mut Vec<Utf8PathBuf>,
) -> Result<()> {
    for entry in fs::read_dir(dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read directory '{dir}'"))?
    {
        let entry = entry.into_diagnostic()?;
        let file_type = entry.file_type().into_diagnostic()?;
        if file_type.is_symlink() {
            continue;
        }
        let entry_path = Utf8PathBuf::from_path_buf(entry.path())
            .map_err(|_| miette::miette!("Non-UTF-8 path"))?;

        if file_type.is_dir() {
            collect_files_recursive(&entry_path, extensions, files)?;
        } else if file_type.is_file()
            && entry_path
                .extension()
                .is_some_and(|ext| extensions.contains(&ext))
        {
            files.push(entry_path);
        }
    }
    Ok(())
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

    #[test]
    fn test_collect_files_recursive_basic() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        fs::create_dir(dir_path.join("sub")).unwrap();
        fs::write(dir_path.join("a.bt"), "").unwrap();
        fs::write(dir_path.join("sub/b.bt"), "").unwrap();
        fs::write(dir_path.join("sub/c.txt"), "").unwrap();

        let mut files = Vec::new();
        collect_files_recursive(dir_path, &["bt"], &mut files).unwrap();
        files.sort();
        assert_eq!(files.len(), 2);
    }
}
