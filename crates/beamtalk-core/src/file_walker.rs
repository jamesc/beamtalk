// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Configurable file-system walker for collecting source files.
//!
//! **DDD Context:** Compilation (shared infrastructure)
//!
//! Replaces the 7+ separate file-collection implementations that were scattered
//! across the codebase with a single, builder-pattern `FileWalker`.

use camino::{Utf8Path, Utf8PathBuf};
use miette::{IntoDiagnostic, WrapErr};
use std::collections::HashSet;
use std::fs;

/// Configurable file walker that recursively collects files matching a set of
/// criteria.
///
/// # Example
///
/// ```no_run
/// use beamtalk_core::file_walker::FileWalker;
///
/// let files = FileWalker::source_files()
///     .walk("src")
///     .unwrap();
/// ```
#[derive(Debug, Clone)]
#[allow(clippy::struct_excessive_bools)] // Builder pattern with independent config flags
pub struct FileWalker {
    /// File extensions to match (without the leading dot).
    extensions: Vec<String>,
    /// Whether to skip symlinks (default: true).
    skip_symlinks: bool,
    /// Directory names to exclude from traversal.
    excluded_dirs: HashSet<String>,
    /// Maximum number of files to collect (`None` = unlimited).
    max_files: Option<usize>,
    /// Whether read errors are fatal (default: true).
    ///
    /// When false, unreadable directories are silently skipped.
    strict_errors: bool,
    /// Whether to sort results after collection (default: true).
    sort: bool,
    /// Whether to recurse into subdirectories (default: true).
    recursive: bool,
}

impl Default for FileWalker {
    fn default() -> Self {
        Self {
            extensions: Vec::new(),
            skip_symlinks: true,
            excluded_dirs: HashSet::new(),
            max_files: None,
            strict_errors: true,
            sort: true,
            recursive: true,
        }
    }
}

impl FileWalker {
    /// Create a new walker with default settings.
    pub fn new() -> Self {
        Self::default()
    }

    // ── Builder methods ─────────────────────────────────────────────────

    /// Set the file extensions to match (without the leading dot).
    #[must_use]
    pub fn extensions(mut self, exts: &[&str]) -> Self {
        self.extensions = exts.iter().map(|s| (*s).to_string()).collect();
        self
    }

    /// Whether to skip symlinks. Default: `true`.
    #[must_use]
    pub fn skip_symlinks(mut self, skip: bool) -> Self {
        self.skip_symlinks = skip;
        self
    }

    /// Set directory names to exclude from traversal.
    #[must_use]
    pub fn exclude_dirs(mut self, dirs: &[&str]) -> Self {
        self.excluded_dirs = dirs.iter().map(|s| (*s).to_string()).collect();
        self
    }

    /// Limit the maximum number of files collected.
    #[must_use]
    pub fn max_files(mut self, limit: usize) -> Self {
        self.max_files = Some(limit);
        self
    }

    /// Whether read errors are fatal. Default: `true`.
    #[must_use]
    pub fn strict_errors(mut self, strict: bool) -> Self {
        self.strict_errors = strict;
        self
    }

    /// Whether to sort results. Default: `true`.
    #[must_use]
    pub fn sort(mut self, sort: bool) -> Self {
        self.sort = sort;
        self
    }

    /// Whether to recurse into subdirectories. Default: `true`.
    #[must_use]
    pub fn recursive(mut self, recursive: bool) -> Self {
        self.recursive = recursive;
        self
    }

    // ── Preset constructors ─────────────────────────────────────────────

    /// Preset for collecting `.bt` source files.
    ///
    /// Recursive, symlink-skipping, sorted, strict errors.
    pub fn source_files() -> Self {
        Self::new().extensions(&["bt"])
    }

    /// Preset for collecting `.bt` test files.
    ///
    /// Same as `source_files()` but excludes `fixtures/`.
    pub fn test_files() -> Self {
        Self::new().extensions(&["bt"]).exclude_dirs(&["fixtures"])
    }

    /// Preset for collecting formattable files (`.bt` and `.btscript`).
    pub fn format_files() -> Self {
        Self::new().extensions(&["bt", "btscript"])
    }

    /// Preset for collecting `.btscript` test files.
    pub fn btscript_files() -> Self {
        Self::new().extensions(&["btscript"])
    }

    /// Preset for collecting native `.erl` files.
    ///
    /// Recursive (covers both `native/*.erl` and `native/test/*.erl`),
    /// excludes `include/` (header files are not linted/formatted).
    pub fn native_erl_files() -> Self {
        Self::new().extensions(&["erl"]).exclude_dirs(&["include"])
    }

    /// Preset for LSP/IDE preloading with a file budget.
    ///
    /// Skips common non-source directories (`.git`, `target`, `node_modules`,
    /// `_build`), silently ignores errors, and caps the number of files.
    pub fn preload_files(budget: usize) -> Self {
        Self::new()
            .extensions(&["bt"])
            .exclude_dirs(&[".git", "target", "node_modules", "_build"])
            .max_files(budget)
            .strict_errors(false)
    }

    /// Preset for linting `.bt` files.
    ///
    /// Same as `source_files()` — recursive, sorted, strict errors.
    pub fn lint_files() -> Self {
        Self::new().extensions(&["bt"])
    }

    // ── Walking ─────────────────────────────────────────────────────────

    /// Walk from `path`, returning matching files.
    ///
    /// If `path` is a file, validates it matches the configured extensions and
    /// returns it as a single-element vector.
    ///
    /// If `path` is a directory, recursively (or non-recursively, per config)
    /// collects matching files.
    ///
    /// # Errors
    ///
    /// Returns an error if the path does not exist, if the file has the wrong
    /// extension, or if a directory cannot be read (when `strict_errors` is true).
    pub fn walk(&self, path: impl AsRef<Utf8Path>) -> miette::Result<Vec<Utf8PathBuf>> {
        let path = path.as_ref();

        if path.is_file() {
            return self.validate_single_file(path);
        }

        if !path.exists() {
            miette::bail!("Path '{}' does not exist", path);
        }

        let mut files = Vec::new();
        self.walk_dir(path, &mut files)?;

        if self.sort {
            files.sort();
        }

        Ok(files)
    }

    /// Walk from `path`, returning `std::path::PathBuf` results.
    ///
    /// Used by consumers that don't require UTF-8 paths (LSP, MCP, build-corpus).
    ///
    /// # Errors
    ///
    /// Returns an error if the path does not exist, if the file has the wrong
    /// extension, or if a directory cannot be read (when `strict_errors` is true).
    pub fn walk_pathbuf(&self, path: &std::path::Path) -> miette::Result<Vec<std::path::PathBuf>> {
        if path.is_file() {
            return self.validate_single_file_pathbuf(path);
        }

        if !path.exists() {
            miette::bail!("Path '{}' does not exist", path.display());
        }

        let mut files = Vec::new();
        self.walk_dir_pathbuf(path, &mut files)?;

        if self.sort {
            files.sort();
        }

        Ok(files)
    }

    // ── Internal helpers ────────────────────────────────────────────────

    fn validate_single_file(&self, path: &Utf8Path) -> miette::Result<Vec<Utf8PathBuf>> {
        if self.extensions.is_empty() || self.matches_extension_utf8(path) {
            Ok(vec![path.to_path_buf()])
        } else {
            let ext_list = self
                .extensions
                .iter()
                .map(|e| format!(".{e}"))
                .collect::<Vec<_>>()
                .join(", ");
            miette::bail!("Expected a {ext_list} file, got '{path}'");
        }
    }

    fn validate_single_file_pathbuf(
        &self,
        path: &std::path::Path,
    ) -> miette::Result<Vec<std::path::PathBuf>> {
        if self.extensions.is_empty() || self.matches_extension_std(path) {
            Ok(vec![path.to_path_buf()])
        } else {
            let ext_list = self
                .extensions
                .iter()
                .map(|e| format!(".{e}"))
                .collect::<Vec<_>>()
                .join(", ");
            miette::bail!("Expected a {ext_list} file, got '{}'", path.display());
        }
    }

    fn budget_exhausted(&self, files: &[impl Sized]) -> bool {
        self.max_files.is_some_and(|max| files.len() >= max)
    }

    fn walk_dir(&self, dir: &Utf8Path, files: &mut Vec<Utf8PathBuf>) -> miette::Result<()> {
        if self.budget_exhausted(files) {
            return Ok(());
        }

        let entries = if self.strict_errors {
            fs::read_dir(dir)
                .into_diagnostic()
                .wrap_err_with(|| format!("Failed to read directory '{dir}'"))?
        } else {
            match fs::read_dir(dir) {
                Ok(entries) => entries,
                Err(_) => return Ok(()),
            }
        };

        for entry in entries {
            if self.budget_exhausted(files) {
                return Ok(());
            }

            let entry: fs::DirEntry = if self.strict_errors {
                entry.into_diagnostic()?
            } else {
                match entry {
                    Ok(e) => e,
                    Err(_) => continue,
                }
            };

            let file_type = if self.strict_errors {
                entry.file_type().into_diagnostic()?
            } else {
                match entry.file_type() {
                    Ok(ft) => ft,
                    Err(_) => continue,
                }
            };

            if self.skip_symlinks && file_type.is_symlink() {
                continue;
            }

            let entry_path = Utf8PathBuf::from_path_buf(entry.path())
                .map_err(|_| miette::miette!("Non-UTF-8 path in '{}'", dir))?;

            if file_type.is_dir() {
                if self.is_excluded_dir(&entry_path) {
                    continue;
                }
                if self.recursive {
                    self.walk_dir(&entry_path, files)?;
                }
            } else if file_type.is_file() && self.matches_extension_utf8(&entry_path) {
                files.push(entry_path);
            }
        }

        Ok(())
    }

    fn walk_dir_pathbuf(
        &self,
        dir: &std::path::Path,
        files: &mut Vec<std::path::PathBuf>,
    ) -> miette::Result<()> {
        if self.budget_exhausted(files) {
            return Ok(());
        }

        let entries = if self.strict_errors {
            fs::read_dir(dir)
                .into_diagnostic()
                .wrap_err_with(|| format!("Failed to read directory '{}'", dir.display()))?
        } else {
            match fs::read_dir(dir) {
                Ok(entries) => entries,
                Err(_) => return Ok(()),
            }
        };

        for entry in entries {
            if self.budget_exhausted(files) {
                return Ok(());
            }

            let entry: fs::DirEntry = if self.strict_errors {
                entry.into_diagnostic()?
            } else {
                match entry {
                    Ok(e) => e,
                    Err(_) => continue,
                }
            };

            let file_type = if self.strict_errors {
                entry.file_type().into_diagnostic()?
            } else {
                match entry.file_type() {
                    Ok(ft) => ft,
                    Err(_) => continue,
                }
            };

            if self.skip_symlinks && file_type.is_symlink() {
                continue;
            }

            let path = entry.path();

            if file_type.is_dir() {
                if self.is_excluded_dir_std(&path) {
                    continue;
                }
                if self.recursive {
                    self.walk_dir_pathbuf(&path, files)?;
                }
            } else if file_type.is_file() && self.matches_extension_std(&path) {
                files.push(path);
            }
        }

        Ok(())
    }

    fn matches_extension_utf8(&self, path: &Utf8Path) -> bool {
        if self.extensions.is_empty() {
            return true;
        }
        path.extension()
            .is_some_and(|ext| self.extensions.iter().any(|e| e == ext))
    }

    fn matches_extension_std(&self, path: &std::path::Path) -> bool {
        if self.extensions.is_empty() {
            return true;
        }
        path.extension()
            .is_some_and(|ext| self.extensions.iter().any(|e| e.as_str() == ext))
    }

    fn is_excluded_dir(&self, path: &Utf8Path) -> bool {
        if self.excluded_dirs.is_empty() {
            return false;
        }
        path.file_name()
            .is_some_and(|name| self.excluded_dirs.contains(name))
    }

    fn is_excluded_dir_std(&self, path: &std::path::Path) -> bool {
        if self.excluded_dirs.is_empty() {
            return false;
        }
        path.file_name()
            .and_then(|name| name.to_str())
            .is_some_and(|name| self.excluded_dirs.contains(name))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_files_preset() {
        let walker = FileWalker::source_files();
        assert_eq!(walker.extensions, vec!["bt"]);
        assert!(walker.skip_symlinks);
        assert!(walker.strict_errors);
        assert!(walker.sort);
        assert!(walker.recursive);
        assert!(walker.excluded_dirs.is_empty());
        assert!(walker.max_files.is_none());
    }

    #[test]
    fn test_test_files_preset() {
        let walker = FileWalker::test_files();
        assert_eq!(walker.extensions, vec!["bt"]);
        assert!(walker.excluded_dirs.contains("fixtures"));
    }

    #[test]
    fn test_preload_files_preset() {
        let walker = FileWalker::preload_files(100);
        assert_eq!(walker.max_files, Some(100));
        assert!(!walker.strict_errors);
        assert!(walker.excluded_dirs.contains(".git"));
        assert!(walker.excluded_dirs.contains("target"));
        assert!(walker.excluded_dirs.contains("node_modules"));
        assert!(walker.excluded_dirs.contains("_build"));
    }

    #[test]
    fn test_walk_single_file() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        let file = dir_path.join("test.bt");
        fs::write(&file, "").unwrap();

        let walker = FileWalker::source_files();
        let files = walker.walk(&file).unwrap();
        assert_eq!(files, vec![file]);
    }

    #[test]
    fn test_walk_single_file_wrong_extension() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        let file = dir_path.join("test.txt");
        fs::write(&file, "").unwrap();

        let walker = FileWalker::source_files();
        let result = walker.walk(&file);
        assert!(result.is_err());
    }

    #[test]
    fn test_walk_directory_sorted() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        fs::write(dir_path.join("b.bt"), "").unwrap();
        fs::write(dir_path.join("a.bt"), "").unwrap();
        fs::write(dir_path.join("c.txt"), "").unwrap();

        let walker = FileWalker::source_files();
        let files = walker.walk(dir_path).unwrap();
        assert_eq!(files.len(), 2);
        assert!(files[0].as_str().ends_with("a.bt"));
        assert!(files[1].as_str().ends_with("b.bt"));
    }

    #[test]
    fn test_walk_recursive() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        fs::create_dir(dir_path.join("sub")).unwrap();
        fs::write(dir_path.join("a.bt"), "").unwrap();
        fs::write(dir_path.join("sub").join("b.bt"), "").unwrap();

        let walker = FileWalker::source_files();
        let files = walker.walk(dir_path).unwrap();
        assert_eq!(files.len(), 2);
    }

    #[test]
    fn test_walk_non_recursive() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        fs::create_dir(dir_path.join("sub")).unwrap();
        fs::write(dir_path.join("a.bt"), "").unwrap();
        fs::write(dir_path.join("sub").join("b.bt"), "").unwrap();

        let walker = FileWalker::source_files().recursive(false);
        let files = walker.walk(dir_path).unwrap();
        assert_eq!(files.len(), 1);
    }

    #[test]
    fn test_walk_excludes_dirs() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        fs::create_dir(dir_path.join("fixtures")).unwrap();
        fs::create_dir(dir_path.join("keep")).unwrap();
        fs::write(dir_path.join("fixtures").join("a.bt"), "").unwrap();
        fs::write(dir_path.join("keep").join("b.bt"), "").unwrap();

        let walker = FileWalker::test_files();
        let files = walker.walk(dir_path).unwrap();
        assert_eq!(files.len(), 1);
        assert!(files[0].as_str().ends_with("b.bt"));
    }

    #[test]
    fn test_walk_max_files() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        for i in 0..10 {
            fs::write(dir_path.join(format!("{i:02}.bt")), "").unwrap();
        }

        let walker = FileWalker::source_files().max_files(3);
        let files = walker.walk(dir_path).unwrap();
        // max_files caps collection; sort happens after, so we get some 3 files
        assert!(files.len() <= 3);
    }

    #[test]
    fn test_walk_nonexistent_path() {
        let walker = FileWalker::source_files();
        let result = walker.walk("/nonexistent/path");
        assert!(result.is_err());
    }

    #[test]
    fn test_walk_pathbuf() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(dir.path().join("test.bt"), "").unwrap();

        let walker = FileWalker::source_files();
        let files = walker.walk_pathbuf(dir.path()).unwrap();
        assert_eq!(files.len(), 1);
    }

    #[test]
    fn test_format_files_preset() {
        let walker = FileWalker::format_files();
        assert_eq!(walker.extensions, vec!["bt", "btscript"]);
    }

    #[test]
    fn test_btscript_files_preset() {
        let walker = FileWalker::btscript_files();
        assert_eq!(walker.extensions, vec!["btscript"]);
    }

    #[cfg(unix)]
    #[test]
    fn test_walk_skips_symlinks() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        let real_file = dir_path.join("real.bt");
        fs::write(&real_file, "").unwrap();
        std::os::unix::fs::symlink(&real_file, dir_path.join("link.bt")).unwrap();

        let walker = FileWalker::source_files();
        let files = walker.walk(dir_path).unwrap();
        assert_eq!(files.len(), 1);
        assert!(files[0].as_str().ends_with("real.bt"));
    }
}
