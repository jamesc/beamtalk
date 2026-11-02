// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Configurable file-system walker for collecting source files.
//!
//! **DDD Context:** Compilation (shared infrastructure)
//!
//! Replaces the 7+ separate file-collection implementations that were scattered
//! across the codebase with a single, builder-pattern `FileWalker`.

use camino::{Utf8Path, Utf8PathBuf};
use ignore::Match;
use ignore::gitignore::{Gitignore, GitignoreBuilder};
use miette::{IntoDiagnostic, WrapErr};
use std::collections::HashSet;
use std::fs;
use std::path::Path;
use tracing::warn;

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
    /// Whether to skip symlinks (default: false — symlinks to files are
    /// followed, like `erlc` and most build tools; symlinked directories
    /// are never traversed).
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
    /// Whether to skip paths ignored by `.gitignore` files (default: true).
    /// See [`FileWalker::respect_gitignore`] for the exact rules.
    respect_gitignore: bool,
}

impl Default for FileWalker {
    fn default() -> Self {
        Self {
            extensions: Vec::new(),
            skip_symlinks: false,
            excluded_dirs: HashSet::new(),
            max_files: None,
            strict_errors: true,
            sort: true,
            recursive: true,
            respect_gitignore: true,
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

    /// Whether to skip symlinks. Default: `false`, meaning symlinks to
    /// files are followed (collected under the link's own path when the
    /// link's name matches the extension filter). Symlinked directories
    /// are never traversed either way. Pass `true` to ignore all symlinks.
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

    /// Whether to skip paths ignored by `.gitignore` files. Default: `true`
    /// (BT-4135), so every preset — and therefore `beamtalk lint`, `build`,
    /// `fmt`, `test`, `doc`, MCP lint and LSP preload, which all walk through
    /// this type — skips ignored trees such as a `.worktrees/` directory
    /// holding nested checkouts, instead of reporting each nested copy's
    /// files as duplicates of the main tree's.
    ///
    /// Rules, following git's (and ripgrep's) semantics:
    /// - Every `.gitignore` in the walk root and the directories below it
    ///   applies to its own subtree, deeper files taking precedence (`!`
    ///   negations included).
    /// - When the walk root lies inside a git repository (the root itself or
    ///   an ancestor contains `.git`), the `.gitignore` files of the
    ///   directories from the repository root down to the walk root apply
    ///   too, as does `.git/info/exclude`. Without a repository, only the
    ///   walk root's own and nested `.gitignore` files apply.
    /// - Rules are matched against each entry the walk visits, never against
    ///   the walk root itself: an explicitly requested directory is always
    ///   walked even if an ancestor rule ignores it (e.g. a dependency
    ///   checkout under a gitignored `_build/`).
    /// - The user's global excludes file (`core.excludesFile`) is not
    ///   consulted: it is per-machine configuration, and which project files
    ///   a command sees must not differ between machines.
    #[must_use]
    pub fn respect_gitignore(mut self, respect: bool) -> Self {
        self.respect_gitignore = respect;
        self
    }

    // ── Preset constructors ─────────────────────────────────────────────

    /// Preset for collecting `.bt` source files.
    ///
    /// Recursive, sorted, strict errors. Follows symlinks to files
    /// (BT-4105) but never traverses into symlinked directories. Excludes
    /// `.git`/`target`/`node_modules`/`_build` — `_build/deps/`
    /// holds full source checkouts for git/registry dependencies (a path
    /// dependency's source stays wherever the path points, outside
    /// `_build`, but a git/registry dependency is cloned straight into
    /// `_build/deps/<name>/`, `.bt` sources included), so any caller that
    /// walks a project root or bare directory — `beamtalk lint`,
    /// `beamtalk build` on a manifest-less directory, dependency-source
    /// collection when a dependency has no `src/` — would otherwise treat a
    /// dependency's own vendored sources as first-party project files: lint
    /// diagnostics against code the user doesn't own, and (for a name that
    /// also appears in the project's own dependency-exported alias/protocol
    /// info, seeded independently via the proper dependency-resolution
    /// path) a spurious duplicate declaration under a different `package`
    /// stamp, which `AliasRegistry::add_pre_loaded`'s identity check cannot
    /// recognise as the same physical declaration.
    pub fn source_files() -> Self {
        Self::new()
            .extensions(&["bt"])
            .exclude_dirs(&[".git", "target", "node_modules", "_build"])
    }

    /// Preset for collecting `.bt` test files.
    ///
    /// Same as `source_files()` but also excludes `fixtures/`. `exclude_dirs`
    /// replaces rather than extends the excluded-dir set, so this repeats
    /// `source_files()`'s list explicitly rather than building on top of it.
    pub fn test_files() -> Self {
        Self::new().extensions(&["bt"]).exclude_dirs(&[
            ".git",
            "target",
            "node_modules",
            "_build",
            "fixtures",
        ])
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
    /// Same as `source_files()` — recursive, sorted, strict errors,
    /// excluding `.git`/`target`/`node_modules`/`_build` and gitignored
    /// paths (see [`FileWalker::respect_gitignore`]).
    pub fn lint_files() -> Self {
        Self::source_files()
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
        // `None` (gitignore handling off, or a root that cannot be
        // canonicalised) means nothing is ignored.
        let canonical_root = if self.respect_gitignore {
            fs::canonicalize(path).ok()
        } else {
            None
        };
        let mut ignores = match &canonical_root {
            Some(root) => IgnoreStack::above_walk_root(root),
            None => IgnoreStack::default(),
        };
        self.walk_dir(path, canonical_root.as_deref(), &mut ignores, &mut files)?;

        if self.sort {
            files.sort();
        }

        Ok(files)
    }

    /// Walk from `path`, returning `std::path::PathBuf` results.
    ///
    /// Used by consumers that don't require UTF-8 paths (LSP, MCP, build-corpus).
    /// Delegates to [`walk`][FileWalker::walk] after converting to a UTF-8 path.
    /// All beamtalk source files must be valid UTF-8, so non-UTF-8 paths produce
    /// an early error.
    ///
    /// # Errors
    ///
    /// Returns an error if the path is not valid UTF-8, does not exist, has the
    /// wrong extension, or if a directory cannot be read (when `strict_errors` is true).
    pub fn walk_pathbuf(&self, path: &std::path::Path) -> miette::Result<Vec<std::path::PathBuf>> {
        let utf8 = camino::Utf8Path::from_path(path)
            .ok_or_else(|| miette::miette!("Non-UTF-8 path '{}'", path.display()))?;
        Ok(self
            .walk(utf8)?
            .into_iter()
            .map(camino::Utf8PathBuf::into_std_path_buf)
            .collect())
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

    fn budget_exhausted(&self, files: &[impl Sized]) -> bool {
        self.max_files.is_some_and(|max| files.len() >= max)
    }

    /// Walk `dir`, whose canonical path is `canonical_dir` when gitignore
    /// handling is active, pushing its own `.gitignore` (if any) for the
    /// duration of its subtree.
    fn walk_dir(
        &self,
        dir: &Utf8Path,
        canonical_dir: Option<&Path>,
        ignores: &mut IgnoreStack,
        files: &mut Vec<Utf8PathBuf>,
    ) -> miette::Result<()> {
        if self.budget_exhausted(files) {
            return Ok(());
        }
        let pushed = canonical_dir.is_some_and(|canonical| ignores.push_dir(canonical));
        let result = self.walk_dir_entries(dir, canonical_dir, ignores, files);
        if pushed {
            ignores.pop_dir();
        }
        result
    }

    fn walk_dir_entries(
        &self,
        dir: &Utf8Path,
        canonical_dir: Option<&Path>,
        ignores: &mut IgnoreStack,
        files: &mut Vec<Utf8PathBuf>,
    ) -> miette::Result<()> {
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

            let entry_path = Utf8PathBuf::from_path_buf(entry.path())
                .map_err(|_| miette::miette!("Non-UTF-8 path in '{}'", dir))?;

            let canonical_entry = canonical_dir.map(|dir| dir.join(entry.file_name()));
            let is_ignored = |is_dir: bool| {
                canonical_entry
                    .as_deref()
                    .is_some_and(|path| ignores.is_ignored(path, is_dir))
            };

            if file_type.is_symlink() {
                if !is_ignored(false) {
                    self.collect_symlink_target(&entry_path, files)?;
                }
                continue;
            }

            if file_type.is_dir() {
                if self.is_excluded_dir(&entry_path) || is_ignored(true) {
                    continue;
                }
                if self.recursive {
                    self.walk_dir(&entry_path, canonical_entry.as_deref(), ignores, files)?;
                }
            } else if file_type.is_file()
                && self.matches_extension_utf8(&entry_path)
                && !is_ignored(false)
            {
                files.push(entry_path);
            }
        }

        Ok(())
    }

    /// Resolve a symlink entry discovered during a directory walk (BT-4105).
    ///
    /// When `skip_symlinks` is set, symlinks are ignored entirely. Otherwise
    /// symlinks to files are followed — like `erlc` and most build tools —
    /// and collected under the link's own path when the link's name matches
    /// the extension filter, so module-name derivation and diagnostics keep
    /// referring to the path the user sees. Symlinks to directories are
    /// never traversed (recursing into them risks infinite cycles), and
    /// other non-file targets are ignored. A dangling symlink whose name
    /// matches the filter is a read error: fatal under `strict_errors`,
    /// silently skipped otherwise.
    fn collect_symlink_target(
        &self,
        link_path: &Utf8Path,
        files: &mut Vec<Utf8PathBuf>,
    ) -> miette::Result<()> {
        if self.skip_symlinks || !self.matches_extension_utf8(link_path) {
            return Ok(());
        }
        match fs::metadata(link_path) {
            Ok(target) if target.is_file() => {
                files.push(link_path.to_path_buf());
                Ok(())
            }
            Ok(_) => Ok(()),
            Err(err) => {
                if self.strict_errors {
                    miette::bail!("'{link_path}' is a dangling symlink: {err}");
                }
                Ok(())
            }
        }
    }

    fn matches_extension_utf8(&self, path: &Utf8Path) -> bool {
        if self.extensions.is_empty() {
            return true;
        }
        path.extension()
            .is_some_and(|ext| self.extensions.iter().any(|e| e == ext))
    }

    fn is_excluded_dir(&self, path: &Utf8Path) -> bool {
        if self.excluded_dirs.is_empty() {
            return false;
        }
        path.file_name()
            .is_some_and(|name| self.excluded_dirs.contains(name))
    }
}

/// Finds a project's `.bt` source files: `src/` if it exists, else
/// `project_root` itself — excluding `project_root/stubs/`, which the
/// native-stub pipeline scans separately and never compiles as ordinary
/// source (a `declare native:` block there is a hard error everywhere
/// else).
///
/// Without the exclusion, a project with no `src/` directory would fall back
/// to searching the whole project root, sweeping `stubs/*.bt` into the
/// normal compile/lint pipeline.
///
/// Shared by `beamtalk build`/`lint`/`test` source discovery, dependency
/// source indexing, and the compiler port's project-scope alias scan
/// (BT-4322) — one rule, one implementation, so a change here (a new
/// excluded dir, a new layout) applies to every surface at once.
///
/// # Errors
///
/// Returns an error if the search directory does not exist or cannot be read.
pub fn collect_project_source_files(project_root: &Utf8Path) -> miette::Result<Vec<Utf8PathBuf>> {
    let src_dir = project_root.join("src");
    let search_dir = if src_dir.exists() {
        src_dir
    } else {
        project_root.to_path_buf()
    };

    let files = FileWalker::source_files().walk(&search_dir)?;
    let stubs_dir = project_root.join("stubs");
    Ok(files
        .into_iter()
        .filter(|f| !f.starts_with(&stubs_dir))
        .collect())
}

/// The `.gitignore` matchers in effect at the current point of a walk
/// (BT-4135), outermost first. Matchers are rooted at canonical directory
/// paths, so entries are matched by their canonical path rather than the
/// walk's own (possibly relative) spelling of it.
#[derive(Debug, Default)]
struct IgnoreStack {
    matchers: Vec<Gitignore>,
}

impl IgnoreStack {
    /// Seed the stack with the rules that apply from above a walk rooted at
    /// `canonical_root`: `.git/info/exclude` and the `.gitignore` files of
    /// the directories from the enclosing repository root down to, but not
    /// including, the walk root (whose own `.gitignore` `walk_dir` pushes).
    fn above_walk_root(canonical_root: &Path) -> Self {
        let mut stack = Self::default();
        let Some(repo_root) = canonical_root
            .ancestors()
            .find(|dir| dir.join(".git").exists())
        else {
            return stack;
        };
        stack.push_file(
            repo_root,
            &repo_root.join(".git").join("info").join("exclude"),
        );
        let mut ancestors: Vec<&Path> = canonical_root
            .ancestors()
            .skip(1)
            .take_while(|dir| dir.starts_with(repo_root))
            .collect();
        ancestors.reverse();
        for dir in ancestors {
            stack.push_dir(dir);
        }
        stack
    }

    /// Push `dir`'s `.gitignore`, returning whether one was pushed.
    fn push_dir(&mut self, dir: &Path) -> bool {
        self.push_file(dir, &dir.join(".gitignore"))
    }

    /// Push a matcher rooted at `root` built from the ignore file `file`.
    /// Returns `false` (pushing nothing) when the file is absent, has no
    /// rules, or cannot be parsed (logged).
    fn push_file(&mut self, root: &Path, file: &Path) -> bool {
        if !file.is_file() {
            return false;
        }
        let mut builder = GitignoreBuilder::new(root);
        if let Some(err) = builder.add(file) {
            warn!("ignoring malformed rules in '{}': {err}", file.display());
        }
        match builder.build() {
            Ok(matcher) if !matcher.is_empty() => {
                self.matchers.push(matcher);
                true
            }
            Ok(_) => false,
            Err(err) => {
                warn!("failed to load '{}': {err}", file.display());
                false
            }
        }
    }

    fn pop_dir(&mut self) {
        self.matchers.pop();
    }

    /// Whether the entry at canonical `path` is ignored. The innermost
    /// matcher with an opinion wins, so a nested `!pattern` re-includes what
    /// an outer file ignored.
    fn is_ignored(&self, path: &Path, is_dir: bool) -> bool {
        for matcher in self.matchers.iter().rev() {
            match matcher.matched(path, is_dir) {
                Match::Ignore(_) => return true,
                Match::Whitelist(_) => return false,
                Match::None => {}
            }
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_files_preset() {
        let walker = FileWalker::source_files();
        assert_eq!(walker.extensions, vec!["bt"]);
        assert!(!walker.skip_symlinks);
        assert!(walker.strict_errors);
        assert!(walker.sort);
        assert!(walker.recursive);
        assert!(walker.excluded_dirs.contains(".git"));
        assert!(walker.excluded_dirs.contains("target"));
        assert!(walker.excluded_dirs.contains("node_modules"));
        assert!(walker.excluded_dirs.contains("_build"));
        assert!(walker.max_files.is_none());
    }

    /// A directory walk over a project root must never treat a
    /// git/registry dependency's vendored checkout under `_build/deps/` as
    /// first-party source — see `source_files()`'s doc for the false
    /// alias-collision/spurious-lint-diagnostic consequences when it does.
    #[test]
    fn test_source_files_excludes_build_dir() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        fs::create_dir_all(dir_path.join("_build").join("deps").join("somedep")).unwrap();
        fs::write(dir_path.join("a.bt"), "").unwrap();
        fs::write(
            dir_path
                .join("_build")
                .join("deps")
                .join("somedep")
                .join("vendored.bt"),
            "",
        )
        .unwrap();

        let files = FileWalker::source_files().walk(dir_path).unwrap();
        assert_eq!(files.len(), 1);
        assert!(files[0].as_str().ends_with("a.bt"));
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
    fn test_walk_follows_symlink_to_file() {
        // BT-4105: a symlinked `.bt` file must be collected, not silently
        // skipped. The link's own path is reported so module-name
        // derivation keeps working as if it were a regular file.
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        let real_file = dir_path.join("real.bt");
        fs::write(&real_file, "").unwrap();
        let link = dir_path.join("link.bt");
        std::os::unix::fs::symlink(&real_file, &link).unwrap();

        let files = FileWalker::source_files().walk(dir_path).unwrap();
        assert_eq!(files.len(), 2);
        assert_eq!(files[0].as_str(), link.as_str());
        assert!(files[1].as_str().ends_with("real.bt"));
    }

    #[cfg(unix)]
    #[test]
    fn test_walk_symlink_outside_tree_is_followed_under_link_name() {
        // BT-4105 repro shape: the link lives in the walked tree while its
        // target lives elsewhere; the extension filter applies to the
        // link's name, not the target's.
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        let outside = dir_path.join("elsewhere");
        fs::create_dir(&outside).unwrap();
        fs::write(outside.join("spike_handler.txt"), "").unwrap();
        let link = dir_path.join("spike_handler.bt");
        std::os::unix::fs::symlink(outside.join("spike_handler.txt"), &link).unwrap();
        // A non-matching link name is not collected even when its target
        // has a matching extension.
        fs::write(outside.join("other.bt"), "").unwrap();
        std::os::unix::fs::symlink(outside.join("other.bt"), dir_path.join("other.txt")).unwrap();

        let files = FileWalker::source_files().walk(dir_path).unwrap();
        assert_eq!(files, vec![outside.join("other.bt"), link]);
    }

    #[cfg(unix)]
    #[test]
    fn test_walk_skip_symlinks_opt_out() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        let real_file = dir_path.join("real.bt");
        fs::write(&real_file, "").unwrap();
        std::os::unix::fs::symlink(&real_file, dir_path.join("link.bt")).unwrap();

        let files = FileWalker::source_files()
            .skip_symlinks(true)
            .walk(dir_path)
            .unwrap();
        assert_eq!(files.len(), 1);
        assert!(files[0].as_str().ends_with("real.bt"));
    }

    #[cfg(unix)]
    #[test]
    fn test_walk_never_traverses_symlinked_dir() {
        // Symlinked directories are never descended into, so a cycle
        // (link back to an ancestor) cannot cause infinite recursion.
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        let sub = dir_path.join("sub");
        fs::create_dir(&sub).unwrap();
        fs::write(sub.join("inner.bt"), "").unwrap();
        // Extension-matching names so the resolve-then-skip-dir branch is
        // exercised, not just the name filter: `cycle.bt` links back to an
        // ancestor, which naive traversal would follow forever.
        std::os::unix::fs::symlink(dir_path, sub.join("cycle.bt")).unwrap();
        std::os::unix::fs::symlink(&sub, dir_path.join("linked_sub.bt")).unwrap();

        let files = FileWalker::source_files().walk(dir_path).unwrap();
        assert_eq!(files.len(), 1);
        assert!(files[0].as_str().ends_with("inner.bt"));
    }

    #[cfg(unix)]
    #[test]
    fn test_walk_dangling_symlink_strict_errors() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        std::os::unix::fs::symlink(dir_path.join("missing.bt"), dir_path.join("dangling.bt"))
            .unwrap();

        let result = FileWalker::source_files().walk(dir_path);
        let err = format!("{:?}", result.unwrap_err());
        assert!(err.contains("dangling symlink"), "unexpected error: {err}");
    }

    #[cfg(unix)]
    #[test]
    fn test_walk_dangling_symlink_lenient_skips() {
        let dir = tempfile::tempdir().unwrap();
        let dir_path = Utf8Path::from_path(dir.path()).unwrap();
        fs::write(dir_path.join("real.bt"), "").unwrap();
        std::os::unix::fs::symlink(dir_path.join("missing.bt"), dir_path.join("dangling.bt"))
            .unwrap();
        // A self-referential link (ELOOP) resolves the same way.
        std::os::unix::fs::symlink(dir_path.join("loop.bt"), dir_path.join("loop.bt")).unwrap();

        let files = FileWalker::source_files()
            .strict_errors(false)
            .walk(dir_path)
            .unwrap();
        assert_eq!(files.len(), 1);
        assert!(files[0].as_str().ends_with("real.bt"));
    }

    // ── .gitignore handling (BT-4135) ───────────────────────────────────

    fn write_file(path: &Utf8Path, content: &str) {
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, content).unwrap();
    }

    /// Relative paths (`/`-separated) of `files` under `root`, sorted.
    fn relative_names(root: &Utf8Path, files: &[Utf8PathBuf]) -> Vec<String> {
        let mut names: Vec<String> = files
            .iter()
            .map(|f| {
                f.strip_prefix(root)
                    .unwrap()
                    .components()
                    .map(|c| c.as_str())
                    .collect::<Vec<_>>()
                    .join("/")
            })
            .collect();
        names.sort();
        names
    }

    /// The reported bug: a gitignored `.worktrees/` holding a nested
    /// checkout must not be walked, or every project command reports each
    /// file twice.
    #[test]
    fn test_walk_skips_gitignored_nested_checkout() {
        let dir = tempfile::tempdir().unwrap();
        let root = Utf8Path::from_path(dir.path()).unwrap();
        fs::create_dir(root.join(".git")).unwrap();
        write_file(&root.join(".gitignore"), ".worktrees/\n*.gen.bt\n");
        write_file(&root.join("src/main.bt"), "");
        write_file(&root.join("src/skip.gen.bt"), "");
        // A nested checkout: its `.git` is a file, as for `git worktree`.
        write_file(&root.join(".worktrees/feature/.git"), "gitdir: elsewhere\n");
        write_file(&root.join(".worktrees/feature/src/main.bt"), "");

        let files = FileWalker::source_files().walk(root).unwrap();
        assert_eq!(relative_names(root, &files), vec!["src/main.bt"]);
    }

    #[test]
    fn test_walk_respect_gitignore_opt_out() {
        let dir = tempfile::tempdir().unwrap();
        let root = Utf8Path::from_path(dir.path()).unwrap();
        write_file(&root.join(".gitignore"), "ignored/\n");
        write_file(&root.join("a.bt"), "");
        write_file(&root.join("ignored/b.bt"), "");

        let files = FileWalker::source_files()
            .respect_gitignore(false)
            .walk(root)
            .unwrap();
        assert_eq!(relative_names(root, &files), vec!["a.bt", "ignored/b.bt"]);
    }

    /// A nested `.gitignore` applies to its own subtree only, and a deeper
    /// `!` negation re-includes what an outer file ignored.
    #[test]
    fn test_walk_nested_gitignore_and_negation() {
        let dir = tempfile::tempdir().unwrap();
        let root = Utf8Path::from_path(dir.path()).unwrap();
        write_file(&root.join(".gitignore"), "*.gen.bt\n");
        write_file(&root.join("lib/.gitignore"), "!keep.gen.bt\nlocal/\n");
        write_file(&root.join("top.gen.bt"), "");
        write_file(&root.join("local/a.bt"), "");
        write_file(&root.join("lib/keep.gen.bt"), "");
        write_file(&root.join("lib/drop.gen.bt"), "");
        write_file(&root.join("lib/local/b.bt"), "");

        let files = FileWalker::source_files().walk(root).unwrap();
        assert_eq!(
            relative_names(root, &files),
            vec!["lib/keep.gen.bt", "local/a.bt"]
        );
    }

    /// Inside a repository, `.gitignore` files above the walk root (up to
    /// the repository root) and `.git/info/exclude` apply too — but the
    /// walk root itself is always walked, even when an ancestor rule
    /// ignores it.
    #[test]
    fn test_walk_applies_ancestor_rules_inside_repo_but_not_to_root() {
        let dir = tempfile::tempdir().unwrap();
        let repo = Utf8Path::from_path(dir.path()).unwrap();
        write_file(&repo.join(".git/info/exclude"), "excluded.bt\n");
        write_file(&repo.join(".gitignore"), "scratch/\n_vendor/\n");
        write_file(&repo.join("pkg/a.bt"), "");
        write_file(&repo.join("pkg/excluded.bt"), "");
        write_file(&repo.join("pkg/scratch/b.bt"), "");
        write_file(&repo.join("_vendor/dep/c.bt"), "");

        let pkg = repo.join("pkg");
        let files = FileWalker::source_files().walk(&pkg).unwrap();
        assert_eq!(relative_names(&pkg, &files), vec!["a.bt"]);

        let vendor = repo.join("_vendor");
        let files = FileWalker::source_files().walk(&vendor).unwrap();
        assert_eq!(relative_names(&vendor, &files), vec!["dep/c.bt"]);
    }

    /// Outside any repository, `.gitignore` files above the walk root do
    /// not apply.
    #[test]
    fn test_walk_ignores_ancestor_gitignore_outside_repo() {
        let dir = tempfile::tempdir().unwrap();
        let outer = Utf8Path::from_path(dir.path()).unwrap();
        write_file(&outer.join(".gitignore"), "*.bt\n");
        write_file(&outer.join("proj/a.bt"), "");

        let proj = outer.join("proj");
        let files = FileWalker::source_files().walk(&proj).unwrap();
        assert_eq!(relative_names(&proj, &files), vec!["a.bt"]);
    }
}
