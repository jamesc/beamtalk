// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk clean` — remove generated build artifacts.
//!
//! **DDD Context:** Build System
//!
//! Removes generated build output, never source. Every path is resolved
//! through [`BuildLayout`] so that `clean` always agrees with `build` about
//! what counts as an artifact, and so that nothing outside `_build/` can ever
//! be deleted (no hardcoded `_build` strings, no `src/`, `native/` source, or
//! `beamtalk.toml`).
//!
//! Scopes:
//! - default: the project's `_build/<profile>/` output plus the type cache.
//! - `--deps`: additionally the fetched/compiled dependency artifacts
//!   (`_build/deps/`).
//! - `--all`: the entire `_build/` directory, including any shared caches.
//! - `--dry-run`: list what would be removed without deleting anything.

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use tracing::{debug, info};

use super::build_layout::BuildLayout;

/// What `clean` is allowed to remove.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CleanScope {
    /// The project's build output for the active profile, plus the type cache.
    /// Never touches dependency caches.
    Default,
    /// `Default` plus fetched/compiled dependency artifacts (`_build/deps/`).
    Deps,
    /// The entire `_build/` directory, including any shared caches.
    All,
}

impl CleanScope {
    /// Derive the scope from the `--deps` / `--all` flags. `--all` wins.
    #[must_use]
    pub fn from_flags(deps: bool, all: bool) -> Self {
        if all {
            Self::All
        } else if deps {
            Self::Deps
        } else {
            Self::Default
        }
    }
}

/// Run `beamtalk clean` from the current working directory.
pub fn run(deps: bool, all: bool, dry_run: bool) -> Result<()> {
    let project_root = find_project_root()?;
    let layout = BuildLayout::new(&project_root);
    let scope = CleanScope::from_flags(deps, all);
    clean_with_layout(&layout, scope, dry_run)
}

/// Resolve the artifact paths for `scope` and remove (or, when `dry_run`,
/// list) every one that exists.
///
/// All returned paths are guaranteed to live under `layout.build_root()`.
fn clean_with_layout(layout: &BuildLayout, scope: CleanScope, dry_run: bool) -> Result<()> {
    let build_root = layout.build_root();
    let targets = clean_targets(layout, scope);

    // Safety net 1: never operate on a path outside `_build/`. This is a
    // lexical (path-component) check that can only fire on a programming
    // error, but the cost of being wrong is deleting source, so we assert it.
    for target in &targets {
        assert!(
            target.starts_with(&build_root),
            "clean target {target} escapes build root {build_root}"
        );
    }

    // Safety net 2: `starts_with` is purely lexical, so it cannot see a
    // symlinked `_build`. If `_build` itself is a symlink, removing a
    // *sub-path* (the Default/Deps scopes call `remove_dir_all` on
    // `_build/dev/`, …) would traverse the link and delete the contents of
    // its target — outside the project. Refuse rather than follow it. (The
    // `--all` scope is unaffected: it removes `_build` directly, and
    // `remove_path` uses `symlink_metadata`, so it unlinks the symlink itself
    // without traversing.)
    if scope != CleanScope::All && is_symlink(&build_root)? {
        miette::bail!(
            "Refusing to clean: '{build_root}' is a symlink. \
             Remove the symlink manually, or use `--all` to delete the link itself."
        );
    }

    // Use `symlink_metadata` (not `exists`, which resolves symlinks) so a
    // *dangling* `_build` symlink — whose target was wiped externally — still
    // counts as present and gets unlinked by `--all`, instead of being silently
    // skipped as "Nothing to clean" and breaking the next build.
    let existing: Vec<&Utf8PathBuf> = targets
        .iter()
        .filter(|t| std::fs::symlink_metadata(t.as_std_path()).is_ok())
        .collect();

    if existing.is_empty() {
        println!("Nothing to clean (no build artifacts found under {build_root}).");
        return Ok(());
    }

    if dry_run {
        println!("Would remove the following build artifacts:");
        for target in existing {
            println!("  {target}");
        }
        println!("(dry run — nothing was deleted)");
        return Ok(());
    }

    // Attempt every target even if one fails, so a permissions error on one
    // path does not silently leave the rest behind. Report each failure and
    // surface the first error once everything has been attempted.
    let mut first_error: Option<miette::Report> = None;
    for target in existing {
        debug!("Removing build artifact: {target}");
        match remove_path(target) {
            Ok(()) => println!("Removed {target}"),
            Err(e) => {
                eprintln!("Warning: failed to remove {target}: {e}");
                if first_error.is_none() {
                    first_error = Some(e);
                }
            }
        }
    }
    if let Some(err) = first_error {
        return Err(err);
    }

    info!(?scope, "Cleaned build artifacts");
    Ok(())
}

/// The set of artifact paths to remove for a given scope, most-specific first.
///
/// Paths are de-duplicated and never overlap: when a broader path (e.g. the
/// whole profile directory or the whole build root) covers a narrower one, the
/// narrower one is omitted so it is not "removed" twice.
fn clean_targets(layout: &BuildLayout, scope: CleanScope) -> Vec<Utf8PathBuf> {
    match scope {
        // `--all` removes the entire build root in one shot, which subsumes
        // the profile output, the type cache, deps, and any shared caches.
        CleanScope::All => vec![layout.build_root()],
        CleanScope::Default => vec![layout.profile_dir(), layout.type_cache_dir()],
        CleanScope::Deps => vec![
            layout.profile_dir(),
            layout.type_cache_dir(),
            layout.deps_dir(),
        ],
    }
}

/// Whether `path` exists and is a symbolic link (without following it).
///
/// A missing path is not a symlink. Any other stat error is surfaced so a
/// genuine I/O problem is not silently treated as "not a symlink".
fn is_symlink(path: &Utf8Path) -> Result<bool> {
    match std::fs::symlink_metadata(path) {
        Ok(meta) => Ok(meta.file_type().is_symlink()),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(false),
        Err(e) => Err(e)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to stat '{path}'")),
    }
}

/// Remove a file or directory tree at `path`.
fn remove_path(path: &Utf8Path) -> Result<()> {
    let metadata = std::fs::symlink_metadata(path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to stat '{path}'"))?;

    if metadata.is_dir() {
        std::fs::remove_dir_all(path)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to remove directory '{path}'"))
    } else {
        std::fs::remove_file(path)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to remove file '{path}'"))
    }
}

/// Find the project root by requiring a `beamtalk.toml` in the current
/// directory.
fn find_project_root() -> Result<Utf8PathBuf> {
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    /// Create a `BuildLayout` rooted at a fresh temp dir.
    fn layout_in(temp: &TempDir) -> BuildLayout {
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        BuildLayout::new(root)
    }

    /// Create a file (and any parent dirs) so we can observe deletion.
    fn touch(path: &Utf8Path) {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).unwrap();
        }
        fs::write(path, b"artifact").unwrap();
    }

    #[test]
    fn scope_from_flags_precedence() {
        assert_eq!(CleanScope::from_flags(false, false), CleanScope::Default);
        assert_eq!(CleanScope::from_flags(true, false), CleanScope::Deps);
        assert_eq!(CleanScope::from_flags(false, true), CleanScope::All);
        // --all wins over --deps
        assert_eq!(CleanScope::from_flags(true, true), CleanScope::All);
    }

    #[test]
    fn default_targets_cover_profile_and_type_cache_only() {
        let temp = TempDir::new().unwrap();
        let layout = layout_in(&temp);
        let targets = clean_targets(&layout, CleanScope::Default);
        assert_eq!(targets, vec![layout.profile_dir(), layout.type_cache_dir()]);
        // Default must never include the deps dir.
        assert!(!targets.contains(&layout.deps_dir()));
    }

    #[test]
    fn deps_targets_add_deps_dir() {
        let temp = TempDir::new().unwrap();
        let layout = layout_in(&temp);
        let targets = clean_targets(&layout, CleanScope::Deps);
        assert!(targets.contains(&layout.deps_dir()));
        assert!(targets.contains(&layout.profile_dir()));
    }

    #[test]
    fn all_targets_remove_whole_build_root() {
        let temp = TempDir::new().unwrap();
        let layout = layout_in(&temp);
        let targets = clean_targets(&layout, CleanScope::All);
        assert_eq!(targets, vec![layout.build_root()]);
    }

    #[test]
    fn all_targets_stay_within_build_root() {
        let temp = TempDir::new().unwrap();
        let layout = layout_in(&temp);
        for scope in [CleanScope::Default, CleanScope::Deps, CleanScope::All] {
            for target in clean_targets(&layout, scope) {
                assert!(
                    target.starts_with(layout.build_root()),
                    "{target} escapes build root for {scope:?}"
                );
            }
        }
    }

    #[test]
    fn dry_run_lists_but_does_not_delete() {
        let temp = TempDir::new().unwrap();
        let layout = layout_in(&temp);
        let beam = layout.ebin_dir().join("foo.beam");
        touch(&beam);

        clean_with_layout(&layout, CleanScope::Default, true).unwrap();

        // Dry run must leave everything in place.
        assert!(beam.exists(), "dry run deleted {beam}");
        assert!(layout.profile_dir().exists());
    }

    #[test]
    fn default_clean_removes_profile_and_type_cache() {
        let temp = TempDir::new().unwrap();
        let layout = layout_in(&temp);
        let beam = layout.ebin_dir().join("foo.beam");
        let hrl = layout.native_include_dir().join("beamtalk_classes.hrl");
        let cached = layout.type_cache_dir().join("spec.json");
        touch(&beam);
        touch(&hrl);
        touch(&cached);

        clean_with_layout(&layout, CleanScope::Default, false).unwrap();

        assert!(!layout.profile_dir().exists(), "profile dir should be gone");
        assert!(
            !layout.type_cache_dir().exists(),
            "type cache should be gone"
        );
    }

    #[test]
    fn default_clean_preserves_deps() {
        let temp = TempDir::new().unwrap();
        let layout = layout_in(&temp);
        touch(&layout.ebin_dir().join("foo.beam"));
        let dep_beam = layout.dep_ebin_dir("utils").join("utils.beam");
        touch(&dep_beam);

        clean_with_layout(&layout, CleanScope::Default, false).unwrap();

        // Default scope must not touch dependency caches.
        assert!(dep_beam.exists(), "default clean removed deps");
    }

    #[test]
    fn deps_clean_removes_deps() {
        let temp = TempDir::new().unwrap();
        let layout = layout_in(&temp);
        let dep_beam = layout.dep_ebin_dir("utils").join("utils.beam");
        touch(&dep_beam);

        clean_with_layout(&layout, CleanScope::Deps, false).unwrap();

        assert!(!layout.deps_dir().exists(), "deps dir should be gone");
    }

    #[test]
    fn deps_clean_preserves_source() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);

        // Source / config that must never be deleted.
        let src = root.join("src").join("Main.bt");
        let native_src = root.join("native").join("foo.erl");
        let manifest = root.join("beamtalk.toml");
        touch(&src);
        touch(&native_src);
        touch(&manifest);
        touch(&layout.dep_ebin_dir("utils").join("utils.beam"));

        clean_with_layout(&layout, CleanScope::Deps, false).unwrap();

        assert!(src.exists(), "src/ was deleted");
        assert!(native_src.exists(), "native/ source was deleted");
        assert!(manifest.exists(), "beamtalk.toml was deleted");
    }

    #[test]
    fn all_clean_removes_entire_build_root() {
        let temp = TempDir::new().unwrap();
        let layout = layout_in(&temp);
        touch(&layout.ebin_dir().join("foo.beam"));
        touch(&layout.dep_ebin_dir("utils").join("utils.beam"));
        touch(&layout.type_cache_dir().join("spec.json"));

        clean_with_layout(&layout, CleanScope::All, false).unwrap();

        assert!(!layout.build_root().exists(), "build root should be gone");
    }

    #[test]
    fn all_clean_preserves_source() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);
        let src = root.join("src").join("Main.bt");
        let manifest = root.join("beamtalk.toml");
        touch(&src);
        touch(&manifest);
        touch(&layout.ebin_dir().join("foo.beam"));

        clean_with_layout(&layout, CleanScope::All, false).unwrap();

        assert!(src.exists(), "src/ was deleted by --all");
        assert!(manifest.exists(), "beamtalk.toml was deleted by --all");
    }

    #[cfg(unix)]
    #[test]
    fn default_clean_refuses_symlinked_build_root() {
        // A symlinked `_build` must not be traversed by Default/Deps removals —
        // doing so would delete the contents of the link target (outside the
        // project). The command should refuse instead.
        let project = TempDir::new().unwrap();
        let external = TempDir::new().unwrap();

        let project_root = Utf8PathBuf::from_path_buf(project.path().to_path_buf()).unwrap();
        let external_root = Utf8PathBuf::from_path_buf(external.path().to_path_buf()).unwrap();

        // Put a real artifact tree (and a sentinel) inside the external target.
        let sentinel = external_root.join("dev").join("ebin").join("foo.beam");
        touch(&sentinel);

        // Make `<project>/_build` a symlink to the external directory.
        let layout = BuildLayout::new(&project_root);
        std::os::unix::fs::symlink(
            external_root.as_std_path(),
            layout.build_root().as_std_path(),
        )
        .unwrap();

        let result = clean_with_layout(&layout, CleanScope::Default, false);
        assert!(result.is_err(), "should refuse a symlinked build root");
        // The external target's contents must be untouched.
        assert!(
            sentinel.exists(),
            "symlink target was traversed and deleted"
        );
    }

    #[cfg(unix)]
    #[test]
    fn all_clean_removes_symlinked_build_root_without_traversing() {
        // `--all` removes `_build` directly via `symlink_metadata`, so it
        // unlinks the symlink itself and never touches the link target.
        let project = TempDir::new().unwrap();
        let external = TempDir::new().unwrap();

        let project_root = Utf8PathBuf::from_path_buf(project.path().to_path_buf()).unwrap();
        let external_root = Utf8PathBuf::from_path_buf(external.path().to_path_buf()).unwrap();

        let sentinel = external_root.join("keep.txt");
        touch(&sentinel);

        let layout = BuildLayout::new(&project_root);
        std::os::unix::fs::symlink(
            external_root.as_std_path(),
            layout.build_root().as_std_path(),
        )
        .unwrap();

        clean_with_layout(&layout, CleanScope::All, false).unwrap();

        // The symlink is gone, but its target's contents survive.
        assert!(!layout.build_root().exists(), "symlink should be unlinked");
        assert!(sentinel.exists(), "--all traversed the symlink target");
    }

    #[test]
    fn clean_is_idempotent_when_nothing_exists() {
        let temp = TempDir::new().unwrap();
        let layout = layout_in(&temp);
        // No _build at all — should succeed as a no-op.
        clean_with_layout(&layout, CleanScope::Default, false).unwrap();
        clean_with_layout(&layout, CleanScope::All, true).unwrap();
        assert!(!layout.build_root().exists());
    }
}
