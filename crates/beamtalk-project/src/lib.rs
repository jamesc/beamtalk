// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Project root discovery utilities.
//!
//! **DDD Context:** Build System
//!
//! Shared project/workspace discovery logic for Beamtalk tooling (CLI, LSP, etc.):
//! project roots, package source files, `[dependencies]` parsing
//! ([`dependencies`]), the on-disk dependency graph ([`dependency_graph`]) and
//! the dependency-extension completeness rule ([`dependency_extensions`]).
//!
//! A standalone crate (ADR 0117 Decision step 2) depending only on
//! `beamtalk-core::file_walker`, so the boundary is `cargo`-enforced rather
//! than a convention within `beamtalk-core`.

pub mod dependencies;
pub mod dependency_extensions;
pub mod dependency_graph;
pub mod package;
pub mod package_name;

use std::fs;
use std::path::{Path, PathBuf};

/// Project marker files/directories, in priority order.
///
/// Earlier markers take priority: if `.beamtalk/` is found, discovery stops
/// even if `.git/` exists in a parent directory.
///
/// Note: `.beamtalk` is skipped when it matches the global config directory
/// (`~/.beamtalk`) to avoid collapsing unrelated directories under `$HOME`.
const PROJECT_MARKERS: &[&str] = &[".beamtalk", "beamtalk.toml", ".git"];

/// Returns the global Beamtalk config directory (`~/.beamtalk`), if resolvable.
fn global_beamtalk_dir() -> Option<PathBuf> {
    beamtalk_home::beamtalk_root_dir()
}

/// Check whether a `.beamtalk` marker refers to global config, not a project marker.
fn is_global_config_dir(candidate: &Path) -> bool {
    global_beamtalk_dir().is_some_and(|global| {
        if candidate == global {
            return true;
        }

        match (fs::canonicalize(candidate), fs::canonicalize(&global)) {
            (Ok(candidate), Ok(global)) => candidate == global,
            _ => false,
        }
    })
}

/// Discover the Beamtalk project root by walking up the directory tree.
///
/// Starts at `start_dir` and returns the first ancestor containing a recognized
/// project marker, or `start_dir` if no marker is found.
#[must_use]
pub fn discover_project_root(start_dir: &Path) -> PathBuf {
    let Ok(meta) = fs::metadata(start_dir) else {
        return start_dir.to_path_buf();
    };

    let mut current = if meta.is_dir() {
        start_dir.to_path_buf()
    } else {
        start_dir.parent().unwrap_or(start_dir).to_path_buf()
    };

    loop {
        // Check Beamtalk-specific markers first (highest priority).
        for marker in &PROJECT_MARKERS[..PROJECT_MARKERS.len() - 1] {
            let candidate = current.join(marker);
            if candidate.exists() {
                // Skip ~/.beamtalk — that's global config, not project root.
                if *marker == ".beamtalk" && is_global_config_dir(&candidate) {
                    continue;
                }
                return current;
            }
        }

        // Check generic fallback marker (.git)
        if let Some(marker) = PROJECT_MARKERS.last() {
            if current.join(marker).exists() {
                return current;
            }
        }

        if !current.pop() {
            break;
        }
    }

    start_dir.to_path_buf()
}

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_core::test_helpers::unique_temp_dir;
    use std::fs;

    #[test]
    fn discover_with_beamtalk_toml_marker() {
        let tmp = unique_temp_dir("beamtalk_core_project_root");
        let project = tmp.join("project");
        let subdir = project.join("src");
        fs::create_dir_all(&subdir).expect("create dirs");
        fs::File::create(project.join("beamtalk.toml")).expect("create manifest");

        let root = discover_project_root(&subdir);
        assert_eq!(root, project);

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn discover_falls_back_when_no_markers() {
        let tmp = unique_temp_dir("beamtalk_core_no_project_root");
        let dir = tmp.join("no-project");
        fs::create_dir_all(&dir).expect("create dirs");

        let root = discover_project_root(&dir);
        assert_eq!(root, dir);

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn discover_uses_parent_when_start_path_is_file() {
        let tmp = unique_temp_dir("beamtalk_core_file_start");
        let project = tmp.join("project");
        let src = project.join("src");
        fs::create_dir_all(&src).expect("create dirs");
        fs::File::create(project.join("beamtalk.toml")).expect("create manifest");

        let file_path = src.join("example.bt");
        fs::File::create(&file_path).expect("create source file");

        let root = discover_project_root(&file_path);
        assert_eq!(root, project);

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn discover_nonexistent_path_returns_input_path() {
        let tmp = unique_temp_dir("beamtalk_core_nonexistent_start");
        let project = tmp.join("project");
        fs::create_dir_all(&project).expect("create project dir");
        fs::File::create(project.join("beamtalk.toml")).expect("create manifest");

        let missing_path = project.join("does-not-exist").join("subdir");
        let root = discover_project_root(&missing_path);
        assert_eq!(root, missing_path);

        let _ = fs::remove_dir_all(&tmp);
    }

    // Marker-priority cases ported from the CLI's deleted pass-through
    // wrapper `commands/workspace/discovery.rs` (BT-4689).

    #[test]
    fn discover_with_git_marker() {
        let tmp = unique_temp_dir("beamtalk_project_root_git");
        let project = tmp.join("project");
        let subdir = project.join("src").join("lib");
        fs::create_dir_all(&subdir).expect("create dirs");
        fs::create_dir(project.join(".git")).expect("create .git");
        assert_eq!(discover_project_root(&subdir), project);
    }

    #[test]
    fn discover_with_beamtalk_dir_marker() {
        let tmp = unique_temp_dir("beamtalk_project_root_dir");
        let project = tmp.join("project");
        let subdir = project.join("src");
        fs::create_dir_all(&subdir).expect("create dirs");
        fs::create_dir(project.join(".beamtalk")).expect("create .beamtalk");
        assert_eq!(discover_project_root(&subdir), project);
    }

    #[test]
    fn beamtalk_marker_takes_priority_over_git_in_parent() {
        let tmp = unique_temp_dir("beamtalk_project_root_prio");
        let repo = tmp.join("repo");
        let project = repo.join("my-project");
        let subdir = project.join("src");
        fs::create_dir_all(&subdir).expect("create dirs");
        fs::create_dir(repo.join(".git")).expect("create .git");
        fs::create_dir(project.join(".beamtalk")).expect("create .beamtalk");
        assert_eq!(discover_project_root(&subdir), project);
    }

    #[test]
    fn git_not_escaped_by_beamtalk_dir_in_parent() {
        let tmp = unique_temp_dir("beamtalk_project_root_noescape");
        let home = tmp.join("home");
        let project = home.join("project");
        let subdir = project.join("src");
        fs::create_dir_all(&subdir).expect("create dirs");
        fs::create_dir(home.join(".beamtalk")).expect("create .beamtalk");
        fs::create_dir(project.join(".git")).expect("create .git");
        assert_eq!(discover_project_root(&subdir), project);
    }

    #[test]
    fn beamtalk_toml_wins_over_git_at_same_level() {
        let tmp = unique_temp_dir("beamtalk_project_root_same");
        let project = tmp.join("project");
        fs::create_dir_all(&project).expect("create dirs");
        fs::File::create(project.join("beamtalk.toml")).expect("create manifest");
        fs::create_dir(project.join(".git")).expect("create .git");
        assert_eq!(discover_project_root(&project), project);
    }
}
