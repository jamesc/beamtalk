// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Automatic workspace discovery based on project directory.
//!
//! **DDD Context:** Workspace Management
//!
//! Walks up the directory tree from the current working directory to find
//! the project root. Project roots are identified by marker files/directories:
//!
//! - `.beamtalk/` - Beamtalk project configuration directory
//! - `beamtalk.toml` - Beamtalk project manifest
//! - `.git/` - Git repository root (fallback marker)
//!
//! If no marker is found, falls back to the starting directory.
//!
//! # Examples
//!
//! ```text
//! /home/user/project/src/lib/
//!   → walks up to /home/user/project/ (has .git/)
//!   → workspace ID = SHA256("/home/user/project")[:12]
//! ```

use std::path::{Path, PathBuf};

/// Discover the project root by walking up the directory tree.
///
/// Starts from `start_dir` and checks each ancestor for project markers.
/// Returns the first directory containing a marker, or `start_dir` if
/// no marker is found.
///
/// At each directory level, Beamtalk-specific markers (`.beamtalk/`,
/// `beamtalk.toml`) take priority over generic markers (`.git/`).
/// The search stops at the innermost directory containing any marker.
pub fn discover_project_root(start_dir: &Path) -> PathBuf {
    beamtalk_core::project::discover_project_root(start_dir)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_discover_with_git_marker() {
        let tmp = TempDir::new().unwrap();
        let project = tmp.path().join("project");
        let subdir = project.join("src").join("lib");
        fs::create_dir_all(&subdir).unwrap();
        fs::create_dir(project.join(".git")).unwrap();

        let root = discover_project_root(&subdir);
        assert_eq!(root, project);
    }

    #[test]
    fn test_discover_with_beamtalk_dir_marker() {
        let tmp = TempDir::new().unwrap();
        let project = tmp.path().join("project");
        let subdir = project.join("src");
        fs::create_dir_all(&subdir).unwrap();
        fs::create_dir(project.join(".beamtalk")).unwrap();

        let root = discover_project_root(&subdir);
        assert_eq!(root, project);
    }

    #[test]
    fn test_discover_with_beamtalk_toml_marker() {
        let tmp = TempDir::new().unwrap();
        let project = tmp.path().join("project");
        let subdir = project.join("src");
        fs::create_dir_all(&subdir).unwrap();
        fs::File::create(project.join("beamtalk.toml")).unwrap();

        let root = discover_project_root(&subdir);
        assert_eq!(root, project);
    }

    #[test]
    fn test_beamtalk_marker_takes_priority_over_git() {
        // Git repo root is parent, but beamtalk marker is in subdirectory
        let tmp = TempDir::new().unwrap();
        let repo = tmp.path().join("repo");
        let project = repo.join("my-project");
        let subdir = project.join("src");
        fs::create_dir_all(&subdir).unwrap();
        fs::create_dir(repo.join(".git")).unwrap();
        fs::create_dir(project.join(".beamtalk")).unwrap();

        let root = discover_project_root(&subdir);
        assert_eq!(root, project, "Beamtalk marker should win over .git");
    }

    #[test]
    fn test_fallback_to_start_dir_when_no_markers() {
        let tmp = TempDir::new().unwrap();
        let dir = tmp.path().join("no-project");
        fs::create_dir_all(&dir).unwrap();

        let root = discover_project_root(&dir);
        assert_eq!(root, dir);
    }

    #[test]
    fn test_discover_at_project_root_directly() {
        let tmp = TempDir::new().unwrap();
        let project = tmp.path().join("project");
        fs::create_dir_all(&project).unwrap();
        fs::create_dir(project.join(".git")).unwrap();

        let root = discover_project_root(&project);
        assert_eq!(root, project);
    }

    #[test]
    fn test_discover_deeply_nested() {
        let tmp = TempDir::new().unwrap();
        let project = tmp.path().join("project");
        let deep = project.join("a").join("b").join("c").join("d");
        fs::create_dir_all(&deep).unwrap();
        fs::create_dir(project.join(".git")).unwrap();

        let root = discover_project_root(&deep);
        assert_eq!(root, project);
    }

    #[test]
    fn test_discover_with_both_git_and_beamtalk_in_same_dir() {
        let tmp = TempDir::new().unwrap();
        let project = tmp.path().join("project");
        let subdir = project.join("src");
        fs::create_dir_all(&subdir).unwrap();
        fs::create_dir(project.join(".git")).unwrap();
        fs::create_dir(project.join(".beamtalk")).unwrap();

        let root = discover_project_root(&subdir);
        assert_eq!(root, project, "Should find project with both markers");
    }

    #[test]
    fn test_git_not_escaped_by_beamtalk_in_parent() {
        // Regression test: .beamtalk in parent should NOT override .git in project.
        // User has .beamtalk/ in home dir, .git in their project.
        let tmp = TempDir::new().unwrap();
        let home = tmp.path().join("home");
        let project = home.join("project");
        let subdir = project.join("src");
        fs::create_dir_all(&subdir).unwrap();
        fs::create_dir(home.join(".beamtalk")).unwrap();
        fs::create_dir(project.join(".git")).unwrap();

        let root = discover_project_root(&subdir);
        assert_eq!(
            root, project,
            "Should stop at .git, not escape to parent .beamtalk"
        );
    }

    #[test]
    fn test_discover_nonexistent_start_dir() {
        // A path that doesn't exist should fall back to itself
        let tmp = TempDir::new().unwrap();
        let nonexistent = tmp.path().join("does").join("not").join("exist");

        let root = discover_project_root(&nonexistent);
        assert_eq!(root, nonexistent, "Should fall back to start_dir");
    }

    #[test]
    fn test_beamtalk_toml_wins_over_git_at_same_level() {
        let tmp = TempDir::new().unwrap();
        let project = tmp.path().join("project");
        fs::create_dir_all(&project).unwrap();
        fs::File::create(project.join("beamtalk.toml")).unwrap();
        fs::create_dir(project.join(".git")).unwrap();

        // beamtalk.toml should win over .git at the same level
        let root = discover_project_root(&project);
        assert_eq!(root, project);
    }

    #[test]
    fn test_global_beamtalk_dir_skipped() {
        // ~/.beamtalk is global config, not a project marker.
        // Discovery should NOT use home dir as project root just because
        // ~/.beamtalk exists. It should fall back to .git or start_dir.
        let home = dirs::home_dir().expect("home dir");
        let global = home.join(".beamtalk");

        if !global.exists() {
            // Can't test without ~/.beamtalk existing; skip gracefully
            return;
        }

        // Create a temp dir under home with no markers
        let tmp = TempDir::new_in(&home).unwrap();
        let subdir = tmp.path().join("subdir");
        fs::create_dir_all(&subdir).unwrap();

        let root = discover_project_root(&subdir);
        // Should NOT return home dir; should fall back to subdir or find .git higher up
        assert_ne!(
            root, home,
            "Should not use home dir as project root via global ~/.beamtalk"
        );
    }
}
