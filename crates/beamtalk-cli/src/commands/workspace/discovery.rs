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

use tracing::debug;

/// Project marker files/directories, in priority order.
///
/// Earlier markers take priority: if `.beamtalk/` is found, we stop
/// even if `.git/` exists in a parent directory.
const PROJECT_MARKERS: &[&str] = &[".beamtalk", "beamtalk.toml", ".git"];

/// Discover the project root by walking up the directory tree.
///
/// Starts from `start_dir` and checks each ancestor for project markers.
/// Returns the first directory containing a marker, or `start_dir` if
/// no marker is found.
///
/// Beamtalk-specific markers (`.beamtalk/`, `beamtalk.toml`) take priority
/// over generic markers (`.git/`). If a Beamtalk marker is found at a
/// different level than `.git/`, the Beamtalk marker wins.
pub fn discover_project_root(start_dir: &Path) -> PathBuf {
    let mut best_match: Option<(PathBuf, usize)> = None; // (path, marker_priority)

    let mut current = start_dir.to_path_buf();
    loop {
        for (priority, marker) in PROJECT_MARKERS.iter().enumerate() {
            if current.join(marker).exists() {
                debug!(
                    marker = %marker,
                    dir = %current.display(),
                    "Found project marker"
                );
                match &best_match {
                    Some((_, best_priority)) if *best_priority <= priority => {
                        // Already have a higher-priority match
                    }
                    _ => {
                        best_match = Some((current.clone(), priority));
                        // If we found a beamtalk-specific marker, stop immediately
                        if priority < PROJECT_MARKERS.len() - 1 {
                            debug!(
                                root = %current.display(),
                                "Using Beamtalk-specific project root"
                            );
                            return current;
                        }
                    }
                }
            }
        }

        if !current.pop() {
            break;
        }
    }

    if let Some((path, _)) = best_match {
        debug!(root = %path.display(), "Using discovered project root");
        path
    } else {
        debug!(
            dir = %start_dir.display(),
            "No project markers found, using start directory"
        );
        start_dir.to_path_buf()
    }
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
}
