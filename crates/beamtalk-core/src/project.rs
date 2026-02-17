// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Project root discovery utilities.
//!
//! **DDD Context:** Build System
//!
//! Shared project/workspace discovery logic for Beamtalk tooling (CLI, LSP, etc.).

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
    dirs::home_dir().map(|home| home.join(".beamtalk"))
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
    let mut current = start_dir.to_path_buf();
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
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn unique_temp_dir(prefix: &str) -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time")
            .as_nanos();
        std::env::temp_dir().join(format!("{prefix}_{}_{}", std::process::id(), nanos))
    }

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
}
