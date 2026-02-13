// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Workspace discovery for finding a running REPL server's port.
//!
//! Mirrors the logic in `beamtalk-cli` for workspace ID generation
//! and port file reading.

use sha2::{Digest, Sha256};
use std::path::{Path, PathBuf};

/// Generate a workspace ID from a project path (SHA256 hex, first 12 chars).
pub fn generate_workspace_id(path: &Path) -> String {
    let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    let mut hasher = Sha256::new();
    hasher.update(canonical.to_string_lossy().as_bytes());
    let result = hasher.finalize();
    hex_encode(&result[..6])
}

/// Get the workspaces directory: `~/.beamtalk/workspaces/`.
fn workspaces_dir() -> Option<PathBuf> {
    dirs::home_dir().map(|h| h.join(".beamtalk").join("workspaces"))
}

/// Get the directory for a specific workspace.
fn workspace_dir(workspace_id: &str) -> Option<PathBuf> {
    workspaces_dir().map(|d| d.join(workspace_id))
}

/// Read the port file for a workspace.
pub fn read_port_file(workspace_id: &str) -> Option<u16> {
    let port_file = workspace_dir(workspace_id)?.join("port");
    let content = std::fs::read_to_string(port_file).ok()?;
    content.trim().parse().ok()
}

/// Discover the REPL port for the current directory.
///
/// 1. If a workspace ID is given, use it directly.
/// 2. Otherwise, generate from the current directory.
pub fn discover_port(workspace_id: Option<&str>) -> Option<u16> {
    let id = if let Some(id) = workspace_id {
        id.to_string()
    } else {
        let cwd = std::env::current_dir().ok()?;
        generate_workspace_id(&cwd)
    };
    read_port_file(&id)
}

/// Find any running workspace and return its port.
///
/// Scans `~/.beamtalk/workspaces/` for directories with port files.
pub fn discover_any_port() -> Option<u16> {
    let dir = workspaces_dir()?;
    let entries = std::fs::read_dir(dir).ok()?;

    for entry in entries.flatten() {
        let is_dir = entry.file_type().is_ok_and(|ft| ft.is_dir());
        if is_dir {
            let port_file = entry.path().join("port");
            if let Ok(content) = std::fs::read_to_string(port_file) {
                if let Ok(port) = content.trim().parse::<u16>() {
                    return Some(port);
                }
            }
        }
    }
    None
}

fn hex_encode(bytes: &[u8]) -> String {
    use std::fmt::Write;
    bytes
        .iter()
        .fold(String::with_capacity(bytes.len() * 2), |mut s, b| {
            let _ = write!(s, "{b:02x}");
            s
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_generate_workspace_id_deterministic() {
        let path = Path::new("/tmp/test-beamtalk-workspace");
        let id1 = generate_workspace_id(path);
        let id2 = generate_workspace_id(path);
        assert_eq!(id1, id2, "Same path should produce same workspace ID");
    }

    #[test]
    fn test_generate_workspace_id_length() {
        let path = Path::new("/tmp/test-beamtalk-workspace");
        let id = generate_workspace_id(path);
        assert_eq!(
            id.len(),
            12,
            "Workspace ID should be 12 hex chars (6 bytes)"
        );
    }

    #[test]
    fn test_generate_workspace_id_hex_format() {
        let path = Path::new("/tmp/test-beamtalk-workspace");
        let id = generate_workspace_id(path);
        assert!(
            id.chars().all(|c| c.is_ascii_hexdigit()),
            "Workspace ID should be all hex digits, got: {id}"
        );
    }

    #[test]
    fn test_different_paths_produce_different_ids() {
        let id1 = generate_workspace_id(Path::new("/tmp/project-a"));
        let id2 = generate_workspace_id(Path::new("/tmp/project-b"));
        assert_ne!(id1, id2, "Different paths should produce different IDs");
    }

    #[test]
    fn test_hex_encode() {
        assert_eq!(hex_encode(&[0x00]), "00");
        assert_eq!(hex_encode(&[0xff]), "ff");
        assert_eq!(hex_encode(&[0xde, 0xad, 0xbe, 0xef]), "deadbeef");
        assert_eq!(hex_encode(&[]), "");
    }

    #[test]
    fn test_read_port_file_missing_workspace() {
        let result = read_port_file("nonexistent_workspace_id_abc123");
        assert_eq!(result, None, "Missing workspace should return None");
    }

    #[test]
    fn test_read_port_file_valid() {
        let workspace_id = format!("test_mcp_{}", std::process::id());
        let dir = workspaces_dir().unwrap().join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("port"), "9876\n").unwrap();

        let result = read_port_file(&workspace_id);
        assert_eq!(result, Some(9876));

        // Cleanup
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_read_port_file_invalid_content() {
        let workspace_id = format!("test_mcp_invalid_{}", std::process::id());
        let dir = workspaces_dir().unwrap().join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("port"), "not_a_number\n").unwrap();

        let result = read_port_file(&workspace_id);
        assert_eq!(result, None, "Invalid port file content should return None");

        // Cleanup
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_read_port_file_empty() {
        let workspace_id = format!("test_mcp_empty_{}", std::process::id());
        let dir = workspaces_dir().unwrap().join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("port"), "").unwrap();

        let result = read_port_file(&workspace_id);
        assert_eq!(result, None, "Empty port file should return None");

        // Cleanup
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_discover_port_with_explicit_id() {
        let workspace_id = format!("test_mcp_discover_{}", std::process::id());
        let dir = workspaces_dir().unwrap().join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("port"), "5555").unwrap();

        let result = discover_port(Some(&workspace_id));
        assert_eq!(result, Some(5555));

        // Cleanup
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_discover_port_missing_id() {
        let result = discover_port(Some("nonexistent_workspace_xyz"));
        assert_eq!(result, None);
    }
}
