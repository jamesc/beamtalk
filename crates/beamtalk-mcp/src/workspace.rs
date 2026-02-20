// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Workspace discovery for finding a running REPL server's port.
//!
//! **DDD Context:** Language Service / Interactive Development
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
///
/// Validates the workspace ID to prevent path traversal attacks.
fn workspace_dir(workspace_id: &str) -> Option<PathBuf> {
    // Reject path traversal components
    if workspace_id.contains('/')
        || workspace_id.contains('\\')
        || workspace_id.contains("..")
        || workspace_id.is_empty()
    {
        return None;
    }
    workspaces_dir().map(|d| d.join(workspace_id))
}

/// Read the port file for a workspace.
/// Port file format (BT-611): `PORT\nNONCE` (two lines). Only the port is returned.
pub fn read_port_file(workspace_id: &str) -> Option<u16> {
    let port_file = workspace_dir(workspace_id)?.join("port");
    let content = std::fs::read_to_string(port_file).ok()?;
    // Parse only the first line (port number); second line is nonce for stale detection
    content.lines().next()?.trim().parse().ok()
}

/// Read the cookie file for a workspace.
pub fn read_cookie_file(workspace_id: &str) -> Option<String> {
    let cookie_file = workspace_dir(workspace_id)?.join("cookie");
    let content = std::fs::read_to_string(cookie_file).ok()?;
    let trimmed = content.trim();
    if trimmed.is_empty() {
        None
    } else {
        Some(trimmed.to_string())
    }
}

/// Discover the REPL port and cookie for the current directory.
///
/// 1. If a workspace ID is given, use it directly.
/// 2. Otherwise, generate from the current directory.
pub fn discover_port_and_cookie(workspace_id: Option<&str>) -> Option<(u16, String)> {
    let id = if let Some(id) = workspace_id {
        id.to_string()
    } else {
        let cwd = std::env::current_dir().ok()?;
        generate_workspace_id(&cwd)
    };
    let port = read_port_file(&id)?;
    let cookie = read_cookie_file(&id)?;
    Some((port, cookie))
}

/// Discover the REPL port for the current directory.
///
/// 1. If a workspace ID is given, use it directly.
/// 2. Otherwise, generate from the current directory.
#[allow(dead_code)] // Used by tests; main uses discover_port_and_cookie
pub fn discover_port(workspace_id: Option<&str>) -> Option<u16> {
    discover_port_and_cookie(workspace_id).map(|(port, _)| port)
}

/// Find any running workspace and return its port and cookie.
///
/// Scans `~/.beamtalk/workspaces/` for directories with port files.
pub fn discover_any_port_and_cookie() -> Option<(u16, String)> {
    let dir = workspaces_dir()?;
    let entries = std::fs::read_dir(dir).ok()?;

    for entry in entries.flatten() {
        let is_dir = entry.file_type().is_ok_and(|ft| ft.is_dir());
        if is_dir {
            let port_file = entry.path().join("port");
            if let Ok(content) = std::fs::read_to_string(port_file) {
                // Parse only the first line (port); second line is nonce (BT-611)
                if let Some(port) = content
                    .lines()
                    .next()
                    .and_then(|l| l.trim().parse::<u16>().ok())
                {
                    let cookie = std::fs::read_to_string(entry.path().join("cookie"))
                        .ok()
                        .map(|s| s.trim().to_string())
                        .filter(|s| !s.is_empty())?;
                    return Some((port, cookie));
                }
            }
        }
    }
    None
}

/// Encode a byte slice as a lowercase hexadecimal string.
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
    fn test_read_port_file_two_line_format() {
        let workspace_id = format!("test_mcp_twoln_{}", std::process::id());
        let dir = workspaces_dir().unwrap().join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        // BT-727: port file has PORT\nNONCE format; reader must parse only first line
        fs::write(dir.join("port"), "9876\nabc123nonce\n").unwrap();

        let result = read_port_file(&workspace_id);
        assert_eq!(result, Some(9876), "Should parse port from first line only");

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
        fs::write(dir.join("cookie"), "testcookie").unwrap();

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

    #[test]
    fn test_workspace_dir_rejects_path_traversal() {
        assert_eq!(workspace_dir("../etc"), None);
        assert_eq!(workspace_dir(".."), None);
        assert_eq!(workspace_dir("foo/bar"), None);
        assert_eq!(workspace_dir("foo\\bar"), None);
        assert_eq!(workspace_dir(""), None);
    }

    #[test]
    fn test_workspace_dir_accepts_valid_ids() {
        let dir = workspace_dir("abc123def456");
        assert!(dir.is_some());
        assert!(dir.unwrap().ends_with("abc123def456"));
    }
}
