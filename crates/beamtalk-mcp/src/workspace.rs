// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Workspace discovery for finding a running REPL server's port.
//!
//! **DDD Context:** Language Service / Interactive Development
//!
//! Delegates workspace ID generation and file I/O to `beamtalk-workspace`.

use std::path::Path;

/// Generate a workspace ID from a project path (SHA256 hex, first 12 chars).
///
/// Returns `None` if the path cannot be canonicalized or contains non-UTF-8 bytes.
pub fn generate_workspace_id(path: &Path) -> Option<String> {
    beamtalk_workspace::generate_workspace_id(path)
        .inspect_err(|err| {
            tracing::warn!(
                "Failed to generate workspace ID for {}: {err}",
                path.display()
            );
        })
        .ok()
}

/// Read the port file for a workspace.
/// Port file format (BT-611): `PORT\nNONCE` (two lines). Only the port is returned.
pub fn read_port_file(workspace_id: &str) -> Option<u16> {
    beamtalk_workspace::read_port_file(workspace_id)
        .ok()
        .flatten()
        .map(|(port, _nonce)| port)
}

/// Read the cookie file for a workspace.
pub fn read_cookie_file(workspace_id: &str) -> Option<String> {
    beamtalk_workspace::read_cookie_file(workspace_id)
        .ok()
        .flatten()
}

/// Discover the REPL port and cookie for the current directory.
///
/// 1. If a workspace ID is given, use it directly.
/// 2. Otherwise, generate from the current directory.
pub fn discover_port_and_cookie(workspace_id: Option<&str>) -> Option<(u16, String)> {
    discover_port_cookie_and_id(workspace_id).map(|(port, cookie, _id)| (port, cookie))
}

/// Like [`discover_port_and_cookie`] but also returns the resolved workspace ID
/// so callers can pass it to `ReplClient` for port re-discovery on reconnect (BT-1416).
pub fn discover_port_cookie_and_id(workspace_id: Option<&str>) -> Option<(u16, String, String)> {
    let id = if let Some(id) = workspace_id {
        id.to_string()
    } else {
        let cwd = std::env::current_dir().ok()?;
        generate_workspace_id(&cwd)?
    };
    let port = read_port_file(&id)?;
    let cookie = read_cookie_file(&id)?;
    Some((port, cookie, id))
}

/// Discover the REPL port for the current directory.
///
/// 1. If a workspace ID is given, use it directly.
/// 2. Otherwise, generate from the current directory.
#[allow(dead_code)] // Used by tests; main uses discover_port_and_cookie
pub fn discover_port(workspace_id: Option<&str>) -> Option<u16> {
    discover_port_and_cookie(workspace_id).map(|(port, _)| port)
}

/// Parse the REPL port from `beamtalk repl` stdout.
///
/// Expects a line like: `Connected to REPL backend on port 12345.`
pub fn parse_repl_port(stdout: &str) -> Option<u16> {
    stdout.lines().find_map(|line| {
        line.strip_prefix("Connected to REPL backend on port ")
            .and_then(|rest| rest.trim_end_matches('.').trim().parse().ok())
    })
}

/// Parse the workspace ID from `beamtalk repl` stdout.
///
/// Expects a line like: `  Workspace: abc123def456 (new)`
pub fn parse_workspace_id(stdout: &str) -> Option<String> {
    stdout.lines().find_map(|line| {
        line.strip_prefix("  Workspace: ")
            .and_then(|rest| rest.split_whitespace().next())
            .map(std::string::ToString::to_string)
    })
}

/// Find any running workspace and return its port, cookie, and workspace ID.
///
/// Scans `~/.beamtalk/workspaces/` for directories with port files.
/// Returns the workspace ID for port re-discovery on reconnect (BT-1416).
pub fn discover_any_port_cookie_and_id() -> Option<(u16, String, String)> {
    let dir = beamtalk_workspace::workspaces_base_dir().ok()?;
    let entries = std::fs::read_dir(dir).ok()?;

    for entry in entries.flatten() {
        let is_dir = entry.file_type().is_ok_and(|ft| ft.is_dir());
        if is_dir {
            let workspace_id = entry.file_name();
            let workspace_id = workspace_id.to_string_lossy();
            if let Some(port) = read_port_file(&workspace_id) {
                if let Some(cookie) = read_cookie_file(&workspace_id) {
                    return Some((port, cookie, workspace_id.into_owned()));
                }
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_generate_workspace_id_deterministic() {
        let path = std::env::temp_dir();
        let id1 = generate_workspace_id(&path).unwrap();
        let id2 = generate_workspace_id(&path).unwrap();
        assert_eq!(id1, id2, "Same path should produce same workspace ID");
    }

    #[test]
    fn test_generate_workspace_id_length() {
        let path = std::env::temp_dir();
        let id = generate_workspace_id(&path).unwrap();
        assert_eq!(
            id.len(),
            12,
            "Workspace ID should be 12 hex chars (6 bytes)"
        );
    }

    #[test]
    fn test_generate_workspace_id_hex_format() {
        let path = std::env::temp_dir();
        let id = generate_workspace_id(&path).unwrap();
        assert!(
            id.chars().all(|c| c.is_ascii_hexdigit()),
            "Workspace ID should be all hex digits, got: {id}"
        );
    }

    #[test]
    fn test_different_paths_produce_different_ids() {
        let temp = std::env::temp_dir();
        let cwd = std::env::current_dir().unwrap();
        if temp != cwd {
            let id1 = generate_workspace_id(&temp).unwrap();
            let id2 = generate_workspace_id(&cwd).unwrap();
            assert_ne!(id1, id2, "Different paths should produce different IDs");
        }
    }

    #[test]
    fn test_read_port_file_missing_workspace() {
        let result = read_port_file("nonexistent_workspace_id_abc123");
        assert_eq!(result, None, "Missing workspace should return None");
    }

    #[test]
    fn test_read_port_file_valid() {
        let workspace_id = format!("test_mcp_{}", std::process::id());
        let dir = beamtalk_workspace::workspaces_base_dir()
            .unwrap()
            .join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("port"), "9876\nnonce123").unwrap();

        let result = read_port_file(&workspace_id);
        assert_eq!(result, Some(9876));

        // Cleanup
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_read_port_file_two_line_format() {
        let workspace_id = format!("test_mcp_twoln_{}", std::process::id());
        let dir = beamtalk_workspace::workspaces_base_dir()
            .unwrap()
            .join(&workspace_id);
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
        let dir = beamtalk_workspace::workspaces_base_dir()
            .unwrap()
            .join(&workspace_id);
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
        let dir = beamtalk_workspace::workspaces_base_dir()
            .unwrap()
            .join(&workspace_id);
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
        let dir = beamtalk_workspace::workspaces_base_dir()
            .unwrap()
            .join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("port"), "5555\nnonce456").unwrap();
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
        assert!(beamtalk_workspace::workspace_dir("../etc").is_err());
        assert!(beamtalk_workspace::workspace_dir("..").is_err());
        assert!(beamtalk_workspace::workspace_dir("foo/bar").is_err());
        assert!(beamtalk_workspace::workspace_dir("foo\\bar").is_err());
        assert!(beamtalk_workspace::workspace_dir("").is_err());
    }

    #[test]
    fn test_workspace_dir_accepts_valid_ids() {
        let dir = beamtalk_workspace::workspace_dir("abc123def456");
        assert!(dir.is_ok());
        assert!(dir.unwrap().ends_with("abc123def456"));
    }

    #[test]
    fn test_parse_repl_port_typical() {
        let stdout = "Welcome to beamtalk REPL\nConnected to REPL backend on port 9876.\n  Workspace: abc123def456 (new)\n";
        assert_eq!(parse_repl_port(stdout), Some(9876));
    }

    #[test]
    fn test_parse_repl_port_missing() {
        assert_eq!(parse_repl_port("some other output\n"), None);
        assert_eq!(parse_repl_port(""), None);
    }

    #[test]
    fn test_parse_repl_port_malformed() {
        assert_eq!(
            parse_repl_port("Connected to REPL backend on port notanumber.\n"),
            None
        );
    }

    #[test]
    fn test_parse_workspace_id_typical() {
        let stdout = "Connected to REPL backend on port 9876.\n  Workspace: abc123def456 (new)\n";
        assert_eq!(parse_workspace_id(stdout), Some("abc123def456".to_string()));
    }

    #[test]
    fn test_parse_workspace_id_missing() {
        assert_eq!(parse_workspace_id("no workspace line\n"), None);
        assert_eq!(parse_workspace_id(""), None);
    }

    #[test]
    fn test_parse_workspace_id_bare() {
        assert_eq!(
            parse_workspace_id("  Workspace: deadbeef1234\n"),
            Some("deadbeef1234".to_string())
        );
    }

    #[test]
    fn test_parse_workspace_id_empty_prefix() {
        // "  Workspace: " present but no ID — must return None, not Some("")
        assert_eq!(parse_workspace_id("  Workspace: \n"), None);
        assert_eq!(parse_workspace_id("  Workspace: "), None);
    }

    #[test]
    fn test_discover_port_cookie_and_id_returns_workspace_id() {
        let workspace_id = format!("test_mcp_pcid_{}", std::process::id());
        let dir = beamtalk_workspace::workspaces_base_dir()
            .unwrap()
            .join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("port"), "7777\nnonce789").unwrap();
        fs::write(dir.join("cookie"), "testcookie").unwrap();

        let result = discover_port_cookie_and_id(Some(&workspace_id));
        assert_eq!(
            result,
            Some((7777, "testcookie".to_string(), workspace_id.clone()))
        );

        // Cleanup
        let _ = fs::remove_dir_all(&dir);
    }

    /// BT-1416: Verify that re-reading a port file after the port changes
    /// returns the new port. This is the mechanism that allows reconnect to
    /// find the new port after a workspace restart.
    #[test]
    fn test_port_file_reread_after_change() {
        let workspace_id = format!("test_mcp_reread_{}", std::process::id());
        let dir = beamtalk_workspace::workspaces_base_dir()
            .unwrap()
            .join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();

        // Write initial port
        fs::write(dir.join("port"), "36109\nnonce1").unwrap();
        assert_eq!(read_port_file(&workspace_id), Some(36109));

        // Simulate workspace restart — port file updated with new port
        fs::write(dir.join("port"), "38901\nnonce2").unwrap();
        assert_eq!(read_port_file(&workspace_id), Some(38901));

        // Cleanup
        let _ = fs::remove_dir_all(&dir);
    }
}
