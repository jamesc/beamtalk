// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared workspace utilities for beamtalk tools.
//!
//! Provides workspace ID generation and file I/O used by both
//! `beamtalk-cli` and `beamtalk-mcp`.
//!
//! **DDD Context:** CLI / Language Service

use std::fs;
use std::path::{Path, PathBuf};

use miette::{IntoDiagnostic, Result, miette};
use sha2::{Digest, Sha256};

/// Generate a workspace ID from a project path.
///
/// Uses SHA256 hash of the canonicalized absolute path (first 12 hex chars).
///
/// # Errors
///
/// Returns an error if:
/// - The path cannot be canonicalized (e.g. does not exist).
/// - The canonicalized path contains non-UTF-8 bytes.
pub fn generate_workspace_id(project_path: &Path) -> Result<String> {
    let absolute = project_path.canonicalize().into_diagnostic()?;
    let path_str = absolute
        .to_str()
        .ok_or_else(|| miette!("Project path contains invalid UTF-8: {:?}", absolute))?;

    let mut hasher = Sha256::new();
    hasher.update(path_str.as_bytes());
    let result = hasher.finalize();

    // Use first 12 hex chars (6 bytes) for readability
    Ok(format!("{result:x}")[..12].to_string())
}

/// Get the base directory for all workspaces (`~/.beamtalk/workspaces/`).
///
/// # Errors
///
/// Returns an error if the home directory cannot be determined.
pub fn workspaces_base_dir() -> Result<PathBuf> {
    let home = dirs::home_dir().ok_or_else(|| miette!("Could not determine home directory"))?;
    Ok(home.join(".beamtalk").join("workspaces"))
}

/// Get the workspace directory for a given workspace ID.
///
/// Validates the workspace ID to prevent path traversal attacks.
///
/// # Errors
///
/// Returns an error if:
/// - The home directory cannot be determined.
/// - The workspace ID contains path traversal components (`..`, `/`, `\`).
/// - The workspace ID is empty.
pub fn workspace_dir(workspace_id: &str) -> Result<PathBuf> {
    if workspace_id.is_empty() {
        return Err(miette!("Workspace ID cannot be empty"));
    }
    if workspace_id.contains("..") || workspace_id.contains('/') || workspace_id.contains('\\') {
        return Err(miette!(
            "Workspace ID contains invalid path components: {:?}",
            workspace_id
        ));
    }
    Ok(workspaces_base_dir()?.join(workspace_id))
}

/// Read the port (and optional nonce) from the port file written by `beamtalk_repl_server`.
///
/// Port file format (BT-611): `PORT\nNONCE` (two lines).
/// Returns `Ok(None)` if the file does not exist.
///
/// # Errors
///
/// Returns an error if the file exists but cannot be read.
pub fn read_port_file(workspace_id: &str) -> Result<Option<(u16, Option<String>)>> {
    let port_file_path = workspace_dir(workspace_id)?.join("port");

    if !port_file_path.exists() {
        return Ok(None);
    }

    let content = fs::read_to_string(&port_file_path).into_diagnostic()?;
    let mut lines = content.lines();
    if let Some(port_line) = lines.next() {
        if let Ok(port) = port_line.trim().parse::<u16>() {
            let nonce = lines
                .next()
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty());
            Ok(Some((port, nonce)))
        } else {
            tracing::warn!("Invalid port file content: {content:?}");
            Ok(None)
        }
    } else {
        Ok(None)
    }
}

/// Read the cookie file for a workspace.
///
/// Returns `Ok(None)` if the file does not exist or is empty.
///
/// # Errors
///
/// Returns an error if the file exists but cannot be read.
pub fn read_cookie_file(workspace_id: &str) -> Result<Option<String>> {
    let cookie_path = workspace_dir(workspace_id)?.join("cookie");

    if !cookie_path.exists() {
        return Ok(None);
    }

    let content = fs::read_to_string(&cookie_path).into_diagnostic()?;
    let trimmed = content.trim();
    if trimmed.is_empty() {
        Ok(None)
    } else {
        Ok(Some(trimmed.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_workspace_id_length() {
        // Use the current directory (which must exist)
        let cwd = std::env::current_dir().unwrap();
        let id = generate_workspace_id(&cwd).unwrap();
        assert_eq!(
            id.len(),
            12,
            "Workspace ID should be 12 hex chars (6 bytes)"
        );
    }

    #[test]
    fn test_generate_workspace_id_hex_format() {
        let cwd = std::env::current_dir().unwrap();
        let id = generate_workspace_id(&cwd).unwrap();
        assert!(
            id.chars().all(|c| c.is_ascii_hexdigit()),
            "Workspace ID should be all hex digits, got: {id}"
        );
    }

    #[test]
    fn test_generate_workspace_id_deterministic() {
        let cwd = std::env::current_dir().unwrap();
        let id1 = generate_workspace_id(&cwd).unwrap();
        let id2 = generate_workspace_id(&cwd).unwrap();
        assert_eq!(id1, id2, "Same path should produce the same workspace ID");
    }

    #[test]
    fn test_generate_workspace_id_different_paths() {
        let id1 = generate_workspace_id(Path::new("/tmp")).unwrap();
        let id2 = generate_workspace_id(Path::new("/")).unwrap();
        assert_ne!(id1, id2, "Different paths should produce different IDs");
    }

    #[test]
    fn test_generate_workspace_id_rejects_nonexistent_path() {
        let result = generate_workspace_id(Path::new("/nonexistent/path/that/does/not/exist"));
        assert!(result.is_err(), "Non-existent path should produce an error");
    }

    #[test]
    fn test_workspace_dir_rejects_path_traversal() {
        assert!(workspace_dir("../etc").is_err());
        assert!(workspace_dir("..").is_err());
        assert!(workspace_dir("foo/bar").is_err());
        assert!(workspace_dir("foo\\bar").is_err());
        assert!(workspace_dir("").is_err());
    }

    #[test]
    fn test_workspace_dir_accepts_valid_ids() {
        let dir = workspace_dir("abc123def456");
        assert!(dir.is_ok());
        assert!(dir.unwrap().ends_with("abc123def456"));
    }

    #[test]
    fn test_read_port_file_missing_workspace() {
        let result = read_port_file("nonexistent_workspace_common_abc123").unwrap();
        assert_eq!(result, None, "Missing workspace should return None");
    }

    #[test]
    fn test_read_port_file_valid() {
        let workspace_id = format!("test_wsc_{}", std::process::id());
        let dir = workspaces_base_dir().unwrap().join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("port"), "9876\nnonce123").unwrap();

        let result = read_port_file(&workspace_id).unwrap();
        assert_eq!(result, Some((9876, Some("nonce123".to_string()))));

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_read_port_file_no_nonce() {
        let workspace_id = format!("test_wsc_nononce_{}", std::process::id());
        let dir = workspaces_base_dir().unwrap().join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("port"), "8765\n").unwrap();

        let result = read_port_file(&workspace_id).unwrap();
        assert_eq!(result, Some((8765, None)));

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_read_port_file_invalid_content() {
        let workspace_id = format!("test_wsc_invalid_{}", std::process::id());
        let dir = workspaces_base_dir().unwrap().join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("port"), "not_a_number\n").unwrap();

        let result = read_port_file(&workspace_id).unwrap();
        assert_eq!(result, None, "Invalid port file should return None");

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_read_cookie_file_missing() {
        let result = read_cookie_file("nonexistent_workspace_common_xyz").unwrap();
        assert_eq!(result, None, "Missing cookie file should return None");
    }

    #[test]
    fn test_read_cookie_file_valid() {
        let workspace_id = format!("test_wsc_cookie_{}", std::process::id());
        let dir = workspaces_base_dir().unwrap().join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("cookie"), "mysecretcookie\n").unwrap();

        let result = read_cookie_file(&workspace_id).unwrap();
        assert_eq!(result, Some("mysecretcookie".to_string()));

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_read_cookie_file_empty() {
        let workspace_id = format!("test_wsc_empty_cookie_{}", std::process::id());
        let dir = workspaces_base_dir().unwrap().join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("cookie"), "  \n  ").unwrap();

        let result = read_cookie_file(&workspace_id).unwrap();
        assert_eq!(result, None, "Empty cookie file should return None");

        let _ = fs::remove_dir_all(&dir);
    }
}
