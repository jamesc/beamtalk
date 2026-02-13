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
