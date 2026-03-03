// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Node liveness queries for workspace BEAM nodes.
//!
//! Owns all "what state is this node in?" queries: OS liveness checks,
//! port file nonce reading, and WebSocket health probes. Isolating these
//! here makes `is_node_running` testable without spawning processes and
//! puts the OS-specific liveness probing code in one auditable place.
//!
//! **DDD Context:** CLI

use std::net::TcpStream;
use std::time::Duration;

use miette::{IntoDiagnostic, Result};
use serde::Deserialize;

use super::storage::{NodeInfo, read_port_file};
use crate::commands::protocol::ProtocolClient;

/// TCP connect timeout in milliseconds.
pub(super) const TCP_CONNECT_TIMEOUT_MS: u64 = 2000;

/// TCP read timeout in milliseconds.
pub(super) const TCP_READ_TIMEOUT_MS: u64 = 5000;

/// Initial delay before first PID discovery attempt in milliseconds.
///
/// With PID-file discovery (vs the old sysinfo process-list scanning), the file
/// appears as soon as the BEAM VM starts its eval command — before OTP apps load.
/// 500ms is enough for `-detached` fork + exec + VM boot on most systems.
pub(super) const PID_DISCOVERY_INITIAL_DELAY_MS: u64 = 500;

/// Delay between PID discovery retry attempts in milliseconds.
pub(super) const PID_DISCOVERY_RETRY_DELAY_MS: u64 = 500;

/// Maximum number of PID discovery attempts.
/// Total worst-case: 500ms initial + 30 × 500ms = 15.5s.
pub(super) const PID_DISCOVERY_MAX_RETRIES: usize = 30;

/// Check if a BEAM node is actually running (handle stale node.info files).
///
/// Uses a lightweight TCP connect probe (cross-platform) to verify the
/// workspace port is listening. If the workspace has a nonce, validates it
/// against the port file nonce to detect stale entries (PID reuse after crash).
///
/// When `workspace_id` is `Some`, the port file is read directly from the
/// workspace directory (O(1)). When `None`, all workspace directories are
/// scanned to find a matching port file (O(N) fallback for callers that do
/// not have the workspace ID available).
pub fn is_node_running(info: &NodeInfo, workspace_id: Option<&str>) -> bool {
    let host = info.connect_host();
    let addr = format!("{host}:{}", info.port);
    let Ok(addr) = addr.parse::<std::net::SocketAddr>() else {
        return false;
    };

    // Lightweight connect-only probe — if the port is listening, the node is likely alive
    if TcpStream::connect_timeout(&addr, Duration::from_millis(TCP_CONNECT_TIMEOUT_MS)).is_err() {
        return false;
    }

    // If we have a nonce, verify it against the port file for stale detection
    if let Some(ref expected_nonce) = info.nonce {
        let file_nonce = if let Some(id) = workspace_id {
            // Fast path: read port file directly from the known workspace directory.
            // Verify the port matches before using the nonce — if the port file
            // belongs to a different startup, comparing nonces is meaningless.
            match read_port_file(id).ok().flatten() {
                Some((port, _)) if port != info.port => return false, // stale node.info
                Some((_, nonce)) => nonce,
                None => None,
            }
        } else {
            // Fallback: scan all workspace directories (O(N))
            read_port_file_nonce(info.port)
        };
        match file_nonce {
            Some(file_nonce) => file_nonce == *expected_nonce,
            None => true, // No port file — trust the TCP probe
        }
    } else {
        true
    }
}

/// Read the nonce from a port file that matches the given port.
/// Scans all workspace directories since we don't know `workspace_id` here.
/// Returns `None` if no matching port file found or on read error.
fn read_port_file_nonce(port: u16) -> Option<String> {
    let workspaces_root = super::storage::workspaces_base_dir().ok()?;

    let entries = std::fs::read_dir(workspaces_root).ok()?;

    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }
        let port_file_path = path.join("port");
        let Ok(contents) = std::fs::read_to_string(&port_file_path) else {
            continue;
        };
        // Port file format (BT-611): PORT\nNONCE (two lines of plain text)
        let mut lines = contents.lines();
        if let Some(port_line) = lines.next() {
            if let Ok(file_port) = port_line.trim().parse::<u16>() {
                if file_port == port {
                    return lines
                        .next()
                        .map(|s| s.trim().to_string())
                        .filter(|s| !s.is_empty());
                }
            }
        }
    }
    None
}

/// Response from a health probe (BT-611).
#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)] // Used in workspace lifecycle and integration tests
pub struct HealthProbeResponse {
    /// Workspace identifier reported by the running node.
    #[allow(dead_code)] // deserialized for protocol completeness; used in Debug output
    pub workspace_id: String,
    /// Nonce for stale detection — compared against port file nonce.
    pub nonce: String,
    /// Status information from the workspace.
    #[serde(default)]
    #[allow(dead_code)] // deserialized for protocol completeness; used in Debug output
    pub status: Vec<String>,
}

/// Send a WebSocket health probe to a workspace (BT-611, ADR 0020).
///
/// Connects to the workspace's WebSocket endpoint, authenticates with the
/// cookie, sends a `{"op":"health"}` message, and returns the parsed response
/// containing `workspace_id` and `nonce`.
#[allow(dead_code)] // Available for workspace lifecycle and integration tests
pub fn tcp_health_probe(host: &str, port: u16, cookie: &str) -> Result<HealthProbeResponse> {
    let mut client = ProtocolClient::connect(
        host,
        port,
        cookie,
        Some(Duration::from_millis(TCP_READ_TIMEOUT_MS)),
    )?;

    // Send health probe request
    let request = serde_json::json!({"op": "health"});
    let response = client.send_raw(&request)?;
    let parsed: HealthProbeResponse = serde_json::from_value(response).into_diagnostic()?;
    Ok(parsed)
}

/// Check whether a process is alive by PID.
///
/// On Unix: uses `kill(pid, 0)` — signal 0 tests process existence without sending a signal.
/// On Windows: uses `OpenProcess` + `GetExitCodeProcess` to check for `STILL_ACTIVE`.
pub(super) fn is_process_alive(pid: u32) -> bool {
    #[cfg(unix)]
    {
        let Ok(pid_i) = i32::try_from(pid) else {
            return false;
        };
        // SAFETY: kill(2) with signal 0 is a standard existence check.
        let ret = unsafe { libc::kill(pid_i, 0) };
        if ret == 0 {
            return true;
        }
        // EPERM means the process exists but we lack permission to signal it —
        // it is still alive.
        std::io::Error::last_os_error().raw_os_error() == Some(libc::EPERM)
    }

    #[cfg(windows)]
    {
        use windows_sys::Win32::Foundation::{CloseHandle, FALSE, STILL_ACTIVE};
        use windows_sys::Win32::System::Threading::{
            GetExitCodeProcess, OpenProcess, PROCESS_QUERY_LIMITED_INFORMATION,
        };

        // SAFETY: Windows API call with documented parameters.
        let handle = unsafe { OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, pid) };
        if handle.is_null() {
            return false;
        }
        let mut exit_code: u32 = 0;
        // SAFETY: handle is valid, exit_code is a local variable.
        let ok = unsafe { GetExitCodeProcess(handle, &raw mut exit_code) };
        // SAFETY: handle is valid, obtained from OpenProcess above.
        unsafe { CloseHandle(handle) };
        ok != FALSE && exit_code == STILL_ACTIVE as u32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_port_file_nonce_from_plain_text() {
        // Set up a temp workspace dir with a plain-text port file
        let home = dirs::home_dir().expect("HOME must be set");
        let ws_id = format!("test_nonce_{}", std::process::id());
        let ws_dir = home.join(".beamtalk").join("workspaces").join(&ws_id);
        std::fs::create_dir_all(&ws_dir).unwrap();

        let port_file = ws_dir.join("port");
        std::fs::write(&port_file, "12345\nabc123def456\n").unwrap();

        let nonce = read_port_file_nonce(12345);
        assert_eq!(nonce, Some("abc123def456".to_string()));

        // Cleanup
        let _ = std::fs::remove_dir_all(&ws_dir);
    }

    #[test]
    fn test_read_port_file_nonce_wrong_port() {
        let home = dirs::home_dir().expect("HOME must be set");
        let ws_id = format!("test_nonce_wrong_{}", std::process::id());
        let ws_dir = home.join(".beamtalk").join("workspaces").join(&ws_id);
        std::fs::create_dir_all(&ws_dir).unwrap();

        let port_file = ws_dir.join("port");
        // Use a distinct port (23456) to avoid racing with read_port_file_nonce_from_plain_text
        // which also writes port 12345. Both tests scan all workspace dirs, so sharing a port
        // number causes non-deterministic results depending on directory enumeration order.
        std::fs::write(&port_file, "23456\nnonce_value\n").unwrap();

        // Looking for a different port should return None
        let nonce = read_port_file_nonce(54321);
        assert_eq!(nonce, None);

        // Cleanup
        let _ = std::fs::remove_dir_all(&ws_dir);
    }

    #[test]
    fn test_read_port_file_nonce_no_nonce_line() {
        let home = dirs::home_dir().expect("HOME must be set");
        let ws_id = format!("test_nonce_none_{}", std::process::id());
        let ws_dir = home.join(".beamtalk").join("workspaces").join(&ws_id);
        std::fs::create_dir_all(&ws_dir).unwrap();

        // Port file with only port, no nonce (use unique port to avoid scan collisions)
        let port_file = ws_dir.join("port");
        std::fs::write(&port_file, "11111\n").unwrap();

        let nonce = read_port_file_nonce(11111);
        assert_eq!(nonce, None, "Missing nonce line should return None");

        // Cleanup
        let _ = std::fs::remove_dir_all(&ws_dir);
    }
}
