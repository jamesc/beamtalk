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
/// Total worst-case: 500ms initial + 59 × 500ms = 30.0s.
/// The higher budget accommodates loaded CI runners where concurrent workspace
/// starts cause resource contention that slows BEAM VM boot.
pub(super) const PID_DISCOVERY_MAX_RETRIES: usize = 60;

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
            // Fallback: scan all workspace directories (O(N)).
            // If the workspaces root cannot be resolved, trust the TCP probe.
            super::storage::workspaces_base_dir()
                .ok()
                .as_deref()
                .and_then(|root| read_port_file_nonce(root, info.port))
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
///
/// Scans all immediate subdirectories of `workspaces_root` for a `port` file
/// whose first line matches `port`, returning the second line (the nonce) when
/// found. Accepts a `workspaces_root` parameter so callers can pass either the
/// real `~/.beamtalk/workspaces` directory or a temporary directory in tests.
///
/// Returns `None` if no matching port file is found or on any I/O error.
fn read_port_file_nonce(workspaces_root: &std::path::Path, port: u16) -> Option<String> {
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
/// Shared with `tests/cli_common` (BT-3077's stale test-cache-dir sweep),
/// so this lives in the lib-level `pid_liveness` module rather than here.
pub(super) use beamtalk_cli::pid_liveness::is_process_alive;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commands::test_support::{BeamtalkHomeOverride, real_home_guard};
    use std::net::TcpListener;
    use std::path::Path;

    /// Write a two-line port file (`PORT\nNONCE\n`) into `workspaces_root/ws_id/port`.
    fn write_port_file(workspaces_root: &Path, ws_id: &str, port: u16, nonce: Option<&str>) {
        let ws_dir = workspaces_root.join(ws_id);
        std::fs::create_dir_all(&ws_dir).unwrap();
        let content = match nonce {
            Some(n) => format!("{port}\n{n}\n"),
            None => format!("{port}\n"),
        };
        std::fs::write(ws_dir.join("port"), content).unwrap();
    }

    fn base_node_info(port: u16) -> NodeInfo {
        NodeInfo {
            node_name: "n@localhost".to_string(),
            port,
            pid: 1,
            start_time: None,
            nonce: None,
            bind_addr: None,
        }
    }

    /// Bind an ephemeral port, then drop the listener — the OS will not
    /// reissue it immediately, so a connect attempt reliably fails without
    /// needing a real BEAM node to have crashed.
    fn unused_port() -> u16 {
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind");
        let port = listener.local_addr().expect("local_addr").port();
        drop(listener);
        port
    }

    // --- is_node_running: early-return / TCP-probe branches (no fixture needed) ---

    #[test]
    fn is_node_running_false_for_unparseable_connect_host() {
        // `connect_host()` returns `bind_addr` verbatim for anything other than
        // "0.0.0.0"/None, so a non-IP value makes the SocketAddr parse fail —
        // the very first bail branch, before any TCP connect is attempted.
        let mut info = base_node_info(12345);
        info.bind_addr = Some("not-an-ip-address".to_string());
        assert!(!is_node_running(&info, None));
    }

    #[test]
    fn is_node_running_false_when_port_not_listening() {
        let info = base_node_info(unused_port());
        assert!(!is_node_running(&info, None));
    }

    #[test]
    fn is_node_running_true_when_listening_and_no_nonce() {
        // No nonce means the function trusts the TCP probe alone (the final
        // `else { true }` branch) without ever touching a port file.
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind");
        let port = listener.local_addr().expect("local_addr").port();

        assert!(is_node_running(&base_node_info(port), None));
        drop(listener);
    }

    // --- is_node_running: workspace_id fast path (Some(id) -> read_port_file) ---

    #[test]
    fn is_node_running_fast_path_nonce_matches() {
        let _guard = real_home_guard();
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind");
        let port = listener.local_addr().expect("local_addr").port();
        let ws_id = format!("bt3401-node-state-match-{}", std::process::id());
        let base = super::super::storage::workspaces_base_dir().unwrap();
        write_port_file(&base, &ws_id, port, Some("nonce-abc"));

        let mut info = base_node_info(port);
        info.nonce = Some("nonce-abc".to_string());
        assert!(is_node_running(&info, Some(&ws_id)));

        drop(listener);
        let _ = std::fs::remove_dir_all(base.join(&ws_id));
    }

    #[test]
    fn is_node_running_fast_path_nonce_mismatch() {
        let _guard = real_home_guard();
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind");
        let port = listener.local_addr().expect("local_addr").port();
        let ws_id = format!("bt3401-node-state-mismatch-{}", std::process::id());
        let base = super::super::storage::workspaces_base_dir().unwrap();
        write_port_file(&base, &ws_id, port, Some("nonce-on-disk"));

        let mut info = base_node_info(port);
        info.nonce = Some("nonce-expected".to_string());
        assert!(
            !is_node_running(&info, Some(&ws_id)),
            "mismatched nonce must be treated as a stale/different node"
        );

        drop(listener);
        let _ = std::fs::remove_dir_all(base.join(&ws_id));
    }

    #[test]
    fn is_node_running_fast_path_stale_port_in_file_returns_false() {
        // The port file on disk belongs to a different (newer) startup than
        // the port recorded in `info` — comparing nonces would be meaningless,
        // so this must short-circuit to false without even reading the nonce.
        let _guard = real_home_guard();
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind");
        let port = listener.local_addr().expect("local_addr").port();
        let ws_id = format!("bt3401-node-state-stale-port-{}", std::process::id());
        let base = super::super::storage::workspaces_base_dir().unwrap();
        write_port_file(&base, &ws_id, port.wrapping_add(1), Some("whatever"));

        let mut info = base_node_info(port);
        info.nonce = Some("whatever".to_string());
        assert!(!is_node_running(&info, Some(&ws_id)));

        drop(listener);
        let _ = std::fs::remove_dir_all(base.join(&ws_id));
    }

    #[test]
    fn is_node_running_fast_path_no_port_file_trusts_probe() {
        // A running-but-nonce-carrying `info` with no port file at all for this
        // workspace ID (e.g. the file has not been written yet) must fall back
        // to trusting the TCP probe rather than treating "no file" as stale.
        let _guard = real_home_guard();
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind");
        let port = listener.local_addr().expect("local_addr").port();
        let ws_id = format!("bt3401-node-state-no-port-file-{}", std::process::id());
        let base = super::super::storage::workspaces_base_dir().unwrap();
        std::fs::create_dir_all(base.join(&ws_id)).unwrap();

        let mut info = base_node_info(port);
        info.nonce = Some("whatever".to_string());
        assert!(is_node_running(&info, Some(&ws_id)));

        drop(listener);
        let _ = std::fs::remove_dir_all(base.join(&ws_id));
    }

    // --- is_node_running: workspace_id None -> workspaces_base_dir() scan fallback ---

    #[test]
    fn is_node_running_none_fallback_scans_workspaces_base_dir() {
        // Hermetic `BeamtalkHomeOverride`, *not* `real_home_guard`: unlike
        // every other test in this file, this one exercises the O(N)
        // `read_port_file_nonce` scan, which walks *every* subdirectory of
        // `workspaces_base_dir()` and returns on the first `port` file whose
        // recorded port matches — it does not check which workspace ID that
        // file belongs to. `real_home_guard` only serializes against a
        // `BeamtalkHomeOverride`; it deliberately lets other real-directory
        // tests run concurrently against the *same* real `~/.beamtalk/workspaces`
        // tree (safe for them, since each reads/writes only its own uniquely
        // named `ws_id` subdirectory via the `Some(id)` fast path). This test
        // has no such isolation: if another concurrently-running test's
        // OS-assigned ephemeral port happens to match this test's port before
        // this test's own directory is reached in listing order, the scan
        // matches that unrelated entry (with a different nonce) instead and
        // `is_node_running` returns `false` — a directory-listing-order race
        // that widens under the slower, more heavily loaded parallel test
        // execution seen on macOS CI runners (BT-3401). `BeamtalkHomeOverride`
        // points the scan at a fresh, exclusive temp directory instead, so no
        // other test's entries can ever be present during the scan.
        let tmp = tempfile::TempDir::new().unwrap();
        let _override = BeamtalkHomeOverride::new(tmp.path());

        let listener = TcpListener::bind("127.0.0.1:0").expect("bind");
        let port = listener.local_addr().expect("local_addr").port();
        let ws_id = format!("bt3401-node-state-fallback-{}", std::process::id());
        let base = super::super::storage::workspaces_base_dir().unwrap();
        write_port_file(&base, &ws_id, port, Some("fallback-nonce"));

        let mut info = base_node_info(port);
        info.nonce = Some("fallback-nonce".to_string());
        // workspace_id is None: exercises the O(N) `workspaces_base_dir` scan
        // fallback rather than the fast per-workspace `read_port_file` path.
        assert!(is_node_running(&info, None));

        drop(listener);
    }

    #[test]
    fn test_read_port_file_nonce_from_plain_text() {
        let tmp = tempfile::TempDir::new().unwrap();
        write_port_file(tmp.path(), "ws1", 12345, Some("abc123def456"));

        let nonce = read_port_file_nonce(tmp.path(), 12345);
        assert_eq!(nonce, Some("abc123def456".to_string()));
    }

    #[test]
    fn test_read_port_file_nonce_wrong_port() {
        let tmp = tempfile::TempDir::new().unwrap();
        write_port_file(tmp.path(), "ws1", 23456, Some("nonce_value"));

        // Looking for a different port should return None
        let nonce = read_port_file_nonce(tmp.path(), 54321);
        assert_eq!(nonce, None);
    }

    #[test]
    fn test_read_port_file_nonce_no_nonce_line() {
        let tmp = tempfile::TempDir::new().unwrap();
        write_port_file(tmp.path(), "ws1", 11111, None);

        let nonce = read_port_file_nonce(tmp.path(), 11111);
        assert_eq!(nonce, None, "Missing nonce line should return None");
    }

    #[test]
    fn test_read_port_file_nonce_multiple_workspaces_returns_correct() {
        let tmp = tempfile::TempDir::new().unwrap();
        write_port_file(tmp.path(), "ws1", 10001, Some("nonce_a"));
        write_port_file(tmp.path(), "ws2", 10002, Some("nonce_b"));
        write_port_file(tmp.path(), "ws3", 10003, Some("nonce_c"));

        assert_eq!(
            read_port_file_nonce(tmp.path(), 10002),
            Some("nonce_b".to_string())
        );
        assert_eq!(read_port_file_nonce(tmp.path(), 10099), None);
    }
}
