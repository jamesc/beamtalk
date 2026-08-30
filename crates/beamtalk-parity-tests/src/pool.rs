// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared REPL workspace pool.
//!
//! The harness starts at most one workspace per process. The same workspace is
//! reused by every REPL and MCP case in a run — this is the key contributor
//! to keeping the parity suite's wall-clock under the e2e suite's ~50s.

use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::sync::OnceLock;
use std::time::Duration;

/// Live-workspace handle returned to the harness.
#[derive(Debug, Clone)]
pub struct SharedRepl {
    /// REPL WebSocket port.
    pub port: u16,
    /// Auth cookie for the REPL.
    pub cookie: String,
    /// Workspace ID (used by the MCP driver for discovery).
    pub workspace_id: String,
}

/// Get-or-start the shared workspace.
///
/// On first call this spawns `beamtalk repl --port 0 --timeout 300`, parses
/// the port and workspace ID from stdout, reads the cookie from workspace
/// storage, and waits for the TCP port to accept connections. Subsequent
/// calls return the cached handle.
///
/// Errors are also cached so a failed startup short-circuits later cases
/// rather than retrying the same expensive failure mode.
pub fn shared_repl() -> Result<SharedRepl, String> {
    static CACHE: OnceLock<Result<SharedRepl, String>> = OnceLock::new();
    CACHE.get_or_init(start_workspace).clone()
}

fn start_workspace() -> Result<SharedRepl, String> {
    let bin = beamtalk_binary("beamtalk")?;
    let pid = std::process::id();
    let stdout_path = std::env::temp_dir().join(format!("beamtalk-parity-{pid}.stdout"));
    let stderr_path = std::env::temp_dir().join(format!("beamtalk-parity-{pid}.stderr"));
    let stdout =
        std::fs::File::create(&stdout_path).map_err(|e| format!("create stdout temp file: {e}"))?;
    let stderr =
        std::fs::File::create(&stderr_path).map_err(|e| format!("create stderr temp file: {e}"))?;

    // Spawn `beamtalk repl` from a unique temp directory so the parity suite
    // gets its OWN workspace ID (workspace IDs are derived from cwd). Sharing
    // a workspace with `just test-mcp` would let leftover parity-test classes
    // leak into the MCP `test-all` integration test.
    let workspace_cwd = std::env::temp_dir().join(format!("beamtalk-parity-cwd-{pid}"));
    std::fs::create_dir_all(&workspace_cwd)
        .map_err(|e| format!("create workspace cwd {}: {e}", workspace_cwd.display()))?;

    let mut child = Command::new(&bin)
        .args(["repl", "--port", "0", "--timeout", "300"])
        .current_dir(&workspace_cwd)
        .stdin(Stdio::null())
        .stdout(stdout)
        .stderr(stderr)
        .spawn()
        .map_err(|e| format!("spawn `beamtalk repl`: {e}"))?;

    let deadline = std::time::Instant::now() + Duration::from_secs(45);
    let mut combined = String::new();
    let mut have_port = false;
    let mut have_ws_id = false;
    while std::time::Instant::now() < deadline {
        if let Ok(Some(status)) = child.try_wait() {
            // Read the rest of stdout before bailing.
            combined = std::fs::read_to_string(&stdout_path).unwrap_or_default();
            let stderr_text = std::fs::read_to_string(&stderr_path).unwrap_or_default();
            let _ = std::fs::remove_file(&stdout_path);
            let _ = std::fs::remove_file(&stderr_path);
            if status.success() && combined.contains("port") {
                // Successful detached start with stdin=null is fine; fall through to parse.
                break;
            }
            return Err(format!(
                "`beamtalk repl` exited {status} before workspace was ready.\n\
                 stdout:\n{combined}\nstderr:\n{stderr_text}"
            ));
        }
        if let Ok(content) = std::fs::read_to_string(&stdout_path) {
            have_port = content.contains("port");
            have_ws_id = content.contains("Workspace:");
            combined = content;
            if have_port && have_ws_id {
                break;
            }
        }
        std::thread::sleep(Duration::from_millis(150));
    }

    // Best-effort cleanup: detach and stop reading the temp files.
    let stderr_text = std::fs::read_to_string(&stderr_path).unwrap_or_default();
    let _ = std::fs::remove_file(&stdout_path);
    let _ = std::fs::remove_file(&stderr_path);
    drop(child);

    let port = beamtalk_workspace::parse_repl_port(&combined).ok_or_else(|| {
        format!(
            "`beamtalk repl` did not report a port within 45s.\nstdout:\n{combined}\nstderr:\n{stderr_text}"
        )
    })?;
    let workspace_id = beamtalk_workspace::parse_repl_workspace_id(&combined).ok_or_else(|| {
        format!(
            "`beamtalk repl` did not report a workspace id.\nstdout:\n{combined}\nstderr:\n{stderr_text}"
        )
    })?;
    let cookie = beamtalk_workspace::read_cookie_file(&workspace_id)
        .map_err(|e| format!("read cookie for workspace {workspace_id}: {e}"))?
        .ok_or_else(|| format!("no cookie file for workspace {workspace_id}"))?;

    wait_for_tcp_ready(port, Duration::from_secs(20))?;
    let _ = (have_port, have_ws_id); // silence dead-store warnings if both branches set them

    Ok(SharedRepl {
        port,
        cookie,
        workspace_id,
    })
}

/// Resolve `<bin>` (or `<bin>.exe` on Windows) next to this test binary.
///
/// Thin re-export of the shared resolution logic in `beamtalk-workspace`
/// (also used by `beamtalk-mcp`'s and `beamtalk-cli`'s own
/// subprocess-spawning integration tests) — see
/// [`beamtalk_workspace::resolve_sibling_binary`] for why a hardcoded
/// `target/debug` path doesn't work under `cargo llvm-cov`.
pub fn beamtalk_binary(name: &str) -> Result<PathBuf, String> {
    beamtalk_workspace::resolve_sibling_binary(name)
}

fn wait_for_tcp_ready(port: u16, timeout: Duration) -> Result<(), String> {
    let deadline = std::time::Instant::now() + timeout;
    while std::time::Instant::now() < deadline {
        if std::net::TcpStream::connect_timeout(
            &format!("127.0.0.1:{port}").parse().unwrap(),
            Duration::from_millis(500),
        )
        .is_ok()
        {
            return Ok(());
        }
        std::thread::sleep(Duration::from_millis(200));
    }
    Err(format!(
        "TCP port {port} did not accept connections within {}s",
        timeout.as_secs()
    ))
}
