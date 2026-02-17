// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Process management for workspace BEAM nodes.
//!
//! Handles starting detached BEAM nodes, TCP health probes, shutdown,
//! and PID discovery.
//!
//! **DDD Context:** CLI

use std::io::{BufRead, BufReader, Write};
use std::net::TcpStream;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::Duration;

use miette::{IntoDiagnostic, Result, miette};
use serde::Deserialize;

#[cfg(target_os = "linux")]
use super::storage::read_proc_start_time;
use super::storage::{
    NodeInfo, get_workspace_metadata, read_port_file, read_workspace_cookie, save_node_info,
};

/// Default idle timeout in seconds (4 hours)
const DEFAULT_IDLE_TIMEOUT_SECONDS: u64 = 3600 * 4;

/// TCP connect timeout in milliseconds.
const TCP_CONNECT_TIMEOUT_MS: u64 = 2000;

/// TCP read timeout in milliseconds.
const TCP_READ_TIMEOUT_MS: u64 = 5000;

/// Initial delay before first PID discovery attempt in milliseconds.
const PID_DISCOVERY_INITIAL_DELAY_MS: u64 = 2000;

/// Delay between PID discovery retry attempts in milliseconds.
const PID_DISCOVERY_RETRY_DELAY_MS: u64 = 500;

/// Maximum number of PID/port discovery attempts.
const DISCOVERY_MAX_RETRIES: usize = 10;

/// Delay between port file read attempts in milliseconds.
const PORT_DISCOVERY_DELAY_MS: u64 = 500;

/// Maximum number of port file discovery attempts.
const PORT_DISCOVERY_MAX_RETRIES: usize = 20;

/// Delay between TCP readiness probe retries in milliseconds.
const READINESS_PROBE_DELAY_MS: u64 = 200;

/// Maximum number of TCP readiness probe attempts.
/// Total worst-case: 30 × (300ms connect + 500ms read + 200ms sleep) = ~30s.
const READINESS_PROBE_MAX_RETRIES: usize = 30;

/// TCP connect timeout for readiness probe in milliseconds (shorter than
/// the full health probe to keep worst-case bounded at ~10s).
const READINESS_CONNECT_TIMEOUT_MS: u64 = 300;

/// TCP read timeout for readiness probe in milliseconds.
const READINESS_READ_TIMEOUT_MS: u64 = 500;

/// TCP connect timeout for exit probe in milliseconds.
const EXIT_PROBE_CONNECT_TIMEOUT_MS: u64 = 500;

/// Check if a BEAM node is actually running (handle stale node.info files).
///
/// Uses TCP health probe (cross-platform) to verify the workspace is alive.
/// If the workspace has a nonce, validates it against the port file nonce
/// to detect stale entries (PID reuse after crash).
pub fn is_node_running(info: &NodeInfo) -> bool {
    match tcp_health_probe(info.port) {
        Ok(response) => {
            // If we have a nonce, verify it matches to detect stale port files
            if let Some(ref expected_nonce) = info.nonce {
                response.nonce == *expected_nonce
            } else {
                // No nonce stored (old node.info format) — trust the probe
                true
            }
        }
        Err(_) => false,
    }
}

/// Response from a TCP health probe (BT-611).
#[derive(Debug, Clone, Deserialize)]
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

/// Send a TCP health probe to a workspace (BT-611).
///
/// Connects to the workspace's TCP port, sends a `{"op":"health"}` message,
/// and returns the parsed response containing `workspace_id` and `nonce`.
/// The nonce can be compared against the port file nonce to detect stale entries.
pub fn tcp_health_probe(port: u16) -> Result<HealthProbeResponse> {
    let addr: std::net::SocketAddr = format!("127.0.0.1:{port}")
        .parse()
        .map_err(|e| miette!("Invalid address: {e}"))?;

    let stream = TcpStream::connect_timeout(&addr, Duration::from_millis(TCP_CONNECT_TIMEOUT_MS))
        .into_diagnostic()?;
    stream
        .set_read_timeout(Some(Duration::from_millis(TCP_READ_TIMEOUT_MS)))
        .into_diagnostic()?;

    let mut writer = stream.try_clone().into_diagnostic()?;
    let mut reader = BufReader::new(stream);

    // Send health probe request
    writer
        .write_all(b"{\"op\":\"health\"}\n")
        .into_diagnostic()?;
    writer.flush().into_diagnostic()?;

    // Read response line
    let mut response_line = String::new();
    reader.read_line(&mut response_line).into_diagnostic()?;

    let response: HealthProbeResponse =
        serde_json::from_str(response_line.trim()).into_diagnostic()?;
    Ok(response)
}

/// Send a TCP shutdown message to a workspace (BT-611).
///
/// Connects to the workspace's TCP port, sends a cookie-authenticated
/// `{"op":"shutdown","cookie":"..."}` message, and waits for acknowledgement.
/// The workspace will call `init:stop()` for OTP-level graceful teardown.
pub fn tcp_send_shutdown(port: u16, cookie: &str) -> Result<()> {
    let addr: std::net::SocketAddr = format!("127.0.0.1:{port}")
        .parse()
        .map_err(|e| miette!("Invalid address: {e}"))?;

    let stream = TcpStream::connect_timeout(&addr, Duration::from_millis(TCP_CONNECT_TIMEOUT_MS))
        .into_diagnostic()?;
    stream
        .set_read_timeout(Some(Duration::from_millis(TCP_READ_TIMEOUT_MS)))
        .into_diagnostic()?;

    let mut writer = stream.try_clone().into_diagnostic()?;
    let mut reader = BufReader::new(stream);

    // Send shutdown request with cookie
    let request = serde_json::json!({"op": "shutdown", "cookie": cookie});
    writer
        .write_all(request.to_string().as_bytes())
        .into_diagnostic()?;
    writer.write_all(b"\n").into_diagnostic()?;
    writer.flush().into_diagnostic()?;

    // Read response
    let mut response_line = String::new();
    reader.read_line(&mut response_line).into_diagnostic()?;

    // Check for error in response
    let response: serde_json::Value =
        serde_json::from_str(response_line.trim()).into_diagnostic()?;
    if let Some(error) = response.get("error") {
        return Err(miette!("Shutdown rejected: {error}"));
    }

    Ok(())
}

/// Start a detached BEAM node for a workspace.
/// Returns the `NodeInfo` for the started node.
#[allow(clippy::too_many_arguments)] // BEAM node requires separate beam dirs per OTP app
pub fn start_detached_node(
    workspace_id: &str,
    port: u16,
    runtime_beam_dir: &Path,
    repl_beam_dir: &Path,
    jsx_beam_dir: &Path,
    compiler_beam_dir: &Path,
    stdlib_beam_dir: &Path,
    extra_code_paths: &[PathBuf],
    auto_cleanup: bool,
    max_idle_seconds: Option<u64>,
) -> Result<NodeInfo> {
    // Generate node name
    let node_name = format!("beamtalk_workspace_{workspace_id}@localhost");

    // Read cookie
    let cookie = read_workspace_cookie(workspace_id)?;

    // Determine idle timeout (explicit arg > environment variable > default)
    let idle_timeout = max_idle_seconds.unwrap_or_else(|| {
        std::env::var("BEAMTALK_WORKSPACE_TIMEOUT")
            .ok()
            .and_then(|s| s.parse::<u64>().ok())
            .unwrap_or(DEFAULT_IDLE_TIMEOUT_SECONDS)
    });

    // Build the eval command to start workspace supervisor and keep running
    let project_path = get_workspace_metadata(workspace_id)?.project_path;
    let project_path_str = project_path
        .to_str()
        .ok_or_else(|| miette!("Project path contains invalid UTF-8: {:?}", project_path))?;
    let eval_cmd = format!(
        "application:set_env(beamtalk_runtime, workspace_id, <<\"{workspace_id}\">>), \
         application:set_env(beamtalk_runtime, project_path, <<\"{project_path_str}\">>), \
         application:set_env(beamtalk_runtime, tcp_port, {port}), \
         {{ok, _}} = application:ensure_all_started(beamtalk_workspace), \
         {{ok, _}} = beamtalk_workspace_sup:start_link(#{{workspace_id => <<\"{workspace_id}\">>, \
                                                          project_path => <<\"{project_path_str}\">>, \
                                                          tcp_port => {port}, \
                                                          auto_cleanup => {auto_cleanup}, \
                                                          max_idle_seconds => {idle_timeout}}}), \
         {{ok, ActualPort}} = beamtalk_repl_server:get_port(), \
         io:format(\"Workspace {workspace_id} started on port ~B~n\", [ActualPort]), \
         receive stop -> ok end."
    );

    // Start detached BEAM node
    let mut cmd = build_detached_node_command(
        &node_name,
        &cookie,
        runtime_beam_dir,
        repl_beam_dir,
        jsx_beam_dir,
        compiler_beam_dir,
        stdlib_beam_dir,
        extra_code_paths,
        &eval_cmd,
        &project_path,
    );

    let _child = cmd.spawn().map_err(|e| {
        miette!("Failed to start detached BEAM node: {e}\nIs Erlang/OTP installed?")
    })?;

    // With -detached, the spawn() returns immediately and the real BEAM node
    // runs independently. We need to wait for it to start up.
    // The compiler app (ADR 0022) adds ~500ms to startup, and under load
    // (e.g., parallel integration tests) it can take longer.
    // Retry PID discovery instead of a single fixed sleep.
    let mut last_err = None;
    let pid_found = 'retry: {
        for attempt in 0..DISCOVERY_MAX_RETRIES {
            if attempt > 0 {
                std::thread::sleep(Duration::from_millis(PID_DISCOVERY_RETRY_DELAY_MS));
            } else {
                // Initial delay before first attempt
                std::thread::sleep(Duration::from_millis(PID_DISCOVERY_INITIAL_DELAY_MS));
            }
            match find_beam_pid_by_node(&node_name) {
                Ok(result) => break 'retry result,
                Err(e) => last_err = Some(e),
            }
        }
        return Err(last_err.unwrap_or_else(|| miette!("PID discovery failed")));
    };
    let (pid, start_time) = pid_found;

    // Read actual port from port file (written by beamtalk_repl_server after binding).
    // This is essential when port=0 is used (OS assigns ephemeral port).
    // Retry a few times since the BEAM node may still be initializing.
    let (actual_port, nonce) = if port == 0 {
        let mut discovered = None;
        for _ in 0..PORT_DISCOVERY_MAX_RETRIES {
            if let Some(port_nonce) = read_port_file(workspace_id)? {
                discovered = Some(port_nonce);
                break;
            }
            std::thread::sleep(Duration::from_millis(PORT_DISCOVERY_DELAY_MS));
        }
        let (p, n) = discovered.ok_or_else(|| {
            miette!(
                "BEAM node did not report its port.\n\
                 The workspace may have failed to start. Check logs."
            )
        })?;
        (p, n)
    } else {
        match read_port_file(workspace_id)? {
            Some((p, n)) => (p, n),
            None => (port, None),
        }
    };

    // Wait for TCP health endpoint to be fully ready before returning.
    wait_for_tcp_ready(actual_port, pid)?;

    // Create node info
    let node_info = NodeInfo {
        node_name: node_name.clone(),
        port: actual_port,
        pid,
        start_time,
        nonce,
    };

    // Save node info
    save_node_info(workspace_id, &node_info)?;

    Ok(node_info)
}

/// Poll until the TCP health endpoint responds on the given port.
///
/// Uses short per-attempt timeouts (300ms connect, 500ms read) to keep the
/// worst-case total bounded at ~25s rather than the ~175s that would result
/// from reusing `tcp_health_probe`'s production timeouts.
fn wait_for_tcp_ready(port: u16, pid: u32) -> Result<()> {
    let addr: std::net::SocketAddr = format!("127.0.0.1:{port}")
        .parse()
        .map_err(|e| miette!("Invalid address: {e}"))?;

    for _ in 0..READINESS_PROBE_MAX_RETRIES {
        if let Ok(stream) =
            TcpStream::connect_timeout(&addr, Duration::from_millis(READINESS_CONNECT_TIMEOUT_MS))
        {
            if stream
                .set_read_timeout(Some(Duration::from_millis(READINESS_READ_TIMEOUT_MS)))
                .is_err()
            {
                std::thread::sleep(Duration::from_millis(READINESS_PROBE_DELAY_MS));
                continue;
            }
            if let Ok(mut writer) = stream.try_clone() {
                let mut reader = BufReader::new(stream);
                if writer.write_all(b"{\"op\":\"health\"}\n").is_ok() && writer.flush().is_ok() {
                    let mut response = String::new();
                    if reader.read_line(&mut response).is_ok() && !response.is_empty() {
                        return Ok(());
                    }
                }
            }
        }
        std::thread::sleep(Duration::from_millis(READINESS_PROBE_DELAY_MS));
    }
    Err(miette!(
        "BEAM node started (PID {pid}) but TCP health endpoint on port {port} \
         did not become ready. The workspace may have failed to initialize."
    ))
}

/// Build a `Command` for starting a detached BEAM workspace node.
///
/// Extracted from `start_detached_node` so the command configuration
/// (args, env vars) can be inspected in tests without spawning a process.
#[allow(clippy::too_many_arguments)] // mirrors start_detached_node params for testability
fn build_detached_node_command(
    node_name: &str,
    cookie: &str,
    runtime_beam_dir: &Path,
    repl_beam_dir: &Path,
    jsx_beam_dir: &Path,
    compiler_beam_dir: &Path,
    stdlib_beam_dir: &Path,
    extra_code_paths: &[PathBuf],
    eval_cmd: &str,
    project_root: &Path,
) -> Command {
    let (node_flag, node_arg) = if node_name.contains('@') {
        ("-name", node_name.to_string())
    } else {
        ("-sname", node_name.to_string())
    };

    let mut args = vec![
        "-detached".to_string(),
        "-noshell".to_string(),
        node_flag.to_string(),
        node_arg,
        "-setcookie".to_string(),
        cookie.to_string(),
        "-pa".to_string(),
        runtime_beam_dir.to_str().unwrap_or("").to_string(),
        "-pa".to_string(),
        repl_beam_dir.to_str().unwrap_or("").to_string(),
        "-pa".to_string(),
        jsx_beam_dir.to_str().unwrap_or("").to_string(),
        "-pa".to_string(),
        compiler_beam_dir.to_str().unwrap_or("").to_string(),
        "-pa".to_string(),
        stdlib_beam_dir.to_str().unwrap_or("").to_string(),
    ];

    // Add extra code paths (e.g. package ebin from auto-compile)
    for path in extra_code_paths {
        args.push("-pa".to_string());
        args.push(path.to_str().unwrap_or("").to_string());
    }

    args.push("-eval".to_string());
    args.push(eval_cmd.to_string());

    let mut cmd = Command::new("erl");
    cmd.args(&args)
        .current_dir(project_root)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null());

    cmd
}

/// Find the PID of a BEAM process by its node name.
///
/// On Unix, uses `ps` to scan for BEAM processes. Only needed for force-kill
/// fallback — normal operation uses TCP probes for liveness/shutdown.
/// On non-Unix, returns sentinel PID 0 (force-kill uses `taskkill` with PID
/// from OS-level process tracking).
#[cfg_attr(not(unix), allow(clippy::unnecessary_wraps))]
fn find_beam_pid_by_node(node_name: &str) -> Result<(u32, Option<u64>)> {
    #[cfg(unix)]
    {
        let output = Command::new("ps")
            .args(["-eo", "pid,command"])
            .output()
            .into_diagnostic()?;

        let stdout = String::from_utf8_lossy(&output.stdout);
        for line in stdout.lines() {
            if line.contains("beam.smp") && line.contains(node_name) {
                let pid_str = line
                    .split_whitespace()
                    .next()
                    .ok_or_else(|| miette!("Failed to parse PID from ps output"))?;
                let pid: u32 = pid_str.parse().into_diagnostic()?;
                #[cfg(target_os = "linux")]
                let start_time = read_proc_start_time(pid);
                #[cfg(not(target_os = "linux"))]
                let start_time = None;
                return Ok((pid, start_time));
            }
        }

        Err(miette!("Could not find BEAM process for node {node_name}"))
    }

    #[cfg(not(unix))]
    {
        let _ = node_name;
        // Windows: PID tracking via ps not available; return sentinel.
        // Workspace liveness is verified via TCP probe instead.
        // Force-kill on Windows uses taskkill with PID from node.info.
        Ok((0, None))
    }
}

/// Poll until a workspace exits or timeout is reached.
///
/// Uses a lightweight TCP connect probe to check liveness (cross-platform).
/// Returns `Ok(())` if the workspace stops responding within `timeout_secs`,
/// or an error suggesting `--force` if it doesn't.
pub(super) fn wait_for_workspace_exit(port: u16, timeout_secs: u64) -> Result<()> {
    let interval = Duration::from_millis(100);
    let deadline = std::time::Instant::now() + Duration::from_secs(timeout_secs);
    let addr: std::net::SocketAddr = format!("127.0.0.1:{port}")
        .parse()
        .map_err(|e| miette!("Invalid address: {e}"))?;

    while std::time::Instant::now() < deadline {
        // Lightweight connect-only probe (no JSON exchange).
        // Connection refused = port released = process exited.
        if TcpStream::connect_timeout(&addr, Duration::from_millis(EXIT_PROBE_CONNECT_TIMEOUT_MS))
            .is_err()
        {
            return Ok(());
        }
        std::thread::sleep(interval);
    }

    Err(miette!(
        "Workspace on port {} did not exit within {}s. Try --force.",
        port,
        timeout_secs
    ))
}

/// Force-kill a process by PID.
///
/// Cross-platform: uses `kill -9` on Unix, `taskkill /F /PID` on Windows.
/// Used as fallback when TCP graceful shutdown fails or times out.
pub(super) fn force_kill_process(pid: u32) -> Result<()> {
    // PID 0 is sentinel for Windows when PID tracking is unavailable
    if pid == 0 {
        return Err(miette!(
            "Cannot force-kill: process ID unavailable. \
             Try stopping gracefully (without --force)."
        ));
    }

    #[cfg(unix)]
    {
        let status = Command::new("kill")
            .args(["-9", &pid.to_string()])
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .into_diagnostic()?;

        if !status.success() {
            return Err(miette!("Failed to kill process {pid}"));
        }
    }

    #[cfg(not(unix))]
    {
        let status = Command::new("taskkill")
            .args(["/F", "/PID", &pid.to_string()])
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .into_diagnostic()?;

        if !status.success() {
            return Err(miette!("Failed to kill process {pid}"));
        }
    }

    Ok(())
}

/// Wait for a node name to be deregistered from `epmd`.
///
/// After force-killing a BEAM node, `epmd` may still hold the registration
/// briefly. This polls `epmd -names` until the node name disappears or
/// timeout is reached. Returns `Ok(())` once deregistered, or `Err` on timeout.
#[cfg(all(unix, test))]
pub(super) fn wait_for_epmd_deregistration(node_name: &str, timeout_secs: u64) -> Result<()> {
    // Extract the short name (before '@') for epmd lookup
    let short_name = node_name.split('@').next().unwrap_or(node_name);

    let interval = Duration::from_millis(100);
    let deadline = std::time::Instant::now() + Duration::from_secs(timeout_secs);

    while std::time::Instant::now() < deadline {
        let output = Command::new("epmd")
            .args(["-names"])
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .output();

        match output {
            Ok(out) => {
                // If epmd exits with a non-success status, keep polling
                // rather than treating empty stdout as "deregistered".
                if !out.status.success() {
                    std::thread::sleep(interval);
                    continue;
                }

                let stdout = String::from_utf8_lossy(&out.stdout);
                // Match exact node name in epmd output format:
                //   "name <node_short_name> at port <N>"
                // Using token-level matching to avoid false positives
                // (e.g. "foo" matching "foobar").
                let still_registered = stdout.lines().any(|line| {
                    let mut parts = line.split_whitespace();
                    matches!(parts.next(), Some("name"))
                        && matches!(parts.next(), Some(name) if name == short_name)
                });

                if !still_registered {
                    return Ok(());
                }
            }
            Err(_) => {
                // epmd not available — nothing to wait for
                return Ok(());
            }
        }
        std::thread::sleep(interval);
    }

    Err(miette!(
        "Node '{}' still registered in epmd after {}s",
        short_name,
        timeout_secs
    ))
}
