// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Process management for workspace BEAM nodes.
//!
//! Handles starting detached BEAM nodes, WebSocket health probes, shutdown,
//! and PID discovery.
//!
//! **DDD Context:** CLI

use std::net::Ipv4Addr;
use std::net::TcpStream;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::Duration;

#[cfg(windows)]
use std::os::windows::process::CommandExt;

use miette::{IntoDiagnostic, Result, miette};
use serde::Deserialize;

use crate::commands::protocol::ProtocolClient;

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

/// Maximum number of PID discovery attempts.
/// Total worst-case: 2s initial + 20 × 500ms = 12s.
const PID_DISCOVERY_MAX_RETRIES: usize = 20;

/// Delay between port file read attempts in milliseconds.
const PORT_DISCOVERY_DELAY_MS: u64 = 500;

/// Maximum number of port file discovery attempts.
const PORT_DISCOVERY_MAX_RETRIES: usize = 20;

/// Delay between TCP readiness probe retries in milliseconds.
const READINESS_PROBE_DELAY_MS: u64 = 200;

/// Maximum number of TCP readiness probe attempts.
/// Total worst-case: 30 × (300ms connect + 500ms read + 200ms sleep) = ~30s.
const READINESS_PROBE_MAX_RETRIES: usize = 30;

/// TCP read timeout for readiness probe in milliseconds.
const READINESS_READ_TIMEOUT_MS: u64 = 500;

/// TCP connect timeout for exit probe in milliseconds.
const EXIT_PROBE_CONNECT_TIMEOUT_MS: u64 = 500;

/// Check if a BEAM node is actually running (handle stale node.info files).
///
/// Uses a lightweight TCP connect probe (cross-platform) to verify the
/// workspace port is listening. If the workspace has a nonce, validates it
/// against the port file nonce to detect stale entries (PID reuse after crash).
pub fn is_node_running(info: &NodeInfo) -> bool {
    let addr = format!("127.0.0.1:{}", info.port);
    let Ok(addr) = addr.parse::<std::net::SocketAddr>() else {
        return false;
    };

    // Lightweight connect-only probe — if the port is listening, the node is likely alive
    if TcpStream::connect_timeout(&addr, Duration::from_millis(TCP_CONNECT_TIMEOUT_MS)).is_err() {
        return false;
    }

    // If we have a nonce, verify it against the port file for stale detection
    if let Some(ref expected_nonce) = info.nonce {
        // Read the port file nonce directly (no auth needed)
        match read_port_file_nonce(info.port) {
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
    #[derive(serde::Deserialize)]
    struct PortFile {
        port: u16,
        nonce: String,
    }

    let home_dir = dirs::home_dir()?;
    let workspaces_root = home_dir.join(".beamtalk").join("workspaces");

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
        if let Ok(parsed) = serde_json::from_str::<PortFile>(&contents) {
            if parsed.port == port {
                return Some(parsed.nonce);
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
pub fn tcp_health_probe(port: u16, cookie: &str) -> Result<HealthProbeResponse> {
    let mut client = ProtocolClient::connect(
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

/// Send a WebSocket shutdown message to a workspace (BT-611, ADR 0020).
///
/// Connects to the workspace's WebSocket endpoint, authenticates with the
/// cookie, sends a `{"op":"shutdown","cookie":"..."}` message, and waits for
/// acknowledgement. The workspace will call `init:stop()` for OTP-level
/// graceful teardown.
pub fn tcp_send_shutdown(port: u16, cookie: &str) -> Result<()> {
    let mut client = ProtocolClient::connect(
        port,
        cookie,
        Some(Duration::from_millis(TCP_READ_TIMEOUT_MS)),
    )?;

    // Send shutdown request with cookie
    let request = serde_json::json!({"op": "shutdown", "cookie": cookie});
    let response = client.send_raw(&request)?;

    // Check for error in response
    if let Some(error) = response.get("error") {
        return Err(miette!("Shutdown rejected: {error}"));
    }

    Ok(())
}

/// Start a detached BEAM node for a workspace.
/// Returns the `NodeInfo` for the started node.
#[allow(clippy::too_many_arguments)] // BEAM node requires separate beam dirs per OTP app
#[allow(clippy::too_many_lines)] // eval command construction is necessarily verbose
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
    bind_addr: Option<Ipv4Addr>,
    ssl_dist_optfile: Option<&Path>,
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

    // On Windows, escape backslashes in the project path for Erlang string syntax (BT-661)
    #[cfg(windows)]
    let project_path_str = project_path_str.replace('\\', "\\\\");

    // Format bind address as Erlang tuple for cowboy socket_opts
    let bind_addr_erl = beamtalk_cli::repl_startup::format_bind_addr_erl(bind_addr);

    let eval_cmd = format!(
        "application:set_env(beamtalk_runtime, workspace_id, <<\"{workspace_id}\">>), \
         application:set_env(beamtalk_runtime, project_path, <<\"{project_path_str}\">>), \
         application:set_env(beamtalk_runtime, tcp_port, {port}), \
         {{ok, _}} = application:ensure_all_started(beamtalk_workspace), \
         {{ok, _}} = beamtalk_workspace_sup:start_link(#{{workspace_id => <<\"{workspace_id}\">>, \
                                                          project_path => <<\"{project_path_str}\">>, \
                                                          tcp_port => {port}, \
                                                          bind_addr => {bind_addr_erl}, \
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
        ssl_dist_optfile,
    );

    let child = cmd.spawn().map_err(|e| {
        miette!("Failed to start detached BEAM node: {e}\nIs Erlang/OTP installed?")
    })?;

    // Windows-specific handling (BT-662 fix):
    // On Windows, Erlang's -detached flag doesn't work when spawned from Rust Command::spawn().
    // Instead, we omit -detached and use CREATE_NO_WINDOW + CREATE_NEW_PROCESS_GROUP flags
    // to achieve similar behavior. We must use mem::forget() to prevent Rust from killing
    // the process when the Child handle is dropped.
    #[cfg(windows)]
    {
        std::mem::forget(child);
    }
    #[cfg(not(windows))]
    {
        // On Unix, -detached makes the BEAM process fully independent
        let _ = child;
    }

    // With -detached (Unix) or CREATE_NEW_PROCESS_GROUP (Windows), the spawn() returns
    // immediately and the real BEAM node runs independently. We need to wait for it to start up.
    // The compiler app (ADR 0022) adds ~500ms to startup, and under load
    // (e.g., parallel integration tests) it can take longer.
    // Retry PID discovery instead of a single fixed sleep.
    let mut last_err = None;
    let pid_found = 'retry: {
        for attempt in 0..PID_DISCOVERY_MAX_RETRIES {
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

    // Wait for WebSocket health endpoint to be fully ready before returning.
    wait_for_tcp_ready(actual_port, pid, &cookie)?;

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

/// Convert a Windows path to Unix-style forward slashes for Erlang.
///
/// Erlang on Windows expects forward slashes in `-pa` arguments, not backslashes.
/// See BT-661 for details on this Windows-specific issue.
fn path_to_erlang_arg(path: &Path) -> String {
    #[cfg(windows)]
    {
        path.to_string_lossy().replace('\\', "/")
    }
    #[cfg(not(windows))]
    {
        path.to_string_lossy().into_owned()
    }
}

/// Poll until the WebSocket health endpoint responds on the given port.
///
/// Uses short per-attempt timeouts to keep the worst-case total bounded.
fn wait_for_tcp_ready(port: u16, pid: u32, cookie: &str) -> Result<()> {
    for _ in 0..READINESS_PROBE_MAX_RETRIES {
        if let Ok(mut client) = ProtocolClient::connect(
            port,
            cookie,
            Some(Duration::from_millis(READINESS_READ_TIMEOUT_MS)),
        ) {
            let request = serde_json::json!({"op": "health"});
            if client.send_raw(&request).is_ok() {
                return Ok(());
            }
        }
        std::thread::sleep(Duration::from_millis(READINESS_PROBE_DELAY_MS));
    }
    Err(miette!(
        "BEAM node started (PID {pid}) but WebSocket health endpoint on port {port} \
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
    ssl_dist_optfile: Option<&Path>,
) -> Command {
    let (node_flag, node_arg) = if node_name.contains('@') {
        ("-name", node_name.to_string())
    } else {
        ("-sname", node_name.to_string())
    };

    let mut args = vec![
        // On Windows, don't use -detached - it doesn't work reliably from Rust spawn
        // Instead, we'll use CREATE_NO_WINDOW flag to prevent console popup
        #[cfg(not(windows))]
        "-detached".to_string(),
        "-noshell".to_string(),
        node_flag.to_string(),
        node_arg,
        "-setcookie".to_string(),
        cookie.to_string(),
        "-pa".to_string(),
        path_to_erlang_arg(runtime_beam_dir),
        "-pa".to_string(),
        path_to_erlang_arg(repl_beam_dir),
        "-pa".to_string(),
        path_to_erlang_arg(jsx_beam_dir),
        "-pa".to_string(),
        path_to_erlang_arg(compiler_beam_dir),
        "-pa".to_string(),
        path_to_erlang_arg(stdlib_beam_dir),
    ];

    // Add extra code paths (e.g. package ebin from auto-compile)
    for path in extra_code_paths {
        args.push("-pa".to_string());
        args.push(path_to_erlang_arg(path));
    }

    // Add TLS distribution args if configured (ADR 0020 Phase 2)
    if let Some(conf_path) = ssl_dist_optfile {
        args.push("-proto_dist".to_string());
        args.push("inet_tls".to_string());
        args.push("-ssl_dist_optfile".to_string());
        args.push(path_to_erlang_arg(conf_path));
    }

    args.push("-eval".to_string());
    args.push(eval_cmd.to_string());

    let mut cmd = Command::new("erl");
    cmd.args(&args)
        .current_dir(project_root)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null());

    // On Windows, set process creation flags to properly detach
    // CREATE_NO_WINDOW (0x0800_0000) prevents console window popup without -detached
    // CREATE_NEW_PROCESS_GROUP (0x0000_0200) allows the process to run independently
    #[cfg(windows)]
    {
        const CREATE_NO_WINDOW: u32 = 0x0800_0000;
        const CREATE_NEW_PROCESS_GROUP: u32 = 0x0000_0200;
        cmd.creation_flags(CREATE_NO_WINDOW | CREATE_NEW_PROCESS_GROUP);
    }

    // Set compiler port binary path (for runtime compilation support)
    // In installed mode, it lives next to the beamtalk binary in bin/.
    // In dev mode, it lives in target/{debug,release}/.
    if let Ok(exe) = std::env::current_exe() {
        if let Some(bin_dir) = exe.parent() {
            let compiler_name = if cfg!(windows) {
                "beamtalk-compiler-port.exe"
            } else {
                "beamtalk-compiler-port"
            };
            let compiler_port = bin_dir.join(compiler_name);
            if compiler_port.exists() {
                cmd.env("BEAMTALK_COMPILER_PORT_BIN", &compiler_port);
            }
        }
    }

    cmd
}

/// Find the PID of a BEAM process by its node name.
///
/// Uses `sysinfo` crate for cross-platform process scanning (ADR 0027).
/// Only needed for force-kill fallback — normal operation uses TCP probes
/// for liveness/shutdown.
fn find_beam_pid_by_node(node_name: &str) -> Result<(u32, Option<u64>)> {
    use sysinfo::{ProcessRefreshKind, ProcessesToUpdate, System, UpdateKind};

    let mut system = System::new();
    system.refresh_processes_specifics(
        ProcessesToUpdate::All,
        true,
        ProcessRefreshKind::new().with_cmd(UpdateKind::Always),
    );

    for (pid, process) in system.processes() {
        let cmd = process.cmd();
        // Match "erl", "erl.exe", or "beam.smp" in command and node name in arguments
        let is_beam = cmd.iter().any(|arg| {
            let arg_str = arg.to_string_lossy();
            arg_str.contains("erl") || arg_str.contains("beam.smp")
        });
        let has_node = cmd
            .iter()
            .any(|arg| arg.to_string_lossy().contains(node_name));

        if is_beam && has_node {
            // On Linux, try to read /proc start time for stale detection
            #[cfg(target_os = "linux")]
            let start_time = read_proc_start_time(pid.as_u32());
            #[cfg(not(target_os = "linux"))]
            let start_time = None;

            return Ok((pid.as_u32(), start_time));
        }
    }

    Err(miette!("Could not find BEAM process for node {node_name}"))
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
/// Cross-platform: uses `sysinfo` for reliable process termination (ADR 0027).
/// Used as fallback when TCP graceful shutdown fails or times out.
pub(super) fn force_kill_process(pid: u32) -> Result<()> {
    use sysinfo::{Pid, ProcessRefreshKind, ProcessesToUpdate, System};

    // PID 0 is sentinel for when PID tracking is unavailable
    if pid == 0 {
        return Err(miette!(
            "Cannot force-kill: process ID unavailable. \
             Try stopping gracefully (without --force)."
        ));
    }

    let mut system = System::new();
    system.refresh_processes_specifics(
        ProcessesToUpdate::Some(&[Pid::from_u32(pid)]),
        true,
        ProcessRefreshKind::new(),
    );

    if let Some(process) = system.process(Pid::from_u32(pid)) {
        if process.kill() {
            Ok(())
        } else {
            Err(miette!("Failed to kill process {pid}"))
        }
    } else {
        Err(miette!("Process {pid} not found"))
    }
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
