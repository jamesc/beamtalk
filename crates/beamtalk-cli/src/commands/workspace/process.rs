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

#[cfg(unix)]
use std::os::unix::process::CommandExt as _;

use miette::{IntoDiagnostic, Result, miette};
use serde::Deserialize;

use crate::commands::protocol::ProtocolClient;

#[cfg(target_os = "linux")]
use super::storage::read_proc_start_time;
use super::storage::{
    NodeInfo, get_workspace_metadata, read_port_file, read_workspace_cookie,
    remove_stale_runtime_files, save_node_info, workspace_dir,
};

/// Default idle timeout in seconds (4 hours)
const DEFAULT_IDLE_TIMEOUT_SECONDS: u64 = 3600 * 4;

/// TCP connect timeout in milliseconds.
const TCP_CONNECT_TIMEOUT_MS: u64 = 2000;

/// TCP read timeout in milliseconds.
const TCP_READ_TIMEOUT_MS: u64 = 5000;

/// Initial delay before first PID discovery attempt in milliseconds.
///
/// With PID-file discovery (vs the old sysinfo process-list scanning), the file
/// appears as soon as the BEAM VM starts its eval command — before OTP apps load.
/// 500ms is enough for `-detached` fork + exec + VM boot on most systems.
const PID_DISCOVERY_INITIAL_DELAY_MS: u64 = 500;

/// Delay between PID discovery retry attempts in milliseconds.
const PID_DISCOVERY_RETRY_DELAY_MS: u64 = 500;

/// Maximum number of PID discovery attempts.
/// Total worst-case: 500ms initial + 30 × 500ms = 15.5s.
const PID_DISCOVERY_MAX_RETRIES: usize = 30;

/// Delay between port file read attempts in milliseconds.
const PORT_DISCOVERY_DELAY_MS: u64 = 500;

/// Maximum number of port file discovery attempts.
const PORT_DISCOVERY_MAX_RETRIES: usize = 20;

/// Delay between TCP readiness probe retries in milliseconds.
const READINESS_PROBE_DELAY_MS: u64 = 200;

/// Maximum number of TCP readiness probe attempts.
///
/// When the port is not yet bound, `connect()` returns ECONNREFUSED immediately,
/// so the real budget is RETRIES × `DELAY_MS` (not RETRIES × `connect_timeout`).
/// 150 × 200ms = 30s budget for the BEAM node to start listening.
const READINESS_PROBE_MAX_RETRIES: usize = 150;

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

/// Send a WebSocket shutdown message to a workspace (BT-611, ADR 0020).
///
/// Connects to the workspace's WebSocket endpoint, authenticates with the
/// cookie, sends a `{"op":"shutdown","cookie":"..."}` message, and waits for
/// acknowledgement. The workspace will call `init:stop()` for OTP-level
/// graceful teardown.
pub fn tcp_send_shutdown(host: &str, port: u16, cookie: &str) -> Result<()> {
    let mut client = ProtocolClient::connect(
        host,
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
    web_port: Option<u16>,
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

    // Remove stale runtime files (pid, port, node.info) from any previous run.
    // Without this, the port/PID discovery loops may read stale values and connect
    // to the wrong BEAM node, causing auth failures in wait_for_tcp_ready.
    remove_stale_runtime_files(workspace_id)?;

    // Compute the PID file path so the BEAM node can write its own PID for reliable discovery.
    // This avoids flaky process-list scanning via sysinfo (which can miss newly-forked processes
    // on loaded CI runners). The workspace dir is guaranteed to exist before we start the node.
    let pid_file_path = workspace_dir(workspace_id)?.join("pid");
    let pid_file_path_str = pid_file_path
        .to_str()
        .ok_or_else(|| miette!("PID file path contains invalid UTF-8: {:?}", pid_file_path))?
        .to_owned();
    #[cfg(windows)]
    let pid_file_path_str = pid_file_path_str.replace('\\', "\\\\");

    // Format bind address as Erlang tuple for cowboy socket_opts
    let bind_addr_erl = beamtalk_cli::repl_startup::format_bind_addr_erl(bind_addr);

    // Format web_port for Erlang (BT-689)
    let web_port_erl = match web_port {
        Some(p) => format!("{p}"),
        None => "undefined".to_string(),
    };

    let eval_cmd = format!(
        "ok = file:write_file(\"{pid_file_path_str}\", os:getpid()), \
         application:set_env(beamtalk_runtime, workspace_id, <<\"{workspace_id}\">>), \
         application:set_env(beamtalk_runtime, project_path, <<\"{project_path_str}\">>), \
         application:set_env(beamtalk_runtime, tcp_port, {port}), \
         application:set_env(beamtalk_runtime, web_port, {web_port_erl}), \
         {{ok, _}} = application:ensure_all_started(beamtalk_workspace), \
         {{ok, _}} = beamtalk_workspace_sup:start_link(#{{workspace_id => <<\"{workspace_id}\">>, \
                                                          project_path => <<\"{project_path_str}\">>, \
                                                          tcp_port => {port}, \
                                                          bind_addr => {bind_addr_erl}, \
                                                          web_port => {web_port_erl}, \
                                                          auto_cleanup => {auto_cleanup}, \
                                                          max_idle_seconds => {idle_timeout}}}), \
         {{ok, ActualPort}} = beamtalk_repl_server:get_port(), \
         io:format(\"Workspace {workspace_id} started on port ~B~n\", [ActualPort]), \
         receive stop -> ok end."
    );

    // Write cookie to args file (BT-726: not visible in `ps aux`)
    let cookie_args_file = write_cookie_args_file(workspace_id, &cookie)?;

    // Start detached BEAM node
    let mut cmd = build_detached_node_command(
        &node_name,
        &cookie_args_file,
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

    // Windows-specific handling (BT-662, BT-727):
    // On Windows, Erlang's -detached flag doesn't work when spawned from Rust Command::spawn().
    // Instead, we omit -detached and use CREATE_NO_WINDOW + CREATE_NEW_PROCESS_GROUP flags
    // to achieve similar behavior. Dropping the Child handle closes the OS handle via
    // CloseHandle() without terminating the process — the BEAM node continues running.
    // (Previously used mem::forget which leaked the handle.)
    #[cfg(windows)]
    {
        drop(child);
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
    //
    // Reliable PID discovery via PID file: the eval command writes the BEAM node's own PID to
    // {workspace_dir}/pid immediately on startup. This avoids flaky sysinfo process-list scanning
    // which can miss newly-forked -detached processes on loaded CI runners (the double-fork in
    // Erlang's daemon mode means the final process PID is not predictable from the outside).
    let pid = 'retry: {
        for attempt in 0..PID_DISCOVERY_MAX_RETRIES {
            if attempt > 0 {
                std::thread::sleep(Duration::from_millis(PID_DISCOVERY_RETRY_DELAY_MS));
            } else {
                // Initial delay before first attempt
                std::thread::sleep(Duration::from_millis(PID_DISCOVERY_INITIAL_DELAY_MS));
            }
            if let Some(pid) = read_pid_file(workspace_id)? {
                break 'retry pid;
            }
        }
        return Err(miette!(
            "BEAM node did not write PID file within timeout.\n\
             The workspace may have failed to start. Check Erlang/OTP is installed."
        ));
    };

    #[cfg(target_os = "linux")]
    let start_time = read_proc_start_time(pid);
    #[cfg(not(target_os = "linux"))]
    let start_time = None;

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

    // Create node info (BT-694: store bind_addr for reconnection)
    let node_info = NodeInfo {
        node_name: node_name.clone(),
        port: actual_port,
        pid,
        start_time,
        nonce,
        bind_addr: bind_addr.map(|a| a.to_string()),
    };

    // Wait for WebSocket health endpoint to be fully ready before returning.
    wait_for_tcp_ready(node_info.connect_host(), actual_port, pid, &cookie)?;

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
fn wait_for_tcp_ready(host: &str, port: u16, pid: u32, cookie: &str) -> Result<()> {
    for _ in 0..READINESS_PROBE_MAX_RETRIES {
        if let Ok(mut client) = ProtocolClient::connect(
            host,
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

/// Write an Erlang args file containing the cookie for secure distribution.
///
/// Uses `-args_file` instead of `-setcookie` on the command line to prevent
/// the cookie from being visible in `ps aux` / `/proc/{pid}/cmdline` (BT-726).
/// The file is created with mode 0600 on Unix for owner-only access.
fn write_cookie_args_file(workspace_id: &str, cookie: &str) -> Result<PathBuf> {
    let args_file_path = super::storage::workspace_dir(workspace_id)?.join("vm.args");
    let content = format!("-setcookie {cookie}\n");

    #[cfg(unix)]
    {
        use std::io::Write;
        use std::os::unix::fs::{OpenOptionsExt, PermissionsExt};

        let mut file = std::fs::OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .mode(0o600)
            .open(&args_file_path)
            .map_err(|e| miette!("Failed to write cookie args file: {e}"))?;

        file.set_permissions(std::fs::Permissions::from_mode(0o600))
            .map_err(|e| miette!("Failed to set cookie args file permissions: {e}"))?;

        file.write_all(content.as_bytes())
            .map_err(|e| miette!("Failed to write cookie args file: {e}"))?;
    }

    #[cfg(not(unix))]
    {
        std::fs::write(&args_file_path, content)
            .map_err(|e| miette!("Failed to write cookie args file: {e}"))?;
    }

    Ok(args_file_path)
}

/// Build a `Command` for starting a detached BEAM workspace node.
///
/// Extracted from `start_detached_node` so the command configuration
/// (args, env vars) can be inspected in tests without spawning a process.
///
/// # Security hardening (BT-726)
///
/// - **Environment allowlist**: `env_clear()` prevents leaking sensitive env vars
///   (`AWS_*`, `DATABASE_URL`, `SSH_AUTH_SOCK`, etc.) to the BEAM node.
/// - **Cookie via args file**: Uses `-args_file` instead of `-setcookie` so the
///   cookie is not visible in `ps aux` / `/proc/{pid}/cmdline`.
/// - **Umask**: Set to 0077 on Unix so workspace files are owner-only.
/// - **setsid**: Creates a new session on Unix for proper daemon detachment.
#[allow(clippy::too_many_arguments)] // mirrors start_detached_node params for testability
fn build_detached_node_command(
    node_name: &str,
    cookie_args_file: &Path,
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
        // Cookie via args file instead of -setcookie (BT-726: not visible in ps)
        "-args_file".to_string(),
        path_to_erlang_arg(cookie_args_file),
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

    // Security: clear inherited environment, then allowlist only required vars (BT-726).
    // Prevents leaking AWS_*, DATABASE_URL, SSH_AUTH_SOCK, etc.
    cmd.env_clear();

    // Erlang needs PATH to find epmd and other tools
    if let Ok(path) = std::env::var("PATH") {
        cmd.env("PATH", path);
    }
    // HOME is needed for Erlang's ~/.erlang.cookie fallback and epmd
    if let Ok(home) = std::env::var("HOME") {
        cmd.env("HOME", home);
    }
    // Locale settings for proper string handling
    for var in &["LANG", "LC_ALL"] {
        if let Ok(val) = std::env::var(var) {
            cmd.env(var, val);
        }
    }
    // Terminal type (for erl console compatibility)
    if let Ok(term) = std::env::var("TERM") {
        cmd.env("TERM", term);
    }
    // Temp directory (used by Erlang's file module and System.getEnv:)
    for var in &["TMPDIR", "TEMP", "TMP"] {
        if let Ok(val) = std::env::var(var) {
            cmd.env(var, val);
        }
    }
    // Windows-specific system variables required for reliable BEAM startup (BT-727).
    // USERPROFILE is the Windows equivalent of HOME; SystemRoot, COMSPEC, PATHEXT,
    // and APPDATA are needed by Erlang and child processes on Windows.
    #[cfg(windows)]
    for var in &["USERPROFILE", "SystemRoot", "COMSPEC", "PATHEXT", "APPDATA"] {
        if let Ok(val) = std::env::var(var) {
            cmd.env(var, val);
        }
    }

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

    // Security: set umask and create new session for proper daemon behavior (BT-726)
    #[cfg(unix)]
    {
        // SAFETY: setsid() and umask() are async-signal-safe per POSIX.
        // They are safe to call in pre_exec (between fork and exec).
        unsafe {
            cmd.pre_exec(|| {
                // Restrictive umask: workspace files are owner-only (0077)
                libc::umask(0o077);
                // New session: fully detach from controlling terminal.
                // Note: -detached already calls setsid() internally; this is
                // harmless (returns EPERM) but we need pre_exec for umask anyway.
                libc::setsid();
                Ok(())
            });
        }
    }

    // Set compiler port binary path (for runtime compilation support).
    // Preserve any user-provided BEAMTALK_COMPILER_PORT_BIN (e.g. via Nix/Homebrew
    // wrapper), falling back to auto-discovery next to the beamtalk binary.
    if let Ok(user_compiler_port) = std::env::var("BEAMTALK_COMPILER_PORT_BIN") {
        cmd.env("BEAMTALK_COMPILER_PORT_BIN", user_compiler_port);
    } else if let Ok(exe) = std::env::current_exe() {
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

/// Read the PID written by the BEAM node to its workspace PID file.
///
/// The BEAM node writes its own OS PID (via `os:getpid()`) to `{workspace_dir}/pid`
/// as the very first step of its eval command. This is the primary PID discovery
/// mechanism — it is more reliable than sysinfo process-list scanning, which can
/// miss newly-forked `-detached` processes on loaded CI runners.
///
/// Returns `Ok(None)` if the file does not yet exist (node still starting up).
/// Returns `Err` for permission or other unexpected IO failures so the caller
/// surfaces a precise error rather than a generic timeout message.
fn read_pid_file(workspace_id: &str) -> Result<Option<u32>> {
    let pid_path = workspace_dir(workspace_id)?.join("pid");
    let content = match std::fs::read_to_string(&pid_path) {
        Ok(c) => c,
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(err) => {
            return Err(miette!(
                "Failed to read PID file {}: {err}",
                pid_path.display()
            ));
        }
    };
    // Treat 0 as invalid: it is the "PID unavailable" sentinel used in force-kill flows.
    Ok(match content.trim().parse::<u32>().ok() {
        Some(0) | None => None,
        Some(pid) => Some(pid),
    })
}

/// Poll until a workspace exits or timeout is reached.
///
/// Uses a lightweight TCP connect probe to check liveness (cross-platform).
/// Returns `Ok(())` if the workspace stops responding within `timeout_secs`,
/// or an error suggesting `--force` if it doesn't.
pub(super) fn wait_for_workspace_exit(host: &str, port: u16, timeout_secs: u64) -> Result<()> {
    let interval = Duration::from_millis(100);
    let deadline = std::time::Instant::now() + Duration::from_secs(timeout_secs);
    let addr: std::net::SocketAddr = format!("{host}:{port}")
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
        ProcessRefreshKind::nothing(),
    );

    if let Some(process) = system.process(Pid::from_u32(pid)) {
        if process.kill() {
            Ok(())
        } else {
            Err(miette!("Failed to kill process {pid}"))
        }
    } else {
        // Process already exited — the goal of ensuring it is not running is achieved.
        // This is the expected outcome when graceful shutdown succeeds just after a
        // wait_for_workspace_exit timeout (race: BEAM exits naturally before we SIGKILL).
        Ok(())
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::OsStr;

    /// Helper: build a test command and return its env vars and args.
    fn build_test_command() -> Command {
        let tmp = std::env::temp_dir();
        let cookie_file = tmp.join("test_cookie_args");
        let beam_dir = tmp.join("test_beam");
        build_detached_node_command(
            "test_node@localhost",
            &cookie_file,
            &beam_dir,
            &beam_dir,
            &beam_dir,
            &beam_dir,
            &beam_dir,
            &[],
            "ok.",
            &tmp,
            None,
        )
    }

    #[test]
    fn test_build_command_clears_env_and_restores_path() {
        // After env_clear(), PATH should be restored from current process
        let cmd = build_test_command();
        let envs: Vec<_> = cmd.get_envs().collect();
        let has_path = envs.iter().any(|(k, _)| k == &OsStr::new("PATH"));
        assert!(has_path, "PATH must be in the env allowlist");
    }

    #[cfg(windows)]
    #[test]
    fn test_build_command_restores_windows_env_vars() {
        let cmd = build_test_command();
        let env_keys: Vec<_> = cmd
            .get_envs()
            .map(|(k, _)| k.to_string_lossy().to_string())
            .collect();
        for var in &["USERPROFILE", "SystemRoot", "COMSPEC", "PATHEXT", "APPDATA"] {
            // Only assert presence if the var exists in the current env
            if std::env::var(var).is_ok() {
                assert!(
                    env_keys.contains(&var.to_string()),
                    "{var} must be in the env allowlist on Windows"
                );
            }
        }
    }

    #[test]
    fn test_build_command_includes_eval_arg() {
        let cmd = build_test_command();
        let args: Vec<_> = cmd.get_args().collect();
        assert!(
            args.contains(&OsStr::new("-eval")),
            "must include -eval arg"
        );
        assert!(
            args.contains(&OsStr::new("ok.")),
            "must include the eval command"
        );
    }

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
