// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Workspace node startup sequence.
//!
//! Owns `start_detached_node` — the entry point that ties together
//! `startup_command` (command construction), `node_state` (liveness checks),
//! and `epmd` (name-conflict detection) to start a BEAM workspace node and
//! wait for it to be ready.
//!
//! **DDD Context:** CLI

use std::net::Ipv4Addr;
use std::path::{Path, PathBuf};
use std::process::Stdio;
use std::time::Duration;

use beamtalk_cli::repl_startup::BeamPaths;

#[cfg(target_os = "linux")]
use super::storage::read_proc_start_time;
use super::storage::{
    NodeInfo, get_workspace_metadata, read_port_file, read_workspace_cookie, remove_file_if_exists,
    remove_stale_runtime_files, save_node_info, workspace_dir,
};

use super::node_state::{
    PID_DISCOVERY_INITIAL_DELAY_MS, PID_DISCOVERY_MAX_RETRIES, PID_DISCOVERY_RETRY_DELAY_MS,
    is_process_alive,
};
use super::startup_command::{build_detached_node_command, write_cookie_args_file};

use crate::commands::protocol::ProtocolClient;
use miette::{Result, miette};

/// Default idle timeout in seconds (4 hours)
const DEFAULT_IDLE_TIMEOUT_SECONDS: u64 = 3600 * 4;

/// Delay between port file read attempts in milliseconds.
const PORT_DISCOVERY_DELAY_MS: u64 = 500;

/// Maximum number of port file discovery attempts.
const PORT_DISCOVERY_MAX_RETRIES: usize = 20;

/// Delay between TCP readiness probe retries in milliseconds.
const READINESS_PROBE_DELAY_MS: u64 = 200;

/// Maximum number of TCP readiness probe attempts.
///
/// Two failure modes drive this constant:
///
/// 1. **ECONNREFUSED window**: rarely, a brief window exists where the port file
///    has been written but TCP connections still fail (e.g. during supervisor
///    restart or OS scheduler jitter on a loaded CI runner). Each ECONNREFUSED
///    returns immediately, costing only the 200 ms sleep per retry.
///    100 × 200 ms = 20 s budget for this window.
///
/// 2. **Slow auth**: once connected, `ProtocolClient::connect` performs a full
///    WebSocket auth exchange. This is covered by `READINESS_READ_TIMEOUT_MS`
///    below; with a 10 s per-attempt timeout, auth succeeds on the first attempt
///    and these retries are not consumed.
const READINESS_PROBE_MAX_RETRIES: usize = 100;

/// TCP read timeout for the WebSocket auth handshake during readiness probing.
///
/// `ProtocolClient::connect` performs a full WebSocket auth exchange before
/// returning. On a heavily-loaded CI runner (12 sequential BEAM nodes), the
/// BEAM VM can take > 500ms to respond to the HTTP upgrade or send the
/// `auth-required` message — causing every probe attempt to time out even
/// though the port IS open. 10 s gives ample headroom; once the port is open
/// auth completes on the first attempt and the retry budget is not consumed.
const READINESS_READ_TIMEOUT_MS: u64 = 10_000;

/// Start a detached BEAM node for a workspace.
/// Returns the `NodeInfo` for the started node.
#[allow(clippy::too_many_arguments)] // workspace node startup requires many independent parameters
#[allow(clippy::too_many_lines)] // eval command construction is necessarily verbose
pub fn start_detached_node(
    workspace_id: &str,
    port: u16,
    beam_paths: &BeamPaths,
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

    // If a `starting` tombstone is present, a previous startup was interrupted
    // mid-flight (crash, OOM, SIGKILL, etc.).  Clean up all runtime files —
    // including the tombstone itself — before attempting a fresh start (BT-969).
    // `remove_stale_runtime_files` also handles the BT-967 case (stale port/pid/
    // node.info from a previous aborted run without a tombstone).
    remove_stale_runtime_files(workspace_id)?;

    // Write the tombstone before spawning the BEAM node.  If startup is
    // interrupted at any point after this, the next call will detect the file
    // and trigger the cleanup above (BT-969).
    let tombstone_path = workspace_dir(workspace_id)?.join("starting");
    std::fs::write(&tombstone_path, b"").map_err(|e| {
        miette!(
            "Failed to write startup tombstone {}: {e}",
            tombstone_path.display()
        )
    })?;

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
        Some(p) => p.to_string(),
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

    // Redirect BEAM node stderr to a log file for crash diagnostics.
    // On startup failure the log will contain OTP crash reports or error_logger output.
    let startup_log_path = workspace_dir(workspace_id)?.join("startup.log");

    // Start detached BEAM node
    let mut cmd = build_detached_node_command(
        &node_name,
        &cookie_args_file,
        beam_paths,
        extra_code_paths,
        &eval_cmd,
        &project_path,
        ssl_dist_optfile,
    );

    // Override the default /dev/null stderr with a workspace log file.
    // Errors opening the log file are non-fatal — we fall back to /dev/null.
    let startup_log_enabled = match std::fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(&startup_log_path)
    {
        Ok(log_file) => {
            cmd.stderr(Stdio::from(log_file));
            true
        }
        Err(e) => {
            eprintln!(
                "Warning: could not open startup log {}: {e}",
                startup_log_path.display()
            );
            false
        }
    };

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
    wait_for_tcp_ready(
        node_info.connect_host(),
        actual_port,
        pid,
        &cookie,
        startup_log_enabled.then_some(startup_log_path.as_path()),
    )?;

    // Save node info
    save_node_info(workspace_id, &node_info)?;

    // Remove the tombstone now that startup completed successfully (BT-969).
    // On any failure path the tombstone is deliberately left in place so the
    // next `start_detached_node` call can detect and clean up the partial state.
    let tombstone_path = workspace_dir(workspace_id)?.join("starting");
    remove_file_if_exists(&tombstone_path)?;

    Ok(node_info)
}

/// Poll until the WebSocket health endpoint responds on the given port.
///
/// Uses short per-attempt timeouts to keep the worst-case total bounded.
/// `log_path` is `Some(path)` only when the startup log file was successfully
/// opened; it is included in timeout error messages to guide diagnosis.
fn wait_for_tcp_ready(
    host: &str,
    port: u16,
    pid: u32,
    cookie: &str,
    log_path: Option<&std::path::Path>,
) -> Result<()> {
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

    // Check whether the BEAM node is still alive to give a more actionable error.
    // If the process is gone it crashed during startup; if it's still running
    // it is either stuck initializing or the WebSocket stack failed to come up.
    let node_alive = is_process_alive(pid);

    // Only mention the log file if we actually opened it (log_path is Some).
    let log_suffix = log_path
        .map(|p| format!(" Check {} for startup logs.", p.display()))
        .unwrap_or_default();

    if node_alive {
        Err(miette!(
            "BEAM node started (PID {pid}) but WebSocket health endpoint on port {port} \
             did not become ready within the timeout. The workspace may be initializing \
             slowly — try again.{log_suffix}"
        ))
    } else {
        Err(miette!(
            "BEAM node (PID {pid}) crashed during startup before WebSocket endpoint \
             on port {port} became ready. Ensure Erlang/OTP is installed correctly.{log_suffix}"
        ))
    }
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
