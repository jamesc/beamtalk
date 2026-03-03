// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Workspace shutdown orchestration.
//!
//! Owns graceful → force-kill fallback shutdown: the TCP shutdown message,
//! the OS-level process kill, the exit-wait probe, and the top-level
//! `stop_workspace` orchestrator. Extracting "how to stop a workspace"
//! here leaves `lifecycle.rs` as a pure create/start/list/status orchestrator.
//!
//! **DDD Context:** CLI

use std::net::TcpStream;
use std::time::Duration;

use miette::{IntoDiagnostic, Result, miette};

use super::discovery;
use super::node_state::{TCP_READ_TIMEOUT_MS, is_node_running};
use super::storage::{
    cleanup_stale_node_info, generate_workspace_id, get_node_info, read_workspace_cookie,
    workspace_exists,
};
use crate::commands::protocol::ProtocolClient;

/// TCP connect timeout for exit probe in milliseconds.
const EXIT_PROBE_CONNECT_TIMEOUT_MS: u64 = 500;

/// Send a WebSocket shutdown message to a workspace (BT-611, ADR 0020).
///
/// Connects to the workspace's WebSocket endpoint, authenticates with the
/// cookie, sends a `{"op":"shutdown","cookie":"..."}` message, and waits for
/// acknowledgement. The workspace will call `init:stop()` for OTP-level
/// graceful teardown.
pub(super) fn tcp_send_shutdown(host: &str, port: u16, cookie: &str) -> Result<()> {
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
/// Cross-platform: uses `libc::kill(SIGKILL)` on Unix and `TerminateProcess` on Windows.
/// Used as fallback when TCP graceful shutdown fails or times out.
pub(super) fn force_kill_process(pid: u32) -> Result<()> {
    // PID 0 is sentinel for when PID tracking is unavailable
    if pid == 0 {
        return Err(miette!(
            "Cannot force-kill: process ID unavailable. \
             Try stopping gracefully (without --force)."
        ));
    }

    #[cfg(unix)]
    {
        let pid_i = i32::try_from(pid).map_err(|_| miette!("PID {pid} too large for platform"))?;
        // SAFETY: kill(2) is safe to call with a valid pid and signal number.
        let ret = unsafe { libc::kill(pid_i, libc::SIGKILL) };
        if ret == 0 {
            return Ok(());
        }
        let err = std::io::Error::last_os_error();
        if err.raw_os_error() == Some(libc::ESRCH) {
            // Process already exited — the goal of ensuring it is not running is achieved.
            // This is the expected outcome when graceful shutdown succeeds just after a
            // wait_for_workspace_exit timeout (race: BEAM exits naturally before we SIGKILL).
            return Ok(());
        }
        Err(miette!("Failed to kill process {pid}: {err}"))
    }

    #[cfg(windows)]
    {
        use windows_sys::Win32::Foundation::{CloseHandle, FALSE};
        use windows_sys::Win32::System::Threading::{
            OpenProcess, PROCESS_TERMINATE, TerminateProcess,
        };

        // SAFETY: Windows API call with documented parameters.
        let handle = unsafe { OpenProcess(PROCESS_TERMINATE, FALSE, pid) };
        if handle.is_null() {
            let err = std::io::Error::last_os_error();
            // ERROR_INVALID_PARAMETER (87) means the process no longer exists.
            if err.raw_os_error() == Some(87) {
                return Ok(());
            }
            return Err(miette!(
                "Failed to open process {pid} for termination: {err}"
            ));
        }
        // SAFETY: handle is valid, obtained from OpenProcess above.
        let ret = unsafe { TerminateProcess(handle, 1) };
        // Capture error *before* CloseHandle, which may clobber GetLastError.
        let term_err = if ret == FALSE {
            Some(std::io::Error::last_os_error())
        } else {
            None
        };
        // SAFETY: handle is valid, obtained from OpenProcess above.
        unsafe { CloseHandle(handle) };
        if ret != FALSE {
            Ok(())
        } else if term_err.as_ref().and_then(std::io::Error::raw_os_error) == Some(5) {
            // ERROR_ACCESS_DENIED (5): process exited between OpenProcess and
            // TerminateProcess — analogous to Unix ESRCH.
            Ok(())
        } else {
            Err(miette!(
                "Failed to kill process {pid}: {}",
                term_err.expect("term_err is Some when ret == FALSE")
            ))
        }
    }
}

/// Stop a workspace by name or ID.
///
/// If `name_or_id` is `None`, attempts to find the workspace for the current directory.
///
/// Uses TCP shutdown (graceful OTP teardown via `init:stop()`) as primary
/// mechanism. Falls back to OS-level force-kill if `force` is true or if
/// graceful shutdown times out.
pub fn stop_workspace(name_or_id: Option<&str>, force: bool) -> Result<()> {
    // Resolve workspace ID
    let workspace_id = if let Some(name) = name_or_id {
        super::lifecycle::resolve_workspace_id(name)?
    } else {
        let cwd = std::env::current_dir().into_diagnostic()?;
        let project_root = discovery::discover_project_root(&cwd);
        super::lifecycle::find_workspace_by_project_path(&project_root)?
            .unwrap_or(generate_workspace_id(&project_root)?)
    };

    if !workspace_exists(&workspace_id)? {
        return Err(match name_or_id {
            Some(name) => miette!("Workspace '{name}' does not exist"),
            None => miette!(
                "No workspace found for current directory. Specify a name: beamtalk workspace stop <name>"
            ),
        });
    }

    let node_info = get_node_info(&workspace_id)?;

    match node_info {
        Some(info) if is_node_running(&info, Some(&workspace_id)) => {
            let host = info.connect_host();
            if force {
                // Force-kill: skip graceful shutdown, go straight to OS kill.
                // On Windows PID may be 0 (sentinel) — fall back to graceful.
                if info.pid == 0 {
                    return Err(miette!(
                        "Force-kill is not available (process ID unknown). \
                         Use graceful shutdown instead (omit --force)."
                    ));
                }
                force_kill_process(info.pid)?;
                // Ensure the node has actually released its port before returning.
                wait_for_workspace_exit(host, info.port, 5).map_err(|_| {
                    miette!(
                        "Workspace did not release port {} within 5s after forced stop. \
                         It may still be shutting down; retry shortly.",
                        info.port
                    )
                })?;
            } else {
                eprintln!(
                    "Stopping workspace '{workspace_id}' (port {})...",
                    info.port
                );

                // Try graceful TCP shutdown first
                let cookie = read_workspace_cookie(&workspace_id)?;
                match tcp_send_shutdown(host, info.port, &cookie) {
                    Ok(()) => {
                        // Wait for the workspace to actually exit.
                        // OTP init:stop() does orderly application teardown which
                        // can take 10+ seconds under CI load.
                        if wait_for_workspace_exit(host, info.port, 30).is_err() {
                            // Graceful shutdown acknowledged but process didn't exit
                            // Fall back to force-kill (if PID available)
                            if info.pid == 0 {
                                return Err(miette!(
                                    "Graceful shutdown timed out. Cannot force-kill \
                                     (process ID unknown). Please manually stop \
                                     the BEAM process or retry."
                                ));
                            }
                            eprintln!("Graceful shutdown timed out, force-killing...");
                            force_kill_process(info.pid)?;
                            wait_for_workspace_exit(host, info.port, 5).map_err(|_| {
                                miette!(
                                    "Workspace did not release port {} within 5s after forced stop. \
                                     It may still be shutting down; retry shortly.",
                                    info.port
                                )
                            })?;
                        }
                    }
                    Err(e) => {
                        // TCP shutdown failed (e.g. connection refused, auth error)
                        // Fall back to force-kill (if PID available)
                        if info.pid == 0 {
                            return Err(miette!(
                                "TCP shutdown failed ({e}). Cannot force-kill \
                                 (process ID unknown). Please manually stop \
                                 the BEAM process or retry."
                            ));
                        }
                        eprintln!("TCP shutdown failed ({e}), force-killing...");
                        force_kill_process(info.pid)?;
                        wait_for_workspace_exit(host, info.port, 5).map_err(|_| {
                            miette!(
                                "Workspace did not release port {} within 5s after forced stop. \
                                 It may still be shutting down; retry shortly.",
                                info.port
                            )
                        })?;
                    }
                }
            }

            // Clean up node.info after process has exited
            cleanup_stale_node_info(&workspace_id)?;

            println!("Workspace '{workspace_id}' stopped");
            Ok(())
        }
        _ => Err(miette!("Workspace '{}' is not running", workspace_id)),
    }
}
