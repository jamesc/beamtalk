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

use miette::{Result, miette};

use super::node_state::{TCP_READ_TIMEOUT_MS, is_node_running};
use super::storage::{
    acquire_workspace_lock, cleanup_stale_node_info, get_node_info, read_workspace_cookie,
    workspace_exists,
};
use crate::commands::protocol::ProtocolClient;

/// TCP connect timeout for exit probe in milliseconds.
const EXIT_PROBE_CONNECT_TIMEOUT_MS: u64 = 500;

/// How long to wait for port release after a forced kill, in seconds.
const FORCE_KILL_WAIT_TIMEOUT_SECS: u64 = 5;

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

/// Force-kill a process and wait for it to release its port.
///
/// Combines `force_kill_process` and `wait_for_workspace_exit` into a single
/// operation used in multiple shutdown fallback paths.
fn force_kill_and_wait(pid: u32, host: &str, port: u16, timeout_secs: u64) -> Result<()> {
    force_kill_process(pid)?;
    wait_for_workspace_exit(host, port, timeout_secs).map_err(|_| {
        miette!(
            "Workspace did not release port {} within {}s after forced stop. \
             It may still be shutting down; retry shortly.",
            port,
            timeout_secs
        )
    })
}

/// Stop a workspace by name or ID.
///
/// If `name_or_id` is `None`, attempts to find the workspace for the current directory.
///
/// Uses TCP shutdown (graceful OTP teardown via `init:stop()`) as primary
/// mechanism. Falls back to OS-level force-kill if `force` is true or if
/// graceful shutdown times out.
pub fn stop_workspace(name_or_id: Option<&str>, force: bool) -> Result<()> {
    let workspace_id = super::lifecycle::resolve_workspace_id_or_cwd(name_or_id)?;

    // Serialize stop with create/start operations on the same workspace.
    // Prevents a concurrent get_or_start_workspace from observing "not running"
    // and starting a new node while stop is still draining or cleaning up.
    // Released when `_lock` is dropped at end of scope (including error paths).
    let _lock = acquire_workspace_lock(&workspace_id)?;

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
                // Ensure the node has actually released its port before returning.
                force_kill_and_wait(info.pid, host, info.port, FORCE_KILL_WAIT_TIMEOUT_SECS)?;
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
                            force_kill_and_wait(
                                info.pid,
                                host,
                                info.port,
                                FORCE_KILL_WAIT_TIMEOUT_SECS,
                            )?;
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
                        force_kill_and_wait(
                            info.pid,
                            host,
                            info.port,
                            FORCE_KILL_WAIT_TIMEOUT_SECS,
                        )?;
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

/// Tests for the actual stop-a-running-node logic (BT-3333): the TCP
/// shutdown request/ack, the exit-wait probe, the force-kill fallback, and
/// `stop_workspace`'s orchestration across all of the above. BT-3326 only
/// covered the "workspace doesn't exist" error path via a CLI subprocess
/// test (`cli_workspace.rs`) — none of the branches below had any coverage.
#[cfg(test)]
mod tests {
    use super::*;
    use std::net::TcpListener;
    use tungstenite::{Message, WebSocket};

    /// Spawn a fake workspace backend that completes the ADR 0020 auth
    /// handshake, waits for exactly one `{"op":"shutdown",...}` request, and
    /// replies with an ack (`error` is `None`) or a rejection (`error` is
    /// `Some(message)`). The listening socket is dropped as soon as this one
    /// connection is handled, so a subsequent TCP probe (as
    /// `wait_for_workspace_exit` performs) sees the port close — simulating
    /// the BEAM node actually exiting after `init:stop()`.
    ///
    /// This is another hand-rolled instance of the same ADR 0020 handshake
    /// double as `repl/client.rs`'s `spawn_auth_ok_server` (BT-3326) and the
    /// `tokio`-async doubles in `beamtalk-lsp`/`beamtalk-mcp` — BT-3331
    /// tracks consolidating all of these; not attempted here since none of
    /// the existing doubles model "one request then the port closes".
    fn spawn_shutdown_server(error: Option<&'static str>) -> u16 {
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind fake server");
        let port = listener.local_addr().expect("local_addr").port();
        std::thread::spawn(move || {
            let Ok((stream, _)) = listener.accept() else {
                return;
            };
            let Ok(mut ws) = tungstenite::accept(stream) else {
                return;
            };
            let send = |ws: &mut WebSocket<std::net::TcpStream>, v: serde_json::Value| {
                let _ = ws.send(Message::Text(v.to_string().into()));
            };
            send(&mut ws, serde_json::json!({"op": "auth-required"}));
            if ws.read().is_err() {
                return;
            }
            send(&mut ws, serde_json::json!({"type": "auth_ok"}));
            send(
                &mut ws,
                serde_json::json!({"op": "session-started", "session": "sess-test"}),
            );
            let Ok(Message::Text(_)) = ws.read() else {
                return;
            };
            match error {
                None => send(&mut ws, serde_json::json!({"status": ["done"]})),
                Some(message) => send(&mut ws, serde_json::json!({"error": message})),
            }
            // `ws` (and the captured `listener`) drop here, closing the port.
        });
        port
    }

    /// Build a unique workspace ID per test so parallel `cargo test` threads
    /// never collide on the same on-disk directory or lockfile.
    fn unique_ws_id(label: &str) -> String {
        static COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
        let n = COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        format!("bt3333-shutdown-{label}-{}-{n}", std::process::id())
    }

    /// RAII guard owning a real (but uniquely-named) workspace directory
    /// under `~/.beamtalk/workspaces/`, matching the pattern already used by
    /// `storage.rs`'s and `node_state.rs`'s own tests (there is no
    /// `BEAMTALK_HOME`-style override yet — see BT-3333's description).
    /// Removes the directory and lockfile on drop, including on panic, so a
    /// failing assertion never leaves real state behind.
    struct WorkspaceFixture {
        id: String,
    }

    impl WorkspaceFixture {
        /// Write metadata + cookie + node.info claiming the node listens on
        /// `port` with the given `pid`, matching the on-disk shape
        /// `stop_workspace` reads via `storage`/`node_state`. `nonce` is left
        /// `None` so `is_node_running` trusts the TCP probe alone.
        fn new(label: &str, port: u16, pid: u32) -> Self {
            use crate::commands::workspace::storage::{NodeInfo, WorkspaceMetadata};

            let id = unique_ws_id(label);
            crate::commands::workspace::storage::save_workspace_metadata(&WorkspaceMetadata {
                workspace_id: id.clone(),
                project_path: std::env::temp_dir(),
                created_at: 0,
            })
            .expect("save metadata");
            crate::commands::workspace::storage::save_workspace_cookie(&id, "cookie")
                .expect("save cookie");
            crate::commands::workspace::storage::save_node_info(
                &id,
                &NodeInfo {
                    node_name: format!("{id}@localhost"),
                    port,
                    pid,
                    start_time: None,
                    nonce: None,
                    bind_addr: None,
                },
            )
            .expect("save node info");
            Self { id }
        }
    }

    impl Drop for WorkspaceFixture {
        fn drop(&mut self) {
            if let Ok(dir) = crate::commands::workspace::storage::workspace_dir(&self.id) {
                let _ = std::fs::remove_dir_all(dir);
            }
            if let Ok(base) = crate::commands::workspace::storage::workspaces_base_dir() {
                let _ = std::fs::remove_file(base.join(format!("{}.lock", self.id)));
            }
        }
    }

    // -- tcp_send_shutdown ---------------------------------------------

    #[test]
    fn tcp_send_shutdown_acks_returns_ok() {
        let port = spawn_shutdown_server(None);
        tcp_send_shutdown("127.0.0.1", port, "cookie").expect("ack should succeed");
    }

    #[test]
    fn tcp_send_shutdown_rejected_returns_err_with_message() {
        let port = spawn_shutdown_server(Some("bad cookie"));
        let err = tcp_send_shutdown("127.0.0.1", port, "cookie").unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("Shutdown rejected"), "got: {msg}");
        assert!(msg.contains("bad cookie"), "got: {msg}");
    }

    // -- wait_for_workspace_exit -----------------------------------------

    #[test]
    fn wait_for_workspace_exit_returns_ok_once_port_closes() {
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind");
        let port = listener.local_addr().expect("local_addr").port();
        drop(listener); // Simulate the node having already exited.

        wait_for_workspace_exit("127.0.0.1", port, 5).expect("port already closed");
    }

    #[test]
    fn wait_for_workspace_exit_times_out_while_port_stays_open() {
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind");
        let port = listener.local_addr().expect("local_addr").port();

        let err = wait_for_workspace_exit("127.0.0.1", port, 1).unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("did not exit within 1s"), "got: {msg}");
        assert!(msg.contains("--force"), "got: {msg}");

        drop(listener);
    }

    // -- force_kill_process ------------------------------------------------

    #[test]
    fn force_kill_process_pid_zero_returns_sentinel_error() {
        let err = force_kill_process(0).unwrap_err();
        assert!(err.to_string().contains("process ID unavailable"));
    }

    #[cfg(unix)]
    #[test]
    fn force_kill_process_kills_a_real_running_child() {
        let mut child = std::process::Command::new("sleep")
            .arg("30")
            .spawn()
            .expect("spawn sleep");
        let pid = child.id();

        force_kill_process(pid).expect("force_kill_process should succeed");

        // Reap the killed child (avoids a zombie) and confirm it is
        // actually gone, not just that the syscall returned 0.
        let status = child.wait().expect("wait on killed child");
        assert!(!status.success(), "SIGKILL'd child should not exit(0)");
        assert!(!beamtalk_cli::pid_liveness::is_process_alive(pid));
    }

    #[cfg(unix)]
    #[test]
    fn force_kill_process_already_exited_process_returns_ok() {
        // `true` exits immediately; once reaped, signalling its PID hits the
        // ESRCH branch, which `force_kill_process` treats as success (the
        // goal — "not running" — is already achieved).
        let mut child = std::process::Command::new("true")
            .spawn()
            .expect("spawn true");
        let pid = child.id();
        child.wait().expect("wait for true to exit");

        force_kill_process(pid).expect("killing an already-exited pid should be Ok");
    }

    // -- force_kill_and_wait ------------------------------------------------

    #[cfg(unix)]
    #[test]
    fn force_kill_and_wait_reports_when_port_does_not_release_after_kill() {
        let mut child = std::process::Command::new("sleep")
            .arg("30")
            .spawn()
            .expect("spawn sleep");
        let pid = child.id();
        // Deliberately unrelated to `child`: the kill succeeds but this port
        // never closes, exercising the "did not release port" fallback.
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind");
        let port = listener.local_addr().expect("local_addr").port();

        let err = force_kill_and_wait(pid, "127.0.0.1", port, 1).unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("did not release port"), "got: {msg}");
        assert!(msg.contains("retry shortly"), "got: {msg}");

        let _ = child.wait();
        drop(listener);
    }

    // -- stop_workspace orchestration ---------------------------------------

    #[test]
    fn stop_workspace_graceful_shutdown_succeeds_and_cleans_up() {
        let port = spawn_shutdown_server(None);
        // pid is unused on this path: the server acks and its port closes,
        // so `wait_for_workspace_exit` succeeds without ever force-killing.
        let fixture = WorkspaceFixture::new("graceful", port, 999_999);

        stop_workspace(Some(&fixture.id), false).expect("graceful stop should succeed");

        assert!(
            get_node_info(&fixture.id).unwrap().is_none(),
            "cleanup_stale_node_info should have removed node.info"
        );
    }

    #[test]
    fn stop_workspace_tcp_shutdown_rejected_and_pid_unknown_errors() {
        let port = spawn_shutdown_server(Some("bad cookie"));
        // pid=0 is the sentinel for "PID tracking unavailable" (e.g.
        // Windows) — the rejected-shutdown fallback can't force-kill, so it
        // must surface a clear error instead of panicking or hanging.
        let fixture = WorkspaceFixture::new("tcp-reject-pid0", port, 0);

        let err = stop_workspace(Some(&fixture.id), false).unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("TCP shutdown failed"), "got: {msg}");
        assert!(msg.contains("process ID unknown"), "got: {msg}");
    }

    #[test]
    fn stop_workspace_force_flag_with_unknown_pid_errors() {
        // A listener with nobody driving the ADR 0020 protocol on it is
        // enough for `is_node_running`'s TCP probe — `--force` skips
        // graceful shutdown entirely, so no handshake ever happens.
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind");
        let port = listener.local_addr().expect("local_addr").port();
        let fixture = WorkspaceFixture::new("force-pid0", port, 0);

        let err = stop_workspace(Some(&fixture.id), true).unwrap_err();
        assert!(
            err.to_string().contains("Force-kill is not available"),
            "got: {err}"
        );

        drop(listener);
    }

    #[test]
    fn stop_workspace_when_node_not_running_errors() {
        // Bind then drop immediately so the port is guaranteed free — the
        // node.info entry is stale (the node isn't actually listening).
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind");
        let port = listener.local_addr().expect("local_addr").port();
        drop(listener);
        let fixture = WorkspaceFixture::new("not-running", port, 4_194_304);

        let err = stop_workspace(Some(&fixture.id), false).unwrap_err();
        assert!(err.to_string().contains("is not running"), "got: {err}");
    }
}
