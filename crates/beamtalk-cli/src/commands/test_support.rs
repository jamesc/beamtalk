// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared test-only fixtures for `beamtalk-cli`'s own (synchronous,
//! `std::net`/blocking-`tungstenite`) ADR 0020 handshake double and on-disk
//! workspace fixture (BT-3349).
//!
//! `repl/client.rs`'s `spawn_auth_ok_server`/`spawn_auth_error_server` and
//! `workspace/shutdown.rs`'s `WorkspaceFixture`/`unique_ws_id` each started
//! life as one module's own private test helper; both are needed unchanged
//! by a second call site (`transcript.rs`, BT-3349), so per the repo's
//! no-duplicate-implementations rule they move here rather than growing a
//! third/fourth copy. This is a *within-crate* consolidation of `beamtalk-cli`'s
//! own synchronous doubles only — separate from BT-3331, which tracks
//! consolidating the *cross-crate* pile of `tokio`-async doubles in
//! `beamtalk-lsp`/`beamtalk-mcp` (`beamtalk_repl_protocol::test_support`);
//! see that module's doc comment for why this crate's synchronous transport
//! deliberately stays out of that shared async implementation.
//!
//! `workspace/shutdown.rs`'s own `spawn_shutdown_server` stays local: it
//! models a meaningfully different wire shape (exactly one request, then the
//! listening socket itself drops so a later TCP probe sees the port close),
//! not a parameterization of the loop [`spawn_auth_ok_server`] below runs.

use std::net::TcpListener;
use std::sync::atomic::{AtomicU64, Ordering};

use tungstenite::{Message, WebSocket};

use super::workspace::storage::{NodeInfo, WorkspaceMetadata};

/// Build a unique workspace ID per test so parallel `cargo test` threads
/// never collide on the same on-disk directory or lockfile. `label` should
/// identify the calling test module (e.g. `"shutdown"`, `"transcript"`) so a
/// failed cleanup is easy to trace back to its source.
pub(crate) fn unique_ws_id(label: &str) -> String {
    static COUNTER: AtomicU64 = AtomicU64::new(0);
    let n = COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("bt3349-{label}-{}-{n}", std::process::id())
}

/// RAII guard owning a real (but uniquely-named) workspace directory under
/// `~/.beamtalk/workspaces/`, matching the pattern already used by
/// `storage.rs`'s and `node_state.rs`'s own tests. `beamtalk_home`'s
/// `BEAMTALK_HOME` override (BT-3364) now exists, but this fixture doesn't
/// use it: adopting it here needs a serialization strategy shared with every
/// other non-`#[serial]` test that touches the real `~/.beamtalk` directory,
/// or a test that sets it races this fixture's real-directory reads/writes
/// (BT-3370 tracks that adoption). Removes the directory and lockfile on
/// drop, including on panic, so a failing assertion never leaves real state
/// behind.
pub(crate) struct WorkspaceFixture {
    pub(crate) id: String,
}

impl WorkspaceFixture {
    /// Write metadata + cookie + node.info claiming the node listens on
    /// `port` with the given `pid`, matching the on-disk shape workspace
    /// commands read via `storage`/`node_state`. `nonce` is left `None` so
    /// `is_node_running` trusts the TCP probe alone.
    pub(crate) fn new(label: &str, port: u16, pid: u32) -> Self {
        use super::workspace::storage::{save_node_info, save_workspace_cookie};

        let id = unique_ws_id(label);
        super::workspace::storage::save_workspace_metadata(&WorkspaceMetadata {
            workspace_id: id.clone(),
            project_path: std::env::temp_dir(),
            created_at: 0,
            project_fingerprint: None,
        })
        .expect("save metadata");
        save_workspace_cookie(&id, "cookie").expect("save cookie");
        save_node_info(
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
        if let Ok(dir) = super::workspace::storage::workspace_dir(&self.id) {
            let _ = std::fs::remove_dir_all(dir);
        }
        if let Ok(base) = super::workspace::storage::workspaces_base_dir() {
            let _ = std::fs::remove_file(base.join(format!("{}.lock", self.id)));
        }
    }
}

/// Spawn a minimal fake workspace backend on an OS-assigned port.
///
/// Accepts connections in a loop (one background thread per connection,
/// since some clients — e.g. `ReplClient::interrupt` — open a *second*
/// connection alongside the main one). Each connection completes the real
/// auth handshake (`auth-required` -> read `auth` -> `auth_ok` ->
/// `session-started`) before handing subsequent parsed request frames to
/// `handler`, which may reply on the same socket. Returns the bound port.
pub(crate) fn spawn_auth_ok_server(
    handler: impl Fn(serde_json::Value, &mut WebSocket<std::net::TcpStream>) + Send + Sync + 'static,
) -> u16 {
    let listener = TcpListener::bind("127.0.0.1:0").expect("bind fake server");
    let port = listener.local_addr().expect("local_addr").port();
    let handler = std::sync::Arc::new(handler);
    std::thread::spawn(move || {
        for stream in listener.incoming().flatten() {
            let handler = std::sync::Arc::clone(&handler);
            std::thread::spawn(move || {
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
                while let Ok(Message::Text(text)) = ws.read() {
                    if let Ok(req) = serde_json::from_str(&text) {
                        handler(req, &mut ws);
                    }
                }
            });
        }
    });
    port
}

/// Spawn a fake backend that rejects auth with the given message.
///
/// Accepts connections in a loop (one thread per connection), same as
/// [`spawn_auth_ok_server`] — a caller that first makes its own bare TCP
/// liveness probe against this port (e.g. `transcript::run`'s
/// `is_node_running` check) before the real handshake attempt must not
/// starve that second connection of an `accept()`.
pub(crate) fn spawn_auth_error_server(message: &'static str) -> u16 {
    let listener = TcpListener::bind("127.0.0.1:0").expect("bind fake server");
    let port = listener.local_addr().expect("local_addr").port();
    std::thread::spawn(move || {
        for stream in listener.incoming().flatten() {
            std::thread::spawn(move || {
                if let Ok(mut ws) = tungstenite::accept(stream) {
                    let _ = ws.send(Message::Text(
                        serde_json::json!({"op": "auth-required"})
                            .to_string()
                            .into(),
                    ));
                    let _ = ws.read();
                    let _ = ws.send(Message::Text(
                        serde_json::json!({"type": "auth_error", "message": message})
                            .to_string()
                            .into(),
                    ));
                }
            });
        }
    });
    port
}
