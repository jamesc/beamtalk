// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared test-only fixtures for `beamtalk-cli`'s own (synchronous,
//! `std::net`/blocking-`tungstenite`) ADR 0020 handshake double and on-disk
//! workspace fixture.
//!
//! `repl/client.rs`'s `spawn_auth_ok_server`/`spawn_auth_error_server` and
//! `workspace/shutdown.rs`'s `WorkspaceFixture`/`unique_ws_id` each started
//! life as one module's own private test helper; both are needed unchanged
//! by a second call site (`transcript.rs`), so per the repo's
//! no-duplicate-implementations rule they move here rather than growing a
//! third/fourth copy. This is a *within-crate* consolidation of `beamtalk-cli`'s
//! own synchronous doubles only — separate from consolidating the *cross-crate*
//! pile of `tokio`-async doubles in
//! `beamtalk-lsp`/`beamtalk-mcp` (`beamtalk_repl_protocol::test_support`);
//! see that module's doc comment for why this crate's synchronous transport
//! deliberately stays out of that shared async implementation.
//!
//! `workspace/shutdown.rs`'s own `spawn_shutdown_server` stays local: it
//! models a meaningfully different wire shape (exactly one request, then the
//! listening socket itself drops so a later TCP probe sees the port close),
//! not a parameterization of the loop [`spawn_auth_ok_server`] below runs.

use std::net::TcpListener;
use std::path::Path;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{PoisonError, RwLock, RwLockReadGuard, RwLockWriteGuard};

use camino::Utf8PathBuf;

use tungstenite::{Message, WebSocket};

use super::workspace::storage::{NodeInfo, WorkspaceMetadata};

/// The shared "nothing is listening here" port (BT-4058, BT-4382); see
/// [`beamtalk_repl_protocol::test_ports::closed_port`].
pub(crate) use beamtalk_repl_protocol::test_ports::closed_port;

/// Build a unique workspace ID per test so parallel `cargo test` threads
/// never collide on the same on-disk directory or lockfile. `label` should
/// identify the calling test module (e.g. `"shutdown"`, `"transcript"`) so a
/// failed cleanup is easy to trace back to its source.
pub(crate) fn unique_ws_id(label: &str) -> String {
    static COUNTER: AtomicU64 = AtomicU64::new(0);
    let n = COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("bt3349-{label}-{}-{n}", std::process::id())
}

/// RAII scratch directory under the OS temp dir.
///
/// Created on [`ScratchDir::new`], removed (recursively) on [`Drop`] even
/// when a test panics, so parallel `cargo test` threads never collide and a
/// failing assertion never leaves stray files behind.
///
/// `label` identifies the test or context (e.g. `"empty"`, `"filter-sort"`)
/// so a stray directory that escaped an abnormal process kill is easy to
/// trace back to its origin. Combined with the process-ID and a per-process
/// counter the path is unique across concurrent test threads.
pub(crate) struct ScratchDir {
    pub(crate) path: Utf8PathBuf,
}

impl ScratchDir {
    pub(crate) fn new(label: &str) -> Self {
        static COUNTER: AtomicU64 = AtomicU64::new(0);
        let n = COUNTER.fetch_add(1, Ordering::Relaxed);
        let path = Utf8PathBuf::from_path_buf(std::env::temp_dir())
            .expect("temp dir is valid UTF-8")
            .join(format!("{label}-{}-{n}", std::process::id()));
        std::fs::create_dir_all(path.as_std_path()).expect("create scratch dir");
        Self { path }
    }
}

impl Drop for ScratchDir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(self.path.as_std_path());
    }
}

/// Guards concurrent access to the real `~/.beamtalk` directory versus a
/// `BEAMTALK_HOME`-overridden hermetic tempdir, across this crate's whole
/// test binary. `cargo test` runs tests as threads within one
/// process by default, and `std::env::set_var`/`remove_var` are
/// process-global — so a test that temporarily overrides `BEAMTALK_HOME`
/// would otherwise race every non-overriding test's real-directory
/// reads/writes for as long as the override is set. Real-directory tests
/// take the shared read side (so they keep running in parallel with each
/// other, same as before this lock existed); a `BEAMTALK_HOME` override
/// takes the exclusive write side for its duration, so it never overlaps a
/// real-directory test.
static BEAMTALK_HOME_LOCK: RwLock<()> = RwLock::new(());

/// Acquire the shared (real-directory) side of [`BEAMTALK_HOME_LOCK`]. Every
/// test that reads/writes the real `~/.beamtalk` directory — directly, or
/// via [`WorkspaceFixture`] — must hold this for its whole body: bind it to
/// a local so it lives long enough, e.g. `let _guard = real_home_guard();`.
/// A poisoned lock (some other guarded test panicked while holding it) is
/// still safe to read through here — the panic is that test's own
/// assertion failure, not a corrupted directory.
///
/// # Panics
///
/// When the calling thread already holds a [`BeamtalkHomeOverride`]: the
/// read side would wait forever on that thread's own write lock (BT-4382,
/// the BT-4374 self-deadlock that hung `run_prune` tests). A test must use
/// one or the other, never both.
pub(crate) fn real_home_guard() -> RwLockReadGuard<'static, ()> {
    assert_no_home_override_held("real_home_guard()");
    BEAMTALK_HOME_LOCK
        .read()
        .unwrap_or_else(PoisonError::into_inner)
}

std::thread_local! {
    /// Whether this thread holds a [`BeamtalkHomeOverride`] (and so the
    /// write side of [`BEAMTALK_HOME_LOCK`]).
    static HOLDS_HOME_OVERRIDE: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Panic with a clear message, instead of self-deadlocking on
/// [`BEAMTALK_HOME_LOCK`], when this thread already holds a
/// [`BeamtalkHomeOverride`].
fn assert_no_home_override_held(acquirer: &str) {
    assert!(
        !HOLDS_HOME_OVERRIDE.with(std::cell::Cell::get),
        "{acquirer} called on a thread that already holds a BeamtalkHomeOverride: \
         BEAMTALK_HOME_LOCK is not reentrant, so this would deadlock. A test that \
         overrides BEAMTALK_HOME must not also take the real-home guard (directly \
         or via WorkspaceFixture)."
    );
}

/// RAII guard that overrides `BEAMTALK_HOME` to `dir` for its lifetime,
/// restoring the previous value (or unsetting it) on drop. Holds the
/// exclusive write side of [`BEAMTALK_HOME_LOCK`] for that whole lifetime,
/// so construction blocks until every in-flight real-directory
/// ([`real_home_guard`]) test has finished, and no new one can start until
/// this guard drops.
pub(crate) struct BeamtalkHomeOverride {
    _lock: RwLockWriteGuard<'static, ()>,
    prev: Option<String>,
}

impl BeamtalkHomeOverride {
    pub(crate) fn new(dir: &Path) -> Self {
        assert_no_home_override_held("BeamtalkHomeOverride::new()");
        let lock = BEAMTALK_HOME_LOCK
            .write()
            .unwrap_or_else(PoisonError::into_inner);
        HOLDS_HOME_OVERRIDE.with(|held| held.set(true));
        let prev = std::env::var("BEAMTALK_HOME").ok();
        // SAFETY: the exclusive write lock above guarantees no other thread is
        // reading BEAMTALK_HOME or the directory it controls concurrently.
        unsafe { std::env::set_var("BEAMTALK_HOME", dir) };
        Self { _lock: lock, prev }
    }
}

impl Drop for BeamtalkHomeOverride {
    fn drop(&mut self) {
        match &self.prev {
            // SAFETY: still holding the exclusive write lock (`_lock` drops after
            // this method returns, not before).
            Some(v) => unsafe { std::env::set_var("BEAMTALK_HOME", v) },
            // SAFETY: see above.
            None => unsafe { std::env::remove_var("BEAMTALK_HOME") },
        }
        HOLDS_HOME_OVERRIDE.with(|held| held.set(false));
    }
}

/// RAII guard owning a real (but uniquely-named) workspace directory under
/// `~/.beamtalk/workspaces/`, matching the pattern already used by
/// `storage.rs`'s and `node_state.rs`'s own tests. Holds the shared
/// (real-directory) side of [`BEAMTALK_HOME_LOCK`] for its whole lifetime
/// so it can never run concurrently with a [`BeamtalkHomeOverride`].
/// Removes the directory and lockfile on drop, including on panic, so a
/// failing assertion never leaves real state behind.
pub(crate) struct WorkspaceFixture {
    pub(crate) id: String,
    _guard: RwLockReadGuard<'static, ()>,
}

impl WorkspaceFixture {
    /// Write metadata + cookie + node.info claiming the node listens on
    /// `port` with the given `pid`, matching the on-disk shape workspace
    /// commands read via `storage`/`node_state`. `nonce` is left `None` so
    /// `is_node_running` trusts the TCP probe alone.
    pub(crate) fn new(label: &str, port: u16, pid: u32) -> Self {
        use super::workspace::storage::{save_node_info, save_workspace_cookie};

        let guard = real_home_guard();
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
        Self { id, _guard: guard }
    }
}

impl Drop for WorkspaceFixture {
    fn drop(&mut self) {
        if let Ok(dir) = super::workspace::storage::workspace_dir(&self.id) {
            let _ = std::fs::remove_dir_all(dir);
        }
        if let Ok(lock) = super::workspace::storage::workspace_lockfile_path(&self.id) {
            let _ = std::fs::remove_file(lock);
        }
    }
}

/// Send `value` as one JSON text frame, ignoring a closed peer.
pub(crate) fn send_json(ws: &mut WebSocket<std::net::TcpStream>, value: &serde_json::Value) {
    let _ = ws.send(Message::Text(value.to_string().into()));
}

/// Server side of a fake workspace node's ADR 0020 handshake (BT-4382):
/// `auth-required` -> read the client's `auth` -> `auth_ok` ->
/// `session-started` (session `sess-test`).
///
/// Returns the `versions` map a version-matching node reports to the
/// client's connect-time `describe` (BT-4327): the versions the client
/// claimed in its `auth` frame, echoed back. `None` when the client's `auth`
/// frame never arrives.
pub(crate) fn fake_node_handshake(
    ws: &mut WebSocket<std::net::TcpStream>,
) -> Option<serde_json::Value> {
    send_json(ws, &serde_json::json!({"op": "auth-required"}));
    let Ok(Message::Text(text)) = ws.read() else {
        return None;
    };
    let auth = serde_json::from_str::<serde_json::Value>(&text).unwrap_or_default();
    let claimed = |key: &str| auth.get(key).cloned().unwrap_or_default();
    let versions = serde_json::json!({
        "beamtalk": claimed("beamtalk_version"),
        "protocol": claimed("protocol_version"),
    });
    send_json(ws, &serde_json::json!({"type": "auth_ok"}));
    send_json(
        ws,
        &serde_json::json!({"op": "session-started", "session": "sess-test"}),
    );
    Some(versions)
}

pub(crate) use beamtalk_repl_protocol::test_versions::stale_versions;

/// When `req` is a `describe`, answer it with `versions` (or with no
/// `versions` map at all, like a node predating the version handshake, when
/// `None`) and return `true`; otherwise send nothing and return `false`.
pub(crate) fn answer_describe(
    ws: &mut WebSocket<std::net::TcpStream>,
    req: &serde_json::Value,
    versions: Option<&serde_json::Value>,
) -> bool {
    if req.get("op").and_then(serde_json::Value::as_str) != Some("describe") {
        return false;
    }
    send_json(
        ws,
        &beamtalk_repl_protocol::test_versions::describe_reply_json(req, versions),
    );
    true
}

/// Read the next JSON text frame, skipping text frames that are not JSON;
/// `None` once the peer is gone or sends a non-text frame.
pub(crate) fn read_json(ws: &mut WebSocket<std::net::TcpStream>) -> Option<serde_json::Value> {
    loop {
        let Ok(Message::Text(text)) = ws.read() else {
            return None;
        };
        if let Ok(value) = serde_json::from_str(&text) {
            return Some(value);
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
                // This fake models a version-matching node (BT-4327).
                let Some(versions) = fake_node_handshake(&mut ws) else {
                    return;
                };
                while let Some(req) = read_json(&mut ws) {
                    // Answer every connect-time `describe` version check in
                    // the fake itself — the per-test `handler` only models
                    // the operation under test.
                    if !answer_describe(&mut ws, &req, Some(&versions)) {
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

#[cfg(test)]
mod tests {
    use super::{BeamtalkHomeOverride, real_home_guard};

    /// BT-4382: taking the real-home guard while this thread holds an
    /// override panics with a clear message instead of deadlocking.
    #[test]
    #[should_panic(expected = "already holds a BeamtalkHomeOverride")]
    fn real_home_guard_panics_under_a_held_override() {
        let tmp = tempfile::TempDir::new().expect("tempdir");
        let _home = BeamtalkHomeOverride::new(tmp.path());
        let _guard = real_home_guard();
    }

    /// Once the override drops, the same thread may take the guard again.
    #[test]
    fn real_home_guard_works_after_the_override_drops() {
        let tmp = tempfile::TempDir::new().expect("tempdir");
        drop(BeamtalkHomeOverride::new(tmp.path()));
        let _guard = real_home_guard();
    }
}
