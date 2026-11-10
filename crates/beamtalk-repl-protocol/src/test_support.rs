// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared test-only loopback WebSocket double for the ADR 0020 auth
//! handshake.
//!
//! `beamtalk-lsp`'s `FakeWorkspace`/`spawn_workspace` (`src/runtime.rs`) and
//! `beamtalk-mcp`'s `FakeRepl`/`spawn_fake_repl` (`src/server.rs`) each
//! independently hand-wrote a loopback WebSocket test double that performs
//! the same wire sequence (`auth-required` -> client `auth` -> `auth_ok` ->
//! `session-started`) for the same protocol, before answering per-test
//! canned responses — differing only in response-shaping ergonomics for each
//! crate's own op set, and in how much of the handshake's failure space they
//! exercise. [`spawn`] is the consolidated `tokio`/`tokio-tungstenite`
//! implementation both now share; each crate still supplies its own
//! [`Responder`] closure (and, for the LSP side, the [`HandshakeMode`]
//! failure variants it needs to exercise `perform_auth_handshake`'s error
//! branches).
//!
//! `beamtalk-cli`'s `ReplClient` uses a *synchronous* transport
//! (`std::net::TcpStream` + blocking `tungstenite`), so its own
//! `spawn_auth_ok_server`/`spawn_auth_error_server` test doubles
//! (`commands/repl/client.rs`) deliberately stay a third, separate
//! implementation rather than sharing this async one — pulling `tokio` into
//! `beamtalk-cli` (which otherwise has no dependency on it) just for tests
//! would cost more than the ~70 lines it would save. See that module's own
//! doc comment for the full reasoning.
//!
//! [`spawn_sequence`] accepts several connections on one port, each with its
//! own handshake and responder, to model a node restarted on the same port
//! (a client's reconnect path, BT-4329). The version-lock fixtures
//! [`stale_versions`]/[`matching_versions`] are re-exported from the
//! sync-safe [`crate::test_versions`] so `beamtalk-cli` shares them too.
//!
//! Gated on `#[cfg(any(test, feature = "test"))]`: a dependent crate opts in
//! via `beamtalk-repl-protocol = { workspace = true, features = ["test"] }`
//! in its own `[dev-dependencies]`.

// Test-only scaffolding: an `.expect()` on a loopback bind or handshake
// frame is a legitimate test failure, not a documented API contract worth a
// `# Panics` section on every internal helper.
#![allow(clippy::missing_panics_doc)]

use std::sync::Arc;

use futures_util::{SinkExt, StreamExt};
use serde_json::{Value, json};
use tokio::sync::{Mutex, oneshot};
use tokio_tungstenite::tungstenite::Message;

/// How the fake server behaves during the pre-`session-started` handshake.
/// `Ok` is the only variant `beamtalk-mcp`'s fake REPL needs — handshake
/// robustness there is `client.rs`'s own concern, covered live via `just
/// test-mcp`. `beamtalk-lsp`'s fake workspace also exercises every failure
/// branch of `perform_auth_handshake`/`read_text` — a real gap remains in
/// live-node coverage for the `beamtalk-mcp` side.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HandshakeMode {
    /// The real sequence: `auth-required` -> (client auth) -> `auth_ok` ->
    /// `session-started`.
    Ok,
    /// Same, but a binary frame precedes each text frame — the client must
    /// skip non-text frames rather than treat them as the handshake.
    OkWithBinaryNoise,
    /// Close the socket before sending anything.
    CloseImmediately,
    /// Drop the socket (FIN, no close frame) before sending anything.
    DropImmediately,
    /// First frame is not JSON.
    UnparseableAuthRequired,
    /// First frame is JSON but not `op: auth-required`.
    WrongPreAuthOp,
    /// Reply to the client's auth with `auth_error`.
    AuthError,
    /// Reply to the client's auth with `auth_error` carrying no `message`.
    AuthErrorWithoutMessage,
    /// Reply to the client's auth with an unrecognised `type`.
    UnexpectedAuthResponse,
    /// Auth succeeds but the follow-up frame is not `op: session-started`.
    WrongPostAuthOp,
}

/// Frame(s) the fake server sends back for one received post-handshake
/// request.
pub type Responder = Box<dyn Fn(&Value) -> Vec<Message> + Send + Sync>;

/// A running fake server. Aborts its task on drop, so a test that returns
/// early never leaks a listener.
#[derive(Debug)]
pub struct FakeWsServer {
    /// The loopback port the server accepts its connection(s) on.
    pub port: u16,
    /// Every request frame the server received, across all connections, in
    /// arrival order (each connection's handshake `auth` frame is pushed
    /// before its post-handshake requests).
    pub seen: Arc<Mutex<Vec<Value>>>,
    /// The same frames as [`Self::seen`], split per accepted connection: one
    /// entry per [`spawn_sequence`] entry, in order.
    pub seen_per_connection: Vec<Arc<Mutex<Vec<Value>>>>,
    /// Fires once the last connection's post-handshake read loop has exited
    /// — i.e. the client hung up. `None` once taken by a caller that awaits
    /// it.
    pub disconnected: Option<oneshot::Receiver<()>>,
    task: tokio::task::JoinHandle<()>,
}

impl Drop for FakeWsServer {
    fn drop(&mut self) {
        self.task.abort();
    }
}

/// Wrap `value` as a text [`Message`].
pub fn text(value: &Value) -> Message {
    Message::Text(value.to_string().into())
}

pub use crate::test_versions::{matching_versions, stale_versions};

/// The text frame a node sends in reply to the `describe` `request`,
/// carrying `versions` (BT-4329). Pass [`stale_versions`] to model a node
/// built from an older toolchain, [`matching_versions`] for one built from
/// this checkout.
pub fn describe_reply(request: &Value, versions: &Value) -> Message {
    text(&crate::test_versions::describe_reply_json(
        request,
        Some(versions),
    ))
}

/// Spawn a fake server on an ephemeral loopback port that accepts exactly
/// one connection. Performs `handshake`, then answers every subsequent
/// request via `responder`. `session_id` is the `session` field on the
/// final `session-started` frame — read by clients (like `beamtalk-mcp`'s
/// `ReplClient`) that store it, ignored by ones (like `beamtalk-lsp`'s
/// `RuntimeClient`) that only check the frame's `op`.
pub async fn spawn(
    handshake: HandshakeMode,
    session_id: &str,
    responder: Responder,
) -> FakeWsServer {
    spawn_sequence(session_id, vec![(handshake, responder)]).await
}

/// Like [`spawn`], but accepts one connection per entry of `connections`,
/// in order, on the same port — each with its own handshake behaviour and
/// responder (BT-4329). Models a node restarted on the same port: a client
/// that reconnects to its cached port lands on the next entry.
pub async fn spawn_sequence(
    session_id: &str,
    connections: Vec<(HandshakeMode, Responder)>,
) -> FakeWsServer {
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0")
        .await
        .expect("bind loopback");
    let port = listener.local_addr().expect("local addr").port();
    let seen: Arc<Mutex<Vec<Value>>> = Arc::new(Mutex::new(Vec::new()));
    let seen_per_connection: Vec<Arc<Mutex<Vec<Value>>>> = connections
        .iter()
        .map(|_| Arc::new(Mutex::new(Vec::new())))
        .collect();
    let log = SeenLog {
        all: Arc::clone(&seen),
        per_connection: seen_per_connection.clone(),
    };
    let (done_tx, done_rx) = oneshot::channel();
    let session_id = session_id.to_string();
    let last = connections.len().saturating_sub(1);

    let task = tokio::spawn(async move {
        for (index, (handshake, responder)) in connections.into_iter().enumerate() {
            let Ok((stream, _peer)) = listener.accept().await else {
                return;
            };
            let Ok(ws) = tokio_tungstenite::accept_async(stream).await else {
                return;
            };
            let served =
                serve_connection(ws, handshake, &session_id, &responder, &log, index).await;
            if served && index == last {
                let _ = done_tx.send(());
                return;
            }
        }
    });

    FakeWsServer {
        port,
        seen,
        seen_per_connection,
        disconnected: Some(done_rx),
        task,
    }
}

/// Where a connection's received frames are recorded.
struct SeenLog {
    all: Arc<Mutex<Vec<Value>>>,
    per_connection: Vec<Arc<Mutex<Vec<Value>>>>,
}

impl SeenLog {
    async fn push(&self, connection: usize, frame: Value) {
        self.per_connection[connection]
            .lock()
            .await
            .push(frame.clone());
        self.all.lock().await.push(frame);
    }
}

/// Run one accepted connection: the `handshake` script, then the
/// post-handshake read loop answering via `responder`. Returns `true` once
/// the read loop has exited (the client hung up or a `Close` was sent),
/// `false` when the handshake script ended the connection early.
async fn serve_connection(
    mut ws: tokio_tungstenite::WebSocketStream<tokio::net::TcpStream>,
    handshake: HandshakeMode,
    session_id: &str,
    responder: &Responder,
    log: &SeenLog,
    connection: usize,
) -> bool {
    if handshake == HandshakeMode::DropImmediately {
        drop(ws);
        return false;
    }
    if handshake == HandshakeMode::CloseImmediately {
        let _ = ws.close(None).await;
        return false;
    }
    if handshake == HandshakeMode::OkWithBinaryNoise {
        let _ = ws.send(Message::Binary(vec![0xF0, 0x9F].into())).await;
    }

    match handshake {
        HandshakeMode::UnparseableAuthRequired => {
            let _ = ws.send(Message::Text("not json at all".into())).await;
            return false;
        }
        HandshakeMode::WrongPreAuthOp => {
            let _ = ws.send(text(&json!({"op": "something-else"}))).await;
            return false;
        }
        _ => {
            let _ = ws.send(text(&json!({"op": "auth-required"}))).await;
        }
    }

    // The client's auth frame.
    let auth = ws.next().await;
    let Some(Ok(Message::Text(auth))) = auth else {
        return false;
    };
    let auth: Value = serde_json::from_str(&auth).unwrap_or(json!({}));
    log.push(connection, auth).await;

    if handshake == HandshakeMode::OkWithBinaryNoise {
        let _ = ws.send(Message::Binary(vec![0x00].into())).await;
    }
    match handshake {
        HandshakeMode::AuthError => {
            let _ = ws
                .send(text(
                    &json!({"type": "auth_error", "message": "invalid cookie"}),
                ))
                .await;
            return false;
        }
        HandshakeMode::AuthErrorWithoutMessage => {
            let _ = ws.send(text(&json!({"type": "auth_error"}))).await;
            return false;
        }
        HandshakeMode::UnexpectedAuthResponse => {
            let _ = ws.send(text(&json!({"type": "who_are_you"}))).await;
            return false;
        }
        _ => {
            let _ = ws.send(text(&json!({"type": "auth_ok"}))).await;
        }
    }

    if handshake == HandshakeMode::WrongPostAuthOp {
        let _ = ws.send(text(&json!({"op": "not-session-started"}))).await;
        return false;
    }
    let _ = ws
        .send(text(
            &json!({"op": "session-started", "session": session_id}),
        ))
        .await;

    // A `Close` from the responder ends this connection outright: waiting
    // for the client's close reply would stall a client that only notices
    // the close when it next reads, and with it any following
    // `spawn_sequence` entry that client is reconnecting to.
    'serve: while let Some(Ok(msg)) = ws.next().await {
        let Message::Text(body) = msg else { continue };
        let request: Value = serde_json::from_str(&body).unwrap_or(json!({}));
        log.push(connection, request.clone()).await;
        for frame in responder(&request) {
            let closing = matches!(frame, Message::Close(_));
            if ws.send(frame).await.is_err() || closing {
                break 'serve;
            }
        }
    }
    true
}
