// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared test-only loopback WebSocket double for the ADR 0020 auth
//! handshake (BT-3331).
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
//! branches — see BT-3330).
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
/// branch of `perform_auth_handshake`/`read_text` (BT-3330 tracks the real
/// gap this leaves in live-node coverage).
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
    /// The loopback port the server accepted its connection on.
    pub port: u16,
    /// Every post-handshake request frame the server received, in arrival
    /// order (the handshake's own `auth` frame is pushed first).
    pub seen: Arc<Mutex<Vec<Value>>>,
    /// Fires once the server's post-handshake read loop has exited — i.e.
    /// the client hung up. `None` once taken by a caller that awaits it.
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

/// Spawn a fake server on an ephemeral loopback port. Performs `handshake`,
/// then answers every subsequent request via `responder`. `session_id` is
/// the `session` field on the final `session-started` frame — read by
/// clients (like `beamtalk-mcp`'s `ReplClient`) that store it, ignored by
/// ones (like `beamtalk-lsp`'s `RuntimeClient`) that only check the frame's
/// `op`.
pub async fn spawn(
    handshake: HandshakeMode,
    session_id: &str,
    responder: Responder,
) -> FakeWsServer {
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0")
        .await
        .expect("bind loopback");
    let port = listener.local_addr().expect("local addr").port();
    let seen: Arc<Mutex<Vec<Value>>> = Arc::new(Mutex::new(Vec::new()));
    let seen_task = Arc::clone(&seen);
    let (done_tx, done_rx) = oneshot::channel();
    let session_id = session_id.to_string();

    let task = tokio::spawn(async move {
        let Ok((stream, _peer)) = listener.accept().await else {
            return;
        };
        let Ok(mut ws) = tokio_tungstenite::accept_async(stream).await else {
            return;
        };

        if handshake == HandshakeMode::DropImmediately {
            drop(ws);
            return;
        }
        if handshake == HandshakeMode::CloseImmediately {
            let _ = ws.close(None).await;
            return;
        }
        if handshake == HandshakeMode::OkWithBinaryNoise {
            let _ = ws.send(Message::Binary(vec![0xF0, 0x9F].into())).await;
        }

        match handshake {
            HandshakeMode::UnparseableAuthRequired => {
                let _ = ws.send(Message::Text("not json at all".into())).await;
                return;
            }
            HandshakeMode::WrongPreAuthOp => {
                let _ = ws.send(text(&json!({"op": "something-else"}))).await;
                return;
            }
            _ => {
                let _ = ws.send(text(&json!({"op": "auth-required"}))).await;
            }
        }

        // The client's auth frame.
        let auth = ws.next().await;
        let Some(Ok(Message::Text(auth))) = auth else {
            return;
        };
        let auth: Value = serde_json::from_str(&auth).unwrap_or(json!({}));
        seen_task.lock().await.push(auth);

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
                return;
            }
            HandshakeMode::AuthErrorWithoutMessage => {
                let _ = ws.send(text(&json!({"type": "auth_error"}))).await;
                return;
            }
            HandshakeMode::UnexpectedAuthResponse => {
                let _ = ws.send(text(&json!({"type": "who_are_you"}))).await;
                return;
            }
            _ => {
                let _ = ws.send(text(&json!({"type": "auth_ok"}))).await;
            }
        }

        if handshake == HandshakeMode::WrongPostAuthOp {
            let _ = ws.send(text(&json!({"op": "not-session-started"}))).await;
            return;
        }
        let _ = ws
            .send(text(
                &json!({"op": "session-started", "session": session_id}),
            ))
            .await;

        while let Some(Ok(msg)) = ws.next().await {
            let Message::Text(body) = msg else { continue };
            let request: Value = serde_json::from_str(&body).unwrap_or(json!({}));
            seen_task.lock().await.push(request.clone());
            for frame in responder(&request) {
                let closing = matches!(frame, Message::Close(_));
                if ws.send(frame).await.is_err() || closing {
                    break;
                }
            }
        }
        let _ = done_tx.send(());
    });

    FakeWsServer {
        port,
        seen,
        disconnected: Some(done_rx),
        task,
    }
}
