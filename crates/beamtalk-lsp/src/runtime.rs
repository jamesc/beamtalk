// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Minimal WebSocket client for the Beamtalk LSP server to talk to a running
//! workspace REPL (ADR 0082 Phase 3, BT-2289).
//!
//! **DDD Context:** Language Service ↔ REPL (Workspace) bridge
//!
//! Mirrors the shape of `beamtalk-mcp`'s `ReplClient` but trimmed to the LSP
//! surface: `evaluate` for `workspace/executeCommand` dispatch, plus a
//! background listener for `flush_completed` push frames so the LSP can emit
//! `workspace/applyEdit` per touched file.
//!
//! The client is single-connection and best-effort: workspace discovery
//! consults `~/.beamtalk/workspaces/<id>/{port,cookie}` via the
//! `beamtalk-workspace` helper; on connect failure the client surfaces an
//! error and the LSP layer reports it back to the IDE as a command-failure
//! response. There is no auto-reconnect today — the LSP can be restarted by
//! the editor.

use std::path::Path;
use std::sync::Arc;
use std::time::Duration;

use futures_util::{SinkExt, StreamExt};
use tokio::net::TcpStream;
use tokio::sync::{Mutex, mpsc, oneshot};
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::{MaybeTlsStream, WebSocketStream, connect_async};
use tracing::{debug, info, warn};

use beamtalk_repl_protocol::{ReplResponse, RequestBuilder};

/// How long to wait for individual WebSocket reads / writes during the auth
/// handshake and per `evaluate` call. Generous enough that a slow local
/// workspace startup doesn't time out, tight enough that LSP commands return
/// in a reasonable time when the workspace has gone away.
const IO_TIMEOUT: Duration = Duration::from_secs(30);

/// How long to wait for the initial TCP+WebSocket upgrade during `connect`.
const CONNECT_TIMEOUT: Duration = Duration::from_secs(5);

type WsStream = WebSocketStream<MaybeTlsStream<TcpStream>>;

/// Errors surfaced by [`RuntimeClient`] / discovery.
#[derive(Debug, thiserror::Error)]
pub enum RuntimeError {
    /// Failed to locate a running workspace for this project — no port file
    /// or cookie file, or the `workspace_id` directory does not exist. Resolved
    /// by the user running `beamtalk repl` / `beamtalk run` against the
    /// project root.
    #[error("no running workspace found for project at {project_path}: {reason}")]
    WorkspaceNotFound {
        /// Project path the LSP attempted to attach to.
        project_path: String,
        /// Underlying reason (missing port file, etc.).
        reason: String,
    },

    /// Failed to open the WebSocket / authenticate.
    #[error("failed to connect to workspace at port {port}: {reason}")]
    Connect {
        /// Port the workspace was advertising.
        port: u16,
        /// Underlying connect error.
        reason: String,
    },

    /// Protocol / I/O error after the connection was established.
    #[error("runtime protocol error: {0}")]
    Protocol(String),
}

/// A flush-completion event surfaced to the LSP server so it can emit
/// `workspace/applyEdit` per touched file.
#[derive(Debug, Clone)]
pub struct FlushEvent {
    /// Absolute or workspace-relative paths of files that were renamed onto
    /// disk by the flush. The runtime sends whatever
    /// `ChangeEntry.sourceFile` carried; the LSP layer canonicalises against
    /// its workspace roots before lookup.
    pub files: Vec<String>,
}

/// WebSocket client to a running Beamtalk workspace.
///
/// Single-connection, single-process. Cloneable handle so async tasks can
/// share it (the inner socket is mutex-protected). Drop will not close the
/// connection — call [`RuntimeClient::close`] explicitly to do that, or just
/// drop and let the OS close the TCP socket when the last reference goes
/// away.
#[derive(Clone)]
pub struct RuntimeClient {
    inner: Arc<RuntimeInner>,
}

struct RuntimeInner {
    /// Sender half of the writer task channel. The LSP side calls
    /// `evaluate(...)` which sends a request + correlation oneshot here; the
    /// writer task serialises to the socket and the reader task fulfills the
    /// oneshot when the reply arrives.
    sender: mpsc::Sender<EvalRequest>,
    /// Listener task handle; stored so the task survives as long as the
    /// `RuntimeClient` exists, and so `close()` can abort it explicitly.
    #[allow(dead_code)] // accessed via `close()`
    listener: Mutex<Option<tokio::task::JoinHandle<()>>>,
    /// Writer task handle; same purpose as `listener`.
    #[allow(dead_code)] // accessed via `close()`
    writer: Mutex<Option<tokio::task::JoinHandle<()>>>,
}

struct EvalRequest {
    request: serde_json::Value,
    id: String,
    reply_to: oneshot::Sender<Result<ReplResponse, RuntimeError>>,
}

impl RuntimeClient {
    /// Discover and connect to the workspace owning `project_path`.
    ///
    /// Reads the workspace id from the project path (via
    /// `beamtalk_workspace::generate_workspace_id`), then the port + cookie
    /// from `~/.beamtalk/workspaces/<id>/`. Errors with
    /// [`RuntimeError::WorkspaceNotFound`] if no port file is present —
    /// callers treat this as "no workspace running, give up" and either skip
    /// the runtime-backed feature or surface a friendly message to the
    /// editor.
    ///
    /// `flush_tx` receives `{flush_completed, files: [...]}` push frames
    /// translated to [`FlushEvent`]. The channel is unbounded so a slow
    /// `applyEdit` task can't backpressure the listener.
    pub async fn connect(
        project_path: &Path,
        flush_tx: mpsc::UnboundedSender<FlushEvent>,
    ) -> Result<Self, RuntimeError> {
        let workspace_id =
            beamtalk_workspace::generate_workspace_id(project_path).map_err(|e| {
                RuntimeError::WorkspaceNotFound {
                    project_path: project_path.display().to_string(),
                    reason: format!("failed to derive workspace id: {e}"),
                }
            })?;

        let (port, _nonce) = beamtalk_workspace::read_port_file(&workspace_id)
            .map_err(|e| RuntimeError::WorkspaceNotFound {
                project_path: project_path.display().to_string(),
                reason: format!("failed to read port file: {e}"),
            })?
            .ok_or_else(|| RuntimeError::WorkspaceNotFound {
                project_path: project_path.display().to_string(),
                reason: "no port file under ~/.beamtalk/workspaces/<id>/".to_string(),
            })?;

        let cookie = beamtalk_workspace::read_cookie_file(&workspace_id)
            .map_err(|e| RuntimeError::WorkspaceNotFound {
                project_path: project_path.display().to_string(),
                reason: format!("failed to read cookie file: {e}"),
            })?
            .ok_or_else(|| RuntimeError::WorkspaceNotFound {
                project_path: project_path.display().to_string(),
                reason: "no cookie file under ~/.beamtalk/workspaces/<id>/".to_string(),
            })?;

        Self::connect_to(port, &cookie, flush_tx).await
    }

    /// Connect directly to a workspace on `port` with `cookie`. Used by
    /// tests; production callers go through [`RuntimeClient::connect`].
    pub async fn connect_to(
        port: u16,
        cookie: &str,
        flush_tx: mpsc::UnboundedSender<FlushEvent>,
    ) -> Result<Self, RuntimeError> {
        let url = format!("ws://127.0.0.1:{port}/ws");
        let connect_fut = connect_async(&url);
        let (mut ws, _resp) = tokio::time::timeout(CONNECT_TIMEOUT, connect_fut)
            .await
            .map_err(|_| RuntimeError::Connect {
                port,
                reason: format!(
                    "timed out connecting to workspace at {url} ({}s)",
                    CONNECT_TIMEOUT.as_secs()
                ),
            })?
            .map_err(|e| RuntimeError::Connect {
                port,
                reason: format!("websocket connect failed: {e}"),
            })?;

        perform_auth_handshake(&mut ws, cookie)
            .await
            .map_err(|e| RuntimeError::Connect { port, reason: e })?;

        info!(port, "LSP runtime client connected to workspace");

        // Split the socket so the reader and writer halves can run
        // independently. The reader dispatches replies + push frames; the
        // writer serialises eval submissions.
        let (sink, stream) = ws.split();
        let pending: Arc<Mutex<PendingMap>> = Arc::new(Mutex::new(PendingMap::default()));
        let (req_tx, req_rx) = mpsc::channel::<EvalRequest>(64);

        let writer = tokio::spawn(writer_task(sink, req_rx, Arc::clone(&pending)));
        let listener = tokio::spawn(listener_task(stream, pending, flush_tx));

        Ok(Self {
            inner: Arc::new(RuntimeInner {
                sender: req_tx,
                listener: Mutex::new(Some(listener)),
                writer: Mutex::new(Some(writer)),
            }),
        })
    }

    /// Submit `code` as an `eval` request and wait for the reply.
    ///
    /// Returns the parsed [`ReplResponse`]; the caller checks `is_error()` to
    /// distinguish between a structured `#beamtalk_error{}` and a successful
    /// value. A transport-level failure (socket closed, timeout) surfaces as
    /// [`RuntimeError::Protocol`].
    pub async fn evaluate(&self, code: &str) -> Result<ReplResponse, RuntimeError> {
        let request = RequestBuilder::eval(code);
        let id = request
            .get("id")
            .and_then(|v| v.as_str())
            .ok_or_else(|| RuntimeError::Protocol("eval request missing id".to_string()))?
            .to_string();

        let (reply_tx, reply_rx) = oneshot::channel();
        let req = EvalRequest {
            request,
            id: id.clone(),
            reply_to: reply_tx,
        };
        self.inner
            .sender
            .send(req)
            .await
            .map_err(|_| RuntimeError::Protocol("runtime client shut down".to_string()))?;

        tokio::time::timeout(IO_TIMEOUT, reply_rx)
            .await
            .map_err(|_| {
                RuntimeError::Protocol(format!(
                    "eval timed out after {}s waiting for reply (id={id})",
                    IO_TIMEOUT.as_secs()
                ))
            })?
            .map_err(|_| {
                RuntimeError::Protocol("eval reply channel dropped before response".to_string())
            })?
    }

    /// Close the underlying connection and abort the listener/writer tasks.
    /// Safe to call multiple times. Not currently called by the LSP backend —
    /// the runtime client lives as long as the LSP process and the OS closes
    /// the TCP socket on exit — but exposed for tests and future use.
    #[allow(dead_code)]
    pub async fn close(&self) {
        if let Some(handle) = self.inner.writer.lock().await.take() {
            handle.abort();
        }
        if let Some(handle) = self.inner.listener.lock().await.take() {
            handle.abort();
        }
    }
}

#[derive(Default)]
struct PendingMap {
    by_id: std::collections::HashMap<String, oneshot::Sender<Result<ReplResponse, RuntimeError>>>,
}

async fn writer_task(
    mut sink: futures_util::stream::SplitSink<WsStream, Message>,
    mut req_rx: mpsc::Receiver<EvalRequest>,
    pending: Arc<Mutex<PendingMap>>,
) {
    while let Some(EvalRequest {
        request,
        id,
        reply_to,
    }) = req_rx.recv().await
    {
        let body = match serde_json::to_string(&request) {
            Ok(s) => s,
            Err(e) => {
                let _ = reply_to.send(Err(RuntimeError::Protocol(format!(
                    "failed to serialise eval request: {e}"
                ))));
                continue;
            }
        };
        pending.lock().await.by_id.insert(id.clone(), reply_to);
        if let Err(e) = sink.send(Message::Text(body.into())).await {
            // Pull the reply_to back out so we can fail it. If something else
            // already removed it (e.g. the listener task), nothing to do.
            if let Some(tx) = pending.lock().await.by_id.remove(&id) {
                let _ = tx.send(Err(RuntimeError::Protocol(format!(
                    "websocket send failed: {e}"
                ))));
            }
            break;
        }
    }
}

async fn listener_task(
    mut stream: futures_util::stream::SplitStream<WsStream>,
    pending: Arc<Mutex<PendingMap>>,
    flush_tx: mpsc::UnboundedSender<FlushEvent>,
) {
    while let Some(msg) = stream.next().await {
        match msg {
            Ok(Message::Text(text)) => {
                let value: serde_json::Value = match serde_json::from_str(&text) {
                    Ok(v) => v,
                    Err(e) => {
                        warn!(error = %e, frame = %text, "runtime: unparseable frame");
                        continue;
                    }
                };
                // Push frame? Dispatch and continue.
                if value.get("type").and_then(|v| v.as_str()) == Some("push") {
                    handle_push_frame(&value, &flush_tx);
                    continue;
                }
                // Otherwise it's a reply to a pending request — look up by id.
                if let Some(id) = value.get("id").and_then(|v| v.as_str()) {
                    let id = id.to_string();
                    let tx_opt = pending.lock().await.by_id.remove(&id);
                    if let Some(tx) = tx_opt {
                        match serde_json::from_value::<ReplResponse>(value) {
                            Ok(resp) => {
                                let _ = tx.send(Ok(resp));
                            }
                            Err(e) => {
                                let _ = tx.send(Err(RuntimeError::Protocol(format!(
                                    "failed to parse runtime reply: {e}"
                                ))));
                            }
                        }
                    } else {
                        debug!(id, "runtime: reply for unknown id");
                    }
                }
            }
            Ok(Message::Close(_)) => {
                debug!("runtime: websocket closed by server");
                break;
            }
            Ok(_) => {
                // Ignore binary / ping / pong frames.
            }
            Err(e) => {
                warn!(error = %e, "runtime: websocket error");
                break;
            }
        }
    }
    // Drain pending requests with a transport error so callers don't hang.
    let mut p = pending.lock().await;
    for (_id, tx) in p.by_id.drain() {
        let _ = tx.send(Err(RuntimeError::Protocol(
            "runtime websocket closed before reply".to_string(),
        )));
    }
}

fn handle_push_frame(value: &serde_json::Value, flush_tx: &mpsc::UnboundedSender<FlushEvent>) {
    let channel = value.get("channel").and_then(|v| v.as_str());
    let event = value.get("event").and_then(|v| v.as_str());
    // Other push channels (classes, actors, bindings, transcript, logs) are
    // not consumed by the LSP today — fall through and drop silently.
    if let (Some("workspace"), Some("flush_completed")) = (channel, event) {
        let files = value
            .get("data")
            .and_then(|d| d.get("files"))
            .and_then(|f| f.as_array())
            .map(|arr| {
                arr.iter()
                    .filter_map(|v| v.as_str().map(String::from))
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        if files.is_empty() {
            debug!("runtime: flush_completed with empty files list");
            return;
        }
        if let Err(e) = flush_tx.send(FlushEvent { files }) {
            warn!(error = %e, "runtime: flush_tx receiver dropped");
        }
    }
}

async fn perform_auth_handshake(ws: &mut WsStream, cookie: &str) -> Result<(), String> {
    use tokio_tungstenite::tungstenite::Message;

    // Read auth-required
    let auth_required = read_text(ws).await?;
    let auth_required_json: serde_json::Value = serde_json::from_str(&auth_required)
        .map_err(|e| format!("failed to parse auth-required: {e}"))?;
    if auth_required_json.get("op").and_then(|v| v.as_str()) != Some("auth-required") {
        return Err(format!("unexpected pre-auth message: {auth_required_json}"));
    }

    // Send auth (no resume — LSP always opens fresh)
    let auth_msg = serde_json::json!({"type": "auth", "cookie": cookie});
    let auth_str =
        serde_json::to_string(&auth_msg).map_err(|e| format!("failed to serialise auth: {e}"))?;
    ws.send(Message::Text(auth_str.into()))
        .await
        .map_err(|e| format!("failed to send auth: {e}"))?;

    // Read auth_ok / auth_error
    let resp = read_text(ws).await?;
    let resp_json: serde_json::Value =
        serde_json::from_str(&resp).map_err(|e| format!("failed to parse auth response: {e}"))?;
    match resp_json.get("type").and_then(|t| t.as_str()) {
        Some("auth_ok") => {}
        Some("auth_error") => {
            let msg = resp_json
                .get("message")
                .and_then(|m| m.as_str())
                .unwrap_or("authentication failed");
            return Err(format!("workspace authentication failed: {msg}"));
        }
        _ => return Err(format!("unexpected auth response: {resp_json}")),
    }

    // Read session-started
    let started = read_text(ws).await?;
    let started_json: serde_json::Value = serde_json::from_str(&started)
        .map_err(|e| format!("failed to parse session-started: {e}"))?;
    if started_json.get("op").and_then(|v| v.as_str()) != Some("session-started") {
        return Err(format!("unexpected post-auth message: {started_json}"));
    }
    Ok(())
}

async fn read_text(ws: &mut WsStream) -> Result<String, String> {
    let read_fut = async {
        loop {
            match ws.next().await {
                Some(Ok(Message::Text(text))) => return Ok::<String, String>(text.to_string()),
                Some(Ok(Message::Close(_))) => {
                    return Err("workspace closed websocket during handshake".to_string());
                }
                Some(Ok(_)) => {
                    // Ignore binary / ping / pong frames during the
                    // handshake; we only care about the JSON text frames.
                }
                Some(Err(e)) => return Err(format!("websocket read failed: {e}")),
                None => return Err("websocket stream ended during handshake".to_string()),
            }
        }
    };
    tokio::time::timeout(IO_TIMEOUT, read_fut)
        .await
        .map_err(|_| format!("websocket read timed out after {}s", IO_TIMEOUT.as_secs()))?
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use tokio::sync::mpsc::unbounded_channel;

    #[tokio::test]
    async fn push_frame_with_files_is_forwarded() {
        let (tx, mut rx) = unbounded_channel::<FlushEvent>();
        handle_push_frame(
            &json!({
                "type": "push",
                "channel": "workspace",
                "event": "flush_completed",
                "data": {
                    "files": ["src/counter.bt", "src/foo.bt"]
                }
            }),
            &tx,
        );
        let evt = rx.recv().await.expect("flush event");
        assert_eq!(evt.files, vec!["src/counter.bt", "src/foo.bt"]);
    }

    #[tokio::test]
    async fn push_frame_with_empty_files_is_dropped() {
        let (tx, mut rx) = unbounded_channel::<FlushEvent>();
        handle_push_frame(
            &json!({
                "type": "push",
                "channel": "workspace",
                "event": "flush_completed",
                "data": { "files": [] }
            }),
            &tx,
        );
        // Empty files: nothing should arrive.
        assert!(rx.try_recv().is_err());
    }

    #[tokio::test]
    async fn push_frame_unknown_channel_is_ignored() {
        let (tx, mut rx) = unbounded_channel::<FlushEvent>();
        handle_push_frame(
            &json!({
                "type": "push",
                "channel": "actors",
                "event": "spawned",
                "data": { "class": "Counter", "pid": "<0.1.0>" }
            }),
            &tx,
        );
        assert!(rx.try_recv().is_err());
    }

    #[tokio::test]
    async fn push_frame_missing_data_is_ignored() {
        let (tx, mut rx) = unbounded_channel::<FlushEvent>();
        handle_push_frame(
            &json!({
                "type": "push",
                "channel": "workspace",
                "event": "flush_completed"
            }),
            &tx,
        );
        assert!(rx.try_recv().is_err());
    }
}
