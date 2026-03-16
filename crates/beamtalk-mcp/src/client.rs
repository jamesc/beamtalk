// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Async WebSocket client for the beamtalk REPL JSON protocol (ADR 0020).
//!
//! **DDD Context:** Language Service / Interactive Development
//!
//! Connects to a running REPL server and sends/receives
//! JSON messages over WebSocket with cookie authentication.

use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Duration;

use futures_util::{SinkExt, StreamExt};
use serde::{Deserialize, Serialize};
use tokio::sync::Mutex;
use tokio_tungstenite::tungstenite::{
    Error as WsError, Message, error::ProtocolError as WsProtocolError,
};
use tracing::instrument;

/// Default timeout for REPL I/O operations.
const REPL_IO_TIMEOUT: Duration = Duration::from_secs(30);

/// Timeout for establishing a WebSocket connection.
///
/// On Linux, connecting to a closed port fails instantly ("connection refused").
/// On Windows, TCP SYN to a closed port can block for ~21 s waiting for the
/// OS-level retransmit timeout.  A 5 s cap keeps tests and MCP startup snappy
/// on all platforms while still being generous for localhost connections.
const CONNECT_TIMEOUT: Duration = Duration::from_secs(5);

/// Async WebSocket client for the beamtalk REPL protocol.
pub struct ReplClient {
    inner: Mutex<ReplClientInner>,
    port: u16,
    cookie: String,
    session: tokio::sync::Mutex<Option<String>>,
}

impl std::fmt::Debug for ReplClient {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ReplClient")
            .field("port", &self.port)
            .field("cookie", &"<redacted>")
            .field("session", &"<redacted>")
            .finish_non_exhaustive()
    }
}

#[derive(Debug)]
struct ReplClientInner {
    ws: tokio_tungstenite::WebSocketStream<
        tokio_tungstenite::MaybeTlsStream<tokio::net::TcpStream>,
    >,
}

impl ReplClient {
    /// Connect to a REPL server at the given port on localhost with cookie auth.
    /// This variant supports optional session resume by including a `resume` field
    /// in the auth handshake which the server understands.
    pub async fn connect_with_resume(
        port: u16,
        cookie: &str,
        resume: Option<&str>,
    ) -> Result<Self, String> {
        let url = format!("ws://127.0.0.1:{port}/ws");
        let (mut ws, _response) =
            tokio::time::timeout(CONNECT_TIMEOUT, tokio_tungstenite::connect_async(&url))
                .await
                .map_err(|_| {
                    format!(
                        "Timed out connecting to REPL at {url} ({}s)",
                        CONNECT_TIMEOUT.as_secs()
                    )
                })?
                .map_err(|e| format!("Failed to connect to REPL at {url}: {e}"))?;

        let session_id = perform_auth_handshake(&mut ws, cookie, resume).await?;

        Ok(Self {
            inner: Mutex::new(ReplClientInner { ws }),
            port,
            cookie: cookie.to_string(),
            session: tokio::sync::Mutex::new(session_id),
        })
    }

    /// Backwards-compatible connect that does not resume a previous session.
    pub async fn connect(port: u16, cookie: &str) -> Result<Self, String> {
        Self::connect_with_resume(port, cookie, None).await
    }

    /// Reconnect the underlying WebSocket, attempting to resume the session
    /// using the last-known session id if available.
    ///
    /// Returns `true` if the previous session was successfully resumed,
    /// `false` if a fresh session was established instead.
    pub async fn reconnect(&self) -> Result<bool, String> {
        let requested_session = { self.session.lock().await.clone() };
        let url = format!("ws://127.0.0.1:{}/ws", self.port);
        let (mut ws, _response) =
            tokio::time::timeout(CONNECT_TIMEOUT, tokio_tungstenite::connect_async(&url))
                .await
                .map_err(|_| {
                    format!(
                        "Timed out reconnecting to REPL at {url} ({}s)",
                        CONNECT_TIMEOUT.as_secs()
                    )
                })?
                .map_err(|e| format!("Failed to reconnect to REPL at {url}: {e}"))?;

        let session_id =
            perform_auth_handshake(&mut ws, &self.cookie, requested_session.as_deref()).await?;

        let resumed =
            requested_session.is_some() && requested_session.as_deref() == session_id.as_deref();
        tracing::debug!(session_id = ?session_id, resumed, "MCP WebSocket reconnect successful");

        // Swap in the new websocket and session
        {
            let mut inner = self.inner.lock().await;
            inner.ws = ws;
        }
        {
            let mut s = self.session.lock().await;
            *s = session_id;
        }

        Ok(resumed)
    }

    /// Send a JSON request and receive a JSON response.
    ///
    /// Times out after [`REPL_IO_TIMEOUT`] to prevent hanging MCP calls
    /// if the REPL becomes unresponsive.
    ///
    /// On communication failure, attempts to reconnect and retry the request
    /// once. **Caution:** if the server processed the request before the read
    /// failed, retrying will re-execute it. Callers should be aware of
    /// duplicate-execution risk for mutating operations.
    ///
    /// The REPL protocol may send intermediate streaming messages (e.g.
    /// `out` chunks) before the final response. This method loops until
    /// it receives a message with a `status` field, which indicates the
    /// final response.
    #[instrument(skip(self, request))]
    pub async fn send(&self, request: &serde_json::Value) -> Result<ReplResponse, String> {
        let request_str =
            serde_json::to_string(request).map_err(|e| format!("Failed to serialize: {e}"))?;

        // Attempt once, and on failure try to reconnect and retry exactly once.
        for attempt in 0..2 {
            // Lock the websocket for this attempt.
            let mut inner = self.inner.lock().await;

            let io_future = async {
                inner
                    .ws
                    .send(Message::Text(request_str.clone().into()))
                    .await
                    .map_err(|e| format!("Failed to send: {e}"))?;

                // Loop to skip intermediate streaming messages (e.g. `out` chunks).
                // The final response always contains a `status` field.
                loop {
                    let response_text = read_text_message(&mut inner.ws).await?;

                    let parsed: serde_json::Value =
                        serde_json::from_str(&response_text).map_err(|e| {
                            format!("Failed to parse response: {e}\nRaw: {response_text}")
                        })?;

                    // Intermediate streaming messages lack a `status` field
                    if parsed.get("status").is_some() {
                        return serde_json::from_value(parsed).map_err(|e| {
                            format!("Failed to deserialize response: {e}\nRaw: {response_text}")
                        });
                    }
                    // Skip intermediate `out` messages and continue reading
                }
            };

            match tokio::time::timeout(REPL_IO_TIMEOUT, io_future).await {
                Ok(Ok(resp)) => return Ok(resp),
                Ok(Err(e)) => {
                    // Don't retry on parse/deserialization errors — reconnecting won't help
                    if e.starts_with("Failed to parse") || e.starts_with("Failed to deserialize") {
                        return Err(e);
                    }
                    if attempt == 0 {
                        drop(inner); // release lock before reconnecting
                        tracing::warn!("Send failed, attempting reconnect: {e}");
                        if let Err(re) = self.reconnect().await {
                            return Err(format!("Send failed: {e}; reconnect failed: {re}"));
                        }
                        continue; // retry
                    }
                    return Err(e);
                }
                Err(_) => {
                    let timeout_err = format!(
                        "REPL I/O timed out after {}s — the REPL may be unresponsive",
                        REPL_IO_TIMEOUT.as_secs()
                    );
                    if attempt == 0 {
                        drop(inner);
                        tracing::warn!("{timeout_err} — attempting reconnect");
                        if let Err(re) = self.reconnect().await {
                            return Err(format!("{timeout_err}; reconnect failed: {re}"));
                        }
                        continue; // retry
                    }
                    return Err(timeout_err);
                }
            }
        }

        unreachable!("send loop always returns on all paths")
    }

    /// Send an eval operation.
    ///
    /// Uses [`send_once`] (no retry) because eval mutates workspace bindings.
    /// A retry after reconnect would re-execute the same code if the server
    /// already processed the request before the connection dropped.
    // Used in integration tests (#[cfg(test)]) — suppress dead_code lint for binary crate.
    #[allow(dead_code)]
    pub async fn eval(&self, code: &str) -> Result<ReplResponse, String> {
        let request = serde_json::json!({
            "op": "eval",
            "id": next_msg_id(),
            "code": code
        });
        self.send_once(&request).await
    }

    /// Evaluate an expression with optional trace mode (BT-1238).
    ///
    /// Unified entry point used by the MCP evaluate tool. When `trace` is false,
    /// behaves identically to the plain `eval` operation. When true, sets
    /// `trace: true` in the protocol request and returns `steps` in the response.
    ///
    /// Uses [`send_once`] (no retry) — see [`eval`] for rationale.
    pub async fn evaluate_with_options(
        &self,
        code: &str,
        trace: bool,
    ) -> Result<ReplResponse, String> {
        let mut request = serde_json::json!({
            "op": "eval",
            "id": next_msg_id(),
            "code": code
        });
        if trace {
            request["trace"] = serde_json::Value::Bool(true);
        }
        self.send_once(&request).await
    }

    /// Send a complete operation.
    ///
    /// `code` must already be truncated to `cursor` (i.e. only the text the
    /// user has typed up to the cursor position). `cursor` is included as a
    /// field in the JSON request; its presence tells the REPL to use the
    /// context-aware chain-completion path (`get_context_completions/2`)
    /// rather than the legacy bare-prefix path. The cursor value itself is not
    /// used by the current REPL handler — only its presence matters.
    pub async fn complete(&self, code: &str, cursor: usize) -> Result<ReplResponse, String> {
        let request = serde_json::json!({
            "op": "complete",
            "id": next_msg_id(),
            "code": code,
            "cursor": cursor
        });
        self.send(&request).await
    }

    /// Send a load-file operation.
    ///
    /// **Deprecated:** Use [`load_source`] or evaluate `Workspace load: "path"` instead.
    ///
    /// Uses [`send_once`] (no retry) because loading a file defines classes on
    /// the server. A retry after partial success would redefine already-loaded
    /// classes and produce duplicate errors.
    pub async fn load_file(&self, path: &str) -> Result<ReplResponse, String> {
        self.send_once(&Self::request_with_param("load-file", "path", path))
            .await
    }

    /// Send a load-source operation to compile inline Beamtalk source.
    ///
    /// Uses [`send_once`] (no retry) because loading source defines classes on
    /// the server, same as [`load_file`].
    // Used in integration tests (#[cfg(test)]) — suppress dead_code lint for binary crate.
    #[allow(dead_code)]
    pub async fn load_source(&self, source: &str) -> Result<ReplResponse, String> {
        self.send_once(&Self::request_with_param("load-source", "source", source))
            .await
    }

    /// Send an inspect operation.
    pub async fn inspect(&self, actor: &str) -> Result<ReplResponse, String> {
        self.send(&Self::request_with_param("inspect", "actor", actor))
            .await
    }

    /// Send an actors operation.
    pub async fn actors(&self) -> Result<ReplResponse, String> {
        self.send(&Self::request("actors")).await
    }

    /// Send a modules operation.
    pub async fn modules(&self) -> Result<ReplResponse, String> {
        self.send(&Self::request("modules")).await
    }

    /// Send a bindings operation.
    pub async fn bindings(&self) -> Result<ReplResponse, String> {
        self.send(&Self::request("bindings")).await
    }

    /// Send a reload operation.
    ///
    /// Uses [`send_once`] (no retry) — reloading a module is a mutating operation;
    /// a retry could reload an already-updated module with stale bytecode.
    pub async fn reload(&self, module: &str) -> Result<ReplResponse, String> {
        self.send_once(&Self::request_with_param("reload", "module", module))
            .await
    }

    /// Send a clear operation to reset REPL bindings.
    ///
    /// Uses [`send_once`] (no retry) — clearing bindings is a mutating operation
    /// that could interleave badly with other operations on reconnect.
    pub async fn clear(&self) -> Result<ReplResponse, String> {
        self.send_once(&Self::request("clear")).await
    }

    /// Send an unload operation to remove a module from the workspace.
    ///
    /// Uses [`send_once`] (no retry) — a retry after the module was already
    /// unloaded would produce a not-found error.
    pub async fn unload(&self, module: &str) -> Result<ReplResponse, String> {
        self.send_once(&Self::request_with_param("unload", "module", module))
            .await
    }

    /// Send an interrupt operation to cancel a running evaluation.
    ///
    /// Note: Since the MCP client uses a single Mutex-protected WebSocket,
    /// interrupt cannot preempt an in-flight eval on the same connection.
    /// This is suitable for canceling evaluations from separate MCP tool
    /// invocations or when no eval is in progress.
    ///
    /// Uses [`send_once`] (no retry) — a retry after reconnect could cancel
    /// a different evaluation that started after the session was resumed.
    pub async fn interrupt(&self) -> Result<ReplResponse, String> {
        self.send_once(&Self::request("interrupt")).await
    }

    /// Send a show-codegen operation to inspect generated Core Erlang.
    pub async fn show_codegen(&self, code: &str) -> Result<ReplResponse, String> {
        self.send(&Self::request_with_param("show-codegen", "code", code))
            .await
    }

    /// Send a show-codegen operation for a loaded class (BT-1236).
    pub async fn show_codegen_class(
        &self,
        class: &str,
        selector: Option<&str>,
    ) -> Result<ReplResponse, String> {
        let mut request = serde_json::json!({
            "op": "show-codegen",
            "id": next_msg_id(),
            "class": class
        });
        if let Some(sel) = selector {
            request["selector"] = serde_json::Value::String(sel.to_owned());
        }
        self.send(&request).await
    }

    /// Send a test operation to run `BUnit` tests for a specific class.
    ///
    /// Uses [`send_once`] (no retry) — tests may have side effects; a retry
    /// could run the same test suite twice and produce misleading results.
    pub async fn test_class(&self, class: &str) -> Result<ReplResponse, String> {
        self.send_once(&Self::request_with_param("test", "class", class))
            .await
    }

    /// Send a test operation scoped to a specific source file path.
    ///
    /// Uses [`send_once`] (no retry) — see [`test_class`] for rationale.
    pub async fn test_file(&self, file: &str) -> Result<ReplResponse, String> {
        self.send_once(&Self::request_with_param("test", "file", file))
            .await
    }

    /// Send a test-all operation to run all `BUnit` tests.
    ///
    /// Uses [`send_once`] (no retry) — see [`test_class`] for rationale.
    pub async fn test_all(&self) -> Result<ReplResponse, String> {
        self.send_once(&Self::request("test-all")).await
    }

    /// Send a describe operation for capability discovery.
    pub async fn describe(&self) -> Result<ReplResponse, String> {
        self.send(&Self::request("describe")).await
    }

    /// Send a load-project operation.
    ///
    /// Uses [`send_once`] (no retry) because `load-project` loads multiple files
    /// sequentially on the server side. A retry after a partial success would
    /// re-execute already-completed file loads, causing class redefinitions and
    /// duplicate errors in the response.
    pub async fn load_project(
        &self,
        path: &str,
        include_tests: bool,
    ) -> Result<ReplResponse, String> {
        let request = serde_json::json!({
            "op": "load-project",
            "id": next_msg_id(),
            "path": path,
            "include_tests": include_tests
        });
        self.send_once(&request).await
    }

    /// Send a JSON request and receive a JSON response for non-idempotent operations.
    ///
    /// Unlike [`send`], this method uses a narrower reconnect policy to reduce
    /// double-execution risk:
    ///
    /// - **Send phase** — reconnects and retries only when the WebSocket is
    ///   definitively closed *before* the send starts
    ///   ([`tungstenite::Error::ConnectionClosed`] / [`tungstenite::Error::AlreadyClosed`]).
    ///   These errors guarantee the request was never transmitted, so a retry is safe.
    ///   Send timeouts and other I/O errors are propagated directly without a retry,
    ///   because partial transmission cannot be ruled out.
    ///
    /// - **Receive phase** — receive failures are propagated without reconnect.
    ///   Once a request is in-flight, the server may have processed it; retrying
    ///   would cause double-execution.
    ///
    /// Used by: [`eval`], [`evaluate_with_options`], [`load_file`], [`load_project`],
    /// [`reload`], [`clear`], [`unload`], [`interrupt`], [`test_class`], [`test_file`], [`test_all`].
    async fn send_once(&self, request: &serde_json::Value) -> Result<ReplResponse, String> {
        let request_str =
            serde_json::to_string(request).map_err(|e| format!("Failed to serialize: {e}"))?;

        for attempt in 0..2 {
            let mut inner = self.inner.lock().await;

            // Send phase: apply REPL_IO_TIMEOUT so a half-open TCP connection
            // (send buffer full, OS hasn't yet detected the dead link) cannot block forever.
            let send_result = tokio::time::timeout(
                REPL_IO_TIMEOUT,
                inner.ws.send(Message::Text(request_str.clone().into())),
            )
            .await;

            match send_result {
                Ok(Ok(())) => {} // send succeeded — fall through to receive phase
                Ok(Err(
                    e @ (WsError::ConnectionClosed
                    | WsError::AlreadyClosed
                    | WsError::Protocol(WsProtocolError::SendAfterClosing)),
                )) if attempt == 0 => {
                    // Connection was definitively closed before the send started —
                    // the server never saw the request, so reconnecting is safe.
                    // Covers: ConnectionClosed (close handshake complete),
                    //         AlreadyClosed (post-ConnectionClosed write),
                    //         SendAfterClosing (write after local close frame).
                    drop(inner); // release lock before reconnecting
                    tracing::warn!("send_once: connection closed ({e}), attempting reconnect");
                    if let Err(re) = self.reconnect().await {
                        return Err(format!("Failed to send: {e}; reconnect failed: {re}"));
                    }
                    continue; // retry with fresh connection
                }
                Ok(Err(e)) => {
                    // Other send error (I/O error, protocol error, or second-attempt
                    // closed-connection): cannot guarantee the server received nothing,
                    // so do not retry. Quarantine the socket through the existing guard
                    // so the next call is forced onto a fresh connection.
                    tracing::warn!("send_once: send failed ({e}); closing socket");
                    let _ =
                        tokio::time::timeout(Duration::from_secs(1), inner.ws.close(None)).await;
                    return Err(format!("Failed to send: {e}"));
                }
                Err(_) => {
                    // Send timed out: request may have been partially transmitted.
                    // Quarantine the socket through the existing guard.
                    tracing::warn!("send_once: send timed out; closing socket");
                    let _ =
                        tokio::time::timeout(Duration::from_secs(1), inner.ws.close(None)).await;
                    return Err(format!(
                        "REPL I/O timed out after {}s — the REPL may be unresponsive",
                        REPL_IO_TIMEOUT.as_secs()
                    ));
                }
            }

            // Receive phase: the request is now in-flight.
            // Do NOT reconnect — the server may have already processed the request.
            //
            // Use select! to apply the timeout while keeping `inner` available through
            // the existing guard. This avoids a re-lock on the error path and ensures
            // the quarantine close always goes through the same guard that owns the socket.
            let receive_result = tokio::select! {
                result = async {
                    loop {
                        let response_text = read_text_message(&mut inner.ws).await?;
                        let parsed: serde_json::Value =
                            serde_json::from_str(&response_text).map_err(|e| {
                                format!("Failed to parse response: {e}\nRaw: {response_text}")
                            })?;
                        if parsed.get("status").is_some() {
                            return serde_json::from_value(parsed).map_err(|e| {
                                format!(
                                    "Failed to deserialize response: {e}\nRaw: {response_text}"
                                )
                            });
                        }
                    }
                } => result,
                () = tokio::time::sleep(REPL_IO_TIMEOUT) => Err(format!(
                    "REPL I/O timed out after {}s — the REPL may be unresponsive",
                    REPL_IO_TIMEOUT.as_secs()
                )),
            };

            if receive_result.is_err() {
                // Quarantine the socket through the existing guard — no re-lock needed.
                // A late server response for this failed request cannot reach the next call.
                tracing::warn!("send_once receive phase failed; closing socket");
                let _ = tokio::time::timeout(Duration::from_secs(1), inner.ws.close(None)).await;
            }

            return receive_result;
        }

        unreachable!("send_once loop always returns on attempt 1")
    }

    // --- Request builder helpers ---

    /// Build a no-param request for the given operation.
    fn request(op: &str) -> serde_json::Value {
        serde_json::json!({"op": op, "id": next_msg_id()})
    }

    /// Build a single-param request for the given operation.
    fn request_with_param(op: &str, key: &str, value: &str) -> serde_json::Value {
        let mut req = serde_json::json!({"op": op, "id": next_msg_id()});
        req[key] = serde_json::Value::String(value.to_string());
        req
    }

    /// Send a docs operation.
    pub async fn docs(&self, class: &str, selector: Option<&str>) -> Result<ReplResponse, String> {
        let mut request = serde_json::json!({
            "op": "docs",
            "id": next_msg_id(),
            "class": class
        });
        if let Some(sel) = selector {
            request["selector"] = serde_json::Value::String(sel.to_string());
        }
        self.send(&request).await
    }
}

/// JSON response from the REPL backend.
#[derive(Debug, Deserialize, Serialize)]
pub struct ReplResponse {
    /// Message correlation ID.
    pub id: Option<String>,
    /// Session ID.
    pub session: Option<String>,
    /// Status flags: `["done"]`, `["done", "error"]`, or `["done", "test-error"]`.
    pub status: Option<Vec<String>>,
    /// Result value.
    pub value: Option<serde_json::Value>,
    /// Captured stdout from evaluation.
    pub output: Option<String>,
    /// Error message.
    pub error: Option<String>,
    /// Bindings map.
    pub bindings: Option<serde_json::Value>,
    /// Loaded classes.
    pub classes: Option<Vec<String>>,
    /// Actor list.
    pub actors: Option<Vec<ActorInfo>>,
    /// Module list.
    pub modules: Option<Vec<ModuleInfo>>,
    /// Completion suggestions.
    pub completions: Option<Vec<String>>,
    /// Symbol info.
    pub info: Option<serde_json::Value>,
    /// Actor state (inspect op).
    pub state: Option<serde_json::Value>,
    /// Compilation warnings.
    pub warnings: Option<Vec<String>>,
    /// Line number (1-based) of a compile error in the submitted snippet (BT-1235).
    pub line: Option<u32>,
    /// Hint text for a compile error, where available (BT-1235).
    pub hint: Option<String>,
    /// Documentation text.
    pub docs: Option<String>,
    /// Number of actors affected by reload.
    pub affected_actors: Option<u32>,
    /// Number of actors that failed code migration.
    pub migration_failures: Option<u32>,
    /// Generated Core Erlang source (show-codegen op).
    pub core_erlang: Option<String>,
    /// Per-file load errors (load-project op). Each entry is a structured map
    /// with at least `path`, `kind`, and `message` fields.
    pub errors: Option<Vec<serde_json::Value>>,
    /// Test results (test / test-all ops).
    pub results: Option<serde_json::Value>,
    /// Per-statement trace steps (eval with trace=true, BT-1238).
    /// Each entry is `{"src": "...", "value": ...}`.
    pub steps: Option<Vec<serde_json::Value>>,
    /// Supported operations and protocol info (describe op).
    pub ops: Option<serde_json::Value>,
    /// Protocol and language versions (describe op).
    pub versions: Option<serde_json::Value>,
}

impl ReplResponse {
    /// Check if this is an error response.
    pub fn is_error(&self) -> bool {
        if let Some(ref status) = self.status {
            return status.iter().any(|s| s == "error");
        }
        false
    }

    /// Check if the response contains a test failure (`"test-error"` status).
    pub fn has_test_error(&self) -> bool {
        if let Some(ref status) = self.status {
            return status.iter().any(|s| s == "test-error");
        }
        false
    }

    /// Get the error message if present.
    pub fn error_message(&self) -> Option<&str> {
        self.error.as_deref()
    }

    /// Get the result value as a formatted string.
    pub fn value_string(&self) -> String {
        match &self.value {
            Some(serde_json::Value::String(s)) => s.clone(),
            Some(v) => v.to_string(),
            None => String::new(),
        }
    }
}

/// Actor information from the REPL.
#[derive(Debug, Deserialize, Serialize)]
pub struct ActorInfo {
    /// Erlang process identifier as a string (e.g., `"<0.173.0>"`).
    pub pid: String,
    /// Beamtalk class name of the actor (e.g., `"Counter"`).
    pub class: String,
    /// Source module the actor was compiled from.
    pub module: String,
    /// Unix timestamp (seconds) when the actor was spawned.
    pub spawned_at: i64,
}

/// Module information from the REPL.
#[derive(Debug, Deserialize, Serialize)]
pub struct ModuleInfo {
    /// Module name (e.g., `"Counter"`).
    pub name: String,
    /// Path to the `.bt` source file that defined this module.
    pub source_file: String,
    /// Number of live actors currently running from this module.
    pub actor_count: u32,
    /// Unix timestamp (seconds) when the module was last loaded.
    pub load_time: i64,
    /// Human-readable relative time since load (e.g., `"2m ago"`).
    pub time_ago: String,
}

/// Perform the ADR 0020 authentication handshake on a WebSocket connection.
///
/// Reads `auth-required`, sends `auth` (with optional `resume`), reads `auth_ok`,
/// and reads `session-started`. Returns the session ID from the server.
async fn perform_auth_handshake<S>(
    ws: &mut tokio_tungstenite::WebSocketStream<S>,
    cookie: &str,
    resume: Option<&str>,
) -> Result<Option<String>, String>
where
    S: tokio::io::AsyncRead + tokio::io::AsyncWrite + Unpin,
{
    // Read auth-required
    let auth_required = read_text_message_with_timeout(ws, REPL_IO_TIMEOUT).await?;
    let auth_required_json: serde_json::Value = serde_json::from_str(&auth_required)
        .map_err(|e| format!("Failed to parse auth-required: {e}"))?;
    match auth_required_json.get("op").and_then(|v| v.as_str()) {
        Some("auth-required") => {}
        _ => return Err(format!("Unexpected pre-auth message: {auth_required_json}")),
    }

    // Send auth with optional resume
    let mut auth_msg = serde_json::json!({"type": "auth", "cookie": cookie});
    if let Some(r) = resume {
        auth_msg["resume"] = serde_json::Value::String(r.to_string());
    }
    let auth_str =
        serde_json::to_string(&auth_msg).map_err(|e| format!("Failed to serialize auth: {e}"))?;
    ws.send(Message::Text(auth_str.into()))
        .await
        .map_err(|e| format!("Failed to send auth: {e}"))?;

    // Read auth response
    let auth_response = read_text_message_with_timeout(ws, REPL_IO_TIMEOUT).await?;
    let auth_json: serde_json::Value = serde_json::from_str(&auth_response)
        .map_err(|e| format!("Failed to parse auth response: {e}"))?;
    match auth_json.get("type").and_then(|t| t.as_str()) {
        Some("auth_ok") => {}
        Some("auth_error") => {
            let msg = auth_json
                .get("message")
                .and_then(|m| m.as_str())
                .unwrap_or("Authentication failed");
            return Err(format!("Workspace authentication failed: {msg}"));
        }
        _ => return Err(format!("Unexpected auth response: {auth_json}")),
    }

    // Read session-started
    let session_started = read_text_message_with_timeout(ws, REPL_IO_TIMEOUT).await?;
    let session_json: serde_json::Value = serde_json::from_str(&session_started)
        .map_err(|e| format!("Failed to parse session-started: {e}"))?;
    match session_json.get("op").and_then(|v| v.as_str()) {
        Some("session-started") => {}
        _ => return Err(format!("Unexpected post-auth message: {session_json}")),
    }
    Ok(session_json
        .get("session")
        .and_then(|v| v.as_str())
        .map(String::from))
}

/// Read the next text message from a WebSocket stream.
async fn read_text_message<S>(
    ws: &mut tokio_tungstenite::WebSocketStream<S>,
) -> Result<String, String>
where
    S: tokio::io::AsyncRead + tokio::io::AsyncWrite + Unpin,
{
    loop {
        match ws.next().await {
            Some(Ok(Message::Text(text))) => return Ok(text.to_string()),
            Some(Ok(Message::Close(_))) => {
                return Err("WebSocket connection closed by server".to_string());
            }
            Some(Ok(_)) => {} // Skip ping/pong/binary
            Some(Err(e)) => return Err(format!("WebSocket read error: {e}")),
            None => return Err("WebSocket stream ended".to_string()),
        }
    }
}

/// Read the next text message with an explicit timeout.
async fn read_text_message_with_timeout<S>(
    ws: &mut tokio_tungstenite::WebSocketStream<S>,
    timeout: Duration,
) -> Result<String, String>
where
    S: tokio::io::AsyncRead + tokio::io::AsyncWrite + Unpin,
{
    tokio::time::timeout(timeout, read_text_message(ws))
        .await
        .map_err(|_| format!("WebSocket read timed out after {}s", timeout.as_secs()))?
}

/// Generate a unique message ID.
fn next_msg_id() -> String {
    static MSG_COUNTER: AtomicU64 = AtomicU64::new(1);
    let n = MSG_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("msg-{n:03}")
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::workspace;
    use std::net::TcpStream as StdTcpStream;
    use std::process::{Command, Stdio};
    use std::sync::LazyLock;
    use std::time::Duration;

    /// Managed REPL workspace for integration tests.
    /// Holds the workspace ID and cookie so we can authenticate and stop it on Drop.
    struct ReplWorkspace {
        port: u16,
        cookie: String,
        workspace_id: Option<String>,
    }

    impl Drop for ReplWorkspace {
        fn drop(&mut self) {
            // Stop workspace explicitly to avoid port leaks on CI.
            if let Some(ref ws_id) = self.workspace_id {
                let bin_name = format!("beamtalk{}", std::env::consts::EXE_SUFFIX);
                if let Some(bin) = find_binary(&bin_name) {
                    let _ = Command::new(bin)
                        .args(["workspace", "stop", ws_id])
                        .stdin(Stdio::null())
                        .stdout(Stdio::null())
                        .stderr(Stdio::null())
                        .spawn();
                }
            }
        }
    }

    /// Walk up from cwd to find a binary in target/debug/.
    fn find_binary(name: &str) -> Option<std::path::PathBuf> {
        let mut dir = std::env::current_dir().ok()?;
        loop {
            let candidate = dir.join("target/debug").join(name);
            if candidate.exists() {
                return Some(candidate);
            }
            if !dir.pop() {
                return None;
            }
        }
    }

    /// Auto-started REPL workspace for integration tests.
    ///
    /// Uses the same startup path as `beamtalk-mcp --start`: runs
    /// `beamtalk repl --port 0 --timeout 300` with stdin closed, parses the
    /// port and workspace ID from stdout (via `workspace::parse_repl_port` /
    /// `workspace::parse_workspace_id`), then reads the cookie from workspace
    /// storage. The workspace is stopped on `Drop`.
    ///
    /// These tests are `#[ignore]` so they only run via `just test-mcp`
    /// (single-threaded with `--test-threads=1`), not in the regular parallel
    /// `just test` pass.
    static REPL: LazyLock<Result<ReplWorkspace, String>> = LazyLock::new(|| {
        let bin_name = format!("beamtalk{}", std::env::consts::EXE_SUFFIX);
        let bin = find_binary(&bin_name).ok_or_else(|| {
            format!("Could not find target/debug/{bin_name} — run `cargo build` first")
        })?;

        // Start REPL with stdin=null so it exits after workspace startup.
        // The workspace node remains alive as a detached BEAM process.
        // --timeout 300: short idle timeout (5 min) so the workspace dies quickly
        // after the test binary exits, rather than lingering for the default 4h.
        //
        // Stdout is redirected to a temp file instead of piped. On Windows,
        // piping creates a handle that the BEAM node inherits; a background
        // reader thread then blocks forever on the pipe (the BEAM never closes
        // its copy), preventing the test binary from exiting. A temp file
        // avoids this: the CLI writes to the file, we poll-read it, and
        // there's no long-lived thread.
        let stdout_file =
            std::env::temp_dir().join(format!("beamtalk-mcp-test-{}.stdout", std::process::id()));
        let stdout_writer = std::fs::File::create(&stdout_file)
            .map_err(|e| format!("Failed to create stdout temp file: {e}"))?;

        let child = Command::new(&bin)
            .args(["repl", "--port", "0", "--timeout", "300"])
            .stdin(Stdio::null())
            .stdout(stdout_writer)
            .stderr(Stdio::null())
            .spawn()
            .map_err(|e| format!("Failed to start REPL at {}: {e}", bin.display()))?;

        // Poll the temp file for port and workspace ID.
        let deadline = std::time::Instant::now() + Duration::from_secs(30);
        let mut stdout_lines = Vec::new();
        let mut found_port = false;
        let mut found_ws_id = false;
        let mut last_line_count = 0;

        while std::time::Instant::now() < deadline {
            // Re-read the file to get new lines
            if let Ok(content) = std::fs::read_to_string(&stdout_file) {
                let lines: Vec<&str> = content.lines().collect();
                for line in lines.iter().skip(last_line_count) {
                    if line.contains("port") {
                        found_port = true;
                    }
                    if line.contains("Workspace:") {
                        found_ws_id = true;
                    }
                    stdout_lines.push(line.to_string());
                }
                last_line_count = lines.len();
                if found_port && found_ws_id {
                    break;
                }
            }
            std::thread::sleep(Duration::from_millis(100));
        }

        // Clean up temp file (best-effort)
        let _ = std::fs::remove_file(&stdout_file);

        // Don't wait for the CLI process — the workspace BEAM node is
        // independent. On Windows, dropping without wait() is fine.
        drop(child);

        let stdout = stdout_lines.join("\n");
        let port = workspace::parse_repl_port(&stdout).ok_or_else(|| {
            format!("REPL did not report port.\nstdout: {stdout}\nexit: unknown (detached)")
        })?;

        let workspace_id = workspace::parse_workspace_id(&stdout);

        // Read cookie from workspace storage for WebSocket auth (ADR 0020)
        let cookie = match workspace_id.as_deref() {
            Some(ws_id) => workspace::read_cookie_file(ws_id)
                .ok_or_else(|| format!("Failed to read cookie for workspace '{ws_id}'"))?,
            None => return Err("REPL did not report workspace ID".to_string()),
        };

        // Wait for TCP readiness (configurable; default is higher on Windows
        // because epmd + BEAM node startup is slower there, especially when
        // epmd is not already running).
        let default_timeout_ms: u64 = if cfg!(windows) { 30_000 } else { 15_000 };
        let timeout_ms: u64 = std::env::var("BEAMTALK_REPL_STARTUP_TIMEOUT_MS")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(default_timeout_ms);
        let max_attempts = timeout_ms.div_ceil(300);

        for _ in 0..max_attempts {
            if StdTcpStream::connect(format!("127.0.0.1:{port}")).is_ok() {
                eprintln!("MCP tests: REPL workspace ready on port {port}");
                return Ok(ReplWorkspace {
                    port,
                    cookie,
                    workspace_id,
                });
            }
            std::thread::sleep(Duration::from_millis(300));
        }
        Err(format!(
            "REPL reported port {port} but TCP connect failed after {timeout_ms}ms"
        ))
    });

    /// Get the port and cookie of the shared REPL, starting it if needed.
    fn test_port_and_cookie() -> Result<(u16, String), String> {
        if let Ok(port) = std::env::var("BEAMTALK_TEST_PORT") {
            if let Ok(p) = port.parse() {
                let cookie = std::env::var("BEAMTALK_TEST_COOKIE").unwrap_or_default();
                return Ok((p, cookie));
            }
        }
        match REPL.as_ref() {
            Ok(repl) => Ok((repl.port, repl.cookie.clone())),
            Err(e) => Err(e.clone()),
        }
    }

    #[tokio::test]
    #[ignore = "integration test"]
    async fn test_eval_arithmetic() -> Result<(), Box<dyn std::error::Error>> {
        let (port, cookie) = test_port_and_cookie()?;
        let client = ReplClient::connect(port, &cookie).await?;
        let resp = client.eval("2 + 3").await.unwrap();
        assert!(!resp.is_error(), "eval should succeed");
        assert_eq!(resp.value_string(), "5");
        Ok(())
    }

    #[tokio::test]
    #[ignore = "integration test"]
    async fn test_eval_string() -> Result<(), Box<dyn std::error::Error>> {
        let (port, cookie) = test_port_and_cookie()?;
        let client = ReplClient::connect(port, &cookie).await?;
        let resp = client.eval("\"hello\"").await.unwrap();
        assert!(!resp.is_error(), "eval should succeed");
        assert_eq!(resp.value_string(), "hello");
        Ok(())
    }

    #[tokio::test]
    #[ignore = "integration test"]
    async fn test_eval_error() -> Result<(), Box<dyn std::error::Error>> {
        let (port, cookie) = test_port_and_cookie()?;
        let client = ReplClient::connect(port, &cookie).await?;
        let resp = client.eval("42 nonexistentMethod").await.unwrap();
        assert!(resp.is_error(), "should be an error");
        assert!(resp.error_message().is_some(), "should have error message");
        Ok(())
    }

    #[tokio::test]
    #[ignore = "integration test"]
    async fn test_bindings() -> Result<(), Box<dyn std::error::Error>> {
        let (port, cookie) = test_port_and_cookie()?;
        let client = ReplClient::connect(port, &cookie).await?;

        // Set a binding
        let resp = client.eval("testVar := 99").await.unwrap();
        assert!(!resp.is_error());

        // Read bindings
        let resp = client.bindings().await.unwrap();
        assert!(!resp.is_error());
        let bindings = resp.bindings.unwrap();
        assert!(
            bindings.get("testVar").is_some(),
            "testVar should be in bindings: {bindings}"
        );
        Ok(())
    }

    #[tokio::test]
    #[ignore = "integration test"]
    async fn test_actors_list() -> Result<(), Box<dyn std::error::Error>> {
        let (port, cookie) = test_port_and_cookie()?;
        let client = ReplClient::connect(port, &cookie).await?;
        let resp = client.actors().await.unwrap();
        assert!(!resp.is_error());
        assert!(resp.actors.is_some(), "should return actors list");
        Ok(())
    }

    #[tokio::test]
    #[ignore = "integration test"]
    async fn test_modules_list() -> Result<(), Box<dyn std::error::Error>> {
        let (port, cookie) = test_port_and_cookie()?;
        let client = ReplClient::connect(port, &cookie).await?;
        let resp = client.modules().await.unwrap();
        assert!(!resp.is_error());
        assert!(resp.modules.is_some(), "should return modules list");
        Ok(())
    }

    #[tokio::test]
    #[ignore = "integration test"]
    async fn test_complete() -> Result<(), Box<dyn std::error::Error>> {
        let (port, cookie) = test_port_and_cookie()?;
        let client = ReplClient::connect(port, &cookie).await?;
        let code = "Integer ";
        let resp = client.complete(code, code.len()).await.unwrap();
        assert!(!resp.is_error());
        assert!(resp.completions.is_some(), "should return completions list");
        Ok(())
    }

    #[tokio::test]
    #[ignore = "integration test"]
    async fn test_complete_chain() -> Result<(), Box<dyn std::error::Error>> {
        let (port, cookie) = test_port_and_cookie()?;
        let client = ReplClient::connect(port, &cookie).await?;
        // Chain completion: "hello" size → should resolve to Integer methods
        let code = "\"hello\" size ";
        let resp = client.complete(code, code.len()).await.unwrap();
        assert!(!resp.is_error());
        let completions = resp.completions.unwrap_or_default();
        assert!(
            completions.iter().any(|c| c == "abs"),
            "chain completions should include Integer method 'abs': {completions:?}"
        );
        Ok(())
    }

    #[tokio::test]
    #[ignore = "integration test"]
    async fn test_load_file_and_spawn_actor() -> Result<(), Box<dyn std::error::Error>> {
        let (port, cookie) = test_port_and_cookie()?;
        let client = ReplClient::connect(port, &cookie).await?;

        // Load counter
        let resp = client
            .load_file("examples/getting-started/src/counter.bt")
            .await
            .unwrap();
        assert!(!resp.is_error(), "load should succeed: {:?}", resp.error);
        let classes = resp.classes.unwrap_or_default();
        assert!(
            classes.contains(&"Counter".to_string()),
            "Counter should be loaded"
        );

        // Spawn actor
        let resp = client.eval("testCounter := Counter spawn").await.unwrap();
        assert!(!resp.is_error());
        let value = resp.value_string();
        assert!(
            value.contains("Actor"),
            "spawn should return actor ref: {value}"
        );

        // Increment
        let resp = client.eval("testCounter increment").await.unwrap();
        assert!(!resp.is_error());

        // Check actors list
        let resp = client.actors().await.unwrap();
        let actors = resp.actors.unwrap_or_default();
        assert!(
            actors.iter().any(|a| a.class == "Counter"),
            "Counter should appear in actors list"
        );

        // Inspect
        let counter = actors.iter().find(|a| a.class == "Counter").unwrap();
        let resp = client.inspect(&counter.pid).await.unwrap();
        assert!(!resp.is_error());
        assert!(resp.state.is_some(), "inspect should return state");
        Ok(())
    }

    #[tokio::test]
    #[ignore = "integration test"]
    async fn test_docs() -> Result<(), Box<dyn std::error::Error>> {
        let (port, cookie) = test_port_and_cookie()?;
        let client = ReplClient::connect(port, &cookie).await?;
        let resp = client.docs("Integer", None).await.unwrap();
        if resp.is_error() {
            // Some test environments may not have stdlib classes loaded; accept
            // a missing-class error as a soft skip.
            if let Some(err) = resp.error_message() {
                if err.contains("Unknown class") {
                    eprintln!("docs skipped: {err}");
                    return Ok(());
                }
            }
        }
        assert!(!resp.is_error(), "docs should succeed: {:?}", resp.error);
        assert!(resp.docs.is_some(), "should return docs");
        let docs = resp.docs.unwrap();
        assert!(!docs.is_empty(), "docs should not be empty");
        Ok(())
    }

    #[tokio::test]
    #[ignore = "integration test"]
    async fn test_value_string_formats_correctly() -> Result<(), Box<dyn std::error::Error>> {
        let (port, cookie) = test_port_and_cookie()?;
        let client = ReplClient::connect(port, &cookie).await?;

        // Integer value
        let resp = client.eval("42").await.unwrap();
        assert_eq!(resp.value_string(), "42");

        // String value
        let resp = client.eval("\"test\"").await.unwrap();
        assert_eq!(resp.value_string(), "test");

        // Nil value
        let resp = client.eval("nil").await.unwrap();
        assert_eq!(resp.value_string(), "nil");
        Ok(())
    }

    #[tokio::test]
    async fn test_connection_failure() {
        // Port 1 should never have a REPL running
        let result = ReplClient::connect(1, "dummy").await;
        assert!(result.is_err(), "connecting to port 1 should fail");
    }

    #[tokio::test]
    #[ignore = "integration test"]
    async fn test_clear_bindings() -> Result<(), Box<dyn std::error::Error>> {
        let (port, cookie) = test_port_and_cookie()?;
        let client = ReplClient::connect(port, &cookie).await?;

        // Set a binding, then clear
        let _ = client.eval("clearTestVar := 42").await.unwrap();
        let resp = client.clear().await.unwrap();
        assert!(!resp.is_error(), "clear should succeed");

        // Verify binding is gone
        let resp = client.bindings().await.unwrap();
        let bindings = resp
            .bindings
            .unwrap_or(serde_json::Value::Object(serde_json::Map::default()));
        assert!(
            bindings.get("clearTestVar").is_none(),
            "clearTestVar should be cleared"
        );
        Ok(())
    }

    #[tokio::test]
    #[ignore = "integration test"]
    async fn test_show_codegen() -> Result<(), Box<dyn std::error::Error>> {
        let (port, cookie) = test_port_and_cookie()?;
        let client = ReplClient::connect(port, &cookie).await?;
        let resp = client.show_codegen("1 + 2").await.unwrap();
        assert!(
            !resp.is_error(),
            "show_codegen should succeed: {:?}",
            resp.error
        );
        assert!(
            resp.core_erlang.is_some(),
            "should return core_erlang output"
        );
        Ok(())
    }

    #[tokio::test]
    #[ignore = "integration test"]
    async fn test_describe() -> Result<(), Box<dyn std::error::Error>> {
        let (port, cookie) = test_port_and_cookie()?;
        let client = ReplClient::connect(port, &cookie).await?;
        let resp = client.describe().await.unwrap();
        assert!(
            !resp.is_error(),
            "describe should succeed: {:?}",
            resp.error
        );
        assert!(resp.ops.is_some(), "should return ops");
        Ok(())
    }

    #[tokio::test]
    #[ignore = "integration test"]
    async fn test_unload_module() -> Result<(), Box<dyn std::error::Error>> {
        let (port, cookie) = test_port_and_cookie()?;
        let client = ReplClient::connect(port, &cookie).await?;

        // BT-1239: unload op is restored — stdlib classes cannot be removed.
        // Integer is a stdlib class (bt@stdlib@Integer) and must be rejected.
        let resp = client.unload("Integer").await.unwrap();
        assert!(resp.is_error(), "stdlib class cannot be unloaded");
        Ok(())
    }

    #[tokio::test]
    #[ignore = "integration test"]
    async fn test_interrupt() -> Result<(), Box<dyn std::error::Error>> {
        let (port, cookie) = test_port_and_cookie()?;
        let client = ReplClient::connect(port, &cookie).await?;
        // Interrupt when nothing is running should still succeed
        let resp = client.interrupt().await.unwrap();
        // It's OK if this returns an error (nothing to interrupt) — we just verify it doesn't crash
        let _ = resp;
        Ok(())
    }

    #[tokio::test]
    #[ignore = "integration test"]
    async fn test_test_all() -> Result<(), Box<dyn std::error::Error>> {
        let (port, cookie) = test_port_and_cookie()?;
        let client = ReplClient::connect(port, &cookie).await?;

        // Load a test fixture first so there is at least one test class to run
        let load_resp = client
            .load_file("stdlib/test/arithmetic_test.bt")
            .await
            .unwrap();
        assert!(
            !load_resp.is_error(),
            "load should succeed: {:?}",
            load_resp.error
        );

        let resp = client.test_all().await.unwrap();
        assert!(
            !resp.is_error(),
            "test-all should succeed: {:?}",
            resp.error
        );
        assert!(
            resp.results.is_some(),
            "test-all should return a results map"
        );
        assert!(
            !resp.has_test_error(),
            "test-all should pass: status={:?} results={:?}",
            resp.status,
            resp.results
        );
        Ok(())
    }

    #[tokio::test]
    #[ignore = "integration test"]
    async fn test_test_class() -> Result<(), Box<dyn std::error::Error>> {
        let (port, cookie) = test_port_and_cookie()?;
        let client = ReplClient::connect(port, &cookie).await?;

        // Load BUnit test fixture first
        let load_resp = client
            .load_file("stdlib/test/arithmetic_test.bt")
            .await
            .unwrap();
        assert!(
            !load_resp.is_error(),
            "load should succeed: {:?}",
            load_resp.error
        );

        // Run tests for the loaded class — must succeed and return structured results
        let resp = client.test_class("ArithmeticTest").await.unwrap();
        assert!(!resp.is_error(), "test should succeed: {:?}", resp.error);
        assert!(resp.results.is_some(), "should return test results");
        assert!(
            !resp.has_test_error(),
            "ArithmeticTest should pass without failures: status={:?} results={:?} error={:?}",
            resp.status,
            resp.results,
            resp.error
        );
        Ok(())
    }

    #[tokio::test]
    #[ignore = "integration test"]
    async fn test_reconnect_resumes_session() -> Result<(), Box<dyn std::error::Error>> {
        let (port, cookie) = test_port_and_cookie()?;
        let client = ReplClient::connect(port, &cookie).await?;

        // Establish a binding that should survive a reconnect
        let _ = client.eval("reconnectTest := 4242").await?;

        // Close the underlying websocket to simulate a network drop
        {
            let mut inner = client.inner.lock().await;
            // Attempt a graceful close; ignore errors
            let _ = inner.ws.close(None).await;
        }

        // Now perform an operation which should trigger reconnect and resume
        let resp = client.bindings().await?;
        assert!(
            !resp.is_error(),
            "bindings should be returned after reconnect"
        );
        let bindings = resp
            .bindings
            .unwrap_or(serde_json::Value::Object(serde_json::Map::new()));
        assert!(
            bindings.get("reconnectTest").is_some(),
            "reconnectTest binding should persist after reconnect: {bindings:?}"
        );
        Ok(())
    }

    /// Verifies that `send_once` operations automatically reconnect when the WebSocket
    /// is closed between requests (BT-1289).
    ///
    /// Before the fix, closing the socket and calling `evaluate_with_options` would
    /// return "Trying to work with closed connection" permanently — requiring Claude
    /// Code to be restarted. After the fix, `send_once` detects a stale-before-send
    /// connection, reconnects once, and succeeds transparently.
    #[tokio::test]
    #[ignore = "integration test"]
    async fn test_send_once_reconnects_on_stale_connection()
    -> Result<(), Box<dyn std::error::Error>> {
        let (port, cookie) = test_port_and_cookie()?;
        let client = ReplClient::connect(port, &cookie).await?;

        // Establish a binding before the forced connection drop.
        let resp = client
            .evaluate_with_options("staleOnce := 77", false)
            .await?;
        assert!(!resp.is_error(), "initial evaluate should succeed");

        // Force-close the underlying WebSocket to simulate a stale connection
        // (e.g. idle timeout, cowboy restart, or session process crash).
        {
            let mut inner = client.inner.lock().await;
            let _ = inner.ws.close(None).await;
        }

        // evaluate_with_options goes through send_once.  It must reconnect
        // transparently and return the correct result (BT-1289).
        let resp = client.evaluate_with_options("staleOnce + 1", false).await?;
        assert!(
            !resp.is_error(),
            "evaluate_with_options should succeed after reconnect on stale connection: {:?}",
            resp.error
        );
        assert_eq!(
            resp.value_string(),
            "78",
            "result should be correct after send_once reconnect"
        );

        // Confirm the session was resumed and the earlier binding is still visible.
        let resp = client.bindings().await?;
        assert!(!resp.is_error(), "bindings should work after reconnect");
        let bindings = resp
            .bindings
            .unwrap_or(serde_json::Value::Object(serde_json::Map::new()));
        assert!(
            bindings.get("staleOnce").is_some(),
            "staleOnce binding should persist across send_once reconnect: {bindings:?}"
        );
        Ok(())
    }

    /// Cleanup test that runs last (alphabetically after all other `test_*` tests).
    /// Stops the workspace and schedules a force-exit after a delay.
    ///
    /// On Windows, the tokio runtime shutdown hangs indefinitely because open
    /// WebSocket TCP connections to the workspace never complete their close
    /// handshake (the BEAM node holds the connections open). Since `libtest`
    /// doesn't call `process::exit()` on success, the process blocks in
    /// runtime teardown.
    ///
    /// This test spawns a background thread that waits for libtest to print
    /// the summary (1s grace period), then calls `process::exit(0)` to
    /// force-terminate. The test itself returns normally so libtest counts
    /// it as passed and prints the summary.
    #[tokio::test]
    #[ignore = "integration test"]
    async fn test_zzz_cleanup() {
        // Stop the workspace so it doesn't linger for 5 minutes.
        if let Ok(repl) = REPL.as_ref() {
            if let Some(ref ws_id) = repl.workspace_id {
                let bin_name = format!("beamtalk{}", std::env::consts::EXE_SUFFIX);
                if let Some(bin) = find_binary(&bin_name) {
                    let _ = Command::new(bin)
                        .args(["workspace", "stop", ws_id])
                        .stdin(Stdio::null())
                        .stdout(Stdio::null())
                        .stderr(Stdio::null())
                        .spawn();
                }
            }
        }
        // Schedule a force-exit after a delay so libtest can print the summary.
        std::thread::spawn(|| {
            std::thread::sleep(Duration::from_secs(1));
            std::process::exit(0);
        });
    }
}
