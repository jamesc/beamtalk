// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared JSON-over-WebSocket protocol client for workspace communication.
//!
//! **DDD Context:** REPL — Protocol Transport
//!
//! Both the REPL and transcript viewer communicate with the workspace
//! BEAM node using JSON messages over WebSocket (ADR 0020). This module
//! provides the shared transport layer with cookie-based authentication.

use std::net::TcpStream;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Duration;

use miette::{IntoDiagnostic, Result, miette};
use tungstenite::{Error as WsError, Message, WebSocket};

/// Default connection timeout in milliseconds.
const CONNECT_TIMEOUT_MS: u64 = 5000;

/// Counter for generating unique message IDs across all protocol clients.
static MSG_COUNTER: AtomicU64 = AtomicU64::new(1);

/// Generate a unique message ID.
pub fn next_msg_id() -> String {
    let n = MSG_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("msg-{n:03}")
}

/// Check if a tungstenite error is a transport-level failure worth retrying.
/// Protocol, UTF-8, and capacity errors indicate a message-level problem
/// that reconnecting won't fix.
fn is_transport_error(e: &WsError) -> bool {
    matches!(
        e,
        WsError::ConnectionClosed
            | WsError::AlreadyClosed
            | WsError::Io(_)
            | WsError::WriteBufferFull(_)
    )
}

/// Low-level JSON-over-WebSocket protocol client (ADR 0020).
///
/// Handles connection, cookie authentication, and JSON message framing
/// over WebSocket. Higher-level clients wrap this to add typed response
/// parsing and domain-specific methods.
pub struct ProtocolClient {
    /// The underlying WebSocket connection.
    ws: WebSocket<TcpStream>,
    /// Session ID from the server welcome message (BT-666).
    session_id: Option<String>,
    /// Connection parameters for reconnects.
    host: String,
    port: u16,
    cookie: String,
    /// Read timeout to reapply on reconnect.
    read_timeout: Option<Duration>,
}

impl ProtocolClient {
    /// Connect to a workspace backend at `ws://{host}:{port}/ws` and
    /// authenticate with the given cookie.
    ///
    /// `read_timeout` sets the TCP read timeout (None for blocking reads).
    pub fn connect(
        host: &str,
        port: u16,
        cookie: &str,
        read_timeout: Option<Duration>,
    ) -> Result<Self> {
        // Delegate to connect_with_resume with no resume requested.
        Self::connect_with_resume(host, port, cookie, read_timeout, None)
    }

    /// Connect with optional session resume. If `resume` is `Some(session_id)`,
    /// the auth handshake will request session resumption on the server side.
    pub fn connect_with_resume(
        host: &str,
        port: u16,
        cookie: &str,
        read_timeout: Option<Duration>,
        resume: Option<&str>,
    ) -> Result<Self> {
        let addr = format!("{host}:{port}");
        let tcp_stream = TcpStream::connect_timeout(
            &addr.parse().into_diagnostic()?,
            Duration::from_millis(CONNECT_TIMEOUT_MS),
        )
        .map_err(|e| miette!("Failed to connect to workspace at {addr}: {e}"))?;

        if let Some(timeout) = read_timeout {
            tcp_stream
                .set_read_timeout(Some(timeout))
                .into_diagnostic()?;
        }

        let url = format!("ws://{addr}/ws");
        let (ws, _response) = tungstenite::client(url, tcp_stream)
            .map_err(|e| miette!("WebSocket handshake failed: {e}"))?;

        let mut client = Self {
            ws,
            session_id: None,
            host: host.to_string(),
            port,
            cookie: cookie.to_string(),
            read_timeout,
        };

        // Read auth-required message (pre-auth, no session yet)
        let auth_required = client.read_response()?;
        match auth_required.get("op").and_then(|v| v.as_str()) {
            Some("auth-required") => {}
            _ => return Err(miette!("Unexpected pre-auth message: {auth_required}")),
        }

        // Build auth message with optional resume field
        let mut auth_map = serde_json::Map::new();
        auth_map.insert(
            "type".to_string(),
            serde_json::Value::String("auth".to_string()),
        );
        auth_map.insert(
            "cookie".to_string(),
            serde_json::Value::String(cookie.to_string()),
        );
        if let Some(res) = resume {
            auth_map.insert(
                "resume".to_string(),
                serde_json::Value::String(res.to_string()),
            );
        }
        let auth_msg = serde_json::Value::Object(auth_map);
        client.send_only(&auth_msg)?;

        // Read auth response
        let auth_response = client.read_response()?;
        match auth_response.get("type").and_then(|t| t.as_str()) {
            Some("auth_ok") => {}
            Some("auth_error") => {
                let msg = auth_response
                    .get("message")
                    .and_then(|m| m.as_str())
                    .unwrap_or("Authentication failed");
                return Err(miette!("Workspace authentication failed: {msg}"));
            }
            _ => {
                return Err(miette!("Unexpected auth response: {}", auth_response));
            }
        }

        // Read session-started message (sent after auth_ok)
        let session_msg = client.read_response()?;
        match session_msg.get("op").and_then(|v| v.as_str()) {
            Some("session-started") => {}
            _ => return Err(miette!("Unexpected session message: {session_msg}")),
        }
        client.session_id = session_msg
            .get("session")
            .and_then(|s| s.as_str())
            .map(String::from);

        Ok(client)
    }

    /// Attempt to reconnect the underlying WebSocket and resume the session
    /// using the last-known session id if present.
    ///
    /// Returns `true` if the previous session was successfully resumed,
    /// `false` if a fresh session was established instead.
    pub fn reconnect(&mut self) -> Result<bool> {
        let requested_session = self.session_id.clone();
        tracing::debug!(
            host = %self.host,
            port = self.port,
            has_session = requested_session.is_some(),
            "Attempting WebSocket reconnect"
        );
        let new_client = match Self::connect_with_resume(
            &self.host,
            self.port,
            &self.cookie,
            self.read_timeout,
            requested_session.as_deref(),
        ) {
            Ok(c) => c,
            Err(e) => {
                tracing::warn!(error = %e, "Reconnect failed");
                return Err(e);
            }
        };
        // Swap in websocket and session id
        self.ws = new_client.ws;
        self.session_id.clone_from(&new_client.session_id);
        let resumed = requested_session.is_some()
            && requested_session.as_deref() == new_client.session_id.as_deref();
        tracing::debug!(
            session_id = ?self.session_id,
            resumed,
            "Reconnect successful"
        );
        Ok(resumed)
    }

    /// Get the session ID assigned by the server during connection.
    pub fn session_id(&self) -> Option<&str> {
        self.session_id.as_deref()
    }

    /// Set the read timeout on the underlying TCP stream.
    pub fn set_read_timeout(&self, timeout: Option<Duration>) -> Result<()> {
        self.ws
            .get_ref()
            .set_read_timeout(timeout)
            .into_diagnostic()
    }

    /// Handle a server-initiated push message (ADR 0017).
    ///
    /// Transcript push messages are printed inline to stdout. Other push
    /// types (actor lifecycle, etc.) are silently ignored.
    ///
    /// Returns `true` if the message was a push and should be skipped for
    /// response parsing.
    fn handle_push(parsed: &serde_json::Value) -> bool {
        let is_push = parsed.get("push").is_some()
            || parsed.get("type").and_then(|v| v.as_str()) == Some("push");
        if !is_push {
            return false;
        }
        if parsed.get("push").and_then(|v| v.as_str()) == Some("transcript") {
            if let Some(text) = parsed.get("text").and_then(|v| v.as_str()) {
                use std::io::Write;
                print!("{text}");
                let _ = std::io::stdout().flush();
            }
        }
        true
    }

    /// Read a single JSON response from the WebSocket, printing any transcript
    /// push messages inline and skipping other push messages.
    fn read_response(&mut self) -> Result<serde_json::Value> {
        loop {
            let msg = self
                .ws
                .read()
                .map_err(|e| miette!("WebSocket read error: {e}"))?;
            match msg {
                Message::Text(text) => {
                    let parsed: serde_json::Value = serde_json::from_str(&text)
                        .map_err(|e| miette!("Failed to parse response: {e}\nRaw: {text}"))?;
                    if Self::handle_push(&parsed) {
                        continue;
                    }
                    return Ok(parsed);
                }
                Message::Close(_) => {
                    return Err(miette!("WebSocket connection closed by server"));
                }
                // Skip ping/pong/binary frames
                _ => {}
            }
        }
    }

    /// Read a single response line from the connection (WebSocket text frame).
    /// Prints transcript push messages inline; skips other push messages.
    /// Returns `Ok(line)` on success, or an error.
    /// When a read timeout is set and expires, returns `Err` with `WouldBlock` kind.
    pub fn read_response_line(&mut self) -> std::io::Result<String> {
        loop {
            match self.ws.read() {
                Ok(Message::Text(text)) => {
                    if let Ok(parsed) = serde_json::from_str::<serde_json::Value>(&text) {
                        if Self::handle_push(&parsed) {
                            continue;
                        }
                    }
                    return Ok(text.to_string());
                }
                Ok(Message::Close(_)) => {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::ConnectionAborted,
                        "WebSocket closed",
                    ));
                }
                Ok(_) => {}
                Err(tungstenite::Error::Io(io_err)) => return Err(io_err),
                Err(e) => {
                    return Err(std::io::Error::other(e.to_string()));
                }
            }
        }
    }

    /// Send a JSON request without reading the response.
    pub fn send_only(&mut self, request: &serde_json::Value) -> Result<()> {
        let request_str = serde_json::to_string(request).into_diagnostic()?;
        self.ws
            .send(Message::Text(request_str.into()))
            .map_err(|e| miette!("WebSocket send error: {e}"))
    }

    /// Send a JSON request and receive a raw JSON response.
    pub fn send_raw(&mut self, request: &serde_json::Value) -> Result<serde_json::Value> {
        self.send_only(request)?;
        self.read_response()
    }

    /// Send a JSON request and deserialize the response into a typed struct.
    ///
    /// On communication failure (send or read), attempts to reconnect and retry
    /// the request once. **Caution:** if the server processed the request before
    /// the read failed, retrying will re-execute it. Callers should be aware of
    /// duplicate-execution risk for mutating operations.
    pub fn send_request<T: serde::de::DeserializeOwned>(
        &mut self,
        request: &serde_json::Value,
    ) -> Result<T> {
        // Attempt once, and on transport failure try reconnect + retry once.
        for attempt in 0..2 {
            if let Err(e) = self.send_only(request) {
                if attempt == 0 {
                    tracing::debug!("Send failed, attempting reconnect: {e}");
                    self.reconnect()?;
                    continue;
                }
                return Err(e);
            }
            loop {
                match self.ws.read() {
                    Ok(Message::Text(text)) => {
                        if let Ok(parsed) = serde_json::from_str::<serde_json::Value>(&text) {
                            if Self::handle_push(&parsed) {
                                continue;
                            }
                        }
                        return serde_json::from_str(&text)
                            .map_err(|e| miette!("Failed to parse response: {e}\nRaw: {text}"));
                    }
                    Ok(Message::Close(_)) => {
                        if attempt == 0 {
                            self.reconnect()?;
                            break; // retry outer loop
                        }
                        return Err(miette!("WebSocket connection closed by server"));
                    }
                    Ok(_) => {}
                    Err(e) if is_transport_error(&e) => {
                        if attempt == 0 {
                            self.reconnect()?;
                            break; // retry outer loop
                        }
                        return Err(miette!("WebSocket transport error: {e}"));
                    }
                    Err(e) => {
                        // Non-transport error (protocol, UTF-8, etc.) — no retry
                        return Err(miette!("WebSocket error: {e}"));
                    }
                }
            }
        }
        unreachable!("send_request loop always returns")
    }
}
