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
use tungstenite::{Message, WebSocket};

/// Default connection timeout in milliseconds.
const CONNECT_TIMEOUT_MS: u64 = 5000;

/// Counter for generating unique message IDs across all protocol clients.
static MSG_COUNTER: AtomicU64 = AtomicU64::new(1);

/// Generate a unique message ID.
pub fn next_msg_id() -> String {
    let n = MSG_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("msg-{n:03}")
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
        };

        // Read auth-required message (pre-auth, no session yet)
        let auth_required = client.read_response()?;
        match auth_required.get("op").and_then(|v| v.as_str()) {
            Some("auth-required") => {}
            _ => return Err(miette!("Unexpected pre-auth message: {auth_required}")),
        }

        // ADR 0020: Cookie handshake — first message must be auth
        let auth_msg = serde_json::json!({"type": "auth", "cookie": cookie});
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

    /// Read a single JSON response from the WebSocket, skipping push messages.
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
                    // Skip push messages (e.g. Transcript push from ADR 0017)
                    if parsed.get("push").is_some() {
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
    /// Skips push messages (server-initiated, e.g. Transcript).
    /// Returns `Ok(line)` on success, or an error.
    /// When a read timeout is set and expires, returns `Err` with `WouldBlock` kind.
    pub fn read_response_line(&mut self) -> std::io::Result<String> {
        loop {
            match self.ws.read() {
                Ok(Message::Text(text)) => {
                    // Skip push messages (e.g. Transcript push from ADR 0017)
                    if let Ok(parsed) = serde_json::from_str::<serde_json::Value>(&text) {
                        if parsed.get("push").is_some() {
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
    pub fn send_request<T: serde::de::DeserializeOwned>(
        &mut self,
        request: &serde_json::Value,
    ) -> Result<T> {
        self.send_only(request)?;
        loop {
            let msg = self
                .ws
                .read()
                .map_err(|e| miette!("WebSocket read error: {e}"))?;
            match msg {
                Message::Text(text) => {
                    // Skip push messages (e.g. Transcript push from ADR 0017)
                    if let Ok(parsed) = serde_json::from_str::<serde_json::Value>(&text) {
                        if parsed.get("push").is_some() {
                            continue;
                        }
                    }
                    return serde_json::from_str(&text)
                        .map_err(|e| miette!("Failed to parse response: {e}\nRaw: {text}"));
                }
                Message::Close(_) => {
                    return Err(miette!("WebSocket connection closed by server"));
                }
                _ => {}
            }
        }
    }
}
