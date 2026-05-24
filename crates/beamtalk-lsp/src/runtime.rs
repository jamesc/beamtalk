// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Runtime-attached navigation query channel (BT-2239).
//!
//! **DDD Context:** Language Service
//!
//! When a Beamtalk runtime is attached (a `beamtalk repl` / `beamtalk run`
//! workspace for the project the editor has open), the LSP can delegate
//! navigation queries — find-references, go-to-implementation, hierarchy — to
//! the live `SystemNavigation` facade instead of its in-process AST walker.
//! This is the static-first, live-augmented tooling model of ADR 0024: the
//! runtime knows things the AST cannot (extension methods per ADR 0066,
//! classes loaded only at the REPL), so its answers are richer.
//!
//! This module is the transport. It discovers an attached runtime via the
//! shared workspace files (`~/.beamtalk/workspaces/<id>/{port,cookie}`,
//! [`beamtalk_workspace`]) and speaks the authenticated JSON-over-WebSocket
//! REPL protocol (ADR 0020) using the shared request/response types from
//! [`beamtalk_repl_protocol`]. It mirrors the reference client in
//! `beamtalk-mcp` but is scoped to the read-only `nav-query` op, so it omits
//! session resume and keepalive: the LSP connects lazily and reconnects on the
//! next query if the socket drops.
//!
//! The per-method navigation children (BT-2240..2244) consume this via the
//! dispatch seam on `Backend`; they implement only the per-query mapping, not
//! the transport.

use std::path::Path;
use std::time::Duration;

use beamtalk_repl_protocol::{ReplResponse, RequestBuilder};
use futures_util::{SinkExt, StreamExt};
use tokio_tungstenite::tungstenite::Message;

/// Timeout for establishing the WebSocket connection to a runtime.
const CONNECT_TIMEOUT: Duration = Duration::from_secs(5);

/// Timeout for a single navigation query round-trip.
const QUERY_TIMEOUT: Duration = Duration::from_secs(10);

type WsStream =
    tokio_tungstenite::WebSocketStream<tokio_tungstenite::MaybeTlsStream<tokio::net::TcpStream>>;

/// An authenticated connection to an attached runtime's query channel.
pub struct RuntimeClient {
    ws: WsStream,
}

impl RuntimeClient {
    /// Discover an attached runtime for `project_root` and connect to it.
    ///
    /// Returns `Ok(None)` when no runtime is attached (no port file for the
    /// project's workspace) — the common cold-file case, in which the caller
    /// falls back to the AST walker. Returns `Err` only when a runtime appears
    /// to be present but the connection or auth handshake fails.
    pub async fn discover_and_connect(project_root: &Path) -> Result<Option<Self>, String> {
        let workspace_id = beamtalk_workspace::generate_workspace_id(project_root)
            .map_err(|e| format!("workspace id: {e}"))?;
        let Some((port, _nonce)) = beamtalk_workspace::read_port_file(&workspace_id)
            .map_err(|e| format!("port file: {e}"))?
        else {
            return Ok(None);
        };
        let cookie = beamtalk_workspace::read_cookie_file(&workspace_id)
            .map_err(|e| format!("cookie file: {e}"))?
            .unwrap_or_default();

        let url = format!("ws://127.0.0.1:{port}/ws");
        let (mut ws, _response) =
            tokio::time::timeout(CONNECT_TIMEOUT, tokio_tungstenite::connect_async(&url))
                .await
                .map_err(|_| format!("timed out connecting to runtime at {url}"))?
                .map_err(|e| format!("failed to connect to runtime at {url}: {e}"))?;

        // Bound the handshake: a runtime that accepts the TCP connection but
        // never completes the ADR 0020 handshake must not hang an LSP request.
        match tokio::time::timeout(CONNECT_TIMEOUT, perform_auth_handshake(&mut ws, &cookie)).await
        {
            Ok(result) => result?,
            Err(_) => return Err("timed out during runtime auth handshake".to_string()),
        }
        Ok(Some(Self { ws }))
    }

    /// Run a navigation query and return the parsed response.
    ///
    /// `kind` is `"sendersOf"` / `"referencesTo"` / `"implementorsOf"`; `arg` is
    /// the selector or class name. Callers read `sites` (senders/references) or
    /// `implementors` from the response and translate via the language service.
    /// A structured error from the runtime is surfaced as `Err`.
    pub async fn nav_query(&mut self, kind: &str, arg: &str) -> Result<ReplResponse, String> {
        let resp = self.send(&RequestBuilder::nav_query(kind, arg)).await?;
        if let Some(err) = resp.error_message() {
            return Err(err.to_string());
        }
        Ok(resp)
    }

    /// Send a request and read the final response, skipping any intermediate
    /// streaming frames (which lack a `status` field).
    async fn send(&mut self, request: &serde_json::Value) -> Result<ReplResponse, String> {
        let request_str =
            serde_json::to_string(request).map_err(|e| format!("serialize request: {e}"))?;
        let io = async {
            self.ws
                .send(Message::Text(request_str.into()))
                .await
                .map_err(|e| format!("send: {e}"))?;
            loop {
                let text = read_text_message(&mut self.ws).await?;
                let value: serde_json::Value =
                    serde_json::from_str(&text).map_err(|e| format!("parse response: {e}"))?;
                if value.get("status").is_some() {
                    return serde_json::from_value(value)
                        .map_err(|e| format!("deserialize response: {e}"));
                }
                // Intermediate streaming frame — keep reading.
            }
        };
        tokio::time::timeout(QUERY_TIMEOUT, io)
            .await
            .map_err(|_| "runtime query timed out".to_string())?
    }
}

/// Perform the ADR 0020 auth handshake: read `auth-required`, send the cookie,
/// read `auth_ok`, read `session-started`.
async fn perform_auth_handshake(ws: &mut WsStream, cookie: &str) -> Result<(), String> {
    let auth_required = read_text_message(ws).await?;
    let parsed: serde_json::Value =
        serde_json::from_str(&auth_required).map_err(|e| format!("parse auth-required: {e}"))?;
    if parsed.get("op").and_then(|v| v.as_str()) != Some("auth-required") {
        return Err(format!("unexpected pre-auth message: {parsed}"));
    }

    let auth_msg = serde_json::json!({"type": "auth", "cookie": cookie});
    let auth_str = serde_json::to_string(&auth_msg).map_err(|e| format!("serialize auth: {e}"))?;
    ws.send(Message::Text(auth_str.into()))
        .await
        .map_err(|e| format!("send auth: {e}"))?;

    let auth_response = read_text_message(ws).await?;
    let auth_json: serde_json::Value =
        serde_json::from_str(&auth_response).map_err(|e| format!("parse auth response: {e}"))?;
    match auth_json.get("type").and_then(|t| t.as_str()) {
        Some("auth_ok") => {}
        Some("auth_error") => {
            let msg = auth_json
                .get("message")
                .and_then(|m| m.as_str())
                .unwrap_or("authentication failed");
            return Err(format!("runtime authentication failed: {msg}"));
        }
        _ => return Err(format!("unexpected auth response: {auth_json}")),
    }

    let session_started = read_text_message(ws).await?;
    let session_json: serde_json::Value = serde_json::from_str(&session_started)
        .map_err(|e| format!("parse session-started: {e}"))?;
    if session_json.get("op").and_then(|v| v.as_str()) != Some("session-started") {
        return Err(format!("unexpected post-auth message: {session_json}"));
    }
    Ok(())
}

/// Read the next text frame, skipping ping/pong/binary frames.
async fn read_text_message(ws: &mut WsStream) -> Result<String, String> {
    loop {
        match ws.next().await {
            Some(Ok(Message::Text(text))) => return Ok(text.to_string()),
            Some(Ok(Message::Close(_))) => {
                return Err("runtime closed the connection".to_string());
            }
            Some(Ok(_)) => {} // ping/pong/binary — keep reading
            Some(Err(e)) => return Err(format!("websocket error: {e}")),
            None => return Err("runtime connection ended".to_string()),
        }
    }
}
