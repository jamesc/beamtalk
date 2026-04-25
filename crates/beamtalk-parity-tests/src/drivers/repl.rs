// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! REPL surface driver.
//!
//! Connects to the shared workspace's WebSocket port and drives the REPL
//! protocol directly with `beamtalk_repl_protocol::RequestBuilder`.
//! No CLI wrapper — this is the rawest surface available.

use std::collections::BTreeSet;

use beamtalk_repl_protocol::{ReplResponse, RequestBuilder};
use futures_util::{SinkExt, StreamExt};
use tokio_tungstenite::tungstenite::Message;

use crate::drivers::SurfaceOutput;
use crate::normalize;
use crate::pool::SharedRepl;

/// Minimal REPL WebSocket client used by the parity harness.
///
/// We deliberately avoid taking a dependency on `beamtalk-mcp::client::ReplClient`
/// because that type is private to the binary crate. The protocol surface we
/// need (auth handshake + send + receive) is small enough to inline.
pub struct ReplDriver {
    ws: tokio_tungstenite::WebSocketStream<
        tokio_tungstenite::MaybeTlsStream<tokio::net::TcpStream>,
    >,
}

impl std::fmt::Debug for ReplDriver {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ReplDriver").finish_non_exhaustive()
    }
}

impl ReplDriver {
    /// Connect and perform the ADR 0020 auth handshake.
    pub async fn connect(repl: &SharedRepl) -> Result<Self, String> {
        let url = format!("ws://127.0.0.1:{}/ws", repl.port);
        let (mut ws, _) = tokio_tungstenite::connect_async(&url)
            .await
            .map_err(|e| format!("ws connect {url}: {e}"))?;

        // auth-required
        let _required = read_text(&mut ws).await?;
        // auth
        let auth_msg = serde_json::json!({"type": "auth", "cookie": repl.cookie});
        ws.send(Message::Text(auth_msg.to_string().into()))
            .await
            .map_err(|e| format!("send auth: {e}"))?;
        // auth_ok
        let auth_ok: serde_json::Value = serde_json::from_str(&read_text(&mut ws).await?)
            .map_err(|e| format!("auth_ok: {e}"))?;
        if auth_ok.get("type").and_then(|v| v.as_str()) != Some("auth_ok") {
            return Err(format!("workspace auth failed: {auth_ok}"));
        }
        // session-started
        let _session = read_text(&mut ws).await?;
        Ok(Self { ws })
    }

    /// Send a request value and read the next response that correlates with
    /// the request's `id`. Push notifications (no `id`, or different `id`)
    /// are skipped.
    async fn rpc(&mut self, request: serde_json::Value) -> Result<ReplResponse, String> {
        let want_id = request
            .get("id")
            .and_then(|v| v.as_str())
            .map(str::to_string);
        self.ws
            .send(Message::Text(request.to_string().into()))
            .await
            .map_err(|e| format!("send: {e}"))?;
        let deadline = tokio::time::Instant::now() + std::time::Duration::from_secs(120);
        loop {
            if tokio::time::Instant::now() >= deadline {
                return Err("timed out waiting for matching REPL response".to_string());
            }
            let text = read_text(&mut self.ws).await?;
            let value: serde_json::Value = serde_json::from_str(&text)
                .map_err(|e| format!("parse response: {e}\nraw: {text}"))?;
            let got_id = value.get("id").and_then(|v| v.as_str()).map(str::to_string);
            if got_id == want_id {
                return serde_json::from_value(value)
                    .map_err(|e| format!("parse response struct: {e}"));
            }
            // Anything else (push notifications, server-initiated frames)
            // gets dropped silently.
        }
    }

    /// Drive an `eval` request and return a normalized [`SurfaceOutput`].
    pub async fn eval(&mut self, code: &str) -> Result<SurfaceOutput, String> {
        let resp = self.rpc(RequestBuilder::eval(code)).await?;
        if resp.is_error() {
            return Err(format!(
                "REPL eval error: {}",
                resp.error_message().unwrap_or("<no message>")
            ));
        }
        let raw = resp.value_string();
        Ok(SurfaceOutput {
            value: Some(normalize::value(&raw)),
            raw,
            ..SurfaceOutput::default()
        })
    }

    /// Drive a `load-project` request and return the resulting class list.
    pub async fn load_project(&mut self, path: &str) -> Result<SurfaceOutput, String> {
        // Pass `force = true` so re-running across cases re-loads even when
        // the workspace incremental cache thinks the files are unchanged.
        let resp = self
            .rpc(RequestBuilder::load_project_with_force(path, false, true))
            .await?;
        let mut classes = BTreeSet::new();
        if let Some(list) = &resp.classes {
            for c in list {
                classes.insert(c.clone());
            }
        }
        let diagnostic_count = resp.errors.len();
        let raw = format!(
            "classes={:?} errors={} status={:?} response_type={:?}",
            classes, diagnostic_count, resp.status, resp.response_type
        );
        Ok(SurfaceOutput {
            classes: Some(classes),
            diagnostic_count: Some(diagnostic_count),
            raw,
            ..SurfaceOutput::default()
        })
    }

    /// Like [`load_project`] but with `include_tests=true` so test classes
    /// land in the workspace too.
    pub async fn load_project_with_tests(&mut self, path: &str) -> Result<SurfaceOutput, String> {
        let resp = self
            .rpc(RequestBuilder::load_project_with_force(path, true, true))
            .await?;
        let mut classes = BTreeSet::new();
        if let Some(list) = &resp.classes {
            for c in list {
                classes.insert(c.clone());
            }
        }
        let raw = format!("classes={:?} status={:?}", classes, resp.status);
        Ok(SurfaceOutput {
            classes: Some(classes),
            diagnostic_count: Some(resp.errors.len()),
            raw,
            ..SurfaceOutput::default()
        })
    }

    /// Drive a `test` request for a class and return a normalized summary.
    pub async fn test_class(&mut self, class: &str) -> Result<SurfaceOutput, String> {
        let resp = self.rpc(RequestBuilder::test_class(class)).await?;
        let raw =
            serde_json::to_string(&resp.results.clone().unwrap_or_default()).unwrap_or_default();
        let summary = if resp.has_test_error() {
            "fail".to_string()
        } else if resp.is_error() {
            "error".to_string()
        } else {
            "pass".to_string()
        };
        Ok(SurfaceOutput {
            value: Some(summary),
            raw,
            ..SurfaceOutput::default()
        })
    }
}

async fn read_text<S>(ws: &mut tokio_tungstenite::WebSocketStream<S>) -> Result<String, String>
where
    S: tokio::io::AsyncRead + tokio::io::AsyncWrite + Unpin,
{
    loop {
        match ws.next().await {
            Some(Ok(Message::Text(t))) => return Ok(t.to_string()),
            Some(Ok(Message::Close(_))) => return Err("ws closed".to_string()),
            Some(Ok(_)) => continue, // ping/pong/binary
            Some(Err(e)) => return Err(format!("ws read: {e}")),
            None => return Err("ws stream ended".to_string()),
        }
    }
}
