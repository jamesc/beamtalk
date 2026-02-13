// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Async TCP client for the beamtalk REPL JSON protocol.
//!
//! Connects to a running REPL server and sends/receives
//! newline-delimited JSON messages over TCP.

use serde::{Deserialize, Serialize};
use std::sync::atomic::{AtomicU64, Ordering};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::TcpStream;
use tokio::sync::Mutex;

/// Async TCP client for the beamtalk REPL protocol.
#[derive(Debug)]
pub struct ReplClient {
    inner: Mutex<ReplClientInner>,
}

#[derive(Debug)]
struct ReplClientInner {
    writer: tokio::io::WriteHalf<TcpStream>,
    reader: BufReader<tokio::io::ReadHalf<TcpStream>>,
}

impl ReplClient {
    /// Connect to a REPL server at the given port on localhost.
    pub async fn connect(port: u16) -> Result<Self, String> {
        let addr = format!("127.0.0.1:{port}");
        let stream = TcpStream::connect(&addr)
            .await
            .map_err(|e| format!("Failed to connect to REPL at {addr}: {e}"))?;

        let (read_half, write_half) = tokio::io::split(stream);

        Ok(Self {
            inner: Mutex::new(ReplClientInner {
                writer: write_half,
                reader: BufReader::new(read_half),
            }),
        })
    }

    /// Send a JSON request and receive a JSON response.
    pub async fn send(&self, request: &serde_json::Value) -> Result<ReplResponse, String> {
        let mut inner = self.inner.lock().await;

        let mut request_str =
            serde_json::to_string(request).map_err(|e| format!("Failed to serialize: {e}"))?;
        request_str.push('\n');

        inner
            .writer
            .write_all(request_str.as_bytes())
            .await
            .map_err(|e| format!("Failed to send: {e}"))?;
        inner
            .writer
            .flush()
            .await
            .map_err(|e| format!("Failed to flush: {e}"))?;

        let mut response_line = String::new();
        inner
            .reader
            .read_line(&mut response_line)
            .await
            .map_err(|e| format!("Failed to read response: {e}"))?;

        if response_line.is_empty() {
            return Err("Connection closed by REPL server".to_string());
        }

        serde_json::from_str(&response_line)
            .map_err(|e| format!("Failed to parse response: {e}\nRaw: {response_line}"))
    }

    /// Send an eval operation.
    pub async fn eval(&self, code: &str) -> Result<ReplResponse, String> {
        let request = serde_json::json!({
            "op": "eval",
            "id": next_msg_id(),
            "code": code
        });
        self.send(&request).await
    }

    /// Send a complete operation.
    pub async fn complete(&self, code: &str) -> Result<ReplResponse, String> {
        let request = serde_json::json!({
            "op": "complete",
            "id": next_msg_id(),
            "code": code
        });
        self.send(&request).await
    }

    /// Send a load-file operation.
    pub async fn load_file(&self, path: &str) -> Result<ReplResponse, String> {
        let request = serde_json::json!({
            "op": "load-file",
            "id": next_msg_id(),
            "path": path
        });
        self.send(&request).await
    }

    /// Send an inspect operation.
    pub async fn inspect(&self, actor: &str) -> Result<ReplResponse, String> {
        let request = serde_json::json!({
            "op": "inspect",
            "id": next_msg_id(),
            "actor": actor
        });
        self.send(&request).await
    }

    /// Send an actors operation.
    pub async fn actors(&self) -> Result<ReplResponse, String> {
        let request = serde_json::json!({
            "op": "actors",
            "id": next_msg_id()
        });
        self.send(&request).await
    }

    /// Send a modules operation.
    pub async fn modules(&self) -> Result<ReplResponse, String> {
        let request = serde_json::json!({
            "op": "modules",
            "id": next_msg_id()
        });
        self.send(&request).await
    }

    /// Send a bindings operation.
    pub async fn bindings(&self) -> Result<ReplResponse, String> {
        let request = serde_json::json!({
            "op": "bindings",
            "id": next_msg_id()
        });
        self.send(&request).await
    }

    /// Send a reload operation.
    pub async fn reload(&self, module: &str) -> Result<ReplResponse, String> {
        let request = serde_json::json!({
            "op": "reload",
            "id": next_msg_id(),
            "module": module
        });
        self.send(&request).await
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
    /// Status flags: `["done"]` or `["done", "error"]`.
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
    /// Documentation text.
    pub docs: Option<String>,
    /// Number of actors affected by reload.
    pub affected_actors: Option<u32>,
    /// Number of actors that failed code migration.
    pub migration_failures: Option<u32>,
}

impl ReplResponse {
    /// Check if this is an error response.
    pub fn is_error(&self) -> bool {
        if let Some(ref status) = self.status {
            return status.iter().any(|s| s == "error");
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
    pub pid: String,
    pub class: String,
    pub module: String,
    pub spawned_at: i64,
}

/// Module information from the REPL.
#[derive(Debug, Deserialize, Serialize)]
pub struct ModuleInfo {
    pub name: String,
    pub source_file: String,
    pub actor_count: u32,
    pub load_time: i64,
    pub time_ago: String,
}

/// Generate a unique message ID.
fn next_msg_id() -> String {
    static MSG_COUNTER: AtomicU64 = AtomicU64::new(1);
    let n = MSG_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("msg-{n:03}")
}
