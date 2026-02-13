// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Async TCP client for the beamtalk REPL JSON protocol.
//!
//! **DDD Context:** Language Service / Interactive Development
//!
//! Connects to a running REPL server and sends/receives
//! newline-delimited JSON messages over TCP.

use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Duration;

use serde::{Deserialize, Serialize};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::TcpStream;
use tokio::sync::Mutex;
use tracing::instrument;

/// Default timeout for REPL I/O operations.
const REPL_IO_TIMEOUT: Duration = Duration::from_secs(30);

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
    ///
    /// Times out after [`REPL_IO_TIMEOUT`] to prevent hanging MCP calls
    /// if the REPL becomes unresponsive.
    #[instrument(skip(self, request))]
    pub async fn send(&self, request: &serde_json::Value) -> Result<ReplResponse, String> {
        let mut inner = self.inner.lock().await;

        let mut request_str =
            serde_json::to_string(request).map_err(|e| format!("Failed to serialize: {e}"))?;
        request_str.push('\n');

        let io_future = async {
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
        };

        tokio::time::timeout(REPL_IO_TIMEOUT, io_future)
            .await
            .map_err(|_| {
                format!(
                    "REPL I/O timed out after {}s — the REPL may be unresponsive",
                    REPL_IO_TIMEOUT.as_secs()
                )
            })?
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

#[cfg(test)]
mod tests {
    use super::*;

    /// Port for integration tests. Reads `BEAMTALK_TEST_PORT` env var,
    /// falls back to 19876 for manual `beamtalk repl --port 19876`.
    fn test_port() -> u16 {
        std::env::var("BEAMTALK_TEST_PORT")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(19876)
    }

    #[tokio::test]
    #[ignore = "integration test — requires running REPL (set BEAMTALK_TEST_PORT or default 19876)"]
    async fn test_eval_arithmetic() {
        let client = ReplClient::connect(test_port()).await.unwrap();
        let resp = client.eval("2 + 3").await.unwrap();
        assert!(!resp.is_error(), "eval should succeed");
        assert_eq!(resp.value_string(), "5");
    }

    #[tokio::test]
    #[ignore = "integration test — requires running REPL (set BEAMTALK_TEST_PORT or default 19876)"]
    async fn test_eval_string() {
        let client = ReplClient::connect(test_port()).await.unwrap();
        let resp = client.eval("'hello'").await.unwrap();
        assert!(!resp.is_error(), "eval should succeed");
        assert_eq!(resp.value_string(), "hello");
    }

    #[tokio::test]
    #[ignore = "integration test — requires running REPL (set BEAMTALK_TEST_PORT or default 19876)"]
    async fn test_eval_error() {
        let client = ReplClient::connect(test_port()).await.unwrap();
        let resp = client.eval("42 nonexistentMethod").await.unwrap();
        assert!(resp.is_error(), "should be an error");
        assert!(resp.error_message().is_some(), "should have error message");
    }

    #[tokio::test]
    #[ignore = "integration test — requires running REPL (set BEAMTALK_TEST_PORT or default 19876)"]
    async fn test_bindings() {
        let client = ReplClient::connect(test_port()).await.unwrap();

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
    }

    #[tokio::test]
    #[ignore = "integration test — requires running REPL (set BEAMTALK_TEST_PORT or default 19876)"]
    async fn test_actors_list() {
        let client = ReplClient::connect(test_port()).await.unwrap();
        let resp = client.actors().await.unwrap();
        assert!(!resp.is_error());
        assert!(resp.actors.is_some(), "should return actors list");
    }

    #[tokio::test]
    #[ignore = "integration test — requires running REPL (set BEAMTALK_TEST_PORT or default 19876)"]
    async fn test_modules_list() {
        let client = ReplClient::connect(test_port()).await.unwrap();
        let resp = client.modules().await.unwrap();
        assert!(!resp.is_error());
        assert!(resp.modules.is_some(), "should return modules list");
    }

    #[tokio::test]
    #[ignore = "integration test — requires running REPL (set BEAMTALK_TEST_PORT or default 19876)"]
    async fn test_complete() {
        let client = ReplClient::connect(test_port()).await.unwrap();
        let resp = client.complete("Integer ").await.unwrap();
        assert!(!resp.is_error());
        assert!(resp.completions.is_some(), "should return completions list");
    }

    #[tokio::test]
    #[ignore = "integration test — requires running REPL (set BEAMTALK_TEST_PORT or default 19876)"]
    async fn test_load_file_and_spawn_actor() {
        let client = ReplClient::connect(test_port()).await.unwrap();

        // Load counter
        let resp = client.load_file("examples/counter.bt").await.unwrap();
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
    }

    #[tokio::test]
    #[ignore = "integration test — requires running REPL (set BEAMTALK_TEST_PORT or default 19876)"]
    async fn test_docs() {
        let client = ReplClient::connect(test_port()).await.unwrap();
        let resp = client.docs("Integer", None).await.unwrap();
        assert!(!resp.is_error());
        assert!(resp.docs.is_some(), "should return docs");
        let docs = resp.docs.unwrap();
        assert!(!docs.is_empty(), "docs should not be empty");
    }

    #[tokio::test]
    #[ignore = "integration test — requires running REPL (set BEAMTALK_TEST_PORT or default 19876)"]
    async fn test_value_string_formats_correctly() {
        let client = ReplClient::connect(test_port()).await.unwrap();

        // Integer value
        let resp = client.eval("42").await.unwrap();
        assert_eq!(resp.value_string(), "42");

        // String value
        let resp = client.eval("'test'").await.unwrap();
        assert_eq!(resp.value_string(), "test");

        // Nil value
        let resp = client.eval("nil").await.unwrap();
        assert_eq!(resp.value_string(), "nil");
    }

    #[tokio::test]
    #[ignore = "integration test — requires running REPL (set BEAMTALK_TEST_PORT or default 19876)"]
    async fn test_connection_failure() {
        // Port 1 should never have a REPL running
        let result = ReplClient::connect(1).await;
        assert!(result.is_err(), "connecting to port 1 should fail");
    }
}
