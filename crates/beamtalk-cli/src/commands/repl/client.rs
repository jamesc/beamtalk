// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! REPL TCP client for communicating with the Beamtalk backend.
//!
//! **DDD Context:** REPL — Client Communication

use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;

use miette::{Result, miette};

use super::ReplResponse;
use crate::commands::protocol::{self, ProtocolClient};

/// Tracks whether the last `:load` was a single file or a directory.
#[derive(Clone, Debug)]
pub(crate) enum LastLoadedPath {
    /// Single file path (e.g., `examples/counter.bt`).
    File(String),
    /// Directory path (e.g., `stdlib/test/`).
    Directory(String),
}

/// REPL client that wraps [`ProtocolClient`] with REPL-specific operations.
pub(crate) struct ReplClient {
    inner: ProtocolClient,
    last_loaded_path: Option<LastLoadedPath>,
    /// Session ID assigned by the server (BT-666)
    session_id: Option<String>,
    /// Host address for this connection (BT-694)
    host: String,
    /// Port used for this connection (needed for interrupt connection)
    port: u16,
    /// Cookie used for this connection (needed for interrupt reconnection)
    cookie: String,
}

#[allow(dead_code)]
impl ReplClient {
    /// Connect to the REPL backend.
    pub(crate) fn connect(host: &str, port: u16, cookie: &str) -> Result<Self> {
        let inner = ProtocolClient::connect(host, port, cookie, None)?;
        let session_id = inner.session_id().map(String::from);

        Ok(Self {
            inner,
            last_loaded_path: None,
            session_id,
            host: host.to_string(),
            port,
            cookie: cookie.to_string(),
        })
    }

    /// Reconnect to the REPL backend, attempting to resume the previous session if possible.
    ///
    /// Returns `true` if the previous session was resumed, `false` if a fresh
    /// session was established.
    pub(crate) fn reconnect(&mut self) -> Result<bool> {
        let requested_session = self.session_id.clone();
        for attempt in 1..=super::MAX_CONNECT_RETRIES {
            match ProtocolClient::connect_with_resume(
                &self.host,
                self.port,
                &self.cookie,
                None,
                requested_session.as_deref(),
            ) {
                Ok(inner) => {
                    self.inner = inner;
                    self.session_id = self.inner.session_id().map(String::from);
                    let resumed = requested_session.is_some()
                        && requested_session.as_deref() == self.session_id.as_deref();
                    return Ok(resumed);
                }
                Err(e) => {
                    if attempt == super::MAX_CONNECT_RETRIES {
                        return Err(e);
                    }
                    std::thread::sleep(std::time::Duration::from_millis(super::RETRY_DELAY_MS));
                }
            }
        }
        unreachable!("reconnect loop always returns on final attempt")
    }

    /// Send a protocol request and receive the response.
    /// On transport error, attempt to reconnect and retry once (session resume).
    /// Parse/protocol errors are returned immediately without retry.
    pub(crate) fn send_request(&mut self, request: &serde_json::Value) -> Result<ReplResponse> {
        match self.inner.send_request::<ReplResponse>(request) {
            Ok(resp) => Ok(resp),
            Err(e) => {
                // Only retry on transport errors, not parse/protocol errors.
                // ProtocolClient::send_request produces "Failed to parse response"
                // for JSON errors — no "Failed to deserialize" at this layer.
                let msg = e.to_string();
                if msg.contains("Failed to parse") {
                    return Err(e);
                }
                self.reconnect()?;
                self.inner.send_request::<ReplResponse>(request)
            }
        }
    }

    /// Send an eval request and receive the response.
    #[allow(dead_code)] // Used in non-interruptible mode or tests
    pub(crate) fn eval(&mut self, expression: &str) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "eval",
            "id": protocol::next_msg_id(),
            "code": expression
        }))
    }

    /// Send an eval request that can be interrupted by Ctrl-C (BT-666).
    ///
    /// Sets a read timeout on the socket and polls for the response while
    /// checking the `interrupted` flag. If the flag is set, sends an
    /// interrupt op on a separate connection to the backend.
    ///
    /// BT-696: Handles multi-message streaming responses. Intermediate
    /// messages with `out` field are printed incrementally. The final
    /// message with `status: ["done"]` is returned as the result.
    pub(crate) fn eval_interruptible(
        &mut self,
        expression: &str,
        interrupted: &Arc<AtomicBool>,
    ) -> Result<ReplResponse> {
        // Send the eval request
        let request = serde_json::json!({
            "op": "eval",
            "id": protocol::next_msg_id(),
            "code": expression
        });
        self.inner.send_only(&request)?;

        // Set a short read timeout for polling
        self.inner
            .set_read_timeout(Some(Duration::from_millis(200)))?;

        // BT-696: Track whether we received streaming output chunks
        let mut streamed = false;

        let result = loop {
            match self.inner.read_response_line() {
                Ok(line) => {
                    // Got a response — parse it
                    let parsed: serde_json::Value = serde_json::from_str(&line)
                        .map_err(|e| miette!("Failed to parse response: {e}\nRaw: {line}"))?;

                    // BT-696: Check for streaming output chunk
                    if parsed.get("out").is_some() && parsed.get("status").is_none() {
                        if let Some(chunk) = parsed["out"].as_str() {
                            print!("{chunk}");
                            let _ = std::io::Write::flush(&mut std::io::stdout());
                        }
                        streamed = true;
                        continue;
                    }

                    // BT-698: Check for need-input status (stdin request)
                    if let Some(status) = parsed.get("status").and_then(|s| s.as_array()) {
                        let has_need_input =
                            status.iter().any(|s| s.as_str() == Some("need-input"));
                        if has_need_input {
                            self.handle_stdin_request(&parsed)?;
                            continue;
                        }
                    }

                    // Final response with status
                    let mut response: ReplResponse = serde_json::from_value(parsed)
                        .map_err(|e| miette!("Failed to parse response: {e}\nRaw: {line}"))?;
                    // BT-696: Clear output field if already streamed to avoid double-printing
                    if streamed {
                        response.output = None;
                    }
                    break Ok(response);
                }
                Err(e)
                    if e.kind() == std::io::ErrorKind::WouldBlock
                        || e.kind() == std::io::ErrorKind::TimedOut =>
                {
                    // Timeout — check if interrupted
                    if interrupted.swap(false, Ordering::SeqCst) {
                        // Send interrupt on a separate connection
                        self.send_interrupt();
                        // Continue waiting for the eval response (which should now be an error)
                    }
                }
                Err(e) => {
                    break Err(miette!("Communication error: {e}"));
                }
            }
        };

        // Restore blocking read timeout
        self.inner.set_read_timeout(None)?;
        result
    }

    /// Handle a need-input request from the server (BT-698).
    /// Prints the prompt, reads a line from stdin, and sends it back.
    fn handle_stdin_request(&mut self, parsed: &serde_json::Value) -> Result<()> {
        use std::io::{BufRead, Write};

        let prompt = parsed.get("prompt").and_then(|p| p.as_str()).unwrap_or("");
        // Reuse the eval request id for correlation
        let req_id = parsed
            .get("id")
            .and_then(|v| v.as_str())
            .map_or_else(protocol::next_msg_id, str::to_string);

        // Print the prompt
        print!("{prompt}");
        let _ = std::io::stdout().flush();

        // Read a line from stdin
        let mut input = String::new();
        let stdin = std::io::stdin();
        match stdin.lock().read_line(&mut input) {
            Ok(0) => {
                // EOF
                self.inner.send_only(&serde_json::json!({
                    "op": "stdin",
                    "id": req_id,
                    "value": "eof"
                }))?;
            }
            Ok(_) => {
                self.inner.send_only(&serde_json::json!({
                    "op": "stdin",
                    "id": req_id,
                    "value": input
                }))?;
            }
            Err(e) => {
                eprintln!("Error reading stdin: {e}");
                self.inner.send_only(&serde_json::json!({
                    "op": "stdin",
                    "id": req_id,
                    "value": "eof"
                }))?;
            }
        }
        Ok(())
    }

    /// Send an interrupt request on a separate WebSocket connection (BT-666).
    fn send_interrupt(&self) {
        let mut interrupt_req = serde_json::json!({
            "op": "interrupt",
            "id": protocol::next_msg_id()
        });
        if let Some(ref session) = self.session_id {
            interrupt_req["session"] = serde_json::Value::String(session.clone());
        }
        // Open a new connection and send interrupt — best effort
        if let Ok(mut interrupt_client) = ProtocolClient::connect_with_resume(
            &self.host,
            self.port,
            &self.cookie,
            Some(Duration::from_secs(2)),
            self.session_id.as_deref(),
        ) {
            let _ = interrupt_client.send_request::<serde_json::Value>(&interrupt_req);
        }
    }

    /// Send a clear bindings request.
    pub(crate) fn clear_bindings(&mut self) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "clear",
            "id": protocol::next_msg_id()
        }))
    }

    /// Get current bindings.
    pub(crate) fn get_bindings(&mut self) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "bindings",
            "id": protocol::next_msg_id()
        }))
    }

    /// Load a single Beamtalk file.
    pub(crate) fn load_file(&mut self, path: &str) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "load-file",
            "id": protocol::next_msg_id(),
            "path": path
        }))
    }

    /// Record that the last `:load` was a single file (called after successful load).
    pub(crate) fn set_last_loaded_file(&mut self, path: &str) {
        self.last_loaded_path = Some(LastLoadedPath::File(path.to_string()));
    }

    /// Record that the last `:load` was a directory (called after successful load).
    pub(crate) fn set_last_loaded_directory(&mut self, path: &str) {
        self.last_loaded_path = Some(LastLoadedPath::Directory(path.to_string()));
    }

    /// Get the last loaded path for `:reload` support.
    pub(crate) fn last_loaded_path(&self) -> Option<&LastLoadedPath> {
        self.last_loaded_path.as_ref()
    }

    /// Reload a specific module by name (looks up source path on server).
    pub(crate) fn reload_module(&mut self, module_name: &str) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "reload",
            "id": protocol::next_msg_id(),
            "module": module_name
        }))
    }

    /// List running actors.
    pub(crate) fn list_actors(&mut self) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "actors",
            "id": protocol::next_msg_id()
        }))
    }

    /// List loaded modules.
    pub(crate) fn list_modules(&mut self) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "modules",
            "id": protocol::next_msg_id()
        }))
    }

    /// List active sessions.
    pub(crate) fn list_sessions(&mut self) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "sessions",
            "id": protocol::next_msg_id()
        }))
    }

    /// Inspect an actor's state.
    pub(crate) fn inspect_actor(&mut self, pid_str: &str) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "inspect",
            "id": protocol::next_msg_id(),
            "actor": pid_str
        }))
    }

    /// Get completions for a prefix.
    ///
    #[allow(dead_code)] // API completeness — completions use separate ProtocolClient with short timeout
    pub(crate) fn complete(&mut self, prefix: &str) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "complete",
            "id": protocol::next_msg_id(),
            "code": prefix
        }))
    }

    /// Get documentation for a class or method.
    pub(crate) fn get_docs(&mut self, class: &str, selector: Option<&str>) -> Result<ReplResponse> {
        let mut req = serde_json::json!({
            "op": "docs",
            "id": protocol::next_msg_id(),
            "class": class
        });
        if let Some(sel) = selector {
            req["selector"] = serde_json::Value::String(sel.to_string());
        }
        self.send_request(&req)
    }

    /// Show generated Core Erlang for an expression (BT-724).
    pub(crate) fn show_codegen(&mut self, code: &str) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "show-codegen",
            "id": protocol::next_msg_id(),
            "code": code
        }))
    }
}
