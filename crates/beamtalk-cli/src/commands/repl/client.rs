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

/// REPL client that wraps [`ProtocolClient`] with REPL-specific operations.
pub(super) struct ReplClient {
    inner: ProtocolClient,
    last_loaded_file: Option<String>,
    /// Session ID assigned by the server (BT-666)
    session_id: Option<String>,
    /// Host address for this connection (BT-694)
    host: String,
    /// Port used for this connection (needed for interrupt connection)
    port: u16,
    /// Cookie used for this connection (needed for interrupt reconnection)
    cookie: String,
}

impl ReplClient {
    /// Connect to the REPL backend.
    pub(super) fn connect(host: &str, port: u16, cookie: &str) -> Result<Self> {
        let inner = ProtocolClient::connect(host, port, cookie, None)?;
        let session_id = inner.session_id().map(String::from);

        Ok(Self {
            inner,
            last_loaded_file: None,
            session_id,
            host: host.to_string(),
            port,
            cookie: cookie.to_string(),
        })
    }

    /// Send a protocol request and receive the response.
    pub(super) fn send_request(&mut self, request: &serde_json::Value) -> Result<ReplResponse> {
        self.inner.send_request(request)
    }

    /// Send an eval request and receive the response.
    #[allow(dead_code)] // Used in non-interruptible mode or tests
    pub(super) fn eval(&mut self, expression: &str) -> Result<ReplResponse> {
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
    pub(super) fn eval_interruptible(
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

        let result = loop {
            match self.inner.read_response_line() {
                Ok(line) => {
                    // Got a response — parse it
                    let response: ReplResponse = serde_json::from_str(&line)
                        .map_err(|e| miette!("Failed to parse response: {e}\nRaw: {line}"))?;
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
        if let Ok(mut interrupt_client) = ProtocolClient::connect(
            &self.host,
            self.port,
            &self.cookie,
            Some(Duration::from_secs(2)),
        ) {
            let _ = interrupt_client.send_request::<serde_json::Value>(&interrupt_req);
        }
    }

    /// Send a clear bindings request.
    pub(super) fn clear_bindings(&mut self) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "clear",
            "id": protocol::next_msg_id()
        }))
    }

    /// Get current bindings.
    pub(super) fn get_bindings(&mut self) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "bindings",
            "id": protocol::next_msg_id()
        }))
    }

    /// Load a Beamtalk file.
    pub(super) fn load_file(&mut self, path: &str) -> Result<ReplResponse> {
        let response = self.send_request(&serde_json::json!({
            "op": "load-file",
            "id": protocol::next_msg_id(),
            "path": path
        }))?;

        // Update last loaded file on success
        if !response.is_error() {
            self.last_loaded_file = Some(path.to_string());
        }

        Ok(response)
    }

    /// Reload the last loaded file.
    pub(super) fn reload_file(&mut self) -> Result<ReplResponse> {
        let path = self
            .last_loaded_file
            .clone()
            .ok_or_else(|| miette!("No file has been loaded yet"))?;
        self.load_file(&path)
    }

    /// Reload a specific module by name (looks up source path on server).
    pub(super) fn reload_module(&mut self, module_name: &str) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "reload",
            "id": protocol::next_msg_id(),
            "module": module_name
        }))
    }

    /// List running actors.
    pub(super) fn list_actors(&mut self) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "actors",
            "id": protocol::next_msg_id()
        }))
    }

    /// Kill an actor by PID string.
    pub(super) fn kill_actor(&mut self, pid_str: &str) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "kill",
            "id": protocol::next_msg_id(),
            "actor": pid_str
        }))
    }

    /// List loaded modules.
    pub(super) fn list_modules(&mut self) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "modules",
            "id": protocol::next_msg_id()
        }))
    }

    /// Unload a module by name.
    pub(super) fn unload_module(&mut self, module_name: &str) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "unload",
            "id": protocol::next_msg_id(),
            "module": module_name
        }))
    }

    /// List active sessions.
    pub(super) fn list_sessions(&mut self) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "sessions",
            "id": protocol::next_msg_id()
        }))
    }

    /// Inspect an actor's state.
    pub(super) fn inspect_actor(&mut self, pid_str: &str) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "inspect",
            "id": protocol::next_msg_id(),
            "actor": pid_str
        }))
    }

    /// Get completions for a prefix.
    ///
    #[allow(dead_code)] // API completeness — completions use separate ProtocolClient with short timeout
    pub(super) fn complete(&mut self, prefix: &str) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "complete",
            "id": protocol::next_msg_id(),
            "code": prefix
        }))
    }

    /// Get documentation for a class or method.
    pub(super) fn get_docs(&mut self, class: &str, selector: Option<&str>) -> Result<ReplResponse> {
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
}
