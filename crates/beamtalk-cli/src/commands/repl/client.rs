// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! REPL TCP client for communicating with the Beamtalk backend.
//!
//! **DDD Context:** REPL — Client Communication

use miette::{Result, miette};

use super::ReplResponse;
use crate::commands::protocol::{self, ProtocolClient};

pub(super) struct ReplClient {
    inner: ProtocolClient,
    last_loaded_file: Option<String>,
}

impl ReplClient {
    /// Connect to the REPL backend.
    pub(super) fn connect(port: u16) -> Result<Self> {
        let inner = ProtocolClient::connect(port, None)?;
        Ok(Self {
            inner,
            last_loaded_file: None,
        })
    }

    /// Send a protocol request and receive the response.
    pub(super) fn send_request(&mut self, request: &serde_json::Value) -> Result<ReplResponse> {
        self.inner.send_request(request)
    }

    /// Send an eval request and receive the response.
    pub(super) fn eval(&mut self, expression: &str) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "eval",
            "id": protocol::next_msg_id(),
            "code": expression
        }))
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

    /// Get documentation for a class or method.
    pub(super) fn get_docs(
        &mut self,
        class: &str,
        selector: Option<&str>,
        show_inherited: bool,
    ) -> Result<ReplResponse> {
        let mut req = serde_json::json!({
            "op": "docs",
            "id": protocol::next_msg_id(),
            "class": class
        });
        if let Some(sel) = selector {
            req["selector"] = serde_json::Value::String(sel.to_string());
        }
        if show_inherited {
            req["show_inherited"] = serde_json::Value::Bool(true);
        }
        self.send_request(&req)
    }
}
