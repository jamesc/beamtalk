// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! REPL TCP client for communicating with the Beamtalk backend.
//!
//! **DDD Context:** REPL — Client Communication

use std::io::{BufRead, BufReader, Write};
use std::net::TcpStream;
use std::time::Duration;

use miette::{IntoDiagnostic, Result, miette};

use super::{CONNECT_TIMEOUT_MS, ReplResponse, next_msg_id};

pub(super) struct ReplClient {
    stream: TcpStream,
    reader: BufReader<TcpStream>,
    last_loaded_file: Option<String>,
}

impl ReplClient {
    /// Connect to the REPL backend.
    pub(super) fn connect(port: u16) -> Result<Self> {
        let addr = format!("127.0.0.1:{port}");
        let stream = TcpStream::connect_timeout(
            &addr.parse().into_diagnostic()?,
            Duration::from_millis(CONNECT_TIMEOUT_MS),
        )
        .map_err(|e| miette!("Failed to connect to REPL backend at {addr}: {e}"))?;

        // Clone for reader
        let reader_stream = stream.try_clone().into_diagnostic()?;
        let reader = BufReader::new(reader_stream);

        Ok(Self {
            stream,
            reader,
            last_loaded_file: None,
        })
    }

    /// Send a protocol request and receive the response.
    pub(super) fn send_request(&mut self, request: &serde_json::Value) -> Result<ReplResponse> {
        let request_str = serde_json::to_string(&request).into_diagnostic()?;
        writeln!(self.stream, "{request_str}").into_diagnostic()?;
        self.stream.flush().into_diagnostic()?;

        let mut response_line = String::new();
        self.reader
            .read_line(&mut response_line)
            .into_diagnostic()?;

        serde_json::from_str(&response_line)
            .map_err(|e| miette!("Failed to parse REPL response: {e}\nRaw: {response_line}"))
    }

    /// Send an eval request and receive the response.
    pub(super) fn eval(&mut self, expression: &str) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "eval",
            "id": next_msg_id(),
            "code": expression
        }))
    }

    /// Send a clear bindings request.
    pub(super) fn clear_bindings(&mut self) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "clear",
            "id": next_msg_id()
        }))
    }

    /// Get current bindings.
    pub(super) fn get_bindings(&mut self) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "bindings",
            "id": next_msg_id()
        }))
    }

    /// Load a Beamtalk file.
    pub(super) fn load_file(&mut self, path: &str) -> Result<ReplResponse> {
        let response = self.send_request(&serde_json::json!({
            "op": "load-file",
            "id": next_msg_id(),
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

    /// List running actors.
    pub(super) fn list_actors(&mut self) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "actors",
            "id": next_msg_id()
        }))
    }

    /// Kill an actor by PID string.
    pub(super) fn kill_actor(&mut self, pid_str: &str) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "kill",
            "id": next_msg_id(),
            "actor": pid_str
        }))
    }

    /// List loaded modules.
    pub(super) fn list_modules(&mut self) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "modules",
            "id": next_msg_id()
        }))
    }

    /// Unload a module by name.
    pub(super) fn unload_module(&mut self, module_name: &str) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "unload",
            "id": next_msg_id(),
            "module": module_name
        }))
    }

    /// List active sessions.
    pub(super) fn list_sessions(&mut self) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "sessions",
            "id": next_msg_id()
        }))
    }

    /// Inspect an actor's state.
    pub(super) fn inspect_actor(&mut self, pid_str: &str) -> Result<ReplResponse> {
        self.send_request(&serde_json::json!({
            "op": "inspect",
            "id": next_msg_id(),
            "actor": pid_str
        }))
    }
}
