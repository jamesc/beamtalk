// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared JSON-over-TCP protocol client for workspace communication.
//!
//! **DDD Context:** REPL — Protocol Transport
//!
//! Both the REPL and transcript viewer communicate with the workspace
//! BEAM node using newline-delimited JSON over TCP. This module provides
//! the shared transport layer.

use std::io::{BufRead, BufReader, Write};
use std::net::TcpStream;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Duration;

use miette::{IntoDiagnostic, Result, miette};

/// Default connection timeout in milliseconds.
const CONNECT_TIMEOUT_MS: u64 = 5000;

/// Counter for generating unique message IDs across all protocol clients.
static MSG_COUNTER: AtomicU64 = AtomicU64::new(1);

/// Generate a unique message ID.
pub fn next_msg_id() -> String {
    let n = MSG_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("msg-{n:03}")
}

/// Low-level JSON-over-TCP protocol client.
///
/// Handles connection, request framing (newline-delimited JSON), and
/// response reading. Higher-level clients wrap this to add typed
/// response parsing and domain-specific methods.
pub struct ProtocolClient {
    /// The underlying TCP stream for writing requests.
    stream: TcpStream,
    /// Buffered reader for the TCP stream, used for reading responses.
    reader: BufReader<TcpStream>,
}

impl ProtocolClient {
    /// Connect to a workspace backend at `127.0.0.1:{port}`.
    ///
    /// `read_timeout` sets the TCP read timeout (None for blocking reads).
    pub fn connect(port: u16, read_timeout: Option<Duration>) -> Result<Self> {
        let addr = format!("127.0.0.1:{port}");
        let stream = TcpStream::connect_timeout(
            &addr.parse().into_diagnostic()?,
            Duration::from_millis(CONNECT_TIMEOUT_MS),
        )
        .map_err(|e| miette!("Failed to connect to workspace at {addr}: {e}"))?;

        if let Some(timeout) = read_timeout {
            stream.set_read_timeout(Some(timeout)).into_diagnostic()?;
        }

        let reader_stream = stream.try_clone().into_diagnostic()?;
        let reader = BufReader::new(reader_stream);

        Ok(Self { stream, reader })
    }

    /// Send a JSON request and receive a raw JSON response.
    pub fn send_raw(&mut self, request: &serde_json::Value) -> Result<serde_json::Value> {
        let request_str = serde_json::to_string(request).into_diagnostic()?;
        writeln!(self.stream, "{request_str}").into_diagnostic()?;
        self.stream.flush().into_diagnostic()?;

        let mut response_line = String::new();
        self.reader
            .read_line(&mut response_line)
            .into_diagnostic()?;

        serde_json::from_str(&response_line)
            .map_err(|e| miette!("Failed to parse response: {e}\nRaw: {response_line}"))
    }

    /// Send a JSON request and deserialize the response into a typed struct.
    pub fn send_request<T: serde::de::DeserializeOwned>(
        &mut self,
        request: &serde_json::Value,
    ) -> Result<T> {
        let request_str = serde_json::to_string(request).into_diagnostic()?;
        writeln!(self.stream, "{request_str}").into_diagnostic()?;
        self.stream.flush().into_diagnostic()?;

        let mut response_line = String::new();
        self.reader
            .read_line(&mut response_line)
            .into_diagnostic()?;

        serde_json::from_str(&response_line)
            .map_err(|e| miette!("Failed to parse response: {e}\nRaw: {response_line}"))
    }
}
