// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Async WebSocket client helpers shared by the Rust surfaces that talk to a
//! workspace (`beamtalk-lsp`, `beamtalk-mcp`): connect with a timeout and read
//! the next text frame (BT-4682). Callers keep their own error wording and
//! read timeouts, mapping [`ConnectError`] / [`ReadError`] to them.

use std::time::Duration;

use futures_util::StreamExt;
use tokio::net::TcpStream;
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::{MaybeTlsStream, WebSocketStream};

/// Why [`connect_with_timeout`] failed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConnectError {
    /// No connection within the timeout.
    TimedOut,
    /// The connection or WebSocket upgrade failed.
    Failed(String),
}

/// Why a text-frame read failed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReadError {
    /// The peer sent a close frame.
    Closed,
    /// The stream ended without a close frame.
    Ended,
    /// The transport reported an error.
    Failed(String),
}

/// Open a WebSocket to `url`, giving up after `timeout`.
///
/// # Errors
/// [`ConnectError::TimedOut`] or [`ConnectError::Failed`].
pub async fn connect_with_timeout(
    url: &str,
    timeout: Duration,
) -> Result<WebSocketStream<MaybeTlsStream<TcpStream>>, ConnectError> {
    tokio::time::timeout(timeout, tokio_tungstenite::connect_async(url))
        .await
        .map_err(|_| ConnectError::TimedOut)?
        .map(|(ws, _response)| ws)
        .map_err(|e| ConnectError::Failed(e.to_string()))
}

/// Read the next text frame, skipping ping / pong / binary frames.
///
/// # Errors
/// [`ReadError::Closed`], [`ReadError::Ended`] or [`ReadError::Failed`].
pub async fn read_text<S>(ws: &mut S) -> Result<String, ReadError>
where
    S: futures_util::Stream<Item = Result<Message, tokio_tungstenite::tungstenite::Error>> + Unpin,
{
    loop {
        match ws.next().await {
            Some(Ok(Message::Text(text))) => return Ok(text.to_string()),
            Some(Ok(Message::Close(_))) => return Err(ReadError::Closed),
            Some(Ok(_)) => {}
            Some(Err(e)) => return Err(ReadError::Failed(e.to_string())),
            None => return Err(ReadError::Ended),
        }
    }
}
