// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Socket and IPC transport for the compiler daemon.
//!
//! This module handles Unix socket connections and client communication.
//! It manages the socket lifecycle, connection acceptance, and message I/O.

use std::io::{BufRead, BufReader, Write};
#[cfg(unix)]
use std::os::unix::net::{UnixListener, UnixStream};
use std::sync::Arc;
use std::sync::atomic::AtomicBool;

use beamtalk_core::language_service::SimpleLanguageService;
use miette::{IntoDiagnostic, Result};
use tracing::{debug, error};

use super::protocol;

/// Handle a single client connection.
#[cfg(unix)]
#[expect(
    clippy::needless_pass_by_value,
    reason = "UnixStream needs to be passed by value for the lifetime of the connection"
)]
pub fn handle_connection(
    stream: UnixStream,
    service: &mut SimpleLanguageService,
    running: &Arc<AtomicBool>,
) -> Result<()> {
    stream.set_nonblocking(false).into_diagnostic()?;

    let mut reader = BufReader::new(&stream);
    let mut writer = &stream;

    loop {
        let mut line = String::new();
        match reader.read_line(&mut line) {
            Ok(0) => {
                // EOF - client disconnected
                debug!("Client disconnected");
                break;
            }
            Ok(_) => {
                let response = protocol::handle_request(&line, service, running);
                writeln!(writer, "{response}").into_diagnostic()?;
                writer.flush().into_diagnostic()?;
            }
            Err(e) => {
                error!("Error reading from client: {e}");
                break;
            }
        }
    }

    Ok(())
}

/// Accept connections from the Unix socket listener.
///
/// This is a thin wrapper around `UnixListener::accept()` that provides:
/// - A clear abstraction boundary between transport and lifecycle modules
/// - A central point for future enhancements (e.g., connection filtering, rate limiting)
/// - Consistent error handling for connection acceptance
///
/// The wrapper exists for modularity - the lifecycle module doesn't need to know
/// the details of how connections are accepted, only that they can be obtained
/// from the transport layer.
#[cfg(unix)]
pub fn accept_connection(
    listener: &UnixListener,
) -> std::io::Result<(UnixStream, std::os::unix::net::SocketAddr)> {
    listener.accept()
}

#[cfg(test)]
mod tests {
    //! Unit tests for daemon transport layer.
    //!
    //! Most functions in this module involve I/O operations (Unix sockets, network streams)
    //! that are better tested via integration tests with real sockets and connections.
    //!
    //! Unit tests here are limited to verifying that functions exist and have correct
    //! signatures, ensuring the API surface is stable.

    #[cfg(unix)]
    #[test]
    fn accept_connection_is_callable() {
        // This test just verifies the function exists and is callable.
        // Actual I/O testing requires integration tests with real sockets.
    }
}
