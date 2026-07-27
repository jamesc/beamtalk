// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! epmd (Erlang Port Mapper Daemon) client utilities.
//!
//! Provides cross-platform epmd interaction using the binary `NAMES_REQ` TCP
//! protocol directly, avoiding subprocess spawning and PATH dependencies.
//!
//! Shared between `beamtalk-cli` (workspace lifecycle: conflict detection,
//! deregistration polling) and `beamtalk-desktop-broker` (ADR 0097 discovery
//! liveness — a *dist ping* is not available to a non-BEAM broker, so a raw
//! epmd `NAMES` query is the liveness check, per the ADR's discovery
//! contract). Originally lived only in `beamtalk-cli`; moved here (BT-2985)
//! so the broker doesn't need to link the CLI's internals to get the same
//! protocol implementation.
//!
//! **DDD Context:** CLI / Language Service

use std::io::{Read, Write};
use std::net::TcpStream;
use std::time::Duration;

use miette::{IntoDiagnostic, Result, miette};

/// epmd binary protocol port.
const EPMD_PORT: u16 = 4369;

/// TCP connect timeout for epmd queries in milliseconds.
const EPMD_CONNECT_TIMEOUT_MS: u64 = 500;

/// TCP read timeout for epmd `NAMES_REQ` responses in milliseconds.
const EPMD_READ_TIMEOUT_MS: u64 = 1000;

/// Query epmd's registered node names directly over TCP using the `NAMES_REQ` protocol.
///
/// Connects to `localhost:4369` and sends a `NAMES_REQ` (3 bytes: big-endian u16
/// length=1, then `0x6e`). epmd responds with a 4-byte big-endian u32 (its own
/// port, discarded), followed by lines of the form `"name <short> at port <N>\n"`
/// until EOF.
///
/// Returns `Ok(names)` on success, or `Ok(vec![])` if epmd is not running.
/// Returns `Err` only for unexpected IO errors after a connection is established.
///
/// Cross-platform: pure TCP, no subprocess, no PATH dependency.
///
/// # Errors
///
/// Returns an error if a connection to epmd is established but an
/// unexpected I/O error occurs while writing the request or reading the
/// response.
pub fn query_epmd_names() -> Result<Vec<String>> {
    let addr = std::net::SocketAddr::from(([127, 0, 0, 1], EPMD_PORT));

    let mut stream =
        match TcpStream::connect_timeout(&addr, Duration::from_millis(EPMD_CONNECT_TIMEOUT_MS)) {
            Ok(s) => s,
            Err(e)
                if e.kind() == std::io::ErrorKind::ConnectionRefused
                    || e.kind() == std::io::ErrorKind::TimedOut =>
            {
                // epmd not running — no names registered
                return Ok(Vec::new());
            }
            Err(e) => return Err(miette!("epmd connect error: {e}")),
        };

    stream
        .set_read_timeout(Some(Duration::from_millis(EPMD_READ_TIMEOUT_MS)))
        .into_diagnostic()?;

    // NAMES_REQ: big-endian u16 length=1, then 0x6e ('n')
    stream.write_all(&[0x00, 0x01, 0x6e]).into_diagnostic()?;

    // Discard the 4-byte big-endian u32 epmd port in the response header.
    let mut header = [0u8; 4];
    stream.read_exact(&mut header).into_diagnostic()?;

    // Read "name <short> at port <N>\n" lines until EOF.
    // epmd closes the connection after sending all names, so read_to_string
    // returns Ok when epmd closes its end. Treat WouldBlock/TimedOut as EOF
    // in case the OS doesn't signal it cleanly before the read timeout fires.
    let mut body = String::new();
    match stream.read_to_string(&mut body) {
        Ok(_) => {}
        Err(e)
            if e.kind() == std::io::ErrorKind::WouldBlock
                || e.kind() == std::io::ErrorKind::TimedOut =>
        {
            // epmd closed its end; accumulated body is complete
        }
        Err(e) => return Err(miette!("epmd read error: {e}")),
    }

    let mut names = Vec::new();
    for line in body.lines() {
        // Token-level parsing avoids false positives (e.g. "foo" matching "foobar").
        let mut parts = line.split_whitespace();
        if matches!(parts.next(), Some("name")) {
            if let Some(short_name) = parts.next() {
                names.push(short_name.to_string());
            }
        }
    }

    Ok(names)
}

/// Wait for a node name to be deregistered from epmd.
///
/// After force-killing a BEAM node, epmd may still hold the registration
/// briefly. This polls epmd's `NAMES_REQ` TCP protocol (port 4369) until the
/// node name disappears or the timeout is reached.
///
/// Returns `Ok(())` once deregistered, or if epmd is not running.
/// Returns `Err` on timeout.
///
/// Cross-platform: uses TCP directly rather than spawning `epmd -names`,
/// so it works on Windows and doesn't require epmd to be in PATH.
///
/// # Errors
///
/// Returns an error if `node_name`'s short name is still registered with
/// epmd after `timeout_secs`.
pub fn wait_for_epmd_deregistration(node_name: &str, timeout_secs: u64) -> Result<()> {
    let short_name = node_name.split('@').next().unwrap_or(node_name);
    let interval = Duration::from_millis(100);
    let deadline = std::time::Instant::now() + Duration::from_secs(timeout_secs);
    let mut last_query_err: Option<String> = None;

    while std::time::Instant::now() < deadline {
        match query_epmd_names() {
            Ok(names) if !names.iter().any(|n| n == short_name) => return Ok(()),
            Ok(_) => {
                last_query_err = None;
            }
            Err(e) => {
                // Transient error (e.g. TCP reset) — keep polling rather than
                // treating the failure as absence. Only explicit absence is success.
                last_query_err = Some(e.to_string());
            }
        }
        std::thread::sleep(interval);
    }

    if let Some(e) = last_query_err {
        Err(miette!(
            "Timed out waiting for '{short_name}' to deregister from epmd after \
             {timeout_secs}s (last query error: {e})"
        ))
    } else {
        Err(miette!(
            "Node '{short_name}' still registered in epmd after {timeout_secs}s"
        ))
    }
}

/// Returns `true` if `node_name`'s short name (the part before `@`) is
/// currently registered with the local epmd.
///
/// Thin convenience wrapper over [`query_epmd_names`] for callers (e.g. the
/// desktop broker's discovery liveness check, ADR 0097) that only need a
/// yes/no answer for one node rather than the full registered-names list.
///
/// # Errors
///
/// Propagates [`query_epmd_names`]'s errors.
pub fn is_registered(node_name: &str) -> Result<bool> {
    let short_name = node_name.split('@').next().unwrap_or(node_name);
    let names = query_epmd_names()?;
    Ok(names.iter().any(|n| n == short_name))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn query_epmd_names_returns_empty_when_not_running() {
        // epmd is almost certainly not running on port 4369 in a unit test
        // environment (or if it is, we just get names back — either is fine).
        // The key property: this must not panic or hang.
        let result = query_epmd_names();
        assert!(
            result.is_ok(),
            "query_epmd_names should not error: {result:?}"
        );
    }

    #[test]
    fn wait_for_epmd_deregistration_succeeds_when_epmd_not_running() {
        // If epmd isn't running, the node is trivially not registered.
        let result = wait_for_epmd_deregistration("nonexistent@localhost", 1);
        assert!(
            result.is_ok(),
            "should succeed immediately when epmd is not running: {result:?}"
        );
    }

    #[test]
    fn is_registered_false_when_epmd_not_running() {
        let result = is_registered("nonexistent@localhost");
        assert!(!result.unwrap());
    }
}
