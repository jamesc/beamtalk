// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! epmd (Erlang Port Mapper Daemon) client utilities.
//!
//! Provides cross-platform epmd interaction using the binary `NAMES_REQ` TCP
//! protocol directly, avoiding subprocess spawning and PATH dependencies.
//!
//! **DDD Context:** CLI

use std::io::{Read, Write};
use std::net::{IpAddr, Ipv4Addr, SocketAddr, TcpStream, UdpSocket};
use std::time::Duration;

use miette::{IntoDiagnostic, Result, miette};

use super::storage::workspace_dir;

/// epmd binary protocol port.
const EPMD_PORT: u16 = 4369;

/// Short connect timeout (ms) for the loopback-posture preflight probes.
const EPMD_POSTURE_PROBE_TIMEOUT_MS: u64 = 300;

/// TCP connect timeout for epmd queries in milliseconds.
const EPMD_CONNECT_TIMEOUT_MS: u64 = 500;

/// TCP read timeout for epmd `NAMES_REQ` responses in milliseconds.
const EPMD_READ_TIMEOUT_MS: u64 = 1000;

/// Maximum retries when epmd rejects a node name due to stale registration.
pub(super) const EPMD_CONFLICT_MAX_RETRIES: usize = 5;

/// Interval between epmd-conflict retry attempts in milliseconds.
pub(super) const EPMD_CONFLICT_RETRY_INTERVAL_MS: u64 = 500;

/// Timeout for each epmd deregistration wait during conflict retry in seconds.
pub(super) const EPMD_CONFLICT_DEREGISTER_TIMEOUT_SECS: u64 = 10;

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
pub(super) fn query_epmd_names() -> Result<Vec<String>> {
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
pub(super) fn wait_for_epmd_deregistration(node_name: &str, timeout_secs: u64) -> Result<()> {
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

/// Check whether a workspace's startup log indicates an epmd name-conflict failure.
///
/// Called after `start_detached_node` returns a PID-file timeout error to
/// distinguish an epmd "already registered" rejection from other crash causes.
/// Reads `startup.log` and looks for Erlang kernel registration error strings.
pub(super) fn is_epmd_name_conflict(workspace_id: &str) -> bool {
    let Ok(dir) = workspace_dir(workspace_id) else {
        return false;
    };
    let Ok(content) = std::fs::read_to_string(dir.join("startup.log")) else {
        return false;
    };
    // Erlang kernel prints one of these when epmd rejects name registration:
    //   "already_registered"  — OTP 25+ net_kernel error atom
    //   "Protocol: register"  — older net_kernel registration failure prefix
    content.contains("already_registered") || content.contains("Protocol: register")
}

/// The bind posture of the epmd daemon this host would register the workspace
/// node with (ADR 0091 Decision 5, review finding F1).
///
/// epmd is a *persistent per-user daemon*: a workspace node joins whatever epmd
/// is already running, which on many developer machines was started by other
/// Erlang tooling and may listen on `0.0.0.0`. "Loopback epmd" is therefore not
/// automatic, and `ERL_EPMD_ADDRESS=127.0.0.1` only governs an epmd a node
/// *starts itself* — it does not re-bind an epmd that is already up. This enum
/// is the result of actively probing the *running* posture so the launcher can
/// warn before exposing a node registration on a non-loopback interface.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum EpmdPosture {
    /// No epmd reachable on loopback — a fresh node will start its own,
    /// constrained to loopback by `ERL_EPMD_ADDRESS` (set in `startup_command`).
    NotRunning,
    /// epmd is reachable on loopback and *not* on this host's primary
    /// non-loopback interface — the safe, default posture.
    LoopbackOnly,
    /// epmd answers on a non-loopback interface (bound to `0.0.0.0` or that
    /// interface): the port mapper, and every node name registered with it, is
    /// visible off-host. The address that answered is carried for the warning.
    Promiscuous(Ipv4Addr),
}

/// Discover this host's primary non-loopback IPv4 address without enumerating
/// interfaces or sending any packets.
///
/// Uses the standard "connected UDP socket" trick: a `connect/2` on a datagram
/// socket only sets the default route/destination (the kernel picks the egress
/// interface) — no datagram is sent — so `local_addr/0` then reports the IP the
/// OS would source from. The destination is `192.0.2.1` (RFC 5737 TEST-NET-1),
/// which is guaranteed never to be routed, so this works offline and on
/// air-gapped hosts. Returns `None` when the host has only loopback addressing.
fn primary_non_loopback_ipv4() -> Option<Ipv4Addr> {
    let socket = UdpSocket::bind(("0.0.0.0", 0)).ok()?;
    socket.connect(("192.0.2.1", 9)).ok()?;
    match socket.local_addr().ok()?.ip() {
        IpAddr::V4(ip) if !ip.is_loopback() && !ip.is_unspecified() => Some(ip),
        _ => None,
    }
}

/// Returns `true` if epmd accepts a TCP connection at `addr` within the short
/// preflight timeout. A refused/timed-out connection means epmd is not reachable
/// at that address (the loopback-only / not-running cases).
fn epmd_reachable_at(addr: IpAddr) -> bool {
    let socket = SocketAddr::new(addr, EPMD_PORT);
    TcpStream::connect_timeout(
        &socket,
        Duration::from_millis(EPMD_POSTURE_PROBE_TIMEOUT_MS),
    )
    .is_ok()
}

/// Probe the running epmd's bind posture (ADR 0091 Decision 5 / finding F1).
///
/// The launcher calls this *before* spawning a workspace node so it can warn
/// when a pre-existing epmd is exposed off-host — the case `ERL_EPMD_ADDRESS`
/// cannot fix because the node merely joins the already-running daemon. The
/// probe is two cheap TCP connects: loopback (is epmd up?) and this host's
/// primary non-loopback interface (is it reachable off-loopback?).
pub(super) fn check_epmd_loopback() -> EpmdPosture {
    if let Some(ip) = primary_non_loopback_ipv4() {
        if epmd_reachable_at(IpAddr::V4(ip)) {
            return EpmdPosture::Promiscuous(ip);
        }
    }

    if epmd_reachable_at(IpAddr::V4(Ipv4Addr::LOCALHOST)) {
        EpmdPosture::LoopbackOnly
    } else {
        EpmdPosture::NotRunning
    }
}

/// Warn (on stderr) when a pre-existing epmd is bound to a non-loopback
/// interface, so an operator launching a workspace knows the port mapper — and
/// every node name it holds — is reachable off-host (ADR 0091 Decision 5).
///
/// This *warns* rather than *refuses*: epmd is a shared per-user daemon that
/// other Erlang tooling may have started promiscuously through no fault of this
/// launch, and refusing would break the zero-config localhost dev story
/// (ADR 0020 Principle 3). The remediation is actionable in the message. Returns
/// the probed posture so callers/tests can assert on it.
pub(super) fn warn_if_epmd_promiscuous() -> EpmdPosture {
    let posture = check_epmd_loopback();
    if let EpmdPosture::Promiscuous(ip) = posture {
        eprintln!(
            "⚠️  epmd is reachable on a non-loopback interface ({ip}:{EPMD_PORT}).\n   \
             Erlang distribution should stay off untrusted networks (ADR 0091). A pre-existing\n   \
             epmd was started promiscuously (likely bound to 0.0.0.0) by other tooling; the\n   \
             workspace node will register with it and be visible off-host.\n   \
             Remediation: stop the stray epmd and let the workspace start its own loopback epmd\n   \
             (`epmd -kill` when no other Erlang nodes need it), or, for a trusted private network,\n   \
             export ERL_EPMD_ADDRESS=<private-interface-ip> before launching — never 0.0.0.0."
        );
    }
    posture
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
    fn is_epmd_name_conflict_returns_false_for_unknown_workspace() {
        let result = is_epmd_name_conflict("__nonexistent_workspace_id__");
        assert!(!result, "unknown workspace should return false");
    }

    #[test]
    fn primary_non_loopback_ipv4_is_never_loopback_or_unspecified() {
        // May be None on a loopback-only host (e.g. minimal CI); when present it
        // must be a genuine non-loopback, specified address — the property the
        // promiscuity probe relies on. Sends no packets.
        if let Some(ip) = primary_non_loopback_ipv4() {
            assert!(
                !ip.is_loopback(),
                "must not return a loopback address: {ip}"
            );
            assert!(!ip.is_unspecified(), "must not return 0.0.0.0: {ip}");
        }
    }

    #[test]
    fn check_epmd_loopback_is_total_and_consistent() {
        // The probe must never panic/hang and must agree with itself: if it
        // reports a non-loopback address, that address must be a real one.
        match check_epmd_loopback() {
            EpmdPosture::NotRunning | EpmdPosture::LoopbackOnly => {}
            EpmdPosture::Promiscuous(ip) => {
                assert!(!ip.is_loopback(), "promiscuous addr must be non-loopback");
            }
        }
    }

    #[test]
    fn warn_if_epmd_promiscuous_returns_a_posture_without_panicking() {
        // Belt-and-braces: warning is best-effort and must return the posture it
        // probed (the value the running-posture check in BT-2424 asserts on)
        // without panicking. We don't pin the variant — the sandbox's epmd state
        // is not under test control — only that a valid posture comes back.
        match warn_if_epmd_promiscuous() {
            EpmdPosture::NotRunning | EpmdPosture::LoopbackOnly => {}
            EpmdPosture::Promiscuous(ip) => assert!(!ip.is_loopback()),
        }
    }
}
