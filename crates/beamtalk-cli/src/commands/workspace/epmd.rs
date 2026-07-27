// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! epmd (Erlang Port Mapper Daemon) client utilities.
//!
//! The `NAMES_REQ` TCP protocol implementation and deregistration polling
//! live in `beamtalk-workspace` (moved there in BT-2985 so the desktop
//! broker's discovery liveness check — the same epmd `NAMES` query — doesn't
//! need to link the CLI's internals). This module re-exports them under
//! their original names and keeps the CLI-specific posture-probing
//! (`EpmdPosture`, promiscuity warning) and conflict-retry constants, which
//! are not shared with the broker.
//!
//! **DDD Context:** CLI

use std::net::{IpAddr, Ipv4Addr, SocketAddr, TcpStream, UdpSocket};
use std::time::Duration;

use super::storage::workspace_dir;

pub(super) use beamtalk_workspace::epmd::wait_for_epmd_deregistration;
// Only exercised by this module's own tests below — everything else in the
// CLI that needs the raw names list goes through `wait_for_epmd_deregistration`.
#[cfg(test)]
use beamtalk_workspace::epmd::query_epmd_names;

/// epmd binary protocol port.
const EPMD_PORT: u16 = 4369;

/// Short connect timeout (ms) for the loopback-posture preflight probes.
const EPMD_POSTURE_PROBE_TIMEOUT_MS: u64 = 300;

/// Maximum retries when epmd rejects a node name due to stale registration.
pub(super) const EPMD_CONFLICT_MAX_RETRIES: usize = 5;

/// Interval between epmd-conflict retry attempts in milliseconds.
pub(super) const EPMD_CONFLICT_RETRY_INTERVAL_MS: u64 = 500;

/// Timeout for each epmd deregistration wait during conflict retry in seconds.
pub(super) const EPMD_CONFLICT_DEREGISTER_TIMEOUT_SECS: u64 = 10;

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
    fn bt2424_default_deployment_keeps_epmd_off_public_interfaces() {
        // BT-2424 transport-posture smoke check: in the default deployment the
        // workspace's epmd must not be reachable on a non-loopback interface. A
        // standard CI/host runs at most a loopback epmd (or none), so the probe
        // must not classify it as Promiscuous. A `Promiscuous` result here is a
        // genuine finding (a stray epmd bound to 0.0.0.0), not test flakiness —
        // which is exactly the posture this check is meant to catch.
        assert!(
            !matches!(check_epmd_loopback(), EpmdPosture::Promiscuous(_)),
            "default deployment must not expose epmd on a public interface"
        );
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
