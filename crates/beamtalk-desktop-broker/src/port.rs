// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Port allocation for spawned fronts (ADR 0097 Broker §2).
//!
//! The spike (`docs/research/desktop-shell-spike.md`, criterion (a)) flagged
//! its throwaway `free_port()` helper — bind port 0, read the assigned
//! number, close, hand that number to the front — as having a
//! check-then-use race: nothing stops another process grabbing the same port
//! between the close and the front's own bind. Nothing in this module makes
//! that race impossible (only the OS can, by keeping the socket open across
//! the handoff, which spawning a *separate* front process can't do), but
//! [`allocate_port_with_retry`] gives the caller a place to retry with a
//! fresh candidate when a spawn attempt reports the port was already taken,
//! rather than assuming the probe was race-free.
//!
//! **DDD Context:** Desktop Shell

use std::net::TcpListener;

use crate::error::{BrokerError, Result};

/// Default number of candidate ports to try before giving up.
pub const DEFAULT_MAX_ATTEMPTS: u32 = 10;

/// Ask the OS for a free TCP port on loopback by binding to port 0, reading
/// back the assigned port, and immediately releasing it.
///
/// This is a *candidate*, not a reservation — see the module docs for the
/// race this implies and why [`allocate_port_with_retry`] exists.
///
/// # Errors
///
/// Returns an error if binding a loopback listener or reading back its
/// assigned port fails.
pub fn find_free_port() -> Result<u16> {
    let listener = TcpListener::bind(("127.0.0.1", 0))?;
    let port = listener.local_addr()?.port();
    drop(listener);
    Ok(port)
}

/// Outcome of a single spawn attempt at a candidate port, as reported by the
/// caller-supplied `try_spawn` closure in [`allocate_port_with_retry`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpawnAttempt {
    /// The port was free and the front was spawned (or spawn was attempted
    /// with no evidence of a port conflict) — stop retrying.
    Bound,
    /// The port was already taken by the time the real spawn tried to bind
    /// it (the race the module docs describe) — try another candidate.
    PortTaken,
}

/// Find a free port and hand it to `try_spawn`, retrying with a fresh
/// candidate up to `max_attempts` times if `try_spawn` reports [`SpawnAttempt::PortTaken`].
///
/// Returns the port that succeeded. Returns [`BrokerError::PortsExhausted`]
/// if every attempt reports `PortTaken`, or propagates the first non-conflict
/// error `try_spawn` returns (a conflict is retryable; anything else is not).
///
/// # Errors
///
/// Returns [`BrokerError::PortsExhausted`] after `max_attempts` conflicts,
/// or propagates the first non-conflict error from `try_spawn` or
/// [`find_free_port`].
pub fn allocate_port_with_retry(
    max_attempts: u32,
    mut try_spawn: impl FnMut(u16) -> Result<SpawnAttempt>,
) -> Result<u16> {
    for _ in 0..max_attempts.max(1) {
        let candidate = find_free_port()?;
        if let SpawnAttempt::Bound = try_spawn(candidate)? {
            return Ok(candidate);
        }
    }
    Err(BrokerError::PortsExhausted(max_attempts))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn find_free_port_returns_a_usable_nonprivileged_port() {
        let port = find_free_port().expect("should find a free port");
        assert!(port >= 1024, "port {port} should be non-privileged");
        // The port must actually be bindable again immediately after —
        // proves it's a real, currently-free port, not a garbage value.
        let listener =
            TcpListener::bind(("127.0.0.1", port)).expect("port should be bindable after probe");
        drop(listener);
    }

    #[test]
    fn find_free_port_returns_distinct_ports_across_many_calls() {
        // Not a hard guarantee (the OS could theoretically recycle), but with
        // ephemeral-range allocation this should hold overwhelmingly in
        // practice and catches a broken "always returns the same port" bug.
        let mut seen = HashSet::new();
        for _ in 0..20 {
            seen.insert(find_free_port().unwrap());
        }
        assert!(
            seen.len() > 1,
            "expected varied ports across calls, got a single repeated value"
        );
    }

    #[test]
    fn allocate_port_with_retry_succeeds_on_first_bindable_candidate() {
        let mut calls = 0;
        let port = allocate_port_with_retry(DEFAULT_MAX_ATTEMPTS, |_candidate| {
            calls += 1;
            Ok(SpawnAttempt::Bound)
        })
        .expect("should succeed immediately");
        assert!(port >= 1024);
        assert_eq!(calls, 1, "should not retry when the first attempt succeeds");
    }

    #[test]
    fn allocate_port_with_retry_retries_on_port_taken() {
        let mut calls = 0;
        let mut seen_candidates = HashSet::new();
        let port = allocate_port_with_retry(DEFAULT_MAX_ATTEMPTS, |candidate| {
            calls += 1;
            seen_candidates.insert(candidate);
            if calls < 3 {
                Ok(SpawnAttempt::PortTaken)
            } else {
                Ok(SpawnAttempt::Bound)
            }
        })
        .expect("should succeed on the third attempt");
        assert!(port >= 1024);
        assert_eq!(calls, 3, "should retry exactly twice before succeeding");
        assert_eq!(
            seen_candidates.len(),
            3,
            "each retry should try a fresh candidate port, not reuse the taken one"
        );
    }

    #[test]
    fn allocate_port_with_retry_gives_up_after_max_attempts() {
        let mut calls = 0;
        let result = allocate_port_with_retry(4, |_candidate| {
            calls += 1;
            Ok(SpawnAttempt::PortTaken)
        });
        assert!(matches!(result, Err(BrokerError::PortsExhausted(4))));
        assert_eq!(calls, 4, "should stop after exactly max_attempts tries");
    }

    #[test]
    fn allocate_port_with_retry_propagates_non_conflict_errors_immediately() {
        let mut calls = 0;
        let result: Result<u16> = allocate_port_with_retry(DEFAULT_MAX_ATTEMPTS, |_candidate| {
            calls += 1;
            Err(BrokerError::CliNotFound)
        });
        assert!(matches!(result, Err(BrokerError::CliNotFound)));
        assert_eq!(
            calls, 1,
            "a non-conflict error should abort immediately, not retry"
        );
    }

    #[test]
    fn allocate_port_with_retry_treats_zero_max_attempts_as_one() {
        let mut calls = 0;
        let result = allocate_port_with_retry(0, |_candidate| {
            calls += 1;
            Ok(SpawnAttempt::PortTaken)
        });
        assert!(result.is_err());
        assert_eq!(calls, 1, "0 attempts should be clamped to at least 1");
    }
}
