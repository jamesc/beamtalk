// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Graceful-then-forced stop: the one implementation of "send SIGTERM, wait up
//! to a grace period for the process to go, then SIGKILL".
//!
//! The policy is the same everywhere; what differs is how a caller signals
//! (one PID, a process group, several groups), how it learns the process is
//! gone (`Child::try_wait`, `kill(pid, 0)`, a reaper thread draining a map)
//! and what it does when the grace runs out. So [`terminate_and_wait`] takes
//! the first two as closures and reports whether the grace ran out; the
//! caller then forces the stop, re-checking identity first if the target
//! could have been recycled meanwhile.

use std::time::{Duration, Instant};

/// How a [`terminate_and_wait`] call ended.
#[must_use]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Outcome {
    /// The target was gone before the grace period ran out.
    Exited,
    /// The grace period ran out with the target still there: the caller
    /// should force it (SIGKILL, `TerminateProcess`, …).
    GraceExpired,
}

/// Ask the target to stop, then wait up to `grace` for it to go.
///
/// Calls `terminate` once, then polls `exited` every `poll` (sleeping with
/// `sleep`, injected so tests need not wait on a clock) until it returns
/// `true` or `grace` has passed. `exited` is checked once more at the
/// deadline, so a zero grace still notices a target that is already gone.
pub fn terminate_and_wait(
    grace: Duration,
    poll: Duration,
    mut sleep: impl FnMut(Duration),
    terminate: impl FnOnce(),
    mut exited: impl FnMut() -> bool,
) -> Outcome {
    terminate();
    let deadline = Instant::now() + grace;
    loop {
        if exited() {
            return Outcome::Exited;
        }
        let now = Instant::now();
        if now >= deadline {
            return Outcome::GraceExpired;
        }
        sleep(poll.min(deadline - now));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::Cell;

    #[test]
    fn returns_exited_as_soon_as_the_target_is_gone() {
        let terminated = Cell::new(false);
        let polls = Cell::new(0);
        let outcome = terminate_and_wait(
            Duration::from_hours(1),
            Duration::from_millis(1),
            |_| {},
            || terminated.set(true),
            || {
                polls.set(polls.get() + 1);
                polls.get() == 3
            },
        );
        assert_eq!(outcome, Outcome::Exited);
        assert!(terminated.get());
        assert_eq!(polls.get(), 3);
    }

    #[test]
    fn reports_grace_expired_when_the_target_stays() {
        let outcome = terminate_and_wait(
            Duration::ZERO,
            Duration::from_millis(1),
            |_| panic!("a zero grace must not sleep"),
            || {},
            || false,
        );
        assert_eq!(outcome, Outcome::GraceExpired);
    }

    #[test]
    fn zero_grace_still_notices_a_target_already_gone() {
        let outcome = terminate_and_wait(Duration::ZERO, Duration::ZERO, |_| {}, || {}, || true);
        assert_eq!(outcome, Outcome::Exited);
    }
}
