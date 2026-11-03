// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared test-only port helpers (BT-4058, BT-4382).
//!
//! Kept apart from [`crate::test_support`] because that module's async WS
//! double needs `tokio`, and the synchronous crates that only want a closed
//! port (`beamtalk-cli`, `beamtalk-desktop-broker`) should not pull an async
//! runtime into their test builds for one constant. Opt in with the
//! `test-ports` feature (the `test` feature implies it).

/// A loopback port tests use when they need "nothing is listening here".
///
/// Never bind-then-drop `127.0.0.1:0` for that: the freed ephemeral port can
/// be handed to a parallel test's own listener before the probe runs,
/// flipping the test down the node-is-running path (BT-4058).
///
/// Port 1 (`tcpmux`) is safe because of who binds what, not because it is
/// unbindable: every fake server in the test suites binds `127.0.0.1:0`, OS
/// ephemeral-port allocators never hand out a port below 1024, no test binds
/// port 1 explicitly, and no standard service listens on it. It *can* be
/// bound by root, inside rootless containers that lower
/// `ip_unprivileged_port_start`, and by any process on Windows; a host that
/// actually runs a listener on port 1 would break these tests.
#[must_use]
pub const fn closed_port() -> u16 {
    1
}
