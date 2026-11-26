// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared leaf: OS child-process helpers (BT-5195).
//!
//! **DDD Context:** Shared Infrastructure
//!
//! Several binaries manage OS child processes: `beamtalk-exec` (the runtime's
//! subprocess port), `beamtalk-desktop-broker` (desktop fronts),
//! `beamtalk-workspace` (stopping a workspace node) and `beamtalk run`. The
//! rules they share live here, once:
//!
//! - [`escalate`]: ask a process to stop, wait a bounded grace period, then
//!   force it.
//! - [`exit_code`]: the shell convention for a child's exit status.
//! - `winjob` (Windows only): a kill-on-close Job Object and a suspended
//!   start that assigns the child before it runs.

pub mod escalate;
pub mod exit_code;
#[cfg(windows)]
pub mod winjob;
