// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Desktop attach connection-broker core (ADR 0097, BT-2985).
//!
//! Implements the desktop shell's connection-broker responsibilities per
//! ADR 0097 ("Desktop Attach Client — One Front Node per Workspace") and the
//! BT-2984 spike verdict: discovery of live workspaces, per-instance spawn
//! with the loopback/no-OIDC/entropy-seeded-sname posture, two-stage
//! readiness probing, post-attach monitoring, and orphan reaping.
//!
//! ```text
//! discover_workspaces() ─┐
//!                        ▼
//!         pick a workspace, spawn::spawn_front_with_port_retry
//!         (refuses on OIDC; retries on an apparent port conflict —
//!          see spawn::spawn_front_with_port_retry's doc comment for
//!          why that retry signal is a heuristic, not a proof)
//!                        │
//!                        ▼
//!          readiness::wait_ready (two-stage: HTTP up, then /readiness,
//!          each with its own readiness::ProbeTimeouts budget)
//!                        │
//!            ┌───────────┴───────────┐
//!            ▼                       ▼
//!         Ready                   Failed / TimedOut
//!            │                  (surface to UI before
//!            ▼                   the window opens)
//!   reap::save_record (bookkeeping,
//!   for orphan reaping on a future
//!   broker restart) + monitor::Monitor
//!   (periodic /readiness re-poll)
//! ```
//!
//! ## What this crate is not
//!
//! This is the broker **core** — a plain Rust library with no GUI
//! dependency. It does not:
//! - build or wire a picker UI ([BT-2986])
//! - bundle or wire an actual Tauri shell / packaging lane ([BT-2987]/[BT-2988])
//! - bundle or supervise the Rust `beamtalk` toolchain (it *invokes* the
//!   user's already-installed CLI for workspace create/stop — [`cli_ops`] —
//!   never links or ships it, per ADR constraint 1)
//!
//! A future shell crate depends on this one for the process-supervision
//! logic and adds the window/event-loop/UI layer on top.
//!
//! [BT-2986]: https://linear.app/beamtalk/issue/BT-2986
//! [BT-2987]: https://linear.app/beamtalk/issue/BT-2987
//! [BT-2988]: https://linear.app/beamtalk/issue/BT-2988
//!
//! **DDD Context:** Desktop Shell

/// Create/stop workspaces via the installed `beamtalk` CLI (ADR 0097 Broker §5).
pub mod cli_ops;
/// Workspace discovery: enumerate `metadata.json`, check epmd liveness.
pub mod discovery;
/// Broker error type.
pub mod error;
/// Post-attach connection monitoring: periodic `/readiness` re-poll.
pub mod monitor;
/// Refuse-to-spawn guard for OIDC configuration.
pub mod oidc_guard;
/// Free port allocation with conflict retry.
pub mod port;
/// Two-stage readiness probe (HTTP up, then `/readiness`).
pub mod readiness;
/// Orphan reaping: PID-file sweep hardened against PID reuse.
pub mod reap;
/// Distribution sname prediction (`BT_ATTACH_NODE_SUFFIX` contract).
pub mod sname;
/// Spawn a per-workspace front with the required env/posture.
pub mod spawn;
/// Windows Job Object wrapper tying a spawned front's process tree to this
/// handle's lifetime (BT-2988) — see the module doc comment for why
/// `Child::kill()` alone is not enough on Windows.
#[cfg(windows)]
pub mod winjob;

#[cfg(test)]
mod test_support;

pub use discovery::{WorkspaceSummary, discover_workspaces};
pub use error::{BrokerError, Result};
pub use spawn::{SpawnAttemptConfig, SpawnConfig, spawn_front, spawn_front_with_port_retry};
