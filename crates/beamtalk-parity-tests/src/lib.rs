// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

// This crate is a test-only harness; relax pedantic lints that aren't useful
// for short-lived integration helpers (missing Errors/Panics docs, inlined
// format args, etc.). Real production code paths have stricter lints.
#![allow(
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::uninlined_format_args,
    clippy::needless_pass_by_value,
    clippy::map_unwrap_or,
    clippy::needless_continue,
    clippy::too_many_lines
)]

//! Cross-surface parity test harness for Beamtalk (BT-2077).
//!
//! **DDD Context:** Cross-cutting — Quality / Tooling
//!
//! This crate drives the same input through multiple surfaces (REPL, MCP, CLI,
//! LSP) and asserts that the observable behaviour is equivalent. It exists to
//! prevent silent surface drift as the language and tooling evolve.
//!
//! The harness is intentionally lightweight: it does not depend on the MCP or
//! LSP client implementations directly. Instead, each surface is driven via
//! its real user-facing entry point (a child process for MCP / CLI / LSP, a
//! WebSocket connection for the REPL).
//!
//! The integration test in `tests/parity.rs` is `#[ignore]`'d by default and
//! must be invoked through `just test-parity` (or with `cargo test --ignored`)
//! because it spins up real workspaces and child processes.

pub mod cases;
pub mod drivers;
pub mod normalize;
pub mod pool;
