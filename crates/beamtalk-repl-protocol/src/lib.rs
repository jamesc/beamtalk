// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared REPL protocol types for the Beamtalk workspace JSON-over-WebSocket
//! protocol (ADR 0020).
//!
//! **DDD Context:** REPL — Protocol Contract
//!
//! This crate provides the canonical request builder and response types used by
//! both `beamtalk-cli` and `beamtalk-mcp` when communicating with the workspace
//! backend. Having a single source of truth prevents the two clients from
//! diverging when the protocol evolves.

pub mod format;
pub mod handshake;
mod request;
mod response;
/// Test-only port helpers (no async deps). Gated on
/// `#[cfg(any(test, feature = "test-ports"))]`; the `test` feature implies
/// `test-ports`.
#[cfg(any(test, feature = "test-ports"))]
pub mod test_ports;
/// Test-only loopback WebSocket double shared by dependent crates' own test
/// suites. Gated on `#[cfg(any(test, feature = "test"))]` to avoid
/// prod binary bloat while allowing dependent crates to opt in via the
/// `test` Cargo feature in their `[dev-dependencies]`.
#[cfg(any(test, feature = "test"))]
pub mod test_support;
/// Test-only version-lock fixtures (stale/matching `versions`, `describe`
/// replies; no async deps). Same gating as [`test_ports`].
#[cfg(any(test, feature = "test-ports"))]
pub mod test_versions;
#[cfg(feature = "ws-client")]
pub mod ws_client;

pub use request::{RequestBuilder, next_msg_id};
pub use response::{
    ActorInfo, ClassInfo, ModuleInfo, ReplResponse, SessionInfo, StackFrameInfo, Warning,
};
