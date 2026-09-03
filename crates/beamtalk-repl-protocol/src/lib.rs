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
/// Test-only loopback WebSocket double shared by dependent crates' own test
/// suites (BT-3331). Gated on `#[cfg(any(test, feature = "test"))]` to avoid
/// prod binary bloat while allowing dependent crates to opt in via the
/// `test` Cargo feature in their `[dev-dependencies]`.
#[cfg(any(test, feature = "test"))]
pub mod test_support;

pub use request::{RequestBuilder, next_msg_id};
pub use response::{ActorInfo, ClassInfo, ModuleInfo, ReplResponse, SessionInfo};
