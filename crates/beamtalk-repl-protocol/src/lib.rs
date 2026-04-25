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
mod request;
mod response;

pub use request::{RequestBuilder, next_msg_id};
pub use response::{ActorInfo, ClassInfo, ModuleInfo, ReplResponse, SessionInfo};
