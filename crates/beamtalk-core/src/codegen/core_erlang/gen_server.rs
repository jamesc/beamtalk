// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `Gen_server` scaffolding code generation.
//!
//! This module generates the OTP `gen_server` boilerplate for Beamtalk actors:
//! - Module structure with exports and attributes
//! - `start_link/0`, `spawn/0`, `spawn/1` functions
//! - `init/1` callback
//! - `handle_cast/2` and `handle_call/3` callbacks
//! - `code_change/3` callback for hot code reloading
//! - `terminate/2` callback
//! - Message dispatch logic
//! - Method table generation

// NOTE: Temporarily disabled during refactoring
// use super::{CoreErlangGenerator, Result};
// use crate::ast::Module;

// Methods will be moved here from erlang.rs
