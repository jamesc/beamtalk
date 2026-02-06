// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `gen_server` scaffolding code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! This module generates the OTP `gen_server` boilerplate for Beamtalk actors:
//! - Module structure with exports and attributes
//! - `start_link/1`, `spawn/0`, `spawn/1` functions
//! - `init/1` callback
//! - `handle_cast/2` and `handle_call/3` callbacks
//! - `code_change/3` callback for hot code reloading
//! - `terminate/2` callback
//! - Message dispatch logic
//! - Method table generation
//!
//! Submodules organize the code by domain:
//! - [`spawn`] — Actor instantiation (`spawn/0`, `spawn/1`, error methods)
//! - [`callbacks`] — OTP `gen_server` callbacks (`init`, `handle_cast`, etc.)
//! - [`dispatch`] — Method routing and lookup
//! - [`state`] — State field initialization and inheritance
//! - [`methods`] — Method body code generation and class registration

mod callbacks;
mod dispatch;
mod methods;
mod spawn;
mod state;
