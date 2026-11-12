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
//! - [`methods`] — Actor method body code generation and dispatch case clauses
//!
//! Class-method lowering is shared with value types and lives outside this
//! module, in `class_method_codegen`; Tier 2 value-call helpers live in
//! `blocks::tier2_value_calls`.

mod callbacks;
mod dispatch;
mod extensions;
mod methods;
mod native_facade;
mod spawn;
mod state;

pub(in crate::core_erlang) use methods::BodyExprKind;
pub(in crate::core_erlang) use methods::extract_package_from_module_name;
pub(in crate::core_erlang) use spawn::SpawnVariant;
