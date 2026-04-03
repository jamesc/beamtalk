// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Code generation commands (ADR 0075 Phase 3).
//!
//! **DDD Context:** Compilation
//!
//! This module groups code-generation subcommands under `beamtalk generate`:
//! - `native` — generate Erlang `gen_server` skeletons from `native:` Actor classes
//! - `stubs` — generate Beamtalk stub definitions from FFI spec files (placeholder)

pub mod cli;
pub mod native;
pub mod stubs;
