// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Code generation for Beamtalk.
//!
//! This module contains code generators for different target formats:
//! - **`core_erlang`**: Core Erlang code generation (BEAM bytecode target)
//!
//! Future modules may include:
//! - **`erlang`**: Erlang source code generation
//! - **`wasm`**: WebAssembly code generation

pub mod core_erlang;
