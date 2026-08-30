// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Code generation for Beamtalk.
//!
//! **DDD Context:** Code Generation
//!
//! This module contains code generators for different target formats:
//! - **`core_erlang`**: Core Erlang code generation (BEAM bytecode target)
//!
//! Future modules may include:
//! - **`erlang`**: Erlang source code generation
//! - **`wasm`**: WebAssembly code generation

pub mod core_erlang;

// Property-based tests for code generation (ADR 0011 Phase 2). BT-3340: moved
// to `tests/codegen_property_tests.rs` (a Cargo integration test) because it
// exercises `beamtalk-repl`'s `generate_repl_expression` — `beamtalk-repl`
// depends on this crate, so calling it from a unit test embedded in this
// crate's own `src/` (compiled with `--cfg test` as part of this crate
// itself) would need two different-cfg copies of `beamtalk-core` in the same
// build graph, which Cargo rejects. An integration test links this crate
// normally (no `--cfg test` on the library itself), matching the copy
// `beamtalk-repl` already depends on — see that file's own doc for detail.

// Property-based tests for Core Erlang output validity (ADR 0011 Phase 2)
#[cfg(test)]
mod core_erlang_validity_tests;
