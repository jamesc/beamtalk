// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Code generation for Beamtalk.
//!
//! **DDD Context:** Code Generation (ADR 0117 step 5 — its own
//! crate, depending on `beamtalk-core`'s Compilation context, never the
//! reverse; see `docs/development/architecture-principles.md` §1)
//!
//! This crate contains code generators for different target formats:
//! - **`core_erlang`**: Core Erlang code generation (BEAM bytecode target)
//!
//! Future modules may include:
//! - **`erlang`**: Erlang source code generation
//! - **`wasm`**: WebAssembly code generation

pub mod core_erlang;

// Property-based tests for code generation (ADR 0011 Phase 2) live in
// `tests/codegen_property_tests.rs` (a Cargo integration test) —
// REPL-specific properties live in
// `beamtalk-repl/tests/codegen_property_tests.rs` (ADR 0117
// Decision step 4) — see that file's own doc for the full detail. What
// lives here exercises only this crate's own `generate_module` and could
// be a plain unit test, but stays a Cargo integration test for
// consistency with its sibling file.

// Property-based tests for Core Erlang output validity (ADR 0011 Phase 2)
#[cfg(test)]
mod core_erlang_validity_tests;
