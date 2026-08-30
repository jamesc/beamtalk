// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! REPL code generation boundary (BT-1462).
//!
//! **DDD Context:** REPL
//!
//! This module owns REPL-specific code generation concerns:
//! - Workspace binding threading (`State` / `Bindings` maps)
//! - Trace mode wrapping (per-expression source-keyed results)
//! - Multi-expression REPL module assembly
//! - Test module generation (no workspace bindings)
//!
//! It delegates expression compilation and state threading to the
//! core codegen through [`CoreErlangGenerator`]'s public API, keeping
//! the Compilation context domain-agnostic.
//!
//! [`CoreErlangGenerator`]: beamtalk_core::codegen::core_erlang::CoreErlangGenerator
//!
//! BT-3340 (ADR 0117 Decision step 2): extracted from `beamtalk-core::repl`
//! into its own crate — `repl` depended only on `codegen`/`ast`/
//! `source_analysis` in production (its one back-edge, `codegen`'s own test
//! files calling `repl::codegen` to validate REPL-specific codegen paths, is
//! test-only, so `beamtalk-core` keeps `beamtalk-repl` as a dev-dependency).
//! Reaching into [`CoreErlangGenerator`]'s previously crate-private state
//! (scope stack, state-threading counters, REPL/workspace-mode flags) is
//! exactly what a REPL-specific codegen boundary is expected to need, so
//! those items widened from `pub(crate)` to `pub` rather than growing a new
//! abstraction layer — see the `pub(crate)` → `pub` comments at each site in
//! `beamtalk-core::codegen::core_erlang`.

pub mod codegen;
