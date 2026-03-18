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

pub mod codegen;
