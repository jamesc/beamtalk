// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared library for the Beamtalk CLI.
//!
//! Contains code shared between the CLI binary and its library target:
//! - [`erlc`] — builder for `erlc` command invocations
//! - [`repl_startup`] — canonical BEAM node startup configuration
//! - [`manifest`] — `beamtalk.toml` parsing and validation
//! - [`build_layout`] — centralised `_build/` path construction
//! - [`dependency_classes`] — best-effort, offline dependency class
//!   resolution shared with `beamtalk-mcp`
//! - [`content_hash`] — the shared SHA-256 hex digest
//! - [`ffi_type_registry`] — ADR 0075's FFI type resolution chain (auto-
//!   extracted specs plus the stub tiers), shared by `beamtalk build`/`lint`
//!   and the MCP `lint`/`diagnostic_summary` tools (BT-4643)
//! - [`erlang_lint`] — text-based lint checks for native `.erl` files
//! - [`lint_pipeline`] — the `beamtalk lint` analysis pipeline, shared with
//!   the MCP `lint`/`diagnostic_summary` tools (BT-4585)
//! - [`native_type_specs`] — Erlang FFI type-spec extraction shared with
//!   `beamtalk-mcp`
//! - [`otp_support`] — OTP version support window (ADR 0125 §3.1), the
//!   single reader of `otp-support.toml`
//! - [`path_util`] — CLI path helpers (forward-slash rendering, symlink checks)
//! - [`pid_liveness`] — cross-platform "is this PID alive?" check, shared
//!   with `tests/cli_common`
//! - [`repl_meta_exprs`] — REPL meta-command → Beamtalk-expression builders,
//!   shared with `tests/repl_protocol.rs`

pub mod build_layout;
pub mod content_hash;
pub mod dependency_classes;
// Moved to `beamtalk-project` so the LSP applies the same rule (BT-4611).
pub use beamtalk_project::dependency_extensions;
pub mod erlang_lint;
pub mod erlc;
pub mod ffi_type_registry;
pub mod lint_pipeline;
pub mod manifest;
pub mod native_type_specs;
pub mod otp_support;
pub mod path_util;
pub mod pid_liveness;
pub mod repl_meta_exprs;
pub mod repl_startup;
