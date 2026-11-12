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
//! - [`compiler_port`] — `beamtalk-compiler-port` binary lookup (BT-4674)
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

pub use beamtalk_package::build_layout;
pub mod compiler_port;
pub use beamtalk_package::content_hash;
pub use beamtalk_package::dependency_classes;
// Moved to `beamtalk-project` so the LSP applies the same rule (BT-4611).
pub use beamtalk_package::erlang_lint;
pub use beamtalk_project::dependency_extensions;
pub mod erlc;
pub use beamtalk_package::ffi_type_registry;
pub use beamtalk_package::lint_pipeline;
pub use beamtalk_package::manifest;
pub use beamtalk_package::native_type_specs;
pub mod otp_support;
pub mod path_util;
pub mod pid_liveness;
pub mod repl_meta_exprs;
pub mod repl_startup;
