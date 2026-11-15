// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared library for the Beamtalk CLI.
//!
//! Contains only CLI-owned code shared between the CLI binary and its
//! integration tests / library target. Modules that live in other crates
//! (`build_layout`, `manifest`, `lint_pipeline`, `pid_liveness`, …) are
//! imported from their owning crate directly — there is no re-export layer.
//!
//! - [`erlc`] — builder for `erlc` command invocations
//! - [`repl_startup`] — canonical BEAM node startup configuration
//! - [`compiler_port`] — `beamtalk-compiler-port` binary lookup (BT-4674)
//! - [`otp_support`] — OTP version support window (ADR 0125 §3.1), the
//!   single reader of `otp-support.toml`
//! - [`path_util`] — CLI path helpers (forward-slash rendering, symlink checks)
//! - [`repl_meta_exprs`] — REPL meta-command → Beamtalk-expression builders,
//!   shared with `tests/repl_protocol.rs`

pub mod compiler_port;
pub mod erlc;
pub mod otp_support;
pub mod path_util;
pub mod repl_meta_exprs;
pub mod repl_startup;
