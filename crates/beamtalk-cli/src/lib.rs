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
//!   resolution shared with `beamtalk-mcp` (BT-2823)

pub mod build_layout;
pub mod dependency_classes;
pub mod erlc;
pub mod manifest;
pub mod repl_startup;
