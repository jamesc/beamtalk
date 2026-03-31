// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared library for the Beamtalk CLI.
//!
//! Contains code shared between the CLI binary and its library target:
//! - [`erlc`] — builder for `erlc` command invocations
//! - [`repl_startup`] — canonical BEAM node startup configuration

pub mod erlc;
pub mod repl_startup;
