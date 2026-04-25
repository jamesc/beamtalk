// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Surface drivers.
//!
//! Each driver exposes the same minimal contract: given the input string from
//! a parity step, return a `SurfaceOutput` whose fields the harness can
//! compare against the step's expectation.

pub mod cli;
pub mod lsp;
pub mod mcp;
pub mod repl;

use std::collections::BTreeSet;

/// What every surface driver returns from a single step.
///
/// Fields are populated only when the surface produced that kind of result.
/// `value` is the canonical text answer, `classes` is filled by load-style
/// operations, and `diagnostic_count` is filled by lint/check operations.
#[derive(Debug, Default)]
pub struct SurfaceOutput {
    /// Normalized textual value (after [`crate::normalize::value`]).
    pub value: Option<String>,
    /// Class names observed (for load-project parity).
    pub classes: Option<BTreeSet<String>>,
    /// Diagnostic count reported by the surface.
    pub diagnostic_count: Option<usize>,
    /// Raw response, kept for failure messages.
    pub raw: String,
}
