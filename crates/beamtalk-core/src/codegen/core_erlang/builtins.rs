// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Built-in operation code generation.
//!
//! This module handles code generation for built-in operations like:
//! - Block evaluation (`value`, `whileTrue:`, `whileFalse:`, `repeat`)
//! - Dictionary operations (`at:`, `at:put:`, etc.)
//! - Boolean operations (`ifTrue:`, `ifFalse:`, etc.)
//! - Integer arithmetic (`+`, `-`, `*`, `/`, etc.)
//! - String operations (`++`, `size`, etc.)
//! - Binary operators with standard math precedence

// NOTE: Temporarily disabled during refactoring
// use super::{CoreErlangGenerator, Result};

// Methods will be moved here from erlang.rs
