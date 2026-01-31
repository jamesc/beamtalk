// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Expression code generation.
//!
//! This module handles code generation for Beamtalk expressions:
//! - Literals (integers, floats, strings, symbols)
//! - Identifiers and variable references
//! - Map literals
//! - Field access (`self.field`)
//! - Field assignment (`self.field := value`)
//! - Blocks (closures)
//! - Message sends (unary, binary, keyword)
//! - Await expressions
//! - Cascades

// NOTE: Temporarily disabled during refactoring
// use super::{CoreErlangGenerator, Result};

// Methods will be moved here from erlang.rs
