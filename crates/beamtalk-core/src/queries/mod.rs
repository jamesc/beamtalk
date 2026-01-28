// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Query modules for language service operations.
//!
//! This module provides specialized query implementations for different
//! language service features. Each submodule focuses on a specific capability:
//!
//! - [`completions`] - Code completion suggestions
//! - [`diagnostics`] - Error and warning reporting
//! - [`hover`] - Hover information and documentation
//!
//! These modules are designed to be extensible and can be enhanced with
//! semantic analysis, type information, and cross-file references in the future.

pub mod completions;
pub mod diagnostics;
pub mod hover;
