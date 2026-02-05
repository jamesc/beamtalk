// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type checking for semantic analysis.
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module implements the `TypeChecker` domain service from the DDD model.
//! The `TypeChecker` is responsible for:
//! - Validating type constraints on bindings
//! - Performing type inference (bottom-up through AST)
//! - Checking message send type compatibility
//! - Emitting type-related diagnostics
//!
//! **Status:** Stub implementation - returns no diagnostics
//!
//! This is a placeholder for future gradual typing support. Currently,
//! Beamtalk is dynamically typed (like Smalltalk), but type annotations
//! and optional static checking are planned future features.
//!
//! **References:**
//! - `docs/beamtalk-ddd-model.md` - Semantic Analysis Context
//! - Future Epic: BT-XXX - Gradual Typing

use crate::ast::Module;
use crate::source_analysis::Diagnostic;

/// Type checking domain service.
///
/// **DDD Context:** Semantic Analysis - Domain Service
///
/// Validates type constraints and performs type inference. This is a
/// stateless service that operates on the AST and Scope.
///
/// **Current status:** Stub implementation that returns no diagnostics.
/// Future implementation will support gradual typing with optional
/// type annotations.
#[derive(Debug)]
pub struct TypeChecker {
    diagnostics: Vec<Diagnostic>,
}

impl TypeChecker {
    /// Creates a new type checker.
    #[must_use]
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
        }
    }

    /// Checks types in a module.
    ///
    /// **Current status:** Stub - does nothing and returns empty diagnostics.
    pub fn check_module(&mut self, _module: &Module) {
        // TODO: Implement type checking
        // - Walk AST and infer types bottom-up
        // - Validate type annotations on bindings
        // - Check message send compatibility
        // - Emit type mismatch diagnostics
    }

    /// Returns all diagnostics collected during type checking.
    #[must_use]
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    /// Takes ownership of diagnostics, leaving an empty vec.
    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        std::mem::take(&mut self.diagnostics)
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::Span;

    #[test]
    fn test_stub_returns_no_diagnostics() {
        let module = Module::new(vec![], Span::default());
        let mut checker = TypeChecker::new();
        checker.check_module(&module);
        assert!(checker.diagnostics().is_empty());
    }
}
