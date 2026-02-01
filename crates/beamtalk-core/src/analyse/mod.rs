// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Semantic analysis for Beamtalk.
//!
//! This module performs semantic analysis on the AST, including:
//! - Variable scope and lifetime analysis
//! - Block context determination (control flow, stored, passed)
//! - Capture analysis for blocks
//! - Mutation tracking for `RefCell` generation
//!
//! The analysis produces diagnostics and metadata used by the code generator.

use crate::ast::Module;
use crate::parse::{Diagnostic, Span};
use std::collections::HashMap;

pub mod error;

pub use error::{SemanticError, SemanticErrorKind};

/// Result of semantic analysis.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnalysisResult {
    /// Diagnostics (errors and warnings) from analysis.
    pub diagnostics: Vec<Diagnostic>,

    /// Block metadata indexed by block span.
    pub block_info: HashMap<Span, BlockInfo>,
}

impl AnalysisResult {
    /// Create a new empty analysis result.
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
            block_info: HashMap::new(),
        }
    }
}

impl Default for AnalysisResult {
    fn default() -> Self {
        Self::new()
    }
}

/// Information about a block expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockInfo {
    /// Context in which the block is used.
    pub context: BlockContext,

    /// Variables captured from outer scopes.
    pub captures: Vec<CapturedVar>,

    /// Mutations that occur within the block.
    pub mutations: Vec<Mutation>,
}

/// Context in which a block is used.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BlockContext {
    /// Block used as control flow (if/while condition).
    ControlFlow,

    /// Block stored in a variable or field.
    Stored,

    /// Block passed as argument to a message send.
    Passed,

    /// Other known context (e.g., immediate evaluation).
    Other,

    /// Context could not be determined.
    Unknown,
}

/// A variable captured from an outer scope.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CapturedVar {
    /// Name of the captured variable.
    pub name: String,

    /// Span where the variable was defined.
    pub defined_at: Span,
}

/// A mutation that occurs within a block or method.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Mutation {
    /// Kind of mutation.
    pub kind: MutationKind,

    /// Span of the mutation.
    pub span: Span,
}

/// Type of mutation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MutationKind {
    /// Assignment to a local variable.
    LocalVariable { name: String },

    /// Assignment to a captured variable (requires `RefCell`).
    CapturedVariable { name: String },

    /// Assignment to an object field.
    Field { name: String },
}

/// Perform semantic analysis on a module.
///
/// This is the main entry point for semantic analysis. It analyzes the module
/// AST and returns diagnostics and metadata for code generation.
///
/// # Examples
///
/// ```
/// # use beamtalk_core::analyse::analyse;
/// # use beamtalk_core::ast::Module;
/// # use beamtalk_core::parse::Span;
/// let module = Module::new(vec![], Span::default());
/// let result = analyse(&module);
/// assert_eq!(result.diagnostics.len(), 0);
/// ```
pub fn analyse(_module: &Module) -> AnalysisResult {
    // TODO: Implement semantic analysis
    // For now, return an empty result
    AnalysisResult::new()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_analyse_empty_module() {
        let module = Module::new(vec![], Span::default());
        let result = analyse(&module);

        assert_eq!(result.diagnostics.len(), 0);
        assert_eq!(result.block_info.len(), 0);
    }

    #[test]
    fn test_analysis_result_default() {
        let result = AnalysisResult::default();
        assert_eq!(result.diagnostics.len(), 0);
        assert_eq!(result.block_info.len(), 0);
    }

    #[test]
    fn test_block_context_values() {
        // Ensure all variants are constructible
        let contexts = [
            BlockContext::ControlFlow,
            BlockContext::Stored,
            BlockContext::Passed,
            BlockContext::Other,
            BlockContext::Unknown,
        ];

        assert_eq!(contexts.len(), 5);
    }

    #[test]
    fn test_semantic_error_creation() {
        let error = SemanticError::new(
            SemanticErrorKind::UndefinedVariable {
                name: "foo".to_string(),
            },
            Span::default(),
        );

        assert!(matches!(
            error.kind,
            SemanticErrorKind::UndefinedVariable { .. }
        ));
    }
}
