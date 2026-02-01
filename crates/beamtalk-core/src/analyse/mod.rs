// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Semantic analysis for Beamtalk.
//!
//! This module performs semantic analysis on the AST, including:
//! - Variable scope and lifetime analysis (via `scope` module)
//! - Block context determination (control flow, stored, passed)
//! - Capture analysis for blocks
//! - Mutation tracking for captured variables in blocks
//!
//! The analysis produces diagnostics and metadata used by the code generator.

use crate::ast::Module;
use crate::parse::{Diagnostic, Span};
use ecow::EcoString;
use std::collections::HashMap;

pub mod error;
pub mod scope;

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
    #[must_use]
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
    pub name: EcoString,

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
    LocalVariable { name: EcoString },

    /// Assignment to a captured variable.
    CapturedVariable { name: EcoString },

    /// Assignment to an object field.
    Field { name: EcoString },
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
            SemanticErrorKind::UndefinedVariable { name: "foo".into() },
            Span::default(),
        );

        assert!(matches!(
            error.kind,
            SemanticErrorKind::UndefinedVariable { .. }
        ));
    }

    #[test]
    fn test_block_info_construction() {
        let block_info = BlockInfo {
            context: BlockContext::ControlFlow,
            captures: vec![CapturedVar {
                name: "count".into(),
                defined_at: Span::default(),
            }],
            mutations: vec![Mutation {
                kind: MutationKind::LocalVariable { name: "x".into() },
                span: Span::default(),
            }],
        };

        assert_eq!(block_info.context, BlockContext::ControlFlow);
        assert_eq!(block_info.captures.len(), 1);
        assert_eq!(block_info.mutations.len(), 1);
    }

    #[test]
    fn test_captured_var_construction() {
        let captured = CapturedVar {
            name: "myVar".into(),
            defined_at: Span::default(),
        };

        assert_eq!(captured.name, "myVar");
    }

    #[test]
    fn test_mutation_kinds() {
        let local = Mutation {
            kind: MutationKind::LocalVariable { name: "x".into() },
            span: Span::default(),
        };

        let captured = Mutation {
            kind: MutationKind::CapturedVariable {
                name: "count".into(),
            },
            span: Span::default(),
        };

        let field = Mutation {
            kind: MutationKind::Field { name: "sum".into() },
            span: Span::default(),
        };

        assert!(matches!(local.kind, MutationKind::LocalVariable { .. }));
        assert!(matches!(
            captured.kind,
            MutationKind::CapturedVariable { .. }
        ));
        assert!(matches!(field.kind, MutationKind::Field { .. }));
    }
}
