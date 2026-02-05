// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Semantic analysis error types.

use crate::source_analysis::Span;
use ecow::EcoString;

/// A semantic error discovered during analysis.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemanticError {
    pub kind: SemanticErrorKind,
    pub span: Span,
}

/// Types of semantic errors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SemanticErrorKind {
    /// Variable referenced before definition.
    UndefinedVariable { name: EcoString },

    /// Variable assigned to but never read.
    UnusedVariable { name: EcoString },

    /// Captured variable mutated in block.
    MutatedCapture { name: EcoString },

    /// Block escapes but has unknown context.
    EscapingBlockUnknownContext,

    /// Multiple assignments to same immutable variable.
    MultipleAssignment { name: EcoString },

    /// Variable bound multiple times in the same pattern.
    DuplicatePatternVariable { name: EcoString, first_span: Span },
}

impl SemanticError {
    /// Create a new semantic error.
    #[must_use]
    pub fn new(kind: SemanticErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}
