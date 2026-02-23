// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Semantic analysis error types.

use crate::source_analysis::Span;
use ecow::EcoString;

/// A semantic error discovered during analysis.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemanticError {
    /// The category of semantic error.
    pub kind: SemanticErrorKind,
    /// Source location where the error was detected.
    pub span: Span,
}

/// Types of semantic errors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SemanticErrorKind {
    /// Variable referenced before definition.
    UndefinedVariable {
        /// The undefined variable name.
        name: EcoString,
    },

    /// Variable assigned to but never read.
    UnusedVariable {
        /// The unused variable name.
        name: EcoString,
    },

    /// Captured variable mutated in block.
    MutatedCapture {
        /// The captured variable name that was mutated.
        name: EcoString,
    },

    /// Block escapes but has unknown context.
    EscapingBlockUnknownContext,

    /// Multiple assignments to same immutable variable.
    MultipleAssignment {
        /// The variable name that was assigned more than once.
        name: EcoString,
    },

    /// Variable bound multiple times in the same pattern.
    DuplicatePatternVariable {
        /// The duplicated variable name.
        name: EcoString,
        /// Source location of the first binding of this variable.
        first_span: Span,
    },

    /// Method expects a symbol literal argument but received something else.
    ///
    /// Reflection methods like `respondsTo:`, `fieldAt:`, and `classNamed:`
    /// require symbol literal arguments (e.g., `#increment`).
    ExpectedSymbolLiteral {
        /// The method selector that expects a symbol.
        method: EcoString,
        /// The identifier name if the user passed a bare identifier.
        found_identifier: Option<EcoString>,
    },
}

impl SemanticError {
    /// Create a new semantic error.
    #[must_use]
    pub fn new(kind: SemanticErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}
