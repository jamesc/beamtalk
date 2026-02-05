// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Beautiful error diagnostics using miette.
//!
//! Converts beamtalk-core diagnostics into miette-formatted errors with:
//! - Source code context with syntax highlighting
//! - Arrows pointing to the error location
//! - Diagnostic codes for easy reference
//! - Support for multiple errors and warnings

// Suppress unused_assignments for struct fields used by derive macros
#![allow(unused_assignments)]

use beamtalk_core::source_analysis::{Diagnostic as CoreDiagnostic, Severity};
use miette::{Diagnostic, SourceSpan};

/// A compilation diagnostic with rich formatting.
#[derive(Debug, Diagnostic, thiserror::Error)]
#[error("{message}")]
#[diagnostic(code(beamtalk::compile))]
pub struct CompileDiagnostic {
    /// Error or warning (stored for potential future use)
    pub severity: Severity,
    /// Human-readable error message
    pub message: String,
    /// Source code for context
    #[source_code]
    pub src: miette::NamedSource<String>,
    /// Location of the error
    #[label("{label}")]
    pub span: SourceSpan,
    /// Label for the error span (interpolated by miette derive macro)
    pub label: String,
}

impl CompileDiagnostic {
    /// Create a new diagnostic from a beamtalk-core diagnostic.
    pub fn from_core_diagnostic(
        diagnostic: &CoreDiagnostic,
        source_path: &str,
        source: &str,
    ) -> Self {
        let label = match diagnostic.severity {
            Severity::Error => "error here",
            Severity::Warning => "warning here",
        };

        Self {
            severity: diagnostic.severity,
            message: diagnostic.message.to_string(),
            src: miette::NamedSource::new(source_path, source.to_string()),
            span: (
                diagnostic.span.start() as usize,
                diagnostic.span.len() as usize,
            )
                .into(),
            label: label.to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_core::source_analysis::Span;

    #[test]
    fn test_from_core_diagnostic_error() {
        let core_diag = CoreDiagnostic::error("Expected expression", Span::new(10, 15));
        let source = "test := [1 + ].";
        let diag = CompileDiagnostic::from_core_diagnostic(&core_diag, "test.bt", source);

        assert_eq!(diag.severity, Severity::Error);
        assert_eq!(diag.message, "Expected expression");
        assert_eq!(diag.span.offset(), 10);
        assert_eq!(diag.span.len(), 5);
        assert_eq!(diag.label, "error here");
    }

    #[test]
    fn test_from_core_diagnostic_warning() {
        let core_diag = CoreDiagnostic::warning("Unused variable", Span::new(5, 8));
        let source = "test := 42";
        let diag = CompileDiagnostic::from_core_diagnostic(&core_diag, "test.bt", source);

        assert_eq!(diag.severity, Severity::Warning);
        assert_eq!(diag.message, "Unused variable");
        assert_eq!(diag.span.offset(), 5);
        assert_eq!(diag.span.len(), 3);
        assert_eq!(diag.label, "warning here");
    }

    #[test]
    fn test_from_core_diagnostic_zero_length_span() {
        let core_diag = CoreDiagnostic::error("Unexpected EOF", Span::new(10, 10));
        let source = "test := [1";
        let diag = CompileDiagnostic::from_core_diagnostic(&core_diag, "test.bt", source);

        assert_eq!(diag.span.offset(), 10);
        assert_eq!(diag.span.len(), 0);
    }

    #[test]
    fn test_from_core_diagnostic_preserves_message() {
        let message = "Custom error message with details";
        let core_diag = CoreDiagnostic::error(message, Span::new(0, 5));
        let source = "test := 42";
        let diag = CompileDiagnostic::from_core_diagnostic(&core_diag, "test.bt", source);

        assert_eq!(diag.message, message);
    }
}
