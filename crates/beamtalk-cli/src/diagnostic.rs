// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Beautiful error diagnostics using miette.
//!
//! Converts beamtalk-core diagnostics into miette-formatted errors with:
//! - Source code context with syntax highlighting
//! - Arrows pointing to the error location
//! - Diagnostic codes for easy reference
//! - Support for multiple errors and warnings

#![allow(unused)]

use beamtalk_core::parse::{Diagnostic as CoreDiagnostic, Severity};
use miette::{Diagnostic, SourceSpan};

/// A compilation diagnostic with rich formatting.
#[derive(Debug, Diagnostic, thiserror::Error)]
#[error("{message}")]
#[diagnostic(code(beamtalk::compile))]
pub struct CompileDiagnostic {
    /// Error or warning
    pub severity: Severity,
    /// Human-readable error message
    pub message: String,
    /// Source code for context
    #[source_code]
    pub src: miette::NamedSource<String>,
    /// Location of the error
    #[label("{label}")]
    pub span: SourceSpan,
    /// Label for the error span
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
                (diagnostic.span.end() - diagnostic.span.start()) as usize,
            )
                .into(),
            label: label.to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_core::parse::Span;

    #[test]
    fn test_from_core_diagnostic() {
        let core_diag = CoreDiagnostic::error("Expected expression", Span::new(10, 15));
        let source = "test := [1 + ].";
        let diag = CompileDiagnostic::from_core_diagnostic(&core_diag, "test.bt", source);

        assert_eq!(diag.severity, Severity::Error);
        assert_eq!(diag.message, "Expected expression");
        assert_eq!(diag.span.offset(), 10);
        assert_eq!(diag.span.len(), 5);
    }
}
