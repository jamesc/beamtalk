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
    /// Optional help text (e.g., "Use --allow-primitives flag...")
    #[help]
    pub help: Option<String>,
}

impl CompileDiagnostic {
    /// Create a new diagnostic from a beamtalk-core diagnostic.
    ///
    /// When the core diagnostic has notes (BT-1588), they are appended to the
    /// message with line references for origin tracing.
    pub fn from_core_diagnostic(
        diagnostic: &CoreDiagnostic,
        source_path: &str,
        source: &str,
    ) -> Self {
        let label = match diagnostic.severity {
            Severity::Error => "error here",
            Severity::Warning => "warning here",
            Severity::Lint => "lint here",
            Severity::Hint => "hint here",
        };

        // BT-1588: Append notes to the message for origin tracing.
        // Notes include context like "variable has type V because it came from
        // `Dictionary at:ifAbsent:` at line 42".
        let message = if diagnostic.notes.is_empty() {
            diagnostic.message.to_string()
        } else {
            use std::fmt::Write;
            let mut msg = diagnostic.message.to_string();
            for note in &diagnostic.notes {
                if let Some(note_span) = note.span {
                    let offset = (note_span.start() as usize).min(source.len());
                    let line = source[..offset].chars().filter(|c| *c == '\n').count() + 1;
                    let _ = write!(msg, "\n  = {} (line {})", note.message, line);
                } else {
                    let _ = write!(msg, "\n  = {}", note.message);
                }
            }
            msg
        };

        Self {
            severity: diagnostic.severity,
            message,
            src: miette::NamedSource::new(source_path, source.to_string()),
            span: (
                diagnostic.span.start() as usize,
                diagnostic.span.len() as usize,
            )
                .into(),
            label: label.to_string(),
            help: diagnostic.hint.as_ref().map(ToString::to_string),
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
    fn test_from_core_diagnostic_lint() {
        let core_diag = CoreDiagnostic::lint("Trailing caret is redundant", Span::new(5, 10));
        let source = "foo => ^bar";
        let diag = CompileDiagnostic::from_core_diagnostic(&core_diag, "test.bt", source);

        assert_eq!(diag.severity, Severity::Lint);
        assert_eq!(diag.label, "lint here");
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

    #[test]
    fn test_from_core_diagnostic_with_notes() {
        // BT-1588: Notes should be appended to the message with line references
        let source =
            "line1\nline2\nval := dict at: key ifAbsent: [\"default\"]\nval ++ \" suffix\"";
        let core_diag = CoreDiagnostic::warning(
            "`++` on String expects a String argument, got V",
            Span::new(50, 65),
        )
        .with_note(
            "note: `Dictionary at:ifAbsent:` returns generic type `V`",
            Some(Span::new(12, 48)),
        );

        let diag = CompileDiagnostic::from_core_diagnostic(&core_diag, "test.bt", source);
        assert!(
            diag.message.contains("Dictionary at:ifAbsent:"),
            "Message should include the origin note"
        );
        assert!(
            diag.message.contains("line 3"),
            "Message should include line number of origin, got: {}",
            diag.message
        );
    }

    #[test]
    fn test_from_core_diagnostic_no_notes() {
        // Diagnostics without notes should not have extra text
        let core_diag = CoreDiagnostic::warning("simple warning", Span::new(0, 5));
        let source = "test := 42";
        let diag = CompileDiagnostic::from_core_diagnostic(&core_diag, "test.bt", source);
        assert_eq!(diag.message, "simple warning");
    }
}
