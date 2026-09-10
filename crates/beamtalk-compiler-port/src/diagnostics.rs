// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Diagnostic filtering and partitioning.
//!
//! Splits a checker's flat `Diagnostic` list into the error/warning
//! subsets each response shape needs, and the structured [`DiagInfo`]
//! wire representation `respond`'s richer diagnostic responses build
//! from.

// ---------------------------------------------------------------------------
// Diagnostic filtering helpers
// ---------------------------------------------------------------------------

/// Collect error-severity diagnostics as references for `diagnostic_error_response`.
pub(crate) fn filter_error_diagnostics(
    diagnostics: &[beamtalk_core::source_analysis::Diagnostic],
) -> Vec<&beamtalk_core::source_analysis::Diagnostic> {
    diagnostics
        .iter()
        .filter(|d| matches!(d.severity, beamtalk_core::source_analysis::Severity::Error))
        .collect()
}

/// Collect warning/hint/lint messages as `Vec<String>` for response construction.
///
/// Includes `Lint` severity so REPL users see effect-free statement hints.
pub(crate) fn collect_warning_messages(
    diagnostics: &[beamtalk_core::source_analysis::Diagnostic],
) -> Vec<String> {
    diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.severity,
                beamtalk_core::source_analysis::Severity::Warning
                    | beamtalk_core::source_analysis::Severity::Hint
                    | beamtalk_core::source_analysis::Severity::Lint
            )
        })
        .map(|d| d.message.to_string())
        .collect()
}

/// Structured diagnostic info returned in compilation responses.
pub(crate) struct DiagInfo {
    /// Human-readable diagnostic message.
    pub(crate) message: String,
    /// Severity level (`"error"`, `"warning"`, `"lint"`, or `"hint"`).
    pub(crate) severity: String,
    /// Diagnostic category (`"Dnu"`, `"Type"`, ...), when the checker tagged
    /// one — `None` for parse errors and other untagged diagnostics
    /// (ADR 0105 Phase 1: the re-check orchestration filters
    /// findings by category).
    pub(crate) category: Option<String>,
    /// Byte offset where the diagnosed span begins.
    pub(crate) start: u32,
    /// Byte offset where the diagnosed span ends.
    pub(crate) end: u32,
}

/// Render a `Diagnostic`'s category as the same `PascalCase` label
/// `beamtalk lint` / `beamtalk-mcp` use (`category_name`), or `None` when
/// the diagnostic carries no category.
pub(crate) fn diag_category(d: &beamtalk_core::source_analysis::Diagnostic) -> Option<String> {
    d.category
        .map(|c| beamtalk_core::source_analysis::category_name(c).to_string())
}

/// Separate diagnostics into errors and warnings, returning structured info.
pub(crate) fn partition_diagnostics(
    diagnostics: &[beamtalk_core::source_analysis::Diagnostic],
) -> (Vec<DiagInfo>, Vec<DiagInfo>) {
    let errors = diagnostics
        .iter()
        .filter(|d| matches!(d.severity, beamtalk_core::source_analysis::Severity::Error))
        .map(|d| DiagInfo {
            message: d.message.to_string(),
            severity: "error".to_string(),
            category: diag_category(d),
            start: d.span.start(),
            end: d.span.end(),
        })
        .collect();
    let warnings = diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.severity,
                beamtalk_core::source_analysis::Severity::Warning
                    | beamtalk_core::source_analysis::Severity::Hint
            )
        })
        .map(|d| DiagInfo {
            message: d.message.to_string(),
            severity: match d.severity {
                beamtalk_core::source_analysis::Severity::Hint => "hint".to_string(),
                _ => "warning".to_string(),
            },
            category: diag_category(d),
            start: d.span.start(),
            end: d.span.end(),
        })
        .collect();
    (errors, warnings)
}
