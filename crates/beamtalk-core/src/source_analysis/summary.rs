// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Diagnostic summary counters shared by `beamtalk lint`, `beamtalk build`,
//! and the MCP `diagnostic_summary` tool (BT-2014).
//!
//! **DDD Context:** Source Analysis
//!
//! Provides [`DiagnosticSummary`] which aggregates a set of diagnostics into
//! per-category, per-severity counts, and [`SeverityCounts`] for the individual
//! tallies. The `Display` impl on `DiagnosticSummary` renders the human-readable
//! table shown at the end of `lint` and `build` runs.

use std::collections::BTreeMap;
use std::fmt;

use super::{Diagnostic, DiagnosticCategory, Severity};

/// Per-severity tallies for a single diagnostic category (or the total).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SeverityCounts {
    /// Number of `Severity::Error` diagnostics.
    pub error: usize,
    /// Number of `Severity::Warning` diagnostics.
    pub warning: usize,
    /// Number of `Severity::Lint` diagnostics.
    pub lint: usize,
    /// Number of `Severity::Hint` diagnostics.
    pub hint: usize,
}

impl SeverityCounts {
    /// Total count across all severities.
    #[must_use]
    pub fn total(&self) -> usize {
        self.error + self.warning + self.lint + self.hint
    }

    /// Add a single diagnostic at the given severity.
    fn add(&mut self, severity: Severity) {
        match severity {
            Severity::Error => self.error += 1,
            Severity::Warning => self.warning += 1,
            Severity::Lint => self.lint += 1,
            Severity::Hint => self.hint += 1,
        }
    }

    /// Merge another `SeverityCounts` into this one.
    fn merge(&mut self, other: &SeverityCounts) {
        self.error += other.error;
        self.warning += other.warning;
        self.lint += other.lint;
        self.hint += other.hint;
    }
}

/// Aggregated diagnostic counts grouped by [`DiagnosticCategory`] and
/// [`Severity`].
///
/// Built via [`DiagnosticSummary::from_diagnostics`] and rendered via its
/// `Display` impl or converted to JSON by the caller.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct DiagnosticSummary {
    /// Counts per diagnostic category.
    pub by_category: BTreeMap<DiagnosticCategory, SeverityCounts>,
    /// Counts for diagnostics without a category.
    pub uncategorised: SeverityCounts,
    /// Number of source files included in the analysis.
    pub files_checked: usize,
}

impl DiagnosticSummary {
    /// Build a summary from an iterator of diagnostics.
    ///
    /// `files_checked` is the number of files that were analysed.
    pub fn from_diagnostics<'a>(
        diagnostics: impl IntoIterator<Item = &'a Diagnostic>,
        files_checked: usize,
    ) -> Self {
        let mut summary = Self {
            by_category: BTreeMap::new(),
            uncategorised: SeverityCounts::default(),
            files_checked,
        };
        for diag in diagnostics {
            if let Some(cat) = diag.category {
                summary
                    .by_category
                    .entry(cat)
                    .or_default()
                    .add(diag.severity);
            } else {
                summary.uncategorised.add(diag.severity);
            }
        }
        summary
    }

    /// Merge another summary into this one (adds counts, takes the sum of
    /// `files_checked`).
    pub fn merge(&mut self, other: &DiagnosticSummary) {
        for (cat, counts) in &other.by_category {
            self.by_category.entry(*cat).or_default().merge(counts);
        }
        self.uncategorised.merge(&other.uncategorised);
        self.files_checked += other.files_checked;
    }

    /// Grand total across all categories and the uncategorised bucket.
    #[must_use]
    pub fn total(&self) -> usize {
        self.by_category
            .values()
            .map(SeverityCounts::total)
            .sum::<usize>()
            + self.uncategorised.total()
    }

    /// Grand total broken down by severity.
    #[must_use]
    pub fn totals_by_severity(&self) -> SeverityCounts {
        let mut totals = self.uncategorised.clone();
        for counts in self.by_category.values() {
            totals.merge(counts);
        }
        totals
    }

    /// Returns `true` if no diagnostics were recorded.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.total() == 0
    }
}

/// Human-readable name for a `DiagnosticCategory`.
fn category_label(cat: DiagnosticCategory) -> &'static str {
    match cat {
        DiagnosticCategory::Dnu => "Dnu",
        DiagnosticCategory::Type => "Type",
        DiagnosticCategory::Unused => "Unused",
        DiagnosticCategory::EmptyBody => "EmptyBody",
        DiagnosticCategory::Lint => "Lint",
        DiagnosticCategory::DeadAssignment => "DeadAssignment",
        DiagnosticCategory::ExtensionConflict => "ExtensionConflict",
        DiagnosticCategory::Deprecation => "Deprecation",
        DiagnosticCategory::ActorNew => "ActorNew",
        DiagnosticCategory::Visibility => "Visibility",
        DiagnosticCategory::UnresolvedClass => "UnresolvedClass",
        DiagnosticCategory::UnresolvedFfi => "UnresolvedFfi",
        DiagnosticCategory::ArityMismatch => "ArityMismatch",
        DiagnosticCategory::ShadowedClass => "ShadowedClass",
        DiagnosticCategory::TypeAnnotation => "TypeAnnotation",
    }
}

/// Return the category label — useful for JSON keys and text display.
#[must_use]
pub fn category_name(cat: DiagnosticCategory) -> &'static str {
    category_label(cat)
}

/// Format a severity count as the dominant severity label (e.g. "12 warning").
fn severity_label(counts: &SeverityCounts) -> String {
    // Show the most severe non-zero level.
    if counts.error > 0 {
        format!("{} error", counts.error)
    } else if counts.warning > 0 {
        format!("{} warning", counts.warning)
    } else if counts.lint > 0 {
        format!("{} lint", counts.lint)
    } else if counts.hint > 0 {
        format!("{} hint", counts.hint)
    } else {
        String::new()
    }
}

impl fmt::Display for DiagnosticSummary {
    /// Render the summary table shown at the end of `lint` / `build` runs.
    ///
    /// ```text
    /// Diagnostic summary (42 files):
    ///   Type              12 warning
    ///   Dnu                3 warning
    ///   Total             15  (1 error, 12 warning, 2 lint)
    /// ```
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let file_plural = if self.files_checked == 1 {
            "file"
        } else {
            "files"
        };
        writeln!(
            f,
            "Diagnostic summary ({} {file_plural}):",
            self.files_checked
        )?;

        // Print each category that has at least one diagnostic.
        for (cat, counts) in &self.by_category {
            let total = counts.total();
            if total == 0 {
                continue;
            }
            let label = category_label(*cat);
            let sev = severity_label(counts);
            write!(f, "  {label:<22}{sev}")?;
            // Annotate categories excluded from --warnings-as-errors.
            if matches!(
                cat,
                DiagnosticCategory::Deprecation
                    | DiagnosticCategory::UnresolvedClass
                    | DiagnosticCategory::UnresolvedFfi
                    | DiagnosticCategory::ArityMismatch
            ) {
                write!(f, "      (excluded from --warnings-as-errors)")?;
            }
            writeln!(f)?;
        }

        if self.uncategorised.total() > 0 {
            let sev = severity_label(&self.uncategorised);
            writeln!(f, "  {:<22}{sev}", "Other")?;
        }

        // Total line.
        let totals = self.totals_by_severity();
        let grand = self.total();
        write!(f, "  Total{grand:>15}")?;

        // Breakdown in parentheses.
        let mut parts = Vec::new();
        if totals.error > 0 {
            parts.push(format!("{} error", totals.error));
        }
        if totals.warning > 0 {
            parts.push(format!("{} warning", totals.warning));
        }
        if totals.lint > 0 {
            parts.push(format!("{} lint", totals.lint));
        }
        if totals.hint > 0 {
            parts.push(format!("{} hint", totals.hint));
        }
        if !parts.is_empty() {
            write!(f, "  ({})", parts.join(", "))?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::Span;

    fn make_diag(severity: Severity, category: Option<DiagnosticCategory>) -> Diagnostic {
        Diagnostic {
            severity,
            message: "test".into(),
            span: Span::new(0, 1),
            hint: None,
            category,
            notes: Vec::new(),
        }
    }

    #[test]
    fn empty_summary() {
        let summary = DiagnosticSummary::from_diagnostics(std::iter::empty(), 0);
        assert!(summary.is_empty());
        assert_eq!(summary.total(), 0);
        assert_eq!(summary.files_checked, 0);
    }

    #[test]
    fn counts_by_category_and_severity() {
        let diags = vec![
            make_diag(Severity::Warning, Some(DiagnosticCategory::Type)),
            make_diag(Severity::Warning, Some(DiagnosticCategory::Type)),
            make_diag(Severity::Error, Some(DiagnosticCategory::UnresolvedClass)),
            make_diag(Severity::Lint, Some(DiagnosticCategory::Lint)),
            make_diag(Severity::Hint, Some(DiagnosticCategory::Dnu)),
            make_diag(Severity::Warning, None), // uncategorised
        ];
        let summary = DiagnosticSummary::from_diagnostics(&diags, 3);
        assert_eq!(summary.files_checked, 3);
        assert_eq!(summary.total(), 6);

        // Type: 2 warning
        let type_counts = &summary.by_category[&DiagnosticCategory::Type];
        assert_eq!(type_counts.warning, 2);
        assert_eq!(type_counts.error, 0);

        // UnresolvedClass: 1 error
        let uc = &summary.by_category[&DiagnosticCategory::UnresolvedClass];
        assert_eq!(uc.error, 1);

        // Uncategorised: 1 warning
        assert_eq!(summary.uncategorised.warning, 1);

        // Totals by severity
        let totals = summary.totals_by_severity();
        assert_eq!(totals.error, 1);
        assert_eq!(totals.warning, 3);
        assert_eq!(totals.lint, 1);
        assert_eq!(totals.hint, 1);
    }

    #[test]
    fn merge_summaries() {
        let d1 = vec![make_diag(Severity::Warning, Some(DiagnosticCategory::Type))];
        let d2 = vec![
            make_diag(Severity::Warning, Some(DiagnosticCategory::Type)),
            make_diag(Severity::Error, Some(DiagnosticCategory::Dnu)),
        ];
        let mut s1 = DiagnosticSummary::from_diagnostics(&d1, 1);
        let s2 = DiagnosticSummary::from_diagnostics(&d2, 2);
        s1.merge(&s2);
        assert_eq!(s1.files_checked, 3);
        assert_eq!(s1.total(), 3);
        assert_eq!(s1.by_category[&DiagnosticCategory::Type].warning, 2);
        assert_eq!(s1.by_category[&DiagnosticCategory::Dnu].error, 1);
    }

    #[test]
    fn display_format() {
        let diags = vec![
            make_diag(Severity::Warning, Some(DiagnosticCategory::Type)),
            make_diag(Severity::Lint, Some(DiagnosticCategory::Lint)),
        ];
        let summary = DiagnosticSummary::from_diagnostics(&diags, 5);
        let text = summary.to_string();
        assert!(text.contains("Diagnostic summary (5 files):"));
        assert!(text.contains("Type"));
        assert!(text.contains("1 warning"));
        assert!(text.contains("Lint"));
        assert!(text.contains("1 lint"));
        assert!(text.contains("Total"));
    }
}
