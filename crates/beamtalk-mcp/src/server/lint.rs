// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Offline lint/diagnostic-summary analysis for the `lint` and
//! `diagnostic_summary` MCP tools (`server::tools::diagnostics`).
//!
//! The analysis is CLI `beamtalk lint`'s own pipeline,
//! [`beamtalk_cli::lint_pipeline`] (BT-4585): the same Pass 1 extraction,
//! dependency merge, knowledge scope, per-file analysis, alias-collision
//! dedup, provision routing, `.erl` linting and verdict gate. This module
//! supplies MCP's offline inputs — dependency metadata from the checkouts
//! already on disk ([`beamtalk_cli::dependency_classes`]), never fetched —
//! and shapes the results for the tools. No REPL connection is required,
//! which is what lets both tools work without a live workspace.

use beamtalk_cli::ffi_type_registry::{ResolvedTypeRegistry, StubReport};
use beamtalk_cli::lint_pipeline::{self, DependencyMetadata, LintGate, PackageSources};
use beamtalk_core::source_analysis::Severity;
use camino::Utf8PathBuf;

/// A single lint diagnostic in structured form.
///
/// `line` is `None` for file-level errors (e.g. unreadable path, non-`.bt` file)
/// where there is no specific source location.  For diagnostics derived from
/// source text it is a 1-indexed line number.
#[derive(Debug, serde::Serialize)]
pub(crate) struct LintDiagnostic {
    pub(crate) file: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) line: Option<u32>,
    pub(crate) message: String,
    pub(crate) severity: &'static str,
}

/// Structured result returned by the `lint` MCP tool.
#[derive(Debug, serde::Serialize)]
pub(crate) struct LintResult {
    pub(crate) warnings: Vec<LintDiagnostic>,
    pub(crate) errors: Vec<LintDiagnostic>,
    pub(crate) total: usize,
    /// Lint-severity diagnostics, `.erl` lint findings included: the set
    /// plain CLI `beamtalk lint` fails on.
    pub(crate) lint_failures: usize,
    /// Diagnostics a REPL `load-project` rejects — any `Error` plus the
    /// non-error `Dnu`/`Type` findings — classified by the shared
    /// [`LintGate`] CLI `beamtalk lint --warnings-as-errors` uses (BT-4336).
    /// File-level read errors are not counted here; they are already in
    /// `errors`.
    pub(crate) live_recheck_failures: usize,
}

impl LintResult {
    /// Whether the `lint` tool reports failure (`is_error`). Without
    /// `warnings_as_errors` only `errors` fail it; with it, it applies CLI
    /// `beamtalk lint --warnings-as-errors`' gate ([`LintGate::fails`]) —
    /// Lint-severity diagnostics or the live-recheck set — plus file-level
    /// errors (BT-4336). Both surfaces run [`lint_pipeline`] (BT-4585)
    /// against the same FFI type registry (BT-4643), so the diagnostics
    /// gated match the CLI's.
    pub(crate) fn fails(&self, warnings_as_errors: bool) -> bool {
        let gate = LintGate {
            lint: self.lint_failures,
            live_recheck: self.live_recheck_failures,
        };
        !self.errors.is_empty() || (warnings_as_errors && gate.fails(true))
    }
}

/// The offline inputs `sources`' package is analysed against: its
/// dependencies' metadata, read from the checkouts a prior `beamtalk build`
/// left on disk
/// ([`beamtalk_cli::dependency_classes::resolve_dependency_class_infos_with_sources`])
/// — never fetched, so a diagnostics query has no network side effects —
/// and the FFI type registry `beamtalk lint` builds
/// ([`beamtalk_cli::ffi_type_registry::lint_type_registry`]: auto-extracted
/// specs plus the distribution, dependency and project stub tiers, or the
/// runtime's specs for a `stdlib/src` target; BT-4643), with those same
/// checkouts' stub directories as the dependency tier. A dependency that
/// has not been fetched contributes nothing and keeps the pre-WS3
/// dependency guard on, as an unresolved one does for the CLI.
fn offline_inputs(
    sources: &PackageSources,
    source_files: &[Utf8PathBuf],
) -> (DependencyMetadata, ResolvedTypeRegistry) {
    let package_root = sources.package_root.as_deref();
    let scan = package_root
        .map(beamtalk_cli::dependency_classes::resolve_dependency_class_infos_with_sources)
        .unwrap_or_default();
    let registry = beamtalk_cli::ffi_type_registry::lint_type_registry(
        package_root,
        source_files,
        &scan.stub_dirs,
    );
    (scan.into(), registry)
}

/// Compute a diagnostic summary (counts + type coverage) for a path.
///
/// Runs `beamtalk lint`'s pipeline ([`lint_pipeline`]) over the path's `.bt`
/// files, aggregates the diagnostics via the shared `DiagnosticSummary`
/// type, and additionally computes type-coverage statistics. Returns a
/// JSON-serializable value suitable for direct MCP tool output.
pub(crate) fn compute_diagnostic_summary(path: &str) -> serde_json::Value {
    use beamtalk_core::semantic_analysis::{CoverageReport, infer_types};
    use beamtalk_core::source_analysis::DiagnosticSummary;

    let lint_path = Utf8PathBuf::from(path);
    let source_files = match lint_pipeline::collect_lint_files(&lint_path, path) {
        Ok((bt_files, _)) if !bt_files.is_empty() => bt_files,
        Ok(_) => {
            return serde_json::json!({
                "error": format!("No .bt source files found in '{path}'"),
                "files_checked": 0,
                "total": 0,
            });
        }
        Err(e) => {
            return serde_json::json!({
                "error": format!("{path}: {e}"),
                "files_checked": 0,
                "total": 0,
            });
        }
    };

    // Pass 1. Track unreadable target files separately so the caller sees a
    // clear error instead of a deceptively-clean `files_checked=0` summary.
    // Unreadable package-only files produce a softer warning since
    // cross-file extraction may be incomplete but the targets themselves
    // were still checked.
    let mut unreadable_files: Vec<String> = Vec::new();
    let mut unreadable_target_files: Vec<String> = Vec::new();
    let sources =
        lint_pipeline::parse_package_sources(&lint_path, &source_files, |file, is_target, _| {
            if is_target {
                unreadable_target_files.push(file.to_string());
            } else {
                unreadable_files.push(file.to_string());
            }
        });
    let (dependencies, resolved_registry) = offline_inputs(&sources, &source_files);
    let native_type_registry = resolved_registry.registry.map(std::sync::Arc::new);

    // Pass 2: analyse each file and collect diagnostics + coverage.
    let mut all_diags = Vec::new();
    let mut files_checked = 0;
    let mut coverage = CoverageReport {
        classes: Vec::new(),
        dynamic_entries: Vec::new(),
        total_expressions: 0,
        typed_expressions: 0,
    };
    let provision_diags = lint_pipeline::analyse_package(
        sources,
        dependencies,
        native_type_registry.as_ref(),
        |file| {
            files_checked += 1;
            all_diags.extend(file.diagnostics);
            let type_map = infer_types(
                file.module,
                file.class_hierarchy,
                native_type_registry.as_deref(),
            );
            coverage.merge(CoverageReport::from_module(
                file.module,
                &type_map,
                file.path.as_str(),
                false,
            ));
        },
    );
    // Each flattened-provision diagnostic once, as `beamtalk lint` publishes it.
    all_diags.extend(provision_diags.into_iter().map(|p| p.diagnostic));

    // Count only files that were actually read and analysed.
    let summary = DiagnosticSummary::from_diagnostics(&all_diags, files_checked);

    let dynamic_pct = if coverage.total_expressions > 0 {
        let typed_pct = coverage.coverage_percent();
        ((100.0 - typed_pct) * 10.0).round() / 10.0
    } else {
        0.0
    };

    let mut result = serde_json::json!({
        "files_checked": files_checked,
        "totals_by_severity": summary.severity_totals_json(),
        "totals_by_category": summary.categories_to_json_map(),
        "total": summary.total(),
        "type_coverage": {
            "typed": coverage.typed_expressions,
            "total": coverage.total_expressions,
            "dynamic_percent": dynamic_pct,
        },
    });
    // Include unreadable package files in the output so the caller
    // knows cross-file class extraction may be incomplete.
    if !unreadable_files.is_empty() {
        result["unreadable_package_files"] = serde_json::json!(unreadable_files);
    }
    // Surface unreadable target files as a structured field and a top-level
    // `error` message so callers treating the response as a summary do not
    // mistake zero-checked-files for a clean result.
    if !unreadable_target_files.is_empty() {
        let joined = unreadable_target_files.join(", ");
        result["unreadable_target_files"] = serde_json::json!(unreadable_target_files);
        result["error"] = serde_json::json!(format!("Failed to read target file(s): {joined}"));
    }
    result
}

/// Build a `LintResult` containing a single file-level error diagnostic.
fn lint_error(file: &str, message: String) -> LintResult {
    let diag = LintDiagnostic {
        file: file.to_string(),
        line: None,
        message,
        severity: "error",
    };
    LintResult {
        warnings: vec![],
        errors: vec![diag],
        total: 1,
        lint_failures: 0,
        live_recheck_failures: 0,
    }
}

/// Appends `diag` to `errors` or `warnings` as a [`LintDiagnostic`] for
/// `file`, its notes folded into the message for origin tracing.
fn push_lint_diagnostic(
    diag: &beamtalk_core::source_analysis::Diagnostic,
    file: String,
    line: Option<u32>,
    warnings: &mut Vec<LintDiagnostic>,
    errors: &mut Vec<LintDiagnostic>,
) {
    let severity = match diag.severity {
        Severity::Error => "error",
        Severity::Warning | Severity::Lint | Severity::Hint => "warning",
    };
    let message = if diag.notes.is_empty() {
        diag.message.to_string()
    } else {
        use std::fmt::Write;
        let mut msg = diag.message.to_string();
        for note in &diag.notes {
            let _ = write!(msg, " ({})", note.message);
        }
        msg
    };
    let entry = LintDiagnostic {
        file,
        line,
        message,
        severity,
    };
    if diag.severity == Severity::Error {
        errors.push(entry);
    } else {
        warnings.push(entry);
    }
}

/// Appends the stub tiers' own diagnostics (parse errors, skipped
/// signatures, version drift, collisions) as `beamtalk lint` streams them:
/// outside the gate, Lint-severity ones omitted. They all go to `warnings`,
/// even an `error`-severity one (a broken stub file), because `errors` fails
/// the tool and CLI `beamtalk lint` never counts a stub diagnostic toward
/// its exit code, with or without `--warnings-as-errors`. A stub file's are
/// reported against that file; a collision, which has no one source, against
/// the winning package.
fn push_stub_reports(reports: &[StubReport], warnings: &mut Vec<LintDiagnostic>) {
    for report in reports {
        let (path, source) = match report {
            StubReport::File(file) => (file.path.to_string(), Some(file.source.as_str())),
            StubReport::Collision { package, .. } => (package.clone(), None),
        };
        for diag in report.diagnostics() {
            if diag.severity == Severity::Lint {
                continue;
            }
            let line = source.map(|source| diag.span.line_number(source));
            let mut errors = Vec::new();
            push_lint_diagnostic(diag, path.clone(), line, warnings, &mut errors);
            warnings.append(&mut errors);
        }
    }
}

/// Run `beamtalk lint`'s pipeline ([`lint_pipeline`]) on `path` (file or
/// directory) and return structured results: the same `.bt` diagnostics,
/// `.erl` lint findings and gate counts as the CLI, given the same
/// dependency checkouts on disk (BT-4585).
pub(crate) fn run_lint_structured(path: &str) -> LintResult {
    let lint_path = Utf8PathBuf::from(path);
    let (source_files, erl_files) = match lint_pipeline::collect_lint_files(&lint_path, path) {
        Ok(files) => files,
        Err(e) => return lint_error(path, e.to_string()),
    };

    let mut warnings = Vec::new();
    let mut errors = Vec::new();
    let mut gate = LintGate::default();

    // Pass 1. An unreadable target is an error; an unreadable package file
    // only used for cross-file extraction is a warning, since extraction
    // silently dropping it could change the diagnostics.
    let sources = lint_pipeline::parse_package_sources(
        &lint_path,
        &source_files,
        |file, is_target, _| {
            if is_target {
                errors.push(LintDiagnostic {
                    file: file.to_string(),
                    line: None,
                    message: format!("Failed to read '{file}'"),
                    severity: "error",
                });
            } else {
                warnings.push(LintDiagnostic {
                file: file.to_string(),
                line: None,
                message: format!(
                    "Failed to read package file '{file}'; cross-file class extraction may be incomplete"
                ),
                severity: "warning",
            });
            }
        },
    );
    let (dependencies, resolved_registry) = offline_inputs(&sources, &source_files);
    push_stub_reports(&resolved_registry.reports, &mut warnings);
    let native_type_registry = resolved_registry.registry.map(std::sync::Arc::new);

    // Pass 2.
    let provision_diags = lint_pipeline::analyse_package(
        sources,
        dependencies,
        native_type_registry.as_ref(),
        |file| {
            for diag in &file.diagnostics {
                gate.count(diag);
                let line = diag.span.line_number(file.source);
                push_lint_diagnostic(
                    diag,
                    file.path.to_string(),
                    Some(line),
                    &mut warnings,
                    &mut errors,
                );
            }
        },
    );

    // Each flattened-provision diagnostic once, against its protocol's file
    // (its spans are offsets into that file, not the using class's).
    for routed in provision_diags {
        gate.count(&routed.diagnostic);
        let line =
            (!routed.source.is_empty()).then(|| routed.diagnostic.span.line_number(&routed.source));
        push_lint_diagnostic(
            &routed.diagnostic,
            routed.path,
            line,
            &mut warnings,
            &mut errors,
        );
    }

    // Native `.erl` files, as `beamtalk lint` checks them.
    for erl_file in &erl_files {
        match lint_pipeline::lint_erl_file(erl_file) {
            Ok(diags) => {
                gate.count_erlang(diags.len());
                warnings.extend(diags.into_iter().map(|d| LintDiagnostic {
                    file: d.file.to_string(),
                    line: u32::try_from(d.line).ok(),
                    message: match d.hint {
                        Some(hint) => format!("{} ({hint})", d.message),
                        None => d.message,
                    },
                    severity: "warning",
                }));
            }
            Err(e) => errors.push(LintDiagnostic {
                file: erl_file.to_string(),
                line: None,
                message: format!("Failed to read '{erl_file}': {e}"),
                severity: "error",
            }),
        }
    }

    let total = warnings.len() + errors.len();
    LintResult {
        warnings,
        errors,
        total,
        lint_failures: gate.lint,
        live_recheck_failures: gate.live_recheck,
    }
}
