// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk lint` — run style/redundancy lint checks on Beamtalk source files.
//!
//! This command parses each `.bt` file, runs all lint passes, and reports
//! [`Severity::Lint`] diagnostics. It exits non-zero if any unsuppressed lint
//! diagnostics are found.
//!
//! Lint diagnostics are suppressed during normal `check`/`compile` — this is
//! the only command that surfaces them.

use crate::commands::build::collect_source_files_from_dir;
use crate::diagnostic::CompileDiagnostic;
use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};
use camino::Utf8PathBuf;
use miette::{IntoDiagnostic, Result};

/// Run lint passes on the given path (file or directory).
///
/// Prints each lint diagnostic and returns an error if any are found.
pub fn run_lint(path: &str, format: OutputFormat) -> Result<()> {
    let source_path = Utf8PathBuf::from(path);

    let source_files = if source_path.is_file() {
        if source_path.extension() == Some("bt") {
            vec![source_path.clone()]
        } else {
            miette::bail!("File '{}' is not a .bt source file", path);
        }
    } else if source_path.is_dir() {
        collect_source_files_from_dir(&source_path)?
    } else {
        miette::bail!("Path '{}' does not exist", path);
    };

    if source_files.is_empty() {
        miette::bail!("No .bt source files found in '{path}'");
    }

    let mut total_lint_count = 0usize;

    for file in &source_files {
        let source = std::fs::read_to_string(file)
            .into_diagnostic()
            .map_err(|e| miette::miette!("Failed to read '{}': {e}", file))?;

        let tokens = lex_with_eof(&source);
        let (module, parse_diags) = parse(tokens);

        // Collect parser-level lint diagnostics (e.g. unnecessary `.` — BT-948)
        // plus AST-level lint passes.
        let mut lint_diags: Vec<_> = parse_diags
            .into_iter()
            .filter(|d| d.severity == Severity::Lint)
            .collect();
        lint_diags.extend(beamtalk_core::lint::run_lint_passes(&module));

        for diag in &lint_diags {
            debug_assert_eq!(diag.severity, Severity::Lint);
            match format {
                OutputFormat::Text => {
                    let compile_diag =
                        CompileDiagnostic::from_core_diagnostic(diag, file.as_str(), &source);
                    eprintln!("{:?}", miette::Report::new(compile_diag));
                }
                OutputFormat::Json => {
                    let json = serde_json::json!({
                        "file": file.as_str(),
                        "severity": "lint",
                        "message": diag.message.as_str(),
                        "span_start": diag.span.start(),
                        "span_end": diag.span.end(),
                        "hint": diag.hint.as_deref(),
                    });
                    println!("{json}");
                }
            }
        }

        total_lint_count += lint_diags.len();
    }

    if total_lint_count > 0 {
        let files_checked = source_files.len();
        let plural = if total_lint_count == 1 { "" } else { "s" };
        miette::bail!(
            "{total_lint_count} lint diagnostic{plural} found in {files_checked} file(s)"
        );
    }

    Ok(())
}

/// Output format for lint diagnostics.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum OutputFormat {
    /// Human-readable text output via miette (default).
    #[default]
    Text,
    /// Machine-readable JSON (one object per line).
    Json,
}

impl std::str::FromStr for OutputFormat {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "text" => Ok(Self::Text),
            "json" => Ok(Self::Json),
            other => Err(format!(
                "unknown format '{other}': expected 'text' or 'json'"
            )),
        }
    }
}
