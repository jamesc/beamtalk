// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk type-coverage` — report type coverage statistics per class and per file.
//!
//! **DDD Context:** Compilation (CLI surface for semantic analysis data)
//!
//! Walks the `TypeMap` produced by type inference and reports what percentage of
//! expressions have non-`Dynamic` types. Supports `--detail`, `--format json`,
//! `--at-least N` (CI ratchet), and `--class ClassName` filtering.
//!
//! **References:** ADR 0077 Section 3

use crate::commands::build::collect_source_files_from_dir;
use beamtalk_core::language_service::{ByteOffset, Position};
use beamtalk_core::semantic_analysis::{ClassHierarchy, CoverageReport, infer_types};
use beamtalk_core::source_analysis::{lex_with_eof, parse};
use camino::{Utf8Path, Utf8PathBuf};
use miette::{IntoDiagnostic, Result};

/// Output format for the coverage report.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputFormat {
    /// Human-readable table output (default).
    Text,
    /// Machine-readable JSON output.
    Json,
}

impl std::str::FromStr for OutputFormat {
    type Err = String;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "text" => Ok(Self::Text),
            "json" => Ok(Self::Json),
            other => Err(format!(
                "unknown format '{other}', expected 'text' or 'json'"
            )),
        }
    }
}

/// Run the `beamtalk type-coverage` command.
///
/// Parses all `.bt` source files, runs type inference, and reports coverage.
pub fn run(
    path: &str,
    detail: bool,
    format: OutputFormat,
    at_least: Option<f64>,
    class_filter: Option<&str>,
) -> Result<()> {
    let source_path = Utf8PathBuf::from(path);
    let source_files = collect_coverage_files(&source_path, path)?;

    if source_files.is_empty() {
        miette::bail!("no .bt source files found in '{path}'");
    }

    // Pass 1: Parse all files and extract class metadata for cross-file hierarchy.
    let mut all_class_infos = Vec::new();
    let mut parsed_files: Vec<(Utf8PathBuf, String, beamtalk_core::ast::Module)> = Vec::new();

    for file in &source_files {
        let source = std::fs::read_to_string(file)
            .into_diagnostic()
            .map_err(|e| miette::miette!("Failed to read '{}': {e}", file))?;

        let tokens = lex_with_eof(&source);
        let (module, _parse_diags) = parse(tokens);

        all_class_infos.extend(ClassHierarchy::extract_class_infos(&module));
        parsed_files.push((file.clone(), source, module));
    }

    // Pass 2: Run type inference per file and compute coverage.
    let mut report = CoverageReport {
        classes: Vec::new(),
        dynamic_entries: Vec::new(),
        total_expressions: 0,
        typed_expressions: 0,
    };

    for (file, _source, module) in &parsed_files {
        let cross_file_classes = ClassHierarchy::cross_file_class_infos(&all_class_infos, module);

        let analysis_result = beamtalk_core::semantic_analysis::analyse_with_options_and_classes(
            module,
            &beamtalk_core::CompilerOptions::default(),
            cross_file_classes,
        );

        let type_map = infer_types(module, &analysis_result.class_hierarchy);

        let file_report = CoverageReport::from_module(module, &type_map, file.as_str(), detail);
        report.merge(file_report);
    }

    // Apply --class filter.
    if let Some(class_name) = class_filter {
        report.classes.retain(|c| c.name.as_str() == class_name);

        if report.classes.is_empty() {
            miette::bail!("no class named '{class_name}' found in project files");
        }

        // Filter dynamic entries to matching class.
        report
            .dynamic_entries
            .retain(|e| e.class_name.as_str() == class_name);

        // Recompute totals from filtered classes.
        report.total_expressions = report.classes.iter().map(|c| c.total).sum();
        report.typed_expressions = report.classes.iter().map(|c| c.typed).sum();
    }

    // Sort classes by file then name for stable output.
    report
        .classes
        .sort_by(|a, b| a.file.cmp(&b.file).then_with(|| a.name.cmp(&b.name)));

    match format {
        OutputFormat::Text => print_text_report(&report, detail, &parsed_files),
        OutputFormat::Json => print_json_report(&report, at_least),
    }

    // --at-least: exit non-zero if coverage is below threshold.
    // Compare unrounded coverage to avoid false passes (e.g. 79.96% rounding to 80.0%).
    if let Some(threshold) = at_least {
        let coverage = report.coverage_percent();
        if coverage < threshold {
            let display = (coverage * 10.0).round() / 10.0;
            miette::bail!(
                "type coverage {display:.1}% is below threshold {threshold:.1}%"
            );
        }
    }

    Ok(())
}

/// Print the human-readable text report.
fn print_text_report(
    report: &CoverageReport,
    detail: bool,
    parsed_files: &[(Utf8PathBuf, String, beamtalk_core::ast::Module)],
) {
    println!("Type Coverage Report");
    println!("====================");
    println!();
    println!("{:<30} {:<20} Coverage", "File", "Class");

    for class in &report.classes {
        let pct = class.coverage_percent();
        println!(
            "{:<30} {:<20} {:.1}%  ({}/{} expressions)",
            class.file, class.name, pct, class.typed, class.total
        );
    }

    let separator = "\u{2500}".repeat(70);
    println!("{separator}");
    let pct = report.coverage_percent();
    println!(
        "{:<30} {:<20} {:.1}%  ({}/{} expressions)",
        "Total", "", pct, report.typed_expressions, report.total_expressions
    );

    if detail && !report.dynamic_entries.is_empty() {
        println!();
        println!("Dynamic expressions:");

        // Build a source map for offset-to-line conversion.
        let source_map: std::collections::HashMap<&str, &str> = parsed_files
            .iter()
            .map(|(path, source, _)| (path.as_str(), source.as_str()))
            .collect();

        for entry in &report.dynamic_entries {
            let location = if let Some(source) = source_map.get(entry.file.as_str()) {
                let byte_offset = ByteOffset::new(entry.span.start());
                if let Some(pos) = Position::from_byte_offset(source, byte_offset) {
                    // Display as 1-based line:col.
                    format!("{}:{}:{}", entry.file, pos.line + 1, pos.column + 1)
                } else {
                    format!("{}:?:?", entry.file)
                }
            } else {
                format!("{}:?:?", entry.file)
            };
            println!("  {:<40} ({})", location, entry.reason.description());
        }
    }
}

/// Print the machine-readable JSON report.
fn print_json_report(report: &CoverageReport, at_least: Option<f64>) {
    let pct = (report.coverage_percent() * 10.0).round() / 10.0;

    let classes: Vec<serde_json::Value> = report
        .classes
        .iter()
        .map(|c| {
            let class_pct = (c.coverage_percent() * 10.0).round() / 10.0;
            serde_json::json!({
                "name": c.name.as_str(),
                "file": c.file,
                "total": c.total,
                "typed": c.typed,
                "coverage_percent": class_pct
            })
        })
        .collect();

    let mut json = serde_json::json!({
        "total_expressions": report.total_expressions,
        "typed_expressions": report.typed_expressions,
        "coverage_percent": pct,
        "classes": classes
    });

    if let Some(threshold) = at_least {
        let unrounded = report.coverage_percent();
        json["threshold"] = serde_json::json!(threshold);
        json["passed"] = serde_json::json!(unrounded >= threshold);
    }

    println!(
        "{}",
        serde_json::to_string_pretty(&json).expect("valid JSON")
    );
}

/// Collect `.bt` source files, excluding deps, test, and stdlib directories.
fn collect_coverage_files(source_path: &Utf8Path, path: &str) -> Result<Vec<Utf8PathBuf>> {
    if source_path.is_file() {
        if source_path.extension().is_some_and(|ext| ext == "bt") {
            return Ok(vec![source_path.to_path_buf()]);
        }
        miette::bail!("'{path}' is not a .bt file");
    }

    if !source_path.is_dir() {
        miette::bail!("'{path}' is not a file or directory");
    }

    // Look for src/ subdirectory first (standard project layout).
    let src_dir = source_path.join("src");
    let search_dir = if src_dir.is_dir() {
        &src_dir
    } else {
        source_path
    };

    let all_files = collect_source_files_from_dir(search_dir)?;

    // Exclude common non-project directories.
    let excluded_prefixes: Vec<Utf8PathBuf> =
        ["deps", "test", "_build", "stdlib", "bootstrap-test"]
            .iter()
            .map(|d| source_path.join(d))
            .collect();

    let files: Vec<Utf8PathBuf> = all_files
        .into_iter()
        .filter(|f| !excluded_prefixes.iter().any(|prefix| f.starts_with(prefix)))
        .collect();

    Ok(files)
}
