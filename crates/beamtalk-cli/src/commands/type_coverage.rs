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

    // BT-2867: Populate the FFI type registry the same way `beamtalk lint` does,
    // so calls to specced Erlang functions (and expressions consuming their
    // results) are reported as typed rather than misreported as `Dynamic`.
    // Falls back to the runtime/stdlib ebin scan (mirroring `extract_type_specs`'s
    // own `stdlib_mode` branch) when scanning a manifest-less tree such as
    // `stdlib/src`, which has no `beamtalk.toml`.
    let package_root = super::lint::find_package_root(&source_path);
    let native_type_registry = match package_root.as_deref() {
        Some(root) => {
            let layout = crate::commands::build_layout::BuildLayout::new(root);
            super::build::extract_type_specs(&layout, true, false)
        }
        None => super::build_stdlib::extract_stdlib_type_specs(),
    };

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

        let type_map = infer_types(
            module,
            &analysis_result.class_hierarchy,
            native_type_registry.as_ref(),
        );

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
            miette::bail!("type coverage {display:.1}% is below threshold {threshold:.1}%");
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
            let reason = entry.reason.description().unwrap_or("unknown");
            println!("  {location:<40} ({reason})");
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

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_core::semantic_analysis::CoverageReport;
    use camino::Utf8PathBuf;
    use std::fs;
    use tempfile::TempDir;

    fn empty_report() -> CoverageReport {
        CoverageReport {
            classes: Vec::new(),
            dynamic_entries: Vec::new(),
            total_expressions: 0,
            typed_expressions: 0,
        }
    }

    // --- OutputFormat::from_str ---

    #[test]
    fn output_format_parses_text() {
        assert_eq!("text".parse::<OutputFormat>().unwrap(), OutputFormat::Text);
    }

    #[test]
    fn output_format_parses_json() {
        assert_eq!("json".parse::<OutputFormat>().unwrap(), OutputFormat::Json);
    }

    #[test]
    fn output_format_rejects_unknown_value() {
        let err = "xml".parse::<OutputFormat>().unwrap_err();
        assert!(
            err.contains("xml"),
            "error should name the bad value: {err}"
        );
    }

    // --- collect_coverage_files ---

    #[test]
    fn collect_rejects_non_bt_file() {
        let dir = TempDir::new().unwrap();
        let p = Utf8PathBuf::from_path_buf(dir.path().join("main.txt")).unwrap();
        fs::write(p.as_std_path(), "hello").unwrap();
        assert!(collect_coverage_files(&p, p.as_str()).is_err());
    }

    #[test]
    fn collect_accepts_single_bt_file() {
        let dir = TempDir::new().unwrap();
        let p = Utf8PathBuf::from_path_buf(dir.path().join("main.bt")).unwrap();
        fs::write(p.as_std_path(), "// empty").unwrap();
        let files = collect_coverage_files(&p, p.as_str()).unwrap();
        assert_eq!(files.len(), 1);
        assert_eq!(files[0], p);
    }

    #[test]
    fn collect_rejects_nonexistent_path() {
        let p = Utf8PathBuf::from("/nonexistent/beamtalk/coverage/path");
        assert!(collect_coverage_files(&p, p.as_str()).is_err());
    }

    #[test]
    fn collect_empty_dir_returns_empty() {
        let dir = TempDir::new().unwrap();
        let p = Utf8PathBuf::from_path_buf(dir.path().to_owned()).unwrap();
        assert!(collect_coverage_files(&p, p.as_str()).unwrap().is_empty());
    }

    #[test]
    fn collect_prefers_src_subdir_over_root() {
        let dir = TempDir::new().unwrap();
        let src = dir.path().join("src");
        fs::create_dir_all(&src).unwrap();
        fs::write(src.join("lib.bt"), "// lib").unwrap();
        // .bt at root level — ignored because src/ exists
        fs::write(dir.path().join("root.bt"), "// root").unwrap();
        let p = Utf8PathBuf::from_path_buf(dir.path().to_owned()).unwrap();
        let files = collect_coverage_files(&p, p.as_str()).unwrap();
        assert_eq!(files.len(), 1);
        assert!(files[0].as_str().ends_with("lib.bt"));
    }

    #[test]
    fn collect_excludes_standard_non_project_dirs() {
        let dir = TempDir::new().unwrap();
        for excluded in &["deps", "test", "_build", "stdlib", "bootstrap-test"] {
            let d = dir.path().join(excluded);
            fs::create_dir_all(&d).unwrap();
            fs::write(d.join("something.bt"), "// excluded").unwrap();
        }
        // One file at root that should be found
        fs::write(dir.path().join("main.bt"), "// main").unwrap();
        let p = Utf8PathBuf::from_path_buf(dir.path().to_owned()).unwrap();
        let files = collect_coverage_files(&p, p.as_str()).unwrap();
        assert_eq!(files.len(), 1, "expected only main.bt, got: {files:?}");
        assert!(files[0].as_str().ends_with("main.bt"));
    }

    // --- print_json_report ---

    #[test]
    fn print_json_report_empty_report_does_not_panic() {
        print_json_report(&empty_report(), None);
    }

    #[test]
    fn print_json_report_with_threshold_includes_passed_field() {
        // threshold=0.0 → always passes; exercises the at_least branch
        print_json_report(&empty_report(), Some(0.0));
    }

    // --- print_text_report ---

    #[test]
    fn print_text_report_empty_report_does_not_panic() {
        let parsed: Vec<(Utf8PathBuf, String, beamtalk_core::ast::Module)> = Vec::new();
        print_text_report(&empty_report(), false, &parsed);
    }

    #[test]
    fn print_text_report_detail_mode_empty_does_not_panic() {
        // detail=true with no dynamic_entries skips the detail block
        let parsed: Vec<(Utf8PathBuf, String, beamtalk_core::ast::Module)> = Vec::new();
        print_text_report(&empty_report(), true, &parsed);
    }

    // --- run() ---

    fn write_minimal_bt(dir: &TempDir, name: &str, content: &str) -> Utf8PathBuf {
        let p = dir.path().join(name);
        fs::write(&p, content).unwrap();
        Utf8PathBuf::from_path_buf(p).unwrap()
    }

    #[test]
    fn run_text_format_on_valid_bt_file_returns_ok() {
        let dir = TempDir::new().unwrap();
        let p = write_minimal_bt(&dir, "Foo.bt", "Object subclass: Foo\n  bar => 42\n");
        assert!(
            run(p.as_str(), false, OutputFormat::Text, None, None).is_ok(),
            "run() should succeed on a valid .bt file"
        );
    }

    #[test]
    fn run_json_format_on_valid_bt_file_returns_ok() {
        let dir = TempDir::new().unwrap();
        let p = write_minimal_bt(&dir, "Foo.bt", "Object subclass: Foo\n  bar => 42\n");
        assert!(run(p.as_str(), false, OutputFormat::Json, None, None).is_ok());
    }

    #[test]
    fn run_detail_mode_on_valid_bt_file_returns_ok() {
        let dir = TempDir::new().unwrap();
        let p = write_minimal_bt(&dir, "Foo.bt", "Object subclass: Foo\n  bar => 42\n");
        assert!(run(p.as_str(), true, OutputFormat::Text, None, None).is_ok());
    }

    #[test]
    fn run_with_nonexistent_path_returns_error() {
        let result = run(
            "/nonexistent/beamtalk/path",
            false,
            OutputFormat::Text,
            None,
            None,
        );
        assert!(result.is_err());
    }

    #[test]
    fn run_with_empty_dir_returns_error() {
        let dir = TempDir::new().unwrap();
        let p = Utf8PathBuf::from_path_buf(dir.path().to_owned()).unwrap();
        let result = run(p.as_str(), false, OutputFormat::Text, None, None);
        assert!(
            result.is_err(),
            "empty dir should error: no .bt source files found"
        );
    }

    #[test]
    fn run_at_least_above_actual_coverage_returns_error() {
        let dir = TempDir::new().unwrap();
        let p = write_minimal_bt(&dir, "Foo.bt", "Object subclass: Foo\n  bar => 42\n");
        // 101% threshold is always unachievable, so run() should bail
        let result = run(p.as_str(), false, OutputFormat::Text, Some(101.0), None);
        assert!(result.is_err(), "threshold above 100% must always fail");
    }

    #[test]
    fn run_at_least_zero_always_passes() {
        let dir = TempDir::new().unwrap();
        let p = write_minimal_bt(&dir, "Foo.bt", "Object subclass: Foo\n  bar => 42\n");
        assert!(run(p.as_str(), false, OutputFormat::Text, Some(0.0), None).is_ok());
    }

    #[test]
    fn run_class_filter_nonexistent_returns_error() {
        let dir = TempDir::new().unwrap();
        let p = write_minimal_bt(&dir, "Foo.bt", "Object subclass: Foo\n  bar => 42\n");
        let result = run(
            p.as_str(),
            false,
            OutputFormat::Text,
            None,
            Some("NonExistent"),
        );
        assert!(result.is_err(), "filter for unknown class should error");
    }

    #[test]
    fn run_class_filter_matching_class_returns_ok() {
        let dir = TempDir::new().unwrap();
        let p = write_minimal_bt(&dir, "Foo.bt", "Object subclass: Foo\n  bar => 42\n");
        assert!(run(p.as_str(), false, OutputFormat::Text, None, Some("Foo")).is_ok());
    }
}
