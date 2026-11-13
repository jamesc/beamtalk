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

use crate::commands::OutputFormat;
use crate::commands::build::collect_source_files_from_dir;
use beamtalk_cli::lint_pipeline::{self, DependencyMetadata};
use beamtalk_core::semantic_analysis::CoverageReport;
use beamtalk_language_service::{ByteOffset, Position};
use beamtalk_project::package;
use camino::{Utf8Path, Utf8PathBuf};
use miette::{IntoDiagnostic, Result};

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

    // Analyse through `beamtalk lint`'s pipeline (BT-4718), so coverage is
    // measured against the context `build`/`lint` type-check with: the whole
    // package's classes, protocols, extensions and aliases (not only the
    // targets'), the dependencies' metadata, the FFI registry with
    // dependency stubs, and the package's compiler options. A hand-built
    // context here once missed all of those and reported every send to a
    // dependency class as `Dynamic`.
    let mut unreadable: Option<miette::Report> = None;
    let sources =
        lint_pipeline::parse_package_sources(&source_path, &source_files, |file, _, e| {
            unreadable.get_or_insert_with(|| miette::miette!("Failed to read '{file}': {e}"));
        });
    if let Some(e) = unreadable {
        return Err(e);
    }
    let (dependency_stubs, dependencies) = match sources.package_root.as_deref() {
        Some(project_root) => super::lint::merge_dependency_infos(project_root),
        None => (Vec::new(), DependencyMetadata::default()),
    };
    let native_type_registry = beamtalk_cli::ffi_type_registry::lint_type_registry(
        sources.package_root.as_deref(),
        &source_files,
        &dependency_stubs,
    )
    .registry
    .map(std::sync::Arc::new);

    let mut report = CoverageReport {
        classes: Vec::new(),
        dynamic_entries: Vec::new(),
        total_expressions: 0,
        typed_expressions: 0,
    };
    // The pipeline reports package files by their canonical path; report
    // them as the caller spelled them, as before.
    let spelled: std::collections::HashMap<std::path::PathBuf, &Utf8PathBuf> = source_files
        .iter()
        .map(|f| (package::try_canonicalize(f.as_std_path()), f))
        .collect();
    let mut file_sources: Vec<(Utf8PathBuf, String)> = Vec::new();
    let _provision_diags = lint_pipeline::analyse_package(
        sources,
        dependencies,
        native_type_registry.as_ref(),
        |file| {
            let path = spelled
                .get(&package::try_canonicalize(file.path.as_std_path()))
                .map_or_else(|| file.path.to_path_buf(), |f| (*f).clone());
            report.merge(CoverageReport::from_module(
                file.module,
                file.type_map,
                path.as_str(),
                detail,
            ));
            file_sources.push((path, file.source.to_string()));
        },
    );

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
        OutputFormat::Text => print_text_report(&report, detail, &file_sources),
        OutputFormat::Json => {
            print_json_report(&report, at_least, &mut std::io::stdout()).into_diagnostic()?;
        }
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
    file_sources: &[(Utf8PathBuf, String)],
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
        let source_map: std::collections::HashMap<&str, &str> = file_sources
            .iter()
            .map(|(path, source)| (path.as_str(), source.as_str()))
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
fn print_json_report(
    report: &CoverageReport,
    at_least: Option<f64>,
    out: &mut dyn std::io::Write,
) -> std::io::Result<()> {
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

    let text = serde_json::to_string_pretty(&json).expect("valid JSON");
    writeln!(out, "{text}")
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

    // Exclude common non-project directories. `stubs/` (ADR 0075)
    // is type-only and never compiled — a `declare native:` block there
    // would otherwise be swept in and rejected as a hard error.
    let excluded_prefixes: Vec<Utf8PathBuf> = [
        "deps",
        "test",
        "_build",
        "stdlib",
        "bootstrap-test",
        "stubs",
    ]
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
        // root.bt is at root level — not scanned because search root shifts to src/
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
        print_json_report(&empty_report(), None, &mut Vec::new()).unwrap();
    }

    #[test]
    fn print_json_report_with_threshold_does_not_panic() {
        // threshold=0.0 → always passes; exercises the at_least branch
        print_json_report(&empty_report(), Some(0.0), &mut Vec::new()).unwrap();
    }

    #[test]
    fn print_json_report_passed_true_when_threshold_met() {
        // empty_report has 100% coverage (0/0 → coverage_percent returns 100.0);
        // threshold=99.0 ensures the test fails if coverage_percent ever returns 0.0
        let mut out = Vec::new();
        print_json_report(&empty_report(), Some(99.0), &mut out).unwrap();
        let json: serde_json::Value = serde_json::from_slice(&out).unwrap();
        assert_eq!(json["passed"], serde_json::json!(true));
    }

    #[test]
    fn print_json_report_passed_false_when_threshold_not_met() {
        let mut out = Vec::new();
        print_json_report(&empty_report(), Some(101.0), &mut out).unwrap();
        let json: serde_json::Value = serde_json::from_slice(&out).unwrap();
        assert_eq!(json["passed"], serde_json::json!(false));
    }

    // --- print_text_report ---

    #[test]
    fn print_text_report_empty_report_does_not_panic() {
        let parsed: Vec<(Utf8PathBuf, String)> = Vec::new();
        print_text_report(&empty_report(), false, &parsed);
    }

    #[test]
    fn print_text_report_detail_mode_empty_does_not_panic() {
        // detail=true with no dynamic_entries skips the detail block
        let parsed: Vec<(Utf8PathBuf, String)> = Vec::new();
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
    fn run_json_format_with_threshold_above_coverage_returns_error() {
        let dir = TempDir::new().unwrap();
        let p = write_minimal_bt(&dir, "Foo.bt", "Object subclass: Foo\n  bar => 42\n");
        // JSON output (with "passed": false) is printed before the bail check triggers
        let result = run(p.as_str(), false, OutputFormat::Json, Some(101.0), None);
        assert!(
            result.is_err(),
            "json + threshold above 100% must always fail"
        );
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

    /// BT-3673: a typed call to a trait-provided method on a class whose
    /// trait lives in another file counts as typed (the flattened `ClassInfo`
    /// carries `tag`), so `Caller` reaches 100% coverage; unflattened, `w tag`
    /// would be `Dynamic`.
    #[test]
    fn run_counts_cross_file_trait_provided_call_as_typed_bt_3673() {
        let dir = TempDir::new().unwrap();
        let src = dir.path().join("src");
        fs::create_dir_all(&src).unwrap();
        fs::write(
            src.join("tagged.bt"),
            "Protocol define: Tagged\n  name -> String\n\n  tag -> String => self name\n",
        )
        .unwrap();
        fs::write(
            src.join("widget.bt"),
            "Object subclass: Widget\n  uses: Tagged\n  name -> String => \"w\"\n",
        )
        .unwrap();
        fs::write(
            src.join("caller.bt"),
            "Object subclass: Caller\n  describe: w :: Widget -> String => w tag\n",
        )
        .unwrap();
        let root = Utf8PathBuf::from_path_buf(src).unwrap();
        let result = run(
            root.as_str(),
            false,
            OutputFormat::Text,
            Some(100.0),
            Some("Caller"),
        );
        assert!(
            result.is_ok(),
            "Caller must be fully typed through the flattened Widget: {result:?}"
        );
    }

    /// BT-4537: a `type` alias attributes identically to its inline
    /// expansion — `input :: TextOrBytes` (alias for `String | Binary`)
    /// keeps `input size` typed, so `Parser` reaches 100% just like the
    /// spelled-out union. Before the fix the alias resolved to an unknown
    /// nominal class and the send inferred as `Dynamic` (50%).
    #[test]
    fn run_alias_annotation_matches_inline_coverage_bt_4537() {
        let inline_src = "Object subclass: Parser\n  parseDecimal: input :: String | Binary -> Integer =>\n    input size\n";
        let alias_src = "type TextOrBytes = String | Binary\n\nObject subclass: Parser\n  parseDecimal: input :: TextOrBytes -> Integer =>\n    input size\n";

        let inline_dir = TempDir::new().unwrap();
        let inline_file = write_minimal_bt(&inline_dir, "Parser.bt", inline_src);
        assert!(
            run(
                inline_file.as_str(),
                false,
                OutputFormat::Text,
                Some(100.0),
                Some("Parser")
            )
            .is_ok(),
            "inline union baseline must be fully typed"
        );

        let alias_dir = TempDir::new().unwrap();
        let alias_file = write_minimal_bt(&alias_dir, "Parser.bt", alias_src);
        let result = run(
            alias_file.as_str(),
            false,
            OutputFormat::Text,
            Some(100.0),
            Some("Parser"),
        );
        assert!(
            result.is_ok(),
            "alias-typed param must attribute identically to the inline union: {result:?}"
        );
    }

    /// BT-4718: a `test/` run analyses against the whole package, so a
    /// test-side class annotated with a `src/` class is fully typed. Before,
    /// only the targets were parsed and `w :: Widget` was an unknown class.
    #[test]
    fn run_on_test_dir_sees_src_classes_bt_4718() {
        let dir = TempDir::new().unwrap();
        fs::write(
            dir.path().join("beamtalk.toml"),
            "[package]\nname = \"pkg\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        fs::create_dir_all(dir.path().join("src")).unwrap();
        fs::create_dir_all(dir.path().join("test")).unwrap();
        fs::write(
            dir.path().join("src").join("widget.bt"),
            "Object subclass: Widget\n  size -> Integer => 3\n",
        )
        .unwrap();
        fs::write(
            dir.path().join("test").join("caller.bt"),
            "Object subclass: Caller\n  measure: w :: Widget -> Integer => w size\n",
        )
        .unwrap();
        let test_dir = Utf8PathBuf::from_path_buf(dir.path().join("test")).unwrap();
        let result = run(
            test_dir.as_str(),
            false,
            OutputFormat::Text,
            Some(100.0),
            Some("Caller"),
        );
        assert!(
            result.is_ok(),
            "Caller must be fully typed through src/'s Widget: {result:?}"
        );
    }

    /// BT-4717: FFI calls whose OTP spec returns an opaque handle
    /// (`zlib:open/0` zstream, `erlang:monitor/2` reference) keep their named
    /// type, so a `zstream` handed back to `zlib` and compared with `=:=` is
    /// fully typed. Before, every such call was "FFI spec is Dynamic" and the
    /// uses of its result were "dynamic receiver" — this fixture sat at 42.9%.
    /// Needs the built Erlang runtime for live spec extraction, like the other
    /// OTP-spec tests; the fixture is `tests/otp-opaque-handles`, which
    /// beamtalk-buccaneer/-catalina mirror (check there with
    /// `beamtalk type-coverage --detail src | grep 'FFI spec is Dynamic'`).
    #[test]
    fn run_types_otp_opaque_handles_bt_4717() {
        let dir = TempDir::new().unwrap();
        fs::write(
            dir.path().join("beamtalk.toml"),
            "[package]\nname = \"otp_opaque_handles\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        let src = dir.path().join("src");
        fs::create_dir_all(&src).unwrap();
        fs::write(
            src.join("handles.bt"),
            include_str!("../../../../tests/otp-opaque-handles/src/handles.bt"),
        )
        .unwrap();
        let root = Utf8PathBuf::from_path_buf(src).unwrap();
        // Live extraction needs OTP `.beam` files on disk; skip rather than
        // false-fail where they are not discoverable (e.g. a sandbox).
        let pkg_root = Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();
        let probe = beamtalk_cli::ffi_type_registry::lint_type_registry(
            Some(&pkg_root),
            &[root.join("handles.bt")],
            &[],
        )
        .registry;
        if probe.is_none_or(|r| r.lookup("zlib", "open", 0).is_none()) {
            eprintln!(
                "skipping run_types_otp_opaque_handles_bt_4717: no OTP .beam files discovered"
            );
            return;
        }
        let result = run(
            root.as_str(),
            false,
            OutputFormat::Text,
            Some(100.0),
            Some("Handles"),
        );
        assert!(
            result.is_ok(),
            "OTP opaque handles must stay typed (no FFI-spec Dynamic): {result:?}"
        );
    }

    /// BT-4718: sends to a dependency's class are typed, as `build` types
    /// them. Before, `type-coverage` loaded no dependency metadata, so
    /// `p value` on a `Producer` from a path dependency was `Dynamic`.
    #[test]
    fn run_types_sends_to_dependency_classes_bt_4718() {
        let temp = TempDir::new().unwrap();
        let producer_dir = temp.path().join("producer");
        fs::create_dir_all(producer_dir.join("src")).unwrap();
        fs::write(
            producer_dir.join("beamtalk.toml"),
            "[package]\nname = \"producer\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        fs::write(
            producer_dir.join("src").join("producer.bt"),
            "Object subclass: Producer\n  value -> Integer => 1\n",
        )
        .unwrap();

        let consumer_dir = temp.path().join("consumer");
        fs::create_dir_all(consumer_dir.join("src")).unwrap();
        fs::write(
            consumer_dir.join("beamtalk.toml"),
            "[package]\nname = \"consumer\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nproducer = { path = \"../producer\" }\n",
        )
        .unwrap();
        fs::write(
            consumer_dir.join("src").join("consumer.bt"),
            "Object subclass: Consumer\n  use: p :: Producer -> Integer => p value\n",
        )
        .unwrap();

        // Fake the producer's compiled state so dependency resolution takes
        // the fresh fast path instead of an `erlc` compile (as lint's
        // dependency tests do).
        let consumer_root = Utf8PathBuf::from_path_buf(consumer_dir).unwrap();
        let layout = crate::commands::build_layout::BuildLayout::new(&consumer_root);
        let ebin_dir = layout.dep_ebin_dir("producer");
        fs::create_dir_all(&ebin_dir).unwrap();
        fs::write(ebin_dir.join("bt@producer@producer.beam"), b"BEAM").unwrap();
        crate::commands::build_stamp::write_stamp(
            &layout.dep_stamp_path("producer"),
            crate::commands::build_stamp::current_otp_version(),
        );

        let src = consumer_root.join("src");
        let result = run(
            src.as_str(),
            false,
            OutputFormat::Text,
            Some(100.0),
            Some("Consumer"),
        );
        assert!(
            result.is_ok(),
            "a send to the dependency's Producer must be typed: {result:?}"
        );
    }
}
