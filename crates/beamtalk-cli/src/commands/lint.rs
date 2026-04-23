// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk lint` — run style/redundancy lint checks on source files.
//!
//! This command parses each `.bt` file, runs all lint passes, and reports
//! [`Severity::Lint`] diagnostics. It also runs semantic analysis to collect
//! DNU hint diagnostics so that `@expect type` annotations are correctly
//! applied (BT-1547). It exits non-zero if any unsuppressed lint diagnostics
//! are found.
//!
//! Native `.erl` files in `native/` and `native/test/` are also checked for
//! missing `-moduledoc`/`-doc` attributes and hardcoded `'bt@...'` module
//! references (BT-1909).
//!
//! Lint diagnostics are suppressed during normal `check`/`compile` — this is
//! the only command that surfaces them.

use crate::commands::build::collect_source_files_from_dir;
use crate::commands::erlang_lint;
use crate::diagnostic::CompileDiagnostic;
use beamtalk_core::file_walker::FileWalker;
use beamtalk_core::project::package;
use beamtalk_core::source_analysis::{Severity, Span, lex_with_eof, parse};
use camino::{Utf8Path, Utf8PathBuf};
use miette::{IntoDiagnostic, Result};
use tracing::warn;

/// Collect lint diagnostics for a parsed module.
///
/// Gathers lint-severity diagnostics from parsing and lint passes, plus
/// DNU hint diagnostics from semantic analysis (BT-1547), then applies
/// `@expect` directives.
///
/// `cross_file_classes` provides class metadata from other files in the same
/// project so that cross-file type/DNU diagnostics match what `build` emits.
/// Without this, `@expect type` / `@expect all` annotations that suppress real
/// diagnostics during build would be reported as stale by lint.
fn collect_diagnostics(
    module: &beamtalk_core::ast::Module,
    parse_diags: Vec<beamtalk_core::source_analysis::Diagnostic>,
    cross_file_classes: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
) -> Vec<beamtalk_core::source_analysis::Diagnostic> {
    // Collect parser-level lint diagnostics (e.g. unnecessary `.` — BT-948)
    // plus AST-level lint passes.
    let mut lint_diags: Vec<_> = parse_diags
        .into_iter()
        .filter(|d| d.severity == Severity::Lint)
        .collect();
    lint_diags.extend(beamtalk_core::lint::run_lint_passes(module));

    // BT-1547: Run semantic analysis to collect all categorised diagnostics
    // so that `@expect` directives can match them. Without this, `@expect type`
    // annotations that suppress real type/DNU diagnostics during build would be
    // reported as stale by lint. We include every diagnostic that has a category
    // (Type, Dnu, Unused, etc.) — this keeps lint in sync with `category_matches`
    // in diagnostic_provider.rs without manually mirroring its match arms.
    //
    // Pass cross-file class info so lint sees the same class hierarchy as build,
    // matching diagnostics for actor instantiation, type errors, etc.
    let analysis_result = beamtalk_core::semantic_analysis::analyse_with_options_and_classes(
        module,
        &beamtalk_core::CompilerOptions::default(),
        cross_file_classes,
    );
    lint_diags.extend(
        analysis_result
            .diagnostics
            .into_iter()
            .filter(|d| d.category.is_some()),
    );

    // BT-1476: Apply @expect directives to suppress matching lint diagnostics.
    // Note: apply_expect_directives may inject Severity::Warning for stale
    // @expect annotations, so we include those in the output.
    beamtalk_core::queries::diagnostic_provider::apply_expect_directives(module, &mut lint_diags);

    lint_diags
}

/// Run lint passes on the given path (file or directory).
///
/// Prints each lint diagnostic and returns an error if any are found.
pub fn run_lint(path: &str, format: OutputFormat) -> Result<()> {
    let source_path = Utf8PathBuf::from(path);
    let (source_files, erl_files) = collect_lint_files(&source_path, path)?;

    // Pass 1: Parse all files and extract class metadata so that cross-file
    // type/DNU diagnostics in Pass 2 match what `build` emits. Without this,
    // `@expect` annotations that suppress real cross-file diagnostics during
    // build would be reported as stale by lint.
    //
    // BT-2027: When the lint target is a subset of a package (e.g. `test/` or a
    // single file), extraction must still cover the full package source set so
    // classes defined in sibling directories (`src/` from `test/`, etc.) are
    // visible. Otherwise a test file that references a `src/` class produces
    // spurious `Unresolved class` diagnostics.
    let package_root = find_package_root(&source_path);
    let (mut all_class_infos, parsed_files) =
        parse_and_extract_class_infos(&source_files, package_root.as_deref())?;

    // Resolve dependency class metadata so lint sees the same class hierarchy
    // as build. Without this, @expect annotations that suppress real cross-package
    // diagnostics would be reported as stale.
    if let Some(ref project_root) = package_root {
        resolve_dep_class_infos(project_root, &mut all_class_infos);
    }

    // Pass 2: Analyse each file with cross-file class context.
    let mut total_lint_count = 0usize;
    let mut all_diags: Vec<beamtalk_core::source_analysis::Diagnostic> = Vec::new();

    for (file, source, module, parse_diags) in parsed_files {
        let cross_file_classes =
            beamtalk_core::semantic_analysis::ClassHierarchy::cross_file_class_infos(
                &all_class_infos,
                &module,
            );

        let lint_diags = collect_diagnostics(&module, parse_diags, cross_file_classes);

        for diag in &lint_diags {
            match format {
                OutputFormat::Text => {
                    let compile_diag =
                        CompileDiagnostic::from_core_diagnostic(diag, file.as_str(), &source);
                    eprintln!("{:?}", miette::Report::new(compile_diag));
                }
                OutputFormat::Json => {
                    // BT-2031: Stream each diagnostic as line-delimited JSON
                    // instead of buffering all diagnostics in memory.
                    let notes: Vec<serde_json::Value> = diag
                        .notes
                        .iter()
                        .map(|n| {
                            serde_json::json!({
                                "message": n.message.as_str(),
                                "span_start": n.span.map(Span::start),
                                "span_end": n.span.map(Span::end),
                            })
                        })
                        .collect();
                    let json = serde_json::json!({
                        "file": file.as_str(),
                        "severity": format!("{:?}", diag.severity).to_lowercase(),
                        "message": diag.message.as_str(),
                        "span_start": diag.span.start(),
                        "span_end": diag.span.end(),
                        "hint": diag.hint.as_deref(),
                        "notes": notes,
                    });
                    println!("{json}");
                }
            }
        }

        // Only count actual lint diagnostics toward the failure threshold.
        // apply_expect_directives may inject Severity::Warning for stale @expect
        // annotations — those should be displayed but not fail the command.
        total_lint_count += lint_diags
            .iter()
            .filter(|d| d.severity == Severity::Lint)
            .count();

        // Collect diagnostics for the summary.
        all_diags.extend(lint_diags);
    }

    // Lint native .erl files (BT-1909).
    total_lint_count += lint_erl_files(&erl_files, format)?;

    // BT-2014 / BT-2031: Build and print diagnostic summary.
    // `all_diags` only contains `.bt` diagnostics, so `files_checked` must
    // count only `.bt` files to keep the ratio consistent.
    let bt_files_checked = source_files.len();
    let total_files_checked = bt_files_checked + erl_files.len();
    let summary = beamtalk_core::source_analysis::DiagnosticSummary::from_diagnostics(
        &all_diags,
        bt_files_checked,
    );

    match format {
        OutputFormat::Text => {
            if !summary.is_empty() {
                eprintln!();
                eprintln!("{summary}");
            }
        }
        OutputFormat::Json => {
            // Per-diagnostic JSON lines were already streamed above (BT-2031).
            // Emit the summary as a final JSON object.
            let summary_json = diagnostic_summary_to_json(&summary);
            println!("{summary_json}");
        }
    }

    if total_lint_count > 0 {
        let plural = if total_lint_count == 1 { "" } else { "s" };
        miette::bail!(
            "{total_lint_count} lint diagnostic{plural} found in {total_files_checked} file(s)"
        );
    }

    Ok(())
}

/// Collect `.bt` and `.erl` files from the given path.
fn collect_lint_files(
    source_path: &Utf8PathBuf,
    path: &str,
) -> Result<(Vec<Utf8PathBuf>, Vec<Utf8PathBuf>)> {
    let mut erl_files: Vec<Utf8PathBuf> = Vec::new();

    let source_files = if source_path.is_file() {
        match source_path.extension() {
            Some("bt") => vec![source_path.clone()],
            Some("erl") => {
                erl_files.push(source_path.clone());
                Vec::new()
            }
            _ => miette::bail!("File '{}' is not a .bt or .erl source file", path),
        }
    } else if source_path.is_dir() {
        let project_root = find_package_root(source_path).unwrap_or_else(|| source_path.clone());
        let native_dir = project_root.join("native");
        if native_dir.is_dir() {
            match FileWalker::native_erl_files().walk(&native_dir) {
                Ok(files) => erl_files = files,
                Err(e) => warn!("failed to scan native directory: {e}"),
            }
        }
        collect_source_files_from_dir(source_path)?
    } else {
        miette::bail!("Path '{}' does not exist", path);
    };

    if source_files.is_empty() && erl_files.is_empty() {
        miette::bail!("No .bt or .erl source files found in '{path}'");
    }

    Ok((source_files, erl_files))
}

/// Lint native `.erl` files and print diagnostics. Returns the total count.
fn lint_erl_files(erl_files: &[Utf8PathBuf], format: OutputFormat) -> Result<usize> {
    let mut count = 0;
    for erl_file in erl_files {
        let source = std::fs::read_to_string(erl_file)
            .into_diagnostic()
            .map_err(|e| miette::miette!("Failed to read '{}': {e}", erl_file))?;

        let diags = erlang_lint::lint_erl_file(erl_file, &source);

        for diag in &diags {
            match format {
                OutputFormat::Text => {
                    eprintln!(
                        "  × {}:{}:{}: {}",
                        diag.file, diag.line, diag.column, diag.message,
                    );
                    if let Some(hint) = &diag.hint {
                        eprintln!("  help: {hint}");
                    }
                    eprintln!();
                }
                OutputFormat::Json => {
                    let json = serde_json::json!({
                        "file": diag.file.as_str(),
                        "severity": "lint",
                        "message": diag.message,
                        "line": diag.line,
                        "column": diag.column,
                        "hint": diag.hint,
                    });
                    println!("{json}");
                }
            }
        }

        count += diags.len();
    }
    Ok(count)
}

/// Collect all `.bt` files in the package's conventional source directories
/// (`src/` and `test/`) plus any explicitly-targeted lint files that fall
/// outside those directories.
///
/// BT-2027: Used so that `beamtalk lint test/` or `beamtalk lint src/foo.bt`
/// extracts class metadata from the full package source set, not just the
/// path the user passed. Without this, a test file that references a `src/`
/// class produces spurious `Unresolved class` diagnostics.
///
/// BT-2060: Thin camino wrapper around
/// [`beamtalk_core::project::package::collect_package_source_files_with_errors`]
/// so MCP and CLI share the underlying implementation. Walk errors are logged
/// via the `tracing` stack that CLI already uses.
fn collect_package_class_files(
    package_root: &Utf8Path,
    target_files: &[Utf8PathBuf],
) -> Vec<Utf8PathBuf> {
    use std::collections::HashSet;

    let (files, errors) =
        package::collect_package_source_files_with_errors(package_root.as_std_path());
    for (dir, e) in errors {
        warn!(
            "failed to walk '{}' for cross-file class extraction: {e}",
            dir.display()
        );
    }

    // Dedup by canonical form: walked paths are absolute (`package_root` is
    // canonicalized upstream) but `target_files` often arrive as relative
    // user-typed paths (e.g. `test/Foo.bt`). Comparing raw `Utf8PathBuf`
    // would let the same file appear twice and get parsed twice.
    let mut seen: HashSet<Utf8PathBuf> = HashSet::new();
    let mut out: Vec<Utf8PathBuf> = Vec::new();

    for f in files {
        let utf8 = Utf8PathBuf::from_path_buf(f)
            .unwrap_or_else(|p| Utf8PathBuf::from(p.to_string_lossy().into_owned()));
        if seen.insert(canonicalize_or_clone(&utf8)) {
            out.push(utf8);
        }
    }

    // Ensure explicitly-targeted files are always included, even if they live
    // outside `src/`/`test/` (e.g. a one-off file at the package root).
    for f in target_files {
        if seen.insert(canonicalize_or_clone(f)) {
            out.push(f.clone());
        }
    }

    out
}

/// Returns the canonical filesystem form of `path`, falling back to a clone
/// when the path cannot be canonicalized (e.g. it does not yet exist). Used
/// as a normalized key for path-based deduplication.
fn canonicalize_or_clone(path: &Utf8Path) -> Utf8PathBuf {
    std::fs::canonicalize(path.as_std_path())
        .ok()
        .and_then(|p| Utf8PathBuf::from_path_buf(p).ok())
        .unwrap_or_else(|| path.to_path_buf())
}

/// Parse each lint target and collect class-info metadata from the package's
/// full source set (src/ + test/) so cross-file class resolution works for
/// partial-path lint targets (BT-2027).
///
/// Returns `(all_class_infos, parsed_files)` where `parsed_files` contains only
/// the files the user asked to lint; sibling files walked purely for
/// class-info extraction are dropped.
type ParsedLintFile = (
    Utf8PathBuf,
    String,
    beamtalk_core::ast::Module,
    Vec<beamtalk_core::source_analysis::Diagnostic>,
);

fn parse_and_extract_class_infos(
    source_files: &[Utf8PathBuf],
    package_root: Option<&Utf8Path>,
) -> Result<(
    Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    Vec<ParsedLintFile>,
)> {
    let extraction_files = match package_root {
        Some(root) => collect_package_class_files(root, source_files),
        None => source_files.to_vec(),
    };

    // Match by canonical form: `source_files` may be user-typed relative
    // paths while `extraction_files` contains absolute paths from the package
    // walk. Comparing raw `Utf8PathBuf` would drop relative targets from
    // `parsed_files` after dedup canonicalized them into walked form.
    let source_file_set: std::collections::HashSet<Utf8PathBuf> = source_files
        .iter()
        .map(|p| canonicalize_or_clone(p))
        .collect();
    let mut all_class_infos = Vec::new();
    let mut parsed_files: Vec<ParsedLintFile> = Vec::new();

    for file in &extraction_files {
        let source = std::fs::read_to_string(file)
            .into_diagnostic()
            .map_err(|e| miette::miette!("Failed to read '{}': {e}", file))?;

        let tokens = lex_with_eof(&source);
        let (module, parse_diags) = parse(tokens);

        all_class_infos
            .extend(beamtalk_core::semantic_analysis::ClassHierarchy::extract_class_infos(&module));

        if source_file_set.contains(&canonicalize_or_clone(file)) {
            parsed_files.push((file.clone(), source, module, parse_diags));
        }
    }

    Ok((all_class_infos, parsed_files))
}

/// Walk ancestors from the given path to find the package root (containing `beamtalk.toml`).
///
/// Returns `None` if no `beamtalk.toml` is found in any ancestor directory.
///
/// BT-2027: Relative paths like `test/` or `src/foo.bt` are canonicalized
/// before ancestor walking so that the search reaches the real package root
/// rather than bailing out when the short relative path runs out of parents.
///
/// BT-2060: Camino wrapper around
/// [`beamtalk_core::project::package::find_package_root`] so MCP and CLI share
/// the same implementation.
pub(crate) fn find_package_root(start: &Utf8Path) -> Option<Utf8PathBuf> {
    package::find_package_root(start.as_std_path()).and_then(|p| Utf8PathBuf::from_path_buf(p).ok())
}

/// Resolve dependency classes and merge them into the class info list.
///
/// Best-effort: if dependency resolution fails (e.g. network error for a git
/// dep), lint continues without dep classes rather than failing entirely.
fn resolve_dep_class_infos(
    project_root: &Utf8Path,
    all_class_infos: &mut Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
) {
    if !project_root.join("beamtalk.toml").exists() {
        return;
    }

    let options = beamtalk_core::CompilerOptions::default();
    match super::deps::ensure_deps_resolved(project_root, &options) {
        Ok(resolved_deps) => {
            for dep in &resolved_deps {
                all_class_infos.extend(dep.class_infos.clone());
            }
        }
        Err(e) => {
            warn!(
                error = %e,
                "Failed to resolve dependencies for lint; \
                 dependency classes may not be available"
            );
        }
    }
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

/// Convert a `DiagnosticSummary` into a `serde_json::Value` for the `--format json`
/// output and the MCP `diagnostic_summary` tool (BT-2014).
pub(crate) fn diagnostic_summary_to_json(
    summary: &beamtalk_core::source_analysis::DiagnosticSummary,
) -> serde_json::Value {
    use beamtalk_core::source_analysis::category_name;

    let totals = summary.totals_by_severity();
    let mut by_category = serde_json::Map::new();
    for (cat, counts) in &summary.by_category {
        by_category.insert(
            category_name(*cat).to_string(),
            serde_json::json!({
                "error": counts.error,
                "warning": counts.warning,
                "lint": counts.lint,
                "hint": counts.hint,
                "total": counts.total(),
            }),
        );
    }

    serde_json::json!({
        "type": "summary",
        "files_checked": summary.files_checked,
        "totals_by_severity": {
            "error": totals.error,
            "warning": totals.warning,
            "lint": totals.lint,
            "hint": totals.hint,
        },
        "totals_by_category": by_category,
        "total": summary.total(),
    })
}

/// Convenience wrapper for tests: parse source and collect lint diagnostics.
#[cfg(test)]
fn collect_lint_diagnostics(source: &str) -> Vec<beamtalk_core::source_analysis::Diagnostic> {
    let tokens = lex_with_eof(source);
    let (module, parse_diags) = parse(tokens);
    collect_diagnostics(&module, parse_diags, vec![])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expect_type_suppresses_dnu_hint_in_lint() {
        // BT-1547: @expect type must not be reported as stale when it
        // suppresses a real DNU hint from semantic analysis.
        // BT-1576: Updated — Result now has generic annotations, so
        // `Result ok: dict` infers unwrap -> Dictionary via constructor
        // inference. Use String (known type without `sqrt`) to trigger DNU.
        let source = r#"Object subclass: LintTest

  class demo =>
    s := "hello"
    @expect type
    val := s sqrt
    val
"#;
        let diags = collect_lint_diagnostics(source);
        let stale = diags.iter().any(|d| d.message.contains("stale @expect"));
        assert!(
            !stale,
            "@expect type should not be stale when DNU hint is present, got: {diags:?}"
        );
    }

    #[test]
    fn dnu_hint_shown_without_expect_type() {
        // Without @expect type, the DNU hint should appear in lint output.
        // BT-1576: Updated — Result now has generic annotations, so
        // `Result ok: dict` infers unwrap -> Dictionary via constructor
        // inference. Use String (known type without `sqrt`) to trigger DNU.
        let source = r#"Object subclass: LintTest2

  class demo =>
    s := "hello"
    val := s sqrt
    val
"#;
        let diags = collect_lint_diagnostics(source);
        let has_dnu = diags
            .iter()
            .any(|d| d.message.contains("does not understand"));
        assert!(
            has_dnu,
            "DNU hint should be present in lint diagnostics, got: {diags:?}"
        );
    }

    #[test]
    fn expect_type_still_stale_without_dnu() {
        // @expect type on an expression with no DNU or type diagnostic
        // should still be reported as stale.
        let source = "Object subclass: StaleTest\n\n  class demo =>\n    @expect type\n    42\n";
        let diags = collect_lint_diagnostics(source);
        let stale = diags.iter().any(|d| d.message.contains("stale @expect"));
        assert!(
            stale,
            "@expect type on `42` must emit stale warning, got: {diags:?}"
        );
    }

    #[test]
    fn expect_all_not_stale_with_cross_file_actor_class() {
        // When cross-file class info tells lint that MyActor is an Actor,
        // `@expect all` on `MyActor new` should not be stale — the
        // instantiation_error diagnostic is emitted.
        let actor_source = "Actor subclass: MyActor\n  run => 42\n";
        let actor_tokens = lex_with_eof(actor_source);
        let (actor_module, _) = parse(actor_tokens);
        let cross_file_classes =
            beamtalk_core::semantic_analysis::ClassHierarchy::extract_class_infos(&actor_module);

        let test_source = r"Object subclass: TestFile

  class demo =>
    @expect all
    MyActor new
";
        let tokens = lex_with_eof(test_source);
        let (module, parse_diags) = parse(tokens);
        let diags = collect_diagnostics(&module, parse_diags, cross_file_classes);
        let stale = diags.iter().any(|d| d.message.contains("stale @expect"));
        assert!(
            !stale,
            "@expect all should not be stale when cross-file Actor class info is provided, got: {diags:?}"
        );
    }

    #[test]
    fn find_package_root_from_subdir() {
        let temp = tempfile::TempDir::new().unwrap();
        let root = temp.path();
        std::fs::write(
            root.join("beamtalk.toml"),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        let src = root.join("src");
        std::fs::create_dir_all(&src).unwrap();

        // Use the canonical form of root for the expected value: on macOS
        // `/tmp` resolves to `/private/tmp`; on Windows short/long path names
        // differ. `find_package_root` canonicalises internally, so the expected
        // value must do the same to match.
        let root_utf8 =
            camino::Utf8PathBuf::from_path_buf(std::fs::canonicalize(root).unwrap()).unwrap();
        let src_utf8 =
            camino::Utf8PathBuf::from_path_buf(std::fs::canonicalize(&src).unwrap()).unwrap();

        // From subdir, should find parent
        assert_eq!(find_package_root(&src_utf8), Some(root_utf8.clone()));

        // From root itself, should find it directly
        assert_eq!(find_package_root(&root_utf8), Some(root_utf8.clone()));
    }

    #[test]
    fn find_package_root_from_file() {
        let temp = tempfile::TempDir::new().unwrap();
        let root = temp.path();
        std::fs::write(
            root.join("beamtalk.toml"),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        let src = root.join("src");
        std::fs::create_dir_all(&src).unwrap();
        std::fs::write(src.join("foo.bt"), "Object subclass: Foo\n").unwrap();

        let file_utf8 = camino::Utf8PathBuf::from_path_buf(src.join("foo.bt")).unwrap();
        // Canonicalise root — see find_package_root_from_subdir for rationale.
        let root_utf8 =
            camino::Utf8PathBuf::from_path_buf(std::fs::canonicalize(root).unwrap()).unwrap();

        assert_eq!(find_package_root(&file_utf8), Some(root_utf8));
    }

    #[test]
    fn find_package_root_none_without_manifest() {
        let temp = tempfile::TempDir::new().unwrap();
        let dir = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        assert_eq!(find_package_root(&dir), None);
    }

    #[test]
    fn expect_all_stale_without_cross_file_actor_class() {
        // Without cross-file class info, lint can't know MyActor is an Actor,
        // so `@expect all` would be stale (no diagnostic emitted).
        let test_source = r"Object subclass: TestFile2

  class demo =>
    @expect all
    MyActor new
";
        let diags = collect_lint_diagnostics(test_source);
        let stale = diags.iter().any(|d| d.message.contains("stale @expect"));
        assert!(
            stale,
            "@expect all should be stale without cross-file class info, got: {diags:?}"
        );
    }

    /// BT-2014: Verify that `DiagnosticSummary` text and JSON representations
    /// report the same counts for the same set of diagnostics.
    #[test]
    #[allow(clippy::cast_possible_truncation)]
    fn summary_text_and_json_match() {
        let source = r#"Object subclass: SummaryTest

  class demo =>
    s := "hello"
    val := s sqrt
    val
"#;
        let diags = collect_lint_diagnostics(source);
        let summary =
            beamtalk_core::source_analysis::DiagnosticSummary::from_diagnostics(&diags, 1);

        // Text representation
        let text = summary.to_string();

        // JSON representation
        let json = diagnostic_summary_to_json(&summary);

        // The JSON total must match the summary total.
        assert_eq!(
            json["total"].as_u64().unwrap() as usize,
            summary.total(),
            "JSON total must match DiagnosticSummary::total()"
        );

        // files_checked must match.
        assert_eq!(
            json["files_checked"].as_u64().unwrap() as usize,
            summary.files_checked,
            "JSON files_checked must match"
        );

        // Totals by severity must match.
        let totals = summary.totals_by_severity();
        let sev = &json["totals_by_severity"];
        assert_eq!(sev["error"].as_u64().unwrap() as usize, totals.error);
        assert_eq!(sev["warning"].as_u64().unwrap() as usize, totals.warning);
        assert_eq!(sev["lint"].as_u64().unwrap() as usize, totals.lint);
        assert_eq!(sev["hint"].as_u64().unwrap() as usize, totals.hint);

        // The text must contain "Diagnostic summary (1 file):" header.
        assert!(
            text.contains("Diagnostic summary (1 file):"),
            "Text should contain file count header, got: {text}"
        );

        // The text must contain the total.
        assert!(
            text.contains(&format!("Total{:>15}", summary.total())),
            "Text should contain total count, got: {text}"
        );
    }

    /// BT-2027: Regression — linting `test/` in a package must pull class
    /// infos from sibling `src/` so references to src-defined classes resolve.
    #[test]
    fn lint_on_test_dir_resolves_sibling_src_classes() {
        let temp = tempfile::TempDir::new().unwrap();
        let root = temp.path();
        std::fs::write(
            root.join("beamtalk.toml"),
            "[package]\nname = \"xpkg\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        let src = root.join("src");
        let test = root.join("test");
        std::fs::create_dir_all(&src).unwrap();
        std::fs::create_dir_all(&test).unwrap();
        std::fs::write(
            src.join("foo.bt"),
            "Object subclass: Foo\n  class demo => 42\n",
        )
        .unwrap();
        std::fs::write(
            test.join("foo_test.bt"),
            "Object subclass: FooTest\n  class run =>\n    Foo demo\n",
        )
        .unwrap();

        // Emulate what run_lint does: walk the `test/` directory, but extract
        // class infos from the full package source set (src/ + test/).
        let test_utf8 = camino::Utf8PathBuf::from_path_buf(test.clone()).unwrap();
        let test_files = collect_source_files_from_dir(&test_utf8).unwrap();
        let pkg_root = find_package_root(&test_utf8).expect("package root must be found");
        let extraction_files = collect_package_class_files(&pkg_root, &test_files);

        let mut all_class_infos = Vec::new();
        for file in &extraction_files {
            let source = std::fs::read_to_string(file).unwrap();
            let tokens = lex_with_eof(&source);
            let (module, _) = parse(tokens);
            all_class_infos.extend(
                beamtalk_core::semantic_analysis::ClassHierarchy::extract_class_infos(&module),
            );
        }

        // Now lint-analyse the test file with the full class info set.
        let test_source = std::fs::read_to_string(test.join("foo_test.bt")).unwrap();
        let tokens = lex_with_eof(&test_source);
        let (module, parse_diags) = parse(tokens);
        let cross_file_classes =
            beamtalk_core::semantic_analysis::ClassHierarchy::cross_file_class_infos(
                &all_class_infos,
                &module,
            );
        let diags = collect_diagnostics(&module, parse_diags, cross_file_classes);

        let unresolved: Vec<_> = diags
            .iter()
            .filter(|d| {
                d.category
                    == Some(beamtalk_core::source_analysis::DiagnosticCategory::UnresolvedClass)
            })
            .collect();
        assert!(
            unresolved.is_empty(),
            "test/ file should resolve src/ classes, got unresolved: {unresolved:?}"
        );
    }

    /// BT-2027: `collect_package_class_files` must dedup across the absolute
    /// paths produced by walking `src/`/`test/` and the relative paths a user
    /// may pass as explicit lint targets. Without canonical-form dedup the
    /// same file would appear twice in the extraction list and be parsed
    /// twice downstream.
    #[test]
    #[serial_test::serial(cwd)]
    fn collect_package_class_files_dedups_absolute_and_relative() {
        let temp = tempfile::TempDir::new().unwrap();
        let root = temp.path();
        std::fs::write(
            root.join("beamtalk.toml"),
            "[package]\nname = \"dp\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        let test = root.join("test");
        std::fs::create_dir_all(&test).unwrap();
        std::fs::write(test.join("foo.bt"), "Object subclass: Foo\n").unwrap();

        let prev_cwd = std::env::current_dir().unwrap();
        std::env::set_current_dir(root).unwrap();

        // Walked form is absolute (under canonical package_root); user-typed
        // target is relative. Both refer to the same file.
        let pkg_root = find_package_root(&camino::Utf8PathBuf::from("test")).expect("package root");
        let relative_target = camino::Utf8PathBuf::from("test/foo.bt");
        let out = collect_package_class_files(&pkg_root, std::slice::from_ref(&relative_target));

        std::env::set_current_dir(prev_cwd).unwrap();

        assert_eq!(
            out.len(),
            1,
            "expected single entry after canonical-form dedup, got {out:?}"
        );
    }

    /// BT-2027: `find_package_root` must work for relative paths like `test/`
    /// by canonicalizing the start path.
    ///
    /// Serialized on `cwd` because it temporarily mutates the process working
    /// directory, matching the convention used by tests in `run.rs` / `test.rs`.
    #[test]
    #[serial_test::serial(cwd)]
    fn find_package_root_canonicalizes_relative_paths() {
        let temp = tempfile::TempDir::new().unwrap();
        let root = temp.path();
        std::fs::write(
            root.join("beamtalk.toml"),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        let test_dir = root.join("test");
        std::fs::create_dir_all(&test_dir).unwrap();

        // Run the check from the package root with a relative argument.
        let prev_cwd = std::env::current_dir().unwrap();
        std::env::set_current_dir(root).unwrap();
        let relative = camino::Utf8PathBuf::from("test");
        let found = find_package_root(&relative);
        std::env::set_current_dir(prev_cwd).unwrap();

        let expected = camino::Utf8PathBuf::from_path_buf(root.canonicalize().unwrap()).unwrap();
        assert_eq!(found, Some(expected));
    }
}
