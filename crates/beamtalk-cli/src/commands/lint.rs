// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk lint` — run style/redundancy lint checks on Beamtalk source files.
//!
//! This command parses each `.bt` file, runs all lint passes, and reports
//! [`Severity::Lint`] diagnostics. It also runs semantic analysis to collect
//! DNU hint diagnostics so that `@expect type` annotations are correctly
//! applied (BT-1547). It exits non-zero if any unsuppressed lint diagnostics
//! are found.
//!
//! Lint diagnostics are suppressed during normal `check`/`compile` — this is
//! the only command that surfaces them.

use crate::commands::build::collect_source_files_from_dir;
use crate::diagnostic::CompileDiagnostic;
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

    // Pass 1: Parse all files and extract class metadata so that cross-file
    // type/DNU diagnostics in Pass 2 match what `build` emits. Without this,
    // `@expect` annotations that suppress real cross-file diagnostics during
    // build would be reported as stale by lint.
    let mut all_class_infos = Vec::new();
    let mut parsed_files: Vec<(
        Utf8PathBuf,
        String,
        beamtalk_core::ast::Module,
        Vec<beamtalk_core::source_analysis::Diagnostic>,
    )> = Vec::new();

    for file in &source_files {
        let source = std::fs::read_to_string(file)
            .into_diagnostic()
            .map_err(|e| miette::miette!("Failed to read '{}': {e}", file))?;

        let tokens = lex_with_eof(&source);
        let (module, parse_diags) = parse(tokens);

        all_class_infos
            .extend(beamtalk_core::semantic_analysis::ClassHierarchy::extract_class_infos(&module));

        parsed_files.push((file.clone(), source, module, parse_diags));
    }

    // Resolve dependency class metadata so lint sees the same class hierarchy
    // as build. Without this, @expect annotations that suppress real cross-package
    // diagnostics would be reported as stale.
    let project_root = if source_path.is_dir() {
        source_path.clone()
    } else {
        source_path
            .parent()
            .map_or_else(|| Utf8PathBuf::from("."), Utf8Path::to_path_buf)
    };
    resolve_dep_class_infos(&project_root, &mut all_class_infos);

    // Pass 2: Analyse each file with cross-file class context.
    let mut total_lint_count = 0usize;

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
}
