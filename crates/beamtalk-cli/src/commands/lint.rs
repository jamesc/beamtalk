// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk lint` — run style/redundancy lint checks on source files.
//!
//! This command parses each `.bt` file, runs all lint passes, and reports
//! [`Severity::Lint`] diagnostics. It also runs semantic analysis to collect
//! DNU hint diagnostics so that `@expect type` annotations are correctly
//! applied. It exits non-zero if any unsuppressed lint diagnostics
//! are found.
//!
//! Native `.erl` files in `native/` and `native/test/` are also checked for
//! missing `-moduledoc`/`-doc` attributes and hardcoded `'bt@...'` module
//! references.
//!
//! Lint diagnostics are suppressed during normal `check`/`compile` — this is
//! the only command that surfaces them.
//!
//! With `--warnings-as-errors`, the command additionally fails on the
//! live-recheck set (non-error `Dnu`/`Type` diagnostics plus any `Error`,
//! including parse errors), matching a REPL `load-project` verdict
//! (BT-4334). The package's `[diagnostics]` table is applied first, as in
//! `build` and the REPL (BT-4377).

use crate::commands::OutputFormat;
use crate::diagnostic::CompileDiagnostic;
use beamtalk_cli::lint_pipeline::{self, DependencyMetadata, LintGate};
#[cfg(test)]
use beamtalk_cli::lint_pipeline::{FileContext, analyse_file, dedup_pre_loaded_alias_collision};
pub(crate) use beamtalk_cli::lint_pipeline::{collect_lint_files, find_package_root};
#[cfg(test)]
use beamtalk_core::source_analysis::{lex_with_eof, parse};
use beamtalk_project::package;
use camino::{Utf8Path, Utf8PathBuf};
use miette::Result;
use tracing::warn;

/// Run lint passes on the given path (file or directory).
///
/// Prints each lint diagnostic and returns an error if any are found.
/// With `warnings_as_errors`, live-recheck diagnostics (non-error
/// `Dnu`/`Type` findings, plus any `Error`) also fail the command, so the
/// buccaneer-style sources the REPL `load-project` rejects fail CI here too
/// (BT-4334).
///
/// The analysis is [`beamtalk_cli::lint_pipeline`], shared with the MCP
/// `lint` tool (BT-4585); this function supplies the CLI's dependency
/// resolution and FFI type registry and prints the results.
pub fn run_lint(path: &str, format: OutputFormat, warnings_as_errors: bool) -> Result<()> {
    let source_path = Utf8PathBuf::from(path);
    let (source_files, erl_files) = collect_lint_files(&source_path, path)?;

    // Pass 1: parse the full package source set — not only the targets —
    // so cross-file type/DNU diagnostics in Pass 2 match what `build` emits.
    // Without this, `@expect` annotations that suppress real cross-file
    // diagnostics during build would be reported as stale by lint, and a
    // test file referencing a `src/` class would report it unresolved.
    let mut unreadable: Option<miette::Report> = None;
    let sources =
        lint_pipeline::parse_package_sources(&source_path, &source_files, |file, _, e| {
            unreadable.get_or_insert_with(|| miette::miette!("Failed to read '{file}': {e}"));
        });
    if let Some(e) = unreadable {
        return Err(e);
    }

    // Merge dependency class/protocol/alias metadata so lint sees the same
    // cross-package picture as build. The resolved deps are also needed
    // below for their `stubs_dir`s (ADR 0075 layer 2).
    let (resolved_deps, dependencies) = match sources.package_root.as_deref() {
        Some(project_root) => merge_dependency_infos(project_root),
        None => (Vec::new(), DependencyMetadata::default()),
    };

    let native_type_registry = lint_type_registry(
        sources.package_root.as_deref(),
        &source_files,
        &resolved_deps,
        format,
    );

    // Pass 2: analyse each file with cross-file class context.
    let mut gate = LintGate::default();
    let mut all_diags: Vec<beamtalk_core::source_analysis::Diagnostic> = Vec::new();
    let provision_diags = lint_pipeline::analyse_package(
        sources,
        dependencies,
        native_type_registry.as_ref(),
        |file| {
            for diag in &file.diagnostics {
                print_diagnostic(format, file.path.as_str(), file.source, diag);
                gate.count(diag);
            }
            all_diags.extend(file.diagnostics);
        },
    );

    // Each flattened-provision diagnostic once, in its protocol's file.
    for routed in provision_diags {
        print_diagnostic(format, &routed.path, &routed.source, &routed.diagnostic);
        gate.count(&routed.diagnostic);
        all_diags.push(routed.diagnostic);
    }

    // Lint native .erl files.
    gate.count_erlang(lint_erl_files(&erl_files, format)?);

    // Build and print diagnostic summary.
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
            // Per-diagnostic JSON lines were already streamed above.
            // Emit the summary as a final JSON object.
            let summary_json = diagnostic_summary_to_json(&summary);
            println!("{summary_json}");
        }
    }

    // Stale `@expect` warnings are displayed but do not fail the command:
    // only Lint-severity diagnostics count toward the default gate.
    if gate.lint > 0 {
        let count = gate.lint;
        let plural = if count == 1 { "" } else { "s" };
        miette::bail!("{count} lint diagnostic{plural} found in {total_files_checked} file(s)");
    }

    // BT-4334: the REPL-parity gate. Reported separately so the default
    // lint-severity message above stays unchanged for existing callers.
    if gate.fails(warnings_as_errors) {
        let count = gate.live_recheck;
        let plural = if count == 1 { "" } else { "s" };
        miette::bail!(
            "{count} type/DNU/error diagnostic{plural} found in {total_files_checked} file(s) (--warnings-as-errors: REPL load-project parity)"
        );
    }

    Ok(())
}

/// Print one `.bt` diagnostic: miette-rendered for text, one JSON line for
/// JSON (streamed rather than buffered).
fn print_diagnostic(
    format: OutputFormat,
    file: &str,
    source: &str,
    diag: &beamtalk_core::source_analysis::Diagnostic,
) {
    match format {
        OutputFormat::Text => {
            let compile_diag = CompileDiagnostic::from_core_diagnostic(diag, file, source);
            eprintln!("{:?}", miette::Report::new(compile_diag));
        }
        OutputFormat::Json => {
            println!("{}", crate::diagnostic::diagnostic_to_json(file, diag));
        }
    }
}

/// The FFI type registry `beamtalk lint` analyses against.
///
/// Built via the same `extract_type_specs` that `beamtalk build` calls,
/// instead of only reading whatever `_build/type_cache/` happens to hold — a
/// cache written by a *previous* build let lint's view of FFI types drift
/// from build's, and an `@expect type` written to suppress a real build-time
/// FFI diagnostic was then flagged as stale by lint. The full ADR 0075 stub
/// resolution chain overrides auto-extract at the function/arity level —
/// the same merge `beamtalk build` performs, via the same shared loaders.
/// These loaders print their own diagnostics (skipped signatures, version
/// drift) directly.
///
/// Stdlib has no `beamtalk.toml`, so `package_root` is `None` when linting
/// it; the registry then comes from `beamtalk build-stdlib`'s own extractor
/// (`extract_stdlib_type_specs`, ADR 0075) instead of being `None`, which
/// would report every `(Erlang m) f:` call in `stdlib/src` as untyped.
fn lint_type_registry(
    package_root: Option<&Utf8Path>,
    source_files: &[Utf8PathBuf],
    resolved_deps: &[super::deps::path::ResolvedDependency],
    format: OutputFormat,
) -> Option<std::sync::Arc<beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>> {
    if let Some(root) = package_root {
        let layout = crate::commands::build_layout::BuildLayout::new(root);
        let auto_extract = super::build::extract_type_specs(&layout, true, false);
        super::build::resolve_package_type_registry(root, auto_extract, resolved_deps, Some(format))
            .registry
            .map(std::sync::Arc::new)
    } else if source_files
        .iter()
        .any(|f| package::is_under_stdlib_src_dir(f.as_std_path()))
    {
        super::build_stdlib::extract_stdlib_type_specs().map(std::sync::Arc::new)
    } else {
        None
    }
}

/// Lint native `.erl` files and print diagnostics. Returns the total count.
fn lint_erl_files(erl_files: &[Utf8PathBuf], format: OutputFormat) -> Result<usize> {
    let mut count = 0;
    for erl_file in erl_files {
        let diags = lint_pipeline::lint_erl_file(erl_file)
            .map_err(|e| miette::miette!("Failed to read '{}': {e}", erl_file))?;

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

/// Resolve the project's dependencies and collect their class, protocol and
/// alias metadata and extension methods (ADR 0070 WS3, BT-4608) for the
/// shared pipeline.
///
/// Best-effort: if dependency resolution fails (e.g. network error for a git
/// dep), lint continues without dependency metadata — and with the pre-WS3
/// dependency guard on — rather than failing entirely. Also returns the
/// resolved dependencies (empty on failure) for their `stubs_dir`s (ADR 0075
/// layer 2); see [`lint_type_registry`].
fn merge_dependency_infos(
    project_root: &Utf8Path,
) -> (
    Vec<super::deps::path::ResolvedDependency>,
    DependencyMetadata,
) {
    let options = beamtalk_core::CompilerOptions::default();
    match super::deps::ensure_deps_resolved(project_root, &options) {
        Ok(resolved_deps) => {
            let mut metadata = DependencyMetadata::default();
            metadata.extensions_complete =
                beamtalk_cli::dependency_extensions::merge_dependency_extensions(
                    resolved_deps.iter().map(|dep| &dep.extensions),
                    &mut metadata.extensions,
                );
            for dep in &resolved_deps {
                metadata.class_infos.extend(dep.class_infos.clone());
                metadata.protocol_infos.extend(dep.protocol_infos.clone());
                metadata.protocol_defs.extend(dep.protocol_defs.clone());
                // First definition wins.
                beamtalk_core::semantic_analysis::protocol_source::merge_protocol_sources(
                    &mut metadata.protocol_sources,
                    dep.protocol_sources.clone(),
                );
                metadata.alias_infos.extend(dep.alias_infos.clone());
            }
            (resolved_deps, metadata)
        }
        Err(e) => {
            warn!(
                error = %e,
                "Failed to resolve dependencies for lint; \
                 dependency classes/protocols/aliases may not be available"
            );
            (Vec::new(), DependencyMetadata::default())
        }
    }
}

/// Convert a `DiagnosticSummary` into a `serde_json::Value` for the `--format json`
/// output and the MCP `diagnostic_summary` tool.
pub(crate) fn diagnostic_summary_to_json(
    summary: &beamtalk_core::source_analysis::DiagnosticSummary,
) -> serde_json::Value {
    serde_json::json!({
        "type": "summary",
        "files_checked": summary.files_checked,
        "totals_by_severity": summary.severity_totals_json(),
        "totals_by_category": summary.categories_to_json_map(),
        "total": summary.total(),
    })
}

/// Convenience wrapper for tests: parse source and collect lint diagnostics.
#[cfg(test)]
fn collect_lint_diagnostics(source: &str) -> Vec<beamtalk_core::source_analysis::Diagnostic> {
    collect_lint_diagnostics_with_stub_flag(source, false)
}

/// As [`collect_lint_diagnostics`], but with `is_stub_file` set explicitly —
/// Regression test for a direct-file-target `beamtalk lint` on a
/// genuine `stubs/` file needs `is_stub_file: true` to reproduce what
/// `analyse_file`'s real caller now derives via `is_under_stubs_dir`.
#[cfg(test)]
fn collect_lint_diagnostics_with_stub_flag(
    source: &str,
    is_stub_file: bool,
) -> Vec<beamtalk_core::source_analysis::Diagnostic> {
    let tokens = lex_with_eof(source);
    let (module, parse_diags) = parse(tokens);
    analyse_file(
        &module,
        source,
        parse_diags,
        FileContext {
            is_stub_file,
            ..FileContext::default()
        },
    )
    .0
}

/// As [`collect_lint_diagnostics`], but with `file_stem` set explicitly —
/// Regression test for `beamtalk lint` reporting the same
/// file-name/class-name mismatch `beamtalk build`/the LSP do.
#[cfg(test)]
fn collect_lint_diagnostics_with_file_stem(
    source: &str,
    file_stem: Option<&str>,
) -> Vec<beamtalk_core::source_analysis::Diagnostic> {
    let tokens = lex_with_eof(source);
    let (module, parse_diags) = parse(tokens);
    analyse_file(
        &module,
        source,
        parse_diags,
        FileContext {
            file_stem,
            ..FileContext::default()
        },
    )
    .0
}

/// As [`collect_lint_diagnostics`], but with `stdlib_mode` set explicitly —
/// Regression test for BT-lint-stdlib-false-positives: `beamtalk lint
/// stdlib/src/...` must not flag a stdlib class overriding a sealed method
/// (e.g. `Uuid class new:`) the way it would for ordinary user code.
#[cfg(test)]
fn collect_lint_diagnostics_with_stdlib_flag(
    source: &str,
    stdlib_mode: bool,
) -> Vec<beamtalk_core::source_analysis::Diagnostic> {
    let tokens = lex_with_eof(source);
    let (module, parse_diags) = parse(tokens);
    analyse_file(
        &module,
        source,
        parse_diags,
        FileContext {
            stdlib_mode,
            ..FileContext::default()
        },
    )
    .0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expect_type_suppresses_dnu_hint_in_lint() {
        // @expect type must not be reported as stale when it
        // suppresses a real DNU hint from semantic analysis.
        // Result has generic annotations, so
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
    fn declare_native_outside_stubs_is_reported() {
        // A `declare native:` block outside `stubs/` (is_stub_file:
        // false, what `analyse_file`'s real caller derives for a
        // direct file-target NOT under `stubs/`) must be reported.
        let source = "declare native: lists\n";
        let diags = collect_lint_diagnostics_with_stub_flag(source, false);
        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("only valid in stubs/ directory")),
            "declare native: outside stubs/ should be reported: {diags:?}"
        );
    }

    #[test]
    fn declare_native_inside_stubs_is_not_reported() {
        // Giving `check_native_declaration_location`'s
        // diagnostic a category (so it survives `analyse_file`'s
        // `category.is_some()` filter) would otherwise newly surface a FALSE
        // positive on a legitimate stub file reached via a direct
        // single-file lint target — `collect_lint_files`'s `stubs/`
        // exclusion only covers its directory-walk branch, so the real
        // caller must derive `is_stub_file` itself (via
        // `beamtalk_project::package::is_under_stubs_dir`) rather than
        // relying on that exclusion. This test pins the `is_stub_file: true`
        // half directly, mirroring how a real `stubs/lists.bt` target is
        // classified.
        let source = "declare native: lists\n";
        let diags = collect_lint_diagnostics_with_stub_flag(source, true);
        assert!(
            !diags
                .iter()
                .any(|d| d.message.contains("only valid in stubs/ directory")),
            "declare native: inside stubs/ should not be reported: {diags:?}"
        );
    }

    #[test]
    fn sealed_override_outside_stdlib_mode_is_reported() {
        // `Uuid` is a real generated stdlib builtin (present in
        // `generated_builtins.rs`), so `is_builtin_class("Uuid")` is true —
        // but with `stdlib_mode: false` (ordinary user code, or `beamtalk
        // lint` before this fix on any target) the sealed-override
        // exemption never applies, so overriding `Value`'s sealed `new:`
        // must still be reported.
        let source = "sealed typed Value subclass: Uuid\n  class new: x :: Object -> Nil => self error: \"nope\"\n";
        let diags = collect_lint_diagnostics_with_stdlib_flag(source, false);
        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Cannot override sealed method")),
            "sealed override outside stdlib_mode should be reported: {diags:?}"
        );
    }

    #[test]
    fn sealed_override_inside_stdlib_mode_is_not_reported() {
        // Regression test for the standalone `beamtalk lint stdlib/src`
        // false positive: a real stdlib class (`Uuid`) legitimately
        // overriding a sealed class method (`Value class new:`, to raise a
        // pointed "use v4/v7/fromString:" error instead of constructing)
        // must NOT be reported once the caller correctly derives
        // `stdlib_mode: true` for a file under `stdlib/src` — matching what
        // `beamtalk build-stdlib` (which always sets `stdlib_mode: true`)
        // already accepts.
        let source = "sealed typed Value subclass: Uuid\n  class new: x :: Object -> Nil => self error: \"nope\"\n";
        let diags = collect_lint_diagnostics_with_stdlib_flag(source, true);
        assert!(
            !diags
                .iter()
                .any(|d| d.message.contains("Cannot override sealed method")),
            "sealed override inside stdlib_mode should not be reported: {diags:?}"
        );
    }

    #[test]
    fn mismatched_file_name_is_reported() {
        // `beamtalk lint` must report the same file-name/class-name
        // mismatch `beamtalk build`/the LSP do, closing the surface-parity
        // gap `check_class_file_name_agreement` would otherwise have only on
        // this surface (the `.category.is_some()` filter alone wouldn't have
        // dropped it — `FileClassNameMismatch` is a real category — the gap
        // is that nothing else calls the check on this surface).
        let source = "Value subclass: ExduraEvent";
        let diags = collect_lint_diagnostics_with_file_stem(source, Some("event"));
        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("does not match declared class")),
            "mismatched file name should be reported: {diags:?}"
        );
    }

    #[test]
    fn matching_file_name_is_not_reported() {
        // Negative control.
        let source = "Value subclass: ExduraEvent";
        let diags = collect_lint_diagnostics_with_file_stem(source, Some("exdura_event"));
        assert!(
            !diags
                .iter()
                .any(|d| d.message.contains("does not match declared class")),
            "matching file name should not be reported: {diags:?}"
        );
    }

    #[test]
    fn dnu_hint_shown_without_expect_type() {
        // Without @expect type, the DNU hint should appear in lint output.
        // Result now has generic annotations, so
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

    // ── near-miss `// === Name ===` divider ─────────────────────────────
    //
    // These exercise `analyse_file` through its real entry point
    // (`collect_lint_diagnostics`, mirroring `run_lint`'s Pass 2 call), not
    // `near_miss_divider::scan_source` directly (already covered by that
    // module's own `scan_source_locates_the_near_miss_comment_line_precisely`
    // test) — proving the CLI wiring itself, not just the underlying scan.

    #[test]
    fn near_miss_divider_span_points_at_the_comment_line_not_the_declaration() {
        // `analyse_file` threads `source` through so this diagnostic's
        // span covers the comment line itself, not the AST's `Comment::span`
        // (which is actually `bar`'s token span — the *following*
        // declaration). Slicing `source` by the reported span proves it
        // covers exactly the comment line.
        let source = "Object subclass: Foo\n  // === Section ====\n  bar => 1\n";
        let diags = collect_lint_diagnostics(source);
        let near_misses: Vec<_> = diags
            .iter()
            .filter(|d| d.message.contains("section divider"))
            .collect();
        assert_eq!(
            near_misses.len(),
            1,
            "expected exactly one near-miss-divider diagnostic: {diags:?}"
        );
        assert_eq!(
            &source[near_misses[0].span.as_range()],
            "  // === Section ====\n",
            "span should cover exactly the comment's own line, not the method below it"
        );
    }

    #[test]
    fn near_miss_divider_multiple_occurrences_get_distinct_correctly_attributed_spans() {
        // Two near-misses in one file must not get their spans mixed up —
        // each diagnostic's span must slice back to its own comment line,
        // not the other one's.
        let source = "Object subclass: Foo\n  // === First ====\n  bar => 1\n\n  // == Second ==\n  baz => 2\n";
        let diags = collect_lint_diagnostics(source);
        let mut near_misses: Vec<_> = diags
            .iter()
            .filter(|d| d.message.contains("section divider"))
            .collect();
        assert_eq!(
            near_misses.len(),
            2,
            "expected exactly two near-miss-divider diagnostics: {diags:?}"
        );
        near_misses.sort_by_key(|d| d.span.start());

        assert_eq!(
            &source[near_misses[0].span.as_range()],
            "  // === First ====\n",
            "first near-miss's span should cover only its own comment line"
        );
        assert_eq!(
            &source[near_misses[1].span.as_range()],
            "  // == Second ==\n",
            "second near-miss's span should cover only its own comment line, not the first's"
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
        let diags = analyse_file(
            &module,
            test_source,
            parse_diags,
            FileContext {
                cross_file_classes,
                ..FileContext::default()
            },
        )
        .0;
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

    /// Verify that `DiagnosticSummary` text and JSON representations
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

    /// BT-3665: lint carries each carried protocol's source path so
    /// a provision diagnostic can be published in the protocol's file once.
    #[test]
    fn lint_extraction_records_protocol_source_identity() {
        let temp = tempfile::TempDir::new().unwrap();
        // Canonicalise root — the recorded protocol source path is the
        // canonical spelling; see find_package_root_from_subdir.
        let root = Utf8PathBuf::from_path_buf(std::fs::canonicalize(temp.path()).unwrap()).unwrap();
        std::fs::write(
            root.join("beamtalk.toml"),
            "[package]\nname = \"xpkg\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        std::fs::create_dir_all(root.join("src")).unwrap();
        let proto = root.join("src").join("broken.bt");
        let text = "Protocol define: Broken\n  name -> String\n\n  probe -> Integer => 3 bogus\n";
        std::fs::write(&proto, text).unwrap();
        let user = root.join("src").join("alpha.bt");
        std::fs::write(
            &user,
            "Object subclass: Alpha\n  uses: Broken\n  name -> String => \"a\"\n",
        )
        .unwrap();

        let parsed =
            lint_pipeline::parse_package_sources(&user, std::slice::from_ref(&user), |f, _, e| {
                panic!("unreadable {f}: {e}")
            });
        assert_eq!(parsed.protocol_defs.len(), 1);
        let src = parsed
            .trait_users
            .protocol_sources()
            .get("Broken")
            .expect("protocol source recorded");
        assert_eq!(src.path.as_deref(), Some(proto.as_str()));
        assert_eq!(src.text.as_str(), text);
        assert_eq!(src.package.as_deref(), Some("xpkg"));
    }

    /// Regression — linting `test/` in a package must pull class
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
        let test_files = crate::commands::build::collect_source_files_from_dir(&test_utf8).unwrap();
        let all_class_infos =
            lint_pipeline::parse_package_sources(&test_utf8, &test_files, |f, _, e| {
                panic!("unreadable {f}: {e}")
            })
            .class_infos;

        // Now lint-analyse the test file with the full class info set.
        let test_source = std::fs::read_to_string(test.join("foo_test.bt")).unwrap();
        let tokens = lex_with_eof(&test_source);
        let (module, parse_diags) = parse(tokens);
        let cross_file_classes =
            beamtalk_core::semantic_analysis::ClassHierarchy::cross_file_class_infos(
                &all_class_infos,
                &module,
            );
        let diags = analyse_file(
            &module,
            &test_source,
            parse_diags,
            FileContext {
                cross_file_classes,
                ..FileContext::default()
            },
        )
        .0;

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

    /// Acceptance criterion: `beamtalk lint` must resolve a
    /// dependency's exported protocol and public type alias — mirroring what
    /// `beamtalk build` already does via `ResolvedDependency.protocol_infos`/
    /// `.alias_infos` — while a dependency's `internal type` stays excluded.
    ///
    /// Sets up a two-package path-dependency fixture on disk: `producer`
    /// exports a public `Status` alias, an `internal` `Secret` alias, and a
    /// `Greetable` protocol; `consumer` depends on `producer` (path dep) and
    /// references all three names without declaring any of them itself.
    ///
    /// `producer`'s ebin/provenance stamp is faked (mirroring
    /// `commands::deps::mod::tests::create_dep_ebin_with_beam`) so
    /// `ensure_deps_resolved`'s "deps are fresh" fast path
    /// (`collect_fresh_deps` → `build_dep_class_index`, pure parsing, no
    /// `erlc`) is used instead of a real compile — keeping this test fast
    /// and runnable without an Erlang toolchain.
    #[test]
    #[allow(clippy::too_many_lines)] // Two-package fixture setup + assertions; splitting would obscure the flow
    fn lint_resolves_dependency_protocol_and_public_alias_but_not_internal_alias() {
        let temp = tempfile::TempDir::new().unwrap();
        // Canonicalise root — parsed files carry the canonical spelling, which
        // `consumer_file` is compared against below; see
        // find_package_root_from_subdir.
        let root = std::fs::canonicalize(temp.path()).unwrap();
        let root = root.as_path();

        // Producer package: public Status alias, internal Secret alias,
        // Greetable protocol.
        let producer_dir = root.join("producer");
        std::fs::create_dir_all(producer_dir.join("src")).unwrap();
        std::fs::write(
            producer_dir.join("beamtalk.toml"),
            "[package]\nname = \"producer\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        std::fs::write(
            producer_dir.join("src").join("types.bt"),
            "type Status = #ok | #error\n\
             internal type Secret = Integer\n\n\
             Protocol define: Greetable\n  \
             greet -> String\n",
        )
        .unwrap();

        // Consumer package: depends on producer, references Status,
        // Greetable, and Secret without declaring any of them.
        let consumer_dir = root.join("consumer");
        std::fs::create_dir_all(consumer_dir.join("src")).unwrap();
        std::fs::write(
            consumer_dir.join("beamtalk.toml"),
            "[package]\nname = \"consumer\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nproducer = { path = \"../producer\" }\n",
        )
        .unwrap();
        std::fs::write(
            consumer_dir.join("src").join("consumer.bt"),
            "Object subclass: Consumer\n  \
             useStatus: s :: Status => s\n  \
             useSecret: s :: Secret => s\n  \
             greetableInfo => Greetable requiredMethods\n",
        )
        .unwrap();

        let consumer_root = camino::Utf8PathBuf::from_path_buf(consumer_dir.clone()).unwrap();

        // Fake the producer's compiled state, *relative to the consumer's
        // own `_build/deps/producer/`* (`ensure_deps_resolved` is called
        // with `consumer_root`, so that's where it looks for freshness) —
        // so `ensure_deps_resolved` takes the fresh/no-recompile fast path
        // (`collect_fresh_deps` → `build_dep_class_index`) instead of a real
        // `erlc` compile, keeping this test fast and toolchain-independent.
        let layout = crate::commands::build_layout::BuildLayout::new(&consumer_root);
        let ebin_dir = layout.dep_ebin_dir("producer");
        std::fs::create_dir_all(&ebin_dir).unwrap();
        std::fs::write(ebin_dir.join("bt@producer@types.beam"), b"BEAM").unwrap();
        crate::commands::build_stamp::write_stamp(
            &layout.dep_stamp_path("producer"),
            crate::commands::build_stamp::current_otp_version(),
        );
        let consumer_file = consumer_root.join("src").join("consumer.bt");
        let source_files = vec![consumer_file.clone()];

        // Mirrors `run_lint`'s own orchestration: extract same-package
        // protocol/alias infos, then merge in the resolved dependency's.
        let sources =
            lint_pipeline::parse_package_sources(&consumer_file, &source_files, |f, _, e| {
                panic!("unreadable {f}: {e}")
            });
        let (_, dependencies) = merge_dependency_infos(&consumer_root);
        // ADR 0070 WS3 (BT-4608): every dependency was loaded, so `lint`
        // lifts the pre-WS3 dependency guard as `build` does.
        assert!(dependencies.extensions_complete);
        let extension_index = sources.extension_index;
        let mut all_protocol_infos = sources.protocol_infos;
        all_protocol_infos.extend(dependencies.protocol_infos);
        let mut all_protocol_defs = sources.protocol_defs;
        all_protocol_defs.extend(dependencies.protocol_defs);
        let mut all_alias_infos = sources.alias_infos;
        all_alias_infos.extend(dependencies.alias_infos);
        let parsed_files = sources.targets;

        assert!(
            all_protocol_infos.iter().any(|p| p.name == "Greetable"),
            "producer's Greetable protocol should be merged in: {all_protocol_infos:?}"
        );
        assert!(
            all_alias_infos
                .iter()
                .any(|a| a.name == "Status" && !a.is_internal),
            "producer's public Status alias should be merged in: {all_alias_infos:?}"
        );
        // The internal Secret alias is *not* filtered out at this merge
        // stage (`merge_dependency_infos`/`ResolvedDependency.alias_infos`
        // carry every declaration, filtered or not) — the seeding-boundary
        // exclusion happens downstream, inside `AliasRegistry::add_pre_loaded`
        // when `analyse_file` calls `analyse_full` below. That
        // exclusion logic itself is already covered by
        // `add_pre_loaded_never_seeds_internal_alias_from_different_package`
        // in `alias_registry.rs`; what this test proves is that the
        // wiring correctly delivers `is_internal`/`package` all the way from
        // `producer`'s source to `consumer`'s lint pass so that exclusion can
        // actually engage.
        let secret = all_alias_infos
            .iter()
            .find(|a| a.name == "Secret")
            .expect("producer's Secret alias should be merged in (unfiltered at this stage)");
        assert!(secret.is_internal, "Secret must be flagged internal");
        assert_eq!(
            secret.package.as_deref(),
            Some("producer"),
            "Secret must be stamped with producer's package name so the \
             seeding-boundary check can tell it apart from consumer's own package"
        );

        // Directly exercise the seeding step `analyse_file` performs
        // internally (`AliasRegistry::add_pre_loaded`, via `analyse_full`),
        // rather than trying to observe the exclusion through a lint diagnostic: a `::
        // Secret` annotation on its own produces no diagnostic either way —
        // `check_unresolved_type_aliases` (structural_validators.rs) is
        // deliberately scoped to near-miss *typos* of already-registered
        // alias names, not general annotation-existence checking (per its
        // own doc comment, "no general annotation-existence checker exists
        // yet"), so an unregistered `Secret` is silently accepted in
        // annotation position regardless of whether the wiring below is
        // correct. This directly proves what actually matters: the
        // `is_internal`/`package` data this test's earlier assertions
        // confirmed reaches `all_alias_infos` correctly is exactly what lets
        // `add_pre_loaded`'s own seeding-boundary filter (already unit-tested
        // in `alias_registry.rs`'s
        // `add_pre_loaded_never_seeds_internal_alias_from_different_package`)
        // actually exclude `Secret` while still seeding `Status`.
        let hierarchy =
            beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy::with_builtins();
        let protocol_registry =
            beamtalk_core::semantic_analysis::protocol_registry::ProtocolRegistry::new();
        let mut alias_registry =
            beamtalk_core::semantic_analysis::alias_registry::AliasRegistry::new();
        let seeding_diags = alias_registry.add_pre_loaded(
            all_alias_infos.clone(),
            &hierarchy,
            &protocol_registry,
            Some("consumer"),
        );
        assert!(
            seeding_diags.is_empty(),
            "seeding should not produce collision diagnostics: {seeding_diags:?}"
        );
        assert!(
            alias_registry.has_alias("Status"),
            "producer's public Status alias must be seeded into consumer's alias table"
        );
        assert!(
            !alias_registry.has_alias("Secret"),
            "producer's internal Secret alias must NOT be seeded into consumer's alias table"
        );

        let lint_pipeline::ParsedFile {
            source,
            module,
            parse_diagnostics: parse_diags,
            ..
        } = parsed_files
            .into_iter()
            .find(|f| f.path == consumer_file)
            .expect("consumer.bt should be among the parsed files");

        let diags = analyse_file(
            &module,
            &source,
            parse_diags,
            FileContext {
                pre_loaded_protocols: all_protocol_infos,
                pre_loaded_protocol_defs: all_protocol_defs,
                pre_loaded_aliases: all_alias_infos,
                knowledge_scope: beamtalk_core::semantic_analysis::KnowledgeScope::ProjectComplete,
                cross_file_extensions: Some(&extension_index),
                dependency_extensions_unknown: true,
                current_package: Some("consumer"),
                ..FileContext::default()
            },
        )
        .0;

        let unresolved_names: Vec<String> = diags
            .iter()
            .filter(|d| {
                d.category
                    == Some(beamtalk_core::source_analysis::DiagnosticCategory::UnresolvedClass)
            })
            .map(|d| d.message.to_string())
            .collect();

        assert!(
            !unresolved_names.iter().any(|m| m.contains("Status")),
            "dependency-exported Status alias should resolve, unresolved: {unresolved_names:?}"
        );
        assert!(
            !unresolved_names.iter().any(|m| m.contains("Greetable")),
            "dependency-exported Greetable protocol should resolve, unresolved: {unresolved_names:?}"
        );
    }

    /// `collect_lint_files`'s directory walk must never treat a
    /// dependency's own vendored checkout under `_build/deps/` as first-party
    /// project source. Unlike a path dependency (whose source lives wherever
    /// the path points, outside `_build`), a git/registry dependency is
    /// cloned straight into `_build/deps/<name>/` — the actual reproduction
    /// shape of the original bug report (an `http` dependency, not a path
    /// dep): without this exclusion, `beamtalk lint .` would (a) lint the
    /// vendored dependency's own source as if it were the user's code, and
    /// (b) extract its declarations a second time stamped with the
    /// *consuming* project's package name, producing a genuine mismatch
    /// against the same declaration merged in correctly (stamped with the
    /// dependency's own package name) via `merge_dependency_infos` — a
    /// collision `AliasRegistry::add_pre_loaded`'s identity check cannot
    /// recognise as a duplicate, since the two entries' `package` differs.
    #[test]
    fn collect_lint_files_excludes_vendored_dependency_checkout_under_build_dir() {
        let temp = tempfile::TempDir::new().unwrap();
        let root = temp.path();
        std::fs::write(
            root.join("beamtalk.toml"),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        std::fs::create_dir_all(root.join("src")).unwrap();
        std::fs::write(
            root.join("src").join("Main.bt"),
            "Object subclass: Main\n  foo => 1\n",
        )
        .unwrap();

        // Simulate a git/registry dependency's checkout (`clone_repo` clones
        // straight into `_build/deps/<name>/` — see `BuildLayout::dep_checkout_dir`).
        std::fs::create_dir_all(
            root.join("_build")
                .join("deps")
                .join("producer")
                .join("src"),
        )
        .unwrap();
        std::fs::write(
            root.join("_build")
                .join("deps")
                .join("producer")
                .join("src")
                .join("types.bt"),
            "type Handler = Block | Integer\n",
        )
        .unwrap();

        let source_path = camino::Utf8PathBuf::from_path_buf(root.to_path_buf()).unwrap();
        let (source_files, _erl_files) =
            collect_lint_files(&source_path, source_path.as_str()).unwrap();

        assert!(
            source_files.iter().all(|f| !f.as_str().contains("_build")),
            "vendored dependency source under _build/ must never be treated as project \
             source: {source_files:?}"
        );
    }

    /// BT-4135: a gitignored directory holding a nested checkout of the
    /// same project (`.worktrees/<name>/`, as `git worktree` users keep
    /// them) must not be linted — otherwise every diagnostic is reported
    /// once per checkout.
    #[test]
    fn collect_lint_files_skips_gitignored_nested_checkout() {
        let temp = tempfile::TempDir::new().unwrap();
        let root = temp.path();
        std::fs::create_dir(root.join(".git")).unwrap();
        std::fs::write(root.join(".gitignore"), ".worktrees/\n").unwrap();
        std::fs::write(
            root.join("beamtalk.toml"),
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        let checkout = root.join(".worktrees").join("feature");
        for base in [root, checkout.as_path()] {
            std::fs::create_dir_all(base.join("src")).unwrap();
            std::fs::write(
                base.join("src").join("Main.bt"),
                "Object subclass: Main\n  foo => 1\n",
            )
            .unwrap();
        }
        std::fs::write(checkout.join(".git"), "gitdir: elsewhere\n").unwrap();

        let source_path = camino::Utf8PathBuf::from_path_buf(root.to_path_buf()).unwrap();
        let (source_files, _erl_files) =
            collect_lint_files(&source_path, source_path.as_str()).unwrap();

        assert_eq!(
            source_files,
            vec![source_path.join("src").join("Main.bt")],
            "gitignored .worktrees/ checkout must not be linted"
        );
    }

    /// Acceptance criterion: a project depending on a package that
    /// declares a type alias exactly once must lint clean across every file
    /// in the project — no false "Pre-loaded type alias ... collides with
    /// another type alias of the same name" diagnostic, no matter how many
    /// project files are analysed against the same merged
    /// `all_alias_infos` seed list.
    ///
    /// Mirrors `lint_resolves_dependency_protocol_and_public_alias_but_not_internal_alias`'s
    /// fixture shape (a real producer/consumer path-dependency pair with a
    /// faked-fresh dependency stamp), but drives `analyse_file` across
    /// *several* consumer files the way `run_lint`'s Pass 2 loop does, since
    /// the bug this guards against only manifests when the same pre-loaded
    /// alias list is re-seeded into a fresh `AliasRegistry` once per file.
    #[test]
    #[allow(clippy::too_many_lines)] // end-to-end pipeline fixture
    fn lint_across_many_files_does_not_flag_a_singly_declared_dependency_alias_as_colliding() {
        let temp = tempfile::TempDir::new().unwrap();
        let root = temp.path();

        let producer_dir = root.join("producer");
        std::fs::create_dir_all(producer_dir.join("src")).unwrap();
        std::fs::write(
            producer_dir.join("beamtalk.toml"),
            "[package]\nname = \"producer\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        std::fs::write(
            producer_dir.join("src").join("types.bt"),
            "type Handler = Block | Integer\n",
        )
        .unwrap();

        let consumer_dir = root.join("consumer");
        std::fs::create_dir_all(consumer_dir.join("src")).unwrap();
        std::fs::write(
            consumer_dir.join("beamtalk.toml"),
            "[package]\nname = \"consumer\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nproducer = { path = \"../producer\" }\n",
        )
        .unwrap();
        let mut source_files = Vec::new();
        for i in 0..8 {
            let file = consumer_dir.join("src").join(format!("File{i}.bt"));
            std::fs::write(
                &file,
                format!("Object subclass: File{i}\n  useIt: h :: Handler => h\n"),
            )
            .unwrap();
            source_files.push(camino::Utf8PathBuf::from_path_buf(file).unwrap());
        }

        let consumer_root = camino::Utf8PathBuf::from_path_buf(consumer_dir.clone()).unwrap();

        // Fake the producer's compiled state so dependency resolution takes
        // the fresh/no-recompile fast path (mirrors the sibling test above).
        let layout = crate::commands::build_layout::BuildLayout::new(&consumer_root);
        let ebin_dir = layout.dep_ebin_dir("producer");
        std::fs::create_dir_all(&ebin_dir).unwrap();
        std::fs::write(ebin_dir.join("bt@producer@types.beam"), b"BEAM").unwrap();
        crate::commands::build_stamp::write_stamp(
            &layout.dep_stamp_path("producer"),
            crate::commands::build_stamp::current_otp_version(),
        );

        let sources =
            lint_pipeline::parse_package_sources(&consumer_root, &source_files, |f, _, e| {
                panic!("unreadable {f}: {e}")
            });
        let (_, dependencies) = merge_dependency_infos(&consumer_root);
        let all_alias_infos: Vec<_> = sources
            .alias_infos
            .iter()
            .chain(&dependencies.alias_infos)
            .collect();
        assert_eq!(
            all_alias_infos
                .iter()
                .filter(|a| a.name == "Handler")
                .count(),
            1,
            "the fixture declares Handler exactly once: {all_alias_infos:?}"
        );

        // The real Pass 2, so its cross-file collision dedup is what runs.
        let mut diags = Vec::new();
        lint_pipeline::analyse_package(sources, dependencies, None, |file| {
            diags.extend(file.diagnostics);
        });
        assert!(
            diags
                .iter()
                .all(|d| !d.message.contains("collides with another type alias")),
            "a once-declared dependency alias must never be flagged as colliding: {diags:?}"
        );
    }

    /// Acceptance criterion: a *genuine* cross-package alias
    /// collision — two different dependencies each exporting a same-named
    /// alias with a different RHS — must be reported exactly once across a
    /// multi-file consumer project, not once per file. Exercises the real
    /// pipeline end to end (`parse_package_sources` +
    /// `merge_dependency_infos` + `analyse_package`, the Pass 2 `run_lint`
    /// uses, dedup included) rather than synthetic diagnostics, so it would
    /// have caught a regression in either the `AliasRegistry::add_pre_loaded`
    /// identity check or the pipeline's cross-file dedup.
    #[test]
    #[allow(clippy::too_many_lines)]
    fn lint_across_many_files_reports_a_genuine_cross_package_alias_collision_exactly_once() {
        let temp = tempfile::TempDir::new().unwrap();
        let root = temp.path();

        for (pkg, rhs) in [("producer_a", "String"), ("producer_b", "Integer")] {
            let dir = root.join(pkg);
            std::fs::create_dir_all(dir.join("src")).unwrap();
            std::fs::write(
                dir.join("beamtalk.toml"),
                format!("[package]\nname = \"{pkg}\"\nversion = \"0.1.0\"\n"),
            )
            .unwrap();
            std::fs::write(
                dir.join("src").join("types.bt"),
                format!("type Shared = {rhs}\n"),
            )
            .unwrap();
        }

        let consumer_dir = root.join("consumer");
        std::fs::create_dir_all(consumer_dir.join("src")).unwrap();
        std::fs::write(
            consumer_dir.join("beamtalk.toml"),
            "[package]\nname = \"consumer\"\nversion = \"0.1.0\"\n\n\
             [dependencies]\nproducer_a = { path = \"../producer_a\" }\n\
             producer_b = { path = \"../producer_b\" }\n",
        )
        .unwrap();
        let mut source_files = Vec::new();
        for i in 0..8 {
            let file = consumer_dir.join("src").join(format!("File{i}.bt"));
            std::fs::write(&file, format!("Object subclass: File{i}\n  foo => 1\n")).unwrap();
            source_files.push(camino::Utf8PathBuf::from_path_buf(file).unwrap());
        }

        let consumer_root = camino::Utf8PathBuf::from_path_buf(consumer_dir.clone()).unwrap();

        // Fake both producers' compiled state so dependency resolution takes
        // the fresh/no-recompile fast path (mirrors the sibling tests above).
        let layout = crate::commands::build_layout::BuildLayout::new(&consumer_root);
        for pkg in ["producer_a", "producer_b"] {
            let ebin_dir = layout.dep_ebin_dir(pkg);
            std::fs::create_dir_all(&ebin_dir).unwrap();
            std::fs::write(ebin_dir.join(format!("bt@{pkg}@types.beam")), b"BEAM").unwrap();
            crate::commands::build_stamp::write_stamp(
                &layout.dep_stamp_path(pkg),
                crate::commands::build_stamp::current_otp_version(),
            );
        }

        let sources =
            lint_pipeline::parse_package_sources(&consumer_root, &source_files, |f, _, e| {
                panic!("unreadable {f}: {e}")
            });
        let (_, dependencies) = merge_dependency_infos(&consumer_root);
        let all_alias_infos: Vec<_> = sources
            .alias_infos
            .iter()
            .chain(&dependencies.alias_infos)
            .collect();
        assert_eq!(
            all_alias_infos
                .iter()
                .filter(|a| a.name == "Shared")
                .count(),
            2,
            "the fixture declares Shared twice, with different RHS: {all_alias_infos:?}"
        );

        // The real Pass 2, so its cross-file collision dedup is what runs.
        let mut collision_count = 0usize;
        lint_pipeline::analyse_package(sources, dependencies, None, |file| {
            collision_count += file
                .diagnostics
                .iter()
                .filter(|d| d.message.contains("collides with another type alias"))
                .count();
        });

        assert_eq!(
            collision_count, 1,
            "a genuine cross-package collision must be reported exactly once for the whole \
             lint run, not once per file"
        );
    }

    /// Acceptance criterion: when two pre-loaded aliases *do*
    /// genuinely collide, the lint pipeline's per-file loop must report that
    /// collision once for the whole run, not once per file — `add_pre_loaded`
    /// rediscovers the same collision on every file (a fresh `AliasRegistry`
    /// seeded from the same list each time), so without the pipeline's own
    /// cross-file dedup the user would see the same diagnostic N times for
    /// an N-file project.
    #[test]
    fn dedup_pre_loaded_alias_collision_keeps_only_the_first_sighting_across_simulated_files() {
        use beamtalk_core::source_analysis::{Diagnostic, DiagnosticCategory, Span};

        let collision = |span: Span| {
            Diagnostic::error(
                "Pre-loaded type alias `Id` collides with another type alias of the same name",
                span,
            )
            .with_category(DiagnosticCategory::Type)
        };

        let mut seen = std::collections::HashSet::new();
        // Same collision, rediscovered on 5 simulated files (byte-identical
        // message + span, exactly as `add_pre_loaded` would re-emit it).
        let kept: Vec<bool> = (0..5)
            .map(|_| dedup_pre_loaded_alias_collision(&collision(Span::new(10, 12)), &mut seen))
            .collect();

        assert_eq!(
            kept,
            vec![true, false, false, false, false],
            "only the first sighting of a given collision should survive"
        );
    }

    #[test]
    fn dedup_pre_loaded_alias_collision_never_drops_unrelated_diagnostics() {
        use beamtalk_core::source_analysis::{Diagnostic, Span};

        let mut seen = std::collections::HashSet::new();
        // Two ordinary (non-"Pre-loaded type alias") diagnostics that happen
        // to share message text and span across two simulated files — these
        // are file-specific by construction and must never be deduped away.
        let unrelated = Diagnostic::error("Unresolved class `Foo`", Span::new(5, 8));
        assert!(dedup_pre_loaded_alias_collision(&unrelated, &mut seen));
        assert!(dedup_pre_loaded_alias_collision(&unrelated, &mut seen));
    }

    /// Pass 1's extraction set must dedup across the absolute
    /// paths produced by walking `src/`/`test/` and the relative paths a user
    /// may pass as explicit lint targets. Without canonical-form dedup the
    /// same file would appear twice in the extraction list and be parsed
    /// twice downstream.
    ///
    /// Serialized on `cwd`: the relative target only resolves against the
    /// process working directory, which this test temporarily changes.
    #[test]
    #[serial_test::serial(cwd)]
    fn package_extraction_dedups_absolute_and_relative_targets() {
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
        let relative_target = camino::Utf8PathBuf::from("test/foo.bt");
        let out = lint_pipeline::parse_package_sources(
            &camino::Utf8PathBuf::from("test"),
            std::slice::from_ref(&relative_target),
            |f, _, e| panic!("unreadable {f}: {e}"),
        );

        std::env::set_current_dir(prev_cwd).unwrap();

        assert_eq!(
            (out.targets.len(), out.class_infos.len()),
            (1, 1),
            "expected one parse of foo.bt after canonical-form dedup, got {:?}",
            out.targets.iter().map(|t| &t.path).collect::<Vec<_>>()
        );
    }

    /// `find_package_root` must work for relative paths like `test/`
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

    /// With no FFI registry, an `(Erlang m) f:` call in a typed class
    /// infers as `Dynamic(UntypedFfi)` and lint emits the
    /// "Dynamic in typed class (untyped FFI)" warning.
    ///
    /// This captures the without-registry lint behaviour, contrasted by the
    /// next test: with the registry loaded, no warning fires.
    #[test]
    fn ffi_call_without_registry_warns_dynamic_in_typed_class() {
        let source = r#"sealed typed Value subclass: TcpCheck
  field: host :: String = "localhost"

  check -> String =>
    result := (Erlang gen_tcp) connect: self.host asAtom port: 80
    result printString
"#;
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diags = analyse_file(
            &module,
            source,
            parse_diags,
            FileContext {
                ..FileContext::default()
            },
        )
        .0;

        let has_untyped_ffi = diags.iter().any(|d| d.message.contains("untyped FFI"));
        assert!(
            has_untyped_ffi,
            "without registry, lint should warn untyped FFI; got: {diags:?}"
        );
    }

    /// With the FFI registry loaded (build cache present), an
    /// `(Erlang m) f:` call resolves to a typed return — `Result(...)` for
    /// `gen_tcp:connect/2`. The receiver is no longer `Dynamic` at the top
    /// level, so the "Dynamic in typed class (untyped FFI)" warning
    /// must NOT fire — matching build's behaviour.
    #[test]
    fn ffi_call_with_registry_does_not_warn_dynamic_in_typed_class() {
        use beamtalk_core::semantic_analysis::type_checker::{
            NativeTypeRegistry, parse_specs_line,
        };

        let mut registry = NativeTypeRegistry::new();
        // Same shape as the cached spec line for gen_tcp:connect/2.
        let line = "beamtalk-specs-module:gen_tcp:[#{arity => 2,line => 1,name => <<\"connect\">>,params => [#{name => <<\"sockaddr\">>,type => <<\"Symbol\">>},#{name => <<\"port\">>,type => <<\"Integer\">>}],return_type => <<\"Result(Dynamic | Tuple, Symbol)\">>}]";
        parse_specs_line(line, &mut registry);
        assert!(
            registry.lookup("gen_tcp", "connect", 2).is_some(),
            "fixture must register gen_tcp:connect/2"
        );

        let source = r#"sealed typed Value subclass: TcpCheck
  field: host :: String = "localhost"

  check -> String =>
    result := (Erlang gen_tcp) connect: self.host asAtom port: 80
    result printString
"#;
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diags = analyse_file(
            &module,
            source,
            parse_diags,
            FileContext {
                native_type_registry: Some(std::sync::Arc::new(registry)),
                ..FileContext::default()
            },
        )
        .0;

        let untyped_ffi: Vec<_> = diags
            .iter()
            .filter(|d| d.message.contains("untyped FFI"))
            .collect();
        assert!(
            untyped_ffi.is_empty(),
            "with registry, lint must not warn untyped FFI; got: {untyped_ffi:?}"
        );
    }

    /// `run_lint` populates its native-type registry via
    /// `super::build::extract_type_specs` — the exact function `beamtalk
    /// build` calls — instead of only reading `_build/type_cache/`. On a
    /// project that has never been built (no cache directory at all), a
    /// cache-only read would silently return `None`, so lint would skip FFI
    /// arg-type checks that build performs; an `@expect type` written for
    /// the resulting build-time diagnostic would then be reported as stale by
    /// lint. Calling the shared extractor makes lint see the same registry
    /// build would, with no prior build required — this test drives that
    /// extractor exactly as `run_lint` does and confirms `@expect type` on
    /// a genuine FFI arg-type mismatch is not stale.
    #[test]
    fn lint_extracts_type_specs_live_on_cold_cache_bt_2851() {
        let temp = tempfile::TempDir::new().unwrap();
        let root = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = crate::commands::build_layout::BuildLayout::new(&root);

        // No `_build/` directory exists yet — the cold-cache case.
        assert!(!layout.type_cache_dir().exists());

        let Some(registry) = crate::commands::build::extract_type_specs(&layout, true, false)
        else {
            // OTP `.beam` discovery is environment-dependent (e.g. a sandbox
            // with no OTP install on disk); skip rather than false-fail.
            eprintln!(
                "skipping lint_extracts_type_specs_live_on_cold_cache_bt_2851: \
                 no OTP .beam files discovered in this environment"
            );
            return;
        };
        assert!(
            registry.lookup("lists", "reverse", 1).is_some(),
            "live extraction with no prior build must still find lists:reverse/1"
        );
        // The extractor writes the same cache a `beamtalk build` would, so a
        // subsequent lint or build run reads it back instead of re-extracting.
        assert!(layout.type_cache_dir().exists());

        let source = "Object subclass: LintFfiColdCacheTest\n\n  @expect type\n  class badCall => Erlang lists reverse: 42\n";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diags = analyse_file(
            &module,
            source,
            parse_diags,
            FileContext {
                native_type_registry: Some(std::sync::Arc::new(registry)),
                ..FileContext::default()
            },
        )
        .0;
        let stale = diags.iter().any(|d| d.message.contains("stale @expect"));
        assert!(
            !stale,
            "@expect type suppressing a genuine cold-cache FFI arg-type \
             mismatch must not be reported as stale by lint, got: {diags:?}"
        );
    }

    /// `load_type_cache_registry` reads `<module>_<16-hex>.json`
    /// files in the cache directory and replays their `specs_line` into a
    /// registry, matching the format `beamtalk build` writes via
    /// `TypeCache::store`. Foreign files (no hash, wrong extension, non-hex
    /// suffix) must be ignored.
    #[test]
    fn load_type_cache_registry_populates_from_cached_json() {
        use crate::beam_compiler::load_type_cache_registry;

        let temp = tempfile::TempDir::new().unwrap();
        let cache_dir = camino::Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        std::fs::create_dir_all(cache_dir.as_std_path()).unwrap();

        // Real cache entry — 16-hex hash matches `TypeCache::cache_path`.
        std::fs::write(
            cache_dir.join("gen_tcp_0123456789abcdef.json").as_std_path(),
            serde_json::json!({
                "beam_mtime_secs": 0,
                "beam_mtime_nanos": 0,
                "mapping_stamp": crate::beam_compiler::current_spec_mapping_stamp(),
                "specs_line": r#"beamtalk-specs-module:gen_tcp:[#{arity => 2,line => 1,name => <<"connect">>,params => [#{name => <<"sockaddr">>,type => <<"Symbol">>},#{name => <<"port">>,type => <<"Integer">>}],return_type => <<"Result(Dynamic | Tuple, Symbol)">>}]"#,
            })
            .to_string(),
        )
        .unwrap();
        // Foreign files that must be ignored: wrong extension, missing hash,
        // non-hex hash, short hash.
        std::fs::write(cache_dir.join("notes.txt").as_std_path(), "ignored").unwrap();
        std::fs::write(
            cache_dir.join("gen_tcp.json").as_std_path(),
            r#"{"beam_mtime_secs":0,"beam_mtime_nanos":0,"specs_line":""}"#,
        )
        .unwrap();
        std::fs::write(
            cache_dir.join("gen_tcp_xyz.json").as_std_path(),
            r#"{"beam_mtime_secs":0,"beam_mtime_nanos":0,"specs_line":""}"#,
        )
        .unwrap();

        let registry = load_type_cache_registry(&cache_dir).expect("registry must load");
        assert!(
            registry.lookup("gen_tcp", "connect", 2).is_some(),
            "loaded registry should contain gen_tcp:connect/2"
        );
    }

    /// An empty or missing `_build/type_cache/` directory must yield
    /// `None`, not an error — projects that have never been built should still
    /// lint without crashing.
    #[test]
    fn load_type_cache_registry_returns_none_when_missing() {
        use crate::beam_compiler::load_type_cache_registry;

        let temp = tempfile::TempDir::new().unwrap();
        let missing = camino::Utf8PathBuf::from_path_buf(temp.path().join("nonexistent")).unwrap();
        assert!(load_type_cache_registry(&missing).is_none());

        let empty = camino::Utf8PathBuf::from_path_buf(temp.path().join("empty")).unwrap();
        std::fs::create_dir_all(empty.as_std_path()).unwrap();
        assert!(load_type_cache_registry(&empty).is_none());
    }

    /// When the cache has accumulated
    /// multiple `<module>_<hash>.json` entries for the same module — e.g.
    /// after a dependency upgrade or BEAM path change moved the spec to a
    /// new hash — the loader must replay only the most-recently-modified
    /// entry: replaying both could let `read_dir` order pick a stale
    /// signature and reintroduce a lint/build disagreement.
    #[test]
    fn load_type_cache_registry_picks_latest_when_module_has_multiple_hashes() {
        use crate::beam_compiler::load_type_cache_registry;
        use beamtalk_core::semantic_analysis::type_checker::FunctionSignature;

        let temp = tempfile::TempDir::new().unwrap();
        let cache_dir = camino::Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        std::fs::create_dir_all(cache_dir.as_std_path()).unwrap();

        // Stale entry — written first, given an older mtime explicitly.
        // The stale spec describes `connect/2` returning `Symbol` (wrong).
        let stale_path = cache_dir.join("gen_tcp_aaaaaaaaaaaaaaaa.json");
        std::fs::write(
            stale_path.as_std_path(),
            serde_json::json!({
                "beam_mtime_secs": 0,
                "beam_mtime_nanos": 0,
                "mapping_stamp": crate::beam_compiler::current_spec_mapping_stamp(),
                "specs_line": r#"beamtalk-specs-module:gen_tcp:[#{arity => 2,line => 1,name => <<"connect">>,params => [#{name => <<"sockaddr">>,type => <<"Symbol">>},#{name => <<"port">>,type => <<"Integer">>}],return_type => <<"Symbol">>}]"#,
            })
            .to_string(),
        )
        .unwrap();
        // Force the stale entry's mtime backwards so the latest-mtime test
        // is unambiguous regardless of filesystem timestamp granularity.
        let old = std::time::SystemTime::UNIX_EPOCH + std::time::Duration::from_secs(1_000_000);
        std::fs::File::options()
            .write(true)
            .open(stale_path.as_std_path())
            .expect("reopen stale fixture")
            .set_modified(old)
            .expect("set stale mtime");

        // Fresh entry — the spec describes the real `Result(...)` return.
        std::fs::write(
            cache_dir
                .join("gen_tcp_bbbbbbbbbbbbbbbb.json")
                .as_std_path(),
            serde_json::json!({
                "beam_mtime_secs": 1,
                "beam_mtime_nanos": 0,
                "mapping_stamp": crate::beam_compiler::current_spec_mapping_stamp(),
                "specs_line": r#"beamtalk-specs-module:gen_tcp:[#{arity => 2,line => 1,name => <<"connect">>,params => [#{name => <<"sockaddr">>,type => <<"Symbol">>},#{name => <<"port">>,type => <<"Integer">>}],return_type => <<"Result(Dynamic | Tuple, Symbol)">>}]"#,
            })
            .to_string(),
        )
        .unwrap();

        let registry = load_type_cache_registry(&cache_dir).expect("registry must load");
        let sig: &FunctionSignature = registry
            .lookup("gen_tcp", "connect", 2)
            .expect("connect/2 should resolve");
        // The fresh `Result(...)` return must win. If the stale `Symbol`
        // return overwrote it (last-write-loses on hash-set replay), the
        // displayed signature would end with `-> Symbol`.
        let display = sig.display_signature();
        assert!(
            display.contains("Result"),
            "loader must pick the latest-mtime cache entry; got: {display}"
        );
    }

    /// When a cache entry records a `beam_path` and the live `.beam`
    /// at that path has a newer mtime than the cached one — e.g. `cargo build`
    /// rebuilt a NIF module between `beamtalk build` and `beamtalk lint` — the
    /// loader must drop the entry so lint does not warn off stale signatures.
    #[test]
    fn load_type_cache_registry_skips_entry_when_beam_mtime_changed() {
        use crate::beam_compiler::load_type_cache_registry;

        let temp = tempfile::TempDir::new().unwrap();
        let cache_dir = camino::Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        std::fs::create_dir_all(cache_dir.as_std_path()).unwrap();

        // Stand-in `.beam` file. We don't need real BEAM bytes here — the
        // loader only stats mtime, not contents.
        let beam_path = temp.path().join("gen_tcp.beam");
        std::fs::write(&beam_path, b"placeholder").unwrap();
        let live = std::fs::metadata(&beam_path)
            .unwrap()
            .modified()
            .unwrap()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap();

        // Cache an mtime far in the past so the live file looks newer.
        // Use serde_json to avoid Windows backslash-escaping pitfalls when
        // interpolating a temp path into a JSON string literal.
        let stale_secs = live.as_secs().saturating_sub(3600);
        let entry_json = serde_json::json!({
            "beam_mtime_secs": stale_secs,
            "beam_mtime_nanos": 0,
            "beam_path": beam_path.to_str().unwrap(),
            "mapping_stamp": crate::beam_compiler::current_spec_mapping_stamp(),
            "specs_line": r#"beamtalk-specs-module:gen_tcp:[#{arity => 2,line => 1,name => <<"connect">>,params => [#{name => <<"sockaddr">>,type => <<"Symbol">>},#{name => <<"port">>,type => <<"Integer">>}],return_type => <<"Symbol">>}]"#,
        })
        .to_string();
        std::fs::write(
            cache_dir
                .join("gen_tcp_0123456789abcdef.json")
                .as_std_path(),
            entry_json,
        )
        .unwrap();

        // Stale entry is skipped — no other entries, so the loader returns None.
        assert!(
            load_type_cache_registry(&cache_dir).is_none(),
            "stale-mtime entry must be skipped, leaving registry empty"
        );
    }

    /// When a cache entry's `beam_path` no longer exists — the BEAM
    /// file was deleted, the project moved, an OTP version was uninstalled —
    /// the entry must be skipped rather than replayed against a registry
    /// that may now be wrong.
    #[test]
    fn load_type_cache_registry_skips_entry_when_beam_missing() {
        use crate::beam_compiler::load_type_cache_registry;

        let temp = tempfile::TempDir::new().unwrap();
        let cache_dir = camino::Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        std::fs::create_dir_all(cache_dir.as_std_path()).unwrap();

        let missing_beam = temp.path().join("never_existed.beam");
        let entry_json = serde_json::json!({
            "beam_mtime_secs": 1,
            "beam_mtime_nanos": 0,
            "beam_path": missing_beam.to_str().unwrap(),
            "mapping_stamp": crate::beam_compiler::current_spec_mapping_stamp(),
            "specs_line": r#"beamtalk-specs-module:gen_tcp:[#{arity => 1,line => 1,name => <<"close">>,params => [#{name => <<"sock">>,type => <<"Object">>}],return_type => <<"Symbol">>}]"#,
        })
        .to_string();
        std::fs::write(
            cache_dir
                .join("gen_tcp_0123456789abcdef.json")
                .as_std_path(),
            entry_json,
        )
        .unwrap();

        assert!(
            load_type_cache_registry(&cache_dir).is_none(),
            "entry pointing at a missing .beam must be skipped"
        );
    }

    /// Legacy entries lacking a `beam_path` field must continue to load —
    /// pessimistically treated as fresh — so a `lint` immediately after
    /// upgrading does not blank out every FFI signature until the user
    /// re-runs `build`.
    ///
    /// This entry does carry a current `mapping_stamp` so the test
    /// isolates the `beam_path` leniency behaviour; see
    /// `load_type_cache_registry_skips_entry_when_mapping_stamp_missing` for
    /// the case where the stamp itself is absent.
    #[test]
    fn load_type_cache_registry_loads_legacy_entry_without_beam_path() {
        use crate::beam_compiler::load_type_cache_registry;

        let temp = tempfile::TempDir::new().unwrap();
        let cache_dir = camino::Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        std::fs::create_dir_all(cache_dir.as_std_path()).unwrap();

        // No `beam_path` field at all — the legacy on-disk shape.
        std::fs::write(
            cache_dir.join("gen_tcp_0123456789abcdef.json").as_std_path(),
            serde_json::json!({
                "beam_mtime_secs": 0,
                "beam_mtime_nanos": 0,
                "mapping_stamp": crate::beam_compiler::current_spec_mapping_stamp(),
                "specs_line": r#"beamtalk-specs-module:gen_tcp:[#{arity => 2,line => 1,name => <<"connect">>,params => [#{name => <<"sockaddr">>,type => <<"Symbol">>},#{name => <<"port">>,type => <<"Integer">>}],return_type => <<"Result(Dynamic | Tuple, Symbol)">>}]"#,
            })
            .to_string(),
        )
        .unwrap();

        let registry = load_type_cache_registry(&cache_dir).expect("legacy entry must load");
        assert!(
            registry.lookup("gen_tcp", "connect", 2).is_some(),
            "legacy entry without beam_path must be tolerated as fresh"
        );
    }

    /// An entry with no `mapping_stamp` field at all (the default, empty
    /// string) — the shape written before compiler-build stamping existed —
    /// must be treated as a graceful cache miss — not a crash — even though
    /// its `.beam` mtime and (absent) `beam_path` would otherwise be
    /// accepted as fresh.
    #[test]
    fn load_type_cache_registry_skips_entry_when_mapping_stamp_missing() {
        use crate::beam_compiler::load_type_cache_registry;

        let temp = tempfile::TempDir::new().unwrap();
        let cache_dir = camino::Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        std::fs::create_dir_all(cache_dir.as_std_path()).unwrap();

        // Legacy shape: no `mapping_stamp` field at all.
        std::fs::write(
            cache_dir.join("gen_tcp_0123456789abcdef.json").as_std_path(),
            r#"{"beam_mtime_secs":0,"beam_mtime_nanos":0,"specs_line":"beamtalk-specs-module:gen_tcp:[#{arity => 2,line => 1,name => <<\"connect\">>,params => [#{name => <<\"sockaddr\">>,type => <<\"Symbol\">>},#{name => <<\"port\">>,type => <<\"Integer\">>}],return_type => <<\"Result(Dynamic | Tuple, Symbol)\">>}]"}"#,
        )
        .unwrap();

        assert!(
            load_type_cache_registry(&cache_dir).is_none(),
            "an entry with no mapping_stamp field must be a graceful miss, not a crash"
        );
    }

    /// An entry stamped by a *different* compiler build (a stale
    /// `mapping_stamp`) must be skipped even though its `.beam` mtime still
    /// matches — a warm cache surviving a change to
    /// `beamtalk_spec_reader.erl`'s type-mapping logic must not keep serving
    /// the old mapping forever.
    #[test]
    fn load_type_cache_registry_skips_entry_when_mapping_stamp_differs() {
        use crate::beam_compiler::load_type_cache_registry;

        let temp = tempfile::TempDir::new().unwrap();
        let cache_dir = camino::Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        std::fs::create_dir_all(cache_dir.as_std_path()).unwrap();

        std::fs::write(
            cache_dir.join("gen_tcp_0123456789abcdef.json").as_std_path(),
            serde_json::json!({
                "beam_mtime_secs": 0,
                "beam_mtime_nanos": 0,
                "mapping_stamp": "stale-mapping-stamp-from-an-older-compiler-build",
                "specs_line": r#"beamtalk-specs-module:gen_tcp:[#{arity => 2,line => 1,name => <<"connect">>,params => [#{name => <<"sockaddr">>,type => <<"Symbol">>},#{name => <<"port">>,type => <<"Integer">>}],return_type => <<"Result(Dynamic | Tuple, Symbol)">>}]"#,
            })
            .to_string(),
        )
        .unwrap();

        assert!(
            load_type_cache_registry(&cache_dir).is_none(),
            "an entry stamped by a different compiler build must be a cache miss"
        );
    }

    /// When a cache entry records a `beam_path` *and* the live file
    /// at that path matches the cached mtime, the entry must be loaded — this
    /// is the steady-state case after a fresh `beamtalk build`.
    #[test]
    fn load_type_cache_registry_loads_entry_when_beam_mtime_matches() {
        use crate::beam_compiler::load_type_cache_registry;

        let temp = tempfile::TempDir::new().unwrap();
        let cache_dir = camino::Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        std::fs::create_dir_all(cache_dir.as_std_path()).unwrap();

        let beam_path = temp.path().join("gen_tcp.beam");
        std::fs::write(&beam_path, b"placeholder").unwrap();
        let modified = std::fs::metadata(&beam_path).unwrap().modified().unwrap();
        let dur = modified.duration_since(std::time::UNIX_EPOCH).unwrap();
        let entry_json = serde_json::json!({
            "beam_mtime_secs": dur.as_secs(),
            "beam_mtime_nanos": dur.subsec_nanos(),
            "beam_path": beam_path.to_str().unwrap(),
            "mapping_stamp": crate::beam_compiler::current_spec_mapping_stamp(),
            "specs_line": r#"beamtalk-specs-module:gen_tcp:[#{arity => 2,line => 1,name => <<"connect">>,params => [#{name => <<"sockaddr">>,type => <<"Symbol">>},#{name => <<"port">>,type => <<"Integer">>}],return_type => <<"Result(Dynamic | Tuple, Symbol)">>}]"#,
        })
        .to_string();
        std::fs::write(
            cache_dir
                .join("gen_tcp_0123456789abcdef.json")
                .as_std_path(),
            entry_json,
        )
        .unwrap();

        let registry = load_type_cache_registry(&cache_dir).expect("registry must load");
        assert!(
            registry.lookup("gen_tcp", "connect", 2).is_some(),
            "fresh entry with matching mtime must be loaded"
        );
    }

    /// Module names that themselves contain
    /// underscores (e.g. `gen_tcp_socket`) must not be confused with a
    /// hash-suffixed entry for `gen_tcp`. The filename parser splits on the
    /// final underscore and requires the trailing segment to be exactly 16
    /// hex chars — the hash format `TypeCache::cache_path` writes.
    #[test]
    fn load_type_cache_registry_disambiguates_underscored_module_names() {
        use crate::beam_compiler::load_type_cache_registry;

        let temp = tempfile::TempDir::new().unwrap();
        let cache_dir = camino::Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        std::fs::create_dir_all(cache_dir.as_std_path()).unwrap();

        std::fs::write(
            cache_dir
                .join("gen_tcp_socket_1111111111111111.json")
                .as_std_path(),
            serde_json::json!({
                "beam_mtime_secs": 0,
                "beam_mtime_nanos": 0,
                "mapping_stamp": crate::beam_compiler::current_spec_mapping_stamp(),
                "specs_line": r#"beamtalk-specs-module:gen_tcp_socket:[#{arity => 1,line => 1,name => <<"close">>,params => [#{name => <<"sock">>,type => <<"Object">>}],return_type => <<"Symbol">>}]"#,
            })
            .to_string(),
        )
        .unwrap();
        std::fs::write(
            cache_dir.join("gen_tcp_2222222222222222.json").as_std_path(),
            serde_json::json!({
                "beam_mtime_secs": 0,
                "beam_mtime_nanos": 0,
                "mapping_stamp": crate::beam_compiler::current_spec_mapping_stamp(),
                "specs_line": r#"beamtalk-specs-module:gen_tcp:[#{arity => 1,line => 1,name => <<"close">>,params => [#{name => <<"sock">>,type => <<"Object">>}],return_type => <<"Symbol">>}]"#,
            })
            .to_string(),
        )
        .unwrap();

        let registry = load_type_cache_registry(&cache_dir).expect("registry must load");
        assert!(
            registry.lookup("gen_tcp_socket", "close", 1).is_some(),
            "gen_tcp_socket module should be loaded under its full name"
        );
        assert!(
            registry.lookup("gen_tcp", "close", 1).is_some(),
            "gen_tcp module should be loaded independently"
        );
    }

    /// Writes a one-file package under a temp dir and returns (dir, path).
    fn write_probe_package(manifest_extra: &str, source: &str) -> (tempfile::TempDir, String) {
        let temp = tempfile::TempDir::new().unwrap();
        let root = temp.path();
        std::fs::write(
            root.join("beamtalk.toml"),
            format!("[package]\nname = \"recheck_probe\"\nversion = \"0.1.0\"\n{manifest_extra}"),
        )
        .unwrap();
        let src_dir = root.join("src");
        std::fs::create_dir_all(&src_dir).unwrap();
        std::fs::write(src_dir.join("recheck_probe.bt"), source).unwrap();
        let path = root.to_str().unwrap().to_string();
        (temp, path)
    }

    /// BT-4377: a package whose `[diagnostics]` table turns `dnu` off loads
    /// clean in the REPL, so the gate must pass the same source.
    #[test]
    fn lint_warnings_as_errors_applies_diagnostics_table() {
        let source = "Value subclass: RecheckProbe\n\n  probe => 42 foo\n";
        let (_temp, path) = write_probe_package("\n[diagnostics]\ndnu = \"off\"\n", source);
        let strict = run_lint(&path, OutputFormat::Text, true);
        assert!(
            strict.is_ok(),
            "`dnu = \"off\"` must silence the DNU under the gate: {strict:?}"
        );

        // Control: without the table entry the same source fails the gate.
        let (_temp, path) = write_probe_package("", source);
        assert!(run_lint(&path, OutputFormat::Text, true).is_err());
    }

    /// BT-4377: a syntax error fails `build` and a REPL load, so it must
    /// fail the gate too (it used to be dropped as a non-lint parse diag).
    #[test]
    fn lint_warnings_as_errors_fails_on_syntax_error() {
        let (_temp, path) =
            write_probe_package("", "Value subclass: RecheckProbe\n\n  probe => 42 +\n");
        let strict = run_lint(&path, OutputFormat::Text, true);
        assert!(
            strict.is_err(),
            "lint --warnings-as-errors must fail on a syntax error"
        );
        let msg = format!("{:?}", strict.unwrap_err());
        assert!(
            msg.contains("type/DNU/error"),
            "failure must name the parity gate, got: {msg}"
        );
    }

    /// BT-4334 regression: a fixture carrying a REPL-only diagnostic today
    /// (DNU hint on a builtin, which plain `lint` displays but does not fail
    /// on) must fail the CLI gate with `--warnings-as-errors`, matching a
    /// REPL `load-project` verdict.
    #[test]
    fn lint_warnings_as_errors_fails_on_repl_only_dnu() {
        let temp = tempfile::TempDir::new().unwrap();
        let root = temp.path();
        std::fs::write(
            root.join("beamtalk.toml"),
            "[package]\nname = \"recheck_probe\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        let src_dir = root.join("src");
        std::fs::create_dir_all(&src_dir).unwrap();
        std::fs::write(
            src_dir.join("recheck_probe.bt"),
            "Value subclass: RecheckProbe\n\n  probe => 42 foo\n",
        )
        .unwrap();

        let path = root.to_str().unwrap().to_string();
        // Plain lint is the lint-severity gate: the DNU hint is displayed
        // but does not fail the command.
        let plain = run_lint(&path, OutputFormat::Text, false);
        assert!(
            plain.is_ok(),
            "plain lint must stay green on a DNU hint: {plain:?}"
        );
        // The parity gate fails on the same source, like REPL load-project.
        let strict = run_lint(&path, OutputFormat::Text, true);
        assert!(
            strict.is_err(),
            "lint --warnings-as-errors must fail on the REPL-only DNU hint"
        );
        let msg = format!("{:?}", strict.unwrap_err());
        assert!(
            msg.contains("type/DNU"),
            "failure must name the parity gate, got: {msg}"
        );
    }
}
