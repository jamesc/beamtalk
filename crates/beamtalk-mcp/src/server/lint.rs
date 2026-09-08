// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Offline lint/diagnostic-summary analysis pipeline shared by the `lint` and
//! `diagnostic_summary` MCP tools (`server::tools::diagnostics`).
//!
//! Runs the same two-pass parse + semantic-analysis pipeline as CLI `beamtalk
//! lint` (BT-2052): Pass 1 parses every file in the package and extracts
//! class metadata so cross-file references resolve; Pass 2 analyses each
//! target file against that cross-file context. Everything here is pure and
//! filesystem/offline — no REPL connection required, which is what lets both
//! MCP tools work without a live workspace.

use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};

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
}

/// BT-2152: Run the shared three-step lint analysis pipeline for a single
/// module. Callers pre-filter `parse_diags` per their severity requirements
/// and pass them in; this helper appends lint-pass results, runs semantic
/// analysis with cross-file class context, filters analysis diagnostics by
/// `category.is_some()`, applies `@expect` directives, and returns the
/// resulting diagnostics together with the `ClassHierarchy` (used by
/// `compute_diagnostic_summary` for type inference).
///
/// `has_package_dependencies` mirrors `beamtalk lint`'s
/// `CompilerOptions::has_package_dependencies` (BT-2794/BT-2823): true when
/// the project's manifest declares `[dependencies]`, regardless of whether
/// any of them could be resolved on disk.
///
/// `native_type_registry` (BT-2858) mirrors `beamtalk lint`'s FFI type
/// registry (BT-2851/BT-2134): when `Some`, `(Erlang m) f:` calls get return
/// type inference and argument-type checks from the registry instead of
/// falling back to `Dynamic(UntypedFfi)` — the same registry `beamtalk
/// build`/`beamtalk lint` use, so MCP `lint`/`diagnostic_summary` never
/// diverge from them on which Erlang calls are seen as typed.
///
/// `current_package` (BT-2921) mirrors `beamtalk lint`'s
/// `CompilerOptions::current_package`: when `Some`, `check_class_visibility`/
/// `check_alias_leaked_visibility` (E0401/E0402/E0403) actually run — they
/// are gated on `current_package: Some(_)` and silently emit zero
/// diagnostics otherwise.
///
/// `source` (BT-3257) is the module's raw source text — mirroring
/// `queries::diagnostic_provider::compute_project_diagnostics_with_analysis`
/// (BT-3240) and `beamtalk lint`'s `collect_diagnostics`: needed so the
/// near-miss `// === Name ===` divider check can scan `source` directly
/// (`beamtalk_core::near_miss_divider::check_near_miss_dividers`) instead of relying on
/// the AST's `Comment::span`, which is actually the *following
/// declaration's* span, not the comment's own.
///
/// `is_stub_file` (BT-3398) mirrors the LSP's `ProjectIndex::is_stub_file`
/// fix (review follow-up on #3679): this crate builds its own
/// `AnalysisContext` directly rather than going through
/// `beamtalk-language-service`'s shared `diagnostic_provider.rs`, so it was
/// never touched by that fix and always analysed every file — including a
/// legitimate `stubs/lists.bt`'s `declare native:` blocks — as if it lived
/// outside `stubs/`. Callers derive this from the file path being analysed
/// (`beamtalk_project::package::is_under_stubs_dir`, called once per
/// top-level `path` argument below rather than by this per-file helper).
///
/// `file_stem` (BT-3431) is the target file's basename without extension,
/// passed to `check_class_file_name_agreement` so MCP `lint`/
/// `diagnostic_summary` report the same file-name/class-name mismatch
/// `beamtalk build`/`beamtalk lint`/the LSP do — `None` for callers with no
/// real file backing the module skips the check.
#[allow(clippy::too_many_arguments)] // BT-3398 added is_stub_file; each param is load-bearing context, same as `beamtalk lint`'s `collect_diagnostics`.
pub(crate) fn run_module_analysis(
    module: &beamtalk_core::ast::Module,
    source: &str,
    all_class_infos: &[beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo],
    mut diags: Vec<beamtalk_core::source_analysis::Diagnostic>,
    has_package_dependencies: bool,
    native_type_registry: Option<
        std::sync::Arc<beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>,
    >,
    current_package: Option<&str>,
    is_stub_file: bool,
    file_stem: Option<&str>,
) -> (
    Vec<beamtalk_core::source_analysis::Diagnostic>,
    beamtalk_core::semantic_analysis::ClassHierarchy,
) {
    use beamtalk_core::semantic_analysis::ClassHierarchy;

    diags.extend(beamtalk_lint::run_lint_passes(module));

    let cross_file_classes = ClassHierarchy::cross_file_class_infos(all_class_infos, module);
    let options = beamtalk_core::CompilerOptions {
        has_package_dependencies,
        current_package: current_package.map(str::to_string),
        ..Default::default()
    };
    let analysis_ctx = beamtalk_core::semantic_analysis::AnalysisContext::default()
        .with_options(&options)
        .with_pre_loaded_classes(cross_file_classes)
        .with_native_type_registry(native_type_registry)
        .with_is_stub_file(is_stub_file);
    let analysis_result = beamtalk_core::semantic_analysis::analyse_full(module, analysis_ctx);
    diags.extend(
        analysis_result
            .diagnostics
            .into_iter()
            .filter(|d| d.category.is_some()),
    );

    // BT-3431: Validate the file name agrees with the class it declares —
    // `analyse_full` doesn't run this check itself (see
    // `check_class_file_name_agreement`'s doc), so it must be called
    // explicitly here, mirroring `compute_project_diagnostics_with_analysis`.
    diags.extend(
        beamtalk_core::semantic_analysis::module_validator::check_class_file_name_agreement(
            module, file_stem,
        ),
    );

    beamtalk_language_service::queries::diagnostic_provider::apply_expect_directives(
        module, &mut diags,
    );

    // BT-3257: mirrors `compute_project_diagnostics_with_analysis`'s
    // placement — appended after `apply_expect_directives` because a
    // near-miss-divider comment's span (the comment's own line) can never
    // be contained in any `@expect`-annotated declaration's target span, so
    // running it through that pass first would be a no-op at best. See that
    // function's BT-3240 comment for the full reasoning.
    beamtalk_core::near_miss_divider::check_near_miss_dividers(source, &mut diags);

    (diags, analysis_result.class_hierarchy)
}

/// BT-2858: Build the Erlang FFI native-type registry for `path`'s package,
/// the same way `beamtalk lint` does (BT-2134/BT-2851's `extract_type_specs`,
/// now shared via `beamtalk_cli::native_type_specs`) — rather than reading a
/// possibly-absent/stale on-disk `_build/type_cache/` written by a *previous*
/// `beamtalk build`. Returns `None` outside a manifest-backed package or when
/// extraction finds no `.beam` files (e.g. runtime not yet compiled).
pub(crate) fn build_native_type_registry(
    path: &str,
) -> Option<std::sync::Arc<beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>> {
    let project_root = beamtalk_project::package::find_package_root(std::path::Path::new(path))?;
    let project_root = camino::Utf8PathBuf::from_path_buf(project_root).ok()?;
    let layout = beamtalk_cli::build_layout::BuildLayout::new(&project_root);
    beamtalk_cli::native_type_specs::extract_project_type_specs(&layout).map(std::sync::Arc::new)
}

/// BT-2014: Compute a diagnostic summary (counts + type coverage) for a path.
///
/// Runs the same two-pass parse + semantic-analysis pipeline as `beamtalk lint`,
/// aggregates all diagnostics via the shared `DiagnosticSummary` type, and
/// additionally computes type-coverage statistics. Returns a JSON-serializable
/// value suitable for direct MCP tool output.
#[allow(clippy::too_many_lines)] // Multi-pass pipeline is inherently sequential.
pub(crate) fn compute_diagnostic_summary(path: &str) -> serde_json::Value {
    use beamtalk_core::semantic_analysis::{ClassHierarchy, CoverageReport, infer_types};
    use beamtalk_core::source_analysis::{DiagnosticSummary, category_name};

    let source_files = match resolve_source_files(path) {
        Ok(files) => files,
        Err(result) => {
            // BT-2031: Surface the actual error (permission, IO, path-not-found)
            // instead of collapsing to a generic "no files found" message. Include
            // the file context from the diagnostic since the message alone may not
            // name the offending path.
            let error_msg = result.errors.first().map_or_else(
                || format!("No .bt source files found in '{path}'"),
                |e| {
                    if e.file.is_empty() {
                        format!("{}: {}", path, e.message)
                    } else {
                        format!("{}: {}", e.file, e.message)
                    }
                },
            );
            return serde_json::json!({
                "error": error_msg,
                "files_checked": 0,
                "total": 0,
            });
        }
    };

    // BT-2052: Determine the full extraction set (package-wide src/ + test/)
    // so cross-file class references resolve correctly.
    let (extraction_files, target_set) = resolve_extraction_files(path, &source_files);

    // BT-2921: Resolve the current package once, mirroring `beamtalk lint`,
    // so E0401/E0402/E0403 visibility checks fire the same way in MCP.
    let current_package = resolve_current_package(path);

    // BT-3398: Resolve the package root once so each file's `is_stub_file`
    // check below (`beamtalk_project::package::is_under_stubs_dir`) doesn't
    // re-walk ancestors per file — mirrors `current_package`/
    // `native_type_registry`'s own one-time-per-call resolution above.
    // `None` outside a manifest-backed package, matching
    // `AnalysisContext::is_stub_file`'s own conservative default (no file is
    // ever treated as a stub without a known project root).
    let project_root = beamtalk_project::package::find_package_root(std::path::Path::new(path));

    // Pass 1: Parse all files and extract class metadata.
    let mut all_class_infos = Vec::new();
    let mut parsed_files = Vec::new();
    let mut unreadable_files: Vec<String> = Vec::new();
    let mut unreadable_target_files: Vec<String> = Vec::new();

    for file in &extraction_files {
        let Ok(source) = std::fs::read_to_string(file) else {
            // BT-2067: Track unreadable target files separately so the caller
            // sees a clear error instead of a deceptively-clean `files_checked=0`
            // summary. BT-2056: Unreadable package-only files produce a softer
            // warning since cross-file class extraction may be incomplete but
            // the targets themselves were still checked.
            let canonical = canonicalize_or_clone(file);
            if target_set.contains(&canonical) {
                unreadable_target_files.push(file.to_string_lossy().into_owned());
            } else {
                unreadable_files.push(file.to_string_lossy().into_owned());
            }
            continue;
        };
        let file_str = file.to_string_lossy().into_owned();
        let tokens = lex_with_eof(&source);
        let (module, parse_diags) = parse(tokens);
        let mut class_infos = ClassHierarchy::extract_class_infos(&module);
        // BT-2921: Stamp the package per-file, same as build's/lint's Pass 1 —
        // without this, a same-package class defined in a sibling file is
        // indistinguishable from a builtin/REPL class and never flagged as a
        // leak by `check_class_visibility`.
        if let Some(pkg) = current_package.as_deref() {
            ClassHierarchy::stamp_package_on_infos(&mut class_infos, pkg);
        }
        all_class_infos.extend(class_infos);

        let canonical = canonicalize_or_clone(file);
        if target_set.contains(&canonical) {
            parsed_files.push((file_str, source, module, parse_diags));
        }
    }

    // BT-2823: Merge dependency class metadata so cross-file references to
    // classes defined only in a git/path dependency (declared in
    // beamtalk.toml) resolve the same way `beamtalk build` does.
    let has_package_dependencies = merge_dependency_class_infos(path, &mut all_class_infos);

    // BT-2858: Populate the FFI type registry the same way `beamtalk lint` does.
    let native_type_registry = build_native_type_registry(path);

    // Pass 2: Analyse each file and collect diagnostics + coverage.
    let mut all_diags = Vec::new();
    let mut coverage = CoverageReport {
        classes: Vec::new(),
        dynamic_entries: Vec::new(),
        total_expressions: 0,
        typed_expressions: 0,
    };

    for (file_str, source, module, parse_diags) in &parsed_files {
        // Pre-filter to lint-severity parse diagnostics (compute_diagnostic_summary
        // intentionally drops parse errors/warnings here — they're surfaced via
        // other channels).
        let initial_diags: Vec<_> = parse_diags
            .iter()
            .filter(|d| d.severity == Severity::Lint)
            .cloned()
            .collect();

        let is_stub_file = project_root.as_deref().is_some_and(|root| {
            beamtalk_project::package::is_under_stubs_dir(root, std::path::Path::new(file_str))
        });
        let (file_diags, class_hierarchy) = run_module_analysis(
            module,
            source,
            &all_class_infos,
            initial_diags,
            has_package_dependencies,
            native_type_registry.clone(),
            current_package.as_deref(),
            is_stub_file,
            std::path::Path::new(file_str)
                .file_stem()
                .and_then(std::ffi::OsStr::to_str),
        );

        all_diags.extend(file_diags);

        // Type coverage.
        let type_map = infer_types(module, &class_hierarchy, native_type_registry.as_deref());
        let file_report = CoverageReport::from_module(module, &type_map, file_str, false);
        coverage.merge(file_report);
    }

    // BT-2031: Count only files that were actually read and analysed,
    // not all resolved files (some may have been unreadable).
    let files_checked = parsed_files.len();
    let summary = DiagnosticSummary::from_diagnostics(&all_diags, files_checked);
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

    let dynamic_pct = if coverage.total_expressions > 0 {
        let typed_pct = coverage.coverage_percent();
        ((100.0 - typed_pct) * 10.0).round() / 10.0
    } else {
        0.0
    };

    // BT-2056: Include unreadable package files in the output so the caller
    // knows cross-file class extraction may be incomplete.
    let mut result = serde_json::json!({
        "files_checked": files_checked,
        "totals_by_severity": {
            "error": totals.error,
            "warning": totals.warning,
            "lint": totals.lint,
            "hint": totals.hint,
        },
        "totals_by_category": by_category,
        "total": summary.total(),
        "type_coverage": {
            "typed": coverage.typed_expressions,
            "total": coverage.total_expressions,
            "dynamic_percent": dynamic_pct,
        },
    });
    if !unreadable_files.is_empty() {
        result["unreadable_package_files"] = serde_json::json!(unreadable_files);
    }
    // BT-2067: Surface unreadable target files as a structured field and a
    // top-level `error` message so callers treating the response as a summary
    // do not mistake zero-checked-files for a clean result.
    if !unreadable_target_files.is_empty() {
        let joined = unreadable_target_files.join(", ");
        result["unreadable_target_files"] = serde_json::json!(unreadable_target_files);
        result["error"] = serde_json::json!(format!("Failed to read target file(s): {joined}"));
    }
    result
}

/// Canonicalize `path`, falling back to a plain clone when the path cannot be
/// resolved (e.g. it does not yet exist or permissions are denied). Used as a
/// normalized key for path-based deduplication in the two-pass lint pipeline.
fn canonicalize_or_clone(path: &std::path::Path) -> std::path::PathBuf {
    std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf())
}

/// Resolve `path` to a list of `.bt` source files, or return a `LintResult`
/// containing a single error diagnostic explaining why no files could be found.
///
/// Returns `Vec<PathBuf>` (not `Utf8PathBuf`) so that files with non-UTF-8
/// names are preserved rather than silently dropped.
fn resolve_source_files(path: &str) -> Result<Vec<std::path::PathBuf>, LintResult> {
    use beamtalk_core::file_walker::FileWalker;

    let source_path = std::path::Path::new(path);
    let files = FileWalker::lint_files()
        .walk_pathbuf(source_path)
        .map_err(|e| lint_error(path, e.to_string()))?;
    if files.is_empty() {
        return Err(lint_error(
            path,
            format!("No .bt source files found in '{path}'"),
        ));
    }
    Ok(files)
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
    }
}

/// BT-2060: Package root / source-file resolution now lives in
/// [`beamtalk_project::package`] so CLI lint and MCP lint share one
/// implementation.
///
/// This thin wrapper exists only to keep the MCP call sites readable — it
/// forwards to
/// [`beamtalk_project::package::resolve_extraction_files`] and adapts
/// the `&str` path argument the MCP layer carries around.
fn resolve_extraction_files(
    path: &str,
    source_files: &[std::path::PathBuf],
) -> (
    Vec<std::path::PathBuf>,
    std::collections::HashSet<std::path::PathBuf>,
) {
    beamtalk_project::package::resolve_extraction_files(std::path::Path::new(path), source_files)
}

/// BT-2921: Resolve the current package name from `path`'s `beamtalk.toml`,
/// mirroring `beamtalk lint`'s `find_manifest_full` resolution
/// (`beamtalk_cli::commands::lint::run_lint`) so `CompilerOptions::current_package`
/// gets set the same way for MCP `lint`/`diagnostic_summary` as it does for
/// the CLI. Without this, `check_class_visibility`/`check_alias_leaked_visibility`
/// (E0401/E0402/E0403) never fire — both are gated on `current_package: Some(_)`.
///
/// Returns `None` outside a manifest-backed package or when the manifest is
/// malformed (visibility checks conservatively disabled, matching CLI lint's
/// error-path behaviour).
fn resolve_current_package(path: &str) -> Option<String> {
    let project_root = beamtalk_project::package::find_package_root(std::path::Path::new(path))?;
    let project_root = camino::Utf8PathBuf::from_path_buf(project_root).ok()?;
    match beamtalk_cli::manifest::find_manifest_full(&project_root) {
        Ok(Some(m)) => Some(m.package.name),
        Ok(None) => None,
        Err(e) => {
            tracing::warn!(
                error = %e,
                "Failed to parse beamtalk.toml for MCP lint/diagnostic_summary; \
                 E0401/E0402/E0403 visibility checks disabled"
            );
            None
        }
    }
}

/// BT-2823: Merge class metadata from `path`'s package dependencies (as
/// declared in `beamtalk.toml`) into `all_class_infos`, so `Unresolved
/// class` diagnostics see the same class hierarchy as `beamtalk
/// build`/`beamtalk lint` for classes defined only in a dependency.
///
/// Delegates to [`beamtalk_cli::dependency_classes::resolve_dependency_class_infos`],
/// which is filesystem-only and best-effort — it never fetches over the
/// network, so this never turns an offline `lint`/`diagnostic_summary` call
/// into one with network side effects. Dependencies that have never been
/// fetched by a prior `beamtalk build` are silently skipped.
///
/// Returns whether the project's manifest declares any dependencies, for use
/// as `CompilerOptions::has_package_dependencies` (BT-2794).
fn merge_dependency_class_infos(
    path: &str,
    all_class_infos: &mut Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
) -> bool {
    let Some(project_root) =
        beamtalk_project::package::find_package_root(std::path::Path::new(path))
    else {
        return false;
    };
    let Ok(project_root) = camino::Utf8PathBuf::from_path_buf(project_root) else {
        return false;
    };

    let (has_package_dependencies, dep_class_infos) =
        beamtalk_cli::dependency_classes::resolve_dependency_class_infos(&project_root);
    all_class_infos.extend(dep_class_infos);
    has_package_dependencies
}

/// Run lint passes on `path` (file or directory) and return structured results.
///
/// BT-2052: Uses a two-pass pipeline mirroring CLI `beamtalk lint`:
/// - Pass 1: Parse all files in the package and extract class metadata
/// - Pass 2: Analyse each target file with cross-file class context
///
/// Without cross-file classes, the MCP lint produces different diagnostics
/// than the CLI — e.g. `@expect type` annotations are falsely reported as
/// stale because the type/DNU diagnostics they suppress require cross-file
/// class resolution to appear.
#[allow(clippy::too_many_lines)] // Two-pass pipeline is inherently sequential.
pub(crate) fn run_lint_structured(path: &str) -> LintResult {
    use beamtalk_core::semantic_analysis::ClassHierarchy;

    let source_files = match resolve_source_files(path) {
        Ok(files) => files,
        Err(result) => return result,
    };

    // BT-2052: Determine the full extraction set (package-wide src/ + test/)
    // so cross-file class references resolve correctly.
    let (extraction_files, target_set) = resolve_extraction_files(path, &source_files);

    // BT-2921: Resolve the current package once, mirroring `beamtalk lint`,
    // so E0401/E0402/E0403 visibility checks fire the same way in MCP.
    let current_package = resolve_current_package(path);

    // BT-3398: Resolve the package root once, mirroring
    // `compute_diagnostic_summary`'s own one-time resolution above, so each
    // target file's `is_stub_file` can be derived via
    // `beamtalk_project::package::is_under_stubs_dir` without re-walking
    // ancestors per file.
    let project_root = beamtalk_project::package::find_package_root(std::path::Path::new(path));

    // Pass 1: Parse files and extract class metadata.
    let mut all_class_infos = Vec::new();
    let mut parsed_targets: Vec<(
        std::path::PathBuf,
        String,
        beamtalk_core::ast::Module,
        Vec<beamtalk_core::source_analysis::Diagnostic>,
    )> = Vec::new();

    let mut warnings = Vec::new();
    let mut errors = Vec::new();

    for file in &extraction_files {
        let Ok(source) = std::fs::read_to_string(file) else {
            let canonical = canonicalize_or_clone(file);
            if target_set.contains(&canonical) {
                errors.push(LintDiagnostic {
                    file: file.to_string_lossy().into_owned(),
                    line: None,
                    message: format!("Failed to read '{}'", file.display()),
                    severity: "error",
                });
            } else {
                // BT-2056: Surface a warning when a package-extraction file
                // (chosen by the resolver but not a direct lint target) cannot
                // be read. Without this, cross-file class extraction silently
                // drops the file, potentially re-introducing diagnostic
                // divergence from CLI lint.
                warnings.push(LintDiagnostic {
                    file: file.to_string_lossy().into_owned(),
                    line: None,
                    message: format!(
                        "Failed to read package file '{}'; cross-file class extraction may be incomplete",
                        file.display()
                    ),
                    severity: "warning",
                });
            }
            continue;
        };

        let tokens = lex_with_eof(&source);
        let (module, parse_diags) = parse(tokens);

        let mut class_infos = ClassHierarchy::extract_class_infos(&module);
        // BT-2921: Stamp the package per-file, same as build's/lint's Pass 1 —
        // without this, a same-package class defined in a sibling file is
        // indistinguishable from a builtin/REPL class and never flagged as a
        // leak by `check_class_visibility`.
        if let Some(pkg) = current_package.as_deref() {
            ClassHierarchy::stamp_package_on_infos(&mut class_infos, pkg);
        }
        all_class_infos.extend(class_infos);

        let canonical = canonicalize_or_clone(file);
        if target_set.contains(&canonical) {
            parsed_targets.push((file.clone(), source, module, parse_diags));
        }
    }

    // BT-2823: Merge dependency class metadata so cross-file references to
    // classes defined only in a git/path dependency (declared in
    // beamtalk.toml) resolve the same way `beamtalk build` does.
    let has_package_dependencies = merge_dependency_class_infos(path, &mut all_class_infos);

    // BT-2858: Populate the FFI type registry the same way `beamtalk lint` does.
    let native_type_registry = build_native_type_registry(path);

    // Pass 2: Analyse each target file with cross-file class context.
    for (file, source, module, parse_diags) in parsed_targets {
        // Include parse errors (syntax problems) and warnings so files with
        // broken syntax or parser-emitted warnings don't silently appear clean.
        // Hint-severity diagnostics (DNU hints) are excluded as they are
        // informational and belong to the check/compile workflow.
        let initial_diags: Vec<_> = parse_diags
            .into_iter()
            .filter(|d| {
                matches!(
                    d.severity,
                    Severity::Error | Severity::Warning | Severity::Lint
                )
            })
            .collect();

        // BT-1587 / BT-2052: run_module_analysis runs lint passes, semantic
        // analysis with cross-file class context (mirroring CLI `beamtalk lint`),
        // and applies @expect directives (BT-1476).
        let is_stub_file = project_root
            .as_deref()
            .is_some_and(|root| beamtalk_project::package::is_under_stubs_dir(root, &file));
        let (lint_diags, _) = run_module_analysis(
            &module,
            &source,
            &all_class_infos,
            initial_diags,
            has_package_dependencies,
            native_type_registry.clone(),
            current_package.as_deref(),
            is_stub_file,
            file.file_stem().and_then(std::ffi::OsStr::to_str),
        );

        let file_name = file.to_string_lossy().into_owned();
        for diag in &lint_diags {
            let line = diag.span.line_number(&source);
            let severity = match diag.severity {
                Severity::Error => "error",
                Severity::Warning | Severity::Lint | Severity::Hint => "warning",
            };
            // BT-1588: Include notes in the message for origin tracing
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
                file: file_name.clone(),
                line: Some(line),
                message,
                severity,
            };
            if diag.severity == Severity::Error {
                errors.push(entry);
            } else {
                warnings.push(entry);
            }
        }
    }

    let total = warnings.len() + errors.len();
    LintResult {
        warnings,
        errors,
        total,
    }
}
