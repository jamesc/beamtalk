// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Offline lint/diagnostic-summary analysis pipeline shared by the `lint` and
//! `diagnostic_summary` MCP tools (`server::tools::diagnostics`).
//!
//! Runs the same two-pass parse + semantic-analysis pipeline as CLI `beamtalk
//! lint`: Pass 1 parses every file in the package and extracts
//! class metadata so cross-file references resolve; Pass 2 analyses each
//! target file against that cross-file context. Everything here is pure and
//! filesystem/offline — no REPL connection required, which is what lets both
//! MCP tools work without a live workspace.

use beamtalk_core::compilation::diagnostics_policy::is_live_recheck_failure;
use beamtalk_core::semantic_analysis::protocol_source::{
    dedupe_provision_diagnostics, provision_report_target,
};
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
    /// Lint-severity diagnostics: the set plain CLI `beamtalk lint` fails on.
    pub(crate) lint_failures: usize,
    /// Diagnostics a REPL `load-project` rejects — any `Error` plus the
    /// non-error `Dnu`/`Type` findings — classified by the shared
    /// [`is_live_recheck_failure`] rule CLI `beamtalk lint
    /// --warnings-as-errors` uses (BT-4336). File-level read errors are not
    /// counted here; they are already in `errors`.
    pub(crate) live_recheck_failures: usize,
}

impl LintResult {
    /// Whether the `lint` tool reports failure (`is_error`). Without
    /// `warnings_as_errors` only `errors` fail it; with it, it applies CLI
    /// `beamtalk lint --warnings-as-errors`' gate — Lint-severity diagnostics
    /// or the live-recheck set — plus file-level errors (BT-4336). The gate
    /// rule is shared, but this crate's analysis pipeline is its own: it
    /// does not yet pass the CLI's cross-file extensions, pre-loaded
    /// aliases/protocols or project-complete knowledge scope, and does not
    /// lint `.erl` files, so the diagnostics it gates can differ (BT-4585).
    pub(crate) fn fails(&self, warnings_as_errors: bool) -> bool {
        !self.errors.is_empty()
            || (warnings_as_errors && (self.lint_failures > 0 || self.live_recheck_failures > 0))
    }
}

/// Run the shared lint analysis pipeline for a single
/// module. Callers pre-filter `parse_diags` per their severity requirements
/// and pass them in; this helper runs semantic analysis with cross-file
/// class context, runs the lint passes against that cross-file hierarchy
/// (BT-4183), filters analysis diagnostics by `category.is_some()`, applies `@expect` directives, and returns the
/// resulting diagnostics together with the `ClassHierarchy` (used by
/// `compute_diagnostic_summary` for type inference).
///
/// `cross_file_extensions` is every package source file's extension
/// methods plus the dependencies' (ADR 0066; ADR 0070 WS3, BT-4608), as
/// `beamtalk lint` registers them, and `dependency_extensions_unknown`
/// mirrors `beamtalk lint`'s `CompilerOptions::dependency_extensions_unknown`:
/// true when the manifest declares `[dependencies]` and not all of their
/// extensions could be loaded from disk.
///
/// `native_type_registry` mirrors `beamtalk lint`'s FFI type
/// registry: when `Some`, `(Erlang m) f:` calls get return
/// type inference and argument-type checks from the registry instead of
/// falling back to `Dynamic(UntypedFfi)` — the same registry `beamtalk
/// build`/`beamtalk lint` use, so MCP `lint`/`diagnostic_summary` never
/// diverge from them on which Erlang calls are seen as typed.
///
/// `current_package` mirrors `beamtalk lint`'s
/// `CompilerOptions::current_package`: when `Some`, `check_class_visibility`/
/// `check_alias_leaked_visibility` (E0401/E0402/E0403) actually run — they
/// are gated on `current_package: Some(_)` and silently emit zero
/// diagnostics otherwise.
///
/// `source` is the module's raw source text — mirroring
/// `queries::diagnostic_provider::compute_project_diagnostics_with_analysis`
/// and `beamtalk lint`'s `collect_diagnostics`: needed so the
/// near-miss `// === Name ===` divider check can scan `source` directly
/// (`beamtalk_core::near_miss_divider::check_near_miss_dividers`) instead of relying on
/// the AST's `Comment::span`, which is actually the *following
/// declaration's* span, not the comment's own.
///
/// `is_stub_file` mirrors the LSP's `ProjectIndex::is_stub_file`
/// treatment: this crate builds its own
/// `AnalysisContext` directly rather than going through
/// `beamtalk-language-service`'s shared `diagnostic_provider.rs`, so without
/// this parameter it would analyse every file — including a
/// legitimate `stubs/lists.bt`'s `declare native:` blocks — as if it lived
/// outside `stubs/`. Callers derive this from the file path being analysed
/// (`beamtalk_project::package::is_under_stubs_dir`, called once per
/// top-level `path` argument below rather than by this per-file helper).
///
/// `file_stem` is the target file's basename without extension,
/// passed to `check_class_file_name_agreement` so MCP `lint`/
/// `diagnostic_summary` report the same file-name/class-name mismatch
/// `beamtalk build`/`beamtalk lint`/the LSP do — `None` for callers with no
/// real file backing the module skips the check.
///
/// `known_erlang_modules` mirrors `beamtalk lint`'s
/// `CompilerOptions::known_erlang_modules` (BT-4022): the package's
/// hex-dep/native ebin modules plus own `native/*.erl`, so the
/// `unresolved_ffi` check agrees with `build`/`lint`/LSP.
///
/// `diagnostics_overrides` is the package's `[diagnostics]` table
/// (BT-4377), applied by the shared lint pipeline after `@expect` so MCP
/// `lint` reports the same severities as `beamtalk lint`/`build`/the REPL.
#[allow(clippy::too_many_arguments)] // Each param is load-bearing context, same as `beamtalk lint`'s `collect_diagnostics`.
pub(crate) fn run_module_analysis(
    module: &beamtalk_core::ast::Module,
    source: &str,
    all_class_infos: &[beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo],
    pre_loaded_protocol_defs: Vec<beamtalk_core::ast::ProtocolDefinition>,
    protocol_sources: beamtalk_core::semantic_analysis::ProtocolSourceMap,
    mut diags: Vec<beamtalk_core::source_analysis::Diagnostic>,
    cross_file_extensions: &beamtalk_core::compilation::extension_index::ExtensionIndex,
    dependency_extensions_unknown: bool,
    native_type_registry: Option<
        std::sync::Arc<beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>,
    >,
    current_package: Option<&str>,
    is_stub_file: bool,
    file_stem: Option<&str>,
    known_erlang_modules: &std::collections::HashSet<String>,
    diagnostics_overrides: &beamtalk_core::compilation::diagnostics_policy::DiagnosticsTable,
) -> (
    Vec<beamtalk_core::source_analysis::Diagnostic>,
    beamtalk_core::semantic_analysis::ClassHierarchy,
) {
    use beamtalk_core::semantic_analysis::ClassHierarchy;

    let cross_file_classes = ClassHierarchy::cross_file_class_infos(all_class_infos, module);
    let options = beamtalk_core::CompilerOptions {
        dependency_extensions_unknown,
        current_package: current_package.map(str::to_string),
        known_erlang_modules: known_erlang_modules.clone(),
        ..Default::default()
    };
    let analysis_ctx = beamtalk_core::semantic_analysis::AnalysisContext::default()
        .with_options(&options)
        .with_pre_loaded_classes(cross_file_classes)
        // Carried protocol ASTs (ADR 0127 §10a; BT-3591): the package's own,
        // then the dependencies' — see `package_then_dependency_protocol_defs`.
        .with_pre_loaded_protocol_defs(pre_loaded_protocol_defs)
        .with_protocol_sources(protocol_sources)
        .with_native_type_registry(native_type_registry)
        .with_cross_file_extensions(cross_file_extensions)
        .with_is_stub_file(is_stub_file);
    let class_hierarchy =
        beamtalk_language_service::queries::diagnostic_provider::run_post_analysis_lint_pipeline(
            module,
            source,
            analysis_ctx,
            file_stem,
            diagnostics_overrides,
            &mut diags,
            // BT-4183: the lint passes run against analyse_full's cross-file
            // hierarchy, as `beamtalk lint` does.
            |hierarchy| beamtalk_lint::run_lint_passes_with_hierarchy(module, hierarchy),
        );

    (diags, class_hierarchy)
}

/// Build the Erlang FFI native-type registry for `path`'s package,
/// the same way `beamtalk lint` does (via the shared `extract_type_specs`,
/// exposed as `beamtalk_cli::native_type_specs`) — rather than reading a
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

/// Collect the project-known Erlang modules for `path`'s package (BT-4022),
/// the same way `beamtalk lint` does — via the shared
/// `collect_project_known_erlang_modules`, anchored on the package root
/// found from `path`. Returns an empty set outside a manifest-backed
/// package (or when the root is not UTF-8), preserving the
/// static-list-only `unresolved_ffi` behaviour there.
pub(crate) fn collect_known_erlang_modules(path: &str) -> std::collections::HashSet<String> {
    let Some(project_root) =
        beamtalk_project::package::find_package_root(std::path::Path::new(path))
    else {
        return std::collections::HashSet::new();
    };
    let Ok(project_root) = camino::Utf8PathBuf::from_path_buf(project_root) else {
        return std::collections::HashSet::new();
    };
    beamtalk_core::ffi_type_specs::collect_project_known_erlang_modules(&project_root)
}

/// Compute a diagnostic summary (counts + type coverage) for a path.
///
/// Runs the same two-pass parse + semantic-analysis pipeline as `beamtalk lint`,
/// aggregates all diagnostics via the shared `DiagnosticSummary` type, and
/// additionally computes type-coverage statistics. Returns a JSON-serializable
/// value suitable for direct MCP tool output.
#[allow(clippy::too_many_lines)] // Multi-pass pipeline is inherently sequential.
pub(crate) fn compute_diagnostic_summary(path: &str) -> serde_json::Value {
    use beamtalk_core::semantic_analysis::{CoverageReport, infer_types};
    use beamtalk_core::source_analysis::DiagnosticSummary;

    let source_files = match resolve_source_files(path) {
        Ok(files) => files,
        Err(result) => {
            // Surface the actual error (permission, IO, path-not-found)
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

    // Determine the full extraction set (package-wide src/ + test/)
    // so cross-file class references resolve correctly.
    let (extraction_files, target_set) = resolve_extraction_files(path, &source_files);

    // Resolve the current package once, mirroring `beamtalk lint`,
    // so E0401/E0402/E0403 visibility checks fire the same way in MCP.
    let current_package = resolve_current_package(path);

    // Resolve the package root once so each file's `is_stub_file`
    // check below (`beamtalk_project::package::is_under_stubs_dir`) doesn't
    // re-walk ancestors per file — mirrors `current_package`/
    // `native_type_registry`'s own one-time-per-call resolution above.
    // `None` outside a manifest-backed package, matching
    // `AnalysisContext::is_stub_file`'s own conservative default (no file is
    // ever treated as a stub without a known project root).
    let project_root = beamtalk_project::package::find_package_root(std::path::Path::new(path));

    // Pass 1: Parse all files and extract class metadata.
    // Track unreadable target files separately so the caller sees a clear error
    // instead of a deceptively-clean `files_checked=0` summary. Unreadable
    // package-only files produce a softer warning since cross-file class
    // extraction may be incomplete but the targets themselves were still checked.
    let mut unreadable_files: Vec<String> = Vec::new();
    let mut unreadable_target_files: Vec<String> = Vec::new();
    let mut pass1 = run_lint_pass1(
        &extraction_files,
        &target_set,
        current_package.as_deref(),
        |file, is_target| {
            let name = file.to_string_lossy().into_owned();
            if is_target {
                unreadable_target_files.push(name);
            } else {
                unreadable_files.push(name);
            }
        },
    );
    let LintAnalysisContext {
        extension_index,
        dependency_extensions_unknown,
        native_type_registry,
        known_erlang_modules,
        diagnostics_overrides,
        pre_loaded_protocol_defs,
        protocol_sources,
    } = build_lint_analysis_context(
        path,
        &mut pass1.all_class_infos,
        &pass1.trait_users,
        std::mem::take(&mut pass1.extension_index),
        current_package.as_deref(),
    );
    let LintPass1State {
        all_class_infos,
        parsed_targets: parsed_files,
        ..
    } = pass1;

    // Pass 2: Analyse each file and collect diagnostics + coverage.
    let mut all_diags = Vec::new();
    let mut all_provision_diags = Vec::new();
    let mut coverage = CoverageReport {
        classes: Vec::new(),
        dynamic_entries: Vec::new(),
        total_expressions: 0,
        typed_expressions: 0,
    };

    for (file, source, module, parse_diags) in &parsed_files {
        let file_str = file.to_string_lossy().into_owned();
        // Pre-filter to lint-severity parse diagnostics (compute_diagnostic_summary
        // intentionally drops parse errors/warnings here — they're surfaced via
        // other channels).
        let initial_diags: Vec<_> = parse_diags
            .iter()
            .filter(|d| d.severity == Severity::Lint)
            .cloned()
            .collect();

        let is_stub_file = project_root
            .as_deref()
            .is_some_and(|root| beamtalk_project::package::is_under_stubs_dir(root, file));
        let (file_diags, class_hierarchy) = run_module_analysis(
            module,
            source,
            &all_class_infos,
            pre_loaded_protocol_defs.clone(),
            protocol_sources.clone(),
            initial_diags,
            &extension_index,
            dependency_extensions_unknown,
            native_type_registry.clone(),
            current_package.as_deref(),
            is_stub_file,
            file.file_stem().and_then(std::ffi::OsStr::to_str),
            &known_erlang_modules,
            &diagnostics_overrides,
        );

        let (provision_diags, file_diags) =
            split_provision_diagnostics(file_diags, &pre_loaded_protocol_defs);
        all_provision_diags.extend(provision_diags);
        all_diags.extend(file_diags);

        // Type coverage.
        let type_map = infer_types(module, &class_hierarchy, native_type_registry.as_deref());
        let file_report = CoverageReport::from_module(module, &type_map, &file_str, false);
        coverage.merge(file_report);
    }

    // Each flattened-provision diagnostic once (merged across its users),
    // as `beamtalk lint` publishes it — not once per using file.
    all_diags.extend(dedupe_provision_diagnostics(all_provision_diags));

    // Count only files that were actually read and analysed,
    // not all resolved files (some may have been unreadable).
    let files_checked = parsed_files.len();
    let summary = DiagnosticSummary::from_diagnostics(&all_diags, files_checked);

    let dynamic_pct = if coverage.total_expressions > 0 {
        let typed_pct = coverage.coverage_percent();
        ((100.0 - typed_pct) * 10.0).round() / 10.0
    } else {
        0.0
    };

    // Include unreadable package files in the output so the caller
    // knows cross-file class extraction may be incomplete.
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
    if !unreadable_files.is_empty() {
        result["unreadable_package_files"] = serde_json::json!(unreadable_files);
    }
    // Surface unreadable target files as a structured field and a
    // top-level `error` message so callers treating the response as a summary
    // do not mistake zero-checked-files for a clean result.
    if !unreadable_target_files.is_empty() {
        let joined = unreadable_target_files.join(", ");
        result["unreadable_target_files"] = serde_json::json!(unreadable_target_files);
        result["error"] = serde_json::json!(format!("Failed to read target file(s): {joined}"));
    }
    result
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
        lint_failures: 0,
        live_recheck_failures: 0,
    }
}

/// Package root / source-file resolution lives in
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

/// Resolve the current package name from `path`'s `beamtalk.toml`,
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

/// Merge class metadata from `path`'s package dependencies (as
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
/// Also collects `protocol_defs`: the full AST of every provision-bearing
/// protocol defined in a dependency (ADR 0127 §10a; BT-3591), so a
/// cross-package `uses:` can flatten in MCP `lint`/`diagnostic_summary` the
/// same way `beamtalk build` already does — see
/// `resolve_dependency_class_infos`'s doc. The package's own carried
/// protocols (a sibling source file's) are collected by Pass 1's
/// `TraitUserCollector` and put ahead of these
/// ([`package_then_dependency_protocol_defs`]); same-package `ProtocolInfo`
/// metadata (conformance/`extending:`) is still not collected here, unlike
/// CLI `beamtalk lint`'s `parse_and_extract_class_infos`.
///
/// Also merges the dependencies' extension methods into `extensions` (ADR
/// 0070 WS3, BT-4608), and returns the pre-WS3 dependency guard for
/// `CompilerOptions::dependency_extensions_unknown` — on when the manifest
/// declares dependencies and not all of their extensions were on disk to
/// load, by the rule `beamtalk lint` uses
/// (`beamtalk_project::dependency_extensions::dependency_extensions_unknown`).
fn merge_dependency_class_infos(
    path: &str,
    all_class_infos: &mut Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    all_protocol_defs: &mut Vec<beamtalk_core::ast::ProtocolDefinition>,
    protocol_sources: &mut beamtalk_core::semantic_analysis::ProtocolSourceMap,
    extensions: &mut beamtalk_core::compilation::extension_index::ExtensionIndex,
) -> bool {
    let Some(project_root) =
        beamtalk_project::package::find_package_root(std::path::Path::new(path))
    else {
        return false;
    };
    let Ok(project_root) = camino::Utf8PathBuf::from_path_buf(project_root) else {
        return false;
    };

    let scan = beamtalk_cli::dependency_classes::resolve_dependency_class_infos_with_sources(
        &project_root,
    );
    all_class_infos.extend(scan.class_infos);
    all_protocol_defs.extend(scan.protocol_defs);
    beamtalk_core::semantic_analysis::protocol_source::merge_protocol_sources(
        protocol_sources,
        scan.protocol_sources,
    );
    extensions.merge(&scan.extensions);
    beamtalk_cli::dependency_extensions::dependency_extensions_unknown(
        scan.has_package_dependencies,
        true,
        scan.extensions_complete,
    )
}

/// The protocol ASTs Pass 2 carries into each file's analysis: the package's
/// own carried protocols (`trait_users`, every source file) first, then the
/// dependencies' — the precedence `beamtalk lint`/`build` use, so a package's
/// own `Comparable` in a sibling file shadows the stdlib's for a `uses:` in
/// the file being analysed, as on every other surface (BT-3915).
fn package_then_dependency_protocol_defs(
    trait_users: &beamtalk_core::semantic_analysis::trait_expansion::TraitUserCollector,
    dependency_protocol_defs: &[beamtalk_core::ast::ProtocolDefinition],
) -> Vec<beamtalk_core::ast::ProtocolDefinition> {
    trait_users
        .protocol_defs()
        .iter()
        .chain(dependency_protocol_defs)
        .cloned()
        .collect()
}

/// Separates the diagnostics raised inside a flattened trait provision from
/// a file's own, after applying the `@expect` directives in the protocols'
/// provided methods to them — as `beamtalk lint` does (ADR 0127 §3,
/// BT-3665). The provision ones are reported once, against the protocol's
/// file ([`provision_report_target`]), after every file is analysed.
fn split_provision_diagnostics(
    mut diagnostics: Vec<beamtalk_core::source_analysis::Diagnostic>,
    protocol_defs: &[beamtalk_core::ast::ProtocolDefinition],
) -> (
    Vec<beamtalk_core::source_analysis::Diagnostic>,
    Vec<beamtalk_core::source_analysis::Diagnostic>,
) {
    beamtalk_core::compilation::diagnostics_policy::apply_protocol_expect_directives(
        protocol_defs,
        &mut diagnostics,
    );
    diagnostics.into_iter().partition(|d| d.provision.is_some())
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

/// The protocol sources Pass 2 carries, in the same precedence as
/// [`package_then_dependency_protocol_defs`]: the package's own first, then
/// the dependencies' — so a provision diagnostic of either is reported against
/// its protocol's file and line.
fn package_then_dependency_protocol_sources(
    trait_users: &beamtalk_core::semantic_analysis::trait_expansion::TraitUserCollector,
    dependency_protocol_sources: beamtalk_core::semantic_analysis::ProtocolSourceMap,
) -> beamtalk_core::semantic_analysis::ProtocolSourceMap {
    let mut sources = trait_users.protocol_sources().clone();
    beamtalk_core::semantic_analysis::protocol_source::merge_protocol_sources(
        &mut sources,
        dependency_protocol_sources,
    );
    sources
}

/// Shared post-Pass-1 context assembled by both `run_lint_structured` and
/// `compute_diagnostic_summary` before their respective Pass-2 loops.
struct LintAnalysisContext {
    /// The package's own extension methods plus its dependencies'.
    extension_index: beamtalk_core::compilation::extension_index::ExtensionIndex,
    dependency_extensions_unknown: bool,
    native_type_registry:
        Option<std::sync::Arc<beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>>,
    known_erlang_modules: std::collections::HashSet<String>,
    /// The package's `[diagnostics]` table (BT-4377); empty outside a package.
    diagnostics_overrides: beamtalk_core::compilation::diagnostics_policy::DiagnosticsTable,
    pre_loaded_protocol_defs: Vec<beamtalk_core::ast::ProtocolDefinition>,
    protocol_sources: beamtalk_core::semantic_analysis::ProtocolSourceMap,
}

/// Merge dependency class metadata and FFI registries into the context shared
/// by both lint-pipeline functions.
///
/// Called once per pipeline run, after Pass 1 has populated `all_class_infos`
/// and `trait_users`. Mutates `all_class_infos` in-place (dependency classes
/// appended; trait provisions flattened), then builds the pre-loaded
/// protocol-def and protocol-source lists for Pass 2 from `trait_users`.
fn build_lint_analysis_context(
    path: &str,
    all_class_infos: &mut Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    trait_users: &beamtalk_core::semantic_analysis::trait_expansion::TraitUserCollector,
    mut extension_index: beamtalk_core::compilation::extension_index::ExtensionIndex,
    current_package: Option<&str>,
) -> LintAnalysisContext {
    // Merge dependency class metadata so cross-file references to
    // classes defined only in a repository/path dependency (declared in
    // beamtalk.toml) resolve the same way `beamtalk build` does. Also
    // collects dependency-sourced provision-bearing protocol ASTs (ADR 0127
    // §10a; BT-3591) so a cross-package `uses:` flattens too.
    let mut all_protocol_defs = Vec::new();
    let mut dependency_protocol_sources =
        beamtalk_core::semantic_analysis::ProtocolSourceMap::new();
    let dependency_extensions_unknown = merge_dependency_class_infos(
        path,
        all_class_infos,
        &mut all_protocol_defs,
        &mut dependency_protocol_sources,
        &mut extension_index,
    );

    // Flatten cross-file trait provisions into the same-package infos (BT-3673),
    // via the helper `beamtalk lint` and `build` use.
    trait_users.flatten(
        all_class_infos,
        all_protocol_defs.iter().cloned(),
        current_package,
    );

    // Populate the FFI type registry the same way `beamtalk lint` does.
    let native_type_registry = build_native_type_registry(path);
    // BT-4022: project-known Erlang modules for the `unresolved_ffi` check.
    let known_erlang_modules = collect_known_erlang_modules(path);
    // BT-4377: the same `[diagnostics]` loader the compiler port (REPL) and
    // LSP use, so MCP `lint` applies the table `beamtalk lint`/`build` apply.
    let diagnostics_overrides = beamtalk_project::package::find_package_root(std::path::Path::new(
        path,
    ))
    .map_or_else(
        beamtalk_core::compilation::diagnostics_policy::DiagnosticsTable::new,
        |root| {
            beamtalk_core::compilation::diagnostics_policy::load_diagnostics_table_for_root(&root)
        },
    );
    let pre_loaded_protocol_defs =
        package_then_dependency_protocol_defs(trait_users, &all_protocol_defs);
    let protocol_sources =
        package_then_dependency_protocol_sources(trait_users, dependency_protocol_sources);

    LintAnalysisContext {
        extension_index,
        dependency_extensions_unknown,
        native_type_registry,
        known_erlang_modules,
        diagnostics_overrides,
        pre_loaded_protocol_defs,
        protocol_sources,
    }
}

/// Parsed file state collected during Pass 1 of the two-pass lint pipeline.
///
/// Fields are populated by [`run_lint_pass1`] and consumed by the Pass-2 loops
/// in [`run_lint_structured`] and [`compute_diagnostic_summary`].
struct LintPass1State {
    all_class_infos: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    trait_users: beamtalk_core::semantic_analysis::trait_expansion::TraitUserCollector,
    /// Every package source file's extension methods (ADR 0066), as
    /// `beamtalk lint`'s Pass 1 collects them.
    extension_index: beamtalk_core::compilation::extension_index::ExtensionIndex,
    /// Files from `target_set` that were read and parsed successfully, in encounter order.
    parsed_targets: Vec<(
        std::path::PathBuf,
        String,
        beamtalk_core::ast::Module,
        Vec<beamtalk_core::source_analysis::Diagnostic>,
    )>,
}

/// Run Pass 1 of the two-pass lint pipeline: parse every file in
/// `extraction_files`, extract cross-file class metadata, and collect trait
/// users. Files that belong to `target_set` are also accumulated in
/// [`LintPass1State::parsed_targets`] for the caller's Pass-2 loop.
///
/// `on_unreadable(file, is_target)` is called once for each file that could
/// not be read; `is_target` is `true` when that file is in `target_set`.
/// Callers use the callback to record the error in whatever form suits their
/// output format without duplicating the shared loop body.
fn run_lint_pass1(
    extraction_files: &[std::path::PathBuf],
    target_set: &std::collections::HashSet<std::path::PathBuf>,
    current_package: Option<&str>,
    mut on_unreadable: impl FnMut(&std::path::Path, bool),
) -> LintPass1State {
    use beamtalk_core::semantic_analysis::ClassHierarchy;

    let mut all_class_infos = Vec::new();
    let mut parsed_targets = Vec::new();
    let mut trait_users =
        beamtalk_core::semantic_analysis::trait_expansion::TraitUserCollector::default();
    let mut extension_index = beamtalk_core::compilation::extension_index::ExtensionIndex::new();

    for file in extraction_files {
        // Canonicalize once per file; used for both the error path and the
        // target-set membership check in the success path below.
        let canonical = beamtalk_project::package::try_canonicalize(file);
        let Ok(source) = std::fs::read_to_string(file) else {
            on_unreadable(file, target_set.contains(&canonical));
            continue;
        };
        let file_str = file.to_string_lossy().into_owned();
        let tokens = lex_with_eof(&source);
        let (module, parse_diags) = parse(tokens);
        let mut class_infos = ClassHierarchy::extract_class_infos(&module);
        // Stamp the package per-file, same as build's/lint's Pass 1 —
        // without this, a same-package class defined in a sibling file is
        // indistinguishable from a builtin/REPL class and never flagged as a
        // leak by `check_class_visibility`.
        if let Some(pkg) = current_package {
            ClassHierarchy::stamp_package_on_infos(&mut class_infos, pkg);
        }
        all_class_infos.extend(class_infos);
        // Records each carried protocol's source too, for provision routing.
        trait_users.add_source_file(&module, &file_str, &source, current_package);
        extension_index.add_module(&module, file);
        if target_set.contains(&canonical) {
            parsed_targets.push((file.clone(), source, module, parse_diags));
        }
    }

    LintPass1State {
        all_class_infos,
        trait_users,
        extension_index,
        parsed_targets,
    }
}

/// Run lint passes on `path` (file or directory) and return structured results.
///
/// Uses a two-pass pipeline mirroring CLI `beamtalk lint`:
/// - Pass 1: Parse all files in the package and extract class metadata
/// - Pass 2: Analyse each target file with cross-file class context
///
/// Without cross-file classes, the MCP lint produces different diagnostics
/// than the CLI — e.g. `@expect type` annotations are falsely reported as
/// stale because the type/DNU diagnostics they suppress require cross-file
/// class resolution to appear.
#[allow(clippy::too_many_lines)] // Two-pass pipeline is inherently sequential.
pub(crate) fn run_lint_structured(path: &str) -> LintResult {
    let source_files = match resolve_source_files(path) {
        Ok(files) => files,
        Err(result) => return result,
    };

    // Determine the full extraction set (package-wide src/ + test/)
    // so cross-file class references resolve correctly.
    let (extraction_files, target_set) = resolve_extraction_files(path, &source_files);

    // Resolve the current package once, mirroring `beamtalk lint`,
    // so E0401/E0402/E0403 visibility checks fire the same way in MCP.
    let current_package = resolve_current_package(path);

    // Resolve the package root once so each target file's `is_stub_file` can
    // be derived via `beamtalk_project::package::is_under_stubs_dir` without
    // re-walking ancestors per file.
    let project_root = beamtalk_project::package::find_package_root(std::path::Path::new(path));

    // Pass 1: Parse files and extract class metadata.
    let mut warnings = Vec::new();
    let mut errors = Vec::new();
    let mut gate = GateCounts::default();
    let mut pass1 = run_lint_pass1(
        &extraction_files,
        &target_set,
        current_package.as_deref(),
        |file, is_target| {
            let name = file.to_string_lossy().into_owned();
            if is_target {
                errors.push(LintDiagnostic {
                    file: name,
                    line: None,
                    message: format!("Failed to read '{}'", file.display()),
                    severity: "error",
                });
            } else {
                // Surface a warning when a package-extraction file
                // (chosen by the resolver but not a direct lint target) cannot
                // be read. Without this, cross-file class extraction silently
                // drops the file, potentially re-introducing diagnostic
                // divergence from CLI lint.
                warnings.push(LintDiagnostic {
                    file: name,
                    line: None,
                    message: format!(
                        "Failed to read package file '{}'; cross-file class extraction may be incomplete",
                        file.display()
                    ),
                    severity: "warning",
                });
            }
        },
    );
    let LintAnalysisContext {
        extension_index,
        dependency_extensions_unknown,
        native_type_registry,
        known_erlang_modules,
        diagnostics_overrides,
        pre_loaded_protocol_defs,
        protocol_sources,
    } = build_lint_analysis_context(
        path,
        &mut pass1.all_class_infos,
        &pass1.trait_users,
        std::mem::take(&mut pass1.extension_index),
        current_package.as_deref(),
    );
    let LintPass1State {
        all_class_infos,
        parsed_targets,
        ..
    } = pass1;

    let mut all_provision_diags = Vec::new();

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

        // run_module_analysis runs semantic analysis with cross-file class
        // context, the lint passes against that cross-file hierarchy
        // (mirroring CLI `beamtalk lint`), and applies @expect directives.
        let is_stub_file = project_root
            .as_deref()
            .is_some_and(|root| beamtalk_project::package::is_under_stubs_dir(root, &file));
        let (lint_diags, _) = run_module_analysis(
            &module,
            &source,
            &all_class_infos,
            pre_loaded_protocol_defs.clone(),
            protocol_sources.clone(),
            initial_diags,
            &extension_index,
            dependency_extensions_unknown,
            native_type_registry.clone(),
            current_package.as_deref(),
            is_stub_file,
            file.file_stem().and_then(std::ffi::OsStr::to_str),
            &known_erlang_modules,
            &diagnostics_overrides,
        );

        let (provision_diags, lint_diags) =
            split_provision_diagnostics(lint_diags, &pre_loaded_protocol_defs);
        all_provision_diags.extend(provision_diags);

        let file_name = file.to_string_lossy().into_owned();
        for diag in &lint_diags {
            gate.count(diag);
            let line = diag.span.line_number(&source);
            push_lint_diagnostic(
                diag,
                file_name.clone(),
                Some(line),
                &mut warnings,
                &mut errors,
            );
        }
    }

    // Publish each flattened-provision diagnostic once, against its
    // protocol's file (its spans are offsets into that file, not the using
    // class's) — `beamtalk lint`'s routing (ADR 0127 §3, BT-3665).
    for diag in dedupe_provision_diagnostics(all_provision_diags) {
        let Some((path, text)) = provision_report_target(&diag, &protocol_sources) else {
            continue;
        };
        gate.count(&diag);
        let line = (!text.is_empty()).then(|| diag.span.line_number(text));
        push_lint_diagnostic(&diag, path, line, &mut warnings, &mut errors);
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

/// The CLI gate counts [`LintResult`] carries (BT-4336), tallied per
/// reported diagnostic with the same rules CLI `beamtalk lint` applies.
#[derive(Default)]
struct GateCounts {
    lint: usize,
    live_recheck: usize,
}

impl GateCounts {
    fn count(&mut self, diag: &beamtalk_core::source_analysis::Diagnostic) {
        if diag.severity == Severity::Lint {
            self.lint += 1;
        }
        if is_live_recheck_failure(diag) {
            self.live_recheck += 1;
        }
    }
}
