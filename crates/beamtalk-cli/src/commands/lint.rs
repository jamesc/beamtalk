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

use crate::commands::OutputFormat;
use crate::commands::build::collect_source_files_from_dir;
use crate::commands::erlang_lint;
use crate::diagnostic::CompileDiagnostic;
use beamtalk_core::file_walker::FileWalker;
use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};
use beamtalk_project::package;
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
///
/// `source` is the module's raw source text — BT-3257, mirroring
/// `queries::diagnostic_provider::compute_project_diagnostics_with_analysis`
/// (BT-3240): needed so the near-miss `// === Name ===` divider check can
/// scan `source` directly (`beamtalk_core::near_miss_divider::check_near_miss_dividers`)
/// instead of relying on the AST's `Comment::span`, which is actually the
/// *following declaration's* span, not the comment's own.
///
/// `is_stub_file` gates `check_native_declaration_location` (BT-3404): a
/// legitimate `declare native:` block is only valid inside a `stubs/`
/// directory, so a genuine stub file must not be flagged. Callers derive
/// this the same way MCP's `run_module_analysis` does for BT-3398 —
/// [`beamtalk_project::package::is_under_stubs_dir`] against the file's own
/// path — since `collect_lint_files`'s `stubs/` exclusion only applies to
/// its *directory-walk* branch, not a direct single-file lint target.
///
/// `file_stem` (BT-3431) is the target file's basename without extension,
/// passed to `check_class_file_name_agreement` so `beamtalk lint` reports
/// the same file-name/class-name mismatch `beamtalk build`/the LSP do (via
/// `ProjectDiagnosticContext::source_file_stem`) — `None` for callers with
/// no real file backing the module skips the check.
#[allow(clippy::too_many_arguments)] // BT-2910 added pre_loaded_protocols/pre_loaded_aliases; each param is load-bearing context
fn collect_diagnostics(
    module: &beamtalk_core::ast::Module,
    source: &str,
    parse_diags: Vec<beamtalk_core::source_analysis::Diagnostic>,
    cross_file_classes: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    pre_loaded_protocols: Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo>,
    pre_loaded_aliases: Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo>,
    native_type_registry: Option<
        std::sync::Arc<beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>,
    >,
    knowledge_scope: beamtalk_core::semantic_analysis::KnowledgeScope,
    cross_file_extensions: &beamtalk_core::compilation::extension_index::ExtensionIndex,
    has_package_dependencies: bool,
    current_package: Option<&str>,
    is_stub_file: bool,
    file_stem: Option<&str>,
) -> Vec<beamtalk_core::source_analysis::Diagnostic> {
    // Collect parser-level lint diagnostics (e.g. unnecessary `.` — BT-948)
    // plus AST-level lint passes.
    let mut lint_diags: Vec<_> = parse_diags
        .into_iter()
        .filter(|d| d.severity == Severity::Lint)
        .collect();
    lint_diags.extend(beamtalk_lint::run_lint_passes(module));

    // BT-1547: Run semantic analysis to collect all categorised diagnostics
    // so that `@expect` directives can match them. Without this, `@expect type`
    // annotations that suppress real type/DNU diagnostics during build would be
    // reported as stale by lint. We include every diagnostic that has a category
    // (Type, Dnu, Unused, etc.) — this keeps lint in sync with `category_matches`
    // in diagnostic_provider.rs without manually mirroring its match arms.
    //
    // Pass cross-file class info so lint sees the same class hierarchy as build,
    // matching diagnostics for actor instantiation, type errors, etc.
    //
    // BT-2134: Pass the FFI type registry (loaded from the build cache) so lint
    // sees `(Erlang m) f:` calls as typed when build does. Without it, every
    // FFI call falls back to `Dynamic(UntypedFfi)` and lint emits a
    // "Dynamic in typed class" warning that build does not — leaving the user
    // with no `@expect` configuration that satisfies both passes.
    // BT-2920: Set current_package so E0401/E0402 visibility checks fire in
    // `beamtalk lint`, matching `beamtalk build` — both gate on
    // `current_package: Some(_)` (see `check_class_visibility`).
    let options = beamtalk_core::CompilerOptions {
        knowledge_scope,
        has_package_dependencies,
        current_package: current_package.map(str::to_string),
        ..Default::default()
    };
    let analysis_ctx = beamtalk_core::semantic_analysis::AnalysisContext::default()
        .with_options(&options)
        .with_pre_loaded_classes(cross_file_classes)
        .with_pre_loaded_protocols(pre_loaded_protocols)
        .with_pre_loaded_aliases(pre_loaded_aliases)
        .with_native_type_registry(native_type_registry)
        .with_cross_file_extensions(cross_file_extensions)
        .with_is_stub_file(is_stub_file);
    let analysis_result = beamtalk_core::semantic_analysis::analyse_full(module, analysis_ctx);
    lint_diags.extend(
        analysis_result
            .diagnostics
            .into_iter()
            .filter(|d| d.category.is_some()),
    );

    // BT-3431: Validate the file name agrees with the class it declares —
    // `analyse_full` doesn't run this check itself (see
    // `check_class_file_name_agreement`'s doc), so it must be called
    // explicitly here, mirroring `compute_project_diagnostics_with_analysis`.
    lint_diags.extend(
        beamtalk_core::semantic_analysis::module_validator::check_class_file_name_agreement(
            module, file_stem,
        ),
    );

    // BT-1476: Apply @expect directives to suppress matching lint diagnostics.
    // Note: apply_expect_directives may inject Severity::Warning for stale
    // @expect annotations, so we include those in the output.
    beamtalk_language_service::queries::diagnostic_provider::apply_expect_directives(
        module,
        &mut lint_diags,
    );

    // BT-3257: mirrors `compute_project_diagnostics_with_analysis`'s
    // placement — appended after `apply_expect_directives` because a
    // near-miss-divider comment's span (the comment's own line) can never
    // be contained in any `@expect`-annotated declaration's target span, so
    // running it through that pass first would be a no-op at best. See that
    // function's BT-3240 comment for the full reasoning.
    beamtalk_core::near_miss_divider::check_near_miss_dividers(source, &mut lint_diags);

    lint_diags
}

/// Decides whether a diagnostic from one file's [`collect_diagnostics`] call
/// should be kept, deduplicating repeat sightings of a pre-loaded alias
/// collision across the whole `run_lint` invocation (BT-3043).
///
/// `run_lint`'s Pass 2 loop calls `collect_diagnostics` once per project
/// file, each time seeding a *fresh* `AliasRegistry` from the same
/// project-wide `all_alias_infos` list. A genuine collision between two of
/// that list's entries is therefore rediscovered — with byte-identical
/// message and span, since neither depends on the file currently being
/// analysed — on every single call, once per file in the project instead of
/// once per lint run. `seen` tracks which (message, span) pairs have already
/// been kept so only the first sighting survives; every other diagnostic
/// (file-specific by construction) always passes through unfiltered.
///
/// The `"Pre-loaded type alias "` prefix is the stable, distinctive lead-in
/// shared by all three of `AliasRegistry::add_pre_loaded`'s diagnostic
/// messages (alias-vs-class, alias-vs-protocol, alias-vs-alias) and no other
/// diagnostic in the compiler — matching on it (rather than deduping every
/// diagnostic indiscriminately by (message, span)) avoids ever dropping a
/// genuinely distinct per-file diagnostic that happens to coincide with
/// another file's on both message text and byte offset.
fn dedup_pre_loaded_alias_collision(
    diag: &beamtalk_core::source_analysis::Diagnostic,
    seen: &mut std::collections::HashSet<(String, beamtalk_core::source_analysis::Span)>,
) -> bool {
    if diag.message.starts_with("Pre-loaded type alias ") {
        seen.insert((diag.message.to_string(), diag.span))
    } else {
        true
    }
}

/// Run lint passes on the given path (file or directory).
///
/// Prints each lint diagnostic and returns an error if any are found.
#[allow(clippy::too_many_lines)] // BT-2920 added current_package resolution; orchestration function
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

    // BT-2920 (review S1): parse beamtalk.toml exactly once, deriving both
    // `current_package` (E0401/E0402 gating, see `check_class_visibility`)
    // and `has_package_dependencies` from the same read. Previously these
    // were two independent re-parses with inconsistent error handling — a
    // malformed manifest silently disabled visibility checks (`.ok()`) while
    // only `has_package_dependencies` warned on the same parse failure.
    // `package_root` (when `Some`) is only ever produced by `find_package_root`
    // finding a `beamtalk.toml`, so the manifest is known to exist here; the
    // only outcome left to distinguish is "parses" vs "malformed".
    let (current_package, has_package_dependencies) = match package_root.as_deref() {
        Some(root) => match super::manifest::find_manifest_full(root) {
            Ok(Some(m)) => {
                let has_deps = !m.dependencies.is_empty();
                (Some(m.package.name), has_deps)
            }
            Ok(None) => (None, false),
            Err(e) => {
                warn!(
                    error = %e,
                    "Failed to parse beamtalk.toml for lint; E0401/E0402 visibility \
                     checks disabled and dependencies conservatively assumed present"
                );
                (None, true)
            }
        },
        None => (None, false),
    };

    // Resolved before extraction so cross-file `ClassInfo` can be
    // package-stamped too (see `parse_and_extract_class_infos`'s
    // `stamp_package_on_infos` call).
    let (
        mut all_class_infos,
        extension_index,
        mut all_protocol_infos,
        mut all_alias_infos,
        parsed_files,
    ) = parse_and_extract_class_infos(
        &source_files,
        package_root.as_deref(),
        current_package.as_deref(),
    )?;

    // Merge dependency class/protocol/alias metadata so lint sees the same
    // cross-package picture as build. Without this, @expect annotations that
    // suppress real cross-package diagnostics would be reported as stale.
    // The resolved deps are also needed below for their `stubs_dir`s (ADR
    // 0075 layer 2).
    let resolved_deps = if let Some(ref project_root) = package_root {
        merge_dependency_infos(
            project_root,
            &mut all_class_infos,
            &mut all_protocol_infos,
            &mut all_alias_infos,
        )
    } else {
        Vec::new()
    };

    // BT-2134 / BT-2851: Populate the FFI type registry via the same
    // `extract_type_specs` that `beamtalk build` calls, instead of only
    // reading whatever `_build/type_cache/` happens to hold. Reading a cache
    // written by a *previous* build let lint's view of FFI types drift from
    // build's live view — on a project that had never been built, lint's
    // cache read silently returned `None` (skipping FFI arg-type checks
    // build performs), and any `@expect type` written to suppress a real
    // build-time FFI diagnostic was then flagged as stale by lint. Calling
    // the shared extractor directly makes `beamtalk lint` and `beamtalk
    // build`/`test` agree on the FFI type registry by construction: a fresh
    // cache still short-circuits to zero `.beam` reads, and a cold/stale one
    // extracts once and writes the same cache a subsequent build would.
    // BT-1847 / BT-3394: the full ADR 0075 stub resolution chain overrides
    // auto-extract at the function/arity level — same merge `beamtalk build`
    // performs, via the same shared loaders, so `beamtalk lint` and
    // `beamtalk build` agree on stub-derived FFI types too, not just
    // auto-extracted ones. These loaders print their own diagnostics
    // (skipped signatures, version drift) directly; they are not folded into
    // `all_diags` below, which is scoped to `.bt` diagnostics.
    let native_type_registry = package_root.as_deref().and_then(|root| {
        let layout = crate::commands::build_layout::BuildLayout::new(root);
        let auto_extract = super::build::extract_type_specs(&layout, true, false);

        let dependency_stubs = super::build::load_dependency_stub_registries(
            &resolved_deps,
            super::build::distribution_stubs_dir().as_deref(),
            auto_extract.as_ref(),
            format,
        );
        let project_stubs =
            super::build::load_project_stub_registry(root, auto_extract.as_ref(), format);

        if dependency_stubs.is_none() && project_stubs.is_none() {
            auto_extract
        } else {
            let mut merged = auto_extract.unwrap_or_default();
            if let Some((dep_registry, _dep_diags)) = dependency_stubs {
                merged.apply_overrides(dep_registry);
            }
            if let Some((stub_registry, _stub_diags)) = project_stubs {
                merged.apply_overrides(stub_registry);
            }
            Some(merged)
        }
        .map(std::sync::Arc::new)
    });

    // Pass 2: Analyse each file with cross-file class context.
    let mut total_lint_count = 0usize;
    let mut all_diags: Vec<beamtalk_core::source_analysis::Diagnostic> = Vec::new();

    // BT-3043: `all_alias_infos` (the pre-loaded/cross-package alias seed
    // list) is identical on every iteration of the Pass 2 loop below, so a
    // genuine collision between two of its entries — reported fresh by
    // `AliasRegistry::add_pre_loaded` inside `collect_diagnostics` (a new
    // `AliasRegistry` per file, seeded from the same list every time) — would
    // otherwise be re-diagnosed once per file in the project instead of once
    // per lint run. Dedup by (message, span): every `add_pre_loaded`
    // collision diagnostic for a given colliding pair has byte-identical
    // text and span regardless of which file triggered the seeding, since
    // neither depends on the file currently being analysed.
    let mut seen_pre_loaded_alias_collisions: std::collections::HashSet<(
        String,
        beamtalk_core::source_analysis::Span,
    )> = std::collections::HashSet::new();

    // BT-2796: With a package root, Pass 1 walked the full package source set
    // (BT-2027), so the injected knowledge is project-complete. Without one
    // (a bare file outside any package), only the targeted files were parsed
    // — keep the conservative `ModuleOnly` default.
    let knowledge_scope = if package_root.is_some() {
        beamtalk_core::semantic_analysis::KnowledgeScope::ProjectComplete
    } else {
        beamtalk_core::semantic_analysis::KnowledgeScope::ModuleOnly
    };

    for (file, source, module, parse_diags) in parsed_files {
        let cross_file_classes =
            beamtalk_core::semantic_analysis::ClassHierarchy::cross_file_class_infos(
                &all_class_infos,
                &module,
            );

        // BT-3404: `collect_lint_files`'s `stubs/` exclusion only covers its
        // directory-walk branch — a direct single-file lint target (e.g.
        // `beamtalk lint stubs/lists.bt`) bypasses it entirely, so a
        // legitimate stub file must be recognised here or
        // `check_native_declaration_location` reports a false
        // "only valid in stubs/ directory" error on it.
        let is_stub_file = package_root.as_deref().is_some_and(|root| {
            package::is_under_stubs_dir(root.as_std_path(), file.as_std_path())
        });

        let lint_diags = collect_diagnostics(
            &module,
            &source,
            parse_diags,
            cross_file_classes,
            all_protocol_infos.clone(),
            all_alias_infos.clone(),
            native_type_registry.clone(),
            knowledge_scope,
            &extension_index,
            has_package_dependencies,
            current_package.as_deref(),
            is_stub_file,
            file.file_stem(),
        );

        // BT-3043: Drop repeat sightings of a pre-loaded alias collision —
        // see `seen_pre_loaded_alias_collisions`'s doc above. Every other
        // diagnostic is file-specific and passes through unfiltered.
        let lint_diags: Vec<_> = lint_diags
            .into_iter()
            .filter(|d| dedup_pre_loaded_alias_collision(d, &mut seen_pre_loaded_alias_collisions))
            .collect();

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
                    let json = crate::diagnostic::diagnostic_to_json(file.as_str(), diag);
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
        // `stubs/` is excluded (ADR 0075, BT-1847): it's type-only and
        // never compiled, but this walk has no src/-only scoping to
        // exclude it implicitly the way `find_source_files` does, so a
        // `declare native:` stub file would otherwise be swept in and
        // rejected here as a hard error.
        let stubs_dir = project_root.join("stubs");
        collect_source_files_from_dir(source_path)?
            .into_iter()
            .filter(|f| !f.starts_with(&stubs_dir))
            .collect()
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
/// [`beamtalk_project::package::collect_package_source_files_with_errors`]
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

#[allow(clippy::type_complexity)] // Mirrors the pre-existing 3-tuple return; BT-2910 adds two sibling collections
fn parse_and_extract_class_infos(
    source_files: &[Utf8PathBuf],
    package_root: Option<&Utf8Path>,
    current_package: Option<&str>,
) -> Result<(
    Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    beamtalk_core::compilation::extension_index::ExtensionIndex,
    Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo>,
    Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo>,
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
    let mut extension_index = beamtalk_core::compilation::extension_index::ExtensionIndex::new();
    // BT-2910: Same-package cross-file protocol/alias metadata — `beamtalk
    // lint` previously collected neither (protocols not at all; aliases only
    // via BT-2928 in `build`), so `:: Alias` and `extending:` diagnostics
    // could disagree between `build` and `lint`.
    let mut all_protocol_infos = Vec::new();
    let mut all_alias_infos = Vec::new();
    let mut parsed_files: Vec<ParsedLintFile> = Vec::new();

    for file in &extraction_files {
        let source = std::fs::read_to_string(file)
            .into_diagnostic()
            .map_err(|e| miette::miette!("Failed to read '{}': {e}", file))?;

        let tokens = lex_with_eof(&source);
        let (module, parse_diags) = parse(tokens);

        // BT-2796: A file with parse errors may have an under-recovered
        // method surface — mark its classes so the receiver-knowledge
        // classifier treats them (and their subclasses) as `Open` rather
        // than emitting hints against a surface extraction never fully saw.
        let has_parse_errors = parse_diags.iter().any(|d| d.severity == Severity::Error);
        let mut class_infos =
            beamtalk_core::semantic_analysis::ClassHierarchy::extract_class_infos(&module);
        if has_parse_errors {
            for info in &mut class_infos {
                info.surface_incomplete = true;
            }
        }
        // BT-2920: Stamp the package per-file, same as build's Pass 1 — see
        // `stamp_package_on_infos`'s doc comment for why this can't wait
        // until each file's own analysis pass.
        if let Some(pkg) = current_package {
            beamtalk_core::semantic_analysis::ClassHierarchy::stamp_package_on_infos(
                &mut class_infos,
                pkg,
            );
        }
        all_class_infos.extend(class_infos);

        // BT-2795: Collect standalone extensions package-wide so cross-file
        // `ClassName >> selector` definitions resolve during lint the same
        // way they do during build.
        extension_index.add_module(&module, file.as_std_path());

        // BT-2910: Extract protocol/alias infos from the already-parsed
        // module — no second parse pass, mirroring the class_infos handling
        // just above. Aliases are stamped with `current_package` the same
        // way `build`'s `collect_project_alias_infos` stamps them, so
        // `AliasRegistry::add_pre_loaded`'s seeding-boundary filter can tell
        // a same-package alias apart from a dependency's `internal` one.
        all_protocol_infos.extend(
            beamtalk_core::semantic_analysis::protocol_registry::ProtocolRegistry::extract_protocol_infos(
                &module,
            ),
        );
        let mut alias_infos =
            beamtalk_core::semantic_analysis::alias_registry::AliasRegistry::extract_alias_infos(
                &module,
            );
        if let Some(pkg) = current_package {
            for info in &mut alias_infos {
                info.package = Some(pkg.into());
            }
        }
        all_alias_infos.extend(alias_infos);

        if source_file_set.contains(&canonicalize_or_clone(file)) {
            parsed_files.push((file.clone(), source, module, parse_diags));
        }
    }

    Ok((
        all_class_infos,
        extension_index,
        all_protocol_infos,
        all_alias_infos,
        parsed_files,
    ))
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
/// [`beamtalk_project::package::find_package_root`] so MCP and CLI share
/// the same implementation.
pub(crate) fn find_package_root(start: &Utf8Path) -> Option<Utf8PathBuf> {
    package::find_package_root(start.as_std_path()).and_then(|p| Utf8PathBuf::from_path_buf(p).ok())
}

/// Resolve dependency classes/protocols/aliases and merge them into the
/// respective info lists.
///
/// Best-effort: if dependency resolution fails (e.g. network error for a git
/// dep), lint continues without dep metadata rather than failing entirely.
///
/// BT-2920 (review S1): `has_package_dependencies` (BT-2794) used to be
/// computed here from its own re-parse of `beamtalk.toml`, independently of
/// `run_lint`'s `current_package` resolution — two reads of the same file
/// with inconsistent error handling. `run_lint` now parses the manifest once
/// and derives both from that single read; this function's only remaining
/// job is the dependency-metadata side effect on the three `all_*` lists.
///
/// BT-2910: Renamed from `merge_dependency_class_infos` — now also merges
/// `dep.protocol_infos`/`dep.alias_infos`, giving `beamtalk lint` the same
/// cross-package protocol/alias resolution `beamtalk build` has.
/// Resolves the project's dependencies, merges their class/protocol/alias
/// metadata into the running lists, and returns the resolved dependencies
/// (empty on resolution failure) so callers can also use their
/// `stubs_dir`s (ADR 0075 layer 2) — see [`run_lint`]'s
/// `native_type_registry` construction.
fn merge_dependency_infos(
    project_root: &Utf8Path,
    all_class_infos: &mut Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    all_protocol_infos: &mut Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo>,
    all_alias_infos: &mut Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo>,
) -> Vec<super::deps::path::ResolvedDependency> {
    let options = beamtalk_core::CompilerOptions::default();
    match super::deps::ensure_deps_resolved(project_root, &options) {
        Ok(resolved_deps) => {
            for dep in &resolved_deps {
                all_class_infos.extend(dep.class_infos.clone());
                all_protocol_infos.extend(dep.protocol_infos.clone());
                all_alias_infos.extend(dep.alias_infos.clone());
            }
            resolved_deps
        }
        Err(e) => {
            warn!(
                error = %e,
                "Failed to resolve dependencies for lint; \
                 dependency classes/protocols/aliases may not be available"
            );
            Vec::new()
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
    collect_lint_diagnostics_with_stub_flag(source, false)
}

/// As [`collect_lint_diagnostics`], but with `is_stub_file` set explicitly —
/// BT-3404's regression test for a direct-file-target `beamtalk lint` on a
/// genuine `stubs/` file needs `is_stub_file: true` to reproduce what
/// `collect_diagnostics`'s real caller now derives via `is_under_stubs_dir`.
#[cfg(test)]
fn collect_lint_diagnostics_with_stub_flag(
    source: &str,
    is_stub_file: bool,
) -> Vec<beamtalk_core::source_analysis::Diagnostic> {
    let tokens = lex_with_eof(source);
    let (module, parse_diags) = parse(tokens);
    collect_diagnostics(
        &module,
        source,
        parse_diags,
        vec![],
        vec![],
        vec![],
        None,
        beamtalk_core::semantic_analysis::KnowledgeScope::default(),
        &beamtalk_core::compilation::extension_index::ExtensionIndex::new(),
        false,
        None,
        is_stub_file,
        None,
    )
}

/// As [`collect_lint_diagnostics`], but with `file_stem` set explicitly —
/// BT-3431's regression test for `beamtalk lint` reporting the same
/// file-name/class-name mismatch `beamtalk build`/the LSP do.
#[cfg(test)]
fn collect_lint_diagnostics_with_file_stem(
    source: &str,
    file_stem: Option<&str>,
) -> Vec<beamtalk_core::source_analysis::Diagnostic> {
    let tokens = lex_with_eof(source);
    let (module, parse_diags) = parse(tokens);
    collect_diagnostics(
        &module,
        source,
        parse_diags,
        vec![],
        vec![],
        vec![],
        None,
        beamtalk_core::semantic_analysis::KnowledgeScope::default(),
        &beamtalk_core::compilation::extension_index::ExtensionIndex::new(),
        false,
        None,
        false,
        file_stem,
    )
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
    fn declare_native_outside_stubs_is_reported() {
        // BT-3404: a `declare native:` block outside `stubs/` (is_stub_file:
        // false, what `collect_diagnostics`'s real caller derives for a
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
        // BT-3404 review fix: giving `check_native_declaration_location`'s
        // diagnostic a category (so it survives `collect_diagnostics`'s
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
    fn mismatched_file_name_is_reported() {
        // BT-3431: `beamtalk lint` must report the same file-name/class-name
        // mismatch `beamtalk build`/the LSP do, closing the surface-parity
        // gap `check_class_file_name_agreement` would otherwise have only on
        // this surface (the `.category.is_some()` filter alone wouldn't have
        // dropped it — `FileClassNameMismatch` is a real category — but
        // nothing called the check here at all before this fix).
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
        // BT-3431 negative control.
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

    // ── near-miss `// === Name ===` divider (BT-3240/BT-3257) ──────────────
    //
    // These exercise `collect_diagnostics` through its real entry point
    // (`collect_lint_diagnostics`, mirroring `run_lint`'s Pass 2 call), not
    // `near_miss_divider::scan_source` directly (already covered by that
    // module's own `scan_source_locates_the_near_miss_comment_line_precisely`
    // test) — proving the CLI wiring itself, not just the underlying scan.

    #[test]
    fn near_miss_divider_span_points_at_the_comment_line_not_the_declaration() {
        // BT-3240/BT-3257: before `source` was threaded through
        // `collect_diagnostics`, this diagnostic's span came from the
        // AST's `Comment::span`, which is actually `bar`'s token span (the
        // *following* declaration), not the comment's own line. Slicing
        // `source` by the reported span proves it now covers exactly the
        // comment line.
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
        let diags = collect_diagnostics(
            &module,
            test_source,
            parse_diags,
            cross_file_classes,
            vec![],
            vec![],
            None,
            beamtalk_core::semantic_analysis::KnowledgeScope::default(),
            &beamtalk_core::compilation::extension_index::ExtensionIndex::new(),
            false,
            None,
            false,
            None,
        );
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
        let diags = collect_diagnostics(
            &module,
            &test_source,
            parse_diags,
            cross_file_classes,
            vec![],
            vec![],
            None,
            beamtalk_core::semantic_analysis::KnowledgeScope::default(),
            &beamtalk_core::compilation::extension_index::ExtensionIndex::new(),
            false,
            None,
            false,
            None,
        );

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

    /// BT-2910 (acceptance criterion): `beamtalk lint` must resolve a
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
        let root = temp.path();

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
        let (
            mut all_class_infos,
            extension_index,
            mut all_protocol_infos,
            mut all_alias_infos,
            parsed_files,
        ) = parse_and_extract_class_infos(&source_files, Some(&consumer_root), Some("consumer"))
            .unwrap();
        merge_dependency_infos(
            &consumer_root,
            &mut all_class_infos,
            &mut all_protocol_infos,
            &mut all_alias_infos,
        );

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
        // when `collect_diagnostics` calls `analyse_full` below. That
        // exclusion logic itself is already covered by
        // `add_pre_loaded_never_seeds_internal_alias_from_different_package`
        // in `alias_registry.rs` (BT-2898); what this test proves is that the
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

        // Directly exercise the seeding step `collect_diagnostics` performs
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

        let (file, source, module, parse_diags) = parsed_files
            .into_iter()
            .find(|(f, ..)| *f == consumer_file)
            .expect("consumer.bt should be among the parsed files");
        assert_eq!(file, consumer_file);

        let diags = collect_diagnostics(
            &module,
            &source,
            parse_diags,
            vec![], // no same-package cross-file classes
            all_protocol_infos,
            all_alias_infos,
            None,
            beamtalk_core::semantic_analysis::KnowledgeScope::ProjectComplete,
            &extension_index,
            true, // has_package_dependencies
            Some("consumer"),
            false,
            None,
        );

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

    /// BT-3043: `collect_lint_files`'s directory walk must never treat a
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

    /// BT-3043 (acceptance criterion): a project depending on a package that
    /// declares a type alias exactly once must lint clean across every file
    /// in the project — no false "Pre-loaded type alias ... collides with
    /// another type alias of the same name" diagnostic, no matter how many
    /// project files are analysed against the same merged
    /// `all_alias_infos` seed list.
    ///
    /// Mirrors `lint_resolves_dependency_protocol_and_public_alias_but_not_internal_alias`'s
    /// fixture shape (a real producer/consumer path-dependency pair with a
    /// faked-fresh dependency stamp), but drives `collect_diagnostics` across
    /// *several* consumer files the way `run_lint`'s Pass 2 loop does, since
    /// the bug this guards against only manifests when the same pre-loaded
    /// alias list is re-seeded into a fresh `AliasRegistry` once per file.
    #[test]
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

        let (
            mut all_class_infos,
            extension_index,
            mut all_protocol_infos,
            mut all_alias_infos,
            parsed_files,
        ) = parse_and_extract_class_infos(&source_files, Some(&consumer_root), Some("consumer"))
            .unwrap();
        merge_dependency_infos(
            &consumer_root,
            &mut all_class_infos,
            &mut all_protocol_infos,
            &mut all_alias_infos,
        );
        assert_eq!(
            all_alias_infos
                .iter()
                .filter(|a| a.name == "Handler")
                .count(),
            1,
            "the fixture declares Handler exactly once: {all_alias_infos:?}"
        );

        let mut seen_pre_loaded_alias_collisions = std::collections::HashSet::new();
        for (_file, source, module, parse_diags) in parsed_files {
            let cross_file_classes =
                beamtalk_core::semantic_analysis::ClassHierarchy::cross_file_class_infos(
                    &all_class_infos,
                    &module,
                );
            let diags = collect_diagnostics(
                &module,
                &source,
                parse_diags,
                cross_file_classes,
                all_protocol_infos.clone(),
                all_alias_infos.clone(),
                None,
                beamtalk_core::semantic_analysis::KnowledgeScope::ProjectComplete,
                &extension_index,
                true,
                Some("consumer"),
                false,
                None,
            )
            .into_iter()
            .filter(|d| dedup_pre_loaded_alias_collision(d, &mut seen_pre_loaded_alias_collisions))
            .collect::<Vec<_>>();

            assert!(
                diags
                    .iter()
                    .all(|d| !d.message.contains("collides with another type alias")),
                "a once-declared dependency alias must never be flagged as colliding: {diags:?}"
            );
        }
    }

    /// BT-3043 (acceptance criterion): a *genuine* cross-package alias
    /// collision — two different dependencies each exporting a same-named
    /// alias with a different RHS — must be reported exactly once across a
    /// multi-file consumer project, not once per file. Exercises the real
    /// pipeline end to end (`parse_and_extract_class_infos` +
    /// `merge_dependency_infos` + the same Pass-2 loop shape `run_lint` uses,
    /// dedup included) rather than synthetic diagnostics, so it would have
    /// caught a regression in either the `AliasRegistry::add_pre_loaded`
    /// identity check or `run_lint`'s cross-file dedup.
    #[test]
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

        let (
            mut all_class_infos,
            extension_index,
            mut all_protocol_infos,
            mut all_alias_infos,
            parsed_files,
        ) = parse_and_extract_class_infos(&source_files, Some(&consumer_root), Some("consumer"))
            .unwrap();
        merge_dependency_infos(
            &consumer_root,
            &mut all_class_infos,
            &mut all_protocol_infos,
            &mut all_alias_infos,
        );
        assert_eq!(
            all_alias_infos
                .iter()
                .filter(|a| a.name == "Shared")
                .count(),
            2,
            "the fixture declares Shared twice, with different RHS: {all_alias_infos:?}"
        );

        let mut seen_pre_loaded_alias_collisions = std::collections::HashSet::new();
        let mut collision_count = 0usize;
        for (_file, source, module, parse_diags) in parsed_files {
            let cross_file_classes =
                beamtalk_core::semantic_analysis::ClassHierarchy::cross_file_class_infos(
                    &all_class_infos,
                    &module,
                );
            let diags = collect_diagnostics(
                &module,
                &source,
                parse_diags,
                cross_file_classes,
                all_protocol_infos.clone(),
                all_alias_infos.clone(),
                None,
                beamtalk_core::semantic_analysis::KnowledgeScope::ProjectComplete,
                &extension_index,
                true,
                Some("consumer"),
                false,
                None,
            )
            .into_iter()
            .filter(|d| dedup_pre_loaded_alias_collision(d, &mut seen_pre_loaded_alias_collisions))
            .collect::<Vec<_>>();

            collision_count += diags
                .iter()
                .filter(|d| d.message.contains("collides with another type alias"))
                .count();
        }

        assert_eq!(
            collision_count, 1,
            "a genuine cross-package collision must be reported exactly once for the whole \
             lint run, not once per file"
        );
    }

    /// BT-3043 (acceptance criterion): when two pre-loaded aliases *do*
    /// genuinely collide, `run_lint`'s per-file loop must report that
    /// collision once for the whole run, not once per file — `add_pre_loaded`
    /// rediscovers the same collision on every file (a fresh `AliasRegistry`
    /// seeded from the same list each time), so without `run_lint`'s own
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

    /// BT-2134: With no FFI registry, an `(Erlang m) f:` call in a typed class
    /// infers as `Dynamic(UntypedFfi)` and lint emits the BT-1914
    /// "Dynamic in typed class (untyped FFI)" warning.
    ///
    /// This is the pre-fix lint behaviour, captured to make the next test's
    /// improvement clear: with the registry loaded, no warning fires.
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
        let diags = collect_diagnostics(
            &module,
            source,
            parse_diags,
            vec![],
            vec![],
            vec![],
            None,
            beamtalk_core::semantic_analysis::KnowledgeScope::default(),
            &beamtalk_core::compilation::extension_index::ExtensionIndex::new(),
            false,
            None,
            false,
            None,
        );

        let has_untyped_ffi = diags.iter().any(|d| d.message.contains("untyped FFI"));
        assert!(
            has_untyped_ffi,
            "without registry, lint should warn untyped FFI; got: {diags:?}"
        );
    }

    /// BT-2134: With the FFI registry loaded (build cache present), an
    /// `(Erlang m) f:` call resolves to a typed return — `Result(...)` for
    /// `gen_tcp:connect/2`. The receiver is no longer `Dynamic` at the top
    /// level, so the BT-1914 "Dynamic in typed class (untyped FFI)" warning
    /// must NOT fire. This is the build behaviour; without this fix lint
    /// disagreed.
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
        let diags = collect_diagnostics(
            &module,
            source,
            parse_diags,
            vec![],
            vec![],
            vec![],
            Some(std::sync::Arc::new(registry)),
            beamtalk_core::semantic_analysis::KnowledgeScope::default(),
            &beamtalk_core::compilation::extension_index::ExtensionIndex::new(),
            false,
            None,
            false,
            None,
        );

        let untyped_ffi: Vec<_> = diags
            .iter()
            .filter(|d| d.message.contains("untyped FFI"))
            .collect();
        assert!(
            untyped_ffi.is_empty(),
            "with registry, lint must not warn untyped FFI; got: {untyped_ffi:?}"
        );
    }

    /// BT-2851: `run_lint` now populates its native-type registry via
    /// `super::build::extract_type_specs` — the exact function `beamtalk
    /// build` calls — instead of only reading `_build/type_cache/`. On a
    /// project that has never been built (no cache directory at all), the
    /// old cache-only read silently returned `None`, so lint skipped FFI
    /// arg-type checks that build performs; an `@expect type` written for
    /// the resulting build-time diagnostic was then reported as stale by
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
        let diags = collect_diagnostics(
            &module,
            source,
            parse_diags,
            vec![],
            vec![],
            vec![],
            Some(std::sync::Arc::new(registry)),
            beamtalk_core::semantic_analysis::KnowledgeScope::default(),
            &beamtalk_core::compilation::extension_index::ExtensionIndex::new(),
            false,
            None,
            false,
            None,
        );
        let stale = diags.iter().any(|d| d.message.contains("stale @expect"));
        assert!(
            !stale,
            "@expect type suppressing a genuine cold-cache FFI arg-type \
             mismatch must not be reported as stale by lint, got: {diags:?}"
        );
    }

    /// BT-2134: `load_type_cache_registry` reads `<module>_<16-hex>.json`
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

    /// BT-2134: An empty or missing `_build/type_cache/` directory must yield
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

    /// BT-2134 (`CodeRabbit` follow-up): When the cache has accumulated
    /// multiple `<module>_<hash>.json` entries for the same module — e.g.
    /// after a dependency upgrade or BEAM path change moved the spec to a
    /// new hash — the loader must replay only the most-recently-modified
    /// entry. Replaying both could let `read_dir` order pick the stale
    /// signature, reintroducing the lint/build disagreement BT-2134 fixed.
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

    /// BT-2139: When a cache entry records a `beam_path` and the live `.beam`
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

    /// BT-2139: When a cache entry's `beam_path` no longer exists — the BEAM
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

    /// BT-2139: Legacy entries written before BT-2139 do not carry a
    /// `beam_path`. Those must continue to load — pessimistically treated as
    /// fresh — so a `lint` immediately after upgrading does not blank out
    /// every FFI signature until the user re-runs `build`.
    ///
    /// This entry does carry a current `mapping_stamp` (BT-2852) so the test
    /// isolates the `beam_path` leniency behaviour; see
    /// `load_type_cache_registry_skips_entry_when_mapping_stamp_missing` for
    /// the case where the stamp itself is absent.
    #[test]
    fn load_type_cache_registry_loads_legacy_entry_without_beam_path() {
        use crate::beam_compiler::load_type_cache_registry;

        let temp = tempfile::TempDir::new().unwrap();
        let cache_dir = camino::Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        std::fs::create_dir_all(cache_dir.as_std_path()).unwrap();

        // No `beam_path` field at all — what BT-2134 wrote.
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

    /// BT-2852: An entry written by a `beamtalk` build *before* this feature
    /// shipped has no `mapping_stamp` field at all (the default, empty
    /// string). It must be treated as a graceful cache miss — not a crash —
    /// even though its `.beam` mtime and (absent) `beam_path` would otherwise
    /// be accepted as fresh.
    #[test]
    fn load_type_cache_registry_skips_entry_when_mapping_stamp_missing() {
        use crate::beam_compiler::load_type_cache_registry;

        let temp = tempfile::TempDir::new().unwrap();
        let cache_dir = camino::Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        std::fs::create_dir_all(cache_dir.as_std_path()).unwrap();

        // Pre-BT-2852 shape: no `mapping_stamp` field at all.
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

    /// BT-2852: An entry stamped by a *different* compiler build (a stale
    /// `mapping_stamp`) must be skipped even though its `.beam` mtime still
    /// matches — this is the regression scenario the issue describes: a warm
    /// cache surviving a change to `beamtalk_spec_reader.erl`'s type-mapping
    /// logic must not keep serving the old mapping forever.
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

    /// BT-2139: When a cache entry records a `beam_path` *and* the live file
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

    /// BT-2134 (`CodeRabbit` follow-up): Module names that themselves contain
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
}
