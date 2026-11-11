// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0075: native FFI type-spec extraction and the binary's rendering of
//! the stub-registry resolution chain (extracted -> distribution ->
//! package-bundled -> project-local). The chain itself lives in the lib
//! target ([`beamtalk_cli::ffi_type_registry`]) so MCP `lint` builds the
//! same registry (BT-4643); this module adapts resolved dependencies to it
//! and renders its stub diagnostics in the caller's output format.

use camino::Utf8Path;

use crate::commands::OutputFormat;
use crate::commands::build_layout::BuildLayout;

/// ADR 0075 Phase 1: Extract type specs from OTP and dependency `.beam` files
/// and cache them.
///
/// Non-fatal: if spec extraction fails (e.g., runtime not compiled), the build
/// succeeds without type information. The LSP will still provide untyped
/// completions in that case.
///
/// Runs in package mode (`has_manifest`) and in `--stdlib-mode` (used by
/// `dialyzer-specs` and the build-stdlib pipeline, which compile a directory
/// of stdlib `.bt` files without a manifest). Single-file builds without
/// either signal return `None`.
///
/// This is the single source of truth for populating a
/// [`NativeTypeRegistry`] from OTP/dependency `.beam` files. `beamtalk build`
/// (via [`super::execute_build_passes`]) and `beamtalk lint` (via
/// [`super::super::lint::run_lint`]) both call this function directly —
/// rather than lint reading a possibly-absent/stale on-disk cache written by
/// a *previous* build — so the two surfaces can never see different FFI type
/// diagnostics for the same code. The tiered cache in `cache_dir` still makes
/// repeat calls (from either surface) cheap: a fresh cache short-circuits to
/// zero `.beam` reads; a cold/stale one pays the extraction cost once and
/// writes the cache for the next caller, whichever surface that is.
pub(crate) fn extract_type_specs(
    layout: &BuildLayout,
    has_manifest: bool,
    stdlib_mode: bool,
) -> Option<beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry> {
    // No manifest: only continue when compiling the stdlib (which has its own
    // FFI surface in runtime/stdlib/workspace ebins). Otherwise nothing to do.
    if !has_manifest {
        return if stdlib_mode {
            crate::commands::build_stdlib::extract_stdlib_type_specs()
        } else {
            None
        };
    }

    // The manifest-backed extraction path lives in the
    // `beamtalk_cli` lib crate (`native_type_specs`) so `beamtalk-mcp` can
    // call the same single source of truth without reading a possibly-stale
    // on-disk cache. This is a thin delegation, not a duplicate.
    beamtalk_cli::native_type_specs::extract_project_type_specs(layout)
}

pub(crate) use beamtalk_cli::ffi_type_registry::ResolvedTypeRegistry;
use beamtalk_cli::ffi_type_registry::StubReport;
#[cfg(test)]
pub(crate) use beamtalk_cli::ffi_type_registry::{
    DependencyStubsDir, StubLayer, distribution_stubs_dir, load_dependency_stub_registries,
    load_project_stub_registry,
};

/// [`beamtalk_cli::ffi_type_registry::resolve_package_type_registry`] for a
/// package whose dependencies `resolved_deps` were resolved by this binary,
/// rendering the stub tiers' diagnostics as it goes.
///
/// The one merge `beamtalk build`, `beamtalk lint`, `beamtalk test` and a
/// dependency's compile for a downstream package (BT-4225) all share.
/// `echo` controls how each stub diagnostic is rendered — `beamtalk build`
/// always passes [`OutputFormat::Text`] (it has no JSON mode); `beamtalk
/// lint` passes its own `--format`, so a stub diagnostic streams as a JSON
/// line under `lint --format=json` the same way every other lint diagnostic
/// does. `None` renders nothing: a dependency compiled for a downstream
/// package keeps its diagnostics to itself (BT-4225).
pub(crate) fn resolve_package_type_registry(
    package_root: &Utf8Path,
    auto_extract: Option<beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>,
    resolved_deps: &[crate::commands::deps::path::ResolvedDependency],
    echo: Option<OutputFormat>,
) -> ResolvedTypeRegistry {
    let resolved = beamtalk_cli::ffi_type_registry::resolve_package_type_registry(
        package_root,
        auto_extract,
        &dependency_stub_dirs(resolved_deps),
    );
    render_stub_reports(&resolved.reports, echo);
    resolved
}

/// Each of `resolved_deps`' own package-bundled stub directories (ADR 0075
/// layer 2), in their resolution order.
pub(crate) fn dependency_stub_dirs(
    resolved_deps: &[crate::commands::deps::path::ResolvedDependency],
) -> Vec<beamtalk_cli::ffi_type_registry::DependencyStubsDir> {
    resolved_deps
        .iter()
        .filter_map(|dep| {
            Some(beamtalk_cli::ffi_type_registry::DependencyStubsDir {
                package: dep.name.clone(),
                stubs_dir: dep.stubs_dir.clone()?,
            })
        })
        .collect()
}

/// Render the stub tiers' diagnostics in `echo`'s format (nothing for
/// `None`): each stub file's against its own source, as
/// `compile_source_with_bindings` renders a `src/` file's — without it, a
/// drift or skipped-signature warning would only ever show up as an
/// anonymous count in the build summary. A sibling-package collision has no
/// single source to render against (its two declarations live in two
/// packages' stub files), so it prints as a plain message.
pub(crate) fn render_stub_reports(reports: &[StubReport], echo: Option<OutputFormat>) {
    let Some(format) = echo else {
        return;
    };
    for report in reports {
        match report {
            StubReport::File(file) => {
                for diagnostic in &file.diagnostics {
                    if diagnostic.severity == beamtalk_core::source_analysis::Severity::Lint {
                        continue;
                    }
                    match format {
                        OutputFormat::Text => {
                            let compile_diag =
                                crate::diagnostic::CompileDiagnostic::from_core_diagnostic(
                                    diagnostic,
                                    file.path.as_str(),
                                    &file.source,
                                );
                            eprintln!("{:?}", miette::Report::new(compile_diag));
                        }
                        OutputFormat::Json => println!(
                            "{}",
                            crate::diagnostic::diagnostic_to_json(file.path.as_str(), diagnostic)
                        ),
                    }
                }
            }
            StubReport::Collision {
                package,
                diagnostic,
            } => match format {
                OutputFormat::Text => eprintln!("warning: {}", diagnostic.message),
                OutputFormat::Json => println!(
                    "{}",
                    crate::diagnostic::diagnostic_to_json(package, diagnostic)
                ),
            },
        }
    }
}
