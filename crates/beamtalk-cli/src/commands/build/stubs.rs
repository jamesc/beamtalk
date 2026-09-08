// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0075: native FFI type-spec extraction and the stub-registry
//! resolution chain (extracted -> distribution -> package-bundled ->
//! project-local), shared by `beamtalk build` and `beamtalk lint`.

use camino::{Utf8Path, Utf8PathBuf};
use std::collections::HashMap;
use std::fs;
use tracing::warn;

use crate::commands::OutputFormat;
use crate::commands::build_layout::BuildLayout;

use super::sources::collect_source_files_from_dir;

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
/// BT-2851: This is the single source of truth for populating a
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

    // BT-2858: the manifest-backed extraction path now lives in the
    // `beamtalk_cli` lib crate (`native_type_specs`) so `beamtalk-mcp` can
    // call the same single source of truth without reading a possibly-stale
    // on-disk cache. This is a thin delegation, not a duplicate.
    beamtalk_cli::native_type_specs::extract_project_type_specs(layout)
}

/// Locate the compiler distribution's own curated FFI type stubs (ADR 0075
/// layer 3), installed alongside the stdlib sources under
/// `{sysroot}/share/beamtalk/stubs/` — the same sysroot convention used by
/// `beamtalk --print-sysroot` and the LSP's `sysroot_stdlib_source_dir`,
/// both backed by the shared [`beamtalk_sysroot`] leaf crate.
/// `BEAMTALK_STUBS_DIR` overrides the sysroot-derived path for development.
///
/// Unlike `BEAMTALK_RUNTIME_DIR` (which hard-errors when set to an invalid
/// path), an unset/missing/non-directory override here just logs a warning
/// and falls through to `None` — this stub tier is optional and best-effort,
/// so a build should never fail over a distribution-stub misconfiguration.
///
/// Returns `None` if no such directory exists on disk — expected until
/// curated distribution stub content is added (BT-1848); the discovery/merge
/// plumbing is still correct with nothing to find.
pub(crate) fn distribution_stubs_dir() -> Option<Utf8PathBuf> {
    if let Ok(dir) = std::env::var("BEAMTALK_STUBS_DIR") {
        let candidate = Utf8PathBuf::from(dir);
        if candidate.is_dir() {
            return Some(candidate);
        }
        warn!(
            dir = %candidate,
            "BEAMTALK_STUBS_DIR is set but is not a directory; ignoring distribution stubs"
        );
        return None;
    }

    let sysroot = beamtalk_sysroot::current_sysroot()?;
    let candidate = Utf8PathBuf::from_path_buf(sysroot.join("share/beamtalk/stubs")).ok()?;
    candidate.is_dir().then_some(candidate)
}

/// ADR 0075 Phase 2 (BT-1847): scan `project_root/stubs/` for `declare
/// native:` stub files and build the project-local stub-tier registry, plus
/// its diagnostics (skipped-signature warnings and version-drift warnings
/// against `auto_extract`).
///
/// `stubs/` is auto-discovered — no `beamtalk.toml` config needed, matching
/// ADR 0075's project-local-stubs design. Returns `None` when there is no
/// `stubs/` directory or it contains no `.bt` files (the common case — most
/// projects have no stubs at all), so callers can tell "nothing to merge"
/// apart from "stubs/ exists but is empty of declarations".
///
/// Pass `auto_extract` as the *pre-merge* auto-extracted registry — the
/// drift check (`NativeTypeRegistry::detect_stub_drift`) needs it as ground
/// truth for what a `.beam` module actually exports, which only holds before
/// stub overrides are applied on top of it.
///
/// Shared by `beamtalk build` and `beamtalk lint`, mirroring
/// [`extract_type_specs`]'s "single source of truth" role, so both surfaces
/// agree on stub-derived types by construction.
///
/// `format` controls how each stub diagnostic (skipped signature, version
/// drift) is rendered as it's discovered — `beamtalk build` always passes
/// [`OutputFormat::Text`] (it has no JSON mode); `beamtalk lint` passes its
/// own `--format`, so a stub diagnostic streams as a JSON line under
/// `lint --format=json` the same way every other lint diagnostic does,
/// instead of always printing miette text regardless of the caller's
/// requested format.
pub(crate) fn load_project_stub_registry(
    project_root: &Utf8Path,
    auto_extract: Option<&beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>,
    format: OutputFormat,
) -> Option<(
    beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry,
    Vec<beamtalk_core::source_analysis::Diagnostic>,
)> {
    load_stub_registry_from_dir(&project_root.join("stubs"), auto_extract, format)
}

/// Load and merge the package-bundled (ADR 0075 layer 2) and distribution
/// (layer 3) stub tiers, ranked in that precedence order: distribution
/// applied first, then each dependency's own bundled stubs on top —
/// matching the resolution chain's "project > package > distribution >
/// auto-extracted" order (project-local stubs are merged separately, last,
/// by [`load_project_stub_registry`]'s caller).
///
/// A dependency's `stubs_dir` (set by dependency resolution from its own
/// `beamtalk.toml` `[stubs] path`, ADR 0075 layer 2) covers only that
/// package's own native code — never its own dependencies' stubs, since
/// those are auto-extracted instead. Because each package is expected to
/// stub only its own native modules, two sibling dependencies declaring the
/// same `(module, function, arity)` is unexpected — `warn_on_package_stub_collisions`
/// flags it (naming both packages) rather than letting the later dependency
/// in iteration order silently win, mirroring how `check_native_module_collisions`
/// surfaces a native-module-implementation collision across dependencies.
///
/// `distribution_stubs_dir` is the compiler distribution's own `stubs/`
/// directory (layer 3), when one is discoverable — `None` until curated
/// distribution stub content exists (BT-1848). Distribution-vs-package
/// overlap is not flagged: package stubs are *meant* to override
/// distribution stubs, per the resolution order.
///
/// Returns `None` only when none of these layers contributed anything.
pub(crate) fn load_dependency_stub_registries(
    resolved_deps: &[crate::commands::deps::path::ResolvedDependency],
    distribution_stubs_dir: Option<&Utf8Path>,
    auto_extract: Option<&beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>,
    format: OutputFormat,
) -> Option<(
    beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry,
    Vec<beamtalk_core::source_analysis::Diagnostic>,
)> {
    let mut registry = beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry::new();
    let mut diagnostics = Vec::new();
    let mut found_any = false;

    if let Some(dist_dir) = distribution_stubs_dir {
        if let Some((dist_registry, dist_diagnostics)) =
            load_stub_registry_from_dir(dist_dir, auto_extract, format)
        {
            registry.apply_overrides(dist_registry);
            diagnostics.extend(dist_diagnostics);
            found_any = true;
        }
    }

    // Tracks which dependency first declared each (module, function, arity)
    // package-bundled stub, so a sibling dependency re-declaring the same
    // signature is flagged rather than silently overriding it.
    let mut package_stub_owners: HashMap<(String, String, u8), String> = HashMap::new();

    for dep in resolved_deps {
        let Some(stubs_dir) = dep.stubs_dir.as_deref() else {
            continue;
        };
        if let Some((dep_registry, dep_diagnostics)) =
            load_stub_registry_from_dir(stubs_dir, auto_extract, format)
        {
            diagnostics.extend(warn_on_package_stub_collisions(
                &dep_registry,
                &dep.name,
                &mut package_stub_owners,
                format,
            ));
            registry.apply_overrides(dep_registry);
            diagnostics.extend(dep_diagnostics);
            found_any = true;
        }
    }

    found_any.then_some((registry, diagnostics))
}

/// Records every `(module, function, arity)` in `dep_registry` as owned by
/// `dep_name` in `owners`, returning a warning diagnostic for each one
/// already owned by a *different* package — a same-module stub collision
/// between two sibling dependencies' own package-bundled stubs (see
/// [`load_dependency_stub_registries`]).
///
/// Each collision is rendered immediately (respecting `format`), mirroring
/// [`load_stub_registry_from_dir`]'s own drift/skipped-signature
/// diagnostics — without it, a collision would only ever show up as an
/// anonymous count in the build summary (`beamtalk build`) or be silently
/// dropped (`beamtalk lint`, which discards the diagnostics its caller
/// doesn't fold into `all_diags`). There is no single stub file's source to
/// render a code snippet against here (the two declarations live in two
/// different packages' stub files), so this prints a plain message rather
/// than a source-annotated miette report.
fn warn_on_package_stub_collisions(
    dep_registry: &beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry,
    dep_name: &str,
    owners: &mut HashMap<(String, String, u8), String>,
    format: OutputFormat,
) -> Vec<beamtalk_core::source_analysis::Diagnostic> {
    let mut collisions = Vec::new();

    for module in dep_registry.module_names() {
        let Some(functions) = dep_registry.module_functions(module) else {
            continue;
        };
        for func in functions {
            let key = (module.to_string(), func.name.clone(), func.arity);
            match owners.insert(key, dep_name.to_string()) {
                Some(existing_owner) if existing_owner != dep_name => {
                    let span = match func.provenance {
                        beamtalk_core::semantic_analysis::type_checker::TypeProvenance::Declared(
                            s,
                        ) => s,
                        _ => beamtalk_core::source_analysis::Span::default(),
                    };
                    let message = format!(
                        "Package-bundled stub collision: both '{existing_owner}' and \
                         '{dep_name}' declare `{module}:{}/{}` — the later dependency's \
                         stub silently wins. Package stubs should cover only that \
                         package's own native code (ADR 0075); rename or remove the \
                         duplicate declaration.",
                        func.name, func.arity
                    );
                    warn!(
                        module,
                        function = %func.name,
                        arity = func.arity,
                        first_owner = %existing_owner,
                        second_owner = %dep_name,
                        "Package-bundled stub collision between sibling dependencies"
                    );
                    let diagnostic =
                        beamtalk_core::source_analysis::Diagnostic::warning(message, span);
                    match format {
                        OutputFormat::Text => eprintln!("warning: {}", diagnostic.message),
                        OutputFormat::Json => {
                            println!(
                                "{}",
                                crate::diagnostic::diagnostic_to_json(dep_name, &diagnostic)
                            );
                        }
                    }
                    collisions.push(diagnostic);
                }
                _ => {}
            }
        }
    }

    collisions
}

/// Parse and merge every `declare native:` stub file directly inside `stubs_dir`
/// into a single [`NativeTypeRegistry`], checking each against `auto_extract`
/// for version drift. Shared by every stub-resolution-chain layer
/// (project-local, package-bundled, distribution) — each layer is just a
/// different directory.
///
/// Returns `None` if `stubs_dir` does not exist or contains no `.bt` files.
fn load_stub_registry_from_dir(
    stubs_dir: &Utf8Path,
    auto_extract: Option<&beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>,
    format: OutputFormat,
) -> Option<(
    beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry,
    Vec<beamtalk_core::source_analysis::Diagnostic>,
)> {
    if !stubs_dir.exists() {
        return None;
    }
    let stub_files = match collect_source_files_from_dir(stubs_dir) {
        Ok(files) => files,
        Err(e) => {
            warn!(dir = %stubs_dir, error = %e, "Cannot read stubs/ directory; skipping stub resolution");
            return None;
        }
    };
    if stub_files.is_empty() {
        return None;
    }

    let mut registry = beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry::new();
    let mut all_diagnostics = Vec::new();

    for file in &stub_files {
        let source = match fs::read_to_string(file) {
            Ok(s) => s,
            Err(e) => {
                warn!(file = %file, error = %e, "Cannot read stub file; skipping");
                continue;
            }
        };
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
        let (module, mut file_diagnostics) = beamtalk_core::source_analysis::parse(tokens);

        // BT-1847: this file's own declarations, checked for drift against
        // `auto_extract` *before* merging into the accumulating `registry` —
        // scoping the check to one file's functions lets each warning render
        // against that file's own source (below), rather than losing track
        // of which stub file a merged function came from.
        let mut file_registry =
            beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry::new();
        file_diagnostics.extend(
            beamtalk_core::semantic_analysis::type_checker::load_native_declarations(
                &module.native_declarations,
                &mut file_registry,
            ),
        );
        if let Some(auto_extract) = auto_extract {
            for (module_name, sig) in auto_extract.detect_stub_drift(&file_registry) {
                let span = match sig.provenance {
                    beamtalk_core::semantic_analysis::type_checker::TypeProvenance::Declared(s) => {
                        s
                    }
                    _ => beamtalk_core::source_analysis::Span::default(),
                };
                file_diagnostics.push(
                    beamtalk_core::source_analysis::Diagnostic::warning(
                        format!(
                            "Stub declares `{module_name}:{}/{}`, which was not found in the \
                             compiled `{module_name}.beam` exports — the stub may be out of date",
                            sig.name, sig.arity
                        ),
                        span,
                    )
                    .with_hint(
                        "Regenerate this stub with `beamtalk generate stubs`, or remove the \
                         stale declaration.",
                    ),
                );
            }
        }
        // Per-function upsert (not `merge`'s whole-module keep-on-collision):
        // two stub files declaring the same Erlang module must combine, with
        // the later file's function/arity winning on overlap.
        registry.apply_overrides(file_registry);

        // Render immediately against this file's own source, mirroring
        // `compile_source_with_bindings`'s rendering for src/ files —
        // without it, a drift or skipped-signature warning would only ever
        // show up as an anonymous count in the build summary. Respects
        // `format` so `lint --format=json` gets these as JSON lines too
        // (see this function's doc).
        for diagnostic in &file_diagnostics {
            if diagnostic.severity == beamtalk_core::source_analysis::Severity::Lint {
                continue;
            }
            match format {
                OutputFormat::Text => {
                    let compile_diag = crate::diagnostic::CompileDiagnostic::from_core_diagnostic(
                        diagnostic,
                        file.as_str(),
                        &source,
                    );
                    eprintln!("{:?}", miette::Report::new(compile_diag));
                }
                OutputFormat::Json => {
                    let json = crate::diagnostic::diagnostic_to_json(file.as_str(), diagnostic);
                    println!("{json}");
                }
            }
        }

        all_diagnostics.extend(file_diagnostics);
    }

    Some((registry, all_diagnostics))
}
