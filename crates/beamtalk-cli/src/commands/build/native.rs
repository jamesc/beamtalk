// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0072: native Erlang compilation — the generated `beamtalk_classes.hrl`
//! header, hex dependencies via bundled rebar3, `native/*.erl` via `erlc`,
//! and the cross-package native-module collision check.

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::collections::HashMap;
use std::fmt::Write;
use std::fs;
use std::path::PathBuf;
use tracing::{debug, info, instrument};

use crate::commands::build_layout::BuildLayout;
use crate::commands::deps::lockfile::{NativeLockSource, NativePackageLock};
use crate::commands::manifest::{
    self, NativeDependency, NativeDependencyMap, NativeOverrideMap, NativeSource,
};
use beamtalk_core::compilation::GitReference;

use super::environment::{BuildEnvironment, DependencyContext};

/// Compile native Erlang sources (native/*.erl and hex dependencies).
///
/// Runs after Pass 1 so the generated `beamtalk_classes.hrl` header is available
/// for native `.erl` files to include. Also validates that native files don't
/// contain hardcoded `bt@<pkg>@` references.
pub(crate) fn compile_native_sources(
    env: &BuildEnvironment,
    dep_ctx: &DependencyContext,
    class_module_index: &HashMap<String, String>,
) -> Result<Option<Rebar3Result>> {
    let pkg_manifest = env.pkg_manifest();

    // Generate beamtalk_classes.hrl before native Erlang compilation.
    // This header provides ?BT_CLASS_MODULE macros so native .erl files can
    // reference Beamtalk class modules without hardcoding bt@<pkg>@<class> atoms.
    if pkg_manifest.is_some() {
        let hrl_dir = env.layout.native_include_dir();
        generate_class_header(&hrl_dir, class_module_index)?;
    }

    // ADR 0072: Compile native Erlang sources after Pass 1.
    // Native modules must be compiled before .bt files (Pass 2) because .bt
    // files may reference them via `(Erlang module)` FFI or `native:` annotations.
    // Moved after Pass 1 so the generated beamtalk_classes.hrl is available.
    let native_result = if pkg_manifest.is_some() && dep_ctx.has_native_deps {
        // Path B: rebar3 handles both hex deps and native/*.erl
        let full_manifest = env.full_manifest.as_ref().unwrap();

        // Aggregate native dependencies from all packages in the dependency graph
        let aggregated = aggregate_native_dependencies(
            &full_manifest.native_dependencies,
            &full_manifest.native_overrides,
            &dep_ctx.resolved_deps,
        );
        for warning in &aggregated.warnings {
            eprintln!("warning: {warning}");
        }

        let rebar3_result = compile_with_rebar3(
            &env.project_root,
            &aggregated.dependencies,
            &aggregated.overrides,
            false,
            &dep_ctx.resolved_deps,
        )?;
        eprintln!(
            "Compiled native deps via rebar3 ({} package(s), {} ebin dir(s))",
            aggregated.dependencies.len(),
            rebar3_result.ebin_paths.len()
        );
        Some(rebar3_result)
    } else if pkg_manifest.is_some() {
        // Nothing in the dependency graph declares a native dependency, so
        // any `[[native_package]]` entries left in beamtalk.lock (e.g. from a
        // dependency that has since dropped its native deps) are orphans.
        // The runtime boots every locked native package, so they must go
        // (BT-4549).
        let pruned =
            crate::commands::deps::lockfile::Lockfile::prune_native_packages(&env.project_root)?;
        if !pruned.is_empty() {
            eprintln!(
                "Pruned orphaned native package lock(s) from beamtalk.lock: {}",
                pruned.join(", ")
            );
        }

        // Path A: compile native/*.erl directly via erlc (no hex deps).
        // Uses compile_native_erlang_with_deps to also compile transitive
        // BT dependency native sources (same as Path B's post-rebar3 step).
        let native_ebin =
            compile_native_erlang_with_deps(&env.project_root, &dep_ctx.resolved_deps)?;
        let module_names = {
            use crate::beam_compiler::discover_native_modules;
            discover_native_modules(&env.project_root)?
        };
        match native_ebin {
            Some(ebin) => Some(Rebar3Result {
                ebin_paths: vec![ebin],
                module_names,
            }),
            None if !module_names.is_empty() => {
                // Native modules discovered but no ebin produced — shouldn't happen
                None
            }
            None => None,
        }
    } else {
        None
    };

    // Validate native .erl files for hardcoded bt@<pkg>@ module references.
    // Warns at compile time so moving a class between packages doesn't cause
    // silent runtime failures.
    if let Some(pkg) = pkg_manifest {
        validate_native_class_references(&env.project_root, &pkg.name, class_module_index)?;
    }

    Ok(native_result)
}

// ── Class module header generation ──────────────────────────────────────────

/// Generate a `beamtalk_classes.hrl` header file with Erlang preprocessor
/// macros that map Beamtalk class names to their compiled BEAM module atoms.
///
/// For each entry in the `class_module_index`, emits:
/// ```erlang
/// -define(BT_CLASS_MODULE_HTTPResponse, 'bt@http@httpresponse').
/// ```
///
/// Native `.erl` files include this header and use `?BT_CLASS_MODULE_ClassName`
/// instead of hardcoding `'bt@<pkg>@<class>'` atoms. When a class moves
/// between packages, the macro value updates automatically on rebuild.
///
/// The header is written to `_build/dev/native/include/beamtalk_classes.hrl`
/// and is added to the erlc include path during native compilation.
pub(crate) fn generate_class_header(
    include_dir: &Utf8Path,
    class_module_index: &HashMap<String, String>,
) -> Result<()> {
    fs::create_dir_all(include_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create native include directory '{include_dir}'"))?;

    let hrl_path = include_dir.join("beamtalk_classes.hrl");

    let mut content = String::new();
    content.push_str("%% Copyright 2026 James Casey\n");
    content.push_str("%% SPDX-License-Identifier: Apache-2.0\n\n");
    content.push_str("%% Generated by beamtalk build — do not edit.\n");
    content.push_str("%% BT-1730: Maps Beamtalk class names to compiled BEAM module atoms.\n");
    content.push_str("%%\n");
    content.push_str("%% Usage in native .erl files:\n");
    content.push_str("%%   -include(\"beamtalk_classes.hrl\").\n");
    content.push_str("%%   ?BT_CLASS_MODULE_HTTPResponse:some_function(Args).\n\n");
    content.push_str("-ifndef(BEAMTALK_CLASSES_HRL).\n");
    content.push_str("-define(BEAMTALK_CLASSES_HRL, true).\n\n");

    // Sort for deterministic output across builds.
    let mut entries: Vec<(&String, &String)> = class_module_index.iter().collect();
    entries.sort_by_key(|(class_name, _)| class_name.as_str());

    for (class_name, module_name) in &entries {
        let _ = writeln!(
            content,
            "-define(BT_CLASS_MODULE_{class_name}, '{module_name}')."
        );
    }

    content.push_str("\n-endif. %% BEAMTALK_CLASSES_HRL\n");

    fs::write(&hrl_path, &content)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write {hrl_path}"))?;

    info!(
        path = %hrl_path,
        count = entries.len(),
        "Generated beamtalk_classes.hrl"
    );

    Ok(())
}

/// Scan native `.erl` files for hardcoded `bt@<pkg>@<class>` module references
/// and warn when they should use `?BT_CLASS_MODULE_*` macros instead.
///
/// This catches the footgun where native code directly references a compiled
/// Beamtalk module atom. If the class moves between packages (or the package
/// is renamed), the hardcoded atom becomes stale and causes silent runtime
/// failures.
///
/// Detects two categories:
/// 1. Any `'bt@<current_pkg>@...'` atom — these should always use macros.
/// 2. Any `'bt@<other_pkg>@...'` atom that matches a known class in the
///    `class_module_index` — catches stale references from a class move.
///
/// Only warns about references in code (not comments or doc strings).
pub(crate) fn validate_native_class_references(
    project_root: &Utf8Path,
    pkg_name: &str,
    class_module_index: &HashMap<String, String>,
) -> Result<()> {
    let native_dir = project_root.join("native");
    if !native_dir.exists() || !native_dir.is_dir() {
        return Ok(());
    }

    // Build a set of known module names for reverse-lookup.
    let known_modules: std::collections::HashSet<&str> =
        class_module_index.values().map(String::as_str).collect();

    // Build a reverse index: module_name -> class_name for reporting.
    let module_to_class: HashMap<&str, &str> = class_module_index
        .iter()
        .map(|(class, module)| (module.as_str(), class.as_str()))
        .collect();

    let mut erl_files: Vec<Utf8PathBuf> = Vec::new();
    collect_erl_files(&native_dir, &mut erl_files)?;

    for erl_file in &erl_files {
        let Ok(source) = fs::read_to_string(erl_file) else {
            continue;
        };

        for (line_no, line) in source.lines().enumerate() {
            let trimmed = line.trim();
            // Skip comments and empty lines.
            if trimmed.starts_with('%') || trimmed.is_empty() {
                continue;
            }

            // Find all 'bt@...' atom references on this line.
            let mut search_from = 0;
            while let Some(offset) = line[search_from..].find("'bt@") {
                let pos = search_from + offset;
                let after_quote = &line[pos + 1..]; // skip leading quote
                if let Some(end) = after_quote.find('\'') {
                    let module_atom = &after_quote[..end];

                    // Category 1: current package reference — always warn
                    let is_current_pkg = module_atom.starts_with(&format!("bt@{pkg_name}@"));
                    // Category 2: known class from any package — warn about hardcoding
                    let is_known_module = known_modules.contains(module_atom);

                    if is_current_pkg || is_known_module {
                        let class_hint = module_to_class
                            .get(module_atom)
                            .map(|c| format!(" (class {c})"))
                            .unwrap_or_default();
                        eprintln!(
                            "warning: {}:{}:{}: hardcoded Beamtalk module reference \
                             '{module_atom}'{class_hint}; use ?BT_CLASS_MODULE_* macro \
                             from beamtalk_classes.hrl instead (BT-1730)",
                            erl_file,
                            line_no + 1,
                            pos + 1,
                        );
                    }

                    // Advance past this atom to find more on the same line.
                    search_from = pos + 1 + end + 1;
                } else {
                    break;
                }
            }
        }
    }

    Ok(())
}

// ── Bundled rebar3 ────────────────────────────────────────────────

/// Locate the rebar3 escript for compiling hex dependencies.
///
/// Resolution order:
/// 1. **Bundled copy** — `runtime/tools/rebar3` relative to the runtime directory
///    (found via [`beamtalk_cli::repl_startup::find_runtime_dir`]). This is the
///    primary path and should always succeed in a standard Beamtalk installation.
/// 2. **System `rebar3`** — falls back to `rebar3` on `$PATH` if the bundled
///    copy is missing (e.g., when running from an unusual layout).
///
/// # Errors
///
/// Returns an error if neither the bundled copy nor a system `rebar3` is found.
///
/// # Version policy
///
/// The bundled rebar3 is pinned to 3.27.0. Update when:
/// - A new rebar3 release adds features we need or fixes bugs we hit
/// - OTP compatibility requires a newer version
/// - Security advisories are published
pub(crate) fn rebar3_path() -> Result<PathBuf> {
    // Try bundled copy relative to the runtime directory
    if let Ok(runtime_dir) = beamtalk_cli::repl_startup::find_runtime_dir() {
        let bundled = runtime_dir.join("tools").join("rebar3");
        if bundled.exists() {
            debug!(path = %bundled.display(), "Using bundled rebar3");
            return Ok(bundled);
        }
    }

    // Fall back to system rebar3 on $PATH
    let system_candidates = if cfg!(windows) {
        vec!["rebar3.cmd", "rebar3.exe", "rebar3"]
    } else {
        vec!["rebar3"]
    };

    for candidate in system_candidates {
        if let Ok(output) = std::process::Command::new(candidate)
            .arg("version")
            .output()
        {
            if output.status.success() {
                info!(
                    rebar3 = candidate,
                    "Using system rebar3 (bundled copy not found)"
                );
                return Ok(PathBuf::from(candidate));
            }
        }
    }

    Err(miette::miette!(
        "rebar3 not found.\n\
         Expected bundled copy at <runtime>/tools/rebar3.\n\
         Install rebar3 or run from the repository root."
    ))
}

// ── ADR 0072 Phase 2: rebar3 integration ──────────────────────────

/// Result of a rebar3 compilation step.
///
/// Contains the ebin paths that must be added to the BEAM code path
/// for subsequent `.bt` compilation and at runtime.
#[derive(Debug)]
pub(crate) struct Rebar3Result {
    /// Ebin directories produced by rebar3 (one per compiled hex dep,
    /// plus the package's own native modules if `native/` exists).
    /// Used by `run.rs` and `repl/mod.rs` via `collect_rebar3_ebin_paths`.
    #[allow(dead_code)] // Informational; code paths are collected from filesystem
    pub ebin_paths: Vec<Utf8PathBuf>,
    /// Erlang module names compiled from native sources.
    /// Used for `.app.src` module list generation.
    pub module_names: Vec<String>,
}

/// Native dependencies and rebar3 overrides aggregated across the whole
/// Beamtalk dependency graph.
#[derive(Debug, Default)]
pub(crate) struct AggregatedNative {
    /// Every native dependency, first declaration wins.
    pub dependencies: NativeDependencyMap,
    /// Every rebar3 override, first declaration wins.
    pub overrides: NativeOverrideMap,
    /// One message per conflicting redeclaration that was dropped
    /// (ADR 0073 "Native dep version conflicts").
    pub warnings: Vec<String>,
}

/// Aggregate native dependencies and rebar3 overrides from the root package
/// and all resolved Beamtalk dependencies in the dependency graph.
///
/// ADR 0072 §5: Because BEAM has a flat module namespace, all native deps
/// across all packages must be resolved in a single rebar3 invocation.
/// The root package is consulted first, then dependencies in resolution
/// order; the first declaration of a name wins. ADR 0073: when a later
/// package declares the same name with a different source (another hex
/// constraint, or git vs hex), the later declaration is dropped and a
/// warning naming both packages is recorded.
pub(crate) fn aggregate_native_dependencies(
    root_native_deps: &NativeDependencyMap,
    root_overrides: &NativeOverrideMap,
    resolved_bt_deps: &[crate::commands::deps::path::ResolvedDependency],
) -> AggregatedNative {
    const ROOT: &str = "the root package";

    let mut aggregated = AggregatedNative {
        dependencies: root_native_deps.clone(),
        overrides: root_overrides.clone(),
        warnings: Vec::new(),
    };
    // Which package each kept entry came from, for conflict warnings.
    let mut dep_origin: HashMap<String, String> = root_native_deps
        .keys()
        .map(|name| (name.clone(), ROOT.to_string()))
        .collect();
    let mut override_origin: HashMap<String, String> = root_overrides
        .keys()
        .map(|name| (name.clone(), ROOT.to_string()))
        .collect();

    // Walk each resolved Beamtalk dependency and collect its [native] section
    for dep in resolved_bt_deps {
        let manifest_path = dep.root.join("beamtalk.toml");
        if !manifest_path.exists() {
            continue;
        }
        let Ok(parsed) = manifest::parse_manifest_full(&manifest_path) else {
            debug!(
                dep = %dep.name,
                "Failed to parse manifest for native dep aggregation — skipping"
            );
            continue;
        };
        let origin = format!("package '{}'", dep.name);

        for (name, native_dep) in &parsed.native_dependencies {
            match aggregated.dependencies.get(name) {
                None => {
                    debug!(
                        dep = %dep.name,
                        native_pkg = %name,
                        source = %native_dep.source,
                        "Aggregating native dependency from transitive package"
                    );
                    aggregated
                        .dependencies
                        .insert(name.clone(), native_dep.clone());
                    dep_origin.insert(name.clone(), origin.clone());
                }
                Some(kept) if kept.source != native_dep.source => {
                    aggregated.warnings.push(format!(
                        "native dependency '{name}' is declared as {} by {} but as {} by {origin} \
                         — using {}",
                        kept.source, dep_origin[name], native_dep.source, kept.source
                    ));
                }
                Some(_) => {}
            }
        }

        for (name, native_override) in &parsed.native_overrides {
            match aggregated.overrides.get(name) {
                None => {
                    aggregated
                        .overrides
                        .insert(name.clone(), native_override.clone());
                    override_origin.insert(name.clone(), origin.clone());
                }
                Some(kept) if kept != native_override => {
                    aggregated.warnings.push(format!(
                        "native override for '{name}' differs between {} and {origin} \
                         — using the one from {}",
                        override_origin[name], override_origin[name]
                    ));
                }
                Some(_) => {}
            }
        }
    }

    aggregated
}

/// Render one `{deps, [...]}` entry for `rebar.config` from the manifest.
///
/// Pinning to locked versions and commits is done by the generated
/// `rebar.lock` (see [`prepare_rebar_lock`]), which rebar3 prefers over
/// `rebar.config`.
fn rebar_dep_entry(dep: &NativeDependency) -> String {
    use crate::beam_compiler::escape_erlang_string;

    let name = &dep.name;
    match &dep.source {
        NativeSource::Hex { constraint } => {
            format!("    {{{name}, \"{}\"}}", escape_erlang_string(constraint))
        }
        NativeSource::Git { url, reference } => {
            let (kind, value) = match reference {
                GitReference::Tag(tag) => ("tag", tag),
                GitReference::Branch(branch) => ("branch", branch),
                GitReference::Rev(rev) => ("ref", rev),
            };
            format!(
                "    {{{name}, {{git, \"{}\", {{{kind}, \"{}\"}}}}}}",
                escape_erlang_string(url),
                escape_erlang_string(value)
            )
        }
    }
}

/// Whether a direct dependency's `beamtalk.lock` entry still satisfies the
/// manifest: a hex version within the constraint, or a git commit resolved
/// from the same URL and tag/branch/rev.
fn lock_entry_is_current(dep: &NativeDependency, lock: Option<&NativePackageLock>) -> bool {
    match (&dep.source, lock.map(|lock| &lock.source)) {
        (
            NativeSource::Hex { constraint },
            Some(NativeLockSource::Hex {
                package, version, ..
            }),
        ) => package == &dep.name && manifest::hex_constraint_matches(constraint, version),
        (
            NativeSource::Git { url, reference },
            Some(NativeLockSource::Git {
                url: locked_url,
                reference: Some(locked_reference),
                ..
            }),
        ) => url == locked_url && reference == locked_reference,
        _ => false,
    }
}

/// How `beamtalk.lock` relates to the manifest's native dependencies.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum NativeLockState {
    /// Every direct dependency's entry is current and no locked direct
    /// dependency has been removed: pin everything.
    Current,
    /// Some direct dependencies changed, were added or removed. Only the
    /// named direct dependencies are still current and stay pinned;
    /// everything else (including all transitive dependencies) re-resolves.
    Partial(Vec<String>),
}

/// Classify `beamtalk.lock`'s native entries against the manifest.
pub(crate) fn native_lock_state(
    native_deps: &NativeDependencyMap,
    locks: &std::collections::BTreeMap<String, NativePackageLock>,
) -> NativeLockState {
    let kept: Vec<String> = native_deps
        .values()
        .filter(|dep| lock_entry_is_current(dep, locks.get(&dep.name)))
        .map(|dep| dep.name.clone())
        .collect();
    let removed_direct = locks
        .values()
        .any(|lock| lock.level == Some(0) && !native_deps.contains_key(&lock.name));
    if kept.len() == native_deps.len() && !removed_direct {
        NativeLockState::Current
    } else {
        NativeLockState::Partial(kept)
    }
}

/// Make rebar3's own lock state match `beamtalk.lock` before `rebar3 compile`.
///
/// rebar3 never re-fetches an existing checkout because `rebar.config`
/// changed — only a `rebar.lock` entry moves one — and it keeps checkouts
/// and lock entries for dependencies nothing needs any more. So:
///
/// - **Current** ([`native_lock_state`]): write `rebar.lock` from all of
///   `beamtalk.lock`, pinning every locked package (direct and transitive),
///   including on a fresh checkout.
/// - **Partial** (a direct dependency's constraint or git source changed, or
///   one was added or removed): pin only the still-current direct
///   dependencies, and delete every other checkout so rebar3 resolves it
///   afresh from `rebar.config`. Transitive dependencies re-resolve too —
///   `beamtalk.lock` does not record which direct dependency needs them.
/// - **No lock / `force_resolve`** (`beamtalk deps update` clears the native
///   locks): the same with nothing pinned.
///
/// Returns whether the build re-resolves anything.
pub(crate) fn prepare_rebar_lock(
    layout: &BuildLayout,
    native_deps: &NativeDependencyMap,
    locks: Option<&std::collections::BTreeMap<String, NativePackageLock>>,
    force_resolve: bool,
) -> Result<bool> {
    use crate::commands::deps::lockfile::render_rebar_lock;

    let empty = std::collections::BTreeMap::new();
    let locks = locks.filter(|_| !force_resolve).unwrap_or(&empty);
    let rebar_lock = layout.native_dir().join("rebar.lock");
    let is_direct = |name: &str| native_deps.contains_key(name);

    let kept = match native_lock_state(native_deps, locks) {
        NativeLockState::Current => {
            info!("Using pinned native package versions from beamtalk.lock");
            write_rebar_lock(&rebar_lock, &render_rebar_lock(locks.values(), &is_direct))?;
            return Ok(false);
        }
        NativeLockState::Partial(kept) => kept,
    };

    info!(
        pinned = ?kept,
        "Native dependencies changed or unlocked — re-resolving the rest via rebar3"
    );
    // Kept entries are direct dependencies: pin them at level 0.
    let pinned = kept
        .iter()
        .filter_map(|name| locks.get(name))
        .map(|lock| lock.clone().at_level(0));
    let pinned: Vec<NativePackageLock> = pinned.collect();
    write_rebar_lock(&rebar_lock, &render_rebar_lock(&pinned, &is_direct))?;

    // Delete every checkout that is not pinned, so rebar3 fetches it afresh
    // (or not at all, once nothing needs it).
    let lib_dir = layout.rebar_lib_dir();
    if lib_dir.exists() {
        let entries = fs::read_dir(&lib_dir)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to read native deps directory '{lib_dir}'"))?;
        for entry in entries.flatten() {
            let keep = entry
                .file_name()
                .to_str()
                .is_some_and(|name| kept.iter().any(|k| k == name));
            if !keep {
                let path = entry.path();
                let removed = if path.is_dir() {
                    fs::remove_dir_all(&path)
                } else {
                    fs::remove_file(&path)
                };
                removed.into_diagnostic().wrap_err_with(|| {
                    format!("Failed to remove stale native dep '{}'", path.display())
                })?;
            }
        }
    }
    Ok(true)
}

fn write_rebar_lock(path: &Utf8Path, content: &str) -> Result<()> {
    fs::write(path, content)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write rebar.lock at '{path}'"))
}

/// Record which git reference each direct git dependency's lock entry was
/// resolved from — rebar.lock only knows the commit. Lets
/// [`native_lock_state`] spot a later branch/tag/rev change.
fn record_manifest_reference(pkg: &mut NativePackageLock, native_deps: &NativeDependencyMap) {
    if let (
        NativeLockSource::Git { url, reference, .. },
        Some(NativeSource::Git {
            url: manifest_url,
            reference: manifest_reference,
        }),
    ) = (
        &mut pkg.source,
        native_deps.get(&pkg.name).map(|dep| &dep.source),
    ) {
        if url == manifest_url {
            *reference = Some(manifest_reference.clone());
        }
    }
}

/// Test-only environment variable (BT-4582): base URL of a local hex
/// repository that replaces `repo.hex.pm` in the generated `rebar.config`.
/// The repository need not be signed; verification is disabled for it.
const TEST_HEX_REPO_URL_ENV: &str = "BEAMTALK_TEST_HEX_REPO_URL";

/// Generate a `rebar.config` file for compiling native dependencies.
///
/// The generated file lives at `_build/dev/native/rebar.config` and is a
/// build artifact — it should not be checked in.
///
/// rebar3 reads `rebar.config` from its current working directory
/// (it has no `--config` flag). We run rebar3 from `_build/dev/native/`
/// so the config lives there.
///
/// The config carries the manifest's constraints and git references;
/// locked versions are pinned through `rebar.lock` instead
/// ([`prepare_rebar_lock`], ADR 0072 §5).
///
/// `overrides` (from `[native.overrides]`) are emitted as rebar3
/// `{overrides, [{override, Dep, Opts}]}`.
///
/// Note: rebar3 only handles native deps. Native `.erl` files from the
/// project and transitive BT dependencies are compiled separately via `erlc`
/// in [`compile_native_erlang_with_deps`].
pub(crate) fn generate_rebar_config(
    project_root: &Utf8Path,
    native_deps: &NativeDependencyMap,
    overrides: &NativeOverrideMap,
) -> Result<Utf8PathBuf> {
    let layout = BuildLayout::new(project_root);
    let rebar_dir = layout.native_dir();
    fs::create_dir_all(&rebar_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create rebar3 build directory '{rebar_dir}'"))?;

    let config_path = rebar_dir.join("rebar.config");

    let mut config = String::new();
    config.push_str("%% Auto-generated by beamtalk build — do not edit\n");

    // Generate {deps, [...]} from the manifest.
    config.push_str("{deps, [\n");
    let dep_entries: Vec<String> = native_deps.values().map(rebar_dep_entry).collect();
    config.push_str(&dep_entries.join(",\n"));
    config.push_str("\n]}.\n");

    // rebar3 only handles native deps — native/*.erl files are compiled
    // separately via erlc after rebar3 finishes (see compile_native_erlang_with_deps).
    // This avoids rebar3 project discovery issues: rebar3 won't compile
    // src_dirs sources without a top-level .app.src, and generating one
    // creates conflicts with the package's own .app file.
    config.push_str("{erl_opts, [debug_info]}.\n");

    // [native.overrides] → {overrides, [{override, Dep, [{erl_opts, [...]}]}]}.
    // Names and erl_opts are validated as plain atoms by the manifest parser.
    let override_entries: Vec<String> = overrides
        .iter()
        .map(|(name, native_override)| {
            format!(
                "    {{override, {name}, [{{erl_opts, [{}]}}]}}",
                native_override.erl_opts.join(", ")
            )
        })
        .collect();
    if !override_entries.is_empty() {
        config.push_str("{overrides, [\n");
        config.push_str(&override_entries.join(",\n"));
        config.push_str("\n]}.\n");
    }

    // Test-only: point the default `hexpm` repo at a local, unsigned hex
    // repository so end-to-end tests run offline (BT-4582).
    if let Ok(url) = std::env::var(TEST_HEX_REPO_URL_ENV) {
        if !url.is_empty() {
            use crate::beam_compiler::escape_erlang_string;
            let _ = writeln!(
                config,
                "{{hex, [{{repos, [#{{name => <<\"hexpm\">>, repo_url => <<\"{}\">>, \
                 repo_verify => false, repo_verify_origin => false}}]}}]}}.",
                escape_erlang_string(&url)
            );
        }
    }

    debug!(path = %config_path, "Writing rebar.config");
    fs::write(&config_path, &config)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write rebar.config at '{config_path}'"))?;

    Ok(config_path)
}

/// Invoke `rebar3 compile` to fetch and compile hex dependencies
/// (and optionally the package's own `native/*.erl` files).
///
/// ADR 0072 §4 Path B: rebar3 runs from `_build/dev/native/` where the
/// generated `rebar.config` lives. `REBAR_BASE_DIR` is set to the same
/// directory so all output stays in the build tree. `{src_dirs}` and
/// `{include_dirs}` use relative paths back to the project root.
///
/// ADR 0072 §5 (Phase 2): After rebar3 resolves and compiles, the resolved
/// versions from `rebar.lock` are captured in `beamtalk.lock` as
/// `[[native_package]]` entries. On subsequent builds, exact pinned
/// versions from the lockfile are used instead of constraints.
#[instrument(skip_all, fields(project_root = %project_root))]
fn compile_with_rebar3(
    project_root: &Utf8Path,
    native_deps: &NativeDependencyMap,
    overrides: &NativeOverrideMap,
    force_resolve: bool,
    resolved_bt_deps: &[crate::commands::deps::path::ResolvedDependency],
) -> Result<Rebar3Result> {
    use crate::commands::deps::lockfile::{self, Lockfile};

    let rebar3 = rebar3_path()?;

    // Serialise native builds of this project: preparing the lock deletes
    // checkouts another build (CLI alongside the LSP, say) may be compiling.
    let _native_build_lock = crate::commands::dir_lock::lock_dir(
        &BuildLayout::new(project_root).native_dir(),
        "native build",
    )?;

    let existing_lock = Lockfile::read(project_root)?;

    // Generate the rebar.config in _build/dev/native/
    let config_path = generate_rebar_config(project_root, native_deps, overrides)?;

    let rebar_layout = BuildLayout::new(project_root);
    let rebar_base_dir = rebar_layout.native_dir();

    // Align rebar3's rebar.lock and checkouts with beamtalk.lock.
    let re_resolving = prepare_rebar_lock(
        &rebar_layout,
        native_deps,
        existing_lock.as_ref().map(|lock| &lock.native_packages),
        force_resolve,
    )?;

    info!(
        rebar3 = %rebar3.display(),
        config = %config_path,
        base_dir = %rebar_base_dir,
        deps = native_deps.len(),
        "Invoking rebar3 compile"
    );

    // REBAR_BASE_DIR must be absolute — rebar3 resolves it relative to cwd,
    // and cwd is already rebar_base_dir, so a relative path would nest.
    let abs_base_dir = std::fs::canonicalize(rebar_base_dir.as_std_path())
        .into_diagnostic()
        .wrap_err("Failed to canonicalize rebar base dir")?;
    let rebar3_command = |subcommand: &str| {
        let mut cmd = std::process::Command::new(&rebar3);
        cmd.arg(subcommand);
        // rebar3 reads rebar.config from cwd — run from _build/dev/native/
        cmd.current_dir(rebar_base_dir.as_std_path());
        // Keep all rebar3 output in the same build directory.
        cmd.env("REBAR_BASE_DIR", &abs_base_dir);
        cmd
    };

    // rebar3 resolves a constraint from its cached package index and only
    // refreshes it on a miss, so a re-resolve (`beamtalk deps update`, a
    // changed constraint) would otherwise keep landing on the version it
    // cached at the previous build. Best effort: with no network rebar3
    // still compiles from its cache.
    if re_resolving {
        match rebar3_command("update").output() {
            Ok(out) if out.status.success() => debug!("Refreshed rebar3 package index"),
            Ok(out) => debug!(
                status = %out.status,
                "rebar3 update failed; resolving from the cached package index"
            ),
            Err(error) => debug!(%error, "rebar3 update could not run"),
        }
    }

    let output = rebar3_command("compile")
        .output()
        .into_diagnostic()
        .wrap_err_with(|| {
            format!(
                "Failed to run rebar3 compile (tried '{}').\nIs Erlang/OTP installed?",
                rebar3.display()
            )
        })?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        let combined = format!("{stdout}{stderr}");
        // Surface rebar3 errors with context
        miette::bail!("rebar3 compile failed:\n{}", combined.trim_end());
    }

    // Collect all ebin directories produced by rebar3
    let ebin_paths = collect_rebar3_ebin_paths(&rebar_layout);

    info!(ebin_count = ebin_paths.len(), "rebar3 compilation complete");

    // ADR 0072 Phase 2: Extract resolved versions from rebar.lock and
    // write them to beamtalk.lock as [[native_package]] entries.
    let resolved_native = lockfile::read_rebar_lock(&rebar_base_dir)?;
    if !resolved_native.is_empty() {
        let mut lockfile = existing_lock.unwrap_or_default();
        // Replace all native entries with the freshly resolved set
        lockfile.clear_native_packages();
        for mut pkg in resolved_native {
            record_manifest_reference(&mut pkg, native_deps);
            info!(
                name = %pkg.name,
                source = ?pkg.source,
                "Locking native package"
            );
            lockfile.insert_native(pkg);
        }
        lockfile.write(project_root)?;
        debug!("Updated beamtalk.lock with native package entries");
    }

    // Discover native module names from the project's native/ directory.
    // Only root package modules go into `module_names`; dependency native
    // modules are tracked separately so the root `.app` doesn't claim
    // ownership of modules that belong to dependencies.
    let module_names = {
        use crate::beam_compiler::discover_native_modules;
        discover_native_modules(project_root)?
    };

    // Compile native/*.erl files via erlc (root package + transitive deps).
    // rebar3 only handles hex deps; native Erlang sources need separate
    // compilation because rebar3 won't compile src_dirs without a top-level
    // .app.src, and generating one conflicts with the package's own .app.
    let native_ebin = compile_native_erlang_with_deps(project_root, resolved_bt_deps)?;
    let mut all_ebin_paths = ebin_paths;
    if let Some(ebin) = native_ebin {
        all_ebin_paths.push(ebin);
    }

    Ok(Rebar3Result {
        ebin_paths: all_ebin_paths,
        module_names,
    })
}

/// Compile native `.erl` files from the root package and transitive BT
/// dependencies via `erlc`, after rebar3 has compiled hex deps.
///
/// Include resolution:
/// - `ERL_LIBS` → rebar3 lib dir (for hex dep headers like cowboy, gun)
/// - `-I` → beamtalk runtime apps dir (for `beamtalk_runtime` headers)
///
/// Outputs `.beam` files to `_build/dev/native/ebin/`.
/// Returns the ebin path if any files were compiled.
pub(crate) fn compile_native_erlang_with_deps(
    project_root: &Utf8Path,
    resolved_bt_deps: &[crate::commands::deps::path::ResolvedDependency],
) -> Result<Option<Utf8PathBuf>> {
    // Collect all native/*.erl files from root + transitive deps
    let mut erl_files: Vec<Utf8PathBuf> = Vec::new();
    let mut include_dirs: Vec<Utf8PathBuf> = Vec::new();

    // Root package
    let native_dir = project_root.join("native");
    if native_dir.exists() && native_dir.is_dir() {
        collect_erl_files(&native_dir, &mut erl_files)?;
        let include_dir = native_dir.join("include");
        if include_dir.exists() && include_dir.is_dir() {
            include_dirs.push(include_dir);
        }
    }

    // Transitive BT dependencies
    for dep in resolved_bt_deps {
        let dep_native = dep.root.join("native");
        if dep_native.exists() && dep_native.is_dir() {
            collect_erl_files(&dep_native, &mut erl_files)?;
            let dep_include = dep_native.join("include");
            if dep_include.exists() && dep_include.is_dir() {
                include_dirs.push(dep_include);
            }
        }
    }

    if erl_files.is_empty() {
        return Ok(None);
    }

    // Sort for deterministic compilation order across platforms.
    erl_files.sort();

    // Create output directory
    let build_layout = BuildLayout::new(project_root);
    let ebin_dir = build_layout.native_ebin_dir();
    fs::create_dir_all(&ebin_dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create native ebin directory '{ebin_dir}'"))?;

    let mut invocation = beamtalk_cli::erlc::ErlcInvocation::new(&ebin_dir)
        .debug_info()
        .docs()
        .erl_libs(&build_layout.rebar_lib_dir())
        .runtime_include()
        // Generated include dir for beamtalk_classes.hrl
        .include_dir(build_layout.native_include_dir());

    for inc in &include_dirs {
        invocation = invocation.include_dir(inc);
    }

    invocation = invocation.source_files(&erl_files);

    info!(
        count = erl_files.len(),
        ebin = %ebin_dir,
        "Compiling native Erlang files via erlc"
    );

    invocation.run("Native Erlang compilation")?;

    // User-visible output confirming native Erlang compilation.
    let count = erl_files.len();
    let plural = if count == 1 { "" } else { "s" };
    eprintln!("Compiled {count} native Erlang file{plural}");

    Ok(Some(ebin_dir))
}

/// Collect `.erl` files from a directory (non-recursive).
pub(crate) fn collect_erl_files(dir: &Utf8Path, out: &mut Vec<Utf8PathBuf>) -> Result<()> {
    let entries = fs::read_dir(dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read directory '{dir}'"))?;
    for entry in entries {
        let entry = entry.into_diagnostic()?;
        let path = entry.path();
        if path.is_file() && path.extension().is_some_and(|ext| ext == "erl") {
            let utf8 = Utf8PathBuf::from_path_buf(path)
                .map_err(|p| miette::miette!("Non-UTF-8 path: {}", p.display()))?;
            out.push(utf8);
        }
    }
    Ok(())
}

/// Collect ebin directories from rebar3's output tree.
///
/// rebar3 places compiled output at `_build/dev/native/default/lib/{app}/ebin/`.
/// This scans for all such directories and returns them.
///
/// Uses [`BuildLayout::rebar_lib_dir`] as the source of truth for the library
/// directory location.
pub(crate) fn collect_rebar3_ebin_paths(layout: &BuildLayout) -> Vec<Utf8PathBuf> {
    let lib_dir = layout.rebar_lib_dir();

    let mut paths = Vec::new();

    if !lib_dir.exists() {
        return paths;
    }

    let Ok(entries) = fs::read_dir(&lib_dir) else {
        return paths;
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }
        let ebin = path.join("ebin");
        if ebin.exists() && ebin.is_dir() {
            if let Ok(utf8) = Utf8PathBuf::from_path_buf(ebin) {
                debug!(ebin = %utf8, "Found rebar3 ebin directory");
                paths.push(utf8);
            }
        }
    }

    paths.sort();
    paths
}

/// ADR 0072 Phase 1: Check for native Erlang module name collisions across
/// packages in the dependency graph.
///
/// BEAM has a flat module namespace — only one version of any module can be
/// loaded at runtime. If two packages define the same native module name
/// (e.g., both have `native/utils.erl`), the last one loaded silently wins,
/// causing subtle runtime breakage. This check runs before compilation to
/// fail fast with a clear error naming both packages and the conflicting module.
pub(crate) fn check_native_module_collisions(
    project_root: &Utf8Path,
    root_package_name: &str,
    resolved_deps: &[crate::commands::deps::path::ResolvedDependency],
) -> Result<()> {
    use crate::beam_compiler::discover_native_modules;

    // Collect native modules from root package
    let root_modules = discover_native_modules(project_root)?;

    // Collect native modules from each dependency
    let mut module_owners: HashMap<String, Vec<String>> = HashMap::new();

    for module in &root_modules {
        module_owners
            .entry(module.clone())
            .or_default()
            .push(root_package_name.to_string());
    }

    for dep in resolved_deps {
        let dep_modules = discover_native_modules(&dep.root)?;
        for module in &dep_modules {
            module_owners
                .entry(module.clone())
                .or_default()
                .push(dep.name.clone());
        }
    }

    // Find collisions: modules owned by more than one package
    let mut collisions: Vec<(String, Vec<String>)> = module_owners
        .into_iter()
        .filter(|(_, owners)| owners.len() > 1)
        .collect();
    collisions.sort_by(|(a, _), (b, _)| a.cmp(b));

    if collisions.is_empty() {
        return Ok(());
    }

    // Build a clear error message listing all collisions
    let mut msg = String::from(
        "Native Erlang module name collision detected.\n\
         BEAM has a flat module namespace — only one version of any module can be loaded.\n\n",
    );

    for (module, owners) in &collisions {
        let _ = writeln!(
            msg,
            "  Module '{}' is defined by: {}",
            module,
            owners.join(", ")
        );
    }

    msg.push_str(
        "\nRename the conflicting module(s) to avoid silent runtime breakage. \
         Convention: prefix native modules with the package name (e.g., 'mypackage_utils').",
    );

    miette::bail!("{msg}")
}
