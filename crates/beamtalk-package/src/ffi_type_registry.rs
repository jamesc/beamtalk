// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0075's FFI type resolution chain: auto-extracted `-spec`s, overridden
//! per function/arity by the distribution, package-bundled and project-local
//! stub tiers.
//!
//! **DDD Context:** Compilation (CLI glue)
//!
//! Lives in the lib target so `beamtalk build`/`lint`/`check`/`test` and the
//! MCP `lint`/`diagnostic_summary` tools build the registry with the same
//! functions (BT-4643). Everything here is quiet: each stub file's
//! diagnostics and each sibling-package collision come back as
//! [`StubReport`]s, which the CLI renders in its own output format and MCP
//! returns as structured warnings.

use std::collections::HashMap;

use beamtalk_core::compilation::ffi_stubs::StubFileLoad;
use beamtalk_core::semantic_analysis::type_checker::{NativeTypeRegistry, TypeProvenance};
use beamtalk_core::source_analysis::{Diagnostic, Span};
use camino::{Utf8Path, Utf8PathBuf};
use tracing::warn;

/// A dependency's own package-bundled stub directory (ADR 0075 layer 2),
/// from its `beamtalk.toml` `[stubs] path`.
#[derive(Debug, Clone)]
pub struct DependencyStubsDir {
    /// The dependency's package name, for collision diagnostics.
    pub package: String,
    /// Its stub directory.
    pub stubs_dir: Utf8PathBuf,
}

/// One source of stub-tier diagnostics, in discovery order.
#[derive(Debug)]
pub enum StubReport {
    /// A stub file's own diagnostics (parse errors, skipped signatures,
    /// version drift), with the source to render them against.
    File(StubFileLoad),
    /// Two sibling dependencies' package-bundled stubs declaring the same
    /// `(module, function, arity)`. The two declarations live in different
    /// files, so there is no one source to render it against; `package` is
    /// the later dependency, whose stub wins.
    Collision {
        /// The dependency whose stub silently wins.
        package: String,
        /// The warning.
        diagnostic: Diagnostic,
    },
}

impl StubReport {
    /// This report's diagnostics.
    #[must_use]
    pub fn diagnostics(&self) -> &[Diagnostic] {
        match self {
            Self::File(file) => &file.diagnostics,
            Self::Collision { diagnostic, .. } => std::slice::from_ref(diagnostic),
        }
    }
}

/// The result of [`resolve_package_type_registry`].
#[derive(Debug, Default)]
pub struct ResolvedTypeRegistry {
    /// The merged FFI type registry, or `None` when neither auto-extraction
    /// nor any stub tier contributed anything.
    pub registry: Option<NativeTypeRegistry>,
    /// The stub tiers' diagnostics, per stub file or collision, in merge
    /// order.
    pub reports: Vec<StubReport>,
    /// Content hash of every stub file read by every tier, in merge order
    /// (empty when no tier had any stub file). `beamtalk build` folds it into
    /// each source file's incremental cache key (BT-4393), so adding, editing
    /// or removing a stub recompiles the package instead of replaying
    /// diagnostics computed against the old stubs.
    pub stub_inputs_hash: String,
}

impl ResolvedTypeRegistry {
    /// Every stub-tier diagnostic, in merge order.
    pub fn diagnostics(&self) -> impl Iterator<Item = &Diagnostic> {
        self.reports.iter().flat_map(StubReport::diagnostics)
    }
}

/// One or more stub-tier directories merged into a single registry, plus
/// their reports and a fingerprint of the stub files they were read from.
#[derive(Debug)]
pub struct StubLayer {
    /// The merged declarations, later files/tiers winning per function/arity.
    pub registry: NativeTypeRegistry,
    /// Every stub file's diagnostics and every collision, in merge order.
    pub reports: Vec<StubReport>,
    /// One `path` + content-hash line per stub file read, in merge order —
    /// the inputs [`ResolvedTypeRegistry::stub_inputs_hash`] is computed from.
    pub inputs: Vec<String>,
}

impl StubLayer {
    fn empty() -> Self {
        Self {
            registry: NativeTypeRegistry::new(),
            reports: Vec::new(),
            inputs: Vec::new(),
        }
    }

    /// Apply `other` on top of `self` (its declarations win on overlap).
    fn absorb(&mut self, other: StubLayer) {
        self.registry.apply_overrides(other.registry);
        self.reports.extend(other.reports);
        self.inputs.extend(other.inputs);
    }

    /// Every diagnostic in this layer, in merge order.
    pub fn diagnostics(&self) -> impl Iterator<Item = &Diagnostic> {
        self.reports.iter().flat_map(StubReport::diagnostics)
    }
}

/// ADR 0075's full FFI type resolution chain for a manifest-backed package
/// rooted at `package_root`: each stub tier overrides `auto_extract` at the
/// function/arity level, applied lowest-precedence first so project-local
/// always wins — extracted -> distribution -> package-bundled (each of
/// `dependency_stubs`) -> project-local (`package_root/stubs/`). Every tier
/// is checked for drift against the pre-merge `auto_extract`.
///
/// The one merge `beamtalk build`, `beamtalk lint`, `beamtalk test`, a
/// dependency's compile for a downstream package (BT-4225) and MCP `lint`
/// (BT-4643) all share, so a package's FFI calls type the same on every
/// surface and whether it is built standalone or as someone else's
/// dependency.
#[must_use]
pub fn resolve_package_type_registry(
    package_root: &Utf8Path,
    auto_extract: Option<NativeTypeRegistry>,
    dependency_stubs: &[DependencyStubsDir],
) -> ResolvedTypeRegistry {
    let dependency_layer = load_dependency_stub_registries(
        dependency_stubs,
        distribution_stubs_dir().as_deref(),
        auto_extract.as_ref(),
    );
    let project_layer = load_project_stub_registry(package_root, auto_extract.as_ref());
    if dependency_layer.is_none() && project_layer.is_none() {
        return ResolvedTypeRegistry {
            registry: auto_extract,
            ..ResolvedTypeRegistry::default()
        };
    }
    let mut merged = auto_extract.unwrap_or_default();
    let mut reports = Vec::new();
    let mut stub_inputs = Vec::new();
    for layer in [dependency_layer, project_layer].into_iter().flatten() {
        merged.apply_overrides(layer.registry);
        reports.extend(layer.reports);
        stub_inputs.extend(layer.inputs);
    }
    ResolvedTypeRegistry {
        registry: Some(merged),
        reports,
        stub_inputs_hash: stub_inputs_hash(&stub_inputs),
    }
}

/// The FFI type registry `beamtalk lint` and MCP `lint`/`diagnostic_summary`
/// analyse `source_files` against (BT-4643).
///
/// In a package (`package_root`), the full resolution chain
/// ([`resolve_package_type_registry`]) over the auto-extracted specs
/// ([`crate::native_type_specs::extract_project_type_specs`], the extractor
/// `beamtalk build` uses), with `dependency_stubs` as the package-bundled
/// tier. Stdlib has no `beamtalk.toml`, so for a target under `stdlib/src`
/// the registry is `beamtalk build-stdlib`'s own
/// ([`crate::native_type_specs::extract_stdlib_type_specs`]) — without it
/// every `(Erlang m) f:` call in `stdlib/src` would be reported as untyped.
/// Otherwise empty.
#[must_use]
pub fn lint_type_registry(
    package_root: Option<&Utf8Path>,
    source_files: &[Utf8PathBuf],
    dependency_stubs: &[DependencyStubsDir],
) -> ResolvedTypeRegistry {
    if let Some(root) = package_root {
        let layout = crate::build_layout::BuildLayout::new(root);
        let auto_extract = crate::native_type_specs::extract_project_type_specs(&layout);
        resolve_package_type_registry(root, auto_extract, dependency_stubs)
    } else if source_files
        .iter()
        .any(|f| beamtalk_project::package::is_under_stdlib_src_dir(f.as_std_path()))
    {
        ResolvedTypeRegistry {
            registry: crate::native_type_specs::extract_stdlib_type_specs(),
            ..ResolvedTypeRegistry::default()
        }
    } else {
        ResolvedTypeRegistry::default()
    }
}

/// Hash of the stub-file fingerprint lines (see [`StubLayer::inputs`]);
/// empty when there are none, so a project without stubs keeps its cache
/// keys unchanged.
fn stub_inputs_hash(inputs: &[String]) -> String {
    if inputs.is_empty() {
        return String::new();
    }
    crate::content_hash::sha256_hex(inputs.join("\n").as_bytes())
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
/// curated distribution stub content is added; the discovery/merge
/// plumbing is still correct with nothing to find.
#[must_use]
pub fn distribution_stubs_dir() -> Option<Utf8PathBuf> {
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

/// ADR 0075 Phase 2: scan `project_root/stubs/` for `declare native:` stub
/// files and build the project-local stub-tier registry, plus its reports
/// (skipped-signature and version-drift warnings against `auto_extract`).
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
#[must_use]
pub fn load_project_stub_registry(
    project_root: &Utf8Path,
    auto_extract: Option<&NativeTypeRegistry>,
) -> Option<StubLayer> {
    load_stub_registry_from_dir(
        &project_root.join(beamtalk_core::compilation::ffi_stubs::PROJECT_STUBS_DIR),
        auto_extract,
    )
}

/// Load and merge the package-bundled (ADR 0075 layer 2) and distribution
/// (layer 3) stub tiers, ranked in that precedence order: distribution
/// applied first, then each dependency's own bundled stubs on top —
/// matching the resolution chain's "project > package > distribution >
/// auto-extracted" order (project-local stubs are merged separately, last,
/// by [`load_project_stub_registry`]'s caller).
///
/// A dependency's `stubs_dir` covers only that package's own native code —
/// never its own dependencies' stubs, since those are auto-extracted
/// instead. Because each package is expected to stub only its own native
/// modules, two sibling dependencies declaring the same `(module, function,
/// arity)` is unexpected — it is reported as a [`StubReport::Collision`]
/// (naming both packages) rather than letting the later dependency in
/// iteration order silently win, mirroring how
/// `check_native_module_collisions` surfaces a native-module-implementation
/// collision across dependencies.
///
/// `distribution_stubs_dir` is the compiler distribution's own `stubs/`
/// directory (layer 3), when one is discoverable. Distribution-vs-package
/// overlap is not flagged: package stubs are *meant* to override
/// distribution stubs, per the resolution order.
///
/// Returns `None` only when none of these layers contributed anything.
#[must_use]
pub fn load_dependency_stub_registries(
    dependency_stubs: &[DependencyStubsDir],
    distribution_stubs_dir: Option<&Utf8Path>,
    auto_extract: Option<&NativeTypeRegistry>,
) -> Option<StubLayer> {
    let mut merged = StubLayer::empty();
    let mut found_any = false;

    if let Some(dist_dir) = distribution_stubs_dir
        && let Some(dist_layer) = load_stub_registry_from_dir(dist_dir, auto_extract)
    {
        merged.absorb(dist_layer);
        found_any = true;
    }

    // Tracks which dependency first declared each (module, function, arity)
    // package-bundled stub, so a sibling dependency re-declaring the same
    // signature is flagged rather than silently overriding it.
    let mut package_stub_owners: HashMap<(String, String, u8), String> = HashMap::new();

    for dep in dependency_stubs {
        if let Some(mut dep_layer) = load_stub_registry_from_dir(&dep.stubs_dir, auto_extract) {
            let collisions = package_stub_collisions(
                &dep_layer.registry,
                &dep.package,
                &mut package_stub_owners,
            );
            dep_layer
                .reports
                .extend(
                    collisions
                        .into_iter()
                        .map(|diagnostic| StubReport::Collision {
                            package: dep.package.clone(),
                            diagnostic,
                        }),
                );
            merged.absorb(dep_layer);
            found_any = true;
        }
    }

    found_any.then_some(merged)
}

/// Records every `(module, function, arity)` in `dep_registry` as owned by
/// `dep_name` in `owners`, returning a warning diagnostic for each one
/// already owned by a *different* package — a same-module stub collision
/// between two sibling dependencies' own package-bundled stubs (see
/// [`load_dependency_stub_registries`]).
fn package_stub_collisions(
    dep_registry: &NativeTypeRegistry,
    dep_name: &str,
    owners: &mut HashMap<(String, String, u8), String>,
) -> Vec<Diagnostic> {
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
                        TypeProvenance::Declared(s) => s,
                        _ => Span::default(),
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
                    collisions.push(Diagnostic::warning(message, span));
                }
                _ => {}
            }
        }
    }

    collisions
}

/// Load one stub-tier directory with the shared core loader
/// ([`beamtalk_core::compilation::ffi_stubs::load_stub_registry_from_dir`],
/// also used by the compiler port's project scope), keeping each file as a
/// [`StubReport::File`]. Shared by every stub-resolution-chain layer
/// (project-local, package-bundled, distribution) — each layer is just a
/// different directory.
///
/// Returns `None` if `stubs_dir` does not exist or contains no `.bt` files.
fn load_stub_registry_from_dir(
    stubs_dir: &Utf8Path,
    auto_extract: Option<&NativeTypeRegistry>,
) -> Option<StubLayer> {
    let load = beamtalk_core::compilation::ffi_stubs::load_stub_registry_from_dir(
        stubs_dir,
        auto_extract,
    )?;
    let inputs = load
        .files
        .iter()
        .map(|file| {
            format!(
                "{}\0{}",
                file.path,
                crate::content_hash::sha256_hex(file.source.as_bytes())
            )
        })
        .collect();
    Some(StubLayer {
        registry: load.registry,
        reports: load.files.into_iter().map(StubReport::File).collect(),
        inputs,
    })
}
