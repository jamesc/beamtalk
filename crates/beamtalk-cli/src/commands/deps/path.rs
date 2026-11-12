// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Path dependency resolution for the Beamtalk package system (ADR 0070 Phase 1).
//!
//! **DDD Context:** Build System
//!
//! Resolves local path dependencies declared in `beamtalk.toml`:
//! - Resolves relative paths from the depending package's directory
//! - Locates and validates the dependency's `beamtalk.toml`
//! - Compiles the dependency and places output in `_build/deps/{name}/ebin/`
//! - Detects circular dependencies
//! - Returns ebin paths for BEAM code path setup

use beamtalk_cli::path_util::normalize_path;
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::collections::HashMap;
use tracing::{debug, info};

use crate::commands::build_layout::BuildLayout;
use crate::commands::manifest;

/// A resolved dependency with its compiled ebin path and class index.
#[derive(Debug)]
pub struct ResolvedDependency {
    /// The dependency's package name.
    pub name: String,
    /// The absolute path to the dependency's root directory.
    pub root: Utf8PathBuf,
    /// The path to the dependency's compiled ebin directory.
    pub ebin_path: Utf8PathBuf,
    /// Maps class names to compiled BEAM module names for this dependency.
    /// E.g., `"Helper"` -> `"bt@utils@helper"`.
    pub class_module_index: HashMap<String, String>,
    /// Full class metadata from the dependency's source files.
    ///
    /// Used by the type checker and structural validator to resolve cross-package
    /// class references (superclass chains, method signatures, etc.).
    /// Empty when metadata is unavailable (e.g., compiled-only dependencies).
    pub class_infos: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    /// Full protocol metadata from the dependency's source files.
    ///
    /// Lets a consumer's `extending:`/conformance checks resolve protocol
    /// names exported by this dependency. Protocols have no `internal`
    /// modifier at the AST level, so every declared protocol is exported —
    /// unlike `alias_infos`, there is no seeding-boundary filter to apply.
    pub protocol_infos: Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo>,
    /// Full ASTs of carried protocols
    /// (`trait_expansion::is_carried_protocol`) from this dependency's
    /// source files (ADR 0127 §10a; BT-3591) — the trait-flattening
    /// counterpart to `protocol_infos` above. Lets a consumer's cross-package
    /// `uses: <this dep>@Name` flatten the protocol's provided methods,
    /// rather than reporting "no source available". Empty when this
    /// dependency's source is unavailable (compiled-only, not fetched) or
    /// defines no carried protocol.
    pub protocol_defs: Vec<beamtalk_core::ast::ProtocolDefinition>,
    /// Source identity of each protocol in `protocol_defs` — the dependency
    /// checkout file it was parsed from and this dependency's package name
    /// (ADR 0127 §3). Lets a flattened dependency provision map its lines to
    /// the protocol's own file (not the using file) and resolve its names in
    /// this package.
    pub protocol_sources: beamtalk_core::semantic_analysis::ProtocolSourceMap,
    /// Full type-alias metadata from the dependency's source files,
    /// each stamped with `.package = Some(name)`.
    ///
    /// Lets a consumer's `:: Alias` annotations resolve type aliases exported
    /// by this dependency. `AliasRegistry::add_pre_loaded`'s existing
    /// seeding-boundary filter uses the stamped `package` to exclude this
    /// dependency's `internal type` declarations while still seeding its
    /// public aliases (ADR 0108 Phase 5).
    pub alias_infos: Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo>,
    /// Whether this is a direct dependency of the root package (ADR 0070 Phase 3).
    /// `false` means it is a transitive dependency.
    pub is_direct: bool,
    /// For transitive dependencies, the chain of packages through which this
    /// dep is reached. E.g., `["json"]` if the root depends on `json` which
    /// depends on this package. Empty for direct dependencies.
    pub via_chain: Vec<String>,
    /// This dependency's own package-bundled FFI type stubs directory (ADR
    /// 0075 layer 2), resolved from its `beamtalk.toml` `[stubs] path` —
    /// `None` if it declares no `[stubs]` section. Covers only this
    /// package's own native code, never its own dependencies' stubs.
    pub stubs_dir: Option<Utf8PathBuf>,
    /// The extension methods this dependency's source defines, and whether
    /// that is all of them (ADR 0070 WS3, BT-4608). Merged into the
    /// consumer's cross-file extension index so its type checker sees them;
    /// the pre-WS3 dependency guard lifts only when every dependency's is
    /// complete. The default is *unknown* (incomplete), which keeps the guard
    /// on.
    pub extensions: beamtalk_cli::dependency_extensions::DependencyExtensions,
}

/// Runs `f` over the dependencies in `resolved` that `dep_name` depends on,
/// directly or transitively ([`beamtalk_core::compilation::dep_order::dependency_closure`]
/// over `dependencies`), in their order in `resolved` — the `prior_deps` a
/// dependency compiles and indexes against (BT-4375).
///
/// `resolved` holds every dependency compiled so far, in compile order, which
/// includes unrelated siblings: their same-named classes would shadow the
/// ancestors this dependency actually inherits from. Those entries are set
/// aside for the call and restored, in the original order, afterwards.
pub(crate) fn with_dependency_closure<R>(
    resolved: &mut Vec<ResolvedDependency>,
    dependencies: &std::collections::BTreeMap<String, Vec<String>>,
    dep_name: &str,
    f: impl FnOnce(&[ResolvedDependency]) -> R,
) -> R {
    let closure = beamtalk_core::compilation::dep_order::dependency_closure(dependencies, dep_name);
    let in_closure: Vec<bool> = resolved
        .iter()
        .map(|dep| closure.contains(&dep.name))
        .collect();
    let (inside, outside): (Vec<_>, Vec<_>) = std::mem::take(resolved)
        .into_iter()
        .partition(|dep| closure.contains(&dep.name));
    let result = f(&inside);
    let mut inside = inside.into_iter();
    let mut outside = outside.into_iter();
    resolved.extend(in_closure.into_iter().filter_map(|is_inside| {
        if is_inside {
            inside.next()
        } else {
            outside.next()
        }
    }));
    result
}

/// Collect ebin paths for already-compiled path dependencies.
///
/// Scans `_build/deps/*/ebin/` for existing dependency ebin directories.
/// Used by commands (run, test) that need to add dependency ebin paths to the
/// BEAM code path without re-resolving/re-compiling.
///
/// Uses [`BuildLayout::deps_dir`] as the source of truth for the dependency
/// directory location.
pub fn collect_dep_ebin_paths(layout: &BuildLayout) -> Vec<Utf8PathBuf> {
    let deps_dir = layout.deps_dir();
    let mut paths = Vec::new();

    if let Ok(entries) = std::fs::read_dir(&deps_dir) {
        for entry in entries.flatten() {
            let ebin = entry.path().join("ebin");
            if ebin.is_dir() {
                if let Ok(utf8) = Utf8PathBuf::from_path_buf(ebin) {
                    paths.push(utf8);
                }
            }
        }
    }

    paths.sort();
    paths
}

/// Resolve a relative dependency path to an absolute path.
///
/// Joins the relative path with the project root and canonicalizes the result.
/// Does not require the target to exist (unlike `std::fs::canonicalize`).
pub(crate) fn canonicalize_dep_path(
    project_root: &Utf8Path,
    relative_path: &Utf8Path,
) -> Utf8PathBuf {
    let joined = project_root.join(relative_path);

    // Normalize the path by resolving `.` and `..` components
    normalize_path(&joined)
}

/// Compile a dependency and return a `ResolvedDependency`.
///
/// This is used by the topological graph resolver (`graph.rs`) which has already
/// resolved prior dependencies. Their class module indexes are merged so that
/// cross-dependency class references resolve during compilation.
///
/// Output goes to `{project_root}/_build/deps/{name}/ebin/`.
pub(crate) fn compile_dependency_at(
    project_root: &Utf8Path,
    dep_root: &Utf8Path,
    dep_name: &str,
    options: &beamtalk_core::CompilerOptions,
    prior_deps: &[ResolvedDependency],
    otp_specs: &OtpTypeSpecs,
) -> Result<ResolvedDependency> {
    let (ebin_path, class_module_index) = compile_dependency_with_context(
        project_root,
        dep_root,
        dep_name,
        options,
        prior_deps,
        otp_specs,
    )?;

    let (_, class_infos, protocol_infos, protocol_defs, alias_infos, protocol_sources, extensions) =
        build_dep_class_index(dep_root, dep_name, prior_deps).unwrap_or_default();

    let stubs_dir = manifest::parse_manifest_full(&dep_root.join("beamtalk.toml"))
        .ok()
        .and_then(|dep_manifest| dep_manifest.stubs_dir(dep_root));

    Ok(ResolvedDependency {
        name: dep_name.to_string(),
        root: dep_root.to_path_buf(),
        ebin_path,
        class_module_index,
        class_infos,
        protocol_infos,
        protocol_defs,
        protocol_sources,
        alias_infos,
        is_direct: false, // Caller sets this based on graph knowledge
        via_chain: Vec::new(),
        stubs_dir,
        extensions,
    })
}

/// Class-hierarchy inputs for compiling one dependency: its own Pass 1
/// indexes plus the full `ClassInfo`s of the dependencies it compiles
/// against (BT-4328) — only those it reaches in the graph (BT-4375).
struct DepCompileClassInputs {
    /// This dependency's own class → module index.
    own_class_module_index: HashMap<String, String>,
    class_superclass_index: HashMap<String, String>,
    /// This dependency's own classes, trait-flattened — what its corpus and
    /// `.app` file list.
    own_class_infos: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    /// `own_class_infos` followed by the exported classes of `prior_deps` —
    /// the hierarchy this dependency compiles against.
    all_class_infos: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    extension_index: beamtalk_core::compilation::extension_index::ExtensionIndex,
    cached_asts: HashMap<Utf8PathBuf, crate::commands::build::CachedAst>,
}

/// Builds [`DepCompileClassInputs`] for a dependency's `source_files`.
///
/// A dependency compiles as its own package, but its superclass chain can
/// reach into another package — without the ancestors' methods and
/// superclass links, the chain breaks at the first cross-package ancestor
/// and every hierarchy-driven codegen decision degrades
/// (`is_server_subclass`, `initialize`/`trap_exit` detection, direct-call
/// fast paths). The merge mirrors the root build's
/// `collect_all_class_infos(&[&source_class_infos, &dep_class_infos])`
/// (own first, dependencies after), so dep-compiled codegen matches a root
/// build of the same sources.
///
/// `prior_deps` must be this dependency's own transitive dependencies
/// ([`with_dependency_closure`]): the hierarchy is first-wins, so an
/// unrelated package's same-named class would otherwise shadow the real
/// ancestor (BT-4375).
fn dep_compile_class_inputs(
    source_files: &[Utf8PathBuf],
    source_root: Option<&Utf8Path>,
    dep_name: &str,
    prior_deps: &[ResolvedDependency],
) -> Result<DepCompileClassInputs> {
    let (
        own_class_module_index,
        class_superclass_index,
        mut own_class_infos,
        extension_index,
        cached_asts,
    ) = crate::commands::build::build_class_module_index(source_files, source_root, dep_name)?;
    // Cross-file trait provisions flattened into the infos this dep's own
    // compile and its dependents see (BT-3673), against its own protocols
    // first and then those of the dependencies it compiles against (BT-3678).
    crate::commands::build::flatten_package_class_infos(
        &mut own_class_infos,
        &cached_asts,
        prior_protocol_defs(prior_deps),
        dep_name,
    );
    // Prior dependencies contribute their exported `ClassInfo`s — already
    // flattened at their own export — so cross-package superclass chains
    // resolve during this dep's compilation exactly as in the root build.
    let mut dep_class_infos = Vec::new();
    for prior in prior_deps {
        dep_class_infos.extend(prior.class_infos.iter().cloned());
    }
    let all_class_infos =
        crate::commands::build::collect_all_class_infos(&[&own_class_infos, &dep_class_infos]);
    Ok(DepCompileClassInputs {
        own_class_module_index,
        class_superclass_index,
        own_class_infos,
        all_class_infos,
        extension_index,
        cached_asts,
    })
}

/// Compile a dependency with context from already-compiled dependencies.
///
/// The `prior_deps` parameter provides class module indexes from dependencies
/// that have already been compiled (in topological order), allowing this
/// dependency's source to reference classes from its own dependencies.
fn compile_dependency_with_context(
    project_root: &Utf8Path,
    dep_root: &Utf8Path,
    dep_name: &str,
    options: &beamtalk_core::CompilerOptions,
    prior_deps: &[ResolvedDependency],
    otp_specs: &OtpTypeSpecs,
) -> Result<(Utf8PathBuf, HashMap<String, String>)> {
    let ebin_path = BuildLayout::new(project_root).dep_ebin_dir(dep_name);
    let options = &dependency_compile_options(options, dep_name);

    // Create the ebin directory
    std::fs::create_dir_all(&ebin_path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create dependency ebin directory: {ebin_path}"))?;

    info!(dep = %dep_name, ebin = %ebin_path, "Compiling path dependency");

    // Find source files in the dependency. `stubs/` is excluded (ADR 0075)
    // — it's type-only and never compiled.
    let src_dir = dep_root.join("src");
    let source_files = beamtalk_core::file_walker::collect_project_source_files(dep_root)?;

    if source_files.is_empty() {
        debug!(dep = %dep_name, "No source files found in dependency, skipping compilation");
        return Ok((ebin_path, HashMap::new()));
    }

    info!(
        dep = %dep_name,
        count = source_files.len(),
        "Found source files in dependency"
    );

    // Build the class → module index for the dependency
    let source_root = if src_dir.exists() {
        Some(src_dir)
    } else {
        None
    };

    let DepCompileClassInputs {
        own_class_module_index,
        class_superclass_index,
        own_class_infos,
        all_class_infos,
        extension_index,
        cached_asts,
    } = dep_compile_class_inputs(&source_files, source_root.as_deref(), dep_name, prior_deps)?;

    // Build a compilation index that includes this dep's own classes plus
    // classes from the dependencies it compiles against, so cross-dependency
    // class references resolve during compilation.  The *own* index is kept
    // separate so we only export this dep's classes to dependents — not
    // transitive imports that happen to be compiled earlier.
    let mut class_module_index = own_class_module_index.clone();
    for prior in prior_deps {
        for (class_name, module_name) in &prior.class_module_index {
            class_module_index
                .entry(class_name.clone())
                .or_insert_with(|| module_name.clone());
        }
    }

    // Same-package cross-file type-alias resolution within the
    // dependency's own multi-file compilation — mirrors `all_class_infos`
    // immediately above. Exporting these aliases to *consumers* of this
    // dependency (cross-package alias resolution) is deferred as a
    // follow-up for this module.
    // Taken before `cached_asts` is handed to `compile_sources_to_core`.
    let mut dep_metadata = extract_dep_protocol_alias_metadata(&cached_asts, dep_name);
    dep_metadata.add_prior_dep_protocols(prior_deps);

    // Compile .bt sources to .core files
    let compile_ctx = crate::beam_compiler::CompileContext {
        hierarchy: crate::beam_compiler::ClassHierarchyContext {
            class_module_index: class_module_index.clone(),
            class_superclass_index: class_superclass_index.clone(),
            pre_loaded_classes: all_class_infos.clone(),
            // Same-package cross-file protocol resolution within the
            // dependency's own multi-file compile (BT-3673): a `uses:` of a
            // trait declared in a sibling file, or in one of the
            // dependency's own dependencies (BT-3678), must resolve and
            // flatten.
            pre_loaded_protocols: dep_metadata.protocol_infos,
            pre_loaded_protocol_defs: dep_metadata.protocol_defs,
            pre_loaded_protocol_sources: dep_metadata.protocol_sources,
            pre_loaded_aliases: dep_metadata.alias_infos,
            // The dep's own project-wide extensions — its files see
            // each other's extensions during its own compilation. Consumers
            // get them through `ResolvedDependency::extensions` (ADR 0070
            // WS3, BT-4608).
            extension_index: extension_index.clone(),
            // A dependency compiles under its consumer's options and does
            // not load *its* dependencies' extensions here, so the pre-WS3
            // dependency guard stays as the options set it.
            dependency_extensions_complete: false,
        },
        dep_registry: None, // No collision detection within dependency compilation
        strict_deps: false, // Dependencies use their own strict-deps setting, not root's
        native_type_registry: dependency_type_registry(
            project_root,
            dep_root,
            prior_deps,
            otp_specs,
        ),
        // ADR 0100 Rule 3: neither the root package's nor the
        // dependency's own `[diagnostics]` table is applied here — this path
        // doesn't load either manifest's `[diagnostics]` section, so
        // dependency compilation always sees an empty table (today's Rule 1
        // defaults). Unlike `strict_deps` (a deliberate "use the dep's own
        // setting, not root's" policy), this is a scoping gap, not a policy
        // choice: a dependency author's own severity overrides (e.g.
        // suppressing a known-noisy category) are silently ignored when that
        // package is compiled as someone else's dependency. Low practical
        // impact — dependency diagnostics are never surfaced to or promoted
        // for the consuming build — but worth revisiting if that changes.
        diagnostics_overrides: crate::commands::manifest::DiagnosticsTable::new(),
        provision_sink: crate::beam_compiler::ProvisionSink::default(),
    };

    let (core_files, module_names) = compile_sources_to_core(
        &source_files,
        source_root.as_deref(),
        dep_name,
        &ebin_path,
        options,
        &compile_ctx,
        cached_asts,
    )?;

    // Batch compile Core Erlang to BEAM
    let compiler = crate::beam_compiler::BeamCompiler::new(ebin_path.clone());
    compiler
        .compile_batch(&core_files)
        .wrap_err_with(|| format!("Failed to compile dependency '{dep_name}' to BEAM bytecode"))?;

    // Generate .app file and corpus
    generate_dependency_app_file(
        dep_root,
        dep_name,
        &ebin_path,
        &module_names,
        &DepAppClasses {
            own: &own_class_infos,
            hierarchy: &all_class_infos,
            own_class_module_index: &own_class_module_index,
        },
        &source_files,
    )?;

    // ADR 0098 Phase 2: stamp the dependency with the producing toolchain, LAST
    // (after its ebin, .app, and corpus have all landed). On the next resolve a
    // version/OTP mismatch marks this dep stale so it is rebuilt rather than
    // reused — the beamtalk-http stale-`_build` fix.
    crate::commands::build_stamp::write_stamp(
        &BuildLayout::new(project_root).dep_stamp_path(dep_name),
        crate::commands::build_stamp::current_otp_version(),
    );

    info!(dep = %dep_name, "Dependency compiled successfully");

    Ok((ebin_path, own_class_module_index))
}

/// The options a dependency compiles with, from the downstream build's.
///
/// It compiles as its own package, not the root's (BT-3915): its classes,
/// aliases and protocols are stamped `dep_name`, so its own `Comparable` is
/// the current package's (shadowing the stdlib's for a bare `uses:
/// Comparable`, as its exported `ClassInfo`s say) and its own `internal`
/// classes/aliases are same-package for E0401/E0402.
///
/// Its warnings stay its own (BT-4225), as cargo shows none for a
/// dependency: the downstream package can't act on them, and they bury its
/// own. Errors still fail the build and are shown, and so is an
/// `InternalVerifier` warning (a compiler bug, never suppressed). The
/// downstream `--warnings-as-errors` is capped the same way, like cargo's
/// `--cap-lints`: a dependency's warnings never fail someone else's build.
fn dependency_compile_options(
    options: &beamtalk_core::CompilerOptions,
    dep_name: &str,
) -> beamtalk_core::CompilerOptions {
    beamtalk_core::CompilerOptions {
        current_package: Some(dep_name.to_owned()),
        suppress_warnings: true,
        warnings_as_errors: false,
        ..options.clone()
    }
}

/// The OTP FFI type specs, extracted once per dependency resolution and
/// shared by every dependency it compiles (BT-4311), lazily, so a resolution
/// that compiles nothing extracts nothing.
#[derive(Default)]
pub(crate) struct OtpTypeSpecs(
    std::cell::OnceCell<Option<beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>>,
);

impl OtpTypeSpecs {
    fn get(
        &self,
        layout: &BuildLayout,
    ) -> Option<&beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry> {
        self.0
            .get_or_init(|| {
                beamtalk_core::ffi_type_specs::extract_type_specs(&layout.type_cache_dir(), &[])
            })
            .as_ref()
    }
}

/// The FFI type registry a dependency compiles against (BT-4225): the
/// resolution chain of its standalone build
/// ([`crate::commands::build::resolve_package_type_registry`] over its own
/// `stubs/`), over the OTP specs and those of the dependencies compiled
/// before it, cached in the downstream package's type cache. The
/// package-bundled stub tier comes from those same dependencies, a superset
/// of its own. Without it every OTP FFI call in the dependency inferred as
/// `Dynamic`, which its standalone build never does.
///
/// Only those `.beam` files, never the rest of `_build/` (BT-4311): a native
/// module left there by an earlier build would type a call the clean build
/// leaves `Dynamic`, so the same source would compile differently.
fn dependency_type_registry(
    project_root: &Utf8Path,
    dep_root: &Utf8Path,
    prior_deps: &[ResolvedDependency],
    otp_specs: &OtpTypeSpecs,
) -> Option<std::sync::Arc<beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>> {
    let layout = BuildLayout::new(project_root);
    let prior_ebins: Vec<Utf8PathBuf> = prior_deps.iter().map(|d| d.ebin_path.clone()).collect();
    let auto_extract = beamtalk_core::ffi_type_specs::extend_with_ebin_type_specs(
        otp_specs.get(&layout),
        &prior_ebins,
        &layout.type_cache_dir(),
    );
    crate::commands::build::resolve_package_type_registry(dep_root, auto_extract, prior_deps, None)
        .registry
        .map(std::sync::Arc::new)
}

/// Compile each source file in a dependency to Core Erlang.
///
/// Returns the list of `.core` file paths and corresponding BEAM module names.
fn compile_sources_to_core(
    source_files: &[Utf8PathBuf],
    source_root: Option<&Utf8Path>,
    dep_name: &str,
    ebin_path: &Utf8PathBuf,
    options: &beamtalk_core::CompilerOptions,
    compile_ctx: &crate::beam_compiler::CompileContext,
    mut cached_asts: HashMap<Utf8PathBuf, crate::commands::build::CachedAst>,
) -> Result<(Vec<Utf8PathBuf>, Vec<String>)> {
    let mut core_files = Vec::new();
    let mut module_names = Vec::new();

    for file in source_files {
        let stem = file
            .file_stem()
            .ok_or_else(|| miette::miette!("File '{}' has no name", file))?;

        // Validate module name
        if !stem.chars().all(|c| c == '_' || c.is_ascii_alphanumeric()) {
            miette::bail!(
                "Invalid module name '{}' in dependency '{dep_name}': \
                 must contain only alphanumeric characters and underscores",
                stem
            );
        }

        let relative_module = crate::commands::build::compute_relative_module(file, source_root)?;
        let module_name = format!("bt@{dep_name}@{relative_module}");
        let core_file = ebin_path.join(format!("{module_name}.core"));

        let cached = cached_asts.remove(file);
        crate::beam_compiler::compile_source_with_bindings(
            file,
            &module_name,
            &core_file,
            options,
            &beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable::new(),
            compile_ctx,
            cached,
        )?;

        core_files.push(core_file);
        module_names.push(module_name);
    }

    Ok((core_files, module_names))
}

/// The classes a compiled dependency's `.app` file and corpus describe.
struct DepAppClasses<'a> {
    /// This dependency's own classes — the only ones listed (BT-4375).
    own: &'a [beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo],
    /// `own` plus the classes it compiled against, so a class's kind
    /// resolves through a cross-package superclass chain.
    hierarchy: &'a [beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo],
    own_class_module_index: &'a HashMap<String, String>,
}

/// Generate the `.app` file and MCP corpus for a compiled dependency.
fn generate_dependency_app_file(
    dep_root: &Utf8Path,
    dep_name: &str,
    ebin_path: &Utf8PathBuf,
    module_names: &[String],
    classes: &DepAppClasses<'_>,
    source_files: &[Utf8PathBuf],
) -> Result<()> {
    let dep_manifest = manifest::parse_manifest(&dep_root.join("beamtalk.toml"))?;
    let class_metadata = crate::commands::build::build_class_metadata(
        classes.hierarchy,
        classes.own,
        classes.own_class_module_index,
        dep_name,
    );
    // ADR 0108 Phase 8: a path dependency is itself a fully-built
    // Beamtalk package, so its own `internal type` declarations must round-
    // trip into its `.app` file the same way the root package's do — the
    // seeding-boundary exclusion of a dependency's internal aliases happens
    // downstream, at browse time (`beamtalk_repl_ops_browse.erl`), not here.
    let alias_metadata = crate::commands::build::build_alias_metadata(source_files);
    // Read full manifest to get dependency names for {applications} list
    let full_manifest = manifest::find_manifest_full(dep_root)?;
    let dep_hex_dep_names: Vec<String> = full_manifest
        .as_ref()
        .map(|m| m.native_dependencies.keys().cloned().collect())
        .unwrap_or_default();
    // Use the manifest's declared BT dependencies (not the full prior_deps
    // topo-order prefix) so {applications} lists only direct dependencies.
    let dep_bt_dep_names: Vec<String> = full_manifest
        .as_ref()
        .map(|m| m.dependencies.keys().cloned().collect())
        .unwrap_or_default();
    crate::commands::app_file::generate_app_file(
        ebin_path,
        &dep_manifest,
        module_names,
        &class_metadata,
        None,
        // TODO(ADR 0072): Wire up native module discovery for path dependencies
        &[],
        &dep_bt_dep_names,
        &dep_hex_dep_names,
        &alias_metadata,
        full_manifest
            .as_ref()
            .and_then(|m| m.application.as_ref())
            .map(|a| a.supervisor.as_str()),
    )?;

    // Generate per-package corpus files for MCP discovery.
    // Place corpus alongside the dep's ebin dir (in _build/deps/{name}/).
    let corpus_dir = ebin_path.parent().unwrap_or(ebin_path);
    crate::commands::build::generate_package_corpus(
        corpus_dir,
        dep_name,
        classes.own,
        source_files,
    )?;

    Ok(())
}

/// Protocol and alias metadata of a dependency's source files, extracted from
/// the ASTs [`build_class_module_index`](crate::commands::build::build_class_module_index)
/// already parsed.
struct DepProtocolAliasMetadata {
    protocol_infos: Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo>,
    protocol_defs: Vec<beamtalk_core::ast::ProtocolDefinition>,
    alias_infos: Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo>,
    protocol_sources: beamtalk_core::semantic_analysis::ProtocolSourceMap,
}

impl DepProtocolAliasMetadata {
    /// Appends the protocols of `prior_deps` after this dependency's own, so
    /// a `uses: dep@Name` resolves against them (first definition wins, own
    /// before prior; BT-3678).
    fn add_prior_dep_protocols(&mut self, prior_deps: &[ResolvedDependency]) {
        for dep in prior_deps {
            self.protocol_infos
                .extend(dep.protocol_infos.iter().cloned());
            for (name, source) in &dep.protocol_sources {
                self.protocol_sources
                    .entry(name.clone())
                    .or_insert_with(|| source.clone());
            }
        }
        self.protocol_defs.extend(prior_protocol_defs(prior_deps));
    }
}

/// The carried protocol ASTs of `prior_deps`, in order — what a
/// dependency compiled after them may `uses:` (BT-3678).
fn prior_protocol_defs(
    prior_deps: &[ResolvedDependency],
) -> impl Iterator<Item = beamtalk_core::ast::ProtocolDefinition> + '_ {
    prior_deps
        .iter()
        .flat_map(|d| d.protocol_defs.iter().cloned())
}

/// Extracts [`DepProtocolAliasMetadata`] from `cached_asts`. Shared by the
/// dependency's fast-path index ([`build_dep_class_index`]) and its own
/// multi-file compile, so the cross-file `uses:` resolution is identical in
/// both (BT-3673).
fn extract_dep_protocol_alias_metadata(
    cached_asts: &HashMap<Utf8PathBuf, crate::commands::build::CachedAst>,
    dep_name: &str,
) -> DepProtocolAliasMetadata {
    // Iterate in a stable (sorted-by-path) order for deterministic output.
    let mut sorted_files: Vec<&Utf8PathBuf> = cached_asts.keys().collect();
    sorted_files.sort();

    let mut protocol_infos = Vec::new();
    let mut protocol_defs = Vec::new();
    let mut protocol_sources = beamtalk_core::semantic_analysis::ProtocolSourceMap::new();
    let mut alias_infos = Vec::new();
    for file in sorted_files {
        let module = &cached_asts[file].module;
        // Source identity of this dependency's provisions (ADR 0127 §3):
        // the checkout file their spans are offsets into, and the package
        // their free names resolve in.
        for protocol in module
            .protocols
            .iter()
            .filter(|p| beamtalk_core::semantic_analysis::trait_expansion::is_carried_protocol(p))
        {
            beamtalk_core::semantic_analysis::protocol_source::insert_protocol_source(
                &mut protocol_sources,
                &protocol.name.name,
                beamtalk_core::semantic_analysis::ProtocolSource {
                    path: Some(file.as_str().into()),
                    text: cached_asts[file].source.as_str().into(),
                    package: Some(dep_name.into()),
                },
            );
        }
        // Shared with the offline scan MCP lint uses (BT-4585).
        let (module_protocols, module_aliases) =
            beamtalk_cli::dependency_classes::dependency_protocol_alias_infos(module, dep_name);
        protocol_infos.extend(module_protocols);
        alias_infos.extend(module_aliases);
        // Full ASTs of carried protocols only (ADR 0127 §10a;
        // BT-3591) — see `ResolvedDependency::protocol_defs`'s doc — stamped
        // with this dependency so `uses: dep@Name` resolves to them (BT-3684).
        protocol_defs.extend(
            beamtalk_core::semantic_analysis::trait_expansion::carried_protocols(
                module,
                Some(dep_name),
            ),
        );
    }

    DepProtocolAliasMetadata {
        protocol_infos,
        protocol_defs,
        alias_infos,
        protocol_sources,
    }
}

/// A dependency's source scan before cross-file trait flattening: its class
/// indexes plus the trait users and protocols to flatten them with.
struct DepIndexScan {
    class_module_index: HashMap<String, String>,
    class_infos: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    trait_users: beamtalk_core::semantic_analysis::trait_expansion::TraitUserCollector,
    metadata: DepProtocolAliasMetadata,
    extensions: beamtalk_cli::dependency_extensions::DependencyExtensions,
    dep_name: String,
}

/// The exports of a [`DepIndexScan`], matching `ResolvedDependency`'s fields.
pub(crate) type DepIndexExports = (
    HashMap<String, String>,
    Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo>,
    Vec<beamtalk_core::ast::ProtocolDefinition>,
    Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo>,
    beamtalk_core::semantic_analysis::ProtocolSourceMap,
    beamtalk_cli::dependency_extensions::DependencyExtensions,
);

impl DepIndexScan {
    /// Flattens cross-file trait provisions (BT-3673) into the exported
    /// `ClassInfo`s, against the dependency's own protocols and then
    /// `extra_protocol_defs` — those of the dependencies it may `uses:`
    /// (BT-3678).
    fn into_exports(
        self,
        extra_protocol_defs: impl IntoIterator<Item = beamtalk_core::ast::ProtocolDefinition>,
    ) -> DepIndexExports {
        let Self {
            class_module_index,
            mut class_infos,
            trait_users,
            metadata,
            mut extensions,
            dep_name,
        } = self;
        trait_users.flatten(&mut class_infos, extra_protocol_defs, Some(&dep_name));
        // BT-4615: after flattening, which replaces trait-users' infos.
        let mut metadata = metadata;
        beamtalk_cli::dependency_extensions::expand_dependency_class_aliases(
            &metadata.alias_infos,
            &mut class_infos,
            &mut metadata.protocol_infos,
        );
        beamtalk_cli::dependency_extensions::expand_dependency_extension_aliases(
            &metadata.alias_infos,
            &mut extensions,
        );
        // BT-4707: the exported protocol ASTs too — flattened (above) against
        // the unexpanded ones in this dependency's own alias scope.
        beamtalk_cli::dependency_extensions::expand_dependency_protocol_defs(
            &metadata.alias_infos,
            &mut metadata.protocol_defs,
        );
        (
            class_module_index,
            class_infos,
            metadata.protocol_infos,
            metadata.protocol_defs,
            metadata.alias_infos,
            metadata.protocol_sources,
            extensions,
        )
    }
}

/// [`DepIndexScan::into_exports`], or empty exports for a dependency without
/// source files — whose extensions are complete: with no `.bt` source it
/// defines none at runtime either.
fn exports_or_empty(
    scan: Option<DepIndexScan>,
    extra_protocol_defs: impl IntoIterator<Item = beamtalk_core::ast::ProtocolDefinition>,
) -> DepIndexExports {
    scan.map_or_else(
        || {
            (
                HashMap::new(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
                beamtalk_core::semantic_analysis::ProtocolSourceMap::new(),
                beamtalk_cli::dependency_extensions::DependencyExtensions::scanning(),
            )
        },
        |scan| scan.into_exports(extra_protocol_defs),
    )
}

/// Scans a dependency's sources without compiling or flattening; see
/// [`build_dep_class_index`]. `Ok(None)` when it has no source files.
fn scan_dep_index(dep_root: &Utf8Path, dep_name: &str) -> Result<Option<DepIndexScan>> {
    // `stubs/` is excluded (ADR 0075) — it's type-only and never
    // compiled.
    let src_dir = dep_root.join("src");
    let source_files = beamtalk_core::file_walker::collect_project_source_files(dep_root)?;
    if source_files.is_empty() {
        return Ok(None);
    }

    let source_root = if src_dir.exists() {
        Some(src_dir)
    } else {
        None
    };

    let (class_module_index, _, class_infos, _, cached_asts) =
        crate::commands::build::build_class_module_index(
            &source_files,
            source_root.as_deref(),
            dep_name,
        )?;

    // ADR 0070 WS3 (BT-4608): the dependency's extensions, from the same
    // parsed ASTs. A file the index pass could not read is missing from
    // `cached_asts`, and leaves the result incomplete.
    let mut extensions = beamtalk_cli::dependency_extensions::DependencyExtensions::scanning();
    for file in &source_files {
        match cached_asts.get(file) {
            Some(ast) => extensions.add_file(&ast.module, file.as_std_path(), &ast.diagnostics),
            None => extensions.mark_unreadable(),
        }
    }

    Ok(Some(DepIndexScan {
        class_module_index,
        class_infos,
        trait_users: crate::commands::build::package_trait_users(&cached_asts, Some(dep_name)),
        metadata: extract_dep_protocol_alias_metadata(&cached_asts, dep_name),
        extensions,
        dep_name: dep_name.to_string(),
    }))
}

/// Build a class module index for a dependency without compiling.
///
/// Scans the dependency's source files and extracts class-to-module mappings,
/// plus protocol and type-alias metadata for cross-package
/// `extending:`/`:: Alias` resolution. This is the fast path used when deps
/// are fresh and don't need recompilation.
///
/// Protocol and alias extraction reuses `build_class_module_index`'s cached,
/// already-parsed ASTs rather than re-lexing/re-parsing the dependency's
/// source files a second and third time.
///
/// `prior_deps` are the dependencies this one may `uses:` traits of — those
/// compiled before it, in compile order; the exported `class_infos` carry their
/// provisions too (BT-3678). The graph compile and the fresh-deps fast path both
/// call this, so they export identical `class_infos` (BT-3684).
pub(crate) fn build_dep_class_index(
    dep_root: &Utf8Path,
    dep_name: &str,
    prior_deps: &[ResolvedDependency],
) -> Result<DepIndexExports> {
    Ok(exports_or_empty(
        scan_dep_index(dep_root, dep_name)?,
        prior_protocol_defs(prior_deps),
    ))
}

#[cfg(test)]
mod tests {
    use super::super::test_support::*;
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_canonicalize_dep_path() {
        let result = canonicalize_dep_path(
            Utf8Path::new("/home/user/my_app"),
            Utf8Path::new("../utils"),
        );
        assert_eq!(result, Utf8PathBuf::from("/home/user/utils"));
    }

    #[test]
    fn test_collect_dep_ebin_paths_empty() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&project_root);

        let paths = collect_dep_ebin_paths(&layout);
        assert!(paths.is_empty());
    }

    #[test]
    fn test_collect_dep_ebin_paths_finds_dirs() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        // Create _build/deps/utils/ebin/
        let test_layout = BuildLayout::new(&project_root);
        let ebin_dir = test_layout.dep_ebin_dir("utils");
        fs::create_dir_all(&ebin_dir).unwrap();

        let paths = collect_dep_ebin_paths(&test_layout);
        assert_eq!(paths.len(), 1);
        assert!(paths[0].as_str().contains("utils"));
    }

    /// ADR 0070 WS3 (BT-4608): a dependency's export carries its extension
    /// methods, complete when every source file parsed cleanly.
    #[test]
    fn build_dep_class_index_collects_dependency_extensions() {
        let temp = TempDir::new().unwrap();
        let dep_dir = temp.path().join("dep_ext");
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, "dep_ext", "0.1.0", "");
        write_source(
            &dep_dir,
            "ext.bt",
            "Object >> shout -> Integer => 1\nInteger class >> fromShout => 2\n",
        );
        let dep_root = Utf8PathBuf::from_path_buf(dep_dir).unwrap();

        let (_, _, _, _, _, _, extensions) =
            build_dep_class_index(&dep_root, "dep_ext", &[]).unwrap();
        assert!(extensions.is_complete());
        assert_eq!(extensions.index().len(), 2);

        write_source(dep_root.as_std_path(), "broken.bt", "Object >> ((( =>\n");
        let (_, _, _, _, _, _, extensions) =
            build_dep_class_index(&dep_root, "dep_ext", &[]).unwrap();
        assert!(
            !extensions.is_complete(),
            "a parse error may have dropped an extension"
        );
    }

    /// A dependency with no `.bt` source defines no extensions at runtime
    /// either, so its (empty) extensions are complete.
    #[test]
    fn build_dep_class_index_without_sources_has_complete_extensions() {
        let temp = TempDir::new().unwrap();
        let dep_dir = temp.path().join("dep_native");
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, "dep_native", "0.1.0", "");
        let dep_root = Utf8PathBuf::from_path_buf(dep_dir).unwrap();

        let (_, _, _, _, _, _, extensions) =
            build_dep_class_index(&dep_root, "dep_native", &[]).unwrap();
        assert!(extensions.is_complete());
        assert!(extensions.index().is_empty());
    }

    /// BT-3673: the `class_infos` a dependency exports to its consumers
    /// carry the provided methods of a trait declared in another of the
    /// dependency's own files (and the metadata its own multi-file compile
    /// pre-loads names that trait).
    #[test]
    fn build_dep_class_index_flattens_cross_file_trait_provisions() {
        let temp = TempDir::new().unwrap();
        let dep_dir = temp.path().join("dep_traits");
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, "dep_traits", "0.1.0", "");
        write_source(
            &dep_dir,
            "tagged.bt",
            "Protocol define: Tagged\n  name -> String\n\n  tag -> String => self name\n",
        );
        write_source(
            &dep_dir,
            "widget.bt",
            "Object subclass: Widget\n  uses: Tagged\n  name -> String => \"w\"\n",
        );

        let dep_root = Utf8PathBuf::from_path_buf(dep_dir).unwrap();
        let (_, class_infos, _, protocol_defs, _, _, _) =
            build_dep_class_index(&dep_root, "dep_traits", &[]).unwrap();

        assert_eq!(protocol_defs.len(), 1, "Tagged is provision-bearing");
        let widget = class_infos
            .iter()
            .find(|c| c.name == "Widget")
            .expect("Widget is exported");
        assert_eq!(widget.package.as_deref(), Some("dep_traits"));
        assert!(
            widget.methods.iter().any(|m| m.selector == "tag"),
            "exported Widget must carry Tagged's provided `tag`: {:?}",
            widget
                .methods
                .iter()
                .map(|m| &m.selector)
                .collect::<Vec<_>>()
        );
    }

    /// BT-4707: the protocol ASTs a dependency exports have its own
    /// `internal type` aliases expanded, so a dependency compiled after it —
    /// with its own, clashing `internal type Secret` — flattens `uses:
    /// dep_a@Tagged` against the right types, including an alias that expands
    /// to a difference type.
    #[test]
    fn build_dep_class_index_expands_internal_aliases_in_exported_protocol_defs() {
        let temp = TempDir::new().unwrap();
        let a_dir = temp.path().join("dep_a");
        fs::create_dir_all(&a_dir).unwrap();
        write_manifest(&a_dir, "dep_a", "0.1.0", "");
        write_source(
            &a_dir,
            "tagged.bt",
            "internal type Secret = Integer\n\
             internal type Label = Symbol \\ #hidden\n\n\
             Protocol define: Tagged\n  \
             name -> String\n\n  \
             tag -> Secret => 1\n\n  \
             label -> Label => #shown\n",
        );
        let a_root = Utf8PathBuf::from_path_buf(a_dir).unwrap();
        let (_, _, protocol_infos, protocol_defs, alias_infos, protocol_sources, _) =
            build_dep_class_index(&a_root, "dep_a", &[]).unwrap();

        // The exported AST names no internal alias.
        let tagged = &protocol_defs[0];
        let returns: Vec<_> = tagged
            .provided_methods
            .iter()
            .map(|m| m.return_type.as_ref().unwrap().type_name().to_string())
            .collect();
        assert_eq!(returns, ["Integer", "Symbol \\ #hidden"]);

        let mut dep_a = named_dep("dep_a");
        dep_a.protocol_infos = protocol_infos;
        dep_a.protocol_defs = protocol_defs;
        dep_a.protocol_sources = protocol_sources;
        dep_a.alias_infos = alias_infos;

        // A later dependency with a clashing internal alias flattens `dep_a`'s
        // provisions without re-reading them in its own alias scope.
        let b_dir = temp.path().join("dep_b");
        fs::create_dir_all(&b_dir).unwrap();
        write_manifest(&b_dir, "dep_b", "0.1.0", "");
        write_source(
            &b_dir,
            "widget.bt",
            "internal type Secret = String\n\
             internal type Label = Boolean\n\n\
             Object subclass: Widget\n  \
             uses: dep_a@Tagged\n  \
             name -> String => \"w\"\n",
        );
        let b_root = Utf8PathBuf::from_path_buf(b_dir).unwrap();
        let (_, class_infos, _, _, _, _, _) =
            build_dep_class_index(&b_root, "dep_b", &[dep_a]).unwrap();
        let widget = class_infos.iter().find(|c| c.name == "Widget").unwrap();
        let ret = |sel: &str| {
            widget
                .methods
                .iter()
                .find(|m| m.selector == sel)
                .and_then(|m| m.return_type.as_ref())
                .map(ToString::to_string)
        };
        assert_eq!(ret("tag").as_deref(), Some("Integer"));
        assert_eq!(ret("label").as_deref(), Some("Symbol \\ #hidden"));
    }

    /// BT-4615: a dependency's own `internal type` aliases are expanded out of
    /// the signatures it exports — class methods and extension methods alike —
    /// because the consumer never has them in its alias scope. Public aliases
    /// and classes stay as names.
    #[test]
    fn build_dep_class_index_expands_internal_aliases_in_signatures() {
        let temp = TempDir::new().unwrap();
        let dep_dir = temp.path().join("dep_secret");
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, "dep_secret", "0.1.0", "");
        write_source(
            &dep_dir,
            "secret.bt",
            "type Status = #ok | #error\n\
             internal type Secret = Integer\n\
             internal type Chain = Secret\n\
             internal type Loop = Loop\n\n\
             Protocol define: Tagged\n  \
             name -> String\n\n  \
             tag -> Secret => 1\n\n\
             Object subclass: Vault\n  \
             uses: Tagged\n  \
             name -> String => \"v\"\n  \
             thing -> Secret => 1\n  \
             many: n :: Secret -> List(Secret) => #()\n  \
             maybe -> Secret | Nil => nil\n  \
             chained -> Chain => 1\n  \
             looped -> Loop => nil\n  \
             status -> Status => #ok\n\n\
             Object >> secretExt -> Secret => 1\n\
             Object >> secretArg: s :: List(Secret) -> Status => #ok\n",
        );

        let dep_root = Utf8PathBuf::from_path_buf(dep_dir).unwrap();
        let (_, class_infos, _, _, _, _, extensions) =
            build_dep_class_index(&dep_root, "dep_secret", &[]).unwrap();

        let vault = class_infos
            .iter()
            .find(|c| c.name.as_str() == "Vault")
            .expect("Vault class info");
        let ret = |sel: &str| {
            vault
                .methods
                .iter()
                .find(|m| m.selector.as_str() == sel)
                .and_then(|m| m.return_type.as_ref())
                .map_or_else(|| panic!("no return type for {sel}"), ToString::to_string)
        };
        assert_eq!(ret("thing"), "Integer");
        assert_eq!(ret("tag"), "Integer", "a flattened provision is expanded");
        assert_eq!(ret("many:"), "List(Integer)");
        assert_eq!(ret("maybe"), "Integer | Nil");
        assert_eq!(ret("chained"), "Integer");
        assert_eq!(ret("looped"), "Loop", "a cycle terminates, left as a name");
        assert_eq!(ret("status"), "Status", "public aliases stay as names");
        let many = vault
            .methods
            .iter()
            .find(|m| m.selector == "many:")
            .unwrap();
        assert_eq!(many.param_types[0].as_ref().unwrap().to_string(), "Integer");

        let ext = |sel: &str| {
            extensions
                .index()
                .entries()
                .iter()
                .find(|(k, _)| k.selector.as_str() == sel)
                .map_or_else(
                    || panic!("no extension {sel}"),
                    |(_, locs)| locs[0].type_info.clone(),
                )
        };
        assert_eq!(ext("secretExt").return_type.as_deref(), Some("Integer"));
        let arg = ext("secretArg:");
        assert_eq!(arg.param_types[0].as_deref(), Some("List(Integer)"));
        assert_eq!(arg.return_type.as_deref(), Some("Status"));
    }

    /// `build_dep_class_index` must extract a dependency's protocol
    /// and type-alias declarations alongside its classes, so that a
    /// consumer's cross-package `extending:`/`:: Alias` resolution can be
    /// wired from `ResolvedDependency.protocol_infos`/`alias_infos`.
    ///
    /// Each `AliasInfo` (public and `internal`) must be stamped with
    /// `.package = Some(dep_name)` — `AliasRegistry::add_pre_loaded`'s
    /// seeding-boundary filter (ADR 0108 Phase 5) relies on that stamp to
    /// exclude a dependency's `internal type` while still seeding its public
    /// aliases. Extraction itself does not filter `internal` aliases out —
    /// that exclusion happens downstream at seeding time.
    #[test]
    fn build_dep_class_index_extracts_protocols_and_aliases() {
        let temp = TempDir::new().unwrap();
        let dep_dir = temp.path().join("dep_types");
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, "dep_types", "0.1.0", "");
        write_source(
            &dep_dir,
            "types.bt",
            "type Status = #ok | #error\n\
             internal type Secret = Integer\n\n\
             Protocol define: Greetable\n  \
             greet -> String\n",
        );

        let dep_root = Utf8PathBuf::from_path_buf(dep_dir).unwrap();
        let (class_module_index, _class_infos, protocol_infos, _protocol_defs, alias_infos, _, _) =
            build_dep_class_index(&dep_root, "dep_types", &[]).unwrap();

        assert!(
            class_module_index.is_empty(),
            "fixture declares no classes: {class_module_index:?}"
        );

        assert_eq!(
            protocol_infos.len(),
            1,
            "should extract the Greetable protocol: {protocol_infos:?}"
        );
        assert_eq!(protocol_infos[0].name.as_str(), "Greetable");

        assert_eq!(
            alias_infos.len(),
            2,
            "should extract both Status and Secret aliases: {alias_infos:?}"
        );
        let status = alias_infos
            .iter()
            .find(|a| a.name.as_str() == "Status")
            .expect("Status alias should be extracted");
        assert!(!status.is_internal, "Status is a public alias");
        assert_eq!(
            status.package.as_deref(),
            Some("dep_types"),
            "alias must be stamped with the dependency's package name"
        );

        let secret = alias_infos
            .iter()
            .find(|a| a.name.as_str() == "Secret")
            .expect(
                "Secret alias should be extracted (filtering happens at seeding, not extraction)",
            );
        assert!(secret.is_internal, "Secret is declared `internal type`");
        assert_eq!(
            secret.package.as_deref(),
            Some("dep_types"),
            "internal alias must still be stamped so add_pre_loaded can filter it"
        );
    }

    /// ADR 0127 §3 / BT-3663: a dependency's provision-bearing protocol
    /// carries its own source identity — the checkout file, its text, and
    /// the dependency's package — so a flattened method maps its lines to
    /// that file (not the using file) and resolves names in that package.
    /// A provision-less protocol has none.
    #[test]
    fn build_dep_class_index_records_dependency_protocol_sources() {
        let temp = TempDir::new().unwrap();
        let dep_dir = temp.path().join("dep_traits");
        fs::create_dir_all(&dep_dir).unwrap();
        write_manifest(&dep_dir, "dep_traits", "0.1.0", "");
        let trait_src = "Protocol define: Retryable\n  \
             attempts -> Integer\n\n  \
             retry -> Integer => self attempts + 1\n";
        write_source(&dep_dir, "retryable.bt", trait_src);
        write_source(
            &dep_dir,
            "plain.bt",
            "Protocol define: Plain\n  name -> String\n",
        );

        let dep_root = Utf8PathBuf::from_path_buf(dep_dir).unwrap();
        let (_, _, _, protocol_defs, _, protocol_sources, _) =
            build_dep_class_index(&dep_root, "dep_traits", &[]).unwrap();

        assert_eq!(protocol_defs.len(), 1);
        // Bare name and `dep_traits@Retryable` (BT-3944).
        assert_eq!(protocol_sources.len(), 2, "{protocol_sources:?}");
        assert_eq!(
            protocol_sources["dep_traits@Retryable"],
            protocol_sources["Retryable"]
        );
        let source = &protocol_sources["Retryable"];
        assert!(
            source
                .path
                .as_deref()
                .is_some_and(|p| p.ends_with("retryable.bt")),
            "{source:?}"
        );
        assert_eq!(source.text.as_str(), trait_src);
        assert_eq!(source.package.as_deref(), Some("dep_traits"));
        assert!(!protocol_sources.contains_key("Plain"));
    }

    fn named_dep(name: &str) -> ResolvedDependency {
        ResolvedDependency {
            name: name.to_string(),
            root: Utf8PathBuf::from("/nonexistent"),
            ebin_path: Utf8PathBuf::from("/nonexistent/ebin"),
            class_module_index: HashMap::new(),
            class_infos: Vec::new(),
            protocol_infos: Vec::new(),
            protocol_defs: Vec::new(),
            protocol_sources: beamtalk_core::semantic_analysis::ProtocolSourceMap::new(),
            alias_infos: Vec::new(),
            is_direct: true,
            via_chain: Vec::new(),
            stubs_dir: None,
            extensions: beamtalk_cli::dependency_extensions::DependencyExtensions::default(),
        }
    }

    /// BT-4375: a dependency sees only the earlier dependencies in its own
    /// transitive closure, in compile order, and every entry is restored to
    /// its place afterwards.
    #[test]
    fn with_dependency_closure_passes_only_the_closure_and_restores_order() {
        let mut resolved: Vec<ResolvedDependency> = ["zulu", "core", "alpha", "base"]
            .into_iter()
            .map(named_dep)
            .collect();
        let dependencies: std::collections::BTreeMap<String, Vec<String>> = [
            ("zulu", vec![]),
            ("core", vec![]),
            ("alpha", vec![]),
            ("base", vec!["core".to_string()]),
            ("app_lib", vec!["base".to_string()]),
        ]
        .into_iter()
        .map(|(name, deps)| (name.to_string(), deps))
        .collect();

        let seen = with_dependency_closure(&mut resolved, &dependencies, "app_lib", |prior| {
            prior.iter().map(|d| d.name.clone()).collect::<Vec<_>>()
        });

        assert_eq!(seen, ["core", "base"]);
        let after: Vec<&str> = resolved.iter().map(|d| d.name.as_str()).collect();
        assert_eq!(after, ["zulu", "core", "alpha", "base"]);
    }

    /// BT-4328: a dependency compiling against an already-compiled
    /// dependency sees the ancestor's full `ClassInfo` (methods and
    /// superclass links), not just its name→module mapping. A child
    /// class inheriting everything from a cross-package parent must
    /// resolve the chain — otherwise hierarchy-driven codegen
    /// (`is_server_subclass`, `initialize`/`trap_exit` detection)
    /// silently degrades to the base-class defaults.
    #[test]
    fn dep_compile_class_inputs_merges_prior_dep_class_infos() {
        let temp = TempDir::new().unwrap();

        // Base package: owns `Base` and its `greet` method.
        let base_dir = temp.path().join("dep_base");
        fs::create_dir_all(&base_dir).unwrap();
        write_manifest(&base_dir, "dep_base", "0.1.0", "");
        write_source(
            &base_dir,
            "base.bt",
            "Object subclass: Base\n  greet => \"hi from base\"\n",
        );
        let base_root = Utf8PathBuf::from_path_buf(base_dir).unwrap();
        let (base_index, base_infos, _, _, _, _, base_extensions) =
            build_dep_class_index(&base_root, "dep_base", &[]).unwrap();
        assert!(
            base_infos.iter().any(|c| c.name.as_str() == "Base"),
            "base export must contain Base: {:?}",
            base_infos
                .iter()
                .map(|c| c.name.as_str())
                .collect::<Vec<_>>()
        );
        let base_dep = ResolvedDependency {
            name: "dep_base".to_string(),
            root: base_root,
            ebin_path: Utf8PathBuf::from("/nonexistent/ebin"),
            class_module_index: base_index,
            class_infos: base_infos,
            protocol_infos: Vec::new(),
            protocol_defs: Vec::new(),
            protocol_sources: beamtalk_core::semantic_analysis::ProtocolSourceMap::new(),
            alias_infos: Vec::new(),
            is_direct: true,
            via_chain: Vec::new(),
            stubs_dir: None,
            extensions: base_extensions,
        };

        // Child package: `Child` inherits everything from cross-package `Base`.
        let child_dir = temp.path().join("dep_child");
        fs::create_dir_all(&child_dir).unwrap();
        write_manifest(&child_dir, "dep_child", "0.1.0", "");
        write_source(
            &child_dir,
            "child.bt",
            "Base subclass: Child\n  label => \"child\"\n",
        );
        let child_root = Utf8PathBuf::from_path_buf(child_dir).unwrap();
        let source_files =
            beamtalk_core::file_walker::collect_project_source_files(&child_root).unwrap();
        let source_root = child_root.join("src");
        assert!(source_root.exists(), "fixture must have a src/ dir");

        let DepCompileClassInputs {
            all_class_infos, ..
        } = dep_compile_class_inputs(
            &source_files,
            Some(source_root.as_path()),
            "dep_child",
            &[base_dep],
        )
        .unwrap();

        let base = all_class_infos.iter().find(|c| c.name.as_str() == "Base");
        assert!(
            base.is_some(),
            "dep compile must see the cross-package ancestor Base, got: {:?}",
            all_class_infos
                .iter()
                .map(|c| c.name.as_str())
                .collect::<Vec<_>>()
        );
        assert!(
            base.is_some_and(|b| b.methods.iter().any(|m| m.selector == "greet")),
            "ancestor Base must carry its `greet` method"
        );
        assert!(
            all_class_infos.iter().any(|c| c.name.as_str() == "Child"),
            "dep compile must keep its own Child class"
        );
    }

    /// BT-4225: a dependency compiles against the same FFI type registry as
    /// its standalone build — its own `stubs/` tier over the OTP specs —
    /// not none, which typed every FFI call in it `Dynamic`.
    #[test]
    fn dependency_type_registry_loads_dep_stubs_and_otp_specs() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().join("app")).unwrap();
        let dep_root = Utf8PathBuf::from_path_buf(temp.path().join("dep")).unwrap();
        fs::create_dir_all(&project_root).unwrap();
        fs::create_dir_all(dep_root.join("stubs")).unwrap();
        fs::write(
            dep_root.join("stubs/dep_nif.bt"),
            "declare native: dep_nif\n  echo: x :: Integer -> Integer\n",
        )
        .unwrap();

        let registry =
            dependency_type_registry(&project_root, &dep_root, &[], &OtpTypeSpecs::default())
                .expect("the dependency's own stubs/ tier must seed its registry");
        assert!(
            registry.lookup("dep_nif", "echo", 1).is_some(),
            "dep's stubs/ declaration must be loaded"
        );
        // OTP discovery is environment-dependent (no runtime → no specs);
        // when it finds any, the dependency sees them too.
        if registry.module_count() > 1 {
            assert!(
                registry.lookup("proplists", "get_value", 3).is_some(),
                "OTP specs must reach the dependency's compile"
            );
        }
    }

    /// Compiles `-module(Module). ping() -> pong.` with a spec into `ebin`,
    /// or answers `false` when `erlc` is not on the path.
    fn compile_spec_beam(ebin: &Utf8Path, module: &str) -> bool {
        fs::create_dir_all(ebin).unwrap();
        let src = ebin.join(format!("{module}.erl"));
        fs::write(
            &src,
            format!(
                "-module({module}).\n-export([ping/0]).\n-spec ping() -> pong.\nping() -> pong.\n"
            ),
        )
        .unwrap();
        let compiled = std::process::Command::new("erlc")
            .arg("+debug_info")
            .arg("-o")
            .arg(ebin.as_str())
            .arg(src.as_str())
            .status()
            .is_ok_and(|status| status.success());
        fs::remove_file(&src).unwrap();
        compiled
    }

    /// BT-4311: a dependency's registry holds the specs of the dependencies
    /// compiled before it and nothing else from `_build/` — a native module
    /// an earlier build left there must not type its calls, or a clean build
    /// and a rebuild would compile the same source differently.
    #[test]
    fn dependency_type_registry_ignores_stale_build_contents() {
        let temp = TempDir::new().unwrap();
        let project_root = Utf8PathBuf::from_path_buf(temp.path().join("app")).unwrap();
        let dep_root = Utf8PathBuf::from_path_buf(temp.path().join("dep")).unwrap();
        let prior_root = Utf8PathBuf::from_path_buf(temp.path().join("prior")).unwrap();
        fs::create_dir_all(&dep_root).unwrap();
        let stale_ebin = project_root.join("_build/dev/native/ebin");
        let prior_ebin = BuildLayout::new(&project_root).dep_ebin_dir("prior");
        if !compile_spec_beam(&stale_ebin, "bt4311_stale_nif")
            || !compile_spec_beam(&prior_ebin, "bt4311_prior_nif")
        {
            eprintln!("skipping: erlc is not available");
            return;
        }
        let prior = ResolvedDependency {
            name: "prior".to_string(),
            root: prior_root,
            ebin_path: prior_ebin,
            class_module_index: HashMap::new(),
            class_infos: Vec::new(),
            protocol_infos: Vec::new(),
            protocol_defs: Vec::new(),
            protocol_sources: HashMap::default(),
            alias_infos: Vec::new(),
            is_direct: true,
            via_chain: Vec::new(),
            stubs_dir: None,
            extensions: beamtalk_cli::dependency_extensions::DependencyExtensions::default(),
        };

        let Some(registry) = dependency_type_registry(
            &project_root,
            &dep_root,
            std::slice::from_ref(&prior),
            &OtpTypeSpecs::default(),
        ) else {
            eprintln!("skipping: no build worker to extract specs in this environment");
            return;
        };
        if registry.lookup("bt4311_prior_nif", "ping", 0).is_none() {
            eprintln!("skipping: spec extraction unavailable in this environment");
            return;
        }
        assert!(
            registry.lookup("bt4311_stale_nif", "ping", 0).is_none(),
            "a native module left in _build/ must not reach the dependency's registry"
        );
    }
}
