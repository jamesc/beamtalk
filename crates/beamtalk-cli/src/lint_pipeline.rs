// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The `beamtalk lint` analysis pipeline, shared by CLI `beamtalk lint` and
//! the MCP `lint`/`diagnostic_summary` tools (BT-4585).
//!
//! **DDD Context:** Language Service
//!
//! Both surfaces used to run their own copy of the two-pass pipeline, and the
//! MCP copy lagged: no pre-loaded protocol infos or type aliases, the
//! `ModuleOnly` knowledge scope instead of the package's project-complete
//! one, no `.erl` linting and no dedup of repeated pre-loaded alias
//! collisions. Since MCP `lint` gained the `warnings_as_errors` gate
//! (BT-4336), those gaps could flip its verdict. Now there is one pipeline:
//!
//! 1. [`collect_lint_files`] picks the `.bt` and `.erl` targets for a path.
//! 2. [`parse_package_sources`] (Pass 1) parses every file in the package —
//!    not only the targets — and extracts the cross-file context: classes,
//!    extensions, protocols, carried protocol ASTs, type aliases and trait
//!    users.
//! 3. [`analyse_package`] (Pass 2) merges the dependencies' metadata, lifts
//!    or keeps the pre-WS3 dependency guard, flattens trait provisions and
//!    analyses each target file with [`analyse_file`], reporting each one to
//!    a callback, then routes flattened-provision diagnostics to their
//!    protocol's file.
//! 4. [`LintGate`] tallies the verdict: Lint-severity diagnostics, plus the
//!    live-recheck set under `--warnings-as-errors`.
//!
//! The surfaces supply only what genuinely differs between them:
//!
//! - **Dependency metadata** ([`DependencyMetadata`]): the CLI resolves
//!   dependencies with `ensure_deps_resolved` (which may fetch and build);
//!   MCP reads whatever checkouts are already on disk
//!   ([`crate::dependency_classes`]), so a diagnostics query never touches
//!   the network. On a package whose dependencies are fetched both see the
//!   same metadata.
//! - **The FFI type registry**: passed in by the caller.

use std::collections::HashSet;
use std::sync::Arc;

use beamtalk_core::ast::{Module, ProtocolDefinition};
use beamtalk_core::compilation::diagnostics_policy::{self, DiagnosticsTable};
use beamtalk_core::compilation::extension_index::ExtensionIndex;
use beamtalk_core::file_walker::FileWalker;
use beamtalk_core::semantic_analysis::alias_registry::{AliasInfo, AliasRegistry};
use beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo;
use beamtalk_core::semantic_analysis::protocol_registry::{ProtocolInfo, ProtocolRegistry};
use beamtalk_core::semantic_analysis::protocol_source::{
    dedupe_provision_diagnostics, merge_protocol_sources, provision_report_target,
};
use beamtalk_core::semantic_analysis::trait_expansion::{TraitUserCollector, is_carried_protocol};
use beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry;
use beamtalk_core::semantic_analysis::{
    AnalysisContext, ClassHierarchy, KnowledgeScope, ProtocolSourceMap,
};
use beamtalk_core::source_analysis::{Diagnostic, Severity, Span, lex_with_eof, parse};
use beamtalk_project::package;
use camino::{Utf8Path, Utf8PathBuf};
use tracing::warn;

use crate::erlang_lint::{self, ErlangLintDiagnostic};
use crate::manifest::{PackageCheckerScope, package_checker_scope};

/// Walk ancestors from `start` to find the package root (the directory
/// holding `beamtalk.toml`).
///
/// Camino wrapper around [`beamtalk_project::package::find_package_root`],
/// which canonicalizes relative paths such as `test/` first. `None` when no
/// ancestor has a manifest, or the root is not UTF-8.
#[must_use]
pub fn find_package_root(start: &Utf8Path) -> Option<Utf8PathBuf> {
    package::find_package_root(start.as_std_path()).and_then(|p| Utf8PathBuf::from_path_buf(p).ok())
}

/// Collect the `.bt` and `.erl` files `beamtalk lint <path>` checks.
///
/// A file target must be a `.bt` or `.erl` file. A directory target yields
/// its `.bt` files (minus the package's type-only `stubs/`, ADR 0075) plus
/// the package's `native/` `.erl` files.
///
/// # Errors
///
/// When `source_path` does not exist, is a file of another kind, or yields
/// no files at all. `path` is the user's spelling, used in the messages.
pub fn collect_lint_files(
    source_path: &Utf8Path,
    path: &str,
) -> miette::Result<(Vec<Utf8PathBuf>, Vec<Utf8PathBuf>)> {
    let mut erl_files: Vec<Utf8PathBuf> = Vec::new();

    let source_files = if source_path.is_file() {
        match source_path.extension() {
            Some("bt") => vec![source_path.to_path_buf()],
            Some("erl") => {
                erl_files.push(source_path.to_path_buf());
                Vec::new()
            }
            _ => miette::bail!("File '{}' is not a .bt or .erl source file", path),
        }
    } else if source_path.is_dir() {
        let project_root =
            find_package_root(source_path).unwrap_or_else(|| source_path.to_path_buf());
        let native_dir = project_root.join("native");
        if native_dir.is_dir() {
            match FileWalker::native_erl_files().walk(&native_dir) {
                Ok(files) => erl_files = files,
                Err(e) => warn!("failed to scan native directory: {e}"),
            }
        }
        // `stubs/` is excluded (ADR 0075): it's type-only and never
        // compiled, but this walk has no src/-only scoping to exclude it
        // implicitly the way `find_source_files` does, so a `declare
        // native:` stub file would otherwise be swept in and rejected here
        // as a hard error. `project_root` is canonical but the walk yields
        // paths as `source_path` spells them (a symlink, macOS `/var`,
        // a Windows 8.3 name), so compare canonically (BT-4656 follow-up).
        FileWalker::lint_files()
            .walk(source_path)?
            .into_iter()
            .filter(|f| !package::is_under_stubs_dir(project_root.as_std_path(), f.as_std_path()))
            .collect()
    } else {
        miette::bail!("Path '{}' does not exist", path);
    };

    if source_files.is_empty() && erl_files.is_empty() {
        miette::bail!("No .bt or .erl source files found in '{path}'");
    }

    Ok((source_files, erl_files))
}

/// Read and lint one native `.erl` file.
///
/// # Errors
///
/// When the file cannot be read.
pub fn lint_erl_file(path: &Utf8Path) -> std::io::Result<Vec<ErlangLintDiagnostic>> {
    let source = std::fs::read_to_string(path)?;
    Ok(erlang_lint::lint_erl_file(path, &source))
}

/// The package's dependencies' contribution to the cross-file context, read
/// by whichever resolver the surface uses (see the module docs).
#[derive(Debug, Default)]
pub struct DependencyMetadata {
    /// Every class the dependencies define, trait provisions flattened.
    pub class_infos: Vec<ClassInfo>,
    /// Every protocol they declare, stamped with its package.
    pub protocol_infos: Vec<ProtocolInfo>,
    /// The full ASTs of their carried protocols (ADR 0127 §10a; BT-3591).
    pub protocol_defs: Vec<ProtocolDefinition>,
    /// The source identity of each protocol in `protocol_defs`.
    pub protocol_sources: ProtocolSourceMap,
    /// Every type alias they declare, `internal` ones included — the
    /// seeding-boundary filter runs in `AliasRegistry::add_pre_loaded`.
    pub alias_infos: Vec<AliasInfo>,
    /// Their extension methods (ADR 0070 WS3, BT-4608).
    pub extensions: ExtensionIndex,
    /// Whether `extensions` holds every dependency's extensions; `false`
    /// keeps the pre-WS3 dependency guard on.
    pub extensions_complete: bool,
}

/// A lint target parsed by Pass 1.
#[derive(Debug)]
pub struct ParsedFile {
    /// The file's path, as collected.
    pub path: Utf8PathBuf,
    /// Its source text.
    pub source: String,
    /// Its parsed module.
    pub module: Module,
    /// The parser's diagnostics.
    pub parse_diagnostics: Vec<Diagnostic>,
}

/// The package's own cross-file context, from Pass 1
/// ([`parse_package_sources`]), plus the parsed lint targets.
#[derive(Debug)]
pub struct PackageSources {
    /// The package root, `None` for files outside any package.
    pub package_root: Option<Utf8PathBuf>,
    /// The knowledge scope, dependency flag and package name, from one
    /// manifest read by the rule `build`/`test` share.
    pub scope: PackageCheckerScope,
    /// Every class in the package's source files, package-stamped.
    pub class_infos: Vec<ClassInfo>,
    /// Every standalone extension method in them (ADR 0066).
    pub extension_index: ExtensionIndex,
    /// Every protocol declared in them.
    pub protocol_infos: Vec<ProtocolInfo>,
    /// The full ASTs of their carried protocols.
    pub protocol_defs: Vec<ProtocolDefinition>,
    /// Every type alias declared in them, package-stamped.
    pub alias_infos: Vec<AliasInfo>,
    /// Their trait users, flattened in Pass 2 once the dependencies'
    /// protocols are merged in (BT-3673).
    pub trait_users: TraitUserCollector,
    /// The lint targets, in extraction order.
    pub targets: Vec<ParsedFile>,
}

impl PackageSources {
    /// The package name, for E0401/E0402 visibility checks.
    #[must_use]
    pub fn current_package(&self) -> Option<&str> {
        self.scope.package_name.as_deref()
    }
}

/// Pass 1: parse every source file of `lint_path`'s package, not only the
/// `targets`, and extract the cross-file context.
///
/// Inside a package, extraction covers the package's `src/` and `test/`
/// ([`beamtalk_project::package::resolve_extraction_files`]) plus the
/// targets, so a test file that references a `src/` class resolves it.
/// Outside one, only the targets are parsed and the scope is `ModuleOnly`.
///
/// `on_unreadable(file, is_target, error)` is called for each file that
/// cannot be read; the file is skipped. A source directory that cannot be
/// walked is logged. Either way Pass 1 has not seen the whole package, so
/// the scope drops to `ModuleOnly`: claiming `ProjectComplete` would turn
/// the missing classes into "genuinely unresolved" ones.
pub fn parse_package_sources(
    lint_path: &Utf8Path,
    targets: &[Utf8PathBuf],
    mut on_unreadable: impl FnMut(&Utf8Path, bool, &std::io::Error),
) -> PackageSources {
    let package_root = find_package_root(lint_path);
    let mut scope = package_checker_scope(package_root.as_deref());
    let current_package = scope.package_name.clone();
    let current_package = current_package.as_deref();

    let target_paths: Vec<std::path::PathBuf> = targets
        .iter()
        .map(|t| t.as_std_path().to_path_buf())
        .collect();
    let (extraction_files, target_set, walk_errors) =
        package::resolve_extraction_files_with_errors(lint_path.as_std_path(), &target_paths);
    for (dir, e) in &walk_errors {
        warn!(
            "failed to walk '{}' for cross-file class extraction: {e}",
            dir.display()
        );
    }
    let mut complete = walk_errors.is_empty();

    let mut class_infos = Vec::new();
    let mut extension_index = ExtensionIndex::new();
    let mut protocol_infos = Vec::new();
    let mut protocol_defs = Vec::new();
    let mut alias_infos = Vec::new();
    let mut trait_users = TraitUserCollector::default();
    let mut parsed_targets = Vec::new();

    for file in extraction_files {
        let file = Utf8PathBuf::from_path_buf(file)
            .unwrap_or_else(|p| Utf8PathBuf::from(p.to_string_lossy().into_owned()));
        let is_target = target_set.contains(&package::try_canonicalize(file.as_std_path()));
        let source = match std::fs::read_to_string(&file) {
            Ok(source) => source,
            Err(e) => {
                on_unreadable(&file, is_target, &e);
                complete = false;
                continue;
            }
        };

        let (module, parse_diagnostics) = parse(lex_with_eof(&source));

        // A file with parse errors may have an under-recovered method
        // surface — mark its classes so the receiver-knowledge classifier
        // treats them (and their subclasses) as `Open` rather than emitting
        // hints against a surface extraction never fully saw.
        let has_parse_errors = parse_diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error);
        let mut file_class_infos = ClassHierarchy::extract_class_infos(&module);
        if has_parse_errors {
            for info in &mut file_class_infos {
                info.surface_incomplete = true;
            }
        }
        // Stamp the package per-file, same as build's Pass 1 — see
        // `stamp_package_on_infos`'s doc for why this can't wait until each
        // file's own analysis pass.
        if let Some(pkg) = current_package {
            ClassHierarchy::stamp_package_on_infos(&mut file_class_infos, pkg);
        }
        class_infos.extend(file_class_infos);
        // Also records each carried protocol's source identity (ADR 0127
        // §3), first definition wins.
        trait_users.add_source_file(&module, file.as_str(), &source, current_package);
        extension_index.add_module(&module, file.as_std_path());
        protocol_infos.extend(ProtocolRegistry::extract_protocol_infos(&module));
        protocol_defs.extend(
            module
                .protocols
                .iter()
                .filter(|p| is_carried_protocol(p))
                .cloned(),
        );
        // Stamped like `build`'s `collect_project_alias_infos`, so
        // `AliasRegistry::add_pre_loaded`'s seeding-boundary filter can tell
        // a same-package alias from a dependency's `internal` one.
        alias_infos.extend(AliasRegistry::extract_package_alias_infos(
            &module,
            current_package,
        ));

        if is_target {
            parsed_targets.push(ParsedFile {
                path: file,
                source,
                module,
                parse_diagnostics,
            });
        }
    }

    if !complete {
        scope.knowledge_scope = KnowledgeScope::ModuleOnly;
    }

    PackageSources {
        package_root,
        scope,
        class_infos,
        extension_index,
        protocol_infos,
        protocol_defs,
        alias_infos,
        trait_users,
        targets: parsed_targets,
    }
}

/// The cross-file context one file is analysed against ([`analyse_file`]).
/// The defaults are an isolated module: no cross-file knowledge, the
/// `ModuleOnly` scope, no package.
#[derive(Debug, Default)]
pub struct FileContext<'a> {
    /// Classes defined in other files
    /// ([`ClassHierarchy::cross_file_class_infos`]).
    pub cross_file_classes: Vec<ClassInfo>,
    /// Protocols declared in other files and dependencies.
    pub pre_loaded_protocols: Vec<ProtocolInfo>,
    /// Carried protocol ASTs: the package's own, then the dependencies'.
    pub pre_loaded_protocol_defs: Vec<ProtocolDefinition>,
    /// The source identity of each carried protocol.
    pub protocol_sources: ProtocolSourceMap,
    /// Type aliases declared in other files and dependencies.
    pub pre_loaded_aliases: Vec<AliasInfo>,
    /// The FFI type registry, so `(Erlang m) f:` calls are typed the way
    /// `build` types them.
    pub native_type_registry: Option<Arc<NativeTypeRegistry>>,
    /// `ProjectComplete` inside a package.
    pub knowledge_scope: KnowledgeScope,
    /// Every standalone extension method, the package's and the
    /// dependencies'; `None` for none.
    pub cross_file_extensions: Option<&'a ExtensionIndex>,
    /// The pre-WS3 dependency guard (`CompilerOptions`).
    pub dependency_extensions_unknown: bool,
    /// The package name; E0401/E0402/E0403 only run when it is set.
    pub current_package: Option<&'a str>,
    /// Whether the file lives under the package's `stubs/`, where `declare
    /// native:` blocks are legal.
    pub is_stub_file: bool,
    /// The file's basename without extension, for the file-name/class-name
    /// agreement check; `None` skips it.
    pub file_stem: Option<&'a str>,
    /// Whether the file is stdlib source, which may override sealed methods
    /// the way `beamtalk build-stdlib` allows.
    pub stdlib_mode: bool,
    /// The package's hex-dep/native Erlang modules, for `unresolved_ffi`
    /// (BT-4022).
    pub known_erlang_modules: HashSet<String>,
    /// The package's `[diagnostics]` table (BT-4377); `None` for none.
    pub diagnostics_overrides: Option<&'a DiagnosticsTable>,
}

/// Analyse one parsed module the way `beamtalk lint` does.
///
/// Keeps the parser's `Error`, `Warning` and `Lint` diagnostics, then runs
/// the shared post-analysis lint pipeline
/// ([`run_post_analysis_lint_pipeline`](beamtalk_language_service::queries::diagnostic_provider::run_post_analysis_lint_pipeline)):
/// semantic analysis against `ctx` (categorised diagnostics only), the lint
/// passes against its cross-file hierarchy (BT-4183), the file-name check,
/// `@expect` directives, near-miss section dividers and the `[diagnostics]`
/// table. Returns the diagnostics and the class hierarchy (for type
/// inference).
#[must_use]
pub fn analyse_file(
    module: &Module,
    source: &str,
    parse_diagnostics: Vec<Diagnostic>,
    ctx: FileContext<'_>,
) -> (Vec<Diagnostic>, ClassHierarchy) {
    let mut diags: Vec<_> = parse_diagnostics
        .into_iter()
        .filter(|d| {
            matches!(
                d.severity,
                Severity::Error | Severity::Warning | Severity::Lint
            )
        })
        .collect();

    let options = beamtalk_core::CompilerOptions {
        knowledge_scope: ctx.knowledge_scope,
        dependency_extensions_unknown: ctx.dependency_extensions_unknown,
        current_package: ctx.current_package.map(str::to_string),
        stdlib_mode: ctx.stdlib_mode,
        known_erlang_modules: ctx.known_erlang_modules,
        ..Default::default()
    };
    let no_extensions = ExtensionIndex::new();
    let no_overrides = DiagnosticsTable::new();
    let analysis_ctx = AnalysisContext::default()
        .with_options(&options)
        .with_pre_loaded_classes(ctx.cross_file_classes)
        .with_pre_loaded_protocols(ctx.pre_loaded_protocols)
        .with_pre_loaded_protocol_defs(ctx.pre_loaded_protocol_defs)
        .with_protocol_sources(ctx.protocol_sources)
        .with_pre_loaded_aliases(ctx.pre_loaded_aliases)
        .with_native_type_registry(ctx.native_type_registry)
        .with_cross_file_extensions(ctx.cross_file_extensions.unwrap_or(&no_extensions))
        .with_is_stub_file(ctx.is_stub_file);
    let hierarchy =
        beamtalk_language_service::queries::diagnostic_provider::run_post_analysis_lint_pipeline(
            module,
            source,
            analysis_ctx,
            ctx.file_stem,
            ctx.diagnostics_overrides.unwrap_or(&no_overrides),
            &mut diags,
            |hierarchy| beamtalk_lint::run_lint_passes_with_hierarchy(module, hierarchy),
        );
    (diags, hierarchy)
}

/// Decides whether a diagnostic should be kept, deduplicating repeat
/// sightings of a pre-loaded alias collision across a whole lint run.
///
/// Each file's analysis seeds a fresh `AliasRegistry` from the same
/// package-wide alias list, so a genuine collision between two of its
/// entries is rediscovered — with byte-identical message and span, since
/// neither depends on the file being analysed — once per file. `seen` keeps
/// only the first sighting. The `"Pre-loaded type alias "` prefix is shared
/// by all three of `AliasRegistry::add_pre_loaded`'s collision messages and
/// no other diagnostic, so every other (file-specific) diagnostic passes
/// through unfiltered.
pub fn dedup_pre_loaded_alias_collision<S: std::hash::BuildHasher>(
    diag: &Diagnostic,
    seen: &mut HashSet<(String, Span), S>,
) -> bool {
    if diag.message.starts_with("Pre-loaded type alias ") {
        seen.insert((diag.message.to_string(), diag.span))
    } else {
        true
    }
}

/// One target file's result from [`analyse_package`].
#[derive(Debug)]
pub struct FileLint<'a> {
    /// The file's path.
    pub path: &'a Utf8Path,
    /// Its source text, for line numbers.
    pub source: &'a str,
    /// Its parsed module.
    pub module: &'a Module,
    /// The class hierarchy it was analysed against.
    pub class_hierarchy: &'a ClassHierarchy,
    /// Its diagnostics, minus those raised inside a flattened trait
    /// provision (reported against the protocol's file instead).
    pub diagnostics: Vec<Diagnostic>,
}

/// A diagnostic raised inside a flattened trait provision, routed to the
/// protocol's file (ADR 0127 §3, BT-3665).
#[derive(Debug)]
pub struct ProvisionDiagnostic {
    /// The protocol's file.
    pub path: String,
    /// The protocol's source text; the diagnostic's span is an offset into
    /// it. Empty when the protocol's text is not known.
    pub source: String,
    /// The diagnostic, merged across the classes that use the protocol.
    pub diagnostic: Diagnostic,
}

/// Pass 2: analyse every lint target in `sources` against the package's and
/// its `dependencies`' cross-file context.
///
/// Each target's diagnostics go to `on_file` as soon as it is analysed, so
/// the CLI can stream them. Diagnostics raised inside a flattened trait
/// provision are held back, merged across the using classes and returned,
/// each routed to its protocol's file.
pub fn analyse_package(
    sources: PackageSources,
    dependencies: DependencyMetadata,
    native_type_registry: Option<&Arc<NativeTypeRegistry>>,
    mut on_file: impl FnMut(FileLint<'_>),
) -> Vec<ProvisionDiagnostic> {
    let PackageSources {
        package_root,
        scope,
        mut class_infos,
        mut extension_index,
        mut protocol_infos,
        mut protocol_defs,
        mut alias_infos,
        trait_users,
        targets,
    } = sources;
    let current_package = scope.package_name.as_deref();

    // The package's own carried protocols first, then the dependencies' —
    // first definition wins (BT-3915).
    let mut protocol_sources = trait_users.protocol_sources().clone();
    class_infos.extend(dependencies.class_infos);
    protocol_infos.extend(dependencies.protocol_infos);
    protocol_defs.extend(dependencies.protocol_defs);
    merge_protocol_sources(&mut protocol_sources, dependencies.protocol_sources);
    alias_infos.extend(dependencies.alias_infos);
    extension_index.merge(&dependencies.extensions);

    // The pre-WS3 dependency guard, by the rule `build`/`check`/`test`
    // share: on unless every dependency's extensions were loaded.
    let dependency_extensions_unknown = crate::dependency_extensions::dependency_extensions_unknown(
        scope.has_package_dependencies,
        package_root.is_some(),
        dependencies.extensions_complete,
    );

    // Flatten cross-file trait provisions into the same-package infos, as
    // `build` does (BT-3673) — after the dependency merge so a `uses:` of a
    // dependency's trait resolves too.
    trait_users.flatten(
        &mut class_infos,
        protocol_defs.iter().cloned(),
        current_package,
    );

    // BT-4022: project-known Erlang modules for the `unresolved_ffi` check.
    let known_erlang_modules = package_root.as_deref().map_or_else(
        HashSet::new,
        beamtalk_core::ffi_type_specs::collect_project_known_erlang_modules,
    );
    // BT-4377: the package's `[diagnostics]` table, via the loader the
    // compiler port (REPL) and LSP use.
    let diagnostics_overrides = package_root
        .as_deref()
        .map_or_else(DiagnosticsTable::new, |root| {
            diagnostics_policy::load_diagnostics_table_for_root(root.as_std_path())
        });

    let mut seen_alias_collisions = HashSet::new();
    let mut provision_diags = Vec::new();

    for target in targets {
        let cross_file_classes =
            ClassHierarchy::cross_file_class_infos(&class_infos, &target.module);
        // `collect_lint_files`'s `stubs/` exclusion only covers its
        // directory walk, so a direct `stubs/lists.bt` target is recognised
        // here.
        let is_stub_file = package_root.as_deref().is_some_and(|root| {
            package::is_under_stubs_dir(root.as_std_path(), target.path.as_std_path())
        });
        let (diags, class_hierarchy) = analyse_file(
            &target.module,
            &target.source,
            target.parse_diagnostics,
            FileContext {
                cross_file_classes,
                pre_loaded_protocols: protocol_infos.clone(),
                pre_loaded_protocol_defs: protocol_defs.clone(),
                protocol_sources: protocol_sources.clone(),
                pre_loaded_aliases: alias_infos.clone(),
                native_type_registry: native_type_registry.cloned(),
                knowledge_scope: scope.knowledge_scope,
                cross_file_extensions: Some(&extension_index),
                dependency_extensions_unknown,
                current_package,
                is_stub_file,
                file_stem: target.path.file_stem(),
                stdlib_mode: package::is_under_stdlib_src_dir(target.path.as_std_path()),
                known_erlang_modules: known_erlang_modules.clone(),
                diagnostics_overrides: Some(&diagnostics_overrides),
            },
        );

        let mut diags: Vec<_> = diags
            .into_iter()
            .filter(|d| dedup_pre_loaded_alias_collision(d, &mut seen_alias_collisions))
            .collect();

        // ADR 0127 §3 / BT-3665: a diagnostic in a flattened trait provision
        // has a span in the *protocol's* file, so it is held back and
        // published once, in that file, after the loop. An `@expect` in the
        // protocol's provided method suppresses it.
        diagnostics_policy::apply_protocol_expect_directives(&protocol_defs, &mut diags);
        let (provisions, own): (Vec<_>, Vec<_>) =
            diags.into_iter().partition(|d| d.provision.is_some());
        provision_diags.extend(provisions);

        on_file(FileLint {
            path: &target.path,
            source: &target.source,
            module: &target.module,
            class_hierarchy: &class_hierarchy,
            diagnostics: own,
        });
    }

    dedupe_provision_diagnostics(provision_diags)
        .into_iter()
        .filter_map(|diagnostic| {
            let (path, source) = provision_report_target(&diagnostic, &protocol_sources)?;
            Some(ProvisionDiagnostic {
                path,
                source: source.to_string(),
                diagnostic,
            })
        })
        .collect()
}

/// The `beamtalk lint` verdict, tallied over every reported diagnostic.
///
/// Plain `beamtalk lint` fails on Lint-severity diagnostics (`.erl` lint
/// findings included). `--warnings-as-errors` also fails on the
/// live-recheck set a REPL `load-project` rejects
/// ([`diagnostics_policy::is_live_recheck_failure`], BT-4334).
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct LintGate {
    /// Lint-severity diagnostics.
    pub lint: usize,
    /// Live-recheck failures: any `Error` plus non-error `Dnu`/`Type`.
    pub live_recheck: usize,
}

impl LintGate {
    /// Count one `.bt` diagnostic.
    pub fn count(&mut self, diag: &Diagnostic) {
        if diag.severity == Severity::Lint {
            self.lint += 1;
        }
        if diagnostics_policy::is_live_recheck_failure(diag) {
            self.live_recheck += 1;
        }
    }

    /// Count `n` `.erl` lint findings (all Lint-severity).
    pub fn count_erlang(&mut self, n: usize) {
        self.lint += n;
    }

    /// Whether the run fails: on any Lint diagnostic, and with
    /// `warnings_as_errors` on any live-recheck failure too.
    #[must_use]
    pub fn fails(&self, warnings_as_errors: bool) -> bool {
        self.lint > 0 || (warnings_as_errors && self.live_recheck > 0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn package_with(files: &[(&str, &str)]) -> (tempfile::TempDir, Utf8PathBuf) {
        let temp = tempfile::TempDir::new().unwrap();
        // Canonical root: the pipeline reports the canonical spelling of
        // package files (`/private/var/...` on macOS, `\\?\C:\...` with
        // long names on Windows), so expected paths must start from it.
        let root = Utf8PathBuf::from_path_buf(std::fs::canonicalize(temp.path()).unwrap()).unwrap();
        std::fs::write(
            root.join("beamtalk.toml"),
            "[package]\nname = \"pkg\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        std::fs::create_dir_all(root.join("src")).unwrap();
        for (name, text) in files {
            std::fs::write(root.join("src").join(name), text).unwrap();
        }
        (temp, root)
    }

    /// `stubs/` stays excluded when the package is reached through a
    /// non-canonical path (here a symlink; on macOS `/var` vs
    /// `/private/var`, on Windows an 8.3 name). The walk yields the
    /// caller's spelling and `find_package_root` the canonical one, so a
    /// plain `starts_with` let the stubs in as lint targets.
    #[cfg(unix)]
    #[test]
    fn collect_lint_files_excludes_stubs_via_symlinked_root() {
        let (_temp, root) = package_with(&[("a.bt", "Object subclass: A\n")]);
        std::fs::create_dir_all(root.join("stubs")).unwrap();
        std::fs::write(root.join("stubs").join("s.bt"), "class Foo { @@@@ }").unwrap();
        let link_dir = tempfile::TempDir::new().unwrap();
        let link = Utf8PathBuf::from_path_buf(link_dir.path().join("pkg")).unwrap();
        std::os::unix::fs::symlink(root.as_std_path(), link.as_std_path()).unwrap();

        let (files, _) = collect_lint_files(&link, link.as_str()).unwrap();
        assert!(
            files.iter().all(|f| !f.as_str().contains("stubs")),
            "stubs/ must not be lint targets: {files:?}"
        );
        assert!(
            files.iter().any(|f| f.as_str().ends_with("a.bt")),
            "{files:?}"
        );
    }

    #[test]
    fn complete_package_parse_is_project_complete() {
        let (_temp, root) = package_with(&[("a.bt", "Object subclass: A\n")]);
        let target = root.join("src").join("a.bt");
        let sources = parse_package_sources(&root, &[target], |f, _, e| panic!("{f}: {e}"));
        assert_eq!(
            sources.scope.knowledge_scope,
            KnowledgeScope::ProjectComplete
        );
        assert_eq!(sources.targets.len(), 1);
    }

    /// An unreadable package file leaves Pass 1 without some of the
    /// package's classes, so the scope must not claim `ProjectComplete`
    /// (MCP `lint` reports the file and carries on; BT-4585 review).
    #[test]
    fn unreadable_package_file_drops_scope_to_module_only() {
        let (_temp, root) = package_with(&[("a.bt", "Object subclass: A\n")]);
        // Not UTF-8, so `read_to_string` fails even for root (which
        // bypasses permission bits).
        std::fs::write(root.join("src").join("b.bt"), [0xff, 0xfe, 0xfd]).unwrap();
        let target = root.join("src").join("a.bt");
        let mut unreadable = Vec::new();
        let sources = parse_package_sources(&root, &[target], |f, is_target, _| {
            unreadable.push((f.to_path_buf(), is_target));
        });
        assert_eq!(
            unreadable,
            vec![(root.join("src").join("b.bt"), false)],
            "the dangling file should be reported as an unreadable non-target"
        );
        assert_eq!(sources.scope.knowledge_scope, KnowledgeScope::ModuleOnly);
        assert_eq!(sources.targets.len(), 1);
    }
}
