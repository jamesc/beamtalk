// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Beamtalk source → Core Erlang front end: analysis and code generation.
//!
//! **DDD Context:** Compilation
//!
//! `CompileContext` and `ClassHierarchyContext` carry the cross-file inputs;
//! `compile_source_with_bindings` runs lex → parse → analyse → codegen for one
//! file and `write_core_erlang_with_bindings` writes the result. This is the
//! front-end half of the former `beam_compiler.rs` (BT-4700); the escript /
//! `BeamCompiler` driver stays in the parent module. Behaviour is unchanged and
//! every item is re-exported from `beam_compiler` under its old path.

use super::{validate_module_name, write_core_erlang_bytes};
use beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry;
use camino::Utf8Path;
use miette::{Context, IntoDiagnostic, Result};
use tracing::{debug, error, instrument};

/// Cross-file class hierarchy data needed during compilation.
///
/// Groups the three class hierarchy indexes that are threaded through the
/// compilation pipeline — eliminating repeated parameter triples in
/// `compile_file`, `compile_source_with_bindings`, and
/// `write_core_erlang_with_bindings`.
///
/// **DDD Context:** Compilation
#[derive(Debug, Clone, Default)]
pub struct ClassHierarchyContext {
    /// Maps Beamtalk class names to their compiled Erlang module names.
    /// E.g., `"AppSup"` → `"bt@my_app@supervision@app_sup"`.
    pub class_module_index: std::collections::HashMap<String, String>,
    /// Maps class names to their direct superclass names.
    /// E.g., `"MyChild"` → `"MyParent"`.
    pub class_superclass_index: std::collections::HashMap<String, String>,
    /// Full `ClassInfo` entries from all source files in the compilation unit.
    /// Injected into the type checker's hierarchy during Pass 2 so cross-file
    /// method resolution works without reading BEAM files.
    pub pre_loaded_classes: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    /// Full `ProtocolInfo` entries from other source files in the compilation
    /// unit (e.g. `BUnit` fixtures). Seeded into the protocol registry during
    /// semantic analysis so the unresolved-class validator and type checker
    /// recognise protocol names defined outside the current module.
    pub pre_loaded_protocols:
        Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo>,
    /// Full ASTs of carried protocols from other source files in
    /// the compilation unit, or from a resolved dependency (ADR 0127 §10a;
    /// BT-3591) — the trait-flattening counterpart to `pre_loaded_protocols`
    /// above. `pre_loaded_protocols` carries name/signature metadata only,
    /// enough for `extending:`/conformance resolution; flattening a
    /// cross-file or cross-package `uses:` needs the provided methods'
    /// actual bodies, which only live here.
    pub pre_loaded_protocol_defs: Vec<beamtalk_core::ast::ProtocolDefinition>,
    /// Source file (path + text) each protocol in `pre_loaded_protocol_defs`
    /// was parsed from (ADR 0127 §3, "Source locations"; BT-3625). Codegen
    /// maps a flattened provision's spans through this so BEAM line
    /// annotations and stack traces point at the protocol file rather than
    /// at the using class's own file.
    pub pre_loaded_protocol_sources: beamtalk_core::semantic_analysis::ProtocolSourceMap,
    /// Type alias declarations (`type Name = ...`) from other source files in
    /// the same package (ADR 0108). Seeded into the `AliasRegistry`
    /// during semantic analysis so a cross-file alias reference resolves
    /// through `resolve_type_annotation` instead of staying an opaque,
    /// unresolved name. Structurally a `ctx.hierarchy.*` field like
    /// `pre_loaded_protocols` above, but populated differently in practice:
    /// the CLI build path (`execute_build_passes`) leaves `pre_loaded_protocols`
    /// empty while fully populating `pre_loaded_aliases` from every source
    /// file in the compilation unit, including the file being compiled —
    /// `analyse_full`'s merge order lets the module's own declaration take
    /// precedence over its duplicate pre-loaded entry.
    pub pre_loaded_aliases: Vec<beamtalk_core::semantic_analysis::alias_registry::AliasInfo>,
    /// Project-wide standalone extension definitions from Pass 1.
    /// Registered into each file's class hierarchy during Pass 2 so
    /// same-project cross-file extensions resolve instead of producing
    /// false `Dnu` hints (ADR 0066 / ADR 0100 Rule 2 WS1).
    pub extension_index: beamtalk_core::compilation::extension_index::ExtensionIndex,
    /// Whether `extension_index` holds every dependency's extension methods
    /// (ADR 0070 WS3, BT-4608) — loaded completely by a dependency resolution
    /// that succeeded. Analysis lifts the pre-WS3 dependency guard
    /// (`CompilerOptions::dependency_extensions_unknown`, which callers set
    /// conservatively to "the package has dependencies") only when this
    /// holds; the default `false` keeps it on.
    ///
    /// **Precondition:** set it `true` only after dependency resolution
    /// *succeeded* and every dependency's `DependencyExtensions` was complete
    /// — `analyse_source_with_bindings` passes `resolved = true` to the
    /// shared rule on the strength of this field alone.
    pub dependency_extensions_complete: bool,
}

/// Compilation context bundling hierarchy data with dependency resolution settings.
///
/// Groups the `ClassHierarchyContext`, optional dependency registry, and
/// strict-deps flag that are threaded through `compile_file` and
/// `compile_source_with_bindings`. Avoids passing these as separate parameters.
///
/// **DDD Context:** Compilation
#[derive(Debug, Default)]
pub struct CompileContext<'a> {
    /// Cross-file class hierarchy indexes.
    pub hierarchy: ClassHierarchyContext,
    /// Optional dependency registry for cross-package collision detection.
    pub dep_registry: Option<&'a beamtalk_core::semantic_analysis::DependencyRegistry>,
    /// Whether to promote transitive dependency usage warnings to errors.
    pub strict_deps: bool,
    /// Native type registry for FFI call inference during build (ADR 0075).
    ///
    /// When set, semantic analysis threads it through
    /// [`beamtalk_core::semantic_analysis::AnalysisContext::with_native_type_registry`]
    /// so that `Erlang <module> <function>:` calls get return type inference and
    /// argument type checking in build output, not just the LSP.
    pub native_type_registry: Option<std::sync::Arc<NativeTypeRegistry>>,
    /// Per-category diagnostic severity overrides from `beamtalk.toml`'s
    /// `[diagnostics]` section (ADR 0100 Rule 3). Empty when the package has
    /// no manifest or no `[diagnostics]` section — absence preserves today's
    /// Rule 1 completeness-ladder defaults.
    ///
    /// An unchanged file's diagnostics, replayed from `build.rs`'s
    /// diagnostics sidecar instead of recompiled, keep whatever
    /// `diagnostics_overrides` (and `suppress_warnings`/`warnings_as_errors`)
    /// were in effect the last time that file was *actually* compiled.
    /// Editing `beamtalk.toml`'s `[diagnostics]` table, or passing different
    /// warning flags, without touching any `.bt` source therefore doesn't
    /// re-filter already-unchanged files until they're next recompiled
    /// (content change or `--force`) — a known, narrow limitation of the
    /// replay path, not a bug in this field itself.
    pub diagnostics_overrides: crate::commands::manifest::DiagnosticsTable,
    /// Where diagnostics that lie in a flattened trait provision's protocol
    /// file are reported (ADR 0127 §3). Disabled by default (each file
    /// prints its own immediately); a whole-package build enables it so a
    /// provision shared by many users is reported once across all of them.
    pub provision_sink: ProvisionSink,
}

/// Collects flattened-provision diagnostics across the files of one build so
/// they can be reported once, against the protocol's file (ADR 0127 §3,
/// "Source locations"). See [`CompileContext::provision_sink`].
#[derive(Debug, Default)]
pub struct ProvisionSink {
    collected: Option<std::sync::Mutex<Vec<beamtalk_core::source_analysis::Diagnostic>>>,
}

impl ProvisionSink {
    /// A sink that accumulates until [`Self::flush`].
    #[must_use]
    pub fn collecting() -> Self {
        Self {
            collected: Some(std::sync::Mutex::new(Vec::new())),
        }
    }

    /// Reports the provision-tagged diagnostics among `diagnostics`: held
    /// until [`Self::flush`] when collecting, printed now otherwise.
    pub fn report(
        &self,
        diagnostics: &[beamtalk_core::source_analysis::Diagnostic],
        protocol_sources: &beamtalk_core::semantic_analysis::ProtocolSourceMap,
        options: &beamtalk_core::CompilerOptions,
    ) {
        let tagged = diagnostics
            .iter()
            .filter(|d| d.provision.is_some())
            .cloned();
        if let Some(collected) = &self.collected {
            collected
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .extend(tagged);
        } else {
            crate::diagnostic::print_provision_diagnostics(tagged, protocol_sources, options);
        }
    }

    /// Takes everything collected so far, leaving the sink empty (always
    /// empty for a non-collecting sink).
    pub(crate) fn drain(&self) -> Vec<beamtalk_core::source_analysis::Diagnostic> {
        self.collected.as_ref().map_or_else(Vec::new, |collected| {
            std::mem::take(
                &mut *collected
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner),
            )
        })
    }

    /// Prints everything collected so far (merged across users) and empties
    /// the sink. A no-op for a non-collecting sink.
    pub fn flush(
        &self,
        protocol_sources: &beamtalk_core::semantic_analysis::ProtocolSourceMap,
        options: &beamtalk_core::CompilerOptions,
    ) {
        let taken = self.drain();
        crate::diagnostic::print_provision_diagnostics(taken, protocol_sources, options);
    }
}

/// Writes Core Erlang code with primitive bindings.
///
/// ADR 0007 Phase 3: Same as [`write_core_erlang`] but accepts
/// a binding table for pragma-driven dispatch.
///
/// Returns the `ThreadedIr` verifier diagnostics (ADR 0111 amendment, BT-3724)
/// — warnings only; the file is written regardless. The caller prints them.
///
/// # Errors
///
/// Returns an error if the module name is invalid, code generation fails,
/// or the output file cannot be written.
#[allow(clippy::too_many_arguments)]
pub fn write_core_erlang_with_bindings(
    module: &beamtalk_core::ast::Module,
    module_name: &str,
    output_path: &Utf8Path,
    options: &beamtalk_core::CompilerOptions,
    bindings: &beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable,
    hierarchy: &ClassHierarchyContext,
    source: Option<(&str, Option<&str>)>,
    native_type_registry: Option<std::sync::Arc<NativeTypeRegistry>>,
    analysis: Option<beamtalk_core::semantic_analysis::AnalysisResult>,
) -> Result<Vec<beamtalk_core::source_analysis::Diagnostic>> {
    validate_module_name(module_name)?;

    let (source_text, source_path) = match source {
        Some((text, path)) => (Some(text), path),
        None => (None, None),
    };
    let mut codegen_options = beamtalk_codegen::core_erlang::CodegenOptions::new(module_name)
        .with_bindings(bindings.clone())
        .with_source_opt(source_text)
        .with_stdlib_mode(options.stdlib_mode)
        .with_class_module_index(hierarchy.class_module_index.clone())
        .with_class_superclass_index(hierarchy.class_superclass_index.clone())
        .with_source_path_opt(source_path)
        .with_class_hierarchy(hierarchy.pre_loaded_classes.clone())
        // Thread cross-module type aliases through to codegen so
        // an alias-typed annotation referencing a `type Name = ...` from
        // another module in the same compilation unit resolves to a
        // `user_type` reference in generated `-spec`/`-type` attributes
        // instead of falling through to `any()` — mirrors
        // `pre_loaded_classes` immediately above.
        .with_pre_loaded_aliases(hierarchy.pre_loaded_aliases.clone())
        .with_protocol_sources(hierarchy.pre_loaded_protocol_sources.clone())
        .with_native_type_registry(native_type_registry)
        // ADR 0098 Phase 3: bake the producing-toolchain identity into __beamtalk_meta.
        .with_provenance(
            env!("BEAMTALK_VERSION"),
            crate::commands::build_stamp::current_otp_version(),
        );
    // Thread the driver's already-computed semantic analysis (class
    // hierarchy, semantic facts, inferred method return types) into codegen
    // when the caller ran `analyse_full` itself (via
    // `compute_project_diagnostics_with_analysis`) — avoids codegen re-deriving
    // all three from scratch, including a second full type-checking pass.
    if let Some(analysis) = analysis {
        codegen_options = codegen_options.with_analysis(analysis);
    }
    write_core_erlang_with_options(module, codegen_options, output_path)
}

/// Generates Core Erlang with fully prepared `codegen_options` and writes it,
/// returning only the `ThreadedIr` verifier diagnostics (warnings; the output
/// is written regardless — ADR 0111 amendment, BT-3724).
///
/// Split out of [`write_core_erlang_with_bindings`] so a test can inject a
/// verifier violation through the options.
fn write_core_erlang_with_options(
    module: &beamtalk_core::ast::Module,
    codegen_options: beamtalk_codegen::core_erlang::CodegenOptions,
    output_path: &Utf8Path,
) -> Result<Vec<beamtalk_core::source_analysis::Diagnostic>> {
    let (core_erlang, verifier_diagnostics) =
        beamtalk_codegen::core_erlang::generate_module_surfacing_verifier(module, codegen_options)
            .into_diagnostic()
            .wrap_err("Failed to generate Core Erlang")?;

    write_core_erlang_bytes(&core_erlang, output_path)?;
    Ok(verifier_diagnostics)
}

/// Compiles a Beamtalk source file (.bt) to Core Erlang (.core).
///
/// This is the single compilation domain service used by all CLI commands
/// (build, build-stdlib). It handles the full pipeline:
/// read source → lex → parse → validate → report diagnostics → generate Core Erlang.
///
/// # Arguments
///
/// * `source_path` - Path to the `.bt` source file
/// * `module_name` - Module name for the generated Core Erlang
/// * `core_output` - Path where the `.core` file should be written
/// * `options` - Compiler options (stdlib mode, primitive handling)
///
/// # Errors
///
/// Returns an error if reading, parsing, or code generation fails,
/// or if any diagnostic has error severity.
#[instrument(skip_all, fields(path = %source_path, module = module_name))]
pub fn compile_source(
    source_path: &Utf8Path,
    module_name: &str,
    core_output: &Utf8Path,
    options: &beamtalk_core::CompilerOptions,
) -> Result<()> {
    compile_source_with_bindings(
        source_path,
        module_name,
        core_output,
        options,
        &beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable::new(),
        &CompileContext::default(),
        None,
    )
    .map(|_diags| ())
}

/// Whether `diagnostics` fail the build/check gate: any error-severity
/// diagnostic, or — under `warnings_as_errors` — any warning/hint.
///
/// Deprecation-category warnings and structural validation warnings
/// (`UnresolvedClass`, `UnresolvedFfi`, `ArityMismatch`) are excluded from
/// warnings-as-errors because they can produce false positives when compiling
/// single files that reference classes, FFI modules, or arities defined in
/// other compilation units.
///
/// ADR 0100 Rule 3: that exclusion is itself the Rule 1 default
/// for those categories — an explicit `[diagnostics]` table entry for one
/// of them is a deliberate, package-level opt back in to promotion, so it
/// wins over the exclusion (per the "explicit table value wins over the
/// exclusion" precedence note).
///
/// Shared by [`compile_source_with_bindings`] (`beamtalk build`) and `beamtalk
/// check` so both surfaces fail on exactly the same diagnostics.
pub(crate) fn diagnostics_are_errors(
    diagnostics: &[beamtalk_core::source_analysis::Diagnostic],
    options: &beamtalk_core::CompilerOptions,
    diagnostics_overrides: &crate::commands::manifest::DiagnosticsTable,
) -> bool {
    diagnostics.iter().any(|d| {
        d.severity == beamtalk_core::source_analysis::Severity::Error
            || (options.warnings_as_errors
                && matches!(
                    d.severity,
                    beamtalk_core::source_analysis::Severity::Warning
                        | beamtalk_core::source_analysis::Severity::Hint
                )
                && (!matches!(
                    d.category,
                    Some(
                        beamtalk_core::source_analysis::DiagnosticCategory::Deprecation
                            | beamtalk_core::source_analysis::DiagnosticCategory::UnresolvedClass
                            | beamtalk_core::source_analysis::DiagnosticCategory::UnresolvedFfi
                            | beamtalk_core::source_analysis::DiagnosticCategory::ArityMismatch
                    )
                ) || d
                    .category
                    .is_some_and(|c| diagnostics_overrides.contains_key(&c))))
    })
}

/// Run lexing, parsing, `@primitive` validation, and the unified
/// semantic-analysis diagnostic pipeline for one source file, printing any
/// diagnostics.
///
/// This is the analysis front-half of [`compile_source_with_bindings`],
/// extracted so `beamtalk check` runs byte-identical checker diagnostics
/// without emitting bytecode: `build` feeds the returned `Module` +
/// `AnalysisResult` into Core Erlang codegen, while `check` stops here.
/// `cached_ast` reuses Pass 1's pre-parsed source + `Module` when available,
/// exactly as the build path does.
pub(crate) fn analyse_source_with_bindings(
    source_path: &Utf8Path,
    options: &beamtalk_core::CompilerOptions,
    ctx: &CompileContext<'_>,
    cached_ast: Option<crate::commands::build::CachedAst>,
) -> Result<(
    String,
    beamtalk_core::ast::Module,
    Vec<beamtalk_core::source_analysis::Diagnostic>,
    beamtalk_core::semantic_analysis::AnalysisResult,
)> {
    // Reuse pre-parsed source + AST from Pass 1 when available,
    // otherwise read and parse from disk (single-file mode, REPL, etc.).
    let (source, module, mut diagnostics) = if let Some(cached) = cached_ast {
        debug!("Using cached AST from Pass 1 for '{}'", source_path);
        // Reuse parse diagnostics from Pass 1 so syntax errors are still reported.
        (cached.source, cached.module, cached.diagnostics)
    } else {
        // Read source file
        let source = std::fs::read_to_string(source_path)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to read file '{source_path}'"))?;

        // Lex and parse
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
        let (module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);
        (source, module, diagnostics)
    };

    // Run @primitive validation (ADR 0007)
    let primitive_diags =
        beamtalk_core::semantic_analysis::primitive_validator::validate_primitives(
            &module, options,
        );
    diagnostics.extend(primitive_diags);

    // Unified post-analysis diagnostic pipeline.
    // Semantic analysis + all post-analysis passes + @expect suppression are now
    // handled by a single function shared between CLI and LSP.
    let cross_file_classes =
        beamtalk_core::semantic_analysis::ClassHierarchy::cross_file_class_infos(
            &ctx.hierarchy.pre_loaded_classes,
            &module,
        );
    let diag_ctx =
        beamtalk_language_service::queries::diagnostic_provider::ProjectDiagnosticContext {
            // The pre-WS3 dependency guard: `options` says whether the
            // package has dependencies; the hierarchy says whether all their
            // extensions are in `extension_index` (ADR 0070 WS3, BT-4608).
            options: beamtalk_core::CompilerOptions {
                dependency_extensions_unknown:
                    beamtalk_cli::dependency_extensions::dependency_extensions_unknown(
                        options.dependency_extensions_unknown,
                        true,
                        ctx.hierarchy.dependency_extensions_complete,
                    ),
                ..options.clone()
            },
            cross_file_classes,
            pre_loaded_protocols: ctx.hierarchy.pre_loaded_protocols.clone(),
            pre_loaded_protocol_defs: ctx.hierarchy.pre_loaded_protocol_defs.clone(),
            pre_loaded_protocol_sources: ctx.hierarchy.pre_loaded_protocol_sources.clone(),
            // Cross-file/package type aliases from Pass 1 — see
            // `ClassHierarchyContext::pre_loaded_aliases`'s doc. `analyse_full`
            // filters out any name the current module redeclares itself, so no
            // "current file's own aliases" pre-filter is needed here (mirrors
            // `pre_loaded_protocols` immediately above).
            pre_loaded_aliases: ctx.hierarchy.pre_loaded_aliases.clone(),
            cross_file_extensions: ctx.hierarchy.extension_index.clone(),
            native_type_registry: ctx.native_type_registry.clone(),
            dep_registry: ctx.dep_registry,
            strict_deps: ctx.strict_deps,
            // ADR 0100 Rule 3: `compute_project_diagnostics`
            // applies this table itself (after `@expect` suppression, before
            // returning) — the single shared pipeline both the CLI and the LSP
            // call, so severity can never drift between the two surfaces.
            diagnostics_overrides: ctx.diagnostics_overrides.clone(),
            // This compile pipeline never reaches a `stubs/`
            // file — `find_source_files`/`collect_lint_files` exclude
            // `stubs/` from the walk that feeds it, and `declare native:`
            // stub declarations are parsed separately by
            // `load_project_stub_registry`, never through here.
            is_stub_file: false,
            // File basename (without extension), so the shared
            // pipeline can validate it agrees with the class declared here —
            // see `ProjectDiagnosticContext::source_file_stem`'s doc.
            source_file_stem: source_path
                .file_stem()
                .map(std::string::ToString::to_string),
        };
    // Capture the `AnalysisResult` this pipeline's `analyse_full`
    // call already produced, so it can be handed to codegen below instead of
    // codegen re-deriving the class hierarchy, semantic facts, and inferred
    // method return types from scratch (a second full type-checking pass).
    let (new_diagnostics, analysis_result) =
        beamtalk_language_service::queries::diagnostic_provider::compute_project_diagnostics_with_analysis(
            &module,
            &source,
            diagnostics,
            &diag_ctx,
        );
    diagnostics = new_diagnostics;
    // ADR 0127 §3: an `@expect` written in a protocol's provided method
    // suppresses that provision's diagnostic here, on the using file's copy,
    // before it is cached, counted or published in the protocol's file.
    beamtalk_core::compilation::diagnostics_policy::apply_protocol_expect_directives(
        &ctx.hierarchy.pre_loaded_protocol_defs,
        &mut diagnostics,
    );

    if !diagnostics.is_empty() {
        debug!(
            diagnostic_count = diagnostics.len(),
            "Found diagnostics during analysis"
        );
        crate::diagnostic::print_diagnostics_text(
            &diagnostics,
            source_path.as_str(),
            &source,
            options,
        );
        ctx.provision_sink.report(
            &diagnostics,
            &ctx.hierarchy.pre_loaded_protocol_sources,
            options,
        );
    }

    debug!("Parsed successfully: {}", source_path);

    Ok((source, module, diagnostics, analysis_result))
}

/// Compiles a Beamtalk source file to Core Erlang with primitive bindings.
///
/// ADR 0007 Phase 3: Same as [`compile_source`] but accepts a
/// [`PrimitiveBindingTable`] for pragma-driven dispatch.
///
/// # Errors
///
/// Returns an error if reading, parsing, or code generation fails,
/// or if any diagnostic has error severity.
#[allow(clippy::too_many_lines)]
#[instrument(skip_all, fields(path = %source_path, module = module_name))]
pub(crate) fn compile_source_with_bindings(
    source_path: &Utf8Path,
    module_name: &str,
    core_output: &Utf8Path,
    options: &beamtalk_core::CompilerOptions,
    bindings: &beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable,
    ctx: &CompileContext<'_>,
    cached_ast: Option<crate::commands::build::CachedAst>,
) -> Result<Vec<beamtalk_core::source_analysis::Diagnostic>> {
    debug!("Compiling module '{}' with bindings", module_name);

    // Analysis (lex/parse, `@primitive` validation, semantic analysis +
    // post-analysis passes, `@expect` suppression, diagnostic printing) is
    // shared with `beamtalk check` — see [`analyse_source_with_bindings`].
    let (source, mut module, diagnostics, analysis_result) =
        analyse_source_with_bindings(source_path, options, ctx, cached_ast)?;

    // Fail on the same diagnostics `beamtalk check` fails on — see
    // [`diagnostics_are_errors`]. Diagnostics were already printed by
    // [`analyse_source_with_bindings`].
    if diagnostics_are_errors(&diagnostics, options, &ctx.diagnostics_overrides) {
        error!("Compilation failed for '{}'", source_path);
        miette::bail!("Failed to compile '{source_path}'");
    }

    // Save diagnostics for the caller to build a summary.
    let mut returned_diags = diagnostics;

    // Generate Core Erlang (with source text for CompiledMethod introspection, and bindings)
    // Use an absolute path so reload works regardless of the
    // working directory at reload time. Always pass the source path so that
    // line annotations and the 'file' attribute populate BEAM stacktraces
    // (even for stdlib). The `beamtalk_source` attribute is suppressed for
    // stdlib mode in codegen (source_path_attr checks stdlib_mode).
    let embed_source_path = std::fs::canonicalize(source_path)
        .ok()
        .and_then(|p| p.into_os_string().into_string().ok())
        .unwrap_or_else(|| source_path.as_str().to_string());
    let embed_source_path = Some(embed_source_path.as_str());
    // Build a codegen-specific hierarchy with every pre-loaded class,
    // *unfiltered*: unlike analysis's `cross_file_classes`, codegen also needs
    // the entries sharing a name with this file's own classes, because a
    // dependency exporting the same name is a runtime collision that keeps
    // sealed-Value sends to it dynamic (BT-4151). Folding them into the
    // hierarchy is still a no-op — codegen's `add_from_beam_meta` only fills
    // vacant names, and this file's classes already occupy theirs.
    // `pre_loaded_aliases` is likewise unfiltered — it already includes the current file's own aliases (see
    // `ClassHierarchyContext::pre_loaded_aliases`'s doc), and codegen's merge
    // (`AliasRegistry::from_module_declarations_with_pre_loaded`) lets the
    // module's own declaration take precedence over its duplicate pre-loaded
    // entry, mirroring how semantic analysis's `AnalysisContext::pre_loaded_aliases`
    // already handles the same list.
    let codegen_hierarchy = ClassHierarchyContext {
        class_module_index: ctx.hierarchy.class_module_index.clone(),
        class_superclass_index: ctx.hierarchy.class_superclass_index.clone(),
        pre_loaded_classes: ctx.hierarchy.pre_loaded_classes.clone(),
        pre_loaded_aliases: ctx.hierarchy.pre_loaded_aliases.clone(),
        pre_loaded_protocol_sources: ctx.hierarchy.pre_loaded_protocol_sources.clone(),
        ..ClassHierarchyContext::default()
    };
    // Prepare the AST (inferred return types, supervisor_kind,
    // class_kind) at the driver boundary, using the same `AnalysisResult`
    // handed off to codegen below via `Some(analysis_result)` — nothing
    // mutates `module` between `compute_project_diagnostics_with_analysis`
    // above and here, so `analysis_result` is always trustworthy (mirrors
    // the "guaranteed no-op" reasoning in the `with_analysis` comment below).
    // Codegen no longer schedules this writeback itself for a handed-off,
    // still-trustworthy analysis — see `generate_module_with_warnings`.
    beamtalk_core::semantic_analysis::lower_analysed_module_for_codegen(
        &mut module,
        &analysis_result,
    );
    let verifier_diagnostics = write_core_erlang_with_bindings(
        &module,
        module_name,
        core_output,
        options,
        bindings,
        &codegen_hierarchy,
        Some((&source, embed_source_path)),
        ctx.native_type_registry.clone(),
        // Hand off the `AnalysisResult` this function's own
        // `compute_project_diagnostics_with_analysis` call above already
        // computed — `codegen_hierarchy`'s `pre_loaded_classes` above is
        // `cross_file_classes` (this analysis's
        // `AnalysisContext::pre_loaded_classes`) plus only entries named like
        // this file's own classes, so codegen's `add_from_beam_meta` call is
        // a guaranteed no-op and this handoff always skips the second
        // type-checking pass for this driver.
        Some(analysis_result),
    )
    .wrap_err_with(|| format!("Failed to generate Core Erlang for '{source_path}'"))?;

    // ADR 0111 amendment (BT-3724): `internal:` verifier findings are
    // warnings raised after the error gate above, so they never fail a build
    // (not even under `--warnings-as-errors`); they are printed and returned
    // for the caller's summary like any other diagnostic.
    if !verifier_diagnostics.is_empty() {
        crate::diagnostic::print_diagnostics_text(
            &verifier_diagnostics,
            source_path.as_str(),
            &source,
            options,
        );
        returned_diags.extend(verifier_diagnostics);
    }

    debug!("Generated Core Erlang: {}", core_output);
    Ok(returned_diags)
}

#[cfg(test)]
mod tests {
    use super::*;
    use camino::Utf8PathBuf;
    use std::fs;
    use tempfile::TempDir;

    /// BT-3724 (ADR 0111 amendment): an injected `ThreadedIr` verifier
    /// violation reaches the CLI build path as an `internal:` warning, and the
    /// build still succeeds and writes its output.
    #[test]
    fn injected_verifier_violation_is_a_warning_and_build_succeeds() {
        use beamtalk_core::source_analysis::{DiagnosticCategory, Severity};

        let temp = TempDir::new().unwrap();
        let output_path = Utf8PathBuf::from_path_buf(temp.path().join("probe.core")).unwrap();
        let tokens =
            beamtalk_core::source_analysis::lex_with_eof("Object subclass: Probe\n  go => 1\n");
        let (module, _) = beamtalk_core::source_analysis::parse(tokens);

        let options = beamtalk_codegen::core_erlang::CodegenOptions::new("probe");
        let clean = write_core_erlang_with_options(&module, options.clone(), &output_path).unwrap();
        assert!(
            clean.is_empty(),
            "valid program emits no new diagnostics: {clean:?}"
        );

        let diags = write_core_erlang_with_options(
            &module,
            options.with_injected_verifier_violation(),
            &output_path,
        )
        .expect("a verifier finding must not fail the build");
        assert_eq!(diags.len(), 1, "{diags:?}");
        assert_eq!(diags[0].severity, Severity::Warning);
        assert_eq!(
            diags[0].category,
            Some(DiagnosticCategory::InternalVerifier)
        );
        assert!(diags[0].message.starts_with("internal: "));
        assert!(output_path.exists());
    }

    #[test]
    fn test_compile_source_valid() {
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let source_file = temp_path.join("test.bt");
        let core_file = temp_path.join("test.core");
        fs::write(&source_file, "test := [1 + 2].").unwrap();

        let options = beamtalk_core::CompilerOptions::default();
        let result = compile_source(&source_file, "test", &core_file, &options);

        assert!(result.is_ok());
        assert!(core_file.exists());
        let content = fs::read_to_string(core_file).unwrap();
        assert!(content.contains("module 'test'"));
    }

    #[test]
    fn test_compile_source_syntax_error() {
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let source_file = temp_path.join("bad.bt");
        let core_file = temp_path.join("bad.core");
        fs::write(&source_file, "test := [1 + ].").unwrap();

        let options = beamtalk_core::CompilerOptions::default();
        let result = compile_source(&source_file, "bad", &core_file, &options);

        assert!(result.is_err());
    }

    #[test]
    fn test_compile_source_missing_file() {
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let source_file = temp_path.join("nonexistent.bt");
        let core_file = temp_path.join("nonexistent.core");

        let options = beamtalk_core::CompilerOptions::default();
        let result = compile_source(&source_file, "nonexistent", &core_file, &options);

        assert!(result.is_err());
    }

    #[test]
    fn test_compile_source_sealed_class_violation() {
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let source = "Integer subclass: MyInt\n  doubled => self * 2\n";
        let source_file = temp_path.join("sealed.bt");
        let core_file = temp_path.join("sealed.core");
        fs::write(&source_file, source).unwrap();

        // Verify parse and primitive validation produce no errors —
        // so compile_source failure must come from semantic analysis.
        let tokens = beamtalk_core::source_analysis::lex_with_eof(source);
        let (module, parse_diags) = beamtalk_core::source_analysis::parse(tokens);
        assert!(
            !parse_diags
                .iter()
                .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error),
            "Source should parse without errors"
        );
        let options = beamtalk_core::CompilerOptions::default();
        let prim_diags = beamtalk_core::semantic_analysis::primitive_validator::validate_primitives(
            &module, &options,
        );
        assert!(
            !prim_diags
                .iter()
                .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error),
            "Primitive validation should produce no errors"
        );

        let result = compile_source(&source_file, "sealed", &core_file, &options);
        assert!(
            result.is_err(),
            "Sealed class violation should fail compilation via semantic analysis"
        );
    }

    #[test]
    fn test_compile_source_sealed_method_override() {
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let source = "Object subclass: Parent\n  sealed frozen => 42\n\nParent subclass: Child\n  frozen => 99\n";
        let source_file = temp_path.join("sealed_method.bt");
        let core_file = temp_path.join("sealed_method.core");
        fs::write(&source_file, source).unwrap();

        // Verify parse and primitive validation produce no errors —
        // so compile_source failure must come from semantic analysis.
        let tokens = beamtalk_core::source_analysis::lex_with_eof(source);
        let (module, parse_diags) = beamtalk_core::source_analysis::parse(tokens);
        assert!(
            !parse_diags
                .iter()
                .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error),
            "Source should parse without errors"
        );
        let options = beamtalk_core::CompilerOptions::default();
        let prim_diags = beamtalk_core::semantic_analysis::primitive_validator::validate_primitives(
            &module, &options,
        );
        assert!(
            !prim_diags
                .iter()
                .any(|d| d.severity == beamtalk_core::source_analysis::Severity::Error),
            "Primitive validation should produce no errors"
        );

        let result = compile_source(&source_file, "sealed_method", &core_file, &options);
        assert!(
            result.is_err(),
            "Sealed method override should fail compilation via semantic analysis"
        );
    }

    /// Source that triggers an actor-new Error (using `new` on an Actor subclass).
    /// Promoted from warning to error.
    const ACTOR_NEW_SOURCE: &str =
        "Actor subclass: TestActorWarnings\n  doNothing => nil\n\nTestActorWarnings new";

    #[test]
    fn test_actor_new_is_compile_error() {
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("actor_new.bt");
        let core_file = temp_path.join("actor_new.core");
        fs::write(&source_file, ACTOR_NEW_SOURCE).unwrap();

        let options = beamtalk_core::CompilerOptions::default();
        let result = compile_source(&source_file, "actor_new", &core_file, &options);

        // Actor new is now an error, not a warning — always fails
        assert!(
            result.is_err(),
            "Should fail to compile: actor new is a compile-time error: {result:?}"
        );
    }

    #[test]
    fn test_warnings_as_errors_overrides_suppress_warnings() {
        // suppress_warnings: true alone would suppress and allow compilation;
        // combined with warnings_as_errors: true, the failure must still happen.
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("actor_new.bt");
        let core_file = temp_path.join("actor_new.core");
        fs::write(&source_file, ACTOR_NEW_SOURCE).unwrap();

        let options = beamtalk_core::CompilerOptions {
            warnings_as_errors: true,
            suppress_warnings: true,
            ..Default::default()
        };
        let result = compile_source(&source_file, "actor_new", &core_file, &options);

        assert!(
            result.is_err(),
            "warnings_as_errors must override suppress_warnings: compilation should fail"
        );
    }

    // ---- ADR 0100 Rule 3 `[diagnostics]` table tests ----

    /// Source with a DNU hint on a known, closed receiver (`String` has no
    /// `frobnicate`) — mirrors the fixture `lint.rs` uses for the same hint.
    const DNU_HINT_SOURCE: &str = "\"hello\" frobnicate";

    /// Source with a top-level reference to a class that doesn't exist —
    /// triggers `DiagnosticCategory::UnresolvedClass` at `Warning` severity
    /// (see `structural_validators::tests::test_unresolved_class_warns_on_unknown`).
    const UNRESOLVED_CLASS_SOURCE: &str = "NonExistentClassBt2793";

    fn diagnostics_table_ctx(
        table: crate::commands::manifest::DiagnosticsTable,
    ) -> CompileContext<'static> {
        CompileContext {
            diagnostics_overrides: table,
            ..CompileContext::default()
        }
    }

    /// Like [`diagnostics_table_ctx`], but with a non-empty
    /// `pre_loaded_classes` — `check_unresolved_classes` only runs when
    /// cross-file metadata is present (`semantic_analysis::mod.rs`'s
    /// `has_cross_file_classes` gate), matching a real multi-file package
    /// build. A single standalone file otherwise can't tell an unresolved
    /// class name apart from one defined in a sibling file it hasn't seen.
    fn diagnostics_table_ctx_with_cross_file_classes(
        table: crate::commands::manifest::DiagnosticsTable,
    ) -> CompileContext<'static> {
        use beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo;

        let sibling = ClassInfo {
            surface_incomplete: false,
            in_caller_new1: false,
            is_module_less: false,
            name: "SiblingBt2793".into(),
            superclass: Some("Object".into()),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_internal: false,
            package: None,
            is_value: false,
            is_native: false,
            auto_ctors: None,
            handle_scope: None,
            state: vec![],
            state_types: std::collections::HashMap::new(),
            state_has_default: std::collections::HashMap::new(),
            state_kinds: std::collections::HashMap::new(),
            initialize_assigns: std::collections::BTreeSet::new(),
            has_dynamic_field_writer: false,
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            type_param_bounds: vec![],
            superclass_type_args: vec![],
        };

        CompileContext {
            hierarchy: ClassHierarchyContext {
                pre_loaded_classes: vec![sibling],
                ..ClassHierarchyContext::default()
            },
            diagnostics_overrides: table,
            ..CompileContext::default()
        }
    }

    #[test]
    fn test_diagnostics_table_absent_dnu_hint_does_not_fail_build() {
        // Baseline: with no [diagnostics] table, a bare Dnu Hint never fails
        // the build on its own (Rule 1 default).
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("dnu.bt");
        let core_file = temp_path.join("dnu.core");
        fs::write(&source_file, DNU_HINT_SOURCE).unwrap();

        let options = beamtalk_core::CompilerOptions::default();
        let diags = compile_source_with_bindings(
            &source_file,
            "dnu",
            &core_file,
            &options,
            &beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable::new(),
            &diagnostics_table_ctx(crate::commands::manifest::DiagnosticsTable::new()),
            None,
        )
        .expect("Dnu hint alone must not fail the build");

        assert!(
            diags.iter().any(
                |d| d.category == Some(beamtalk_core::source_analysis::DiagnosticCategory::Dnu)
            ),
            "fixture should still produce a Dnu diagnostic: {diags:?}"
        );
    }

    #[test]
    fn test_diagnostics_table_off_drops_dnu_diagnostic() {
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("dnu.bt");
        let core_file = temp_path.join("dnu.core");
        fs::write(&source_file, DNU_HINT_SOURCE).unwrap();

        let mut table = crate::commands::manifest::DiagnosticsTable::new();
        table.insert(
            beamtalk_core::source_analysis::DiagnosticCategory::Dnu,
            crate::commands::manifest::DiagnosticSeverityOverride::Off,
        );

        let options = beamtalk_core::CompilerOptions::default();
        let diags = compile_source_with_bindings(
            &source_file,
            "dnu",
            &core_file,
            &options,
            &beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable::new(),
            &diagnostics_table_ctx(table),
            None,
        )
        .expect("off-category diagnostics must not fail the build");

        assert!(
            !diags.iter().any(
                |d| d.category == Some(beamtalk_core::source_analysis::DiagnosticCategory::Dnu)
            ),
            "dnu = \"off\" must drop the Dnu diagnostic entirely: {diags:?}"
        );
    }

    #[test]
    fn test_diagnostics_table_expect_directive_wins_over_error_override() {
        // ADR 0100 Rule 3 precedence step 1: a site-level `@expect dnu`
        // always wins, regardless of the `[diagnostics]` table. `@expect`
        // suppression runs inside `compute_project_diagnostics`, *before*
        // `apply_diagnostics_table` — so even `dnu = "error"` never sees a
        // diagnostic that `@expect dnu` already removed.
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("dnu.bt");
        let core_file = temp_path.join("dnu.core");
        fs::write(&source_file, "@expect dnu\n\"hello\" frobnicate").unwrap();

        let mut table = crate::commands::manifest::DiagnosticsTable::new();
        table.insert(
            beamtalk_core::source_analysis::DiagnosticCategory::Dnu,
            crate::commands::manifest::DiagnosticSeverityOverride::Error,
        );

        let options = beamtalk_core::CompilerOptions::default();
        let result = compile_source_with_bindings(
            &source_file,
            "dnu",
            &core_file,
            &options,
            &beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable::new(),
            &diagnostics_table_ctx(table),
            None,
        );

        assert!(
            result.is_ok(),
            "@expect dnu must suppress the diagnostic before dnu = \"error\" ever sees it: \
             {result:?}"
        );
    }

    #[test]
    fn test_diagnostics_table_error_fails_build_without_warnings_as_errors() {
        // dnu = "error" sets the category's *base* severity to Error — this
        // must fail the build unconditionally, independent of
        // --warnings-as-errors (ADR 0100 Rule 3 precedence: the table sets
        // base severity; --warnings-as-errors is a separate, later promotion
        // pass over whatever the table leaves as Warning/Hint).
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("dnu.bt");
        let core_file = temp_path.join("dnu.core");
        fs::write(&source_file, DNU_HINT_SOURCE).unwrap();

        let mut table = crate::commands::manifest::DiagnosticsTable::new();
        table.insert(
            beamtalk_core::source_analysis::DiagnosticCategory::Dnu,
            crate::commands::manifest::DiagnosticSeverityOverride::Error,
        );

        let options = beamtalk_core::CompilerOptions::default();
        let result = compile_source_with_bindings(
            &source_file,
            "dnu",
            &core_file,
            &options,
            &beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable::new(),
            &diagnostics_table_ctx(table),
            None,
        );

        assert!(
            result.is_err(),
            "dnu = \"error\" must fail the build even without --warnings-as-errors: {result:?}"
        );
    }

    #[test]
    fn test_diagnostics_table_severity_floor_protects_hard_errors() {
        // `ActorNew` (`Actor
        // subclass new` must always fail the build) is emitted at
        // Severity::Error unconditionally, not via the Rule 1 completeness
        // ladder. A table entry like `actor-new = "warn"` must NOT silently
        // downgrade it — the severity floor keeps hard structural errors
        // out of reach of Rule 3's escalation-only table.
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("actor_new.bt");
        let core_file = temp_path.join("actor_new.core");
        fs::write(&source_file, ACTOR_NEW_SOURCE).unwrap();

        let mut table = crate::commands::manifest::DiagnosticsTable::new();
        table.insert(
            beamtalk_core::source_analysis::DiagnosticCategory::ActorNew,
            crate::commands::manifest::DiagnosticSeverityOverride::Warn,
        );

        let options = beamtalk_core::CompilerOptions::default();
        let result = compile_source_with_bindings(
            &source_file,
            "actor_new",
            &core_file,
            &options,
            &beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable::new(),
            &diagnostics_table_ctx(table),
            None,
        );

        assert!(
            result.is_err(),
            "actor-new = \"warn\" must not downgrade the hard ActorNew error: {result:?}"
        );
    }

    #[test]
    fn test_diagnostics_table_severity_floor_ignores_off_on_hard_error() {
        // Same severity-floor guarantee for "off": must not silently drop a
        // hard-error diagnostic either.
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("actor_new.bt");
        let core_file = temp_path.join("actor_new.core");
        fs::write(&source_file, ACTOR_NEW_SOURCE).unwrap();

        let mut table = crate::commands::manifest::DiagnosticsTable::new();
        table.insert(
            beamtalk_core::source_analysis::DiagnosticCategory::ActorNew,
            crate::commands::manifest::DiagnosticSeverityOverride::Off,
        );

        let options = beamtalk_core::CompilerOptions::default();
        let result = compile_source_with_bindings(
            &source_file,
            "actor_new",
            &core_file,
            &options,
            &beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable::new(),
            &diagnostics_table_ctx(table),
            None,
        );

        assert!(
            result.is_err(),
            "actor-new = \"off\" must not drop the hard ActorNew error: {result:?}"
        );
    }

    #[test]
    fn test_diagnostics_table_absent_unresolved_class_excluded_from_warnings_as_errors() {
        // Baseline (today's default): UnresolvedClass is excluded from
        // --warnings-as-errors promotion, so this must still succeed.
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("unresolved.bt");
        let core_file = temp_path.join("unresolved.core");
        fs::write(&source_file, UNRESOLVED_CLASS_SOURCE).unwrap();

        let options = beamtalk_core::CompilerOptions {
            warnings_as_errors: true,
            ..Default::default()
        };
        let result = compile_source_with_bindings(
            &source_file,
            "unresolved",
            &core_file,
            &options,
            &beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable::new(),
            &diagnostics_table_ctx_with_cross_file_classes(
                crate::commands::manifest::DiagnosticsTable::new(),
            ),
            None,
        );

        assert!(
            result.is_ok(),
            "UnresolvedClass must stay excluded from --warnings-as-errors with no \
             [diagnostics] override: {result:?}"
        );
    }

    #[test]
    fn test_diagnostics_table_explicit_entry_lifts_gradual_migration_exclusion() {
        // An explicit [diagnostics] entry for a gradual-migration category
        // (UnresolvedClass) is a deliberate opt back in to promotion — it
        // must win over the default --warnings-as-errors exclusion (ADR 0100
        // Rule 3: "explicit table value wins over the exclusion").
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("unresolved.bt");
        let core_file = temp_path.join("unresolved.core");
        fs::write(&source_file, UNRESOLVED_CLASS_SOURCE).unwrap();

        let mut table = crate::commands::manifest::DiagnosticsTable::new();
        table.insert(
            beamtalk_core::source_analysis::DiagnosticCategory::UnresolvedClass,
            crate::commands::manifest::DiagnosticSeverityOverride::Warn,
        );

        let options = beamtalk_core::CompilerOptions {
            warnings_as_errors: true,
            ..Default::default()
        };
        let result = compile_source_with_bindings(
            &source_file,
            "unresolved",
            &core_file,
            &options,
            &beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable::new(),
            &diagnostics_table_ctx_with_cross_file_classes(table),
            None,
        );

        assert!(
            result.is_err(),
            "explicit unresolved-class = \"warn\" + --warnings-as-errors must fail: {result:?}"
        );
    }

    #[test]
    fn test_diagnostics_table_hint_override_also_lifts_gradual_migration_exclusion() {
        // The exclusion-lift keys off
        // table *presence* for the category, not the chosen severity value —
        // per ADR 0100 Rule 3's literal precedence note ("the explicit table
        // value wins over the exclusion"), so `unresolved-class = "hint"`
        // (not just `"warn"`) also opts the category back into
        // --warnings-as-errors promotion. This is intentional (see the
        // `[diagnostics]` section of docs/beamtalk-packages.md) — pinned here
        // so the behaviour doesn't drift silently.
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("unresolved.bt");
        let core_file = temp_path.join("unresolved.core");
        fs::write(&source_file, UNRESOLVED_CLASS_SOURCE).unwrap();

        let mut table = crate::commands::manifest::DiagnosticsTable::new();
        table.insert(
            beamtalk_core::source_analysis::DiagnosticCategory::UnresolvedClass,
            crate::commands::manifest::DiagnosticSeverityOverride::Hint,
        );

        let options = beamtalk_core::CompilerOptions {
            warnings_as_errors: true,
            ..Default::default()
        };
        let result = compile_source_with_bindings(
            &source_file,
            "unresolved",
            &core_file,
            &options,
            &beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable::new(),
            &diagnostics_table_ctx_with_cross_file_classes(table),
            None,
        );

        assert!(
            result.is_err(),
            "unresolved-class = \"hint\" + --warnings-as-errors must also fail \
             (any explicit entry lifts the exclusion, not just \"warn\"): {result:?}"
        );
    }

    #[test]
    fn test_diagnostics_table_lint_override_stays_inert_under_warnings_as_errors() {
        // Contrast with the "hint" case above: `"lint"` rewrites the
        // diagnostic to `Severity::Lint`, which `--warnings-as-errors` never
        // promotes (it only promotes `Warning`/`Hint`) — so even though the
        // exclusion is lifted, the build still succeeds. Lint diagnostics are
        // also suppressed from normal build output entirely (shown only by
        // `beamtalk lint`).
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("unresolved.bt");
        let core_file = temp_path.join("unresolved.core");
        fs::write(&source_file, UNRESOLVED_CLASS_SOURCE).unwrap();

        let mut table = crate::commands::manifest::DiagnosticsTable::new();
        table.insert(
            beamtalk_core::source_analysis::DiagnosticCategory::UnresolvedClass,
            crate::commands::manifest::DiagnosticSeverityOverride::Lint,
        );

        let options = beamtalk_core::CompilerOptions {
            warnings_as_errors: true,
            ..Default::default()
        };
        let result = compile_source_with_bindings(
            &source_file,
            "unresolved",
            &core_file,
            &options,
            &beamtalk_codegen::core_erlang::primitive_bindings::PrimitiveBindingTable::new(),
            &diagnostics_table_ctx_with_cross_file_classes(table),
            None,
        );

        assert!(
            result.is_ok(),
            "unresolved-class = \"lint\" must stay inert under --warnings-as-errors \
             (Lint severity is never promoted): {result:?}"
        );
    }

    /// Keyword/class-kind mismatches are now hard compile errors
    /// (promoted from deprecation warnings in Phase 4 of ADR 0067).
    #[test]
    fn test_keyword_class_kind_mismatch_is_compile_error() {
        let temp = TempDir::new().unwrap();
        let temp_path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let source_file = temp_path.join("mismatch.bt");
        let core_file = temp_path.join("mismatch.core");
        // Object subclass with state: is now a hard compile error
        fs::write(
            &source_file,
            "Object subclass: BadObj\n  state: x = 0\n  value => self.x",
        )
        .unwrap();

        let options = beamtalk_core::CompilerOptions::default();
        let result = compile_source(&source_file, "bt@mismatch", &core_file, &options);

        assert!(
            result.is_err(),
            "Object subclass with state: should be a compile error: {result:?}"
        );
    }
}
