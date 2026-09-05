// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Diagnostic provider for the language service.
//!
//! **DDD Context:** Language Service
//!
//! This domain service implements the `DiagnosticProvider` from the DDD model.
//! It collects errors and warnings from multiple compilation phases (lexing,
//! parsing, semantic analysis) and reports them to the editor. The provider
//! follows LSP terminology and aligns with the ubiquitous language defined in
//! `docs/beamtalk-ddd-model.md`.
//!
//! # Design
//!
//! Diagnostics come from multiple sources:
//! - Lexical errors (invalid tokens)
//! - Parse errors (syntax errors)
//! - Semantic errors (type errors, undefined references, mutations, etc.)
//!
//! # Performance
//!
//! Must respond in <50ms for typical file sizes.
//!
//! # References
//!
//! - DDD model: `docs/beamtalk-ddd-model.md` (Language Service Context)
//! - LSP specification: Language Server Protocol publishDiagnostics notification

use beamtalk_core::ast::Module;
use beamtalk_core::semantic_analysis;
use beamtalk_core::source_analysis::{Diagnostic, DiagnosticCategory};
use ecow::EcoString;

/// Project-level context for the unified diagnostic pipeline (BT-2009).
///
/// Bundles all optional inputs that vary between the CLI compiler and the LSP.
/// Both callers construct a `ProjectDiagnosticContext` and pass it to
/// [`compute_project_diagnostics`], ensuring the same post-analysis passes
/// run in both environments.
///
/// Fields that the LSP cannot supply (e.g. `dep_registry` in standalone mode)
/// are `Option` / default and the pipeline skips the corresponding pass.
#[derive(Debug, Default)]
pub struct ProjectDiagnosticContext<'a> {
    /// Compiler options (`stdlib_mode`, `warnings_as_errors`, etc.).
    pub options: beamtalk_core::CompilerOptions,
    /// Cross-file class metadata from other compilation units.
    /// Injected into the class hierarchy before type checking so that
    /// cross-file method resolution works.
    pub cross_file_classes: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    /// Pre-loaded protocol definitions from other source files.
    pub pre_loaded_protocols:
        Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo>,
    /// Pre-loaded type alias definitions from other source files in the same
    /// package (BT-2928, ADR 0108). Mirrors `pre_loaded_protocols` — seeded
    /// into the `AliasRegistry` before the current module's own aliases are
    /// registered, so a `type Name = ...` declared in a different file
    /// resolves cross-file the same way a cross-file class reference already
    /// does. Empty for callers that don't (yet) supply project-wide alias
    /// metadata — the pipeline degrades to today's same-file-only resolution.
    pub pre_loaded_aliases: Vec<beamtalk_core::semantic_analysis::AliasInfo>,
    /// Project-wide standalone extension definitions (BT-2795, ADR 0066).
    /// Registered into the class hierarchy so cross-file
    /// `ClassName >> selector` extensions resolve instead of producing
    /// false `Dnu` hints. May include the current file's own entries —
    /// duplicates are skipped during registration.
    pub cross_file_extensions: beamtalk_core::compilation::extension_index::ExtensionIndex,
    /// Native type registry for FFI call inference (ADR 0075).
    pub native_type_registry:
        Option<std::sync::Arc<beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>>,
    /// Optional dependency registry for cross-package collision detection.
    pub dep_registry: Option<&'a beamtalk_core::semantic_analysis::DependencyRegistry>,
    /// Whether to promote transitive dependency usage warnings to errors.
    pub strict_deps: bool,
    /// Per-category diagnostic severity overrides from `beamtalk.toml`'s
    /// `[diagnostics]` section (ADR 0100 Rule 3, BT-2800). Empty when the
    /// package has no manifest or no `[diagnostics]` section — absence
    /// preserves today's Rule 1 completeness-ladder defaults. Applied here,
    /// inside the shared pipeline, so the CLI (`beamtalk build`) and the LSP
    /// can never disagree about the resulting severity — see
    /// `beamtalk_core::compilation::diagnostics_policy::apply_diagnostics_table`.
    pub diagnostics_overrides: beamtalk_core::compilation::diagnostics_policy::DiagnosticsTable,
    /// Whether the file being analysed lives under a project's `stubs/`
    /// directory (ADR 0075, BT-1846/BT-1847) — `declare native:` is only
    /// legal there. Callers derive this from the file path they're about to
    /// analyse (e.g. `SimpleLanguageService::diagnostics`); defaulting to
    /// `false` matches `AnalysisContext::is_stub_file`'s own default, so a
    /// caller that never sets this still correctly rejects `declare native:`
    /// outside `stubs/`.
    pub is_stub_file: bool,
    /// Basename (without extension) of the file being analysed, or `None`
    /// when there is no real file backing the module (REPL sessions,
    /// in-memory snippets). Used by
    /// [`check_class_file_name_agreement`](beamtalk_core::semantic_analysis::module_validator::check_class_file_name_agreement)
    /// (BT-3431) to validate that the file name agrees with the class it
    /// declares — a mismatch silently breaks self-dispatch codegen with no
    /// other diagnostic. Callers derive this from the file path they're
    /// about to analyse, mirroring `is_stub_file` above.
    pub source_file_stem: Option<String>,
}

/// Unified post-analysis diagnostic pipeline (BT-2009).
///
/// Runs semantic analysis followed by all post-analysis passes (stdlib name
/// shadowing, collision detection, transitive dep usage, unresolved-class
/// hint enrichment) and finally applies `@expect` directives. Both the CLI
/// compiler and the LSP diagnostic provider call this function so that
/// diagnostics are consistent across environments.
///
/// # Arguments
///
/// * `module` - The parsed AST
/// * `source` - The module's raw source text (BT-3240: needed to give the
///   near-miss-divider check an accurate comment span — see
///   `beamtalk_core::near_miss_divider::scan_source`'s doc)
/// * `initial_diagnostics` - Pre-analysis diagnostics (parse + any earlier passes,
///   e.g. `@primitive` validation from the CLI compiler); the function appends
///   semantic and post-analysis diagnostics to this list.
/// * `ctx` - Project-level context bundling all optional inputs
///
/// # Returns
///
/// A list of all diagnostics (errors and warnings) after `@expect` suppression.
#[must_use]
pub fn compute_project_diagnostics(
    module: &Module,
    source: &str,
    initial_diagnostics: Vec<Diagnostic>,
    ctx: &ProjectDiagnosticContext<'_>,
) -> Vec<Diagnostic> {
    compute_project_diagnostics_with_analysis(module, source, initial_diagnostics, ctx).0
}

/// [`compute_project_diagnostics`], additionally returning the
/// [`AnalysisResult`](beamtalk_core::semantic_analysis::AnalysisResult) the pipeline's
/// `analyse_full` call produced (BT-3123).
///
/// Callers that go on to run codegen for the same module (e.g. the CLI build
/// pipeline) should use this variant and thread the returned `AnalysisResult`
/// into `CodegenOptions::with_analysis`, instead of letting codegen re-derive
/// the class hierarchy, semantic facts, and inferred method return types from
/// scratch. Callers that only need diagnostics (e.g. the LSP) can keep using
/// [`compute_project_diagnostics`].
#[must_use]
pub fn compute_project_diagnostics_with_analysis(
    module: &Module,
    source: &str,
    initial_diagnostics: Vec<Diagnostic>,
    ctx: &ProjectDiagnosticContext<'_>,
) -> (Vec<Diagnostic>, semantic_analysis::AnalysisResult) {
    let mut diagnostics = initial_diagnostics;

    // Run semantic analysis with the richest available context.
    // BT-2928: thread `pre_loaded_aliases` through so a cross-file/package
    // type alias resolves the same way a cross-file class reference already
    // does — see `AnalysisContext::pre_loaded_aliases`'s doc.
    let analysis_ctx = beamtalk_core::semantic_analysis::AnalysisContext::default()
        .with_options(&ctx.options)
        .with_pre_loaded_classes(ctx.cross_file_classes.clone())
        .with_pre_loaded_protocols(ctx.pre_loaded_protocols.clone())
        .with_pre_loaded_aliases(ctx.pre_loaded_aliases.clone())
        .with_native_type_registry(ctx.native_type_registry.clone())
        .with_cross_file_extensions(&ctx.cross_file_extensions)
        .with_is_stub_file(ctx.is_stub_file);
    let mut analysis_result = beamtalk_core::semantic_analysis::analyse_full(module, analysis_ctx);
    // BT-3123: diagnostics are consumed below (and by every downstream pass
    // in this pipeline); take them out of `analysis_result` so the rest of
    // `AnalysisResult` (hierarchy, semantic facts, inferred return types,
    // alias registry) can be handed to codegen without cloning it.
    diagnostics.extend(std::mem::take(&mut analysis_result.diagnostics));

    // BT-3431: Validate that the file name agrees with the class it
    // declares — a mismatch silently breaks self-dispatch codegen (see
    // `check_class_file_name_agreement`'s doc) with no other diagnostic.
    diagnostics.extend(
        beamtalk_core::semantic_analysis::module_validator::check_class_file_name_agreement(
            module,
            ctx.source_file_stem.as_deref(),
        ),
    );

    // BT-1732: Enrich unresolved class warnings with dependency package hints.
    if let Some(registry) = ctx.dep_registry {
        for diag in &mut diagnostics {
            if diag.category == Some(DiagnosticCategory::UnresolvedClass) {
                if let Some(class_name) = diag
                    .message
                    .strip_prefix("Unresolved class `")
                    .and_then(|s| s.strip_suffix('`'))
                {
                    if let Some(exports) = registry.lookup(class_name) {
                        if let Some(export) = exports.first() {
                            diag.hint = Some(
                                format!(
                                    "Did you mean `{class_name}` from dependency '{}'? \
                                     Ensure the dependency is declared in beamtalk.toml.",
                                    export.package
                                )
                                .into(),
                            );
                        }
                    }
                }
            }
        }
    }

    // BT-738: Warn when user code shadows a stdlib class name.
    if !ctx.options.stdlib_mode {
        let mut stdlib_shadow_diags = Vec::new();
        beamtalk_core::semantic_analysis::check_stdlib_name_shadowing(
            module,
            &mut stdlib_shadow_diags,
        );
        diagnostics.extend(stdlib_shadow_diags);
    }

    // BT-1653 / ADR 0070 Phase 3: Cross-package class collision detection
    // and BT-1654: transitive dependency usage warnings.
    if let Some(registry) = ctx.dep_registry {
        beamtalk_core::semantic_analysis::check_collision_at_use_sites(
            module,
            registry,
            &mut diagnostics,
        );
        beamtalk_core::semantic_analysis::check_transitive_dep_usage(
            module,
            registry,
            ctx.strict_deps,
            &mut diagnostics,
        );
    }

    // BT-782: Apply @expect directives to suppress matching diagnostics.
    // BT-3384: this pipeline never runs `beamtalk_lint::run_lint_passes`
    // (that's `beamtalk lint`-only), so a lint-only `@expect` category (e.g.
    // `dead_assignment`) must not be validated for staleness here — see
    // `apply_expect_directives_excluding_lint_only`'s doc.
    apply_expect_directives_excluding_lint_only(module, &mut diagnostics);

    // ADR 0100 Rule 3 (BT-2793 / BT-2800): apply the package's `[diagnostics]`
    // table last, after `@expect` suppression and ahead of any
    // `--warnings-as-errors`-style promotion pass a caller runs over this
    // function's result. A no-op (empty table) when the package has no
    // manifest or no `[diagnostics]` section. Living here — inside the one
    // pipeline both the CLI compiler and the LSP call — is what makes
    // `beamtalk build` and the LSP agree on severity for every diagnostic
    // category by construction, closing the BT-2800 surface-parity gap.
    diagnostics = beamtalk_core::compilation::diagnostics_policy::apply_diagnostics_table(
        diagnostics,
        &ctx.diagnostics_overrides,
    );

    // BT-3240: near-miss `// === Name ===` section-divider comments (typoed
    // `=` run lengths, too-short runs, or a `///`/`/* */` comment where the
    // divider convention requires a plain `//` line) get no signal anywhere
    // today — they silently fall back to an ordinary comment and the
    // methods below are mis-categorized with no diagnostic. Unlike every
    // lint pass in the standalone `beamtalk-lint` crate (`beamtalk
    // lint`-only), this one check also runs here so it reaches the LSP's
    // live diagnostics too — see `beamtalk_core::near_miss_divider::check_near_miss_dividers`'s
    // doc (BT-3340: this check stays a `beamtalk-core` leaf module rather
    // than moving to `beamtalk-lint` with the rest, precisely so this call
    // doesn't need a new crate dependency) for why. Scans
    // `source` directly (not `module`) so the diagnostic's span is the
    // comment's own line, not the AST's (inaccurate, see that doc) token span.
    //
    // Deliberately appended *after* both `apply_expect_directives` and
    // `apply_diagnostics_table` (adversarial review, BT-3240) rather than
    // mixed into `diagnostics` beforehand:
    // - `apply_expect_directives` matches an `@expect` directive to a
    //   diagnostic by `target_span.contains(diag.span)`, where `target_span`
    //   is the annotated *declaration's* span — which never includes that
    //   declaration's own *leading comments*. A near-miss-divider comment's
    //   span (the comment's own line) can therefore never be contained in
    //   any declaration's span, so no `@expect` (not even `@expect all`)
    //   could ever have matched one — running it through that pass first
    //   would be a no-op at best, so skipping it changes nothing observable.
    // - `apply_diagnostics_table` promotes/demotes purely by
    //   `DiagnosticCategory`, not by which pass produced a diagnostic. Two
    //   pre-existing `semantic_analysis` checks (`check_effect_free_statements`,
    //   BT-951; the BT-2140 redundant-type-annotation check) also emit
    //   `Severity::Lint` diagnostics tagged `DiagnosticCategory::Lint` and
    //   *do* need the table applied to them (a project's `[diagnostics] lint
    //   = "..."` must still control those) — so this can't be solved by
    //   filtering on severity or category before the table runs; appending
    //   after it is what keeps every other check's table behavior
    //   unaffected while still guaranteeing *this* check's diagnostics can
    //   never be promoted to a build-breaking severity by any project
    //   config, by construction rather than by coincidence.
    beamtalk_core::near_miss_divider::check_near_miss_dividers(source, &mut diagnostics);

    (diagnostics, analysis_result)
}

// BT-3361 (ADR 0117 Decision step 5): `compute_diagnostics`,
// `compute_diagnostics_with_known_vars`, `apply_expect_directives`, and
// `apply_expect_directives`'s private helpers moved to
// `beamtalk_core::compilation::diagnostics_policy` and are re-exported here
// under their original names/paths — every existing call site (inside this
// module's own tests, and external callers like `beamtalk-mcp`/`beamtalk-cli`)
// keeps compiling unchanged. They're pure Compilation-context diagnostics
// post-processing with zero Language-Service-specific types (no `Position`,
// `Completion`, ...) — like `apply_diagnostics_table`, their existing
// neighbor in that module — so this isn't a Language-Service item reaching
// backward into Compilation; it's recognizing they were always Compilation's
// and relocating them somewhere both `beamtalk-core`'s own unit tests and
// this crate can reach without a dev-dependency cycle: `beamtalk-core`'s
// unit tests (compiled as part of the same `--cfg test` build as the
// library) cannot depend on this crate, which itself depends on
// `beamtalk-core`, without one.
pub use beamtalk_core::compilation::diagnostics_policy::{
    apply_expect_directives, apply_expect_directives_excluding_lint_only, compute_diagnostics,
    compute_diagnostics_with_known_vars,
};

/// Computes diagnostics with native type registry for FFI type warnings (ADR 0075).
///
/// When `native_types` is `Some`, FFI calls get typed return inference and
/// keyword mismatch / argument type warnings from the registry.
#[must_use]
pub fn compute_diagnostics_with_native_types(
    module: &beamtalk_core::ast::Module,
    parse_diagnostics: Vec<Diagnostic>,
    native_types: Option<
        std::sync::Arc<beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>,
    >,
) -> Vec<Diagnostic> {
    let mut all_diagnostics = parse_diagnostics;

    if native_types.is_some() {
        let options = beamtalk_core::CompilerOptions::default();
        let ctx = beamtalk_core::semantic_analysis::AnalysisContext::default()
            .with_options(&options)
            .with_native_type_registry(native_types);
        let analysis_result = beamtalk_core::semantic_analysis::analyse_full(module, ctx);
        all_diagnostics.extend(analysis_result.diagnostics);
    } else {
        let analysis_result = beamtalk_core::semantic_analysis::analyse(module);
        all_diagnostics.extend(analysis_result.diagnostics);
    }

    apply_expect_directives(module, &mut all_diagnostics);
    all_diagnostics
}

/// Shared finalization pipeline for the REPL / compiler-port diagnostics
/// entry points.
///
/// Combines `parse_diagnostics` with the diagnostics produced by whichever
/// `analyse_*` variant was called, applies `@expect` suppressions, then
/// applies the per-package severity-override table — in that fixed order,
/// matching [`compute_project_diagnostics`].
fn run_diagnostic_pipeline(
    module: &beamtalk_core::ast::Module,
    parse_diagnostics: Vec<Diagnostic>,
    analysis_diagnostics: Vec<Diagnostic>,
    diagnostics_overrides: &beamtalk_core::compilation::diagnostics_policy::DiagnosticsTable,
) -> Vec<Diagnostic> {
    let mut all_diagnostics = parse_diagnostics;
    all_diagnostics.extend(analysis_diagnostics);
    // BT-3384: the REPL never runs `beamtalk_lint::run_lint_passes` either —
    // see `apply_expect_directives_excluding_lint_only`'s doc.
    apply_expect_directives_excluding_lint_only(module, &mut all_diagnostics);
    beamtalk_core::compilation::diagnostics_policy::apply_diagnostics_table(
        all_diagnostics,
        diagnostics_overrides,
    )
}

/// Computes diagnostics with pre-defined REPL variables and pre-loaded class
/// entries from BEAM metadata (ADR 0050 Phase 4).
///
/// `pre_loaded_classes` are injected into the `ClassHierarchy` before `TypeChecking`,
/// making REPL-session user classes visible to the `TypeChecker`.
///
/// This is the REPL's diagnostics entry point (called from
/// `beamtalk-compiler-port`'s `compile_expression`/`compile`/`diagnostics`
/// request handlers). `diagnostics_overrides` is the package's `beamtalk.toml`
/// `[diagnostics]` severity-override table (ADR 0100 Rule 3); applying it
/// here — after `@expect` suppression, mirroring the order
/// [`compute_project_diagnostics`] uses — closes the BT-2839 surface-parity
/// gap: a package that sets `dnu = "error"` now fails `beamtalk build`, shows
/// an `Error` in the LSP (BT-2800), *and* shows an `Error` at the REPL,
/// instead of a REPL-only soft `Hint`. Pass an empty table (the `Default`)
/// for callers with no manifest context, which is a complete no-op.
#[must_use]
pub fn compute_diagnostics_with_known_vars_and_classes(
    module: &beamtalk_core::ast::Module,
    parse_diagnostics: Vec<Diagnostic>,
    known_vars: &[&str],
    pre_loaded_classes: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    diagnostics_overrides: &beamtalk_core::compilation::diagnostics_policy::DiagnosticsTable,
) -> Vec<Diagnostic> {
    let ctx = beamtalk_core::semantic_analysis::AnalysisContext::default()
        .with_known_vars(known_vars)
        .with_pre_loaded_classes(pre_loaded_classes);
    let analysis_result = beamtalk_core::semantic_analysis::analyse_full(module, ctx);
    run_diagnostic_pipeline(
        module,
        parse_diagnostics,
        analysis_result.diagnostics,
        diagnostics_overrides,
    )
}

/// Computes diagnostics with pre-defined REPL variables, pre-loaded classes,
/// and pre-loaded type aliases from earlier REPL turns (ADR 0108 Phase 8,
/// BT-2902).
///
/// Mirrors [`compute_diagnostics_with_known_vars_and_classes`] — see its doc
/// — with `pre_loaded_aliases` additionally injected into the `AliasRegistry`
/// before `TypeChecking`, so a `::` annotation referencing a `type Name = ...`
/// declared in an *earlier* turn of the same session resolves instead of
/// producing an unresolved-type diagnostic.
#[must_use]
pub fn compute_diagnostics_with_known_vars_classes_and_aliases(
    module: &beamtalk_core::ast::Module,
    parse_diagnostics: Vec<Diagnostic>,
    known_vars: &[&str],
    pre_loaded_classes: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    pre_loaded_aliases: Vec<beamtalk_core::semantic_analysis::AliasInfo>,
    diagnostics_overrides: &beamtalk_core::compilation::diagnostics_policy::DiagnosticsTable,
) -> Vec<Diagnostic> {
    let ctx = beamtalk_core::semantic_analysis::AnalysisContext::default()
        .with_known_vars(known_vars)
        .with_pre_loaded_classes(pre_loaded_classes)
        .with_pre_loaded_aliases(pre_loaded_aliases);
    let analysis_result = beamtalk_core::semantic_analysis::analyse_full(module, ctx);
    run_diagnostic_pipeline(
        module,
        parse_diagnostics,
        analysis_result.diagnostics,
        diagnostics_overrides,
    )
}

/// [`compute_diagnostics_with_known_vars_classes_and_aliases`], additionally
/// returning the alias names this compile's annotations transitively
/// referenced (ADR 0108 hot-reload re-check trigger, BT-2899).
///
/// Used by the compiler port's `compile`/`compile_method`/`diagnostics`
/// handlers, which need this set to populate the Erlang-side alias-name →
/// dependent-class index (`beamtalk_alias_xref`) — the lookup key a live
/// alias redefinition's re-check trigger consults instead of sweeping every
/// live class (unlike ADR 0107's `trigger_leaf_change/1`). Every other
/// caller of the sibling function only needs diagnostics, so that function's
/// signature is left unchanged rather than widened for this one consumer.
#[must_use]
pub fn compute_diagnostics_and_referenced_aliases(
    module: &beamtalk_core::ast::Module,
    parse_diagnostics: Vec<Diagnostic>,
    known_vars: &[&str],
    pre_loaded_classes: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    pre_loaded_aliases: Vec<beamtalk_core::semantic_analysis::AliasInfo>,
    diagnostics_overrides: &beamtalk_core::compilation::diagnostics_policy::DiagnosticsTable,
) -> (Vec<Diagnostic>, Vec<EcoString>) {
    let (diagnostics, analysis_result) = compute_diagnostics_and_analysis(
        module,
        parse_diagnostics,
        known_vars,
        pre_loaded_classes,
        pre_loaded_aliases,
        diagnostics_overrides,
    );
    (diagnostics, analysis_result.referenced_aliases)
}

/// [`compute_diagnostics_and_referenced_aliases`], additionally returning the
/// full [`AnalysisResult`](semantic_analysis::AnalysisResult) (BT-3123) —
/// class hierarchy, semantic facts, and inferred method return types,
/// alongside `referenced_aliases` (still reachable as a field on the result).
///
/// Used by compiler-port handlers that go on to run codegen for the same
/// module (`compile`, `compile_method`, and `compile_expression`'s
/// class/protocol-defining paths) so codegen can consume this analysis via
/// `CodegenOptions::with_analysis` instead of re-deriving the class
/// hierarchy, semantic facts, and inferred method return types from scratch.
/// Callers that only need diagnostics/`referenced_aliases` should keep using
/// [`compute_diagnostics_and_referenced_aliases`].
#[must_use]
pub fn compute_diagnostics_and_analysis(
    module: &beamtalk_core::ast::Module,
    parse_diagnostics: Vec<Diagnostic>,
    known_vars: &[&str],
    pre_loaded_classes: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    pre_loaded_aliases: Vec<beamtalk_core::semantic_analysis::AliasInfo>,
    diagnostics_overrides: &beamtalk_core::compilation::diagnostics_policy::DiagnosticsTable,
) -> (Vec<Diagnostic>, semantic_analysis::AnalysisResult) {
    let ctx = beamtalk_core::semantic_analysis::AnalysisContext::default()
        .with_known_vars(known_vars)
        .with_pre_loaded_classes(pre_loaded_classes)
        .with_pre_loaded_aliases(pre_loaded_aliases);
    let mut analysis_result = beamtalk_core::semantic_analysis::analyse_full(module, ctx);
    // BT-3123: diagnostics are consumed by `run_diagnostic_pipeline` below;
    // take them out so the rest of `analysis_result` can be returned for
    // codegen without cloning it.
    let analysis_diagnostics = std::mem::take(&mut analysis_result.diagnostics);
    let diagnostics = run_diagnostic_pipeline(
        module,
        parse_diagnostics,
        analysis_diagnostics,
        diagnostics_overrides,
    );
    (diagnostics, analysis_result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};

    #[test]
    fn compute_diagnostics_returns_parse_errors() {
        let source = "x := :="; // Invalid syntax
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        let diagnostics = compute_diagnostics(&module, parse_diags);

        assert!(!diagnostics.is_empty());
    }

    #[test]
    fn compute_diagnostics_empty_for_valid_code() {
        let source = "x := 42";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        let diagnostics = compute_diagnostics(&module, parse_diags);

        assert!(diagnostics.is_empty());
    }

    #[test]
    fn compute_diagnostics_emits_error_for_field_assignment_in_stored_block() {
        // Test: myBlock := [self.sum := 0]
        let source = "myBlock := [self.sum := 0]";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        let diagnostics = compute_diagnostics(&module, parse_diags);

        // Should have error for field assignment in stored block
        let has_field_error = diagnostics.iter().any(|d| {
            d.message.contains("cannot assign to field") && d.message.contains("stored closure")
        });
        assert!(
            has_field_error,
            "Expected field assignment error, got: {diagnostics:?}"
        );
    }

    #[test]
    fn compute_diagnostics_no_error_for_field_assignment_in_field_stored_block() {
        // BT-2797: self.onTick := [:x | self.sum := 0] must NOT emit the
        // stored-closure field error — unlike the local-var case above
        // (compute_diagnostics_emits_error_for_field_assignment_in_stored_block,
        // still unaffected), a block stored into a *field* is unconditionally
        // safe: every self.field value(:...) call site now runtime-discriminates
        // Tier 1 vs Tier 2, regardless of which method later invokes it.
        let source = "self.onTick := [:x | self.sum := 0]";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_field_error = diagnostics.iter().any(|d| {
            d.message.contains("cannot assign to field") && d.message.contains("stored closure")
        });
        assert!(
            !has_field_error,
            "Should not have field-in-stored-block error for a block stored \
             into a field (BT-2797), got: {diagnostics:?}"
        );
    }

    #[test]
    fn compute_diagnostics_no_warning_for_captured_variable_mutation_in_stored_block() {
        // BT-856 (ADR 0041 Phase 3): Captured variable mutations in stored blocks are
        // now supported via the Tier 2 stateful block protocol (BT-852). No warning needed.
        let source = "count := 0. myBlock := [count := count + 1]";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        let diagnostics = compute_diagnostics(&module, parse_diags);

        // Should NOT have a warning for captured variable mutation — Tier 2 handles it correctly
        let has_stale_warning = diagnostics
            .iter()
            .any(|d| d.message.contains("has no effect on outer scope"));
        assert!(
            !has_stale_warning,
            "Unexpected stale warning for captured variable mutation: {diagnostics:?}"
        );
    }

    #[test]
    fn compute_diagnostics_no_error_for_control_flow_blocks() {
        // Test: 10 timesRepeat: [self.sum := 0]
        let source = "10 timesRepeat: [self.sum := 0]";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        let diagnostics = compute_diagnostics(&module, parse_diags);

        // Should NOT have field assignment error for control flow blocks
        let has_field_error = diagnostics
            .iter()
            .any(|d| d.message.contains("cannot assign to field"));
        assert!(
            !has_field_error,
            "Should not have field assignment error for control flow, got: {diagnostics:?}"
        );
    }

    #[test]
    fn compute_diagnostics_with_known_vars_suppresses_undefined_error() {
        // Without known vars, 'x' would be undefined
        let source = "x + 1";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        // With empty known_vars, should report undefined variable
        let diags_without = compute_diagnostics_with_known_vars(&module, parse_diags.clone(), &[]);
        assert!(
            diags_without
                .iter()
                .any(|d| d.message.contains("Undefined variable: x")),
            "Should report undefined variable without known vars"
        );

        // With 'x' in known_vars, should NOT report undefined
        let diags_with = compute_diagnostics_with_known_vars(&module, parse_diags, &["x"]);
        assert!(
            !diags_with
                .iter()
                .any(|d| d.message.contains("Undefined variable: x")),
            "Should not report undefined variable when in known_vars, got: {diags_with:?}"
        );
    }

    #[test]
    fn compute_diagnostics_with_known_vars_handles_multiple_vars() {
        // Expression using multiple REPL variables
        let source = "x + y * z";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        // All three should be recognized as known
        let diagnostics =
            compute_diagnostics_with_known_vars(&module, parse_diags, &["x", "y", "z"]);

        let has_undefined = diagnostics
            .iter()
            .any(|d| d.message.contains("Undefined variable"));
        assert!(
            !has_undefined,
            "Should not report any undefined variables, got: {diagnostics:?}"
        );
    }

    /// BT-2839 (ADR 0100 Rule 3 surface-parity gap): the REPL's diagnostics
    /// entry point must apply the `[diagnostics]` table exactly like
    /// `beamtalk build` (BT-2793) and the LSP (BT-2800) — a `dnu = "error"`
    /// override promotes the default `Hint` on an unresolved selector to
    /// `Error`, and an empty table (no manifest) is a complete no-op.
    #[test]
    fn compute_diagnostics_with_known_vars_and_classes_applies_severity_overrides() {
        use beamtalk_core::compilation::diagnostics_policy::{
            DiagnosticSeverityOverride, DiagnosticsTable,
        };
        use beamtalk_core::source_analysis::{DiagnosticCategory, Severity};

        let source = "\"hello\" frobnicate";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        // Baseline: empty table (no manifest) is a no-op — Rule 1 default is Hint.
        let baseline = compute_diagnostics_with_known_vars_and_classes(
            &module,
            parse_diags.clone(),
            &[],
            vec![],
            &DiagnosticsTable::new(),
        );
        assert!(
            baseline.iter().any(
                |d| d.category == Some(DiagnosticCategory::Dnu) && d.severity == Severity::Hint
            ),
            "expected a Dnu Hint with no overrides: {baseline:?}"
        );

        let mut table = DiagnosticsTable::new();
        table.insert(DiagnosticCategory::Dnu, DiagnosticSeverityOverride::Error);
        let overridden = compute_diagnostics_with_known_vars_and_classes(
            &module,
            parse_diags,
            &[],
            vec![],
            &table,
        );
        assert!(
            overridden
                .iter()
                .any(|d| d.category == Some(DiagnosticCategory::Dnu)
                    && d.severity == Severity::Error),
            "dnu = \"error\" override must promote the Dnu diagnostic to Error: {overridden:?}"
        );
    }

    // ── BT-563 / BT-1524: Actor subclass new/new: errors ──

    #[test]
    fn error_actor_subclass_new() {
        // Counter is an Actor subclass — using `new` should error
        let source = "Actor subclass: Counter\n  state: value = 0\n  increment => self.value := self.value + 1\n\nCounter new";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_error = diagnostics.iter().any(|d| {
            d.message.contains("Actor subclass")
                && d.message.contains("spawn")
                && d.severity == beamtalk_core::source_analysis::Severity::Error
        });
        assert!(has_error, "Expected actor new error, got: {diagnostics:?}");
    }

    #[test]
    fn error_actor_subclass_new_with_args() {
        let source = "Actor subclass: Counter\n  state: value = 0\n  increment => self.value := self.value + 1\n\nCounter new: #{value => 0}";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_error = diagnostics.iter().any(|d| {
            d.message.contains("Actor subclass")
                && d.message.contains("spawn")
                && d.message.contains("new:")
                && d.severity == beamtalk_core::source_analysis::Severity::Error
        });
        assert!(has_error, "Expected actor new: error, got: {diagnostics:?}");
    }

    #[test]
    fn no_warn_actor_subclass_spawn() {
        let source = "Actor subclass: Counter\n  state: value = 0\n  increment => self.value := self.value + 1\n\nCounter spawn";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_warning = diagnostics.iter().any(|d| {
            d.message.contains("Actor subclass")
                && d.severity == beamtalk_core::source_analysis::Severity::Warning
        });
        assert!(
            !has_warning,
            "Should not warn on spawn, got: {diagnostics:?}"
        );
    }

    #[test]
    fn no_warn_non_actor_new() {
        // Object subclass using new should NOT warn
        let source =
            "Object subclass: Point\n  state: x = 0\n  state: y = 0\n  getX => self.x\n\nPoint new";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_warning = diagnostics
            .iter()
            .any(|d| d.message.contains("Actor subclass") && d.message.contains("spawn"));
        assert!(
            !has_warning,
            "Should not warn for non-Actor subclass, got: {diagnostics:?}"
        );
    }

    #[test]
    fn warn_actor_new_has_hint() {
        let source = "Actor subclass: Counter\n  state: value = 0\n  increment => self.value := self.value + 1\n\nCounter new";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let diag = diagnostics
            .iter()
            .find(|d| d.message.contains("Actor subclass"))
            .expect("Should have actor warning");
        assert!(
            diag.hint.as_ref().is_some_and(|h| h.contains("spawn")),
            "Should have hint about spawn, got: {:?}",
            diag.hint
        );
    }

    // ── BT-563: Field name validation ──

    #[test]
    fn warn_unknown_field_in_new() {
        let source = "Object subclass: Point\n  state: x = 0\n  state: y = 0\n  getX => self.x\n\nPoint new: #{#z => 1}";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_warning = diagnostics
            .iter()
            .any(|d| d.message.contains("Unknown field") && d.message.contains('z'));
        assert!(
            has_warning,
            "Expected unknown field warning, got: {diagnostics:?}"
        );
    }

    #[test]
    fn no_warn_valid_fields_in_new() {
        let source = "Object subclass: Point\n  state: x = 0\n  state: y = 0\n  getX => self.x\n\nPoint new: #{#x => 1, #y => 2}";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_field_warning = diagnostics
            .iter()
            .any(|d| d.message.contains("Unknown field"));
        assert!(
            !has_field_warning,
            "Should not warn for valid fields, got: {diagnostics:?}"
        );
    }

    #[test]
    fn warn_unknown_field_has_hint() {
        let source = "Object subclass: Point\n  state: x = 0\n  state: y = 0\n  getX => self.x\n\nPoint new: #{#z => 1}";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let diag = diagnostics
            .iter()
            .find(|d| d.message.contains("Unknown field"))
            .expect("Should have unknown field warning");
        assert!(
            diag.hint
                .as_ref()
                .is_some_and(|h| h.contains('x') && h.contains('y')),
            "Should hint about declared fields, got: {:?}",
            diag.hint
        );
    }

    // ── BT-563: Class variable access ──

    #[test]
    fn warn_undefined_classvar() {
        let source = "Object subclass: Config\n  classState: debug = false\n  check => 1\n\nConfig classState: #verbose";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_warning = diagnostics.iter().any(|d| {
            d.message.contains("Undefined class variable") && d.message.contains("verbose")
        });
        assert!(
            has_warning,
            "Expected undefined classState warning, got: {diagnostics:?}"
        );
    }

    #[test]
    fn no_warn_valid_classvar() {
        let source = "Object subclass: Config\n  classState: debug = false\n  check => 1\n\nConfig classState: #debug";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_warning = diagnostics
            .iter()
            .any(|d| d.message.contains("Undefined class variable"));
        assert!(
            !has_warning,
            "Should not warn for valid classState, got: {diagnostics:?}"
        );
    }

    #[test]
    fn warn_classvar_has_hint() {
        let source = "Object subclass: Config\n  classState: debug = false\n  check => 1\n\nConfig classState: #verbose";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let diag = diagnostics
            .iter()
            .find(|d| d.message.contains("Undefined class variable"))
            .expect("Should have classState warning");
        assert!(
            diag.hint.as_ref().is_some_and(|h| h.contains("debug")),
            "Should hint about declared class vars, got: {:?}",
            diag.hint
        );
    }

    #[test]
    fn error_actor_new_inside_method_body() {
        // Actor new error should also fire inside method bodies
        let source = "Actor subclass: Counter\n  state: value = 0\n  increment => self.value := self.value + 1\n\nObject subclass: Factory\n  make => Counter new";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_error = diagnostics.iter().any(|d| {
            d.message.contains("Actor subclass")
                && d.message.contains("spawn")
                && d.severity == beamtalk_core::source_analysis::Severity::Error
        });
        assert!(
            has_error,
            "Expected actor new error inside method, got: {diagnostics:?}"
        );
    }

    #[test]
    fn no_warn_class_without_state_in_new() {
        // Classes with no declared state should not warn about fields
        let source = "Object subclass: Empty\n  greet => 42\n\nEmpty new: #{#x => 1}";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_field_warning = diagnostics
            .iter()
            .any(|d| d.message.contains("Unknown field"));
        assert!(
            !has_field_warning,
            "Should not warn about fields for class with no state, got: {diagnostics:?}"
        );
    }

    #[test]
    fn type_checker_warning_with_hint() {
        // Type checker should surface warnings with "Did you mean" hints
        let source = "42 lenght";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let type_warning = diagnostics
            .iter()
            .find(|d| d.message.contains("does not understand"));
        assert!(
            type_warning.is_some(),
            "Should emit type warning for unknown selector. Got: {diagnostics:?}"
        );
    }

    #[test]
    fn type_checker_dnu_severity_is_hint() {
        use beamtalk_core::source_analysis::Severity;

        let source = "42 foo";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let dnu_diag = diagnostics
            .iter()
            .find(|d| d.message.contains("does not understand"));
        assert!(
            dnu_diag.is_some(),
            "Should have DNU diagnostic. Got: {diagnostics:?}"
        );
        assert_eq!(
            dnu_diag.unwrap().severity,
            Severity::Hint,
            "DNU diagnostics should be Hint severity, not Warning"
        );
    }

    // ── BT-631: Empty method body warnings ──

    #[test]
    fn error_empty_instance_method_body() {
        let source = "Object subclass: Foo\n  doNothing =>";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_error = diagnostics.iter().any(|d| {
            d.message.contains("doNothing")
                && d.message.contains("empty body")
                && d.severity == beamtalk_core::source_analysis::Severity::Error
        });
        assert!(has_error, "Expected empty body error, got: {diagnostics:?}");
    }

    #[test]
    fn error_empty_class_method_body() {
        let source = "Object subclass: Foo\n  class reset =>";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_error = diagnostics.iter().any(|d| {
            d.message.contains("reset")
                && d.message.contains("empty body")
                && d.severity == beamtalk_core::source_analysis::Severity::Error
        });
        assert!(
            has_error,
            "Expected empty body error for class method, got: {diagnostics:?}"
        );
    }

    #[test]
    fn no_warning_for_nonempty_method() {
        let source = "Object subclass: Foo\n  getValue => 42";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_empty_warning = diagnostics.iter().any(|d| d.message.contains("empty body"));
        assert!(
            !has_empty_warning,
            "Should not warn about non-empty method, got: {diagnostics:?}"
        );
    }

    #[test]
    fn empty_body_error_has_hint() {
        let source = "Object subclass: Foo\n  doNothing =>";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let error = diagnostics
            .iter()
            .find(|d| d.message.contains("empty body"));
        assert!(error.is_some(), "Expected error, got: {diagnostics:?}");
        assert!(error.unwrap().hint.is_some(), "Error should have a hint");
    }

    // ── BT-782: @expect directive ──

    #[test]
    fn expect_dnu_suppresses_dnu_hint() {
        // @expect dnu before a message send that has a DNU hint
        let source = "@expect dnu\n42 foo";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let dnu = diagnostics
            .iter()
            .any(|d| d.message.contains("does not understand"));
        assert!(
            !dnu,
            "DNU hint should be suppressed by @expect dnu, got: {diagnostics:?}"
        );
    }

    #[test]
    fn expect_dnu_stale_when_no_dnu() {
        // @expect dnu where there is no DNU diagnostic → stale warning
        let source = "@expect dnu\n42";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let stale = diagnostics
            .iter()
            .any(|d| d.message.contains("stale @expect"));
        assert!(
            stale,
            "Should emit stale @expect error, got: {diagnostics:?}"
        );
    }

    #[test]
    fn expect_all_suppresses_any_diagnostic() {
        // @expect all suppresses any diagnostic on the following expression
        let source = "@expect all\n42 foo";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let dnu = diagnostics
            .iter()
            .any(|d| d.message.contains("does not understand"));
        assert!(
            !dnu,
            "@expect all should suppress DNU, got: {diagnostics:?}"
        );
    }

    #[test]
    fn expect_dnu_in_method_body() {
        // @expect dnu inside a method body
        let source = "Object subclass: Foo\n  test =>\n    @expect dnu\n    42 unknownMethod";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let dnu = diagnostics
            .iter()
            .any(|d| d.message.contains("does not understand"));
        assert!(
            !dnu,
            "DNU hint in method should be suppressed, got: {diagnostics:?}"
        );
    }

    // ── BT-1273: @expect type covers method-not-found at type-erasure boundaries ──

    #[test]
    fn expect_type_suppresses_dnu_hint() {
        // BT-1273: @expect type suppresses DNU hints in addition to type-mismatch warnings.
        let source = "@expect type\n42 unknownMethod";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let dnu = diagnostics
            .iter()
            .any(|d| d.message.contains("does not understand"));
        assert!(
            !dnu,
            "@expect type should suppress DNU hint, got: {diagnostics:?}"
        );
        // And no stale error either
        let stale = diagnostics
            .iter()
            .any(|d| d.message.contains("stale @expect"));
        assert!(
            !stale,
            "@expect type must not be stale when DNU hint is present, got: {diagnostics:?}"
        );
    }

    #[test]
    fn expect_type_stale_when_neither_type_nor_dnu() {
        // BT-1273: @expect type is still stale when there is no type or DNU diagnostic.
        let source = "@expect type\n42";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let stale = diagnostics
            .iter()
            .any(|d| d.message.contains("stale @expect"));
        assert!(
            stale,
            "@expect type on `42` (no diagnostic) must emit stale warning, got: {diagnostics:?}"
        );
    }

    #[test]
    fn unknown_expect_category_is_parse_error() {
        // @expect typo should emit a parse error (prevents silent suppression of nothing)
        let source = "@expect selfcapture\n42 foo";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_error = diagnostics
            .iter()
            .any(|d| d.message.contains("unknown @expect category"));
        assert!(
            has_error,
            "Typo in @expect category should be a parse error, got: {diagnostics:?}"
        );
    }

    // ── BT-3387: combined `@expect cat1, cat2` form ──

    #[test]
    fn expect_combined_categories_suppresses_dnu() {
        // @expect dnu, type is still valid single-line syntax when only one
        // category is actually needed — the comma form must not break the
        // plain single-category case.
        let source = "@expect dnu, type\n42 unknownMethod";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        assert!(
            parse_diags.is_empty(),
            "Should have no parse errors, got: {parse_diags:?}"
        );
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let dnu = diagnostics
            .iter()
            .any(|d| d.message.contains("does not understand"));
        assert!(
            !dnu,
            "@expect dnu, type should suppress DNU hint, got: {diagnostics:?}"
        );
        let stale = diagnostics
            .iter()
            .any(|d| d.message.contains("stale @expect"));
        assert!(
            !stale,
            "@expect dnu, type must not be stale when DNU hint is present, got: {diagnostics:?}"
        );
    }

    #[test]
    fn expect_combined_categories_unknown_name_skipped_valid_still_applies() {
        // BT-3387: a typo mixed into a category list should still report an
        // "unknown @expect category" error for the bad name, while the
        // other, valid name in the same directive still suppresses.
        let source = "@expect selfcapture, dnu\n42 foo";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let has_error = diagnostics
            .iter()
            .any(|d| d.message.contains("unknown @expect category"));
        assert!(
            has_error,
            "Typo in one of several categories should still be a parse error, got: {diagnostics:?}"
        );
        let dnu = diagnostics
            .iter()
            .any(|d| d.message.contains("does not understand"));
        assert!(
            !dnu,
            "the other, valid category in the list should still suppress DNU, got: {diagnostics:?}"
        );
    }

    #[test]
    fn expect_combined_categories_comma_does_not_continue_across_a_newline() {
        // Review follow-up on BT-3387: a trailing comma at the end of an
        // @expect line (e.g. a typo or an aborted edit) must not have the
        // next line's leading identifier silently absorbed into the
        // category list just because it happens to spell a real category
        // name (`type` here doubles as a very plausible variable/receiver
        // name). The comma-continuation must be same-line only, mirroring
        // the existing same-line rule for the reason string.
        let source = "@expect dnu,\ntype unknownMethod";
        let tokens = lex_with_eof(source);
        let (module, _parse_diags) = parse(tokens);

        match &module.expressions[0].expression {
            beamtalk_core::ast::Expression::ExpectDirective { categories, .. } => {
                assert_eq!(
                    categories,
                    &[beamtalk_core::ast::ExpectCategory::Dnu],
                    "the next line's `type` must not be absorbed into the category list"
                );
            }
            other => panic!("expected ExpectDirective, got: {other:?}"),
        }
        // The dangling comma must be consumed with its own diagnostic (not
        // left dangling as a stray token) so `type unknownMethod` still
        // parses as its own statement.
        assert_eq!(module.expressions.len(), 2, "got: {:?}", module.expressions);
        assert!(
            matches!(
                &module.expressions[1].expression,
                beamtalk_core::ast::Expression::MessageSend { .. }
            ),
            "the next line must still parse as its own statement, got: {:?}",
            module.expressions[1].expression
        );
    }

    #[test]
    fn expect_combined_categories_dangling_comma_does_not_truncate_class_body() {
        // BT-3387 review follow-up: a trailing comma at the end of a
        // declaration-level `@expect` line must not be left dangling —
        // `parse_class_body`'s caller treats a stray `,` as "not a valid
        // declaration" and would otherwise silently drop every subsequent
        // state/method declaration in the class (the same failure mode the
        // BT-1918 comment on the reason-string lookahead guards against).
        let source = "\
Object subclass: Foo
  @expect dnu,
  state: x = 0
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        assert!(
            parse_diags
                .iter()
                .any(|d| d.message.contains("trailing ','")),
            "expected a diagnostic about the dangling comma, got: {parse_diags:?}"
        );
        assert!(
            !parse_diags.iter().any(|d| d
                .message
                .contains("must precede a state/field or method declaration")),
            "the class body must not be treated as truncated, got: {parse_diags:?}"
        );
        assert_eq!(module.classes.len(), 1, "got: {:?}", module.classes);
        assert_eq!(
            module.classes[0].state.len(),
            1,
            "the state: x declaration after the dangling comma must still parse, got: {:?}",
            module.classes[0].state
        );
    }

    #[test]
    fn expect_declaration_no_category_garbage_does_not_truncate_class_body() {
        // Review follow-up: `@expect` immediately followed by a non-identifier,
        // non-string, same-line token (e.g. a stray `,`) with zero valid
        // categories parsed must not be left dangling either — same
        // class-body-truncation risk the dangling-comma-in-a-list fix
        // addresses, but for the "no identifier at all" branch.
        let source = "\
Object subclass: Foo
  @expect ,
  state: x = 0
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        assert!(
            parse_diags.iter().any(|d| d
                .message
                .contains("@expect must be followed by a category name")),
            "expected a parse error for the missing category, got: {parse_diags:?}"
        );
        assert!(
            !parse_diags.iter().any(|d| d
                .message
                .contains("must precede a state/field or method declaration")),
            "the class body must not be treated as truncated, got: {parse_diags:?}"
        );
        assert_eq!(module.classes.len(), 1, "got: {:?}", module.classes);
        assert_eq!(
            module.classes[0].state.len(),
            1,
            "the state: x declaration after the bad @expect must still parse, got: {:?}",
            module.classes[0].state
        );
    }

    #[test]
    fn expect_declaration_no_category_on_own_line_preserves_next_line_declaration() {
        // Companion to the test above: when `@expect` has nothing at all
        // after it on its own line, the real next declaration on the
        // *following* line must be left completely alone (not consumed as
        // if it were garbage) — it's the legitimate next declaration, e.g.
        // a category name simply forgotten.
        let source = "\
Object subclass: Foo
  @expect
  state: x = 0
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        assert!(
            parse_diags.iter().any(|d| d
                .message
                .contains("@expect must be followed by a category name")),
            "expected a parse error for the missing category, got: {parse_diags:?}"
        );
        assert_eq!(module.classes.len(), 1, "got: {:?}", module.classes);
        assert_eq!(
            module.classes[0].state.len(),
            1,
            "the state: x declaration on the next line must still parse, got: {:?}",
            module.classes[0].state
        );
    }

    #[test]
    fn expect_declaration_no_category_same_line_as_real_declaration_is_not_swallowed() {
        // Review follow-up: `@expect` with the category forgotten and NO
        // separator at all before the real declaration on the *same* line
        // (`@expect state: x = 0`) must not have `state:` swallowed as if
        // it were garbage — `state:` lexes as a Keyword, not an Identifier,
        // so it falls into the same "no category" recovery branch as a
        // stray `,`, but unlike a stray `,` it IS the real next
        // declaration and must be left alone.
        let source = "\
Object subclass: Foo
  @expect state: x = 0
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        assert!(
            parse_diags.iter().any(|d| d
                .message
                .contains("@expect must be followed by a category name")),
            "expected a parse error for the missing category, got: {parse_diags:?}"
        );
        assert_eq!(module.classes.len(), 1, "got: {:?}", module.classes);
        assert_eq!(
            module.classes[0].state.len(),
            1,
            "the state: x declaration must not be swallowed, got: {:?}",
            module.classes[0].state
        );
    }

    #[test]
    fn expect_combined_categories_comma_continuation_into_a_declaration_keyword_is_not_swallowed() {
        // Review follow-up: the comma-continuation loop can also land on a
        // declaration keyword — `@expect dnu, state: x = 0` continues past
        // the comma (both `dnu,` and `state:` are on the same line), then
        // hits `state:` expecting another category identifier. `state:`
        // must be left alone here too, for the same reason as the test
        // above.
        let source = "\
Object subclass: Foo
  @expect dnu, state: x = 0
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        assert!(
            parse_diags.iter().any(|d| d
                .message
                .contains("@expect must be followed by a category name")),
            "expected a parse error for the missing second category, got: {parse_diags:?}"
        );
        assert_eq!(module.classes.len(), 1, "got: {:?}", module.classes);
        assert_eq!(
            module.classes[0].state.len(),
            1,
            "the state: x declaration must not be swallowed, got: {:?}",
            module.classes[0].state
        );
    }

    #[test]
    fn expect_combined_categories_round_trips_through_unparse() {
        // @expect unresolved_ffi, type should round-trip through unparse.
        let source = "@expect unresolved_ffi, type\n42 unknownMethod";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);

        let output = beamtalk_core::unparse::unparse_module(&module);
        assert!(
            output.contains("@expect unresolved_ffi, type"),
            "Unparsed output should contain the combined category list, got: {output}"
        );
    }

    #[test]
    fn expect_combined_categories_on_method_declaration_round_trips() {
        // BT-3387's motivating case: a combined @expect on a method
        // declaration (the form that previously required splitting into two
        // methods, since stacking two separate `@expect` lines before a
        // declaration was rejected outright).
        let source = "\
typed Object subclass: MyTyped
  @expect unresolved_ffi, type
  publicKeyModule => Erlang public_key
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        assert!(
            parse_diags.is_empty(),
            "Should have no parse errors, got: {parse_diags:?}"
        );

        let output = beamtalk_core::unparse::unparse_module(&module);
        assert!(
            output.contains("@expect unresolved_ffi, type"),
            "Unparsed output should contain the combined category list, got: {output}"
        );
    }

    #[test]
    fn expect_with_no_category_on_declaration_does_not_truncate_class_body() {
        // BT-3387 review follow-up: a category-less `@expect` (not even an
        // invalid category name) followed by a stray token, e.g. a reason
        // string with nothing to attach to, must not derail parsing of the
        // rest of the class body. `parse_expect_tail`'s reason-string
        // lookahead deliberately runs even when no category was found, so
        // the stray string is consumed as a (discarded) reason rather than
        // left for `parse_class_body`'s caller to trip over as "not a valid
        // declaration" — which would otherwise silently end the class body
        // right there.
        let source = "\
Object subclass: Foo
  @expect \"oops\"
  state: x = 0
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        assert!(
            parse_diags.iter().any(|d| d
                .message
                .contains("@expect must be followed by a category name")),
            "expected a parse error for the missing category, got: {parse_diags:?}"
        );
        assert!(
            !parse_diags.iter().any(|d| d
                .message
                .contains("must precede a state/field or method declaration")),
            "the class body must not be treated as truncated, got: {parse_diags:?}"
        );
        assert_eq!(module.classes.len(), 1, "got: {:?}", module.classes);
        assert_eq!(
            module.classes[0].state.len(),
            1,
            "the state: x declaration after the bad @expect must still parse, got: {:?}",
            module.classes[0].state
        );
    }

    // ── BT-1476: Dead block assignment warning + @expect dead_assignment ──

    // ── BT-1476: @expect dead_assignment parsing and stale detection ──

    #[test]
    fn expect_dead_assignment_stale_when_no_diagnostic() {
        // @expect dead_assignment with no matching diagnostic → stale
        let source = "@expect dead_assignment\n42";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let stale = diagnostics
            .iter()
            .any(|d| d.message.contains("stale @expect"));
        assert!(
            stale,
            "Should emit stale @expect when no dead assignment, got: {diagnostics:?}"
        );
    }

    #[test]
    fn expect_dead_assignment_parses_correctly() {
        // @expect dead_assignment should parse without errors
        let source = "@expect dead_assignment\n42";
        let tokens = lex_with_eof(source);
        let (_, parse_diags) = parse(tokens);

        let has_parse_error = parse_diags
            .iter()
            .any(|d| d.message.contains("unknown @expect"));
        assert!(
            !has_parse_error,
            "dead_assignment should be a recognized @expect category, got: {parse_diags:?}"
        );
    }

    // ── BT-1856: Declaration-level @expect ──────────────────────────────────────

    #[test]
    fn typed_state_no_default_no_warning() {
        // BT-1947: A type annotation replaces the need for a default value.
        // `state: deps :: OrchestratorDeps` (no default) should produce no
        // uninitialized warning.
        let source = "\
Actor subclass: MyActor
  state: running :: Dictionary = #{}
  state: deps :: OrchestratorDeps
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let uninitialized = diagnostics
            .iter()
            .any(|d| d.message.contains("uninitialized"));
        assert!(
            !uninitialized,
            "Typed state without default should not warn (BT-1947), got: {diagnostics:?}"
        );
    }

    #[test]
    fn expect_type_on_method_suppresses_missing_annotation() {
        // @expect type before a method in a typed class should suppress
        // missing-type-annotation warnings.
        let source = "\
typed Object subclass: MyTyped
  @expect type
  first => 42
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let missing_annotation = diagnostics
            .iter()
            .any(|d| d.message.contains("Missing") && d.message.contains("type annotation"));
        assert!(
            !missing_annotation,
            "@expect type should suppress missing annotation warnings, got: {diagnostics:?}"
        );
    }

    #[test]
    fn stale_expect_on_state_declaration() {
        // @expect unused on a state field that is actually used should emit
        // a stale warning (since there is no unused-field diagnostic).
        let source = "\
Object subclass: Foo
  @expect unused
  state: x = 0

  getX => self.x
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let stale = diagnostics
            .iter()
            .any(|d| d.message.contains("stale @expect"));
        assert!(
            stale,
            "Should emit stale @expect when no matching diagnostic, got: {diagnostics:?}"
        );
    }

    #[test]
    fn stale_expect_on_method_declaration() {
        // @expect type on a fully-annotated method in a typed class should be stale.
        let source = "\
typed Object subclass: MyTyped
  @expect type
  getValue -> Integer => 42
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let stale = diagnostics
            .iter()
            .any(|d| d.message.contains("stale @expect"));
        assert!(
            stale,
            "Should emit stale @expect when method already has annotations, got: {diagnostics:?}"
        );
    }

    #[test]
    fn expect_type_on_state_round_trips_through_unparse() {
        // Parsing and unparsing @expect type on state should round-trip.
        let source = "\
Actor subclass: MyActor
  @expect type
  state: deps :: OrchestratorDeps
";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);

        let output = beamtalk_core::unparse::unparse_module(&module);
        assert!(
            output.contains("@expect type"),
            "Unparsed output should contain @expect type, got: {output}"
        );
        assert!(
            output.contains("state: deps :: OrchestratorDeps"),
            "Unparsed output should contain the state declaration, got: {output}"
        );
    }

    #[test]
    fn expect_type_on_method_round_trips_through_unparse() {
        // Parsing and unparsing @expect type on a method should round-trip.
        let source = "\
typed Object subclass: MyTyped
  @expect type
  first => 42
";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);

        let output = beamtalk_core::unparse::unparse_module(&module);
        assert!(
            output.contains("@expect type"),
            "Unparsed output should contain @expect type, got: {output}"
        );
    }

    #[test]
    fn expect_before_invalid_position_in_class_body() {
        // @expect at the end of a class body (not before state/method) should error.
        let source = "\
Object subclass: Foo
  state: x = 0
  @expect type

Object subclass: Bar
  state: y = 0
";
        let tokens = lex_with_eof(source);
        let (_, parse_diags) = parse(tokens);

        let invalid_pos = parse_diags
            .iter()
            .any(|d| d.message.contains("must precede"));
        assert!(
            invalid_pos,
            "@expect before invalid position should produce error, got: {parse_diags:?}"
        );
    }

    // ── BT-1918: TypeAnnotation category ──

    #[test]
    fn expect_type_annotation_suppresses_missing_annotation() {
        // @expect type_annotation before a method in a typed class should suppress
        // missing-type-annotation warnings but not type mismatch warnings.
        let source = "\
typed Object subclass: MyTyped
  @expect type_annotation
  first => 42
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let missing_annotation = diagnostics
            .iter()
            .any(|d| d.message.contains("Missing") && d.message.contains("type annotation"));
        assert!(
            !missing_annotation,
            "@expect type_annotation should suppress missing annotation warnings, got: {diagnostics:?}"
        );
        let stale = diagnostics
            .iter()
            .any(|d| d.message.contains("stale @expect"));
        assert!(
            !stale,
            "@expect type_annotation should not be stale, got: {diagnostics:?}"
        );
    }

    #[test]
    fn expect_type_annotation_does_not_suppress_type_mismatch() {
        // @expect type_annotation should NOT suppress DNU/type-mismatch warnings.
        let source = "@expect type_annotation\n42 unknownMethod";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let dnu = diagnostics
            .iter()
            .any(|d| d.message.contains("does not understand"));
        assert!(
            dnu,
            "@expect type_annotation should NOT suppress DNU hints, got: {diagnostics:?}"
        );
    }

    #[test]
    fn expect_type_annotation_on_state_field() {
        // @expect type_annotation on a state field in a typed class should suppress
        // the missing-annotation warning for that field.
        let source = "\
typed Object subclass: MyTyped
  @expect type_annotation
  state: count = 0
  getValue -> Integer => 42
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let missing_state = diagnostics.iter().any(|d| {
            d.message
                .contains("Missing type annotation for state field `count`")
        });
        assert!(
            !missing_state,
            "@expect type_annotation should suppress state field warning, got: {diagnostics:?}"
        );
    }

    // ── BT-1918: @expect reason strings ──

    #[test]
    fn expect_with_reason_parses_correctly() {
        // @expect dnu "FFI boundary" should parse without errors.
        let source = "@expect dnu \"FFI boundary\"\n42 unknownMethod";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        assert!(
            parse_diags.is_empty(),
            "Should have no parse errors, got: {parse_diags:?}"
        );
        let diagnostics = compute_diagnostics(&module, Vec::new());
        let dnu = diagnostics
            .iter()
            .any(|d| d.message.contains("does not understand"));
        assert!(
            !dnu,
            "@expect dnu with reason should still suppress DNU, got: {diagnostics:?}"
        );
    }

    #[test]
    fn stale_expect_with_reason_includes_reason_text() {
        // Stale @expect with reason should include the reason in the warning message.
        let source = "@expect dnu \"FFI boundary\"\n42";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let stale = diagnostics
            .iter()
            .find(|d| d.message.contains("stale @expect"));
        assert!(
            stale.is_some(),
            "Should emit stale warning, got: {diagnostics:?}"
        );
        assert!(
            stale.unwrap().message.contains("FFI boundary"),
            "Stale warning should include reason text, got: {}",
            stale.unwrap().message
        );
    }

    #[test]
    fn expect_with_reason_round_trips_through_unparse() {
        // @expect type "reason" should round-trip through unparse.
        let source = "@expect dnu \"FFI boundary\"\n42 unknownMethod";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);

        let output = beamtalk_core::unparse::unparse_module(&module);
        assert!(
            output.contains("@expect dnu \"FFI boundary\""),
            "Unparsed output should contain reason string, got: {output}"
        );
    }

    #[test]
    fn expect_with_reason_on_declaration_round_trips() {
        // @expect type_annotation "migrating" on a declaration should round-trip.
        let source = "\
typed Object subclass: MyTyped
  @expect type_annotation \"migrating\"
  first => 42
";
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);

        let output = beamtalk_core::unparse::unparse_module(&module);
        assert!(
            output.contains("@expect type_annotation \"migrating\""),
            "Unparsed output should contain reason string, got: {output}"
        );
    }

    // ── BT-1923: Drift prevention — every Warning/Hint must have a category ──

    /// Compiles a source snippet and returns only the Warning/Hint diagnostics.
    fn warnings_and_hints(source: &str) -> Vec<Diagnostic> {
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let all = compute_diagnostics(&module, parse_diags);
        all.into_iter()
            .filter(|d| {
                matches!(
                    d.severity,
                    beamtalk_core::source_analysis::Severity::Warning
                        | beamtalk_core::source_analysis::Severity::Hint
                        | beamtalk_core::source_analysis::Severity::Lint
                )
            })
            .collect()
    }

    /// BT-1923: Every Warning/Hint/Lint diagnostic MUST have a category.
    ///
    /// This test compiles source snippets that trigger diagnostics from every
    /// compiler phase (parser, name resolver, semantic analysis, type checker,
    /// lint validators, etc.). If a new warning or hint is added anywhere in
    /// the compiler without setting `.with_category(...)`, this test will fail.
    ///
    /// To fix a failure: find the `Diagnostic::warning(...)` or
    /// `Diagnostic::hint(...)` call that produces the uncategorised diagnostic
    /// and chain `.with_category(DiagnosticCategory::Foo)` onto it.
    #[test]
    fn all_warnings_and_hints_have_categories() {
        // Each snippet is designed to trigger one or more Warning/Hint diagnostics
        // from a specific compiler phase. We collect them all and assert that every
        // single one has `category.is_some()`.
        let snippets: Vec<(&str, &str)> = vec![
            // ── Name resolver: unused variable ──
            (
                "unused variable",
                "Object subclass: Foo\n  bar => x := 42. 0",
            ),
            // ── Name resolver: variable shadowing ──
            (
                "variable shadowing",
                "Object subclass: Foo\n  bar =>\n    x := 1.\n    [| :x | x + 1] value: 2",
            ),
            // ── Name resolver: unreachable code ──
            (
                "unreachable code after early return",
                "Object subclass: Foo\n  bar => ^1. 2",
            ),
            // ── Type checker: DNU hint ──
            (
                "DNU hint on unknown method",
                "typed Object subclass: Foo\n  state: x :: Integer = 0\n  bar => self.x noSuchMethod",
            ),
            // ── Type checker: type mismatch hint ──
            (
                "type mismatch in typed class",
                "typed Object subclass: Foo\n  state: x :: Integer = 0\n  bar => self.x := \"hello\"",
            ),
            // ── Lint validator: always-true condition ──
            ("always-true condition", "true ifTrue: [1] ifFalse: [2]"),
            // Note: "actor new" (`Actor subclass: A ... A new`) used to live here as
            // a Warning/Hint exemplar, but BT-3071 lifted Actor's `new`/`new:` into
            // real, hierarchy-resolvable `class sealed new`/`new:` declarations on
            // actor.bt — so the TypeChecker no longer treats the send as unknown and
            // stops contributing a Warning/Hint diagnostic for it (the DNU-style
            // secondary signal this snippet exercised). The actual "use spawn, not
            // new" protection is untouched: `check_actor_new_usage` (BT-563/BT-1524)
            // still raises a hard compile Error independent of hierarchy resolution
            // — see `semantic_analysis::tests::test_actor_new_error_in_standalone_method`
            // and `error_actor_subclass_new` below — just not a Warning/Hint/Lint this
            // category-completeness test cares about, so the snippet was removed
            // rather than left permanently vacuous.
            // ── Type checker: missing type annotation in typed class ──
            (
                "missing type annotation in typed class",
                "typed Object subclass: Foo\n  state: x = 0\n  bar => self.x",
            ),
            // ── Type checker: Dynamic inference warning in typed class ──
            (
                "dynamic inference in typed class",
                "typed Object subclass: Foo\n  state: x :: Integer = 0\n  bar :: Integer => self.x abs",
            ),
        ];

        let mut failures: Vec<String> = Vec::new();

        for (label, source) in &snippets {
            let diags = warnings_and_hints(source);
            for diag in &diags {
                if diag.category.is_none() {
                    failures.push(format!(
                        "[{label}] {severity:?} diagnostic has no category: \"{msg}\"",
                        severity = diag.severity,
                        msg = diag.message
                    ));
                }
            }
        }

        assert!(
            failures.is_empty(),
            "Found Warning/Hint/Lint diagnostics without categories \
             (drift prevention BT-1923):\n  {}",
            failures.join("\n  ")
        );
    }

    /// BT-1923: Sanity check — the drift prevention snippets actually produce diagnostics.
    ///
    /// If this test fails, it means the snippets no longer trigger any warnings/hints,
    /// which would make the category assertion vacuously true (and useless).
    #[test]
    fn drift_prevention_snippets_produce_diagnostics() {
        // A subset of snippets that should always produce at least one warning/hint.
        let must_produce = vec![
            (
                "unused variable",
                "Object subclass: Foo\n  bar => x := 42. 0",
            ),
            ("always-true condition", "true ifTrue: [1] ifFalse: [2]"),
            // "actor new" removed (BT-3071) — see the matching note in
            // `all_warnings_and_hints_have_categories` above; it no longer produces
            // a Warning/Hint/Lint diagnostic now that Actor's `new`/`new:` are real,
            // resolvable class methods, only the unrelated hard compile Error this
            // Warning/Hint-scoped test doesn't collect.
            (
                "missing type annotation in typed class",
                "typed Object subclass: Foo\n  state: x = 0\n  bar => self.x",
            ),
        ];

        for (label, source) in must_produce {
            let diags = warnings_and_hints(source);
            assert!(
                !diags.is_empty(),
                "Snippet [{label}] should produce at least one Warning/Hint/Lint diagnostic \
                 but produced none — update the drift prevention test snippets"
            );
        }
    }

    // ── BT-2010: @expect inside block bodies ──────────────────────────────────

    #[test]
    fn expect_dnu_inside_block_body_suppresses_dnu() {
        // @expect dnu inside an ifTrue: [...] block body should suppress the DNU
        // hint on the next expression inside the same block.
        let source = "\
Object subclass: Foo
  test =>
    true ifTrue: [
      @expect dnu
      42 unknownMethod
    ]
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let dnu = diagnostics
            .iter()
            .any(|d| d.message.contains("does not understand"));
        assert!(
            !dnu,
            "@expect dnu inside block body should suppress DNU hint, got: {diagnostics:?}"
        );
    }

    #[test]
    fn expect_dnu_inside_nested_block_works() {
        // @expect dnu inside a nested block (block-inside-block) should work.
        let source = "\
Object subclass: Foo
  test =>
    true ifTrue: [
      true ifTrue: [
        @expect dnu
        42 unknownMethod
      ]
    ]
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let dnu = diagnostics
            .iter()
            .any(|d| d.message.contains("does not understand"));
        assert!(
            !dnu,
            "@expect dnu inside nested block should suppress DNU, got: {diagnostics:?}"
        );
    }

    #[test]
    fn stale_expect_inside_block_body_is_reported() {
        // @expect dnu inside a block body where no DNU diagnostic fires should
        // produce a stale @expect warning.
        let source = "\
Object subclass: Foo
  test =>
    true ifTrue: [
      @expect dnu
      42
    ]
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let stale = diagnostics
            .iter()
            .any(|d| d.message.contains("stale @expect"));
        assert!(
            stale,
            "Stale @expect inside block body should be reported, got: {diagnostics:?}"
        );
    }

    #[test]
    fn expect_at_method_level_still_works() {
        // Existing @expect at method-body level must remain unchanged.
        let source = "\
Object subclass: Foo
  test =>
    @expect dnu
    42 unknownMethod
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let dnu = diagnostics
            .iter()
            .any(|d| d.message.contains("does not understand"));
        assert!(
            !dnu,
            "@expect dnu at method level should still suppress DNU, got: {diagnostics:?}"
        );
    }

    #[test]
    fn expect_at_module_level_still_works() {
        // Existing @expect at module level must remain unchanged.
        let source = "@expect dnu\n42 unknownMethod";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        let diagnostics = compute_diagnostics(&module, parse_diags);

        let dnu = diagnostics
            .iter()
            .any(|d| d.message.contains("does not understand"));
        assert!(
            !dnu,
            "@expect dnu at module level should still suppress DNU, got: {diagnostics:?}"
        );
    }

    // ── BT-2009: Unified pipeline consistency ──────────────────────────────────

    #[test]
    fn project_diagnostics_matches_legacy_path() {
        // BT-2009: The unified `compute_project_diagnostics` must produce the
        // same diagnostics as the old `compute_diagnostics_with_native_types`
        // when given equivalent inputs (no cross-file classes, no dep registry).
        let source = "42 unknownMethod";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        // Old path
        let old_diags = compute_diagnostics_with_native_types(&module, parse_diags.clone(), None);

        // New unified path with default context (no project-level inputs)
        let ctx = ProjectDiagnosticContext::default();
        let new_diags = compute_project_diagnostics(&module, source, parse_diags, &ctx);

        // Both should contain a DNU hint
        let old_dnu = old_diags
            .iter()
            .any(|d| d.message.contains("does not understand"));
        let new_dnu = new_diags
            .iter()
            .any(|d| d.message.contains("does not understand"));
        assert_eq!(
            old_dnu, new_dnu,
            "Old and new pipelines should agree on DNU diagnostics"
        );

        // Same number of diagnostics
        assert_eq!(
            old_diags.len(),
            new_diags.len(),
            "Old path produced {} diagnostics, new path produced {}: \nold: {old_diags:?}\nnew: {new_diags:?}",
            old_diags.len(),
            new_diags.len(),
        );
    }

    #[test]
    fn project_diagnostics_expect_type_in_typed_class() {
        // BT-2009: This is the exact case that previously diverged between CLI
        // and LSP. In a typed class, calling a method with no return annotation
        // triggers "expression inferred as Dynamic". `@expect type` must
        // suppress that warning in both pipelines.
        let source = "\
typed Object subclass: Callee
  helper => 42

typed Object subclass: Caller
  @expect type
  run => Callee new helper
";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        let ctx = ProjectDiagnosticContext::default();
        let diagnostics = compute_project_diagnostics(&module, source, parse_diags, &ctx);

        let stale = diagnostics
            .iter()
            .any(|d| d.message.contains("stale @expect"));
        assert!(
            !stale,
            "@expect type should not be stale in unified pipeline \
             (BT-2009 divergence case), got: {diagnostics:?}"
        );
    }

    #[test]
    fn project_diagnostics_with_cross_file_classes() {
        // BT-2009: When cross-file class metadata is provided, the unified
        // pipeline should use it for type checking. This verifies the
        // cross-file classes are actually threaded through to semantic analysis.

        // Parse a "helper" module to extract its ClassInfo
        let helper_source = "\
Object subclass: Helper
  greet => 42
";
        let helper_tokens = lex_with_eof(helper_source);
        let (helper_module, _) = parse(helper_tokens);
        let helper_infos =
            beamtalk_core::semantic_analysis::ClassHierarchy::extract_class_infos(&helper_module);

        // Parse a "user" module that references the helper class.
        // With cross-file classes, the type checker knows about Helper
        // and can verify `greet` exists, so no DNU hint is produced.
        let user_source = "Helper new greet";
        let user_tokens = lex_with_eof(user_source);
        let (user_module, parse_diags) = parse(user_tokens);

        // With cross-file classes: Helper is known, `greet` resolves cleanly.
        let ctx = ProjectDiagnosticContext {
            cross_file_classes: helper_infos,
            ..Default::default()
        };
        let diagnostics = compute_project_diagnostics(&user_module, user_source, parse_diags, &ctx);

        // `greet` should NOT produce a DNU hint when Helper is in the hierarchy.
        let dnu_greet = diagnostics
            .iter()
            .any(|d| d.message.contains("does not understand") && d.message.contains("greet"));
        assert!(
            !dnu_greet,
            "With cross-file classes, 'greet' should be resolved, got: {diagnostics:?}"
        );
    }

    #[test]
    fn project_diagnostics_surfaces_near_miss_divider() {
        // BT-3240: `compute_project_diagnostics` (the LSP-facing pipeline)
        // must actually surface a near-miss-divider finding, not just the
        // lint module's own unit tests in isolation.
        let source = "Object subclass: Foo\n  // === Section ====\n  bar => 1\n";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        let ctx = ProjectDiagnosticContext::default();
        let diagnostics = compute_project_diagnostics(&module, source, parse_diags, &ctx);

        let near_miss = diagnostics
            .iter()
            .find(|d| d.message.contains("section divider"));
        let near_miss = near_miss.unwrap_or_else(|| {
            panic!("expected a near-miss-divider diagnostic, got: {diagnostics:?}")
        });
        assert_eq!(near_miss.severity, Severity::Lint);
    }

    #[test]
    fn project_diagnostics_lint_error_override_does_not_promote_near_miss_divider() {
        // BT-3240 (adversarial review): `apply_diagnostics_table` keys
        // purely on `DiagnosticCategory`, regardless of a diagnostic's
        // starting severity — so a project that sets `[diagnostics] lint =
        // "error"` must not be able to promote the near-miss-divider
        // diagnostic to `Severity::Error` and break `beamtalk build`
        // (which unconditionally skips `Severity::Lint`, but has no such
        // exemption for `Error`). Every other lint pass never reaches this
        // pipeline at all, so this key must stay a no-op for this one too.
        let source = "Object subclass: Foo\n  // === Section ====\n  bar => 1\n";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        let mut overrides = beamtalk_core::compilation::diagnostics_policy::DiagnosticsTable::new();
        overrides.insert(
            DiagnosticCategory::Lint,
            beamtalk_core::compilation::diagnostics_policy::DiagnosticSeverityOverride::Error,
        );
        let ctx = ProjectDiagnosticContext {
            diagnostics_overrides: overrides,
            ..Default::default()
        };
        let diagnostics = compute_project_diagnostics(&module, source, parse_diags, &ctx);

        let near_miss = diagnostics
            .iter()
            .find(|d| d.message.contains("section divider"));
        let near_miss = near_miss.unwrap_or_else(|| {
            panic!("expected a near-miss-divider diagnostic, got: {diagnostics:?}")
        });
        assert_eq!(
            near_miss.severity,
            Severity::Lint,
            "near-miss-divider severity must never be promoted by the \
             `[diagnostics]` table, even when `lint = \"error\"` is set — \
             got: {diagnostics:?}"
        );
    }

    #[test]
    fn project_diagnostics_stdlib_shadowing_in_non_stdlib_mode() {
        // BT-2009: The unified pipeline should run stdlib name shadowing
        // checks when stdlib_mode is false.
        let source = "Object subclass: Integer\n  foo => 42";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        let ctx = ProjectDiagnosticContext {
            options: beamtalk_core::CompilerOptions {
                stdlib_mode: false,
                ..Default::default()
            },
            ..Default::default()
        };
        let diagnostics = compute_project_diagnostics(&module, source, parse_diags, &ctx);

        let has_shadow = diagnostics
            .iter()
            .any(|d| d.message.contains("conflicts with a stdlib class"));
        assert!(
            has_shadow,
            "Should warn about shadowing stdlib class name 'Integer', got: {diagnostics:?}"
        );
    }

    #[test]
    fn project_diagnostics_no_stdlib_shadowing_in_stdlib_mode() {
        // BT-2009: The unified pipeline should NOT run stdlib name shadowing
        // checks when stdlib_mode is true.
        let source = "Object subclass: Integer\n  foo => 42";
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);

        let ctx = ProjectDiagnosticContext {
            options: beamtalk_core::CompilerOptions {
                stdlib_mode: true,
                ..Default::default()
            },
            ..Default::default()
        };
        let diagnostics = compute_project_diagnostics(&module, source, parse_diags, &ctx);

        let has_shadow = diagnostics
            .iter()
            .any(|d| d.message.contains("conflicts with a stdlib class"));
        assert!(
            !has_shadow,
            "Should NOT warn about shadowing in stdlib_mode, got: {diagnostics:?}"
        );
    }
}
