// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Helpers for turning a parsed inline class/protocol/type-alias
//! declaration (a REPL `compile_expression` turn) into its response —
//! shared by [`super::expression::handle_compile_expression`].

use eetf::Term;

use crate::decode::merge_method;
use crate::respond::{
    class_definition_ok_response, error_response, format_codegen_error,
    protocol_definition_ok_response, type_alias_definition_ok_response,
};

/// Derive a BEAM module name for a class, using either an explicit
/// override (from package-mode callers) or the default `bt@{snake_case}`
/// convention.  All code paths that produce module names for `.bt` classes
/// should call this function so the derivation logic is unified.
///
/// ADR 0119: the no-override branch mints the name via
/// `ClassModuleRegistry::assign` — this is exactly that method's documented
/// use case (a hot-reloaded inline class definition with no `.bt` source
/// file to derive a path-based name from), exercising the registry primitive
/// `beamtalk-core` provides. `module_name_override` is left as a
/// direct pass-through: it is the exact, already-correct (subdirectory-aware)
/// module name a package-mode caller's own Pass 1 computed, not a name this
/// function should re-derive.
pub(crate) fn derive_class_module_name(
    class_name: &str,
    module_name_override: Option<&str>,
    stdlib_mode: bool,
) -> String {
    use beamtalk_core::semantic_analysis::{ClassModuleRegistry, ModuleNamingScheme, PackageId};

    if let Some(name) = module_name_override {
        return name.to_string();
    }
    let (pkg, naming) = if stdlib_mode {
        (PackageId::Stdlib, ModuleNamingScheme::Stdlib)
    } else {
        (PackageId::SingleFile, ModuleNamingScheme::SingleFile)
    };
    ClassModuleRegistry::new()
        .assign(&pkg, class_name, &naming)
        .as_str()
        .to_string()
}

/// Handle inline class definition in REPL expression context.
/// Merges any standalone method definitions into the class, generates code,
/// and returns a `class_definition` response.
/// Also compiles any trailing expressions and includes them in the response.
/// Accepts `class_superclass_index` to resolve cross-file inheritance chains.
/// Accepts `class_module_index` for package-qualified class references in trailing
/// expressions and the class body itself.
/// Accepts optional `module_name_override` so package-qualified names
/// are used consistently across all load paths.
/// Accepts `referenced_aliases` — the caller's already-computed
/// alias-dependency set (`parse_and_check_expression`'s
/// `compute_diagnostics_and_analysis` result), threaded into the
/// response so the Erlang side can register the same `beamtalk_alias_xref`
/// dependency edges a file-defining compile gets. Mirrors
/// `handle_inline_protocol_definition`'s identical parameter.
/// Accepts `analysis` — the caller's already-computed
/// [`AnalysisResult`](beamtalk_core::semantic_analysis::AnalysisResult),
/// threaded into codegen via `CodegenOptions::with_analysis` so it doesn't
/// re-derive the class hierarchy, semantic facts, and inferred method return
/// types from scratch. `None` when the caller has no analysis to hand off
/// (or, per `handle_compile_expression`'s call site, when a standalone
/// method merge between `analysis` and codegen would make it stale) —
/// codegen falls back to computing its own.
#[allow(clippy::too_many_arguments)]
pub(crate) fn handle_inline_class_definition(
    module: beamtalk_core::ast::Module,
    source: &str,
    expr_module_name: &str,
    warnings: &[String],
    class_superclass_index: &std::collections::HashMap<String, String>,
    class_module_index: std::collections::HashMap<String, String>,
    pre_class_hierarchy: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    pre_loaded_aliases: Vec<beamtalk_core::semantic_analysis::AliasInfo>,
    module_name_override: Option<&str>,
    referenced_aliases: &[ecow::EcoString],
    analysis: Option<beamtalk_core::semantic_analysis::AnalysisResult>,
) -> Term {
    let mut module = module;
    let mut warnings = warnings.to_vec();
    if !module.method_definitions.is_empty() {
        let method_defs = std::mem::take(&mut module.method_definitions);
        for method_def in method_defs {
            let target_class = method_def.class_name.name.as_str();
            if let Some(class) = module
                .classes
                .iter_mut()
                .find(|c| c.name.name == target_class)
            {
                let methods = if method_def.is_class_method {
                    &mut class.class_methods
                } else {
                    &mut class.methods
                };
                merge_method(methods, method_def.method);
            } else {
                warnings.push(format!(
                    "Standalone method targets unknown class `{target_class}` in this module"
                ));
            }
        }
    }

    // Use unified module name derivation so inline class definitions
    // in package mode produce the same module name as file-based compilation.
    let class_module_name =
        derive_class_module_name(&module.classes[0].name.name, module_name_override, false);

    let classes: Vec<(String, String)> = module
        .classes
        .iter()
        .map(|c| (c.name.name.to_string(), c.superclass_name().to_string()))
        .collect();

    // Compile trailing expressions (after class body) so the Erlang side
    // can evaluate them and return their result instead of the class name.
    let trailing_core_erlang = if module.expressions.is_empty() {
        None
    } else {
        let trailing_exprs: Vec<_> = module
            .expressions
            .iter()
            .map(|s| s.expression.clone())
            .collect();
        match beamtalk_repl::codegen::generate_repl_expressions_with_index(
            &trailing_exprs,
            expr_module_name,
            class_module_index.clone(),
        ) {
            Ok(code) => Some(code),
            Err(e) => {
                return error_response(&[format_codegen_error(&e, source)]);
            }
        }
    };

    let mut codegen_options =
        beamtalk_codegen::core_erlang::CodegenOptions::new(&class_module_name)
            .with_workspace_mode(true)
            .with_source(source)
            .with_class_superclass_index(class_superclass_index.clone())
            .with_class_module_index(class_module_index)
            .with_class_hierarchy(pre_class_hierarchy)
            .with_pre_loaded_aliases(pre_loaded_aliases);
    if let Some(analysis) = analysis {
        // Prepare the AST at the driver boundary using the same
        // analysis handed off to codegen just below — codegen no longer
        // schedules this writeback itself for a trustworthy hand-off.
        beamtalk_core::semantic_analysis::lower_module_for_codegen(
            &mut module,
            &analysis.class_hierarchy,
            &analysis.method_return_types,
        );
        codegen_options = codegen_options.with_analysis(analysis);
    }
    match beamtalk_codegen::core_erlang::generate_module(&module, codegen_options) {
        Ok(code) => class_definition_ok_response(
            &code,
            &class_module_name,
            &classes,
            trailing_core_erlang.as_deref(),
            &warnings,
            referenced_aliases,
        ),
        Err(e) => error_response(&[format_codegen_error(&e, source)]),
    }
}

/// Handle protocol definitions in the REPL.
///
/// Compiles protocol-only modules to Core Erlang (which generates `register_class/0`
/// with protocol registration), then returns a `protocol_definition`
/// response so the Erlang side can load and execute the module.
/// Accepts optional `module_name_override` for package-mode consistency.
/// Accepts `referenced_aliases` — the caller's already-computed
/// alias-dependency set, threaded straight into the response so the Erlang
/// side can register the same `beamtalk_alias_xref` dependency edges a
/// class-defining compile gets. Both callers (the REPL
/// `compile_expression` path and `handle_compile`'s file-compile path)
/// pass a genuinely-computed set, never a hardcoded empty one.
/// Accepts `pre_loaded_aliases` — mirrors the `.with_pre_loaded_aliases(...)`
/// wiring applied to the other three `CodegenOptions` call sites in this
/// file (`handle_inline_class_definition`, `handle_compile`'s class path,
/// `handle_compile_method`) — so a protocol method signature referencing a
/// cross-module alias resolves to a `user_type` reference in the generated
/// `-type` attributes instead of silently dropping the alias (empty registry).
/// Accepts `analysis` — mirrors `handle_inline_class_definition`'s
/// identical parameter; both callers pass their own already-computed
/// [`AnalysisResult`](beamtalk_core::semantic_analysis::AnalysisResult)
/// unconditionally since, unlike the class path, nothing mutates `module`
/// between computing it and this call.
#[allow(clippy::too_many_arguments)]
pub(crate) fn handle_inline_protocol_definition(
    module: &beamtalk_core::ast::Module,
    source: &str,
    warnings: &[String],
    class_superclass_index: &std::collections::HashMap<String, String>,
    class_module_index: std::collections::HashMap<String, String>,
    pre_class_hierarchy: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    pre_loaded_aliases: Vec<beamtalk_core::semantic_analysis::AliasInfo>,
    module_name_override: Option<&str>,
    stdlib_mode: bool,
    referenced_aliases: &[ecow::EcoString],
    analysis: Option<beamtalk_core::semantic_analysis::AnalysisResult>,
) -> Term {
    let first_protocol_name = &module.protocols[0].name.name;
    let protocol_module_name =
        derive_class_module_name(first_protocol_name, module_name_override, stdlib_mode);

    let protocol_names: Vec<String> = module
        .protocols
        .iter()
        .map(|p| p.name.name.to_string())
        .collect();

    let mut codegen_options =
        beamtalk_codegen::core_erlang::CodegenOptions::new(&protocol_module_name)
            .with_workspace_mode(true)
            .with_source(source)
            .with_class_superclass_index(class_superclass_index.clone())
            .with_class_module_index(class_module_index)
            .with_class_hierarchy(pre_class_hierarchy)
            .with_pre_loaded_aliases(pre_loaded_aliases);
    // No `lower_module_for_codegen` call needed here — both callers
    // only reach this function once `module.classes` is confirmed empty (a
    // protocol-only compile), and the writeback trio only ever touches
    // `module.classes`/`module.method_definitions`, so it would be a no-op.
    if let Some(analysis) = analysis {
        codegen_options = codegen_options.with_analysis(analysis);
    }
    match beamtalk_codegen::core_erlang::generate_module(module, codegen_options) {
        Ok(code) => protocol_definition_ok_response(
            &code,
            &protocol_module_name,
            &protocol_names,
            warnings,
            referenced_aliases,
        ),
        Err(e) => error_response(&[format_codegen_error(&e, source)]),
    }
}

/// Handle a single `type Name = ...` declaration typed directly at the REPL
/// (ADR 0108 Phase 8).
///
/// By the time this is called, `parse_and_check_expression` has already run
/// full semantic analysis (`AliasRegistry::register_module` — namespace
/// collision, duplicate, and unbound-type-variable checks) and returned no
/// error diagnostics, so the declaration is already known-valid; this just
/// shapes the response. Mirrors the standalone-method-definition precedent
/// of requiring exactly one declaration per turn — batch multi-alias
/// resolution (topological ordering across several declarations at once) is
/// a file-compile concern, not a single REPL turn's.
pub(crate) fn handle_inline_type_alias_definition(
    module: &beamtalk_core::ast::Module,
    warnings: &[String],
) -> Term {
    if module.type_aliases.len() > 1 {
        return error_response(&[
            "Multiple type alias definitions in a single expression are not supported. \
             Define each `type Name = ...` alias separately."
                .to_string(),
        ]);
    }
    if !module.expressions.is_empty() {
        return error_response(&[
            "A type alias declaration cannot be combined with other expressions in the \
             same input; declare it on its own, then use it in a later expression."
                .to_string(),
        ]);
    }

    let alias = &module.type_aliases[0];
    let expansion = beamtalk_core::unparse::unparse_type_annotation_display(&alias.annotation);
    type_alias_definition_ok_response(
        &alias.name.name,
        &expansion,
        alias.doc_comment.as_deref(),
        warnings,
    )
}
