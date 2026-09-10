// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `compile` request handler (file/class compilation).

use beamtalk_etf::{map_get, term_to_bool, term_to_string};
use eetf::{Map, Term};

use crate::decode::{
    extract_class_hierarchy, extract_known_type_aliases, extract_optional_string_map,
    extract_protocol_registry, merge_method,
};
use crate::diagnostics::{DiagInfo, filter_error_diagnostics, partition_diagnostics};
use crate::registry::diagnostics_overrides;
use crate::respond::{
    compile_ok_response, diagnostic_error_response, error_response, format_codegen_error,
};

use super::inline_definitions::{derive_class_module_name, handle_inline_protocol_definition};

/// Handle a `compile` request (file/class compilation).
#[allow(clippy::too_many_lines)]
pub(crate) fn handle_compile(request: &Map) -> Term {
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };

    let stdlib_mode = map_get(request, "stdlib_mode")
        .and_then(term_to_bool)
        .unwrap_or(false);

    let workspace_mode = map_get(request, "workspace_mode")
        .and_then(term_to_bool)
        .unwrap_or(true);

    let pre_class_hierarchy = extract_class_hierarchy(request);
    // A class/protocol-defining compile needs the ambient protocol
    // cache too, not just `diagnostics/3` — see `extract_protocol_registry`'s
    // doc. Without this, a live REPL `compile` of a class whose method
    // signature references a cross-file protocol hits the same nominal-
    // mismatch/Dnu false positive already fixed for `diagnostics/3`.
    let pre_loaded_protocols = extract_protocol_registry(request);
    // ADR 0108: a class/protocol-defining compile needs session
    // carried-over type aliases too, not just `compile_expression` — see
    // `extract_known_type_aliases`'s doc. Without this, a live REPL
    // redefinition of a class/protocol over an earlier turn's `type Foo =
    // ...` never sees the alias at all, so `AliasRegistry::add_pre_loaded`'s
    // existing collision check (alias name vs. `hierarchy`/
    // `protocol_registry`) never gets a chance to run.
    let pre_loaded_aliases = extract_known_type_aliases(request);

    // Parse the source
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (module, parse_diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    // Run semantic analysis, also capturing `referenced_aliases` —
    // shipped back in the response so the Erlang side can populate
    // `beamtalk_alias_xref`'s alias-name → dependent-class index at class
    // install time (see `diagnostics_ok_response`/this handler's response
    // builder for where the field is attached).
    // `compute_diagnostics_and_analysis` additionally returns the
    // full `AnalysisResult`, threaded into codegen below via
    // `CodegenOptions::with_analysis` so it doesn't re-derive the class
    // hierarchy, semantic facts, and inferred method return types from
    // scratch.
    let (mut all_diagnostics, analysis) =
        beamtalk_language_service::queries::diagnostic_provider::compute_diagnostics_and_analysis(
            &module,
            parse_diagnostics,
            &[],
            pre_class_hierarchy.clone(),
            pre_loaded_protocols,
            pre_loaded_aliases.clone(),
            diagnostics_overrides(),
        );
    let referenced_aliases = analysis.referenced_aliases.clone();

    // Run @primitive validation
    let options = beamtalk_core::CompilerOptions {
        stdlib_mode,
        allow_primitives: false,
        workspace_mode,
        suppress_warnings: false,
        ..Default::default()
    };
    let primitive_diags =
        beamtalk_core::semantic_analysis::primitive_validator::validate_primitives(
            &module, &options,
        );
    all_diagnostics.extend(primitive_diags);

    // Warn when user code shadows a stdlib class name (not for stdlib itself).
    if !stdlib_mode {
        let mut stdlib_shadow_diags = Vec::new();
        beamtalk_core::semantic_analysis::check_stdlib_name_shadowing(
            &module,
            &mut stdlib_shadow_diags,
        );
        all_diagnostics.extend(stdlib_shadow_diags);
    }

    let error_diags = filter_error_diagnostics(&all_diagnostics);
    let (_, mut warnings) = partition_diagnostics(&all_diagnostics);

    if !error_diags.is_empty() {
        return diagnostic_error_response(&error_diags, &source);
    }

    // Merge standalone method definitions into their target classes.
    // Captured *before* the merge so the class-codegen call below
    // knows whether `analysis` (computed pre-merge) is still trustworthy —
    // see `handle_compile_expression`'s identical `analysis` gating for why.
    let had_standalone_method_definitions = !module.method_definitions.is_empty();
    let mut module = module;
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
                warnings.push(DiagInfo {
                    message: format!(
                        "Standalone method targets unknown class `{target_class}` in this module"
                    ),
                    severity: "warning".to_string(),
                    category: None,
                    start: method_def.span.start(),
                    end: method_def.span.end(),
                });
            }
        }
    }

    // Accept optional module_name override from caller.
    // When provided, use it directly instead of deriving from the class name.
    // This allows the REPL/MCP load path to produce package-qualified names
    // matching the build system (e.g., bt@my_app@scheme@symbol).
    // Uses the unified derive_class_module_name function.
    let module_name_override = map_get(request, "module_name").and_then(term_to_string);

    // Derive module name from the sole top-level definition, which enforces
    // a single top-level definition per file (one class OR one protocol).
    let primary_name = module
        .classes
        .first()
        .map(|c| c.name.name.as_str())
        .or_else(|| module.protocols.first().map(|p| p.name.name.as_str()));

    let module_name = match primary_name {
        Some(name) => derive_class_module_name(name, module_name_override.as_deref(), stdlib_mode),
        None => {
            return error_response(
                &["No class or protocol definition found in source".to_string()],
            );
        }
    };

    let class_module_index = match extract_optional_string_map(request, "class_module_index") {
        Ok(map) => map,
        Err(resp) => return resp,
    };
    let class_superclass_index =
        match extract_optional_string_map(request, "class_superclass_index") {
            Ok(map) => map,
            Err(resp) => return resp,
        };

    // Protocol-only files need the same early-return path as
    // handle_compile_expression. generate_module assumes at least
    // one class exists and errors with "Value type module has no class" for
    // protocol-only files. Route through the protocol codegen instead.
    if !module.protocols.is_empty() && module.classes.is_empty() {
        let warning_msgs: Vec<String> = warnings.iter().map(|w| w.message.clone()).collect();
        // `referenced_aliases` was already computed above
        // for this file-compile path — thread it through so a protocol-only
        // file's alias-typed method signatures get the same
        // `beamtalk_alias_xref` registration a class-defining compile gets.
        return handle_inline_protocol_definition(
            &module,
            &source,
            &warning_msgs,
            &class_superclass_index,
            class_module_index,
            pre_class_hierarchy,
            pre_loaded_aliases,
            module_name_override.as_deref(),
            stdlib_mode,
            &referenced_aliases,
            Some(analysis),
        );
    }

    // Extract class info
    let classes: Vec<(String, String)> = module
        .classes
        .iter()
        .map(|c| (c.name.name.to_string(), c.superclass_name().to_string()))
        .collect();

    // Extract optional source file path to embed as beamtalk_source attribute.
    let source_path = map_get(request, "source_path").and_then(term_to_string);

    // Generate Core Erlang
    let warning_msgs: Vec<String> = warnings.iter().map(|w| w.message.clone()).collect();
    let mut codegen_options = beamtalk_codegen::core_erlang::CodegenOptions::new(&module_name)
        .with_workspace_mode(workspace_mode)
        .with_source(&source)
        .with_class_module_index(class_module_index)
        .with_class_superclass_index(class_superclass_index)
        .with_class_hierarchy(pre_class_hierarchy)
        .with_pre_loaded_aliases(pre_loaded_aliases)
        .with_source_path_opt(source_path.as_deref());
    // Only trust `analysis` (computed pre-merge) when nothing was
    // merged into `module` afterward — see `had_standalone_method_definitions`'s
    // doc above.
    if !had_standalone_method_definitions {
        // Prepare the AST at the driver boundary using the same
        // still-trustworthy analysis handed off to codegen just below —
        // codegen no longer schedules this writeback itself in that case.
        beamtalk_core::semantic_analysis::lower_module_for_codegen(
            &mut module,
            &analysis.class_hierarchy,
            &analysis.method_return_types,
        );
        codegen_options = codegen_options.with_analysis(analysis);
    }
    match beamtalk_codegen::core_erlang::generate_module(&module, codegen_options) {
        Ok(code) => compile_ok_response(
            &code,
            &module_name,
            &classes,
            &warning_msgs,
            &referenced_aliases,
        ),
        Err(e) => error_response(&[format_codegen_error(&e, &source)]),
    }
}
