// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `compile_expression` / `compile_expression_trace` request handlers.

use beamtalk_etf::{map_get, term_to_string, term_to_string_list};
use eetf::{Map, Term};

use crate::decode::{
    extract_class_hierarchy, extract_known_type_aliases, extract_optional_string_map,
    extract_protocol_registry,
};
use crate::diagnostics::{collect_warning_messages, filter_error_diagnostics};
use crate::registry::diagnostics_overrides;
use crate::respond::{
    diagnostic_error_response, error_response, format_codegen_error, method_definition_ok_response,
    method_signature_terms, ok_response,
};

use super::inline_definitions::{
    handle_inline_class_definition, handle_inline_protocol_definition,
    handle_inline_type_alias_definition,
};

/// Parse a Beamtalk expression source and run full diagnostics with primitive validation.
///
/// Returns `Ok((module, warnings, analysis))` on success, or
/// `Err(response_term)` containing a formatted `diagnostic_error_response`
/// that the caller should return directly. `analysis` is the full
/// [`AnalysisResult`](beamtalk_core::semantic_analysis::AnalysisResult) this
/// function's own semantic-analysis pass produced — callers that go
/// on to run codegen for the same module thread it into
/// `CodegenOptions::with_analysis` instead of letting codegen re-derive the
/// class hierarchy, semantic facts, and inferred method return types from
/// scratch. `analysis.referenced_aliases` is the alias-dependency set
/// (the ADR 0108 hot-reload re-check trigger) callers receive
/// as this tuple's third element directly.
///
/// `pre_loaded_aliases` (ADR 0108 Phase 8) carries type aliases
/// declared in earlier turns of the same REPL session, re-parsed standalone
/// by [`extract_known_type_aliases`] — see that function's doc for why
/// aliases need their own re-parse path rather than `pre_class_hierarchy`'s
/// recover-from-live-BEAM-state mechanism.
///
/// `pre_loaded_protocols` carries the live image's ambient
/// protocol cache — see [`extract_protocol_registry`]'s doc. Without it, a
/// cross-file protocol-typed receiver in a live `compile_expression` (the
/// REPL's `eval`) hits the same nominal-mismatch/Dnu false positive already
/// fixed for `diagnostics/3`.
///
/// Uses `compute_diagnostics_and_analysis` (the same analysis as
/// `compute_diagnostics_with_known_vars_classes_and_aliases`, additionally
/// returning the full `AnalysisResult`) so the REPL-inline
/// `compile_expression` path computes the same alias-dependency set
/// `handle_compile`'s file-compile path already does, giving
/// `handle_inline_class_definition` and `handle_inline_protocol_definition`
/// a real set to thread through rather than a hardcoded `&[]`.
pub(crate) fn parse_and_check_expression(
    source: &str,
    known_vars: &[String],
    pre_class_hierarchy: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    pre_loaded_protocols: Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo>,
    pre_loaded_aliases: Vec<beamtalk_core::semantic_analysis::AliasInfo>,
) -> Result<
    (
        beamtalk_core::ast::Module,
        Vec<String>,
        beamtalk_core::semantic_analysis::AnalysisResult,
    ),
    Term,
> {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(source);
    let (module, parse_diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    let known_var_refs: Vec<&str> = known_vars.iter().map(String::as_str).collect();
    let (mut all_diagnostics, analysis) =
        beamtalk_language_service::queries::diagnostic_provider::compute_diagnostics_and_analysis(
            &module,
            parse_diagnostics,
            &known_var_refs,
            pre_class_hierarchy,
            pre_loaded_protocols,
            pre_loaded_aliases,
            diagnostics_overrides(),
        );

    let options = beamtalk_core::CompilerOptions::default();
    let primitive_diags =
        beamtalk_core::semantic_analysis::primitive_validator::validate_primitives(
            &module, &options,
        );
    all_diagnostics.extend(primitive_diags);

    let error_diags = filter_error_diagnostics(&all_diagnostics);
    let warnings = collect_warning_messages(&all_diagnostics);

    if !error_diags.is_empty() {
        return Err(diagnostic_error_response(&error_diags, source));
    }

    Ok((module, warnings, analysis))
}

/// Handle a single `compile_expression` request.
#[allow(clippy::too_many_lines)]
pub(crate) fn handle_compile_expression(request: &Map) -> Term {
    // Extract required fields
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };

    let Some(module_name) = map_get(request, "module").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'module' field".to_string()]);
    };

    let known_vars = map_get(request, "known_vars")
        .and_then(term_to_string_list)
        .unwrap_or_default();

    let class_superclass_index =
        match extract_optional_string_map(request, "class_superclass_index") {
            Ok(map) => map,
            Err(resp) => return resp,
        };
    let class_module_index = match extract_optional_string_map(request, "class_module_index") {
        Ok(map) => map,
        Err(resp) => return resp,
    };
    let pre_class_hierarchy = extract_class_hierarchy(request);
    let pre_loaded_protocols = extract_protocol_registry(request);
    let pre_loaded_aliases = extract_known_type_aliases(request);

    let (module, warnings, analysis) = match parse_and_check_expression(
        &source,
        &known_vars,
        pre_class_hierarchy.clone(),
        pre_loaded_protocols,
        pre_loaded_aliases.clone(),
    ) {
        Ok(r) => r,
        Err(resp) => return resp,
    };

    // Extract optional module_name override for inline class definitions
    // so they produce the same module name as file-based compilation in package mode.
    let module_name_override = map_get(request, "module_name").and_then(term_to_string);

    // If the parsed module contains class definitions, use compile path
    if !module.classes.is_empty() {
        let referenced_aliases = analysis.referenced_aliases.clone();
        // `handle_inline_class_definition` merges any standalone
        // `module.method_definitions` into their target class *after* this
        // point — a method the type checker saw as a standalone extension
        // during `analysis` above. `infer_method_return_types`/writeback key
        // return types by `(ClassName, Selector, IsClassMethod)` regardless
        // of whether the method AST lives standalone or inside
        // `class.methods`, so the map itself stays valid across the merge —
        // but only thread `analysis` through when there's no merge about to
        // happen at all, so this call site never has to reason about it:
        // codegen's own no-analysis-supplied fallback (still correct, just
        // not the fast path) covers the rarer "class def + inline standalone
        // method in the same eval" REPL pattern.
        let analysis = module.method_definitions.is_empty().then_some(analysis);
        return handle_inline_class_definition(
            module,
            &source,
            &module_name,
            &warnings,
            &class_superclass_index,
            class_module_index,
            pre_class_hierarchy,
            pre_loaded_aliases,
            module_name_override.as_deref(),
            &referenced_aliases,
            analysis,
        );
    }

    // If the parsed module contains standalone method definitions, return method info
    if !module.method_definitions.is_empty() {
        if module.method_definitions.len() > 1 {
            return error_response(&[
                "Multiple standalone method definitions in a single expression are not supported. \
                 Define each method separately, or use a class definition with inline methods."
                    .to_string(),
            ]);
        }
        let method_def = &module.method_definitions[0];
        let class_name = method_def.class_name.name.to_string();
        let selector = method_def.method.selector.name().to_string();
        // `method_source` must be the METHOD's source (`sel => body`), not the
        // full `Class >> sel => body` input — it is recorded verbatim in the
        // ChangeLog and written back on flush. Echoing the input would splice a
        // stray `Class >>` extension into the class body on flush.
        // `unparse_method` re-emits the parsed method, comments and
        // all, so the recorded source round-trips cleanly.
        let method_source = beamtalk_core::unparse::unparse_method(&method_def.method);
        let (return_type, param_types) = method_signature_terms(&method_def.method);
        return method_definition_ok_response(
            &class_name,
            &selector,
            method_def.is_class_method,
            &method_source,
            &return_type,
            &param_types,
            &warnings,
        );
    }

    // If the parsed module contains protocol definitions, compile and return them
    if !module.protocols.is_empty() {
        // `parse_and_check_expression` computes
        // `referenced_aliases` for this REPL-expression path too (mirroring
        // `handle_compile`'s file-compile path below), so this call site
        // passes a real set rather than a hardcoded `&[]`.
        let referenced_aliases = analysis.referenced_aliases.clone();
        return handle_inline_protocol_definition(
            &module,
            &source,
            &warnings,
            &class_superclass_index,
            class_module_index,
            pre_class_hierarchy,
            pre_loaded_aliases,
            module_name_override.as_deref(),
            false, // REPL expressions are never stdlib
            &referenced_aliases,
            Some(analysis),
        );
    }

    // ADR 0108 Phase 8: If the parsed module contains a `type
    // Name = ...` declaration, return alias metadata for the REPL session
    // to register — no Core Erlang / bytecode step, since aliases erase
    // entirely at resolution time (ADR 0108 Semantics) and have no runtime
    // representation to compile.
    if !module.type_aliases.is_empty() {
        return handle_inline_type_alias_definition(&module, &warnings);
    }

    if module.expressions.is_empty() {
        return error_response(&["No expressions to compile".to_string()]);
    }

    // Generate Core Erlang for all expressions (multi-statement support)
    let expressions: Vec<_> = module
        .expressions
        .iter()
        .map(|s| s.expression.clone())
        .collect();
    match beamtalk_repl::codegen::generate_repl_expressions_with_index(
        &expressions,
        &module_name,
        class_module_index,
    ) {
        Ok(code) => ok_response(&code, &warnings),
        Err(e) => error_response(&[format_codegen_error(&e, &source)]),
    }
}

/// Handle a `compile_expression_trace` request.
///
/// Same parsing/validation as `compile_expression` but generates a trace module
/// whose `eval/1` returns `{[{<<"src0">>, V0}, ...], FinalState}` instead of
/// `{Result, FinalState}`.
///
/// Returns the same `ok_response` format as `compile_expression` — the difference
/// is in the generated module semantics, not the port protocol.
pub(crate) fn handle_compile_expression_trace(request: &Map) -> Term {
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };
    let Some(module_name) = map_get(request, "module").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'module' field".to_string()]);
    };
    let known_vars = map_get(request, "known_vars")
        .and_then(term_to_string_list)
        .unwrap_or_default();
    let class_module_index = match extract_optional_string_map(request, "class_module_index") {
        Ok(map) => map,
        Err(resp) => return resp,
    };
    let pre_class_hierarchy = extract_class_hierarchy(request);
    let pre_loaded_protocols = extract_protocol_registry(request);
    let pre_loaded_aliases = extract_known_type_aliases(request);

    // Trace mode never defines classes/protocols/aliases (rejected below), so
    // neither the `referenced_aliases` nor the rest of the `AnalysisResult`
    // this also computes has a consumer here —
    // trace-mode expressions never reach a `generate_module` call that could
    // use it, and can't reference an alias in a position that needs xref
    // registration either.
    let (module, warnings, _analysis) = match parse_and_check_expression(
        &source,
        &known_vars,
        pre_class_hierarchy,
        pre_loaded_protocols,
        pre_loaded_aliases,
    ) {
        Ok(r) => r,
        Err(resp) => return resp,
    };

    if !module.classes.is_empty()
        || !module.method_definitions.is_empty()
        || !module.protocols.is_empty()
        || !module.type_aliases.is_empty()
    {
        return error_response(&[
            "trace mode does not support class, method, protocol, or type alias \
             definitions; use eval without trace to define them"
                .to_string(),
        ]);
    }

    // Protocol definitions are not supported in trace mode.
    if !module.protocols.is_empty() {
        return error_response(&["trace mode does not support protocol definitions; \
             use eval without trace to define protocols"
            .to_string()]);
    }

    if module.expressions.is_empty() {
        return error_response(&["No expressions to compile".to_string()]);
    }

    let expressions: Vec<_> = module
        .expressions
        .iter()
        .map(|s| s.expression.clone())
        .collect();
    match beamtalk_repl::codegen::generate_repl_expressions_traced(
        &expressions,
        &source,
        &module_name,
        class_module_index,
    ) {
        Ok(code) => ok_response(&code, &warnings),
        Err(e) => error_response(&[format_codegen_error(&e, &source)]),
    }
}
