// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `compile_method` request handler — the structured single-method
//! compile that backs the live-image write-surface (IDE save /
//! `compile:source:` / REPL `>>`).

use beamtalk_etf::{map_get, term_to_bool, term_to_string};
use eetf::{Map, Term};

use crate::decode::{
    extract_class_hierarchy, extract_known_type_aliases, extract_optional_string_map,
    extract_protocol_registry, merge_method,
};
use crate::diagnostics::{filter_error_diagnostics, partition_diagnostics};
use crate::registry::diagnostics_overrides;
use crate::respond::{
    compile_method_diagnostic_response, compile_method_ok_response, diagnostic_error_response,
    error_response, format_codegen_error, method_signature_terms,
};

use super::inline_definitions::derive_class_module_name;

/// Handle a `compile_method` request — the structured single-method compile that
/// backs the live-image write-surface (IDE save / `compile:source:` / REPL `>>`).
///
/// Inputs:
///   - `class_source`: the current full class definition,
///   - `method_source`: the BARE method body (comments and all) — NO `Class >>`
///     prefix and NO header-sniffing,
///   - `is_class_method`: instance-side vs class-side,
///   - plus the usual `module_name` override, `source_path`, and class indexes.
///
/// The method is parsed standalone (so its source round-trips byte-for-byte),
/// merged into the parsed class via the SAME `merge_method` path the file/`>>`
/// compile uses, and codegen'd. The response carries the compiled module AND the
/// canonical method source (`unparse_method`) for the `ChangeLog`. This is the
/// rock-solid replacement for the textual `Class >> <source>` wrap.
#[allow(clippy::too_many_lines)]
pub(crate) fn handle_compile_method(request: &Map) -> Term {
    let Some(class_source) = map_get(request, "class_source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'class_source' field".to_string()]);
    };
    let Some(method_source) = map_get(request, "method_source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'method_source' field".to_string()]);
    };
    let is_class_method = map_get(request, "is_class_method")
        .and_then(term_to_bool)
        .unwrap_or(false);
    let stdlib_mode = map_get(request, "stdlib_mode")
        .and_then(term_to_bool)
        .unwrap_or(false);
    let workspace_mode = map_get(request, "workspace_mode")
        .and_then(term_to_bool)
        .unwrap_or(true);
    let pre_class_hierarchy = extract_class_hierarchy(request);
    // See `handle_compile`'s equivalent comment — a `compile_method`
    // patch is a class-defining/-patching compile too, so it needs the
    // ambient protocol cache for the same reason.
    let pre_loaded_protocols = extract_protocol_registry(request);
    // ADR 0108: see `handle_compile`'s equivalent comment — a
    // `compile_method` patch is a class-defining/-patching compile too, so
    // it needs session carried-over aliases for the same reason.
    let pre_loaded_aliases = extract_known_type_aliases(request);

    // 1. Parse the bare method body directly — no `Class >>`, no normalize.
    let method_tokens = beamtalk_core::source_analysis::lex_with_eof(&method_source);
    let (parsed_method, method_diags) = beamtalk_core::source_analysis::parse_method(method_tokens);
    let method_errors = filter_error_diagnostics(&method_diags);
    if !method_errors.is_empty() {
        return diagnostic_error_response(&method_errors, &method_source);
    }
    let Some(method) = parsed_method else {
        return error_response(&["method_source is not a single method definition".to_string()]);
    };

    // Canonical stored form + selector come straight from the parsed AST, so the
    // source the workspace records is exactly what `unparse_method` re-emits.
    let canonical_method_source = beamtalk_core::unparse::unparse_method(&method);
    let selector = method.selector.name().to_string();

    // 2. Parse the existing class. Initial parse diagnostics are intentionally
    //    discarded: in the workspace flow `class_source` is always the
    //    `merged_class_source` produced by a previous successful `compile_method`,
    //    so it is syntactically clean. Diagnostics are computed post-merge on the
    //    re-parsed merged source (below), where spans share one coordinate system.
    let class_tokens = beamtalk_core::source_analysis::lex_with_eof(&class_source);
    let (mut module, _class_parse_diags) = beamtalk_core::source_analysis::parse(class_tokens);

    if module.classes.is_empty() {
        return error_response(&["class_source contains no class definition".to_string()]);
    }
    // Pick the class to patch: the caller's `class_name` when given (a
    // class_source may legitimately define more than one class — REPL inline
    // multi-class, dependency files), else the sole/first class.
    let target_name = map_get(request, "class_name").and_then(term_to_string);
    let target_idx = target_name
        .as_deref()
        .and_then(|n| module.classes.iter().position(|c| c.name.name == n))
        .unwrap_or(0);
    let class_name = module.classes[target_idx].name.name.to_string();

    // 3. Merge the method into the target class (replace-or-add) via the shared path.
    let patched_kind = method.kind;
    {
        let class = &mut module.classes[target_idx];
        let methods = if is_class_method {
            &mut class.class_methods
        } else {
            &mut class.methods
        };
        merge_method(methods, method);
    }

    // The merged MODULE, unparsed, is the new canonical class source the
    // workspace stores for the next patch. Unparsing the whole module (not just
    // the target class) preserves any sibling classes in a multi-class source —
    // matching the textual accumulation path it replaces — so the stored source
    // stays a clean inline definition with no `Class >>` extension accumulation.
    let merged_class_source = beamtalk_core::unparse::unparse_module(&module);

    // Re-parse the merged source so EVERY span — for diagnostics AND codegen —
    // shares one coordinate system rooted at `merged_class_source`. The patched
    // method was parsed standalone (its span indexes into the bare `method_source`)
    // while the surviving methods index into `class_source`; the AST merge left
    // those two span bases mixed in `module`. Re-parsing the unparsed merge rebases
    // them all, so `span_to_line` annotates the freshly-patched method with the
    // right BEAM line and semantic diagnostics resolve against a
    // single coherent source — no fragile per-diagnostic source
    // routing. `unparse_module` round-trips a valid module, so the re-parse is
    // clean; a non-empty diag list here means an unparser regression.
    let merged_tokens = beamtalk_core::source_analysis::lex_with_eof(&merged_class_source);
    let (mut merged_module, merged_parse_diags) =
        beamtalk_core::source_analysis::parse(merged_tokens);
    // `unparse_module` round-trips a valid module, so a non-empty diag list here is
    // an unparser regression, not user error. Fail loudly in debug (CI/tests); in
    // release, surface an internal error rather than leaking parse diagnostics —
    // rendered against an internal canonical string — to a user who wrote valid
    // code (`debug_assert!` is elided in release).
    debug_assert!(
        merged_parse_diags.is_empty(),
        "unparse_module produced source that failed to re-parse: {merged_parse_diags:?}"
    );
    if !merged_parse_diags.is_empty() {
        return error_response(&[format!(
            "internal error: re-parsing the merged class source produced {} diagnostic(s); \
             this indicates an unparser regression, not a problem with your code",
            merged_parse_diags.len()
        )]);
    }

    // Locate the freshly-patched method in the re-parsed module so method-body
    // diagnostics can be reported relative to the method snippet the user edits
    // Its span indexes into `merged_class_source`, the same
    // coordinate system as every diagnostic span.
    let patched_method = merged_module
        .classes
        .iter()
        .find(|c| c.name.name == class_name)
        .and_then(|c| {
            let methods = if is_class_method {
                &c.class_methods
            } else {
                &c.methods
            };
            methods
                .iter()
                .find(|m| m.selector.name() == selector && m.kind == patched_kind)
        });
    let patched_method_span = patched_method.map(|m| m.span);
    // ADR 0105 Phase 1: declared signature of the patched method, read
    // from the re-parsed merged module so it reflects exactly what was installed
    // (not the standalone pre-merge parse). Falls back to "Dynamic"/no params in
    // the never-should-happen case the method isn't found post-merge.
    let (patched_return_type, patched_param_types) = patched_method.map_or_else(
        || ("Dynamic".to_string(), Vec::new()),
        method_signature_terms,
    );

    // 4. Full semantic analysis on the MERGED module — catches method errors in
    //    class context (undefined fields, type errors). Every diagnostic span
    //    indexes into `merged_class_source`; method-body errors are then reported
    //    relative to the patched method (so the method editor shows a snippet-local
    //    line) while rarer class-context errors keep their merged-source line — all
    //    accurate and in-range.
    // `compute_diagnostics_and_analysis` additionally returns the
    // full `AnalysisResult` for the already-merged `merged_module` — no
    // further mutation happens before the codegen call below, so it's always
    // safe to thread through via `CodegenOptions::with_analysis`.
    let (mut all_diagnostics, analysis) =
        beamtalk_language_service::queries::diagnostic_provider::compute_diagnostics_and_analysis(
            &merged_module,
            merged_parse_diags,
            &[],
            pre_class_hierarchy.clone(),
            pre_loaded_protocols,
            pre_loaded_aliases.clone(),
            diagnostics_overrides(),
        );
    let referenced_aliases = analysis.referenced_aliases.clone();
    let options = beamtalk_core::CompilerOptions {
        stdlib_mode,
        allow_primitives: false,
        workspace_mode,
        suppress_warnings: false,
        ..Default::default()
    };
    all_diagnostics.extend(
        beamtalk_core::semantic_analysis::primitive_validator::validate_primitives(
            &merged_module,
            &options,
        ),
    );
    let error_diags = filter_error_diagnostics(&all_diagnostics);
    let (_, warnings) = partition_diagnostics(&all_diagnostics);
    if !error_diags.is_empty() {
        return compile_method_diagnostic_response(
            &error_diags,
            &merged_class_source,
            patched_method_span,
        );
    }

    // 5. Package-qualified module name (via override) + codegen — identical to
    //    the file/extension compile path, so the installed module matches.
    let module_name_override = map_get(request, "module_name").and_then(term_to_string);
    let module_name =
        derive_class_module_name(&class_name, module_name_override.as_deref(), stdlib_mode);
    let source_path = map_get(request, "source_path").and_then(term_to_string);
    let class_module_index = match extract_optional_string_map(request, "class_module_index") {
        Ok(map) => map,
        Err(resp) => return resp,
    };
    let class_superclass_index =
        match extract_optional_string_map(request, "class_superclass_index") {
            Ok(map) => map,
            Err(resp) => return resp,
        };
    let classes: Vec<(String, String)> = merged_module
        .classes
        .iter()
        .map(|c| (c.name.name.to_string(), c.superclass_name().to_string()))
        .collect();
    let warning_msgs: Vec<String> = warnings.iter().map(|w| w.message.clone()).collect();

    // Prepare the AST at the driver boundary using the same
    // still-trustworthy analysis (computed on `merged_module` with no
    // mutation since) handed off to codegen just below — codegen no longer
    // schedules this writeback itself in that case.
    beamtalk_core::semantic_analysis::lower_module_for_codegen(
        &mut merged_module,
        &analysis.class_hierarchy,
        &analysis.method_return_types,
    );
    let codegen_options = beamtalk_codegen::core_erlang::CodegenOptions::new(&module_name)
        .with_workspace_mode(workspace_mode)
        .with_source(&merged_class_source)
        .with_class_module_index(class_module_index)
        .with_class_superclass_index(class_superclass_index)
        .with_class_hierarchy(pre_class_hierarchy)
        .with_pre_loaded_aliases(pre_loaded_aliases)
        .with_source_path_opt(source_path.as_deref())
        // `analysis` was computed on `merged_module` with no
        // mutation since — always safe to hand off.
        .with_analysis(analysis);
    match beamtalk_codegen::core_erlang::generate_module(&merged_module, codegen_options) {
        Ok(code) => compile_method_ok_response(
            &code,
            &module_name,
            &classes,
            &selector,
            is_class_method,
            &canonical_method_source,
            &merged_class_source,
            &patched_return_type,
            &patched_param_types,
            &warning_msgs,
            &referenced_aliases,
        ),
        // Codegen spans are now relative to the re-parsed merged module, so the
        // error location resolves against the merged source.
        Err(e) => error_response(&[format_codegen_error(&e, &merged_class_source)]),
    }
}
