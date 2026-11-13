// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `compile` request handler (file/class compilation).

use beamtalk_etf::{map_get, term_to_bool, term_to_string};
use eetf::{Map, Term};

use crate::decode::{
    extract_class_hierarchy, extract_known_type_aliases, extract_optional_string_map,
    extract_protocol_registry,
};
use crate::diagnostics::{
    DiagInfo, filter_error_diagnostics, partition_diagnostics, warnings_from_diag_infos,
};
use crate::respond::{
    compile_ok_response, diagnostic_error_response, error_response, format_codegen_error,
    with_verifier_warnings,
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
    let session_aliases = extract_known_type_aliases(request);
    // BT-4322: resolve the compiled file's own project from `source_path`
    // (the real file path the workspace load path always sends) so the file
    // typechecks with the same package scope `beamtalk build` gives it —
    // project + dependency aliases, the project's FFI registry, and its own
    // `[diagnostics]` table. Files with no enclosing manifest keep the
    // legacy behaviour (port-process cwd table, no registry, session
    // aliases only); stdlib files do too (the stdlib has no manifest — the
    // build compiles it as one coherent package, not via manifest
    // walk-up). A session alias keeps its declaration over a same-named
    // project alias (REPL "current turn wins"); everything else appends
    // project-first, mirroring the build's merge order.
    // BT-4378: a `load_project` tags its compiles with one `project_batch`
    // token, so the project scope is verified once per batch, not per file.
    let scope_path = if stdlib_mode {
        None
    } else {
        map_get(request, "source_path").and_then(term_to_string)
    };
    let batch = map_get(request, "project_batch").and_then(term_to_string);
    let scope =
        crate::project_context::RequestScope::for_source(scope_path.as_deref(), batch.as_deref());
    // A file compile's source is its whole file: it declares the file's own
    // aliases, so the on-disk copy's are not pre-loaded on top (BT-4325).
    let pre_loaded_aliases = scope.whole_file_aliases(session_aliases);
    // ADR 0127 §10a / BT-3593: the live-reload fan-out recompiles a
    // protocol's users from their own files, one `compile` request per file
    // — each such request carries the just-edited protocol's full source
    // under `protocol_sources` (name -> raw `.bt` text) so `uses:` flattening
    // sees the NEW provisions rather than whatever this port process last
    // saw (or nothing, for a protocol it never compiled at all). See
    // `parse_protocol_defs`'s own doc for why this is a full-source channel
    // rather than a signature-only one like `protocol_registry` above.
    let pre_loaded_protocol_defs = match extract_optional_string_map(request, "protocol_sources") {
        Ok(sources) => parse_protocol_defs(&sources),
        Err(resp) => return resp,
    };

    // Parse the source
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (mut module, mut parse_diagnostics) = beamtalk_core::source_analysis::parse(tokens);
    // One compilation unit per class (BT-4721): fold same-file extensions into
    // the host class before analysis, exactly as `beamtalk build` does. Only
    // cross-file (foreign) standalone methods remain afterwards.
    parse_diagnostics.extend(beamtalk_core::compilation::fold_same_file_extensions(
        &mut module,
    ));

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
            pre_loaded_protocol_defs,
            pre_loaded_aliases.clone(),
            scope.diagnostics_table(),
            scope.analysis_project(),
        );
    let referenced_aliases = analysis.referenced_aliases.clone();

    // Run @primitive validation
    let options = beamtalk_core::CompilerOptions {
        stdlib_mode,
        allow_primitives: false,
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

    // Same-file extensions were folded above. A standalone method that still
    // targets a class this module does not declare has no home in a REPL file
    // compile: warn and drop it.
    let had_standalone_method_definitions = !module.method_definitions.is_empty();
    for method_def in std::mem::take(&mut module.method_definitions) {
        let target_class = method_def.class_name.name.as_str();
        warnings.push(DiagInfo {
            message: format!(
                "Standalone method targets unknown class `{target_class}` in this module"
            ),
            hint: None,
            severity: "warning".to_string(),
            category: None,
            start: method_def.span.start(),
            end: method_def.span.end(),
        });
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
        let warning_msgs = warnings_from_diag_infos(&warnings);
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
    let warning_msgs = warnings_from_diag_infos(&warnings);
    let mut codegen_options = beamtalk_codegen::core_erlang::CodegenOptions::new(&module_name)
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
        beamtalk_core::semantic_analysis::lower_analysed_module_for_codegen(&mut module, &analysis);
        codegen_options = codegen_options.with_analysis(analysis);
    }
    match beamtalk_codegen::core_erlang::generate_module_surfacing_verifier(
        &module,
        codegen_options,
    ) {
        Ok((code, verifier_diagnostics)) => compile_ok_response(
            &code,
            &module_name,
            &classes,
            &with_verifier_warnings(&warning_msgs, &verifier_diagnostics),
            &referenced_aliases,
        ),
        Err(e) => error_response(&[format_codegen_error(&e, &source)]),
    }
}

/// Parse `protocol_sources` (`{name => raw .bt source}`, see `handle_compile`'s
/// doc) into full `ProtocolDefinition` ASTs for `pre_loaded_protocol_defs`.
///
/// Each entry is lexed and parsed standalone, exactly like the file compile
/// this handler itself runs above — a source that fails to parse, or whose
/// module doesn't actually define a protocol named `name` (a caller bug: the
/// Erlang side always names the entry after the protocol whose source it
/// carries), silently contributes nothing, mirroring
/// `parse_protocol_registry_from_term`'s "degrade gracefully on a malformed
/// entry" convention rather than failing the whole request over one
/// unrelated user's stale value. A protocol whose provisions this omits
/// simply isn't visible for flattening on THIS compile — the caller
/// (`beamtalk_repl_loader`'s two-stage reload) treats a resulting "unknown
/// protocol"/missing-provision diagnostic on the user file as a compile
/// failure for that user, same as any other, so this never silently
/// installs a half-flattened class.
fn parse_protocol_defs(
    sources: &std::collections::HashMap<String, String>,
) -> Vec<beamtalk_core::ast::ProtocolDefinition> {
    sources
        .iter()
        .filter_map(|(name, source)| {
            let tokens = beamtalk_core::source_analysis::lex_with_eof(source);
            let (module, _parse_diagnostics) = beamtalk_core::source_analysis::parse(tokens);
            module
                .protocols
                .into_iter()
                .find(|p| p.name.name.as_str() == name)
        })
        .collect()
}
