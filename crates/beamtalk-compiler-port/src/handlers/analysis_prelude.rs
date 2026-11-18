// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared prelude and tail of the compile-family handlers.
//!
//! `handle_compile`, `handle_compile_method` and `parse_and_check_expression`
//! each run the same two steps after parsing — semantic analysis with the
//! request's ambient context, then `@primitive` validation — and the two
//! class-producing handlers also read the same wire indexes and derive the same
//! `(class, superclass)` list for the response. Those steps live here so a
//! change to one (a new ambient input, a new validator) reaches every handler.
//!
//! Deliberately **not** shared: `check_stdlib_name_shadowing`. It is a
//! whole-file check run by `handle_compile` only, because a name that shadows a
//! stdlib class is introduced by the file that *declares* the class. A
//! `compile_method` patch never declares a new class, and the original
//! declaration was already checked when the class was first compiled, so
//! repeating the check there would re-warn on every method edit.

use std::collections::HashMap;

use beamtalk_core::ast::{Module, ProtocolDefinition};
use beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo;
use beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo;
use beamtalk_core::semantic_analysis::{AliasInfo, AnalysisResult};
use beamtalk_core::source_analysis::Diagnostic;
use eetf::{Map, Term};

use crate::decode::extract_optional_string_map;
use crate::project_context::RequestScope;

/// The ambient (request-supplied) inputs semantic analysis needs.
pub(super) struct AmbientContext<'a> {
    /// Variables already bound in the REPL session.
    pub(super) known_vars: &'a [&'a str],
    pub(super) class_hierarchy: Vec<ClassInfo>,
    pub(super) protocols: Vec<ProtocolInfo>,
    /// Full ASTs of cross-file protocols, for `uses:` flattening.
    pub(super) protocol_defs: Vec<ProtocolDefinition>,
    pub(super) aliases: Vec<AliasInfo>,
    pub(super) scope: &'a RequestScope,
    /// Whether the module belongs to the stdlib (`@primitive` is allowed).
    pub(super) stdlib_mode: bool,
}

/// Run semantic analysis on `module` and append `@primitive` validation
/// diagnostics. Returns every diagnostic plus the full [`AnalysisResult`], which
/// callers hand to codegen via `CodegenOptions::with_analysis`.
pub(super) fn analyse_module(
    module: &Module,
    parse_diagnostics: Vec<Diagnostic>,
    ctx: AmbientContext<'_>,
) -> (Vec<Diagnostic>, AnalysisResult) {
    let (mut diagnostics, analysis) =
        beamtalk_language_service::queries::diagnostic_provider::compute_diagnostics_and_analysis(
            module,
            parse_diagnostics,
            ctx.known_vars,
            ctx.class_hierarchy,
            ctx.protocols,
            ctx.protocol_defs,
            ctx.aliases,
            ctx.scope.diagnostics_table(),
            ctx.scope.analysis_project(),
        );
    let options = beamtalk_core::CompilerOptions {
        stdlib_mode: ctx.stdlib_mode,
        allow_primitives: false,
        suppress_warnings: false,
        ..Default::default()
    };
    diagnostics.extend(
        beamtalk_core::semantic_analysis::primitive_validator::validate_primitives(
            module, &options,
        ),
    );
    (diagnostics, analysis)
}

/// `(class_module_index, class_superclass_index)` wire maps.
pub(super) type ClassIndexes = (HashMap<String, String>, HashMap<String, String>);

/// The `class_module_index` and `class_superclass_index` wire maps, in that
/// order, or the error response for a malformed one.
pub(super) fn extract_class_indexes(request: &Map) -> Result<ClassIndexes, Term> {
    let class_module_index = extract_optional_string_map(request, "class_module_index")?;
    let class_superclass_index = extract_optional_string_map(request, "class_superclass_index")?;
    Ok((class_module_index, class_superclass_index))
}

/// `(class name, superclass name)` for every class in `module`, for the
/// response's class list.
pub(super) fn class_name_pairs(module: &Module) -> Vec<(String, String)> {
    module
        .classes
        .iter()
        .map(|c| (c.name.name.to_string(), c.superclass_name().to_string()))
        .collect()
}
