// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `resolve_completion_type` request handler (BT-1068).

use beamtalk_etf::{atom, binary_from_str as binary, map_get, term_to_string};
use eetf::{Map, Term};

use crate::decode::extract_class_hierarchy;
use crate::registry::native_type_registry;
use crate::respond::error_response;

/// Handle a `resolve_completion_type` request (BT-1068).
///
/// Resolves the type of an arbitrary expression for REPL completion fallback.
/// This is called when `tokenise_send_chain/1` fails (e.g. parenthesised
/// subexpressions, binary message chains, keyword sends mid-chain).
///
/// Request fields:
/// - `expression` (binary): the full receiver expression with the incomplete prefix stripped
/// - `class_hierarchy` (optional map): user-defined class metadata from the REPL session
///
/// Also consults the process-wide native type registry (BT-2891, see
/// [`native_type_registry`]), loaded once from `_build/type_cache/`, so an
/// FFI expression (e.g. `Erlang lists reverse: x`) resolves its typed return
/// class instead of falling back to `Dynamic` when the project has been built.
///
/// Response: `#{status => ok, class_name => <<"String">>}` on success,
/// or `#{status => not_found}` when the type cannot be inferred.
pub(crate) fn handle_resolve_completion_type(request: &Map) -> Term {
    let Some(expression) = map_get(request, "expression").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'expression' field".to_string()]);
    };

    let pre_class_hierarchy = extract_class_hierarchy(request);
    resolve_completion_type_response(&expression, pre_class_hierarchy, native_type_registry())
}

/// Core `resolve_completion_type` resolution, taking the native type registry
/// as a parameter rather than reading the process-wide [`native_type_registry`]
/// directly, so the registry-provided path is unit-testable without touching
/// the global `OnceLock` or the filesystem (BT-2891).
pub(crate) fn resolve_completion_type_response(
    expression: &str,
    pre_class_hierarchy: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    native_type_registry: &beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry,
) -> Term {
    let mut hierarchy = beamtalk_core::semantic_analysis::ClassHierarchy::with_builtins();
    if !pre_class_hierarchy.is_empty() {
        hierarchy.add_from_beam_meta(pre_class_hierarchy);
    }

    match beamtalk_language_service::queries::completion_provider::resolve_expression_type(
        expression,
        &hierarchy,
        Some(native_type_registry),
    ) {
        Some(class_name) => Term::from(Map::from([
            (atom("status"), atom("ok")),
            (atom("class_name"), binary(&class_name)),
        ])),
        None => Term::from(Map::from([(atom("status"), atom("not_found"))])),
    }
}
