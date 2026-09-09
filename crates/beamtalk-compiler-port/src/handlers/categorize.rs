// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `categorize_methods` request handler.

use beamtalk_etf::{atom, binary_from_str as binary, map_get, term_to_string};
use eetf::{List, Map, Term};

use crate::respond::error_response;

use super::class_span::span_term;

/// Builds a `#{selector => <<...>>, side => instance | class, span =>
/// #{start, end}}` map term for one [`CategorizedMethod`](beamtalk_core::source_analysis::CategorizedMethod).
pub(crate) fn categorized_method_term(
    method: &beamtalk_core::source_analysis::CategorizedMethod,
) -> Term {
    use beamtalk_core::source_analysis::MethodSide;
    let side = match method.side {
        MethodSide::Instance => "instance",
        MethodSide::Class => "class",
    };
    Term::from(Map::from([
        (atom("selector"), binary(&method.selector)),
        (atom("side"), atom(side)),
        (atom("span"), span_term(method.span)),
    ]))
}

/// Builds a `#{name => <<...>> | undefined, divider_span => #{start, end} |
/// undefined, methods => [MethodMap, ...]}` map term for one
/// [`MethodCategory`](beamtalk_core::source_analysis::MethodCategory).
///
/// `name`/`divider_span` are always present (using the atom `undefined` as
/// the "absent" sentinel, never an omitted key) — the write-path
/// caller (the Cockpit's `save-section` op) needs `divider_span` to locate
/// an existing divider's byte span for a rename, and a consistent key set
/// makes both consumers' Erlang-side pattern matching uniform. `undefined`
/// as a value is indistinguishable from an omitted key to `maps:get/3`'s
/// default-value form (the original read-only consumer,
/// `beamtalk_interface.erl`, already reads `name` that way), so this is a
/// superset of the original "omit, never null" shape, not a breaking
/// change to it.
pub(crate) fn category_term(category: &beamtalk_core::source_analysis::MethodCategory) -> Term {
    let methods: Vec<Term> = category
        .methods
        .iter()
        .map(categorized_method_term)
        .collect();
    Term::from(Map::from([
        (
            atom("name"),
            category
                .name
                .as_deref()
                .map_or_else(|| atom("undefined"), binary),
        ),
        (
            atom("divider_span"),
            category
                .divider_span
                .map_or_else(|| atom("undefined"), span_term),
        ),
        (atom("methods"), Term::from(List::from(methods))),
    ]))
}

/// Handle a `categorize_methods` request.
///
/// Groups a class's methods by its `// === Name ===` section dividers —
/// `beamtalk_core::source_analysis::categorize_methods_in_source` is the
/// single, canonical recognizer already used by the LSP's
/// `documentSymbol` outline; this command is the bridge that lets Erlang
/// surfaces (which have no Rust parser of their own) reach the same
/// function instead of reimplementing its recognition grammar — see that
/// module's doc for why a second implementation must be avoided.
/// The Cockpit's grouped method view + section
/// authoring is the second consumer and the reason each category also
/// carries `divider_span` and each method a `span` (the original
/// REPL/MCP consumer only needed `name`/`selector`/`side`; the Cockpit's
/// `save-section` write path needs the divider's own byte span to splice a
/// rename).
///
/// Request fields:
/// - `source` (binary): the current on-disk source text of the `.bt` file
/// - `class_name` (binary): the target class name (e.g. `Counter`)
///
/// Response on success: `#{status => ok, categories => [CategoryMap, ...]}`,
/// each `CategoryMap` shaped `#{name => <<...>> | undefined, divider_span =>
/// #{start, end} | undefined, methods => [MethodMap, ...]}` and each
/// `MethodMap` shaped `#{selector => <<...>>, side => instance | class, span
/// => #{start, end}}`, all in source order. A class with no dividers comes
/// back as a single category with `name => undefined` — callers gate on this
/// (`has_dividers`, mirroring `document_symbols_provider.rs`) to fall back to
/// their flat rendering. Failure (class not found, or the class
/// name is ambiguous — more than one class definition with that name in
/// `source`) comes back as `#{status => error, reason => class_not_found |
/// ambiguous, message => <<...>>}`.
pub(crate) fn handle_categorize_methods(request: &Map) -> Term {
    use beamtalk_core::source_analysis::categorize_methods_in_source;

    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };
    let Some(class_name) = map_get(request, "class_name").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'class_name' field".to_string()]);
    };

    // Parse diagnostics are intentionally not surfaced as a failure here,
    // same rationale as `handle_resolve_class_span`: the caller only needs
    // the categorized methods.
    let (result, _diagnostics) = categorize_methods_in_source(&source, &class_name);
    match result {
        Ok(categories) => {
            let category_terms: Vec<Term> = categories.iter().map(category_term).collect();
            Term::from(Map::from([
                (atom("status"), atom("ok")),
                (atom("categories"), Term::from(List::from(category_terms))),
            ]))
        }
        Err(err) => categorize_methods_error_response(&err),
    }
}

pub(crate) fn categorize_methods_error_response(
    err: &beamtalk_core::source_analysis::CategorizeMethodsError,
) -> Term {
    use beamtalk_core::source_analysis::CategorizeMethodsError;
    let reason = match err {
        CategorizeMethodsError::ClassNotFound { .. } => "class_not_found",
        CategorizeMethodsError::Ambiguous { .. } => "ambiguous",
    };
    Term::from(Map::from([
        (atom("status"), atom("error")),
        (atom("reason"), atom(reason)),
        (atom("message"), binary(&err.to_string())),
    ]))
}
