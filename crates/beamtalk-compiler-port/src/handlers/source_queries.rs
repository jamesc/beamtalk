// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Single-compiled-method source queries: `find_senders_in_source`,
//! `find_all_sends_in_source`, `find_announce_sites_in_source`,
//! `find_references_to_in_source`, `find_field_readers_in_source`,
//! `find_field_writers_in_source`, and `find_ffi_sites_in_source`.

use beamtalk_etf::{
    atom, binary_from_str as binary, int_term, map_get, term_to_string, term_to_usize,
};
use eetf::{List, Map, Term};

use crate::respond::error_response;

/// Handle a `find_senders_in_source` request.
///
/// Backs `SystemNavigation sendersOf:` — parses the source of a single compiled method
/// and reports 1-based line numbers (relative to the input source) where a
/// `MessageSend` or `Cascade` with the given selector appears.
///
/// Request fields:
/// - `source` (binary): the method source text as returned by `CompiledMethod source`
/// - `selector` (binary): the target selector name (without the leading `#`)
///
/// Response: `#{status => ok, lines => [Line, ...]}`. Returns an empty list
/// when no senders are found or the source cannot be parsed.
pub(crate) fn handle_find_senders_in_source(request: &Map) -> Term {
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };
    let Some(selector) = map_get(request, "selector").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'selector' field".to_string()]);
    };

    let lines = beamtalk_language_service::queries::senders_query::find_senders_in_source(
        &source, &selector,
    );
    ok_lines_response(&lines)
}

/// Handle a `find_all_sends_in_source` request.
///
/// Backs `SystemNavigation unimplementedSelectors` — parses the source of a
/// single compiled method and reports EVERY message send (selector name,
/// 1-based line number relative to the input, and receiver kind), in a single
/// pass. The typo-finder computes `allSentSelectors − allDefinedSelectors`
/// from these results without re-parsing each method per candidate selector.
///
/// Request fields:
/// - `source` (binary): the method source text as returned by `CompiledMethod source`
///
/// Response: `#{status => ok, sends => [#{selector => <binary>, line => <int>,
/// recv => self|super|erlang_ffi|other, target_module => <binary>}, ...]}`.
/// `target_module` is the native (Erlang) module an `erlang_ffi` send targets
/// it is the empty binary (`<<>>`) for non-FFI sends and for FFI
/// chains whose module receiver is not a static `Erlang <module>` form. It is
/// returned as a binary (not an atom) so the response decodes safely with
/// `[safe]`; the caller interns it only when indexing. Returns an empty list
/// when the source has no sends or cannot be parsed.
pub(crate) fn handle_find_all_sends_in_source(request: &Map) -> Term {
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };

    let sends =
        beamtalk_language_service::queries::all_sends_query::find_all_sends_in_source(&source);
    let send_terms: Vec<Term> = sends
        .iter()
        .map(|hit| {
            let recv = match hit.receiver {
                beamtalk_language_service::queries::all_sends_query::ReceiverKind::SelfReceiver => atom("self"),
                beamtalk_language_service::queries::all_sends_query::ReceiverKind::SuperReceiver => {
                    atom("super")
                }
                beamtalk_language_service::queries::all_sends_query::ReceiverKind::ErlangFfi => {
                    atom("erlang_ffi")
                }
                beamtalk_language_service::queries::all_sends_query::ReceiverKind::Other => atom("other"),
            };
            let target_module = hit.target_module.as_deref().unwrap_or("");
            Term::from(Map::from([
                (atom("selector"), binary(&hit.selector)),
                (
                    atom("line"),
                    int_term(i32::try_from(hit.line).unwrap_or(i32::MAX)),
                ),
                (atom("recv"), recv),
                (atom("target_module"), binary(target_module)),
            ]))
        })
        .collect();

    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("sends"), Term::from(List::from(send_terms))),
    ]))
}

/// Handle a `find_announce_sites_in_source` request.
///
/// Backs `SystemNavigation announcementsSentBy:` — parses the source of a single
/// compiled method and reports every `announce:` / `announceAndWait:` /
/// `announceAndWait:timeout:` emission site: the announce selector, the 1-based
/// line number (relative to the input), and the syntactically-resolved
/// announcement class name. The class name is empty (`<<>>`) when the event
/// argument is unresolvable (a bare identifier, literal, chained send, …) — the
/// caller treats that as a documented miss, not an error.
///
/// Request fields:
/// - `source` (binary): the method source text as returned by `CompiledMethod source`
///
/// Response: `#{status => ok, sites => [#{selector => <binary>, line => <int>,
/// announcement_class => <binary>}, ...]}`. The `announcement_class` is returned
/// as a binary (not an atom) so the response decodes safely with `[safe]`; the
/// caller interns it to an atom only when resolving to a live class. Returns an
/// empty list when the source has no emissions or cannot be parsed.
pub(crate) fn handle_find_announce_sites_in_source(request: &Map) -> Term {
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };

    let sites =
        beamtalk_language_service::queries::announce_sites_query::find_announce_sites_in_source(
            &source,
        );
    let site_terms: Vec<Term> = sites
        .iter()
        .map(|hit| {
            let class_bin = hit.announcement_class.as_deref().unwrap_or("");
            Term::from(Map::from([
                (atom("selector"), binary(&hit.selector)),
                (
                    atom("line"),
                    int_term(i32::try_from(hit.line).unwrap_or(i32::MAX)),
                ),
                (atom("announcement_class"), binary(class_bin)),
            ]))
        })
        .collect();

    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("sites"), Term::from(List::from(site_terms))),
    ]))
}

/// Handle a `find_references_to_in_source` request.
///
/// Backs `SystemNavigation referencesTo:` — parses the source of a single
/// compiled method and reports 1-based line numbers (relative to the input
/// source) where a `ClassReference` AST node with the given class name
/// appears.
///
/// Request fields:
/// - `source` (binary): the method source text as returned by `CompiledMethod source`
/// - `class_name` (binary): the target class name (without the leading `#`)
///
/// Response: `#{status => ok, lines => [Line, ...]}`. Returns an empty list
/// when no references are found or the source cannot be parsed.
pub(crate) fn handle_find_references_to_in_source(request: &Map) -> Term {
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };
    let Some(class_name) = map_get(request, "class_name").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'class_name' field".to_string()]);
    };

    let lines =
        beamtalk_language_service::queries::references_to_query::find_references_to_in_source(
            &source,
            &class_name,
        );
    ok_lines_response(&lines)
}

/// Handle a `find_field_readers_in_source` request.
///
/// Backs `SystemNavigation fieldReadersOf:in:` — parses the source of a
/// single compiled method and reports 1-based line numbers (relative to the
/// input source) where the named field is READ (`self.x` outside
/// an assignment target).
///
/// Request fields:
/// - `source` (binary): the method source text as returned by `CompiledMethod source`
/// - `field` (binary): the target field name (without the leading `#`)
///
/// Response: `#{status => ok, lines => [Line, ...]}`. Returns an empty list
/// when no reads are found or the source cannot be parsed.
pub(crate) fn handle_find_field_readers_in_source(request: &Map) -> Term {
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };
    let Some(field) = map_get(request, "field").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'field' field".to_string()]);
    };

    let lines =
        beamtalk_language_service::queries::field_accesses_query::find_field_readers_in_source(
            &source, &field,
        );
    ok_lines_response(&lines)
}

/// Handle a `find_field_writers_in_source` request.
///
/// Backs `SystemNavigation fieldWritersOf:in:` — parses the source of a
/// single compiled method and reports 1-based line numbers (relative to the
/// input source) where the named field is WRITTEN (`self.x := ...`,
/// the assignment target).
///
/// Request fields:
/// - `source` (binary): the method source text as returned by `CompiledMethod source`
/// - `field` (binary): the target field name (without the leading `#`)
///
/// Response: `#{status => ok, lines => [Line, ...]}`. Returns an empty list
/// when no writes are found or the source cannot be parsed.
pub(crate) fn handle_find_field_writers_in_source(request: &Map) -> Term {
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };
    let Some(field) = map_get(request, "field").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'field' field".to_string()]);
    };

    let lines =
        beamtalk_language_service::queries::field_accesses_query::find_field_writers_in_source(
            &source, &field,
        );
    ok_lines_response(&lines)
}

/// Handle a `find_ffi_sites_in_source` request.
///
/// Backs `SystemNavigation ffiSitesFor:` — parses the source of a single
/// compiled method and reports 1-based line numbers (relative to the input
/// source) where the named Erlang FFI function (`module`:`function`, optionally
/// constrained to `arity`) is invoked through the `Erlang` bridge.
///
/// Request fields:
/// - `source` (binary): the method source text as returned by `CompiledMethod source`
/// - `module` (binary): the Erlang module name (e.g. `lists`)
/// - `function` (binary): the Erlang function name (e.g. `reverse`)
/// - `arity` (int, optional): when present, only call sites with this argument
///   count match; absent means any arity
///
/// Response: `#{status => ok, lines => [Line, ...]}`. Returns an empty list
/// when no sites are found or the source cannot be parsed.
pub(crate) fn handle_find_ffi_sites_in_source(request: &Map) -> Term {
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };
    let Some(module) = map_get(request, "module").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'module' field".to_string()]);
    };
    let Some(function) = map_get(request, "function").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'function' field".to_string()]);
    };
    // `arity` is optional: absent (or non-integer) means "match any arity".
    let arity = map_get(request, "arity").and_then(term_to_usize);

    let lines = beamtalk_language_service::queries::ffi_sites_query::find_ffi_sites_in_source(
        &source, &module, &function, arity,
    );
    ok_lines_response(&lines)
}

/// Build the standard `#{status => ok, lines => [...]}` response shared by the
/// senders query, references-to query, field reader/writer
/// queries, and FFI sites query.
pub(crate) fn ok_lines_response(lines: &[u32]) -> Term {
    let line_terms: Vec<Term> = lines
        .iter()
        .map(|&line| int_term(i32::try_from(line).unwrap_or(i32::MAX)))
        .collect();

    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("lines"), Term::from(List::from(line_terms))),
    ]))
}
