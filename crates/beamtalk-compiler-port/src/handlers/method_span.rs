// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0082 Phase 1 (BT-2283) method-source-span commands:
//! `resolve_method_span`, `reindent_method_source`,
//! `find_selector_send_spans`, and `find_definition_selector_spans`.

use beamtalk_etf::{
    atom, binary_from_str as binary, int_term, map_get, term_to_atom, term_to_string,
};
use eetf::{List, Map, Term};

use crate::respond::error_response;

/// Handle a `resolve_method_span` request (ADR 0082, Phase 1 — BT-2283).
///
/// Backs the live-patch install hook: given the current on-disk source of a
/// `.bt` file and a target `(class, selector, side)`, resolve the exact byte
/// span of that method's definition (the Phase 0 resolver) and return both the
/// span and the bytes currently occupying it (`prev_source`). The install hook
/// records these on the `ChangeEntry` so a later `Workspace flush` can splice the
/// patched body back into the file by byte-span replacement, and so restart can
/// detect whether disk has drifted from the recorded `prev_source`.
///
/// Request fields:
/// - `source` (binary): the current on-disk source text of the `.bt` file
/// - `class_name` (binary): the target class name (e.g. `Counter`)
/// - `selector` (binary): the canonical selector string (e.g. `increment`,
///   `incrementBy:`, `+`)
/// - `side` (atom, optional): `instance` (default) or `class`
///
/// Response on success: `#{status => ok, span => #{start => S, end => E},
/// prev_source => <<...>>}`. The resolver is purely parser-level: it never
/// installs anything and never panics. Failures (selector not found, class not
/// found, ambiguous) come back as `#{status => error, reason => <atom>, ...}`
/// so the hook can downgrade to a memory-only patch (no `ChangeEntry`) rather
/// than crash the install.
pub(crate) fn handle_resolve_method_span(request: &Map) -> Term {
    use beamtalk_core::source_analysis::{MethodSide, resolve_method_span};

    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };
    let Some(class_name) = map_get(request, "class_name").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'class_name' field".to_string()]);
    };
    let Some(selector) = map_get(request, "selector").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'selector' field".to_string()]);
    };
    // `side` is optional; absent (or unrecognised) means the instance side.
    let side = match map_get(request, "side").and_then(term_to_atom).as_deref() {
        Some("class") => MethodSide::Class,
        _ => MethodSide::Instance,
    };

    // Parse diagnostics are intentionally not surfaced as a failure here: the
    // hook only needs the span. A method whose own body is malformed would not
    // have been installed in the first place (the install path compiles before
    // hooking), so a clean span resolution against the disk file is what matters.
    let (result, _diagnostics) = resolve_method_span(&source, &class_name, &selector, side);
    match result {
        Ok(span) => {
            let start = span.start();
            let end = span.end();
            // The resolved span must address real bytes of `source`. If slicing
            // fails (out-of-bounds / non-char-boundary), surface a structured
            // error rather than recording an empty `prev_source` under
            // `status => ok` — a bogus span/source would corrupt later flush and
            // drift checks.
            let Some(prev_source) = source.get(start as usize..end as usize) else {
                return Term::from(Map::from([
                    (atom("status"), atom("error")),
                    (atom("reason"), atom("invalid_span")),
                    (
                        atom("message"),
                        binary(&format!(
                            "Resolved method span {start}..{end} is out of bounds \
                             for source of length {}",
                            source.len()
                        )),
                    ),
                ]));
            };
            let span_map = Term::from(Map::from([
                (
                    atom("start"),
                    int_term(i32::try_from(start).unwrap_or(i32::MAX)),
                ),
                (
                    atom("end"),
                    int_term(i32::try_from(end).unwrap_or(i32::MAX)),
                ),
            ]));
            Term::from(Map::from([
                (atom("status"), atom("ok")),
                (atom("span"), span_map),
                (atom("prev_source"), binary(prev_source)),
            ]))
        }
        Err(err) => method_span_error_response(&err),
    }
}

/// Handle a `reindent_method_source` request (BT-2584).
///
/// Re-lays-out a canonical (column-0) method body at the given `base_indent`,
/// producing the on-disk byte-span shape. It re-parses the body and re-renders
/// it with the line-width budget reduced by the indent, so width-sensitive lines
/// re-break exactly as `bt fmt` does on disk (BT-2594), then shifts. The
/// live-patch install hook calls this so the `ChangeEntry`'s stored `source` is a
/// drop-in for `disk[span]` — `source_ref == disk[span]` by construction — and a
/// later `Workspace flush` splices it verbatim with no reshaping (retiring the
/// former `beamtalk_workspace_flush:reindent/2`).
///
/// Request fields:
/// - `source` (binary): the canonical column-0 method body (`unparse_method`)
/// - `base_indent` (binary, optional): the leading whitespace of the on-disk
///   definition's first line (empty = identity)
///
/// Response: `#{status => ok, source => <<...>>}`. The transform always succeeds
/// — it falls back to a plain whitespace shift when the body does not re-parse.
/// (The Erlang port wrappers still surface transport/timeout errors as
/// `{error, port_error, _}` around this call.)
pub(crate) fn handle_reindent_method_source(request: &Map) -> Term {
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };
    let base_indent = map_get(request, "base_indent")
        .and_then(term_to_string)
        .unwrap_or_default();
    let reindented = beamtalk_core::unparse::reindent_method_source(&base_indent, &source);
    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("source"), binary(&reindented)),
    ]))
}

/// Handle a `find_selector_send_spans` request (ADR 0114, BT-3279).
///
/// Backs `Behaviour>>renameSelector:to:`'s reference-site rewrite: given one
/// owning method's source text (a slice already resolved via
/// `resolve_method_span`) and an `(old_selector, new_selector)` pair,
/// resolve the exact byte span(s) of every self/super-directed send of
/// `old_selector` within it — the splice targets a safe auto-rewrite needs.
/// `beamtalk_xref:senders_of/1` only carries a *line* number per sending
/// method, and a whole-method span is too coarse to splice a single send's
/// selector token(s) without corrupting the rest of the body; see
/// [`beamtalk_language_service::queries::selector_rename_query::find_selector_send_spans`]
/// for the full "why not regex" rationale (a multi-keyword selector like
/// `at:put:` can have arbitrary nested expressions between its keyword
/// parts).
///
/// Request fields:
/// - `method_source` (binary): the method source text to search
/// - `old_selector` (binary): the selector being renamed
/// - `new_selector` (binary): its replacement
///
/// Response: `#{status => ok, occurrences => [[#{start => S, end => E,
/// new_text => <<...>>}, ...], ...]}` — one inner list per matched self/super
/// send (a keyword selector contributes one map per keyword part, in
/// keyword-part order; unary/binary contribute a single-element inner
/// list). Returns an empty outer list when no matching sends are found, the
/// source cannot be parsed, or `old_selector`/`new_selector` differ in
/// keyword arity for a given occurrence (that occurrence is simply skipped,
/// never a panic) — this resolver has no failure mode beyond "found
/// nothing" (see that function's own doc), so there is no `status => error`
/// shape here.
pub(crate) fn handle_find_selector_send_spans(request: &Map) -> Term {
    let Some(method_source) = map_get(request, "method_source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'method_source' field".to_string()]);
    };
    let Some(old_selector) = map_get(request, "old_selector").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'old_selector' field".to_string()]);
    };
    let Some(new_selector) = map_get(request, "new_selector").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'new_selector' field".to_string()]);
    };

    let occurrences =
        beamtalk_language_service::queries::selector_rename_query::find_selector_send_spans(
            &method_source,
            &old_selector,
            &new_selector,
        );

    let occurrence_terms: Vec<Term> = occurrences
        .iter()
        .map(|spans| Term::from(List::from(selector_send_span_terms(spans))))
        .collect();

    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (
            atom("occurrences"),
            Term::from(List::from(occurrence_terms)),
        ),
    ]))
}

/// Shared `#{start, end, new_text}` term builder for both selector-rename
/// span commands (`find_selector_send_spans`, `find_definition_selector_spans`).
pub(crate) fn selector_send_span_terms(
    spans: &[beamtalk_language_service::queries::selector_rename_query::SelectorSendSpan],
) -> Vec<Term> {
    spans
        .iter()
        .map(|s| {
            Term::from(Map::from([
                (
                    atom("start"),
                    int_term(i32::try_from(s.span.start()).unwrap_or(i32::MAX)),
                ),
                (
                    atom("end"),
                    int_term(i32::try_from(s.span.end()).unwrap_or(i32::MAX)),
                ),
                (atom("new_text"), binary(&s.new_text)),
            ]))
        })
        .collect()
}

/// Handle a `find_definition_selector_spans` request (ADR 0114, BT-3279).
///
/// Backs `Behaviour>>renameSelector:to:`'s DEFINITION-site rewrite: given a
/// class's current full source, resolve `(old_selector, side)`'s own
/// method-definition selector-token span(s) — a narrow splice target, never
/// the whole method body. See
/// [`beamtalk_language_service::queries::selector_rename_query::find_definition_selector_spans`]'s
/// doc for why a whole-body replacement here would corrupt the method's own
/// parameter names/logic on rewrite, and for how the unary/binary case
/// (which carries no dedicated selector span from the parser) is resolved.
///
/// Request fields:
/// - `source` (binary): the class's current full source text
/// - `class_name` (binary): the target class name
/// - `old_selector` (binary): the selector being renamed
/// - `new_selector` (binary): its replacement
/// - `side` (atom, optional): `instance` (default) or `class`
///
/// Response on success: `#{status => ok, spans => [#{start => S, end => E,
/// new_text => <<...>>}, ...]}` (empty when `old_selector`/`new_selector`
/// differ in keyword arity — never a panic). Failures (class not found,
/// selector not found, ambiguous) come back as `#{status => error, reason
/// => <atom>, ...}`, mirroring `resolve_method_span`'s own error shape
/// exactly (same [`SpanResolveError`] variants, same `method_span_error_response`).
pub(crate) fn handle_find_definition_selector_spans(request: &Map) -> Term {
    use beamtalk_core::source_analysis::MethodSide;
    use beamtalk_language_service::queries::selector_rename_query::find_definition_selector_spans;

    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };
    let Some(class_name) = map_get(request, "class_name").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'class_name' field".to_string()]);
    };
    let Some(old_selector) = map_get(request, "old_selector").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'old_selector' field".to_string()]);
    };
    let Some(new_selector) = map_get(request, "new_selector").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'new_selector' field".to_string()]);
    };
    // `side` is optional; absent (or unrecognised) means the instance side —
    // same convention as `handle_resolve_method_span`.
    let side = match map_get(request, "side").and_then(term_to_atom).as_deref() {
        Some("class") => MethodSide::Class,
        _ => MethodSide::Instance,
    };

    match find_definition_selector_spans(&source, &class_name, &old_selector, &new_selector, side) {
        Ok(spans) => Term::from(Map::from([
            (atom("status"), atom("ok")),
            (
                atom("spans"),
                Term::from(List::from(selector_send_span_terms(&spans))),
            ),
        ])),
        Err(err) => method_span_error_response(&err),
    }
}

/// Build a structured error response for a [`SpanResolveError`].
///
/// The `reason` atom lets the Erlang hook branch without string-matching; the
/// `message` carries the human-readable detail for logging.
pub(crate) fn method_span_error_response(
    err: &beamtalk_core::source_analysis::SpanResolveError,
) -> Term {
    use beamtalk_core::source_analysis::SpanResolveError;
    let reason = match err {
        SpanResolveError::ClassNotFound { .. } => "class_not_found",
        SpanResolveError::SelectorNotFound { .. } => "selector_not_found",
        SpanResolveError::Ambiguous { .. } => "ambiguous",
    };
    Term::from(Map::from([
        (atom("status"), atom("error")),
        (atom("reason"), atom(reason)),
        (atom("message"), binary(&err.to_string())),
    ]))
}
