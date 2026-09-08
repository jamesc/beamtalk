// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0082 extension class-source-span commands: `resolve_class_span`
//! (BT-3248), `class_state_field_defaults` (BT-3254), and
//! `build_class_module_index_in_source` (BT-3441).

use beamtalk_etf::{atom, binary_from_str as binary, int_term, map_get, term_to_string};
use eetf::{List, Map, Term};

use crate::respond::error_response;

/// Handle a `resolve_class_span` request (ADR 0082 extension, BT-3248).
///
/// Backs the CHANGES dock's disk-vs-memory diff for a `'class-def'`
/// `ChangeEntry` (redefining an *existing* class via the cockpit `:def` tab):
/// given the current on-disk source of a `.bt` file and a target class name,
/// resolve the byte span of that class's declaration line through its last
/// `state:`/`field:` declaration — **never** its methods (the
/// [`resolve_class_span`](beamtalk_core::source_analysis::resolve_class_span)
/// resolver's own module doc has the full "why" and the data-loss bug this
/// boundary avoids) — and return both the span and the bytes currently
/// occupying it (`prev_source`).
///
/// Request fields:
/// - `source` (binary): the current on-disk source text of the `.bt` file
/// - `class_name` (binary): the target class name (e.g. `Counter`)
///
/// Response on success: `#{status => ok, span => #{start => S, end => E},
/// prev_source => <<...>>}`. Failures (class not found, ambiguous) come back
/// as `#{status => error, reason => <atom>, ...}`.
pub(crate) fn handle_resolve_class_span(request: &Map) -> Term {
    use beamtalk_core::source_analysis::resolve_class_span;

    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };
    let Some(class_name) = map_get(request, "class_name").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'class_name' field".to_string()]);
    };

    // Parse diagnostics are intentionally not surfaced as a failure here, same
    // rationale as `handle_resolve_method_span`: the hook only needs the span.
    let (result, _diagnostics) = resolve_class_span(&source, &class_name);
    match result {
        Ok(span) => {
            let start = span.start();
            let end = span.end();
            let Some(prev_source) = source.get(start as usize..end as usize) else {
                return Term::from(Map::from([
                    (atom("status"), atom("error")),
                    (atom("reason"), atom("invalid_span")),
                    (
                        atom("message"),
                        binary(&format!(
                            "Resolved class span {start}..{end} is out of bounds \
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
        Err(err) => class_span_error_response(&err),
    }
}

/// Handle a `class_state_field_defaults` request (ADR 0082 extension,
/// BT-3254).
///
/// Backs `beamtalk_repl_loader:class_def_source_is_skeleton_shaped/2`'s
/// sibling safety check before marking a `'class-def'` `ChangeLog` entry
/// flushable: whether resubmitting a candidate class-body text for `class`
/// would silently drop a field's default value, compared against the
/// on-disk text — see
/// [`class_state_field_defaults`](beamtalk_core::source_analysis::class_state_field_defaults)'s
/// own doc for the full "why" (live class reflection cannot recover a
/// compiled class's default-value TEXT, only whether one exists).
///
/// Request fields:
/// - `source` (binary): the class-body source text to inspect
/// - `class_name` (binary): the target class name (e.g. `Counter`)
///
/// Response on success: `#{status => ok, field_defaults => #{FieldName =>
/// true | false, ...}}`, one entry per declared `state:`/`field:`. Failures
/// (class not found, ambiguous) come back as `#{status => error, ...}`, same
/// shape as `resolve_class_span`.
pub(crate) fn handle_class_state_field_defaults(request: &Map) -> Term {
    use beamtalk_core::source_analysis::class_state_field_defaults;

    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };
    let Some(class_name) = map_get(request, "class_name").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'class_name' field".to_string()]);
    };

    match class_state_field_defaults(&source, &class_name) {
        Some(defaults) => {
            let mut field_map: std::collections::HashMap<Term, Term> =
                std::collections::HashMap::new();
            for (field, has_default) in defaults {
                field_map.insert(
                    binary(&field),
                    atom(if has_default { "true" } else { "false" }),
                );
            }
            Term::from(Map::from([
                (atom("status"), atom("ok")),
                (atom("field_defaults"), Term::from(Map::from(field_map))),
            ]))
        }
        // `class_state_field_defaults` collapses "not found" and "ambiguous"
        // into one `None` (it has no splice-safety span to report, unlike
        // `resolve_class_span`, so the finer distinction isn't needed by its
        // one caller) — the loader treats either as "cannot confirm safety".
        None => Term::from(Map::from([
            (atom("status"), atom("error")),
            (atom("reason"), atom("class_not_found")),
            (
                atom("message"),
                binary(&format!(
                    "class `{class_name}` not found or ambiguous in source"
                )),
            ),
        ])),
    }
}

/// Handle a `build_class_module_index_in_source` request (BT-3441).
///
/// Backs the REPL/workspace cold-load fallback for `class_module_index`
/// (ADR 0050, `beamtalk_repl_ops_load:build_source_class_module_index/1`):
/// given the source text of a single `src/**/*.bt` file, its path relative
/// to `src/` (extension included, `/`-joined), and the project's package
/// name, returns every class the file declares plus the package-qualified
/// module atom the CLI's own index build (`build_class_module_index` /
/// `compute_relative_module`, `crates/beamtalk-cli/src/commands/build.rs`)
/// would compute for it. Class extraction uses the real parser (never a
/// regex, so a `subclass:` declaration in any shape the grammar allows is
/// found), and the module name is computed via the shared
/// `relative_module_segments` leaf — the same one `compute_relative_module`
/// and `ClassModuleRegistry`'s Pass-1 construction already use — so the
/// Erlang cold-load index can never diverge from the CLI's by re-deriving
/// its own casing rule.
///
/// Request fields:
/// - `source` (binary): the `.bt` file's source text
/// - `relative_path` (binary): the file's path relative to `src/`,
///   `/`-joined, extension included (e.g. `util/http_response.bt`)
/// - `package_name` (binary): the project's package name (e.g. `web`)
///
/// Response on success: `#{status => ok, module_name =>
/// <<"bt@web@util@http_response">>, classes => [<<"ClassName">>, ...]}`.
/// Parse errors in `source` are not surfaced as a failure — a partially
/// recovered class list still beats the previous regex scanner's silent
/// drop, and the caller only needs whatever classes the file declares.
/// `relative_path` is expected to always be a real project file under
/// `src/` (never client-supplied), but a segment outside
/// `[A-Za-z0-9_]` comes back as `#{status => error, reason =>
/// invalid_path_segment, message => <<...>>}` rather than a crash.
pub(crate) fn handle_build_class_module_index_in_source(request: &Map) -> Term {
    use beamtalk_core::semantic_analysis::relative_module_segments;

    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };
    let Some(relative_path) = map_get(request, "relative_path").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'relative_path' field".to_string()]);
    };
    let Some(package_name) = map_get(request, "package_name").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'package_name' field".to_string()]);
    };

    let segments = match relative_module_segments(camino::Utf8Path::new(&relative_path)) {
        Ok(segments) => segments,
        Err(err) => {
            return Term::from(Map::from([
                (atom("status"), atom("error")),
                (atom("reason"), atom("invalid_path_segment")),
                (atom("message"), binary(&err.to_string())),
            ]));
        }
    };
    let module_name = format!("bt@{package_name}@{}", segments.join("@"));

    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (module, _diagnostics) = beamtalk_core::source_analysis::parse(tokens);
    let classes: Vec<Term> = module
        .classes
        .iter()
        .map(|class| binary(&class.name.name))
        .collect();

    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("module_name"), binary(&module_name)),
        (atom("classes"), Term::from(List::from(classes))),
    ]))
}

pub(crate) fn class_span_error_response(
    err: &beamtalk_core::source_analysis::ClassSpanResolveError,
) -> Term {
    use beamtalk_core::source_analysis::ClassSpanResolveError;
    let reason = match err {
        ClassSpanResolveError::ClassNotFound { .. } => "class_not_found",
        ClassSpanResolveError::Ambiguous { .. } => "ambiguous",
    };
    Term::from(Map::from([
        (atom("status"), atom("error")),
        (atom("reason"), atom(reason)),
        (atom("message"), binary(&err.to_string())),
    ]))
}

/// Builds a `#{start => S, end => E}` map term for `span`, the shared shape
/// used by every span-carrying response in this file (`resolve_method_span`,
/// `resolve_class_span`, and this command).
pub(crate) fn span_term(span: beamtalk_core::source_analysis::Span) -> Term {
    Term::from(Map::from([
        (
            atom("start"),
            int_term(i32::try_from(span.start()).unwrap_or(i32::MAX)),
        ),
        (
            atom("end"),
            int_term(i32::try_from(span.end()).unwrap_or(i32::MAX)),
        ),
    ]))
}
