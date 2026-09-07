// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `diagnostics` (syntax/semantic check only) and `version` request
//! handlers.

use beamtalk_etf::{atom, binary_from_str as binary, map_get, term_to_string};
use eetf::{Map, Term};

use crate::decode::{
    extract_class_hierarchy, extract_known_type_aliases, extract_protocol_registry,
};
use crate::diagnostics::{DiagInfo, diag_category};
use crate::registry::diagnostics_overrides;
use crate::respond::{diagnostics_ok_response, error_response};

/// Handle a `diagnostics` request (syntax/semantic check only).
///
/// The optional `mode` field selects the grammar the buffer is analysed under:
///
///   * absent / `"expression"` — the buffer is a top-level script (the cockpit
///     Workspace + REPL editors): full-module `parse` + semantic analysis.
///   * `"method"` — the buffer is a BARE method body (the System Browser
///     method-editor tabs, e.g. `decrement => self.value := self.value - 1`).
///     A bare body is not a valid top-level construct, so `parse` reports a
///     false `expected expression, found =>` at the method-body separator
///     (BT-2569). We parse it with `parse_method` (the same standalone entry
///     `compile_method` uses) and return PARSE-ONLY diagnostics: a method
///     analysed outside its class has no field/`self`/type context, so running
///     semantic analysis here would emit false positives. Those checks run on
///     Compile (`compile_method`, which has class context); live squiggles
///     cover syntax.
///
/// The optional `class_hierarchy` field (ADR 0105 Phase 1, BT-2778) carries
/// pre-loaded class metadata — the same channel `compile_expression` /
/// `compile_method` already accept — so a re-check can inject a reloaded
/// class's *new* signature and see the resulting diagnostics located and
/// severity-tagged (`"expression"` mode only; `"method"` mode stays
/// class-context-free per the paragraph above).
///
/// The optional `protocol_registry` field (BT-3473) rides the same opt-in as
/// `class_hierarchy` (`beamtalk_compiler_server` threads both together, gated
/// on the same `class_hierarchy => true` request flag) and carries the live
/// image's ambient protocol cache. Without it, a protocol registered in
/// another file reaches `analyse_full` only as a zero-method `ClassInfo` in
/// `class_hierarchy` (the image's `beamtalk_object_class`/class-registration
/// path has no concept of "this class is actually a protocol"), which
/// defeats `is_type_compatible`'s nominal-mismatch escape hatch and makes
/// every selector on a protocol-typed receiver look unresolved. Supplying
/// the real protocol names (and their required selectors) lets the existing
/// BT-2088/BT-3472 filter in `analyse_full` drop the synthetic class entry
/// the same way it already does for the LSP's `ProjectIndex` path.
pub(crate) fn handle_diagnostics(request: &Map) -> Term {
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };

    let mode = map_get(request, "mode").and_then(term_to_string);
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (diagnostics, referenced_aliases) = if mode.as_deref() == Some("method") {
        let (_method, parse_diagnostics) = beamtalk_core::source_analysis::parse_method(tokens);
        (parse_diagnostics, Vec::new())
    } else {
        let (module, parse_diagnostics) = beamtalk_core::source_analysis::parse(tokens);
        let pre_class_hierarchy = extract_class_hierarchy(request);
        let pre_loaded_protocols = extract_protocol_registry(request);
        // BT-2899 (ADR 0108): `known_type_aliases` (the same channel
        // `compile_expression`/`compile_method` accept) so a re-check
        // round trip (`beamtalk_recheck.erl`) resolves `::` annotations
        // against the *current* session alias table — without this, a
        // candidate class referencing a live-redefined alias would resolve
        // it as an unknown nominal class instead of picking up the
        // redefinition, defeating the whole point of re-checking it.
        let pre_loaded_aliases = extract_known_type_aliases(request);
        beamtalk_language_service::queries::diagnostic_provider::compute_diagnostics_and_referenced_aliases(
            &module,
            parse_diagnostics,
            &[],
            pre_class_hierarchy,
            pre_loaded_protocols,
            pre_loaded_aliases,
            diagnostics_overrides(),
        )
    };

    let all_diags: Vec<DiagInfo> = diagnostics.iter().map(diag_info).collect();

    diagnostics_ok_response(&all_diags, &referenced_aliases)
}

/// Map a compiler `Diagnostic` to the wire `DiagInfo` (byte-offset span +
/// stringified severity) the cockpit's `@codemirror/lint` source consumes.
pub(crate) fn diag_info(d: &beamtalk_core::source_analysis::Diagnostic) -> DiagInfo {
    DiagInfo {
        message: d.message.to_string(),
        severity: d.severity.as_str().to_string(),
        category: diag_category(d),
        start: d.span.start(),
        end: d.span.end(),
    }
}

/// Handle a `version` request.
pub(crate) fn handle_version() -> Term {
    let version = env!("BEAMTALK_VERSION");
    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("version"), binary(version)),
    ]))
}
