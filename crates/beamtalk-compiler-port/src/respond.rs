// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Response term construction for compiler-port replies.
//!
//! Builds the `#{status => ok | error, ...}` ETF response maps every
//! request handler returns, from `ok_response`'s minimal shape through
//! the richer class/method/protocol/type-alias-definition and
//! diagnostic-error response builders.

use beamtalk_etf::{atom, binary_from_str as binary};
use eetf::{List, Map, Term};

use crate::diagnostics::DiagInfo;

// ---------------------------------------------------------------------------
// Response term construction helpers
// ---------------------------------------------------------------------------

/// Build ETF warning terms from string messages.
pub(crate) fn build_warning_terms(warnings: &[String]) -> Vec<Term> {
    warnings.iter().map(|w| binary(w)).collect()
}

/// Build ETF class terms from `(name, superclass)` pairs.
pub(crate) fn build_class_terms(classes: &[(String, String)]) -> Vec<Term> {
    classes
        .iter()
        .map(|(name, superclass)| {
            Term::from(Map::from([
                (atom("name"), binary(name)),
                (atom("superclass"), binary(superclass)),
            ]))
        })
        .collect()
}

/// Build ETF terms for the `referenced_aliases` response field (the ADR 0108
/// hot-reload re-check trigger) — every alias name this compile's
/// annotations transitively depended on
/// (`AnalysisResult::referenced_aliases`, already sorted/deduplicated
/// there), as a plain list of binaries. The Erlang side
/// (`beamtalk_repl_loader`) feeds this into `beamtalk_alias_xref:
/// register_class/2` at class-install time.
pub(crate) fn build_referenced_alias_terms(referenced_aliases: &[ecow::EcoString]) -> Vec<Term> {
    referenced_aliases.iter().map(|n| binary(n)).collect()
}

/// Build a response map for a successful `compile_expression`.
pub(crate) fn ok_response(core_erlang: &str, warnings: &[String]) -> Term {
    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("core_erlang"), binary(core_erlang)),
        (
            atom("warnings"),
            Term::from(List::from(build_warning_terms(warnings))),
        ),
    ]))
}

/// Build a response map for a successful inline class definition in REPL.
/// `trailing_core_erlang` is Some when trailing expressions follow the class body.
/// `referenced_aliases` (the ADR 0108 hot-reload re-check trigger)
/// mirrors `compile_ok_response`'s field of the same
/// name — every alias name this class's own method-signature annotations
/// transitively depended on, so the Erlang side can register the same
/// `beamtalk_alias_xref` dependency edges a file-defining compile gets.
pub(crate) fn class_definition_ok_response(
    core_erlang: &str,
    module_name: &str,
    classes: &[(String, String)],
    trailing_core_erlang: Option<&str>,
    warnings: &[String],
    referenced_aliases: &[ecow::EcoString],
) -> Term {
    let mut map: std::collections::HashMap<Term, Term> = std::collections::HashMap::from([
        (atom("status"), atom("ok")),
        (atom("kind"), atom("class_definition")),
        (atom("core_erlang"), binary(core_erlang)),
        (atom("module_name"), binary(module_name)),
        (
            atom("classes"),
            Term::from(List::from(build_class_terms(classes))),
        ),
        (
            atom("warnings"),
            Term::from(List::from(build_warning_terms(warnings))),
        ),
        (
            atom("referenced_aliases"),
            Term::from(List::from(build_referenced_alias_terms(referenced_aliases))),
        ),
    ]);
    if let Some(trailing) = trailing_core_erlang {
        map.insert(atom("trailing_core_erlang"), binary(trailing));
    }
    Term::from(Map::from(map))
}

/// Compute the declared signature (return type + parameter types) of a method
/// definition for the ADR 0105 Phase 1 signature-generation store.
///
/// Uses `TypeAnnotation::type_name()` — the same canonical string rendering
/// the class hierarchy already uses for declared state/method types — so the
/// workspace-side store compares like-for-like against `__beamtalk_meta`
/// seeded signatures. An absent annotation (param or return) is reported as
/// the sentinel `"Dynamic"` rather than omitted, so the signature-diff always
/// has two comparable values.
///
/// **Scope note (Phase 0 finding, `docs/internal/adr-0105-phase0-spike-findings.md`
/// §1b):** only *declared* annotations are captured here, not re-inferred
/// return types. Reading declared annotations is the cheaper interim the spike
/// recommended; extending this to inferred (unannotated) return types is a
/// follow-up once a cheap way to thread `TypeMap` through the port response
/// exists.
pub(crate) fn method_signature_terms(
    method: &beamtalk_core::ast::MethodDefinition,
) -> (String, Vec<String>) {
    const DYNAMIC: &str = "Dynamic";
    let return_type = method
        .return_type
        .as_ref()
        .map_or_else(|| DYNAMIC.to_string(), |rt| rt.type_name().to_string());
    let param_types = method
        .parameters
        .iter()
        .map(|p| {
            p.type_annotation
                .as_ref()
                .map_or_else(|| DYNAMIC.to_string(), |ta| ta.type_name().to_string())
        })
        .collect();
    (return_type, param_types)
}

/// Build ETF term list from parameter type-name strings.
pub(crate) fn build_param_type_terms(param_types: &[String]) -> Term {
    Term::from(List::from(
        param_types.iter().map(|t| binary(t)).collect::<Vec<_>>(),
    ))
}

/// Build a response map for a successful standalone method definition in REPL.
#[allow(clippy::too_many_arguments)]
pub(crate) fn method_definition_ok_response(
    class_name: &str,
    selector: &str,
    is_class_method: bool,
    method_source: &str,
    return_type: &str,
    param_types: &[String],
    warnings: &[String],
) -> Term {
    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("kind"), atom("method_definition")),
        (atom("class_name"), binary(class_name)),
        (atom("selector"), binary(selector)),
        (
            atom("is_class_method"),
            if is_class_method {
                atom("true")
            } else {
                atom("false")
            },
        ),
        (atom("method_source"), binary(method_source)),
        // ADR 0105 Phase 1: the compiled method's declared signature,
        // carried so the workspace can capture it into the signature-generation
        // store before the patch installs (the pre-patch signature is otherwise
        // unrecoverable — see beamtalk_object_class.erl's put_method/4 clearing).
        (atom("return_type"), binary(return_type)),
        (atom("param_types"), build_param_type_terms(param_types)),
        (
            atom("warnings"),
            Term::from(List::from(build_warning_terms(warnings))),
        ),
    ]))
}

/// Build a response map for a successful protocol definition in REPL.
///
/// `referenced_aliases` (the ADR 0108 hot-reload re-check trigger)
/// mirrors `compile_ok_response`'s field of the same
/// name — every alias name this protocol's own method-signature annotations
/// transitively depended on, so the Erlang side can register the same
/// `beamtalk_alias_xref` dependency edges a class-defining compile gets.
pub(crate) fn protocol_definition_ok_response(
    core_erlang: &str,
    module_name: &str,
    protocol_names: &[String],
    warnings: &[String],
    referenced_aliases: &[ecow::EcoString],
) -> Term {
    let protocol_terms: Vec<Term> = protocol_names.iter().map(|n| binary(n)).collect();
    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("kind"), atom("protocol_definition")),
        (atom("core_erlang"), binary(core_erlang)),
        (atom("module_name"), binary(module_name)),
        (atom("protocols"), Term::from(List::from(protocol_terms))),
        (
            atom("warnings"),
            Term::from(List::from(build_warning_terms(warnings))),
        ),
        (
            atom("referenced_aliases"),
            Term::from(List::from(build_referenced_alias_terms(referenced_aliases))),
        ),
    ]))
}

/// Build a response map for a successful `type Name = ...` declaration in
/// the REPL (ADR 0108 Phase 8).
///
/// No `core_erlang`/bytecode: an alias erases entirely at annotation
/// resolution and has no runtime representation to compile (ADR 0108
/// Semantics). `expansion` is the unparsed `TypeAnnotation` display form
/// (e.g. `#north | #south | #east | #west`) — both what `:help` shows and
/// what the REPL session resends verbatim as a `type Name = <expansion>`
/// line in `known_type_aliases` on later turns (see
/// `extract_known_type_aliases`). `doc_comment` is `None` when the
/// declaration had no `///` doc comment.
pub(crate) fn type_alias_definition_ok_response(
    alias_name: &str,
    expansion: &str,
    doc_comment: Option<&str>,
    warnings: &[String],
) -> Term {
    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("kind"), atom("type_alias_definition")),
        (atom("alias_name"), binary(alias_name)),
        (atom("expansion"), binary(expansion)),
        (
            atom("doc_comment"),
            doc_comment.map_or_else(|| atom("undefined"), binary),
        ),
        (
            atom("warnings"),
            Term::from(List::from(build_warning_terms(warnings))),
        ),
    ]))
}

/// Build a response map for a successful `compile` (file compilation).
pub(crate) fn compile_ok_response(
    core_erlang: &str,
    module_name: &str,
    classes: &[(String, String)],
    warnings: &[String],
    referenced_aliases: &[ecow::EcoString],
) -> Term {
    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("core_erlang"), binary(core_erlang)),
        (atom("module_name"), binary(module_name)),
        (
            atom("classes"),
            Term::from(List::from(build_class_terms(classes))),
        ),
        (
            atom("warnings"),
            Term::from(List::from(build_warning_terms(warnings))),
        ),
        (
            atom("referenced_aliases"),
            Term::from(List::from(build_referenced_alias_terms(referenced_aliases))),
        ),
    ]))
}

/// Build a response map for a successful `compile_method` (structured
/// single-method compile). Carries the compiled class module AND the method
/// metadata the workspace `ChangeLog` needs (canonical source, selector, side).
#[allow(clippy::too_many_arguments)]
pub(crate) fn compile_method_ok_response(
    core_erlang: &str,
    module_name: &str,
    classes: &[(String, String)],
    selector: &str,
    is_class_method: bool,
    method_source: &str,
    merged_class_source: &str,
    return_type: &str,
    param_types: &[String],
    warnings: &[String],
    referenced_aliases: &[ecow::EcoString],
) -> Term {
    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("kind"), atom("method_definition")),
        (atom("core_erlang"), binary(core_erlang)),
        (atom("module_name"), binary(module_name)),
        (
            atom("classes"),
            Term::from(List::from(build_class_terms(classes))),
        ),
        (atom("selector"), binary(selector)),
        (
            atom("is_class_method"),
            if is_class_method {
                atom("true")
            } else {
                atom("false")
            },
        ),
        (atom("method_source"), binary(method_source)),
        (atom("merged_class_source"), binary(merged_class_source)),
        // ADR 0105 Phase 1: see method_definition_ok_response's doc
        // comment for why this is carried (signature-generation store capture).
        (atom("return_type"), binary(return_type)),
        (atom("param_types"), build_param_type_terms(param_types)),
        (
            atom("warnings"),
            Term::from(List::from(build_warning_terms(warnings))),
        ),
        (
            atom("referenced_aliases"),
            Term::from(List::from(build_referenced_alias_terms(referenced_aliases))),
        ),
    ]))
}

/// Build a response map for a successful `diagnostics` query.
pub(crate) fn diagnostics_ok_response(
    diagnostics: &[DiagInfo],
    referenced_aliases: &[ecow::EcoString],
) -> Term {
    let diag_terms: Vec<Term> = diagnostics
        .iter()
        .map(|d| {
            Term::from(Map::from([
                (atom("message"), binary(&d.message)),
                (atom("severity"), binary(&d.severity)),
                (
                    atom("category"),
                    match &d.category {
                        Some(c) => binary(c),
                        None => atom("undefined"),
                    },
                ),
                (
                    atom("start"),
                    Term::from(eetf::FixInteger::from(
                        i32::try_from(d.start).unwrap_or(i32::MAX),
                    )),
                ),
                (
                    atom("end"),
                    Term::from(eetf::FixInteger::from(
                        i32::try_from(d.end).unwrap_or(i32::MAX),
                    )),
                ),
            ]))
        })
        .collect();
    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("diagnostics"), Term::from(List::from(diag_terms))),
        (
            atom("referenced_aliases"),
            Term::from(List::from(build_referenced_alias_terms(referenced_aliases))),
        ),
    ]))
}

/// Build a response map for a simple string error (protocol-level errors, not diagnostics).
pub(crate) fn error_response(diagnostics: &[String]) -> Term {
    let diag_terms: Vec<Term> = diagnostics
        .iter()
        .map(|d| Term::from(Map::from([(atom("message"), binary(d))])))
        .collect();
    Term::from(Map::from([
        (atom("status"), atom("error")),
        (atom("diagnostics"), Term::from(List::from(diag_terms))),
    ]))
}

/// Compute 1-based column number for a byte offset in source text.
///
/// Counts Unicode characters (not bytes) from the start of the line,
/// so columns are correct for multibyte UTF-8 source.
pub(crate) fn byte_offset_to_col(source: &str, offset: u32) -> u32 {
    let offset_clamped = (offset as usize).min(source.len());
    let line_start = source.as_bytes()[..offset_clamped]
        .iter()
        .rposition(|&b| b == b'\n')
        .map_or(0, |pos| pos + 1);

    // Count Unicode scalar values from line_start to offset_clamped
    let mut col = 1u32;
    for (rel_byte, _) in source[line_start..].char_indices() {
        if line_start + rel_byte >= offset_clamped {
            break;
        }
        col = col.saturating_add(1);
    }
    col
}

/// Format a `CodeGenError` with source-aware location info for MCP responses.
///
/// When the error carries a `Span`, formats the location as `"line N, col C"`
/// using the source text. Falls back to the default `Display` format otherwise.
pub(crate) fn format_codegen_error(
    e: &beamtalk_codegen::core_erlang::CodeGenError,
    source: &str,
) -> String {
    use beamtalk_codegen::core_erlang::CodeGenError;
    match e {
        CodeGenError::UnsupportedFeature {
            feature,
            span: Some(span),
        } => {
            let line = span.line_number(source);
            let col = byte_offset_to_col(source, span.start());
            format!(
                "Code generation failed: unsupported feature: {feature} at line {line}, col {col}"
            )
        }
        _ => format!("Code generation failed: {e}"),
    }
}

/// Build a response map for compile-time diagnostic errors.
/// Each diagnostic entry includes `message`, `line` (1-based), and optionally `hint`.
pub(crate) fn diagnostic_error_response(
    diagnostics: &[&beamtalk_core::source_analysis::Diagnostic],
    source: &str,
) -> Term {
    let diag_terms: Vec<Term> = diagnostics
        .iter()
        .map(|d| {
            let line = d.span.line_number(source);
            let line_term = Term::from(eetf::FixInteger::from(
                i32::try_from(line).unwrap_or(i32::MAX),
            ));
            let mut map: std::collections::HashMap<Term, Term> = std::collections::HashMap::from([
                (atom("message"), binary(d.message.as_ref())),
                (atom("line"), line_term),
            ]);
            if let Some(ref hint) = d.hint {
                map.insert(atom("hint"), binary(hint.as_ref()));
            }
            Term::from(Map::from(map))
        })
        .collect();
    Term::from(Map::from([
        (atom("status"), atom("error")),
        (atom("diagnostics"), Term::from(List::from(diag_terms))),
    ]))
}

/// Build a `compile_method` diagnostic response with method-relative line numbers
/// for errors inside the patched method body.
///
/// Diagnostics are computed on the re-parsed merged module, so every span indexes
/// into `merged_class_source` — one coordinate system, no fragile byte-length
/// routing. The Erlang layer renders these as `"Line N: <message>"`
/// (`beamtalk_repl_compiler:format_diagnostic_text/1`), so for the common case — a
/// method-body error (type error, undefined var) — `N` is reported relative to the
/// patched method, matching the snippet the user is editing rather than the line in
/// the whole class. A diagnostic whose span lands outside the patched method (a
/// rarer class-context error) keeps its merged-source line, which is accurate and
/// in-range.
pub(crate) fn compile_method_diagnostic_response(
    diagnostics: &[&beamtalk_core::source_analysis::Diagnostic],
    merged_class_source: &str,
    patched_method_span: Option<beamtalk_core::source_analysis::Span>,
) -> Term {
    let diag_terms: Vec<Term> = diagnostics
        .iter()
        .map(|d| {
            let abs_line = d.span.line_number(merged_class_source);
            let line = match patched_method_span {
                Some(ms) if ms.contains(d.span) => {
                    let method_start_line = ms.line_number(merged_class_source);
                    abs_line.saturating_sub(method_start_line).saturating_add(1)
                }
                _ => abs_line,
            };
            let line_term = Term::from(eetf::FixInteger::from(
                i32::try_from(line).unwrap_or(i32::MAX),
            ));
            let mut map: std::collections::HashMap<Term, Term> = std::collections::HashMap::from([
                (atom("message"), binary(d.message.as_ref())),
                (atom("line"), line_term),
            ]);
            if let Some(ref hint) = d.hint {
                map.insert(atom("hint"), binary(hint.as_ref()));
            }
            Term::from(Map::from(map))
        })
        .collect();
    Term::from(Map::from([
        (atom("status"), atom("error")),
        (atom("diagnostics"), Term::from(List::from(diag_terms))),
    ]))
}
