// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! OTP Port binary for the Beamtalk compiler (ADR 0022).
//!
//! **DDD Context:** Compilation (Anti-Corruption Layer boundary)
//!
//! Reads ETF-encoded requests from stdin ({packet, 4} framing),
//! calls beamtalk-core compile functions, and writes ETF-encoded
//! responses to stdout.
//!
//! Supports commands: `compile_expression`, `compile`, `diagnostics`, `version`.

use std::io;

use beamtalk_etf::{
    self as etf, atom, binary_from_str as binary, int_term, map_get, term_to_atom, term_to_bool,
    term_to_string, term_to_string_list, term_to_usize,
};
use clap::{ArgAction, Parser};
use tracing_subscriber::{self, EnvFilter};

use eetf::{List, Map, Term};

// ────────────────────────────────────────────────────────────────
// Domain-specific ETF helpers (not shared via beamtalk-etf)
//
// The compiler port's `term_to_string_map` needs richer error reporting
// than the generic `Option`-based version in beamtalk-etf, so it remains
// local.

/// Extract a string->string map from a Term with descriptive errors.
///
/// Returns `Err` if the term is present but contains non-string keys or values,
/// so callers can surface the problem rather than silently falling back.
fn term_to_string_map_checked(
    term: &Term,
    field_name: &str,
) -> Result<std::collections::HashMap<String, String>, String> {
    match term {
        Term::Map(m) => {
            let mut result = std::collections::HashMap::new();
            for (k, v) in &m.map {
                let key = term_to_string(k)
                    .ok_or_else(|| format!("{field_name} key is not a string: {k:?}"))?;
                let val = term_to_string(v).ok_or_else(|| {
                    format!("{field_name} value for '{key}' is not a string: {v:?}")
                })?;
                result.insert(key, val);
            }
            Ok(result)
        }
        _ => Err(format!(
            "{field_name} must be a map of string->string, got: {term:?}"
        )),
    }
}

/// Extract a list of atom strings from a Term.
fn term_to_atom_list(term: &Term) -> Vec<ecow::EcoString> {
    match term {
        Term::List(list) => list
            .elements
            .iter()
            .filter_map(term_to_atom)
            .map(|s| ecow::EcoString::from(s.as_str()))
            .collect(),
        _ => vec![],
    }
}

/// Extract an atom→atom map from a Term (for `field_types`).
fn term_to_atom_atom_map(
    term: &Term,
) -> std::collections::HashMap<ecow::EcoString, ecow::EcoString> {
    match term {
        Term::Map(m) => m
            .map
            .iter()
            .filter_map(|(k, v)| {
                let key = term_to_atom(k)?;
                let val = term_to_atom(v)?;
                Some((
                    ecow::EcoString::from(key.as_str()),
                    ecow::EcoString::from(val.as_str()),
                ))
            })
            .collect(),
        _ => std::collections::HashMap::new(),
    }
}

/// BT-1976: Extract an atom→bool map (for `field_has_default`).
fn term_to_atom_bool_map(term: &Term) -> std::collections::HashMap<ecow::EcoString, bool> {
    match term {
        Term::Map(m) => m
            .map
            .iter()
            .filter_map(|(k, v)| {
                let key = term_to_atom(k)?;
                let flag = term_to_bool(v)?;
                Some((ecow::EcoString::from(key.as_str()), flag))
            })
            .collect(),
        _ => std::collections::HashMap::new(),
    }
}

/// Parse method infos from a `method_info` or `class_method_info` ETF map.
///
/// Each entry: `selector_atom => #{arity => int, param_types => [atom...], return_type => atom}`.
/// Returns empty Vec on missing key or malformed data (graceful degradation).
fn parse_method_infos_from_map(
    m: &Map,
    key: &str,
    class_name: &str,
) -> Vec<beamtalk_core::semantic_analysis::class_hierarchy::MethodInfo> {
    use beamtalk_core::ast::MethodKind;
    use beamtalk_core::semantic_analysis::class_hierarchy::MethodInfo;

    let Some(Term::Map(method_map)) = map_get(m, key) else {
        return vec![];
    };
    method_map
        .map
        .iter()
        .filter_map(|(sel_term, info_term)| {
            let selector = term_to_atom(sel_term)?;
            let Term::Map(info_map) = info_term else {
                return None;
            };
            let arity = map_get(info_map, "arity").and_then(term_to_usize)?;
            let return_type = map_get(info_map, "return_type")
                .and_then(term_to_atom)
                .and_then(|s| {
                    if s == "none" {
                        None
                    } else {
                        Some(ecow::EcoString::from(s.as_str()))
                    }
                });
            let param_types: Vec<Option<ecow::EcoString>> = match map_get(info_map, "param_types") {
                Some(Term::List(list)) => list
                    .elements
                    .iter()
                    .map(|t| {
                        term_to_atom(t).and_then(|s| {
                            if s == "none" {
                                None
                            } else {
                                Some(ecow::EcoString::from(s.as_str()))
                            }
                        })
                    })
                    .collect(),
                _ => vec![],
            };
            Some(MethodInfo {
                selector: ecow::EcoString::from(selector.as_str()),
                arity,
                kind: MethodKind::Primary,
                defined_in: ecow::EcoString::from(class_name),
                is_sealed: map_get(info_map, "is_sealed")
                    .and_then(term_to_bool)
                    .unwrap_or(false),
                is_internal: map_get(info_map, "visibility")
                    .and_then(term_to_atom)
                    .is_some_and(|v| v == "internal"),
                spawns_block: false,
                return_type,
                param_types,
                doc: None,
            })
        })
        .collect()
}

/// Deserialize a single `__beamtalk_meta/0` ETF map into a `ClassInfo`.
///
/// Returns `None` if `term` is not a map. Degrades gracefully on missing keys
/// (old-format modules without `method_info` etc.).
fn parse_class_info_from_meta_term(
    class_name: &str,
    term: &Term,
) -> Option<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo> {
    use beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo;

    let Term::Map(m) = term else { return None };

    let superclass = map_get(m, "superclass")
        .and_then(term_to_atom)
        .and_then(|s| {
            if s == "none" {
                None
            } else {
                Some(ecow::EcoString::from(s.as_str()))
            }
        });

    let is_sealed = map_get(m, "is_sealed")
        .and_then(term_to_bool)
        .unwrap_or(false);
    let is_abstract = map_get(m, "is_abstract")
        .and_then(term_to_bool)
        .unwrap_or(false);
    let is_value = map_get(m, "is_value")
        .and_then(term_to_bool)
        .unwrap_or(false);
    let is_typed = map_get(m, "is_typed")
        .and_then(term_to_bool)
        .unwrap_or(false);
    let is_internal = map_get(m, "is_internal")
        .and_then(term_to_bool)
        .unwrap_or(false);

    let state = map_get(m, "fields")
        .map(term_to_atom_list)
        .unwrap_or_default();
    let state_types = map_get(m, "field_types")
        .map(term_to_atom_atom_map)
        .unwrap_or_default();
    // BT-1976: Read field_has_default map emitted by codegen. Missing key
    // (older BEAM artifacts) → empty map; AST-less cross-file validation
    // degrades gracefully (fields without entries are treated as "unknown",
    // which means the post-init check skips them as before).
    let state_has_default = map_get(m, "field_has_default")
        .map(term_to_atom_bool_map)
        .unwrap_or_default();
    let class_variables = map_get(m, "class_variables")
        .map(term_to_atom_list)
        .unwrap_or_default();

    let methods = parse_method_infos_from_map(m, "method_info", class_name);
    let class_methods = parse_method_infos_from_map(m, "class_method_info", class_name);

    // ADR 0071: Extract package from BEAM metadata (populated by codegen)
    let package = map_get(m, "package").and_then(term_to_atom).and_then(|s| {
        if s == "none" {
            None
        } else {
            Some(ecow::EcoString::from(s.as_str()))
        }
    });

    Some(ClassInfo {
        name: ecow::EcoString::from(class_name),
        superclass,
        is_sealed,
        is_abstract,
        is_internal,
        package,
        is_value,
        is_native: false, // BEAM cache doesn't carry native flag; re-derived at parse time
        // ADR 0103: handle scope is a declaration, not structurally re-derivable,
        // so read it back from the meta map — a binary-only dependency that
        // declares `handleScope:` must keep its tier across package boundaries.
        handle_scope: map_get(m, "handle_scope")
            .and_then(term_to_atom)
            .map(ecow::EcoString::from),
        // BEAM metadata comes from successfully-compiled modules, whose
        // surfaces are complete by construction (BT-2796).
        surface_incomplete: false,
        is_typed,
        state,
        state_types,
        state_has_default,
        methods,
        class_methods,
        class_variables,
        type_params: Vec::new(),
        type_param_bounds: Vec::new(),
        superclass_type_args: Vec::new(),
    })
}

/// Parse a `class_hierarchy` ETF term (`#{atom() => meta_map()}`) into `Vec<ClassInfo>`.
///
/// Skips stdlib builtins — the Rust `ClassHierarchy::with_builtins()` already has
/// richer data for them. Degrades gracefully on malformed entries (silently skipped).
fn parse_class_hierarchy_from_term(
    term: &Term,
) -> Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo> {
    let Term::Map(m) = term else { return vec![] };
    m.map
        .iter()
        .filter_map(|(name_term, meta_term)| {
            let class_name = term_to_atom(name_term)?;
            if beamtalk_core::semantic_analysis::ClassHierarchy::is_builtin_class(&class_name) {
                return None;
            }
            parse_class_info_from_meta_term(&class_name, meta_term)
        })
        .collect()
}

/// Merge a method into a method list, replacing any existing method with the
/// same selector and kind, else appending it.
///
/// The instance/class **side** is encoded entirely by *which* list the caller
/// passes (`class.methods` vs `class.class_methods`) — `handle_compile_method`
/// selects it from the `is_class_method` flag. `MethodKind` does *not* encode the
/// side (it only distinguishes `Primary` from future AOP advice kinds), so a
/// standalone-parsed body lands in the right side purely by list choice, and the
/// `kind` match is a within-list replace-or-add discriminator — not a side check.
/// (BT-2563 #3: there is therefore no class-side "kind trap" / duplicate-push.)
fn merge_method(
    methods: &mut Vec<beamtalk_core::ast::MethodDefinition>,
    method: beamtalk_core::ast::MethodDefinition,
) {
    let selector = method.selector.name();
    if let Some(existing) = methods
        .iter_mut()
        .find(|m| m.selector.name() == selector && m.kind == method.kind)
    {
        *existing = method;
    } else {
        methods.push(method);
    }
}

// ---------------------------------------------------------------------------
// Request field extraction helpers
// ---------------------------------------------------------------------------

/// Extract an optional `String→String` map from a request field.
///
/// Returns an empty map when the key is absent, and an error response Term
/// when the key is present but malformed.
fn extract_optional_string_map(
    request: &Map,
    key: &str,
) -> Result<std::collections::HashMap<String, String>, Term> {
    match map_get(request, key) {
        None => Ok(std::collections::HashMap::new()),
        Some(term) => term_to_string_map_checked(term, key).map_err(|e| error_response(&[e])),
    }
}

/// Extract an optional `class_hierarchy` field, returning `Vec<ClassInfo>`.
fn extract_class_hierarchy(
    request: &Map,
) -> Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo> {
    match map_get(request, "class_hierarchy") {
        None => vec![],
        Some(term) => parse_class_hierarchy_from_term(term),
    }
}

/// Extract an optional `known_type_aliases` field: a list of standalone
/// `type Name = <expansion>` source strings (ADR 0108 Phase 8, BT-2902),
/// re-parsing each into an `AliasInfo`.
///
/// Aliases erase to nothing at runtime, so — unlike `class_hierarchy`, which
/// the REPL session recovers from live BEAM class metadata every turn —
/// there is nothing to query on a later turn. The REPL layer is the source
/// of truth: it stores the exact `type Name = <expansion>` text this port
/// returned when the alias was first declared (see
/// `type_alias_definition_ok_response`'s `expansion` field) and resends the
/// full set on every subsequent `compile_expression` call. A malformed
/// entry (should not happen — the text round-trips through this port's own
/// unparse output) is skipped rather than failing the whole request, so a
/// corrupted session doesn't wedge the REPL.
fn extract_known_type_aliases(request: &Map) -> Vec<beamtalk_core::semantic_analysis::AliasInfo> {
    let Some(sources) = map_get(request, "known_type_aliases").and_then(term_to_string_list) else {
        return vec![];
    };
    sources
        .iter()
        .filter_map(|src| {
            let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
            let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
            module
                .type_aliases
                .first()
                .map(beamtalk_core::semantic_analysis::AliasInfo::from_definition)
        })
        .collect()
}

// ---------------------------------------------------------------------------
// Diagnostic filtering helpers
// ---------------------------------------------------------------------------

/// Collect error-severity diagnostics as references for `diagnostic_error_response`.
fn filter_error_diagnostics(
    diagnostics: &[beamtalk_core::source_analysis::Diagnostic],
) -> Vec<&beamtalk_core::source_analysis::Diagnostic> {
    diagnostics
        .iter()
        .filter(|d| matches!(d.severity, beamtalk_core::source_analysis::Severity::Error))
        .collect()
}

/// Collect warning/hint/lint messages as `Vec<String>` for response construction.
///
/// Includes `Lint` severity so REPL users see effect-free statement hints (BT-979).
fn collect_warning_messages(
    diagnostics: &[beamtalk_core::source_analysis::Diagnostic],
) -> Vec<String> {
    diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.severity,
                beamtalk_core::source_analysis::Severity::Warning
                    | beamtalk_core::source_analysis::Severity::Hint
                    | beamtalk_core::source_analysis::Severity::Lint
            )
        })
        .map(|d| d.message.to_string())
        .collect()
}

// ---------------------------------------------------------------------------
// Response term construction helpers
// ---------------------------------------------------------------------------

/// Build ETF warning terms from string messages.
fn build_warning_terms(warnings: &[String]) -> Vec<Term> {
    warnings.iter().map(|w| binary(w)).collect()
}

/// Build ETF class terms from `(name, superclass)` pairs.
fn build_class_terms(classes: &[(String, String)]) -> Vec<Term> {
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

/// Build ETF terms for the `referenced_aliases` response field (ADR 0108
/// hot-reload re-check trigger, BT-2899) — every alias name this compile's
/// annotations transitively depended on
/// (`AnalysisResult::referenced_aliases`, already sorted/deduplicated
/// there), as a plain list of binaries. The Erlang side
/// (`beamtalk_repl_loader`) feeds this into `beamtalk_alias_xref:
/// register_class/2` at class-install time.
fn build_referenced_alias_terms(referenced_aliases: &[ecow::EcoString]) -> Vec<Term> {
    referenced_aliases.iter().map(|n| binary(n)).collect()
}

/// Build a response map for a successful `compile_expression`.
fn ok_response(core_erlang: &str, warnings: &[String]) -> Term {
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
/// BT-885: `trailing_core_erlang` is Some when trailing expressions follow the class body.
/// `referenced_aliases` (ADR 0108 hot-reload re-check trigger, BT-2899 /
/// BT-2952 follow-up) mirrors `compile_ok_response`'s field of the same
/// name — every alias name this class's own method-signature annotations
/// transitively depended on, so the Erlang side can register the same
/// `beamtalk_alias_xref` dependency edges a file-defining compile gets.
fn class_definition_ok_response(
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
/// definition for the ADR 0105 Phase 1 signature-generation store (BT-2777).
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
fn method_signature_terms(method: &beamtalk_core::ast::MethodDefinition) -> (String, Vec<String>) {
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
fn build_param_type_terms(param_types: &[String]) -> Term {
    Term::from(List::from(
        param_types.iter().map(|t| binary(t)).collect::<Vec<_>>(),
    ))
}

/// Build a response map for a successful standalone method definition in REPL.
#[allow(clippy::too_many_arguments)]
fn method_definition_ok_response(
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
        // ADR 0105 Phase 1 (BT-2777): the compiled method's declared signature,
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

/// Build a response map for a successful protocol definition in REPL (BT-1612).
///
/// `referenced_aliases` (ADR 0108 hot-reload re-check trigger, BT-2899 /
/// BT-2917 follow-up) mirrors `compile_ok_response`'s field of the same
/// name — every alias name this protocol's own method-signature annotations
/// transitively depended on, so the Erlang side can register the same
/// `beamtalk_alias_xref` dependency edges a class-defining compile gets.
fn protocol_definition_ok_response(
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
/// the REPL (ADR 0108 Phase 8, BT-2902).
///
/// No `core_erlang`/bytecode: an alias erases entirely at annotation
/// resolution and has no runtime representation to compile (ADR 0108
/// Semantics). `expansion` is the unparsed `TypeAnnotation` display form
/// (e.g. `#north | #south | #east | #west`) — both what `:help` shows and
/// what the REPL session resends verbatim as a `type Name = <expansion>`
/// line in `known_type_aliases` on later turns (see
/// `extract_known_type_aliases`). `doc_comment` is `None` when the
/// declaration had no `///` doc comment.
fn type_alias_definition_ok_response(
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
fn compile_ok_response(
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
fn compile_method_ok_response(
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
        // ADR 0105 Phase 1 (BT-2777): see method_definition_ok_response's doc
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
fn diagnostics_ok_response(
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
fn error_response(diagnostics: &[String]) -> Term {
    let diag_terms: Vec<Term> = diagnostics
        .iter()
        .map(|d| Term::from(Map::from([(atom("message"), binary(d))])))
        .collect();
    Term::from(Map::from([
        (atom("status"), atom("error")),
        (atom("diagnostics"), Term::from(List::from(diag_terms))),
    ]))
}

/// Compute 1-based line number for a byte offset in source text.
/// Uses byte scanning (`\n` is always 0x0A in UTF-8) so there is no panic
/// risk from non-char-boundary offsets.
fn byte_offset_to_line(source: &str, offset: u32) -> u32 {
    let offset_clamped = (offset as usize).min(source.len());
    let mut newlines: u32 = 0;
    for &b in &source.as_bytes()[..offset_clamped] {
        if b == b'\n' {
            newlines = newlines.saturating_add(1);
        }
    }
    newlines.saturating_add(1)
}

/// Compute 1-based column number for a byte offset in source text.
///
/// Counts Unicode characters (not bytes) from the start of the line,
/// so columns are correct for multibyte UTF-8 source.
fn byte_offset_to_col(source: &str, offset: u32) -> u32 {
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
fn format_codegen_error(e: &beamtalk_core::erlang::CodeGenError, source: &str) -> String {
    use beamtalk_core::erlang::CodeGenError;
    match e {
        CodeGenError::UnsupportedFeature {
            feature,
            span: Some(span),
        } => {
            let line = byte_offset_to_line(source, span.start());
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
fn diagnostic_error_response(
    diagnostics: &[&beamtalk_core::source_analysis::Diagnostic],
    source: &str,
) -> Term {
    let diag_terms: Vec<Term> = diagnostics
        .iter()
        .map(|d| {
            let line = byte_offset_to_line(source, d.span.start());
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
/// in-range (BT-2563 #2).
fn compile_method_diagnostic_response(
    diagnostics: &[&beamtalk_core::source_analysis::Diagnostic],
    merged_class_source: &str,
    patched_method_span: Option<beamtalk_core::source_analysis::Span>,
) -> Term {
    let diag_terms: Vec<Term> = diagnostics
        .iter()
        .map(|d| {
            let abs_line = byte_offset_to_line(merged_class_source, d.span.start());
            let line = match patched_method_span {
                Some(ms) if ms.contains(d.span) => {
                    let method_start_line = byte_offset_to_line(merged_class_source, ms.start());
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

/// Structured diagnostic info returned in compilation responses.
struct DiagInfo {
    /// Human-readable diagnostic message.
    message: String,
    /// Severity level (`"error"`, `"warning"`, `"lint"`, or `"hint"`).
    severity: String,
    /// Diagnostic category (`"Dnu"`, `"Type"`, ...), when the checker tagged
    /// one — `None` for parse errors and other untagged diagnostics
    /// (ADR 0105 Phase 1, BT-2778: the re-check orchestration filters
    /// findings by category).
    category: Option<String>,
    /// Byte offset where the diagnosed span begins.
    start: u32,
    /// Byte offset where the diagnosed span ends.
    end: u32,
}

/// Render a `Diagnostic`'s category as the same `PascalCase` label
/// `beamtalk lint` / `beamtalk-mcp` use (`category_name`), or `None` when
/// the diagnostic carries no category.
fn diag_category(d: &beamtalk_core::source_analysis::Diagnostic) -> Option<String> {
    d.category
        .map(|c| beamtalk_core::source_analysis::category_name(c).to_string())
}

/// Separate diagnostics into errors and warnings, returning structured info.
fn partition_diagnostics(
    diagnostics: &[beamtalk_core::source_analysis::Diagnostic],
) -> (Vec<DiagInfo>, Vec<DiagInfo>) {
    let errors = diagnostics
        .iter()
        .filter(|d| matches!(d.severity, beamtalk_core::source_analysis::Severity::Error))
        .map(|d| DiagInfo {
            message: d.message.to_string(),
            severity: "error".to_string(),
            category: diag_category(d),
            start: d.span.start(),
            end: d.span.end(),
        })
        .collect();
    let warnings = diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.severity,
                beamtalk_core::source_analysis::Severity::Warning
                    | beamtalk_core::source_analysis::Severity::Hint
            )
        })
        .map(|d| DiagInfo {
            message: d.message.to_string(),
            severity: match d.severity {
                beamtalk_core::source_analysis::Severity::Hint => "hint".to_string(),
                _ => "warning".to_string(),
            },
            category: diag_category(d),
            start: d.span.start(),
            end: d.span.end(),
        })
        .collect();
    (errors, warnings)
}

/// Process-wide cache for the package's `beamtalk.toml` `[diagnostics]`
/// severity-override table (ADR 0100 Rule 3, BT-2839).
///
/// The compiler port is a long-lived process spawned once per BEAM node
/// session (interactive REPL, `beamtalk run`, or a connected/LiveView
/// session attached to either) with its working directory set to the
/// project root — every node-startup path pins its own cwd there (e.g.
/// `crates/beamtalk-cli/src/commands/repl/process.rs`,
/// `crates/beamtalk-cli/src/commands/run.rs`) — and this port process
/// inherits it at spawn time, unaffected by any later `file:set_cwd/1` on
/// the Erlang side. Reading and parsing `beamtalk.toml`
/// on every `compile_expression`/`compile`/`diagnostics` request would repeat
/// disk I/O on a hot path (`diagnostics` in particular fires on a ~150ms
/// idle-debounce as the user types); caching once per process — mirroring the
/// LSP's "load once at startup" (`Backend::load_diagnostics_table`, BT-2800)
/// — avoids that while keeping the same lenient, no-manifest-is-a-no-op
/// semantics.
static DIAGNOSTICS_OVERRIDES: std::sync::OnceLock<beamtalk_core::compilation::DiagnosticsTable> =
    std::sync::OnceLock::new();

/// Returns the process-wide `[diagnostics]` table, loading it from the
/// current working directory's `beamtalk.toml` on first use.
fn diagnostics_overrides() -> &'static beamtalk_core::compilation::DiagnosticsTable {
    DIAGNOSTICS_OVERRIDES.get_or_init(|| {
        let cwd = std::env::current_dir().unwrap_or_default();
        load_diagnostics_overrides_from(&cwd)
    })
}

/// Load the `[diagnostics]` severity-override table from `<root>/beamtalk.toml`
/// (ADR 0100 Rule 3).
///
/// Lenient by design, mirroring the LSP's `load_diagnostics_table`: a root
/// with no `beamtalk.toml`, or one that fails to parse, yields an empty table
/// (Rule 1 defaults) rather than blocking diagnostics — a malformed manifest
/// already fails loudly at `beamtalk build` time, and the REPL must keep
/// evaluating regardless. Parse failures are logged so the mismatch is
/// discoverable. Pure function of `root` (no global state) so it is directly
/// unit-testable without touching the process's real working directory.
fn load_diagnostics_overrides_from(
    root: &std::path::Path,
) -> beamtalk_core::compilation::DiagnosticsTable {
    let manifest_path = root.join("beamtalk.toml");
    let Ok(content) = std::fs::read_to_string(&manifest_path) else {
        return beamtalk_core::compilation::DiagnosticsTable::new();
    };
    match beamtalk_core::compilation::parse_diagnostics_table_from_manifest_toml(&content) {
        Ok(table) => {
            if !table.is_empty() {
                tracing::debug!(
                    count = table.len(),
                    "Loaded [diagnostics] severity override(s) from beamtalk.toml"
                );
            }
            table
        }
        Err(e) => {
            tracing::warn!(
                path = %manifest_path.display(),
                error = %e,
                "failed to parse [diagnostics] table in beamtalk.toml; using Rule 1 defaults"
            );
            beamtalk_core::compilation::DiagnosticsTable::new()
        }
    }
}

/// Process-wide cache of the project's Erlang FFI type signatures (ADR 0075,
/// BT-2891), loaded once from `<root>/_build/type_cache/` — the same on-disk
/// cache `beamtalk build`/`beamtalk lint` write and read (see
/// `beamtalk_core::ffi_type_specs`).
///
/// Mirrors [`DIAGNOSTICS_OVERRIDES`]: loaded once per process rather than per
/// request, using the project-root cwd every node-startup path already pins
/// (see that static's doc comment). Reads the on-disk cache only — never
/// live-extracts from `.beam` files (unlike the LSP's `load_type_cache`,
/// which may spawn a `beamtalk_build_worker` BEAM node) — because
/// `resolve_completion_type` sits on the REPL's tight completion-latency
/// budget (ADR 0045) and a project that has never run `beamtalk build` should
/// stay registry-blind rather than block a keystroke on a build-worker spawn.
static NATIVE_TYPE_REGISTRY: std::sync::OnceLock<
    beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry,
> = std::sync::OnceLock::new();

/// Returns the process-wide native type registry, loading it from the
/// current working directory's `_build/type_cache/` on first use.
fn native_type_registry()
-> &'static beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry {
    NATIVE_TYPE_REGISTRY.get_or_init(|| {
        let cwd = std::env::current_dir().unwrap_or_default();
        load_native_type_registry_from(&cwd)
    })
}

/// Load the Erlang FFI type registry from `<root>/_build/type_cache/`
/// (ADR 0075, BT-2891).
///
/// Lenient by design, mirroring [`load_diagnostics_overrides_from`]: a root
/// with no `_build/type_cache/` (project never built) yields an empty
/// registry rather than an error — `resolve_expression_type` already treats
/// `None`/empty identically (falls back to `Dynamic`), so this degrades to
/// exactly the pre-BT-2891 registry-blind behaviour. Pure function of `root`
/// so it is directly unit-testable without touching the process's real
/// working directory.
fn load_native_type_registry_from(
    root: &std::path::Path,
) -> beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry {
    use beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry;

    let Some(root) = camino::Utf8Path::from_path(root) else {
        tracing::debug!(
            root = %root.display(),
            "Project root is not valid UTF-8; native type registry stays empty"
        );
        return NativeTypeRegistry::new();
    };
    let cache_dir = root.join("_build").join("type_cache");
    if let Some(registry) = beamtalk_core::ffi_type_specs::load_type_cache_registry(&cache_dir) {
        tracing::debug!(
            modules = registry.module_count(),
            functions = registry.function_count(),
            "Loaded native type registry from _build/type_cache/"
        );
        registry
    } else {
        tracing::debug!(
            cache_dir = %cache_dir,
            "No _build/type_cache/ found; native type registry stays empty \
             (FFI expressions fall back to Dynamic until `beamtalk build` runs)"
        );
        NativeTypeRegistry::new()
    }
}

/// Parse a Beamtalk expression source and run full diagnostics with primitive validation.
///
/// Returns `Ok((module, warnings, referenced_aliases))` on success, or
/// `Err(response_term)` containing a formatted `diagnostic_error_response`
/// that the caller should return directly.
///
/// `pre_loaded_aliases` (ADR 0108 Phase 8, BT-2902) carries type aliases
/// declared in earlier turns of the same REPL session, re-parsed standalone
/// by [`extract_known_type_aliases`] — see that function's doc for why
/// aliases need their own re-parse path rather than `pre_class_hierarchy`'s
/// recover-from-live-BEAM-state mechanism.
///
/// BT-2952: uses `compute_diagnostics_and_referenced_aliases` (the same
/// analysis as `compute_diagnostics_with_known_vars_classes_and_aliases`,
/// additionally returning `AnalysisResult::referenced_aliases`) so the
/// REPL-inline `compile_expression` path computes the same alias-dependency
/// set `handle_compile`'s file-compile path already did — previously this
/// function discarded it, so `handle_inline_class_definition` never got a
/// real set to thread through and `handle_inline_protocol_definition` was
/// called with a hardcoded `&[]` (BT-2917's known limitation).
fn parse_and_check_expression(
    source: &str,
    known_vars: &[String],
    pre_class_hierarchy: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    pre_loaded_aliases: Vec<beamtalk_core::semantic_analysis::AliasInfo>,
) -> Result<
    (
        beamtalk_core::ast::Module,
        Vec<String>,
        Vec<ecow::EcoString>,
    ),
    Term,
> {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(source);
    let (module, parse_diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    let known_var_refs: Vec<&str> = known_vars.iter().map(String::as_str).collect();
    let (mut all_diagnostics, referenced_aliases) =
        beamtalk_core::queries::diagnostic_provider::compute_diagnostics_and_referenced_aliases(
            &module,
            parse_diagnostics,
            &known_var_refs,
            pre_class_hierarchy,
            pre_loaded_aliases,
            diagnostics_overrides(),
        );

    let options = beamtalk_core::CompilerOptions::default();
    let primitive_diags =
        beamtalk_core::semantic_analysis::primitive_validator::validate_primitives(
            &module, &options,
        );
    all_diagnostics.extend(primitive_diags);

    let error_diags = filter_error_diagnostics(&all_diagnostics);
    let warnings = collect_warning_messages(&all_diagnostics);

    if !error_diags.is_empty() {
        return Err(diagnostic_error_response(&error_diags, source));
    }

    Ok((module, warnings, referenced_aliases))
}

/// Handle a single `compile_expression` request.
fn handle_compile_expression(request: &Map) -> Term {
    // Extract required fields
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };

    let Some(module_name) = map_get(request, "module").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'module' field".to_string()]);
    };

    let known_vars = map_get(request, "known_vars")
        .and_then(term_to_string_list)
        .unwrap_or_default();

    let class_superclass_index =
        match extract_optional_string_map(request, "class_superclass_index") {
            Ok(map) => map,
            Err(resp) => return resp,
        };
    let class_module_index = match extract_optional_string_map(request, "class_module_index") {
        Ok(map) => map,
        Err(resp) => return resp,
    };
    let pre_class_hierarchy = extract_class_hierarchy(request);
    let pre_loaded_aliases = extract_known_type_aliases(request);

    let (module, warnings, referenced_aliases) = match parse_and_check_expression(
        &source,
        &known_vars,
        pre_class_hierarchy.clone(),
        pre_loaded_aliases.clone(),
    ) {
        Ok(r) => r,
        Err(resp) => return resp,
    };

    // BT-1670: Extract optional module_name override for inline class definitions
    // so they produce the same module name as file-based compilation in package mode.
    let module_name_override = map_get(request, "module_name").and_then(term_to_string);

    // BT-571: If the parsed module contains class definitions, use compile path
    if !module.classes.is_empty() {
        return handle_inline_class_definition(
            module,
            &source,
            &module_name,
            &warnings,
            &class_superclass_index,
            class_module_index,
            pre_class_hierarchy,
            pre_loaded_aliases,
            module_name_override.as_deref(),
            &referenced_aliases,
        );
    }

    // BT-571: If the parsed module contains standalone method definitions, return method info
    if !module.method_definitions.is_empty() {
        if module.method_definitions.len() > 1 {
            return error_response(&[
                "Multiple standalone method definitions in a single expression are not supported. \
                 Define each method separately, or use a class definition with inline methods."
                    .to_string(),
            ]);
        }
        let method_def = &module.method_definitions[0];
        let class_name = method_def.class_name.name.to_string();
        let selector = method_def.method.selector.name().to_string();
        // `method_source` must be the METHOD's source (`sel => body`), not the
        // full `Class >> sel => body` input — it is recorded verbatim in the
        // ChangeLog and written back on flush. Echoing the input would splice a
        // stray `Class >>` extension into the class body on flush (BT-2553
        // follow-up). `unparse_method` re-emits the parsed method, comments and
        // all, so the recorded source round-trips cleanly.
        let method_source = beamtalk_core::unparse::unparse_method(&method_def.method);
        let (return_type, param_types) = method_signature_terms(&method_def.method);
        return method_definition_ok_response(
            &class_name,
            &selector,
            method_def.is_class_method,
            &method_source,
            &return_type,
            &param_types,
            &warnings,
        );
    }

    // BT-1612: If the parsed module contains protocol definitions, compile and return them
    if !module.protocols.is_empty() {
        // BT-2952: `parse_and_check_expression` now computes
        // `referenced_aliases` for this REPL-expression path too (mirroring
        // `handle_compile`'s file-compile path below), closing the gap
        // `handle_inline_protocol_definition`'s doc comment used to describe
        // (BT-2917 shipped the protocol-side wiring but left this call site
        // passing a hardcoded `&[]`, since the Rust side didn't compute a
        // real set yet).
        return handle_inline_protocol_definition(
            &module,
            &source,
            &warnings,
            &class_superclass_index,
            class_module_index,
            pre_class_hierarchy,
            pre_loaded_aliases,
            module_name_override.as_deref(),
            false, // REPL expressions are never stdlib
            &referenced_aliases,
        );
    }

    // ADR 0108 Phase 8 (BT-2902): If the parsed module contains a `type
    // Name = ...` declaration, return alias metadata for the REPL session
    // to register — no Core Erlang / bytecode step, since aliases erase
    // entirely at resolution time (ADR 0108 Semantics) and have no runtime
    // representation to compile.
    if !module.type_aliases.is_empty() {
        return handle_inline_type_alias_definition(&module, &warnings);
    }

    if module.expressions.is_empty() {
        return error_response(&["No expressions to compile".to_string()]);
    }

    // BT-780: Generate Core Erlang for all expressions (multi-statement support)
    let expressions: Vec<_> = module
        .expressions
        .iter()
        .map(|s| s.expression.clone())
        .collect();
    match beamtalk_core::erlang::generate_repl_expressions_with_index(
        &expressions,
        &module_name,
        class_module_index,
    ) {
        Ok(code) => ok_response(&code, &warnings),
        Err(e) => error_response(&[format_codegen_error(&e, &source)]),
    }
}

/// Handle a `compile_expression_trace` request (BT-1238).
///
/// Same parsing/validation as `compile_expression` but generates a trace module
/// whose `eval/1` returns `{[{<<"src0">>, V0}, ...], FinalState}` instead of
/// `{Result, FinalState}`.
///
/// Returns the same `ok_response` format as `compile_expression` — the difference
/// is in the generated module semantics, not the port protocol.
fn handle_compile_expression_trace(request: &Map) -> Term {
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };
    let Some(module_name) = map_get(request, "module").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'module' field".to_string()]);
    };
    let known_vars = map_get(request, "known_vars")
        .and_then(term_to_string_list)
        .unwrap_or_default();
    let class_module_index = match extract_optional_string_map(request, "class_module_index") {
        Ok(map) => map,
        Err(resp) => return resp,
    };
    let pre_class_hierarchy = extract_class_hierarchy(request);
    let pre_loaded_aliases = extract_known_type_aliases(request);

    // Trace mode never defines classes/protocols/aliases (rejected below), so
    // the `referenced_aliases` this also now computes (BT-2952) has no
    // consumer here — trace-mode expressions can't reference an alias in a
    // position that needs xref registration.
    let (module, warnings, _referenced_aliases) = match parse_and_check_expression(
        &source,
        &known_vars,
        pre_class_hierarchy,
        pre_loaded_aliases,
    ) {
        Ok(r) => r,
        Err(resp) => return resp,
    };

    if !module.classes.is_empty()
        || !module.method_definitions.is_empty()
        || !module.protocols.is_empty()
        || !module.type_aliases.is_empty()
    {
        return error_response(&[
            "trace mode does not support class, method, protocol, or type alias \
             definitions; use eval without trace to define them"
                .to_string(),
        ]);
    }

    // BT-1612: Protocol definitions are not supported in trace mode.
    if !module.protocols.is_empty() {
        return error_response(&["trace mode does not support protocol definitions; \
             use eval without trace to define protocols"
            .to_string()]);
    }

    if module.expressions.is_empty() {
        return error_response(&["No expressions to compile".to_string()]);
    }

    let expressions: Vec<_> = module
        .expressions
        .iter()
        .map(|s| s.expression.clone())
        .collect();
    match beamtalk_core::erlang::generate_repl_expressions_traced(
        &expressions,
        &source,
        &module_name,
        class_module_index,
    ) {
        Ok(code) => ok_response(&code, &warnings),
        Err(e) => error_response(&[format_codegen_error(&e, &source)]),
    }
}

/// BT-1670: Derive a BEAM module name for a class, using either an explicit
/// override (from package-mode callers) or the default `bt@{snake_case}`
/// convention.  All code paths that produce module names for `.bt` classes
/// should call this function so the derivation logic is unified.
fn derive_class_module_name(
    class_name: &str,
    module_name_override: Option<&str>,
    stdlib_mode: bool,
) -> String {
    if let Some(name) = module_name_override {
        return name.to_string();
    }
    let base_name = beamtalk_core::erlang::to_module_name(class_name);
    if stdlib_mode {
        format!("bt@stdlib@{base_name}")
    } else {
        format!("bt@{base_name}")
    }
}

/// BT-571: Handle inline class definition in REPL expression context.
/// Merges any standalone method definitions into the class, generates code,
/// and returns a `class_definition` response.
/// BT-885: Also compiles any trailing expressions and includes them in the response.
/// BT-907: Accepts `class_superclass_index` to resolve cross-file inheritance chains.
/// Accepts `class_module_index` for package-qualified class references in trailing
/// expressions and the class body itself.
/// BT-1670: Accepts optional `module_name_override` so package-qualified names
/// are used consistently across all load paths.
/// BT-2952: Accepts `referenced_aliases` — the caller's already-computed
/// alias-dependency set (`parse_and_check_expression`'s
/// `compute_diagnostics_and_referenced_aliases` result), threaded into the
/// response so the Erlang side can register the same `beamtalk_alias_xref`
/// dependency edges a file-defining compile gets. Mirrors
/// `handle_inline_protocol_definition`'s identical parameter.
#[allow(clippy::too_many_arguments)]
fn handle_inline_class_definition(
    module: beamtalk_core::ast::Module,
    source: &str,
    expr_module_name: &str,
    warnings: &[String],
    class_superclass_index: &std::collections::HashMap<String, String>,
    class_module_index: std::collections::HashMap<String, String>,
    pre_class_hierarchy: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    pre_loaded_aliases: Vec<beamtalk_core::semantic_analysis::AliasInfo>,
    module_name_override: Option<&str>,
    referenced_aliases: &[ecow::EcoString],
) -> Term {
    let mut module = module;
    let mut warnings = warnings.to_vec();
    if !module.method_definitions.is_empty() {
        let method_defs = std::mem::take(&mut module.method_definitions);
        for method_def in method_defs {
            let target_class = method_def.class_name.name.as_str();
            if let Some(class) = module
                .classes
                .iter_mut()
                .find(|c| c.name.name == target_class)
            {
                let methods = if method_def.is_class_method {
                    &mut class.class_methods
                } else {
                    &mut class.methods
                };
                merge_method(methods, method_def.method);
            } else {
                warnings.push(format!(
                    "Standalone method targets unknown class `{target_class}` in this module"
                ));
            }
        }
    }

    // BT-1670: Use unified module name derivation so inline class definitions
    // in package mode produce the same module name as file-based compilation.
    let class_module_name =
        derive_class_module_name(&module.classes[0].name.name, module_name_override, false);

    let classes: Vec<(String, String)> = module
        .classes
        .iter()
        .map(|c| (c.name.name.to_string(), c.superclass_name().to_string()))
        .collect();

    // BT-885: Compile trailing expressions (after class body) so the Erlang side
    // can evaluate them and return their result instead of the class name.
    let trailing_core_erlang = if module.expressions.is_empty() {
        None
    } else {
        let trailing_exprs: Vec<_> = module
            .expressions
            .iter()
            .map(|s| s.expression.clone())
            .collect();
        match beamtalk_core::erlang::generate_repl_expressions_with_index(
            &trailing_exprs,
            expr_module_name,
            class_module_index.clone(),
        ) {
            Ok(code) => Some(code),
            Err(e) => {
                return error_response(&[format_codegen_error(&e, source)]);
            }
        }
    };

    match beamtalk_core::erlang::generate_module(
        &module,
        beamtalk_core::erlang::CodegenOptions::new(&class_module_name)
            .with_workspace_mode(true)
            .with_source(source)
            .with_class_superclass_index(class_superclass_index.clone())
            .with_class_module_index(class_module_index)
            .with_class_hierarchy(pre_class_hierarchy)
            .with_pre_loaded_aliases(pre_loaded_aliases),
    ) {
        Ok(code) => class_definition_ok_response(
            &code,
            &class_module_name,
            &classes,
            trailing_core_erlang.as_deref(),
            &warnings,
            referenced_aliases,
        ),
        Err(e) => error_response(&[format_codegen_error(&e, source)]),
    }
}

/// Handle protocol definitions in the REPL (BT-1612).
///
/// Compiles protocol-only modules to Core Erlang (which generates `register_class/0`
/// with protocol registration via BT-1610), then returns a `protocol_definition`
/// response so the Erlang side can load and execute the module.
/// BT-1670: Accepts optional `module_name_override` for package-mode consistency.
/// BT-2917: Accepts `referenced_aliases` — the caller's already-computed
/// alias-dependency set, threaded straight into the response so the Erlang
/// side can register the same `beamtalk_alias_xref` dependency edges a
/// class-defining compile gets. BT-2952: both callers (the REPL
/// `compile_expression` path and `handle_compile`'s file-compile path) now
/// pass a genuinely-computed set — `compile_expression`'s used to pass a
/// hardcoded `&[]` since the Rust side didn't compute one for that path yet.
/// BT-2941: Accepts `pre_loaded_aliases` — mirrors the `.with_pre_loaded_aliases(...)`
/// wiring BT-2932 applied to the other three `CodegenOptions` call sites in this
/// file (`handle_inline_class_definition`, `handle_compile`'s class path,
/// `handle_compile_method`) — so a protocol method signature referencing a
/// cross-module alias resolves to a `user_type` reference in the generated
/// `-type` attributes instead of silently dropping the alias (empty registry).
#[allow(clippy::too_many_arguments)]
fn handle_inline_protocol_definition(
    module: &beamtalk_core::ast::Module,
    source: &str,
    warnings: &[String],
    class_superclass_index: &std::collections::HashMap<String, String>,
    class_module_index: std::collections::HashMap<String, String>,
    pre_class_hierarchy: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    pre_loaded_aliases: Vec<beamtalk_core::semantic_analysis::AliasInfo>,
    module_name_override: Option<&str>,
    stdlib_mode: bool,
    referenced_aliases: &[ecow::EcoString],
) -> Term {
    let first_protocol_name = &module.protocols[0].name.name;
    let protocol_module_name =
        derive_class_module_name(first_protocol_name, module_name_override, stdlib_mode);

    let protocol_names: Vec<String> = module
        .protocols
        .iter()
        .map(|p| p.name.name.to_string())
        .collect();

    match beamtalk_core::erlang::generate_module(
        module,
        beamtalk_core::erlang::CodegenOptions::new(&protocol_module_name)
            .with_workspace_mode(true)
            .with_source(source)
            .with_class_superclass_index(class_superclass_index.clone())
            .with_class_module_index(class_module_index)
            .with_class_hierarchy(pre_class_hierarchy)
            .with_pre_loaded_aliases(pre_loaded_aliases),
    ) {
        Ok(code) => protocol_definition_ok_response(
            &code,
            &protocol_module_name,
            &protocol_names,
            warnings,
            referenced_aliases,
        ),
        Err(e) => error_response(&[format_codegen_error(&e, source)]),
    }
}

/// Handle a single `type Name = ...` declaration typed directly at the REPL
/// (ADR 0108 Phase 8, BT-2902).
///
/// By the time this is called, `parse_and_check_expression` has already run
/// full semantic analysis (`AliasRegistry::register_module` — namespace
/// collision, duplicate, and unbound-type-variable checks) and returned no
/// error diagnostics, so the declaration is already known-valid; this just
/// shapes the response. Mirrors the standalone-method-definition precedent
/// of requiring exactly one declaration per turn — batch multi-alias
/// resolution (topological ordering across several declarations at once) is
/// BT-2896's file-compile concern, not a single REPL turn's.
fn handle_inline_type_alias_definition(
    module: &beamtalk_core::ast::Module,
    warnings: &[String],
) -> Term {
    if module.type_aliases.len() > 1 {
        return error_response(&[
            "Multiple type alias definitions in a single expression are not supported. \
             Define each `type Name = ...` alias separately."
                .to_string(),
        ]);
    }
    if !module.expressions.is_empty() {
        return error_response(&[
            "A type alias declaration cannot be combined with other expressions in the \
             same input; declare it on its own, then use it in a later expression."
                .to_string(),
        ]);
    }

    let alias = &module.type_aliases[0];
    let expansion = beamtalk_core::unparse::unparse_type_annotation_display(&alias.annotation);
    type_alias_definition_ok_response(
        &alias.name.name,
        &expansion,
        alias.doc_comment.as_deref(),
        warnings,
    )
}

/// Handle a `compile` request (file/class compilation).
#[allow(clippy::too_many_lines)]
fn handle_compile(request: &Map) -> Term {
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };

    let stdlib_mode = map_get(request, "stdlib_mode")
        .and_then(term_to_bool)
        .unwrap_or(false);

    let workspace_mode = map_get(request, "workspace_mode")
        .and_then(term_to_bool)
        .unwrap_or(true);

    let pre_class_hierarchy = extract_class_hierarchy(request);
    // BT-2899 (ADR 0108): a class/protocol-defining compile needs session
    // carried-over type aliases too, not just `compile_expression` — see
    // `extract_known_type_aliases`'s doc. Without this, a live REPL
    // redefinition of a class/protocol over an earlier turn's `type Foo =
    // ...` never sees the alias at all, so `AliasRegistry::add_pre_loaded`'s
    // existing collision check (alias name vs. `hierarchy`/
    // `protocol_registry`) never gets a chance to run.
    let pre_loaded_aliases = extract_known_type_aliases(request);

    // Parse the source
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
    let (module, parse_diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    // Run semantic analysis. BT-2899: also capture `referenced_aliases` —
    // shipped back in the response so the Erlang side can populate
    // `beamtalk_alias_xref`'s alias-name → dependent-class index at class
    // install time (see `diagnostics_ok_response`/this handler's response
    // builder for where the field is attached).
    let (mut all_diagnostics, referenced_aliases) =
        beamtalk_core::queries::diagnostic_provider::compute_diagnostics_and_referenced_aliases(
            &module,
            parse_diagnostics,
            &[],
            pre_class_hierarchy.clone(),
            pre_loaded_aliases.clone(),
            diagnostics_overrides(),
        );

    // Run @primitive validation
    let options = beamtalk_core::CompilerOptions {
        stdlib_mode,
        allow_primitives: false,
        workspace_mode,
        suppress_warnings: false,
        ..Default::default()
    };
    let primitive_diags =
        beamtalk_core::semantic_analysis::primitive_validator::validate_primitives(
            &module, &options,
        );
    all_diagnostics.extend(primitive_diags);

    // BT-738: Warn when user code shadows a stdlib class name (not for stdlib itself).
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

    // BT-571: Merge standalone method definitions into their target classes
    let mut module = module;
    if !module.method_definitions.is_empty() {
        let method_defs = std::mem::take(&mut module.method_definitions);
        for method_def in method_defs {
            let target_class = method_def.class_name.name.as_str();
            if let Some(class) = module
                .classes
                .iter_mut()
                .find(|c| c.name.name == target_class)
            {
                let methods = if method_def.is_class_method {
                    &mut class.class_methods
                } else {
                    &mut class.methods
                };
                merge_method(methods, method_def.method);
            } else {
                warnings.push(DiagInfo {
                    message: format!(
                        "Standalone method targets unknown class `{target_class}` in this module"
                    ),
                    severity: "warning".to_string(),
                    category: None,
                    start: method_def.span.start(),
                    end: method_def.span.end(),
                });
            }
        }
    }

    // BT-775 / BT-1670: Accept optional module_name override from caller.
    // When provided, use it directly instead of deriving from the class name.
    // This allows the REPL/MCP load path to produce package-qualified names
    // matching the build system (e.g., bt@my_app@scheme@symbol).
    // Uses the unified derive_class_module_name function.
    let module_name_override = map_get(request, "module_name").and_then(term_to_string);

    // Derive module name from the sole top-level definition. BT-1666 enforces
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

    // BT-1950: Protocol-only files need the same early-return path as
    // handle_compile_expression (BT-1612). generate_module assumes at least
    // one class exists and errors with "Value type module has no class" for
    // protocol-only files. Route through the protocol codegen instead.
    if !module.protocols.is_empty() && module.classes.is_empty() {
        let warning_msgs: Vec<String> = warnings.iter().map(|w| w.message.clone()).collect();
        // BT-2917: `referenced_aliases` was already computed above (BT-2899)
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
        );
    }

    // Extract class info
    let classes: Vec<(String, String)> = module
        .classes
        .iter()
        .map(|c| (c.name.name.to_string(), c.superclass_name().to_string()))
        .collect();

    // BT-845/BT-860: Extract optional source file path to embed as beamtalk_source attribute.
    let source_path = map_get(request, "source_path").and_then(term_to_string);

    // Generate Core Erlang
    let warning_msgs: Vec<String> = warnings.iter().map(|w| w.message.clone()).collect();
    match beamtalk_core::erlang::generate_module(
        &module,
        beamtalk_core::erlang::CodegenOptions::new(&module_name)
            .with_workspace_mode(workspace_mode)
            .with_source(&source)
            .with_class_module_index(class_module_index)
            .with_class_superclass_index(class_superclass_index)
            .with_class_hierarchy(pre_class_hierarchy)
            .with_pre_loaded_aliases(pre_loaded_aliases)
            .with_source_path_opt(source_path.as_deref()),
    ) {
        Ok(code) => compile_ok_response(
            &code,
            &module_name,
            &classes,
            &warning_msgs,
            &referenced_aliases,
        ),
        Err(e) => error_response(&[format_codegen_error(&e, &source)]),
    }
}

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
fn handle_compile_method(request: &Map) -> Term {
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
    // BT-2899 (ADR 0108): see `handle_compile`'s equivalent comment — a
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
    // right BEAM line (BT-2563 #1) and semantic diagnostics resolve against a
    // single coherent source (BT-2563 #2) — no fragile per-diagnostic source
    // routing. `unparse_module` round-trips a valid module, so the re-parse is
    // clean; a non-empty diag list here means an unparser regression.
    let merged_tokens = beamtalk_core::source_analysis::lex_with_eof(&merged_class_source);
    let (merged_module, merged_parse_diags) = beamtalk_core::source_analysis::parse(merged_tokens);
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
    // (BT-2563 #2). Its span now indexes into `merged_class_source`, the same
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
    // ADR 0105 Phase 1 (BT-2777): declared signature of the patched method, read
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
    //    accurate and in-range (BT-2563 #2).
    let (mut all_diagnostics, referenced_aliases) =
        beamtalk_core::queries::diagnostic_provider::compute_diagnostics_and_referenced_aliases(
            &merged_module,
            merged_parse_diags,
            &[],
            pre_class_hierarchy.clone(),
            pre_loaded_aliases.clone(),
            diagnostics_overrides(),
        );
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

    match beamtalk_core::erlang::generate_module(
        &merged_module,
        beamtalk_core::erlang::CodegenOptions::new(&module_name)
            .with_workspace_mode(workspace_mode)
            .with_source(&merged_class_source)
            .with_class_module_index(class_module_index)
            .with_class_superclass_index(class_superclass_index)
            .with_class_hierarchy(pre_class_hierarchy)
            .with_pre_loaded_aliases(pre_loaded_aliases)
            .with_source_path_opt(source_path.as_deref()),
    ) {
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
fn handle_diagnostics(request: &Map) -> Term {
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
        // BT-2899 (ADR 0108): `known_type_aliases` (the same channel
        // `compile_expression`/`compile_method` accept) so a re-check
        // round trip (`beamtalk_recheck.erl`) resolves `::` annotations
        // against the *current* session alias table — without this, a
        // candidate class referencing a live-redefined alias would resolve
        // it as an unknown nominal class instead of picking up the
        // redefinition, defeating the whole point of re-checking it.
        let pre_loaded_aliases = extract_known_type_aliases(request);
        beamtalk_core::queries::diagnostic_provider::compute_diagnostics_and_referenced_aliases(
            &module,
            parse_diagnostics,
            &[],
            pre_class_hierarchy,
            pre_loaded_aliases,
            diagnostics_overrides(),
        )
    };

    let all_diags: Vec<DiagInfo> = diagnostics.iter().map(diag_info).collect();

    diagnostics_ok_response(&all_diags, &referenced_aliases)
}

/// Map a compiler `Diagnostic` to the wire `DiagInfo` (byte-offset span +
/// stringified severity) the cockpit's `@codemirror/lint` source consumes.
fn diag_info(d: &beamtalk_core::source_analysis::Diagnostic) -> DiagInfo {
    DiagInfo {
        message: d.message.to_string(),
        severity: match d.severity {
            beamtalk_core::source_analysis::Severity::Error => "error".to_string(),
            beamtalk_core::source_analysis::Severity::Warning => "warning".to_string(),
            beamtalk_core::source_analysis::Severity::Lint => "lint".to_string(),
            beamtalk_core::source_analysis::Severity::Hint => "hint".to_string(),
        },
        category: diag_category(d),
        start: d.span.start(),
        end: d.span.end(),
    }
}

/// Handle a `version` request.
fn handle_version() -> Term {
    let version = env!("BEAMTALK_VERSION");
    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("version"), binary(version)),
    ]))
}

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
fn handle_resolve_completion_type(request: &Map) -> Term {
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
fn resolve_completion_type_response(
    expression: &str,
    pre_class_hierarchy: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    native_type_registry: &beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry,
) -> Term {
    let mut hierarchy = beamtalk_core::semantic_analysis::ClassHierarchy::with_builtins();
    if !pre_class_hierarchy.is_empty() {
        hierarchy.add_from_beam_meta(pre_class_hierarchy);
    }

    match beamtalk_core::queries::completion_provider::resolve_expression_type(
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

/// Handle a `find_senders_in_source` request (BT-2190).
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
fn handle_find_senders_in_source(request: &Map) -> Term {
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };
    let Some(selector) = map_get(request, "selector").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'selector' field".to_string()]);
    };

    let lines = beamtalk_core::queries::senders_query::find_senders_in_source(&source, &selector);
    let line_terms: Vec<Term> = lines
        .iter()
        .map(|&line| int_term(i32::try_from(line).unwrap_or(i32::MAX)))
        .collect();

    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("lines"), Term::from(List::from(line_terms))),
    ]))
}

/// Handle a `find_all_sends_in_source` request (BT-2206).
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
/// (BT-2669); it is the empty binary (`<<>>`) for non-FFI sends and for FFI
/// chains whose module receiver is not a static `Erlang <module>` form. It is
/// returned as a binary (not an atom) so the response decodes safely with
/// `[safe]`; the caller interns it only when indexing. Returns an empty list
/// when the source has no sends or cannot be parsed.
fn handle_find_all_sends_in_source(request: &Map) -> Term {
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };

    let sends = beamtalk_core::queries::all_sends_query::find_all_sends_in_source(&source);
    let send_terms: Vec<Term> = sends
        .iter()
        .map(|hit| {
            let recv = match hit.receiver {
                beamtalk_core::queries::all_sends_query::ReceiverKind::SelfReceiver => atom("self"),
                beamtalk_core::queries::all_sends_query::ReceiverKind::SuperReceiver => {
                    atom("super")
                }
                beamtalk_core::queries::all_sends_query::ReceiverKind::ErlangFfi => {
                    atom("erlang_ffi")
                }
                beamtalk_core::queries::all_sends_query::ReceiverKind::Other => atom("other"),
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

/// Handle a `find_announce_sites_in_source` request (BT-2475).
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
fn handle_find_announce_sites_in_source(request: &Map) -> Term {
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };

    let sites =
        beamtalk_core::queries::announce_sites_query::find_announce_sites_in_source(&source);
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

/// Handle a `find_references_to_in_source` request (BT-2203).
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
fn handle_find_references_to_in_source(request: &Map) -> Term {
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };
    let Some(class_name) = map_get(request, "class_name").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'class_name' field".to_string()]);
    };

    let lines = beamtalk_core::queries::references_to_query::find_references_to_in_source(
        &source,
        &class_name,
    );
    let line_terms: Vec<Term> = lines
        .iter()
        .map(|&line| int_term(i32::try_from(line).unwrap_or(i32::MAX)))
        .collect();

    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("lines"), Term::from(List::from(line_terms))),
    ]))
}

/// Handle a `find_field_readers_in_source` request (BT-2208).
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
fn handle_find_field_readers_in_source(request: &Map) -> Term {
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };
    let Some(field) = map_get(request, "field").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'field' field".to_string()]);
    };

    let lines =
        beamtalk_core::queries::field_accesses_query::find_field_readers_in_source(&source, &field);
    field_lines_response(&lines)
}

/// Handle a `find_field_writers_in_source` request (BT-2208).
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
fn handle_find_field_writers_in_source(request: &Map) -> Term {
    let Some(source) = map_get(request, "source").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'source' field".to_string()]);
    };
    let Some(field) = map_get(request, "field").and_then(term_to_string) else {
        return error_response(&["Missing or invalid 'field' field".to_string()]);
    };

    let lines =
        beamtalk_core::queries::field_accesses_query::find_field_writers_in_source(&source, &field);
    field_lines_response(&lines)
}

/// Handle a `find_ffi_sites_in_source` request (BT-2211).
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
fn handle_find_ffi_sites_in_source(request: &Map) -> Term {
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

    let lines = beamtalk_core::queries::ffi_sites_query::find_ffi_sites_in_source(
        &source, &module, &function, arity,
    );
    field_lines_response(&lines)
}

/// Build the standard `#{status => ok, lines => [...]}` response shared by the
/// field reader/writer queries (BT-2208) and the FFI sites query (BT-2211).
fn field_lines_response(lines: &[u32]) -> Term {
    let line_terms: Vec<Term> = lines
        .iter()
        .map(|&line| int_term(i32::try_from(line).unwrap_or(i32::MAX)))
        .collect();

    Term::from(Map::from([
        (atom("status"), atom("ok")),
        (atom("lines"), Term::from(List::from(line_terms))),
    ]))
}

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
fn handle_resolve_method_span(request: &Map) -> Term {
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
fn handle_reindent_method_source(request: &Map) -> Term {
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

/// Build a structured error response for a [`SpanResolveError`].
///
/// The `reason` atom lets the Erlang hook branch without string-matching; the
/// `message` carries the human-readable detail for logging.
fn method_span_error_response(err: &beamtalk_core::source_analysis::SpanResolveError) -> Term {
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

/// Handle a single request and return a response Term.
fn handle_request(request_term: &Term) -> Term {
    let Term::Map(map) = request_term else {
        return error_response(&["Request must be a map".to_string()]);
    };

    // Extract command atom
    let command = match map_get(map, "command") {
        Some(Term::Atom(a)) => a.name.as_str(),
        _ => return error_response(&["Missing or invalid 'command' field".to_string()]),
    };

    match command {
        "compile_expression" => handle_compile_expression(map),
        "compile_expression_trace" => handle_compile_expression_trace(map),
        "compile" => handle_compile(map),
        "compile_method" => handle_compile_method(map),
        "diagnostics" => handle_diagnostics(map),
        "version" => handle_version(),
        "resolve_completion_type" => handle_resolve_completion_type(map),
        "find_senders_in_source" => handle_find_senders_in_source(map),
        "find_all_sends_in_source" => handle_find_all_sends_in_source(map),
        "find_references_to_in_source" => handle_find_references_to_in_source(map),
        "find_field_readers_in_source" => handle_find_field_readers_in_source(map),
        "find_field_writers_in_source" => handle_find_field_writers_in_source(map),
        "find_ffi_sites_in_source" => handle_find_ffi_sites_in_source(map),
        "find_announce_sites_in_source" => handle_find_announce_sites_in_source(map),
        "resolve_method_span" => handle_resolve_method_span(map),
        "reindent_method_source" => handle_reindent_method_source(map),
        _ => error_response(&[format!("Unknown command: {command}")]),
    }
}

#[derive(Debug, Parser)]
#[command(name = "beamtalk-compiler-port", about = "Beamtalk compiler port")]
struct Cli {
    /// Increase logging verbosity (-v: debug, -vv+: trace)
    #[arg(short, long, action = ArgAction::Count)]
    verbose: u8,
}

/// Match the CLI's 8 MB stack so deeply-nested source files don't overflow
/// the default Windows 1 MB thread stack.
const STACK_SIZE: usize = 8 * 1024 * 1024;

fn main() {
    // Spawn the real entry point on a thread with a larger stack.
    // On Windows the default is 1 MB, which overflows on non-trivial
    // Beamtalk source files. Linux defaults to 8 MB so it rarely hits
    // this, but the explicit size makes behaviour consistent everywhere.
    std::thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(run)
        .expect("failed to spawn main thread")
        .join()
        .expect("main thread panicked");
}

fn run() {
    let cli = Cli::parse();

    // Only initialize tracing when explicitly requested.
    // The compiler port is spawned by the Erlang runtime without args, so
    // default (verbose=0) must produce no stderr output to avoid interfering
    // with the OTP port protocol.
    let has_rust_log = std::env::var("RUST_LOG").is_ok();
    if has_rust_log || cli.verbose > 0 {
        let env_filter = if has_rust_log {
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("warn"))
        } else {
            EnvFilter::new(directive_for_verbosity(cli.verbose))
        };
        let _ = tracing_subscriber::fmt()
            .with_env_filter(env_filter)
            .with_writer(std::io::stderr)
            .with_ansi(false)
            .try_init();
    }

    let mut stdin = io::stdin().lock();
    let mut stdout = io::stdout().lock();

    loop {
        // Read next request
        let packet = match etf::read_packet(&mut stdin) {
            Ok(Some(data)) => data,
            Ok(None) => break, // EOF — port closed
            Err(e) => {
                eprintln!("Failed to read packet: {e}");
                break;
            }
        };

        // Decode ETF
        let term = match Term::decode(io::Cursor::new(&packet)) {
            Ok(t) => t,
            Err(e) => {
                // Send error response for decode failures
                let response = error_response(&[format!("ETF decode error: {e}")]);
                let mut buf = Vec::new();
                if response.encode(&mut buf).is_ok() {
                    let _ = etf::write_packet(&mut stdout, &buf);
                }
                continue;
            }
        };

        // Handle the request
        let response = handle_request(&term);

        // Encode and send response
        let mut buf = Vec::new();
        match response.encode(&mut buf) {
            Ok(()) => {
                if let Err(e) = etf::write_packet(&mut stdout, &buf) {
                    eprintln!("Failed to write response: {e}");
                    break;
                }
            }
            Err(e) => {
                eprintln!("Failed to encode response: {e}");
                break;
            }
        }
    }
}

fn directive_for_verbosity(v: u8) -> &'static str {
    // Target must match Rust module paths (`beamtalk_compiler_port`, `beamtalk_core`).
    // `beamtalk=…` only matches `beamtalk::*`, not `beamtalk_compiler_port`.
    match v {
        0 => "beamtalk_compiler_port=info,beamtalk_core=info",
        1 => "beamtalk_compiler_port=debug,beamtalk_core=debug",
        _ => "beamtalk_compiler_port=trace,beamtalk_core=trace",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// BT-907: Inline class definition with cross-file superclass index must compile
    /// as a value type, not an Actor, when the parent's chain resolves to Object.
    #[test]
    fn inline_class_definition_with_superclass_index_compiles_as_value_type() {
        // Build a compile_expression request for `Shape subclass: Triangle`
        // where Shape's superclass (Object) is provided via class_superclass_index.
        let superclass_index_map = Map::from([(binary("Shape"), binary("Object"))]);
        let request = Map::from([
            (atom("command"), atom("compile_expression")),
            (
                atom("source"),
                binary(
                    "Shape subclass: Triangle\n  state: base = 1.0\n  class withBase: b => self new: #{#base => b}",
                ),
            ),
            (atom("module"), binary("bt@triangle")),
            (atom("known_vars"), Term::from(eetf::List::from(vec![]))),
            (
                atom("class_superclass_index"),
                Term::from(superclass_index_map),
            ),
        ]);

        let response = handle_compile_expression(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected a map response, got: {response:?}");
        };

        // Must succeed
        let status = map_get(m, "status");
        assert_eq!(
            status,
            Some(&atom("ok")),
            "Expected ok status, got: {response:?}"
        );

        // Must be a class_definition response
        let kind = map_get(m, "kind");
        assert_eq!(
            kind,
            Some(&atom("class_definition")),
            "Expected class_definition kind, got: {response:?}"
        );

        // The generated Core Erlang must NOT contain gen_server (value type, not Actor)
        let core_erlang = map_get(m, "core_erlang")
            .and_then(term_to_string)
            .expect("core_erlang field must be present");
        assert!(
            !core_erlang.contains("'gen_server'"),
            "Triangle should be a value type (Object chain), not an Actor. \
             Shape→Object means Triangle→Shape→Object. Got core_erlang:\n{core_erlang}"
        );
    }

    #[test]
    fn directive_defaults() {
        assert_eq!(
            directive_for_verbosity(0),
            "beamtalk_compiler_port=info,beamtalk_core=info"
        );
        assert_eq!(
            directive_for_verbosity(1),
            "beamtalk_compiler_port=debug,beamtalk_core=debug"
        );
        assert_eq!(
            directive_for_verbosity(2),
            "beamtalk_compiler_port=trace,beamtalk_core=trace"
        );
    }

    /// ADR 0050 Phase 4: roundtrip — construct ETF `class_hierarchy` map, deserialize,
    /// verify `ClassInfo` fields match.
    #[test]
    fn parse_class_hierarchy_from_term_roundtrip() {
        use eetf::{FixInteger, List};

        let value_method_map = Map::from([
            (atom("arity"), Term::from(FixInteger::from(0))),
            (atom("param_types"), Term::from(List::from(vec![]))),
            (atom("return_type"), atom("Integer")),
        ]);
        let method_info_map = Map::from([(atom("value"), Term::from(value_method_map))]);

        let new_class_method_map = Map::from([
            (atom("arity"), Term::from(FixInteger::from(0))),
            (atom("param_types"), Term::from(List::from(vec![]))),
            (atom("return_type"), atom("counter")),
        ]);
        let class_method_info_map = Map::from([(atom("new"), Term::from(new_class_method_map))]);

        let field_types_map = Map::from([(atom("count"), atom("Integer"))]);

        let meta_map = Map::from([
            (atom("class"), atom("counter")),
            (atom("superclass"), atom("Actor")),
            (atom("meta_version"), Term::from(FixInteger::from(2))),
            (atom("is_sealed"), atom("false")),
            (atom("is_abstract"), atom("false")),
            (atom("is_value"), atom("false")),
            (atom("is_typed"), atom("false")),
            (atom("fields"), Term::from(List::from(vec![atom("count")]))),
            (atom("field_types"), Term::from(field_types_map)),
            (atom("method_info"), Term::from(method_info_map)),
            (atom("class_method_info"), Term::from(class_method_info_map)),
            (atom("class_variables"), Term::from(List::from(vec![]))),
        ]);

        let class_hierarchy_term = Term::from(Map::from([(atom("counter"), Term::from(meta_map))]));

        let classes = parse_class_hierarchy_from_term(&class_hierarchy_term);
        assert_eq!(classes.len(), 1, "Should parse one class");

        let info = &classes[0];
        assert_eq!(info.name.as_str(), "counter");
        assert_eq!(info.superclass.as_deref(), Some("Actor"));
        assert!(!info.is_sealed);
        assert!(!info.is_abstract);
        assert!(!info.is_value);
        assert!(!info.is_typed);
        assert_eq!(info.state.len(), 1);
        assert_eq!(info.state[0].as_str(), "count");
        assert_eq!(
            info.state_types.get("count").map(ecow::EcoString::as_str),
            Some("Integer")
        );
        assert_eq!(info.methods.len(), 1);
        assert_eq!(info.methods[0].selector.as_str(), "value");
        assert_eq!(info.methods[0].arity, 0);
        assert_eq!(info.methods[0].return_type.as_deref(), Some("Integer"));
        assert_eq!(info.class_methods.len(), 1);
        assert_eq!(info.class_methods[0].selector.as_str(), "new");
        assert_eq!(info.class_methods[0].arity, 0);
    }

    /// ADR 0050 Phase 4: `class_hierarchy` in `compile_expression` request is accepted
    /// and does not cause errors (backward-compatible optional key).
    #[test]
    fn compile_expression_accepts_class_hierarchy_key() {
        use eetf::{FixInteger, List};

        let method_info = Map::from([(
            atom("value"),
            Term::from(Map::from([
                (atom("arity"), Term::from(FixInteger::from(0))),
                (atom("param_types"), Term::from(List::from(vec![]))),
                (atom("return_type"), atom("Integer")),
            ])),
        )]);
        let counter_meta = Map::from([
            (atom("class"), atom("Counter")),
            (atom("superclass"), atom("Object")),
            (atom("meta_version"), Term::from(FixInteger::from(2))),
            (atom("is_sealed"), atom("false")),
            (atom("is_abstract"), atom("false")),
            (atom("is_value"), atom("false")),
            (atom("is_typed"), atom("false")),
            (atom("fields"), Term::from(List::from(vec![]))),
            (atom("field_types"), Term::from(Map::from([]))),
            (atom("method_info"), Term::from(method_info)),
            (atom("class_method_info"), Term::from(Map::from([]))),
            (atom("class_variables"), Term::from(List::from(vec![]))),
        ]);
        let class_hierarchy_term =
            Term::from(Map::from([(atom("Counter"), Term::from(counter_meta))]));

        let request = Map::from([
            (atom("command"), atom("compile_expression")),
            (atom("source"), binary("1 + 1.")),
            (atom("module"), binary("bt@test_repl")),
            (atom("known_vars"), Term::from(List::from(vec![]))),
            (atom("class_hierarchy"), class_hierarchy_term),
        ]);

        let response = handle_compile_expression(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response");
        };
        assert_eq!(
            map_get(m, "status"),
            Some(&atom("ok")),
            "compile_expression with class_hierarchy should succeed: {response:?}"
        );
    }

    /// ADR 0105 Phase 1 (BT-2778): `diagnostics` accepts `class_hierarchy`
    /// (like `compile_expression`/`compile_method` already did) and returns
    /// severity- and category-tagged diagnostics, so a re-check can tell a
    /// removed-selector `Dnu` from a `Type` mismatch without location alone.
    #[test]
    fn diagnostics_accepts_class_hierarchy_and_reports_category() {
        use eetf::{FixInteger, List};

        let method_info = Map::from([(
            atom("getCount"),
            Term::from(Map::from([
                (atom("arity"), Term::from(FixInteger::from(0))),
                (atom("param_types"), Term::from(List::from(vec![]))),
                // The reload changed Counter>>getCount's return type to
                // String — the caller's `+ 1` is now stale.
                (atom("return_type"), atom("String")),
            ])),
        )]);
        let counter_meta = Map::from([
            (atom("superclass"), atom("Object")),
            (atom("method_info"), Term::from(method_info)),
        ]);
        let class_hierarchy_term =
            Term::from(Map::from([(atom("Counter"), Term::from(counter_meta))]));

        let request = Map::from([
            (atom("command"), atom("diagnostics")),
            (
                atom("source"),
                binary(
                    "Object subclass: Dashboard\n  refresh: c :: Counter -> Integer => (c getCount) + 1\n",
                ),
            ),
            (atom("class_hierarchy"), class_hierarchy_term),
        ]);

        let response = handle_diagnostics(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response");
        };
        assert_eq!(map_get(m, "status"), Some(&atom("ok")));
        let Some(Term::List(diagnostics)) = map_get(m, "diagnostics") else {
            panic!("Expected diagnostics list: {response:?}");
        };
        assert!(
            !diagnostics.elements.is_empty(),
            "expected at least one diagnostic for the now-stale `+ 1`: {response:?}"
        );
        // Every diagnostic carries a `category` key (an atom `undefined` or a
        // binary label) — BT-2778's re-check orchestration filters on it.
        for diag in &diagnostics.elements {
            let Term::Map(dm) = diag else {
                panic!("Expected diagnostic map, got {diag:?}");
            };
            assert!(
                map_get(dm, "category").is_some(),
                "diagnostic missing category key: {diag:?}"
            );
        }
    }

    /// ADR 0108 hot-reload re-check trigger (BT-2899): `diagnostics` now
    /// threads `known_type_aliases` through (mirroring `compile_expression`)
    /// and reports the resolved compile's `referenced_aliases` — the
    /// alias-name → dependent-class index's raw material. Both the alias
    /// resolving correctly (proven the same way
    /// `known_type_aliases_resolves_alias_from_earlier_turn` proves it: a
    /// `matchExhaustive:` non-exhaustive diagnostic naming the residual
    /// member) and the new field being populated with the referenced name
    /// are pinned here.
    #[test]
    fn diagnostics_with_known_type_aliases_resolves_and_reports_referenced_aliases() {
        let request = Map::from([
            (atom("command"), atom("diagnostics")),
            (
                atom("source"),
                binary(
                    "scrutinee :: Direction := #north. \
                     scrutinee matchExhaustive: [#north -> 0; #south -> 1; #east -> 2]",
                ),
            ),
            (
                atom("known_type_aliases"),
                Term::from(List::from(vec![binary(
                    "type Direction = #north | #south | #east | #west",
                )])),
            ),
        ]);

        let response = handle_diagnostics(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response");
        };
        assert_eq!(map_get(m, "status"), Some(&atom("ok")));

        let Some(Term::List(diagnostics)) = map_get(m, "diagnostics") else {
            panic!("Expected diagnostics list: {response:?}");
        };
        let found = diagnostics.elements.iter().any(|d| {
            let Term::Map(dm) = d else { return false };
            map_get(dm, "message")
                .and_then(term_to_string)
                .is_some_and(|msg| {
                    msg.contains("non-exhaustive matchExhaustive:") && msg.contains("#west")
                })
        });
        assert!(
            found,
            "expected Direction to resolve to the closed union via known_type_aliases: {response:?}"
        );

        let Some(Term::List(referenced)) = map_get(m, "referenced_aliases") else {
            panic!("Expected referenced_aliases list: {response:?}");
        };
        let names: Vec<String> = referenced
            .elements
            .iter()
            .filter_map(term_to_string)
            .collect();
        assert_eq!(
            names,
            vec!["Direction".to_string()],
            "expected Direction to be recorded as referenced: {response:?}"
        );
    }

    /// Without `known_type_aliases`, `referenced_aliases` is simply absent
    /// any alias touch — an ordinary compile with no aliases in scope must
    /// not report anything.
    #[test]
    fn diagnostics_without_known_type_aliases_reports_empty_referenced_aliases() {
        let request = Map::from([
            (atom("command"), atom("diagnostics")),
            (atom("source"), binary("1 + 1.")),
        ]);

        let response = handle_diagnostics(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response");
        };
        let Some(Term::List(referenced)) = map_get(m, "referenced_aliases") else {
            panic!("Expected referenced_aliases list: {response:?}");
        };
        assert!(
            referenced.elements.is_empty(),
            "expected no referenced aliases: {response:?}"
        );
    }

    /// Without `class_hierarchy`, `diagnostics` behaves exactly as before
    /// (Counter is unknown, so no diagnostic is produced for the `+ 1` — the
    /// checker treats an undeclared receiver type as unresolved-class, not
    /// as `Counter`).
    #[test]
    fn diagnostics_without_class_hierarchy_is_unaffected() {
        let request = Map::from([
            (atom("command"), atom("diagnostics")),
            (atom("source"), binary("1 + 1.")),
        ]);

        let response = handle_diagnostics(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response");
        };
        assert_eq!(map_get(m, "status"), Some(&atom("ok")));
        let Some(Term::List(diagnostics)) = map_get(m, "diagnostics") else {
            panic!("Expected diagnostics list: {response:?}");
        };
        assert!(
            diagnostics.elements.is_empty(),
            "expected no diagnostics for a plain valid expression: {response:?}"
        );
    }

    /// BT-2839 (ADR 0100 Rule 3 surface-parity gap): a project root with a
    /// `dnu = "error"` `[diagnostics]` table parses into a table that
    /// escalates `Dnu`, mirroring what `beamtalk build` and the LSP
    /// (BT-2800) already do for the same `beamtalk.toml`.
    #[test]
    fn load_diagnostics_overrides_from_parses_project_manifest() {
        use beamtalk_core::compilation::DiagnosticSeverityOverride;
        use beamtalk_core::source_analysis::DiagnosticCategory;

        let dir = tempfile::tempdir().expect("failed to create temp dir");
        std::fs::write(
            dir.path().join("beamtalk.toml"),
            "[package]\nname = \"demo\"\nversion = \"0.1.0\"\n\n[diagnostics]\ndnu = \"error\"\n",
        )
        .expect("failed to write beamtalk.toml");

        let table = load_diagnostics_overrides_from(dir.path());
        assert_eq!(
            table.get(&DiagnosticCategory::Dnu),
            Some(&DiagnosticSeverityOverride::Error),
            "expected dnu = \"error\" to parse into the table, got: {table:?}"
        );
    }

    /// Lenient by design: a root with no `beamtalk.toml` at all — the common
    /// case for an ad-hoc `beamtalk repl` session outside a project — must
    /// not block diagnostics. An empty table is a complete no-op (Rule 1
    /// defaults), matching the LSP's `load_diagnostics_table_absent_manifest_is_noop`.
    #[test]
    fn load_diagnostics_overrides_from_missing_manifest_is_empty() {
        let dir = tempfile::tempdir().expect("failed to create temp dir");

        let table = load_diagnostics_overrides_from(dir.path());
        assert!(
            table.is_empty(),
            "expected an empty table with no beamtalk.toml, got: {table:?}"
        );
    }

    /// A `beamtalk.toml` with an invalid `[diagnostics]` table (unknown
    /// severity string) must not panic or block diagnostics — it logs and
    /// falls back to an empty table, same as a missing manifest.
    #[test]
    fn load_diagnostics_overrides_from_malformed_table_is_empty() {
        let dir = tempfile::tempdir().expect("failed to create temp dir");
        std::fs::write(
            dir.path().join("beamtalk.toml"),
            "[diagnostics]\ndnu = \"not-a-real-severity\"\n",
        )
        .expect("failed to write beamtalk.toml");

        let table = load_diagnostics_overrides_from(dir.path());
        assert!(
            table.is_empty(),
            "expected an empty table for a malformed [diagnostics] entry, got: {table:?}"
        );
    }

    /// ADR 0050 Phase 4: `class_hierarchy` in `compile` request is accepted
    /// and does not cause errors (backward-compatible optional key).
    #[test]
    fn compile_accepts_class_hierarchy_key() {
        use eetf::{FixInteger, List};

        let counter_meta = Map::from([
            (atom("class"), atom("Counter")),
            (atom("superclass"), atom("Actor")),
            (atom("meta_version"), Term::from(FixInteger::from(2))),
            (atom("is_sealed"), atom("false")),
            (atom("is_abstract"), atom("false")),
            (atom("is_value"), atom("false")),
            (atom("is_typed"), atom("false")),
            (atom("fields"), Term::from(List::from(vec![]))),
            (atom("field_types"), Term::from(Map::from([]))),
            (atom("method_info"), Term::from(Map::from([]))),
            (atom("class_method_info"), Term::from(Map::from([]))),
            (atom("class_variables"), Term::from(List::from(vec![]))),
        ]);
        let class_hierarchy_term =
            Term::from(Map::from([(atom("Counter"), Term::from(counter_meta))]));

        let request = Map::from([
            (atom("command"), atom("compile")),
            (
                atom("source"),
                binary("Object subclass: MyThing\n  hello => 42"),
            ),
            (atom("module_name"), binary("bt@my_thing")),
            (atom("class_hierarchy"), class_hierarchy_term),
        ]);

        let response = handle_compile(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response");
        };
        assert_eq!(
            map_get(m, "status"),
            Some(&atom("ok")),
            "compile with class_hierarchy should succeed: {response:?}"
        );
    }

    /// ADR 0108 hot-reload re-check trigger (BT-2899): `compile` now threads
    /// `known_type_aliases` through too (previously only
    /// `compile_expression` did), so a class-defining compile reports which
    /// alias names its own annotations referenced — the raw material for
    /// `beamtalk_alias_xref`'s alias-name → dependent-class index.
    #[test]
    fn compile_with_known_type_aliases_reports_referenced_aliases() {
        let request = Map::from([
            (atom("command"), atom("compile")),
            (
                atom("source"),
                binary(
                    "Object subclass: Dashboard\n  \
                     heading: h :: Direction -> Integer => 0\n",
                ),
            ),
            (atom("module_name"), binary("bt@dashboard")),
            (
                atom("known_type_aliases"),
                Term::from(List::from(vec![binary(
                    "type Direction = #north | #south | #east | #west",
                )])),
            ),
        ]);

        let response = handle_compile(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response");
        };
        assert_eq!(
            map_get(m, "status"),
            Some(&atom("ok")),
            "expected compile to succeed: {response:?}"
        );
        let Some(Term::List(referenced)) = map_get(m, "referenced_aliases") else {
            panic!("Expected referenced_aliases list: {response:?}");
        };
        let names: Vec<String> = referenced
            .elements
            .iter()
            .filter_map(term_to_string)
            .collect();
        assert_eq!(
            names,
            vec!["Direction".to_string()],
            "expected Direction to be recorded as referenced: {response:?}"
        );
    }

    /// BT-2917 (BT-2899 follow-up): the sibling of
    /// `compile_with_known_type_aliases_reports_referenced_aliases` for a
    /// protocol-only `compile` — before this fix, `protocol_definition`'s
    /// response had no `referenced_aliases` field at all, so
    /// `beamtalk_repl_compiler.erl`'s protocol arm had nothing to register
    /// into `beamtalk_alias_xref`, even though the exact same annotation on
    /// a class method's signature (the test above) already worked.
    #[test]
    fn compile_protocol_with_known_type_aliases_reports_referenced_aliases() {
        let request = Map::from([
            (atom("command"), atom("compile")),
            (
                atom("source"),
                binary("Protocol define: Directional\n  heading: d :: Direction -> Boolean\n"),
            ),
            (atom("module_name"), binary("bt@directional")),
            (
                atom("known_type_aliases"),
                Term::from(List::from(vec![binary(
                    "type Direction = #north | #south | #east | #west",
                )])),
            ),
        ]);

        let response = handle_compile(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response");
        };
        assert_eq!(
            map_get(m, "status"),
            Some(&atom("ok")),
            "expected compile to succeed: {response:?}"
        );
        assert_eq!(
            map_get(m, "kind"),
            Some(&atom("protocol_definition")),
            "expected a protocol_definition response: {response:?}"
        );
        let Some(Term::List(referenced)) = map_get(m, "referenced_aliases") else {
            panic!("Expected referenced_aliases list: {response:?}");
        };
        let names: Vec<String> = referenced
            .elements
            .iter()
            .filter_map(term_to_string)
            .collect();
        assert_eq!(
            names,
            vec!["Direction".to_string()],
            "expected Direction to be recorded as referenced: {response:?}"
        );
    }

    /// BT-2952: the REPL-inline sibling of
    /// `compile_with_known_type_aliases_reports_referenced_aliases` for a
    /// class defined via `compile_expression` (as opposed to `:load`d from
    /// a file via `compile`) — before this fix, `parse_and_check_expression`
    /// called `compute_diagnostics_with_known_vars_classes_and_aliases`,
    /// which discards `AnalysisResult::referenced_aliases` entirely, so
    /// `class_definition_ok_response` had no field to carry it in at all.
    #[test]
    fn compile_expression_class_with_known_type_aliases_reports_referenced_aliases() {
        let request = Map::from([
            (atom("command"), atom("compile_expression")),
            (
                atom("source"),
                binary(
                    "Object subclass: Dashboard\n  \
                     heading: h :: Direction -> Integer => 0\n",
                ),
            ),
            (atom("module"), binary("bt@repl_eval_1")),
            (atom("known_vars"), Term::from(List::from(vec![]))),
            (
                atom("known_type_aliases"),
                Term::from(List::from(vec![binary(
                    "type Direction = #north | #south | #east | #west",
                )])),
            ),
        ]);

        let response = handle_compile_expression(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response");
        };
        assert_eq!(
            map_get(m, "status"),
            Some(&atom("ok")),
            "expected compile_expression to succeed: {response:?}"
        );
        assert_eq!(
            map_get(m, "kind"),
            Some(&atom("class_definition")),
            "expected a class_definition response: {response:?}"
        );
        let Some(Term::List(referenced)) = map_get(m, "referenced_aliases") else {
            panic!("Expected referenced_aliases list: {response:?}");
        };
        let names: Vec<String> = referenced
            .elements
            .iter()
            .filter_map(term_to_string)
            .collect();
        assert_eq!(
            names,
            vec!["Direction".to_string()],
            "expected Direction to be recorded as referenced: {response:?}"
        );
    }

    /// BT-2952: the REPL-inline sibling of
    /// `compile_protocol_with_known_type_aliases_reports_referenced_aliases`
    /// for a protocol defined via `compile_expression` — before this fix,
    /// `handle_compile_expression`'s protocol branch called
    /// `handle_inline_protocol_definition` with a hardcoded `&[]` since
    /// `parse_and_check_expression` never computed a real set for this path.
    #[test]
    fn compile_expression_protocol_with_known_type_aliases_reports_referenced_aliases() {
        let request = Map::from([
            (atom("command"), atom("compile_expression")),
            (
                atom("source"),
                binary("Protocol define: Directional\n  heading: d :: Direction -> Boolean\n"),
            ),
            (atom("module"), binary("bt@repl_eval_1")),
            (atom("known_vars"), Term::from(List::from(vec![]))),
            (
                atom("known_type_aliases"),
                Term::from(List::from(vec![binary(
                    "type Direction = #north | #south | #east | #west",
                )])),
            ),
        ]);

        let response = handle_compile_expression(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response");
        };
        assert_eq!(
            map_get(m, "status"),
            Some(&atom("ok")),
            "expected compile_expression to succeed: {response:?}"
        );
        assert_eq!(
            map_get(m, "kind"),
            Some(&atom("protocol_definition")),
            "expected a protocol_definition response: {response:?}"
        );
        let Some(Term::List(referenced)) = map_get(m, "referenced_aliases") else {
            panic!("Expected referenced_aliases list: {response:?}");
        };
        let names: Vec<String> = referenced
            .elements
            .iter()
            .filter_map(term_to_string)
            .collect();
        assert_eq!(
            names,
            vec!["Direction".to_string()],
            "expected Direction to be recorded as referenced: {response:?}"
        );
    }

    /// BT-2941: `handle_inline_protocol_definition` (the protocol-only branch
    /// of `handle_compile`) previously never threaded `pre_loaded_aliases`
    /// into `CodegenOptions`, so `self.alias_registry` at codegen time was
    /// always the empty module-local registry — a protocol source file never
    /// declares its own `type Name = ...` (that's a separate top-level
    /// declaration), so every alias a protocol method signature could
    /// reference is necessarily cross-module/pre-loaded. Mirrors the BT-2932
    /// wiring pattern: `Wrapper`'s own alias body references `Base` (a
    /// second, separately pre-loaded alias) by name, so the fix is only
    /// observable if the *whole* pre-loaded registry reaches codegen, not
    /// just semantic analysis (which already resolved both names before this
    /// fix — see `compile_protocol_with_known_type_aliases_reports_referenced_aliases`
    /// above, which passed even before this fix since it only checks
    /// `referenced_aliases`, a semantic-analysis-only signal).
    #[test]
    fn compile_protocol_cross_module_alias_reference_emits_user_type() {
        let request = Map::from([
            (atom("command"), atom("compile")),
            (
                atom("source"),
                binary("Protocol define: Directional\n  heading: d :: Wrapper -> Boolean\n"),
            ),
            (atom("module_name"), binary("bt@directional")),
            (
                atom("known_type_aliases"),
                Term::from(List::from(vec![
                    binary("type Base = #north | #south | #east | #west"),
                    binary("type Wrapper = Base"),
                ])),
            ),
        ]);

        let response = handle_compile(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response");
        };
        assert_eq!(
            map_get(m, "status"),
            Some(&atom("ok")),
            "expected compile to succeed: {response:?}"
        );
        let core_erlang = map_get(m, "core_erlang")
            .and_then(term_to_string)
            .expect("core_erlang field must be present");
        assert!(
            core_erlang.contains("{'user_type', 0, 'base', []}"),
            "Wrapper's alias body should resolve Base as a user_type reference \
             now that pre_loaded_aliases reaches the protocol codegen path. \
             Got:\n{core_erlang}"
        );
        assert!(
            core_erlang.contains("'wrapper'") && core_erlang.contains("'base'"),
            "module must declare named -type attributes for both pre-loaded \
             aliases. Got:\n{core_erlang}"
        );
    }

    /// BT-2941 sibling of `compile_protocol_cross_module_alias_reference_emits_user_type`
    /// for the OTHER `handle_inline_protocol_definition` caller: the REPL-inline
    /// `compile_expression` path (`handle_compile_expression`'s protocol branch).
    /// Both call sites needed the same `.with_pre_loaded_aliases(...)` wiring.
    #[test]
    fn compile_expression_protocol_cross_module_alias_reference_emits_user_type() {
        let request = Map::from([
            (atom("command"), atom("compile_expression")),
            (
                atom("source"),
                binary("Protocol define: Directional\n  heading: d :: Wrapper -> Boolean\n"),
            ),
            (atom("module"), binary("bt@repl_eval_1")),
            (atom("known_vars"), Term::from(List::from(vec![]))),
            (
                atom("known_type_aliases"),
                Term::from(List::from(vec![
                    binary("type Base = #north | #south | #east | #west"),
                    binary("type Wrapper = Base"),
                ])),
            ),
        ]);

        let response = handle_compile_expression(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected a map response, got: {response:?}");
        };
        assert_eq!(
            map_get(m, "status").and_then(term_to_atom).as_deref(),
            Some("ok"),
            "expected compile_expression to succeed: {response:?}"
        );
        let core_erlang = map_get(m, "core_erlang")
            .and_then(term_to_string)
            .expect("core_erlang field must be present");
        assert!(
            core_erlang.contains("{'user_type', 0, 'base', []}"),
            "Wrapper's alias body should resolve Base as a user_type reference \
             now that pre_loaded_aliases reaches the REPL-inline protocol codegen \
             path. Got:\n{core_erlang}"
        );
    }

    /// The concrete BT-2912 repro, exercised through the compiler port
    /// exactly as a live REPL turn would present it: turn 1 declares `type
    /// Point = Integer` (carried forward via `known_type_aliases`, mirroring
    /// how the workspace re-seeds it every turn — ADR 0108 Phase 8); turn 2
    /// sends `Object subclass: Point`. Before BT-2899, `compile` never
    /// threaded `known_type_aliases` at all, so
    /// `AliasRegistry::add_pre_loaded`'s existing collision check
    /// (`alias_registry.rs`) never had a chance to see the class — the class
    /// compiled clean, silently shadowing the alias in every subsequent `::`
    /// annotation. It must now fail with the namespace-collision diagnostic.
    #[test]
    fn compile_class_over_earlier_turn_alias_is_flagged() {
        let request = Map::from([
            (atom("command"), atom("compile")),
            (
                atom("source"),
                binary("Object subclass: Point\n  hello => 42\n"),
            ),
            (atom("module_name"), binary("bt@point")),
            (
                atom("known_type_aliases"),
                Term::from(List::from(vec![binary("type Point = Integer")])),
            ),
        ]);

        let response = handle_compile(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response");
        };
        assert_eq!(
            map_get(m, "status"),
            Some(&atom("error")),
            "expected the class-vs-alias collision to fail the compile: {response:?}"
        );
        let Some(Term::List(diags)) = map_get(m, "diagnostics") else {
            panic!("Expected diagnostics list: {response:?}");
        };
        let found = diags.elements.iter().any(|d| {
            let Term::Map(dm) = d else { return false };
            map_get(dm, "message")
                .and_then(term_to_string)
                .is_some_and(|msg| msg.contains("Point") && msg.contains("collides with class"))
        });
        assert!(
            found,
            "expected a Point-vs-alias collision diagnostic: {response:?}"
        );
    }

    // --- resolve_completion_type tests (BT-1068) ---

    #[test]
    fn resolve_completion_type_string_literal() {
        let request = Map::from([
            (atom("command"), atom("resolve_completion_type")),
            (atom("expression"), binary("\"hello\"")),
        ]);
        let response = handle_resolve_completion_type(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response");
        };
        assert_eq!(map_get(m, "status"), Some(&atom("ok")));
        assert_eq!(
            map_get(m, "class_name").and_then(term_to_string),
            Some("String".to_string())
        );
    }

    #[test]
    fn resolve_completion_type_parenthesized_binary_send() {
        let request = Map::from([
            (atom("command"), atom("resolve_completion_type")),
            (atom("expression"), binary("(\"foo\" ++ \"bar\")")),
        ]);
        let response = handle_resolve_completion_type(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response");
        };
        assert_eq!(map_get(m, "status"), Some(&atom("ok")));
        assert_eq!(
            map_get(m, "class_name").and_then(term_to_string),
            Some("String".to_string())
        );
    }

    /// BT-2891: `load_native_type_registry_from` reads `<module>_<16-hex>.json`
    /// entries from `<root>/_build/type_cache/` (the same on-disk format
    /// `beamtalk build`/`beamtalk lint` write via
    /// `beamtalk_core::ffi_type_specs`) and replays their `specs_line` into a
    /// `NativeTypeRegistry`.
    #[test]
    fn load_native_type_registry_from_reads_type_cache() {
        use beamtalk_core::semantic_analysis::type_checker::InferredType;

        let dir = tempfile::tempdir().expect("failed to create temp dir");
        let cache_dir = dir.path().join("_build").join("type_cache");
        std::fs::create_dir_all(&cache_dir).expect("failed to create type_cache dir");
        std::fs::write(
            cache_dir.join("lists_0123456789abcdef.json"),
            format!(
                r#"{{"beam_mtime_secs":0,"beam_mtime_nanos":0,"mapping_stamp":"{}","specs_line":"beamtalk-specs-module:lists:[#{{name => <<\"reverse\">>,arity => 1,params => [#{{name => <<\"list\">>,type => <<\"List\">>}}],return_type => <<\"List\">>}}]"}}"#,
                beamtalk_core::ffi_type_specs::current_spec_mapping_stamp()
            ),
        )
        .expect("failed to write cache entry");

        let registry = load_native_type_registry_from(dir.path());
        let sig = registry
            .lookup("lists", "reverse", 1)
            .expect("lists:reverse/1 should be registered");
        assert_eq!(sig.arity, 1);
        assert_eq!(sig.return_type, InferredType::known("List"));
    }

    /// BT-2891: a project that has never run `beamtalk build` (no
    /// `_build/type_cache/`) yields an empty registry rather than an error —
    /// the REPL must keep evaluating, registry-blind, exactly like pre-BT-2891.
    #[test]
    fn load_native_type_registry_from_missing_cache_is_empty() {
        let dir = tempfile::tempdir().expect("failed to create temp dir");
        let registry = load_native_type_registry_from(dir.path());
        assert_eq!(registry.module_count(), 0);
    }

    /// BT-2891: with an empty native type registry (the pre-BT-2891 state,
    /// and what a project that has never run `beamtalk build` yields), an FFI
    /// expression still falls back to `not_found` (`Dynamic`) — unchanged
    /// behaviour. Exercises `resolve_completion_type_response` directly with
    /// an explicit empty registry rather than `handle_resolve_completion_type`'s
    /// process-wide `OnceLock` (which reads the real cwd's `_build/type_cache/`
    /// and is shared across every test in this binary) so the assertion can't
    /// flip depending on what happens to be on disk at test time.
    #[test]
    fn resolve_completion_type_response_ffi_expression_with_empty_registry_stays_not_found() {
        use beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry;

        let response = resolve_completion_type_response(
            "Erlang lists reverse: #(1, 2, 3)",
            vec![],
            &NativeTypeRegistry::new(),
        );
        let Term::Map(ref m) = response else {
            panic!("Expected map response");
        };
        assert_eq!(
            map_get(m, "status"),
            Some(&atom("not_found")),
            "{response:?}"
        );
    }

    /// BT-2891: with a populated native type registry, `resolve_completion_type`
    /// resolves an FFI expression to its real return class instead of falling
    /// back to `Dynamic`/`not_found`. Exercises `resolve_completion_type_response`
    /// directly (rather than `handle_resolve_completion_type`'s process-wide
    /// `OnceLock`) so the registry-provided path is covered without touching
    /// the filesystem or global state — mirroring BT-2887's
    /// `resolve_expression_type_with_native_registry_resolves_ffi_call` test in
    /// `completion_provider.rs`.
    #[test]
    fn resolve_completion_type_response_resolves_ffi_call_with_populated_registry() {
        use beamtalk_core::semantic_analysis::type_checker::{
            FunctionSignature, InferredType, NativeTypeRegistry, ParamType, TypeProvenance,
        };

        let mut registry = NativeTypeRegistry::new();
        registry.register_module(
            "lists",
            vec![FunctionSignature {
                name: "reverse".to_string(),
                arity: 1,
                params: vec![ParamType {
                    keyword: Some(ecow::EcoString::from("list")),
                    type_: InferredType::known("List"),
                }],
                return_type: InferredType::known("List"),
                provenance: TypeProvenance::Extracted,
                line: None,
            }],
        );

        let response =
            resolve_completion_type_response("Erlang lists reverse: #(1, 2, 3)", vec![], &registry);
        let Term::Map(ref m) = response else {
            panic!("Expected map response");
        };
        assert_eq!(map_get(m, "status"), Some(&atom("ok")), "{response:?}");
        assert_eq!(
            map_get(m, "class_name").and_then(term_to_string),
            Some("List".to_string())
        );
    }

    // --- resolve_method_span tests (ADR 0082 Phase 1, BT-2283) ---

    const SPAN_FIXTURE: &str = "\
Object subclass: Counter

  increment => self.value := self.value + 1

  class new => self basicNew
";

    #[test]
    fn resolve_method_span_instance_method() {
        let request = Map::from([
            (atom("command"), atom("resolve_method_span")),
            (atom("source"), binary(SPAN_FIXTURE)),
            (atom("class_name"), binary("Counter")),
            (atom("selector"), binary("increment")),
        ]);
        let response = handle_resolve_method_span(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response, got: {response:?}");
        };
        assert_eq!(map_get(m, "status"), Some(&atom("ok")), "{response:?}");
        // prev_source must be exactly the bytes occupying the span — splicing it
        // back is a no-op, the load-bearing property of the splice strategy.
        let prev = map_get(m, "prev_source")
            .and_then(term_to_string)
            .expect("prev_source present");
        // The span is a verbatim full-line slice, so it carries the method's
        // leading indentation (and its doc comment, when present — BT-2577).
        assert!(prev.starts_with("  increment =>"), "got: {prev:?}");
        assert!(
            prev.ends_with('\n'),
            "span includes trailing newline: {prev:?}"
        );
        let Some(Term::Map(span)) = map_get(m, "span") else {
            panic!("span should be a map: {response:?}");
        };
        let start = map_get(span, "start").and_then(term_to_usize).unwrap();
        let end = map_get(span, "end").and_then(term_to_usize).unwrap();
        assert_eq!(
            &SPAN_FIXTURE[start..end],
            prev,
            "span must bound prev_source"
        );
    }

    #[test]
    fn resolve_method_span_class_side() {
        let request = Map::from([
            (atom("command"), atom("resolve_method_span")),
            (atom("source"), binary(SPAN_FIXTURE)),
            (atom("class_name"), binary("Counter")),
            (atom("selector"), binary("new")),
            (atom("side"), atom("class")),
        ]);
        let response = handle_resolve_method_span(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response, got: {response:?}");
        };
        assert_eq!(map_get(m, "status"), Some(&atom("ok")), "{response:?}");
        let prev = map_get(m, "prev_source")
            .and_then(term_to_string)
            .expect("prev_source present");
        assert!(prev.starts_with("  class new =>"), "got: {prev:?}");
    }

    #[test]
    fn resolve_method_span_selector_not_found_is_structured_error() {
        let request = Map::from([
            (atom("command"), atom("resolve_method_span")),
            (atom("source"), binary(SPAN_FIXTURE)),
            (atom("class_name"), binary("Counter")),
            (atom("selector"), binary("nope")),
        ]);
        let response = handle_resolve_method_span(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response, got: {response:?}");
        };
        assert_eq!(map_get(m, "status"), Some(&atom("error")), "{response:?}");
        assert_eq!(map_get(m, "reason"), Some(&atom("selector_not_found")));
    }

    #[test]
    fn resolve_method_span_class_not_found_is_structured_error() {
        let request = Map::from([
            (atom("command"), atom("resolve_method_span")),
            (atom("source"), binary(SPAN_FIXTURE)),
            (atom("class_name"), binary("NoSuchClass")),
            (atom("selector"), binary("increment")),
        ]);
        let response = handle_resolve_method_span(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response, got: {response:?}");
        };
        assert_eq!(map_get(m, "status"), Some(&atom("error")), "{response:?}");
        assert_eq!(map_get(m, "reason"), Some(&atom("class_not_found")));
    }

    // --- reindent_method_source tests (BT-2584) ---

    #[test]
    fn reindent_method_source_shifts_canonical_to_base() {
        let request = Map::from([
            (atom("command"), atom("reindent_method_source")),
            (atom("source"), binary("/// doc\ndecrement => self.v - 2\n")),
            (atom("base_indent"), binary("  ")),
        ]);
        let response = handle_reindent_method_source(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response, got: {response:?}");
        };
        assert_eq!(map_get(m, "status"), Some(&atom("ok")), "{response:?}");
        assert_eq!(
            map_get(m, "source").and_then(term_to_string).as_deref(),
            Some("  /// doc\n  decrement => self.v - 2\n"),
        );
    }

    #[test]
    fn reindent_method_source_empty_base_is_identity() {
        let request = Map::from([
            (atom("command"), atom("reindent_method_source")),
            (atom("source"), binary("foo => 1\n")),
            (atom("base_indent"), binary("")),
        ]);
        let response = handle_reindent_method_source(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response, got: {response:?}");
        };
        assert_eq!(
            map_get(m, "source").and_then(term_to_string).as_deref(),
            Some("foo => 1\n"),
        );
    }

    #[test]
    fn reindent_method_source_missing_base_indent_defaults_empty() {
        // `base_indent` is optional and defaults to empty (identity).
        let request = Map::from([
            (atom("command"), atom("reindent_method_source")),
            (atom("source"), binary("foo => 1\n")),
        ]);
        let response = handle_reindent_method_source(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response, got: {response:?}");
        };
        assert_eq!(map_get(m, "status"), Some(&atom("ok")), "{response:?}");
        assert_eq!(
            map_get(m, "source").and_then(term_to_string).as_deref(),
            Some("foo => 1\n"),
        );
    }

    /// BT-1238: `compile_expression_trace` produces Core Erlang with trace list return.
    #[test]
    fn compile_expression_trace_single_expression() {
        use eetf::List;

        let request = Map::from([
            (atom("command"), atom("compile_expression_trace")),
            (atom("source"), binary("1 + 1.")),
            (atom("module"), binary("bt@trace_test")),
            (atom("known_vars"), Term::from(List::from(vec![]))),
        ]);
        let response = handle_compile_expression_trace(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response, got: {response:?}");
        };
        assert_eq!(
            map_get(m, "status"),
            Some(&atom("ok")),
            "compile_expression_trace should succeed: {response:?}"
        );
        let core_erlang = map_get(m, "core_erlang")
            .and_then(term_to_string)
            .expect("core_erlang field must be present");
        // Trace module must export eval/1 and return a steps list, not a plain {Result, State}.
        assert!(
            core_erlang.contains("'eval'/1"),
            "Trace module must export eval/1: {core_erlang}"
        );
        // The return must be a cons list [...] wrapping step tuples, not a plain 2-tuple.
        assert!(
            core_erlang.contains("[{"),
            "Trace return must be a list of step tuples: {core_erlang}"
        );
    }

    /// BT-1238: `compile_expression_trace` rejects class definitions.
    #[test]
    fn compile_expression_trace_rejects_class_definition() {
        use eetf::List;

        let request = Map::from([
            (atom("command"), atom("compile_expression_trace")),
            (atom("source"), binary("Object subclass: Foo\n  bar => 42")),
            (atom("module"), binary("bt@trace_class_test")),
            (atom("known_vars"), Term::from(List::from(vec![]))),
        ]);
        let response = handle_compile_expression_trace(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response");
        };
        assert_eq!(
            map_get(m, "status"),
            Some(&atom("error")),
            "compile_expression_trace should reject class definitions: {response:?}"
        );
    }

    #[test]
    fn resolve_completion_type_unknown_expression() {
        let request = Map::from([
            (atom("command"), atom("resolve_completion_type")),
            (atom("expression"), binary("unknownVar")),
        ]);
        let response = handle_resolve_completion_type(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response");
        };
        assert_eq!(map_get(m, "status"), Some(&atom("not_found")));
    }

    #[test]
    fn resolve_completion_type_empty_expression() {
        let request = Map::from([
            (atom("command"), atom("resolve_completion_type")),
            (atom("expression"), binary("")),
        ]);
        let response = handle_resolve_completion_type(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected map response");
        };
        assert_eq!(map_get(m, "status"), Some(&atom("not_found")));
    }

    #[test]
    fn parse_class_hierarchy_skips_builtins() {
        use eetf::{FixInteger, List};

        let meta_map = Map::from([
            (atom("class"), atom("Integer")),
            (atom("superclass"), atom("Number")),
            (atom("meta_version"), Term::from(FixInteger::from(2))),
            (atom("is_sealed"), atom("false")),
            (atom("is_abstract"), atom("false")),
            (atom("is_value"), atom("false")),
            (atom("is_typed"), atom("false")),
            (atom("fields"), Term::from(List::from(vec![]))),
            (atom("field_types"), Term::from(Map::from([]))),
            (atom("method_info"), Term::from(Map::from([]))),
            (atom("class_method_info"), Term::from(Map::from([]))),
            (atom("class_variables"), Term::from(List::from(vec![]))),
        ]);
        let term = Term::from(Map::from([(atom("Integer"), Term::from(meta_map))]));
        let classes = parse_class_hierarchy_from_term(&term);
        assert!(
            classes.is_empty(),
            "Integer is a builtin — should be skipped"
        );
    }

    /// BT-1670: `derive_class_module_name` uses override when provided.
    #[test]
    fn derive_class_module_name_with_override() {
        let result = derive_class_module_name("Counter", Some("bt@my_app@counter"), false);
        assert_eq!(result, "bt@my_app@counter");
    }

    /// BT-1670: `derive_class_module_name` falls back to `bt@{snake}` without override.
    #[test]
    fn derive_class_module_name_no_override() {
        let result = derive_class_module_name("MyCounter", None, false);
        assert_eq!(result, "bt@my_counter");
    }

    /// BT-1670: `derive_class_module_name` uses stdlib prefix in stdlib mode.
    #[test]
    fn derive_class_module_name_stdlib_mode() {
        let result = derive_class_module_name("Integer", None, true);
        assert_eq!(result, "bt@stdlib@integer");
    }

    /// BT-1670: `derive_class_module_name` override takes precedence over stdlib mode.
    #[test]
    fn derive_class_module_name_override_over_stdlib() {
        let result = derive_class_module_name("Integer", Some("bt@custom@integer"), true);
        assert_eq!(result, "bt@custom@integer");
    }

    /// BT-1670: Inline class definition uses `module_name` override when provided,
    /// ensuring package-mode REPL class definitions get the same module name as
    /// file-based compilation.
    #[test]
    fn inline_class_definition_with_module_name_override() {
        let request = Map::from([
            (atom("command"), atom("compile_expression")),
            (
                atom("source"),
                binary("Object subclass: MyThing\n  greet => \"hello\""),
            ),
            (atom("module"), binary("bt@repl_eval_1")),
            (atom("known_vars"), Term::from(List::from(vec![]))),
            (atom("module_name"), binary("bt@my_app@my_thing")),
        ]);

        let response = handle_compile_expression(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected a map response, got: {response:?}");
        };

        // Should be a class_definition response
        let status = map_get(m, "status").and_then(term_to_atom);
        assert_eq!(status.as_deref(), Some("ok"));

        let resp_kind = map_get(m, "kind").and_then(term_to_atom);
        assert_eq!(resp_kind.as_deref(), Some("class_definition"));

        // The module name should use the override, not bt@my_thing
        let module_name = map_get(m, "module_name").and_then(term_to_string);
        assert_eq!(module_name.as_deref(), Some("bt@my_app@my_thing"));
    }

    /// Protocol-only file compilation succeeds and derives module name from protocol.
    #[test]
    fn compile_protocol_only_file() {
        let request = Map::from([
            (atom("command"), atom("compile")),
            (atom("source"), binary("Protocol define: Awaitable")),
        ]);

        let response = handle_compile(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected a map response, got: {response:?}");
        };

        let status = map_get(m, "status").and_then(term_to_atom);
        assert_eq!(
            status.as_deref(),
            Some("ok"),
            "Protocol-only file should compile successfully"
        );

        // BT-1950: Protocol-only files now return a protocol_definition response
        let kind = map_get(m, "kind").and_then(term_to_atom);
        assert_eq!(
            kind.as_deref(),
            Some("protocol_definition"),
            "Protocol-only file should return protocol_definition kind"
        );

        let module_name = map_get(m, "module_name").and_then(term_to_string);
        assert_eq!(module_name.as_deref(), Some("bt@awaitable"));

        // Protocols list should contain the protocol name
        let protocols = map_get(m, "protocols").expect("response should include 'protocols' key");
        let Term::List(list) = protocols else {
            panic!("Expected protocols to be a list, got: {protocols:?}");
        };
        assert_eq!(list.elements.len(), 1, "Should have one protocol");
        assert_eq!(
            term_to_string(&list.elements[0]).as_deref(),
            Some("Awaitable")
        );

        // core_erlang should be present
        assert!(
            map_get(m, "core_erlang").is_some(),
            "Protocol response should include core_erlang"
        );
    }

    /// Protocol-only file compilation with `module_name` override.
    #[test]
    fn compile_protocol_only_file_with_override() {
        let request = Map::from([
            (atom("command"), atom("compile")),
            (atom("source"), binary("Protocol define: Awaitable")),
            (atom("module_name"), binary("bt@exdura@awaitable")),
        ]);

        let response = handle_compile(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected a map response, got: {response:?}");
        };

        let status = map_get(m, "status").and_then(term_to_atom);
        assert_eq!(status.as_deref(), Some("ok"));

        // BT-1950: Protocol-only files return protocol_definition kind
        let kind = map_get(m, "kind").and_then(term_to_atom);
        assert_eq!(kind.as_deref(), Some("protocol_definition"));

        let module_name = map_get(m, "module_name").and_then(term_to_string);
        assert_eq!(module_name.as_deref(), Some("bt@exdura@awaitable"));
    }

    /// BT-1670: Inline class definition without override uses default `bt@` prefix.
    #[test]
    fn inline_class_definition_without_module_name_override() {
        let request = Map::from([
            (atom("command"), atom("compile_expression")),
            (
                atom("source"),
                binary("Object subclass: MyThing\n  greet => \"hello\""),
            ),
            (atom("module"), binary("bt@repl_eval_1")),
            (atom("known_vars"), Term::from(List::from(vec![]))),
        ]);

        let response = handle_compile_expression(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected a map response, got: {response:?}");
        };

        let resp_kind = map_get(m, "kind").and_then(term_to_atom);
        assert_eq!(resp_kind.as_deref(), Some("class_definition"));

        // Without override, should derive from class name: bt@my_thing
        let module_name = map_get(m, "module_name").and_then(term_to_string);
        assert_eq!(module_name.as_deref(), Some("bt@my_thing"));
    }

    /// ADR 0105 Phase 1 (BT-2777): the standalone `Class >> sel` method-definition
    /// response (the REPL `>>` live-patch path) must carry the declared signature
    /// alongside the existing `method_source`, so the workspace can capture it into
    /// the signature-generation store before the patch installs.
    #[test]
    fn standalone_method_definition_carries_declared_signature() {
        let request = Map::from([
            (atom("command"), atom("compile_expression")),
            (
                atom("source"),
                binary("Counter >> setValue: v :: Integer -> Object => self.value := v"),
            ),
            (atom("module"), binary("bt@repl_eval_1")),
            (atom("known_vars"), Term::from(List::from(vec![]))),
        ]);

        let response = handle_compile_expression(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected a map response, got: {response:?}");
        };

        assert_eq!(
            map_get(m, "kind").and_then(term_to_atom).as_deref(),
            Some("method_definition")
        );
        assert_eq!(
            map_get(m, "return_type")
                .and_then(term_to_string)
                .as_deref(),
            Some("Object")
        );
        let Some(Term::List(param_types)) = map_get(m, "param_types") else {
            panic!("Expected param_types list, got: {m:?}");
        };
        let param_type_strs: Vec<String> = param_types
            .elements
            .iter()
            .filter_map(term_to_string)
            .collect();
        assert_eq!(param_type_strs, vec!["Integer".to_string()]);
    }

    /// Unannotated standalone method definitions report the `"Dynamic"` sentinel
    /// rather than omitting the fields.
    #[test]
    fn standalone_method_definition_reports_dynamic_when_unannotated() {
        let request = Map::from([
            (atom("command"), atom("compile_expression")),
            (
                atom("source"),
                binary("Counter >> increment => self.value := self.value + 1"),
            ),
            (atom("module"), binary("bt@repl_eval_1")),
            (atom("known_vars"), Term::from(List::from(vec![]))),
        ]);

        let response = handle_compile_expression(&request);
        let Term::Map(ref m) = response else {
            panic!("Expected a map response, got: {response:?}");
        };

        assert_eq!(
            map_get(m, "return_type")
                .and_then(term_to_string)
                .as_deref(),
            Some("Dynamic")
        );
        assert_eq!(
            map_get(m, "param_types"),
            Some(&Term::from(List::from(Vec::<Term>::new())))
        );
    }
}

// ============================================================================
// Property-based tests (ADR 0011 Phase 4)
// ============================================================================

#[cfg(test)]
mod property_tests {
    //! Property-based tests for the compiler port compile round-trip.
    //!
    //! These tests use `proptest` to verify that `handle_compile()` and
    //! `handle_compile_expression()` never panic on arbitrary input and
    //! always return well-formed ETF responses.
    //!
    //! **DDD Context:** Compilation (Anti-Corruption Layer)
    //!
    //! ADR 0011 Phase 4.

    use super::*;
    use proptest::prelude::*;

    /// Build an ETF Map for a `compile` request with the given source.
    fn compile_request(source: &str) -> Map {
        Map::from([
            (atom("command"), atom("compile")),
            (atom("source"), binary(source)),
        ])
    }

    /// Build an ETF Map for a `compile_expression` request with the given source.
    fn compile_expression_request(source: &str) -> Map {
        Map::from([
            (atom("command"), atom("compile_expression")),
            (atom("source"), binary(source)),
            (atom("module"), binary("bt@test_module")),
        ])
    }

    /// Extract the status atom from a response Term.
    fn response_status(term: &Term) -> Option<String> {
        if let Term::Map(map) = term {
            map_get(map, "status").and_then(|t| {
                if let Term::Atom(a) = t {
                    Some(a.name.clone())
                } else {
                    None
                }
            })
        } else {
            None
        }
    }

    /// Extract the diagnostics list from a response Term.
    fn response_diagnostics(term: &Term) -> Option<&List> {
        if let Term::Map(map) = term {
            if let Some(Term::List(list)) = map_get(map, "diagnostics") {
                return Some(list);
            }
        }
        None
    }

    /// Extract a string-valued (binary) field from a response Term.
    fn response_field_str(term: &Term, key: &str) -> Option<String> {
        if let Term::Map(map) = term {
            map_get(map, key).and_then(term_to_string)
        } else {
            None
        }
    }

    /// Extract a list-of-binary-valued field from a response Term (e.g. `param_types`).
    fn response_field_str_list(term: &Term, key: &str) -> Option<Vec<String>> {
        if let Term::Map(map) = term {
            if let Some(Term::List(list)) = map_get(map, key) {
                return Some(list.elements.iter().filter_map(term_to_string).collect());
            }
        }
        None
    }

    const COMPILE_METHOD_CLASS: &str = "Actor subclass: EventStore\n  state: events = #{}\n\n  initialize -> Nil =>\n    self.events := #{}";

    #[test]
    fn diagnostics_method_mode_accepts_a_bare_method_body() {
        // BT-2569: the System Browser method editor sends a BARE method body. Under
        // the default (expression) grammar the `=>` body separator is not a valid
        // top-level token, so the parser reports a false
        // `Unexpected token: expected expression, found ⇒` — the bug. With
        // `mode: "method"` the body parses with `parse_method` and is clean.
        let body = "decrement => self.value := self.value - 1";

        // Default (expression) grammar: the bug — a false parse error on `=>`.
        let expr_request = Term::from(Map::from([
            (atom("command"), atom("diagnostics")),
            (atom("source"), binary(body)),
        ]));
        let expr_response = handle_request(&expr_request);
        assert_eq!(response_status(&expr_response).as_deref(), Some("ok"));
        assert!(
            !response_diagnostics(&expr_response)
                .expect("diagnostics")
                .elements
                .is_empty(),
            "expression grammar should reject a bare method body (regression guard)"
        );

        // Method grammar: the same body parses clean — no diagnostics.
        let method_request = Term::from(Map::from([
            (atom("command"), atom("diagnostics")),
            (atom("source"), binary(body)),
            (atom("mode"), binary("method")),
        ]));
        let method_response = handle_request(&method_request);
        assert_eq!(response_status(&method_response).as_deref(), Some("ok"));
        assert!(
            response_diagnostics(&method_response)
                .expect("diagnostics")
                .elements
                .is_empty(),
            "method grammar should accept a valid bare method body: {method_response:?}"
        );
    }

    #[test]
    fn diagnostics_method_mode_still_reports_a_broken_body() {
        // The parse-only method path is not a no-op: a genuinely broken body (`:=`
        // with no right-hand side) still produces diagnostics in method mode.
        let request = Term::from(Map::from([
            (atom("command"), atom("diagnostics")),
            (atom("source"), binary("decrement => self.value :=")),
            (atom("mode"), binary("method")),
        ]));
        let response = handle_request(&request);
        assert_eq!(response_status(&response).as_deref(), Some("ok"));
        assert!(
            !response_diagnostics(&response)
                .expect("diagnostics")
                .elements
                .is_empty(),
            "a broken method body should still produce diagnostics: {response:?}"
        );
    }

    #[test]
    fn compile_method_preserves_source_and_compiles() {
        // A `// --- … ---` banner over a `///` doc block over the header. The
        // per-method canonical `method_source` keeps the `///` doc block but drops
        // the leading `//` banner — the banner is inter-method file structure, not
        // part of the method's edit unit, and the byte span excludes it (BT-2594).
        // It is preserved in the file via `merged_class_source` (whole-file unparse).
        let method_source = "// --- Execution CRUD ---\n\n/// Store a new workflow execution.\n/// Raises if the workflowId already exists.\ncreateExecution: execution :: Object -> Object =>\n  execution";
        let request = Term::from(Map::from([
            (atom("command"), atom("compile_method")),
            (atom("class_source"), binary(COMPILE_METHOD_CLASS)),
            (atom("method_source"), binary(method_source)),
            (atom("is_class_method"), atom("false")),
        ]));
        let response = handle_request(&request);
        assert_eq!(
            response_status(&response).as_deref(),
            Some("ok"),
            "resp: {response:?}"
        );
        assert_eq!(
            response_field_str(&response, "selector").as_deref(),
            Some("createExecution:")
        );
        let ms = response_field_str(&response, "method_source").expect("method_source");
        assert!(
            !ms.contains("--- Execution CRUD ---"),
            "leading `//` banner should be dropped from per-method source: {ms}"
        );
        assert!(
            ms.contains("/// Store a new workflow execution."),
            "first doc line lost: {ms}"
        );
        assert!(
            ms.contains("/// Raises if the workflowId already exists."),
            "doc line lost: {ms}"
        );
        assert!(
            response_field_str(&response, "core_erlang").is_some_and(|c| !c.is_empty()),
            "no core erlang emitted"
        );
        // The merged class source (stored for the next patch) is a clean inline
        // class that now contains the new method — no `>>` accumulation.
        let merged = response_field_str(&response, "merged_class_source").expect("merged");
        assert!(
            merged.contains("createExecution:"),
            "merged class missing the new method:\n{merged}"
        );
        assert!(
            merged.contains("Actor subclass: EventStore"),
            "merged class lost its header:\n{merged}"
        );
        assert!(
            !merged.contains(">>"),
            "merged class should be inline, not `>>` extensions:\n{merged}"
        );
    }

    #[test]
    fn compile_method_response_carries_declared_signature() {
        // ADR 0105 Phase 1 (BT-2777): the compile_method response must carry the
        // patched method's declared return/param types so the workspace can
        // capture them into the signature-generation store before install.
        let request = Term::from(Map::from([
            (atom("command"), atom("compile_method")),
            (atom("class_source"), binary(COMPILE_METHOD_CLASS)),
            (
                atom("method_source"),
                binary("touch: n :: Integer -> Object =>\n  self"),
            ),
            (atom("is_class_method"), atom("false")),
        ]));
        let response = handle_request(&request);
        assert_eq!(
            response_status(&response).as_deref(),
            Some("ok"),
            "resp: {response:?}"
        );
        assert_eq!(
            response_field_str(&response, "return_type").as_deref(),
            Some("Object")
        );
        assert_eq!(
            response_field_str_list(&response, "param_types").as_deref(),
            Some(&["Integer".to_string()][..])
        );
    }

    #[test]
    fn compile_method_response_reports_dynamic_for_unannotated_signature() {
        // No return/param annotations on the patched method → both fields report
        // the "Dynamic" sentinel (never omitted, so the diff always compares two
        // values).
        let request = Term::from(Map::from([
            (atom("command"), atom("compile_method")),
            (atom("class_source"), binary(COMPILE_METHOD_CLASS)),
            (atom("method_source"), binary("touch: n =>\n  self")),
            (atom("is_class_method"), atom("false")),
        ]));
        let response = handle_request(&request);
        assert_eq!(response_status(&response).as_deref(), Some("ok"));
        assert_eq!(
            response_field_str(&response, "return_type").as_deref(),
            Some("Dynamic")
        );
        assert_eq!(
            response_field_str_list(&response, "param_types").as_deref(),
            Some(&["Dynamic".to_string()][..])
        );
    }

    #[test]
    fn compile_method_honors_module_name_override() {
        // Package-qualified override must flow into the compiled module name
        // (this is what keeps EventStore as bt@exdura@event_store on a patch).
        let request = Term::from(Map::from([
            (atom("command"), atom("compile_method")),
            (atom("class_source"), binary(COMPILE_METHOD_CLASS)),
            (atom("method_source"), binary("touch => self.events")),
            (atom("is_class_method"), atom("false")),
            (atom("module_name"), binary("bt@exdura@event_store")),
        ]));
        let response = handle_request(&request);
        assert_eq!(response_status(&response).as_deref(), Some("ok"));
        assert_eq!(
            response_field_str(&response, "module_name").as_deref(),
            Some("bt@exdura@event_store")
        );
    }

    #[test]
    fn compile_method_rejects_multi_class_source_cleanly() {
        // One-class-per-file (ADR 0040) is enforced by semantic analysis, so a
        // multi-class `class_source` can never load/compile through any path.
        // compile_method must surface that as a clean error (not silently drop a
        // sibling class from the merged source) — the caller then leaves the
        // stored source untouched, so there is no corruption.
        let two = "Actor subclass: Alpha\n  state: a = 1\n\n  va => self.a\n\n\
                   Actor subclass: Beta\n  state: b = 2\n\n  vb => self.b";
        let request = Term::from(Map::from([
            (atom("command"), atom("compile_method")),
            (atom("class_source"), binary(two)),
            (atom("class_name"), binary("Alpha")),
            (atom("method_source"), binary("bumped => self.a + 1")),
            (atom("is_class_method"), atom("false")),
        ]));
        let response = handle_request(&request);
        assert_eq!(response_status(&response).as_deref(), Some("error"));
    }

    #[test]
    fn compile_method_replaces_class_side_method_without_duplicating() {
        // BT-2563 #3: the instance/class side is chosen by which method list the
        // backend merges into (driven by `is_class_method`), NOT by `MethodKind`.
        // A class-side patch must REPLACE the existing class method, never push a
        // duplicate alongside it — there is no "kind trap".
        let class_source = "Actor subclass: Counter\n  state: count = 0\n\n  class create -> Nil =>\n    Counter new\n\n  increment => count := count + 1";
        let request = Term::from(Map::from([
            (atom("command"), atom("compile_method")),
            (atom("class_source"), binary(class_source)),
            (atom("method_source"), binary("create -> Nil =>\n  42")),
            (atom("is_class_method"), atom("true")),
        ]));
        let response = handle_request(&request);
        assert_eq!(
            response_status(&response).as_deref(),
            Some("ok"),
            "resp: {response:?}"
        );
        assert_eq!(
            response_field_str(&response, "selector").as_deref(),
            Some("create")
        );
        let merged = response_field_str(&response, "merged_class_source").expect("merged");
        // Exactly one class-side `create` — replaced in place, not duplicated.
        assert_eq!(
            merged.matches("class create").count(),
            1,
            "class-side method duplicated instead of replaced:\n{merged}"
        );
        assert!(
            merged.contains("42"),
            "class-side method body not updated:\n{merged}"
        );
        // The instance method is untouched.
        assert!(
            merged.contains("increment"),
            "instance method dropped:\n{merged}"
        );
    }

    #[test]
    fn compile_method_annotates_patched_method_at_its_merged_line() {
        // BT-2563 #1: the patched method is parsed standalone, so its raw span
        // indexes into the bare snippet (line 1). Codegen must annotate the
        // method's message sends with the line they occupy in the MERGED class
        // source (the send carries the BEAM stacktrace line), not line 1.
        let request = Term::from(Map::from([
            (atom("command"), atom("compile_method")),
            (atom("class_source"), binary(COMPILE_METHOD_CLASS)),
            (
                atom("method_source"),
                binary("touch => self.events isEmpty"),
            ),
            (atom("is_class_method"), atom("false")),
        ]));
        let response = handle_request(&request);
        assert_eq!(
            response_status(&response).as_deref(),
            Some("ok"),
            "resp: {response:?}"
        );
        let merged = response_field_str(&response, "merged_class_source").expect("merged");
        let touch_line = merged
            .lines()
            .position(|l| l.contains("touch"))
            .map(|i| i + 1)
            .expect("touch present in merged source");
        assert!(
            touch_line > 1,
            "expected the patched method below line 1 in the merged source:\n{merged}"
        );
        let core = response_field_str(&response, "core_erlang").expect("core erlang");
        // Bare line annotation (no source_path in this request): `-| [<line>]`.
        assert!(
            core.contains(&format!(" -| [{touch_line}]")),
            "patched method not annotated at its merged line {touch_line}:\n{core}"
        );
    }

    #[test]
    fn compile_method_class_context_diagnostic_resolves_against_merged_source() {
        // BT-2563 #2: a post-merge semantic diagnostic whose span lands in the
        // CLASS (not the patched method body) must resolve against the merged
        // class source, not the short `method_source`. The one-class-per-file
        // error points at the second class declaration, several lines into the
        // merged source; the buggy path rendered it against the 1-line method
        // body and clamped it to line 1.
        let two = "Actor subclass: Alpha\n  state: a = 1\n\n  va => self.a\n\n\
                   Actor subclass: Beta\n  state: b = 2\n\n  vb => self.b";
        let request = Term::from(Map::from([
            (atom("command"), atom("compile_method")),
            (atom("class_source"), binary(two)),
            (atom("class_name"), binary("Alpha")),
            (atom("method_source"), binary("bumped => self.a + 1")),
            (atom("is_class_method"), atom("false")),
        ]));
        let response = handle_request(&request);
        assert_eq!(response_status(&response).as_deref(), Some("error"));
        let list = response_diagnostics(&response).expect("diagnostics");
        let line = list.elements.iter().find_map(|d| {
            if let Term::Map(m) = d {
                if let Some(Term::FixInteger(i)) = map_get(m, "line") {
                    return Some(i.value);
                }
            }
            None
        });
        // The second class declaration sits several lines into the merged source
        // (`Beta` is below all of `Alpha` plus the patched method), so a correctly
        // resolved span lands well past the 1-line method body. The buggy path
        // clamped this class-context span into the method body (line 1); requiring
        // it to be clearly past the method body — without hardcoding the exact line,
        // which depends on the unparser's blank-line output — guards the regression.
        assert!(
            line.is_some_and(|l| l >= 5),
            "class-context diagnostic clamped into the method body instead of \
             resolving against the merged source: {response:?}"
        );
    }

    #[test]
    fn compile_method_body_diagnostic_is_method_relative() {
        // BT-2563 #2: a semantic error INSIDE the patched method body must report a
        // line relative to the method snippet the user is editing — not its absolute
        // line in the merged class. `probe` is appended below the multi-line
        // `initialize` method, so its absolute merged line is well past 2; the
        // undefined-variable error on the method's second line must still report
        // line 2 (`beamtalk_repl_compiler:format_diagnostic_text/1` renders it as
        // "Line 2: ...", matching the method editor's view).
        let request = Term::from(Map::from([
            (atom("command"), atom("compile_method")),
            (atom("class_source"), binary(COMPILE_METHOD_CLASS)),
            (
                atom("method_source"),
                binary("probe =>\n    undefinedLocal\n    self.events"),
            ),
            (atom("is_class_method"), atom("false")),
        ]));
        let response = handle_request(&request);
        assert_eq!(
            response_status(&response).as_deref(),
            Some("error"),
            "resp: {response:?}"
        );
        let list = response_diagnostics(&response).expect("diagnostics");
        let (msg, line) = list
            .elements
            .iter()
            .find_map(|d| {
                if let Term::Map(m) = d {
                    let msg = match map_get(m, "message") {
                        Some(Term::Binary(b)) => String::from_utf8_lossy(&b.bytes).into_owned(),
                        _ => return None,
                    };
                    let line = match map_get(m, "line") {
                        Some(Term::FixInteger(i)) => i.value,
                        _ => return None,
                    };
                    Some((msg, line))
                } else {
                    None
                }
            })
            .expect("a structured diagnostic with message + line");
        assert!(
            msg.contains("undefinedLocal"),
            "expected the undefined-variable diagnostic, got: {msg}"
        );
        // Method-relative: the error is on the method's second line. The absolute
        // line in the merged class is larger (the method is appended below
        // `initialize`); reporting 2 proves the span was rebased into method space.
        assert_eq!(
            line, 2,
            "method-body diagnostic not reported method-relative: {response:?}"
        );
    }

    #[test]
    fn compile_method_rejects_non_method_source() {
        let request = Term::from(Map::from([
            (atom("command"), atom("compile_method")),
            (atom("class_source"), binary(COMPILE_METHOD_CLASS)),
            (atom("method_source"), binary("1 + 1")),
            (atom("is_class_method"), atom("false")),
        ]));
        let response = handle_request(&request);
        assert_eq!(response_status(&response).as_deref(), Some("error"));
    }

    /// Near-valid Beamtalk source fragments (reuses patterns from parser property tests).
    const FRAGMENTS: &[&str] = &[
        "42",
        "3.14",
        "\"hello\"",
        "true",
        "false",
        "nil",
        "x := 42",
        "x + y",
        "arr at: 1",
        "[:x | x + 1]",
        "(3 + 4)",
        "^42",
        "self",
        "#(1, 2, 3)",
        "#{#a => 1}",
        "Object subclass: Counter\n  state: count = 0\n  increment => count := count + 1",
        "3 timesRepeat: [x := x + 1]",
        "#[first, ...rest] := #[1, 2, 3]",
        "[1] ensure: [nil]",
        "x match: { 1 => \"one\", _ => \"other\" }",
    ];

    /// Generates a near-valid Beamtalk input using one of several mutation strategies.
    fn near_valid_beamtalk() -> impl Strategy<Value = String> {
        prop_oneof![
            // Valid fragments
            prop::sample::select(FRAGMENTS).prop_map(std::string::ToString::to_string),
            // Truncated valid expressions
            prop::sample::select(FRAGMENTS).prop_flat_map(|s| {
                let len = s.len();
                if len <= 1 {
                    Just(s.to_string()).boxed()
                } else {
                    (1..len)
                        .prop_map(move |cut| {
                            let safe_cut = s.floor_char_boundary(cut);
                            if safe_cut == 0 {
                                s.to_string()
                            } else {
                                s[..safe_cut].to_string()
                            }
                        })
                        .boxed()
                }
            }),
            // Mismatched brackets
            prop::sample::select(FRAGMENTS).prop_map(|s| {
                s.chars()
                    .map(|ch| match ch {
                        '[' => '(',
                        ']' => '}',
                        '(' => '[',
                        _ => ch,
                    })
                    .collect()
            }),
        ]
    }

    /// Default is 512 cases for standard CI; override via `PROPTEST_CASES` env var
    /// for nightly extended runs (e.g., `PROPTEST_CASES=10000`).
    fn proptest_config() -> ProptestConfig {
        let default = ProptestConfig::default();
        ProptestConfig {
            // Use at least 512 cases, but allow PROPTEST_CASES to increase beyond that
            cases: default.cases.max(512),
            ..default
        }
    }

    proptest! {
        #![proptest_config(proptest_config())]

        /// Property 1a: `handle_compile` never panics on arbitrary string input.
        #[test]
        fn compile_never_panics(input in "\\PC{0,500}") {
            let request = compile_request(&input);
            let response = handle_compile(&request);
            let status = response_status(&response);
            prop_assert!(
                status.is_some(),
                "Response must have a status field for input: {:?}",
                input,
            );
            let status = status.unwrap();
            prop_assert!(
                status == "ok" || status == "error",
                "Status must be 'ok' or 'error', got {:?} for input: {:?}",
                status,
                input,
            );
        }

        /// Property 1b: `handle_compile_expression` never panics on arbitrary string input.
        #[test]
        fn compile_expression_never_panics(input in "\\PC{0,500}") {
            let request = compile_expression_request(&input);
            let response = handle_compile_expression(&request);
            let status = response_status(&response);
            prop_assert!(
                status.is_some(),
                "Response must have a status field for input: {:?}",
                input,
            );
            let status = status.unwrap();
            prop_assert!(
                status == "ok" || status == "error",
                "Status must be 'ok' or 'error', got {:?} for input: {:?}",
                status,
                input,
            );
        }

        /// Property 1c: `handle_compile` never panics on near-valid structured input.
        #[test]
        fn compile_never_panics_near_valid(input in near_valid_beamtalk()) {
            let request = compile_request(&input);
            let response = handle_compile(&request);
            let status = response_status(&response);
            prop_assert!(status.is_some());
        }

        /// Property 1d: `handle_compile_expression` never panics on near-valid structured input.
        #[test]
        fn compile_expression_never_panics_near_valid(input in near_valid_beamtalk()) {
            let request = compile_expression_request(&input);
            let response = handle_compile_expression(&request);
            let status = response_status(&response);
            prop_assert!(status.is_some());
        }

        /// Property 2: Error responses have non-empty diagnostics.
        ///
        /// When status is "error", the diagnostics list must be non-empty.
        #[test]
        fn error_responses_have_diagnostics(input in "\\PC{0,500}") {
            // Test both compile paths
            for response in [
                handle_compile(&compile_request(&input)),
                handle_compile_expression(&compile_expression_request(&input)),
            ] {
                if response_status(&response).as_deref() == Some("error") {
                    let diags = response_diagnostics(&response);
                    prop_assert!(
                        diags.is_some(),
                        "Error response must have 'diagnostics' field for input: {:?}",
                        input,
                    );
                    prop_assert!(
                        !diags.unwrap().elements.is_empty(),
                        "Error diagnostics must be non-empty for input: {:?}",
                        input,
                    );
                }
            }
        }

        /// Property 3: Diagnostic entries are structured maps with a non-empty message.
        ///
        /// BT-1235: Every diagnostic in an error response must be a Map with a
        /// non-empty `message` binary field (and optionally `line` and `hint`).
        #[test]
        fn diagnostics_are_nonempty_strings(input in "\\PC{0,500}") {
            for response in [
                handle_compile(&compile_request(&input)),
                handle_compile_expression(&compile_expression_request(&input)),
            ] {
                if response_status(&response).as_deref() == Some("error") {
                    if let Some(diags) = response_diagnostics(&response) {
                        for (i, diag_term) in diags.elements.iter().enumerate() {
                            let Term::Map(diag_map) = diag_term else {
                                prop_assert!(
                                    false,
                                    "Diagnostic {} is not a Map term for input: {:?}",
                                    i,
                                    input,
                                );
                                continue;
                            };
                            let msg_term = diag_map.map.get(&atom("message"));
                            prop_assert!(
                                msg_term.is_some(),
                                "Diagnostic {} has no 'message' field for input: {:?}",
                                i,
                                input,
                            );
                            if let Some(Term::Binary(b)) = msg_term {
                                let text = String::from_utf8(b.bytes.clone());
                                prop_assert!(
                                    text.is_ok(),
                                    "Diagnostic {} message is not valid UTF-8 for input: {:?}",
                                    i,
                                    input,
                                );
                                prop_assert!(
                                    !text.unwrap().is_empty(),
                                    "Diagnostic {} message is empty for input: {:?}",
                                    i,
                                    input,
                                );
                            } else {
                                prop_assert!(
                                    false,
                                    "Diagnostic {} message is not a Binary for input: {:?}",
                                    i,
                                    input,
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    // -------------------------------------------------------------------
    // ADR 0108 Phase 8 (BT-2902): `type Name = ...` REPL declarations
    // -------------------------------------------------------------------

    /// True when a response field is present and is the atom `undefined`
    /// (Rust's `None`, e.g. an alias declaration with no doc comment).
    fn response_field_is_undefined_atom(term: &Term, key: &str) -> bool {
        let Term::Map(map) = term else {
            return false;
        };
        matches!(map_get(map, key), Some(Term::Atom(a)) if a.name == "undefined")
    }

    #[test]
    fn type_alias_declaration_without_doc_comment() {
        let response = handle_compile_expression(&compile_expression_request(
            "type Direction = #north | #south | #east | #west",
        ));
        assert_eq!(
            response_status(&response).as_deref(),
            Some("ok"),
            "resp: {response:?}"
        );
        let Term::Map(m) = &response else {
            panic!("expected a map response, got: {response:?}");
        };
        assert_eq!(map_get(m, "kind"), Some(&atom("type_alias_definition")));
        assert_eq!(
            response_field_str(&response, "alias_name").as_deref(),
            Some("Direction")
        );
        assert_eq!(
            response_field_str(&response, "expansion").as_deref(),
            Some("#north | #south | #east | #west")
        );
        assert!(
            response_field_is_undefined_atom(&response, "doc_comment"),
            "no doc comment on the declaration must round-trip as `undefined`, \
             not an empty string: {response:?}"
        );
    }

    #[test]
    fn type_alias_declaration_with_doc_comment() {
        let response = handle_compile_expression(&compile_expression_request(
            "/// How a supervised child restarts after exit.\n\
             type RestartStrategy = #temporary | #transient | #permanent",
        ));
        assert_eq!(
            response_status(&response).as_deref(),
            Some("ok"),
            "resp: {response:?}"
        );
        assert_eq!(
            response_field_str(&response, "alias_name").as_deref(),
            Some("RestartStrategy")
        );
        assert_eq!(
            response_field_str(&response, "expansion").as_deref(),
            Some("#temporary | #transient | #permanent")
        );
        assert_eq!(
            response_field_str(&response, "doc_comment").as_deref(),
            Some("How a supervised child restarts after exit.")
        );
    }

    #[test]
    fn multiple_type_alias_declarations_in_one_turn_is_an_error() {
        let response = handle_compile_expression(&compile_expression_request(
            "type A = Integer\ntype B = String",
        ));
        assert_eq!(
            response_status(&response).as_deref(),
            Some("error"),
            "resp: {response:?}"
        );
    }

    /// `known_type_aliases` (ADR 0108 Phase 8) makes an alias declared in an
    /// *earlier* REPL turn resolvable in the current turn's `::` annotation
    /// — the cross-turn persistence mechanism this issue adds, since an
    /// alias has no live BEAM artifact for the session to recover it from
    /// the way a REPL-declared class is recovered. Asserts on the
    /// `matchExhaustive:` diagnostic text (not just "no unresolved-type
    /// error") so this also proves `Direction` expanded to the exact
    /// closed singleton union — not merely a silently-accepted unknown name.
    #[test]
    fn known_type_aliases_resolves_alias_from_earlier_turn() {
        let request = Map::from([
            (atom("command"), atom("compile_expression")),
            (
                atom("source"),
                binary(
                    "scrutinee :: Direction := #north. \
                     scrutinee matchExhaustive: [#north -> 0; #south -> 1; #east -> 2]",
                ),
            ),
            (atom("module"), binary("bt@test_module")),
            (
                atom("known_type_aliases"),
                Term::from(List::from(vec![binary(
                    "type Direction = #north | #south | #east | #west",
                )])),
            ),
        ]);
        let response = handle_compile_expression(&request);
        assert_eq!(
            response_status(&response).as_deref(),
            Some("error"),
            "resp: {response:?}"
        );
        let diags = response_diagnostics(&response).expect("diagnostics");
        let found = diags.elements.iter().any(|d| {
            let Term::Map(m) = d else { return false };
            map_get(m, "message")
                .and_then(term_to_string)
                .is_some_and(|msg| {
                    msg.contains("non-exhaustive matchExhaustive:") && msg.contains("#west")
                })
        });
        assert!(
            found,
            "expected a non-exhaustive matchExhaustive: `#west` diagnostic \
             (proves Direction resolved to the closed union, not an unresolved-type \
             fallback), got: {response:?}"
        );
    }

    /// The `known_type_aliases` round trip (declare → `unparse_type_annotation_display`
    /// → resend as `type Name = <expansion>` → reparse) is not limited to simple
    /// singleton unions — ADR 0108 Semantics explicitly allows any `TypeAnnotation`
    /// on the RHS, including `\`/`&` forms (`type PublicTag = Symbol \ (#reserved |
    /// #internal)`). Pins that a `Difference` RHS survives the same declare-then-use
    /// two-turn round trip `known_type_aliases_resolves_alias_from_earlier_turn`
    /// exercises for a plain union, since the reparse path
    /// (`extract_known_type_aliases`) silently drops any alias whose expansion text
    /// fails to reparse — a regression here would fail closed (the alias just
    /// vanishes) rather than loudly, so it needs its own pin.
    #[test]
    fn known_type_aliases_round_trips_a_difference_rhs() {
        let declare_response = handle_compile_expression(&compile_expression_request(
            "type PublicTag = Symbol \\ (#reserved | #internal)",
        ));
        assert_eq!(
            response_status(&declare_response).as_deref(),
            Some("ok"),
            "resp: {declare_response:?}"
        );
        let expansion = response_field_str(&declare_response, "expansion")
            .expect("expansion field must be present");

        let request = Map::from([
            (atom("command"), atom("compile_expression")),
            (atom("source"), binary("tag :: PublicTag := #anything")),
            (atom("module"), binary("bt@test_module")),
            (
                atom("known_type_aliases"),
                Term::from(List::from(vec![binary(&format!(
                    "type PublicTag = {expansion}"
                ))])),
            ),
        ]);
        let response = handle_compile_expression(&request);
        assert_eq!(
            response_status(&response).as_deref(),
            Some("ok"),
            "a Difference-RHS alias declared in an earlier turn must still resolve \
             (not silently vanish) when referenced in a later turn's `::` annotation: \
             {response:?}"
        );
    }
}
