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

/// BT-3076: Deserialize a single `method_info`/`class_method_info`
/// `return_type`/`param_types` entry — the `MetaTypeRepr` wire shape codegen
/// emits (`crate::codegen::core_erlang::gen_server::methods::MetaTypeRepr`'s
/// `meta_type_repr_doc`, beamtalk-core) — into a structured
/// [`DeclaredType`](beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType).
///
/// This is THE single place strings become types crossing the ETF boundary:
/// before BT-3076, `term_to_atom` only matched a bare `Term::Atom`, so every
/// tagged tuple (`{type_param, ...}`, `{generic, ...}`, and now `{union,
/// ...}` / `{singleton, ...}`) silently degraded to `None` — a generic
/// return type crossing the compiler port lost its structure entirely (the
/// bug this stage fixes; see `test_generic_return_type_survives_etf_meta`).
///
/// Wire shapes:
/// - `'none'` → `None` (the `Option` wrapper, not `DeclaredType::None` — no
///   such variant exists).
/// - A bare atom (e.g. `'Integer'`) → [`DeclaredType::parse`] — handles
///   legacy artifacts (pre-BT-3076 compiled modules only ever emitted flat
///   atoms) and the old return-type writeback union strings the same way
///   `resolve_type_string` used to. `parse` also recognises the flat
///   self-type renderings codegen still emits for method signatures
///   (`'Self'`, `'Self class'`, `'<Name> class'` — the `MetaTypeRepr::Atom`
///   fallback), so those round-trip structurally as
///   `SelfType`/`SelfClass`/`ClassOf` instead of decaying to `Simple` (see
///   `self_type_return_survives_etf_meta`).
/// - `{'type_param', Name, _Index}` → `DeclaredType::Simple(Name)` — the
///   codegen-internal `Index` (class-param position, or `-1` for
///   method-local) has no `DeclaredType` counterpart; a bare type-param name
///   round-trips as an ordinary `Simple`, exactly like a `TypeAnnotation::
///   Simple` naming a generic param does everywhere else in the checker.
/// - `{'generic', Base, [Params]}` → `DeclaredType::Generic { base, parameters }`,
///   recursively.
/// - `{'union', [Members]}` → `DeclaredType::Union(members)`, recursively
///   (BT-3076 wire extension).
/// - `{'singleton', Name}` → `DeclaredType::Singleton(Name)` (BT-3076 wire
///   extension).
/// - Anything else (malformed/unknown tag) → `None`, matching this parser's
///   existing graceful-degradation convention.
fn term_to_declared_type(
    term: &Term,
) -> Option<beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType> {
    use beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType;

    match term {
        Term::Atom(_) => {
            let name = term_to_atom(term)?;
            if name == "none" {
                None
            } else {
                Some(DeclaredType::parse(&name))
            }
        }
        Term::Tuple(t) => {
            let tag = t.elements.first().and_then(term_to_atom)?;
            match tag.as_str() {
                "type_param" => {
                    let name = t.elements.get(1).and_then(term_to_atom)?;
                    Some(DeclaredType::simple(name))
                }
                "generic" => {
                    let base = t.elements.get(1).and_then(term_to_atom)?;
                    let parameters = t
                        .elements
                        .get(2)
                        .map(term_to_declared_type_list)
                        .unwrap_or_default();
                    Some(DeclaredType::generic(base, parameters))
                }
                "union" => {
                    let members = t
                        .elements
                        .get(1)
                        .map(term_to_declared_type_list)
                        .unwrap_or_default();
                    Some(DeclaredType::union(members))
                }
                "singleton" => {
                    let name = t.elements.get(1).and_then(term_to_atom)?;
                    Some(DeclaredType::singleton(name))
                }
                _ => None,
            }
        }
        _ => None,
    }
}

/// Deserializes a `[MetaTypeRepr, ...]` ETF list — each element via
/// [`term_to_declared_type`] — for `Generic`/`Union`'s nested parameters.
/// Malformed elements are silently dropped (matches this file's existing
/// graceful-degradation convention throughout).
fn term_to_declared_type_list(
    term: &Term,
) -> Vec<beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType> {
    match term {
        Term::List(list) => list
            .elements
            .iter()
            .filter_map(term_to_declared_type)
            .collect(),
        _ => vec![],
    }
}

/// BT-3076: Deserialize an atom→`DeclaredType` map (for `field_types` /
/// `ClassInfo::state_types`).
///
/// `field_types` is still emitted by codegen as flat atoms (`'Integer'`, or
/// `'none'` for an untyped field — `meta_field_types_map`, beamtalk-core) —
/// unlike `method_info`/`class_method_info`, it never carries the tagged
/// `MetaTypeRepr` tuples, so every value here goes through
/// [`DeclaredType::parse`] rather than the full [`term_to_declared_type`].
/// The `'none'` sentinel is intentionally *not* filtered out here — it
/// round-trips as `DeclaredType::Simple("none")`, preserving this map's
/// pre-BT-3076 behaviour verbatim (the old atom→atom reader never
/// special-cased it either) rather than fixing that latent quirk as a
/// drive-by change.
fn term_to_declared_type_atom_map(
    term: &Term,
) -> std::collections::HashMap<
    ecow::EcoString,
    beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType,
> {
    use beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType;

    match term {
        Term::Map(m) => m
            .map
            .iter()
            .filter_map(|(k, v)| {
                let key = term_to_atom(k)?;
                let val = term_to_atom(v)?;
                Some((
                    ecow::EcoString::from(key.as_str()),
                    DeclaredType::parse(&val),
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
            let return_type = map_get(info_map, "return_type").and_then(term_to_declared_type);
            let param_types = match map_get(info_map, "param_types") {
                Some(Term::List(list)) => list.elements.iter().map(term_to_declared_type).collect(),
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
        .map(term_to_declared_type_atom_map)
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

/// Parse a `required_methods`/`required_class_methods` ETF list
/// (`beamtalk_protocol_registry:register_protocol/1`'s wire shape —
/// `[#{selector => atom(), arity => integer()}, ...]`) into
/// `ProtocolMethodRequirement`s (BT-3473).
///
/// Selector/arity only: the live image's ambient protocol cache never carries
/// the original `::`-annotated parameter/return type text (that lives only in
/// the protocol's defining source file, which this port doesn't have access
/// to for a cross-file re-check), so `return_type`/`param_types` are always
/// `None`. Good enough for the escape hatches that key off protocol *names*
/// (`is_protocol_type`, DNU suppression via `ClassHierarchy::has_class`) —
/// see `parse_protocol_info_from_meta_term`'s doc.
fn parse_protocol_method_requirements(
    term: Option<&Term>,
) -> Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolMethodRequirement> {
    use beamtalk_core::semantic_analysis::protocol_registry::ProtocolMethodRequirement;

    let Some(Term::List(list)) = term else {
        return vec![];
    };
    list.elements
        .iter()
        .filter_map(|entry| {
            let Term::Map(m) = entry else { return None };
            let selector = map_get(m, "selector").and_then(term_to_atom)?;
            let arity = map_get(m, "arity").and_then(term_to_usize)?;
            Some(ProtocolMethodRequirement {
                selector: ecow::EcoString::from(selector.as_str()),
                arity,
                return_type: None,
                param_types: vec![None; arity],
            })
        })
        .collect()
}

/// Deserialize a single ambient protocol-registry entry (BT-3473) —
/// `beamtalk_protocol_registry:register_protocol/1`'s `Info` map, threaded
/// through `beamtalk_compiler_server`'s `protocols` cache the same way
/// `class_hierarchy` threads `register_class/2`'s — into a `ProtocolInfo`.
///
/// Returns `None` if `term` is not a map.
fn parse_protocol_info_from_meta_term(
    protocol_name: &str,
    term: &Term,
) -> Option<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo> {
    use beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo;
    use beamtalk_core::source_analysis::Span;

    let Term::Map(m) = term else { return None };

    let type_params = map_get(m, "type_params")
        .map(term_to_atom_list)
        .unwrap_or_default();
    // No wire representation for bounds on this channel (BT-3473 scope is
    // suppressing false positives via name/selector recognition, not full
    // generic-bounds re-derivation) — unbounded for every type param.
    let type_param_bounds = vec![None; type_params.len()];
    let extending = map_get(m, "extending")
        .and_then(term_to_atom)
        .and_then(|s| {
            if s == "undefined" {
                None
            } else {
                Some(ecow::EcoString::from(s.as_str()))
            }
        });
    let methods = parse_protocol_method_requirements(map_get(m, "required_methods"));
    let class_methods = parse_protocol_method_requirements(map_get(m, "required_class_methods"));

    Some(ProtocolInfo {
        name: ecow::EcoString::from(protocol_name),
        type_params,
        type_param_bounds,
        extending,
        methods,
        class_methods,
        // Synthetic entry — there is no source span in the live image to
        // point diagnostics at (mirrors ClassInfo's BEAM-metadata-derived
        // entries, which carry no span either).
        span: Span::new(0, 0),
    })
}

/// Parse a `protocol_registry` ETF term (`#{atom() => meta_map()}`,
/// `beamtalk_compiler_server`'s ambient `protocols` cache) into
/// `Vec<ProtocolInfo>` (BT-3473). Degrades gracefully on malformed entries
/// (silently skipped), mirroring `parse_class_hierarchy_from_term`.
fn parse_protocol_registry_from_term(
    term: &Term,
) -> Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo> {
    let Term::Map(m) = term else { return vec![] };
    m.map
        .iter()
        .filter_map(|(name_term, meta_term)| {
            let protocol_name = term_to_atom(name_term)?;
            parse_protocol_info_from_meta_term(&protocol_name, meta_term)
        })
        .collect()
}

/// Extract an optional `protocol_registry` field, returning `Vec<ProtocolInfo>`
/// (BT-3473). Mirrors `extract_class_hierarchy`.
fn extract_protocol_registry(
    request: &Map,
) -> Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo> {
    match map_get(request, "protocol_registry") {
        None => vec![],
        Some(term) => parse_protocol_registry_from_term(term),
    }
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
fn format_codegen_error(e: &beamtalk_codegen::core_erlang::CodeGenError, source: &str) -> String {
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
fn diagnostic_error_response(
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
/// in-range (BT-2563 #2).
fn compile_method_diagnostic_response(
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
/// Delegates to [`beamtalk_core::compilation::load_diagnostics_table_for_root`]
/// for the lenient read-parse-or-empty semantics: missing manifest → empty
/// table (silent), non-`NotFound` I/O errors (permissions, EISDIR, etc.) →
/// `WARN` log + empty table, parse failure → `WARN` log + empty table / Rule 1
/// defaults. The `debug!` log on a non-empty result is compiler-port-specific
/// telemetry kept here in the caller.
///
/// Pure function of `root` (no global state) so it is directly unit-testable
/// without touching the process's real working directory.
fn load_diagnostics_overrides_from(
    root: &std::path::Path,
) -> beamtalk_core::compilation::DiagnosticsTable {
    let table = beamtalk_core::compilation::load_diagnostics_table_for_root(root);
    if !table.is_empty() {
        tracing::debug!(
            count = table.len(),
            "Loaded [diagnostics] severity override(s) from beamtalk.toml"
        );
    }
    table
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
/// Returns `Ok((module, warnings, analysis))` on success, or
/// `Err(response_term)` containing a formatted `diagnostic_error_response`
/// that the caller should return directly. `analysis` is the full
/// [`AnalysisResult`](beamtalk_core::semantic_analysis::AnalysisResult) this
/// function's own semantic-analysis pass produced (BT-3123) — callers that go
/// on to run codegen for the same module thread it into
/// `CodegenOptions::with_analysis` instead of letting codegen re-derive the
/// class hierarchy, semantic facts, and inferred method return types from
/// scratch. `analysis.referenced_aliases` is the alias-dependency set
/// (ADR 0108 hot-reload re-check trigger, BT-2899) callers used to receive
/// as this tuple's third element directly.
///
/// `pre_loaded_aliases` (ADR 0108 Phase 8, BT-2902) carries type aliases
/// declared in earlier turns of the same REPL session, re-parsed standalone
/// by [`extract_known_type_aliases`] — see that function's doc for why
/// aliases need their own re-parse path rather than `pre_class_hierarchy`'s
/// recover-from-live-BEAM-state mechanism.
///
/// `pre_loaded_protocols` (BT-3473, BT-3477) carries the live image's ambient
/// protocol cache — see [`extract_protocol_registry`]'s doc. Without it, a
/// cross-file protocol-typed receiver in a live `compile_expression` (the
/// REPL's `eval`) hits the same nominal-mismatch/Dnu false positive BT-3473
/// fixed for `diagnostics/3`.
///
/// BT-2952: uses `compute_diagnostics_and_analysis` (the same analysis as
/// `compute_diagnostics_with_known_vars_classes_and_aliases`, additionally
/// returning the full `AnalysisResult`) so the REPL-inline
/// `compile_expression` path computes the same alias-dependency set
/// `handle_compile`'s file-compile path already did — previously this
/// function discarded it, so `handle_inline_class_definition` never got a
/// real set to thread through and `handle_inline_protocol_definition` was
/// called with a hardcoded `&[]` (BT-2917's known limitation).
fn parse_and_check_expression(
    source: &str,
    known_vars: &[String],
    pre_class_hierarchy: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
    pre_loaded_protocols: Vec<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo>,
    pre_loaded_aliases: Vec<beamtalk_core::semantic_analysis::AliasInfo>,
) -> Result<
    (
        beamtalk_core::ast::Module,
        Vec<String>,
        beamtalk_core::semantic_analysis::AnalysisResult,
    ),
    Term,
> {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(source);
    let (module, parse_diagnostics) = beamtalk_core::source_analysis::parse(tokens);

    let known_var_refs: Vec<&str> = known_vars.iter().map(String::as_str).collect();
    let (mut all_diagnostics, analysis) =
        beamtalk_language_service::queries::diagnostic_provider::compute_diagnostics_and_analysis(
            &module,
            parse_diagnostics,
            &known_var_refs,
            pre_class_hierarchy,
            pre_loaded_protocols,
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

    Ok((module, warnings, analysis))
}

/// Handle a single `compile_expression` request.
#[allow(clippy::too_many_lines)]
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
    let pre_loaded_protocols = extract_protocol_registry(request);
    let pre_loaded_aliases = extract_known_type_aliases(request);

    let (module, warnings, analysis) = match parse_and_check_expression(
        &source,
        &known_vars,
        pre_class_hierarchy.clone(),
        pre_loaded_protocols,
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
        let referenced_aliases = analysis.referenced_aliases.clone();
        // BT-3123: `handle_inline_class_definition` merges any standalone
        // `module.method_definitions` into their target class *after* this
        // point — a method the type checker saw as a standalone extension
        // during `analysis` above. `infer_method_return_types`/writeback key
        // return types by `(ClassName, Selector, IsClassMethod)` regardless
        // of whether the method AST lives standalone or inside
        // `class.methods`, so the map itself stays valid across the merge —
        // but only thread `analysis` through when there's no merge about to
        // happen at all, so this call site never has to reason about it:
        // codegen's own no-analysis-supplied fallback (still correct, just
        // not the fast path) covers the rarer "class def + inline standalone
        // method in the same eval" REPL pattern.
        let analysis = module.method_definitions.is_empty().then_some(analysis);
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
            analysis,
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
        let referenced_aliases = analysis.referenced_aliases.clone();
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
            Some(analysis),
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
    match beamtalk_repl::codegen::generate_repl_expressions_with_index(
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
    let pre_loaded_protocols = extract_protocol_registry(request);
    let pre_loaded_aliases = extract_known_type_aliases(request);

    // Trace mode never defines classes/protocols/aliases (rejected below), so
    // neither the `referenced_aliases` nor the rest of the `AnalysisResult`
    // this also now computes (BT-2952 / BT-3123) has a consumer here —
    // trace-mode expressions never reach a `generate_module` call that could
    // use it, and can't reference an alias in a position that needs xref
    // registration either.
    let (module, warnings, _analysis) = match parse_and_check_expression(
        &source,
        &known_vars,
        pre_class_hierarchy,
        pre_loaded_protocols,
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
    match beamtalk_repl::codegen::generate_repl_expressions_traced(
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
///
/// ADR 0119 / BT-3436: the no-override branch mints the name via
/// `ClassModuleRegistry::assign` — this is exactly that method's documented
/// use case (a hot-reloaded inline class definition with no `.bt` source
/// file to derive a path-based name from), exercising the registry primitive
/// `beamtalk-core` added in BT-3435. `module_name_override` is left as a
/// direct pass-through: it is the exact, already-correct (subdirectory-aware)
/// module name a package-mode caller's own Pass 1 computed, not a name this
/// function should re-derive.
fn derive_class_module_name(
    class_name: &str,
    module_name_override: Option<&str>,
    stdlib_mode: bool,
) -> String {
    use beamtalk_core::semantic_analysis::{ClassModuleRegistry, ModuleNamingScheme, PackageId};

    if let Some(name) = module_name_override {
        return name.to_string();
    }
    let (pkg, naming) = if stdlib_mode {
        (PackageId::Stdlib, ModuleNamingScheme::Stdlib)
    } else {
        (PackageId::SingleFile, ModuleNamingScheme::SingleFile)
    };
    ClassModuleRegistry::new()
        .assign(&pkg, class_name, &naming)
        .as_str()
        .to_string()
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
/// `compute_diagnostics_and_analysis` result), threaded into the
/// response so the Erlang side can register the same `beamtalk_alias_xref`
/// dependency edges a file-defining compile gets. Mirrors
/// `handle_inline_protocol_definition`'s identical parameter.
/// BT-3123: Accepts `analysis` — the caller's already-computed
/// [`AnalysisResult`](beamtalk_core::semantic_analysis::AnalysisResult),
/// threaded into codegen via `CodegenOptions::with_analysis` so it doesn't
/// re-derive the class hierarchy, semantic facts, and inferred method return
/// types from scratch. `None` when the caller has no analysis to hand off
/// (or, per `handle_compile_expression`'s call site, when a standalone
/// method merge between `analysis` and codegen would make it stale) —
/// codegen falls back to computing its own, exactly as before BT-3123.
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
    analysis: Option<beamtalk_core::semantic_analysis::AnalysisResult>,
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
        match beamtalk_repl::codegen::generate_repl_expressions_with_index(
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

    let mut codegen_options =
        beamtalk_codegen::core_erlang::CodegenOptions::new(&class_module_name)
            .with_workspace_mode(true)
            .with_source(source)
            .with_class_superclass_index(class_superclass_index.clone())
            .with_class_module_index(class_module_index)
            .with_class_hierarchy(pre_class_hierarchy)
            .with_pre_loaded_aliases(pre_loaded_aliases);
    if let Some(analysis) = analysis {
        // BT-3125: prepare the AST at the driver boundary using the same
        // analysis handed off to codegen just below — codegen no longer
        // schedules this writeback itself for a trustworthy hand-off.
        beamtalk_core::semantic_analysis::lower_module_for_codegen(
            &mut module,
            &analysis.class_hierarchy,
            &analysis.method_return_types,
        );
        codegen_options = codegen_options.with_analysis(analysis);
    }
    match beamtalk_codegen::core_erlang::generate_module(&module, codegen_options) {
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
/// BT-3123: Accepts `analysis` — mirrors `handle_inline_class_definition`'s
/// identical parameter; both callers pass their own already-computed
/// [`AnalysisResult`](beamtalk_core::semantic_analysis::AnalysisResult)
/// unconditionally since, unlike the class path, nothing mutates `module`
/// between computing it and this call.
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
    analysis: Option<beamtalk_core::semantic_analysis::AnalysisResult>,
) -> Term {
    let first_protocol_name = &module.protocols[0].name.name;
    let protocol_module_name =
        derive_class_module_name(first_protocol_name, module_name_override, stdlib_mode);

    let protocol_names: Vec<String> = module
        .protocols
        .iter()
        .map(|p| p.name.name.to_string())
        .collect();

    let mut codegen_options =
        beamtalk_codegen::core_erlang::CodegenOptions::new(&protocol_module_name)
            .with_workspace_mode(true)
            .with_source(source)
            .with_class_superclass_index(class_superclass_index.clone())
            .with_class_module_index(class_module_index)
            .with_class_hierarchy(pre_class_hierarchy)
            .with_pre_loaded_aliases(pre_loaded_aliases);
    // BT-3125: no `lower_module_for_codegen` call needed here — both callers
    // only reach this function once `module.classes` is confirmed empty (a
    // protocol-only compile), and the writeback trio only ever touches
    // `module.classes`/`module.method_definitions`, so it would be a no-op.
    if let Some(analysis) = analysis {
        codegen_options = codegen_options.with_analysis(analysis);
    }
    match beamtalk_codegen::core_erlang::generate_module(module, codegen_options) {
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
    // BT-3477: a class/protocol-defining compile needs the ambient protocol
    // cache too, not just `diagnostics/3` — see `extract_protocol_registry`'s
    // doc. Without this, a live REPL `compile` of a class whose method
    // signature references a cross-file protocol hits the same nominal-
    // mismatch/Dnu false positive BT-3473 fixed for `diagnostics/3`.
    let pre_loaded_protocols = extract_protocol_registry(request);
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
    // BT-3123: `compute_diagnostics_and_analysis` additionally returns the
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
            pre_loaded_aliases.clone(),
            diagnostics_overrides(),
        );
    let referenced_aliases = analysis.referenced_aliases.clone();

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

    // BT-571: Merge standalone method definitions into their target classes.
    // BT-3123: captured *before* the merge so the class-codegen call below
    // knows whether `analysis` (computed pre-merge) is still trustworthy —
    // see `handle_compile_expression`'s identical `analysis` gating for why.
    let had_standalone_method_definitions = !module.method_definitions.is_empty();
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
            Some(analysis),
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
    let mut codegen_options = beamtalk_codegen::core_erlang::CodegenOptions::new(&module_name)
        .with_workspace_mode(workspace_mode)
        .with_source(&source)
        .with_class_module_index(class_module_index)
        .with_class_superclass_index(class_superclass_index)
        .with_class_hierarchy(pre_class_hierarchy)
        .with_pre_loaded_aliases(pre_loaded_aliases)
        .with_source_path_opt(source_path.as_deref());
    // BT-3123: only trust `analysis` (computed pre-merge) when nothing was
    // merged into `module` afterward — see `had_standalone_method_definitions`'s
    // doc above.
    if !had_standalone_method_definitions {
        // BT-3125: prepare the AST at the driver boundary using the same
        // still-trustworthy analysis handed off to codegen just below —
        // codegen no longer schedules this writeback itself in that case.
        beamtalk_core::semantic_analysis::lower_module_for_codegen(
            &mut module,
            &analysis.class_hierarchy,
            &analysis.method_return_types,
        );
        codegen_options = codegen_options.with_analysis(analysis);
    }
    match beamtalk_codegen::core_erlang::generate_module(&module, codegen_options) {
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
    // BT-3477: see `handle_compile`'s equivalent comment — a `compile_method`
    // patch is a class-defining/-patching compile too, so it needs the
    // ambient protocol cache for the same reason.
    let pre_loaded_protocols = extract_protocol_registry(request);
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
    let (mut merged_module, merged_parse_diags) =
        beamtalk_core::source_analysis::parse(merged_tokens);
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
    // BT-3123: `compute_diagnostics_and_analysis` additionally returns the
    // full `AnalysisResult` for the already-merged `merged_module` — no
    // further mutation happens before the codegen call below, so it's always
    // safe to thread through via `CodegenOptions::with_analysis`.
    let (mut all_diagnostics, analysis) =
        beamtalk_language_service::queries::diagnostic_provider::compute_diagnostics_and_analysis(
            &merged_module,
            merged_parse_diags,
            &[],
            pre_class_hierarchy.clone(),
            pre_loaded_protocols,
            pre_loaded_aliases.clone(),
            diagnostics_overrides(),
        );
    let referenced_aliases = analysis.referenced_aliases.clone();
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

    // BT-3125: prepare the AST at the driver boundary using the same
    // still-trustworthy analysis (computed on `merged_module` with no
    // mutation since) handed off to codegen just below — codegen no longer
    // schedules this writeback itself in that case.
    beamtalk_core::semantic_analysis::lower_module_for_codegen(
        &mut merged_module,
        &analysis.class_hierarchy,
        &analysis.method_return_types,
    );
    let codegen_options = beamtalk_codegen::core_erlang::CodegenOptions::new(&module_name)
        .with_workspace_mode(workspace_mode)
        .with_source(&merged_class_source)
        .with_class_module_index(class_module_index)
        .with_class_superclass_index(class_superclass_index)
        .with_class_hierarchy(pre_class_hierarchy)
        .with_pre_loaded_aliases(pre_loaded_aliases)
        .with_source_path_opt(source_path.as_deref())
        // BT-3123: `analysis` was computed on `merged_module` with no
        // mutation since — always safe to hand off.
        .with_analysis(analysis);
    match beamtalk_codegen::core_erlang::generate_module(&merged_module, codegen_options) {
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
fn diag_info(d: &beamtalk_core::source_analysis::Diagnostic) -> DiagInfo {
    DiagInfo {
        message: d.message.to_string(),
        severity: d.severity.as_str().to_string(),
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

    let lines = beamtalk_language_service::queries::senders_query::find_senders_in_source(
        &source, &selector,
    );
    ok_lines_response(&lines)
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

    let lines =
        beamtalk_language_service::queries::references_to_query::find_references_to_in_source(
            &source,
            &class_name,
        );
    ok_lines_response(&lines)
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
        beamtalk_language_service::queries::field_accesses_query::find_field_readers_in_source(
            &source, &field,
        );
    ok_lines_response(&lines)
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
        beamtalk_language_service::queries::field_accesses_query::find_field_writers_in_source(
            &source, &field,
        );
    ok_lines_response(&lines)
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

    let lines = beamtalk_language_service::queries::ffi_sites_query::find_ffi_sites_in_source(
        &source, &module, &function, arity,
    );
    ok_lines_response(&lines)
}

/// Build the standard `#{status => ok, lines => [...]}` response shared by the
/// senders query (BT-2200), references-to query (BT-2203), field reader/writer
/// queries (BT-2208), and FFI sites query (BT-2211).
fn ok_lines_response(lines: &[u32]) -> Term {
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
fn handle_find_selector_send_spans(request: &Map) -> Term {
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
fn selector_send_span_terms(
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
fn handle_find_definition_selector_spans(request: &Map) -> Term {
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
fn handle_resolve_class_span(request: &Map) -> Term {
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
fn handle_class_state_field_defaults(request: &Map) -> Term {
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
fn handle_build_class_module_index_in_source(request: &Map) -> Term {
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

fn class_span_error_response(err: &beamtalk_core::source_analysis::ClassSpanResolveError) -> Term {
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
fn span_term(span: beamtalk_core::source_analysis::Span) -> Term {
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

/// Builds a `#{selector => <<...>>, side => instance | class, span =>
/// #{start, end}}` map term for one [`CategorizedMethod`](beamtalk_core::source_analysis::CategorizedMethod).
fn categorized_method_term(method: &beamtalk_core::source_analysis::CategorizedMethod) -> Term {
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
/// the "absent" sentinel, never an omitted key) — BT-3238's write-path
/// caller (the Cockpit's `save-section` op) needs `divider_span` to locate
/// an existing divider's byte span for a rename, and a consistent key set
/// makes both consumers' Erlang-side pattern matching uniform. `undefined`
/// as a value is indistinguishable from an omitted key to `maps:get/3`'s
/// default-value form (BT-3239's original read-only consumer,
/// `beamtalk_interface.erl`, already reads `name` that way), so this is a
/// superset of BT-3239's original "omit, never null" shape, not a breaking
/// change to it.
fn category_term(category: &beamtalk_core::source_analysis::MethodCategory) -> Term {
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

/// Handle a `categorize_methods` request (BT-3239, extended by BT-3238).
///
/// Groups a class's methods by its `// === Name ===` section dividers —
/// `beamtalk_core::source_analysis::categorize_methods_in_source` is the
/// single, canonical recognizer (BT-2601) already used by the LSP's
/// `documentSymbol` outline; this command is the bridge that lets Erlang
/// surfaces (which have no Rust parser of their own) reach the same
/// function instead of reimplementing its recognition grammar — see that
/// module's doc for why a second implementation is exactly what BT-3239 was
/// written to avoid. BT-3238 (the Cockpit's grouped method view + section
/// authoring) is the second consumer and the reason each category also
/// carries `divider_span` and each method a `span` (BT-3239's original
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
/// their pre-BT-2601 flat rendering. Failure (class not found, or the class
/// name is ambiguous — more than one class definition with that name in
/// `source`) comes back as `#{status => error, reason => class_not_found |
/// ambiguous, message => <<...>>}`.
fn handle_categorize_methods(request: &Map) -> Term {
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

fn categorize_methods_error_response(
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

    // BT-3095: this match arm's string literals are the Rust half of the
    // compiler-port wire vocabulary; `beamtalk_compiler.erl` (fanning out
    // through `beamtalk_compiler_server`/`beamtalk_compiler_port`) is the
    // Erlang half, sending each `command => <atom>` from separate call
    // sites. The two lists cannot literally share code (different
    // languages/processes either side of the OTP port), so a command
    // added to one side and not the other is a silent drift whose only
    // symptom is a runtime "Unknown command" error wherever it's invoked
    // (BT-3078 drift audit; BT-3091 flagged this pair for evaluation).
    // A shared corpus fixture
    // (`runtime/apps/beamtalk_compiler/test/fixtures/compiler_port_command_vocabulary_corpus.json`)
    // pins both sides to the same 19-command list end-to-end: the Rust test
    // below dispatches each corpus command through `handle_request` and
    // checks it isn't the catch-all arm, while
    // `beamtalk_compiler_tests:command_vocabulary_corpus_is_recognized_test/0`
    // drives the real compiled binary through `beamtalk_compiler`'s public
    // API (one command per corpus entry) and asserts each one dispatches
    // successfully — so a command missing on either side fails a build-time
    // test instead of surfacing only at runtime.
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
        "resolve_class_span" => handle_resolve_class_span(map),
        "find_selector_send_spans" => handle_find_selector_send_spans(map),
        "find_definition_selector_spans" => handle_find_definition_selector_spans(map),
        "categorize_methods" => handle_categorize_methods(map),
        "class_state_field_defaults" => handle_class_state_field_defaults(map),
        "build_class_module_index_in_source" => handle_build_class_module_index_in_source(map),
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
mod tests;
