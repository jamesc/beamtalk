// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ETF term decoding for compiler-port requests.
//!
//! Converts wire-format `Term`/`Map` values into `beamtalk-core` domain
//! types (declared types, method/class/protocol info, class hierarchies)
//! and extracts structured fields (class hierarchy, protocol registry,
//! known type aliases) from request maps.

use beamtalk_etf::{
    map_get, term_to_atom, term_to_bool, term_to_string, term_to_string_list, term_to_usize,
};
use eetf::{Map, Term};

use crate::respond::error_response;

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
pub(crate) fn term_to_string_map_checked(
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
pub(crate) fn term_to_atom_list(term: &Term) -> Vec<ecow::EcoString> {
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

/// Deserialize a single `method_info`/`class_method_info`
/// `return_type`/`param_types` entry — the `MetaTypeRepr` wire shape codegen
/// emits (`crate::codegen::core_erlang::gen_server::methods::MetaTypeRepr`'s
/// `meta_type_repr_doc`, beamtalk-core) — into a structured
/// [`DeclaredType`](beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType).
///
/// This is THE single place strings become types crossing the ETF boundary:
/// `term_to_atom` alone only matches a bare `Term::Atom`, so every
/// tagged tuple (`{type_param, ...}`, `{generic, ...}`, `{union, ...}`,
/// `{singleton, ...}`) needs this parser, or it silently degrades to `None`
/// — a generic return type crossing the compiler port would lose its
/// structure entirely (see `test_generic_return_type_survives_etf_meta`).
///
/// Wire shapes:
/// - `'none'` → `None` (the `Option` wrapper, not `DeclaredType::None` — no
///   such variant exists).
/// - A bare atom (e.g. `'Integer'`) → [`DeclaredType::parse`] — handles
///   legacy artifacts (older compiled modules only ever emitted flat
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
/// - `{'union', [Members]}` → `DeclaredType::Union(members)`, recursively.
/// - `{'singleton', Name}` → `DeclaredType::Singleton(Name)`.
/// - Anything else (malformed/unknown tag) → `None`, matching this parser's
///   existing graceful-degradation convention.
pub(crate) fn term_to_declared_type(
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
pub(crate) fn term_to_declared_type_list(
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

/// Deserialize an atom→`DeclaredType` map (for `field_types` /
/// `ClassInfo::state_types`).
///
/// `field_types` is still emitted by codegen as flat atoms (`'Integer'`, or
/// `'none'` for an untyped field — `meta_field_types_map`, beamtalk-core) —
/// unlike `method_info`/`class_method_info`, it never carries the tagged
/// `MetaTypeRepr` tuples, so every value here goes through
/// [`DeclaredType::parse`] rather than the full [`term_to_declared_type`].
/// The `'none'` sentinel is intentionally *not* filtered out here — it
/// round-trips as `DeclaredType::Simple("none")`, preserving this map's
/// this map's original behaviour verbatim (the old atom→atom reader never
/// special-cased it either) rather than fixing that latent quirk as a
/// drive-by change.
pub(crate) fn term_to_declared_type_atom_map(
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

/// Extract an atom→bool map (for `field_has_default`).
pub(crate) fn term_to_atom_bool_map(
    term: &Term,
) -> std::collections::HashMap<ecow::EcoString, bool> {
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
pub(crate) fn parse_method_infos_from_map(
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
pub(crate) fn parse_class_info_from_meta_term(
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
    // Read field_has_default map emitted by codegen. Missing key
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
        // surfaces are complete by construction.
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
pub(crate) fn parse_class_hierarchy_from_term(
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
/// `ProtocolMethodRequirement`s.
///
/// Selector/arity only: the live image's ambient protocol cache never carries
/// the original `::`-annotated parameter/return type text (that lives only in
/// the protocol's defining source file, which this port doesn't have access
/// to for a cross-file re-check), so `return_type`/`param_types` are always
/// `None`. Good enough for the escape hatches that key off protocol *names*
/// (`is_protocol_type`, DNU suppression via `ClassHierarchy::has_class`) —
/// see `parse_protocol_info_from_meta_term`'s doc.
pub(crate) fn parse_protocol_method_requirements(
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

/// Deserialize a single ambient protocol-registry entry —
/// `beamtalk_protocol_registry:register_protocol/1`'s `Info` map, threaded
/// through `beamtalk_compiler_server`'s `protocols` cache the same way
/// `class_hierarchy` threads `register_class/2`'s — into a `ProtocolInfo`.
///
/// Returns `None` if `term` is not a map.
pub(crate) fn parse_protocol_info_from_meta_term(
    protocol_name: &str,
    term: &Term,
) -> Option<beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo> {
    use beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo;
    use beamtalk_core::source_analysis::Span;

    let Term::Map(m) = term else { return None };

    let type_params = map_get(m, "type_params")
        .map(term_to_atom_list)
        .unwrap_or_default();
    // No wire representation for bounds on this channel (the scope here is
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
/// `Vec<ProtocolInfo>`. Degrades gracefully on malformed entries
/// (silently skipped), mirroring `parse_class_hierarchy_from_term`.
pub(crate) fn parse_protocol_registry_from_term(
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

/// Extract an optional `protocol_registry` field, returning `Vec<ProtocolInfo>`.
/// Mirrors `extract_class_hierarchy`.
pub(crate) fn extract_protocol_registry(
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
/// There is therefore no class-side "kind trap" / duplicate-push.
pub(crate) fn merge_method(
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
pub(crate) fn extract_optional_string_map(
    request: &Map,
    key: &str,
) -> Result<std::collections::HashMap<String, String>, Term> {
    match map_get(request, key) {
        None => Ok(std::collections::HashMap::new()),
        Some(term) => term_to_string_map_checked(term, key).map_err(|e| error_response(&[e])),
    }
}

/// Extract an optional `class_hierarchy` field, returning `Vec<ClassInfo>`.
pub(crate) fn extract_class_hierarchy(
    request: &Map,
) -> Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo> {
    match map_get(request, "class_hierarchy") {
        None => vec![],
        Some(term) => parse_class_hierarchy_from_term(term),
    }
}

/// Extract an optional `known_type_aliases` field: a list of standalone
/// `type Name = <expansion>` source strings (ADR 0108 Phase 8),
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
pub(crate) fn extract_known_type_aliases(
    request: &Map,
) -> Vec<beamtalk_core::semantic_analysis::AliasInfo> {
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
