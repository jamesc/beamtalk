// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `resolve_completion_type` tests (BT-1068) and `load_native_type_registry_from`'s live/missing-cache FFI type-registry resolution.

use super::*;

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
