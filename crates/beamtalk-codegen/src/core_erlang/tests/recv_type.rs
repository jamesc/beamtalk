// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3217 (ADR 0115 Phase 2): `recv_type` write-path fixture coverage.
//!
//! Each test compiles a small module, locates the `method_xref` `sends` row
//! for a specific selector, and asserts its baked `'recv_type' => ...` value
//! matches the write-path projection rule (`project_recv_type` in
//! `gen_server/methods.rs`) for that receiver's `InferredType`. Covers every
//! case the issue's acceptance criteria enumerate: typed local, protocol-typed
//! local, untyped/`Dynamic` local, `Union`-typed local, native-type-typed
//! local, alias-typed local, `Meta{C}` (class-object) receiver, self-send,
//! FFI receiver — at both instance and class side.

use beamtalk_core::semantic_analysis::type_checker::{
    FunctionSignature, NativeTypeRegistry, ParamType,
};
use beamtalk_core::semantic_analysis::{
    AnalysisContext, InferredType, TypeProvenance, analyse_full,
};

fn parse_fixture(src: &str) -> beamtalk_core::ast::Module {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);
    let errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == beamtalk_core::source_analysis::Severity::Error)
        .collect();
    assert!(errors.is_empty(), "fixture failed to parse: {errors:?}");
    module
}

/// Self-sufficient codegen (no pre-run analysis) — exercises the
/// `infer_types_and_returns` swap at `codegen/core_erlang/mod.rs:944`.
fn codegen_self_sufficient(src: &str) -> String {
    let module = parse_fixture(src);
    crate::core_erlang::generate_module(&module, crate::core_erlang::CodegenOptions::new("test"))
        .expect("codegen should succeed")
}

/// Self-sufficient codegen with a native type registry wired in (for FFI /
/// native-type-typed-local cases).
fn codegen_with_native_registry(src: &str, registry: NativeTypeRegistry) -> String {
    let module = parse_fixture(src);
    crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test")
            .with_native_type_registry(Some(std::sync::Arc::new(registry))),
    )
    .expect("codegen should succeed")
}

/// `analyse_full` + `CodegenOptions::with_analysis` — the only path that
/// resolves protocols/aliases declared in the same module (the self-sufficient
/// path's plain `check_module` has no protocol/alias registry at all).
fn codegen_with_full_analysis(src: &str) -> String {
    let module = parse_fixture(src);
    let analysis = analyse_full(&module, AnalysisContext::default());
    crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test").with_analysis(analysis),
    )
    .expect("codegen should succeed")
}

/// Extracts a **send** entry map (`~{'selector' => ..., 'recv_kind' => ...,
/// 'recv_type' => ...}~`) for `selector` from the generated code.
///
/// A `methodXref` row's own top-level `'selector' => 'X', 'line' => N` (the
/// *defining* method, e.g. `Widget`'s own `ping` row) matches the same
/// `'selector' => '<name>'` prefix as a *send* to that selector elsewhere —
/// the two are disambiguated by requiring `'recv_kind'` to follow shortly
/// after within the same `~{...}~` entry, since only a send entry carries it.
/// Panics with the whole generated module if no matching send entry exists,
/// for an actionable failure message.
fn find_send_entry<'a>(code: &'a str, selector: &str) -> &'a str {
    let needle = format!("'selector' => '{selector}',");
    let mut search_from = 0;
    while let Some(rel) = code[search_from..].find(&needle) {
        let start = search_from + rel;
        let entry_start = code[..start]
            .rfind("~{")
            .expect("send entry has an open ~{");
        let entry_end = code[start..].find("}~").expect("send entry has a close }~") + start + 2;
        let entry = &code[entry_start..entry_end];
        if entry.contains("'recv_kind'") {
            return entry;
        }
        search_from = start + needle.len();
    }
    panic!("no send entry (with 'recv_kind') for selector {selector:?} found. Got:\n{code}");
}

fn assert_recv_type(entry: &str, expected: &str) {
    assert_recv_type_raw(entry, &format!("'{expected}'"));
}

/// Like [`assert_recv_type`], but `expected_raw` is the exact literal text
/// after `'recv_type' => ` — for a composed `{'union', [...]}`/
/// `{'intersection', [...]}` tuple (BT-3215), which isn't a single quoted
/// atom.
fn assert_recv_type_raw(entry: &str, expected_raw: &str) {
    let needle = format!("'recv_type' => {expected_raw}");
    assert!(
        entry.contains(&needle),
        "expected recv_type {expected_raw}, got entry:\n{entry}"
    );
}

// ---------------------------------------------------------------------------
// Typed local — instance and class side
// ---------------------------------------------------------------------------

#[test]
fn typed_local_receiver_resolves_to_class_name_instance_side() {
    let code = codegen_self_sufficient(concat!(
        "Object subclass: Widget\n",
        "  ping => 1\n\n",
        "Object subclass: Caller\n",
        "  useIt: w :: Widget =>\n",
        "    w ping\n",
    ));
    let entry = find_send_entry(&code, "ping");
    assert_recv_type(entry, "Widget");
}

#[test]
fn typed_local_receiver_resolves_to_class_name_class_side() {
    let code = codegen_self_sufficient(concat!(
        "Object subclass: Widget\n",
        "  ping => 1\n\n",
        "Object subclass: Caller\n",
        "  class useIt: w :: Widget =>\n",
        "    w ping\n",
    ));
    let entry = find_send_entry(&code, "ping");
    assert_recv_type(entry, "Widget");
}

// ---------------------------------------------------------------------------
// Untyped / Dynamic local — instance and class side
// ---------------------------------------------------------------------------

#[test]
fn untyped_local_receiver_coarsens_to_dynamic_instance_side() {
    let code = codegen_self_sufficient(concat!(
        "Object subclass: Caller\n",
        "  useIt: w =>\n",
        "    w ping\n",
    ));
    let entry = find_send_entry(&code, "ping");
    assert_recv_type(entry, "dynamic");
}

#[test]
fn untyped_local_receiver_coarsens_to_dynamic_class_side() {
    let code = codegen_self_sufficient(concat!(
        "Object subclass: Caller\n",
        "  class useIt: w =>\n",
        "    w ping\n",
    ));
    let entry = find_send_entry(&code, "ping");
    assert_recv_type(entry, "dynamic");
}

// ---------------------------------------------------------------------------
// Union-typed local — resolves to a composed `{'union', [...]}` recv_type
// when every member resolves cleanly (BT-3215; write-path v1 coarsened this
// to `dynamic`, see ADR 0115 Alternatives Considered).
// ---------------------------------------------------------------------------

#[test]
fn union_typed_local_receiver_resolves_to_composed_union() {
    let code = codegen_self_sufficient(concat!(
        "Object subclass: Widget\n",
        "  ping => 1\n\n",
        "Object subclass: Gadget\n",
        "  ping => 1\n\n",
        "Object subclass: Caller\n",
        "  useIt: w :: Widget | Gadget =>\n",
        "    w ping\n",
    ));
    let entry = find_send_entry(&code, "ping");
    assert_recv_type_raw(entry, "{'union', ['Gadget', 'Widget']}");
}

// A union with an unresolvable member (e.g. a native/FFI type with no
// `beamtalk_class_metadata` row) coarsening the whole union to `dynamic` is
// covered directly at the unit level
// (`project_recv_type_union_with_unresolvable_member_coarsens_to_dynamic` in
// `gen_server/methods.rs`) rather than fixtured here — same call the
// original PR made for `Intersection`/`Negation` above: constructing a
// `::`-annotated local whose *member* type resolves through
// `TypeProvenance::Extracted` needs native-registry fixture machinery this
// file doesn't otherwise exercise, for a result the unit test already pins
// precisely.

// ---------------------------------------------------------------------------
// Self-send — instance and class side both resolve to the method's own
// owning class name (ADR 0115 §Schema extension: "a self_recv site's
// recv_type will typically resolve to its own owner"). Unlike an *explicit*
// class-object receiver (`Widget new`, tested below as `Meta{C}`), `self`
// inside a class method is not distinguished from instance `self` by the
// type checker's own typing convention — both key off the owning class name.
// ---------------------------------------------------------------------------

#[test]
fn self_send_resolves_to_own_class_name_instance_side() {
    let code = codegen_self_sufficient(concat!(
        "Object subclass: Widget\n",
        "  bar => self ping\n",
        "  ping => 1\n",
    ));
    let entry = find_send_entry(&code, "ping");
    assert_recv_type(entry, "Widget");
}

#[test]
fn self_send_resolves_to_own_class_name_class_side() {
    let code = codegen_self_sufficient(concat!(
        "Object subclass: Widget\n",
        "  class bar => self ping\n",
        "  class ping => 1\n",
    ));
    let entry = find_send_entry(&code, "ping");
    assert_recv_type(entry, "Widget");
}

// ---------------------------------------------------------------------------
// Meta{C} (class-object) receiver — `Widget new`
// ---------------------------------------------------------------------------

#[test]
fn class_object_receiver_renders_class_tag_shape() {
    let code = codegen_self_sufficient(concat!(
        "Object subclass: Widget\n",
        "  ping => 1\n\n",
        "Object subclass: Caller\n",
        "  useIt =>\n",
        "    Widget new\n",
    ));
    let entry = find_send_entry(&code, "new");
    assert_recv_type(entry, "Widget class");
}

// ---------------------------------------------------------------------------
// Native-type-typed local (ADR 0075) — coarsens to dynamic (spike §4)
// ---------------------------------------------------------------------------

fn lists_reverse_registry() -> NativeTypeRegistry {
    let mut reg = NativeTypeRegistry::new();
    reg.register_module(
        "lists",
        vec![FunctionSignature {
            name: "reverse".to_string(),
            arity: 1,
            params: vec![ParamType {
                keyword: Some(ecow::EcoString::from("list")),
                type_: InferredType::known("List"),
            }],
            // Real FFI specs resolve through `native_types::map_type_name`,
            // which tags the result `TypeProvenance::Extracted` — reproduced
            // directly here so this fixture matches production shape rather
            // than `InferredType::known`'s `Inferred` default.
            return_type: InferredType::Known {
                class_name: ecow::EcoString::from("List"),
                type_args: vec![],
                provenance: TypeProvenance::Extracted,
            },
            provenance: TypeProvenance::Extracted,
            line: None,
        }],
    );
    reg
}

#[test]
fn native_type_typed_local_receiver_coarsens_to_dynamic() {
    let code = codegen_with_native_registry(
        concat!(
            "Object subclass: Caller\n",
            "  useIt =>\n",
            "    result := Erlang lists reverse: #(1, 2, 3)\n",
            "    result size\n",
        ),
        lists_reverse_registry(),
    );
    let entry = find_send_entry(&code, "size");
    assert_recv_type(entry, "dynamic");
}

// ---------------------------------------------------------------------------
// Alias-typed local (ADR 0108) — coarsens to dynamic (spike §4). Needs
// `analyse_full` — like protocols, the self-sufficient path's plain
// `check_module` has no alias registry, so `WidgetAlias` would resolve as an
// (unresolved) opaque class-name reference rather than expanding through
// `TypeProvenance::Aliased`, which is the shape this test exists to exercise.
// ---------------------------------------------------------------------------

#[test]
fn alias_typed_local_receiver_coarsens_to_dynamic() {
    let code = codegen_with_full_analysis(concat!(
        "Object subclass: Widget\n",
        "  ping => 1\n\n",
        "type WidgetAlias = Widget\n\n",
        "Object subclass: Caller\n",
        "  useIt: w :: WidgetAlias =>\n",
        "    w ping\n",
    ));
    let entry = find_send_entry(&code, "ping");
    assert_recv_type(entry, "dynamic");
}

// ---------------------------------------------------------------------------
// Protocol-typed local (ADR 0068) — resolves to the protocol name, same as a
// nominal class (needs `analyse_full` — the self-sufficient path's plain
// `check_module` has no protocol registry).
// ---------------------------------------------------------------------------

#[test]
fn protocol_typed_local_receiver_resolves_to_protocol_name() {
    let code = codegen_with_full_analysis(concat!(
        "Protocol define: Greetable\n",
        "  greet -> String\n\n",
        "Object subclass: Widget\n",
        "  greet -> String => \"hi\"\n\n",
        "Object subclass: Caller\n",
        "  useIt: g :: Greetable =>\n",
        "    g greet\n",
    ));
    let entry = find_send_entry(&code, "greet");
    assert_recv_type(entry, "Greetable");
}

// ---------------------------------------------------------------------------
// FFI receiver (`Erlang lists`) — resolves to the real `ErlangModule` class,
// dropping the module-name type argument (same "drop type_args" rule as any
// other generic `Known`).
// ---------------------------------------------------------------------------

#[test]
fn ffi_module_receiver_resolves_to_erlang_module_class() {
    let code = codegen_self_sufficient(concat!(
        "Object subclass: Caller\n",
        "  useIt =>\n",
        "    Erlang lists reverse: #(1, 2, 3)\n",
    ));
    let entry = find_send_entry(&code, "reverse:");
    assert_recv_type(entry, "ErlangModule");
}
