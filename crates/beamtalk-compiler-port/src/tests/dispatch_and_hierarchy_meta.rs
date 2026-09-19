// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `handle_request` command-vocabulary dispatch, inline class definitions with a superclass index, REPL directive defaults, and class-hierarchy round-tripping (including generic/Self return-type survival) through the ETF wire.

use super::*;

/// Conformance: every command in the shared wire-vocabulary
/// corpus must be recognized by `handle_request`'s dispatch — i.e. it
/// must not fall through to the catch-all `"Unknown command: ..."` arm.
/// The corpus is the single source of truth both implementations are
/// pinned to; the Erlang side asserts the identical list drives real
/// dispatch on the compiled binary in
/// `beamtalk_compiler_tests:command_vocabulary_corpus_is_recognized_test/0`.
/// See `handle_request`'s doc comment for the full rationale.
#[test]
fn handle_request_recognizes_shared_command_vocabulary_corpus() {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("crates/")
            .parent()
            .expect("repo root")
            .join(
                "runtime/apps/beamtalk_compiler/test/fixtures/compiler_port_command_vocabulary_corpus.json",
            );
    let raw = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("read corpus {}: {e}", path.display()));
    let corpus: Vec<String> = serde_json::from_str(&raw).expect("corpus is a JSON array");
    assert!(!corpus.is_empty(), "corpus must have cases");

    for command in &corpus {
        // Deliberately send no fields beyond `command` — a recognized
        // command may still fail (e.g. "Missing or invalid 'source'
        // field"), but that failure is distinct from the dispatcher's
        // catch-all message, which is the unique fingerprint of an
        // unrecognized command atom.
        let request = Term::from(Map::from([(atom("command"), atom(command.as_str()))]));
        let response = handle_request(&request);
        let Term::Map(ref m) = response else {
            panic!("command {command:?}: expected a map response, got {response:?}");
        };
        let unknown_message = format!("Unknown command: {command}");
        if let Some(Term::List(diagnostics)) = map_get(m, "diagnostics") {
            for diag in &diagnostics.elements {
                let Term::Map(diag_map) = diag else {
                    continue;
                };
                if let Some(msg) = map_get(diag_map, "message").and_then(term_to_string) {
                    assert_ne!(
                        msg, unknown_message,
                        "corpus command {command:?} is not recognized by handle_request's \
                             dispatch — add a match arm (or remove it from the corpus if it was \
                             deliberately retired)"
                    );
                }
            }
        }
    }
}

/// Inline class definition with cross-file superclass index must compile
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
        (atom("class_fields"), Term::from(List::from(vec![]))),
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
        info.state_types.get("count"),
        Some(&beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType::simple("Integer"))
    );
    assert_eq!(info.methods.len(), 1);
    assert_eq!(info.methods[0].selector.as_str(), "value");
    assert_eq!(info.methods[0].arity, 0);
    assert_eq!(
        info.methods[0].return_type,
        Some(beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType::simple("Integer"))
    );
    assert_eq!(info.class_methods.len(), 1);
    assert_eq!(info.class_methods[0].selector.as_str(), "new");
    assert_eq!(info.class_methods[0].arity, 0);
}

/// ADR 0124 §1/B5a: `'field_kinds'` and `'class_field_kinds'`
/// (`class_meta.rs`'s `meta_field_kinds_map`) must survive the ETF
/// `__beamtalk_meta/0` boundary into `ClassInfo.state_kinds` and
/// `ClassInfo.class_variables[].kind` — the cross-file half of the guarded
/// read (B3): a `late` slot declared in a class whose AST is not in the
/// current compilation is only visible through this metadata.
#[test]
fn field_kinds_and_class_field_kinds_survive_etf_meta() {
    use beamtalk_core::ast::SlotKind;
    use eetf::{FixInteger, List};

    let field_types_map = Map::from([(atom("proc"), atom("Subprocess"))]);
    let field_kinds_map = Map::from([(atom("proc"), atom("late")), (atom("id"), atom("eager"))]);
    let class_field_kinds_map = Map::from([(atom("current"), atom("late"))]);

    let meta_map = Map::from([
        (atom("class"), atom("codexClient")),
        (atom("superclass"), atom("Actor")),
        (atom("meta_version"), Term::from(FixInteger::from(2))),
        (atom("is_sealed"), atom("false")),
        (atom("is_abstract"), atom("false")),
        (atom("is_value"), atom("false")),
        (atom("is_typed"), atom("true")),
        (
            atom("fields"),
            Term::from(List::from(vec![atom("proc"), atom("id")])),
        ),
        (atom("field_types"), Term::from(field_types_map)),
        (atom("field_kinds"), Term::from(field_kinds_map)),
        (atom("method_info"), Term::from(Map::from([]))),
        (atom("class_method_info"), Term::from(Map::from([]))),
        (
            atom("class_fields"),
            Term::from(List::from(vec![atom("current")])),
        ),
        (atom("class_field_kinds"), Term::from(class_field_kinds_map)),
    ]);

    let class_hierarchy_term = Term::from(Map::from([(atom("codexClient"), Term::from(meta_map))]));
    let classes = parse_class_hierarchy_from_term(&class_hierarchy_term);
    assert_eq!(classes.len(), 1, "Should parse one class");

    let info = &classes[0];
    assert_eq!(
        info.state_kinds.get("proc").copied(),
        Some(SlotKind::Late),
        "late instance field must decode as SlotKind::Late"
    );
    assert_eq!(
        info.state_kinds.get("id").copied(),
        Some(SlotKind::Eager),
        "ordinary instance field must decode as SlotKind::Eager"
    );
    assert_eq!(info.class_variables.len(), 1);
    assert_eq!(info.class_variables[0].name.as_str(), "current");
    assert_eq!(
        info.class_variables[0].kind,
        SlotKind::Late,
        "late class variable must decode as SlotKind::Late"
    );
}

/// A field absent from `'field_kinds'` (an older BEAM artifact compiled
/// before ADR 0124) must degrade to `SlotKind::Eager` — the pre-existing
/// behaviour every slot had — never panic or leave the map empty for a
/// declared field.
#[test]
fn missing_field_kinds_key_degrades_to_eager() {
    use eetf::List;

    let meta_map = Map::from([
        (atom("class"), atom("legacy")),
        (atom("superclass"), atom("Object")),
        (atom("is_sealed"), atom("false")),
        (atom("is_abstract"), atom("false")),
        (atom("is_value"), atom("false")),
        (atom("is_typed"), atom("false")),
        (atom("fields"), Term::from(List::from(vec![atom("value")]))),
        (atom("field_types"), Term::from(Map::from([]))),
        (atom("method_info"), Term::from(Map::from([]))),
        (atom("class_method_info"), Term::from(Map::from([]))),
        (atom("class_fields"), Term::from(List::from(vec![]))),
    ]);

    let class_hierarchy_term = Term::from(Map::from([(atom("legacy"), Term::from(meta_map))]));
    let classes = parse_class_hierarchy_from_term(&class_hierarchy_term);
    let info = &classes[0];
    assert!(
        info.state_kinds.is_empty(),
        "no field_kinds key means no entries — readers fall back to Eager \
             (see ClassHierarchy::state_field_kind)"
    );
}

/// A generic return type — the `{'generic', Base, [Params]}`
/// `MetaTypeRepr` tagged tuple codegen emits for e.g. `-> Result(T, E)`
/// (`crate::codegen::core_erlang::gen_server::methods::MetaTypeRepr`,
/// beamtalk-core) — must survive the ETF `__beamtalk_meta/0` boundary
/// into `MethodInfo::return_type` *structurally*.
///
/// Before this stage, `term_to_atom` only matched a bare `Term::Atom`,
/// so this exact tagged tuple silently degraded to `None` — a real
/// latent bug (a generic return type crossing the compiler port lost
/// its structure entirely). `term_to_declared_type` is the fix: this
/// test pins the shape `{'generic', 'Result', [{'type_param', 'T', 0},
/// 'Error']}` (a class-level type param nested inside a generic,
/// mirroring what codegen actually emits for `class Box(T) ... unwrap
/// -> Result(T, Error)`) round-tripping to
/// `DeclaredType::Generic { base: "Result", parameters: [Simple("T"),
/// Simple("Error")] }` — the codegen-internal `type_param` index has no
/// `DeclaredType` counterpart and is intentionally dropped (see
/// `term_to_declared_type`'s doc).
#[test]
fn generic_return_type_survives_etf_meta() {
    use beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType;
    use eetf::{FixInteger, List, Tuple};

    let generic_return = Term::from(Tuple {
        elements: vec![
            atom("generic"),
            atom("Result"),
            Term::from(List::from(vec![
                Term::from(Tuple {
                    elements: vec![
                        atom("type_param"),
                        atom("T"),
                        Term::from(FixInteger::from(0)),
                    ],
                }),
                atom("Error"),
            ])),
        ],
    });

    let unwrap_method_map = Map::from([
        (atom("arity"), Term::from(FixInteger::from(0))),
        (atom("param_types"), Term::from(List::from(vec![]))),
        (atom("return_type"), generic_return),
    ]);
    let method_info_map = Map::from([(atom("unwrap"), Term::from(unwrap_method_map))]);

    let meta_map = Map::from([
        (atom("class"), atom("box")),
        (atom("superclass"), atom("Object")),
        (atom("meta_version"), Term::from(FixInteger::from(2))),
        (atom("is_sealed"), atom("false")),
        (atom("is_abstract"), atom("false")),
        (atom("is_value"), atom("true")),
        (atom("is_typed"), atom("false")),
        (atom("fields"), Term::from(List::from(vec![]))),
        (atom("field_types"), Term::from(Map::from([]))),
        (atom("method_info"), Term::from(method_info_map)),
        (atom("class_method_info"), Term::from(Map::from([]))),
        (atom("class_fields"), Term::from(List::from(vec![]))),
    ]);

    let class_hierarchy_term = Term::from(Map::from([(atom("box"), Term::from(meta_map))]));
    let classes = parse_class_hierarchy_from_term(&class_hierarchy_term);
    assert_eq!(classes.len(), 1, "Should parse one class");

    let info = &classes[0];
    let method = info
        .methods
        .iter()
        .find(|m| m.selector == "unwrap")
        .expect("unwrap method should be present");
    assert_eq!(
        method.return_type,
        Some(DeclaredType::generic(
            "Result",
            vec![DeclaredType::simple("T"), DeclaredType::simple("Error")],
        )),
        "generic return type must survive the ETF meta boundary structurally, \
             not degrade to None (the bug this test guards against)"
    );
}

/// `-> Self` / `-> Self class` / `-> <Name> class` return types
/// cross the ETF `__beamtalk_meta/0` boundary as *flat atoms* (codegen's
/// `MetaTypeRepr::Atom` fallback renders them via `Display`), so a method
/// inherited from a class compiled in a previous REPL/workspace step must
/// re-enter `MethodInfo::return_type` as the structured
/// `SelfType`/`SelfClass`/`ClassOf` variants — the checker's fluent-setter
/// and metatype special cases match on those variants, and a degraded
/// `Simple("Self")` would silently fall back to `Dynamic`. Companion to
/// `generic_return_type_survives_etf_meta` for the self-type shapes.
#[test]
fn self_type_return_survives_etf_meta() {
    use beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType;
    use eetf::{FixInteger, List};

    let method = |return_type: Term| {
        Term::from(Map::from([
            (atom("arity"), Term::from(FixInteger::from(0))),
            (atom("param_types"), Term::from(List::from(vec![]))),
            (atom("return_type"), return_type),
        ]))
    };
    let method_info_map = Map::from([
        (atom("withX"), method(atom("Self"))),
        (atom("species"), method(atom("Self class"))),
        (atom("actorClass"), method(atom("Actor class"))),
    ]);

    let meta_map = Map::from([
        (atom("class"), atom("box")),
        (atom("superclass"), atom("Object")),
        (atom("meta_version"), Term::from(FixInteger::from(2))),
        (atom("is_sealed"), atom("false")),
        (atom("is_abstract"), atom("false")),
        (atom("is_value"), atom("true")),
        (atom("is_typed"), atom("false")),
        (atom("fields"), Term::from(List::from(vec![]))),
        (atom("field_types"), Term::from(Map::from([]))),
        (atom("method_info"), Term::from(method_info_map)),
        (atom("class_method_info"), Term::from(Map::from([]))),
        (atom("class_fields"), Term::from(List::from(vec![]))),
    ]);

    let class_hierarchy_term = Term::from(Map::from([(atom("box"), Term::from(meta_map))]));
    let classes = parse_class_hierarchy_from_term(&class_hierarchy_term);
    assert_eq!(classes.len(), 1, "Should parse one class");

    let info = &classes[0];
    let return_type_of = |selector: &str| {
        info.methods
            .iter()
            .find(|m| m.selector == selector)
            .unwrap_or_else(|| panic!("{selector} method should be present"))
            .return_type
            .clone()
    };
    assert_eq!(
        return_type_of("withX"),
        Some(DeclaredType::SelfType),
        "'Self' atom must round-trip as SelfType, not Simple(\"Self\")"
    );
    assert_eq!(
        return_type_of("species"),
        Some(DeclaredType::SelfClass),
        "'Self class' atom must round-trip as SelfClass"
    );
    assert_eq!(
        return_type_of("actorClass"),
        Some(DeclaredType::ClassOf("Actor".into())),
        "'Actor class' atom must round-trip as ClassOf(\"Actor\")"
    );
}
