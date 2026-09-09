// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0098: `__beamtalk_meta` map construction — toolchain provenance, type
//! params, package, kind, and visibility — plus ADR 0068's `MetaTypeRepr`
//! conversion and rendering.

use super::*;
use beamtalk_core::ast::{
    ClassDefinition, ClassKind, Identifier, Module, TypeAnnotation, TypeParamDecl,
};
use beamtalk_core::source_analysis::Span;
use beamtalk_core::test_helpers::test_support::make_actor_class;

fn s() -> Span {
    Span::new(0, 0)
}

// ─── ADR 0098: `__beamtalk_meta` map construction ──────────────────────────

#[test]
fn test_meta_method_info_map_with_type_params() {
    let entries: Vec<MethodInfoEntry> = vec![(
        "unwrap".to_string(),
        0,
        MetaTypeRepr::TypeParam {
            name: "T".to_string(),
            index: 0,
        },
        vec![],
        true,
        false,
    )];
    let doc = CoreErlangGenerator::meta_method_info_map(&entries);
    let output = doc.to_pretty_string();
    assert!(
        output.contains("{'type_param', 'T', 0}"),
        "method_info map should contain type_param tagged tuple. Got: {output}"
    );
}

#[test]
fn test_meta_type_params_in_meta_map() {
    // Build a generic class and verify type_params appears in meta map
    let mut class = make_actor_class("Container");
    class.type_params = vec![
        TypeParamDecl::unbounded(Identifier::new("T", s())),
        TypeParamDecl::unbounded(Identifier::new("E", s())),
    ];
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: vec![],
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: s(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let doc = CoreErlangGenerator::build_meta_map_doc(
        module.classes.first().unwrap(),
        &module,
        false,
        false,
        None,
        MetaProvenance::default(),
    );
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'type_params' => ['T', 'E']"),
        "meta map should include type_params list. Got: {output}"
    );
}

/// Helper: build a single-class module from an actor class (ADR 0098 tests).
fn module_with(class: ClassDefinition) -> Module {
    Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: vec![],
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: s(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    }
}

#[test]
fn test_meta_provenance_keys_emitted_when_supplied() {
    // ADR 0098 Phase 3: a known toolchain bakes beamtalk_version + otp_release
    // into __beamtalk_meta as binary string literals (the same compound OTP key
    // the stamp uses — never a runtime system_info call).
    let module = module_with(make_actor_class("Counter"));
    let provenance = MetaProvenance {
        beamtalk_version: Some("0.4.0-dev+abc123"),
        otp_release: Some("28-16.4"),
    };
    let output = CoreErlangGenerator::build_meta_map_doc(
        module.classes.first().unwrap(),
        &module,
        false,
        false,
        None,
        provenance,
    )
    .to_pretty_string();

    assert!(
        output.contains("'beamtalk_version' => "),
        "meta map should include beamtalk_version key. Got: {output}"
    );
    assert!(
        output.contains("'otp_release' => "),
        "meta map should include otp_release key. Got: {output}"
    );
    // Values are baked verbatim as binary literals.
    assert!(
        output.contains(&beamtalk_cerl_doc::binary::binary_string_literal(
            "0.4.0-dev+abc123"
        )),
        "beamtalk_version value not baked correctly. Got: {output}"
    );
    assert!(
        output.contains(&beamtalk_cerl_doc::binary::binary_string_literal("28-16.4")),
        "otp_release value not baked correctly. Got: {output}"
    );
}

#[test]
fn test_meta_provenance_keys_absent_by_default() {
    // REPL / test / older-toolchain codegen supplies no provenance; the keys
    // must be omitted entirely (readers treat absence as a stale module).
    let module = module_with(make_actor_class("Counter"));
    let output = CoreErlangGenerator::build_meta_map_doc(
        module.classes.first().unwrap(),
        &module,
        false,
        false,
        None,
        MetaProvenance::default(),
    )
    .to_pretty_string();

    assert!(
        !output.contains("beamtalk_version"),
        "meta map must omit beamtalk_version when unknown. Got: {output}"
    );
    assert!(
        !output.contains("otp_release"),
        "meta map must omit otp_release when unknown. Got: {output}"
    );
}

#[test]
fn test_meta_provenance_version_only_when_otp_unknown() {
    // OTP probe failed but the version is known: emit beamtalk_version alone.
    let module = module_with(make_actor_class("Counter"));
    let provenance = MetaProvenance {
        beamtalk_version: Some("1.2.3"),
        otp_release: None,
    };
    let output = CoreErlangGenerator::build_meta_map_doc(
        module.classes.first().unwrap(),
        &module,
        false,
        false,
        None,
        provenance,
    )
    .to_pretty_string();

    assert!(
        output.contains("'beamtalk_version' => "),
        "beamtalk_version should be present. Got: {output}"
    );
    assert!(
        !output.contains("otp_release"),
        "otp_release must be omitted when OTP is unknown. Got: {output}"
    );
}

#[test]
fn test_meta_type_params_empty_for_non_generic() {
    let class = make_actor_class("Counter");
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: vec![],
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: s(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let doc = CoreErlangGenerator::build_meta_map_doc(
        module.classes.first().unwrap(),
        &module,
        false,
        false,
        None,
        MetaProvenance::default(),
    );
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'type_params' => []"),
        "non-generic class should have empty type_params. Got: {output}"
    );
}

#[test]
fn test_meta_map_includes_package_name() {
    let class = make_actor_class("Counter");
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: vec![],
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: s(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let doc = CoreErlangGenerator::build_meta_map_doc(
        module.classes.first().unwrap(),
        &module,
        false,
        false,
        Some("my_counter"),
        MetaProvenance::default(),
    );
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'package' => 'my_counter'"),
        "meta map should include package name. Got: {output}"
    );
}

#[test]
fn test_meta_map_package_none_without_package() {
    let class = make_actor_class("Counter");
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: vec![],
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: s(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let doc = CoreErlangGenerator::build_meta_map_doc(
        module.classes.first().unwrap(),
        &module,
        false,
        false,
        None,
        MetaProvenance::default(),
    );
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'package' => 'none'"),
        "meta map should have 'none' package when no package. Got: {output}"
    );
}

#[test]
fn test_meta_map_includes_kind_actor() {
    let class = make_actor_class("Counter");
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: vec![],
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: s(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let doc = CoreErlangGenerator::build_meta_map_doc(
        module.classes.first().unwrap(),
        &module,
        false,
        false,
        None,
        MetaProvenance::default(),
    );
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'kind' => 'actor'"),
        "actor class meta should have kind 'actor'. Got: {output}"
    );
}

#[test]
fn test_meta_map_includes_kind_value() {
    let mut class = make_actor_class("Point");
    class.class_kind = ClassKind::Value;
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: vec![],
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: s(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let doc = CoreErlangGenerator::build_meta_map_doc(
        module.classes.first().unwrap(),
        &module,
        false,
        false,
        None,
        MetaProvenance::default(),
    );
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'kind' => 'value'"),
        "value class meta should have kind 'value'. Got: {output}"
    );
}

#[test]
fn test_extract_package_from_module_name() {
    assert_eq!(
        extract_package_from_module_name("bt@my_counter@counter"),
        Some("my_counter".to_string())
    );
    assert_eq!(
        extract_package_from_module_name("bt@stdlib@integer"),
        Some("stdlib".to_string())
    );
    assert_eq!(extract_package_from_module_name("beamtalk_integer"), None);
    assert_eq!(extract_package_from_module_name("bt@"), None);
    assert_eq!(
        extract_package_from_module_name("bt@pkg@sub@dir@class"),
        Some("pkg".to_string())
    );
}

#[test]
fn test_meta_map_visibility_public_by_default() {
    let class = make_actor_class("Counter");
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: vec![],
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: s(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let doc = CoreErlangGenerator::build_meta_map_doc(
        module.classes.first().unwrap(),
        &module,
        false,
        false,
        None,
        MetaProvenance::default(),
    );
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'visibility' => 'public'"),
        "non-internal class should have visibility 'public'. Got: {output}"
    );
}

#[test]
fn test_meta_map_visibility_internal() {
    let mut class = make_actor_class("Helper");
    class.is_internal = true;
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: vec![],
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: s(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let doc = CoreErlangGenerator::build_meta_map_doc(
        module.classes.first().unwrap(),
        &module,
        false,
        false,
        None,
        MetaProvenance::default(),
    );
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'visibility' => 'internal'"),
        "internal class should have visibility 'internal'. Got: {output}"
    );
}

#[test]
fn test_method_info_visibility_public_by_default() {
    let entries: Vec<MethodInfoEntry> = vec![(
        "getValue".to_string(),
        0,
        MetaTypeRepr::None,
        vec![],
        false,
        false,
    )];
    let doc = CoreErlangGenerator::meta_method_info_map(&entries);
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'visibility' => 'public'"),
        "non-internal method should have visibility 'public'. Got: {output}"
    );
}

#[test]
fn test_method_info_visibility_internal() {
    let entries: Vec<MethodInfoEntry> = vec![(
        "helperMethod".to_string(),
        0,
        MetaTypeRepr::None,
        vec![],
        false,
        true,
    )];
    let doc = CoreErlangGenerator::meta_method_info_map(&entries);
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'visibility' => 'internal'"),
        "internal method should have visibility 'internal'. Got: {output}"
    );
}

// ─── ADR 0068: `MetaTypeRepr` conversion and rendering ─────────────────────

#[test]
fn test_meta_type_repr_none_renders_none_atom() {
    let doc = CoreErlangGenerator::meta_type_repr_doc(&MetaTypeRepr::None);
    assert_eq!(doc.to_pretty_string(), "'none'");
}

#[test]
fn test_meta_type_repr_atom_renders_quoted() {
    let doc = CoreErlangGenerator::meta_type_repr_doc(&MetaTypeRepr::Atom("Integer".to_string()));
    assert_eq!(doc.to_pretty_string(), "'Integer'");
}

#[test]
fn test_meta_type_repr_type_param_renders_tagged_tuple() {
    let doc = CoreErlangGenerator::meta_type_repr_doc(&MetaTypeRepr::TypeParam {
        name: "T".to_string(),
        index: 0,
    });
    assert_eq!(doc.to_pretty_string(), "{'type_param', 'T', 0}");
}

#[test]
fn test_meta_type_repr_type_param_method_local() {
    let doc = CoreErlangGenerator::meta_type_repr_doc(&MetaTypeRepr::TypeParam {
        name: "R".to_string(),
        index: -1,
    });
    assert_eq!(doc.to_pretty_string(), "{'type_param', 'R', -1}");
}

#[test]
fn test_meta_type_repr_generic_renders_tagged_tuple() {
    let doc = CoreErlangGenerator::meta_type_repr_doc(&MetaTypeRepr::Generic {
        base: "Result".to_string(),
        parameters: vec![
            MetaTypeRepr::TypeParam {
                name: "T".to_string(),
                index: 0,
            },
            MetaTypeRepr::TypeParam {
                name: "E".to_string(),
                index: 1,
            },
        ],
    });
    assert_eq!(
        doc.to_pretty_string(),
        "{'generic', 'Result', [{'type_param', 'T', 0}, {'type_param', 'E', 1}]}"
    );
}

#[test]
fn test_meta_type_repr_union_renders_tagged_tuple() {
    // `Integer | String` → `{'union', ['Integer', 'String']}`.
    let doc = CoreErlangGenerator::meta_type_repr_doc(&MetaTypeRepr::Union(vec![
        MetaTypeRepr::Atom("Integer".to_string()),
        MetaTypeRepr::Atom("String".to_string()),
    ]));
    assert_eq!(doc.to_pretty_string(), "{'union', ['Integer', 'String']}");
}

#[test]
fn test_meta_type_repr_singleton_renders_tagged_tuple() {
    // `#north` → `{'singleton', 'north'}`.
    let doc =
        CoreErlangGenerator::meta_type_repr_doc(&MetaTypeRepr::Singleton("north".to_string()));
    assert_eq!(doc.to_pretty_string(), "{'singleton', 'north'}");
}

#[test]
fn test_meta_type_repr_generic_of_union_nests_tagged_tuples() {
    // `Result(Integer | String, Error)` — Union nested inside
    // Generic, exercising the shared `meta_type_repr_list_doc` helper.
    let doc = CoreErlangGenerator::meta_type_repr_doc(&MetaTypeRepr::Generic {
        base: "Result".to_string(),
        parameters: vec![
            MetaTypeRepr::Union(vec![
                MetaTypeRepr::Atom("Integer".to_string()),
                MetaTypeRepr::Atom("String".to_string()),
            ]),
            MetaTypeRepr::Atom("Error".to_string()),
        ],
    });
    assert_eq!(
        doc.to_pretty_string(),
        "{'generic', 'Result', [{'union', ['Integer', 'String']}, 'Error']}"
    );
}

#[test]
fn test_declared_type_to_meta_repr_union_converts_structurally() {
    // `declared_type_to_meta_repr` — not the pre-existing
    // atom-of-rendered-string fallback — handles `Union` structurally.
    let dt = DeclaredType::Union(vec![
        DeclaredType::simple("Integer"),
        DeclaredType::simple("String"),
    ]);
    let repr = CoreErlangGenerator::declared_type_to_meta_repr(&dt, &[]);
    assert_eq!(
        repr,
        MetaTypeRepr::Union(vec![
            MetaTypeRepr::Atom("Integer".to_string()),
            MetaTypeRepr::Atom("String".to_string()),
        ])
    );
}

#[test]
fn test_declared_type_to_meta_repr_singleton_converts_structurally() {
    let dt = DeclaredType::singleton("north");
    let repr = CoreErlangGenerator::declared_type_to_meta_repr(&dt, &[]);
    assert_eq!(repr, MetaTypeRepr::Singleton("north".to_string()));
}

#[test]
fn test_type_annotation_to_meta_repr_simple_concrete() {
    let ta = TypeAnnotation::simple("Integer", s());
    let class_tp = vec![];
    let repr = CoreErlangGenerator::type_annotation_to_meta_repr(&ta, &class_tp);
    assert_eq!(repr, MetaTypeRepr::Atom("Integer".to_string()));
}

#[test]
fn test_type_annotation_to_meta_repr_simple_type_param() {
    let ta = TypeAnnotation::simple("T", s());
    let class_tp = vec![
        TypeParamDecl::unbounded(Identifier::new("T", s())),
        TypeParamDecl::unbounded(Identifier::new("E", s())),
    ];
    let repr = CoreErlangGenerator::type_annotation_to_meta_repr(&ta, &class_tp);
    assert_eq!(
        repr,
        MetaTypeRepr::TypeParam {
            name: "T".to_string(),
            index: 0,
        }
    );
}

#[test]
fn test_type_annotation_to_meta_repr_method_local_type_param() {
    // 'R' is a single uppercase letter not in class type_params → method-local
    let ta = TypeAnnotation::simple("R", s());
    let class_tp = vec![TypeParamDecl::unbounded(Identifier::new("T", s()))];
    let repr = CoreErlangGenerator::type_annotation_to_meta_repr(&ta, &class_tp);
    assert_eq!(
        repr,
        MetaTypeRepr::TypeParam {
            name: "R".to_string(),
            index: -1,
        }
    );
}

#[test]
fn test_type_annotation_to_meta_repr_generic_with_type_params() {
    // Result(R, E) where class has T, E → R is method-local (-1), E is class param (1)
    let ta = TypeAnnotation::generic(
        Identifier::new("Result", s()),
        vec![
            TypeAnnotation::simple("R", s()),
            TypeAnnotation::simple("E", s()),
        ],
        s(),
    );
    let class_tp = vec![
        TypeParamDecl::unbounded(Identifier::new("T", s())),
        TypeParamDecl::unbounded(Identifier::new("E", s())),
    ];
    let repr = CoreErlangGenerator::type_annotation_to_meta_repr(&ta, &class_tp);
    assert_eq!(
        repr,
        MetaTypeRepr::Generic {
            base: "Result".to_string(),
            parameters: vec![
                MetaTypeRepr::TypeParam {
                    name: "R".to_string(),
                    index: -1,
                },
                MetaTypeRepr::TypeParam {
                    name: "E".to_string(),
                    index: 1,
                },
            ],
        }
    );
}
