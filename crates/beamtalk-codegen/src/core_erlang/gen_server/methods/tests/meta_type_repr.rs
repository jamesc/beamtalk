// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0068/BT-3076: `MetaTypeRepr` conversion and rendering.

use super::*;
use beamtalk_core::ast::{Identifier, TypeAnnotation, TypeParamDecl};

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
    // BT-3076: `Integer | String` → `{'union', ['Integer', 'String']}`.
    let doc = CoreErlangGenerator::meta_type_repr_doc(&MetaTypeRepr::Union(vec![
        MetaTypeRepr::Atom("Integer".to_string()),
        MetaTypeRepr::Atom("String".to_string()),
    ]));
    assert_eq!(doc.to_pretty_string(), "{'union', ['Integer', 'String']}");
}

#[test]
fn test_meta_type_repr_singleton_renders_tagged_tuple() {
    // BT-3076: `#north` → `{'singleton', 'north'}`.
    let doc =
        CoreErlangGenerator::meta_type_repr_doc(&MetaTypeRepr::Singleton("north".to_string()));
    assert_eq!(doc.to_pretty_string(), "{'singleton', 'north'}");
}

#[test]
fn test_meta_type_repr_generic_of_union_nests_tagged_tuples() {
    // BT-3076: `Result(Integer | String, Error)` — Union nested inside
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
    // BT-3076: `declared_type_to_meta_repr` — not the pre-existing
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
