// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Dialyzer `-spec` generation from type annotations.
//!
//! **DDD Context:** Code Generation
//!
//! Generates Core Erlang `'spec'` module attributes from Beamtalk type
//! annotations on method parameters and return types. This enables Dialyzer
//! to perform cross-language type checking at the BEAM level.
//!
//! ## Core Erlang Spec Format
//!
//! Each spec is encoded as an abstract type representation in the module
//! attributes list:
//!
//! ```erlang
//! 'spec' = [{{'function_name', Arity},
//!            [{'type', 0, 'fun',
//!              [{'type', 0, 'product', [ParamTypes...]}
//!               , ReturnType]}]}]
//! ```
//!
//! ## Type Mapping
//!
//! | Beamtalk Type | Erlang Type |
//! |---------------|-------------|
//! | `Integer` | `integer()` |
//! | `Float` | `float()` |
//! | `Number` | `number()` |
//! | `String` | `binary()` |
//! | `Boolean` | `boolean()` |
//! | `Symbol` | `atom()` |
//! | `Character` | `integer()` |
//! | `List` | `list()` |
//! | `Tuple` | `tuple()` |
//! | `Dictionary` | `map()` |
//! | `Set` | `map()` |
//! | `Nil` / `UndefinedObject` | `nil` (atom) |
//! | `True` | `true` (atom) |
//! | `False` | `false` (atom) |
//! | `Object` / custom | `any()` |
//! | Union | `union(...)` |
//! | Singleton `#foo` | atom `foo` |

use super::document::{join, Document};
use crate::ast::{ClassDefinition, MethodDefinition, MethodKind, TypeAnnotation};
use crate::docvec;

/// Converts a `TypeAnnotation` to its Core Erlang abstract type representation.
fn type_annotation_to_spec(annotation: &TypeAnnotation) -> Document<'static> {
    match annotation {
        TypeAnnotation::Simple(id) => simple_type_to_spec(id.name.as_str()),
        TypeAnnotation::Union { types, .. } => {
            let type_specs: Vec<Document<'static>> =
                types.iter().map(type_annotation_to_spec).collect();
            docvec![
                "{'type', 0, 'union', [",
                join(type_specs, &Document::Str(", ")),
                "]}"
            ]
        }
        TypeAnnotation::Singleton { name, .. } => {
            Document::String(format!("{{'atom', 0, '{name}'}}"))
        }
        TypeAnnotation::Generic { base, .. } => simple_type_to_spec(base.name.as_str()),
        TypeAnnotation::FalseOr { inner, .. } => {
            let inner_spec = type_annotation_to_spec(inner);
            docvec![
                "{'type', 0, 'union', [",
                inner_spec,
                ", {'atom', 0, 'false'}]}"
            ]
        }
    }
}

/// Maps a simple Beamtalk type name to its Core Erlang abstract type representation.
fn simple_type_to_spec(name: &str) -> Document<'static> {
    Document::Str(match name {
        "Integer" | "Character" => "{'type', 0, 'integer', []}",
        "Float" => "{'type', 0, 'float', []}",
        "Number" => "{'type', 0, 'number', []}",
        "String" => "{'type', 0, 'binary', []}",
        "Boolean" => "{'type', 0, 'boolean', []}",
        "Symbol" => "{'type', 0, 'atom', []}",
        "List" => "{'type', 0, 'list', []}",
        "Tuple" => "{'type', 0, 'tuple', 'any'}",
        "Dictionary" | "Set" => "{'type', 0, 'map', 'any'}",
        "Nil" | "UndefinedObject" => "{'atom', 0, 'nil'}",
        "True" => "{'atom', 0, 'true'}",
        "False" => "{'atom', 0, 'false'}",
        "Block" => "{'type', 0, 'fun', []}",
        _ => "{'type', 0, 'any', []}",
    })
}

/// Generates the spec attribute for a single method.
///
/// Returns `None` if the method has no type annotations at all.
/// When at least one annotation exists, unannotated parts default to `any()`.
///
/// For actor methods, the `dispatch_arity` is the selector arity (no Self/State params).
/// For value type methods, the arity includes the Self parameter.
pub fn generate_method_spec(
    method: &MethodDefinition,
    is_value_type: bool,
) -> Option<Document<'static>> {
    let has_any_annotation = method.return_type.is_some()
        || method
            .parameters
            .iter()
            .any(|p| p.type_annotation.is_some());

    if !has_any_annotation {
        return None;
    }

    let erlang_name = method.selector.to_erlang_atom();

    let mut param_types: Vec<Document<'static>> = Vec::new();

    if is_value_type {
        param_types.push(Document::Str("{'type', 0, 'map', 'any'}"));
    }

    for param in &method.parameters {
        let type_spec = param
            .type_annotation
            .as_ref()
            .map_or(Document::Str("{'type', 0, 'any', []}"), |ann| {
                type_annotation_to_spec(ann)
            });
        param_types.push(type_spec);
    }

    let return_spec = method
        .return_type
        .as_ref()
        .map_or(Document::Str("{'type', 0, 'any', []}"), |ann| {
            type_annotation_to_spec(ann)
        });

    let arity = if is_value_type {
        method.parameters.len() + 1
    } else {
        method.parameters.len()
    };

    let product = if param_types.is_empty() {
        Document::Str("{'type', 0, 'product', []}")
    } else {
        docvec![
            "{'type', 0, 'product', [",
            join(param_types, &Document::Str(", ")),
            "]}"
        ]
    };

    Some(docvec![
        Document::String(format!("{{'{erlang_name}', {arity}}}")),
        ", [{'type', 0, 'fun', [",
        product,
        ", ",
        return_spec,
        "]}]"
    ])
}

/// Generates the spec attribute for a class-side method.
///
/// Class methods have two implicit parameters (`ClassSelf`, `ClassVars`)
/// and use the `class_{selector}` naming convention.
fn generate_class_method_spec(method: &MethodDefinition) -> Option<Document<'static>> {
    let has_any_annotation = method.return_type.is_some()
        || method
            .parameters
            .iter()
            .any(|p| p.type_annotation.is_some());

    if !has_any_annotation {
        return None;
    }

    let erlang_name = format!("class_{}", method.selector.to_erlang_atom());

    let mut param_types: Vec<Document<'static>> = vec![
        Document::Str("{'type', 0, 'any', []}"),    // ClassSelf
        Document::Str("{'type', 0, 'map', 'any'}"), // ClassVars
    ];

    for param in &method.parameters {
        let type_spec = param
            .type_annotation
            .as_ref()
            .map_or(Document::Str("{'type', 0, 'any', []}"), |ann| {
                type_annotation_to_spec(ann)
            });
        param_types.push(type_spec);
    }

    let return_spec = method
        .return_type
        .as_ref()
        .map_or(Document::Str("{'type', 0, 'any', []}"), |ann| {
            type_annotation_to_spec(ann)
        });

    let arity = method.parameters.len() + 2;

    let product = docvec![
        "{'type', 0, 'product', [",
        join(param_types, &Document::Str(", ")),
        "]}"
    ];

    Some(docvec![
        Document::String(format!("{{'{erlang_name}', {arity}}}")),
        ", [{'type', 0, 'fun', [",
        product,
        ", ",
        return_spec,
        "]}]"
    ])
}

/// Generates all spec attributes for methods in a class definition.
///
/// Returns a vector of spec attribute Documents, one per annotated method.
/// Only primary methods generate specs.
/// Includes both instance methods and class-side methods.
pub fn generate_class_specs(
    class: &ClassDefinition,
    is_value_type: bool,
) -> Vec<Document<'static>> {
    let instance_specs = class
        .methods
        .iter()
        .filter(|m| m.kind == MethodKind::Primary)
        .filter_map(|m| {
            generate_method_spec(m, is_value_type)
                .map(|spec| docvec!["'spec' =\n        [{", spec, "}]"])
        });

    let class_specs = class
        .class_methods
        .iter()
        .filter(|m| m.kind == MethodKind::Primary)
        .filter_map(|m| {
            generate_class_method_spec(m).map(|spec| docvec!["'spec' =\n        [{", spec, "}]"])
        });

    instance_specs.chain(class_specs).collect()
}

/// Formats spec attributes for inclusion in the module `attributes [...]` list.
///
/// Returns `None` if there are no specs to generate.
pub fn format_spec_attributes(specs: &[Document<'static>]) -> Option<Document<'static>> {
    if specs.is_empty() {
        return None;
    }
    Some(join(
        specs.to_vec(),
        &Document::Str(",\n     "),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        Identifier, KeywordPart, MessageSelector, MethodDefinition, ParameterDefinition,
        TypeAnnotation,
    };
    use crate::source_analysis::Span;

    fn span() -> Span {
        Span::new(0, 0)
    }

    fn render(doc: Document<'_>) -> String {
        doc.to_pretty_string()
    }

    #[test]
    fn integer_type_maps_correctly() {
        let ann = TypeAnnotation::simple("Integer", span());
        assert_eq!(
            render(type_annotation_to_spec(&ann)),
            "{'type', 0, 'integer', []}"
        );
    }

    #[test]
    fn string_type_maps_to_binary() {
        let ann = TypeAnnotation::simple("String", span());
        assert_eq!(
            render(type_annotation_to_spec(&ann)),
            "{'type', 0, 'binary', []}"
        );
    }

    #[test]
    fn float_type_maps_correctly() {
        let ann = TypeAnnotation::simple("Float", span());
        assert_eq!(
            render(type_annotation_to_spec(&ann)),
            "{'type', 0, 'float', []}"
        );
    }

    #[test]
    fn boolean_type_maps_correctly() {
        let ann = TypeAnnotation::simple("Boolean", span());
        assert_eq!(
            render(type_annotation_to_spec(&ann)),
            "{'type', 0, 'boolean', []}"
        );
    }

    #[test]
    fn symbol_type_maps_to_atom() {
        let ann = TypeAnnotation::simple("Symbol", span());
        assert_eq!(
            render(type_annotation_to_spec(&ann)),
            "{'type', 0, 'atom', []}"
        );
    }

    #[test]
    fn nil_maps_to_atom_nil() {
        let ann = TypeAnnotation::simple("Nil", span());
        assert_eq!(render(type_annotation_to_spec(&ann)), "{'atom', 0, 'nil'}");
    }

    #[test]
    fn true_maps_to_atom_true() {
        let ann = TypeAnnotation::simple("True", span());
        assert_eq!(
            render(type_annotation_to_spec(&ann)),
            "{'atom', 0, 'true'}"
        );
    }

    #[test]
    fn false_maps_to_atom_false() {
        let ann = TypeAnnotation::simple("False", span());
        assert_eq!(
            render(type_annotation_to_spec(&ann)),
            "{'atom', 0, 'false'}"
        );
    }

    #[test]
    fn list_type_maps_correctly() {
        let ann = TypeAnnotation::simple("List", span());
        assert_eq!(
            render(type_annotation_to_spec(&ann)),
            "{'type', 0, 'list', []}"
        );
    }

    #[test]
    fn dictionary_maps_to_map() {
        let ann = TypeAnnotation::simple("Dictionary", span());
        assert_eq!(
            render(type_annotation_to_spec(&ann)),
            "{'type', 0, 'map', 'any'}"
        );
    }

    #[test]
    fn tuple_maps_correctly() {
        let ann = TypeAnnotation::simple("Tuple", span());
        assert_eq!(
            render(type_annotation_to_spec(&ann)),
            "{'type', 0, 'tuple', 'any'}"
        );
    }

    #[test]
    fn custom_class_maps_to_any() {
        let ann = TypeAnnotation::simple("Counter", span());
        assert_eq!(
            render(type_annotation_to_spec(&ann)),
            "{'type', 0, 'any', []}"
        );
    }

    #[test]
    fn union_type_maps_correctly() {
        let ann = TypeAnnotation::union(
            vec![
                TypeAnnotation::simple("Integer", span()),
                TypeAnnotation::simple("Nil", span()),
            ],
            span(),
        );
        let result = render(type_annotation_to_spec(&ann));
        assert_eq!(
            result,
            "{'type', 0, 'union', [{'type', 0, 'integer', []}, {'atom', 0, 'nil'}]}"
        );
    }

    #[test]
    fn singleton_type_maps_to_atom() {
        let ann = TypeAnnotation::singleton("north", span());
        assert_eq!(
            render(type_annotation_to_spec(&ann)),
            "{'atom', 0, 'north'}"
        );
    }

    #[test]
    fn false_or_type_maps_correctly() {
        let ann = TypeAnnotation::false_or(TypeAnnotation::simple("Integer", span()), span());
        let result = render(type_annotation_to_spec(&ann));
        assert_eq!(
            result,
            "{'type', 0, 'union', [{'type', 0, 'integer', []}, {'atom', 0, 'false'}]}"
        );
    }

    #[test]
    fn unary_method_with_return_type_generates_spec() {
        let method = MethodDefinition::with_return_type(
            MessageSelector::Unary("getBalance".into()),
            vec![],
            vec![],
            TypeAnnotation::simple("Integer", span()),
            span(),
        );

        let spec = render(generate_method_spec(&method, false).unwrap());
        assert_eq!(
            spec,
            "{'getBalance', 0}, [{'type', 0, 'fun', [{'type', 0, 'product', []}, {'type', 0, 'integer', []}]}]"
        );
    }

    #[test]
    fn keyword_method_with_typed_param_generates_spec() {
        let method = MethodDefinition::with_return_type(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::with_type(
                Identifier::new("amount", span()),
                TypeAnnotation::simple("Integer", span()),
            )],
            vec![],
            TypeAnnotation::simple("Integer", span()),
            span(),
        );

        let spec = render(generate_method_spec(&method, false).unwrap());
        assert_eq!(
            spec,
            "{'deposit:', 1}, [{'type', 0, 'fun', [{'type', 0, 'product', [{'type', 0, 'integer', []}]}, {'type', 0, 'integer', []}]}]"
        );
    }

    #[test]
    fn method_without_annotations_returns_none() {
        let method = MethodDefinition::new(
            MessageSelector::Unary("increment".into()),
            vec![],
            vec![],
            span(),
        );

        assert!(generate_method_spec(&method, false).is_none());
    }

    #[test]
    fn method_with_only_return_type_uses_any_for_params() {
        let method = MethodDefinition::with_return_type(
            MessageSelector::Unary("getValue".into()),
            vec![ParameterDefinition::new(Identifier::new("x", span()))],
            vec![],
            TypeAnnotation::simple("Integer", span()),
            span(),
        );

        let spec = render(generate_method_spec(&method, false).unwrap());
        assert!(spec.contains("'any'"));
        assert!(spec.contains("'integer'"));
    }

    #[test]
    fn value_type_method_includes_self_param() {
        let method = MethodDefinition::with_return_type(
            MessageSelector::Unary("size".into()),
            vec![],
            vec![],
            TypeAnnotation::simple("Integer", span()),
            span(),
        );

        let spec = render(generate_method_spec(&method, true).unwrap());
        assert!(spec.contains("{'size', 1}"));
        assert!(spec.contains("'map', 'any'"));
    }

    #[test]
    fn multiple_keyword_params() {
        let method = MethodDefinition::with_return_type(
            MessageSelector::Keyword(vec![
                KeywordPart::new("transfer:", span()),
                KeywordPart::new("to:", span()),
            ]),
            vec![
                ParameterDefinition::with_type(
                    Identifier::new("amount", span()),
                    TypeAnnotation::simple("Integer", span()),
                ),
                ParameterDefinition::with_type(
                    Identifier::new("target", span()),
                    TypeAnnotation::simple("String", span()),
                ),
            ],
            vec![],
            TypeAnnotation::simple("Boolean", span()),
            span(),
        );

        let spec = render(generate_method_spec(&method, false).unwrap());
        assert!(spec.contains("'transfer:to:'"));
        assert!(spec.contains("'integer'"));
        assert!(spec.contains("'binary'"));
        assert!(spec.contains("'boolean'"));
    }

    #[test]
    fn union_return_type() {
        let method = MethodDefinition::with_return_type(
            MessageSelector::Keyword(vec![KeywordPart::new("find:", span())]),
            vec![ParameterDefinition::with_type(
                Identifier::new("key", span()),
                TypeAnnotation::simple("Symbol", span()),
            )],
            vec![],
            TypeAnnotation::union(
                vec![
                    TypeAnnotation::simple("Integer", span()),
                    TypeAnnotation::simple("Nil", span()),
                ],
                span(),
            ),
            span(),
        );

        let spec = render(generate_method_spec(&method, false).unwrap());
        assert!(spec.contains("'union'"));
        assert!(spec.contains("'integer'"));
        assert!(spec.contains("'nil'"));
    }

    #[test]
    fn block_type_maps_to_fun() {
        let ann = TypeAnnotation::simple("Block", span());
        assert_eq!(
            render(type_annotation_to_spec(&ann)),
            "{'type', 0, 'fun', []}"
        );
    }

    #[test]
    fn number_type_maps_correctly() {
        let ann = TypeAnnotation::simple("Number", span());
        assert_eq!(
            render(type_annotation_to_spec(&ann)),
            "{'type', 0, 'number', []}"
        );
    }

    #[test]
    fn character_maps_to_integer() {
        let ann = TypeAnnotation::simple("Character", span());
        assert_eq!(
            render(type_annotation_to_spec(&ann)),
            "{'type', 0, 'integer', []}"
        );
    }

    #[test]
    fn set_maps_to_map() {
        let ann = TypeAnnotation::simple("Set", span());
        assert_eq!(
            render(type_annotation_to_spec(&ann)),
            "{'type', 0, 'map', 'any'}"
        );
    }

    #[test]
    fn format_spec_attributes_empty() {
        assert!(format_spec_attributes(&[]).is_none());
    }

    #[test]
    fn format_spec_attributes_single() {
        let specs = vec![Document::Str("'spec' =\n        [{{spec}}]")];
        let result = render(format_spec_attributes(&specs).unwrap());
        assert_eq!(result, "'spec' =\n        [{{spec}}]");
    }

    #[test]
    fn format_spec_attributes_multiple() {
        let specs = vec![
            Document::Str("'spec' =\n        [{{spec1}}]"),
            Document::Str("'spec' =\n        [{{spec2}}]"),
        ];
        let result = render(format_spec_attributes(&specs).unwrap());
        assert!(result.contains("spec1"));
        assert!(result.contains("spec2"));
    }

    #[test]
    fn value_type_method_with_two_params_generates_correct_cons() {
        let method = MethodDefinition::with_return_type(
            MessageSelector::Keyword(vec![
                KeywordPart::new("at:", span()),
                KeywordPart::new("put:", span()),
            ]),
            vec![
                ParameterDefinition::with_type(
                    Identifier::new("key", span()),
                    TypeAnnotation::simple("Symbol", span()),
                ),
                ParameterDefinition::with_type(
                    Identifier::new("value", span()),
                    TypeAnnotation::simple("Object", span()),
                ),
            ],
            vec![],
            TypeAnnotation::simple("Object", span()),
            span(),
        );

        let spec = render(generate_method_spec(&method, true).unwrap());
        assert!(spec.contains("{'at:put:', 3}"));
        assert!(
            spec.contains(
                "{'type', 0, 'map', 'any'}, {'type', 0, 'atom', []}, {'type', 0, 'any', []}"
            ),
            "Product should use proper cons nesting: got {spec}"
        );
    }

    #[test]
    fn three_element_union_generates_correct_cons() {
        let ann = TypeAnnotation::union(
            vec![
                TypeAnnotation::simple("Integer", span()),
                TypeAnnotation::simple("String", span()),
                TypeAnnotation::simple("Nil", span()),
            ],
            span(),
        );
        let result = render(type_annotation_to_spec(&ann));
        assert_eq!(
            result,
            "{'type', 0, 'union', [{'type', 0, 'integer', []}, {'type', 0, 'binary', []}, {'atom', 0, 'nil'}]}"
        );
    }

    #[test]
    fn class_method_spec_includes_class_self_and_class_vars() {
        let method = MethodDefinition::with_return_type(
            MessageSelector::Keyword(vec![KeywordPart::new("from:", span())]),
            vec![ParameterDefinition::with_type(
                Identifier::new("start", span()),
                TypeAnnotation::simple("Integer", span()),
            )],
            vec![],
            TypeAnnotation::simple("Stream", span()),
            span(),
        );

        let spec = render(generate_class_method_spec(&method).unwrap());
        assert!(
            spec.contains("{'class_from:', 3}"),
            "Expected class_ prefix and arity 3 (ClassSelf + ClassVars + 1 param), got: {spec}"
        );
        assert!(spec.contains("'any'"));
        assert!(spec.contains("'map'"));
        assert!(spec.contains("'integer'"));
    }

    #[test]
    fn class_method_without_annotations_returns_none() {
        let method = MethodDefinition::new(
            MessageSelector::Unary("create".into()),
            vec![],
            vec![],
            span(),
        );
        assert!(generate_class_method_spec(&method).is_none());
    }
}
