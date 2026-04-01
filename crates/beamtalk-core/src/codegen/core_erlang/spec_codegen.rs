// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Dialyzer `-spec` and `-type` generation from type annotations.
//!
//! **DDD Context:** Code Generation
//!
//! Generates Core Erlang `'spec'` and `'type'` module attributes from Beamtalk
//! type annotations. This enables Dialyzer to perform cross-language type
//! checking at the BEAM level.
//!
//! - [`generate_class_specs`] / [`format_spec_attributes`] — emit `-spec`
//!   annotations for each method with type-annotated parameters or return type
//! - [`generate_type_alias`] — emit a `-type t()` map alias for Value classes
//!   with `state:` declarations, so Erlang producers can reference `Module:t()`
//!   in their own `-spec` annotations (requires a paired `-export_type([t/0])`
//!   emitted by `value_type_codegen.rs`)
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

use super::document::{Document, join};
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
        TypeAnnotation::Generic {
            base, parameters, ..
        } => generic_type_to_spec(base.name.as_str(), parameters),
        TypeAnnotation::FalseOr { inner, .. } => {
            let inner_spec = type_annotation_to_spec(inner);
            docvec![
                "{'type', 0, 'union', [",
                inner_spec,
                ", {'atom', 0, 'false'}]}"
            ]
        }
        // Self resolves to the receiver class at call sites; in specs, treat as any()
        TypeAnnotation::SelfType { .. } => Document::Str("{'type', 0, 'any', []}"),
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

/// Maps a generic `TypeAnnotation::Generic` to its Core Erlang abstract type representation.
///
/// For known parameterized types, incorporates the type parameters into the spec:
///
/// - `List(T)` → `{'type', 0, 'list', [T_spec]}`
/// - `Block(A, R)` → `{'type', 0, 'fun', [{'type', 0, 'product', [A_spec]}, R_spec]}`
/// - `Block(R)` → `{'type', 0, 'fun', [{'type', 0, 'product', []}, R_spec]}`
///
/// For types where Dialyzer does not support parameterization (Dictionary, Set, Tuple),
/// the base type is emitted without parameters. For unknown/custom classes, `any()` is used.
///
/// Unresolved type parameters (bare names like `T`, `E` that do not match a known class)
/// are recursively resolved — ultimately falling through to `any()` via [`simple_type_to_spec`].
fn generic_type_to_spec(base_name: &str, parameters: &[TypeAnnotation]) -> Document<'static> {
    match base_name {
        // List(T) → list(T) — Erlang list type supports an element type parameter
        "List" => {
            if let Some(elem) = parameters.first() {
                let elem_spec = type_annotation_to_spec(elem);
                docvec!["{'type', 0, 'list', [", elem_spec, "]}"]
            } else {
                Document::Str("{'type', 0, 'list', []}")
            }
        }
        // Block(...) → fun type with product args and return type
        // Block(R) = zero-arg block returning R
        // Block(A, R) = one-arg block with arg A, returning R
        // Block(A, B, R) = two-arg block, etc.
        // Last parameter is always the return type; preceding ones are arg types.
        "Block" => {
            if parameters.is_empty() {
                Document::Str("{'type', 0, 'fun', []}")
            } else {
                let (arg_params, return_param) = parameters.split_at(parameters.len() - 1);
                let return_spec = type_annotation_to_spec(&return_param[0]);
                let arg_specs: Vec<Document<'static>> =
                    arg_params.iter().map(type_annotation_to_spec).collect();
                let product = if arg_specs.is_empty() {
                    Document::Str("{'type', 0, 'product', []}")
                } else {
                    docvec![
                        "{'type', 0, 'product', [",
                        join(arg_specs, &Document::Str(", ")),
                        "]}"
                    ]
                };
                docvec!["{'type', 0, 'fun', [", product, ", ", return_spec, "]}"]
            }
        }
        // Dictionary, Set, Tuple — Dialyzer doesn't support parameterized forms
        // for these in the abstract type representation, so emit the base type.
        "Dictionary" | "Set" => Document::Str("{'type', 0, 'map', 'any'}"),
        "Tuple" => Document::Str("{'type', 0, 'tuple', 'any'}"),
        // Any other generic type (custom classes like Result, Array, etc.)
        // maps to any() — the base class is not a built-in Erlang type.
        _ => simple_type_to_spec(base_name),
    }
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
    Some(join(specs.to_vec(), &Document::Str(",\n     ")))
}

/// Generates the `-type t()` module attribute for a Value class with `state:` fields.
///
/// Produces a map type that includes the `$beamtalk_class` tag field (as a required
/// `:=` association) followed by each declared `state:` field with its Erlang type.
///
/// Returns `None` if the class has no `state:` declarations.
///
/// ## Generated Format
///
/// ```erlang
/// 'type' =
///         [{'t', {'type', 0, 'map', [
///           {'type', 0, 'map_field_exact', [{'atom', 0, '$beamtalk_class'}, {'atom', 0, 'ClassName'}]},
///           {'type', 0, 'map_field_exact', [{'atom', 0, 'field1'}, FieldType1]},
///           ...]}, []}]
/// ```
pub fn generate_type_alias(class: &ClassDefinition, class_name: &str) -> Option<Document<'static>> {
    if class.state.is_empty() {
        return None;
    }

    let mut field_types: Vec<Document<'static>> = Vec::new();

    // '$beamtalk_class' tag field: always present, value = the class atom
    field_types.push(docvec![
        "{'type', 0, 'map_field_exact', [{'atom', 0, '$beamtalk_class'}, {'atom', 0, '",
        Document::String(class_name.to_string()),
        "'}]}"
    ]);

    // One required field entry per declared state: field
    for field in &class.state {
        let fname = Document::String(field.name.name.to_string());
        let ftype = field.type_annotation.as_ref().map_or(
            Document::Str("{'type', 0, 'any', []}"),
            type_annotation_to_spec,
        );
        field_types.push(docvec![
            "{'type', 0, 'map_field_exact', [{'atom', 0, '",
            fname,
            "'}, ",
            ftype,
            "]}"
        ]);
    }

    Some(docvec![
        "'type' =\n        [{'t', {'type', 0, 'map', [",
        join(field_types, &Document::Str(", ")),
        "]}, []}]"
    ])
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        ClassModifiers, Identifier, KeywordPart, MessageSelector, MethodDefinition,
        ParameterDefinition, TypeAnnotation,
    };
    use crate::source_analysis::Span;

    fn span() -> Span {
        Span::new(0, 0)
    }

    fn render(doc: &Document<'_>) -> String {
        doc.to_pretty_string()
    }

    #[test]
    fn integer_type_maps_correctly() {
        let ann = TypeAnnotation::simple("Integer", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'integer', []}"
        );
    }

    #[test]
    fn string_type_maps_to_binary() {
        let ann = TypeAnnotation::simple("String", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'binary', []}"
        );
    }

    #[test]
    fn float_type_maps_correctly() {
        let ann = TypeAnnotation::simple("Float", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'float', []}"
        );
    }

    #[test]
    fn boolean_type_maps_correctly() {
        let ann = TypeAnnotation::simple("Boolean", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'boolean', []}"
        );
    }

    #[test]
    fn symbol_type_maps_to_atom() {
        let ann = TypeAnnotation::simple("Symbol", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'atom', []}"
        );
    }

    #[test]
    fn nil_maps_to_atom_nil() {
        let ann = TypeAnnotation::simple("Nil", span());
        assert_eq!(render(&type_annotation_to_spec(&ann)), "{'atom', 0, 'nil'}");
    }

    #[test]
    fn true_maps_to_atom_true() {
        let ann = TypeAnnotation::simple("True", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'atom', 0, 'true'}"
        );
    }

    #[test]
    fn false_maps_to_atom_false() {
        let ann = TypeAnnotation::simple("False", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'atom', 0, 'false'}"
        );
    }

    #[test]
    fn list_type_maps_correctly() {
        let ann = TypeAnnotation::simple("List", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'list', []}"
        );
    }

    #[test]
    fn dictionary_maps_to_map() {
        let ann = TypeAnnotation::simple("Dictionary", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'map', 'any'}"
        );
    }

    #[test]
    fn tuple_maps_correctly() {
        let ann = TypeAnnotation::simple("Tuple", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'tuple', 'any'}"
        );
    }

    #[test]
    fn custom_class_maps_to_any() {
        let ann = TypeAnnotation::simple("Counter", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
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
        let result = render(&type_annotation_to_spec(&ann));
        assert_eq!(
            result,
            "{'type', 0, 'union', [{'type', 0, 'integer', []}, {'atom', 0, 'nil'}]}"
        );
    }

    #[test]
    fn singleton_type_maps_to_atom() {
        let ann = TypeAnnotation::singleton("north", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'atom', 0, 'north'}"
        );
    }

    #[test]
    fn false_or_type_maps_correctly() {
        let ann = TypeAnnotation::false_or(TypeAnnotation::simple("Integer", span()), span());
        let result = render(&type_annotation_to_spec(&ann));
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

        let spec = render(&generate_method_spec(&method, false).unwrap());
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

        let spec = render(&generate_method_spec(&method, false).unwrap());
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

        let spec = render(&generate_method_spec(&method, false).unwrap());
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

        let spec = render(&generate_method_spec(&method, true).unwrap());
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

        let spec = render(&generate_method_spec(&method, false).unwrap());
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

        let spec = render(&generate_method_spec(&method, false).unwrap());
        assert!(spec.contains("'union'"));
        assert!(spec.contains("'integer'"));
        assert!(spec.contains("'nil'"));
    }

    #[test]
    fn block_type_maps_to_fun() {
        let ann = TypeAnnotation::simple("Block", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'fun', []}"
        );
    }

    #[test]
    fn number_type_maps_correctly() {
        let ann = TypeAnnotation::simple("Number", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'number', []}"
        );
    }

    #[test]
    fn character_maps_to_integer() {
        let ann = TypeAnnotation::simple("Character", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'integer', []}"
        );
    }

    #[test]
    fn set_maps_to_map() {
        let ann = TypeAnnotation::simple("Set", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
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
        let result = render(&format_spec_attributes(&specs).unwrap());
        assert_eq!(result, "'spec' =\n        [{{spec}}]");
    }

    #[test]
    fn format_spec_attributes_multiple() {
        let specs = vec![
            Document::Str("'spec' =\n        [{{spec1}}]"),
            Document::Str("'spec' =\n        [{{spec2}}]"),
        ];
        let result = render(&format_spec_attributes(&specs).unwrap());
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

        let spec = render(&generate_method_spec(&method, true).unwrap());
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
        let result = render(&type_annotation_to_spec(&ann));
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

        let spec = render(&generate_class_method_spec(&method).unwrap());
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

    // Tests for generate_type_alias

    fn make_class_with_state(
        class_name: &str,
        fields: Vec<(&str, Option<TypeAnnotation>)>,
    ) -> ClassDefinition {
        use crate::ast::StateDeclaration;
        let state = fields
            .into_iter()
            .map(|(name, ann)| match ann {
                Some(a) => StateDeclaration::with_type(Identifier::new(name, span()), a, span()),
                None => StateDeclaration::new(Identifier::new(name, span()), span()),
            })
            .collect();
        ClassDefinition::with_modifiers(
            Identifier::new(class_name, span()),
            Some(Identifier::new("Value", span())),
            ClassModifiers::default(),
            state,
            vec![],
            span(),
        )
    }

    #[test]
    fn type_alias_returns_none_for_empty_state() {
        let class = make_class_with_state("Empty", vec![]);
        assert!(generate_type_alias(&class, "Empty").is_none());
    }

    #[test]
    fn type_alias_includes_beamtalk_class_tag_field() {
        let class = make_class_with_state(
            "Point",
            vec![("x", Some(TypeAnnotation::simple("Integer", span())))],
        );
        let result = render(&generate_type_alias(&class, "Point").unwrap());
        assert!(
            result.contains("'$beamtalk_class'"),
            "Should include $beamtalk_class tag field, got: {result}"
        );
        assert!(
            result.contains("'Point'"),
            "Should include class name atom, got: {result}"
        );
    }

    #[test]
    fn type_alias_maps_integer_field() {
        let class = make_class_with_state(
            "Counter",
            vec![("count", Some(TypeAnnotation::simple("Integer", span())))],
        );
        let result = render(&generate_type_alias(&class, "Counter").unwrap());
        assert!(
            result.contains("'count'"),
            "Should include field name, got: {result}"
        );
        assert!(
            result.contains("'integer'"),
            "Should include integer type, got: {result}"
        );
        assert!(
            result.contains("'map_field_exact'"),
            "Should use map_field_exact for required fields, got: {result}"
        );
    }

    #[test]
    fn type_alias_http_response_fields() {
        let class = make_class_with_state(
            "HTTPResponse",
            vec![
                ("status", Some(TypeAnnotation::simple("Integer", span()))),
                ("headers", Some(TypeAnnotation::simple("List", span()))),
                ("body", Some(TypeAnnotation::simple("String", span()))),
            ],
        );
        let result = render(&generate_type_alias(&class, "HTTPResponse").unwrap());
        assert!(
            result.contains("'HTTPResponse'"),
            "Should contain class name atom"
        );
        assert!(result.contains("'status'"), "Should contain status field");
        assert!(result.contains("'headers'"), "Should contain headers field");
        assert!(result.contains("'body'"), "Should contain body field");
        assert!(result.contains("'integer'"), "status should be integer");
        assert!(result.contains("'list'"), "headers should be list");
        assert!(
            result.contains("'binary'"),
            "body (String) should be binary"
        );
        assert!(
            result.starts_with("'type' ="),
            "Should start with type attribute"
        );
        assert!(
            result.contains("'map'"),
            "Should use map type, got: {result}"
        );
    }

    #[test]
    fn type_alias_unannotated_field_uses_any() {
        let class = make_class_with_state("Pair", vec![("value", None)]);
        let result = render(&generate_type_alias(&class, "Pair").unwrap());
        assert!(
            result.contains("'any'"),
            "Unannotated field should use any(), got: {result}"
        );
    }

    // Tests for generic type spec generation (BT-1574)

    #[test]
    fn generic_list_with_integer_element() {
        let ann = TypeAnnotation::generic(
            Identifier::new("List", span()),
            vec![TypeAnnotation::simple("Integer", span())],
            span(),
        );
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'list', [{'type', 0, 'integer', []}]}"
        );
    }

    #[test]
    fn generic_list_with_string_element() {
        let ann = TypeAnnotation::generic(
            Identifier::new("List", span()),
            vec![TypeAnnotation::simple("String", span())],
            span(),
        );
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'list', [{'type', 0, 'binary', []}]}"
        );
    }

    #[test]
    fn generic_list_no_params_falls_back_to_unparameterized() {
        let ann = TypeAnnotation::generic(Identifier::new("List", span()), vec![], span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'list', []}"
        );
    }

    #[test]
    fn generic_block_zero_arg_with_return_type() {
        // Block(Integer) = zero-arg block returning Integer
        let ann = TypeAnnotation::generic(
            Identifier::new("Block", span()),
            vec![TypeAnnotation::simple("Integer", span())],
            span(),
        );
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'fun', [{'type', 0, 'product', []}, {'type', 0, 'integer', []}]}"
        );
    }

    #[test]
    fn generic_block_one_arg() {
        // Block(Integer, String) = one-arg block: Integer → String
        let ann = TypeAnnotation::generic(
            Identifier::new("Block", span()),
            vec![
                TypeAnnotation::simple("Integer", span()),
                TypeAnnotation::simple("String", span()),
            ],
            span(),
        );
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'fun', [{'type', 0, 'product', [{'type', 0, 'integer', []}]}, {'type', 0, 'binary', []}]}"
        );
    }

    #[test]
    fn generic_block_two_args() {
        // Block(Integer, String, Boolean) = two-arg block: (Integer, String) → Boolean
        let ann = TypeAnnotation::generic(
            Identifier::new("Block", span()),
            vec![
                TypeAnnotation::simple("Integer", span()),
                TypeAnnotation::simple("String", span()),
                TypeAnnotation::simple("Boolean", span()),
            ],
            span(),
        );
        let result = render(&type_annotation_to_spec(&ann));
        assert_eq!(
            result,
            "{'type', 0, 'fun', [{'type', 0, 'product', [{'type', 0, 'integer', []}, {'type', 0, 'binary', []}]}, {'type', 0, 'boolean', []}]}"
        );
    }

    #[test]
    fn generic_block_no_params_falls_back() {
        let ann = TypeAnnotation::generic(Identifier::new("Block", span()), vec![], span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'fun', []}"
        );
    }

    #[test]
    fn generic_dictionary_ignores_params() {
        // Dictionary(String, Integer) → map() (Dialyzer doesn't parameterize maps)
        let ann = TypeAnnotation::generic(
            Identifier::new("Dictionary", span()),
            vec![
                TypeAnnotation::simple("String", span()),
                TypeAnnotation::simple("Integer", span()),
            ],
            span(),
        );
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'map', 'any'}"
        );
    }

    #[test]
    fn generic_set_ignores_params() {
        let ann = TypeAnnotation::generic(
            Identifier::new("Set", span()),
            vec![TypeAnnotation::simple("Integer", span())],
            span(),
        );
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'map', 'any'}"
        );
    }

    #[test]
    fn generic_tuple_ignores_params() {
        let ann = TypeAnnotation::generic(
            Identifier::new("Tuple", span()),
            vec![
                TypeAnnotation::simple("Integer", span()),
                TypeAnnotation::simple("String", span()),
            ],
            span(),
        );
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'tuple', 'any'}"
        );
    }

    #[test]
    fn generic_custom_class_maps_to_any() {
        // Result(Integer, Error) — custom class maps to any()
        let ann = TypeAnnotation::generic(
            Identifier::new("Result", span()),
            vec![
                TypeAnnotation::simple("Integer", span()),
                TypeAnnotation::simple("Error", span()),
            ],
            span(),
        );
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'any', []}"
        );
    }

    #[test]
    fn unresolved_type_param_maps_to_any() {
        // Bare type variable T maps to any() via simple_type_to_spec catch-all
        let ann = TypeAnnotation::simple("T", span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'any', []}"
        );
    }

    #[test]
    fn generic_list_with_unresolved_type_param() {
        // List(T) — T is unresolved, maps to any()
        let ann = TypeAnnotation::generic(
            Identifier::new("List", span()),
            vec![TypeAnnotation::simple("T", span())],
            span(),
        );
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'list', [{'type', 0, 'any', []}]}"
        );
    }

    #[test]
    fn nested_generic_list_of_lists() {
        // List(List(Integer)) — nested generic
        let inner = TypeAnnotation::generic(
            Identifier::new("List", span()),
            vec![TypeAnnotation::simple("Integer", span())],
            span(),
        );
        let ann = TypeAnnotation::generic(Identifier::new("List", span()), vec![inner], span());
        assert_eq!(
            render(&type_annotation_to_spec(&ann)),
            "{'type', 0, 'list', [{'type', 0, 'list', [{'type', 0, 'integer', []}]}]}"
        );
    }

    #[test]
    fn generic_block_with_nested_generic_return() {
        // Block(Integer, List(String)) — block taking Integer, returning List(String)
        let return_type = TypeAnnotation::generic(
            Identifier::new("List", span()),
            vec![TypeAnnotation::simple("String", span())],
            span(),
        );
        let ann = TypeAnnotation::generic(
            Identifier::new("Block", span()),
            vec![TypeAnnotation::simple("Integer", span()), return_type],
            span(),
        );
        let result = render(&type_annotation_to_spec(&ann));
        assert_eq!(
            result,
            "{'type', 0, 'fun', [{'type', 0, 'product', [{'type', 0, 'integer', []}]}, {'type', 0, 'list', [{'type', 0, 'binary', []}]}]}"
        );
    }

    #[test]
    fn method_with_generic_param_type_generates_spec() {
        // Method: process: items :: List(Integer) -> Integer
        let method = MethodDefinition::with_return_type(
            MessageSelector::Keyword(vec![KeywordPart::new("process:", span())]),
            vec![ParameterDefinition::with_type(
                Identifier::new("items", span()),
                TypeAnnotation::generic(
                    Identifier::new("List", span()),
                    vec![TypeAnnotation::simple("Integer", span())],
                    span(),
                ),
            )],
            vec![],
            TypeAnnotation::simple("Integer", span()),
            span(),
        );

        let spec = render(&generate_method_spec(&method, false).unwrap());
        assert!(
            spec.contains("'list', [{'type', 0, 'integer', []}]"),
            "Should contain parameterized list type, got: {spec}"
        );
        assert!(
            spec.contains("{'process:', 1}"),
            "Should have correct selector and arity, got: {spec}"
        );
    }

    #[test]
    fn method_with_generic_return_type_generates_spec() {
        // Method: getItems -> List(String)
        let method = MethodDefinition::with_return_type(
            MessageSelector::Unary("getItems".into()),
            vec![],
            vec![],
            TypeAnnotation::generic(
                Identifier::new("List", span()),
                vec![TypeAnnotation::simple("String", span())],
                span(),
            ),
            span(),
        );

        let spec = render(&generate_method_spec(&method, false).unwrap());
        assert!(
            spec.contains("'list', [{'type', 0, 'binary', []}]"),
            "Return type should be parameterized list, got: {spec}"
        );
    }

    #[test]
    fn method_with_block_param_generates_fun_spec() {
        // Method: do: block :: Block(Integer, Object) -> Nil
        let method = MethodDefinition::with_return_type(
            MessageSelector::Keyword(vec![KeywordPart::new("do:", span())]),
            vec![ParameterDefinition::with_type(
                Identifier::new("block", span()),
                TypeAnnotation::generic(
                    Identifier::new("Block", span()),
                    vec![
                        TypeAnnotation::simple("Integer", span()),
                        TypeAnnotation::simple("Object", span()),
                    ],
                    span(),
                ),
            )],
            vec![],
            TypeAnnotation::simple("Nil", span()),
            span(),
        );

        let spec = render(&generate_method_spec(&method, false).unwrap());
        assert!(
            spec.contains("'fun'"),
            "Block param should generate fun type, got: {spec}"
        );
        assert!(
            spec.contains("'integer'"),
            "Block arg type should be integer, got: {spec}"
        );
    }

    #[test]
    fn type_alias_with_generic_field_type() {
        // Value class with a field typed as List(Integer)
        use crate::ast::StateDeclaration;
        let state = vec![StateDeclaration::with_type(
            Identifier::new("items", span()),
            TypeAnnotation::generic(
                Identifier::new("List", span()),
                vec![TypeAnnotation::simple("Integer", span())],
                span(),
            ),
            span(),
        )];
        let class = ClassDefinition::with_modifiers(
            Identifier::new("Container", span()),
            Some(Identifier::new("Value", span())),
            ClassModifiers::default(),
            state,
            vec![],
            span(),
        );
        let result = render(&generate_type_alias(&class, "Container").unwrap());
        assert!(
            result.contains("'list', [{'type', 0, 'integer', []}]"),
            "Field type should be parameterized list, got: {result}"
        );
    }
}
