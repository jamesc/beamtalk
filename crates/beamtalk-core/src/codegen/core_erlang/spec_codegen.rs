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
//!               | [ReturnType]]}]}]
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

use crate::ast::{ClassDefinition, MethodDefinition, MethodKind, TypeAnnotation};

/// Converts a `TypeAnnotation` to its Core Erlang abstract type representation.
///
/// Returns the string form of the abstract type tuple used in `-spec` attributes.
fn type_annotation_to_spec(annotation: &TypeAnnotation) -> String {
    match annotation {
        TypeAnnotation::Simple(id) => simple_type_to_spec(id.name.as_str()),
        TypeAnnotation::Union { types, .. } => {
            let type_specs: Vec<String> = types.iter().map(type_annotation_to_spec).collect();
            format!("{{'type', 0, 'union', [{}]}}", join_with_cons(&type_specs))
        }
        TypeAnnotation::Singleton { name, .. } => {
            format!("{{'atom', 0, '{name}'}}")
        }
        TypeAnnotation::Generic { base, .. } => {
            // Generic types (e.g., Collection<Integer>) map to their base type
            simple_type_to_spec(base.name.as_str())
        }
        TypeAnnotation::FalseOr { inner, .. } => {
            let inner_spec = type_annotation_to_spec(inner);
            format!(
                "{{'type', 0, 'union', [{inner_spec} | [{{'atom', 0, 'false'}}]]}}",
            )
        }
    }
}

/// Maps a simple Beamtalk type name to its Core Erlang abstract type representation.
fn simple_type_to_spec(name: &str) -> String {
    match name {
        "Integer" | "Character" => "{'type', 0, 'integer', []}".to_string(),
        "Float" => "{'type', 0, 'float', []}".to_string(),
        "Number" => "{'type', 0, 'number', []}".to_string(),
        "String" => "{'type', 0, 'binary', []}".to_string(),
        "Boolean" => "{'type', 0, 'boolean', []}".to_string(),
        "Symbol" => "{'type', 0, 'atom', []}".to_string(),
        "List" => "{'type', 0, 'list', []}".to_string(),
        "Tuple" => "{'type', 0, 'tuple', 'any'}".to_string(),
        "Dictionary" | "Set" => "{'type', 0, 'map', 'any'}".to_string(),
        "Nil" | "UndefinedObject" => "{'atom', 0, 'nil'}".to_string(),
        "True" => "{'atom', 0, 'true'}".to_string(),
        "False" => "{'atom', 0, 'false'}".to_string(),
        "Block" => "{'type', 0, 'fun', []}".to_string(),
        // Custom classes and Object map to any()
        _ => "{'type', 0, 'any', []}".to_string(),
    }
}

/// Joins type spec strings into a Core Erlang cons-cell list.
///
/// Core Erlang lists in attributes use cons syntax: `[a | [b | [c]]]`
fn join_with_cons(specs: &[String]) -> String {
    match specs.len() {
        0 => String::new(),
        1 => specs[0].clone(),
        _ => {
            // Build right-nested cons: A | [B | [C]]
            // Start from rightmost and wrap each level
            let mut result = format!("[{}]", specs[specs.len() - 1]);
            for spec in specs[1..specs.len() - 1].iter().rev() {
                result = format!("[{spec} | {result}]");
            }
            // The outermost level doesn't get wrapped (caller wraps in [...])
            format!("{} | {result}", specs[0])
        }
    }
}

/// Generates the spec attribute string for a single method.
///
/// Returns `None` if the method has no type annotations at all.
/// When at least one annotation exists, unannotated parts default to `any()`.
///
/// For actor methods, the `dispatch_arity` is the selector arity (no Self/State params).
/// For value type methods, the arity includes the Self parameter.
pub fn generate_method_spec(method: &MethodDefinition, is_value_type: bool) -> Option<String> {
    let has_any_annotation = method.return_type.is_some()
        || method
            .parameters
            .iter()
            .any(|p| p.type_annotation.is_some());

    if !has_any_annotation {
        return None;
    }

    let erlang_name = method.selector.to_erlang_atom();

    // Build parameter types
    let mut param_types: Vec<String> = Vec::new();

    if is_value_type {
        // Value type methods take Self (a map) as first parameter
        param_types.push("{'type', 0, 'map', 'any'}".to_string());
    }

    for param in &method.parameters {
        let type_spec = param.type_annotation.as_ref().map_or_else(
            || "{'type', 0, 'any', []}".to_string(),
            type_annotation_to_spec,
        );
        param_types.push(type_spec);
    }

    // Build return type
    let return_spec = method.return_type.as_ref().map_or_else(
        || "{'type', 0, 'any', []}".to_string(),
        type_annotation_to_spec,
    );

    // Function arity for spec
    let arity = if is_value_type {
        method.parameters.len() + 1 // +1 for Self
    } else {
        method.parameters.len()
    };

    // Build the product type for parameters
    let product = if param_types.is_empty() {
        "{'type', 0, 'product', []}".to_string()
    } else {
        format!(
            "{{'type', 0, 'product', [{}]}}",
            join_with_cons(&param_types)
        )
    };

    // Full spec: {{'name', arity}, [{'type', 0, 'fun', [Product | [Return]]}]}
    Some(format!(
        "{{'{erlang_name}', {arity}}}, [{{'type', 0, 'fun', [{product} | [{return_spec}]]}}]"
    ))
}

/// Generates all spec attributes for methods in a class definition.
///
/// Returns a vector of spec attribute strings, one per annotated method.
/// Only primary methods generate specs (not before/after/around advice).
pub fn generate_class_specs(class: &ClassDefinition, is_value_type: bool) -> Vec<String> {
    class
        .methods
        .iter()
        .filter(|m| m.kind == MethodKind::Primary)
        .filter_map(|m| {
            generate_method_spec(m, is_value_type)
                .map(|spec| format!("'spec' =\n        [{{{spec}}}]"))
        })
        .collect()
}

/// Formats spec attributes for inclusion in the module `attributes [...]` list.
///
/// Returns `None` if there are no specs to generate.
pub fn format_spec_attributes(specs: &[String]) -> Option<String> {
    if specs.is_empty() {
        return None;
    }
    Some(specs.join(",\n     "))
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

    #[test]
    fn integer_type_maps_correctly() {
        let ann = TypeAnnotation::simple("Integer", span());
        assert_eq!(type_annotation_to_spec(&ann), "{'type', 0, 'integer', []}");
    }

    #[test]
    fn string_type_maps_to_binary() {
        let ann = TypeAnnotation::simple("String", span());
        assert_eq!(type_annotation_to_spec(&ann), "{'type', 0, 'binary', []}");
    }

    #[test]
    fn float_type_maps_correctly() {
        let ann = TypeAnnotation::simple("Float", span());
        assert_eq!(type_annotation_to_spec(&ann), "{'type', 0, 'float', []}");
    }

    #[test]
    fn boolean_type_maps_correctly() {
        let ann = TypeAnnotation::simple("Boolean", span());
        assert_eq!(type_annotation_to_spec(&ann), "{'type', 0, 'boolean', []}");
    }

    #[test]
    fn symbol_type_maps_to_atom() {
        let ann = TypeAnnotation::simple("Symbol", span());
        assert_eq!(type_annotation_to_spec(&ann), "{'type', 0, 'atom', []}");
    }

    #[test]
    fn nil_maps_to_atom_nil() {
        let ann = TypeAnnotation::simple("Nil", span());
        assert_eq!(type_annotation_to_spec(&ann), "{'atom', 0, 'nil'}");
    }

    #[test]
    fn true_maps_to_atom_true() {
        let ann = TypeAnnotation::simple("True", span());
        assert_eq!(type_annotation_to_spec(&ann), "{'atom', 0, 'true'}");
    }

    #[test]
    fn false_maps_to_atom_false() {
        let ann = TypeAnnotation::simple("False", span());
        assert_eq!(type_annotation_to_spec(&ann), "{'atom', 0, 'false'}");
    }

    #[test]
    fn list_type_maps_correctly() {
        let ann = TypeAnnotation::simple("List", span());
        assert_eq!(type_annotation_to_spec(&ann), "{'type', 0, 'list', []}");
    }

    #[test]
    fn dictionary_maps_to_map() {
        let ann = TypeAnnotation::simple("Dictionary", span());
        assert_eq!(type_annotation_to_spec(&ann), "{'type', 0, 'map', 'any'}");
    }

    #[test]
    fn tuple_maps_correctly() {
        let ann = TypeAnnotation::simple("Tuple", span());
        assert_eq!(type_annotation_to_spec(&ann), "{'type', 0, 'tuple', 'any'}");
    }

    #[test]
    fn custom_class_maps_to_any() {
        let ann = TypeAnnotation::simple("Counter", span());
        assert_eq!(type_annotation_to_spec(&ann), "{'type', 0, 'any', []}");
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
        let result = type_annotation_to_spec(&ann);
        assert_eq!(
            result,
            "{'type', 0, 'union', [{'type', 0, 'integer', []} | [{'atom', 0, 'nil'}]]}"
        );
    }

    #[test]
    fn singleton_type_maps_to_atom() {
        let ann = TypeAnnotation::singleton("north", span());
        assert_eq!(type_annotation_to_spec(&ann), "{'atom', 0, 'north'}");
    }

    #[test]
    fn false_or_type_maps_correctly() {
        let ann = TypeAnnotation::false_or(TypeAnnotation::simple("Integer", span()), span());
        let result = type_annotation_to_spec(&ann);
        assert_eq!(
            result,
            "{'type', 0, 'union', [{'type', 0, 'integer', []} | [{'atom', 0, 'false'}]]}"
        );
    }

    #[test]
    fn unary_method_with_return_type_generates_spec() {
        // getBalance -> Integer => ...
        let method = MethodDefinition::with_return_type(
            MessageSelector::Unary("getBalance".into()),
            vec![],
            vec![],
            TypeAnnotation::simple("Integer", span()),
            span(),
        );

        let spec = generate_method_spec(&method, false).unwrap();
        assert_eq!(
            spec,
            "{'getBalance', 0}, [{'type', 0, 'fun', [{'type', 0, 'product', []} | [{'type', 0, 'integer', []}]]}]"
        );
    }

    #[test]
    fn keyword_method_with_typed_param_generates_spec() {
        // deposit: amount: Integer -> Integer => ...
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

        let spec = generate_method_spec(&method, false).unwrap();
        assert_eq!(
            spec,
            "{'deposit:', 1}, [{'type', 0, 'fun', [{'type', 0, 'product', [{'type', 0, 'integer', []}]} | [{'type', 0, 'integer', []}]]}]"
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
        // getValue -> Integer => ... (no param types)
        let method = MethodDefinition::with_return_type(
            MessageSelector::Unary("getValue".into()),
            vec![ParameterDefinition::new(Identifier::new("x", span()))],
            vec![],
            TypeAnnotation::simple("Integer", span()),
            span(),
        );

        let spec = generate_method_spec(&method, false).unwrap();
        assert!(spec.contains("'any'"));
        assert!(spec.contains("'integer'"));
    }

    #[test]
    fn value_type_method_includes_self_param() {
        // For value types, Self (a map) is the first parameter
        let method = MethodDefinition::with_return_type(
            MessageSelector::Unary("size".into()),
            vec![],
            vec![],
            TypeAnnotation::simple("Integer", span()),
            span(),
        );

        let spec = generate_method_spec(&method, true).unwrap();
        // Arity should be 1 (Self param)
        assert!(spec.contains("{'size', 1}"));
        // Product should include Self as map type
        assert!(spec.contains("'map', 'any'"));
    }

    #[test]
    fn multiple_keyword_params() {
        // transfer: amount: Integer to: target: String -> Boolean
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

        let spec = generate_method_spec(&method, false).unwrap();
        assert!(spec.contains("'transfer:to:'"));
        assert!(spec.contains("'integer'"));
        assert!(spec.contains("'binary'"));
        assert!(spec.contains("'boolean'"));
    }

    #[test]
    fn union_return_type() {
        // find: key: Symbol -> Integer | Nil
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

        let spec = generate_method_spec(&method, false).unwrap();
        assert!(spec.contains("'union'"));
        assert!(spec.contains("'integer'"));
        assert!(spec.contains("'nil'"));
    }

    #[test]
    fn block_type_maps_to_fun() {
        let ann = TypeAnnotation::simple("Block", span());
        assert_eq!(type_annotation_to_spec(&ann), "{'type', 0, 'fun', []}");
    }

    #[test]
    fn number_type_maps_correctly() {
        let ann = TypeAnnotation::simple("Number", span());
        assert_eq!(type_annotation_to_spec(&ann), "{'type', 0, 'number', []}");
    }

    #[test]
    fn character_maps_to_integer() {
        let ann = TypeAnnotation::simple("Character", span());
        assert_eq!(type_annotation_to_spec(&ann), "{'type', 0, 'integer', []}");
    }

    #[test]
    fn set_maps_to_map() {
        let ann = TypeAnnotation::simple("Set", span());
        assert_eq!(type_annotation_to_spec(&ann), "{'type', 0, 'map', 'any'}");
    }

    #[test]
    fn format_spec_attributes_empty() {
        assert!(format_spec_attributes(&[]).is_none());
    }

    #[test]
    fn format_spec_attributes_single() {
        let specs = vec!["'spec' =\n        [{{spec}}]".to_string()];
        let result = format_spec_attributes(&specs).unwrap();
        assert_eq!(result, "'spec' =\n        [{{spec}}]");
    }

    #[test]
    fn format_spec_attributes_multiple() {
        let specs = vec![
            "'spec' =\n        [{{spec1}}]".to_string(),
            "'spec' =\n        [{{spec2}}]".to_string(),
        ];
        let result = format_spec_attributes(&specs).unwrap();
        assert!(result.contains("spec1"));
        assert!(result.contains("spec2"));
    }

    #[test]
    fn value_type_method_with_two_params_generates_correct_cons() {
        // For value types with 2 params: product has 3 types (Self + 2 params)
        // Must produce [Self | [Param1 | [Param2]]] not [Self | [Param1] | [Param2]]
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

        let spec = generate_method_spec(&method, true).unwrap();
        // Arity = 3 (Self + 2 params)
        assert!(spec.contains("{'at:put:', 3}"));
        // Product must use proper cons nesting
        assert!(
            spec.contains(
                "{'type', 0, 'map', 'any'} | [{'type', 0, 'atom', []} | [{'type', 0, 'any', []}]]"
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
        let result = type_annotation_to_spec(&ann);
        assert_eq!(
            result,
            "{'type', 0, 'union', [{'type', 0, 'integer', []} | [{'type', 0, 'binary', []} | [{'atom', 0, 'nil'}]]]}"
        );
    }
}
