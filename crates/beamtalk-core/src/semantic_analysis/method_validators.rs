// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Method-specific semantic validation framework.
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module provides a registry of validators that check method arguments
//! during semantic analysis. Each validator is associated with one or more
//! message selectors and can produce diagnostics when arguments don't meet
//! expected constraints.
//!
//! Additionally, receiver-based validators check the receiver expression
//! for known anti-patterns like instantiating primitive classes or mutating
//! immutable literals.
//!
//! ## Example
//!
//! The `ReflectionMethodValidator` checks that reflection methods like
//! `respondsTo:`, `instVarAt:`, `instVarAt:put:`, and `classNamed:` receive
//! symbol literal arguments rather than bare identifiers.

use crate::ast::{Expression, Literal, MessageSelector};
use crate::source_analysis::{Diagnostic, Span};
use std::collections::HashMap;

/// A validator for method-specific argument constraints.
pub(crate) trait MethodValidator {
    /// Validate the arguments of a message send.
    ///
    /// Returns diagnostics for any invalid arguments.
    fn validate(
        &self,
        selector: &MessageSelector,
        arguments: &[Expression],
        span: Span,
    ) -> Vec<Diagnostic>;
}

/// Registry mapping message selectors to their validators.
pub(crate) struct MethodValidatorRegistry {
    validators: HashMap<&'static str, Box<dyn MethodValidator>>,
}

impl MethodValidatorRegistry {
    /// Create a new registry with all built-in validators.
    pub(crate) fn new() -> Self {
        let mut registry = Self {
            validators: HashMap::new(),
        };
        registry.register_builtins();
        registry
    }

    /// Look up a validator for the given selector name.
    pub(crate) fn get(&self, selector_name: &str) -> Option<&dyn MethodValidator> {
        self.validators.get(selector_name).map(AsRef::as_ref)
    }

    fn register_builtins(&mut self) {
        let reflection = Box::new(ReflectionMethodValidator);
        self.validators.insert("respondsTo:", reflection);

        let reflection = Box::new(ReflectionMethodValidator);
        self.validators.insert("instVarAt:", reflection);

        let reflection = Box::new(ReflectionMethodValidator);
        self.validators.insert("instVarAt:put:", reflection);

        let reflection = Box::new(ReflectionMethodValidator);
        self.validators.insert("classNamed:", reflection);
    }
}

/// Validates that reflection methods receive symbol literal arguments.
///
/// Methods like `respondsTo:`, `instVarAt:`, and `classNamed:` expect
/// symbol literals (e.g., `#increment`). When users pass bare identifiers,
/// this validator produces a helpful diagnostic suggesting `#symbol` syntax.
struct ReflectionMethodValidator;

impl MethodValidator for ReflectionMethodValidator {
    fn validate(
        &self,
        selector: &MessageSelector,
        arguments: &[Expression],
        _span: Span,
    ) -> Vec<Diagnostic> {
        if arguments.is_empty() {
            return vec![];
        }

        let first_arg = &arguments[0];
        let selector_name = selector.name();

        match first_arg {
            // Symbol literal is correct usage
            Expression::Literal(Literal::Symbol(_), _) => vec![],

            // Bare identifier - most common mistake
            Expression::Identifier(id) => {
                let message = format!(
                    "{selector_name} expects a symbol literal, not an identifier\n\
                     \n\
                     = note: {selector_name} {}",
                    method_description(&selector_name),
                );
                vec![Diagnostic {
                    severity: crate::source_analysis::Severity::Error,
                    message: message.into(),
                    span: id.span,
                    hint: Some(format!("Use #{} instead of {}", id.name, id.name).into()),
                }]
            }

            // Class reference (e.g., `classNamed: Counter` instead of `classNamed: #Counter`)
            Expression::ClassReference { name, span, .. } => {
                let message = format!(
                    "{selector_name} expects a symbol literal, not a class reference\n\
                     \n\
                     = note: {selector_name} {}",
                    method_description(&selector_name),
                );
                vec![Diagnostic {
                    severity: crate::source_analysis::Severity::Error,
                    message: message.into(),
                    span: *span,
                    hint: Some(format!("Use #{} instead of {}", name.name, name.name).into()),
                }]
            }

            // Other expression types (numbers, strings, blocks, etc.)
            other => {
                let message = format!(
                    "{selector_name} expects a symbol literal\n\
                     \n\
                     = note: {selector_name} {}",
                    method_description(&selector_name),
                );
                vec![Diagnostic {
                    severity: crate::source_analysis::Severity::Error,
                    message: message.into(),
                    span: other.span(),
                    hint: Some("Use a symbol literal like #methodName".into()),
                }]
            }
        }
    }
}

/// Returns a human-readable description of what a reflection method does.
fn method_description(selector: &str) -> &'static str {
    match selector {
        "respondsTo:" => "checks if an object understands a message",
        "instVarAt:" => "accesses an instance variable by name",
        "instVarAt:put:" => "sets an instance variable by name",
        "classNamed:" => "looks up a class by name in the system dictionary",
        _ => "requires a symbol argument",
    }
}

/// Primitive class names that cannot be instantiated with `new` or `new:`.
const PRIMITIVE_CLASS_NAMES: &[&str] = &[
    "Integer", "String", "Float", "True", "False", "Nil", "Symbol",
];

/// Check if the receiver of `new` or `new:` is a known primitive class.
///
/// Returns a diagnostic if `Integer new`, `String new:`, etc. is detected.
pub(crate) fn validate_primitive_instantiation(
    receiver: &Expression,
    selector_name: &str,
    span: Span,
) -> Option<Diagnostic> {
    if selector_name != "new" && selector_name != "new:" {
        return None;
    }

    let class_name = match receiver {
        Expression::ClassReference { name, .. } => &name.name,
        _ => return None,
    };

    if !PRIMITIVE_CLASS_NAMES
        .iter()
        .any(|&p| p == class_name.as_str())
    {
        return None;
    }

    let hint = match class_name.as_str() {
        "Integer" => "Integer values are created with literals (e.g., 42)",
        "Float" => "Float values are created with literals (e.g., 3.14)",
        "String" => "String values are created with literals (e.g., 'hello')",
        "Symbol" => "Symbol values are created with literals (e.g., #name)",
        "True" => "Use the literal `true` instead",
        "False" => "Use the literal `false` instead",
        "Nil" => "Use the literal `nil` instead",
        _ => "Primitive values are created with literals",
    };

    Some(Diagnostic {
        severity: crate::source_analysis::Severity::Error,
        message: format!("Cannot instantiate primitive class `{class_name}`").into(),
        span,
        hint: Some(hint.into()),
    })
}

/// Check if the receiver of `instVarAt:put:` is an immutable literal.
///
/// Returns a diagnostic if `42 instVarAt: #x put: 99` is detected.
pub(crate) fn validate_immutable_mutation(
    receiver: &Expression,
    selector_name: &str,
    span: Span,
) -> Option<Diagnostic> {
    if selector_name != "instVarAt:put:" {
        return None;
    }

    let type_name = match receiver {
        Expression::Literal(Literal::Integer(_), _) => "Integer literal",
        Expression::Literal(Literal::Float(_), _) => "Float literal",
        Expression::Literal(Literal::String(_), _) => "String literal",
        Expression::Literal(Literal::Symbol(_), _) => "Symbol literal",
        Expression::Literal(Literal::Character(_), _) => "Character literal",
        Expression::Identifier(id) if matches!(id.name.as_str(), "true" | "false") => {
            "Boolean literal"
        }
        Expression::Identifier(id) if id.name.as_str() == "nil" => "nil",
        _ => return None,
    };

    Some(Diagnostic {
        severity: crate::source_analysis::Severity::Error,
        message: format!("Cannot mutate immutable value ({type_name})").into(),
        span,
        hint: Some("Primitive values like integers, strings, and booleans are immutable".into()),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Identifier, KeywordPart};
    use crate::source_analysis::Span;

    fn test_span() -> Span {
        Span::new(0, 10)
    }

    fn responds_to_selector() -> MessageSelector {
        MessageSelector::Keyword(vec![KeywordPart::new("respondsTo:", test_span())])
    }

    fn class_named_selector() -> MessageSelector {
        MessageSelector::Keyword(vec![KeywordPart::new("classNamed:", test_span())])
    }

    #[test]
    fn test_symbol_literal_is_valid() {
        let validator = ReflectionMethodValidator;
        let args = vec![Expression::Literal(
            Literal::Symbol("increment".into()),
            test_span(),
        )];

        let diagnostics = validator.validate(&responds_to_selector(), &args, test_span());
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_identifier_produces_error() {
        let validator = ReflectionMethodValidator;
        let args = vec![Expression::Identifier(Identifier::new(
            "increment",
            Span::new(15, 24),
        ))];

        let diagnostics = validator.validate(&responds_to_selector(), &args, test_span());
        assert_eq!(diagnostics.len(), 1);
        assert!(diagnostics[0].message.contains("expects a symbol literal"));
        assert!(
            diagnostics[0]
                .message
                .contains("checks if an object understands a message")
        );
        assert_eq!(diagnostics[0].span, Span::new(15, 24));
        // Fix suggestion is in the hint field only (not duplicated in message)
        let hint = diagnostics[0].hint.as_ref().unwrap();
        assert!(hint.contains("#increment"));
    }

    #[test]
    fn test_class_reference_produces_error() {
        let validator = ReflectionMethodValidator;
        let args = vec![Expression::ClassReference {
            name: Identifier::new("Counter", Span::new(12, 19)),
            span: Span::new(12, 19),
        }];

        let diagnostics = validator.validate(&class_named_selector(), &args, test_span());
        assert_eq!(diagnostics.len(), 1);
        assert!(
            diagnostics[0]
                .message
                .contains("expects a symbol literal, not a class reference")
        );
        let hint = diagnostics[0].hint.as_ref().unwrap();
        assert!(hint.contains("#Counter"));
    }

    #[test]
    fn test_integer_literal_produces_error() {
        let validator = ReflectionMethodValidator;
        let args = vec![Expression::Literal(Literal::Integer(42), test_span())];

        let diagnostics = validator.validate(&responds_to_selector(), &args, test_span());
        assert_eq!(diagnostics.len(), 1);
        assert!(diagnostics[0].message.contains("expects a symbol literal"));
    }

    #[test]
    fn test_empty_arguments_no_error() {
        let validator = ReflectionMethodValidator;
        let diagnostics = validator.validate(&responds_to_selector(), &[], test_span());
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_registry_finds_responds_to() {
        let registry = MethodValidatorRegistry::new();
        assert!(registry.get("respondsTo:").is_some());
    }

    #[test]
    fn test_registry_finds_inst_var_at() {
        let registry = MethodValidatorRegistry::new();
        assert!(registry.get("instVarAt:").is_some());
    }

    #[test]
    fn test_registry_finds_class_named() {
        let registry = MethodValidatorRegistry::new();
        assert!(registry.get("classNamed:").is_some());
    }

    #[test]
    fn test_registry_unknown_selector_returns_none() {
        let registry = MethodValidatorRegistry::new();
        assert!(registry.get("unknownMethod:").is_none());
    }

    #[test]
    fn test_hint_contains_fix_suggestion() {
        let validator = ReflectionMethodValidator;
        let args = vec![Expression::Identifier(Identifier::new(
            "increment",
            test_span(),
        ))];

        let diagnostics = validator.validate(&responds_to_selector(), &args, test_span());
        assert_eq!(diagnostics.len(), 1);
        let hint = diagnostics[0].hint.as_ref().unwrap();
        assert!(hint.contains("#increment"));
    }

    #[test]
    fn test_method_description_responds_to() {
        assert_eq!(
            method_description("respondsTo:"),
            "checks if an object understands a message"
        );
    }

    #[test]
    fn test_method_description_class_named() {
        assert_eq!(
            method_description("classNamed:"),
            "looks up a class by name in the system dictionary"
        );
    }

    #[test]
    fn test_method_description_inst_var_at() {
        assert_eq!(
            method_description("instVarAt:"),
            "accesses an instance variable by name"
        );
    }

    #[test]
    fn test_method_description_inst_var_at_put() {
        assert_eq!(
            method_description("instVarAt:put:"),
            "sets an instance variable by name"
        );
    }

    #[test]
    fn test_registry_finds_inst_var_at_put() {
        let registry = MethodValidatorRegistry::new();
        assert!(registry.get("instVarAt:put:").is_some());
    }

    #[test]
    fn test_inst_var_at_put_symbol_is_valid() {
        let validator = ReflectionMethodValidator;
        let selector = MessageSelector::Keyword(vec![
            KeywordPart::new("instVarAt:", test_span()),
            KeywordPart::new("put:", test_span()),
        ]);
        let args = vec![
            Expression::Literal(Literal::Symbol("x".into()), test_span()),
            Expression::Literal(Literal::Integer(99), test_span()),
        ];
        let diagnostics = validator.validate(&selector, &args, test_span());
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_inst_var_at_put_identifier_produces_error() {
        let validator = ReflectionMethodValidator;
        let selector = MessageSelector::Keyword(vec![
            KeywordPart::new("instVarAt:", test_span()),
            KeywordPart::new("put:", test_span()),
        ]);
        let args = vec![
            Expression::Identifier(Identifier::new("x", Span::new(15, 16))),
            Expression::Literal(Literal::Integer(99), test_span()),
        ];
        let diagnostics = validator.validate(&selector, &args, test_span());
        assert_eq!(diagnostics.len(), 1);
        assert!(diagnostics[0].message.contains("expects a symbol literal"));
        let hint = diagnostics[0].hint.as_ref().unwrap();
        assert!(hint.contains("#x"));
    }

    // ── Primitive instantiation validator tests ──────────────────────────

    #[test]
    fn test_primitive_instantiation_integer_new() {
        let receiver = Expression::ClassReference {
            name: Identifier::new("Integer", test_span()),
            span: test_span(),
        };
        let diag = validate_primitive_instantiation(&receiver, "new", test_span());
        assert!(diag.is_some());
        let diag = diag.unwrap();
        assert!(
            diag.message
                .contains("Cannot instantiate primitive class `Integer`")
        );
        assert!(diag.hint.as_ref().unwrap().contains("42"));
    }

    #[test]
    fn test_primitive_instantiation_string_new() {
        let receiver = Expression::ClassReference {
            name: Identifier::new("String", test_span()),
            span: test_span(),
        };
        let diag = validate_primitive_instantiation(&receiver, "new", test_span());
        assert!(diag.is_some());
        assert!(diag.unwrap().message.contains("`String`"));
    }

    #[test]
    fn test_primitive_instantiation_true_new() {
        let receiver = Expression::ClassReference {
            name: Identifier::new("True", test_span()),
            span: test_span(),
        };
        let diag = validate_primitive_instantiation(&receiver, "new", test_span());
        assert!(diag.is_some());
        assert!(diag.unwrap().hint.as_ref().unwrap().contains("`true`"));
    }

    #[test]
    fn test_primitive_instantiation_symbol_new_colon() {
        let receiver = Expression::ClassReference {
            name: Identifier::new("Symbol", test_span()),
            span: test_span(),
        };
        let diag = validate_primitive_instantiation(&receiver, "new:", test_span());
        assert!(diag.is_some());
        assert!(diag.unwrap().message.contains("`Symbol`"));
    }

    #[test]
    fn test_primitive_instantiation_actor_new_is_ok() {
        let receiver = Expression::ClassReference {
            name: Identifier::new("Actor", test_span()),
            span: test_span(),
        };
        let diag = validate_primitive_instantiation(&receiver, "new", test_span());
        assert!(diag.is_none());
    }

    #[test]
    fn test_primitive_instantiation_non_class_receiver_is_ok() {
        let receiver = Expression::Identifier(Identifier::new("x", test_span()));
        let diag = validate_primitive_instantiation(&receiver, "new", test_span());
        assert!(diag.is_none());
    }

    #[test]
    fn test_primitive_instantiation_other_selector_is_ok() {
        let receiver = Expression::ClassReference {
            name: Identifier::new("Integer", test_span()),
            span: test_span(),
        };
        let diag = validate_primitive_instantiation(&receiver, "spawn", test_span());
        assert!(diag.is_none());
    }

    // ── Immutable mutation validator tests ───────────────────────────────

    #[test]
    fn test_immutable_mutation_integer_literal() {
        let receiver = Expression::Literal(Literal::Integer(42), test_span());
        let diag = validate_immutable_mutation(&receiver, "instVarAt:put:", test_span());
        assert!(diag.is_some());
        let diag = diag.unwrap();
        assert!(
            diag.message
                .contains("Cannot mutate immutable value (Integer literal)")
        );
        assert!(diag.hint.as_ref().unwrap().contains("immutable"));
    }

    #[test]
    fn test_immutable_mutation_string_literal() {
        let receiver = Expression::Literal(Literal::String("hello".into()), test_span());
        let diag = validate_immutable_mutation(&receiver, "instVarAt:put:", test_span());
        assert!(diag.is_some());
        assert!(diag.unwrap().message.contains("String literal"));
    }

    #[test]
    fn test_immutable_mutation_float_literal() {
        let receiver = Expression::Literal(Literal::Float(1.5), test_span());
        let diag = validate_immutable_mutation(&receiver, "instVarAt:put:", test_span());
        assert!(diag.is_some());
        assert!(diag.unwrap().message.contains("Float literal"));
    }

    #[test]
    fn test_immutable_mutation_symbol_literal() {
        let receiver = Expression::Literal(Literal::Symbol("x".into()), test_span());
        let diag = validate_immutable_mutation(&receiver, "instVarAt:put:", test_span());
        assert!(diag.is_some());
        assert!(diag.unwrap().message.contains("Symbol literal"));
    }

    #[test]
    fn test_immutable_mutation_character_literal() {
        let receiver = Expression::Literal(Literal::Character('a'), test_span());
        let diag = validate_immutable_mutation(&receiver, "instVarAt:put:", test_span());
        assert!(diag.is_some());
        assert!(diag.unwrap().message.contains("Character literal"));
    }

    #[test]
    fn test_immutable_mutation_true_identifier() {
        let receiver = Expression::Identifier(Identifier::new("true", test_span()));
        let diag = validate_immutable_mutation(&receiver, "instVarAt:put:", test_span());
        assert!(diag.is_some());
        assert!(diag.unwrap().message.contains("Boolean literal"));
    }

    #[test]
    fn test_immutable_mutation_false_identifier() {
        let receiver = Expression::Identifier(Identifier::new("false", test_span()));
        let diag = validate_immutable_mutation(&receiver, "instVarAt:put:", test_span());
        assert!(diag.is_some());
        assert!(diag.unwrap().message.contains("Boolean literal"));
    }

    #[test]
    fn test_immutable_mutation_nil_identifier() {
        let receiver = Expression::Identifier(Identifier::new("nil", test_span()));
        let diag = validate_immutable_mutation(&receiver, "instVarAt:put:", test_span());
        assert!(diag.is_some());
        assert!(diag.unwrap().message.contains("nil"));
    }

    #[test]
    fn test_immutable_mutation_variable_is_ok() {
        let receiver = Expression::Identifier(Identifier::new("obj", test_span()));
        let diag = validate_immutable_mutation(&receiver, "instVarAt:put:", test_span());
        assert!(diag.is_none());
    }

    #[test]
    fn test_immutable_mutation_other_selector_is_ok() {
        let receiver = Expression::Literal(Literal::Integer(42), test_span());
        let diag = validate_immutable_mutation(&receiver, "respondsTo:", test_span());
        assert!(diag.is_none());
    }
}
