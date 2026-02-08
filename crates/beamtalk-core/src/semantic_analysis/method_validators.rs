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
//! ## Example
//!
//! The `ReflectionMethodValidator` checks that reflection methods like
//! `respondsTo:`, `instVarAt:`, and `classNamed:` receive symbol literal
//! arguments rather than bare identifiers.

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
        "classNamed:" => "looks up a class by name in the system dictionary",
        _ => "requires a symbol argument",
    }
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
}
