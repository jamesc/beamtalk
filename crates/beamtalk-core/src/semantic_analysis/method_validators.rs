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
//! `respondsTo:`, `fieldAt:`, `fieldAt:put:`, and `classNamed:` receive
//! symbol literal arguments rather than bare identifiers.

use crate::ast::{Expression, Literal, MessageSelector};
use crate::semantic_analysis::type_checker::InferredType;
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use std::collections::HashMap;

/// A validator for method-specific argument constraints.
pub(crate) trait MethodValidator {
    /// Validate the arguments of a message send.
    ///
    /// `receiver_type` is the inferred type of the receiver expression,
    /// if available. Validators can use this to disambiguate polymorphic
    /// methods (e.g., `at:` on List vs Dictionary).
    ///
    /// Returns diagnostics for any invalid arguments.
    fn validate(
        &self,
        selector: &MessageSelector,
        arguments: &[Expression],
        receiver_type: Option<&InferredType>,
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
        self.validators.insert("fieldAt:", reflection);

        let reflection = Box::new(ReflectionMethodValidator);
        self.validators.insert("fieldAt:put:", reflection);

        let reflection = Box::new(ReflectionMethodValidator);
        self.validators.insert("classNamed:", reflection);

        // Block arity validators (1-parameter block)
        for selector in &[
            "do:",
            "collect:",
            "select:",
            "reject:",
            "detect:",
            "anySatisfy:",
            "allSatisfy:",
            "groupBy:",
            "partition:",
        ] {
            self.validators
                .insert(selector, Box::new(BlockArityValidator::new(1, 0)));
        }

        // Block arity validators (2-parameter block at arg 0)
        for selector in &["eachWithIndex:", "sortWith:", "keysAndValuesDo:"] {
            self.validators
                .insert(selector, Box::new(BlockArityValidator::new(2, 0)));
        }

        // Block arity validators (0-parameter block)
        for selector in &["whileTrue:", "whileFalse:", "timesRepeat:"] {
            self.validators
                .insert(selector, Box::new(BlockArityValidator::new(0, 0)));
        }

        // inject:into: — block is the second argument (index 1)
        self.validators
            .insert("inject:into:", Box::new(BlockArityValidator::new(2, 1)));

        // Integer argument validators
        for selector in &["take:", "drop:", "repeat:", "from:to:"] {
            self.validators
                .insert(selector, Box::new(IntegerArgumentValidator));
        }

        // Type-aware integer validators (polymorphic methods)
        // at: and at:put: require integer args on List/Tuple/String, but
        // Dictionary uses any key type. Only validate when receiver type is known.
        for selector in &["at:", "at:put:"] {
            self.validators
                .insert(selector, Box::new(TypeAwareIntegerArgValidator));
        }

        // String argument validators
        for selector in &["startsWith:", "endsWith:", "splitOn:"] {
            self.validators
                .insert(selector, Box::new(StringArgumentValidator));
        }

        // Type-aware string validators (polymorphic methods)
        // includes: and indexOf: require string args on String, but
        // List/Collection accepts any element type.
        for selector in &["includes:", "indexOf:"] {
            self.validators
                .insert(selector, Box::new(TypeAwareStringArgValidator));
        }
    }
}

/// Validates that reflection methods receive symbol literal arguments.
///
/// Methods like `respondsTo:`, `fieldAt:`, and `classNamed:` expect
/// symbol literals (e.g., `#increment`). When users pass bare identifiers,
/// this validator produces a helpful diagnostic suggesting `#symbol` syntax.
struct ReflectionMethodValidator;

impl MethodValidator for ReflectionMethodValidator {
    fn validate(
        &self,
        selector: &MessageSelector,
        arguments: &[Expression],
        _receiver_type: Option<&InferredType>,
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
                    category: None,
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
                    category: None,
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
                    category: None,
                }]
            }
        }
    }
}

/// Returns a human-readable description of what a reflection method does.
fn method_description(selector: &str) -> &'static str {
    match selector {
        "respondsTo:" => "checks if an object understands a message",
        "fieldAt:" => "accesses a field by name",
        "fieldAt:put:" => "sets a field by name",
        "classNamed:" => "looks up a class by name in the system dictionary",
        _ => "requires a symbol argument",
    }
}

/// Primitive class names that cannot be instantiated with `new` or `new:`.
const PRIMITIVE_CLASS_NAMES: &[&str] = &[
    "Integer",
    "String",
    "Float",
    "True",
    "False",
    "UndefinedObject",
    "Symbol",
    "Block",
    "CompiledMethod",
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
        "UndefinedObject" => "Use the literal `nil` instead",
        "Block" => "Block values are created with block literals (e.g., [:x | x + 1])",
        "CompiledMethod" => {
            "CompiledMethod instances are created by the compiler when defining methods"
        }
        _ => "Primitive values are created with literals",
    };

    Some(Diagnostic {
        severity: crate::source_analysis::Severity::Error,
        message: format!("Cannot instantiate primitive class `{class_name}`").into(),
        span,
        hint: Some(hint.into()),
        category: None,
    })
}

/// Check if the receiver of `fieldAt:put:` is an immutable literal.
///
/// Returns a diagnostic if `42 fieldAt: #x put: 99` is detected.
pub(crate) fn validate_immutable_mutation(
    receiver: &Expression,
    selector_name: &str,
    span: Span,
) -> Option<Diagnostic> {
    if selector_name != "fieldAt:put:" {
        return None;
    }

    let type_name = match receiver {
        Expression::Literal(Literal::Integer(_), _) => "Integer literal",
        Expression::Literal(Literal::Float(_), _) => "Float literal",
        Expression::Literal(Literal::String(_), _) => "String literal",
        Expression::Literal(Literal::Symbol(_), _) => "Symbol literal",
        Expression::Literal(Literal::Character(_), _) => "Character literal",
        Expression::Literal(Literal::List(_), _) | Expression::ListLiteral { .. } => "List literal",
        Expression::Identifier(id) if matches!(id.name.as_str(), "true" | "false") => {
            "Boolean literal"
        }
        Expression::Identifier(id) if id.name.as_str() == "nil" => "nil",
        _ => return None,
    };

    Some(Diagnostic {
        severity: crate::source_analysis::Severity::Warning,
        message: format!("Cannot mutate immutable value ({type_name})").into(),
        span,
        hint: Some("Primitive values like integers, strings, and booleans are immutable".into()),
        category: Some(DiagnosticCategory::Type),
    })
}

// ── Block arity validator ───────────────────────────────────────────────

/// Expected block arity for a method, with the argument position (0-based).
struct BlockArityExpectation {
    arity: usize,
    arg_index: usize,
}

/// Validates that literal block arguments have the expected number of parameters.
///
/// Only fires when the argument is a literal `Block` expression.
/// Variables and other expressions are skipped (runtime handles those).
struct BlockArityValidator {
    expectation: BlockArityExpectation,
}

impl BlockArityValidator {
    const fn new(arity: usize, arg_index: usize) -> Self {
        Self {
            expectation: BlockArityExpectation { arity, arg_index },
        }
    }
}

impl MethodValidator for BlockArityValidator {
    fn validate(
        &self,
        selector: &MessageSelector,
        arguments: &[Expression],
        _receiver_type: Option<&InferredType>,
        _span: Span,
    ) -> Vec<Diagnostic> {
        let idx = self.expectation.arg_index;
        let expected = self.expectation.arity;

        let Some(arg) = arguments.get(idx) else {
            return vec![];
        };

        let Expression::Block(block) = arg else {
            return vec![];
        };

        let actual = block.arity();
        if actual == expected {
            return vec![];
        }

        let selector_name = selector.name();
        let params_word = if expected == 1 {
            "parameter"
        } else {
            "parameters"
        };
        let message = format!(
            "{selector_name} expects a block with {expected} {params_word}, but got {actual}"
        );
        let hint = block_arity_hint(&selector_name, expected);

        vec![Diagnostic {
            severity: crate::source_analysis::Severity::Error,
            message: message.into(),
            span: block.span,
            hint: Some(hint.into()),
            category: None,
        }]
    }
}

fn block_arity_hint(selector: &str, expected: usize) -> String {
    match (selector, expected) {
        (
            "do:" | "collect:" | "select:" | "reject:" | "detect:" | "anySatisfy:" | "allSatisfy:"
            | "groupBy:" | "partition:",
            1,
        ) => {
            format!("Use a 1-parameter block: list {selector} [:item | ...]")
        }
        ("eachWithIndex:", 2) => {
            "Use a 2-parameter block: list eachWithIndex: [:item :index | ...]".into()
        }
        ("sortWith:", 2) => "Use a 2-parameter block: list sortWith: [:a :b | ...]".into(),
        ("inject:into:", 2) => {
            "Use a 2-parameter block: list inject: 0 into: [:acc :item | ...]".into()
        }
        ("keysAndValuesDo:", 2) => {
            "Use a 2-parameter block: dict keysAndValuesDo: [:key :value | ...]".into()
        }
        (_, 0) => format!("Use a 0-parameter block: receiver {selector} [...]"),
        _ => format!("Use a block with {expected} parameters"),
    }
}

// ── Integer argument validator ──────────────────────────────────────────

/// Validates that methods expecting integer arguments receive integer literals.
///
/// Only flags literal non-integer arguments. Variables and other expressions
/// are skipped (runtime type guards handle those).
struct IntegerArgumentValidator;

impl MethodValidator for IntegerArgumentValidator {
    fn validate(
        &self,
        selector: &MessageSelector,
        arguments: &[Expression],
        _receiver_type: Option<&InferredType>,
        _span: Span,
    ) -> Vec<Diagnostic> {
        if arguments.is_empty() {
            return vec![];
        }

        let selector_name = selector.name();
        // from:to: has two integer bounds; other methods have one
        let indices: &[usize] = if selector_name == "from:to:" {
            &[0, 1]
        } else {
            &[0]
        };
        let mut diags = Vec::new();

        for &idx in indices {
            let Some(arg) = arguments.get(idx) else {
                continue;
            };
            match arg {
                Expression::Literal(Literal::Integer(_), _) => {}
                Expression::Literal(lit, lit_span) => {
                    let type_name = literal_type_name(lit);
                    diags.push(Diagnostic {
                        severity: crate::source_analysis::Severity::Warning,
                        message: format!(
                            "{selector_name} expects an integer argument, got {type_name}"
                        )
                        .into(),
                        span: *lit_span,
                        hint: Some(integer_arg_hint(&selector_name).into()),
                        category: None,
                    });
                }
                _ => {}
            }
        }

        diags
    }
}

fn literal_type_name(lit: &Literal) -> &'static str {
    match lit {
        Literal::Integer(_) => "an integer",
        Literal::Float(_) => "a float",
        Literal::String(_) => "a string",
        Literal::Symbol(_) => "a symbol",
        Literal::Character(_) => "a character",
        Literal::List(_) => "a list",
    }
}

fn integer_arg_hint(selector: &str) -> String {
    match selector {
        "take:" => "Use an integer count: list take: 3".into(),
        "drop:" => "Use an integer count: list drop: 2".into(),
        "repeat:" => "Use an integer count: string repeat: 3".into(),
        "from:to:" => "Use integer bounds: list from: 1 to: 3".into(),
        _ => format!("Use an integer argument: receiver {selector} 1"),
    }
}

// ── String argument validator ───────────────────────────────────────────

/// Validates that methods expecting string arguments receive string literals.
///
/// Only flags literal non-string arguments. Variables and other expressions
/// are skipped (runtime type guards handle those).
struct StringArgumentValidator;

impl MethodValidator for StringArgumentValidator {
    fn validate(
        &self,
        selector: &MessageSelector,
        arguments: &[Expression],
        _receiver_type: Option<&InferredType>,
        _span: Span,
    ) -> Vec<Diagnostic> {
        if arguments.is_empty() {
            return vec![];
        }

        let first_arg = &arguments[0];
        match first_arg {
            // String literal — correct
            Expression::Literal(Literal::String(_), _) => vec![],

            // Other literal types — definitely wrong
            Expression::Literal(lit, lit_span) => {
                let type_name = literal_type_name(lit);
                let selector_name = selector.name();
                vec![Diagnostic {
                    severity: crate::source_analysis::Severity::Error,
                    message: format!("{selector_name} expects a string argument, got {type_name}")
                        .into(),
                    span: *lit_span,
                    hint: Some(string_arg_hint(&selector_name).into()),
                    category: None,
                }]
            }

            // Non-literal — skip (runtime handles)
            _ => vec![],
        }
    }
}

fn string_arg_hint(selector: &str) -> String {
    match selector {
        "includes:" => r#"Use a string argument: "hello" includes: "ell""#.into(),
        "startsWith:" => r#"Use a string argument: "hello" startsWith: "hel""#.into(),
        "endsWith:" => r#"Use a string argument: "hello" endsWith: "llo""#.into(),
        "indexOf:" => r#"Use a string argument: "hello" indexOf: "ell""#.into(),
        "splitOn:" => r#"Use a string argument: "a,b,c" splitOn: ",""#.into(),
        _ => format!(r#"Use a string argument: receiver {selector} "text""#),
    }
}

// ── Type-aware validators (polymorphic methods) ─────────────────────────

/// Classes where `at:` and `at:put:` require integer arguments.
const INTEGER_AT_CLASSES: &[&str] = &["List", "Tuple", "String"];

/// Validates `at:` and `at:put:` with receiver type awareness.
///
/// When the receiver is a List, Tuple, or String, requires integer arguments.
/// When the receiver is a Dictionary or unknown, skips validation.
struct TypeAwareIntegerArgValidator;

impl MethodValidator for TypeAwareIntegerArgValidator {
    fn validate(
        &self,
        selector: &MessageSelector,
        arguments: &[Expression],
        receiver_type: Option<&InferredType>,
        _span: Span,
    ) -> Vec<Diagnostic> {
        if arguments.is_empty() {
            return vec![];
        }

        // Only validate when receiver type is a known integer-indexed class
        let is_integer_indexed = match receiver_type {
            Some(InferredType::Known(class_name)) => {
                INTEGER_AT_CLASSES.iter().any(|&c| c == class_name.as_str())
            }
            _ => false,
        };

        if !is_integer_indexed {
            return vec![];
        }

        let first_arg = &arguments[0];
        match first_arg {
            Expression::Literal(Literal::Integer(_), _) => vec![],
            Expression::Literal(lit, lit_span) => {
                let type_name = literal_type_name(lit);
                let selector_name = selector.name();
                vec![Diagnostic {
                    severity: crate::source_analysis::Severity::Error,
                    message: format!(
                        "{selector_name} expects an integer argument, got {type_name}"
                    )
                    .into(),
                    span: *lit_span,
                    hint: Some("Use a 1-based integer index: list at: 1".into()),
                    category: None,
                }]
            }
            _ => vec![],
        }
    }
}

/// Classes where `includes:` and `indexOf:` require string arguments.
const STRING_INCLUDES_CLASSES: &[&str] = &["String"];

/// Validates `includes:` and `indexOf:` with receiver type awareness.
///
/// When the receiver is a String, requires string arguments.
/// When the receiver is a List/Collection or unknown, skips validation.
struct TypeAwareStringArgValidator;

impl MethodValidator for TypeAwareStringArgValidator {
    fn validate(
        &self,
        selector: &MessageSelector,
        arguments: &[Expression],
        receiver_type: Option<&InferredType>,
        _span: Span,
    ) -> Vec<Diagnostic> {
        if arguments.is_empty() {
            return vec![];
        }

        // Only validate when receiver is known to be String
        let is_string_receiver = match receiver_type {
            Some(InferredType::Known(class_name)) => STRING_INCLUDES_CLASSES
                .iter()
                .any(|&c| c == class_name.as_str()),
            _ => false,
        };

        if !is_string_receiver {
            return vec![];
        }

        let first_arg = &arguments[0];
        match first_arg {
            Expression::Literal(Literal::String(_), _) => vec![],
            Expression::Literal(lit, lit_span) => {
                let type_name = literal_type_name(lit);
                let selector_name = selector.name();
                vec![Diagnostic {
                    severity: crate::source_analysis::Severity::Error,
                    message: format!("{selector_name} expects a string argument, got {type_name}")
                        .into(),
                    span: *lit_span,
                    hint: Some(string_arg_hint(&selector_name).into()),
                    category: None,
                }]
            }
            _ => vec![],
        }
    }
}

#[cfg(test)]
mod tests {
    //! Tests for method-specific semantic validators (responds-to, inst-var-at, primitives, mutations).
    use super::*;
    use crate::ast::{Block, BlockParameter, ExpressionStatement, Identifier, KeywordPart};
    use crate::semantic_analysis::test_helpers::test_span;
    use crate::source_analysis::Span;

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

        let diagnostics = validator.validate(&responds_to_selector(), &args, None, test_span());
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_identifier_produces_error() {
        let validator = ReflectionMethodValidator;
        let args = vec![Expression::Identifier(Identifier::new(
            "increment",
            Span::new(15, 24),
        ))];

        let diagnostics = validator.validate(&responds_to_selector(), &args, None, test_span());
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

        let diagnostics = validator.validate(&class_named_selector(), &args, None, test_span());
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

        let diagnostics = validator.validate(&responds_to_selector(), &args, None, test_span());
        assert_eq!(diagnostics.len(), 1);
        assert!(diagnostics[0].message.contains("expects a symbol literal"));
    }

    #[test]
    fn test_empty_arguments_no_error() {
        let validator = ReflectionMethodValidator;
        let diagnostics = validator.validate(&responds_to_selector(), &[], None, test_span());
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
        assert!(registry.get("fieldAt:").is_some());
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

        let diagnostics = validator.validate(&responds_to_selector(), &args, None, test_span());
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
        assert_eq!(method_description("fieldAt:"), "accesses a field by name");
    }

    #[test]
    fn test_method_description_inst_var_at_put() {
        assert_eq!(method_description("fieldAt:put:"), "sets a field by name");
    }

    #[test]
    fn test_registry_finds_inst_var_at_put() {
        let registry = MethodValidatorRegistry::new();
        assert!(registry.get("fieldAt:put:").is_some());
    }

    #[test]
    fn test_inst_var_at_put_symbol_is_valid() {
        let validator = ReflectionMethodValidator;
        let selector = MessageSelector::Keyword(vec![
            KeywordPart::new("fieldAt:", test_span()),
            KeywordPart::new("put:", test_span()),
        ]);
        let args = vec![
            Expression::Literal(Literal::Symbol("x".into()), test_span()),
            Expression::Literal(Literal::Integer(99), test_span()),
        ];
        let diagnostics = validator.validate(&selector, &args, None, test_span());
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_inst_var_at_put_identifier_produces_error() {
        let validator = ReflectionMethodValidator;
        let selector = MessageSelector::Keyword(vec![
            KeywordPart::new("fieldAt:", test_span()),
            KeywordPart::new("put:", test_span()),
        ]);
        let args = vec![
            Expression::Identifier(Identifier::new("x", Span::new(15, 16))),
            Expression::Literal(Literal::Integer(99), test_span()),
        ];
        let diagnostics = validator.validate(&selector, &args, None, test_span());
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
        let diag = validate_immutable_mutation(&receiver, "fieldAt:put:", test_span());
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
        let diag = validate_immutable_mutation(&receiver, "fieldAt:put:", test_span());
        assert!(diag.is_some());
        assert!(diag.unwrap().message.contains("String literal"));
    }

    #[test]
    fn test_immutable_mutation_float_literal() {
        let receiver = Expression::Literal(Literal::Float(1.5), test_span());
        let diag = validate_immutable_mutation(&receiver, "fieldAt:put:", test_span());
        assert!(diag.is_some());
        assert!(diag.unwrap().message.contains("Float literal"));
    }

    #[test]
    fn test_immutable_mutation_symbol_literal() {
        let receiver = Expression::Literal(Literal::Symbol("x".into()), test_span());
        let diag = validate_immutable_mutation(&receiver, "fieldAt:put:", test_span());
        assert!(diag.is_some());
        assert!(diag.unwrap().message.contains("Symbol literal"));
    }

    #[test]
    fn test_immutable_mutation_character_literal() {
        let receiver = Expression::Literal(Literal::Character('a'), test_span());
        let diag = validate_immutable_mutation(&receiver, "fieldAt:put:", test_span());
        assert!(diag.is_some());
        assert!(diag.unwrap().message.contains("Character literal"));
    }

    #[test]
    fn test_immutable_mutation_true_identifier() {
        let receiver = Expression::Identifier(Identifier::new("true", test_span()));
        let diag = validate_immutable_mutation(&receiver, "fieldAt:put:", test_span());
        assert!(diag.is_some());
        assert!(diag.unwrap().message.contains("Boolean literal"));
    }

    #[test]
    fn test_immutable_mutation_false_identifier() {
        let receiver = Expression::Identifier(Identifier::new("false", test_span()));
        let diag = validate_immutable_mutation(&receiver, "fieldAt:put:", test_span());
        assert!(diag.is_some());
        assert!(diag.unwrap().message.contains("Boolean literal"));
    }

    #[test]
    fn test_immutable_mutation_nil_identifier() {
        let receiver = Expression::Identifier(Identifier::new("nil", test_span()));
        let diag = validate_immutable_mutation(&receiver, "fieldAt:put:", test_span());
        assert!(diag.is_some());
        assert!(diag.unwrap().message.contains("nil"));
    }

    #[test]
    fn test_immutable_mutation_variable_is_ok() {
        let receiver = Expression::Identifier(Identifier::new("obj", test_span()));
        let diag = validate_immutable_mutation(&receiver, "fieldAt:put:", test_span());
        assert!(diag.is_none());
    }

    #[test]
    fn test_immutable_mutation_other_selector_is_ok() {
        let receiver = Expression::Literal(Literal::Integer(42), test_span());
        let diag = validate_immutable_mutation(&receiver, "respondsTo:", test_span());
        assert!(diag.is_none());
    }

    #[test]
    fn test_immutable_mutation_list_literal() {
        let receiver = Expression::Literal(Literal::List(vec![Literal::Integer(1)]), test_span());
        let diag = validate_immutable_mutation(&receiver, "fieldAt:put:", test_span());
        assert!(diag.is_some());
        assert!(diag.unwrap().message.contains("List literal"));
    }

    #[test]
    fn test_immutable_mutation_list_literal_expression() {
        let receiver = Expression::ListLiteral {
            elements: vec![Expression::Literal(Literal::Integer(1), test_span())],
            tail: None,
            span: test_span(),
        };
        let diag = validate_immutable_mutation(&receiver, "fieldAt:put:", test_span());
        assert!(diag.is_some());
        assert!(diag.unwrap().message.contains("List literal"));
    }

    #[test]
    fn test_primitive_instantiation_block_new() {
        let receiver = Expression::ClassReference {
            name: Identifier::new("Block", test_span()),
            span: test_span(),
        };
        let diag = validate_primitive_instantiation(&receiver, "new", test_span());
        assert!(diag.is_some());
        assert!(
            diag.unwrap()
                .hint
                .as_ref()
                .unwrap()
                .contains("block literal")
        );
    }

    #[test]
    fn test_primitive_instantiation_compiled_method_new() {
        let receiver = Expression::ClassReference {
            name: Identifier::new("CompiledMethod", test_span()),
            span: test_span(),
        };
        let diag = validate_primitive_instantiation(&receiver, "new", test_span());
        assert!(diag.is_some());
        assert!(diag.unwrap().hint.as_ref().unwrap().contains("compiler"));
    }

    #[test]
    fn test_primitive_instantiation_undefined_object_new() {
        let receiver = Expression::ClassReference {
            name: Identifier::new("UndefinedObject", test_span()),
            span: test_span(),
        };
        let diag = validate_primitive_instantiation(&receiver, "new", test_span());
        assert!(diag.is_some());
        assert!(diag.unwrap().hint.as_ref().unwrap().contains("`nil`"));
    }

    // ── Block arity validator tests ─────────────────────────────────────

    fn make_block(arity: usize) -> Expression {
        let params: Vec<BlockParameter> = (0..arity)
            .map(|i| BlockParameter {
                name: format!("p{i}").into(),
                span: test_span(),
            })
            .collect();
        Expression::Block(Block::new(
            params,
            vec![ExpressionStatement::bare(Expression::Literal(
                Literal::Integer(1),
                test_span(),
            ))],
            test_span(),
        ))
    }

    fn keyword_selector(name: &str) -> MessageSelector {
        MessageSelector::Keyword(vec![KeywordPart::new(name, test_span())])
    }

    fn multi_keyword_selector(parts: &[&str]) -> MessageSelector {
        MessageSelector::Keyword(
            parts
                .iter()
                .map(|p| KeywordPart::new(*p, test_span()))
                .collect(),
        )
    }

    #[test]
    fn test_block_arity_correct_single_param() {
        let validator = BlockArityValidator::new(1, 0);
        let args = vec![make_block(1)];
        let diags = validator.validate(&keyword_selector("collect:"), &args, None, test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_block_arity_wrong_single_param() {
        let validator = BlockArityValidator::new(1, 0);
        let args = vec![make_block(2)];
        let diags = validator.validate(&keyword_selector("collect:"), &args, None, test_span());
        assert_eq!(diags.len(), 1);
        assert!(
            diags[0]
                .message
                .contains("expects a block with 1 parameter, but got 2")
        );
        assert!(
            diags[0]
                .hint
                .as_ref()
                .unwrap()
                .contains("1-parameter block")
        );
    }

    #[test]
    fn test_block_arity_zero_param_correct() {
        let validator = BlockArityValidator::new(0, 0);
        let args = vec![make_block(0)];
        let diags = validator.validate(&keyword_selector("timesRepeat:"), &args, None, test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_block_arity_zero_param_wrong() {
        let validator = BlockArityValidator::new(0, 0);
        let args = vec![make_block(1)];
        let diags = validator.validate(&keyword_selector("timesRepeat:"), &args, None, test_span());
        assert_eq!(diags.len(), 1);
        assert!(
            diags[0]
                .message
                .contains("expects a block with 0 parameters, but got 1")
        );
    }

    #[test]
    fn test_block_arity_two_param_correct() {
        let validator = BlockArityValidator::new(2, 0);
        let args = vec![make_block(2)];
        let diags = validator.validate(
            &keyword_selector("eachWithIndex:"),
            &args,
            None,
            test_span(),
        );
        assert!(diags.is_empty());
    }

    #[test]
    fn test_block_arity_two_param_wrong() {
        let validator = BlockArityValidator::new(2, 0);
        let args = vec![make_block(1)];
        let diags = validator.validate(
            &keyword_selector("eachWithIndex:"),
            &args,
            None,
            test_span(),
        );
        assert_eq!(diags.len(), 1);
        assert!(
            diags[0]
                .message
                .contains("expects a block with 2 parameters, but got 1")
        );
    }

    #[test]
    fn test_block_arity_non_block_arg_skipped() {
        let validator = BlockArityValidator::new(1, 0);
        let args = vec![Expression::Identifier(Identifier::new(
            "myBlock",
            test_span(),
        ))];
        let diags = validator.validate(&keyword_selector("collect:"), &args, None, test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_block_arity_empty_args_skipped() {
        let validator = BlockArityValidator::new(1, 0);
        let diags = validator.validate(&keyword_selector("collect:"), &[], None, test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_block_arity_inject_into_second_arg() {
        let validator = BlockArityValidator::new(2, 1);
        let selector = multi_keyword_selector(&["inject:", "into:"]);
        let args = vec![
            Expression::Literal(Literal::Integer(0), test_span()),
            make_block(2),
        ];
        let diags = validator.validate(&selector, &args, None, test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_block_arity_inject_into_wrong_arity() {
        let validator = BlockArityValidator::new(2, 1);
        let selector = multi_keyword_selector(&["inject:", "into:"]);
        let args = vec![
            Expression::Literal(Literal::Integer(0), test_span()),
            make_block(1),
        ];
        let diags = validator.validate(&selector, &args, None, test_span());
        assert_eq!(diags.len(), 1);
        assert!(
            diags[0]
                .message
                .contains("expects a block with 2 parameters, but got 1")
        );
        assert!(diags[0].hint.as_ref().unwrap().contains("inject"));
    }

    #[test]
    fn test_block_arity_registry_has_all_selectors() {
        let registry = MethodValidatorRegistry::new();
        for selector in &[
            "do:",
            "collect:",
            "select:",
            "reject:",
            "detect:",
            "anySatisfy:",
            "allSatisfy:",
            "groupBy:",
            "partition:",
            "eachWithIndex:",
            "sortWith:",
            "keysAndValuesDo:",
            "whileTrue:",
            "whileFalse:",
            "timesRepeat:",
            "inject:into:",
        ] {
            assert!(
                registry.get(selector).is_some(),
                "Registry missing validator for {selector}"
            );
        }
    }

    // ── Integer argument validator tests ─────────────────────────────────

    #[test]
    fn test_integer_arg_integer_literal_ok() {
        let validator = IntegerArgumentValidator;
        let args = vec![Expression::Literal(Literal::Integer(1), test_span())];
        let diags = validator.validate(&keyword_selector("take:"), &args, None, test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_integer_arg_string_literal_error() {
        let validator = IntegerArgumentValidator;
        let args = vec![Expression::Literal(
            Literal::String("hello".into()),
            test_span(),
        )];
        let diags = validator.validate(&keyword_selector("take:"), &args, None, test_span());
        assert_eq!(diags.len(), 1);
        assert!(
            diags[0]
                .message
                .contains("expects an integer argument, got a string")
        );
        assert!(diags[0].hint.as_ref().unwrap().contains("integer count"));
    }

    #[test]
    fn test_integer_arg_float_literal_error() {
        let validator = IntegerArgumentValidator;
        let args = vec![Expression::Literal(Literal::Float(2.72), test_span())];
        let diags = validator.validate(&keyword_selector("take:"), &args, None, test_span());
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("got a float"));
    }

    #[test]
    fn test_integer_arg_symbol_literal_error() {
        let validator = IntegerArgumentValidator;
        let args = vec![Expression::Literal(
            Literal::Symbol("x".into()),
            test_span(),
        )];
        let diags = validator.validate(&keyword_selector("drop:"), &args, None, test_span());
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("got a symbol"));
    }

    #[test]
    fn test_integer_arg_variable_skipped() {
        let validator = IntegerArgumentValidator;
        let args = vec![Expression::Identifier(Identifier::new("idx", test_span()))];
        let diags = validator.validate(&keyword_selector("take:"), &args, None, test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_integer_arg_empty_args_skipped() {
        let validator = IntegerArgumentValidator;
        let diags = validator.validate(&keyword_selector("take:"), &[], None, test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_integer_arg_from_to_both_valid() {
        let validator = IntegerArgumentValidator;
        let selector = multi_keyword_selector(&["from:", "to:"]);
        let args = vec![
            Expression::Literal(Literal::Integer(1), test_span()),
            Expression::Literal(Literal::Integer(3), test_span()),
        ];
        let diags = validator.validate(&selector, &args, None, test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_integer_arg_from_to_second_arg_error() {
        let validator = IntegerArgumentValidator;
        let selector = multi_keyword_selector(&["from:", "to:"]);
        let args = vec![
            Expression::Literal(Literal::Integer(1), test_span()),
            Expression::Literal(Literal::String("end".into()), Span::new(20, 25)),
        ];
        let diags = validator.validate(&selector, &args, None, test_span());
        assert_eq!(diags.len(), 1);
        assert!(
            diags[0]
                .message
                .contains("expects an integer argument, got a string")
        );
        assert_eq!(diags[0].span, Span::new(20, 25));
    }

    #[test]
    fn test_integer_arg_from_to_both_args_error() {
        let validator = IntegerArgumentValidator;
        let selector = multi_keyword_selector(&["from:", "to:"]);
        let args = vec![
            Expression::Literal(Literal::String("start".into()), test_span()),
            Expression::Literal(Literal::String("end".into()), test_span()),
        ];
        let diags = validator.validate(&selector, &args, None, test_span());
        assert_eq!(diags.len(), 2);
    }

    #[test]
    fn test_integer_arg_from_to_second_arg_variable_skipped() {
        let validator = IntegerArgumentValidator;
        let selector = multi_keyword_selector(&["from:", "to:"]);
        let args = vec![
            Expression::Literal(Literal::Integer(1), test_span()),
            Expression::Identifier(Identifier::new("n", test_span())),
        ];
        let diags = validator.validate(&selector, &args, None, test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_integer_arg_registry_has_selectors() {
        let registry = MethodValidatorRegistry::new();
        for selector in &["take:", "drop:", "repeat:", "from:to:", "at:", "at:put:"] {
            assert!(
                registry.get(selector).is_some(),
                "Registry missing validator for {selector}"
            );
        }
    }

    // ── String argument validator tests ──────────────────────────────────

    #[test]
    fn test_string_arg_string_literal_ok() {
        let validator = StringArgumentValidator;
        let args = vec![Expression::Literal(
            Literal::String("ell".into()),
            test_span(),
        )];
        let diags = validator.validate(&keyword_selector("startsWith:"), &args, None, test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_string_arg_integer_literal_error() {
        let validator = StringArgumentValidator;
        let args = vec![Expression::Literal(Literal::Integer(42), test_span())];
        let diags = validator.validate(&keyword_selector("startsWith:"), &args, None, test_span());
        assert_eq!(diags.len(), 1);
        assert!(
            diags[0]
                .message
                .contains("expects a string argument, got an integer")
        );
        assert!(diags[0].hint.as_ref().unwrap().contains("startsWith:"));
    }

    #[test]
    fn test_string_arg_symbol_literal_error() {
        let validator = StringArgumentValidator;
        let args = vec![Expression::Literal(
            Literal::Symbol("x".into()),
            test_span(),
        )];
        let diags = validator.validate(&keyword_selector("endsWith:"), &args, None, test_span());
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("got a symbol"));
    }

    #[test]
    fn test_string_arg_variable_skipped() {
        let validator = StringArgumentValidator;
        let args = vec![Expression::Identifier(Identifier::new("s", test_span()))];
        let diags = validator.validate(&keyword_selector("startsWith:"), &args, None, test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_string_arg_empty_args_skipped() {
        let validator = StringArgumentValidator;
        let diags = validator.validate(&keyword_selector("startsWith:"), &[], None, test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_string_arg_registry_has_selectors() {
        let registry = MethodValidatorRegistry::new();
        for selector in &[
            "startsWith:",
            "endsWith:",
            "splitOn:",
            "includes:",
            "indexOf:",
        ] {
            assert!(
                registry.get(selector).is_some(),
                "Registry missing validator for {selector}"
            );
        }
    }

    #[test]
    fn test_string_arg_block_skipped() {
        let validator = StringArgumentValidator;
        let args = vec![make_block(0)];
        let diags = validator.validate(&keyword_selector("startsWith:"), &args, None, test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_integer_arg_block_skipped() {
        let validator = IntegerArgumentValidator;
        let args = vec![make_block(0)];
        let diags = validator.validate(&keyword_selector("take:"), &args, None, test_span());
        assert!(diags.is_empty());
    }

    // ── Type-aware integer validator tests ───────────────────────────────

    fn known_type(name: &str) -> InferredType {
        InferredType::Known(name.into())
    }

    #[test]
    fn test_type_aware_at_list_integer_ok() {
        let validator = TypeAwareIntegerArgValidator;
        let ty = known_type("List");
        let args = vec![Expression::Literal(Literal::Integer(1), test_span())];
        let diags = validator.validate(&keyword_selector("at:"), &args, Some(&ty), test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_type_aware_at_list_string_error() {
        let validator = TypeAwareIntegerArgValidator;
        let ty = known_type("List");
        let args = vec![Expression::Literal(
            Literal::String("key".into()),
            test_span(),
        )];
        let diags = validator.validate(&keyword_selector("at:"), &args, Some(&ty), test_span());
        assert_eq!(diags.len(), 1);
        assert!(
            diags[0]
                .message
                .contains("expects an integer argument, got a string")
        );
    }

    #[test]
    fn test_type_aware_at_dictionary_symbol_ok() {
        let validator = TypeAwareIntegerArgValidator;
        let ty = known_type("Dictionary");
        let args = vec![Expression::Literal(
            Literal::Symbol("key".into()),
            test_span(),
        )];
        let diags = validator.validate(&keyword_selector("at:"), &args, Some(&ty), test_span());
        assert!(
            diags.is_empty(),
            "Dictionary at: should accept non-integer keys"
        );
    }

    #[test]
    fn test_type_aware_at_unknown_receiver_skips() {
        let validator = TypeAwareIntegerArgValidator;
        let args = vec![Expression::Literal(
            Literal::String("key".into()),
            test_span(),
        )];
        let diags = validator.validate(&keyword_selector("at:"), &args, None, test_span());
        assert!(diags.is_empty(), "Unknown receiver should skip validation");
    }

    #[test]
    fn test_type_aware_at_dynamic_receiver_skips() {
        let validator = TypeAwareIntegerArgValidator;
        let ty = InferredType::Dynamic;
        let args = vec![Expression::Literal(
            Literal::String("key".into()),
            test_span(),
        )];
        let diags = validator.validate(&keyword_selector("at:"), &args, Some(&ty), test_span());
        assert!(diags.is_empty(), "Dynamic receiver should skip validation");
    }

    #[test]
    fn test_type_aware_at_tuple_string_error() {
        let validator = TypeAwareIntegerArgValidator;
        let ty = known_type("Tuple");
        let args = vec![Expression::Literal(
            Literal::String("key".into()),
            test_span(),
        )];
        let diags = validator.validate(&keyword_selector("at:"), &args, Some(&ty), test_span());
        assert_eq!(diags.len(), 1);
    }

    #[test]
    fn test_type_aware_at_string_receiver_error() {
        let validator = TypeAwareIntegerArgValidator;
        let ty = known_type("String");
        let args = vec![Expression::Literal(
            Literal::Symbol("x".into()),
            test_span(),
        )];
        let diags = validator.validate(&keyword_selector("at:"), &args, Some(&ty), test_span());
        assert_eq!(diags.len(), 1);
    }

    // ── Type-aware string validator tests ────────────────────────────────

    #[test]
    fn test_type_aware_includes_string_receiver_string_ok() {
        let validator = TypeAwareStringArgValidator;
        let ty = known_type("String");
        let args = vec![Expression::Literal(
            Literal::String("ell".into()),
            test_span(),
        )];
        let diags = validator.validate(
            &keyword_selector("includes:"),
            &args,
            Some(&ty),
            test_span(),
        );
        assert!(diags.is_empty());
    }

    #[test]
    fn test_type_aware_includes_string_receiver_integer_error() {
        let validator = TypeAwareStringArgValidator;
        let ty = known_type("String");
        let args = vec![Expression::Literal(Literal::Integer(42), test_span())];
        let diags = validator.validate(
            &keyword_selector("includes:"),
            &args,
            Some(&ty),
            test_span(),
        );
        assert_eq!(diags.len(), 1);
        assert!(
            diags[0]
                .message
                .contains("expects a string argument, got an integer")
        );
    }

    #[test]
    fn test_type_aware_includes_list_receiver_integer_ok() {
        let validator = TypeAwareStringArgValidator;
        let ty = known_type("List");
        let args = vec![Expression::Literal(Literal::Integer(42), test_span())];
        let diags = validator.validate(
            &keyword_selector("includes:"),
            &args,
            Some(&ty),
            test_span(),
        );
        assert!(
            diags.is_empty(),
            "List includes: should accept any element type"
        );
    }

    #[test]
    fn test_type_aware_includes_unknown_receiver_skips() {
        let validator = TypeAwareStringArgValidator;
        let args = vec![Expression::Literal(Literal::Integer(42), test_span())];
        let diags = validator.validate(&keyword_selector("includes:"), &args, None, test_span());
        assert!(diags.is_empty(), "Unknown receiver should skip validation");
    }

    #[test]
    fn test_type_aware_includes_dynamic_receiver_skips() {
        let validator = TypeAwareStringArgValidator;
        let ty = InferredType::Dynamic;
        let args = vec![Expression::Literal(Literal::Integer(42), test_span())];
        let diags = validator.validate(
            &keyword_selector("includes:"),
            &args,
            Some(&ty),
            test_span(),
        );
        assert!(diags.is_empty(), "Dynamic receiver should skip validation");
    }

    #[test]
    fn test_type_aware_indexof_string_receiver_symbol_error() {
        let validator = TypeAwareStringArgValidator;
        let ty = known_type("String");
        let args = vec![Expression::Literal(
            Literal::Symbol("x".into()),
            test_span(),
        )];
        let diags =
            validator.validate(&keyword_selector("indexOf:"), &args, Some(&ty), test_span());
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("got a symbol"));
    }

    #[test]
    fn test_type_aware_at_variable_skipped() {
        let validator = TypeAwareIntegerArgValidator;
        let ty = known_type("List");
        let args = vec![Expression::Identifier(Identifier::new("idx", test_span()))];
        let diags = validator.validate(&keyword_selector("at:"), &args, Some(&ty), test_span());
        assert!(diags.is_empty());
    }
}
