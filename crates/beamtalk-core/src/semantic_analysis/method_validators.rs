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
        // Note: at: and at:put: are excluded because Dictionary uses them with
        // non-integer keys (e.g., dict at: #key). Without receiver type info,
        // we can't distinguish List at: (integer) from Dictionary at: (any key).
        for selector in &["take:", "drop:", "repeat:", "from:to:"] {
            self.validators
                .insert(selector, Box::new(IntegerArgumentValidator));
        }

        // String argument validators
        // Note: includes: and indexOf: are excluded because List also defines
        // these methods with non-string arguments (e.g., list includes: 42).
        for selector in &["startsWith:", "endsWith:", "splitOn:"] {
            self.validators
                .insert(selector, Box::new(StringArgumentValidator));
        }
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
        Expression::Literal(Literal::List(_), _) | Expression::ListLiteral { .. } => "List literal",
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
        _span: Span,
    ) -> Vec<Diagnostic> {
        if arguments.is_empty() {
            return vec![];
        }

        let first_arg = &arguments[0];
        match first_arg {
            // Integer literal — correct
            Expression::Literal(Literal::Integer(_), _) => vec![],

            // Other literal types — definitely wrong
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
                    hint: Some(integer_arg_hint(&selector_name).into()),
                }]
            }

            // Non-literal — skip (runtime handles)
            _ => vec![],
        }
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
                }]
            }

            // Non-literal — skip (runtime handles)
            _ => vec![],
        }
    }
}

fn string_arg_hint(selector: &str) -> String {
    match selector {
        "startsWith:" => r#"Use a string argument: "hello" startsWith: "hel""#.into(),
        "endsWith:" => r#"Use a string argument: "hello" endsWith: "llo""#.into(),
        "splitOn:" => r#"Use a string argument: "a,b,c" splitOn: ",""#.into(),
        _ => format!(r#"Use a string argument: receiver {selector} "text""#),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Block, BlockParameter, Identifier, KeywordPart};
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

    #[test]
    fn test_immutable_mutation_list_literal() {
        let receiver = Expression::Literal(Literal::List(vec![Literal::Integer(1)]), test_span());
        let diag = validate_immutable_mutation(&receiver, "instVarAt:put:", test_span());
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
        let diag = validate_immutable_mutation(&receiver, "instVarAt:put:", test_span());
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
            vec![Expression::Literal(Literal::Integer(1), test_span())],
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
        let diags = validator.validate(&keyword_selector("collect:"), &args, test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_block_arity_wrong_single_param() {
        let validator = BlockArityValidator::new(1, 0);
        let args = vec![make_block(2)];
        let diags = validator.validate(&keyword_selector("collect:"), &args, test_span());
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
        let diags = validator.validate(&keyword_selector("timesRepeat:"), &args, test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_block_arity_zero_param_wrong() {
        let validator = BlockArityValidator::new(0, 0);
        let args = vec![make_block(1)];
        let diags = validator.validate(&keyword_selector("timesRepeat:"), &args, test_span());
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
        let diags = validator.validate(&keyword_selector("eachWithIndex:"), &args, test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_block_arity_two_param_wrong() {
        let validator = BlockArityValidator::new(2, 0);
        let args = vec![make_block(1)];
        let diags = validator.validate(&keyword_selector("eachWithIndex:"), &args, test_span());
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
        let diags = validator.validate(&keyword_selector("collect:"), &args, test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_block_arity_empty_args_skipped() {
        let validator = BlockArityValidator::new(1, 0);
        let diags = validator.validate(&keyword_selector("collect:"), &[], test_span());
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
        let diags = validator.validate(&selector, &args, test_span());
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
        let diags = validator.validate(&selector, &args, test_span());
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
        let diags = validator.validate(&keyword_selector("take:"), &args, test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_integer_arg_string_literal_error() {
        let validator = IntegerArgumentValidator;
        let args = vec![Expression::Literal(
            Literal::String("hello".into()),
            test_span(),
        )];
        let diags = validator.validate(&keyword_selector("take:"), &args, test_span());
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
        let diags = validator.validate(&keyword_selector("take:"), &args, test_span());
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
        let diags = validator.validate(&keyword_selector("drop:"), &args, test_span());
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("got a symbol"));
    }

    #[test]
    fn test_integer_arg_variable_skipped() {
        let validator = IntegerArgumentValidator;
        let args = vec![Expression::Identifier(Identifier::new("idx", test_span()))];
        let diags = validator.validate(&keyword_selector("take:"), &args, test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_integer_arg_empty_args_skipped() {
        let validator = IntegerArgumentValidator;
        let diags = validator.validate(&keyword_selector("take:"), &[], test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_integer_arg_registry_has_selectors() {
        let registry = MethodValidatorRegistry::new();
        // at: and at:put: excluded — polymorphic with Dictionary
        for selector in &["take:", "drop:", "repeat:", "from:to:"] {
            assert!(
                registry.get(selector).is_some(),
                "Registry missing validator for {selector}"
            );
        }
        // Verify at: and at:put: are NOT registered (Dictionary uses non-integer keys)
        assert!(registry.get("at:").is_none());
        assert!(registry.get("at:put:").is_none());
    }

    // ── String argument validator tests ──────────────────────────────────

    #[test]
    fn test_string_arg_string_literal_ok() {
        let validator = StringArgumentValidator;
        let args = vec![Expression::Literal(
            Literal::String("ell".into()),
            test_span(),
        )];
        let diags = validator.validate(&keyword_selector("startsWith:"), &args, test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_string_arg_integer_literal_error() {
        let validator = StringArgumentValidator;
        let args = vec![Expression::Literal(Literal::Integer(42), test_span())];
        let diags = validator.validate(&keyword_selector("startsWith:"), &args, test_span());
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
        let diags = validator.validate(&keyword_selector("endsWith:"), &args, test_span());
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("got a symbol"));
    }

    #[test]
    fn test_string_arg_variable_skipped() {
        let validator = StringArgumentValidator;
        let args = vec![Expression::Identifier(Identifier::new("s", test_span()))];
        let diags = validator.validate(&keyword_selector("startsWith:"), &args, test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_string_arg_empty_args_skipped() {
        let validator = StringArgumentValidator;
        let diags = validator.validate(&keyword_selector("startsWith:"), &[], test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_string_arg_registry_has_selectors() {
        let registry = MethodValidatorRegistry::new();
        // includes: and indexOf: excluded — polymorphic with List
        for selector in &["startsWith:", "endsWith:", "splitOn:"] {
            assert!(
                registry.get(selector).is_some(),
                "Registry missing validator for {selector}"
            );
        }
        // Verify includes: and indexOf: are NOT registered (polymorphic with List)
        assert!(registry.get("includes:").is_none());
        assert!(registry.get("indexOf:").is_none());
    }

    #[test]
    fn test_string_arg_block_skipped() {
        let validator = StringArgumentValidator;
        let args = vec![make_block(0)];
        let diags = validator.validate(&keyword_selector("startsWith:"), &args, test_span());
        assert!(diags.is_empty());
    }

    #[test]
    fn test_integer_arg_block_skipped() {
        let validator = IntegerArgumentValidator;
        let args = vec![make_block(0)];
        let diags = validator.validate(&keyword_selector("take:"), &args, test_span());
        assert!(diags.is_empty());
    }
}
