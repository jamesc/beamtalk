// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `ClassHierarchy` integration (user classes, sealed-class
//! diagnostics) and method-validator integration (`respondsTo:`,
//! `class name`, `instVarAt:`, cascades).

use super::*;

// --- ClassHierarchy integration tests ---

#[test]
fn test_analyse_populates_class_hierarchy() {
    let module = Module::new(vec![], Span::default());
    let result = analyse(&module);

    // Hierarchy should be populated with built-in classes
    assert!(result.class_hierarchy.has_class("ProtoObject"));
    assert!(result.class_hierarchy.has_class("Object"));
    assert!(result.class_hierarchy.has_class("Actor"));
    assert!(result.class_hierarchy.has_class("Integer"));
}

#[test]
fn test_analyse_hierarchy_includes_user_classes() {
    use crate::ast::{
        ClassDefinition, ClassKind, CommentAttachment, DeclaredKeyword, MethodDefinition,
        MethodKind, StateDeclaration,
    };

    let class = ClassDefinition {
        name: Identifier::new("Counter", test_span()),
        superclass: Some(Identifier::new("Actor", test_span())),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![StateDeclaration {
            name: Identifier::new("count", test_span()),
            type_annotation: None,
            default_value: None,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            declared_keyword: DeclaredKeyword::default(),
            span: test_span(),
        }],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Unary("increment".into()),
            parameters: vec![],
            body: vec![],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: test_span(),
    };

    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let result = analyse(&module);

    assert!(result.class_hierarchy.has_class("Counter"));
    assert!(
        result
            .class_hierarchy
            .resolves_selector("Counter", "increment")
    );
    // spawn is class-side on Actor — verify via all_class_methods
    assert!(
        result
            .class_hierarchy
            .all_class_methods("Counter")
            .iter()
            .any(|m| m.selector.as_str() == "spawn"),
        "Counter should inherit class-side spawn from Actor"
    );
}

#[test]
fn test_analyse_reports_sealed_class_diagnostic() {
    use crate::ast::{ClassDefinition, ClassKind};

    let class = ClassDefinition {
        name: Identifier::new("MyInt", test_span()),
        superclass: Some(Identifier::new("Integer", test_span())),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: test_span(),
    };

    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let result = analyse(&module);

    // Should have sealed class diagnostic
    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.message.contains("sealed") && d.message.contains("Integer")),
        "Expected sealed class diagnostic, got: {:?}",
        result.diagnostics
    );
}

// --- Method Validator Integration Tests ---

#[test]
fn test_responds_to_with_symbol_no_diagnostic() {
    // counter respondsTo: #increment — should be fine
    let expr = Expression::MessageSend {
        receiver: Box::new(Expression::Identifier(Identifier::new(
            "counter",
            test_span(),
        ))),
        selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
            "respondsTo:",
            test_span(),
        )]),
        arguments: vec![Expression::Literal(
            Literal::Symbol("increment".into()),
            test_span(),
        )],
        is_cast: false,
        span: test_span(),
    };

    let module = Module::new(vec![bare(expr)], test_span());
    let known_vars = ["counter"];
    let result = analyse_full(
        &module,
        AnalysisContext::default().with_known_vars(&known_vars),
    );

    // No symbol-related diagnostics
    assert!(
        !result
            .diagnostics
            .iter()
            .any(|d| d.message.contains("expects a symbol literal")),
        "Should not report error for symbol literal, got: {:?}",
        result.diagnostics
    );
}

#[test]
fn test_responds_to_with_identifier_no_error() {
    // counter respondsTo: sel — identifier arg is allowed
    let expr = Expression::MessageSend {
        receiver: Box::new(Expression::Identifier(Identifier::new(
            "counter",
            test_span(),
        ))),
        selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
            "respondsTo:",
            test_span(),
        )]),
        arguments: vec![Expression::Identifier(Identifier::new(
            "sel",
            Span::new(20, 23),
        ))],
        is_cast: false,
        span: test_span(),
    };

    let module = Module::new(vec![bare(expr)], test_span());
    let known_vars = ["counter", "sel"];
    let result = analyse_full(
        &module,
        AnalysisContext::default().with_known_vars(&known_vars),
    );

    let symbol_errors: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("expects a symbol literal"))
        .collect();

    assert_eq!(
        symbol_errors.len(),
        0,
        "identifier arg to respondsTo: should not produce symbol literal error, got: {:?}",
        result.diagnostics
    );
}

#[test]
fn test_class_named_with_class_reference_emits_error() {
    // Beamtalk classNamed: Counter — should error
    let expr = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Beamtalk", test_span()),
            span: test_span(),
            package: None,
        }),
        selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
            "classNamed:",
            test_span(),
        )]),
        arguments: vec![Expression::ClassReference {
            name: Identifier::new("Counter", Span::new(22, 29)),
            span: Span::new(22, 29),
            package: None,
        }],
        is_cast: false,
        span: test_span(),
    };

    let module = Module::new(vec![bare(expr)], test_span());
    let result = analyse(&module);

    let symbol_errors: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("expects a symbol literal"))
        .collect();

    assert_eq!(
        symbol_errors.len(),
        1,
        "Expected 1 symbol literal error, got: {:?}",
        result.diagnostics
    );
    let hint = symbol_errors[0].hint.as_ref().unwrap();
    assert!(hint.contains("#Counter"));
    assert!(
        symbol_errors[0]
            .message
            .contains("looks up a class by name")
    );
}

#[test]
fn test_inst_var_at_with_integer_emits_error() {
    // obj fieldAt: 42 — should error
    let expr = Expression::MessageSend {
        receiver: Box::new(Expression::Identifier(Identifier::new("obj", test_span()))),
        selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
            "fieldAt:",
            test_span(),
        )]),
        arguments: vec![Expression::Literal(Literal::Integer(42), Span::new(15, 17))],
        is_cast: false,
        span: test_span(),
    };

    let module = Module::new(vec![bare(expr)], test_span());
    let known_vars = ["obj"];
    let result = analyse_full(
        &module,
        AnalysisContext::default().with_known_vars(&known_vars),
    );

    let symbol_errors: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("expects a symbol literal"))
        .collect();

    assert_eq!(
        symbol_errors.len(),
        1,
        "Expected 1 symbol literal error, got: {:?}",
        result.diagnostics
    );
    assert!(
        symbol_errors[0]
            .message
            .contains("accesses a field by name")
    );
}

#[test]
fn test_non_reflection_method_no_validation() {
    // obj someMethod: increment — should NOT trigger validator
    let expr = Expression::MessageSend {
        receiver: Box::new(Expression::Identifier(Identifier::new("obj", test_span()))),
        selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
            "someMethod:",
            test_span(),
        )]),
        arguments: vec![Expression::Identifier(Identifier::new(
            "increment",
            test_span(),
        ))],
        is_cast: false,
        span: test_span(),
    };

    let module = Module::new(vec![bare(expr)], test_span());
    let known_vars = ["obj"];
    let result = analyse_full(
        &module,
        AnalysisContext::default().with_known_vars(&known_vars),
    );

    assert!(
        !result
            .diagnostics
            .iter()
            .any(|d| d.message.contains("expects a symbol literal")),
        "Non-reflection methods should not trigger symbol validation, got: {:?}",
        result.diagnostics
    );
}

#[test]
fn test_cascade_responds_to_with_identifier_no_error() {
    // counter respondsTo: sel; size — identifier arg is allowed in cascade
    let cascade = Expression::Cascade {
        receiver: Box::new(Expression::Identifier(Identifier::new(
            "counter",
            test_span(),
        ))),
        messages: vec![
            crate::ast::CascadeMessage::new(
                MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
                    "respondsTo:",
                    test_span(),
                )]),
                vec![Expression::Identifier(Identifier::new(
                    "sel",
                    Span::new(20, 23),
                ))],
                test_span(),
            ),
            crate::ast::CascadeMessage::new(
                MessageSelector::Unary("size".into()),
                vec![],
                test_span(),
            ),
        ],
        span: test_span(),
    };

    let module = Module::new(vec![bare(cascade)], test_span());
    let known_vars = ["counter", "sel"];
    let result = analyse_full(
        &module,
        AnalysisContext::default().with_known_vars(&known_vars),
    );

    let symbol_errors: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("expects a symbol literal"))
        .collect();

    assert_eq!(
        symbol_errors.len(),
        0,
        "identifier arg to respondsTo: in cascade should not produce symbol literal error, got: {:?}",
        result.diagnostics
    );
}

#[test]
fn test_abstract_class_instantiation_error() {
    use crate::ast::{ClassDefinition, ClassKind, CommentAttachment, MethodDefinition, MethodKind};

    // abstract class cannot be instantiated
    let class = ClassDefinition {
        name: Identifier::new("Shape", test_span()),
        superclass: Some(Identifier::new("Actor", test_span())),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: true,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Unary("area".into()),
            parameters: vec![],
            body: vec![bare(Expression::Literal(Literal::Integer(42), test_span()))],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: test_span(),
    };

    // Top-level expression: Shape spawn
    let spawn_expr = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Shape", test_span()),
            span: test_span(),
            package: None,
        }),
        selector: MessageSelector::Unary("spawn".into()),
        arguments: vec![],
        is_cast: false,
        span: test_span(),
    };

    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![bare(spawn_expr)],
        span: test_span(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let result = analyse(&module);

    let abstract_errors: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("Cannot instantiate abstract class"))
        .collect();

    assert_eq!(
        abstract_errors.len(),
        1,
        "Should detect abstract class instantiation, got: {:?}",
        result.diagnostics
    );
    assert!(abstract_errors[0].message.contains("Shape"));
}
