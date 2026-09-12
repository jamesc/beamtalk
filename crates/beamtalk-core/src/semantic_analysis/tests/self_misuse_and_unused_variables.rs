// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `self`-misuse diagnostics (outside/inside method bodies) and
//! unused-variable/parameter warnings, including underscore-
//! prefixed suppression and closures.

use super::*;

// --- Self misuse diagnostic tests ---

#[test]
fn test_self_outside_method_gives_specialized_error() {
    // Using self at module top level should give a specialized message
    let self_expr = Expression::Identifier(Identifier::new("self", Span::new(0, 4)));
    let module = Module::new(vec![bare(self_expr)], test_span());
    let result = analyse(&module);

    let self_errors: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("self"))
        .collect();
    assert_eq!(self_errors.len(), 1);
    assert!(
        self_errors[0]
            .message
            .contains("self can only be used inside a method body")
    );
    // Should NOT say "Undefined variable: self"
    assert!(!self_errors[0].message.contains("Undefined variable"));
}

#[test]
fn test_self_inside_method_no_error() {
    // self inside a method body should work fine
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
            name: Identifier::new("value", test_span()),
            type_annotation: None,
            default_value: None,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            declared_keyword: DeclaredKeyword::default(),
            span: test_span(),
        }],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Unary("getValue".into()),
            parameters: vec![],
            body: vec![bare(Expression::FieldAccess {
                receiver: Box::new(Expression::Identifier(Identifier::new("self", test_span()))),
                field: Identifier::new("value", test_span()),
                span: test_span(),
            })],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: crate::ast::MethodKind::Primary,
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

    let self_errors: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("self"))
        .collect();
    assert!(self_errors.is_empty());
}

// --- Unused variable warning tests ---

#[test]
fn test_unused_variable_in_method_warns() {
    // Method with unused local: getValue => x := 42. self.value
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
            name: Identifier::new("value", test_span()),
            type_annotation: None,
            default_value: None,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            declared_keyword: DeclaredKeyword::default(),
            span: test_span(),
        }],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Unary("getValue".into()),
            parameters: vec![],
            body: vec![
                bare(Expression::Assignment {
                    target: Box::new(Expression::Identifier(Identifier::new(
                        "x",
                        Span::new(10, 11),
                    ))),
                    value: Box::new(Expression::Literal(Literal::Integer(42), test_span())),
                    type_annotation: None,
                    span: test_span(),
                }),
                bare(Expression::FieldAccess {
                    receiver: Box::new(Expression::Identifier(Identifier::new(
                        "self",
                        test_span(),
                    ))),
                    field: Identifier::new("value", test_span()),
                    span: test_span(),
                }),
            ],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: crate::ast::MethodKind::Primary,
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

    let warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("Unused variable"))
        .collect();
    assert_eq!(warnings.len(), 1);
    assert!(warnings[0].message.contains("`x`"));
    assert_eq!(warnings[0].severity, Severity::Warning);
    assert!(warnings[0].hint.is_some());
    assert!(warnings[0].hint.as_ref().unwrap().contains("_x"));
}

#[test]
fn test_used_variable_no_warning() {
    // Method where variable is used: getValue => x := 42. x
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
        state: vec![],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Unary("getValue".into()),
            parameters: vec![],
            body: vec![
                bare(Expression::Assignment {
                    target: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
                    value: Box::new(Expression::Literal(Literal::Integer(42), test_span())),
                    type_annotation: None,
                    span: test_span(),
                }),
                bare(Expression::Identifier(Identifier::new("x", test_span()))),
            ],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: crate::ast::MethodKind::Primary,
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

    let warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("Unused variable"))
        .collect();
    assert!(warnings.is_empty());
}

#[test]
fn test_underscore_prefixed_variable_no_warning() {
    // Method with _x := 42 should not warn
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
        state: vec![],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Unary("doSomething".into()),
            parameters: vec![],
            body: vec![bare(Expression::Assignment {
                target: Box::new(Expression::Identifier(Identifier::new("_x", test_span()))),
                value: Box::new(Expression::Literal(Literal::Integer(42), test_span())),
                type_annotation: None,
                span: test_span(),
            })],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: crate::ast::MethodKind::Primary,
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

    let warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("Unused variable"))
        .collect();
    assert!(warnings.is_empty());
}

#[test]
fn test_unused_parameter_emits_warning() {
    // Unused method parameter should warn
    // process: newValue => 0  // Warning: parameter newValue unused
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
        state: vec![],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart {
                keyword: "setValue:".into(),
                span: test_span(),
            }]),
            parameters: vec![crate::ast::ParameterDefinition {
                name: Identifier::new("newValue", test_span()),
                type_annotation: None,
            }],
            body: vec![bare(Expression::Literal(Literal::Integer(0), test_span()))],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: crate::ast::MethodKind::Primary,
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

    let warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("Unused parameter"))
        .collect();
    assert_eq!(warnings.len(), 1);
    assert!(warnings[0].message.contains("newValue"));
    assert_eq!(warnings[0].severity, Severity::Warning);
}

#[test]
fn test_unused_parameter_underscore_suppresses_warning() {
    // Parameter prefixed with _ should not warn
    // process: _newValue => 0  // No warning
    let class = ClassDefinition {
        name: Identifier::new("Counter", test_span()),
        superclass: Some(Identifier::new("Actor", test_span())),
        superclass_package: None,
        class_kind: crate::ast::ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart {
                keyword: "setValue:".into(),
                span: test_span(),
            }]),
            parameters: vec![crate::ast::ParameterDefinition {
                name: Identifier::new("_newValue", test_span()),
                type_annotation: None,
            }],
            body: vec![bare(Expression::Literal(Literal::Integer(0), test_span()))],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: crate::ast::MethodKind::Primary,
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

    let warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("Unused parameter"))
        .collect();
    assert!(warnings.is_empty());
}

#[test]
fn test_used_parameter_no_warning() {
    // Used method parameter should not warn
    // process: x => x  // No warning
    let class = ClassDefinition {
        name: Identifier::new("Counter", test_span()),
        superclass: Some(Identifier::new("Actor", test_span())),
        superclass_package: None,
        is_abstract: false,
        class_kind: crate::ast::ClassKind::Actor,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart {
                keyword: "process:".into(),
                span: test_span(),
            }]),
            parameters: vec![crate::ast::ParameterDefinition {
                name: Identifier::new("x", test_span()),
                type_annotation: None,
            }],
            body: vec![bare(Expression::Identifier(Identifier::new(
                "x",
                test_span(),
            )))],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: crate::ast::MethodKind::Primary,
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

    let warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("Unused parameter"))
        .collect();
    assert!(warnings.is_empty());
}

#[test]
fn test_unused_parameter_primitive_body_no_warning() {
    // @primitive body implicitly passes all params to the Erlang primitive —
    // no unused-parameter warning should fire.
    // at: index => @primitive "at:"
    let class = ClassDefinition {
        name: Identifier::new("Array", test_span()),
        superclass: Some(Identifier::new("Object", test_span())),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart {
                keyword: "at:".into(),
                span: test_span(),
            }]),
            parameters: vec![crate::ast::ParameterDefinition {
                name: Identifier::new("index", test_span()),
                type_annotation: None,
            }],
            body: vec![bare(Expression::Primitive {
                name: "at:".into(),
                is_quoted: true,
                is_intrinsic: false,
                is_inferred: false,
                span: test_span(),
            })],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: crate::ast::MethodKind::Primary,
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
    let warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("Unused parameter"))
        .collect();
    assert!(
        warnings.is_empty(),
        "@primitive body: all params are passed to the primitive, expected no warning, got: {warnings:?}"
    );
}

#[test]
fn test_unused_parameter_intrinsic_body_no_warning() {
    // @intrinsic body (unquoted primitive) also passes all params implicitly —
    // no unused-parameter warning should fire.
    // spawnWith: initArgs => @intrinsic actorSpawnWith
    let class = ClassDefinition {
        name: Identifier::new("Actor", test_span()),
        superclass: Some(Identifier::new("Object", test_span())),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart {
                keyword: "spawnWith:".into(),
                span: test_span(),
            }]),
            parameters: vec![crate::ast::ParameterDefinition {
                name: Identifier::new("initArgs", test_span()),
                type_annotation: None,
            }],
            body: vec![bare(Expression::Primitive {
                name: "actorSpawnWith".into(),
                is_quoted: false,
                is_intrinsic: false,
                is_inferred: false,
                span: test_span(),
            })],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: crate::ast::MethodKind::Primary,
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
    let warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("Unused parameter"))
        .collect();
    assert!(
        warnings.is_empty(),
        "@intrinsic body: all params are passed to the primitive, expected no warning, got: {warnings:?}"
    );
}

#[test]
fn test_unused_variable_in_class_method_warns() {
    // Class method with unused local: class create => x := 42. nil
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
        state: vec![],
        methods: vec![],
        class_methods: vec![MethodDefinition {
            selector: MessageSelector::Unary("create".into()),
            parameters: vec![],
            body: vec![
                bare(Expression::Assignment {
                    target: Box::new(Expression::Identifier(Identifier::new(
                        "temp",
                        Span::new(10, 14),
                    ))),
                    value: Box::new(Expression::Literal(Literal::Integer(42), test_span())),
                    type_annotation: None,
                    span: test_span(),
                }),
                bare(Expression::Identifier(Identifier::new("nil", test_span()))),
            ],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: true,
            kind: crate::ast::MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: test_span(),
        }],
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

    let warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("Unused variable"))
        .collect();
    assert_eq!(warnings.len(), 1);
    assert!(warnings[0].message.contains("`temp`"));
}

#[test]
fn test_block_parameter_no_unused_warning() {
    // Block parameters should not warn: getValue => [:x | x]
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
        state: vec![],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Unary("getValue".into()),
            parameters: vec![],
            body: vec![bare(Expression::Block(Block::new(
                vec![BlockParameter::new("x", test_span())],
                vec![bare(Expression::Identifier(Identifier::new(
                    "x",
                    test_span(),
                )))],
                test_span(),
            )))],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: crate::ast::MethodKind::Primary,
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

    let warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("Unused variable"))
        .collect();
    assert!(warnings.is_empty());
}

#[test]
fn test_pattern_variable_no_unused_warning() {
    // Pattern variables in match arms should not warn:
    // getValue => x match: { 1 -> #one }
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
        state: vec![],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Unary("getValue".into()),
            parameters: vec![],
            body: vec![bare(Expression::Match {
                exhaustive: false,
                value: Box::new(Expression::Literal(Literal::Integer(1), test_span())),
                arms: vec![MatchArm::new(
                    Pattern::Variable(Identifier::new("result", test_span())),
                    Expression::Identifier(Identifier::new("result", test_span())),
                    test_span(),
                )],
                span: test_span(),
            })],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: crate::ast::MethodKind::Primary,
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

    let warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("Unused variable"))
        .collect();
    assert!(warnings.is_empty());
}

#[test]
fn test_unused_variable_in_nested_block_warns() {
    // Unused variable declared inside a nested block should warn:
    // getValue => [unused := 42]
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
        state: vec![],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Unary("getValue".into()),
            parameters: vec![],
            body: vec![bare(Expression::Block(Block::new(
                vec![],
                vec![bare(Expression::Assignment {
                    target: Box::new(Expression::Identifier(Identifier::new(
                        "unused",
                        Span::new(10, 16),
                    ))),
                    value: Box::new(Expression::Literal(Literal::Integer(42), test_span())),
                    type_annotation: None,
                    span: test_span(),
                })],
                test_span(),
            )))],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: crate::ast::MethodKind::Primary,
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

    let warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("Unused variable"))
        .collect();
    assert_eq!(warnings.len(), 1);
    assert!(warnings[0].message.contains("`unused`"));
}

#[test]
fn test_variable_used_via_closure_no_warning() {
    // Variable defined at method scope, used inside a block:
    // getValue => x := 1. [x]
    // The block reads x, so no unused warning.
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
        state: vec![],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Unary("getValue".into()),
            parameters: vec![],
            body: vec![
                bare(Expression::Assignment {
                    target: Box::new(Expression::Identifier(Identifier::new(
                        "x",
                        Span::new(10, 11),
                    ))),
                    value: Box::new(Expression::Literal(Literal::Integer(1), test_span())),
                    type_annotation: None,
                    span: test_span(),
                }),
                bare(Expression::Block(Block::new(
                    vec![],
                    vec![bare(Expression::Identifier(Identifier::new(
                        "x",
                        test_span(),
                    )))],
                    test_span(),
                ))),
            ],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            kind: crate::ast::MethodKind::Primary,
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

    let warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("Unused variable"))
        .collect();
    // x is used via closure in the block
    assert!(warnings.is_empty());
}
