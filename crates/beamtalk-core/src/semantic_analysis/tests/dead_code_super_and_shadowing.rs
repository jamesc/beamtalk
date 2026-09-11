// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Dead-code-after-return diagnostics, `super` outside/inside
//! method diagnostics, and variable-shadowing warnings (block
//! parameters, match patterns).

use super::*;

// --- Dead code after early return tests ---

#[test]
fn test_dead_code_after_return_in_method() {
    // Method: getValue => ^ 42. self doSomething
    let class = ClassDefinition {
        name: Identifier::new("Foo", test_span()),
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
            selector: MessageSelector::Unary("getValue".into()),
            parameters: vec![],
            body: vec![
                bare(Expression::Return {
                    value: Box::new(Expression::Literal(Literal::Integer(42), Span::new(0, 4))),
                    span: Span::new(0, 4),
                }),
                bare(Expression::MessageSend {
                    receiver: Box::new(Expression::Identifier(Identifier::new(
                        "self",
                        Span::new(5, 9),
                    ))),
                    selector: MessageSelector::Unary("doSomething".into()),
                    arguments: vec![],
                    is_cast: false,
                    span: Span::new(5, 20),
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

    let dead_code: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("Unreachable code"))
        .collect();
    assert_eq!(dead_code.len(), 1);
    assert!(
        dead_code[0]
            .message
            .contains("Unreachable code after early return")
    );
}

#[test]
fn test_no_dead_code_without_return() {
    // Method with no return: getValue => 42
    let class = ClassDefinition {
        name: Identifier::new("Foo", test_span()),
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
            selector: MessageSelector::Unary("getValue".into()),
            parameters: vec![],
            body: vec![bare(Expression::Literal(Literal::Integer(42), test_span()))],
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

    let dead_code: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("Unreachable code"))
        .collect();
    assert!(dead_code.is_empty());
}

#[test]
fn test_dead_code_in_block() {
    // Block with return followed by expression: [^ 42. 99]
    let block = Expression::Block(Block {
        parameters: vec![],
        body: vec![
            bare(Expression::Return {
                value: Box::new(Expression::Literal(Literal::Integer(42), Span::new(0, 4))),
                span: Span::new(0, 4),
            }),
            bare(Expression::Literal(Literal::Integer(99), Span::new(5, 7))),
        ],
        span: test_span(),
    });

    // Put block inside a method body so it's in scope
    let class = ClassDefinition {
        name: Identifier::new("Foo", test_span()),
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
            selector: MessageSelector::Unary("test".into()),
            parameters: vec![],
            body: vec![bare(block)],
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

    let dead_code: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("Unreachable code"))
        .collect();
    assert_eq!(dead_code.len(), 1);
}

#[test]
fn test_return_at_end_no_warning() {
    // Return as the last expression — no dead code
    let class = ClassDefinition {
        name: Identifier::new("Foo", test_span()),
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
            selector: MessageSelector::Unary("getValue".into()),
            parameters: vec![],
            body: vec![bare(Expression::Return {
                value: Box::new(Expression::Literal(Literal::Integer(42), test_span())),
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

    let dead_code: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("Unreachable code"))
        .collect();
    assert!(dead_code.is_empty());
}

// --- Super outside method tests ---

#[test]
fn test_super_outside_method_gives_error() {
    // Using super at module top level should error
    let super_expr = Expression::Super(Span::new(0, 5));
    let module = Module::new(vec![bare(super_expr)], test_span());
    let result = analyse(&module);

    let super_errors: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("super"))
        .collect();
    assert_eq!(super_errors.len(), 1);
    assert!(
        super_errors[0]
            .message
            .contains("super can only be used inside a method body")
    );
    assert_eq!(super_errors[0].severity, Severity::Error);
}

#[test]
fn test_super_inside_method_no_error() {
    // super inside a method body should not emit an error from name resolution
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
            selector: MessageSelector::Unary("reset".into()),
            parameters: vec![],
            body: vec![bare(Expression::MessageSend {
                receiver: Box::new(Expression::Super(test_span())),
                selector: MessageSelector::Unary("reset".into()),
                arguments: vec![],
                is_cast: false,
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

    let super_errors: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("super can only"))
        .collect();
    assert!(super_errors.is_empty());
}

#[test]
fn test_super_in_class_scope_gives_error() {
    // super at class level (not inside method) should error
    // This tests depth 1 (class scope, not method scope)
    let super_expr = Expression::Super(Span::new(0, 5));
    let module = Module::new(vec![bare(super_expr)], test_span());
    let result = analyse(&module);

    let super_errors: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("super can only"))
        .collect();
    assert_eq!(super_errors.len(), 1);
}

// --- Variable shadowing tests ---

#[test]
fn test_block_param_shadows_outer_variable() {
    // [:x | [:x | x + 1]] — inner x shadows outer x
    let inner_block = Expression::Block(Block {
        parameters: vec![BlockParameter {
            name: "x".into(),
            span: Span::new(10, 11),
        }],
        body: vec![bare(Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new(
                "x",
                Span::new(14, 15),
            ))),
            selector: MessageSelector::Binary("+".into()),
            arguments: vec![Expression::Literal(Literal::Integer(1), Span::new(18, 19))],
            is_cast: false,
            span: Span::new(14, 19),
        })],
        span: Span::new(8, 20),
    });

    let outer_block = Expression::Block(Block {
        parameters: vec![BlockParameter {
            name: "x".into(),
            span: Span::new(1, 2),
        }],
        body: vec![bare(inner_block)],
        span: Span::new(0, 21),
    });

    // Put in method body
    let class = ClassDefinition {
        name: Identifier::new("Foo", test_span()),
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
            selector: MessageSelector::Unary("test".into()),
            parameters: vec![],
            body: vec![bare(outer_block)],
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

    let shadow_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("shadows"))
        .collect();
    assert_eq!(shadow_warnings.len(), 1);
    assert!(shadow_warnings[0].message.contains("Variable `x` shadows"));
    assert_eq!(shadow_warnings[0].severity, Severity::Warning);
}

#[test]
fn test_underscore_prefixed_no_shadow_warning() {
    // [:_x | [:_x | _x + 1]] — underscore-prefixed, no warning
    let inner_block = Expression::Block(Block {
        parameters: vec![BlockParameter {
            name: "_x".into(),
            span: Span::new(10, 12),
        }],
        body: vec![bare(Expression::Identifier(Identifier::new(
            "_x",
            Span::new(15, 17),
        )))],
        span: Span::new(8, 18),
    });

    let outer_block = Expression::Block(Block {
        parameters: vec![BlockParameter {
            name: "_x".into(),
            span: Span::new(1, 3),
        }],
        body: vec![bare(inner_block)],
        span: Span::new(0, 19),
    });

    let class = ClassDefinition {
        name: Identifier::new("Foo", test_span()),
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
            selector: MessageSelector::Unary("test".into()),
            parameters: vec![],
            body: vec![bare(outer_block)],
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

    let shadow_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("shadows"))
        .collect();
    assert!(shadow_warnings.is_empty());
}

#[test]
fn test_no_shadow_warning_different_names() {
    // [:x | [:y | y + 1]] — different names, no shadowing
    let inner_block = Expression::Block(Block {
        parameters: vec![BlockParameter {
            name: "y".into(),
            span: Span::new(10, 11),
        }],
        body: vec![bare(Expression::Identifier(Identifier::new(
            "y",
            Span::new(14, 15),
        )))],
        span: Span::new(8, 16),
    });

    let outer_block = Expression::Block(Block {
        parameters: vec![BlockParameter {
            name: "x".into(),
            span: Span::new(1, 2),
        }],
        body: vec![bare(inner_block)],
        span: Span::new(0, 17),
    });

    let class = ClassDefinition {
        name: Identifier::new("Foo", test_span()),
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
            selector: MessageSelector::Unary("test".into()),
            parameters: vec![],
            body: vec![bare(outer_block)],
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

    let shadow_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("shadows"))
        .collect();
    assert!(shadow_warnings.is_empty());
}

#[test]
fn test_block_match_pattern_var_not_treated_as_capture() {
    // A block containing a match expression where the pattern variable
    // has the same name as an outer variable should NOT treat the pattern
    // variable as a captured variable.
    //
    // Code equivalent:
    //   x := 0
    //   [:val | val match: [x -> x]]
    //
    // The `x` in the match arm pattern and body refers to the pattern-bound
    // variable, not the outer `x`. It should NOT appear in block captures.
    let outer_x = Expression::Assignment {
        target: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
        value: Box::new(Expression::Literal(Literal::Integer(0), test_span())),
        type_annotation: None,
        span: test_span(),
    };

    let block_span = Span::new(100, 200);
    let match_expr = Expression::Match {
        exhaustive: false,
        value: Box::new(Expression::Identifier(Identifier::new("val", test_span()))),
        arms: vec![MatchArm::new(
            Pattern::Variable(Identifier::new("x", test_span())),
            Expression::Identifier(Identifier::new("x", test_span())),
            test_span(),
        )],
        span: test_span(),
    };

    let block = Block::new(
        vec![BlockParameter::new("val", test_span())],
        vec![bare(match_expr)],
        block_span,
    );

    let module = Module::new(
        vec![bare(outer_x), bare(Expression::Block(block))],
        test_span(),
    );

    let result = analyse(&module);

    let block_info = result.block_info.get(&block_span).unwrap();
    // The pattern variable `x` should NOT be in captures,
    // even though an outer variable `x` exists.
    assert!(
        block_info.captures.is_empty(),
        "Pattern variable 'x' should not be treated as a capture, but found: {:?}",
        block_info
            .captures
            .iter()
            .map(|c| &c.name)
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_block_match_captures_real_outer_variable() {
    // Counterpart to the above test: when a match arm body references a
    // variable that is NOT a pattern variable, it SHOULD be captured.
    //
    // Code equivalent:
    //   y := 42
    //   [:val | val match: [x -> y]]
    //
    // Here `y` in the match body is genuinely captured from outer scope.
    let outer_y = Expression::Assignment {
        target: Box::new(Expression::Identifier(Identifier::new("y", test_span()))),
        value: Box::new(Expression::Literal(Literal::Integer(42), test_span())),
        type_annotation: None,
        span: test_span(),
    };

    let block_span = Span::new(100, 200);
    let match_expr = Expression::Match {
        exhaustive: false,
        value: Box::new(Expression::Identifier(Identifier::new("val", test_span()))),
        arms: vec![MatchArm::new(
            Pattern::Variable(Identifier::new("x", test_span())),
            Expression::Identifier(Identifier::new("y", test_span())),
            test_span(),
        )],
        span: test_span(),
    };

    let block = Block::new(
        vec![BlockParameter::new("val", test_span())],
        vec![bare(match_expr)],
        block_span,
    );

    let module = Module::new(
        vec![bare(outer_y), bare(Expression::Block(block))],
        test_span(),
    );

    let result = analyse(&module);

    let block_info = result.block_info.get(&block_span).unwrap();
    assert_eq!(
        block_info.captures.len(),
        1,
        "Should capture outer variable 'y'"
    );
    assert_eq!(block_info.captures[0].name, "y");
}
