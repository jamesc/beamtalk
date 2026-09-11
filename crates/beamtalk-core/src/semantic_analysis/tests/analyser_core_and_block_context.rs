// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Core `Analyser` fundamentals: empty-module analysis,
//! `BlockContext`/`SemanticError`/`BlockInfo`/`CapturedVar`/
//! mutation-kind construction, and block-context classification
//! (control-flow, stored, passed) including field-assignment and
//! captured-mutation diagnostics inside blocks.

use super::*;

#[test]
fn test_analyse_empty_module() {
    let module = Module::new(vec![], Span::default());
    let result = analyse(&module);

    assert_eq!(result.diagnostics.len(), 0);
    assert_eq!(result.block_info.len(), 0);
}

#[test]
fn test_analysis_result_default() {
    let result = AnalysisResult::default();
    assert_eq!(result.diagnostics.len(), 0);
    assert_eq!(result.block_info.len(), 0);
}

#[test]
fn test_block_context_values() {
    // Ensure all variants are constructible
    let contexts = [
        BlockContext::ControlFlow,
        BlockContext::Stored,
        BlockContext::Passed,
        BlockContext::Other,
        BlockContext::Unknown,
    ];

    assert_eq!(contexts.len(), 5);
}

#[test]
fn test_semantic_error_creation() {
    let error = SemanticError::new(
        SemanticErrorKind::UndefinedVariable { name: "foo".into() },
        Span::default(),
    );

    assert!(matches!(
        error.kind,
        SemanticErrorKind::UndefinedVariable { .. }
    ));
}

#[test]
fn test_block_info_construction() {
    let block_info = BlockInfo {
        context: BlockContext::ControlFlow,
        captures: vec![CapturedVar {
            name: "count".into(),
            defined_at: Span::default(),
        }],
        mutations: vec![Mutation {
            kind: MutationKind::LocalVariable { name: "x".into() },
            span: Span::default(),
        }],
    };

    assert_eq!(block_info.context, BlockContext::ControlFlow);
    assert_eq!(block_info.captures.len(), 1);
    assert_eq!(block_info.mutations.len(), 1);
}

#[test]
fn test_captured_var_construction() {
    let captured = CapturedVar {
        name: "myVar".into(),
        defined_at: Span::default(),
    };

    assert_eq!(captured.name, "myVar");
}

#[test]
fn test_mutation_kinds() {
    let local = Mutation {
        kind: MutationKind::LocalVariable { name: "x".into() },
        span: Span::default(),
    };

    let captured = Mutation {
        kind: MutationKind::CapturedVariable {
            name: "count".into(),
        },
        span: Span::default(),
    };

    let field = Mutation {
        kind: MutationKind::Field { name: "sum".into() },
        span: Span::default(),
    };

    assert!(matches!(local.kind, MutationKind::LocalVariable { .. }));
    assert!(matches!(
        captured.kind,
        MutationKind::CapturedVariable { .. }
    ));
    assert!(matches!(field.kind, MutationKind::Field { .. }));
}

#[test]
fn test_analyse_simple_block() {
    // Create a simple block: [:x | x + 1]
    let block = Block::new(
        vec![BlockParameter::new("x", test_span())],
        vec![bare(Expression::Identifier(Identifier::new(
            "x",
            test_span(),
        )))],
        test_span(),
    );
    let expr = Expression::Block(block);
    let module = Module::new(vec![bare(expr)], test_span());

    let result = analyse(&module);

    // Block should be recorded
    assert_eq!(result.block_info.len(), 1);
    assert_eq!(result.diagnostics.len(), 0);
}

#[test]
fn test_analyse_block_with_capture() {
    // Create: count := 0. [:x | count + x]
    let count_def = Expression::Assignment {
        target: Box::new(Expression::Identifier(Identifier::new(
            "count",
            test_span(),
        ))),
        value: Box::new(Expression::Literal(
            crate::ast::Literal::Integer(0),
            test_span(),
        )),
        type_annotation: None,
        span: test_span(),
    };

    let block = Block::new(
        vec![BlockParameter::new("x", test_span())],
        vec![bare(Expression::Identifier(Identifier::new(
            "count",
            test_span(),
        )))],
        Span::new(10, 20),
    );

    let module = Module::new(
        vec![bare(count_def), bare(Expression::Block(block))],
        test_span(),
    );

    let result = analyse(&module);

    // Block should capture 'count'
    let block_span = Span::new(10, 20);
    let block_info = result.block_info.get(&block_span).unwrap();
    assert_eq!(block_info.captures.len(), 1);
    assert_eq!(block_info.captures[0].name, "count");
}

#[test]
fn test_analyse_block_with_local_mutation() {
    // Create: [:x | temp := x. temp]
    let block = Block::new(
        vec![BlockParameter::new("x", test_span())],
        vec![
            bare(Expression::Assignment {
                target: Box::new(Expression::Identifier(Identifier::new("temp", test_span()))),
                value: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
                type_annotation: None,
                span: test_span(),
            }),
            bare(Expression::Identifier(Identifier::new("temp", test_span()))),
        ],
        Span::new(10, 20),
    );

    let module = Module::new(vec![bare(Expression::Block(block))], test_span());

    let result = analyse(&module);

    // Block should have local mutation
    let block_span = Span::new(10, 20);
    let block_info = result.block_info.get(&block_span).unwrap();
    assert_eq!(block_info.mutations.len(), 1);
    assert!(matches!(
        block_info.mutations[0].kind,
        MutationKind::LocalVariable { .. }
    ));
}

#[test]
fn test_analyse_block_with_captured_mutation() {
    // Create: count := 0. [:x | count := count + x]
    let count_def = Expression::Assignment {
        target: Box::new(Expression::Identifier(Identifier::new(
            "count",
            test_span(),
        ))),
        value: Box::new(Expression::Literal(
            crate::ast::Literal::Integer(0),
            test_span(),
        )),
        type_annotation: None,
        span: test_span(),
    };

    let block = Block::new(
        vec![BlockParameter::new("x", test_span())],
        vec![bare(Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new(
                "count",
                test_span(),
            ))),
            value: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
            type_annotation: None,
            span: test_span(),
        })],
        Span::new(10, 20),
    );

    let module = Module::new(
        vec![bare(count_def), bare(Expression::Block(block))],
        test_span(),
    );

    let result = analyse(&module);

    // Block should have captured mutation
    let block_span = Span::new(10, 20);
    let block_info = result.block_info.get(&block_span).unwrap();
    assert_eq!(block_info.mutations.len(), 1);
    assert!(matches!(
        block_info.mutations[0].kind,
        MutationKind::CapturedVariable { .. }
    ));
}

#[test]
fn test_analyse_control_flow_block_context() {
    // Create: 5 timesRepeat: [x := 1]
    let block = Block::new(
        vec![],
        vec![bare(Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
            value: Box::new(Expression::Literal(
                crate::ast::Literal::Integer(1),
                test_span(),
            )),
            type_annotation: None,
            span: test_span(),
        })],
        Span::new(20, 30),
    );

    let message_send = Expression::MessageSend {
        receiver: Box::new(Expression::Literal(
            crate::ast::Literal::Integer(5),
            test_span(),
        )),
        selector: MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
            "timesRepeat:",
            test_span(),
        )]),
        arguments: vec![Expression::Block(block)],
        is_cast: false,
        span: test_span(),
    };

    let module = Module::new(vec![bare(message_send)], test_span());

    let result = analyse(&module);

    // Block should have ControlFlow context
    let block_span = Span::new(20, 30);
    let block_info = result.block_info.get(&block_span).unwrap();
    assert_eq!(block_info.context, BlockContext::ControlFlow);
}

// PR Review Comment Tests

#[test]
fn test_block_assigned_to_variable_gets_stored_context() {
    // Comment 7: Block assigned to variable should get Stored context
    // Code: myBlock := [:x | x + 1]
    let block = Block::new(
        vec![BlockParameter::new("x", Span::new(15, 16))],
        vec![bare(Expression::Identifier(Identifier::new(
            "x",
            Span::new(19, 20),
        )))],
        Span::new(12, 25),
    );

    let assignment = Expression::Assignment {
        target: Box::new(Expression::Identifier(Identifier::new(
            "myBlock",
            Span::new(0, 7),
        ))),
        value: Box::new(Expression::Block(block)),
        type_annotation: None,
        span: Span::new(0, 25),
    };

    let module = Module::new(vec![bare(assignment)], Span::new(0, 25));
    let result = analyse(&module);

    // Block should have Stored context
    let block_span = Span::new(12, 25);
    let block_info = result.block_info.get(&block_span).unwrap();
    assert_eq!(block_info.context, BlockContext::Stored);
}

#[test]
fn test_block_passed_as_argument_gets_passed_context() {
    // Comment 8: Block passed as non-control-flow argument should get Passed context
    // Code: array at: 1 put: [:x | x + 1]
    let block = Block::new(
        vec![BlockParameter::new("x", Span::new(23, 24))],
        vec![bare(Expression::Identifier(Identifier::new(
            "x",
            Span::new(27, 28),
        )))],
        Span::new(20, 33),
    );

    let message = Expression::MessageSend {
        receiver: Box::new(Expression::Identifier(Identifier::new(
            "array",
            Span::new(0, 5),
        ))),
        selector: MessageSelector::Keyword(vec![
            crate::ast::KeywordPart::new("at:", Span::new(6, 9)),
            crate::ast::KeywordPart::new("put:", Span::new(12, 16)),
        ]),
        arguments: vec![
            Expression::Literal(crate::ast::Literal::Integer(1), Span::new(10, 11)),
            Expression::Block(block),
        ],
        is_cast: false,
        span: Span::new(0, 33),
    };

    let module = Module::new(vec![bare(message)], Span::new(0, 33));
    let result = analyse(&module);

    // Block should have Passed context
    let block_span = Span::new(20, 33);
    let block_info = result.block_info.get(&block_span).unwrap();
    assert_eq!(block_info.context, BlockContext::Passed);
}

#[test]
fn test_self_available_in_method_bodies() {
    // Test that 'self' is implicitly available in method bodies
    // Simple method that uses self:
    //   getValue => self.value

    use crate::ast::{
        ClassDefinition, ClassKind, CommentAttachment, DeclaredKeyword, MessageSelector,
        MethodDefinition, MethodKind, StateDeclaration,
    };

    let get_value_method = MethodDefinition {
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
        kind: MethodKind::Primary,
        expect: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: test_span(),
    };

    let state_decl = StateDeclaration {
        name: Identifier::new("value", test_span()),
        type_annotation: None,
        default_value: Some(Expression::Literal(Literal::Integer(0), test_span())),
        expect: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        declared_keyword: DeclaredKeyword::default(),
        span: test_span(),
    };

    let class_def = ClassDefinition {
        name: Identifier::new("Counter", test_span()),
        superclass: Some(Identifier::new("Actor", test_span())),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![state_decl],
        methods: vec![get_value_method],
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
        classes: vec![class_def],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
        span: test_span(),
    };
    let result = analyse(&module);

    // Should have NO diagnostics - 'self' should be recognized
    assert_eq!(
        result.diagnostics.len(),
        0,
        "Expected no diagnostics, but got: {:?}",
        result.diagnostics
    );
}

#[test]
fn test_field_assignment_in_stored_block_emits_error() {
    // Test: myBlock := [self.sum := 0] should emit error

    let field_assignment = Expression::Assignment {
        target: Box::new(Expression::FieldAccess {
            receiver: Box::new(Expression::Identifier(Identifier::new("self", test_span()))),
            field: Identifier::new("sum", test_span()),
            span: test_span(),
        }),
        value: Box::new(Expression::Literal(Literal::Integer(0), test_span())),
        type_annotation: None,
        span: test_span(),
    };

    let block = Expression::Block(crate::ast::Block {
        parameters: vec![],
        body: vec![bare(field_assignment)],
        span: test_span(),
    });

    // Assign block to variable (Stored context)
    let assignment = Expression::Assignment {
        target: Box::new(Expression::Identifier(Identifier::new(
            "myBlock",
            test_span(),
        ))),
        value: Box::new(block),
        type_annotation: None,
        span: test_span(),
    };

    let module = Module::new(vec![bare(assignment)], test_span());
    let result = analyse(&module);

    // Should have at least 1 error diagnostic for field assignment in stored block
    // (may have additional errors for undefined 'self', which is expected)
    assert!(!result.diagnostics.is_empty());
    let has_field_error = result.diagnostics.iter().any(|d| {
        d.message.contains("cannot assign to field 'sum'") && d.message.contains("stored closure")
    });
    assert!(
        has_field_error,
        "Expected field assignment error, got: {:?}",
        result.diagnostics
    );
}

#[test]
fn test_field_assignment_in_passed_block_no_error() {
    // obj callWith: [:x | self.sum := 0] does not emit an error.
    // Field-write blocks are promoted to Tier 2 (stateful) when passed to HOMs,
    // so field mutations thread through StateAcc correctly.
    // "callWith:" is not a control flow selector, so block is Passed context.
    let field_assignment = Expression::Assignment {
        target: Box::new(Expression::FieldAccess {
            receiver: Box::new(Expression::Identifier(Identifier::new("self", test_span()))),
            field: Identifier::new("sum", test_span()),
            span: test_span(),
        }),
        value: Box::new(Expression::Literal(Literal::Integer(0), test_span())),
        type_annotation: None,
        span: test_span(),
    };

    let block = Expression::Block(crate::ast::Block {
        parameters: vec![crate::ast::BlockParameter::new("x", test_span())],
        body: vec![bare(field_assignment)],
        span: test_span(),
    });

    // Pass block to a message send with a non-control-flow selector
    let message_send = Expression::MessageSend {
        receiver: Box::new(Expression::Identifier(Identifier::new("obj", test_span()))),
        selector: crate::ast::MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
            "callWith:",
            test_span(),
        )]),
        arguments: vec![block],
        is_cast: false,
        span: test_span(),
    };

    let module = Module::new(vec![bare(message_send)], test_span());
    let result = analyse(&module);

    // Should NOT have a field assignment error for passed blocks.
    // (may have unrelated errors for undefined variables, which is expected)
    let has_field_error = result
        .diagnostics
        .iter()
        .any(|d| d.message.contains("cannot assign to field 'sum'"));
    assert!(
        !has_field_error,
        "Should not have field-in-passed-block error (BT-1140), got: {:?}",
        result.diagnostics
    );
}

#[test]
fn test_field_assignment_in_field_stored_block_no_error() {
    // self.onTick := [:x | self.sum := 0] must NOT emit the
    // stored-closure field error, unlike a *local* var (see
    // test_field_assignment_in_stored_block_emits_error above, which is
    // unaffected — this test is specifically about `self.field := [block]`).
    // Every `self.field value(:...)` call site runtime-discriminates
    // Tier 1 vs Tier 2, so a block stored into a field is unconditionally
    // safe regardless of which method later invokes it.
    let field_assignment = Expression::Assignment {
        target: Box::new(Expression::FieldAccess {
            receiver: Box::new(Expression::Identifier(Identifier::new("self", test_span()))),
            field: Identifier::new("sum", test_span()),
            span: test_span(),
        }),
        value: Box::new(Expression::Literal(Literal::Integer(0), test_span())),
        type_annotation: None,
        span: test_span(),
    };

    let block = Expression::Block(crate::ast::Block {
        parameters: vec![crate::ast::BlockParameter::new("x", test_span())],
        body: vec![bare(field_assignment)],
        span: test_span(),
    });

    // self.onTick := [:x | self.sum := 0] — block stored into a field.
    let assignment = Expression::Assignment {
        target: Box::new(Expression::FieldAccess {
            receiver: Box::new(Expression::Identifier(Identifier::new("self", test_span()))),
            field: Identifier::new("onTick", test_span()),
            span: test_span(),
        }),
        value: Box::new(block),
        type_annotation: None,
        span: test_span(),
    };

    let module = Module::new(vec![bare(assignment)], test_span());
    let result = analyse(&module);

    let has_field_error = result
        .diagnostics
        .iter()
        .any(|d| d.message.contains("cannot assign to field 'sum'"));
    assert!(
        !has_field_error,
        "Should not have field-in-stored-block error for a block stored into \
         a field (BT-2797), got: {:?}",
        result.diagnostics
    );
}

#[test]
fn test_captured_variable_mutation_in_stored_block_no_warning() {
    // ADR 0041 Phase 3: Captured variable mutations in stored blocks are
    // valid and supported via the Tier 2 stateful block protocol. Tier 2
    // threads state through StateAcc maps so mutations propagate correctly.
    let count_def = Expression::Assignment {
        target: Box::new(Expression::Identifier(Identifier::new(
            "count",
            test_span(),
        ))),
        value: Box::new(Expression::Literal(Literal::Integer(0), test_span())),
        type_annotation: None,
        span: test_span(),
    };

    let count_mutation = Expression::Assignment {
        target: Box::new(Expression::Identifier(Identifier::new(
            "count",
            test_span(),
        ))),
        value: Box::new(Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new(
                "count",
                test_span(),
            ))),
            selector: crate::ast::MessageSelector::Binary("+".into()),
            arguments: vec![Expression::Literal(Literal::Integer(1), test_span())],
            is_cast: false,
            span: test_span(),
        }),
        type_annotation: None,
        span: test_span(),
    };

    let block = Expression::Block(crate::ast::Block {
        parameters: vec![],
        body: vec![bare(count_mutation)],
        span: test_span(),
    });

    // Assign block to variable (Stored context)
    let block_assignment = Expression::Assignment {
        target: Box::new(Expression::Identifier(Identifier::new(
            "myBlock",
            test_span(),
        ))),
        value: Box::new(block),
        type_annotation: None,
        span: test_span(),
    };

    let module = Module::new(vec![bare(count_def), bare(block_assignment)], test_span());
    let result = analyse(&module);

    // Should have NO diagnostic — captured variable mutations in stored blocks are valid
    assert_eq!(
        result.diagnostics.len(),
        0,
        "Unexpected diagnostics: {:?}",
        result.diagnostics
    );
}

#[test]
fn test_field_assignment_in_control_flow_block_no_diagnostic() {
    // Test: 10 timesRepeat: [self.sum := 0] should NOT emit error
    let field_assignment = Expression::Assignment {
        target: Box::new(Expression::FieldAccess {
            receiver: Box::new(Expression::Identifier(Identifier::new("self", test_span()))),
            field: Identifier::new("sum", test_span()),
            span: test_span(),
        }),
        value: Box::new(Expression::Literal(Literal::Integer(0), test_span())),
        type_annotation: None,
        span: test_span(),
    };

    let block = Expression::Block(crate::ast::Block {
        parameters: vec![],
        body: vec![bare(field_assignment)],
        span: test_span(),
    });

    // Use in control flow position
    let message_send = Expression::MessageSend {
        receiver: Box::new(Expression::Literal(Literal::Integer(10), test_span())),
        selector: crate::ast::MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
            "timesRepeat:",
            test_span(),
        )]),
        arguments: vec![block],
        is_cast: false,
        span: test_span(),
    };

    let module = Module::new(vec![bare(message_send)], test_span());
    let result = analyse(&module);

    // Should have NO diagnostics for field assignment in control flow blocks
    // (may have errors for undefined 'self', but no mutation warnings)
    let has_field_error = result
        .diagnostics
        .iter()
        .any(|d| d.message.contains("cannot assign to field"));
    assert!(
        !has_field_error,
        "Should not have field assignment error for control flow, got: {:?}",
        result.diagnostics
    );
}

#[test]
fn test_local_variable_mutation_in_stored_block_no_diagnostic() {
    // Test: myBlock := [x := 0. x := x + 1] should NOT emit warning
    // (only captured variable mutations get warnings)
    let x_def = Expression::Assignment {
        target: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
        value: Box::new(Expression::Literal(Literal::Integer(0), test_span())),
        type_annotation: None,
        span: test_span(),
    };

    let x_mutation = Expression::Assignment {
        target: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
        value: Box::new(Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
            selector: crate::ast::MessageSelector::Binary("+".into()),
            arguments: vec![Expression::Literal(Literal::Integer(1), test_span())],
            is_cast: false,
            span: test_span(),
        }),
        type_annotation: None,
        span: test_span(),
    };

    let block = Expression::Block(crate::ast::Block {
        parameters: vec![],
        body: vec![bare(x_def), bare(x_mutation)],
        span: test_span(),
    });

    // Assign block to variable (Stored context)
    let block_assignment = Expression::Assignment {
        target: Box::new(Expression::Identifier(Identifier::new(
            "myBlock",
            test_span(),
        ))),
        value: Box::new(block),
        type_annotation: None,
        span: test_span(),
    };

    let module = Module::new(vec![bare(block_assignment)], test_span());
    let result = analyse(&module);

    // Should have NO diagnostics - local variables can be mutated
    assert_eq!(result.diagnostics.len(), 0);
}

#[test]
fn test_analyse_with_known_vars_suppresses_undefined() {
    // Test: x + 1 where 'x' is a known REPL variable
    let expr = Expression::MessageSend {
        receiver: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
        selector: crate::ast::MessageSelector::Binary("+".into()),
        arguments: vec![Expression::Literal(Literal::Integer(1), test_span())],
        is_cast: false,
        span: test_span(),
    };

    let module = Module::new(vec![bare(expr)], test_span());

    // Without known vars - should report undefined
    let result_without = analyse(&module);
    assert!(
        result_without
            .diagnostics
            .iter()
            .any(|d| d.message.contains("Undefined variable: x")),
        "Should report undefined variable without known vars"
    );

    // With 'x' in known vars - should NOT report undefined
    let known_vars = ["x"];
    let result_with = analyse_full(
        &module,
        AnalysisContext::default().with_known_vars(&known_vars),
    );
    assert!(
        !result_with
            .diagnostics
            .iter()
            .any(|d| d.message.contains("Undefined variable: x")),
        "Should not report undefined variable when in known_vars, got: {:?}",
        result_with.diagnostics
    );
}

#[test]
fn test_analyse_with_known_vars_handles_reassignment() {
    // Test: x := x + 1 where 'x' is a known REPL variable (reassignment)
    let expr = Expression::Assignment {
        target: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
        value: Box::new(Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new("x", test_span()))),
            selector: crate::ast::MessageSelector::Binary("+".into()),
            arguments: vec![Expression::Literal(Literal::Integer(1), test_span())],
            is_cast: false,
            span: test_span(),
        }),
        type_annotation: None,
        span: test_span(),
    };

    let module = Module::new(vec![bare(expr)], test_span());

    // With 'x' known, the RHS reference should not report undefined
    let known_vars = ["x"];
    let result = analyse_full(
        &module,
        AnalysisContext::default().with_known_vars(&known_vars),
    );
    assert!(
        !result
            .diagnostics
            .iter()
            .any(|d| d.message.contains("Undefined variable")),
        "Should not report undefined for known REPL variable reassignment, got: {:?}",
        result.diagnostics
    );
}
