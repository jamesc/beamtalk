// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for control-flow Core Erlang code generation.
//!
//! Covers loop constructs (whileTrue, whileFalse, repeat), stored-closure
//! validation, conditional inline-case generation for ifTrue/ifFalse, and
//! match: arms with array/map destructuring patterns (BT-1296).

use super::*;

#[test]
fn test_block_while_true_loop() {
    // [counter < 5] whileTrue: [counter := counter + 1]
    let mut generator = CoreErlangGenerator::new("test");

    // Condition block: [counter < 5]
    let condition_block = Block::new(
        vec![],
        vec![bare(Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new(
                "counter",
                Span::new(1, 8),
            ))),
            selector: MessageSelector::Binary("<".into()),
            arguments: vec![Expression::Literal(Literal::Integer(5), Span::new(11, 12))],
            is_cast: false,
            span: Span::new(1, 12),
        })],
        Span::new(0, 13),
    );

    // Body block: [counter := counter + 1]
    let body_block = Block::new(
        vec![],
        vec![bare(Expression::Literal(
            Literal::Integer(0),
            Span::new(0, 1),
        ))], // simplified body
        Span::new(15, 38),
    );

    let receiver = Expression::Block(condition_block);
    let selector =
        MessageSelector::Keyword(vec![KeywordPart::new("whileTrue:", Span::new(14, 24))]);
    let arguments = vec![Expression::Block(body_block)];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("letrec"),
        "whileTrue: should generate a letrec for looping. Got: {output}"
    );
    assert!(
        output.contains("case apply"),
        "whileTrue: should have case on condition result. Got: {output}"
    );
    assert!(
        output.contains("<'true'> when 'true'"),
        "whileTrue: should match on true to continue. Got: {output}"
    );
    assert!(
        output.contains("<'false'> when 'true' -> 'nil'"),
        "whileTrue: should return nil when condition is false. Got: {output}"
    );
    // Binary ops no longer wrap operands with maybe_await (ADR-0043 / BT-1321)
    assert!(
        !output.contains("maybe_await"),
        "whileTrue: binary ops should not wrap operands with maybe_await. Got: {output}"
    );
}

#[test]
fn test_block_while_false_loop() {
    // [done] whileFalse: [process next]
    let mut generator = CoreErlangGenerator::new("test");

    let condition_block = Block::new(
        vec![],
        vec![bare(Expression::Identifier(Identifier::new(
            "done",
            Span::new(1, 5),
        )))],
        Span::new(0, 6),
    );
    let body_block = Block::new(vec![], vec![], Span::new(8, 10));

    let receiver = Expression::Block(condition_block);
    let selector =
        MessageSelector::Keyword(vec![KeywordPart::new("whileFalse:", Span::new(7, 18))]);
    let arguments = vec![Expression::Block(body_block)];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("letrec"),
        "whileFalse: should generate a letrec for looping. Got: {output}"
    );
    assert!(
        output.contains("<'false'> when 'true' ->"),
        "whileFalse: should continue when condition is false. Got: {output}"
    );
    assert!(
        output.contains("<'true'> when 'true' -> 'nil'"),
        "whileFalse: should stop when condition is true. Got: {output}"
    );
}

#[test]
fn test_block_repeat_infinite_loop() {
    // [process] repeat
    let mut generator = CoreErlangGenerator::new("test");

    let body_block = Block::new(
        vec![],
        vec![bare(Expression::Literal(
            Literal::Integer(1),
            Span::new(1, 2),
        ))],
        Span::new(0, 3),
    );

    let receiver = Expression::Block(body_block);
    let selector = MessageSelector::Unary("repeat".into());

    let doc = generator
        .generate_message_send(&receiver, &selector, &[])
        .unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("letrec"),
        "repeat should generate a letrec for looping. Got: {output}"
    );
    assert!(
        output.contains("'_Loop"),
        "repeat should create a loop function. Got: {output}"
    );
    // repeat has no condition, just loops forever
    assert!(
        !output.contains("case"),
        "repeat should NOT have a case (no condition). Got: {output}"
    );
    assert!(
        !output.contains("beamtalk_future"),
        "repeat should NOT create futures. Got: {output}"
    );
}

#[test]
fn test_while_true_compiles_through_erlc() {
    // Generate a complete module with a whileTrue: expression
    let mut generator = CoreErlangGenerator::new("test_while_loop");

    // Start module
    let mut full_output = String::from("module 'test_while_loop' ['main'/0]\n  attributes []\n\n");
    full_output.push_str("'main'/0 = fun () ->\n    ");

    // Generate: [true] whileTrue: [42]
    // This creates a loop that runs once (returns nil after first iteration)
    let condition_block = Block::new(
        vec![],
        vec![bare(Expression::Identifier(Identifier::new(
            "false",
            Span::new(1, 6),
        )))],
        Span::new(0, 7),
    );
    let body_block = Block::new(
        vec![],
        vec![bare(Expression::Literal(
            Literal::Integer(42),
            Span::new(9, 11),
        ))],
        Span::new(8, 12),
    );

    let receiver = Expression::Block(condition_block);
    let selector = MessageSelector::Keyword(vec![KeywordPart::new("whileTrue:", Span::new(7, 17))]);
    let arguments = vec![Expression::Block(body_block)];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    full_output.push_str(&doc.to_pretty_string());

    full_output.push_str("\n\nend\n");

    crate::test_helpers::assert_compiles_through_erlc("test_while_loop", &full_output);
}

#[test]
fn test_validate_stored_closure_empty_block() {
    // Empty block should not trigger errors
    let block = Block {
        parameters: vec![],
        body: vec![],
        span: Span::new(0, 2),
    };

    let analysis = crate::codegen::core_erlang::block_analysis::analyze_block(&block);
    let result = CoreErlangGenerator::validate_stored_closure(&analysis, || "test".to_string());
    assert!(result.is_ok(), "Empty block should be valid");
}

#[test]
fn test_validate_stored_closure_with_captured_mutation() {
    // Block that reads and writes a captured variable: [count := count + 1]
    // `count` is read (captured from outer scope) and written → should error
    let block = Block {
        parameters: vec![],
        body: vec![bare(Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new(
                "count",
                Span::new(1, 6),
            ))),
            value: Box::new(Expression::MessageSend {
                receiver: Box::new(Expression::Identifier(Identifier::new(
                    "count",
                    Span::new(10, 15),
                ))),
                selector: MessageSelector::Binary("+".into()),
                arguments: vec![Expression::Literal(Literal::Integer(1), Span::new(18, 19))],
                is_cast: false,
                span: Span::new(10, 19),
            }),
            type_annotation: None,
            span: Span::new(1, 19),
        })],
        span: Span::new(0, 20),
    };

    let analysis = crate::codegen::core_erlang::block_analysis::analyze_block(&block);
    let result = CoreErlangGenerator::validate_stored_closure(&analysis, || "test".to_string());
    assert!(
        result.is_err(),
        "Captured variable mutation should produce error"
    );

    if let Err(CodeGenError::LocalMutationInStoredClosure { variable, .. }) = result {
        assert_eq!(variable, "count");
    } else {
        panic!("Expected LocalMutationInStoredClosure error");
    }
}

#[test]
fn test_validate_stored_closure_with_new_local_definition() {
    // BT-665: Block with only new local variable definition: [temp := 1]
    // `temp` is never read from outer scope → should be allowed
    let block = Block {
        parameters: vec![],
        body: vec![bare(Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new(
                "temp",
                Span::new(1, 5),
            ))),
            value: Box::new(Expression::Literal(Literal::Integer(1), Span::new(9, 10))),
            type_annotation: None,
            span: Span::new(1, 10),
        })],
        span: Span::new(0, 11),
    };

    let analysis = crate::codegen::core_erlang::block_analysis::analyze_block(&block);
    let result = CoreErlangGenerator::validate_stored_closure(&analysis, || "test".to_string());
    assert!(
        result.is_ok(),
        "New local definition should be allowed in stored closure"
    );
}

#[test]
fn test_validate_stored_closure_with_new_local_used_later() {
    // BT-665: Block defines a new local and uses it later: [:x | temp := x * 2. temp + 1]
    // `temp` is defined then read — NOT a captured variable → should be allowed
    let block = Block {
        parameters: vec![BlockParameter::new("x", Span::new(1, 2))],
        body: vec![
            bare(Expression::Assignment {
                target: Box::new(Expression::Identifier(Identifier::new(
                    "temp",
                    Span::new(5, 9),
                ))),
                value: Box::new(Expression::MessageSend {
                    receiver: Box::new(Expression::Identifier(Identifier::new(
                        "x",
                        Span::new(13, 14),
                    ))),
                    selector: MessageSelector::Binary("*".into()),
                    arguments: vec![Expression::Literal(Literal::Integer(2), Span::new(17, 18))],
                    is_cast: false,
                    span: Span::new(13, 18),
                }),
                type_annotation: None,
                span: Span::new(5, 18),
            }),
            bare(Expression::MessageSend {
                receiver: Box::new(Expression::Identifier(Identifier::new(
                    "temp",
                    Span::new(20, 24),
                ))),
                selector: MessageSelector::Binary("+".into()),
                arguments: vec![Expression::Literal(Literal::Integer(1), Span::new(27, 28))],
                is_cast: false,
                span: Span::new(20, 28),
            }),
        ],
        span: Span::new(0, 29),
    };

    let analysis = crate::codegen::core_erlang::block_analysis::analyze_block(&block);
    let result = CoreErlangGenerator::validate_stored_closure(&analysis, || "test".to_string());
    assert!(
        result.is_ok(),
        "Block with new local definition used later should be allowed"
    );
}

#[test]
fn test_validate_stored_closure_with_field_assignment() {
    // Block with field assignment: [self.value := 1]
    let block = Block {
        parameters: vec![],
        body: vec![bare(Expression::Assignment {
            target: Box::new(Expression::FieldAccess {
                receiver: Box::new(Expression::Identifier(Identifier::new(
                    "self",
                    Span::new(1, 5),
                ))),
                field: Identifier::new("value", Span::new(6, 11)),
                span: Span::new(1, 11),
            }),
            value: Box::new(Expression::Literal(Literal::Integer(1), Span::new(15, 16))),
            type_annotation: None,
            span: Span::new(1, 16),
        })],
        span: Span::new(0, 17),
    };

    let analysis = crate::codegen::core_erlang::block_analysis::analyze_block(&block);
    let result = CoreErlangGenerator::validate_stored_closure(&analysis, || "test".to_string());
    assert!(result.is_err(), "Field assignment should produce error");

    if let Err(CodeGenError::FieldAssignmentInUnsupportedBlock {
        field,
        field_capitalized,
        ..
    }) = result
    {
        assert_eq!(field, "value");
        assert_eq!(field_capitalized, "Value");
    } else {
        panic!("Expected FieldAssignmentInUnsupportedBlock error");
    }
}

#[test]
fn test_validate_stored_closure_field_takes_precedence_contract() {
    // Block with both field and local assignment
    // Field error should be reported first.
    //
    // This exercises validate_stored_closure's own contract directly. Through
    // the production generate_block path this precedence is never actually
    // observed: generate_block only calls validate_stored_closure when
    // field_writes is non-empty, and the field branch always returns early,
    // so validate_stored_closure only ever sees the local-mutation branch
    // when field_writes is already empty (which can only happen via direct
    // unit-test calls, not through generate_block).
    let block = Block {
        parameters: vec![],
        body: vec![
            bare(Expression::Assignment {
                target: Box::new(Expression::Identifier(Identifier::new(
                    "count",
                    Span::new(1, 6),
                ))),
                value: Box::new(Expression::Literal(Literal::Integer(0), Span::new(10, 11))),
                type_annotation: None,
                span: Span::new(1, 11),
            }),
            bare(Expression::Assignment {
                target: Box::new(Expression::FieldAccess {
                    receiver: Box::new(Expression::Identifier(Identifier::new(
                        "self",
                        Span::new(13, 17),
                    ))),
                    field: Identifier::new("value", Span::new(18, 23)),
                    span: Span::new(13, 23),
                }),
                value: Box::new(Expression::Literal(Literal::Integer(1), Span::new(27, 28))),
                type_annotation: None,
                span: Span::new(13, 28),
            }),
        ],
        span: Span::new(0, 29),
    };

    let analysis = crate::codegen::core_erlang::block_analysis::analyze_block(&block);
    let result = CoreErlangGenerator::validate_stored_closure(&analysis, || "test".to_string());
    assert!(result.is_err());

    // Should be field error (checked first), not local
    assert!(
        matches!(
            result,
            Err(CodeGenError::FieldAssignmentInUnsupportedBlock { .. })
        ),
        "Field error should take precedence over local mutation"
    );
}

#[test]
fn test_codegen_rejects_stored_closure_with_field_assignment() {
    // BT-2792: Integration test covering the full codegen pipeline for
    // test := [ myBlock := [self.value := 1]. myBlock ]
    //
    // This used to be asserted as allowed (BT-852, "supported via Tier 2 stateful
    // block protocol"), but Tier 2 promotion only ever triggers on *captured local*
    // mutations, never on `self.field :=` writes — so this actually produced Core
    // Erlang that `erlc` rejects with "unbound variable" (the inner block's `fun`
    // bumps the shared state-version counter, but that binding is scoped inside the
    // `fun` and never reaches the caller). Must now be a clear compile-time error.
    let module = Module {
        classes: vec![],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        expressions: vec![bare(Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new(
                "test",
                Span::new(0, 4),
            ))),
            value: Box::new(Expression::Block(Block {
                parameters: vec![],
                body: vec![
                    // myBlock := [self.value := self.value + 1]
                    bare(Expression::Assignment {
                        target: Box::new(Expression::Identifier(Identifier::new(
                            "myBlock",
                            Span::new(10, 17),
                        ))),
                        value: Box::new(Expression::Block(Block {
                            parameters: vec![],
                            body: vec![bare(Expression::Assignment {
                                target: Box::new(Expression::FieldAccess {
                                    receiver: Box::new(Expression::Identifier(Identifier::new(
                                        "self",
                                        Span::new(22, 26),
                                    ))),
                                    field: Identifier::new("value", Span::new(27, 32)),
                                    span: Span::new(22, 32),
                                }),
                                value: Box::new(Expression::Literal(
                                    Literal::Integer(1),
                                    Span::new(36, 37),
                                )),
                                type_annotation: None,
                                span: Span::new(22, 37),
                            })],
                            span: Span::new(21, 38),
                        })),
                        type_annotation: None,
                        span: Span::new(10, 38),
                    }),
                    bare(Expression::Identifier(Identifier::new(
                        "myBlock",
                        Span::new(40, 47),
                    ))),
                ],
                span: Span::new(8, 49),
            })),
            type_annotation: None,
            span: Span::new(0, 49),
        })],
        span: Span::new(0, 50),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let result = generate(&module);
    assert!(
        matches!(
            result,
            Err(CodeGenError::FieldAssignmentInUnsupportedBlock { .. })
        ),
        "Field assignment in a stored closure must be a compile-time error, not silently \
         accepted (BT-2792). Got: {result:?}"
    );
}

#[test]
fn test_codegen_allows_stored_closure_with_local_mutation() {
    // Integration test: verify the full codegen pipeline successfully handles local mutations
    // in stored closures via the Tier 2 protocol (i.e., codegen is allowed and succeeds).
    // Build a module with: test := [ count := 0. myBlock := [count := count + 1]. myBlock ]
    let module = Module {
        classes: vec![],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        expressions: vec![bare(Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new(
                "test",
                Span::new(0, 4),
            ))),
            value: Box::new(Expression::Block(Block {
                parameters: vec![],
                body: vec![
                    // count := 0
                    bare(Expression::Assignment {
                        target: Box::new(Expression::Identifier(Identifier::new(
                            "count",
                            Span::new(10, 15),
                        ))),
                        value: Box::new(Expression::Literal(
                            Literal::Integer(0),
                            Span::new(19, 20),
                        )),
                        type_annotation: None,
                        span: Span::new(10, 20),
                    }),
                    // myBlock := [count := count + 1]
                    bare(Expression::Assignment {
                        target: Box::new(Expression::Identifier(Identifier::new(
                            "myBlock",
                            Span::new(22, 29),
                        ))),
                        value: Box::new(Expression::Block(Block {
                            parameters: vec![],
                            body: vec![bare(Expression::Assignment {
                                target: Box::new(Expression::Identifier(Identifier::new(
                                    "count",
                                    Span::new(34, 39),
                                ))),
                                value: Box::new(Expression::MessageSend {
                                    receiver: Box::new(Expression::Identifier(Identifier::new(
                                        "count",
                                        Span::new(43, 48),
                                    ))),
                                    selector: MessageSelector::Binary("+".into()),
                                    arguments: vec![Expression::Literal(
                                        Literal::Integer(1),
                                        Span::new(51, 52),
                                    )],
                                    is_cast: false,
                                    span: Span::new(43, 52),
                                }),
                                type_annotation: None,
                                span: Span::new(34, 52),
                            })],
                            span: Span::new(33, 53),
                        })),
                        type_annotation: None,
                        span: Span::new(22, 53),
                    }),
                    bare(Expression::Identifier(Identifier::new(
                        "myBlock",
                        Span::new(55, 62),
                    ))),
                ],
                span: Span::new(8, 64),
            })),
            type_annotation: None,
            span: Span::new(0, 64),
        })],
        span: Span::new(0, 65),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    // BT-852: Stored closures with local mutations are now allowed via Tier 2 protocol.
    let result = generate(&module);
    assert!(
        result.is_ok(),
        "Local mutation in stored closure should now be allowed via Tier 2 protocol. Got: {result:?}"
    );
}

#[test]
fn test_if_true_with_field_mutation_generates_inline_case() {
    // BT-915: `flag ifTrue: [self.count := self.count + 1]` inside an actor method
    // should compile to an inline case expression that threads state through the
    // true branch, returning {Result, NewState} so the outer method body can
    // update its state chain.
    let src = "Actor subclass: Ctr\n  state: count = 0\n\n  increment: flag =>\n    flag ifTrue: [self.count := self.count + 1].\n    self.count\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("ctr_if_true").with_workspace_mode(true),
    )
    .expect("codegen should succeed");

    eprintln!("Generated code for ifTrue: with field mutation:\n{code}");

    // Should use inline case expression — not beamtalk_message_dispatch:send
    assert!(
        !code.contains("'send'(") || !code.contains("'ifTrue:'"),
        "ifTrue: with mutation should NOT go through runtime dispatch. Got:\n{code}"
    );

    // The conditional should be compiled as an inline case
    assert!(
        code.contains("case "),
        "Should generate inline case expression. Got:\n{code}"
    );

    // The true branch should contain maps:put for 'count'
    assert!(
        code.contains("maps':'put'('count'"),
        "True branch should update 'count' via maps:put. Got:\n{code}"
    );

    // StateAcc should be used inside the branch (loop-body naming to avoid conflicts)
    assert!(
        code.contains("StateAcc"),
        "Branch body should use StateAcc naming. Got:\n{code}"
    );

    // The outer state (State1) should be extracted from the result tuple via element/2
    assert!(
        code.contains("'erlang':'element'(2,"),
        "Outer method body should extract NewState via element(2, ...). Got:\n{code}"
    );
}

#[test]
fn test_if_false_with_field_mutation_generates_inline_case() {
    // BT-915: `flag ifFalse: [self.count := self.count - 1]` should compile to
    // an inline case expression with state threading in the false branch.
    let src = "Actor subclass: Ctr\n  state: count = 0\n\n  decrement: flag =>\n    flag ifFalse: [self.count := self.count - 1].\n    self.count\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("ctr_if_false").with_workspace_mode(true),
    )
    .expect("codegen should succeed");

    eprintln!("Generated code for ifFalse: with field mutation:\n{code}");

    assert!(
        code.contains("case "),
        "Should generate inline case expression. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'put'('count'"),
        "False branch should update 'count' via maps:put. Got:\n{code}"
    );
    assert!(
        code.contains("StateAcc"),
        "Branch body should use StateAcc naming. Got:\n{code}"
    );
}

#[test]
fn test_if_true_if_false_with_field_mutation_generates_inline_case() {
    // BT-915: `flag ifTrue: [...] ifFalse: [...]` with field mutations in both
    // branches should compile to an inline case with two state-threading branches.
    let src = "Actor subclass: Ctr\n  state: count = 0\n\n  toggle: flag =>\n    flag ifTrue: [self.count := self.count + 10]\n         ifFalse: [self.count := self.count - 1].\n    self.count\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("ctr_if_true_if_false").with_workspace_mode(true),
    )
    .expect("codegen should succeed");

    eprintln!("Generated code for ifTrue:ifFalse: with field mutations:\n{code}");

    assert!(
        code.contains("case "),
        "Should generate inline case expression. Got:\n{code}"
    );
    // Both branches should mutate count
    assert!(
        code.contains("maps':'put'('count'"),
        "Should update 'count' in a branch via maps:put. Got:\n{code}"
    );
    // Both branches return {Result, StateAccN} tuples
    assert!(
        code.contains("StateAcc"),
        "Both branches should use StateAcc naming. Got:\n{code}"
    );
    // Outer method body extracts NewState
    assert!(
        code.contains("'erlang':'element'(2,"),
        "Outer method body should extract NewState via element(2, ...). Got:\n{code}"
    );
}

#[test]
fn test_if_true_without_mutation_uses_runtime_dispatch() {
    // BT-915: `flag ifTrue: [42]` with no mutations should still use runtime dispatch,
    // not the inline case generation. This ensures we don't break the non-mutation path.
    let src = "Actor subclass: Ctr\n  state: x = 0\n\n  check: flag =>\n    flag ifTrue: [42]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("ctr_no_mutation").with_workspace_mode(true),
    )
    .expect("codegen should succeed");

    eprintln!("Generated code for ifTrue: without mutation:\n{code}");

    // Pure ifTrue: should go through runtime dispatch
    assert!(
        code.contains("'beamtalk_message_dispatch':'send'"),
        "Pure ifTrue: should use runtime dispatch. Got:\n{code}"
    );
}

#[test]
fn test_nested_if_true_with_field_mutation_threads_state() {
    // BT-915: Nested `flag1 ifTrue: [flag2 ifTrue: [self.count := ...]]` should
    // correctly unpack the inner {Result, State} tuple so the outer branch
    // threads state from the inner conditional.
    let src = "Actor subclass: Ctr\n  state: count = 0\n\n  nested: a and: b =>\n    a ifTrue: [\n      b ifTrue: [self.count := self.count + 100]\n    ].\n    self.count\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("ctr_nested").with_workspace_mode(true),
    )
    .expect("codegen should succeed");

    eprintln!("Generated code for nested ifTrue: with field mutation:\n{code}");

    // Should generate inline case expressions (at least two)
    let case_count = code.matches("case ").count();
    assert!(
        case_count >= 2,
        "Should generate at least 2 inline case expressions for nested conditionals. Found {case_count}. Got:\n{code}"
    );

    // Should use maps:put for count
    assert!(
        code.contains("maps':'put'('count'"),
        "Inner branch should update 'count' via maps:put. Got:\n{code}"
    );
}

// BT-1296: match: arms with array and map patterns

#[test]
fn test_match_array_pattern_arm_generates_is_map_guard_chain() {
    // BT-1296: `arr match: [#[h, t] -> h + t; _ -> 0]` should compile to a
    // conditional chain using is_map + maps:get('$beamtalk_class', ..., 'undefined') + size check.
    let src = "Object subclass: Foo\n  test: arr =>\n    arr match: [\n      #[h, t] -> h + t;\n      _ -> 0\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    eprintln!("Generated code for array pattern match:\n{code}");

    assert!(
        code.contains("call 'erlang':'is_map'("),
        "Should emit is_map guard check for array pattern. Got:\n{code}"
    );
    assert!(
        code.contains("'$beamtalk_class'"),
        "Should check '$beamtalk_class' key for array type guard. Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_array':'size'("),
        "Should check array size. Got:\n{code}"
    );
    assert!(
        code.contains("'at:'"),
        "Should extract elements via at: dispatch. Got:\n{code}"
    );
}

#[test]
fn test_match_map_pattern_arm_generates_core_erlang_map_pattern() {
    // BT-1296: `d match: [#{#event => evName} -> evName; _ -> "none"]`
    // should compile to a native Core Erlang map pattern `~{'event' := EvName}~`.
    let src = "Object subclass: Foo\n  test: d =>\n    d match: [\n      #{#event => evName} -> evName;\n      _ -> \"none\"\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    eprintln!("Generated code for map pattern match:\n{code}");

    assert!(
        code.contains("~{"),
        "Should emit Core Erlang map pattern ~{{...}}~. Got:\n{code}"
    );
    assert!(
        code.contains(":="),
        "Should use := binding syntax in map pattern. Got:\n{code}"
    );
    assert!(
        code.contains("'event'"),
        "Should include the 'event' key in the map pattern. Got:\n{code}"
    );
}

#[test]
fn test_match_array_pattern_fallthrough_to_wildcard() {
    // BT-1296: When the array pattern fails (wrong type/size), execution must
    // fall through to the next arm — not crash.
    // Wildcard fallback arm should be present in the generated code.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      #[a, b] -> a + b;\n      _ -> 42\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    // The fallback (42) should appear in the generated code
    assert!(
        code.contains("42"),
        "Fallback value should appear in generated code. Got:\n{code}"
    );
    // The is_map check should be present — if false, falls through to 42
    assert!(
        code.contains("call 'erlang':'is_map'("),
        "Should emit is_map check. Got:\n{code}"
    );
}

#[test]
fn test_match_nested_array_pattern_arm() {
    // BT-1296: `arr match: [#[#[a, b], c] -> a+b+c; _ -> 0]`
    // should generate nested is_map + size checks for the inner array.
    let src = "Object subclass: Foo\n  test: arr =>\n    arr match: [\n      #[#[a, b], c] -> a + b + c;\n      _ -> 0\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    eprintln!("Generated code for nested array pattern match:\n{code}");

    // Should have multiple is_map checks (outer + inner array)
    let is_map_count = code.matches("call 'erlang':'is_map'(").count();
    assert!(
        is_map_count >= 2,
        "Should emit at least 2 is_map checks for nested array. Found {is_map_count}. Got:\n{code}"
    );
    // Should have multiple beamtalk_array:size calls (outer + inner)
    let size_count = code.matches("call 'beamtalk_array':'size'(").count();
    assert!(
        size_count >= 2,
        "Should emit at least 2 size checks for nested array. Found {size_count}. Got:\n{code}"
    );
}

#[test]
fn test_match_array_pattern_uses_maps_get_with_default_not_map_get() {
    // BT-1296: The class-tag lookup must use maps:get/3 with a default value so
    // that a plain Erlang map (Beamtalk Dictionary) as the match subject does not
    // crash with {badkey, '$beamtalk_class'} — it should fall through instead.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      #[a, b] -> a + b;\n      _ -> 0\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    // Verify the 3-arg form maps:get/3 is used — the key, the map, AND the default 'undefined'.
    // This prevents regression to maps:get/2 or erlang:map_get/2 (both throw on missing key).
    assert!(
        code.contains("call 'maps':'get'('$beamtalk_class',") && code.contains(", 'undefined')"),
        "Should use maps:get/3 with 'undefined' default. Got:\n{code}"
    );
    assert!(
        !code.contains("call 'erlang':'map_get'("),
        "Must NOT use erlang:map_get/2 — throws badkey when '$beamtalk_class' absent. Got:\n{code}"
    );
}

#[test]
fn test_match_array_pattern_duplicate_variable_emits_equality_check() {
    // BT-1315: `arr match: [#[x, x] -> "equal"; _ -> "differ"]`
    // The second occurrence of `x` must emit an `erlang:=:=` equality check
    // rather than a bare re-binding.
    let src = "Object subclass: Foo\n  test: arr =>\n    arr match: [\n      #[x, x] -> \"equal\";\n      _ -> \"differ\"\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    eprintln!("Generated code for duplicate-variable array pattern:\n{code}");

    // Must emit a strict-equality call for the duplicate variable.
    assert!(
        code.contains("call 'erlang':'=:='("),
        "Should emit erlang:=:= equality check for duplicate variable. Got:\n{code}"
    );
    // The primary binding `let X = ... at: [1]` should appear.
    assert!(
        code.contains("'at:', [1]"),
        "Should extract first element for primary binding. Got:\n{code}"
    );
    // A second extraction for position 2 must also be present (into a temp var).
    assert!(
        code.contains("'at:', [2]"),
        "Should extract second element into temp for equality check. Got:\n{code}"
    );
}

// BT-2854 / ADR 0107 Phase A: `Pattern::Nil` and `Pattern::Type` codegen

#[test]
fn test_match_nil_pattern_compiles_to_atom_literal() {
    // ADR 0107 Phase A: `nil` pattern reuses the existing atom-literal
    // codegen path verbatim — no new runtime mechanism.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      nil -> 0;\n      _ -> 1\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    eprintln!("Generated code for nil pattern match:\n{code}");

    assert!(
        code.contains("<'nil'>"),
        "nil pattern should compile to the atom-literal case pattern 'nil'. Got:\n{code}"
    );
}

// BT-2855 / ADR 0107 Phase A: `Pattern::Type` codegen (`generate_type_pattern`)

#[test]
fn test_match_type_pattern_string_compiles_to_is_binary_test() {
    // `s :: String` compiles to a guard-safe `is_binary` case test — the
    // single-level generalization of `generate_array_match_arm`'s `is_map`
    // check to an arbitrary primitive BIF.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      s :: String -> s;\n      _ -> \"none\"\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    eprintln!("Generated code for String type pattern match:\n{code}");

    assert!(
        code.contains("'erlang':'is_binary'"),
        "String type pattern should test is_binary. Got:\n{code}"
    );
}

#[test]
fn test_match_type_pattern_tagged_class_compiles_to_map_tag_check() {
    // An exact tagged `Value`/sealed class reuses the `'$beamtalk_class'`
    // map-key check `generate_constructor_pattern` already uses for
    // `Result`, generalized to the pattern's `class` field. `generate_module`
    // compiles a single class per call, so the pattern's target class (`Bar`)
    // and the method under test live on the same class.
    let src = "Value subclass: Bar\n  field: n = 0\n\n  test: x =>\n    x match: [\n      b :: Bar -> b;\n      _ -> \"none\"\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("bar")).expect("codegen should succeed");

    eprintln!("Generated code for tagged-class type pattern match:\n{code}");

    assert!(
        code.contains("'erlang':'is_map'"),
        "Tagged-class type pattern should test is_map first. Got:\n{code}"
    );
    assert!(
        code.contains("'maps':'get'('$beamtalk_class'"),
        "Tagged-class type pattern should check '$beamtalk_class'. Got:\n{code}"
    );
    assert!(
        code.contains("'Bar'"),
        "Tagged-class type pattern should match the class name atom. Got:\n{code}"
    );
}

#[test]
fn test_match_type_pattern_mixed_with_nil_and_native_arms_compiles() {
    // A `match:` mixing `nil`, a primitive `Type` arm, and a wildcard must
    // still compile into one dispatch chain (verifies the
    // dispatch/interleaving layer, not just each strategy in isolation).
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      nil -> 0;\n      n :: Integer -> n;\n      _ -> -1\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    eprintln!("Generated code for mixed nil/type/wildcard match:\n{code}");

    assert!(
        code.contains("<'nil'>"),
        "Should still contain the nil arm. Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'is_integer'"),
        "Should still contain the Integer arm's test. Got:\n{code}"
    );
}

#[test]
fn test_match_type_pattern_block_compiles_to_is_function_test() {
    // A Beamtalk block compiles to a plain Erlang `fun`, never a map — it
    // needs its own guard-safe BIF entry rather than falling into the
    // tagged-class `is_map` path (which would never match a `fun`).
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      blk :: Block -> blk;\n      _ -> \"none\"\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    eprintln!("Generated code for Block type pattern match:\n{code}");

    assert!(
        code.contains("'erlang':'is_function'"),
        "Block type pattern should test is_function. Got:\n{code}"
    );
}

#[test]
fn test_match_type_pattern_true_false_nil_use_exact_atom_match() {
    // `True`/`False` (real sealed `Boolean` subclasses) and
    // `Nil`/`UndefinedObject` (the nil class and its legacy alias) all
    // compile to a bare atom, never a map — each needs an exact
    // single-atom test rather than the tagged-class `is_map` check (which
    // would never match a plain atom).
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      t :: True -> 1;\n      f :: False -> 2;\n      n :: Nil -> 3;\n      u :: UndefinedObject -> 4;\n      _ -> 0\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    eprintln!("Generated code for True/False/Nil/UndefinedObject type patterns:\n{code}");

    assert!(
        !code.contains("is_map"),
        "None of True/False/Nil/UndefinedObject should use the tagged-class is_map check. Got:\n{code}"
    );
    assert!(
        code.matches("<'true'>").count() >= 1,
        "Should contain an exact 'true' match for the True arm. Got:\n{code}"
    );
    assert!(
        code.matches("<'false'>").count() >= 1,
        "Should contain an exact 'false' match for the False arm. Got:\n{code}"
    );
    assert!(
        code.matches("<'nil'>").count() >= 2,
        "Should contain an exact 'nil' match for both the Nil and UndefinedObject arms. Got:\n{code}"
    );
}

#[test]
fn test_match_type_pattern_actor_class_uses_tuple_tag_check_and_compiles() {
    // BT-2855: an actor reference is `{'beamtalk_object', ClassAtom,
    // ModuleAtom, Pid}` — a 4-tuple, not a map. Naively generalizing the
    // tagged-class `is_map` check to Actor subclasses would silently never
    // match a live actor instance. Verifies both the generated shape and
    // that it actually compiles through erlc (not just pretty-prints).
    let src = "Actor subclass: Counter\n  state: count = 0\n\n  test: x =>\n    x match: [\n      c :: Counter -> c;\n      _ -> \"none\"\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("counter")).expect("codegen should succeed");

    eprintln!("Generated code for actor type pattern match:\n{code}");

    assert!(
        code.contains("'erlang':'is_tuple'"),
        "Actor-class type pattern should test is_tuple. Got:\n{code}"
    );
    assert!(
        code.contains("'beamtalk_object'"),
        "Actor-class type pattern should check the 'beamtalk_object' tag. Got:\n{code}"
    );
    assert!(
        code.contains("'Counter'"),
        "Actor-class type pattern should match the class name atom. Got:\n{code}"
    );

    crate::test_helpers::assert_compiles_through_erlc("counter", &code);
}

#[test]
fn test_match_type_pattern_pid_reference_port_use_guard_safe_bifs() {
    // `Pid`/`Reference`/`Port` are raw BEAM terms (never maps) — each needs
    // its own guard-safe BIF entry rather than the tagged-class `is_map`
    // check (which would never match a bare pid/ref/port).
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      p :: Pid -> 1;\n      r :: Reference -> 2;\n      pt :: Port -> 3;\n      _ -> 0\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    eprintln!("Generated code for Pid/Reference/Port type patterns:\n{code}");

    assert!(
        code.contains("'erlang':'is_pid'"),
        "Pid type pattern should test is_pid. Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'is_reference'"),
        "Reference type pattern should test is_reference. Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'is_port'"),
        "Port type pattern should test is_port. Got:\n{code}"
    );
}

#[test]
fn test_match_type_pattern_tuple_excludes_actor_reference_shape() {
    // BT-2855: an actor reference is *also* a plain Erlang tuple
    // structurally (`{'beamtalk_object', ClassAtom, ModuleAtom, Pid}`), so
    // `x :: Tuple` must explicitly exclude the reserved actor/supervisor
    // tags or it would incorrectly match a live actor reference too.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      t :: Tuple -> 1;\n      _ -> 0\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    eprintln!("Generated code for Tuple type pattern:\n{code}");

    assert!(
        code.contains("'erlang':'is_tuple'"),
        "Tuple type pattern should test is_tuple. Got:\n{code}"
    );
    assert!(
        code.contains("'beamtalk_object'"),
        "Tuple type pattern should exclude the actor-reference tag. Got:\n{code}"
    );
    assert!(
        code.contains("'beamtalk_supervisor'"),
        "Tuple type pattern should exclude the supervisor-reference tag. Got:\n{code}"
    );
}

#[test]
fn test_match_type_pattern_supervisor_subclass_is_compile_error() {
    // Supervisor references use a third runtime shape
    // (`{'beamtalk_supervisor' | 'beamtalk_supervisor_new', ...}`) distinct
    // from both the tagged-map and actor-tuple strategies — rejected
    // explicitly (fail loudly) rather than risk silently-wrong codegen.
    let src = "Supervisor subclass: WebApp\n\nObject subclass: Foo\n  test: x =>\n    x match: [\n      s :: WebApp -> s;\n      _ -> \"none\"\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, diags) = crate::source_analysis::parse(tokens);
    assert!(diags.is_empty(), "Parse failed: {diags:?}");

    let (hierarchy_result, _) =
        crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module);
    let hierarchy = hierarchy_result.expect("hierarchy build should succeed");
    let mut diagnostics = Vec::new();
    crate::semantic_analysis::validators::check_type_pattern_classes(
        &module,
        &hierarchy,
        false,
        &mut diagnostics,
    );

    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("WebApp") && d.message.contains("Supervisor")),
        "Expected a Supervisor-subclass type-pattern error, got: {diagnostics:?}"
    );
}

#[test]
fn test_match_type_pattern_dictionary_and_tagged_class_arms_interleave_in_one_case() {
    // `Dictionary` and an exact tagged class both use an `is_map`-based
    // strategy; a `match:` mixing the two must still compile into one
    // dispatch chain, not collide on temp-var names or short-circuit past
    // the second arm.
    let src = "Value subclass: Bar\n  field: n = 0\n\n  test: x =>\n    x match: [\n      d :: Dictionary -> d;\n      b :: Bar -> b;\n      _ -> \"none\"\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("bar")).expect("codegen should succeed");

    eprintln!("Generated code for Dictionary + tagged-class match:\n{code}");

    assert!(
        code.matches("'erlang':'is_map'").count() >= 2,
        "Should contain at least two independent is_map checks (Dictionary arm + Bar arm; \
         other class-boilerplate `is_map` checks may also appear). Got:\n{code}"
    );
    assert!(
        code.contains("'undefined'"),
        "Should still contain the Dictionary arm's 'undefined' tag check. Got:\n{code}"
    );
    assert!(
        code.contains("'Bar'"),
        "Should still contain the Bar arm's class-tag check. Got:\n{code}"
    );
}

#[test]
fn test_match_type_pattern_nested_in_tuple_pattern_is_codegen_error() {
    // A `Pattern::Type` nested inside a composite pattern (here a `Tuple`)
    // has no codegen path — `generate_type_pattern` only handles a
    // top-level arm pattern, since its per-class runtime test needs to wrap
    // the whole arm. Verifies this is a clean, reported codegen error, not
    // a panic or silently-wrong output.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      {s :: String, n} -> n;\n      _ -> 0\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let err = generate_module(&module, CodegenOptions::new("foo"))
        .expect_err("nested Pattern::Type should be a codegen error, not silently accepted");
    let message = err.to_string();
    assert!(
        message.contains("Type pattern") && message.contains("nested"),
        "error should describe the nested Type pattern restriction. Got: {message}"
    );
}

#[test]
fn test_match_arm_self_mutating_value_block_threads_actor_state() {
    // BT-2880: a `match:` arm body that is a multi-statement `[...] value`
    // block mutating `self.<state>` must thread actor state correctly and
    // unwrap to the block's real value — not leak the internal `{Value,
    // NewState}` state-threading tuple as the match's result. Mixes a
    // native `nil ->` arm (mutating) with a `Pattern::Type` arm (`x ::
    // Integer -> x`, non-mutating) so this exercises `generate_match_chain`,
    // the exact path the original bug report's repro takes.
    let src = "Actor subclass: Registry\n  state: count :: Integer\n\n  initialize -> Nil =>\n    self.count := 0\n    nil\n\n  bumpMatch -> Integer =>\n    nil match: [\n      nil -> [\n        self.count := self.count + 1\n        self.count\n      ] value;\n      x :: Integer -> x\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("registry")).expect("codegen should succeed");

    eprintln!("Generated code for match: arm self-mutating value block:\n{code}");

    assert!(
        code.contains("'maps':'put'('count'"),
        "Self-mutating match: arm must thread state via maps:put. Got:\n{code}"
    );
    // The bug produced a raw {Value, NewState} tuple as the whole match:'s
    // result instead of unwrapping it — the generated `handle_call` clause
    // for `bumpMatch` must return `{'reply', <ResultVar>, <StateVar>}` (two
    // bare variables), not a doubly-nested tuple wrapping the match's own
    // {Value, NewState} tuple as the reply value.
    let bump_match_clause = code
        .split("<'bumpMatch'>")
        .nth(1)
        .expect("handle_call must have a bumpMatch clause")
        .split("<OtherSelector>")
        .next()
        .expect("bumpMatch clause must be followed by the OtherSelector fallback");
    assert!(
        bump_match_clause.contains("{'reply', ") && !bump_match_clause.contains("{'reply', {"),
        "bumpMatch's state-threading tuple must be unwrapped into plain \
         Result/State vars before the gen_server reply, not leaked as the \
         reply value itself. Got clause:\n{bump_match_clause}"
    );

    crate::test_helpers::assert_compiles_through_erlc("registry", &code);
}

#[test]
fn test_match_two_adjacent_mutating_arms_each_thread_from_the_same_base_state() {
    // BT-2880 review follow-up: two DIFFERENT arms in the same match: each
    // mutate a different field via a `[...] value` block. Since only one arm
    // fires at runtime, each arm's `with_branch_context` call must reset to
    // the SAME pre-match base_state — arm 2's compilation must not see any
    // state_version advancement leaked from arm 1's compilation (they are
    // alternatives, not a sequence). Both arms use `Pattern::Literal`
    // (Integer), a genuine native case-literal match (unlike bare `true`/
    // `false`, which the parser treats as `Pattern::Variable` — an
    // unconditional catch-all binding, not a boolean literal test; only
    // `nil` is a reserved pattern keyword per ADR 0107), so this also
    // re-confirms the all-native fast path handles more than one mutating
    // arm.
    let src = "Actor subclass: TwoMutatingArms\n  state: a :: Integer = 0\n  state: b :: Integer = 0\n\n  run: choice -> Integer =>\n    choice match: [\n      1 -> [self.a := self.a + 1. self.a] value;\n      2 -> [self.b := self.b + 1. self.b] value\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("two_mutating_arms"))
        .expect("codegen should succeed");

    eprintln!("Generated code for match: with two adjacent mutating arms:\n{code}");

    assert!(
        code.contains("'maps':'put'('a'") && code.contains("'maps':'put'('b'"),
        "both arms must thread their own field's mutation via maps:put. Got:\n{code}"
    );
    let run_clause = code
        .split("<'run:'>")
        .nth(1)
        .expect("handle_call must have a run: clause")
        .split("<OtherSelector>")
        .next()
        .expect("run: clause must be followed by the OtherSelector fallback");
    assert!(
        run_clause.contains("'erlang':'element'(2, "),
        "run:'s match: result must be unpacked via erlang:element/2 before \
         the gen_server reply, regardless of which arm fired. Got clause:\n{run_clause}"
    );

    crate::test_helpers::assert_compiles_through_erlc("two_mutating_arms", &code);
}

#[test]
fn test_match_arm_self_field_held_block_value_call_compiles_without_double_wrapping() {
    // BT-2880 / BT-2814: a match: arm body that invokes a *dynamically held*
    // Tier 2 block via `self.<field> value` (not a `[...] value` block
    // literal) is a different receiver shape than the one this fix inlines.
    // `is_tier2_value_call` still classifies it as needing threading (so
    // `match_needs_mutation_threading` returns true for the whole match:),
    // but `generate_match_arm_body`'s literal-block branch doesn't match a
    // `self.field` receiver, so it falls to `expression_doc`, which already
    // unwraps+discards that call's own NewState via
    // `close_tier2_value_subexpr_doc` (the same BT-2814 sub-expression-
    // position limitation `test_bt2814_field_stored_tier2_value_call_in_argument_position_unpacks_result`
    // pins elsewhere — the held block's mutation is not threaded forward,
    // by design, both before and after this fix). This test only pins that
    // the combination compiles cleanly through erlc and does not
    // double-wrap the already-unwrapped value in an extra tuple layer.
    let src = "Actor subclass: Ctr\n  state: total = 0\n  state: onTick = nil\n\n  setup => self.onTick := [:x | self.total := self.total + x]\n\n  run: n =>\n    n match: [\n      x :: Integer -> self.onTick value: x;\n      _ -> 0\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("ctr")).expect("codegen should succeed");

    eprintln!("Generated code for match: arm self.field-held block value call:\n{code}");

    // The arm's own value: call must be unwrapped exactly once (BT-2814's
    // close_tier2_value_subexpr_doc), not left as a raw tuple nor wrapped a
    // second time by generate_match_arm_body's plain-arm fallback.
    assert!(
        code.contains("'erlang':'element'(1, "),
        "self.onTick value: x must still go through the BT-2814 single-unwrap \
         path inside the match: arm. Got:\n{code}"
    );

    crate::test_helpers::assert_compiles_through_erlc("ctr", &code);
}

#[test]
fn test_match_arm_cascade_tier2_value_call_compiles_without_double_wrapping() {
    // BT-2880 review follow-up: a match: arm body that is a `Cascade`
    // (`blk value: a; value: b`, BT-2808) on a Tier 2 local var is a third
    // `is_tier2_value_call` receiver shape besides the block-literal one
    // this fix inlines and the self.field-held one the sibling test above
    // pins. `generate_match_arm_body`'s literal-block branch only matches an
    // `Expression::MessageSend` with an `Expression::Block` receiver, so a
    // `Cascade` body falls through to `expression_doc`, which (per BT-2808)
    // already threads state correctly across BOTH cascaded sends and
    // extracts element(1) as its own value. Only the wrapping matters here:
    // this pins that the plain-arm `{value, base_state}` fallback does not
    // double-wrap that already-unwrapped value.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: item -> Integer =>\n    blk := [:x | self.total := self.total + x]\n    nil match: [\n      nil -> (blk value: item; value: item);\n      x :: Integer -> x\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("ctr")).expect("codegen should succeed");

    eprintln!("Generated code for match: arm cascade tier2 value: call:\n{code}");

    assert!(
        code.contains("'maps':'put'('total'"),
        "the cascade's block mutation must still thread state via maps:put. Got:\n{code}"
    );

    crate::test_helpers::assert_compiles_through_erlc("ctr", &code);
}

#[test]
fn test_match_arm_nested_if_true_mutation_without_value_wrapper_threads_actor_state() {
    // BT-2880 (generalization): the same bug also reproduces when an arm
    // body is itself a nested control-flow-with-mutations construct — here
    // `flag ifTrue: [self.count := ...]` — directly, with no `[...] value`
    // wrapper around it. `ifTrue:`/`ifFalse:` already compile such a body to
    // a correctly state-threaded `{Value, NewState}` tuple on their own; the
    // bug was that `match:` didn't know to route that tuple through the
    // gen_server reply's unwrap machinery, since `Expression::Match` was
    // invisible to `control_flow_has_mutations`.
    let src = "Actor subclass: Registry\n  state: count :: Integer\n\n  initialize -> Nil =>\n    self.count := 0\n    nil\n\n  bumpMatch: flag -> Integer =>\n    nil match: [\n      nil -> flag ifTrue: [self.count := self.count + 1] ifFalse: [self.count := self.count - 1];\n      x :: Integer -> x\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("registry")).expect("codegen should succeed");

    eprintln!("Generated code for match: arm nested ifTrue:/ifFalse: mutation:\n{code}");

    assert!(
        code.contains("'maps':'put'('count'"),
        "Nested ifTrue:/ifFalse: mutation inside a match: arm must thread state via maps:put. Got:\n{code}"
    );
    let bump_match_clause = code
        .split("<'bumpMatch:'>")
        .nth(1)
        .expect("handle_call must have a bumpMatch: clause")
        .split("<OtherSelector>")
        .next()
        .expect("bumpMatch: clause must be followed by the OtherSelector fallback");
    // The bug leaked the match:'s raw {Value, NewState} tuple as the reply
    // itself (`{'reply', {_Val, _State}, State}`, a tuple nested inside the
    // reply). The fix unwraps it via `erlang:element/2` first, so the reply
    // wraps two bare variables. (Ignore the unrelated `bad_arity` fallback
    // clause, which legitimately contains a literal `{'reply', {'error', ...`.)
    assert!(
        bump_match_clause.contains("'erlang':'element'(2, "),
        "bumpMatch:'s match: result must be unpacked via erlang:element/2 \
         before the gen_server reply. Got clause:\n{bump_match_clause}"
    );
    let reply_re =
        regex::Regex::new(r"\{'reply', (_?[A-Za-z][A-Za-z0-9_]*), (_?[A-Za-z][A-Za-z0-9_]*)\}")
            .unwrap();
    assert!(
        reply_re.is_match(bump_match_clause),
        "bumpMatch:'s state-threading tuple must be unwrapped into plain \
         Result/State vars before the gen_server reply, not leaked as the \
         reply value itself (e.g. `{{'reply', {{Val, State}}, State}}`). Got clause:\n{bump_match_clause}"
    );

    crate::test_helpers::assert_compiles_through_erlc("registry", &code);
}

#[test]
fn test_match_arm_self_mutating_value_block_threads_actor_state_all_native_fast_path() {
    // BT-2880: same bug, but with every arm using a native Core Erlang
    // pattern (`nil` and `_`, no `Pattern::Type`/`Pattern::Array`) — this
    // routes through `generate_match`'s flat all-native fast path (the
    // single `case` built directly in `generate_match`) rather than the
    // recursive `generate_match_chain`, which the other BT-2880 regression
    // tests exercise via their `x :: Integer` arm. Both arm-body compile
    // call sites needed the same fix.
    let src = "Actor subclass: Registry\n  state: count :: Integer\n\n  initialize -> Nil =>\n    self.count := 0\n    nil\n\n  bumpMatch -> Integer =>\n    nil match: [\n      nil -> [\n        self.count := self.count + 1\n        self.count\n      ] value;\n      _ -> -1\n    ]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("registry")).expect("codegen should succeed");

    eprintln!("Generated code for match: arm self-mutating value block (fast path):\n{code}");

    assert!(
        code.contains("'maps':'put'('count'"),
        "Self-mutating match: arm must thread state via maps:put. Got:\n{code}"
    );
    let bump_match_clause = code
        .split("<'bumpMatch'>")
        .nth(1)
        .expect("handle_call must have a bumpMatch clause")
        .split("<OtherSelector>")
        .next()
        .expect("bumpMatch clause must be followed by the OtherSelector fallback");
    assert!(
        bump_match_clause.contains("'erlang':'element'(2, "),
        "bumpMatch's match: result must be unpacked via erlang:element/2 \
         before the gen_server reply. Got clause:\n{bump_match_clause}"
    );

    crate::test_helpers::assert_compiles_through_erlc("registry", &code);
}

// BT-2359: value-type outer-local threading for count:/detect: predicates and
// threading constructs used as a (parenthesized) assignment RHS.

#[test]
fn test_vt_count_predicate_threads_outer_local() {
    // BT-2359: `count:` whose predicate read+writes a captured outer local must
    // thread the mutation back so the post-`count:` read sees the final value.
    let src = "Value subclass: V\n  state: dummy = 0\n\n  run =>\n    n := 0\n    #(1, 2, 3) count: [:i | n := n + 1. i > 0]\n    n\n";
    let code = codegen(src);
    eprintln!("Generated code for vt count: threading:\n{code}");
    // The count: foldl packs the threaded local into the StateAcc; the open
    // extraction must read it back via maps:get('__local__n', ...).
    assert!(
        code.contains("'__local__n'"),
        "count: must thread the captured outer local 'n' via the StateAcc. Got:\n{code}"
    );
    assert!(
        code.contains("'lists':'foldl'"),
        "count: with a mutating predicate must compile to a stateful lists:foldl. Got:\n{code}"
    );
}

#[test]
fn test_vt_detect_if_none_predicate_threads_outer_local() {
    // BT-2359: `detect:ifNone:` whose predicate read+writes a captured outer
    // local must thread the mutation back.
    let src = "Value subclass: V\n  state: dummy = 0\n\n  run =>\n    n := 0\n    #(1, 2, 3) detect: [:i | n := n + 1. i > 10] ifNone: [-1]\n    n\n";
    let code = codegen(src);
    eprintln!("Generated code for vt detect:ifNone: threading:\n{code}");
    assert!(
        code.contains("'__local__n'"),
        "detect:ifNone: must thread the captured outer local 'n' via the StateAcc. Got:\n{code}"
    );
}

#[test]
fn test_vt_loop_as_parenthesized_assign_rhs_threads_sibling_local() {
    // BT-2359: a counted loop used as a parenthesized RHS — `_r := (1 to: 5 do:
    // [...])` — must thread its sibling outer local (`sum`) into method scope.
    let src = "Value subclass: V\n  state: dummy = 0\n\n  run =>\n    sum := 0\n    _r := (1 to: 5 do: [:i | sum := sum + i])\n    sum\n";
    let code = codegen(src);
    eprintln!("Generated code for vt parenthesized loop assign-RHS:\n{code}");
    // The loop packs sum into the StateAcc; the assignment must extract it back
    // (element 2 of the {value, StateAcc} tuple → maps:get('__local__sum', ...)).
    assert!(
        code.contains("'__local__sum'"),
        "Parenthesized loop assign-RHS must thread sibling local 'sum'. Got:\n{code}"
    );
}

#[test]
fn test_vt_conditional_as_assign_rhs_threads_sibling_local() {
    // BT-2359: a threading conditional as an assignment RHS — `_r := flag ifTrue:
    // [x := 5. 42] ifFalse: [0]` — must bind the target to the branch's logical
    // value AND thread the sibling local (`x`) into method scope.
    let src = "Value subclass: V\n  state: dummy = 0\n\n  run: flag =>\n    x := 0\n    _r := flag ifTrue: [x := 5. 42] ifFalse: [0]\n    x\n";
    let code = codegen(src);
    eprintln!("Generated code for vt conditional assign-RHS:\n{code}");
    // Each branch returns {LogicalValue, X}; the assignment binds the target to
    // element 1 and rebinds x from element 2 of the case result.
    assert!(
        code.contains("'erlang':'element'(1,") && code.contains("'erlang':'element'(2,"),
        "Conditional assign-RHS must extract logical value (element 1) and sibling local (element 2). Got:\n{code}"
    );
    assert!(
        code.contains("case "),
        "Conditional assign-RHS must compile to an inline case. Got:\n{code}"
    );
}

#[test]
fn test_vt_nested_loop_in_conditional_assign_rhs_threads_local() {
    // BT-2359 (CodeRabbit follow-up): a threaded loop *nested* inside an
    // assign-RHS conditional branch must still rebind its outer local, so a
    // later read in the same branch (and after) sees the update.
    let src = "Value subclass: V\n  state: dummy = 0\n\n  run: flag =>\n    sum := 0\n    _r := flag ifTrue: [1 to: 5 do: [:i | sum := sum + i]. sum] ifFalse: [0]\n    sum\n";
    let code = codegen(src);
    eprintln!("Generated code for nested loop in conditional assign-RHS:\n{code}");
    // The nested loop packs sum into its StateAcc; the branch must extract it
    // back via maps:get('__local__sum', ...).
    assert!(
        code.contains("'__local__sum'"),
        "Nested loop inside a conditional assign-RHS branch must thread 'sum'. Got:\n{code}"
    );
}

#[test]
fn test_actor_conditional_last_expr_lower_actor_threaded_last() {
    // BT-2378: When the LAST expression of an Actor method is a conditional with
    // field mutations, `lower_actor_threaded_last` must bind element 1 of the
    // {Value, NewState} tuple as the reply value and element 2 as the new
    // gen_server State, then emit {'reply', ReplyValue, NewState}.
    //
    // All prior Actor conditional-mutation tests read `self.count` AFTER the
    // conditional, so the conditional was never in last position. This exercises
    // the `lower_actor_threaded_last` path that was completely uncovered.
    let src = "Actor subclass: Ctr\n  state: count = 0\n\n  setByFlag: flag =>\n    flag ifTrue: [self.count := 1] ifFalse: [self.count := -1]\n";
    let code = codegen(src);
    eprintln!("Generated code for actor conditional-as-last:\n{code}");

    // lower_actor_threaded_last binds element 1 (reply value) and element 2 (State).
    assert!(
        code.contains("'erlang':'element'(1,"),
        "Last-position conditional must extract reply value via element(1, ...). Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'element'(2,"),
        "Last-position conditional must extract new State via element(2, ...). Got:\n{code}"
    );
    // The gen_server reply tuple must carry the State extracted from element 2.
    assert!(
        code.contains("{'reply',"),
        "Actor method must return a gen_server reply tuple. Got:\n{code}"
    );
    // The conditional compiles to an inline case.
    assert!(
        code.contains("case "),
        "Conditional must compile to an inline case expression. Got:\n{code}"
    );
}

#[test]
fn test_actor_conditional_assign_rhs_emit_actor_threaded_assign_rhs() {
    // BT-2378: `result := flag ifTrue: [...] ifFalse: [...]` with local variable
    // mutations in an Actor method must route through `emit_actor_threaded_assign_rhs`:
    //   - bind element 1 of the {Value, NewState} tuple → assignment target
    //   - bind element 2 → next gen_server State version
    //   - rebind sibling outer locals (here `x`) from the new State map
    let src = "Actor subclass: Ctr\n  state: count = 0\n\n  compute: flag =>\n    x := 0\n    result := flag ifTrue: [x := 1. x] ifFalse: [x := -1. x]\n    result\n";
    let code = codegen(src);
    eprintln!("Generated code for actor conditional assign-RHS:\n{code}");

    // emit_actor_threaded_assign_rhs must extract value (element 1) and State (element 2).
    assert!(
        code.contains("'erlang':'element'(1,"),
        "Conditional assign-RHS must extract value via element(1, ...). Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'element'(2,"),
        "Conditional assign-RHS must extract new State via element(2, ...). Got:\n{code}"
    );
    // The sibling local `x` must be rebound from the updated State map.
    assert!(
        code.contains("'__local__x'"),
        "Sibling local 'x' must be rebound from the updated State map. Got:\n{code}"
    );
    // The conditional compiles to an inline case.
    assert!(
        code.contains("case "),
        "Conditional must compile to an inline case expression. Got:\n{code}"
    );
}

#[test]
fn test_immediately_invoked_literal_block_with_field_mutation_compiles() {
    // BT-2792: `[self.total := self.total + n] value` — a literal block that is
    // immediately invoked (the block is the *receiver* of `value`, not stored or
    // passed) — must NOT hit the FieldAssignmentInUnsupportedBlock rejection. The
    // compiler inlines this case correctly (state threads through StateAcc, same
    // as ifTrue:/do:), unlike a block bound to a variable and invoked later.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: n =>\n    [self.total := self.total + n] value\n";
    let code = codegen(src);
    assert!(
        code.contains("'maps':'put'('total'"),
        "Immediately-invoked block with a field mutation must thread state via maps:put. Got:\n{code}"
    );
}

#[test]
fn test_immediately_invoked_literal_block_value_keyword_with_field_mutation_compiles() {
    // BT-2792 (PR review follow-up): same as the unary `value` case above, but
    // for the keyword form `[...] value: arg`. This goes through a separate
    // code path (`try_generate_block_value_keyword`'s BT-1481 check in
    // intrinsics.rs) that must also inline field mutations rather than falling
    // through to generate_block's rejection.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: n =>\n    [:x | self.total := self.total + x] value: n\n";
    let code = codegen(src);
    assert!(
        code.contains("'maps':'put'('total'"),
        "Immediately-invoked block (value: form) with a field mutation must thread state via maps:put. Got:\n{code}"
    );
}

#[test]
fn test_block_returned_from_method_with_field_mutation_is_compile_error() {
    // BT-2792: `^[self.total := self.total + 1]` — a block *returned as a value*
    // (never invoked in this method) is not caught by any of the semantic-analysis
    // passes that guard field mutations in blocks (they only flag blocks that are
    // stored to a variable or passed as a literal argument to an unsafe message
    // send — see block_analyzer.rs's BlockContext::Stored check and
    // class_validators.rs's BT-1793 check). It still reaches generate_block's
    // generic fallback and must be rejected there.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  makeBlock =>\n    ^[self.total := self.total + 1]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let result = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::FieldAssignmentInUnsupportedBlock { .. })
        ),
        "A field-mutating block returned as a value must be a compile-time error \
         (BT-2792), not silently accepted. Got: {result:?}"
    );
}

#[test]
fn test_block_with_mixed_local_and_field_mutation_stored_then_invoked_compiles() {
    // BT-2792 (PR review follow-up): a block with BOTH a captured-local
    // mutation and a field write — e.g. `[:x | outerCount := outerCount + x.
    // self.total := self.total + outerCount]` stored in a var and invoked
    // later — used to bypass the field-write check entirely: captured_mutations
    // is non-empty (from `outerCount`), so generate_block routed straight to
    // Tier 2 for the local mutation, never reaching validate_stored_closure.
    // That produced Core Erlang that *compiled* (passed erlc) but crashed at
    // runtime: the resulting block is a 2-arity stateful fun (params + State),
    // but generate_block_value_call and friends called it with only its
    // declared params (no State argument) — `badarity`. BT-2792 closed that
    // gap by making it a compile-time error instead.
    //
    // BT-2797 replaces the compile-time error with a real fix for exactly
    // this shape: `blk`'s only use in the rest of the method is the `value:`
    // call below, which `prescan_tier2_local_vars` proves is safe (BT-2797),
    // so the block is now compiled via `generate_block_stateful` and invoked
    // through the Tier 2 calling convention (`apply Fun(Args, State)`,
    // unpacking the `{Result, NewState}` tuple) — see
    // `test_bt2797_same_method_tier2_local_var_threads_state_correctly` below
    // for a check of the generated Core Erlang shape itself.
    //
    // `outerCount := outerCount + x` is deliberately the block's first use of
    // `outerCount`: block_analysis classifies a name as a *captured* mutation
    // only when it's read before being locally defined *within the block*,
    // and `outerCount + x` on the assignment's right-hand side reads the
    // name before this statement (the block's only mention of it) defines
    // it. Writing the block as `outerCount := 0. outerCount := outerCount +
    // x` instead would make `outerCount` a fresh block-local, not a captured
    // one, and the mixed local+field shape this test targets wouldn't
    // reproduce. The outer method's own `outerCount := 0` — appearing after
    // `blk`'s definition in program order — only needs to exist so the
    // source parses as a valid Beamtalk program; it has no bearing on the
    // capture classification itself.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: item =>\n    blk := [:x | outerCount := outerCount + x. self.total := self.total + outerCount]\n    outerCount := 0\n    blk value: item\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let result = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    );
    assert!(
        result.is_ok(),
        "A block with both a local and a field mutation, stored then invoked \
         via `value:` later in the same method, must compile now that the \
         compiler can prove the call site threads state correctly (BT-2797). \
         Got: {result:?}"
    );
}

#[test]
fn test_bt2797_same_method_tier2_local_var_threads_state_correctly() {
    // BT-2797: verifies the *shape* of the generated Core Erlang for the
    // scenario above, not just that codegen returns Ok(..). `blk` must be a
    // 2-arity fun taking a trailing state accumulator and returning a
    // `{Result, NewState}` tuple, and the call site must `apply` it with the
    // outer method's State and unpack the tuple — not naively `apply Fun
    // (Arg)` (which would compile but badarity-crash at runtime).
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: item =>\n    blk := [:x | outerCount := outerCount + x. self.total := self.total + outerCount]\n    outerCount := 0\n    blk value: item\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let code = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("should compile (BT-2797)");

    // Structural checks (not hardcoded fresh-variable counter values, which
    // shift whenever unrelated codegen changes the counter sequence before
    // this point).
    assert!(
        regex::Regex::new(r"let Blk = fun \(_\w+, StateAcc\) ->")
            .unwrap()
            .is_match(&code),
        "blk must compile to a 2-arity Tier 2 fun (block param + trailing \
         state accumulator). Got: {code}"
    );
    assert!(
        regex::Regex::new(r"apply _Fun\w* \(_item\w*, State\)")
            .unwrap()
            .is_match(&code),
        "the `blk value: item` call site must apply the block with the \
         outer method's State as a trailing argument. Got: {code}"
    );
    assert!(
        regex::Regex::new(r"call 'erlang':'element'\(1, _T2Tuple\w*\)")
            .unwrap()
            .is_match(&code)
            && regex::Regex::new(r"call 'erlang':'element'\(2, _T2Tuple\w*\)")
                .unwrap()
                .is_match(&code),
        "the call site must unpack the returned {{Result, NewState}} tuple \
         rather than treating the raw apply result as the method's return \
         value. Got: {code}"
    );
}

#[test]
fn test_bt2808_cascade_on_tier2_local_var_compiles_and_threads_state() {
    // BT-2808: `blk value: item; value: item` — a cascade sending two safe
    // `value:` sends to the SAME Tier 2 local var. Before the fix,
    // `scan_var_uses`'s `Cascade` arm hit the generic `Identifier` arm on the
    // receiver (since the receiver *is* `blk`) and unconditionally reported it
    // unsafe, so `prescan_tier2_local_vars` never promoted `blk` and this hit
    // the `FieldAssignmentInUnsupportedBlock` compile-time diagnostic even
    // though the pattern is exactly as safe as a single `blk value: item`.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: item =>\n    blk := [:x | self.total := self.total + x]\n    blk value: item; value: item\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let code = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("cascade of safe value: sends on a Tier 2 local var must compile (BT-2808)");

    // Both cascaded `value:` sends must apply the block with a threaded state
    // argument, and each must unpack its own {Result, NewState} tuple — not
    // just the first message (which would silently drop the second mutation).
    let apply_count = regex::Regex::new(r"apply _Fun\w* \(_item\w*, \w*State\w*\)")
        .unwrap()
        .find_iter(&code)
        .count();
    assert_eq!(
        apply_count, 2,
        "both cascaded value: sends must apply the block with a threaded \
         state argument. Got: {code}"
    );
    let element1_count = regex::Regex::new(r"call 'erlang':'element'\(1, _\w+\)")
        .unwrap()
        .find_iter(&code)
        .count();
    let element2_count = regex::Regex::new(r"call 'erlang':'element'\(2, _\w+\)")
        .unwrap()
        .find_iter(&code)
        .count();
    assert!(
        element1_count >= 2 && element2_count >= 2,
        "each cascaded value: send's {{Result, NewState}} tuple must be \
         unpacked separately so both mutations thread through. Got: {code}"
    );
}

#[test]
fn test_bt2797_local_tier2_block_never_invoked_again_is_still_compile_error() {
    // BT-2797 regression guard: `blk := [block needing Tier 2]` where `blk` is
    // never referenced again in the rest of the method — here because the
    // assignment is the method's *last* statement, so the raw Tier 2 fun value
    // implicitly escapes as the method's own return value. `prescan_tier2_local_vars`
    // must NOT promote this: an early, buggy version of the safety check asked
    // "is there no *unsafe* use of blk afterward?", which is vacuously true
    // when there's no use at all (`[].iter().all(...)` on an empty slice), so
    // it wrongly promoted variables that are simply never used again. The fix
    // requires proof of at least one *safe* use, not just the absence of an
    // unsafe one. This must keep hitting the compile-time diagnostic instead of
    // producing Core Erlang that returns a raw 2-arity fun to a caller with no
    // idea it needs to thread state through it.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: item =>\n    blk := [:x | self.total := self.total + x]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let result = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::FieldAssignmentInUnsupportedBlock { .. })
        ),
        "A Tier 2 block stored in a local var and never invoked again (so it \
         escapes as the method's implicit return value) must remain a \
         compile-time error. Got: {result:?}"
    );
}

#[test]
fn test_bt2797_local_tier2_block_invoked_inside_nested_do_block_is_still_compile_error() {
    // BT-2797 regression guard (PR review follow-up): `blk value: item` found
    // only *inside* a nested block literal (here, the `do:` iteration block)
    // must NOT be treated as a safe use, even though it looks identical to a
    // safe top-level `value:` call. A nested block compiles through a
    // completely separate path (`generate_block_body_slice`/`BlockExprKind`,
    // not `generate_body_exprs_with_reply`/`BodyExprKind`) that has no
    // Tier2-tuple-unpacking logic and never resets `tier2_local_vars` for its
    // own body — so wrongly promoting `blk` here would either leak an
    // unpacked `{Result, NewState}` tuple as the inner block's return value,
    // or badarity-crash calling a 2-arity Tier 2 fun with 1 argument.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: items =>\n    blk := [:x | self.total := self.total + x]\n    items do: [:item | blk value: item]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let result = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::FieldAssignmentInUnsupportedBlock { .. })
        ),
        "A Tier 2 local block invoked only from inside a nested `do:` block \
         must remain a compile-time error, not silently-broken Core Erlang. \
         Got: {result:?}"
    );
}

#[test]
fn test_bt2797_local_tier2_block_invoked_inside_nested_if_true_block_is_still_compile_error() {
    // BT-2797 regression guard (PR review follow-up): same as the `do:` case
    // above, but for a `ifTrue:` control-flow block — the other concrete
    // trigger the reviewer flagged.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: n =>\n    blk := [:x | self.total := self.total + x]\n    n > 0 ifTrue: [blk value: n]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let result = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::FieldAssignmentInUnsupportedBlock { .. })
        ),
        "A Tier 2 local block invoked only from inside a nested `ifTrue:` \
         block must remain a compile-time error, not a silent state-mutation \
         loss at runtime. Got: {result:?}"
    );
}

#[test]
fn test_bt2797_field_stored_block_invoked_from_different_method_threads_state_correctly() {
    // BT-2797: the main real-world motivator — a block with field mutations,
    // assigned to an instance field in one method (`setup`) and invoked via
    // `value:` from a *different* method (`tick:`). Static per-method tracking
    // (tier2_block_params / tier2_local_vars) can't see across methods, so this
    // relies on:
    // 1. `generate_field_assignment_value_doc` (dispatch_codegen.rs) promoting
    //    the stored block to Tier 2 unconditionally (safe because every
    //    `self.field value(:...)` call site now runtime-discriminates), and
    // 2. `generate_block_value_call_runtime_discriminated` (intrinsics.rs)
    //    checking the field's *runtime* arity (`is_function/2`) at the call
    //    site to decide whether to thread state — the BT-909 precedent
    //    generalized from Erlang FFI interop to Beamtalk-level block calls.
    let src = "Actor subclass: Ctr\n  state: total = 0\n  state: onTick = nil\n\n  setup =>\n    self.onTick := [:x | self.total := self.total + x]\n\n  tick: x =>\n    self.onTick value: x\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let code = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("should compile (BT-2797)");

    // Structural checks (not hardcoded fresh-variable counter values).
    assert!(
        regex::Regex::new(r"fun \(_\w+, StateAcc\) ->")
            .unwrap()
            .is_match(&code)
            && code.contains("'maps':'put'('onTick',"),
        "setup must store a 2-arity Tier 2 fun (block param + trailing state \
         accumulator) into the onTick field. Got: {code}"
    );
    assert!(
        code.contains("'maps':'get'('onTick', State)"),
        "tick: must read the block back out of the onTick field. Got: {code}"
    );
    // The Tier 1 and Tier 2 arity checks, and the Tier 2 apply, must all
    // reference the *same* captured block variable — tie them together via
    // the name captured from the arity-1 check rather than three independent
    // (and therefore looser) pattern matches.
    let arity_check_re = regex::Regex::new(r"is_function'\((_Fun\w*), 1\)").unwrap();
    let fun_var = &arity_check_re.captures(&code).unwrap_or_else(|| {
        panic!(
            "tick: must runtime-discriminate Tier 1 (arity 1: just the block \
                 param) before applying. Got: {code}"
        )
    })[1];
    assert!(
        code.contains(&format!("is_function'({fun_var}, 2)")),
        "tick: must also check Tier 2 arity (block param + state) for the \
         same block variable ({fun_var}). Got: {code}"
    );
    assert!(
        regex::Regex::new(&format!(r"apply {fun_var} \(_\w+, State\)"))
            .unwrap()
            .is_match(&code),
        "the Tier 2 branch must apply the field's block ({fun_var}) with the \
         calling method's State as a trailing argument. Got: {code}"
    );
}

#[test]
fn test_bt2797_field_stored_block_with_captured_local_and_field_write_is_still_compile_error() {
    // BT-2797 (PR #2899 review fix): a block stored in a field that mutates
    // BOTH a captured outer local AND a field must still be rejected at
    // compile time, not silently promoted to Tier 2 like the field-writes-only
    // case. `generate_block_stateful`'s captured-local handling reads a
    // `'__local__<var>'` key from the *calling* method's StateAcc, falling
    // back to the value closed over at block-definition time when absent —
    // correct only when the block is invoked from the same method it was
    // defined in. A field-stored block can be invoked from a *different*
    // method (that's the entire point of BT-2797), so that fallback would
    // silently return a stale value forever, and the key would then leak
    // into the actor's persistent state once the returned NewState is merged
    // back in. This combination was a compile-time error before BT-2797 and
    // must remain one.
    let src = "Actor subclass: Ctr\n  state: total = 0\n  state: callback = nil\n\n  setup =>\n    count := 0\n    self.callback := [:n | count := count + n. self.total := self.total + count]\n\n  process: n =>\n    self.callback value: n\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let result = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::FieldAssignmentInUnsupportedBlock { .. })
        ),
        "a block stored in a field that also captures and mutates an outer \
         local must still be rejected at compile time — promoting it would \
         leak a '__local__<var>' state key into the actor's persistent state \
         and read a stale definition-time fallback value when invoked from a \
         different method than the one that stored it. Got: {result:?}"
    );
}

#[test]
fn test_bt2797_nonliteral_field_mutating_block_passed_to_self_send_is_compile_error() {
    // BT-2797 (verification, acceptance criterion 5): a field-mutating block
    // held in a local var and passed as a *non-literal* argument to a
    // self-send — `self applyBlock: blk to: x`, where `blk` was assigned
    // separately — is a case `scan_class_for_tier2_blocks`
    // (dispatch_codegen.rs) can't see: it only recognizes a *literal* block
    // at the call site to promote the callee's parameter into
    // `tier2_method_info`/`tier2_block_params`. If this compiled anyway with
    // `aBlock value: x` inside `applyBlock:to:` naively applying with no
    // state, it would badarity-crash at runtime whenever `blk` is actually a
    // Tier 2 fun.
    //
    // Confirms this is instead a compile-time error: `prescan_tier2_local_vars`
    // only promotes `blk` when every later use is a *safe* value/value: call —
    // here `blk` is passed as an *argument* to `applyBlock:to:`, not a value:
    // receiver, so prescan correctly leaves it unpromoted and it falls through
    // to `generate_block`'s existing `FieldAssignmentInUnsupportedBlock` gate
    // (BT-2792) — a safe compile-time failure, not a silent runtime crash.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  applyBlock: aBlock to: x =>\n    aBlock value: x\n\n  run: x =>\n    blk := [:y | self.total := self.total + y]\n    self applyBlock: blk to: x\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let result = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::FieldAssignmentInUnsupportedBlock { .. })
        ),
        "A field-mutating block passed as a non-literal argument to a \
         self-send must be a compile-time error, not code that compiles but \
         risks badarity at runtime. Got: {result:?}"
    );
}

#[test]
fn test_bt2797_no_regression_pure_block_value_fast_path() {
    // BT-2797 (acceptance criterion 7): the zero-cost fast path for a pure
    // (non-mutating) block literal immediately invoked via `value`/`value:`
    // must be untouched — no `is_function` runtime check, no state-threading
    // overhead. BT-2797's new runtime-discrimination codegen
    // (generate_block_value_call_runtime_discriminated) is deliberately
    // scoped to `self.field value(:...)` receivers only (see
    // try_generate_block_value_unary/keyword in intrinsics.rs) — a literal
    // block receiver is intercepted earlier and takes the plain
    // generate_block_value_call path regardless.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run =>\n    [42] value\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let code = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("should compile");

    assert!(
        !code.contains("is_function"),
        "A pure block literal invoked via `value` must not emit any \
         is_function runtime check. Got: {code}"
    );
}

#[test]
fn test_bt2797_no_regression_pure_local_var_block_value_fast_path() {
    // BT-2797: a block held in a local var (not a field) that has NO captured
    // or field mutations must also stay on the pre-existing plain
    // `is_function` guard (generate_value_keyword_guard, unaffected by
    // BT-2797) — never the new self.field-scoped runtime-discrimination path,
    // and never the Tier 2 stateful protocol.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: item =>\n    blk := [:x | x + 1]\n    blk value: item\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let code = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("should compile");

    assert!(
        !code.contains("StateAcc"),
        "A pure block stored in a local var must not be promoted to the \
         Tier 2 stateful protocol. Got: {code}"
    );
}

#[test]
fn test_bt2803_field_stored_block_invoked_via_value_with_arguments_threads_state_correctly() {
    // BT-2803: valueWithArguments: on a self.field receiver needs the same
    // runtime Tier 1/Tier 2 discrimination as `value:` (BT-2797), but the
    // argument count is a runtime list length instead of a compile-time-known
    // static arity —
    // generate_block_value_with_arguments_call_runtime_discriminated
    // generalizes generate_block_value_call_runtime_discriminated for this.
    let src = "Actor subclass: Ctr\n  state: total = 0\n  state: onTick = nil\n\n  setup =>\n    self.onTick := [:x | self.total := self.total + x]\n\n  tick =>\n    self.onTick valueWithArguments: #(5)\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let code = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("should compile (BT-2803)");

    assert!(
        code.contains("'maps':'get'('onTick', State)"),
        "tick must read the block back out of the onTick field. Got: {code}"
    );

    // Tie the length computation, both arity checks, and both applies
    // together via the captured fun/args/length variable names rather than
    // three independent (and therefore looser) pattern matches.
    let len_re =
        regex::Regex::new(r"let (_Fun\w*) = .*'onTick', State\) in let (_Args\w*) = \[5\] in let (_ArgsLen\w*) = call 'erlang':'length'\((_Args\w*)\) in let (_ArgsLenPlusOne\w*) = call 'erlang':'\+'\((_ArgsLen\w*), 1\)")
            .unwrap();
    let caps = len_re.captures(&code).unwrap_or_else(|| {
        panic!(
            "tick must bind the field's block, hoist Args to a runtime list, \
             and compute both length(Args) and length(Args) + 1 before \
             discriminating. Got: {code}"
        )
    });
    let fun_var = &caps[1];
    let args_var = &caps[2];
    let tier1_len_var = &caps[3];
    let tier2_len_var = &caps[5];

    assert!(
        code.contains(&format!("is_function'({fun_var}, {tier1_len_var})")),
        "tick must runtime-discriminate Tier 1 arity (length(Args)) for the \
         field's block ({fun_var}). Got: {code}"
    );
    assert!(
        code.contains(&format!("is_function'({fun_var}, {tier2_len_var})")),
        "tick must also check the Tier 2 arity (length(Args) + 1) for the \
         same block variable ({fun_var}). Got: {code}"
    );
    assert!(
        code.contains(&format!(
            "{{call 'erlang':'apply'({fun_var}, {args_var}), State}}"
        )),
        "the Tier 1 branch must apply the field's block ({fun_var}) with the \
         plain Args list, leaving State unchanged. Got: {code}"
    );
    assert!(
        code.contains(&format!(
            "call 'erlang':'apply'({fun_var}, call 'erlang':'++'({args_var}, [State]))"
        )),
        "the Tier 2 branch must apply the field's block ({fun_var}) with the \
         calling method's State appended to the Args list. Got: {code}"
    );
    assert!(
        code.contains(&format!(
            "{{call 'beamtalk_primitive':'send'({fun_var}, 'valueWithArguments:', [{args_var}]), State}}"
        )),
        "the non-function fallback must DNU-dispatch valueWithArguments: with \
         State unchanged. Got: {code}"
    );
}

#[test]
fn test_bt2803_no_regression_pure_block_value_with_arguments_fast_path() {
    // BT-2803 (mirrors BT-2797's fast-path regression guard): a literal
    // block receiver never needs the runtime is_function guard —
    // try_generate_block_value_with_arguments_keyword's literal-block fast
    // path applies Args directly via erlang:apply, no runtime check at all.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run =>\n    [:x :y | x + y] valueWithArguments: #(3, 4)\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let code = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("should compile");

    assert!(
        !code.contains("is_function"),
        "A block literal invoked via valueWithArguments: must not emit any \
         is_function runtime check. Got: {code}"
    );
    assert!(
        code.contains("call 'erlang':'apply'("),
        "must still apply the block to the runtime Args list. Got: {code}"
    );
}

#[test]
fn test_bt2803_no_regression_pure_local_var_value_with_arguments_fast_path() {
    // BT-2803: a block held in a local var (not a field, no captured/field
    // mutations) reaches the generic is_function/1 guard
    // (generate_block_value_with_arguments_call) — never the Tier 2 runtime-
    // discriminated path, and never the stateful protocol.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run =>\n    blk := [:x | x + 1]\n    blk valueWithArguments: #(5)\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let code = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("should compile");

    assert!(
        !code.contains("StateAcc"),
        "A pure block stored in a local var must not be promoted to the \
         Tier 2 stateful protocol. Got: {code}"
    );
    assert!(
        regex::Regex::new(r"is_function'\(_ValRecv\w*\) of")
            .unwrap()
            .is_match(&code),
        "must use the plain is_function/1 guard on the hoisted receiver \
         (generate_block_value_with_arguments_call), not the runtime-length \
         Tier 1/Tier 2 discrimination. Got: {code}"
    );
    assert!(
        !code.contains("'erlang':'length'("),
        "must not compute a runtime Args length — that's only needed for \
         the Tier 1/Tier 2 discriminated path. Got: {code}"
    );
}

#[test]
fn test_bt2813_bare_tier2_value_call_inside_do_loop_body_unpacks_tuple() {
    // BT-2813: a bare (non-assigned) `self.field value:` statement inside a
    // `do:` loop body. Before the fix, the outer loop was correctly routed
    // into the state-threading (StateAcc) path by `block_needs_mutation_threading`
    // (BT-2807's `has_field_value_call` fact), but the loop body's own
    // statement codegen (`generate_threaded_loop_body_inner`) had no case for
    // a bare Tier2ValueCall — it fell through to `emit_non_assign_expr`,
    // which emitted a plain (Tier-1-only) apply and crashed with badarity for
    // a genuinely Tier 2 (2-arity) field-stored block. Structural check only
    // (see stdlib/test/tier2_stored_block_matrix_test.bt for the runtime
    // end-to-end check).
    let src = "Actor subclass: Ctr\n  state: total = 0\n  state: onTick = nil\n\n  setup => self.onTick := [:x | self.total := self.total + x]\n\n  tickEach: items =>\n    items do: [:x | self.onTick value: x]\n    self.total\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let code = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("should compile (BT-2813)");

    assert!(
        regex::Regex::new(r"let _T2LoopTuple\w* = .*is_function.*in let StateAcc\w* = call 'erlang':'element'\(2, _T2LoopTuple")
            .unwrap()
            .is_match(&code),
        "the bare Tier2ValueCall statement inside the do: loop body must \
         runtime-discriminate the field's block and unpack the returned \
         {{Result, NewState}} tuple via element/2, threading the new state \
         forward into the next fold iteration. Got: {code}"
    );
}

#[test]
fn test_bt2813_bare_tier2_value_call_inside_collect_block_unpacks_tuple() {
    // BT-2813: same gap as the do: case above, but for collect: — the loop
    // body must also extract element(1) of the tuple as the collected value.
    let src = "Actor subclass: Ctr\n  state: total = 0\n  state: onTick = nil\n\n  setup => self.onTick := [:x | self.total := self.total + x]\n\n  tickEachCollect: items =>\n    items collect: [:x | self.onTick value: x]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let code = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("should compile (BT-2813)");

    assert!(
        regex::Regex::new(r"let _T2LoopVal\w* = call 'erlang':'element'\(1, _T2LoopTuple\w*\) in \{\[_T2LoopVal\w* \| AccList\]")
            .unwrap()
            .is_match(&code),
        "the bare Tier2ValueCall statement inside the collect: loop body \
         must extract element(1) of the returned tuple as the collected \
         item value. Got: {code}"
    );
}

#[test]
fn test_bt2814_local_var_tier2_value_call_in_argument_position_unpacks_result() {
    // BT-2814: a Tier 2 block held in a local var, invoked via `value:` in
    // *argument* (sub-expression) position. Before the fix,
    // `try_generate_block_value_keyword` intercepted this receiver shape
    // (tier2_local_vars) and called `generate_block_value_call_stateful`
    // directly, returning the raw {Result, NewState} tuple straight into the
    // arithmetic — `10 + {Result, NewState}` crashes with badarith at
    // runtime. `close_tier2_value_subexpr_doc` now unpacks element(1) so the
    // arithmetic sees a plain value. Structural check only (see
    // stdlib/test/tier2_stored_block_matrix_test.bt for the runtime
    // end-to-end check).
    let src = "Actor subclass: Ctr\n  state: dummy = 0\n\n  run: x =>\n    r := 0\n    blk := [:n | r := r + n]\n    10 + (blk value: x)\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let code = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("should compile (BT-2814)");

    assert!(
        regex::Regex::new(r"call 'erlang':'\+'\(10, let _T2SubTuple\w* = .*apply.*in call 'erlang':'element'\(1, _T2SubTuple")
            .unwrap()
            .is_match(&code),
        "the local-var Tier2 block's value: call in argument position must \
         be wrapped in a self-contained let-chain that extracts element(1) \
         of the returned tuple before the addition, not the raw tuple. \
         Got: {code}"
    );
}

#[test]
fn test_bt2814_field_stored_tier2_value_call_in_argument_position_unpacks_result() {
    // BT-2814: the self.field variant of the same gap. Before the fix,
    // `try_generate_block_value_keyword`/`_unary` deliberately did NOT
    // intercept a self.field receiver in sub-expression position at all,
    // falling back to a Tier-1-only (arity-N, no State) apply — badarity for
    // a genuinely Tier 2 block. `close_tier2_value_subexpr_doc` now
    // intercepts and unpacks it, consistent with the local-var case above.
    let src = "Actor subclass: Ctr\n  state: total = 0\n  state: onTick = nil\n\n  setup => self.onTick := [:x | self.total := self.total + x]\n\n  addTickResult: x =>\n    self.total := self.total + (self.onTick value: x)\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let code = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("should compile (BT-2814)");

    assert!(
        regex::Regex::new(
            r"let _T2SubTuple\w* = .*is_function.*in call 'erlang':'element'\(1, _T2SubTuple"
        )
        .unwrap()
        .is_match(&code),
        "the field-stored Tier2 block's value: call in argument position \
         must runtime-discriminate the field's block and extract element(1) \
         of the returned tuple, not leak the raw tuple or fall back to a \
         Tier-1-only apply. Got: {code}"
    );
}
#[test]
fn test_bt2815_named_local_var_captured_mutation_rebinds_after_call() {
    // BT-2815: a block assigned to a LOCAL variable (not a field) whose only
    // mutation is a captured outer local, invoked later via `value:` in the
    // same method. Before the fix, `get_inline_block_captured_mutations`
    // only recognized an INLINE block literal receiver (the original BT-1213
    // scope) — a NAMED `tier2_local_vars` identifier receiver fell through
    // with no rebinding, so the caller's own `outer` variable silently kept
    // its stale pre-call value even though the call itself succeeded and
    // internally computed the right value. `prescan_tier2_local_vars` now
    // records the captured-mutation var names keyed by variable name
    // (`tier2_local_var_captured_mutations`) so the call site can rebind
    // them the same way it already does for an inline literal. Structural
    // check only (see stdlib/test/tier2_stored_block_matrix_test.bt for the
    // runtime end-to-end check).
    let src = "Actor subclass: Ctr\n  state: dummy = 0\n\n  run =>\n    outer := 0\n    blk := [:n | outer := outer + n]\n    blk value: 5\n    outer\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let code = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("should compile (BT-2815)");

    assert!(
        regex::Regex::new(
            r"let State1 = call 'erlang':'element'\(2, _T2Tuple\w*\) in let Outer\w* = call 'maps':'get'\('__local__outer', State1\)"
        )
        .unwrap()
        .is_match(&code),
        "after the named-local-var Tier2 block's value: call, the caller's \
         own `outer` binding must be rebound from '__local__outer' in the \
         call's returned NewState, mirroring the inline-block-literal case. \
         Got: {code}"
    );
}

#[test]
fn test_bt2815_named_local_var_cascade_captured_mutation_rebinds_after_call() {
    // BT-2815 acceptance criteria: verify the cascade variant too —
    // `blk value: x; value: x` (BT-2808's cascade codegen) invoked twice
    // must also rebind the caller's `outer` var from the cascade's final
    // NewState, not just the single-send case above.
    let src = "Actor subclass: Ctr\n  state: dummy = 0\n\n  run =>\n    outer := 0\n    blk := [:n | outer := outer + n]\n    blk value: 4; value: 4\n    outer\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    let code = crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("should compile (BT-2815)");

    assert!(
        regex::Regex::new(
            r"let Outer\w* = call 'maps':'get'\('__local__outer', State\w*\) in let _Result = Outer"
        )
        .unwrap()
        .is_match(&code),
        "after the named-local-var Tier2 block's cascade (value: x; value: \
         x), the caller's own `outer` binding must be rebound from \
         '__local__outer' in the cascade's final NewState. Got: {code}"
    );
}
