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

    let result = CoreErlangGenerator::validate_stored_closure(&block, "test".to_string());
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

    let result = CoreErlangGenerator::validate_stored_closure(&block, "test".to_string());
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

    let result = CoreErlangGenerator::validate_stored_closure(&block, "test".to_string());
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

    let result = CoreErlangGenerator::validate_stored_closure(&block, "test".to_string());
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

    let result = CoreErlangGenerator::validate_stored_closure(&block, "test".to_string());
    assert!(result.is_err(), "Field assignment should produce error");

    if let Err(CodeGenError::FieldAssignmentInStoredClosure {
        field,
        field_capitalized,
        ..
    }) = result
    {
        assert_eq!(field, "value");
        assert_eq!(field_capitalized, "Value");
    } else {
        panic!("Expected FieldAssignmentInStoredClosure error");
    }
}

#[test]
fn test_validate_stored_closure_field_takes_precedence() {
    // Block with both field and local assignment
    // Field error should be reported first
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

    let result = CoreErlangGenerator::validate_stored_closure(&block, "test".to_string());
    assert!(result.is_err());

    // Should be field error (checked first), not local
    assert!(
        matches!(
            result,
            Err(CodeGenError::FieldAssignmentInStoredClosure { .. })
        ),
        "Field error should take precedence over local mutation"
    );
}

#[test]
fn test_codegen_allows_stored_closure_with_field_assignment() {
    // Integration test: verify the full codegen pipeline allows field assignments
    // in stored closures and successfully generates code for: test := [ myBlock := [self.value := 1]. myBlock ]
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

    // BT-852: Stored closures with field assignments are now allowed via Tier 2 protocol.
    let result = generate(&module);
    assert!(
        result.is_ok(),
        "Field assignment in stored closure should now be allowed via Tier 2 protocol. Got: {result:?}"
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
