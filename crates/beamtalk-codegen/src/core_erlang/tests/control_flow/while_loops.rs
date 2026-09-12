// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `whileTrue:`/`whileFalse:`/`repeat` loop codegen, including
//! stored-closure compilation through `erlc`.

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
    // Binary ops no longer wrap operands with maybe_await (ADR-0043)
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

    assert_compiles_through_erlc("test_while_loop", &full_output);
}
