// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use crate::core_erlang::CoreErlangGenerator;
use beamtalk_core::ast::{Block, BlockParameter, Expression, ExpressionStatement, Literal};
use beamtalk_core::source_analysis::Span;

fn s() -> Span {
    Span::new(0, 0)
}

fn bare(expr: Expression) -> ExpressionStatement {
    ExpressionStatement::bare(expr)
}

#[test]
fn test_generate_block_no_params() {
    let mut generator = CoreErlangGenerator::new("test");
    let block = Block::new(
        vec![],
        vec![bare(Expression::Literal(Literal::Integer(99), s()))],
        s(),
    );
    let doc = generator.generate_block(&block).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("fun ()"),
        "block header should be 'fun ()'. Got: {output}"
    );
    assert!(
        output.contains("99"),
        "block body should contain literal. Got: {output}"
    );
}

#[test]
fn test_generate_block_with_param() {
    let mut generator = CoreErlangGenerator::new("test");
    let block = Block::new(
        vec![BlockParameter::new("x", s())],
        vec![bare(Expression::Literal(Literal::Integer(0), s()))],
        s(),
    );
    let doc = generator.generate_block(&block).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("fun ("),
        "block should start with 'fun ('. Got: {output}"
    );
    assert!(
        output.contains("->"),
        "block should have arrow. Got: {output}"
    );
}
