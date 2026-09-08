// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use crate::core_erlang::CoreErlangGenerator;
use beamtalk_core::ast::{Expression, Identifier, Literal, MapPair};
use beamtalk_core::source_analysis::Span;

fn s() -> Span {
    Span::new(0, 0)
}

#[test]
fn test_generate_empty_map_literal() {
    let mut generator = CoreErlangGenerator::new("test");
    let doc = generator.generate_map_literal(&[]).unwrap();
    assert_eq!(doc.to_pretty_string(), "~{}~");
}

#[test]
fn test_generate_map_literal_with_symbol_key() {
    let mut generator = CoreErlangGenerator::new("test");
    let pairs = vec![MapPair::new(
        Expression::Literal(Literal::Symbol("x".into()), s()),
        Expression::Literal(Literal::Integer(1), s()),
        s(),
    )];
    let doc = generator.generate_map_literal(&pairs).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'x'"),
        "map key should be atom. Got: {output}"
    );
    assert!(
        output.contains("=> 1"),
        "map value should be integer. Got: {output}"
    );
    assert!(
        output.contains("=>"),
        "map should use => syntax. Got: {output}"
    );
}

#[test]
fn test_generate_empty_list_literal() {
    let mut generator = CoreErlangGenerator::new("test");
    let doc = generator.generate_list_literal(&[], None).unwrap();
    assert_eq!(doc.to_pretty_string(), "[]");
}

#[test]
fn test_generate_list_literal_with_elements() {
    let mut generator = CoreErlangGenerator::new("test");
    let elements = vec![
        Expression::Literal(Literal::Integer(1), s()),
        Expression::Literal(Literal::Integer(2), s()),
    ];
    let doc = generator.generate_list_literal(&elements, None).unwrap();
    assert_eq!(doc.to_pretty_string(), "[1, 2]");
}

#[test]
fn test_generate_array_literal_calls_from_list() {
    let mut generator = CoreErlangGenerator::new("test");
    let elements = vec![Expression::Literal(Literal::Integer(42), s())];
    let doc = generator.generate_array_literal(&elements).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("beamtalk_array':'from_list'"),
        "array should call from_list. Got: {output}"
    );
    assert!(
        output.contains("42"),
        "array should contain element. Got: {output}"
    );
}

#[test]
fn test_generate_identifier_reserved_words() {
    let mut generator = CoreErlangGenerator::new("test");
    let t = generator
        .generate_identifier(&Identifier::new("true", s()))
        .unwrap();
    assert_eq!(t.to_pretty_string(), "'true'");
    let f = generator
        .generate_identifier(&Identifier::new("false", s()))
        .unwrap();
    assert_eq!(f.to_pretty_string(), "'false'");
    let n = generator
        .generate_identifier(&Identifier::new("nil", s()))
        .unwrap();
    assert_eq!(n.to_pretty_string(), "'nil'");
}
