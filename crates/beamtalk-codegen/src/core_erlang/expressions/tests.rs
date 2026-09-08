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

// ─── BT-3466: generate_field_assignment (Closure::Closed) × FieldWriteSite ─

#[test]
fn test_field_assignment_closed_actor_threads_state() {
    let mut generator = CoreErlangGenerator::new("test");
    let value = Expression::Literal(Literal::Integer(42), s());
    let output = generator
        .generate_field_assignment("count", &value)
        .unwrap()
        .to_pretty_string();
    assert_eq!(
        output,
        "let _Val1 = 42 in let State1 = call 'maps':'put'('count', _Val1, State) in _Val1"
    );
}

#[test]
fn test_field_assignment_closed_value_type_threads_self() {
    // BT-833: a value-type field write threads `Self{N}`, never `State{N}` —
    // this is the shape `generate_field_assignment_open`'s pre-BT-3466
    // Actor-only fallback silently got wrong for the "open" sibling of this
    // exact write (see the `_open_value_type_...` tests below).
    let mut generator = CoreErlangGenerator::new("test");
    generator.context = crate::core_erlang::CodeGenContext::ValueType;
    let value = Expression::Literal(Literal::Integer(42), s());
    let output = generator
        .generate_field_assignment("count", &value)
        .unwrap()
        .to_pretty_string();
    assert_eq!(
        output,
        "let _Val1 = 42 in let Self1 = call 'maps':'put'('count', _Val1, Self) in _Val1"
    );
    assert!(
        !output.contains("State"),
        "value-type field write must never reference State. Got: {output}"
    );
}

#[test]
fn test_field_assignment_closed_class_var_threads_class_vars_with_shadow_write() {
    // BT-412/ADR 0110: a class-method field write threads `ClassVars{N}` and
    // carries the shadow write (`erlang:put/2` under `$bt_class_vars_shadow`)
    // that lets a foreign NLR relay observe the mutation.
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_in_class_method(true);
    generator.class_var_names_mut().insert("total".to_string());
    let value = Expression::Literal(Literal::Integer(42), s());
    let output = generator
        .generate_field_assignment("total", &value)
        .unwrap()
        .to_pretty_string();
    assert!(
        output.contains("let ClassVars1 = call 'maps':'put'('total', _Val1, ClassVars) in"),
        "class-var write should thread ClassVars. Got: {output}"
    );
    assert!(
        output.contains("'$bt_class_vars_shadow'"),
        "class-var write should carry ADR 0110's shadow write. Got: {output}"
    );
    assert!(
        !output.contains("State"),
        "class-var write must never reference State. Got: {output}"
    );
}
