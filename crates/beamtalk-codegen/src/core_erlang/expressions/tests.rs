// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use crate::core_erlang::CoreErlangGenerator;
use crate::core_erlang::control_flow::{Closure, FieldWriteSite};
use crate::core_erlang::threaded_ir::FrameId;
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
    // `render_field_write`: the `Closed` rendering without BT-3803's escape
    // check, which `generate_field_assignment` (production) runs.
    let output = closed_write(&mut generator, &value);
    assert_eq!(
        output,
        "let _Val1 = 42 in let State1 = call 'maps':'put'('count', _Val1, State) in _Val1"
    );
}

fn closed_write(generator: &mut CoreErlangGenerator, value: &Expression) -> String {
    let site = FieldWriteSite::for_context(generator.context);
    generator
        .render_field_write(site, Closure::Closed, "count", value, FrameId::ROOT)
        .unwrap()
        .0
        .to_pretty_string()
}

/// BT-3803: a `Closed` field write mints a `State`/`Self` version inside a
/// self-contained document, so a consumer that compiles a field assignment as a
/// plain sub-expression scopes the rebind away. Reaching the closed rendering
/// is reported through `report_threaded_ir_verify_errors` (a hard failure in
/// debug and CI, an `internal:` warning in release).
#[cfg(debug_assertions)]
#[test]
#[should_panic(expected = "field-assignment state rebind trapped in a closed expression")]
fn test_field_assignment_closed_is_reported_as_an_escaping_state_rebind() {
    let mut generator = CoreErlangGenerator::new("test");
    let value = Expression::Literal(Literal::Integer(42), s());
    // Debug: the shared reporter's `debug_assert!` fires.
    let _ = generator.generate_field_assignment("count", &value);
}

/// The release half of the same check: an `internal:` verifier warning, no panic.
#[cfg(not(debug_assertions))]
#[test]
fn test_field_assignment_closed_is_reported_as_an_escaping_state_rebind() {
    let mut generator = CoreErlangGenerator::new("test");
    let value = Expression::Literal(Literal::Integer(42), s());
    generator
        .generate_field_assignment("count", &value)
        .unwrap();
    assert!(
        generator
            .take_codegen_warnings()
            .iter()
            .any(|d| d.message.contains("trapped in a closed expression")),
        "release builds report an internal warning"
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
    let output = closed_write(&mut generator, &value);
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
fn test_field_assignment_closed_class_var_is_an_in_place_put_answering_the_value() {
    // ADR 0130 §2: a class-method field write is a `put` into the class
    // process's dictionary that answers the assigned value; no `ClassVars{N}`
    // rebinding and no shadow write.
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_in_class_method(true);
    generator.class_var_names_mut().insert("total".to_string());
    let value = Expression::Literal(Literal::Integer(42), s());
    let output = generator
        .generate_field_assignment("total", &value)
        .unwrap()
        .to_pretty_string();
    assert!(
        output.starts_with("let _CVVal1 = 42 in case call 'erlang':'get'({'$bt_class_vars', "),
        "the value is evaluated first, then the class key is probed. Got: {output}"
    );
    assert!(
        output.contains("call 'erlang':'put'({'$bt_class_vars', ")
            && output.contains("call 'maps':'put'('total', _CVVal1, "),
        "class-var write should put the new map back in place. Got: {output}"
    );
    assert!(
        output.contains("call 'beamtalk_class_vars':'put'(ClassSelf, 'total', _CVVal1)"),
        "a miss falls back to the runtime helper. Got: {output}"
    );
    assert!(
        !output.contains("ClassVars") && !output.contains("State"),
        "class-var write must never reference ClassVars or State. Got: {output}"
    );
}
