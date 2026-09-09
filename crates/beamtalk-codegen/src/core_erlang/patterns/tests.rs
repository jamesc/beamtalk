// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use crate::core_erlang::CoreErlangGenerator;
use beamtalk_core::ast::{
    Expression, Identifier, Literal, MapPatternKey, MapPatternPair, MatchArm, MessageSelector,
    Pattern,
};
use beamtalk_core::source_analysis::Span;

fn s() -> Span {
    Span::new(0, 0)
}

// ── ADR 0107 Phase A: `Pattern::Type` codegen ────────────────────────────

/// Builds a top-level `binding :: class -> body` match arm.
fn type_arm(binding: &str, class: &str, body: Expression) -> MatchArm {
    MatchArm::new(
        Pattern::Type {
            binding: Identifier::new(binding, s()),
            class: Identifier::new(class, s()),
            span: s(),
        },
        body,
        s(),
    )
}

/// Builds a wildcard `_ -> body` fallthrough arm.
fn wildcard_arm(body: Expression) -> MatchArm {
    MatchArm::new(Pattern::Wildcard(s()), body, s())
}

fn ident_expr(name: &str) -> Expression {
    Expression::Identifier(Identifier::new(name, s()))
}

fn int_expr(n: i64) -> Expression {
    Expression::Literal(Literal::Integer(n), s())
}

#[test]
fn test_type_pattern_string_uses_is_binary() {
    let mut generator = CoreErlangGenerator::new("test");
    let value = ident_expr("x");
    let arms = vec![
        type_arm("s", "String", ident_expr("s")),
        wildcard_arm(int_expr(0)),
    ];
    let output = generator
        .generate_match(&value, &arms)
        .unwrap()
        .to_pretty_string();
    assert!(
        output.contains("'erlang':'is_binary'"),
        "String pattern should test is_binary. Got: {output}"
    );
}

#[test]
fn test_type_pattern_integer_uses_is_integer() {
    let mut generator = CoreErlangGenerator::new("test");
    let value = ident_expr("x");
    let arms = vec![
        type_arm("n", "Integer", ident_expr("n")),
        wildcard_arm(int_expr(0)),
    ];
    let output = generator
        .generate_match(&value, &arms)
        .unwrap()
        .to_pretty_string();
    assert!(
        output.contains("'erlang':'is_integer'"),
        "Integer pattern should test is_integer. Got: {output}"
    );
}

#[test]
fn test_type_pattern_float_uses_is_float() {
    let mut generator = CoreErlangGenerator::new("test");
    let value = ident_expr("x");
    let arms = vec![
        type_arm("f", "Float", ident_expr("f")),
        wildcard_arm(int_expr(0)),
    ];
    let output = generator
        .generate_match(&value, &arms)
        .unwrap()
        .to_pretty_string();
    assert!(
        output.contains("'erlang':'is_float'"),
        "Float pattern should test is_float. Got: {output}"
    );
}

#[test]
fn test_type_pattern_list_uses_is_list() {
    let mut generator = CoreErlangGenerator::new("test");
    let value = ident_expr("x");
    let arms = vec![
        type_arm("l", "List", ident_expr("l")),
        wildcard_arm(int_expr(0)),
    ];
    let output = generator
        .generate_match(&value, &arms)
        .unwrap()
        .to_pretty_string();
    assert!(
        output.contains("'erlang':'is_list'"),
        "List pattern should test is_list. Got: {output}"
    );
}

#[test]
fn test_type_pattern_dictionary_uses_nested_map_tag_check() {
    let mut generator = CoreErlangGenerator::new("test");
    let value = ident_expr("x");
    let arms = vec![
        type_arm("d", "Dictionary", ident_expr("d")),
        wildcard_arm(int_expr(0)),
    ];
    let output = generator
        .generate_match(&value, &arms)
        .unwrap()
        .to_pretty_string();
    assert!(
        output.contains("'erlang':'is_map'"),
        "Dictionary pattern should test is_map first. Got: {output}"
    );
    assert!(
        output.contains("'maps':'get'('$beamtalk_class'"),
        "Dictionary pattern should check '$beamtalk_class' via maps:get. Got: {output}"
    );
    assert!(
        output.contains("'undefined'"),
        "Dictionary pattern matches the *absence* of '$beamtalk_class' ('undefined'). Got: {output}"
    );
    // Must NOT use maps:get/3 inside a guard position (not guard-safe) —
    // it must appear only as a case scrutinee.
    assert!(
        !output.contains("when call 'maps':'get'"),
        "maps:get must not appear in guard position. Got: {output}"
    );
}

#[test]
fn test_type_pattern_symbol_excludes_nil_true_false() {
    let mut generator = CoreErlangGenerator::new("test");
    let value = ident_expr("x");
    let arms = vec![
        type_arm("sym", "Symbol", ident_expr("sym")),
        wildcard_arm(int_expr(0)),
    ];
    let output = generator
        .generate_match(&value, &arms)
        .unwrap()
        .to_pretty_string();
    assert!(
        output.contains("'erlang':'is_atom'"),
        "Symbol pattern should test is_atom. Got: {output}"
    );
    assert!(
        output.contains("'erlang':'=/='") && output.contains("'nil'"),
        "Symbol pattern should exclude 'nil'. Got: {output}"
    );
    assert!(
        output.contains("'true'") && output.contains("'false'"),
        "Symbol pattern should exclude 'true'/'false'. Got: {output}"
    );
}

#[test]
fn test_type_pattern_boolean_is_exact_literal_match_not_is_atom() {
    let mut generator = CoreErlangGenerator::new("test");
    let value = ident_expr("x");
    let arms = vec![
        type_arm("b", "Boolean", ident_expr("b")),
        wildcard_arm(int_expr(0)),
    ];
    let output = generator
        .generate_match(&value, &arms)
        .unwrap()
        .to_pretty_string();
    assert!(
        !output.contains("is_atom"),
        "Boolean pattern must not use a bare is_atom guard. Got: {output}"
    );
    assert!(
        output.contains("<'true'>") && output.contains("<'false'>"),
        "Boolean pattern should match literal 'true'/'false'. Got: {output}"
    );
}

#[test]
fn test_type_pattern_tagged_class_uses_beamtalk_class_map_check() {
    let mut generator = CoreErlangGenerator::new("test");
    let value = ident_expr("x");
    let arms = vec![
        type_arm("shape", "Circle", ident_expr("shape")),
        wildcard_arm(int_expr(0)),
    ];
    let output = generator
        .generate_match(&value, &arms)
        .unwrap()
        .to_pretty_string();
    assert!(
        output.contains("'erlang':'is_map'"),
        "Tagged-class pattern should test is_map first. Got: {output}"
    );
    assert!(
        output.contains("'maps':'get'('$beamtalk_class'"),
        "Tagged-class pattern should check '$beamtalk_class'. Got: {output}"
    );
    assert!(
        output.contains("'Circle'"),
        "Tagged-class pattern should match the class name atom. Got: {output}"
    );
}

#[test]
fn test_type_pattern_binds_value_to_binding_name() {
    let mut generator = CoreErlangGenerator::new("test");
    let value = ident_expr("x");
    let arms = vec![
        type_arm("path", "String", ident_expr("path")),
        wildcard_arm(int_expr(0)),
    ];
    let output = generator
        .generate_match(&value, &arms)
        .unwrap()
        .to_pretty_string();
    assert!(
        output.contains("Path = "),
        "binding should be bound via a `let` to the matched value. Got: {output}"
    );
}

#[test]
fn test_type_pattern_guard_scopes_over_binding() {
    let mut generator = CoreErlangGenerator::new("test");
    let value = ident_expr("x");
    // path :: String when: [path > 0] -> path
    let guard = Expression::MessageSend {
        receiver: Box::new(ident_expr("path")),
        selector: MessageSelector::Binary(">".into()),
        arguments: vec![int_expr(0)],
        is_cast: false,
        span: s(),
    };
    let arms = vec![
        MatchArm::with_guard(
            Pattern::Type {
                binding: Identifier::new("path", s()),
                class: Identifier::new("String", s()),
                span: s(),
            },
            guard,
            ident_expr("path"),
            s(),
        ),
        wildcard_arm(int_expr(0)),
    ];
    let output = generator
        .generate_match(&value, &arms)
        .unwrap()
        .to_pretty_string();
    assert!(
        output.contains("'erlang':'is_binary'"),
        "Guarded Type arm should still emit the class test. Got: {output}"
    );
    assert!(
        output.contains("'erlang':'>'"),
        "Guarded Type arm should emit the guard, referencing the bound name. Got: {output}"
    );
}

#[test]
fn test_match_mixes_primitive_type_arm_and_native_arm_in_one_case() {
    // A `match:` with a primitive `Type` arm followed by a plain
    // `Literal`/wildcard arm must compile into a single interleaved
    // chain (dispatch/interleaving layer, not just each strategy in
    // isolation).
    let mut generator = CoreErlangGenerator::new("test");
    let value = ident_expr("x");
    let arms = vec![
        type_arm("s", "String", ident_expr("s")),
        MatchArm::new(
            Pattern::Literal(Literal::Integer(42), s()),
            int_expr(1),
            s(),
        ),
        wildcard_arm(int_expr(0)),
    ];
    let output = generator
        .generate_match(&value, &arms)
        .unwrap()
        .to_pretty_string();
    assert!(
        output.contains("'erlang':'is_binary'"),
        "Should still contain the String arm's test. Got: {output}"
    );
    assert!(
        output.contains("<42>"),
        "Should still contain the native literal arm. Got: {output}"
    );
}

// ── destructure.rs: edge / error-path coverage ───────────────────────────────

fn var_pat(name: &str) -> Pattern {
    Pattern::Variable(Identifier::new(name, s()))
}

fn array_pat(elements: Vec<Pattern>) -> Pattern {
    Pattern::Array {
        elements,
        rest: None,
        list_syntax: false,
        span: s(),
    }
}

fn tuple_pat(elements: Vec<Pattern>) -> Pattern {
    Pattern::Tuple {
        elements,
        span: s(),
    }
}

fn map_pat(pairs: Vec<MapPatternPair>) -> Pattern {
    Pattern::Map { pairs, span: s() }
}

fn sym_pair(key: &str, value: Pattern) -> MapPatternPair {
    MapPatternPair {
        key: MapPatternKey::Symbol(key.into()),
        value,
        span: s(),
    }
}

// generate_destructure_bindings_from_var: exercises the delegating path through
// generate_pattern_extractions_from_var with a pre-evaluated RHS variable.
#[test]
fn destructure_bindings_from_var_tuple_emits_element_extractions() {
    let mut generator = CoreErlangGenerator::new("test");
    let pat = tuple_pat(vec![var_pat("a"), var_pat("b")]);
    let docs = generator
        .generate_destructure_bindings_from_var(&pat, "Tup1")
        .unwrap();
    let out: String = docs
        .iter()
        .map(beamtalk_cerl_doc::Document::to_pretty_string)
        .collect();
    assert!(
        out.contains("'erlang':'tuple_size'"),
        "should emit arity check. Got: {out}"
    );
    assert!(
        out.contains("'erlang':'element'(1"),
        "should extract element 1. Got: {out}"
    );
    assert!(
        out.contains("'erlang':'element'(2"),
        "should extract element 2. Got: {out}"
    );
}

// generate_destructure_bindings (which calls generate_destructure_extractions)
// with a Map pattern: exercises the Map arm at line 107 in destructure.rs.
#[test]
fn destructure_bindings_map_pattern_emits_map_get() {
    let mut generator = CoreErlangGenerator::new("test");
    let pat = map_pat(vec![sym_pair("key", var_pat("v"))]);
    let docs = generator
        .generate_destructure_bindings(&pat, &int_expr(1))
        .unwrap();
    let out: String = docs
        .iter()
        .map(beamtalk_cerl_doc::Document::to_pretty_string)
        .collect();
    assert!(
        out.contains("'erlang':'map_get'"),
        "Map destructuring should use erlang:map_get. Got: {out}"
    );
    assert!(
        out.contains("'key'"),
        "Map key atom should appear. Got: {out}"
    );
}

// Array Pattern::Literal: exercises the guard-check block in the Array branch.
#[test]
fn destructure_array_literal_element_emits_guard_check() {
    let mut generator = CoreErlangGenerator::new("test");
    let pat = array_pat(vec![Pattern::Literal(Literal::Integer(42), s())]);
    let (docs, _) = generator
        .generate_pattern_extractions_from_var(&pat, "Arr1", "let ", " in ")
        .unwrap();
    let out: String = docs
        .iter()
        .map(beamtalk_cerl_doc::Document::to_pretty_string)
        .collect();
    assert!(
        out.contains("'beamtalk_message_dispatch':'send'"),
        "should extract element via message dispatch. Got: {out}"
    );
    assert!(out.contains("'at:'"), "should use at: selector. Got: {out}");
    assert!(
        out.contains("<42>"),
        "should guard-check against the literal. Got: {out}"
    );
    assert!(
        out.contains("'badmatch'"),
        "mismatch should raise badmatch. Got: {out}"
    );
}

// Array Pattern::Wildcard: exercises line 204 — wildcard produces no binding.
#[test]
fn destructure_array_wildcard_element_produces_no_bindings() {
    let mut generator = CoreErlangGenerator::new("test");
    let pat = array_pat(vec![Pattern::Wildcard(s())]);
    let (docs, bound) = generator
        .generate_pattern_extractions_from_var(&pat, "Arr1", "let ", " in ")
        .unwrap();
    assert!(
        docs.is_empty(),
        "wildcard should emit no let-bindings. Got {docs:?}"
    );
    assert!(bound.is_empty(), "wildcard should produce no bound pairs");
}

// Tuple Pattern::Literal: exercises the guard-check block in the Tuple branch.
#[test]
fn destructure_tuple_literal_element_emits_guard_check() {
    let mut generator = CoreErlangGenerator::new("test");
    let pat = tuple_pat(vec![Pattern::Literal(Literal::Integer(7), s())]);
    let (docs, _) = generator
        .generate_pattern_extractions_from_var(&pat, "Tup1", "let ", " in ")
        .unwrap();
    let out: String = docs
        .iter()
        .map(beamtalk_cerl_doc::Document::to_pretty_string)
        .collect();
    assert!(
        out.contains("'erlang':'tuple_size'"),
        "should emit arity check. Got: {out}"
    );
    assert!(
        out.contains("'erlang':'element'"),
        "should extract the element. Got: {out}"
    );
    assert!(
        out.contains("<7>"),
        "should guard-check against the literal. Got: {out}"
    );
    assert!(
        out.contains("'badmatch'"),
        "mismatch should raise badmatch. Got: {out}"
    );
}

// Map Pattern::Wildcard value: wildcard value in a map pair skips binding.
#[test]
fn destructure_map_wildcard_value_produces_no_binding() {
    let mut generator = CoreErlangGenerator::new("test");
    let pat = map_pat(vec![sym_pair("key", Pattern::Wildcard(s()))]);
    let (docs, bound) = generator
        .generate_pattern_extractions_from_var(&pat, "Map1", "let ", " in ")
        .unwrap();
    assert!(
        docs.is_empty(),
        "map wildcard should emit no bindings. Got {docs:?}"
    );
    assert!(bound.is_empty(), "map wildcard should have no bound pairs");
}

// Array nested pattern: exercises the error arm for unsupported nested patterns in the Array branch.
#[test]
fn destructure_array_nested_pattern_returns_error() {
    let mut generator = CoreErlangGenerator::new("test");
    let nested = array_pat(vec![var_pat("x")]);
    let pat = array_pat(vec![nested]);
    let result = generator.generate_pattern_extractions_from_var(&pat, "Arr1", "let ", " in ");
    assert!(result.is_err(), "nested array pattern should be an error");
}

// Tuple nested pattern: exercises the error arm for unsupported nested patterns in the Tuple branch.
#[test]
fn destructure_tuple_nested_pattern_returns_error() {
    let mut generator = CoreErlangGenerator::new("test");
    let nested = tuple_pat(vec![var_pat("x")]);
    let pat = tuple_pat(vec![nested]);
    let result = generator.generate_pattern_extractions_from_var(&pat, "Tup1", "let ", " in ");
    assert!(result.is_err(), "nested tuple pattern should be an error");
}

// Map nested pattern value: exercises the error arm for unsupported nested patterns in the Map branch.
#[test]
fn destructure_map_nested_pattern_returns_error() {
    let mut generator = CoreErlangGenerator::new("test");
    let nested_val = tuple_pat(vec![var_pat("x")]);
    let pat = map_pat(vec![sym_pair("key", nested_val)]);
    let result = generator.generate_pattern_extractions_from_var(&pat, "Map1", "let ", " in ");
    assert!(
        result.is_err(),
        "unsupported map value pattern should be an error"
    );
}

// Unsupported outer pattern kind: exercises the catch-all error arm in generate_pattern_extractions_from_var.
#[test]
fn destructure_unsupported_outer_pattern_returns_error() {
    let mut generator = CoreErlangGenerator::new("test");
    let result = generator.generate_pattern_extractions_from_var(
        &Pattern::Wildcard(s()),
        "X",
        "let ",
        " in ",
    );
    assert!(
        result.is_err(),
        "a bare Wildcard at top level should be an unsupported-pattern error"
    );
}
