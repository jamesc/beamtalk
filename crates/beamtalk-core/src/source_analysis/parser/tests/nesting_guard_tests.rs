// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Stack-safety tests for every recursive parse path (BT-5181).
//!
//! Each test lexes and parses hostile input on a thread with a deliberately
//! small stack. A path that recurses without the nesting guard overflows that
//! stack, which aborts the whole test process, so a missing guard cannot pass
//! silently. The deeply nested inputs must report the "nesting is too deep"
//! diagnostic; the long flat chains must report the "too long" diagnostic and
//! leave an AST shallow enough to walk and drop.

use super::*;
use crate::ast::{Expression, Module};
use crate::ast_walker::{for_each_child, for_each_root_expr};

/// Stack for the parse thread: far below the 2 MiB a default test thread has,
/// so an unguarded path overflows quickly instead of happening to fit.
const SMALL_STACK: usize = 256 * 1024;

/// Nesting levels for the deep inputs: well past `MAX_NESTING_DEPTH`.
const DEEP: usize = 5_000;

/// Terms in the long flat chains (the issue's 100k-term chain).
const LONG: usize = 100_000;

/// Lexes and parses `source` on a small-stack thread; answers the
/// diagnostics and the greatest expression depth in the module.
fn parse_on_small_stack(source: String) -> (Vec<Diagnostic>, usize) {
    std::thread::Builder::new()
        .stack_size(SMALL_STACK)
        .spawn(move || {
            let tokens = lex_with_eof(&source);
            let (module, diagnostics) = parse(tokens);
            let depth = module_depth(&module);
            (diagnostics, depth)
        })
        .expect("spawn parse thread")
        .join()
        .expect("parse thread panicked")
}

/// The greatest expression depth in `module`, measured without recursion.
fn module_depth(module: &Module) -> usize {
    let mut roots: Vec<&Expression> = Vec::new();
    for_each_root_expr(module, |e| roots.push(e));
    let mut max = 0;
    let mut stack: Vec<(&Expression, usize)> = roots.into_iter().map(|e| (e, 1)).collect();
    while let Some((expr, depth)) = stack.pop() {
        max = max.max(depth);
        for_each_child(expr, &mut |child| stack.push((child, depth + 1)));
    }
    max
}

fn assert_too_deep(source: String) {
    let (diagnostics, _) = parse_on_small_stack(source);
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("nesting is too deep")),
        "expected the nesting-too-deep diagnostic, got: {:?}",
        diagnostics.iter().take(5).collect::<Vec<_>>()
    );
}

/// Asserts `diagnostics` reports the chain-too-long diagnostic exactly once:
/// the links past the limit are consumed without being reported again.
fn assert_reported_too_long_once(diagnostics: &[Diagnostic]) {
    let count = diagnostics
        .iter()
        .filter(|d| d.message.contains("too long"))
        .count();
    assert_eq!(
        count,
        1,
        "expected one chain-too-long diagnostic, got {count}: {:?}",
        diagnostics.iter().take(5).collect::<Vec<_>>()
    );
}

fn assert_too_long(source: String) {
    let (diagnostics, depth) = parse_on_small_stack(source);
    assert_reported_too_long_once(&diagnostics);
    assert!(
        depth <= MAX_EXPRESSION_DEPTH,
        "a capped chain must leave a shallow AST, got depth {depth}"
    );
}

/// `open` repeated `DEEP` times, then `middle`, then `close` repeated.
fn nested(open: &str, middle: &str, close: &str) -> String {
    open.repeat(DEEP) + middle + &close.repeat(DEEP)
}

// ── Collection literals ─────────────────────────────────────────────────────

#[test]
fn nested_list_literals_are_bounded() {
    assert_too_deep(nested("#(", "1", ")"));
}

#[test]
fn nested_array_literals_are_bounded() {
    assert_too_deep(nested("#[", "1", "]"));
}

#[test]
fn nested_tuple_literals_are_bounded() {
    assert_too_deep(format!("x := {}", nested("{", "1", "}")));
}

#[test]
fn nested_tuple_literal_statement_is_bounded() {
    assert_too_deep(nested("{", "1", "}"));
}

#[test]
fn nested_map_literal_values_are_bounded() {
    assert_too_deep(nested("#{#a => ", "1", "}"));
}

#[test]
fn nested_map_literal_keys_are_bounded() {
    assert_too_deep(nested("#{", "1", " => 1}"));
}

#[test]
fn nested_list_tails_are_bounded() {
    assert_too_deep(nested("#(1 | ", "#()", ")"));
}

#[test]
fn nested_literals_in_keyword_arguments_are_bounded() {
    assert_too_deep(nested("a at: #(", "1", ")"));
}

#[test]
fn nested_binary_constructions_are_bounded() {
    assert_too_deep(nested("<<(", "1", ")>>"));
}

// ── Operators ───────────────────────────────────────────────────────────────

#[test]
fn right_associative_power_chain_is_bounded() {
    assert_too_deep("2 ** ".repeat(DEEP) + "2");
}

// ── Type annotations ────────────────────────────────────────────────────────

#[test]
fn nested_generic_types_are_bounded() {
    assert_too_deep(format!("x :: {} := 1", nested("Foo(", "Integer", ")")));
}

#[test]
fn nested_parenthesised_types_are_bounded() {
    assert_too_deep(format!("x :: {} := 1", nested("(", "Integer", ")")));
}

#[test]
fn nested_shape_types_are_bounded() {
    assert_too_deep(format!("x :: {} := 1", nested("#{#a => ", "Integer", "}")));
}

#[test]
fn nested_parameter_type_in_method_header_is_bounded() {
    // The header lookahead (`skip_type_operand` and friends) recurses over
    // the parameter's type before the method is parsed.
    assert_too_deep(format!(
        "Object subclass: Foo\n  bar: x :: {} => x",
        nested("Foo(", "Integer", ")")
    ));
}

#[test]
fn nested_grouped_parameter_type_in_method_header_is_bounded() {
    assert_too_deep(format!(
        "Object subclass: Foo\n  bar: x :: {} => x",
        nested("(", "Integer", ")")
    ));
}

#[test]
fn nested_return_type_in_method_header_is_bounded() {
    assert_too_deep(format!(
        "Object subclass: Foo\n  bar -> {} => 1",
        nested("Foo(", "Integer", ")")
    ));
}

#[test]
fn nested_block_parameter_type_is_bounded() {
    assert_too_deep(format!("[:x :: {} | x]", nested("Foo(", "Integer", ")")));
}

#[test]
fn unclosed_shape_return_types_on_many_lines_do_not_overflow() {
    // `skip_shape_type` asks `is_method_definition_at` about each later
    // header line, which looks at that line's own `-> #{` return type: one
    // lookahead frame per line.
    let source = String::from("Object subclass: Foo\n") + &"  bar -> #{\n".repeat(DEEP);
    let (diagnostics, _) = parse_on_small_stack(source);
    assert!(!diagnostics.is_empty());
}

// ── Patterns ────────────────────────────────────────────────────────────────

#[test]
fn nested_tuple_patterns_are_bounded() {
    assert_too_deep(format!("x match: [{} -> 1]", nested("{", "a", "}")));
}

#[test]
fn nested_array_patterns_are_bounded() {
    assert_too_deep(format!("x match: [{} -> 1]", nested("#[", "a", "]")));
}

#[test]
fn nested_constructor_patterns_are_bounded() {
    assert_too_deep(format!(
        "x match: [{} -> 1]",
        nested("Result ok: {", "a", "}")
    ));
}

#[test]
fn nested_generic_type_patterns_do_not_overflow() {
    // A type pattern's generic arguments are rejected and skipped
    // iteratively rather than parsed, so this reports that instead.
    let (diagnostics, _) = parse_on_small_stack(format!(
        "x match: [v :: {} -> 1]",
        nested("Foo(", "Integer", ")")
    ));
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("generic type arguments")),
        "{:?}",
        diagnostics.iter().take(5).collect::<Vec<_>>()
    );
}

// ── Strings ─────────────────────────────────────────────────────────────────

#[test]
fn nested_string_interpolation_is_bounded() {
    // The lexer re-lexes each interpolation body with a sub-lexer.
    assert_too_deep(nested("\"a{", "1", "}a\""));
}

// ── Long flat chains ────────────────────────────────────────────────────────

#[test]
fn long_binary_chain_is_capped() {
    assert_too_long("a".to_string() + &" + a".repeat(LONG));
}

#[test]
fn long_unary_chain_is_capped() {
    assert_too_long("a".to_string() + &" foo".repeat(LONG));
}

#[test]
fn long_pipeline_is_capped() {
    assert_too_long("a".to_string() + &" |> foo".repeat(LONG));
}

#[test]
fn long_field_access_chain_is_capped() {
    assert_too_long("self".to_string() + &".a".repeat(LONG));
}

#[test]
fn long_type_intersection_chain_is_capped() {
    let (diagnostics, _) = parse_on_small_stack(format!("x :: A{} := 1", " & A".repeat(LONG)));
    assert_reported_too_long_once(&diagnostics);
}

#[test]
fn long_type_difference_chain_is_capped() {
    let (diagnostics, _) = parse_on_small_stack(format!("x :: A{} := 1", " \\ #a".repeat(LONG)));
    assert_reported_too_long_once(&diagnostics);
}

#[test]
fn nested_long_chains_are_bounded_by_total_depth() {
    // Each level is a chain just under the cap whose first operand is the
    // next level, so no single chain trips the length cap but the whole
    // expression is far deeper than any one chain.
    let per_level = MAX_EXPRESSION_DEPTH - 1;
    let mut source = String::new();
    for _ in 0..40 {
        source.push('(');
    }
    source.push('a');
    for _ in 0..40 {
        source.push_str(&" + a".repeat(per_level));
        source.push(')');
    }
    let (diagnostics, depth) = parse_on_small_stack(source);
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("too long") || d.message.contains("nesting is too deep")),
        "expected a depth diagnostic, got: {:?}",
        diagnostics.iter().take(5).collect::<Vec<_>>()
    );
    assert!(
        depth <= MAX_EXPRESSION_DEPTH,
        "the parser must not hand on an expression this deep, got depth {depth}"
    );
}

// ── Within the limits ───────────────────────────────────────────────────────

#[test]
fn moderately_nested_literals_still_parse() {
    let source = nested_n("#(", "1", ")", 30);
    let (diagnostics, _) = parse_on_small_stack(source);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
fn moderately_long_chains_still_parse() {
    let source = "a".to_string() + &" + a".repeat(500);
    let (diagnostics, _) = parse_on_small_stack(source);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

fn nested_n(open: &str, middle: &str, close: &str, n: usize) -> String {
    open.repeat(n) + middle + &close.repeat(n)
}
