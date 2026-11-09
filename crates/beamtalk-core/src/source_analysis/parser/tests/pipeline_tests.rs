// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The `|>` pipeline (ADR 0145, BT-4514): the parser builds a pipeline as
//! the left-nested sends it means, with hand-nested spans, and records the
//! sends written with `|>` on the owning definition.

use super::*;
use crate::ast::normalize::peel_operand_parens;
use crate::ast::{CommentAttachment, Module};
use crate::ast_walker::walk_expression;
use crate::test_helpers::test_support::without_spans;

/// The top-level expressions of `source`, peeled of redundant operand
/// parentheses (so a hand-nested `(a m1) m2` compares as `a m1 m2`), with
/// spans erased.
fn peeled_expressions(source: &str) -> String {
    let mut module = parse_ok(source);
    peel_operand_parens(&mut module);
    without_spans(&module.expressions)
}

#[track_caller]
fn assert_same_tree(pipeline: &str, hand_nested: &str) {
    assert_eq!(
        peeled_expressions(pipeline),
        peeled_expressions(hand_nested),
        "`{pipeline}` must build the tree of `{hand_nested}`"
    );
}

/// The source text each recorded `|>` send spans.
fn recorded_sends(source: &str, module: &Module) -> Vec<String> {
    module
        .pipeline_sends
        .spans()
        .map(|span| source[span.start() as usize..span.end() as usize].to_string())
        .collect()
}

// --- The tree is the hand-nested one ---

#[test]
fn unary_stage_builds_hand_nested_send() {
    assert_same_tree("x foo |> bar", "(x foo) bar");
}

#[test]
fn keyword_stages_build_hand_nested_sends() {
    assert_same_tree(
        "p withX: 5 |> withY: 10 |> withZ: 1",
        "((p withX: 5) withY: 10) withZ: 1",
    );
}

#[test]
fn head_without_message_is_the_first_receiver() {
    assert_same_tree("st |> withX: 1", "st withX: 1");
}

#[test]
fn multi_message_unary_stage_is_one_stage() {
    // `|> keys sorted` sends `keys` and then `sorted` to the previous value.
    assert_same_tree("x foo: 1 |> keys sorted", "((x foo: 1) keys) sorted");
}

#[test]
fn unary_then_keyword_stage_is_one_stage() {
    assert_same_tree("x foo: 1 |> foo at: 1", "((x foo: 1) foo) at: 1");
}

#[test]
fn unary_then_binary_stage_is_one_stage() {
    assert_same_tree("x foo: 1 |> size + 1", "((x foo: 1) size) + 1");
}

#[test]
fn unary_binary_keyword_stage_is_one_stage() {
    assert_same_tree(
        "x foo: 1 |> size + 1 max: 3",
        "(((x foo: 1) size) + 1) max: 3",
    );
}

#[test]
fn stage_may_start_on_the_line_after_a_trailing_pipe() {
    assert_same_tree("x foo: 1 |>\n  size", "(x foo: 1) size");
}

#[test]
fn stages_may_start_lines() {
    assert_same_tree(
        "x select: [:e | e] \n  |> collect: [:e | e]\n  |> size",
        "((x select: [:e | e]) collect: [:e | e]) size",
    );
}

#[test]
fn pipeline_at_every_parse_cascade_site() {
    // Assignment right-hand side, `^` value, parentheses, block body.
    assert_same_tree("y := x a: 1 |> b: 2", "y := (x a: 1) b: 2");
    assert_same_tree("^x a: 1 |> b: 2", "^(x a: 1) b: 2");
    assert_same_tree("(x a: 1 |> b: 2) size", "((x a: 1) b: 2) size");
    assert_same_tree("[:x | x a: 1 |> b: 2]", "[:x | (x a: 1) b: 2]");
}

// --- Spans (ADR 0145 § Spans) ---

/// Every send span in `source`'s top-level expressions, checking each
/// contains its receiver's span.
fn send_spans(module: &Module) -> Vec<Span> {
    let mut spans = Vec::new();
    for stmt in &module.expressions {
        walk_expression(&stmt.expression, &mut |expr| {
            if let Expression::MessageSend { receiver, span, .. } = expr {
                assert!(
                    span.contains(receiver.span()),
                    "send span {span:?} must contain its receiver's {:?}",
                    receiver.span()
                );
                spans.push(*span);
            }
        });
    }
    spans
}

#[test]
fn desugared_send_spans_are_unique_and_contain_their_receivers() {
    let source = "x m1: 2 |> keys sorted |> foo at: 1 |> size + 1 |> withY: 3";
    let module = parse_ok(source);
    let spans = send_spans(&module);
    // m1:, keys, sorted, foo, at:, size, +, withY:
    assert_eq!(spans.len(), 8);
    let mut unique = spans.clone();
    unique.sort_by_key(|s| (s.start(), s.end()));
    unique.dedup();
    assert_eq!(unique.len(), spans.len(), "duplicate send spans: {spans:?}");
}

#[test]
fn desugared_send_spans_are_hand_nested() {
    // Each send spans from the pipeline head to the end of its message.
    let source = "x m1: 2 |> keys sorted";
    let module = parse_ok(source);
    let texts: Vec<&str> = send_spans(&module)
        .iter()
        .map(|span| &source[span.start() as usize..span.end() as usize])
        .collect();
    assert_eq!(
        texts,
        vec!["x m1: 2 |> keys sorted", "x m1: 2 |> keys", "x m1: 2"]
    );
}

// --- The re-sugaring record ---

#[test]
fn records_the_send_each_pipe_introduces() {
    let source = "x m1: 2 |> keys sorted |> foo at: 1 |> withY: 3";
    let module = parse_ok(source);
    assert_eq!(
        recorded_sends(source, &module),
        vec![
            "x m1: 2 |> keys",
            "x m1: 2 |> keys sorted |> foo",
            "x m1: 2 |> keys sorted |> foo at: 1 |> withY: 3",
        ]
    );
}

#[test]
fn plain_sends_record_nothing() {
    let module = parse_ok("x foo bar. (x a: 1) b: 2. x foo; bar");
    assert!(module.pipeline_sends.is_empty());
}

#[test]
fn method_pipelines_are_recorded_on_the_method() {
    let source = "Object subclass: A\n  \
                  foo: p => p withX: 5 |> withY: 10\n  \
                  bar => 1\n\
                  A new foo: 1 |> size";
    let module = parse_ok(source);
    let class = &module.classes[0];
    assert_eq!(class.methods.len(), 2);
    let foo = &class.methods[0];
    assert_eq!(foo.pipeline_sends.len(), 1);
    let span = foo.pipeline_sends.spans().next().unwrap();
    assert_eq!(
        &source[span.start() as usize..span.end() as usize],
        "p withX: 5 |> withY: 10"
    );
    assert!(class.methods[1].pipeline_sends.is_empty());
    // The top-level pipeline is the module's, not the method's.
    assert_eq!(
        recorded_sends(source, &module),
        vec!["A new foo: 1 |> size"]
    );
}

#[test]
fn standalone_method_pipelines_are_recorded_on_the_method() {
    let source = "A >> foo: p => p |> keys sorted";
    let module = parse_ok(source);
    assert_eq!(module.method_definitions[0].method.pipeline_sends.len(), 1);
    assert!(module.pipeline_sends.is_empty());
}

#[test]
fn final_keyword_stage_stops_at_the_next_method_header() {
    let source = "Object subclass: A\n  \
                  foo: d => d |> at: #a\n  \
                  bar: x => x\n";
    let module = parse_ok(source);
    assert_eq!(module.classes[0].methods.len(), 2);
}

#[test]
fn comments_between_stages_attach_to_the_following_send() {
    let source = "x foo  // after foo\n  // before bar\n  |> bar\n  |> baz";
    let module = parse_ok(source);
    let mut entries: Vec<(String, CommentAttachment)> = module
        .pipeline_sends
        .spans()
        .map(|span| {
            (
                source[span.start() as usize..span.end() as usize].to_string(),
                module.pipeline_sends.comments(span).unwrap().clone(),
            )
        })
        .collect();
    entries.sort_by_key(|(text, _)| text.len());
    let (bar_text, bar) = &entries[0];
    assert!(bar_text.ends_with("|> bar"));
    assert_eq!(bar.trailing.as_ref().unwrap().content, "after foo");
    let leading: Vec<&str> = bar.leading.iter().map(|c| c.content.as_str()).collect();
    assert_eq!(leading, vec!["before bar"]);
    let (_, baz) = &entries[1];
    assert!(baz.trailing.is_none() && baz.leading.is_empty());
}

// --- Errors (the full set is BT-4516's) ---

#[test]
fn pipe_without_a_message_is_an_error() {
    let diagnostics = parse_err("x foo |> 10");
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("expected a message after `|>`")),
        "{diagnostics:?}"
    );
}

#[test]
fn stage_starting_with_a_binary_message_is_an_error() {
    let diagnostics = parse_err("x at: 1 |> + 1");
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("cannot start with a binary message")),
        "{diagnostics:?}"
    );
}

#[test]
fn mixing_cascade_and_pipeline_is_an_error() {
    for source in ["p withX: 5 |> withY: 10; withZ: 3", "p foo; bar |> baz"] {
        let diagnostics = parse_err(source);
        assert!(
            diagnostics.iter().any(|d| d
                .message
                .contains("cannot mix cascade `;` and pipeline `|>`")),
            "{source}: {diagnostics:?}"
        );
    }
}
