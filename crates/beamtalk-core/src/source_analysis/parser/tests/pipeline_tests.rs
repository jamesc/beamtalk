// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The `|>` pipeline (ADR 0145, BT-4514, BT-4516): the parser builds a
//! pipeline as the left-nested sends it means, with hand-nested spans, and
//! records the sends written with `|>` on the owning definition.

use super::*;
use crate::ast::normalize::peel_operand_parens;
use crate::ast::{CommentAttachment, Module};
use crate::ast_walker::walk_expression;
use crate::source_analysis::{is_input_complete, needs_blank_line_to_complete};
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

// --- The keyword-expression sites (BT-4516) ---

#[test]
fn pipeline_in_string_interpolation() {
    assert_same_tree(
        "\"total: {xs select: [:x | x] |> sum}\"",
        "\"total: {(xs select: [:x | x]) sum}\"",
    );
}

#[test]
fn pipeline_in_match_arm_bodies() {
    // Plain body, `^` body and assignment right-hand side.
    assert_same_tree(
        "x match: [1 -> a withX: 1 |> withY: 2; _ -> b]",
        "x match: [1 -> (a withX: 1) withY: 2; _ -> b]",
    );
    assert_same_tree(
        "x match: [1 -> ^a withX: 1 |> withY: 2; _ -> b]",
        "x match: [1 -> ^(a withX: 1) withY: 2; _ -> b]",
    );
    assert_same_tree(
        "x match: [1 -> y := a withX: 1 |> withY: 2; _ -> b]",
        "x match: [1 -> y := (a withX: 1) withY: 2; _ -> b]",
    );
}

#[test]
fn semicolon_still_ends_a_match_arm_after_a_pipeline() {
    let module = parse_ok("x match: [1 -> a foo |> bar; 2 -> b |> baz; _ -> c]");
    let Expression::Match { arms, .. } = &module.expressions[0].expression else {
        panic!("expected a match: {:?}", module.expressions[0].expression);
    };
    assert_eq!(arms.len(), 3);
}

#[test]
fn pipeline_in_map_values() {
    assert_same_tree(
        "#{#a => x foo: 1 |> bar, #b => 2}",
        "#{#a => (x foo: 1) bar, #b => 2}",
    );
}

#[test]
fn map_keys_still_need_parentheses() {
    assert!(!parse_err("#{x |> foo => 1}").is_empty());
    assert_same_tree("#{(x |> foo) => 1}", "#{(x foo) => 1}");
}

#[test]
fn pipeline_in_collection_elements() {
    assert_same_tree("#(a foo |> bar, b)", "#((a foo) bar, b)");
    assert_same_tree("#(a foo |> bar | rest)", "#((a foo) bar | rest)");
    assert_same_tree("#[a at: 1 |> bar, b]", "#[(a at: 1) bar, b]");
    assert_same_tree("{a at: 1 |> bar, b}", "{(a at: 1) bar, b}");
}

#[test]
fn keyword_arguments_still_need_parentheses() {
    assert_same_tree(
        "x at: k put: (v withA: 1 |> withB: 2)",
        "x at: k put: ((v withA: 1) withB: 2)",
    );
    // Without them, `|>` ends the argument and pipes the whole send.
    assert_same_tree("x at: k put: v |> foo", "(x at: k put: v) foo");
}

#[test]
fn every_keyword_expression_site_records_its_sends() {
    let cases = [
        ("\"{x foo |> bar}\"", "x foo |> bar"),
        ("x match: [_ -> a |> foo]", "a |> foo"),
        ("x match: [_ -> ^a |> foo]", "a |> foo"),
        ("x match: [_ -> y := a |> foo]", "a |> foo"),
        ("#{#k => a |> foo}", "a |> foo"),
        ("#(a |> foo)", "a |> foo"),
        ("#[a |> foo]", "a |> foo"),
        ("{a |> foo}", "a |> foo"),
    ];
    for (source, sent) in cases {
        let module = parse_ok(source);
        assert_eq!(recorded_sends(source, &module), vec![sent], "{source}");
    }
}

#[test]
fn method_body_sites_record_on_the_method() {
    let source = "Object subclass: A\n  \
                  foo: d => #(d |> keys, d |> size)\n";
    let module = parse_ok(source);
    assert_eq!(module.classes[0].methods[0].pipeline_sends.len(), 2);
    assert!(module.pipeline_sends.is_empty());
}

// --- Layout ---

#[test]
fn lines_starting_with_a_pipe_continue_a_method_body_statement() {
    let source = "Object subclass: A\n  \
                  foo: xs =>\n    \
                  xs select: [:x | x]\n      \
                  |> collect: [:x | x]\n      \
                  |> size\n  \
                  bar => 1\n";
    let module = parse_ok(source);
    let methods = &module.classes[0].methods;
    assert_eq!(methods.len(), 2);
    assert_eq!(methods[0].body.len(), 1);
    assert_eq!(methods[0].pipeline_sends.len(), 2);
}

// --- Errors ---

/// The diagnostic for `source` whose message contains `message`.
#[track_caller]
fn assert_error(source: &str, message: &str) -> Diagnostic {
    let diagnostics = parse_err(source);
    diagnostics
        .iter()
        .find(|d| d.message.contains(message))
        .unwrap_or_else(|| panic!("{source}: expected `{message}` in {diagnostics:?}"))
        .clone()
}

#[test]
fn pipe_without_a_message_is_an_error() {
    assert_error("x foo |> 10", "expected a message after `|>`");
}

#[test]
fn pipe_at_the_end_is_an_error() {
    assert_error("x foo |>", "expected a message after `|>`");
    assert_error("#(x foo |>)", "expected a message after `|>`");
}

#[test]
fn stage_starting_with_a_binary_message_is_an_error() {
    let diagnostic = assert_error(
        "x at: 1 |> + 1",
        "a pipeline stage cannot start with a binary message; write `(…) + …`",
    );
    assert_eq!(
        diagnostic.hint.as_deref(),
        Some("parenthesise the left side: `(…) + …`")
    );
    for source in ["x |> -> 1", "x |> >> #foo"] {
        assert_error(
            source,
            "a pipeline stage cannot start with a binary message",
        );
    }
}

#[test]
fn mixing_is_not_an_error_where_semicolon_ends_a_match_arm() {
    parse_ok("x match: [1 -> a foo |> bar; _ -> b foo]");
}

#[test]
fn pipeline_cannot_end_in_a_cast() {
    let help = "bind the pipeline's result to a local, then cast to it on the next line";
    // Top level, a stage with more messages, a method body and a block body.
    for source in [
        "x |> foo!",
        "x foo: 1 |> size + 1!",
        "Object subclass: A\n  run: x => x |> foo!\n",
        "[:x | x |> foo!]",
    ] {
        let diagnostic = assert_error(source, "Cast (!) has no return value");
        assert_eq!(diagnostic.hint.as_deref(), Some(help), "{source}");
    }
    // The pipeline is not silently turned into a cast.
    let (module, _) = parse(lex_with_eof("x |> foo!"));
    assert!(matches!(
        module.expressions[0].expression,
        Expression::MessageSend { is_cast: false, .. }
    ));
}

#[test]
fn a_send_to_a_parenthesised_pipeline_can_be_cast() {
    for source in ["(x |> foo) bar!", "x foo!"] {
        let module = parse_ok(source);
        assert!(
            matches!(
                module.expressions[0].expression,
                Expression::MessageSend { is_cast: true, .. }
            ),
            "{source}"
        );
    }
}

#[test]
fn statement_starting_with_a_pipe_is_an_error() {
    // After a cast terminator, and on a line of its own after a complete
    // statement (the REPL case).
    for source in ["a m1! |> m2", "x foo.\n|> bar"] {
        let diagnostic = assert_error(source, "a statement cannot start with `|>`");
        assert!(
            diagnostic.hint.as_deref().unwrap().contains("`( … )`"),
            "{diagnostic:?}"
        );
        // The stages are consumed with the error, not reported again.
        assert_eq!(parse_err(source).len(), 1, "{source}");
    }
}

// --- REPL input completeness ---

#[test]
fn line_ending_in_a_pipe_is_incomplete() {
    assert!(!is_input_complete("x foo |>"));
    assert!(!is_input_complete("x foo |>  // more"));
    assert!(is_input_complete("x foo |>\n  bar"));
    assert!(!needs_blank_line_to_complete("x foo |>"));
}

#[test]
fn leading_pipe_layout_needs_parentheses_in_the_repl() {
    // The line above is complete on its own, so the REPL would run it ...
    assert!(is_input_complete("x foo"));
    // ... and a leading-`|>` layout is wrapped in parentheses instead.
    assert!(!is_input_complete("(x foo\n  |> bar"));
    assert!(is_input_complete("(x foo\n  |> bar)"));
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
