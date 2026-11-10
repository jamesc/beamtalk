// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0145 (BT-4514): `|>` survives `beamtalk fmt` and a method's reflected
//! source. The tree holds only nested sends; the unparser prints `|>` before
//! each send the owning definition's record names.

use super::common::*;
use crate::ast::normalize::{
    peel_operand_parens, peel_operand_parens_recording, restore_operand_parens,
};
use crate::ast::{MethodDefinition, PipelineSends};
use crate::test_helpers::test_support::{parse_bt, without_spans};

const CLASS: &str = "\
Object subclass: Piper

  shifted: p => p withX: 5 |> withY: 10

  keysOf: d => d |> keys sorted

  head: xs => xs reversed |> sorted at: 1

  bumped: xs => xs |> size + 1

  matched: x => x foo |> match: [_ -> 0]

  inside: x => #(1) collect: [:e | e withX: 1 |> withY: 2]

  argument: x => x at: (x foo |> bar)

  operand: x => (x |> foo: 1) + 1
";

#[test]
fn fmt_keeps_pipelines_as_written() {
    assert_identity(CLASS);
}

#[test]
fn fmt_keeps_top_level_pipelines_as_written() {
    assert_identity("x := p withX: 5 |> withY: 10\nx |> keys sorted |> size + 1\n");
}

fn first_method(source: &str) -> MethodDefinition {
    parse_bt(source).classes[0].methods[0].clone()
}

#[test]
fn method_source_keeps_pipelines_as_written() {
    let module = parse_bt(CLASS);
    let class = &module.classes[0];
    let sources: Vec<String> = class.methods.iter().map(unparse_method).collect();
    for (method, written) in sources.iter().zip(CLASS.lines().skip(2).step_by(2)) {
        assert_eq!(method, written.trim_start());
    }
}

#[test]
fn method_source_keeps_pipelines_after_the_operand_paren_peel() {
    // Reflected source is unparsed from codegen's peeled tree with the
    // peeled parentheses restored; the record's spans still match.
    let mut module = parse_bt(CLASS);
    let written: Vec<String> = module.classes[0]
        .methods
        .iter()
        .map(unparse_method)
        .collect();
    let peeled = peel_operand_parens_recording(&mut module);
    for (method, written) in module.classes[0].methods.iter().zip(&written) {
        let mut restored = method.clone();
        restore_operand_parens(&mut restored, &peeled);
        assert_eq!(&unparse_method(&restored), written);
    }
}

#[test]
fn without_the_record_a_pipeline_prints_as_nested_sends() {
    let mut method = first_method(CLASS);
    method.pipeline_sends = PipelineSends::default();
    assert_eq!(
        unparse_method(&method),
        "shifted: p => (p withX: 5) withY: 10"
    );
}

#[test]
fn a_method_record_does_not_leak_into_module_code() {
    // The module's top-level pipeline and the method's are re-sugared from
    // their own records.
    assert_identity("Object subclass: A\n\n  m: p => p foo |> bar\n\nA new m: 1 |> size\n");
}

#[test]
fn comments_between_stages_round_trip() {
    // Each comment stays before the `|>` it was written before, and a
    // comment between stages breaks the pipeline one stage per line.
    assert_identity(
        "x foo  // after foo\n  // before bar\n  |> bar  // after bar\n  |> baz  // after baz\n",
    );
    assert_idempotent("x foo  // after foo\n  |> bar\n  |> baz\n");
}

#[test]
fn a_peeled_pipeline_argument_gets_its_parentheses_back() {
    // The operand peel removes `( … )` around a send argument; unparse must
    // add them back around a pipeline, or `|>` would pipe the whole send.
    let source = "x at: (a |> foo)\ny bar: (a foo: 1 |> baz) + (b |> c)\nz foo; at: (a |> d)\n\
                  (a |> foo) bar; baz\n(a |> foo) at: 1; baz\n";
    let mut module = parse_bt(source);
    peel_operand_parens(&mut module);
    let text = unparse_module(&module);
    let mut reparsed = parse_bt(&text);
    peel_operand_parens(&mut reparsed);
    assert_eq!(
        without_spans(&reparsed),
        without_spans(&module),
        "re-parse of the peeled unparse differs:\n{text}"
    );
}

#[test]
fn a_comment_after_a_trailing_pipe_is_kept() {
    // `x foo |>  // why` with the stage on the next line: the comment belongs
    // to that stage's entry, so fmt keeps it (before the `|>`) rather than
    // dropping it.
    let source = "x foo |>  // why\n  bar\n";
    let text = unparse_module(&parse_bt(source));
    assert!(text.contains("// why"), "comment dropped:\n{text}");
    assert!(text.contains("|> bar"), "pipeline lost:\n{text}");
    assert_idempotent(source);

    // A comment on its own line between a trailing `|>` and the stage.
    let own_line = "x foo |>\n  // why\n  bar\n";
    let text = unparse_module(&parse_bt(own_line));
    assert!(text.contains("// why"), "comment dropped:\n{text}");
    assert!(text.contains("|> bar"), "pipeline lost:\n{text}");
    assert_idempotent(own_line);
}

// --- Layout (BT-4517) ---

#[test]
fn fmt_puts_a_short_pipeline_on_one_line() {
    assert_eq!(
        format_source("x foo\n  |> bar baz\n  |> qux: 1\n").as_deref(),
        Some("x foo |> bar baz |> qux: 1\n")
    );
}

#[test]
fn fmt_breaks_a_long_pipeline_one_stage_per_line() {
    let long = "total := orders select: [:o | o isPaid] |> collect: [:o | o amount] \
                |> inject: 0 into: [:sum :each | sum + each]\n";
    assert_eq!(
        format_source(long).as_deref(),
        Some(
            "total := orders select: [:o | o isPaid]\n  |> collect: [:o | o amount]\n  \
             |> inject: 0 into: [:sum :each | sum + each]\n"
        )
    );
}

#[test]
fn a_comment_between_stages_breaks_the_whole_pipeline() {
    // The comment forces a break, and a broken pipeline puts every stage on
    // its own line, not only the one after the comment.
    assert_eq!(
        format_source("x foo |> bar  // why\n  |> baz |> qux\n").as_deref(),
        Some("x foo\n  |> bar  // why\n  |> baz\n  |> qux\n")
    );
}

#[test]
fn a_stage_spanning_lines_breaks_the_pipeline() {
    // A stage whose block argument holds several statements spans lines, so
    // the pipeline is broken and the next stage starts its own line.
    assert_eq!(
        format_source("xs |> collect: [:e |\n  e foo\n  e bar] |> size\n").as_deref(),
        Some("xs\n  |> collect: [:e |\n    e foo\n    e bar\n  ]\n  |> size\n")
    );
}

#[test]
fn a_broken_keyword_stage_keeps_its_first_keyword_after_the_pipe() {
    // Three keywords always break; the first stays on the `|>` line and the
    // rest align under it.
    assert_eq!(
        format_source("x |> at: 1 put: 2 ifAbsent: [0]\n").as_deref(),
        Some("x\n  |> at: 1\n     put: 2\n     ifAbsent: [0]\n")
    );
}

#[test]
fn the_break_decision_counts_the_column_the_pipeline_starts_at() {
    // 78 columns at the top level fit; indented under a method they do not.
    let stage = "|> inject: 0 into: [:sum :each | sum + each]";
    let flat = format!("allOrders select: [:o | o isPaid] {stage}");
    assert_eq!(flat.len(), 78);
    assert_identity(&format!("{flat}\n"));
    let method = format!("Object subclass: A\n\n  m: allOrders => {flat}\n");
    assert_eq!(
        format_source(&method),
        Some(format!(
            "Object subclass: A\n\n  m: allOrders =>\n    allOrders select: [:o | o isPaid]\n      {stage}\n"
        ))
    );
}
