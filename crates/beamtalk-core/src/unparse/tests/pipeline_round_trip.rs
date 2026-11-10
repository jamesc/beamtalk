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
    // Each comment stays before the `|>` it was written before. A stage with
    // a comment before it starts its own line; laying out the rest of a
    // broken pipeline one stage per line is BT-4517's.
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
}
