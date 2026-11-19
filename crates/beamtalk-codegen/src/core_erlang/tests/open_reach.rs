// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5041: every `*_open` lowering's "Reached by" note stays true.
//!
//! Each row compiles the input a note names, through the driver path every
//! real compile takes (`analyse_full`, `lower_analysed_module_for_codegen`,
//! then codegen with the analysis handed off), and asserts the lowering ran
//! (`reach::hit`). A row that stops reaching its function means the note went
//! stale or the function became dead; either way, fix the note or delete the
//! function (after a probe over the corpora, not on this test alone).

use crate::core_erlang::{CodeGenError, CodegenOptions, generate_module_with_warnings, reach};
use beamtalk_core::semantic_analysis::{
    AnalysisContext, analyse_full, lower_analysed_module_for_codegen,
};
use beamtalk_core::source_analysis::{lex_with_eof, parse};

/// Compiles `src` and returns the lowerings that ran, with the codegen result.
fn compile(
    src: &str,
) -> (
    std::collections::BTreeSet<&'static str>,
    Result<(), CodeGenError>,
) {
    let (module, _) = parse(lex_with_eof(src));
    let analysis = analyse_full(&module, AnalysisContext::default());
    let mut lowered = module.clone();
    lower_analysed_module_for_codegen(&mut lowered, &analysis);
    reach::take();
    let result = generate_module_with_warnings(
        &lowered,
        CodegenOptions::new("open_reach").with_analysis(analysis),
    )
    .map(|_| ());
    (reach::take(), result)
}

/// `(lowering, repo-relative file named in its "Reached by" note)`.
const FIXTURE_ROWS: &[(&str, &str)] = &[
    (
        "generate_vt_field_assignment_open",
        "stdlib/test/actor_coordination_test.bt",
    ),
    (
        "generate_vt_block_value_open",
        "stdlib/test/bt4889nested_local_block_value_in_field_block_test.bt",
    ),
    (
        "generate_vt_while_open",
        "stdlib/test/fixtures/bt3746stateful_while_value.bt",
    ),
    (
        "emit_vt_loop_open_extraction",
        "stdlib/test/fixtures/bt3746stateful_while_value.bt",
    ),
    (
        "generate_vt_counted_loop_open",
        "stdlib/test/fixtures/bt3947inline_block_loop_value.bt",
    ),
    (
        "generate_vt_foldl_list_op_open",
        "stdlib/test/fixtures/bt3956own_do_coll.bt",
    ),
    ("generate_value_type_do_open", "stdlib/src/list.bt"),
    (
        "generate_value_type_do_open_tuple",
        "stdlib/test/fixtures/do_accumulator_value.bt",
    ),
    (
        "generate_vt_conditional_open",
        "stdlib/test/value_type_conditional_field_arms_value_position_test.bt",
    ),
];

#[test]
fn each_open_lowering_is_reached_by_the_input_its_note_names() {
    let root = super::repo_root();
    let mut missed = Vec::new();
    for (lowering, file) in FIXTURE_ROWS {
        let src =
            std::fs::read_to_string(root.join(file)).unwrap_or_else(|e| panic!("{file}: {e}"));
        let (hits, _) = compile(&src);
        if !hits.contains(lowering) {
            missed.push(format!("{lowering} not reached by {file} (hit: {hits:?})"));
        }
    }
    assert!(
        missed.is_empty(),
        "stale \"Reached by\" notes:\n{}",
        missed.join("\n")
    );
}

/// `generate_vt_exception_construct_open`: a value-type method whose non-last
/// `on:do:` body holds a field-writing-arm conditional that also writes a local.
/// Codegen runs the lowering and then rejects the source, so the CLI does not
/// reject it earlier than codegen.
#[test]
fn the_exception_construct_open_is_reached_by_user_source_on_its_way_to_a_rejection() {
    let src = "TestCase subclass: OpenReachProbe\n  field: n = 0\n  field: m = 0\n\n  probe =>\n    t := 0\n    [self.m > 5 ifTrue: [t := 1. self.n := 5. t := t + 1] ifFalse: [t := 2]] on: Error do: [:e | 0]\n    t + self.n\n";
    let (hits, result) = compile(src);
    assert!(
        hits.contains("generate_vt_exception_construct_open"),
        "not reached: {hits:?}"
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::FieldAssignmentInUnsupportedBlock { .. })
        ),
        "expected the structured rejection, got {result:?}"
    );
}

/// BT-5041: a class method has no `*_open` lowering for a conditional or an
/// `on:do:`/`ensure:` that rebinds an outer local; the local-threading producer
/// takes them first, and `generate_class_method_non_last_expr` reports an internal
/// error for the two kinds. Pin that these shapes never reach that error: each parses and
/// compiles (so none can drift into `Internal`, or stop parsing and pass vacuously).
#[test]
fn class_method_local_threading_constructs_never_reach_the_internal_error_arm() {
    let bodies = [
        "t := 0\n    flag ifTrue: [#(1, 2) do: [:e | t := t + e]. t := t + 1] ifFalse: [t := 2]\n    t",
        "t := 0\n    flag ifTrue: [(flag and: [t > 0]) ifTrue: [t := 5] ifFalse: [t := 6]. t := t + 1] ifFalse: [t := 2]\n    t",
        "t := 0\n    flag ifTrue: [[t := t + 1] on: Error do: [:e | t := 0]. t := t + 1] ifFalse: [t := 2]\n    t",
        "t := 0\n    [flag ifTrue: [t := 1] ifFalse: [t := 2]. t := t + 1] on: Error do: [:e | t := 0]\n    t",
        "t := 0\n    [t := t + 1. flag ifTrue: [t := 3]] ensure: [t := t + 10]\n    t",
        "t := 0\n    flag match: [#yes -> [t := 1]; _ -> [t := 2]]\n    t",
        "t := 0\n    flag ifTrue: [flag match: [#yes -> [t := 1]; _ -> [t := 2]]] ifFalse: [t := 9]\n    t",
        "t := 0\n    u := 0\n    flag ifTrue: [t := 1. flag not ifTrue: [u := 1] ifFalse: [t := t + 1]] ifFalse: [u := 2]\n    t + u",
    ];
    for (i, body) in bodies.iter().enumerate() {
        let src =
            format!("Object subclass: ClassMethodReach{i}\n\n  class probe: flag =>\n    {body}\n");
        let (_, parse_diagnostics) = parse(lex_with_eof(&src));
        assert!(
            parse_diagnostics.is_empty(),
            "body {i} does not parse cleanly, so the pin would test nothing: {parse_diagnostics:?}\n{src}"
        );
        let (_, result) = compile(&src);
        // Every shape compiles today (through the producer route); a rejection with a
        // structured error would also be fine, but `Internal` never is.
        assert!(
            result.is_ok(),
            "class method body {i} no longer compiles: {result:?}\n{src}"
        );
    }
}
