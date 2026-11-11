// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0145 wire check (BT-4514): the parser builds a `|>` pipeline as the
//! nested sends it means, so codegen compiles it exactly as the hand-nested
//! form, with no codegen change. Only the method's reflected source differs,
//! and it shows the `|>` as written.

use super::paren_transparency::{source_entry, without_reflected_text};
use crate::core_erlang::{CodegenOptions, generate_module_with_warnings};
use beamtalk_core::ast::Module;
use beamtalk_core::semantic_analysis::{
    AnalysisContext, analyse_full, lower_analysed_module_for_codegen,
};
use beamtalk_core::source_analysis::{lex_with_eof, parse};

/// Methods written with `|>`, and the reflected source each must show.
const PIPED: [(&str, &str); 4] = [
    ("shifted:", "shifted: p => p withX: 5 |> withY: 10"),
    ("keysOf:", "keysOf: d => d |> keys sort"),
    ("head:", "head: xs => xs reversed |> sort at: 1"),
    ("bumped:", "bumped: xs => xs |> size + 1"),
];

/// The same methods, hand-nested.
const NESTED: [&str; 4] = [
    "shifted: p => (p withX: 5) withY: 10",
    "keysOf: d => d keys sort",
    "head: xs => xs reversed sort at: 1",
    "bumped: xs => xs size + 1",
];

fn class_source(methods: impl IntoIterator<Item = &'static str>) -> String {
    let mut source = String::from("Object subclass: Piper\n");
    for method in methods {
        source.push_str("  ");
        source.push_str(method);
        source.push('\n');
    }
    source
}

fn parse_clean(source: &str) -> Module {
    let (module, diagnostics) = parse(lex_with_eof(source));
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    module
}

/// Core Erlang for `module` through self-sufficient codegen and through the
/// driver path (`analyse_full` and the lowering, then codegen).
fn both_paths(module: &Module) -> [(&'static str, String); 2] {
    let self_sufficient = generate_module_with_warnings(module, CodegenOptions::new("piper"))
        .expect("compiles")
        .code;
    let analysis = analyse_full(module, AnalysisContext::default());
    let mut lowered = module.clone();
    lower_analysed_module_for_codegen(&mut lowered, &analysis);
    let analysed = generate_module_with_warnings(
        &lowered,
        CodegenOptions::new("piper").with_analysis(analysis),
    )
    .expect("compiles")
    .code;
    [("self-sufficient", self_sufficient), ("analysed", analysed)]
}

#[test]
fn pipeline_compiles_as_the_hand_nested_form_and_reflects_as_written() {
    let piped = both_paths(&parse_clean(&class_source(
        PIPED.iter().map(|(_, source)| *source),
    )));
    let nested = both_paths(&parse_clean(&class_source(NESTED)));

    for ((path, piped_code), (_, nested_code)) in piped.iter().zip(&nested) {
        for (selector, source) in PIPED {
            assert!(
                piped_code.contains(&source_entry(selector, source)),
                "{path}: `{selector}` should reflect `{source}` as written. Got:\n{piped_code}"
            );
        }
        assert_eq!(
            without_reflected_text(piped_code),
            without_reflected_text(nested_code),
            "{path}: a pipeline must compile exactly as its hand-nested form"
        );
    }
}

/// BT-4517: a `ClassBuilder` block method (`#sel => [:self | …]`) is rebuilt
/// as a fresh method definition, but its `|>` sends were recorded on the
/// enclosing method. Its reflected source still shows them as written,
/// through both codegen paths, including a comment between stages and a
/// pipeline next to a peeled parenthesis.
#[test]
fn a_class_builder_block_method_reflects_its_pipeline_as_written() {
    let reflected = [
        "m => self foo |> bar baz |> qux: 1",
        "n: x => (x |> size) + 1",
        "o: x =>\n  x foo  // why\n    |> bar",
    ];
    let source = "Object subclass: Maker\n  class make =>\n    Object classBuilder name: #Made; \
         methods: #{#m => [:self | self foo |> bar baz |> qux: 1], \
         #n: => [:self :x | (x |> size) + 1], \
         #o: => [:self :x |\n      x foo  // why\n        |> bar\n    ]}; register\n";
    for (path, code) in both_paths(&parse_clean(source)) {
        for text in reflected {
            let entry = format!(
                "#{{{}}}#",
                beamtalk_cerl_doc::binary::binary_byte_segments(text)
            );
            assert!(
                code.contains(&entry),
                "{path}: the block method should reflect `{text}` as written. Got:\n{code}"
            );
        }
    }
}
