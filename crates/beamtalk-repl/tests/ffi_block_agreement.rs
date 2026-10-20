// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3986: the agreement between semantic analysis and codegen that makes
//! `CoreErlangGenerator::generate_erlang_interop_block`'s internal error
//! unreachable.
//!
//! Codegen refuses a block literal at an Erlang call site (an FFI argument, or
//! the callable of `do:`/`collect:`/`select:`) whose body writes a captured
//! local (`captured_mutations_for_block`). Semantic analysis is what keeps
//! such a block from reaching codegen (the ADR 0133 `Escape::Ffi` error and
//! the list-op `loop_threaded_locals` gate): three definitions in two crates.
//! This corpus is the test that they agree: over writing blocks in FFI and
//! list-op positions (plain, parenthesized, in a cascade, as a keyword
//! argument, in a conditional arm) in Actor, Value, Class and REPL contexts,
//! any program analysis accepts must never make codegen return that internal
//! error.

use beamtalk_codegen::core_erlang::{CodeGenError, CodegenOptions, generate_module};
use beamtalk_core::ast::{Expression, Module};
use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};
use beamtalk_repl::codegen::generate_repl_expressions;

/// Statements that put a block writing the captured local `t` (declared by
/// the context) at an Erlang call site.
const BODIES: &[&str] = &[
    "Erlang lists foreach: [:x | t := t + x] with: #(1)",
    "(Erlang lists) map: [:x | t := t + 1. x] with: #(1)",
    "Erlang lists map: ([:x | t := t + 1. x]) with: #(1)",
    "r := Erlang lists map: [:x | t := t + 1. x] with: #(1)\nr",
    "self note: (Erlang lists map: [:x | t := t + 1. x] with: #(1))",
    "Erlang erlang element: 1 of: (Erlang erlang make_tuple: 1 init: [:x | t := t + x])",
    "Erlang lists foreach: [:x | t := t + x] with: #(1); yourself",
    "flag ifTrue: [Erlang lists foreach: [:x | t := t + x] with: #(1)] ifFalse: [0]",
    "#(1, 2) do: [:x | t := t + x]",
    "#(1, 2) collect: [:x | t := t + x]",
    "#(1, 2) select: [:x | t := t + x. true]",
    "#(1, 2) do: ([:x | t := t + x])",
    "#(1, 2) do: [:x | t := t + x]; yourself",
    "self note: (#(1, 2) collect: [:x | t := t + x])",
    "flag ifTrue: [#(1, 2) select: [:x | t := t + x. true]] ifFalse: [0]",
    "#(1, 2) do: [:x | #(3) do: [:y | t := t + y]]",
    // Writes only the block's own locals: accepted, and codegen must agree.
    "Erlang lists map: [:x | u := x + 1. u] with: #(1)",
    "#(1, 2) collect: [:x | u := x + 1. u]",
];

fn contexts(body: &str) -> Vec<String> {
    let indented = body
        .lines()
        .map(|l| format!("    {l}"))
        .collect::<Vec<_>>()
        .join("\n");
    vec![
        format!(
            "Actor subclass: Ac\n  state: n = 0\n  note: x => x\n  probe: flag =>\n    t := 0\n{indented}\n"
        ),
        format!("Value subclass: Vt\n  note: x => x\n  probe: flag =>\n    t := 0\n{indented}\n"),
        format!(
            "Object subclass: Cl\n  class note: x => x\n  class probe: flag =>\n    t := 0\n{indented}\n"
        ),
    ]
}

fn errors(module: &Module, known: &[&str]) -> bool {
    analyse_full(module, AnalysisContext::default().with_known_vars(known))
        .diagnostics
        .iter()
        .any(|d| d.severity == Severity::Error)
}

fn is_the_internal_error(e: &CodeGenError) -> bool {
    matches!(e, CodeGenError::Internal(m) if m.contains("reached an Erlang call site"))
}

#[test]
fn analysis_accepts_no_writing_block_at_an_erlang_call_site() {
    let mut rejected = 0;
    let mut accepted = 0;
    let mut guarded = 0;
    let mut failures = Vec::new();

    for body in BODIES {
        // Module contexts: Actor, Value, Class.
        for src in contexts(body) {
            let (module, parse_diags) = parse(lex_with_eof(&src));
            if !parse_diags.is_empty() {
                continue;
            }
            if errors(&module, &[]) {
                rejected += 1;
                // Codegen alone (it skips no check of its own) shows the guard
                // is exercised by this corpus.
                if generate_module(&module, CodegenOptions::new("agree"))
                    .is_err_and(|e| is_the_internal_error(&e))
                {
                    guarded += 1;
                }
                continue;
            }
            accepted += 1;
            if let Err(e) = generate_module(&module, CodegenOptions::new("agree")) {
                if is_the_internal_error(&e) {
                    failures.push(format!("module: {e}\n{src}"));
                }
            }
        }

        // REPL: `t`, `u`, `flag` are bindings earlier inputs made.
        let src = (*body).to_string();
        let (module, parse_diags) = parse(lex_with_eof(&src));
        if !parse_diags.is_empty() {
            continue;
        }
        if errors(&module, &["t", "u", "flag", "self"]) {
            rejected += 1;
            continue;
        }
        accepted += 1;
        let expressions: Vec<Expression> = module
            .expressions
            .iter()
            .map(|s| s.expression.clone())
            .collect();
        if let Err(e) = generate_repl_expressions(&expressions, "agree") {
            if is_the_internal_error(&e) {
                failures.push(format!("repl: {e}\n{src}"));
            }
        }
    }

    assert!(
        failures.is_empty(),
        "analysis accepted a program codegen refuses as an internal error:\n{}",
        failures.join("\n---\n")
    );
    // Not vacuous: writing blocks are rejected, and the clean ones accepted.
    assert!(rejected >= 20, "only {rejected} programs were rejected");
    assert!(
        guarded > 0,
        "no rejected program reached the internal error"
    );
    assert!(accepted >= 6, "only {accepted} programs were accepted");
}
