// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4589: at the REPL a `repeat` whose receiver block writes a binding
//! carries the write from one iteration to the next.
//!
//! **Invariant:** a literal `[b] repeat` is the loop `[true] whileTrue: [b]`.
//! The normalising pass (`ast::normalize::desugar_unary_loops`) rewrites it to
//! that shape before analysis and codegen, so both classify, thread and lower
//! one loop, not two that must agree. Before, codegen lowered `repeat` on a
//! path of its own that no state-threading recognizer knew: a receiver that
//! wrote a binding compiled as a stateful block (`fun (StateAcc)`) applied
//! with no arguments (`arity_mismatch`), and an `on:do:` around such a loop
//! expected a `{Result, Bindings}` tuple it never built.
//!
//! [`repl_repeat_agrees_with_while_true`] proves it over body x exit x
//! position: each `repeat` row gets the same Phase 0 verdict as its explicit
//! `[true] whileTrue:` twin and, when accepted, generates the same Core Erlang
//! (with the debug verifier on), which compiles through `erlc`. The answers
//! (exit by an error and by `^`) are pinned in the REPL protocol suite
//! (`tests/repl-protocol/cases/bt4589_repeat_binding.btscript`).

mod common;

use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};
use beamtalk_repl::codegen::generate_repl_expressions;
use common::erlc_batched_in_temp_dir;
use std::panic::{AssertUnwindSafe, catch_unwind};

/// Loop bodies (`EXIT` ends the loop): the issue's repro, a body that reads
/// one binding into another, writes in a conditional arm, a fold, a counted
/// loop and an `on:do:`, a value-position write, and a body that writes
/// nothing.
const BODIES: &[&str] = &[
    "cw := cw + 1. cw >= 3 ifTrue: [EXIT]",
    "cw := cw + 1. ck := ck + cw. cw >= 3 ifTrue: [EXIT]",
    "cw := cw + 1. (cw > 1) ifTrue: [ck := ck + cw]. cw >= 3 ifTrue: [EXIT]",
    "#(1, 2) do: [:e | ck := ck + e]. cw := cw + 1. cw >= 3 ifTrue: [EXIT]",
    "1 to: 2 do: [:i | ck := ck + i]. cw := cw + 1. cw >= 3 ifTrue: [EXIT]",
    "[ck := ck + 1] on: Error do: [:err | 0]. cw := cw + 1. cw >= 3 ifTrue: [EXIT]",
    "x := (cw := cw + 1). cw >= 3 ifTrue: [EXIT]",
    "cw >= 3 ifTrue: [EXIT]",
];

/// How the loop ends: an error, and a `^` (an input-ending return at the
/// REPL, BT-4288).
const EXITS: &[&str] = &["Error signal: \"done\"", "^cw"];

/// Where the loop `{L}` sits in the REPL input.
const POSITIONS: &[&str] = &[
    "{L}",
    "ck := 1. {L}",
    "r := {L}",
    "(ck >= 0) ifTrue: [{L}]",
    "[{L}] on: Error do: [:err | 0]",
    "#(1) do: [:e | {L}]",
];

const KNOWN: &[&str] = &["cw", "ck", "x", "r"];

/// `Ok(Some(code))` for an input the Phase 0 check accepts; `Ok(None)` when
/// it rejects it. Panics (the debug verifier) and codegen errors are `Err`.
fn generate(src: &str, module: &str) -> Result<Option<String>, String> {
    let (parsed, diags) = parse(lex_with_eof(src));
    if !diags.is_empty() {
        return Err(format!("parse: {diags:?}"));
    }
    let analysis = analyse_full(&parsed, AnalysisContext::default().with_known_vars(KNOWN));
    if analysis
        .diagnostics
        .iter()
        .any(|d| d.severity == Severity::Error)
    {
        return Ok(None);
    }
    let expressions: Vec<_> = parsed
        .expressions
        .into_iter()
        .map(|s| s.expression)
        .collect();
    match catch_unwind(AssertUnwindSafe(|| {
        generate_repl_expressions(&expressions, module)
    })) {
        Ok(Ok(code)) => Ok(Some(code)),
        Ok(Err(e)) => Err(format!("codegen: {e:?}")),
        Err(payload) => Err(format!(
            "panic: {}",
            payload
                .downcast_ref::<String>()
                .cloned()
                .or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_string()))
                .unwrap_or_default()
        )),
    }
}

/// Every row as `(repeat input, whileTrue: twin)`.
fn rows() -> Vec<(String, String)> {
    let mut out = Vec::new();
    for position in POSITIONS {
        for exit in EXITS {
            for body in BODIES {
                let body = body.replace("EXIT", exit);
                let input = |lp: &str| {
                    format!(
                        "cw := 0\nck := 0\nx := 0\n{}\n#[cw, ck, x]",
                        position.replace("{L}", lp)
                    )
                };
                out.push((
                    input(&format!("[{body}] repeat")),
                    input(&format!("[true] whileTrue: [{body}]")),
                ));
            }
        }
    }
    out
}

#[test]
fn repl_repeat_agrees_with_while_true() {
    let mut failures = Vec::new();
    let mut compiled = Vec::new();
    let mut rejected = 0;
    for (i, (repeat, twin)) in rows().iter().enumerate() {
        let module = format!("bt4589_repl_{i}");
        match (generate(repeat, &module), generate(twin, &module)) {
            (Ok(Some(code)), Ok(Some(twin_code))) => {
                if code != twin_code {
                    failures.push(format!(
                        "{repeat}\n: Core differs from `{twin}`\n--- repeat\n{code}\n--- whileTrue:\n{twin_code}"
                    ));
                }
                compiled.push((module, code, repeat.clone()));
            }
            (Ok(None), Ok(None)) => rejected += 1,
            (Ok(a), Ok(b)) => failures.push(format!(
                "{repeat}\n: Phase 0 verdict differs from `{twin}` (repeat accepted: {}, whileTrue: accepted: {})",
                a.is_some(),
                b.is_some()
            )),
            (Err(e), _) | (_, Err(e)) => failures.push(format!("{repeat}\n: {e}")),
        }
    }
    assert!(
        compiled.len() >= 60,
        "only {} REPL row(s) generated ({rejected} rejected); the corpus has thinned out",
        compiled.len()
    );
    let modules: Vec<(String, String)> = compiled
        .iter()
        .map(|(m, c, _)| (m.clone(), c.clone()))
        .collect();
    if erlc_batched_in_temp_dir("bt4589_repl", &modules).is_err() {
        for (module, code, src) in &compiled {
            if let Err(e) =
                erlc_batched_in_temp_dir("bt4589_repl", &[(module.clone(), code.clone())])
            {
                failures.push(format!("{src}\n: erlc: {e}"));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "{} REPL repeat row(s) fail:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// The issue's repro: was a stateful receiver applied with no arguments.
#[test]
fn bt4589_repro_threads_the_bindings() {
    let src = "cw := 0\n[cw := cw + 1. cw >= 3 ifTrue: [Error signal: \"done\"]] repeat";
    let code = generate(src, "bt4589_repro")
        .expect("generates")
        .expect("Phase 0 accepts the repro");
    assert!(
        code.contains("letrec 'while'/1 = fun (StateAcc)"),
        "the loop threads the bindings map:\n{code}"
    );
    assert!(
        !code.contains("apply _BodyFun"),
        "no stateful receiver applied with no arguments:\n{code}"
    );
    erlc_batched_in_temp_dir("bt4589_repro", &[("bt4589_repro".to_string(), code)])
        .expect("erlc compiles the repro");
}
