// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4015: at the REPL a binding written in a `whileTrue:` / `whileFalse:`
//! condition is carried out of the condition closure (flips the
//! `PIN-BUG BT-4015` pin this file held).
//!
//! **Invariant** (every scope kind): a loop carries the union of the families
//! its condition and its body produce. At the REPL that family is the bindings
//! map, so a condition that writes a binding hands its map back to the loop
//! (`while_condition_has_carried_effects`), and its statements are lowered as
//! the condition frame's own `ThreadedStmt`s, value-position writes spliced in
//! (`generate_stateful_while_condition`). Before, the condition was a plain
//! closure answering only its boolean: a body that read the binding read the
//! closure's own variable (`erlc`: unbound variable), and otherwise the write
//! was lost and the loop never ended.
//!
//! [`repl_while_condition_binding_corpus_compiles`] proves it over condition
//! shape x body x loop x position: every input the Phase 0 check accepts
//! generates with the debug verifier on (a finding panics, including
//! `ConditionFamilyNotCarried` over the written binding) and compiles through
//! `erlc`. The repro's answers are pinned in the REPL protocol suite
//! (`tests/repl-protocol/cases/bt4015_while_condition_binding.btscript`); the
//! verifier catching the pre-fix lowering is shown in `beamtalk-codegen`'s
//! `while_condition_binding_corpus.rs`.

use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};
use beamtalk_repl::codegen::generate_repl_expressions;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::process::Command;

/// Conditions that write a binding (`CMP` is the loop's comparison): a plain
/// statement, a value-position write in a statement and in the boolean tail,
/// a write in a conditional arm, a fold, a counted loop and an `and:` block,
/// and a write whose value is a send.
const CONDITIONS: &[&str] = &[
    "cw := cw + 1. cw CMP 3",
    "x := (w := cw + 10). cw := cw + 1. cw CMP 3",
    "(cw := cw + 1) CMP 3",
    "cw := cw + 1. (w := cw) CMP 3",
    "(cw > 0) ifTrue: [w := cw]. cw := cw + 1. cw CMP 3",
    "#(1, 2) do: [:e | w := w + e]. cw := cw + 1. cw CMP 3",
    "1 to: 2 do: [:i | w := w + i]. cw := cw + 1. cw CMP 3",
    "cw := cw + 1. (cw CMP 3) and: [w := cw. true]",
    "cw := cw + 1. x := cw printString size. cw CMP 3",
    "cw := (cw + 1) max: 0. cw CMP 3",
];

/// Loop bodies: carrying nothing, reading the condition's write (the repro),
/// writing another binding, a conditional and a fold that write one, and a
/// value-position write.
const BODIES: &[&str] = &[
    "nil",
    "Timer sleep: 0",
    "ck := ck + cw",
    "ck := ck + 1",
    "(ck >= 0) ifTrue: [ck := ck + 1]",
    "#(1, 2) do: [:e | ck := ck + e]",
    "x := (ck := ck + cw)",
];

/// The loop selector and the comparison that ends it once `cw` reaches 3.
const LOOPS: &[(&str, &str)] = &[("whileTrue:", "<"), ("whileFalse:", ">=")];

/// Where the loop `{L}` sits in the REPL input. (A loop or fold body is not
/// a position: the Phase 0 check rejects a binding-writing `whileTrue:` there
/// whatever its condition.)
const POSITIONS: &[&str] = &[
    "{L}",
    "r := {L}",
    "(ck >= 0) ifTrue: [{L}]",
    "[{L}] on: Error do: [:err | 0]",
];

const KNOWN: &[&str] = &["cw", "ck", "w", "x", "r"];

fn inputs() -> Vec<String> {
    let mut out = Vec::new();
    for position in POSITIONS {
        for (selector, cmp) in LOOPS {
            for cond in CONDITIONS {
                for body in BODIES {
                    let lp = format!("[{}] {selector} [{body}]", cond.replace("CMP", cmp));
                    out.push(format!(
                        "cw := 0\nck := 0\nw := 0\nx := 0\n{}\n#[cw, ck, w, x]",
                        position.replace("{L}", &lp)
                    ));
                }
            }
        }
    }
    out
}

/// `Some(code)` for an input the Phase 0 check accepts; `None` when it rejects
/// it. Panics (the debug verifier, or a codegen error) fail the row.
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

/// `Ok(())` when `erlc` accepts every module (or is not installed).
fn erlc(modules: &[(String, String)]) -> Result<(), String> {
    let dir = beamtalk_core::test_helpers::unique_temp_dir("bt4015_repl");
    std::fs::create_dir_all(&dir).expect("temp dir");
    let mut files = Vec::new();
    for (name, code) in modules {
        let file = dir.join(format!("{name}.core"));
        std::fs::write(&file, code).expect("write core");
        files.push(file);
    }
    let output = Command::new("erlc")
        .arg("+from_core")
        .arg("-o")
        .arg(&dir)
        .args(&files)
        .output();
    let _ = std::fs::remove_dir_all(&dir);
    match output {
        Err(_) => Ok(()),
        Ok(o) if o.status.success() => Ok(()),
        Ok(o) => Err(format!(
            "{}{}",
            String::from_utf8_lossy(&o.stdout),
            String::from_utf8_lossy(&o.stderr)
        )),
    }
}

/// Generates and `erlc`-compiles every input, bisecting to the failing rows.
fn assert_inputs_compile(inputs: &[String], least: usize) {
    let mut failures = Vec::new();
    let mut compiled = Vec::new();
    for (i, src) in inputs.iter().enumerate() {
        let module = format!("bt4015_repl_{i}");
        match generate(src, &module) {
            Ok(Some(code)) => compiled.push((module, code, src.clone())),
            Ok(None) => {}
            Err(e) => failures.push(format!("{src}\n: {e}")),
        }
    }
    assert!(
        compiled.len() >= least,
        "only {} REPL row(s) generated; the corpus has thinned out",
        compiled.len()
    );
    let modules: Vec<(String, String)> = compiled
        .iter()
        .map(|(m, c, _)| (m.clone(), c.clone()))
        .collect();
    if erlc(&modules).is_err() {
        for (module, code, src) in &compiled {
            if let Err(e) = erlc(&[(module.clone(), code.clone())]) {
                failures.push(format!("{src}\n: erlc: {e}"));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "{} REPL while-condition binding row(s) fail:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

#[test]
fn repl_while_condition_binding_corpus_compiles() {
    assert_inputs_compile(&inputs(), 300);
}

/// The issue's repro and its `whileFalse:` twin, with the body reading the
/// condition's write (was `erlc`: unbound variable) and with a body that
/// carries nothing (was a silent infinite loop).
#[test]
fn bt4015_repros_compile() {
    let repros: Vec<String> = [
        "[cw := cw + 1. cw < 3] whileTrue: [ck := ck + cw]",
        "[cw := cw + 1. cw >= 3] whileFalse: [ck := ck + cw]",
        "[cw := cw + 1. cw < 3] whileTrue: [nil]",
        "[cw := cw + 1. cw >= 3] whileFalse: [nil]",
    ]
    .iter()
    .map(|lp| format!("cw := 0\nck := 0\n{lp}\n#[cw, ck]"))
    .collect();
    assert_inputs_compile(&repros, repros.len());
}
