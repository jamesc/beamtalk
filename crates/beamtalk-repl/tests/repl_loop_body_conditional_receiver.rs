// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4000: the REPL rows of the loop-body conditional-receiver matrix (the
//! actor rows are `beamtalk-codegen`'s `loop_body_conditional_receiver_corpus.rs`).
//!
//! A conditional whose receiver has a prelude of its own (a send, a binding
//! assignment, an `and:` / `or:` operand) as a loop or fold body statement at
//! the REPL, crossed with the conditional, an arm that writes nothing or a
//! binding, and the loop kind. Every input the Phase 0 check accepts generates
//! with the debug verifier on (a finding panics) and compiles through `erlc`.

use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};
use beamtalk_repl::codegen::generate_repl_expressions;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::process::Command;

const RECEIVERS: &[&str] = &[
    "(c isAlive: n)",
    "(c size)",
    "((t := t + 1) > 2)",
    "((c isAlive: n) and: [n > 1])",
    "((c size) or: [n > 1])",
];

const CONDITIONALS: &[&str] = &[
    "R ifTrue: [B]",
    "R ifFalse: [B]",
    "R ifTrue: [B] ifFalse: [nil]",
    "R ifNil: [B]",
    "R and: [B. true]",
    "R or: [B. false]",
];

const ARMS: &[&str] = &["Timer sleep: 0", "n := n - 1"];

const LOOPS: &[&str] = &[
    "[n > 0] whileTrue: [S]",
    "[n <= 0] whileFalse: [S]",
    "3 timesRepeat: [S]",
    "1 to: 3 do: [:i | S]",
    "1 to: 5 by: 2 do: [:i | S]",
    "#(1, 2) do: [:e | S]",
    "S",
];

fn inputs() -> Vec<String> {
    let mut out = Vec::new();
    for lp in LOOPS {
        for receiver in RECEIVERS {
            for cond in CONDITIONALS {
                for arm in ARMS {
                    let stmt = cond.replace('R', receiver).replace('B', arm);
                    out.push(lp.replace('S', &stmt));
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
    let known = ["c", "n", "t"];
    let analysis = analyse_full(&parsed, AnalysisContext::default().with_known_vars(&known));
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
    let dir = beamtalk_core::test_helpers::unique_temp_dir("bt4000_repl");
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

#[test]
fn repl_loop_body_conditional_receiver_corpus_compiles() {
    let mut failures = Vec::new();
    let mut compiled = Vec::new();
    for (i, src) in inputs().iter().enumerate() {
        let module = format!("bt4000_repl_{i}");
        match generate(src, &module) {
            Ok(Some(code)) => compiled.push((module, code, src.clone())),
            Ok(None) => {}
            Err(e) => failures.push(format!("{src}: {e}")),
        }
    }
    assert!(
        compiled.len() >= 200,
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
                failures.push(format!("{src}: erlc: {e}"));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "{} REPL loop-body conditional-receiver row(s) fail:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}
