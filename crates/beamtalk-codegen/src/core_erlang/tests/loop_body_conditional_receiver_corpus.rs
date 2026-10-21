// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4000: an actor conditional whose receiver advances `State`, as a loop
//! body statement.
//!
//! `[n > 0] whileTrue: [(self done) ifTrue: [Timer sleep: 0]]` in an actor
//! instance method panicked the debug verifier (`UnboundVersion State1`). The
//! conditional's producer compiled its receiver's self-send prelude into the
//! conditional's own tuple document, so the dispatch's `State` `Bind` was out
//! of the loop frame's sight: the conditional's `State` extraction read a
//! version no `Bind` of the frame produced, and the loop's recursive call
//! carried the pre-statement `State` (release builds dropped the self-send's
//! state change).
//!
//! **Invariant** (every scope kind): a statement whose evaluation produces a
//! `State` version binds it in the frame the statement runs in, before anything
//! reads it. A conditional's receiver runs first and unconditionally, so its
//! prelude is spliced into the caller's frame (`conditional_receiver_operand`,
//! the receiver-prelude splice BT-3910 introduced for local-assignment
//! receivers) at the method root, in a conditional arm, a `Letrec` loop body, a
//! fold body and an `on:do:`/`ensure:` arm alike.
//!
//! [`loop_body_conditional_receiver_corpus_compiles`] proves it over receiver
//! form x conditional x arm write x loop kind x actor context: every row passes
//! the debug verifier and compiles through `erlc`.
//! [`loop_body_conditional_receiver_corpus_reports_the_pre_fix_lowering`] shows
//! the verifier catches the pre-fix lowering
//! ([`LocalRebindFault::ReceiverPreludeInTuple`]) over the same rows.

use super::{assert_compiles_through_erlc, codegen};
use crate::core_erlang::threaded_expr::fault::LocalRebindFault;
use std::panic::{AssertUnwindSafe, catch_unwind};

const HELPERS: &str = "  done => false
  isAlive: k => k > 0
  maybe: k => nil
  + k => k > 0
";

/// Conditional receivers whose evaluation advances `State`: a unary, keyword
/// and binary self-send, and a self-send as the operand of `and:` / `or:`.
const RECEIVERS: &[&str] = &[
    "(self done)",
    "(self isAlive: n)",
    "(self + n)",
    "((self isAlive: n) and: [n > 1])",
    "((self done) or: [n > 1])",
];

/// The conditionals over a receiver `R` and an arm body `B`.
const CONDITIONALS: &[&str] = &[
    "R ifTrue: [B]",
    "R ifFalse: [B]",
    "R ifTrue: [B] ifFalse: [nil]",
    "R ifNil: [B]",
    "R and: [B. true]",
    "R or: [B. false]",
];

/// What the arm writes: nothing, an outer local, a field.
const ARMS: &[&str] = &["Timer sleep: 0", "n := n - 1", "self.f := self.f + 1"];

/// Where the conditional statement `S` runs: every `Letrec` loop kind
/// (`LOOPS[..5]`), a fold body, the method root, a conditional arm, an
/// `on:do:` body, an `ensure:` body, and a `Letrec` loop nested in a
/// conditional arm.
const LOOPS: &[&str] = &[
    "[n > 0] whileTrue: [S]",
    "[n <= 0] whileFalse: [S]",
    "3 timesRepeat: [S]",
    "1 to: 3 do: [:i | S]",
    "1 to: 5 by: 2 do: [:i | S]",
    "#(1, 2) do: [:e | S]",
    "S",
    "(n > 0) ifTrue: [S]",
    "[S] on: Error do: [:err | 0]",
    "[S] ensure: [nil]",
    "(n > 0) ifTrue: [3 timesRepeat: [S]]",
];

/// The actor contexts: a method whose first `State` version is the
/// conditional's receiver, and one that advances `State` first.
const PRELUDES: &[(&str, &str)] = &[("actor", ""), ("actor_field_write", "    self.f := 1\n")];

const HEADER: &str = "Actor subclass: LoopCondReceiverProbe\n  state: f = 0\n";

/// Every `(name, loop source)` row.
fn rows() -> Vec<(String, String)> {
    let mut out = Vec::new();
    for (l, lp) in LOOPS.iter().enumerate() {
        for (r, receiver) in RECEIVERS.iter().enumerate() {
            for (c, cond) in CONDITIONALS.iter().enumerate() {
                for (a, arm) in ARMS.iter().enumerate() {
                    let stmt = cond.replace('R', receiver).replace('B', arm);
                    out.push((
                        format!("loop{l} recv{r} cond{c} arm{a}"),
                        lp.replace('S', &stmt),
                    ));
                }
            }
        }
    }
    out
}

fn method(prelude: &str, i: usize, body: &str) -> String {
    format!("  probe{i} =>\n{prelude}    n := 3\n    {body}\n    n\n")
}

fn passes(src: &str) -> bool {
    catch_unwind(AssertUnwindSafe(|| {
        assert_compiles_through_erlc("test", &codegen(src));
    }))
    .is_ok()
}

/// BT-4000: every row passes the debug verifier and compiles through `erlc`
/// (one run per context; bisected to the failing rows on failure).
#[test]
fn loop_body_conditional_receiver_corpus_compiles() {
    let rows = rows();
    assert!(rows.len() >= 600, "the corpus has thinned out");
    let mut failures = Vec::new();
    for (ctx, prelude) in PRELUDES {
        let methods: Vec<String> = rows
            .iter()
            .enumerate()
            .map(|(i, (_, body))| method(prelude, i, body))
            .collect();
        if passes(&format!("{HEADER}{HELPERS}{}", methods.concat())) {
            continue;
        }
        failures.extend(rows.iter().zip(&methods).filter_map(|((name, _), method)| {
            let single = format!("{HEADER}{HELPERS}{method}");
            (!passes(&single)).then(|| format!("{ctx} / {name}:\n{single}"))
        }));
    }
    assert!(
        failures.is_empty(),
        "{} loop-body conditional-receiver row(s) fail verified codegen:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// Whether the ADR 0131/0133 Phase 0 check rejects `src` before codegen (an
/// arm's outer-local write in an operand position no construct threads).
fn phase0_rejects(src: &str) -> bool {
    use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
    use beamtalk_core::source_analysis::{DiagnosticCategory, Severity};
    let (module, _) =
        beamtalk_core::source_analysis::parse(beamtalk_core::source_analysis::lex_with_eof(src));
    analyse_full(&module, AnalysisContext::default())
        .diagnostics
        .iter()
        .any(|d| {
            d.severity == Severity::Error
                && matches!(
                    d.category,
                    Some(
                        DiagnosticCategory::UnmigratedLocalThreading
                            | DiagnosticCategory::Tier2BlockNoReturnChannel
                    )
                )
        })
}

/// The conditional `C` in value position inside the body statement: an
/// assigned local, a field write, a self-send argument, the last statement of
/// a nested block value, and a non-local return.
const VALUE_POSITIONS: &[&str] = &[
    "r := C",
    "self.f := (C) printString size",
    "self isAlive: (C) printString size",
    "x := 0. x := C. x",
    "(n > 5) ifTrue: [^ C]",
];

/// BT-4000: the conditional in value position (every [`VALUE_POSITIONS`]
/// wrapper) in every position of [`LOOPS`], over every receiver with
/// `ifTrue:ifFalse:` and each arm write. Every row passes the debug verifier
/// and compiles through `erlc`.
#[test]
fn loop_body_conditional_receiver_value_positions_compile() {
    let mut methods = Vec::new();
    let mut names = Vec::new();
    for (l, lp) in LOOPS.iter().enumerate() {
        for (r, receiver) in RECEIVERS.iter().enumerate() {
            for (a, arm) in ARMS.iter().enumerate() {
                let cond = format!("{receiver} ifTrue: [{arm}. 1] ifFalse: [2]");
                for (v, wrap) in VALUE_POSITIONS.iter().enumerate() {
                    let stmt = wrap.replace('C', &cond);
                    let m = method("", methods.len(), &lp.replace('S', &stmt));
                    if phase0_rejects(&format!("{HEADER}{HELPERS}{m}")) {
                        continue;
                    }
                    names.push(format!("loop{l} recv{r} arm{a} value{v}"));
                    methods.push(m);
                }
            }
        }
    }
    assert!(
        methods.len() >= 400,
        "only {} rows; the corpus has thinned out",
        methods.len()
    );
    let mut failures = Vec::new();
    if !passes(&format!("{HEADER}{HELPERS}{}", methods.concat())) {
        failures.extend(names.iter().zip(&methods).filter_map(|(name, method)| {
            let single = format!("{HEADER}{HELPERS}{method}");
            (!passes(&single)).then(|| format!("{name}:\n{single}"))
        }));
    }
    assert!(
        failures.is_empty(),
        "{} value-position row(s) fail verified codegen:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// BT-4000: with the receiver prelude rendered into the conditional's tuple
/// again ([`LocalRebindFault::ReceiverPreludeInTuple`]), the verifier reports
/// `UnboundVersion` for every row whose conditional is a statement of a
/// `Letrec` loop body, a conditional arm or an `on:do:`/`ensure:` body (the
/// frames the bug reached). The check that was missing already existed, and
/// it is what catches the pre-fix lowering, not the repro. The fold body and
/// the method root (`LOOPS[5]`, `LOOPS[6]`) are verified with opaque version
/// gaps, so the corpus run is their proof.
#[cfg(debug_assertions)]
#[test]
fn loop_body_conditional_receiver_corpus_reports_the_pre_fix_lowering() {
    let mut missed = Vec::new();
    for (name, body) in rows()
        .into_iter()
        .filter(|(name, _)| !name.starts_with("loop5 ") && !name.starts_with("loop6 "))
    {
        let src = format!("{HEADER}{HELPERS}{}", method("", 0, &body));
        let report = super::verifier_report(&src, Some(LocalRebindFault::ReceiverPreludeInTuple));
        if !report
            .as_deref()
            .is_some_and(|r| r.contains("UnboundVersion"))
        {
            missed.push(format!("{name}: {report:?}\n{body}"));
        }
    }
    assert!(
        missed.is_empty(),
        "{} row(s) where the verifier missed the pre-fix lowering:\n{}",
        missed.len(),
        missed.join("\n")
    );
}

/// The issue's repros (BT-4000), each with a silent verifier and through
/// `erlc`.
#[test]
fn bt4000_repros_compile() {
    for body in [
        "[n > 0] whileTrue: [(self done) ifTrue: [Timer sleep: 0]]",
        "3 timesRepeat: [(self done) ifTrue: [Timer sleep: 0]]",
        "[n > 0] whileTrue: [(self done) ifTrue: [n := 1]]",
        "[n > 0] whileTrue: [self done ifTrue: [3]]",
    ] {
        let src = format!("{HEADER}{HELPERS}{}", method("", 0, body));
        assert_eq!(super::verifier_report(&src, None), None, "{src}");
        assert!(passes(&src), "{src}");
    }
}
