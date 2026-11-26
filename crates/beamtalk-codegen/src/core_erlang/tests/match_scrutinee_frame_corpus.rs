// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5289: an actor `match:` whose scrutinee advances `State`, as a statement
//! of every frame kind.
//!
//! `items isNil ifTrue: [nil] ifFalse: [items first match: [_ -> items > 0
//! ifTrue: [self.total := items]]]` in an actor instance method panicked the
//! debug verifier (`NonLinearVersion` / `UnboundVersion State1`). The arm
//! statement lowered the `match:` as a raw `{Value, State}` tuple
//! (`generate_match`), which compiled the sent scrutinee's `SyncedCall`
//! inside the tuple's own document. The `State` version that call binds was
//! out of the arm frame's sight, so the arm's `State` extraction off the tuple
//! read a version no `Bind` of the frame produced. The emitted Core Erlang was
//! right (the scrutinee's `StateAcc1` is in scope inside the tuple), so this
//! was a verifier false positive, not a miscompile.
//!
//! **Invariant** (every scope kind, BT-4000's): a statement whose evaluation
//! produces a `State` version binds it in the frame the statement runs in,
//! before anything reads it. A `match:`'s scrutinee runs first and
//! unconditionally, like a conditional's receiver, so it is the construct's
//! sequenced operand (`sequenced_construct_operand`): its prelude is spliced
//! into the caller's frame (`conditional_receiver_operand`) at the method
//! root, in a conditional arm, a `Letrec` loop body, a fold body and an
//! `on:do:`/`ensure:` arm alike.
//!
//! [`match_scrutinee_frame_corpus_compiles`] proves it over scrutinee x arms x
//! arm write x position x frame x actor context: every row passes the debug
//! verifier and compiles through `erlc`.
//! [`local_assignment_scrutinee_corpus_compiles`] and
//! [`repl_local_assignment_scrutinee_matches_verify`] cover the other trigger
//! of the same splice (a scrutinee holding a local assignment, BT-3910) in
//! every context: actor, class, value-type and plain-object methods and the
//! REPL.
//! [`match_scrutinee_frame_corpus_reports_the_pre_fix_lowering`] shows the
//! verifier catches the pre-fix lowering ([`Fault::ReceiverPreludeInTuple`])
//! over the same rows.

use super::loop_body_conditional_receiver_corpus::{
    ARMS, HELPERS, LOOPS, PRELUDES, RECEIVERS, passes, phase0_rejects,
};
use super::match_scrutinee_state_corpus::INLINE_HEADERS;
use super::while_condition_binding_corpus::repl_report;
use super::{assert_all_compile_through_erlc, assert_parses, codegen};
#[cfg(debug_assertions)]
use crate::core_erlang::fault::Fault;

/// `match:` scrutinees whose evaluation advances `State`: BT-4000's
/// self-send receivers, and the issue's send to a parameter (`xs first`, a
/// dynamic send that runs in a `SyncedCall` window).
fn scrutinees() -> Vec<&'static str> {
    let mut out = RECEIVERS.to_vec();
    out.extend(["(xs first)", "xs first"]);
    out
}

/// The `match:` over a scrutinee `{R}` and an arm body `{B}`: a lone wildcard
/// arm, a type arm before a wildcard (a chain-lowered `match:`), and the
/// issue's nested `ifTrue:` in the arm.
const MATCHES: &[&str] = &[
    "{R} match: [_ -> {B}]",
    "{R} match: [b :: True -> {B}; _ -> nil]",
    "{R} match: [_ -> n > 0 ifTrue: [{B}]]",
];

/// The `match:` `{M}` in each position of the frame's statement `{S}`: a bare
/// statement, an assigned local, a field write's right-hand side (the arm's C6
/// -> C1 reroute) and a destructuring statement's (`{P}`, a `match:`
/// answering a two-element array). The last two mirror BT-4000's
/// `VALUE_POSITIONS` for conditionals.
const POSITIONS: &[&str] = &[
    "{M}",
    "r := {M}",
    "self.f := ({M}) printString size",
    "#(a, b) := {P}. self.f := a + b",
];

/// The arm writes of BT-4000's `ARMS` a `match:` arm may make: nothing
/// (`ARMS[0]`) and a field (`ARMS[2]`). `ARMS[1]` writes an outer local, which
/// a `match:` arm may not (BT-3875, rejected before codegen).
const ARM_WRITES: &[usize] = &[0, 2];

const HEADER: &str = "Actor subclass: MatchScrutineeFrameProbe\n  state: f = 0\n";

/// Every `(name, method body)` row the Phase 0 check accepts.
fn rows() -> Vec<(String, String)> {
    let mut out = Vec::new();
    for (l, lp) in LOOPS.iter().enumerate() {
        for (r, scrutinee) in scrutinees().iter().enumerate() {
            for (m, shape) in MATCHES.iter().enumerate() {
                for &a in ARM_WRITES {
                    let arm = ARMS[a];
                    let matched = shape.replace("{R}", scrutinee).replace("{B}", arm);
                    let pair = format!(
                        "{scrutinee} match: [_ -> n > 0 ifTrue: [{arm}. #(1, 2)] ifFalse: [#(3, 4)]]"
                    );
                    for (p, position) in POSITIONS.iter().enumerate() {
                        let stmt = position.replace("{M}", &matched).replace("{P}", &pair);
                        let body = lp.replace("{S}", &stmt);
                        if phase0_rejects(&source(&method("", 0, &body))) {
                            continue;
                        }
                        out.push((format!("loop{l} scrut{r} match{m} pos{p} arm{a}"), body));
                    }
                }
            }
        }
    }
    out
}

fn method(prelude: &str, i: usize, body: &str) -> String {
    format!("  probe{i}: xs =>\n{prelude}    n := 3\n    {body}\n    n\n")
}

fn source(method: &str) -> String {
    format!("{HEADER}{HELPERS}{method}")
}

/// BT-5289: every row passes the debug verifier and compiles through `erlc`
/// (one run per context; bisected to the failing rows on failure).
#[test]
fn match_scrutinee_frame_corpus_compiles() {
    let rows = rows();
    assert!(
        rows.len() >= 1000,
        "only {} rows; the corpus has thinned out",
        rows.len()
    );
    for (_, body) in &rows {
        assert_parses(&source(&method("", 0, body)));
    }
    let mut failures = Vec::new();
    for (ctx, prelude) in PRELUDES {
        let methods: Vec<String> = rows
            .iter()
            .enumerate()
            .map(|(i, (_, body))| method(prelude, i, body))
            .collect();
        if passes(&source(&methods.concat())) {
            continue;
        }
        failures.extend(rows.iter().zip(&methods).filter_map(|((name, _), method)| {
            let single = source(method);
            (!passes(&single)).then(|| format!("{ctx} / {name}:\n{single}"))
        }));
    }
    assert!(
        failures.is_empty(),
        "{} match-scrutinee row(s) fail verified codegen:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// BT-5289: with the scrutinee prelude rendered into the `match:`'s tuple
/// again ([`Fault::ReceiverPreludeInTuple`]), the verifier reports
/// `UnboundVersion` for every row whose `match:` threads `State` (an arm that
/// writes a field, `ARMS[2]`) as a statement of a `Letrec` loop body, a
/// conditional arm or an `on:do:`/`ensure:` body: the frames the bug reached.
/// The fold body and the method root (`LOOPS[5]`, `LOOPS[6]`) are verified
/// with opaque version gaps, so the corpus run is their proof, as for
/// BT-4000's receivers.
#[cfg(debug_assertions)]
#[test]
fn match_scrutinee_frame_corpus_reports_the_pre_fix_lowering() {
    let mut missed = Vec::new();
    let mut checked = 0;
    for (name, body) in rows().into_iter().filter(|(name, _)| {
        !name.starts_with("loop5 ") && !name.starts_with("loop6 ") && name.ends_with(" arm2")
    }) {
        checked += 1;
        let src = source(&method("", 0, &body));
        let report = super::verifier_report(&src, Some(Fault::ReceiverPreludeInTuple));
        if !report
            .as_deref()
            .is_some_and(|r| r.contains("UnboundVersion"))
        {
            missed.push(format!("{name}: {report:?}\n{body}"));
        }
    }
    assert!(checked >= 150, "only {checked} faulted rows checked");
    assert!(
        missed.is_empty(),
        "{} row(s) where the verifier missed the pre-fix lowering:\n{}",
        missed.len(),
        missed.join("\n")
    );
}

/// Scrutinees holding a local assignment (BT-3910's trigger of the splice,
/// context-independent): alone, as an operand, and assigning a second local.
const LOCAL_ASSIGNMENT_SCRUTINEES: &[&str] = &[
    "(t := t + 1)",
    "((t := t + 1) + u)",
    "((u := t + 5) max: 0)",
];

/// `match:` shapes over a local-assignment scrutinee `{R}` whose arms write
/// no field (so they run in every context): a value-answering literal arm
/// and a nested conditional arm.
const VALUE_MATCHES: &[&str] = &[
    "{R} match: [1 -> 10; _ -> 20]",
    "{R} match: [k :: Integer -> k > 0 ifTrue: [k] ifFalse: [0]; _ -> 0]",
];

/// The frames of a local-assignment scrutinee's `match:` statement `{S}`: the
/// method root, a conditional arm, a `Letrec` loop body, a fold body and an
/// `on:do:` body.
const LOCAL_FRAMES: &[&str] = &[
    "{S}",
    "(t >= 0) ifTrue: [{S}] ifFalse: [nil]",
    "1 to: 2 do: [:i | {S}]",
    "#(1, 2) do: [:e | {S}]",
    "[{S}] on: Error do: [:err | 0]",
];

/// BT-5289: a `match:` whose scrutinee holds a local assignment is sequenced
/// into its statement's frame in every context, not only an actor instance
/// method. Every scrutinee x shape x position x frame x context row passes
/// the debug verifier and compiles through `erlc`.
#[test]
fn local_assignment_scrutinee_corpus_compiles() {
    let actor = "Actor subclass: Ctr\n  state: f = 0\n\n  bump => 1\n\n  run =>\n";
    let mut rows = Vec::new();
    for header in std::iter::once(&actor).chain(INLINE_HEADERS) {
        for scrutinee in LOCAL_ASSIGNMENT_SCRUTINEES {
            for shape in VALUE_MATCHES {
                let m = shape.replace("{R}", scrutinee);
                for position in ["{M}", "r := {M}"] {
                    for frame in LOCAL_FRAMES {
                        let stmt = frame.replace("{S}", &position.replace("{M}", &m));
                        let src = format!(
                            "{header}    t := 0\n    u := 0\n    r := 0\n    {stmt}\n    {{t, u, r}}\n"
                        );
                        assert_parses(&src);
                        if phase0_rejects(&src) {
                            continue;
                        }
                        rows.push((src.clone(), codegen(&src)));
                    }
                }
            }
        }
    }
    assert!(rows.len() >= 200, "only {} rows", rows.len());
    assert_all_compile_through_erlc(&rows);
}

/// BT-5289: the REPL form of [`local_assignment_scrutinee_corpus_compiles`]:
/// a `match:` over a local-assignment scrutinee lowers with a silent verifier.
#[test]
fn repl_local_assignment_scrutinee_matches_verify() {
    let failures: Vec<String> = LOCAL_ASSIGNMENT_SCRUTINEES
        .iter()
        .flat_map(|scrutinee| {
            VALUE_MATCHES.iter().flat_map(move |shape| {
                let m = shape.replace("{R}", scrutinee);
                [
                    format!("t := 0. u := 0. {m}"),
                    format!("t := 0. u := 0. r := {m}"),
                ]
            })
        })
        .filter_map(|row| repl_report(&row, None).map(|r| format!("{row}: {r}")))
        .collect();
    assert!(
        failures.is_empty(),
        "{} REPL local-assignment scrutinee row(s) fail:\n{}",
        failures.len(),
        failures.join("\n")
    );
}
