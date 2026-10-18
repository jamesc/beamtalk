// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3863 (ADR 0131 §4, ADR 0111 §Verifier honesty): `FoldLocalsNotCarried`. A fold frame that
//! threads outer locals must end its lambda body in the accumulator that carries them (the tuple
//! repack, or the `StateAcc` map), a typed `ThreadedStmt::FoldAccumulator` built by every
//! `BodyKind` epilogue.
//!
//! BT-3839 found the write it guards dropped silently: the fold's gate threaded the locals a
//! nested loop wrote, the statement classifier did not see the nested loop as a producer, and the
//! `do:` body answered the loop's raw value as its accumulator (`erlc`-clean, the answer 0
//! instead of 6). BT-3839 made the two one function; [`LocalRebindFault::SplitFoldStatementGate`]
//! splits them again. Proved over a generated corpus, not the repro alone: every fold frame whose
//! body ends in a loop or fold writing `t` (the loop-body agreement corpus's constructs, as a
//! bare and a parenthesised statement, alone or inside a nested loop, in class, value-type and
//! actor methods) compiles with a silent verifier, and with the gate split the verifier reports
//! every one.

use super::local_producer_agreement::{CONSTRUCTS, body_sources, loop_body_agreement};
use super::local_producer_agreement_lookup::rejected;
use super::verifier_report;
use crate::core_erlang::threaded_expr::fault::{self, LocalRebindFault};

/// The BT-3839 repro, in each method kind.
fn repro_sources() -> [String; 3] {
    let body = "    t := 0\n    d := #{#a => 10}\n    ks := #(#a, #b, #c)\n    #(1, 2) do: [:j |\n      1 to: 3 do: [:i | d at: (ks at: i) ifAbsent: [t := t + j]]\n    ]\n    t\n";
    [
        format!("Object subclass: ReproClass\n  class go =>\n{body}"),
        format!("Value subclass: ReproValue\n  go =>\n{body}"),
        format!("Actor subclass: ReproActor\n  state: n = 0\n\n  go =>\n{body}"),
    ]
}

/// `do:`-kind fold frames whose body's last statement is `{s}`, or a loop or fold around it. Only
/// the `do:` tail can end in a raw value: under the same split, a `collect:` / `inject:into:` /
/// predicate body still ends in its correctly shaped epilogue, and the write the missed producer
/// drops (inside an opaque statement) is outside what `FoldLocalsNotCarried` checks; the
/// loop-body producer-agreement check catches it ([`OTHER_FOLD_FRAMES`]).
const FOLD_TAIL_FRAMES: &[&str] = &[
    "#(1, 2) do: [:k | {s}]",
    "#{#a => 1, #b => 2} keysAndValuesDo: [:k :w | {s}]",
    "#{#a => 1, #b => 2} doWithKey: [:k :w | {s}]",
    "#(1, 2) do: [:k | 1 to: 2 do: [:i | {s}]]",
    "#(1, 2) do: [:k | #{#a => 1} keysAndValuesDo: [:kk :w | {s}]]",
];

/// The construct as the fold body's statement, bare and parenthesised.
const TAIL_STATEMENTS: &[&str] = &["{c}", "({c})"];

/// `(case name, program)` for every tail frame x construct x statement x method kind that the
/// Phase 0 / §6 checks accept (a parenthesised handler is a block value, rejected there).
fn programs() -> Vec<(String, String)> {
    let mut out = Vec::new();
    for frame in FOLD_TAIL_FRAMES {
        for construct in CONSTRUCTS {
            for statement in TAIL_STATEMENTS {
                for (kind, src) in ["class", "value", "actor"]
                    .iter()
                    .zip(body_sources(frame, construct, statement))
                {
                    if !rejected(&src) {
                        out.push((format!("{kind} / {frame} / {statement} / {construct}"), src));
                    }
                }
            }
        }
    }
    out
}

#[test]
fn the_bt3839_repro_is_reported_when_the_fold_statement_gate_splits() {
    for src in repro_sources() {
        assert_eq!(verifier_report(&src, None), None, "{src}");
        let report = verifier_report(&src, Some(LocalRebindFault::SplitFoldStatementGate));
        assert!(
            report
                .as_deref()
                .is_some_and(|r| r.contains("FoldLocalsNotCarried")),
            "{report:?}\n{src}"
        );
    }
}

#[test]
fn every_fold_ending_in_a_producer_compiles_with_a_silent_verifier() {
    // `programs` keeps only what the Phase 0 / §6 checks accept, so any panic here (a verifier
    // report or a codegen crash) is a failure.
    let programs = programs();
    let failures: Vec<String> = programs
        .iter()
        .filter_map(|(name, src)| {
            verifier_report(src, None).map(|report| format!("{name}: {report}"))
        })
        .collect();
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
    assert!(
        programs.len() >= 400,
        "only {} programs compile: the corpus has thinned out",
        programs.len()
    );
}

#[test]
fn a_fold_ending_in_a_producer_is_reported_when_the_statement_gate_splits() {
    let mut unreported = Vec::new();
    for (name, src) in programs() {
        match verifier_report(&src, Some(LocalRebindFault::SplitFoldStatementGate)) {
            Some(report) if report.contains("FoldLocalsNotCarried") => {}
            Some(report) => unreported.push(format!("{name}: wrong report: {report}")),
            None => unreported.push(format!("{name}: compiled without a report")),
        }
    }
    assert!(unreported.is_empty(), "{}", unreported.join("\n\n"));
}

/// Fold frames of every other kind whose body holds `{s}` before its own tail: under the gate
/// split each still ends in a correctly shaped accumulator, so `FoldLocalsNotCarried` stays
/// silent and the dropped write is the producer-agreement check's to catch.
const OTHER_FOLD_FRAMES: &[&str] = &[
    "#(1, 2) collect: [:k | {s}. k]",
    "#(1, 2) inject: 0 into: [:a :k | {s}. a + k]",
    "#(1, 2) anySatisfy: [:k | {s}. false]",
    "#(1, 2) allSatisfy: [:k | {s}. true]",
    "#(1, 2) collect: [:k | 2 timesRepeat: [{s}]. k]",
];

#[test]
fn a_split_gate_in_any_other_fold_kind_is_caught_by_the_producer_agreement_check() {
    let mut compared = 0;
    let mut uncaught = Vec::new();
    for frame in OTHER_FOLD_FRAMES {
        for construct in CONSTRUCTS {
            for statement in TAIL_STATEMENTS {
                for src in body_sources(frame, construct, statement) {
                    match loop_body_agreement(&src) {
                        Ok(0) => continue,
                        Ok(_) => compared += 1,
                        Err(disagreement) => panic!("agrees unfaulted: {disagreement}"),
                    }
                    let faulted = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        fault::with(LocalRebindFault::SplitFoldStatementGate, || {
                            loop_body_agreement(&src)
                        })
                    }));
                    if matches!(faulted, Ok(Ok(_))) {
                        uncaught.push(format!("{frame} / {statement} / {construct}\n{src}"));
                    }
                }
            }
        }
    }
    assert!(uncaught.is_empty(), "{}", uncaught.join("\n\n"));
    assert!(
        compared >= 100,
        "only {compared} programs compared: the corpus has thinned out"
    );
}
