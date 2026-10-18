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

use super::local_producer_agreement::{CONSTRUCTS, body_sources};
use super::local_producer_agreement_lookup::rejected;
use super::verifier_report;
use crate::core_erlang::threaded_expr::fault::LocalRebindFault;

/// The BT-3839 repro, in each method kind.
fn repro_sources() -> [String; 3] {
    let body = "    t := 0\n    d := #{#a => 10}\n    ks := #(#a, #b, #c)\n    #(1, 2) do: [:j |\n      1 to: 3 do: [:i | d at: (ks at: i) ifAbsent: [t := t + j]]\n    ]\n    t\n";
    [
        format!("Object subclass: ReproClass\n  class go =>\n{body}"),
        format!("Value subclass: ReproValue\n  go =>\n{body}"),
        format!("Actor subclass: ReproActor\n  state: n = 0\n\n  go =>\n{body}"),
    ]
}

/// `do:`-kind fold frames whose body's last statement is `{s}`, or a loop or fold around it.
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

/// Whether a compile's panic message is a verifier finding (rather than a structured rejection).
fn is_verifier_report(report: &str) -> bool {
    report.contains("ThreadedIr verify found")
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
    let mut accepted = 0;
    let failures: Vec<String> = programs()
        .into_iter()
        .filter_map(|(name, src)| match verifier_report(&src, None) {
            None => {
                accepted += 1;
                None
            }
            Some(report) if is_verifier_report(&report) => Some(format!("{name}: {report}")),
            Some(_) => None,
        })
        .collect();
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
    assert!(
        accepted >= 400,
        "only {accepted} programs compile: the corpus has thinned out"
    );
}

#[test]
fn a_fold_ending_in_a_producer_is_reported_when_the_statement_gate_splits() {
    let mut unreported = Vec::new();
    for (name, src) in programs() {
        if verifier_report(&src, None).is_some() {
            continue;
        }
        match verifier_report(&src, Some(LocalRebindFault::SplitFoldStatementGate)) {
            Some(report) if report.contains("FoldLocalsNotCarried") => {}
            Some(report) => unreported.push(format!("{name}: wrong report: {report}")),
            None => unreported.push(format!("{name}: compiled without a report")),
        }
    }
    assert!(unreported.is_empty(), "{}", unreported.join("\n\n"));
}
