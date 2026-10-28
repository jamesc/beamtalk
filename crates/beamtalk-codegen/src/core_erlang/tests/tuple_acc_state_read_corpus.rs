// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4178: a tuple-accumulator fold's lambda binds no state map, only a
//! tuple of locals, so every actor-state name its body resolves (a field
//! read, above all) must be the fold's entry state. Before the fix the body
//! named the loop context's `StateAcc`: unbound in a `collect:`-family fold
//! (an `erlc` failure), and the tuple itself in a `do:` (`badmap`).
//!
//! Proved over a generated corpus, not the repro: every fold kind x every
//! read form x every enclosing context compiles with a silent verifier and
//! never reads a field from a fold's tuple accumulator. With the fold's entry
//! capture skipped ([`LocalRebindFault::TupleAccNoEntryCapture`]) every
//! program whose code the fault changes must be reported as
//! `TupleAccStateReadNotEntryState` (a program it leaves unchanged holds no
//! tuple-acc fold, or one whose entry state is already the name the loop
//! context derives), every fold kind must be reported at least once, and
//! each context exactly as its flag says.
//!
//! Folds and contexts avoid guarded operators (`a + e` over untyped
//! locals): a loop that brackets one reads fields through the cell
//! (BT-4121), which is a different path.

use super::{codegen, verifier_report};
use crate::core_erlang::threaded_expr::fault::LocalRebindFault;
use crate::core_erlang::threaded_ir::{ThreadingMode, VerifyError, verify_tuple_acc_state_reads};
use beamtalk_core::source_analysis::Span;

const HEADER: &str = "Actor subclass: Probe\n  state: n = 7\n\n";

/// Folds whose body writes the outer local `k`; `READ` is the
/// field-reading expression.
const FOLDS: [&str; 8] = [
    "#(1, 2) do: [:e | k := READ. e]",
    "#(1, 2) collect: [:e | k := READ. e]",
    "#(1, 2) select: [:e | k := READ. true]",
    "#(1, 2) reject: [:e | k := READ. false]",
    "#(1, 2) inject: 0 into: [:a :e | k := READ. a]",
    "#(1, 2) detect: [:e | k := READ. false] ifNone: [0]",
    "#{#a => 1} do: [:v | k := READ. v]",
    "#{#a => 1} keysAndValuesDo: [:key :v | k := READ. v]",
];

const READS: [&str; 3] = [
    "self.n",
    "k + self.n",
    "(self.n > 0) ifTrue: [1] ifFalse: [2]",
];

/// Where the fold sits: `FOLD` is the fold statement; `acc`, `i` and `k`
/// are method locals. The flag says whether the fold's entry capture is
/// load-bearing there. A fold opening a `StateAcc` body enters at
/// that body's (or conditional arm's) unversioned `StateAcc`, the very name
/// the loop context derives: there the fresh accumulator name is what keeps
/// the read off the tuple, and with the lambda's accumulator named
/// `StateAcc` again the clean corpus reports the shadowing.
const CONTEXTS: [(&str, bool); 9] = [
    ("FOLD", true),
    ("#(1, 2) do: [:m | FOLD. acc := acc + m]", false),
    ("#(1, 2) do: [:m | acc := acc + m. FOLD]", true),
    ("#(1, 2) do: [:m | #(3) do: [:q | FOLD]]", false),
    ("#(1, 2) inject: 0 into: [:s :m | FOLD. s + m]", false),
    (
        "#(1, 2) do: [:m | acc := acc + m. (m > 0) ifTrue: [FOLD]]",
        false,
    ),
    (
        "#(1, 2) do: [:m | (m > 0) ifTrue: [acc := acc + m. FOLD]]",
        true,
    ),
    ("[i < 2] whileTrue: [i := i + 1. FOLD]", true),
    ("1 to: 2 do: [:q | acc := acc + q. FOLD]", true),
];

/// `(context index, fold index, program)` for the whole corpus.
fn programs() -> Vec<(usize, usize, String)> {
    let mut out = Vec::new();
    for (c, (context, _)) in CONTEXTS.iter().enumerate() {
        for (f, fold) in FOLDS.iter().enumerate() {
            for read in READS {
                let stmt = context.replace("FOLD", &fold.replace("READ", read));
                out.push((
                    c,
                    f,
                    format!(
                        "{HEADER}  run =>\n    acc := 0.\n    i := 0.\n    k := 0.\n    {stmt}.\n    acc + k\n"
                    ),
                ));
            }
        }
    }
    out
}

#[test]
fn tuple_acc_fold_bodies_read_the_entry_state() {
    let failures: Vec<String> = programs()
        .into_iter()
        .filter_map(|(_, _, src)| verifier_report(&src, None).map(|r| format!("{r}\n{src}")))
        .collect();
    assert!(failures.is_empty(), "{}", failures.join("\n---\n"));
}

#[test]
fn tuple_acc_fold_bodies_never_read_their_accumulator() {
    for (_, _, src) in programs() {
        let out = codegen(&src);
        for read in out.split("call 'maps':'get'('n', ").skip(1) {
            let var = read.split(')').next().unwrap_or_default();
            assert!(
                !var.starts_with("_AccSt"),
                "field read from the tuple accumulator `{var}`:\n{src}\n{out}"
            );
        }
    }
}

#[test]
fn skipping_the_entry_capture_is_reported() {
    let mut missed = Vec::new();
    let mut fired_contexts = [false; CONTEXTS.len()];
    let mut fired_folds = [false; FOLDS.len()];
    for (c, f, src) in programs() {
        let clean = codegen(&src);
        let report = verifier_report(&src, Some(LocalRebindFault::TupleAccNoEntryCapture));
        let fired = report
            .as_deref()
            .is_some_and(|r| r.contains("TupleAccStateReadNotEntryState"));
        if fired {
            fired_contexts[c] = true;
            fired_folds[f] = true;
            continue;
        }
        let faulted = crate::core_erlang::threaded_expr::fault::with(
            LocalRebindFault::TupleAccNoEntryCapture,
            || codegen(&src),
        );
        if faulted != clean {
            missed.push(format!("changed but unreported ({report:?}):\n{src}"));
        }
    }
    assert!(missed.is_empty(), "{}", missed.join("\n---\n"));
    for (fired, (context, load_bearing)) in fired_contexts.iter().zip(CONTEXTS) {
        assert_eq!(*fired, load_bearing, "context reported: {context}");
    }
    for (f, fired) in fired_folds.iter().enumerate() {
        assert!(fired, "fold never reported: {}", FOLDS[f]);
    }
}

#[test]
fn verifier_rejects_reads_off_the_entry_state_or_of_the_accumulator() {
    let mode = ThreadingMode::TupleAcc(0);
    let at = Span::new(0, 1);
    let reads = |names: &[&str]| names.iter().map(ToString::to_string).collect::<Vec<_>>();
    assert!(
        verify_tuple_acc_state_reads(&mode, "_AccSt@1", "State", &reads(&["State"]), at).is_empty()
    );
    let off = verify_tuple_acc_state_reads(
        &mode,
        "_AccSt@1",
        "State",
        &reads(&["StateAcc", "StateAcc"]),
        at,
    );
    assert!(
        matches!(off.as_slice(), [VerifyError::TupleAccStateReadNotEntryState { read_from, .. }] if read_from == "StateAcc"),
        "{off:?}"
    );
    // The entry state shadowed by the accumulator parameter.
    let shadowed =
        verify_tuple_acc_state_reads(&mode, "StateAcc", "StateAcc", &reads(&["StateAcc"]), at);
    assert_eq!(shadowed.len(), 1, "{shadowed:?}");
    // Any other mode is not constrained here.
    assert!(
        verify_tuple_acc_state_reads(
            &ThreadingMode::DirectParams,
            "StateAcc",
            "State",
            &reads(&["StateAcc"]),
            at
        )
        .is_empty()
    );
}
