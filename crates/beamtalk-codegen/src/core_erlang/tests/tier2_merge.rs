// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4122 (ADR 0137): the Tier 2 merge and the `FieldReadFromStateAcc`
//! rule.
//!
//! A stored Tier 2 block that may write the actor's fields lowers merged:
//! its body runs cell-mode for fields (like a Tier 1 closure) with the
//! captured locals unpacked from `StateAcc` on entry, and its exit takes
//! the live cell, flushes the locals into it, stores the merged map back,
//! and answers it. A statement unpack rebinds `State` from that complete
//! map; an operand call merges into its own bracket, which binds element
//! 1 as the value, reloads the exit-stored map, and rebinds the call's
//! locals from it.
//!
//! The validator rejects stored blocks that assign captured locals
//! (ADR 0133), so these shapes are reachable only in unvalidated
//! compiles — these tests compile them with `generate_module` directly,
//! bypassing validation. The `FieldReadFromStateAcc` fun-body arm forces
//! the merged shape: under the legacy fault (which compiles the legacy
//! `fun` back) a field read or write in the body reports instead of
//! threading through `StateAcc`.

use super::codegen;
use crate::core_erlang::fault::{self, Fault};

/// The panic message of compiling `src` (a verifier finding is a
/// debug-build panic), or `None` when it compiles.
fn enforce_report(src: &str) -> Option<String> {
    match std::panic::catch_unwind(|| codegen(src)) {
        Ok(_) => None,
        Err(payload) => Some(
            payload
                .downcast_ref::<String>()
                .cloned()
                .or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_string()))
                .unwrap_or_else(|| "<non-string panic>".to_string()),
        ),
    }
}

fn with_legacy_fault<T>(f: impl FnOnce() -> T) -> T {
    fault::set(Fault::LegacyTier2, true);
    let result = f();
    fault::set(Fault::LegacyTier2, false);
    result
}

fn with_no_brackets_fault<T>(f: impl FnOnce() -> T) -> T {
    fault::set(Fault::NoBrackets, true);
    let result = f();
    fault::set(Fault::NoBrackets, false);
    result
}

const PREAMBLE: &str = "Actor subclass: Ctr\n  state: n = 0\n\n";

/// A stored block writing a captured local and a field, called in operand
/// position: the merge (element-1 value, reloaded state, rebound local).
const OPERAND: &str = "  run =>\n    count := 0\n    b := [count := count + 1. self.n := self.n + 100. count]\n    ^(b value) + self.n\n";

/// The same block called as a statement: merged `fun`, tuple unpack.
const STMT: &str = "  run =>\n    count := 0\n    b := [count := count + 1. self.n := self.n + 100. count]\n    b value\n    self.n * 100\n";

#[test]
fn operand_merged_call_unpacks_value_and_rebinds_local() {
    let out = codegen(&format!("{PREAMBLE}{OPERAND}"));
    // The bracket binds element 1 of the apply's tuple as its value ...
    assert!(
        out.contains("element'(1, _T2Merge"),
        "operand merge unpacks element 1:\n{out}"
    );
    // ... reloads the exit-stored map from the cell ...
    assert!(
        out.contains("'take'()"),
        "operand merge reloads from the cell:\n{out}"
    );
    // ... and rebinds the call's captured local from the reloaded state.
    assert!(
        out.contains("'__local__count'"),
        "operand merge rebinds the captured local:\n{out}"
    );
    // The merged `fun` exit stores its take()-plus-locals map for the reload.
    assert!(
        out.contains("T2Cell"),
        "merged fun exit takes the live cell:\n{out}"
    );
}

#[test]
fn stmt_merged_call_compiles_without_findings() {
    let out = codegen(&format!("{PREAMBLE}{STMT}"));
    // Merged shape (cell take at the exit), no legacy `StateAcc` field path.
    assert!(
        out.contains("T2Cell"),
        "merged fun exit takes the live cell:\n{out}"
    );
    assert!(
        !out.contains("FieldReadFromStateAcc"),
        "no findings on the merged shape:\n{out}"
    );
}

#[test]
fn legacy_fault_fun_body_read_reports() {
    let report = with_legacy_fault(|| enforce_report(&format!("{PREAMBLE}{STMT}")));
    let report = report.expect("faulted compile should report");
    assert!(
        report.contains("FieldReadFromStateAcc"),
        "legacy fun body read reports:\n{report}"
    );
}

#[test]
fn legacy_fault_operand_reports_fun_body_read() {
    // Under the legacy fault the operand close takes the legacy drop
    // (the legacy return holds the fields, so there is nothing sound to
    // reload) — the finding is the fun body's own `StateAcc` field read
    // (the `self.n` on the write's right-hand side).
    let report = with_legacy_fault(|| enforce_report(&format!("{PREAMBLE}{OPERAND}")));
    let report = report.expect("faulted compile should report");
    assert!(
        report.contains("tier2 fun body reads field via StateAcc"),
        "legacy fun body read reports:\n{report}"
    );
}

#[test]
fn no_brackets_fault_operand_tier2_call_is_unbracketed_gap() {
    // An operand Tier 2 call is noted like any other user-code send: with
    // no bracket forthcoming it reports `UnbracketedUserCodeSend`, never
    // silently. (The `fun` stays merged — this fault only removes
    // windows — so no `FieldReadFromStateAcc` fires alongside.)
    let report = with_no_brackets_fault(|| enforce_report(&format!("{PREAMBLE}{OPERAND}")));
    let report = report.expect("faulted compile should report");
    assert!(
        report.contains("UnbracketedUserCodeSend"),
        "unbracketed Tier 2 call reports:\n{report}"
    );
    assert!(
        !report.contains("FieldReadFromStateAcc"),
        "merged fun body stays clean:\n{report}"
    );
}

/// The same block returning non-locally: the `^` bypasses the exit flush.
const NLR: &str = "  run =>\n    count := 0\n    b := [count := count + 1. self.n := self.n + 100. ^count]\n    b value\n";

#[test]
fn caret_out_of_merged_fun_carries_take_plus_locals() {
    let out = codegen(&format!("{PREAMBLE}{NLR}"));
    // The throw bypasses the exit flush, so the `^` carries take()+locals.
    assert!(
        out.contains("T2Nlr"),
        "merged fun ^ carries take() plus locals:\n{out}"
    );
    assert!(
        out.contains("'__local__count'"),
        "merged fun ^ flushes the captured local:\n{out}"
    );
    assert!(
        !out.contains("FieldReadFromStateAcc"),
        "no findings on the merged shape:\n{out}"
    );
}

/// BT-4312 (finding A of the review): a merged fun's `^` that unwinds through
/// a writing `ensure:` carries the captured locals as they were at the `^`
/// (`count` is 11 there). The relay resumes the throw with the carried state
/// under the cell (`refresh(Self, Carried)`), so the cleanup's field write
/// rides along and `count` keeps the value the `^` flushed. Rebuilding the
/// state at the `ensure:` re-flushed `count` as bound there (1), which
/// `RebuildsCarriedStateAtSite` rejects (`ensure_relay_corpus.rs`).
#[test]
fn merged_fun_ensure_relay_keeps_the_locals_the_return_carried() {
    let out = codegen(&format!(
        "{PREAMBLE}  run =>\n    count := 0\n    b := [count := count + 1. self.n := self.n + 1. [count := count + 10. ^count] ensure: [self.n := 9]]\n    b value\n    0\n"
    ));
    assert!(
        out.contains("call 'beamtalk_actor_state':'refresh'(Self, _EnsureCarried"),
        "the relay resumes with the carried state under the cell:\n{out}"
    );
    let relay = out
        .split("when call 'erlang':'=:='(_EnsureNlrTok")
        .nth(1)
        .and_then(|clause| clause.split("<_EnsureOther").next())
        .expect("a relay clause");
    assert!(
        !relay.contains("'__local__count'"),
        "the relay must not re-flush `count` from the `ensure:` site:\n{relay}"
    );
}

/// BT-4535: an arm `^` that the conditional lowering builds as a
/// `ThreadedStmt::NlrThrow` (BT-3495's `^(self.f := ...)`, BT-3374's
/// `^self foo`) never runs inside a merged Tier 2 fun: the merged body is
/// cell-mode, so the arm's field write and self-send go through the cell and
/// its `^` takes the generic `Return` lowering, whose throw carries the
/// merged map (`nlr_throw_state_doc`'s `Tier2Merged` slot: `take/0`, then the
/// captured locals flushed). The merged fun's every throw carries that map,
/// in every arm shape and receiver.
#[test]
fn merged_fun_arm_returns_carry_the_merged_map() {
    for ret in ["^(self.n := self.n + 10)", "^(self bump)", "^self.n"] {
        for cond in [
            "flag ifTrue: [RET]",
            "flag ifFalse: [RET]",
            "flag ifTrue: [RET] ifFalse: [self.n := 1]",
            "(flag == true) ifTrue: [RET]",
        ] {
            let stmt = cond.replace("RET", ret);
            let src = format!(
                "{PREAMBLE}  bump => self.n := self.n + 1\n\n  run: flag =>\n    count := 0\n    b := [count := count + 1. self.n := self.n + 1. {stmt}. count]\n    b value\n    count\n"
            );
            let out = codegen(&src);
            let body = out
                .split("fun (StateAcc) ->")
                .nth(1)
                .unwrap_or_else(|| panic!("{stmt}: no merged fun:\n{out}"));
            assert!(
                body.contains("_T2Nlr"),
                "{stmt}: the merged fun's `^` carries the merged map:\n{out}"
            );
            assert!(
                !body.contains("_NlrThrow"),
                "{stmt}: an arm `NlrThrow` ran inside the merged fun:\n{out}"
            );
        }
    }
}
