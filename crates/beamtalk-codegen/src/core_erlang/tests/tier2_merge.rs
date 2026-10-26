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

use super::synced_call::{FAULT_LEGACY_TIER2, FAULT_NO_BRACKETS};
use crate::core_erlang::{CodegenOptions, generate_module};
use beamtalk_core::source_analysis::{lex_with_eof, parse};

fn compile(src: &str) -> String {
    let (module, _) = parse(lex_with_eof(src));
    generate_module(&module, CodegenOptions::new("test")).expect("codegen should succeed")
}

/// The panic message of compiling `src` (a verifier finding is a
/// debug-build panic), or `None` when it compiles.
fn enforce_report(src: &str) -> Option<String> {
    match std::panic::catch_unwind(|| compile(src)) {
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
    FAULT_LEGACY_TIER2.with(|c| c.set(true));
    let result = f();
    FAULT_LEGACY_TIER2.with(|c| c.set(false));
    result
}

fn with_no_brackets_fault<T>(f: impl FnOnce() -> T) -> T {
    FAULT_NO_BRACKETS.with(|c| c.set(true));
    let result = f();
    FAULT_NO_BRACKETS.with(|c| c.set(false));
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
    let out = compile(&format!("{PREAMBLE}{OPERAND}"));
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
    let out = compile(&format!("{PREAMBLE}{STMT}"));
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
    let out = compile(&format!("{PREAMBLE}{NLR}"));
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
