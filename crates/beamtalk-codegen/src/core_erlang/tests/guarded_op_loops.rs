// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4121 (ADR 0137): a guarded operator's slow path in a loop that holds
//! no whole `State` runs against the live cell. The loop brackets around it
//! (a store before the loop, a reload woven into its exit) while keeping its
//! direct/tuple-accumulator parameters; anything left unbracketed reports
//! [`VerifyError::UnbracketedGuardedOperator`], never silently.

use super::synced_call::{
    FAULT_NO_BRACKETS, FAULT_STALE_STATE_IN_BRACKETED_LOOP, GuardedOpCheckout, GuardedOpLoopMarker,
    GuardedOpNeed, StaleStateUseKind, ThreadedStateUse,
};
use crate::core_erlang::{CodegenOptions, CoreErlangGenerator, generate_module};
use beamtalk_core::source_analysis::{Span, lex_with_eof, parse};

fn compile(src: &str) -> String {
    let (module, _) = parse(lex_with_eof(src));
    generate_module(&module, CodegenOptions::new("test")).expect("codegen should succeed")
}

fn count(out: &str, pat: &str) -> usize {
    out.matches(pat).count()
}

/// How many bracketed calls `out` holds (each stores through `store/1`).
fn brackets(out: &str) -> usize {
    count(out, "let _Synced")
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
                .or_else(|| {
                    payload
                        .downcast_ref::<&str>()
                        .map(std::string::ToString::to_string)
                })
                .unwrap_or_else(|| "<non-string panic>".to_string()),
        ),
    }
}

fn stores(out: &str) -> usize {
    count(out, "'store'")
}

fn takes(out: &str) -> usize {
    count(out, "'take'")
}

const ACTOR_PREAMBLE: &str = "Actor subclass: Probe\n  state: n = 0\n\n";

/// BT-4194: the `[...] value` block in a `do:` body is lowered over a scratch
/// map seeded fresh from the loop's locals; a bracket on its guarded operator
/// stored that scratch map into the cell, so the enclosing loop's reload read
/// it back as the `State` and the nest answered stale values. Inside a loop
/// that holds no whole `State` the operator is covered by the loop's bracket
/// instead.
fn loop_nest_value_block(iter: &str) -> String {
    format!(
        "{ACTOR_PREAMBLE}  run: c =>\n    t := 0.\n    m := 0.\n    [m < 2] whileTrue: [m := m + 1. #(1, 2) {iter} [:z | u := 0. [u := u + 1] value. t := u]].\n    t\n"
    )
}

#[test]
fn value_block_over_scratch_in_stateless_loop_does_not_bracket_its_scratch() {
    let src = loop_nest_value_block("do:");
    // Verifier-clean (`SyncedCallOnPrivateScratch` would panic here).
    assert_eq!(enforce_report(&src), None);
    let out = compile(&src);
    let seeded = out.find("_SeededState").expect("seeded scratch map");
    let region_end = out[seeded..].find("_FoldResult").expect("fold result");
    assert!(
        !out[seeded..seeded + region_end].contains("'store'"),
        "scratch map stored into the cell:\n{out}"
    );
}

#[test]
fn collect_twin_of_value_block_over_scratch_is_verifier_clean() {
    assert_eq!(enforce_report(&loop_nest_value_block("collect:")), None);
}

#[test]
fn do_loop_with_guarded_op_brackets() {
    let out = compile(&format!(
        "{ACTOR_PREAMBLE}  sum: items =>\n    acc := 0.\n    items do: [:m | acc := acc + m].\n    acc\n"
    ));
    // One pre-store before the loop plus the method exit store; one reload
    // woven into the exit pack.
    assert_eq!(stores(&out), 2, "stores:\n{out}");
    assert_eq!(takes(&out), 1, "takes:\n{out}");
    // The loop keeps its tuple-accumulator parameters (a tuple seed, not a
    // packed `StateAcc` map).
    assert!(
        out.contains("call 'lists':'foldl'(") && out.contains("{Acc}"),
        "tuple-acc foldl kept:\n{out}"
    );
}

#[test]
fn loop_without_operators_emits_no_cell_traffic() {
    // Gate pin: a loop with no guarded operator compiles exactly as before
    // (no marker action, no store, no reload).
    let out = compile(&format!(
        "{ACTOR_PREAMBLE}  run: items =>\n    acc := 0.\n    items do: [:m | acc := m].\n    acc\n"
    ));
    assert_eq!(stores(&out), 0, "stores:\n{out}");
    assert_eq!(takes(&out), 0, "takes:\n{out}");
}

#[test]
fn numeric_loop_keeps_direct_params() {
    // Gate 3 pin (BT-4121): an arithmetic loop over untyped locals keeps
    // its direct parameters — it is bracketed around the loop, never
    // threaded through it.
    let out = compile(&format!(
        "{ACTOR_PREAMBLE}  sum: items =>\n    acc := 0.\n    items do: [:m | acc := acc + 1].\n    acc\n"
    ));
    assert_eq!(takes(&out), 1, "takes:\n{out}");
    assert!(out.contains("{Acc}"), "tuple-accumulator seed kept:\n{out}");
    assert_eq!(brackets(&out), 0, "no per-operator bracket:\n{out}");
}

#[test]
fn while_loop_with_guarded_op_brackets() {
    let out = compile(&format!(
        "{ACTOR_PREAMBLE}  run: limit =>\n    acc := 0.\n    i := 0.\n    [i < limit] whileTrue: [acc := acc + i. i := i + 1].\n    acc\n"
    ));
    assert_eq!(stores(&out), 2, "stores:\n{out}");
    assert_eq!(takes(&out), 1, "takes:\n{out}");
    // Direct params kept: the fun binds the locals, not a whole `State`.
    assert!(
        out.contains("'while'/2 = fun (Acc, I)"),
        "direct-params fun kept:\n{out}"
    );
}

#[test]
fn counted_loop_with_guarded_op_brackets() {
    let out = compile(&format!(
        "{ACTOR_PREAMBLE}  run: n with: m =>\n    acc := 0.\n    1 to: n do: [:i | acc := acc + m].\n    acc\n"
    ));
    assert_eq!(stores(&out), 2, "stores:\n{out}");
    assert_eq!(takes(&out), 1, "takes:\n{out}");
}

#[test]
fn collect_with_carried_guarded_op_brackets() {
    let out = compile(&format!(
        "{ACTOR_PREAMBLE}  run: items =>\n    acc := 0.\n    items collect: [:m | acc := acc + m]\n"
    ));
    assert_eq!(stores(&out), 2, "stores:\n{out}");
    assert_eq!(takes(&out), 1, "takes:\n{out}");
}

#[test]
fn hybrid_loop_with_guarded_op_threads_state() {
    // A hybrid loop cannot bracket (its field parameters advance past the
    // entry `State`), so the mode selection threads `StateAcc` instead and
    // the operators bracket normally inside it.
    let out = compile(
        "Actor subclass: Probe\n  state: total = 0\n\n  run: n =>\n    i := n.\n    [i > 0] whileTrue: [self.total := self.total + i. i := i - 1].\n    self.total\n",
    );
    assert!(
        out.contains("call 'maps':'get'('total', StateAcc)"),
        "field reads thread StateAcc:\n{out}"
    );
    assert!(
        brackets(&out) > 0,
        "operators bracket inside the threaded loop:\n{out}"
    );
    assert_eq!(
        enforce_report(
            "Actor subclass: Probe\n  state: total = 0\n\n  run: n =>\n    i := n.\n    [i > 0] whileTrue: [self.total := self.total + i. i := i - 1].\n    self.total\n",
        ),
        None
    );
}

#[test]
fn noncarried_times_repeat_with_guarded_op_wraps_send() {
    // Not inlined (nothing carried): the `timesRepeat:` send itself is
    // bracketed around the closure call.
    let out = compile(&format!(
        "{ACTOR_PREAMBLE}  run: g =>\n    3 timesRepeat: [g + g].\n    self.n\n"
    ));
    assert_eq!(brackets(&out), 1, "one send bracket:\n{out}");
}

#[test]
fn evaluated_literal_block_with_guarded_op_wraps_send() {
    let out = compile(&format!(
        "{ACTOR_PREAMBLE}  run: x =>\n    [:m | m + m] value: x.\n    self.n\n"
    ));
    assert_eq!(brackets(&out), 1, "one send bracket:\n{out}");
}

#[test]
fn nested_stateless_loops_share_outer_bracket() {
    // The inner loop has no `State` to store, so it hands its need outward:
    // one pre-store before the outer loop, and both exit packs seed from
    // the cell (the inner pack carries the inner locals through the outer
    // fun; the outer pack loads the writes back).
    let out = compile(&format!(
        "{ACTOR_PREAMBLE}  run: xs with: ys =>\n    acc := 0.\n    xs do: [:x | ys do: [:y | acc := acc + y]].\n    acc\n"
    ));
    assert_eq!(stores(&out), 2, "stores:\n{out}");
    assert_eq!(takes(&out), 1, "takes:\n{out}");
    assert_eq!(
        enforce_report(&format!(
            "{ACTOR_PREAMBLE}  run: xs with: ys =>\n    acc := 0.\n    xs do: [:x | ys do: [:y | acc := acc + y]].\n    acc\n"
        )),
        None
    );
}

#[test]
fn inner_stateless_loop_in_stateful_body_brackets_itself() {
    // The outer `do:` threads `StateAcc` (a nested `whileTrue:` in its
    // body), so the inner direct-params loop has a `State` in scope and
    // brackets itself: one pre-store of the outer `StateAcc`, one reload
    // woven into its exit pack (which the outer body threads onward).
    let src = format!(
        "{ACTOR_PREAMBLE}  run: xs with: n =>\n    acc := 0.\n    xs do: [:x | i := n. [i > 0] whileTrue: [acc := acc + x. i := i - 1]].\n    acc\n"
    );
    let out = compile(&src);
    assert_eq!(stores(&out), 2, "stores:\n{out}");
    assert_eq!(takes(&out), 1, "takes:\n{out}");
    assert!(
        out.contains("'store'(StateAcc)"),
        "inner pre-store of the outer StateAcc:\n{out}"
    );
    assert_eq!(enforce_report(&src), None);
}

#[test]
fn early_return_in_bracketed_loop_carries_live_cell() {
    // A `^` out of a bracketed loop carries the live cell like the loop's
    // own exit pack (exit seed, the throw's state, and the pre-throw store
    // that keeps them), so the slow-path writes made before it survive
    // instead of being clobbered by a store of the entry state.
    // (`^` under a conditional lowers through the closure arm, which is
    // already live; the direct `^` takes the loop-entry arm this covers.)
    let src = format!(
        "{ACTOR_PREAMBLE}  run: xs =>\n    acc := 0.\n    xs do: [:m | acc := acc + m. ^ acc].\n    acc\n"
    );
    let out = compile(&src);
    assert_eq!(takes(&out), 3, "takes:\n{out}");
    assert!(out.contains("'$bt_nlr'"), "nlr throw:\n{out}");
    assert_eq!(enforce_report(&src), None);
}

#[test]
fn guarded_op_gap_fires_loud_without_marker() {
    // Under the no-brackets fault no marker is pushed, so the exemption
    // reports `UnbracketedGuardedOperator` instead of bracketing — the rule
    // is never silent.
    FAULT_NO_BRACKETS.with(|f| f.set(true));
    let report = enforce_report(&format!(
        "{ACTOR_PREAMBLE}  sum: items =>\n    acc := 0.\n    items do: [:m | acc := acc + m].\n    acc\n"
    ));
    FAULT_NO_BRACKETS.with(|f| f.set(false));
    let report = report.expect("faulted compile should report");
    assert!(
        report.contains("UnbracketedGuardedOperator"),
        "loud guarded-op finding:\n{report}"
    );
}

// ─── Marker decision table (unit) ────────────────────────────────────────────

fn op_marker(will_bracket: bool, need: bool) -> GuardedOpLoopMarker {
    GuardedOpLoopMarker {
        will_bracket,
        need: need.then(|| GuardedOpNeed {
            selector: "+".to_string(),
            at: Span::new(0, 1),
        }),
        entry_state: "State".to_string(),
        state_uses: Vec::new(),
    }
}

#[test]
fn take_reads_when_any_enclosing_loop_brackets() {
    // `guarded_op_take_reads` is any-over-the-stack, not innermost-only: a
    // bracketing loop's slow paths write the cell during the iteration, so
    // every read in the iteration must be live — including reads in a
    // nested loop with no guarded operator of its own. The take-reads
    // decision has no verifier backstop (unlike gaps, which are loud), so
    // the predicate stays conservative here even though no end-to-end
    // shape nests a stateless reader inside a bracketing loop today.
    let mut generator = CoreErlangGenerator::new("test");
    generator.loop_mode.in_direct_params_loop = true;
    generator
        .synced
        .guarded_op_loops
        .borrow_mut()
        .push(op_marker(true, false));
    generator
        .synced
        .guarded_op_loops
        .borrow_mut()
        .push(op_marker(false, false));
    assert!(generator.guarded_op_take_reads());
    generator.synced.guarded_op_loops.borrow_mut().clear();
    generator
        .synced
        .guarded_op_loops
        .borrow_mut()
        .push(op_marker(false, false));
    assert!(!generator.guarded_op_take_reads());
}

#[test]
fn checkout_brackets_when_flagged() {
    let mut generator = CoreErlangGenerator::new("test");
    let out = generator.checkout_guarded_op_loop(op_marker(true, true), false, Span::new(0, 1));
    assert_eq!(out, GuardedOpCheckout::Bracket);
    assert!(generator.synced.guarded_op_gaps.is_empty());
}

#[test]
fn checkout_rests_without_operators() {
    let mut generator = CoreErlangGenerator::new("test");
    let out = generator.checkout_guarded_op_loop(op_marker(false, false), false, Span::new(0, 1));
    assert_eq!(out, GuardedOpCheckout::Nothing);
    assert!(generator.synced.guarded_op_gaps.is_empty());
}

#[test]
fn checkout_walk_miss_reports_loud() {
    // An operator fired that the push-time walk missed: field reads already
    // lowered against the entry `State` cannot be covered after the fact.
    let mut generator = CoreErlangGenerator::new("test");
    let out = generator.checkout_guarded_op_loop(op_marker(false, true), false, Span::new(0, 1));
    assert_eq!(out, GuardedOpCheckout::Nothing);
    assert_eq!(generator.synced.guarded_op_gaps.len(), 1);
    assert_eq!(generator.synced.guarded_op_gaps[0].selector, "+");
}

#[test]
fn checkout_hybrid_reports_loud() {
    // A hybrid loop cannot bracket (its parameters advance past the entry
    // `State`); the mode selection routes these to `StateAcc`, so reaching
    // here means it missed.
    let mut generator = CoreErlangGenerator::new("test");
    let out = generator.checkout_guarded_op_loop(op_marker(true, true), true, Span::new(0, 1));
    assert_eq!(out, GuardedOpCheckout::Nothing);
    assert_eq!(generator.synced.guarded_op_gaps.len(), 1);
}

#[test]
fn checkout_nested_propagates_to_outer_marker() {
    // Nested in another stateless loop's fun: no `State` to store, so the
    // need hands outward while this loop still seeds its exit from the cell.
    let mut generator = CoreErlangGenerator::new("test");
    generator.push_guarded_op_loop(
        &beamtalk_core::ast::Block::new(vec![], vec![], Span::new(0, 0)),
        None,
    );
    generator.loop_mode.in_direct_params_loop = true;
    let out = generator.checkout_guarded_op_loop(op_marker(true, true), false, Span::new(0, 1));
    assert_eq!(out, GuardedOpCheckout::Bracket);
    assert!(generator.synced.guarded_op_gaps.is_empty());
    let outer = generator.pop_guarded_op_loop();
    assert!(outer.need.is_some());
}

#[test]
fn checkout_nested_without_outer_marker_reports_loud() {
    let mut generator = CoreErlangGenerator::new("test");
    generator.loop_mode.in_direct_params_loop = true;
    let out = generator.checkout_guarded_op_loop(op_marker(true, true), false, Span::new(0, 1));
    assert_eq!(out, GuardedOpCheckout::Nothing);
    assert_eq!(generator.synced.guarded_op_gaps.len(), 1);
}

#[test]
fn note_without_marker_is_loud_unless_windowed() {
    let mut generator = CoreErlangGenerator::new("test");
    generator.note_stateless_guarded_op("+".to_string(), Span::new(0, 1));
    assert_eq!(generator.synced.guarded_op_gaps.len(), 1);
    // Inside a bracket window an outer send-wrap covers the operator.
    generator.synced.window_depth = 1;
    generator.note_stateless_guarded_op("+".to_string(), Span::new(2, 3));
    assert_eq!(generator.synced.guarded_op_gaps.len(), 1);
}

#[test]
fn gaps_finish_as_their_own_variant() {
    // The new rule reports `UnbracketedGuardedOperator`, separately from
    // `UnbracketedUserCodeSend` (kept cleanly separable for BT-4122).
    let result = std::panic::catch_unwind(|| {
        let mut generator = CoreErlangGenerator::new("test");
        generator.note_stateless_guarded_op("+".to_string(), Span::new(0, 1));
        generator.finish_synced_calls(Span::new(0, 1));
    });
    let payload = result.expect_err("finish should report the gap");
    let report = payload
        .downcast_ref::<String>()
        .cloned()
        .or_else(|| {
            payload
                .downcast_ref::<&str>()
                .map(std::string::ToString::to_string)
        })
        .unwrap_or_default();
    assert!(
        report.contains("UnbracketedGuardedOperator"),
        "own variant reported:\n{report}"
    );
    assert!(
        !report.contains("UnbracketedUserCodeSend"),
        "not folded into the send rule:\n{report}"
    );
}

/// BT-4179: the stateless loop shapes that bracket guarded operators, each
/// holding an `on:do:` in its body — directly, or nested in an inner
/// direct-params loop (which hands its need to the bracketing outer loop).
/// `{op}` is the protected block's body and `{h}` the handler block.
const ON_DO_LOOPS: [&str; 6] = [
    "i := 0. [i < 3] whileTrue: [acc := acc + x. [{op}] on: Error do: {h}. i := i + 1]",
    "1 to: 3 do: [:i | acc := acc + x. [{op}] on: Error do: {h}]",
    "3 timesRepeat: [acc := acc + x. [{op}] on: Error do: {h}]",
    "1 to: 3 do: [:i | acc := acc + x. 1 to: 2 do: [:j | k := j. [{op}] on: Error do: {h}]]",
    "i := 0. [i < 3] whileTrue: [acc := acc + x. 2 timesRepeat: [k := k. [{op}] on: Error do: {h}]. i := i + 1]",
    "3 timesRepeat: [acc := acc + x. i := 0. [i < 2] whileTrue: [[{op}] on: Error do: {h}. i := i + 1]]",
];

/// The protected bodies and handlers the corpus crosses with each loop. None
/// sends a message that runs user code: one would make the loop thread `State`
/// (a `StateAcc` loop, where the pre-region `State` is current) instead of
/// staying stateless and bracketing. A protected body or handler that writes
/// a local keeps the loop stateless too.
const ON_DO_PROTECTED: [&str; 4] = ["x - x", "x + x. x - x", "x - x. 0", "k := x - x"];
const ON_DO_HANDLERS: [&str; 4] = ["[:e | nil]", "[nil]", "[:e | e]", "[:e | k := 1]"];

const ON_DO_PREAMBLE: &str = "Actor subclass: Probe\n  state: n = 0\n  state: m = 0\n\n";

fn on_do_loop_src(shape: &str, op: &str, handler: &str) -> String {
    let body = shape.replace("{op}", op).replace("{h}", handler);
    format!("{ON_DO_PREAMBLE}  run: x =>\n    acc := 0.\n    k := 0.\n    {body}.\n    acc + k\n")
}

/// BT-4179: hybrid loops (a field written in the body is held in a fun
/// parameter) holding an `on:do:`, at method level and nested in a loop that
/// brackets guarded operators. A hybrid body holds no guarded operator, no
/// self-send and no send that may run user code (each routes the loop to
/// `StateAcc`), so its `on:do:` cannot run user code that would read the cell,
/// and it never sits under a marker that brackets: no loop that brackets
/// (direct-params, tuple-accumulator) admits a field write in its body. Its
/// boundary stores the threaded `State` (`PreState`).
const ON_DO_HYBRID_LOOPS: [&str; 6] = [
    // Field write.
    "1 to: 2 do: [:j | k := j. self.n := j. [{op}] on: Error do: {h}]",
    // Field read (a read-only field parameter) and write.
    "1 to: 2 do: [:j | k := j. self.n := self.m. [{op}] on: Error do: {h}]",
    "[true] whileTrue: [k := nil. self.n := k. [{op}] on: Error do: {h}]",
    // Nested in a bracketing loop.
    "1 to: 3 do: [:i | acc := acc + x. 1 to: 2 do: [:j | k := j. self.n := j. [{op}] on: Error do: {h}]]",
    "1 to: 3 do: [:i | acc := acc + x. 1 to: 2 do: [:j | k := j. self.n := self.m. [{op}] on: Error do: {h}]]",
    "i := 0. [i < 3] whileTrue: [acc := acc + x. 2 timesRepeat: [k := nil. self.n := k. [{op}] on: Error do: {h}]. i := i + 1]",
];

/// Protected bodies and handlers that keep a hybrid loop hybrid: no operator,
/// no send that may run user code.
const ON_DO_HYBRID_PROTECTED: [&str; 2] = ["0 error: \"boom\"", "0"];
const ON_DO_HYBRID_HANDLERS: [&str; 3] = ["[:e | nil]", "[nil]", "[:e | e]"];

/// Every cell-boundary entry in `out`: the text between each class-variable
/// snapshot and the `try` that follows it.
fn on_do_entries(out: &str) -> Vec<&str> {
    out.match_indices("'beamtalk_class_vars':'snapshot'()")
        .map(|(i, _)| {
            let rest = &out[i..];
            &rest[..rest.find("try ").expect("on:do: entry precedes its try")]
        })
        .collect()
}

#[test]
fn on_do_in_bracketed_loop_snapshots_the_live_cell() {
    // BT-4179: in a stateless loop that brackets guarded operators the
    // loop-entry `State` is stale for the slow paths' cell writes. The
    // `on:do:` boundary snapshots the live cell on entry to the protected
    // block and restores that snapshot on catch — it never stores the
    // loop-entry `State` (which dropped every earlier slow-path write).
    for shape in ON_DO_LOOPS {
        let src = on_do_loop_src(shape, "x - x", "[:e | nil]");
        let out = compile(&src);
        let entries = on_do_entries(&out);
        assert_eq!(entries.len(), 1, "one on:do: boundary in {shape}:\n{out}");
        assert!(
            entries[0].contains("CellSnap = call 'beamtalk_actor_state':'snapshot'()"),
            "entry snapshots the live cell in {shape}:\n{}",
            entries[0]
        );
        assert!(
            !entries[0].contains("'store'("),
            "entry stores no threaded State in {shape}:\n{}",
            entries[0]
        );
        assert!(
            out.contains("do call 'beamtalk_actor_state':'restore'(Self, _ExClass"),
            "catch restores the entry snapshot in {shape}:\n{out}"
        );
    }
}

#[test]
fn on_do_outside_bracketed_loop_keeps_the_pre_state_store() {
    // Gate pin: at method level the threaded `State` is current, so the
    // boundary keeps storing the pre-region version.
    let out = compile(&format!(
        "{ACTOR_PREAMBLE}  run: x =>\n    acc := x + x.\n    [x - x] on: Error do: [:e | nil].\n    acc\n"
    ));
    let entries = on_do_entries(&out);
    assert_eq!(entries.len(), 1, "{out}");
    assert!(
        entries[0].contains("'beamtalk_actor_state':'store'("),
        "method-level entry stores the pre-region State:\n{}",
        entries[0]
    );
    assert!(!entries[0].contains("CellSnap"), "{}", entries[0]);
}

#[test]
fn on_do_in_bracketed_loops_corpus_is_verifier_clean() {
    // BT-4179: the generated corpus (every bracketing loop shape × protected
    // body × handler) compiles with no verifier finding, and every boundary
    // snapshots the cell.
    for shape in ON_DO_LOOPS {
        for op in ON_DO_PROTECTED {
            for handler in ON_DO_HANDLERS {
                let src = on_do_loop_src(shape, op, handler);
                assert_eq!(enforce_report(&src), None, "{src}");
                let out = compile(&src);
                let entries = on_do_entries(&out);
                assert!(!entries.is_empty(), "{src}\n{out}");
                for entry in entries {
                    assert!(entry.contains("CellSnap"), "{src}\n{entry}");
                }
            }
        }
    }
}

/// Compiles each of `srcs` under [`FAULT_STALE_STATE_IN_BRACKETED_LOOP`],
/// answering each source with its verifier report.
fn faulted_reports(srcs: Vec<String>) -> Vec<(String, Option<String>)> {
    FAULT_STALE_STATE_IN_BRACKETED_LOOP.with(|f| f.set(true));
    let reports = srcs
        .into_iter()
        .map(|src| {
            let report = enforce_report(&src);
            (src, report)
        })
        .collect();
    FAULT_STALE_STATE_IN_BRACKETED_LOOP.with(|f| f.set(false));
    reports
}

#[test]
fn checkout_reports_a_pre_state_boundary_in_a_bracketing_loop() {
    // BT-4179: under the pre-fix gate (`Snapshot` only in an actor closure)
    // every bracketing shape of the corpus stores the loop-entry `State`.
    // The loop checkout reports each one from its own bracket decision and
    // entry `State`, so the rule holds even if the gate regresses.
    let srcs = ON_DO_LOOPS
        .iter()
        .flat_map(|shape| {
            ON_DO_PROTECTED
                .iter()
                .map(move |op| on_do_loop_src(shape, op, "[:e | nil]"))
        })
        .collect();
    for (src, report) in faulted_reports(srcs) {
        let report = report.unwrap_or_else(|| panic!("faulted compile should report:\n{src}"));
        assert!(
            report.contains("PreStateOverLiveCell"),
            "loud live-cell finding for\n{src}\n{report}"
        );
    }
}

#[test]
fn on_do_in_hybrid_loops_corpus_keeps_the_pre_state_boundary() {
    // BT-4179 review: a hybrid loop holds the fields it writes in fun
    // parameters, not the cell. Every shape here reaches hybrid mode (a
    // `Field` parameter), compiles verifier-clean — under the pre-fix gate
    // too, since no enclosing loop brackets — and stores the threaded `State`.
    let mut srcs = Vec::new();
    for shape in ON_DO_HYBRID_LOOPS {
        for op in ON_DO_HYBRID_PROTECTED {
            for handler in ON_DO_HYBRID_HANDLERS {
                let src = on_do_loop_src(shape, op, handler);
                assert_eq!(enforce_report(&src), None, "{src}");
                let out = compile(&src);
                assert!(out.contains("Field"), "hybrid loop reached:\n{src}\n{out}");
                let entries = on_do_entries(&out);
                assert_eq!(entries.len(), 1, "{src}\n{out}");
                assert!(
                    entries[0].contains("'store'(State"),
                    "{src}\n{}",
                    entries[0]
                );
                srcs.push(src);
            }
        }
    }
    for (src, report) in faulted_reports(srcs) {
        assert_eq!(report, None, "no bracket overtakes the State:\n{src}");
    }
}

/// BT-4179: stateless loops that bracket guarded operators, each holding a
/// nested `StateAcc` producer (`{inner}`): a fold whose body holds a
/// parenthesised conditional writing an outer local, which lowers through
/// the fold's `StateAcc` while the enclosing loop keeps direct parameters.
const STATEACC_IN_BRACKETED_LOOPS: [&str; 3] = [
    "1 to: 2 do: [:q | acc := acc + x. {inner}]",
    "i := 0. [i < 2] whileTrue: [acc := acc + x. {inner}. i := i + 1]",
    "2 timesRepeat: [acc := acc + x. {inner}]",
];

const NESTED_STATEACC_PRODUCERS: [&str; 4] = [
    "#(1, 2) do: [:e | ((e == 1) ifTrue: [t := t + 1])]",
    "#(1, 2) do: [:e | ((e == 1) ifTrue: [[t := t + 1] on: Error do: [:z | 0]])]",
    "#(1, 2) collect: [:e | ((e == 1) ifTrue: [t := t + 1]). e]",
    "#(1, 2) do: [:e | ((e == 1) ifTrue: [t := t + 1] ifFalse: [t := t - 1])]",
];

fn stateacc_in_loop_src(shape: &str, inner: &str) -> String {
    let body = shape.replace("{inner}", inner);
    format!("{ON_DO_PREAMBLE}  run: x =>\n    acc := 0.\n    t := 0.\n    {body}.\n    acc + t\n")
}

#[test]
fn stateacc_producer_in_bracketed_loop_seeds_from_the_cell() {
    // BT-4179: a `StateAcc` fold nested in a bracketing loop packs its
    // accumulator from the cell, which holds the slow paths' writes, never
    // from the loop-entry `State` (whose every store would drop them); an
    // `on:do:` in it stores that cell-derived accumulator. Verifier-clean;
    // under the pre-fix fault the checkout reports the stale seed.
    let mut srcs = Vec::new();
    for shape in STATEACC_IN_BRACKETED_LOOPS {
        for inner in NESTED_STATEACC_PRODUCERS {
            let src = stateacc_in_loop_src(shape, inner);
            assert_eq!(enforce_report(&src), None, "{src}");
            let out = compile(&src);
            assert!(
                out.contains("TakeSeed")
                    && out.contains("= call 'beamtalk_actor_state':'take'() in"),
                "nested StateAcc seeds from the cell:\n{src}\n{out}"
            );
            assert!(
                !out.contains("'maps':'put'('__local__t', T, State)"),
                "no seed from the loop-entry State:\n{src}\n{out}"
            );
            srcs.push(src);
        }
    }
    for (src, report) in faulted_reports(srcs) {
        let report = report.unwrap_or_else(|| panic!("faulted compile should report:\n{src}"));
        assert!(
            report.contains("StaleStateSeedInBracketedLoop"),
            "loud stale-seed finding for\n{src}\n{report}"
        );
    }
}

#[test]
fn checkout_reports_only_uses_of_its_entry_state() {
    // A bracketing checkout reports a use of its own entry `State`, and
    // ignores one of a `State` bound inside the loop (from the cell).
    let use_of = |var: &str, kind| ThreadedStateUse {
        kind,
        var: var.to_string(),
        at: Span::new(3, 4),
    };
    let mut generator = CoreErlangGenerator::new("test");
    let mut marker = op_marker(true, true);
    marker.state_uses = vec![
        use_of("State", StaleStateUseKind::OnDoStore),
        use_of("StateAcc", StaleStateUseKind::OnDoStore),
        use_of("State", StaleStateUseKind::StateAccSeed),
    ];
    assert_eq!(
        generator.checkout_guarded_op_loop(marker, false, Span::new(0, 1)),
        GuardedOpCheckout::Bracket
    );
    let kinds: Vec<StaleStateUseKind> = generator
        .synced
        .stale_state_gaps
        .iter()
        .map(|(kind, _)| *kind)
        .collect();
    assert_eq!(
        kinds,
        vec![
            StaleStateUseKind::OnDoStore,
            StaleStateUseKind::StateAccSeed
        ]
    );
}

#[test]
fn checkout_hands_state_uses_to_the_enclosing_marker() {
    // A loop that does not bracket passes its uses outward; the enclosing
    // loop's bracket reports them, and with no bracket anywhere they are
    // dropped.
    let state_use = ThreadedStateUse {
        kind: StaleStateUseKind::OnDoStore,
        var: "State".to_string(),
        at: Span::new(3, 4),
    };
    let mut generator = CoreErlangGenerator::new("test");
    generator
        .synced
        .guarded_op_loops
        .borrow_mut()
        .push(op_marker(true, true));
    let mut inner = op_marker(false, false);
    inner.state_uses.push(state_use.clone());
    assert_eq!(
        generator.checkout_guarded_op_loop(inner, false, Span::new(0, 1)),
        GuardedOpCheckout::Nothing
    );
    assert!(generator.synced.stale_state_gaps.is_empty());
    let outer = generator.pop_guarded_op_loop();
    assert_eq!(outer.state_uses, vec![state_use.clone()]);
    assert_eq!(
        generator.checkout_guarded_op_loop(outer, false, Span::new(0, 1)),
        GuardedOpCheckout::Bracket
    );
    assert_eq!(generator.synced.stale_state_gaps.len(), 1);
    assert_eq!(generator.synced.stale_state_gaps[0].1.at, state_use.at);

    let mut generator = CoreErlangGenerator::new("test");
    let mut lone = op_marker(false, false);
    lone.state_uses.push(state_use);
    generator.checkout_guarded_op_loop(lone, false, Span::new(0, 1));
    assert!(generator.synced.stale_state_gaps.is_empty());
}
