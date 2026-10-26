// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4121 (ADR 0137): a guarded operator's slow path in a loop that holds
//! no whole `State` runs against the live cell. The loop brackets around it
//! (a store before the loop, a reload woven into its exit) while keeping its
//! direct/tuple-accumulator parameters; anything left unbracketed reports
//! [`VerifyError::UnbracketedGuardedOperator`], never silently.

use super::synced_call::{
    FAULT_NO_BRACKETS, GuardedOpCheckout, GuardedOpLoopMarker, GuardedOpNeed,
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
