// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4524 (ADR 0146 P0): a call that may run a block holding a `^` of the
//! current scope stores the method's state to the cell first.
//!
//! A `^` in an actor closure carries the cell (`nlr_state`). A call that ran
//! such a closure with no store before it (`UserCodeCall::No`) let the throw
//! carry a cell older than the method's `State`, so the method's earlier field
//! writes were lost: `self.n := 6. [^2] ensure: [nil]` left `n` at 0, and so
//! did `timesRepeat:`, `to:do:`, `whileTrue:` and `[^5] value`. A conditional
//! the compiler does not inline (a guarded operator in a branch) and an
//! inline conditional's non-Boolean fallback send had the same gap.
//!
//! The rule is checked where the block becomes a closure
//! (`VerifyError::UnbracketedNlrClosure`): a `^`-holding operand of a send
//! classified `No` outside every window, compiled as a closure with no
//! covering store. The corpus is how the closure is called x where the call
//! sits x whether a field write precedes it: clean and `erlc`-valid, and with
//! the fix off (`FAULT_NLR_CLOSURES_UNSTORED`) every shape that relied on it
//! reports, while the shapes covered independently (a bracketed send, the
//! `on:do:` entry store, an inlined construct) stay clean.
//!
//! BT-4532 (ADR 0146) widens the rule from block literals to every operand
//! that may be a block: a construct that runs a block parameter or variable
//! as a closure (`3 timesRepeat: blk`, `blk value`, `[p foo] on: Error do:
//! blk`, ...) is classified as user code and bracketed, and with that rule
//! off the send itself reports. A store vouches only for the block literals
//! it stores for (`with_nlr_closure_cover`), so a `^`-closure of another send
//! nested in a non-block operand is checked on its own.
//!
//! BT-4612: the one remaining pre-store (a conditional's non-Boolean
//! fallback) records the `State` version it stores and is verified after the
//! version its frame has bound (`VerifyError::StalePrestore`). With the store
//! read before the conditional's receiver is sequenced
//! (`FAULT_PRESTORE_READS_BEFORE_OPERANDS`), every row whose receiver steps
//! `State` reports and no other does. A parenthesised block literal operand
//! (`([^5]) value`) is the literal: `ast::normalize` peels send operands
//! before codegen (BT-4059), so every place that matches an operand as a
//! block literal sees it bare, and these rows report with the fix off like
//! their unparenthesised twins.

use super::assert_compiles_through_erlc;
use crate::core_erlang::synced_call::{
    CLASSIFY_SEND_CALLS, FAULT_NLR_CLOSURES_UNSTORED, FAULT_PRESTORE_READS_BEFORE_OPERANDS,
    PRESTORE_EARLY_READ,
};
use crate::core_erlang::{CodegenOptions, generate_module};
use beamtalk_core::source_analysis::{lex_with_eof, parse};

const HEADER: &str = "Actor subclass: NlrClosureCallProbe
  state: n = 0

  flip: v => self.n := 9. v
";

/// How the block holding the `^` is called, and whether the fix is what
/// stores before it (`true`: the shape reports under the fault).
const CALLS: &[(&str, &str, bool)] = &[
    // Bracketed sends (`UserCodeCall::Yes` without the `^` rule).
    ("collection_do", "#(1, 2) do: [:x | ^x]", false),
    (
        "collection_inject",
        "#(1, 2) inject: 0 into: [:a :x | ^x]",
        false,
    ),
    ("unknown_receiver_send", "p at: 1 ifAbsent: [^3]", false),
    // Intrinsic loops lowered as closures (nothing carried).
    ("times_repeat", "3 timesRepeat: [^7]", true),
    ("to_do", "1 to: 3 do: [:i | ^i]", true),
    ("to_by_do", "1 to: 3 by: 1 do: [:i | ^i]", true),
    ("while_true", "[true] whileTrue: [^2]", true),
    ("while_false", "[false] whileFalse: [^2]", true),
    ("while_true_unary", "[^2] whileTrue", true),
    ("repeat", "[^2] repeat", true),
    // Evaluated literal blocks applied as closures.
    ("value", "[^5] value", true),
    ("value_arg", "[:x | ^x] value: 1", true),
    // Closure `ensure:` / `on:do:`.
    ("closure_ensure", "[^2] ensure: [nil]", true),
    ("closure_on_do_body", "[^3] on: Error do: [:e | 0]", false),
    (
        "closure_on_do_handler",
        "[p foo] on: Error do: [:e | ^4]",
        false,
    ),
    // Conditionals: a non-inlined one (a guarded operator in a branch) and an
    // inline one's non-Boolean fallback send.
    ("dynamic_conditional", "r := p > 0 ifTrue: [^p * 2]", true),
    ("pure_fallback", "r := p ifTrue: [^8]", true),
    ("pure_fallback_and", "r := p and: [^8]", true),
    // A threaded conditional (the `ifFalse:` arm's local write forces the
    // mutation path) touching no field: its non-Boolean fallback is not
    // bracketed, passes the `^` arm as a closure (the writing arm is a
    // raising stub) and relies on the pre-store (`nlr_closure_prestore`).
    (
        "threaded_arm_prestore",
        "r := p ifTrue: [^8] ifFalse: [m := 1]",
        true,
    ),
    // An arm that writes a local and holds the `^` goes to the fallback as a
    // raising stub (`fallback_passes_closure`): no closure runs the `^`.
    ("threaded_arm_stub", "r := p ifTrue: [m := 1. ^8]", false),
    // Nil tests: the block that threads nothing is applied as a closure
    // (`try_generate_nil_protocol`).
    ("if_nil", "r := p ifNil: [^8]", true),
    ("if_not_nil", "r := p ifNotNil: [:v | ^v]", true),
    (
        "if_nil_if_not_nil",
        "r := p ifNil: [^8] ifNotNil: [:v | ^v]",
        true,
    ),
    (
        "if_not_nil_if_nil",
        "r := p ifNotNil: [:v | ^v] ifNil: [^8]",
        true,
    ),
    // Inlined in place: the `^` stores the threaded state itself.
    ("inline_ensure", "[self.n := 1. ^2] ensure: [nil]", false),
    (
        "carried_loop",
        "k := 0. 1 to: 3 do: [:i | k := i. ^k]",
        false,
    ),
    // BT-4532: a block parameter (`blk`) run as a closure by a construct.
    // It may hold a `^` of the method that passed it, so the construct is
    // user code (`SendUserCode::ClosureOperand`) and is bracketed.
    ("param_value", "blk value", true),
    ("param_times_repeat", "3 timesRepeat: blk", true),
    ("param_to_do", "1 to: 3 do: blk", true),
    ("param_while_true", "[true] whileTrue: blk", true),
    ("param_collection_do", "#(1, 2) do: blk", true),
    ("param_ensure_body", "blk ensure: [nil]", true),
    ("param_ensure_cleanup", "[nil] ensure: blk", true),
    ("param_on_do_handler", "[p foo] on: Error do: blk", true),
    ("param_if_true", "r := q ifTrue: blk", true),
    // A send to an unknown receiver brackets whatever it is passed.
    ("param_unknown_receiver", "p at: 1 ifAbsent: blk", false),
    // BT-4532: an operand that steps `State` (a field-writing self-send)
    // ahead of the `^` block. Whatever stores for the `^` reads the state
    // after the operands are threaded, so the store carries the step.
    (
        "state_step_operand_send",
        "r := p foo: (self flip: p) bar: [^8]",
        false,
    ),
    (
        "state_step_conditional",
        "r := (self flip: p) ifTrue: [^8]",
        true,
    ),
    // BT-4606: a send classified as never running user code
    // (`SendUserCode::Never`) never runs its block operands, so a `^` in one
    // can never throw from it: nothing to store, and nothing to report.
    ("never_yourself", "r := [^1] yourself", false),
    ("never_hash", "r := [^1] hash", false),
    ("never_field_at_put", "p fieldAt: #n put: [^1]", false),
    ("never_as_type", "r := [^1] asType: Block", false),
    // BT-4612: the one shape found that reaches the runtime dispatch with a
    // `^` block outside every window: a `Never` send, which runs nothing,
    // so the dispatch stores nothing (it has no pre-store).
    ("never_spawn_with", "p spawnWith: [^1]", false),
    // BT-4612: a parenthesised block literal operand is the literal (the
    // operand normalisation pass peels it), recorded, stored for and covered
    // like the bare one (`note_nlr_operands`, `on:do:`'s cover).
    ("paren_value", "r := ([^5]) value", true),
    ("paren_times_repeat_arg", "3 timesRepeat: ([^7])", true),
    ("paren_ensure_body", "([^2]) ensure: [nil]", true),
    ("paren_conditional_arg", "r := p ifTrue: ([^8])", true),
    ("paren_on_do_body", "([^3]) on: Error do: [:e | 0]", false),
    (
        "paren_on_do_handler",
        "[p foo] on: Error do: ([:e | ^4])",
        false,
    ),
];

/// BT-4612: conditionals whose receiver steps `State` (a field-writing
/// self-send) and whose non-Boolean fallback passes a `^` arm as a closure,
/// so the fallback's pre-store must store the state after that step. Rows of
/// the corpus like [`CALLS`]'s (`state_step_conditional` is the first).
///
/// The two whose conditional is an operand of a bracketed send
/// (`state_step_operand`, `state_step_argument`) do not report with the
/// pre-store off: the `UnbracketedNlrClosure` rule exempts every closure
/// compiled inside a bracket window, though the operands run before the
/// window's store (BT-4632). The pre-store is emitted there all the same.
const STATE_STEP_PRESTORES: &[(&str, &str, bool)] = &[
    (
        "state_step_if_true",
        "r := (self flip: p) ifTrue: [^8]",
        true,
    ),
    (
        "state_step_if_false",
        "r := (self flip: p) ifFalse: [^8]",
        true,
    ),
    ("state_step_and", "r := (self flip: p) and: [^8]", true),
    ("state_step_or", "r := (self flip: p) or: [^8]", true),
    (
        "state_step_if_true_if_false",
        "r := (self flip: p) ifTrue: [^8] ifFalse: [0]",
        true,
    ),
    (
        "state_step_threaded_arm",
        "r := (self flip: p) ifTrue: [^8] ifFalse: [m := 1]",
        true,
    ),
    (
        "state_step_paren_arg",
        "r := (self flip: p) ifTrue: ([^8])",
        true,
    ),
    (
        "state_step_two_steps",
        "r := ((self flip: p) and: [self flip: q]) ifTrue: [^8]",
        true,
    ),
    (
        "state_step_operand",
        "r := 1 + ((self flip: p) ifTrue: [^8])",
        false,
    ),
    (
        "state_step_argument",
        "p foo: ((self flip: p) ifTrue: [^8])",
        false,
    ),
];

/// Where the call sits.
const POSITIONS: &[(&str, &str)] = &[
    ("method_root", "CALL"),
    ("if_true_arm", "q ifTrue: [CALL]"),
    ("loop_body", "1 to: 2 do: [:j | m := j. CALL]"),
];

/// Whether a field write precedes the call.
const PREFIXES: &[(&str, &str)] = &[("field_write", "self.n := 6.\n    "), ("none", "")];

fn corpus() -> Vec<(String, String, bool)> {
    let mut out = Vec::new();
    let state_steps = STATE_STEP_PRESTORES.iter().copied();
    for (call_name, call, needs_fix) in CALLS.iter().copied().chain(state_steps) {
        for (pos_name, pos) in POSITIONS {
            for (prefix_name, prefix) in PREFIXES {
                let body = pos.replace("CALL", call);
                let src = format!(
                    "{HEADER}\n  probe: p with: q with: blk =>\n    m := 0.\n    {prefix}{body}.\n    0\n"
                );
                out.push((
                    format!("{call_name} / {pos_name} / {prefix_name}"),
                    src,
                    needs_fix,
                ));
            }
        }
    }
    out
}

fn compile(src: &str) -> String {
    let (module, _) = parse(lex_with_eof(src));
    generate_module(&module, CodegenOptions::new("test")).expect("codegen should succeed")
}

/// The panic message of compiling `src` (a verifier finding is a debug-build
/// panic), or `None` when it compiles.
fn report(src: &str) -> Option<String> {
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

fn with_fix_off<T>(f: impl FnOnce() -> T) -> T {
    FAULT_NLR_CLOSURES_UNSTORED.with(|flag| flag.set(true));
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(f));
    FAULT_NLR_CLOSURES_UNSTORED.with(|flag| flag.set(false));
    result.unwrap_or_else(|payload| std::panic::resume_unwind(payload))
}

/// Every shape is verifier-clean and compiles through `erlc`.
#[test]
fn nlr_closure_call_corpus_is_clean() {
    let mut failures = Vec::new();
    for (name, src, _) in corpus() {
        if std::panic::catch_unwind(|| assert_compiles_through_erlc("test", &compile(&src)))
            .is_err()
        {
            failures.push(format!("{name}:\n{src}"));
        }
    }
    assert!(
        failures.is_empty(),
        "{} case(s) fail verified codegen:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// With the fix off, every shape whose call stored nothing before it reports
/// `UnbracketedNlrClosure`, in every position and with or without a field
/// write; the shapes another rule covers stay clean. In the `ifTrue:` arm the
/// conditional is itself a call that may run a `^`: where it does not bracket
/// (no state touched in the arm) its non-Boolean fallback send relies on the
/// fix too, so a finding there may name `ifTrue:` instead.
#[test]
fn unstored_nlr_closure_calls_report_with_the_fix_off() {
    let mut wrong = Vec::new();
    let mut reported = 0;
    for (name, src, needs_fix) in corpus() {
        let finding = with_fix_off(|| report(&src));
        let in_arm = name.contains("/ if_true_arm /");
        let ok = match (&finding, needs_fix) {
            (Some(f), true) => f.contains("UnbracketedNlrClosure"),
            (None, false) => true,
            (Some(f), false) => {
                in_arm && f.contains("UnbracketedNlrClosure { selector: \"ifTrue:\"")
            }
            (None, true) => false,
        };
        reported += usize::from(finding.is_some());
        if !ok {
            wrong.push(format!(
                "{name}: expected a finding: {needs_fix}, got: {finding:?}\n{src}"
            ));
        }
    }
    assert!(
        wrong.is_empty(),
        "{} case(s) disagree:\n\n{}",
        wrong.len(),
        wrong.join("\n\n")
    );
    let needing = CALLS.iter().filter(|(_, _, needs)| *needs).count();
    assert!(reported >= needing * POSITIONS.len() * PREFIXES.len());
}

/// Runs `f` with the `^`-closure pre-store reading the state before the
/// operand its call follows (`FAULT_PRESTORE_READS_BEFORE_OPERANDS`).
fn with_prestore_read_early<T>(f: impl FnOnce() -> T) -> T {
    let set = |on: bool| {
        FAULT_PRESTORE_READS_BEFORE_OPERANDS.with(|flag| flag.set(on));
        PRESTORE_EARLY_READ.with(|slot| slot.borrow_mut().clear());
    };
    set(true);
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(f));
    set(false);
    result.unwrap_or_else(|payload| std::panic::resume_unwind(payload))
}

/// Whether corpus row `name` is a pre-store after a `State`-stepping
/// receiver.
fn is_state_step_prestore(name: &str) -> bool {
    let call = name.split(" / ").next().unwrap_or_default();
    call == "state_step_conditional" || STATE_STEP_PRESTORES.iter().any(|(n, _, _)| *n == call)
}

/// BT-4612: with the pre-store reading the state before its operands are
/// threaded (BT-4532's first cut), every row whose conditional's receiver
/// steps `State` ahead of the pre-store reports `StalePrestore`, in every
/// position and with or without a field write, and no other row reports
/// anything: a store read where its operands are not yet threaded is the
/// verifier's finding, not a silently lost write.
#[test]
fn a_prestore_read_before_its_operands_reports_stale() {
    let mut wrong = Vec::new();
    let mut reported = 0;
    for (name, src, _) in corpus() {
        let finding = with_prestore_read_early(|| report(&src));
        let ok = if is_state_step_prestore(&name) {
            finding
                .as_deref()
                .is_some_and(|f| f.contains("StalePrestore"))
        } else {
            finding.is_none()
        };
        reported += usize::from(finding.is_some());
        if !ok {
            wrong.push(format!("{name}: got: {finding:?}\n{src}"));
        }
    }
    assert!(
        wrong.is_empty(),
        "{} case(s) disagree:\n\n{}",
        wrong.len(),
        wrong.join("\n\n")
    );
    assert_eq!(
        reported,
        (STATE_STEP_PRESTORES.len() + 1) * POSITIONS.len() * PREFIXES.len()
    );
}

/// BT-4532: a `^`-closure of one send nested in a non-block operand of
/// another send whose closures a store covers (here `on:do:`'s handler, whose
/// catch boundary stores on entry). The nested `[^4] value` runs while the
/// handler operand is evaluated, before that store, so it is covered only by
/// its own call's bracket: with the `^` rule off it reports by its own
/// selector (`value`), not exempt under the `on:do:` cover.
const NESTED_IN_OPERAND: &str = "[p foo] on: Error do: ([^4] value)";

fn nested_in_operand_src() -> String {
    format!("{HEADER}\n  probe: p =>\n    self.n := 6.\n    {NESTED_IN_OPERAND}.\n    0\n")
}

#[test]
fn nlr_closure_nested_in_a_non_block_operand_is_clean() {
    assert_compiles_through_erlc("test", &compile(&nested_in_operand_src()));
}

#[test]
fn nlr_closure_nested_in_a_non_block_operand_reports_with_the_fix_off() {
    let finding = with_fix_off(|| report(&nested_in_operand_src()));
    assert!(
        finding
            .as_deref()
            .is_some_and(|f| f.contains("UnbracketedNlrClosure { selector: \"value\"")),
        "expected the nested `[^4] value` to report on its own: {finding:?}"
    );
}

/// How many times compiling an actor method whose `^` sits in conditionals
/// nested `depth` deep classifies a send.
fn classify_calls_at_depth(depth: usize) -> usize {
    let mut body = "^x".to_string();
    for k in 0..depth {
        body = format!("a{} ifTrue: [{body}]", k % 3);
    }
    let src = format!(
        "{HEADER}\n  probe: x with: a0 with: a1 with: a2 =>\n    self.n := 6.\n    {body}.\n    0\n"
    );
    CLASSIFY_SEND_CALLS.with(|n| n.set(0));
    compile(&src);
    CLASSIFY_SEND_CALLS.with(std::cell::Cell::get)
}

/// Deciding whether a `^`-holding conditional is inlined pure probes its
/// blocks for bracketed calls, and a nested conditional's own decision probes
/// again. A probe nested in another keeps the classes it caches for the rest
/// of the outer probe, so each send is classified a bounded number of times
/// per outermost probe: the count grows polynomially with the nesting depth,
/// where discarding them made it `2^depth - 1`.
#[test]
fn nested_nlr_conditional_probes_stay_polynomial() {
    let at_12 = classify_calls_at_depth(12);
    let at_14 = classify_calls_at_depth(14);
    assert!(
        at_12 < 12usize.pow(3) && at_14 < at_12 * 2,
        "classify_send calls: depth 12 -> {at_12}, depth 14 -> {at_14}"
    );
}
