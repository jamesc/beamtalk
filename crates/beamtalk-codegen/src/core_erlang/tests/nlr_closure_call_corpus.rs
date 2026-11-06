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

use super::assert_compiles_through_erlc;
use crate::core_erlang::synced_call::{CLASSIFY_SEND_CALLS, FAULT_NLR_CLOSURES_UNSTORED};
use crate::core_erlang::{CodegenOptions, generate_module};
use beamtalk_core::source_analysis::{lex_with_eof, parse};

const HEADER: &str = "Actor subclass: NlrClosureCallProbe
  state: n = 0
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
    for (call_name, call, needs_fix) in CALLS {
        for (pos_name, pos) in POSITIONS {
            for (prefix_name, prefix) in PREFIXES {
                let body = pos.replace("CALL", call);
                let src = format!(
                    "{HEADER}\n  probe: p with: q =>\n    m := 0.\n    {prefix}{body}.\n    0\n"
                );
                out.push((
                    format!("{call_name} / {pos_name} / {prefix_name}"),
                    src,
                    *needs_fix,
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
