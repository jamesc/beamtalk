// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4451: an Erlang FFI call whose selector is a conditional selector
//! (`Erlang beamtalk_guard_return ifTrue: c`) is an FFI call, not a
//! conditional.
//!
//! `conditional_send_receiver` took it for a conditional, so a state-threading
//! consumer (an actor conditional over it whose arm writes a field) sequenced
//! its `Erlang <module>` receiver ahead. `try_handle_erlang_interop` consumes
//! that receiver structurally and never reads a sequenced value back, so
//! `finish_precompiled_scope` failed with "pre-sequenced sub-expression never
//! substituted". `sequenced_send_children` likewise left the call opaque, so
//! an argument that advances `State` was never sequenced.
//!
//! **Invariant** (every scope kind): the sequencing rule never registers an
//! operand its parent's lowering does not read back. A send is a conditional
//! only when the compiler inlines it — a conditional selector with literal
//! block arguments; any other conditional-selector send is a covered send (an
//! FFI call's arguments are sequenced, its receiver never is). One predicate
//! (`is_conditional_send`) decides it for both the conditional-receiver and the
//! send-children recognizers.
//!
//! [`ffi_conditional_selector_corpus_compiles`] proves it over call x argument
//! x position x method context (and, in the actor, FFI receivers with literal
//! block arguments, which do lower as a conditional): every row passes the debug
//! verifier and the sequencing check, and compiles through `erlc`.

use super::{assert_compiles_through_erlc, assert_parses, codegen};
use std::panic::{AssertUnwindSafe, catch_unwind};

/// Sends with a conditional selector, over an argument `{A}` that is not a
/// literal block (so the send is not a conditional the compiler inlines): FFI
/// calls, and (the same rule) an ordinary receiver.
const CALLS: &[&str] = &[
    "(Erlang beamtalk_guard_return ifTrue: {A})",
    "(Erlang beamtalk_guard_return ifFalse: {A})",
    "(Erlang m ifTrue: {A} ifFalse: {A})",
    "(Erlang m and: {A})",
    "(Erlang m or: {A})",
    "(Erlang m ifNil: {A})",
    "(Erlang m ifNotNil: {A})",
    "(c ifTrue: {A})",
    "(c and: {A})",
];

/// FFI receivers with a conditional selector and literal block arguments,
/// which the compiler does inline as a conditional over the module proxy
/// (actor context only: the arm `{W}` writes a field).
const BLOCK_CALLS: &[&str] = &[
    "(Erlang m ifTrue: [{W}. true])",
    "(Erlang m ifFalse: [{W}. true])",
    "(Erlang m ifTrue: [{W}. true] ifFalse: [false])",
    "(Erlang m and: [{W}. true])",
    "(Erlang m or: [{W}. false])",
];

/// The argument: plain, an outer-local rebind (every context: analysis
/// accepts it, and before BT-4451 codegen failed it in every context), and
/// (actor contexts only) an operand whose evaluation advances `State` — a
/// self-send, a field write.
const ARGS: &[(&str, bool)] = &[
    ("c", false),
    ("(x := c)", false),
    ("(self probe: c)", true),
    ("(self.n := c)", true),
];

/// Where the FFI call `{F}` sits: the receiver of a conditional whose arm writes
/// state, of `and:`/`or:`, an operand of a binary send, a self-send argument,
/// an assignment, and a statement of a loop body / conditional arm / `on:do:`.
const POSITIONS: &[&str] = &[
    "{F} ifTrue: [1] ifFalse: [{W}. 3]",
    "{F} ifTrue: [{W}]",
    "({F} and: [c == 1]) ifTrue: [{W}]",
    "{F} or: [{W}. false]",
    "x := {F}",
    "x := {F} == true",
    "3 timesRepeat: [{F} ifTrue: [{W}]]",
    "(c == 1) ifTrue: [{F} ifFalse: [{W}]]",
    "[{F} ifTrue: [{W}]] on: Error do: [:e | 0]",
];

/// The method contexts: (name, class header, arm write `{W}`, whether `State`
/// producers are available in the FFI argument).
const CONTEXTS: &[(&str, &str, &str, bool)] = &[
    (
        "actor",
        "Actor subclass: FfiCondProbe\n  state: n = 0\n  probe: k => k\n",
        "self.n := 2",
        true,
    ),
    (
        "object",
        "Object subclass: FfiCondProbe\n  probe: k => k\n",
        "x := 2",
        false,
    ),
    (
        "value",
        "Value subclass: FfiCondProbe\n  state: n :: Integer = 0\n  probe: k => k\n",
        "x := 2",
        false,
    ),
];

fn rows(stateful: bool) -> Vec<(String, String)> {
    let mut calls = Vec::new();
    for (f, call) in CALLS.iter().enumerate() {
        for (a, (arg, needs_state)) in ARGS.iter().enumerate() {
            if !*needs_state || stateful {
                calls.push((format!("call{f} arg{a}"), call.replace("{A}", arg)));
            }
        }
    }
    if stateful {
        calls.extend(
            BLOCK_CALLS
                .iter()
                .enumerate()
                .map(|(b, call)| (format!("block{b}"), (*call).to_string())),
        );
    }
    let mut out = Vec::new();
    for (name, call) in &calls {
        for (p, pos) in POSITIONS.iter().enumerate() {
            out.push((format!("{name} pos{p}"), pos.replace("{F}", call)));
        }
    }
    out
}

fn method(i: usize, body: &str, write: &str) -> String {
    format!(
        "  m{i}: c =>\n    x := 0\n    {}\n    x\n",
        body.replace("{W}", write)
    )
}

fn passes(src: &str) -> bool {
    catch_unwind(AssertUnwindSafe(|| {
        assert_compiles_through_erlc("test", &codegen(src));
    }))
    .is_ok()
}

/// BT-4451: every row compiles (sequencing check and debug verifier) and
/// passes `erlc` (one run per context; bisected to the failing rows on
/// failure).
#[test]
fn ffi_conditional_selector_corpus_compiles() {
    let mut failures = Vec::new();
    let mut total = 0;
    for (ctx, header, write, stateful) in CONTEXTS {
        let rows = rows(*stateful);
        total += rows.len();
        let methods: Vec<String> = rows
            .iter()
            .enumerate()
            .map(|(i, (_, body))| method(i, body, write))
            .collect();
        for m in &methods {
            assert_parses(&format!("{header}{m}"));
        }
        if passes(&format!("{header}{}", methods.concat())) {
            continue;
        }
        failures.extend(rows.iter().zip(&methods).filter_map(|((name, _), m)| {
            let single = format!("{header}{m}");
            (!passes(&single)).then(|| format!("{ctx} / {name}:\n{single}"))
        }));
    }
    assert!(
        total >= 300,
        "only {total} rows; the corpus has thinned out"
    );
    assert!(
        failures.is_empty(),
        "{} FFI conditional-selector row(s) fail verified codegen:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}
