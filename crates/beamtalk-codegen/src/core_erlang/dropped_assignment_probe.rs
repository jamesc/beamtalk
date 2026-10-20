// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3910 (ADR 0131 §1, §4): a recorder of the local assignments codegen
//! drops, and the corpus the agreement tests run it over.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! `generate_expression`'s `Expression::Assignment` arm emits only the value of
//! a local (identifier-target) assignment: it never rebinds the local, so an
//! assignment that reaches that arm loses its write (BT-3860). A value-position
//! local assignment is a producer (`threaded_expression`), so none should reach
//! it, and the arm reports one that does as
//! `VerifyError::UnthreadedLocalAssignment`. The agreement tests (this crate's
//! `tests/value_position_assignment_agreement.rs` for class, value-type and
//! actor methods, and `beamtalk-repl/tests/value_position_assignment_agreement.rs`
//! for REPL inputs) record the arm's hits while compiling [`corpus`] and prove
//! there are none.
//!
//! Test-only: compiled for this crate's tests and for dependents that enable
//! the `test` feature.

use beamtalk_core::source_analysis::Span;
use std::cell::RefCell;

thread_local! {
    static DROPPED: RefCell<Option<Vec<Span>>> = const { RefCell::new(None) };
}

/// Records that the local assignment at `span` was compiled to its value
/// only, when [`recording`] is collecting.
pub(crate) fn record(span: Span) {
    DROPPED.with(|d| {
        if let Some(spans) = d.borrow_mut().as_mut() {
            spans.push(span);
        }
    });
}

/// Runs `body`, returning its result and the span of every local assignment
/// it compiled to its value only (dropping the write).
pub fn recording<T>(body: impl FnOnce() -> T) -> (T, Vec<Span>) {
    DROPPED.with(|d| *d.borrow_mut() = Some(Vec::new()));
    let result = body();
    let spans = DROPPED.with(|d| d.borrow_mut().take()).unwrap_or_default();
    (result, spans)
}

/// The local assignments placed at `@A`: one that reads and writes `t`, and
/// one that writes a second local.
const ASSIGNMENTS: &[&str] = &["t := t + 1", "w := t + 1"];

/// The positions of `@A` in one statement: statement position (bare and
/// parenthesised), then value position.
const POSITIONS: &[&str] = &[
    "@A",
    "(@A)",
    "((@A))",
    // An assignment's value, chained and parenthesised, and an operand.
    "x := @A",
    "x := (@A)",
    "x := (@A) + 1",
    "(@A) + 1",
    "x := 1 + (@A)",
    "x := 0 max: (@A)",
    // A literal element and an interpolated segment.
    "#[t, (@A), t]",
    "x := #[(@A)]",
    "#{#k => (@A)}",
    "x := \"v{(@A)}\"",
    // A conditional's, a cascade's and a `match:`'s receiver, and an arm.
    "(@A) > 0 ifTrue: [u := 9]",
    "(@A) > 0 ifTrue: [u := 9] ifFalse: [u := 1]",
    "((@A) > 0) and: [true]",
    "(@A) isNil ifFalse: [u := 9]",
    "(@A) printString; size",
    "x := (@A) match: [_ -> 0]",
    "x := 3 match: [3 -> @A; _ -> 0]",
    // Inside a block that is itself in value position: its statements bind.
    "x := [:k | @A] value: 1",
    "x := [@A. t] value",
    // The value of `^`, bare and as an operand, and of a non-local return
    // from a block.
    "^@A",
    "^(@A) + 1",
    "x := [:k | ^@A] value: 1",
];

/// The bodies `@S` (one statement from [`POSITIONS`]) sits in, each with its
/// own statement lowering: the root, loop and fold bodies, inlined arms (as
/// the arm's value and before it), exception bodies and closures.
const BODIES: &[&str] = &[
    "@S",
    "#(1, 2) do: [:k | @S]",
    "#(1, 2) do: [:k | @S. k]",
    "1 to: 2 do: [:k | @S]",
    "2 timesRepeat: [@S]",
    "[s < 2] whileTrue: [s := s + 1. @S]",
    // BT-3910: a `whileTrue:`/`whileFalse:` condition block, as a statement of
    // the condition and as its value.
    "[@S. s < 2] whileTrue: [s := s + 1]",
    "[s := s + 1. @S. s < 2] whileTrue: [nil]",
    "[@S. s >= 2] whileFalse: [s := s + 1]",
    "x := #(1, 2) collect: [:k | @S. k]",
    "x := #(1, 2) inject: 0 into: [:a :k | @S. a + k]",
    "s > 0 ifFalse: [@S]",
    "s > 0 ifFalse: [@S. 1]",
    "x := s > 0 ifFalse: [@S]",
    "x := s > 0 ifTrue: [0] ifFalse: [@S]",
    "x := s > 0 ifTrue: [0] ifFalse: [@S. 1]",
    "x := s > 0 ifTrue: [@S. 1] ifFalse: [@S. 2]",
    "x := nil ifNil: [@S. 1]",
    "x := 5 ifNotNil: [:v | @S. v]",
    "x := (s > 0) and: [@S. true]",
    "x := (s > 0) or: [@S. true]",
    "x := #{} at: #k ifAbsent: [@S. 1]",
    "[@S] value",
    "x := [@S. 1] value",
    "[@S] on: Error do: [:e | 0]",
    "x := [@S. 1] on: Error do: [:e | 0]",
    "[@S. 1] ensure: [0]",
    "b := [@S]\nb value",
    "b := [@S. 1]\nb value",
];

/// Whether `why` (a compile failure rendered with `{:?}`) is the exact
/// `UnsupportedFeature` rejection of a Tier 2 block that assigns a local and
/// rebinds it inside a loop (PIN-BUG BT-3947, BT-3939): loud, not dropped. Any
/// other failure, a verifier panic included, is not it.
#[must_use]
pub fn is_tier2_loop_rebind_pin(why: &str) -> bool {
    why.starts_with("UnsupportedFeature")
        && why.contains("a Tier 2 block assigns local")
        && why.contains(super::blocks::TIER2_LOOP_REBIND_PIN)
}

/// The locals every corpus body reads, bound ahead of it.
pub const PRELUDE: &str = "t := 0\nu := 0\ns := 0\nw := 0\nx := 0";

/// Every body of the corpus (statements, to follow [`PRELUDE`]): `@A` in each
/// position, in each body, as the last statement and followed by one that
/// reads the locals.
#[must_use]
pub fn corpus() -> Vec<String> {
    let mut out = Vec::new();
    for assignment in ASSIGNMENTS {
        for position in POSITIONS {
            let statement = position.replace("@A", assignment);
            for body in BODIES {
                let body = body.replace("@S", &statement);
                out.push(format!("{body}\n#[t, u, w, x]"));
                out.push(body);
            }
        }
    }
    out
}
