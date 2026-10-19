// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0133 Phase 3 (BT-3911): the `ArmLocalPutNotCarried` verifier invariant over a generated
//! corpus of real compiles, not only hand-built IR (`threaded_ir/tests/local_rebind.rs`).
//!
//! **Invariant** (every arm of every frame): an arm is a `BranchArm` frame that carries the
//! outer locals its construct threads (`threads`). An assignment in the arm to any other local
//! (one bound in the arm, which the construct's result tuple never hands back) is a plain `let`:
//! it is never written into the arm's scratch map. A write the frame does not carry is dead at
//! best, and at worst the sign of an arm lowered against a scratch map its frame does not have
//! (a conditional nested in a `whileTrue:` condition closure wrote its block-local into an
//! unbound `StateAcc`, BT-3891).
//!
//! The corpus is the cross product of every construct that has arms (conditionals,
//! `on:do:`/`ensure:` blocks, a `detect:ifNone:` handler) writing a local of its own and every
//! frame it can sit in (a method-root statement, a loop body, a loop condition, a `do:` body, a
//! protected body, a handler, an arm of another conditional), in a class, a value-type and an
//! actor method. Each is compiled twice: unbroken (control: compiles and the verifier is
//! silent) and with the frame's slot gate removed
//! ([`fault::LocalRebindFault::PutUncarriedLocal`]), which the verifier must report wherever the
//! arm is lowered inline.

use super::{codegen, verifier_report};
use crate::core_erlang::threaded_expr::fault::LocalRebindFault;

/// The method headers, by context.
const CONTEXTS: &[(&str, &str)] = &[
    (
        "class",
        "Object subclass: ArmCorpus\n  classState: n = 0\n\n  class probe: c =>\n",
    ),
    ("value", "Value subclass: ArmCorpus\n\n  probe: c =>\n"),
    (
        "actor",
        "Actor subclass: ArmCorpus\n  state: n = 0\n\n  probe: c =>\n",
    ),
];

/// Constructs with arms, each binding the local `x` of its own arm.
const CONSTRUCTS: &[(&str, &str)] = &[
    ("ifTrue", "c ifTrue: [x := 1. x]"),
    ("ifFalse", "c ifFalse: [x := 1. x]"),
    (
        "ifTrueIfFalse",
        "c ifTrue: [x := 1. x] ifFalse: [y := 2. y]",
    ),
    ("ifNil", "c ifNil: [x := 1. x]"),
    ("and", "c and: [x := 1. x > 0]"),
    ("or", "c or: [x := 1. x > 0]"),
    ("onDoBody", "[x := 1. x] on: Error do: [:e | 0]"),
    ("onDoHandler", "[1] on: Error do: [:e | x := 1. x]"),
    ("ensure", "[1] ensure: [x := 1. x]"),
    (
        "detectHandler",
        "#(1, 2) detect: [:e | e > 5] ifNone: [x := 1. x]",
    ),
];

/// Frames the construct `EXPR` can sit in.
const FRAMES: &[(&str, &str)] = &[
    ("root", "    EXPR\n"),
    (
        "loop body",
        "    i := 0\n    [i < 2] whileTrue: [i := i + 1. EXPR]\n",
    ),
    (
        "loop condition",
        "    i := 0\n    [EXPR. i < 2] whileTrue: [i := i + 1]\n",
    ),
    ("do body", "    #(1, 2) do: [:z | EXPR]\n"),
    ("protected body", "    [EXPR. 7] on: Error do: [:e | 0]\n"),
    ("handler", "    [7] on: Error do: [:e | EXPR]\n"),
    ("arm", "    c ifTrue: [EXPR]\n"),
];

/// The corpus: `(case name, program)`.
fn programs() -> Vec<(String, String)> {
    let mut out = Vec::new();
    for (context, header) in CONTEXTS {
        for (construct, expr) in CONSTRUCTS {
            for (frame, template) in FRAMES {
                let body = template.replace("EXPR", expr);
                out.push((
                    format!("{context} / {construct} / {frame}"),
                    format!("{header}    t := 0\n{body}    t\n"),
                ));
            }
        }
    }
    out
}

#[test]
fn every_unbroken_arm_local_write_compiles_with_a_silent_verifier() {
    let failures: Vec<String> = programs()
        .into_iter()
        .filter_map(|(name, src)| {
            verifier_report(&src, None).map(|report| format!("{name}: {report}\n{src}"))
        })
        .collect();
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
}

#[test]
fn an_arm_local_put_its_frame_does_not_carry_is_reported_wherever_the_arm_is_lowered_inline() {
    let mut reported = 0;
    let mut wrong = Vec::new();
    for (name, src) in programs() {
        match verifier_report(&src, Some(LocalRebindFault::PutUncarriedLocal)) {
            Some(report) if report.contains("ArmLocalPutNotCarried") => reported += 1,
            Some(report) => wrong.push(format!("{name}: wrong report: {report}\n{src}")),
            // An arm that is not lowered inline (a plain closure) writes no scratch map:
            // the fault changes nothing, so its output is the unbroken compile's.
            None => {
                let broken = crate::core_erlang::threaded_expr::fault::with(
                    LocalRebindFault::PutUncarriedLocal,
                    || codegen(&src),
                );
                if broken != codegen(&src) {
                    wrong.push(format!("{name}: changed output without a report\n{src}"));
                }
            }
        }
    }
    assert!(wrong.is_empty(), "{}", wrong.join("\n\n"));
    assert!(
        reported >= 60,
        "only {reported} of {} cases lower an arm inline: the corpus has thinned out",
        programs().len()
    );
}
