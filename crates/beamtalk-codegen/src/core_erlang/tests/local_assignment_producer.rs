// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3910 (ADR 0131 §1, §1a, §4): a local assignment in value position is a
//! producer, lowered as a `LocalRebind` of the enclosing frame.
//!
//! [`ROWS`] is the BT-3860 repro table (every body runs after `t := 0` and
//! `u := 0`). Three proofs over it, in a class, a value-type and an actor
//! method:
//!
//! 1. each row compiles through the debug `ThreadedIr` verifier and `erlc`;
//! 2. with the producer gate disabled ([`LocalRebindFault::NotAProducer`]), the
//!    assignment reaches the value-only lowering and every row fails with
//!    `VerifyError::UnthreadedLocalAssignment`, the check that fails on the
//!    repro before the fix;
//! 3. `#[t, (t := 7), t]` fails `LocalReadAfterSiblingRebind` when the §1a
//!    snapshot is skipped, so the rule counts the new producer.
//!
//! The runtime answers (`#[0, 7, 7]`, `t` = 2, ...) are `BUnit` tests:
//! `stdlib/test/adr0131nested_assign_value_position_test.bt`. The corpus proof
//! is `value_position_assignment_agreement.rs`.

use super::{assert_compiles_through_erlc, codegen, erlc_available, verifier_report};
use crate::core_erlang::threaded_expr::fault::LocalRebindFault;

/// The BT-3860 repro table: a row name and a body.
const ROWS: &[(&str, &str)] = &[
    ("operand", "x := (t := t + 1) + 1\n#[t, x]"),
    ("literal element", "#[t, (t := 7), t]"),
    ("assignment value", "x := t := 7\n#[t, x]"),
    (
        "conditional receiver",
        "(t := t + 1) > 0 ifTrue: [u := 9]\n#[t, u]",
    ),
    ("and: receiver", "((t := t + 1) > 0) and: [true]\n#[t, u]"),
    (
        "do: body conditional",
        "#(1, 2) do: [:k | (t := t + 1) > 1 ifTrue: [u := 9]]\n#[t, u]",
    ),
    ("do: body value", "#(1, 2) do: [:k | (t := t + 1) > 0]\nt"),
    (
        "to:do: body",
        "1 to: 2 do: [:k | (t := t + 1) > 1 ifTrue: [u := 9]]\n#[t, u]",
    ),
    (
        "timesRepeat: body",
        "2 timesRepeat: [(t := t + 1) > 1 ifTrue: [u := 9]]\n#[t, u]",
    ),
    (
        "whileTrue: body",
        "s := 0\n[s < 2] whileTrue: [s := s + 1. (t := t + 1) > 1 ifTrue: [u := 9]]\n#[t, u]",
    ),
    (
        "collect: body",
        "r := #(1, 2) collect: [:k | (t := t + 1) > 1 ifTrue: [u := 9] ifFalse: [u := 1]. k]\n#[t, u, r]",
    ),
    (
        "inject:into: body",
        "r := #(1, 2) inject: 0 into: [:a :k | (t := t + 1) > 1 ifFalse: [u := 9]. a + k]\n#[t, u, r]",
    ),
    ("returned", "^t := t + 1"),
    ("returned operand", "^(t := t + 1) + 1"),
];

/// `body` as the body of a class, a value-type and an actor method.
fn sources(body: &str) -> [(&'static str, String); 3] {
    let mut indented = String::new();
    for line in format!("t := 0\nu := 0\n{body}").lines() {
        indented.push_str("    ");
        indented.push_str(line);
        indented.push('\n');
    }
    [
        (
            "class",
            format!("Object subclass: Producer\n  class go =>\n{indented}"),
        ),
        (
            "value type",
            format!("Value subclass: Producer\n  go =>\n{indented}"),
        ),
        (
            "actor",
            format!("Actor subclass: Producer\n  state: n = 0\n\n  go =>\n{indented}"),
        ),
    ]
}

#[test]
fn every_repro_row_compiles_through_the_verifier_and_erlc() {
    for (row, body) in ROWS {
        for (context, src) in sources(body) {
            let code = codegen(&src);
            if erlc_available() {
                assert_compiles_through_erlc("test", &code);
            }
            assert!(
                code.contains("_Assigned"),
                "{row} ({context}): no producer carrier in\n{code}"
            );
        }
    }
}

#[test]
fn the_unthreaded_assignment_check_fails_every_repro_row_when_the_producer_is_off() {
    for (row, body) in ROWS {
        for (context, src) in sources(body) {
            match verifier_report(&src, Some(LocalRebindFault::NotAProducer)) {
                Some(report) if report.contains("UnthreadedLocalAssignment") => {}
                other => panic!(
                    "{row} ({context}): expected UnthreadedLocalAssignment, got {other:?}\n{src}"
                ),
            }
        }
    }
}

#[test]
fn a_value_position_assignment_is_a_sibling_rebind() {
    for (context, src) in sources("#[t, (t := 7), t]") {
        assert_eq!(verifier_report(&src, None), None, "{context}");
        match verifier_report(&src, Some(LocalRebindFault::SkipSiblingSnapshot)) {
            Some(report) if report.contains("LocalReadAfterSiblingRebind") => {}
            other => panic!("{context}: expected LocalReadAfterSiblingRebind, got {other:?}"),
        }
    }
}

/// A statement-position assignment is not a producer: it keeps its lowering.
#[test]
fn a_statement_position_assignment_is_not_a_producer() {
    for (context, src) in sources("t := t + 1\nx := t + 1\n#(1, 2) do: [:k | t := t + k]\n#[t, x]")
    {
        let code = codegen(&src);
        assert!(
            !code.contains("_Assigned"),
            "{context}: a statement was lowered as a producer in\n{code}"
        );
    }
}

/// A legacy module-level body (`name := [block]`, the workspace dispatch) installs
/// the value-position sites too: the reviewer's repro `foo := [:a | t := 0. x :=
/// (t := a) + 1. t]` answered 0 with the write dropped silently. The final `t`
/// now reads the rebound value (the carrier of the assignment), and the module
/// compiles through the verifier and `erlc`.
#[test]
fn a_legacy_module_body_lowers_a_value_position_assignment() {
    let code = codegen("foo := [:a |\n    t := 0\n    x := (t := a) + 1\n    t\n]\n");
    assert!(code.contains("_Assigned"), "no producer carrier in\n{code}");
    if erlc_available() {
        assert_compiles_through_erlc("test", &code);
    }
}
