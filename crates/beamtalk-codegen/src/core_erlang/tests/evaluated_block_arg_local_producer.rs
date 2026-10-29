// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4263 (ADR 0131 §2, the Local half of BT-3982): an evaluated literal
//! block with arguments (`[:y | t := y] value: 5`, `value:value:`) that writes
//! an outer local is a local-threading producer wherever the unary form is
//! (`conditional_arm_blocks`, the one site rule): its arguments are threaded
//! first, in the enclosing frame, then its block is the degenerate
//! conditional's arm that always runs (`generate_evaluated_block_producer`).
//!
//! The producer binds the block's parameters in a variable scope of its own,
//! and the arm's `LocalRebind`s are bound inside it by the shared
//! `construct_rebind_prelude`. Popping that scope used to drop them, so every
//! later read of the local saw its pre-construct value: `{[:y | t := y] value:
//! 5, t}` answered `{5, 0}`, `erlc`-clean. [`ROWS`] is the issue's repro
//! table, in class, value-type and actor methods. Two proofs over it:
//!
//! 1. each row compiles through the debug `ThreadedIr` verifier and `erlc`;
//! 2. with the rebinding after the scope pop disabled
//!    ([`LocalRebindFault::EvaluatedBlockRebindsScoped`], the lowering before
//!    the fix), every row fails with `VerifyError::RebindNotVisible`, the check
//!    that fails on the repro before the fix.
//!
//! The runtime answers are the `arg*` cells of
//! `stdlib/test/adr0131eval_block_test.bt` (the BT-3841 value oracle, its
//! `value:` column). The corpus proof is the evaluated-block rows of the
//! conditional agreement corpus (`local_producer_agreement_conditional.rs`:
//! `CONDITIONALS`, `ARM_FRAMES`, `ARM_CONSTRUCTS`), compiled through the
//! verifier in every position and method context.

use super::{assert_compiles_through_erlc, codegen, erlc_available, verifier_report};
use crate::core_erlang::threaded_expr::fault::LocalRebindFault;

/// The BT-4263 repro table: a row name and a method body (after `t := 0`).
/// Every row reads `t` after the `value:` send, so a dropped rebind is
/// observable.
const ROWS: &[(&str, &str)] = &[
    ("tuple element", "{[:y | t := y] value: 5, t}"),
    ("list element", "#[[:y | t := y] value: 5, t]"),
    ("keyword argument", "self take: ([:y | t := y] value: 5)\nt"),
    ("statement", "[:y | t := y] value: 5\nt"),
    ("assigned", "r := [:y | t := y] value: 5\n#[r, t]"),
    (
        "binary operand",
        "r := ([:y | t := t + y. 1] value: 2) + t\n#[r, t]",
    ),
    (
        "two arguments",
        "{[:a :b | t := a + b] value: 2 value: 3, t}",
    ),
    (
        "argument writes the local",
        "{[:y | t := t + y] value: (t := 5), t}",
    ),
    ("in an arm", "4 > 3 ifTrue: [[:y | t := y] value: 5]\nt"),
    (
        "in a do: body",
        "#(1, 2) do: [:e | [:y | t := t + y] value: e]\nt",
    ),
    (
        "in a protected body",
        "[[:y | t := y] value: 5] on: Error do: [:e | 0]\nt",
    ),
    ("nested", "[:a | [:y | t := t + y] value: a] value: 5\nt"),
];

/// `body` as the probe method of a class (class side), value type or actor,
/// each with a `take:` that answers its argument.
fn sources(body: &str) -> [String; 3] {
    let indented = format!(
        "    t := 0\n{}",
        body.lines()
            .map(|l| format!("    {l}"))
            .collect::<Vec<_>>()
            .join("\n")
    );
    [
        format!(
            "Object subclass: EvalArgClass\n  class take: v => v\n\n  class go =>\n{indented}\n"
        ),
        format!("Value subclass: EvalArgValue\n  take: v => v\n\n  go =>\n{indented}\n"),
        format!("Actor subclass: EvalArgActor\n  take: v => v\n\n  go =>\n{indented}\n"),
    ]
}

#[test]
fn every_repro_row_compiles_through_the_verifier_and_erlc_in_every_context() {
    for (row, body) in ROWS {
        for src in sources(body) {
            assert_eq!(verifier_report(&src, None), None, "{row}\n{src}");
            if erlc_available() {
                assert_compiles_through_erlc("test", &codegen(&src));
            }
        }
    }
}

#[test]
fn the_rebind_visibility_check_fails_every_repro_row_when_the_rebinds_stay_scoped() {
    for (row, body) in ROWS {
        for src in sources(body) {
            match verifier_report(&src, Some(LocalRebindFault::EvaluatedBlockRebindsScoped)) {
                Some(report) if report.contains("RebindNotVisible") => {}
                other => panic!("{row}: expected RebindNotVisible, got {other:?}\n{src}"),
            }
        }
    }
}
