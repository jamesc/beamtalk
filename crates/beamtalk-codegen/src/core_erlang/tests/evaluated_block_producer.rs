// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3982 (ADR 0118 §Decision 2, ADR 0131 §4): an actor evaluated literal
//! block (`[...] value`, `[:y | ...] value: x`, `value:value:`) that writes a
//! field, sends to `self` or binds a local is a `ThreadedIr` producer wherever
//! it sits (`generate_evaluated_block_producer`): one carrier in the enclosing
//! frame and its `State` `Bind`, answering `element(1, Carrier)`.
//!
//! [`ROWS`] is the BT-3982 repro table. Two proofs over it, in an actor method:
//!
//! 1. each row compiles through the debug `ThreadedIr` verifier and `erlc`;
//! 2. with the producer gate disabled
//!    ([`LocalRebindFault::EvaluatedBlockNotAProducer`]), the send reaches the
//!    legacy inline lowering, whose raw `{Value, State}` tuple became the value
//!    (`{[:y | self.count := y] value: 5, self.count}` answered
//!    `{{5, Counter(count: 5)}, 0}`), and every row fails with
//!    `VerifyError::UnthreadedEvaluatedBlock`, the check that fails on the
//!    repro before the fix.
//!
//! The runtime answers are `BUnit` tests (the value oracle):
//! `stdlib/test/bt3982evalblock_test.bt` and the full host x position grid of
//! `stdlib/test/bt3981evalblock_test.bt`. The corpus proof over every value
//! position and enclosing shape is `field_assignment_value_position.rs`
//! (`EXPRS`).

use super::{assert_compiles_through_erlc, codegen, erlc_available, verifier_report};
use crate::core_erlang::threaded_expr::fault::LocalRebindFault;

/// The BT-3982 repro table: a row name and an actor method body.
const ROWS: &[(&str, &str)] = &[
    (
        "tuple element",
        "{[:y | self.count := y] value: 5, self.count}",
    ),
    ("unary", "{[self.count := 5] value, self.count}"),
    (
        "array element",
        "#[[:y | self.count := y] value: 5, self.count]",
    ),
    (
        "list element",
        "#([:y | self.count := y] value: 5, self.count)",
    ),
    (
        "keyword arguments",
        "self pair: ([:y | self.count := y] value: 5) with: self.count",
    ),
    (
        "keyword argument",
        "self take: ([:y | self.count := y] value: 5)",
    ),
    (
        "binary operand",
        "([:y | self.count := y] value: 5) + self.count",
    ),
    (
        "two arguments",
        "{[:y :z | self.count := y + z] value: 2 value: 3, self.count}",
    ),
    (
        "three arguments",
        "{[:a :b :c | self.count := a + b + c] value: 1 value: 2 value: 2, self.count}",
    ),
    (
        "argument writes a field",
        "{[:y | self.m := y] value: (self.count := 5), self.count, self.m}",
    ),
    ("own local", "([n := 3. n] value) + 1"),
    ("self-send", "#(3) includes: [self peek: 3] value"),
    (
        "in an arm",
        "flag := true\nflag ifTrue: [{[:y | self.count := y] value: 5, self.count}] ifFalse: [0]",
    ),
    (
        "in a collect: body",
        "#(5) collect: [:z | {[:y | self.count := y] value: z, self.count}]",
    ),
    (
        "in a protected body",
        "[{[:y | self.count := y] value: 5, self.count}] on: Error do: [:e | 0]",
    ),
];

fn source(body: &str) -> String {
    let indented = body
        .lines()
        .map(|l| format!("    {l}"))
        .collect::<Vec<_>>()
        .join("\n");
    format!(
        "Actor subclass: EvalBlockProducer\n  state: count = 0\n  state: m = 0\n\n  pair: a with: b => #(a, b)\n\n  take: v => v\n\n  peek: v => v\n\n  go =>\n{indented}\n"
    )
}

#[test]
fn every_repro_row_compiles_through_the_verifier_and_erlc() {
    for (row, body) in ROWS {
        let src = source(body);
        assert_eq!(verifier_report(&src, None), None, "{row}\n{src}");
        if erlc_available() {
            assert_compiles_through_erlc("test", &codegen(&src));
        }
    }
}

#[test]
fn the_unthreaded_evaluated_block_check_fails_every_repro_row_when_the_producer_is_off() {
    for (row, body) in ROWS {
        let src = source(body);
        match verifier_report(&src, Some(LocalRebindFault::EvaluatedBlockNotAProducer)) {
            Some(report) if report.contains("UnthreadedEvaluatedBlock") => {}
            other => panic!("{row}: expected UnthreadedEvaluatedBlock, got {other:?}\n{src}"),
        }
    }
}

/// A statement-position evaluated block is the producer too (no Tier 2
/// statement unpack is left for a literal block receiver in an actor method),
/// and the field write reaches the next statement.
#[test]
fn a_statement_evaluated_block_threads_state_to_the_next_statement() {
    for body in [
        "[:y | self.count := y] value: 5\nself.count",
        "r := [:y | self.count := y] value: 5\nr + self.count",
        "^[:y | self.count := y] value: 5",
        "#(1, 2) do: [:z | r := z. [n := 3. n] value]\nnil",
        "3 match: [3 -> [self peek: 3] value; _ -> 0]",
    ] {
        let src = source(body);
        assert_eq!(verifier_report(&src, None), None, "{src}");
        if erlc_available() {
            assert_compiles_through_erlc("test", &codegen(&src));
        }
    }
}
