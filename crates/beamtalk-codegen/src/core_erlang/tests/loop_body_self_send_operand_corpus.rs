// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3809: a self-send statement in a loop or fold body sequences its operands.
//!
//! A `do:`/`collect:`/`inject:into:`/... body statement `self take: (self bump) with: (self
//! bump)` compiled its operands into the dispatch's closed argument list: each operand's
//! `State` rebind was scoped inside the list, so a single operand's rebind was dropped
//! silently (the dispatch read the pre-operand `StateAcc`) and a second operand read an
//! unbound version (`erlc`: `unbound variable 'StateAcc1'`). The Letrec bodies
//! (`whileTrue:`, `to:do:`, `timesRepeat:`) and the method body were fine because they
//! lower a self-send through `threaded_expression`'s producer, which sequences the
//! operands (ADR 0118 §Decision 3) and splices their prelude into the statement sequence.
//!
//! The invariant is `VerifyError::StateEffectEscapesExpression`, reported by
//! `capture_argument_list_doc` when an operand's prelude carries a versioned `Bind` (an
//! operand whose rebind a closed argument list would scope away); a body that sequences
//! its operands never reaches it. The owner of the fold-body statement scope is
//! `lower_foldl_body_in_sites`, which now lowers a self-send statement through the same
//! `sequenced_self_dispatch` as `threaded_expression` and the Letrec body.
//!
//! [`loop_body_self_send_operands_thread_state`] proves it over operand shape x body shape:
//! each program must pass the debug `ThreadedIr` verifier and compile through `erlc`.

use super::{assert_compiles_through_erlc, codegen};
use std::panic::{AssertUnwindSafe, catch_unwind};

const HEADER: &str = "Actor subclass: LoopSelfSendProbe
  state: n = 0
  state: m = 0

  bump -> Integer =>
    self.n := self.n + 1
    self.n

  take: x -> Integer => x
  take: x with: y -> Integer => x
";

/// A self-send statement (`STMT`) with a state-changing operand at the named position.
const SELF_SENDS: &[(&str, &str)] = &[
    ("no_operand", "self bump"),
    ("pure_operand", "self take: 3"),
    ("one_state_operand", "self take: (self bump)"),
    (
        "two_state_operands",
        "self take: (self bump) with: (self bump)",
    ),
    (
        "operand_then_state_read",
        "self take: (self bump) with: self.n",
    ),
    (
        "state_read_then_operand",
        "self take: self.n with: (self bump)",
    ),
    (
        "field_assignment_operand",
        "self take: (self.n := 5) with: (self bump)",
    ),
    (
        "nested_self_send_operand",
        "self take: (self take: (self bump))",
    ),
    (
        "binary_of_state_operands",
        "self take: (self bump) + (self bump)",
    ),
    (
        "cascade_operand",
        "self take: (Object classBuilder name: #A; superclass: (self bump))",
    ),
    (
        "sibling_before_state_operand",
        "self take: 1 with: (self bump)",
    ),
];

/// A loop or fold body holding the statement `STMT`: the fold kinds
/// (`BodyKind::Foldl*`), where `STMT` is the last expression (whose value the kind's
/// epilogue consumes) or followed by one, and the Letrec kinds as the control group.
const BODIES: &[(&str, &str)] = &[
    ("do_last", "    #(1, 2) do: [:e | STMT].\n    self.n\n"),
    (
        "do_then_read",
        "    #(1, 2) do: [:e | STMT. self.n].\n    self.n\n",
    ),
    (
        "do_between_writes",
        "    #(1, 2) do: [:e | self.m := 1. STMT. self.m := self.m + 1].\n    self.n\n",
    ),
    (
        "do_consecutive",
        "    #(1, 2) do: [:e | STMT. STMT].\n    self.n\n",
    ),
    (
        "nested_do",
        "    #(1, 2) do: [:e | #(3, 4) do: [:f | STMT]].\n    self.n\n",
    ),
    (
        "collect_last",
        "    #(1, 2) collect: [:e | STMT].\n    self.n\n",
    ),
    (
        "collect_then_value",
        "    #(1, 2) collect: [:e | STMT. e].\n    self.n\n",
    ),
    (
        "inject_last",
        "    #(1, 2) inject: 0 into: [:a :e | STMT].\n    self.n\n",
    ),
    (
        "inject_then_acc",
        "    #(1, 2) inject: 0 into: [:a :e | STMT. a].\n    self.n\n",
    ),
    (
        "select_last",
        "    #(1, 2) select: [:e | STMT].\n    self.n\n",
    ),
    (
        "reject_last",
        "    #(1, 2) reject: [:e | STMT].\n    self.n\n",
    ),
    (
        "detect_last",
        "    #(1, 2) detect: [:e | STMT].\n    self.n\n",
    ),
    (
        "any_satisfy_last",
        "    #(1, 2) anySatisfy: [:e | STMT].\n    self.n\n",
    ),
    (
        "all_satisfy_last",
        "    #(1, 2) allSatisfy: [:e | STMT].\n    self.n\n",
    ),
    (
        "count_last",
        "    #(1, 2) count: [:e | STMT].\n    self.n\n",
    ),
    (
        "take_while_last",
        "    #(1, 2) takeWhile: [:e | STMT].\n    self.n\n",
    ),
    (
        "drop_while_last",
        "    #(1, 2) dropWhile: [:e | STMT].\n    self.n\n",
    ),
    (
        "partition_last",
        "    #(1, 2) partition: [:e | STMT].\n    self.n\n",
    ),
    (
        "group_by_last",
        "    #(1, 2) groupBy: [:e | STMT].\n    self.n\n",
    ),
    (
        "sort_last",
        "    #(1, 2) sort: [:a :b | STMT].\n    self.n\n",
    ),
    (
        "do_conditional_arm",
        "    #(1, 2) do: [:e | e > 1 ifTrue: [STMT] ifFalse: [self bump]].\n    self.n\n",
    ),
    (
        "while_body",
        "    i := 0.\n    [i < 2] whileTrue: [STMT. i := i + 1].\n    self.n\n",
    ),
    ("to_do_body", "    1 to: 2 do: [:i | STMT].\n    self.n\n"),
    (
        "times_repeat_body",
        "    2 timesRepeat: [STMT].\n    self.n\n",
    ),
];

#[test]
fn loop_body_self_send_operands_thread_state() {
    let passes = |src: &str| {
        catch_unwind(AssertUnwindSafe(|| {
            assert_compiles_through_erlc("test", &codegen(src));
        }))
        .is_ok()
    };
    let mut failures: Vec<String> = Vec::new();
    for (body_name, body) in BODIES {
        let cases: Vec<(&str, String)> = SELF_SENDS
            .iter()
            .enumerate()
            .map(|(i, (send_name, send))| {
                (
                    *send_name,
                    format!("  probe{i} =>\n{}", body.replace("STMT", send)),
                )
            })
            .collect();
        // One `erlc` run per body over every send at once...
        let methods: Vec<&str> = cases.iter().map(|(_, m)| m.as_str()).collect();
        if passes(&format!("{HEADER}\n{}", methods.join("\n"))) {
            continue;
        }
        // ...and one per case only when that fails, so a failure names its case.
        for (send_name, method) in &cases {
            let single = format!("{HEADER}\n{method}");
            if !passes(&single) {
                failures.push(format!("{body_name} / {send_name}:\n{single}"));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "{} loop-body self-send case(s) fail verified codegen:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// BT-3814: positions that do not lower a self-send *statement* through
/// `sequenced_self_dispatch`: `capture_argument_list_doc`'s
/// `StateEffectEscapesExpression` guard sits under six dispatch callers
/// (`generate_self_cast_send`, `generate_discarding_self_dispatch`,
/// `generate_self_dispatch_call_doc_for`, `generate_sealed_self_dispatch`,
/// `generate_direct_sealed_call`, `generate_super_send`); each must be
/// unreachable from valid source (`take: (self bump)` here either threads or
/// is rejected by the language, never trips the guard). The `on:do:`/
/// `ensure:` rows are the arm that BT-3814 routes through
/// `sequenced_self_dispatch`.
const OTHER_POSITIONS: &[(&str, &str)] = &[
    (
        "cascade_message",
        "    (OrderedCollection new) add: 1; add: (self take: (self bump)); yourself.\n    self.n\n",
    ),
    (
        "match_arm",
        "    1 match: [1 -> (self take: (self bump)); _ -> 0].\n    self.n\n",
    ),
    (
        "match_scrutinee",
        "    (self take: (self bump)) match: [1 -> #one; _ -> #other].\n    self.n\n",
    ),
    (
        "sort_comparator",
        "    #(1, 2) sort: [:a :b | (self take: (self bump)) > 0].\n    self.n\n",
    ),
    (
        "interpolation_segment",
        "    \"v={self take: (self bump)}\".\n    self.n\n",
    ),
    (
        "interpolation_after_effectful_segment",
        "    \"v={self bump} w={self take: (self bump)}\".\n    self.n\n",
    ),
    (
        "cast_statement",
        "    self take: (self bump)!\n    self.n\n",
    ),
    (
        "cast_in_do",
        "    #(1, 2) do: [:e | self take: (self bump)!].\n    self.n\n",
    ),
    (
        "on_do_protected_last",
        "    [self take: (self bump)] on: Error do: [:e | 0].\n    self.n\n",
    ),
    (
        "on_do_protected_two_operands",
        "    [self take: (self bump) with: (self bump)] on: Error do: [:e | 0].\n    self.n\n",
    ),
    (
        "on_do_protected_then_read",
        "    [self take: (self bump). self.n] on: Error do: [:e | 0].\n    self.n\n",
    ),
    (
        "on_do_handler",
        "    [1] on: Error do: [:e | self take: (self bump)].\n    self.n\n",
    ),
    (
        "ensure_body",
        "    [self take: (self bump)] ensure: [self.m := 1].\n    self.n\n",
    ),
    (
        "ensure_cleanup",
        "    [1] ensure: [self take: (self bump)].\n    self.n\n",
    ),
    (
        "ensure_cleanup_two_operands",
        "    [1] ensure: [self take: (self bump) with: (self bump)].\n    self.n\n",
    ),
    ("super_method_body", "    super take: (self bump)\n"),
    (
        "super_non_last_statement",
        "    super take: (self bump).\n    self.n\n",
    ),
    (
        "super_two_operands",
        "    super take: (self bump) with: (self bump).\n    self.n\n",
    ),
    (
        "super_in_conditional_arm",
        "    true ifTrue: [super take: (self bump)].\n    self.n\n",
    ),
    (
        "super_in_on_do",
        "    [super take: (self bump)] on: Error do: [:e | 0].\n    self.n\n",
    ),
    (
        "super_in_do",
        "    #(1, 2) do: [:e | super take: (self bump)].\n    self.n\n",
    ),
];

#[test]
fn self_send_operands_thread_state_in_every_other_position() {
    let passes = |src: &str| {
        catch_unwind(AssertUnwindSafe(|| {
            assert_compiles_through_erlc("test", &codegen(src));
        }))
        .is_ok()
    };
    let mut failures: Vec<String> = Vec::new();
    for (name, body) in OTHER_POSITIONS {
        let src = format!("{HEADER}\n  probe =>\n{body}");
        if !passes(&src) {
            failures.push(format!("{name}:\n{src}"));
        }
    }
    assert!(
        failures.is_empty(),
        "{} self-send operand position(s) fail verified codegen:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}
