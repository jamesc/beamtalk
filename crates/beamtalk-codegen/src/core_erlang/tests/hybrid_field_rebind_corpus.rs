// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3811: a hybrid-loop field rebind is lowered in the caller's frame.
//!
//! In a mixed-family hybrid loop a field written in the body is extracted to a direct fun
//! parameter, so `self.m := (self.n := i)` rebinds two parameters instead of stepping the
//! `State` map. `generate_field_assignment_open`'s hybrid branch lowered that rebind with a
//! hardcoded `FrameId::ROOT` and flattened its ahead prelude (the nested `self.n := i`) to
//! one opaque `Statement`, so `ThreadedIr::verify` could neither see the rebind nor tell
//! which frame it belonged to. It now lowers through `lower_hybrid_field_rebind`, the one
//! lowering `lower_field_assignment_bind` (Letrec bodies, conditional arms, stateful block
//! bodies) shares, with the caller's `current_frame()`.
//!
//! The invariant is `VerifyError::BindFrameMismatch` (every `Bind` targets the frame that
//! encloses it), reported by the full verifier walk and, for the `Document`-returning
//! consumers, by `rendered_field_rebind_doc` before the rebind is flattened to text.
//! [`hybrid_field_rebind_is_verified_in_every_loop_and_container`] proves it over loop shape
//! x enclosing container x write shape: each program must pass the debug `ThreadedIr`
//! verifier and compile through `erlc`, and the corpus itself must reach the hybrid rebind
//! (checked by the `Field` parameter it renders) in every container.

use super::{assert_compiles_through_erlc, codegen};
use std::panic::{AssertUnwindSafe, catch_unwind};

const HEADER: &str = "Actor subclass: HybridRebindProbe
  state: n = 0
  state: m = 0
  state: k = 0

  bump -> Integer =>
    self.n := self.n + 1
    self.n

  take: x -> Integer => x
  take: x with: y -> Integer => x
";

/// A loop whose body is `BODY` and whose index is `i`, ending in the method's value.
const LOOPS: &[(&str, &str)] = &[
    // BT-4121: the scaffold stays operator-free (`[true]`, `i := nil`) so the guard-free
    // write probes below keep hybrid mode. A guarded add/compare in the condition or the
    // loop epilogue would force the `StateAcc` fallback (same rule as the bracket).
    (
        "while_true",
        "    i := 0.\n    [true] whileTrue: [BODY. i := nil].\n    self.n\n",
    ),
    (
        "while_false",
        "    i := 0.\n    [true] whileFalse: [BODY. i := nil].\n    self.n\n",
    ),
    ("to_do", "    1 to: 3 do: [:i | BODY].\n    self.n\n"),
    ("do", "    #(1, 2) do: [:i | BODY].\n    self.n\n"),
    (
        "inject_into",
        "    #(1, 2) inject: 0 into: [:a :i | BODY. a].\n    self.n\n",
    ),
];

/// What encloses the write statement `STMT` inside the loop body.
const CONTAINERS: &[(&str, &str)] = &[
    ("bare", "STMT"),
    ("if_true_arm", "i > 0 ifTrue: [STMT]"),
    ("if_false_arm", "i > 5 ifFalse: [STMT]"),
    (
        "if_true_if_false_arms",
        "i > 1 ifTrue: [STMT] ifFalse: [self.k := i]",
    ),
    ("nested_if_arm", "i > 0 ifTrue: [i < 9 ifTrue: [STMT]]"),
    (
        "arm_then_state_read",
        "i > 0 ifTrue: [STMT. self.k := self.n + self.m]",
    ),
    ("block_value_body", "[STMT] value"),
    ("block_value_arg", "[:x | STMT. x] value: i"),
    ("nested_do", "#(1) do: [:e | STMT]"),
    ("on_do_protected", "[STMT] on: Error do: [:e | 0]"),
    ("ensure_body", "[STMT] ensure: [self.k := 1]"),
];

/// The write: a hybrid field rebind with a nested field write, self-send or read around it.
const WRITES: &[(&str, &str)] = &[
    ("nested_rhs", "self.m := (self.n := i)"),
    ("binary_operand_rhs", "self.m := 1 + (self.n := i)"),
    ("triple_nested_rhs", "self.k := (self.m := (self.n := i))"),
    ("send_arg_in_rhs", "self.m := (self take: (self.n := i))"),
    ("self_send_in_nested_rhs", "self.m := (self.n := self bump)"),
    (
        "nested_rhs_then_read",
        "self.m := (self.n := i). self.k := self.n + self.m",
    ),
];

/// The `loop / container` cases that must reach the hybrid field rebind (see
/// [`hybrid_field_rebind_is_verified_in_every_loop_and_container`]).
const REACHES_THE_REBIND: &[&str] = &[
    "while_true / bare",
    "while_false / bare",
    "while_true / block_value_body",
];

/// One family per loop of [`LOOPS`] (`per_family_tests!`, BT-3888).
const LOOP_FAMILIES: &[std::ops::Range<usize>] = &[0..1, 1..2, 2..3, 3..4, 4..5];

/// The hybrid field rebind proof over one family of [`LOOPS`] x every
/// container x every write: the whole proof is the per-loop tests below.
fn hybrid_field_rebind_is_verified_in_every_loop_and_container(loops: &[(&str, &str)]) {
    let mut failures: Vec<String> = Vec::new();
    let mut not_reached: Vec<String> = Vec::new();
    for (loop_name, lp) in loops {
        for (container_name, container) in CONTAINERS {
            let methods: Vec<(&str, String)> = WRITES
                .iter()
                .enumerate()
                .map(|(i, (write_name, write))| {
                    let stmt = container.replace("STMT", write);
                    (
                        *write_name,
                        format!("  probe{i} =>\n{}", lp.replace("BODY", &stmt)),
                    )
                })
                .collect();
            // One program per (loop, container) over every write at once...
            let all: Vec<&str> = methods.iter().map(|(_, m)| m.as_str()).collect();
            let src = format!("{HEADER}\n{}", all.join("\n"));
            let compiles = |src: &str| {
                catch_unwind(AssertUnwindSafe(|| {
                    let code = codegen(src);
                    assert_compiles_through_erlc("test", &code);
                    code
                }))
            };
            match compiles(&src) {
                Ok(code) => {
                    if !code.contains("NField") {
                        not_reached.push(format!("{loop_name} / {container_name}"));
                    }
                }
                // ...and one per write only when that fails, so a failure names its case.
                Err(_) => {
                    for (write_name, method) in &methods {
                        let single = format!("{HEADER}\n{method}");
                        if compiles(&single).is_err() {
                            failures.push(format!(
                                "{loop_name} / {container_name} / {write_name}:\n{single}"
                            ));
                        }
                    }
                }
            }
        }
    }
    assert!(
        failures.is_empty(),
        "{} hybrid-rebind case(s) fail verified codegen:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
    // The corpus is only a proof if it reaches the rebind. A mutation-carrying conditional or
    // handler forces the `StateAcc` fallback (mutually exclusive with hybrid mode), so only
    // the hybrid loop's own body statements and plain block bodies render the `Field`
    // parameter; pin that those do, so a change that stops reaching it fails here.
    //
    //
    // A guarded operator inside a loop body forces the `StateAcc` fallback (ADR 0137,
    // BT-4121), so the writes carrying one (`binary_operand_rhs`, `nested_rhs_then_read`)
    // now verify through the bracket path instead of hybrid mode. The pins above stay
    // green through the guard-free write probes, which still hybridize exactly as before.
    for expected in REACHES_THE_REBIND {
        let (loop_name, _) = expected.split_once(" / ").expect("`loop / container`");
        if !loops.iter().any(|(name, _)| *name == loop_name) {
            continue;
        }
        let missed = not_reached.iter().any(|name| name == expected);
        assert!(
            !missed,
            "{expected} no longer reaches the hybrid field rebind (not reached: {not_reached:?})"
        );
    }
}

beamtalk_core::per_family_tests! {
    list = LOOPS,
    families = LOOP_FAMILIES,
    check = hybrid_field_rebind_is_verified_in_every_loop_and_container,
    partition = the_hybrid_field_rebind_tests_cover_every_loop,
    hybrid_field_rebind_is_verified_in_every_while_true_container => 0,
    hybrid_field_rebind_is_verified_in_every_while_false_container => 1,
    hybrid_field_rebind_is_verified_in_every_to_do_container => 2,
    hybrid_field_rebind_is_verified_in_every_do_container => 3,
    hybrid_field_rebind_is_verified_in_every_inject_into_container => 4,
}

/// Every pin of [`REACHES_THE_REBIND`] names a loop of [`LOOPS`]: a pin for a
/// loop no family holds would never be checked.
#[test]
fn every_reaches_the_rebind_pin_names_a_corpus_loop() {
    for expected in REACHES_THE_REBIND {
        let (loop_name, _) = expected.split_once(" / ").expect("`loop / container`");
        assert!(
            LOOPS.iter().any(|(name, _)| *name == loop_name),
            "{expected} names no loop of LOOPS"
        );
    }
}
