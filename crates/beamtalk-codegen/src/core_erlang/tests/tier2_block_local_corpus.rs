// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3939: the Tier 2 block-body gate is semantic (any construct reachable
//! from a statement, without crossing a closure, that rebinds a local the body
//! bound), not the statement shapes of the repro in
//! `tier2_block_local_inner_loop.rs`. Generated (host x binding x position x
//! construct) cases, with a host that does not depend on the self-send
//! promotion.

use super::tier2_block_local_inner_loop::compile;

/// Constructs that rebind the body local `n`.
const CONSTRUCTS: &[&str] = &[
    "xs do: [:y | n := n + y]",
    "xs inject: 0 into: [:a :y | n := n + y. a + y]",
    "xs collect: [:y | n := n + y]",
    "1 to: 2 do: [:i | n := n + i]",
    "[n < 2] whileTrue: [n := n + 1]",
    "2 timesRepeat: [n := n + 1]",
];

/// Where the construct sits in a statement of the block body (`n` read after).
const POSITIONS: &[&str] = &[
    "{c}. n",
    "r := {c}. n",
    "r := ({c}) + 1. n",
    "r := ({c}) printString. n",
    "self note: ({c}). n",
    "flag ifTrue: [{c}] ifFalse: [0]. n",
    "[{c}] value. n",
    "[{c}] on: Error do: [:e | 0]. n",
];

/// How the body binds `n` before the construct.
const BINDINGS: &[&str] = &["n := 0. ", "{n, m} := #(0, 0). "];

/// Block hosts: a block an actor self-send promotes, and a stored block that
/// writes a captured local (Tier 2 without any promotion).
fn hosts(body: &str) -> [String; 2] {
    [
        format!(
            "Actor subclass: G
  state: groups = #(#(1, 2), #(3))
  state: hits = 0
  sealed fwd: block => self.groups collect: block
  promote => self fwd: [:xs | self.hits := self.hits + 1. xs]
  note: x => x
  note: x with: z => x
  go: flag => self fwd: [:xs | {body}]
"
        ),
        format!(
            "Actor subclass: G
  state: hits = 0
  note: x => x
  note: x with: z => x
  go: flag =>
    c := 0
    blk := [:xs | c := c + 1. {body}]
    blk value: #(1, 2)
"
        ),
    ]
}

// PIN-BUG BT-3947: every program below answers the pre-construct `n` (and
// state) if it compiles; the correct answer is parked with BT-3947.
#[test]
fn every_construct_at_every_position_that_rebinds_a_body_local_is_an_error() {
    let mut checked = 0;
    for binding in BINDINGS {
        for position in POSITIONS {
            for construct in CONSTRUCTS {
                let body = format!("{binding}{}", position.replace("{c}", construct));
                for src in hosts(&body) {
                    match compile(&src) {
                        Ok(_) => panic!("answers a stale value silently:\n{src}"),
                        Err(e) => {
                            assert!(e.contains("BT-3939") && e.contains("`n`"), "{e}\n{src}");
                            checked += 1;
                        }
                    }
                }
            }
        }
    }
    assert_eq!(
        checked,
        BINDINGS.len() * POSITIONS.len() * CONSTRUCTS.len() * 2
    );
}

/// The binding is part of the same statement as the construct, before it runs:
/// `n` is not in scope when the statement is lowered, so the scope alone sees
/// neither the local nor the construct's write to it.
const SAME_STATEMENT_POSITIONS: &[&str] = &[
    "r := (n := 0) + ({c}). n",
    "r := #[(n := 0), ({c})]. n",
    "self note: (n := 0) with: ({c}). n",
    "self note: (n := 0); note: ({c}). n",
    "r := (n := 0) max: ({c}). n",
];

// PIN-BUG BT-3947: answers the pre-construct `n` if it compiles.
#[test]
fn a_local_bound_earlier_in_the_same_statement_is_an_error() {
    let mut checked = 0;
    for position in SAME_STATEMENT_POSITIONS {
        for construct in CONSTRUCTS {
            let body = position.replace("{c}", construct);
            for src in hosts(&body) {
                match compile(&src) {
                    Ok(_) => panic!("answers a stale value silently:\n{src}"),
                    Err(e) => {
                        assert!(e.contains("BT-3939") && e.contains("`n`"), "{e}\n{src}");
                        checked += 1;
                    }
                }
            }
        }
    }
    assert_eq!(
        checked,
        SAME_STATEMENT_POSITIONS.len() * CONSTRUCTS.len() * 2
    );
}

/// The binding is made inside an INLINED block (a conditional arm, an
/// evaluated `[...] value`): that block's own frame lowers the loop, so the
/// Tier 2 gate (a local the Tier 2 body itself binds) does not apply. Checked
/// on a BEAM by `stdlib/test/bt3939tier2inlined_test.bt` (all answer the
/// post-loop value); here, over the generated constructs, that they compile.
const INLINED_BINDING_POSITIONS: &[&str] = &[
    "flag ifTrue: [n := 0. {c}. n] ifFalse: [0]",
    "r := [n := 0. {c}. n] value. r",
    "flag ifTrue: [flag ifTrue: [n := 0. {c}. n] ifFalse: [1]] ifFalse: [0]",
];

#[test]
fn a_local_bound_inside_an_inlined_block_is_that_blocks_own() {
    for position in INLINED_BINDING_POSITIONS {
        for construct in CONSTRUCTS {
            let body = position.replace("{c}", construct);
            for src in hosts(&body) {
                compile(&src).unwrap_or_else(|e| panic!("{e}\n{src}"));
            }
        }
    }
}

// PIN-BUG BT-3947: the one source path to a Tier 2 body that survives BT-3922
// (found empirically): a block passed to an Erlang FFI call that writes a
// captured local goes through `generate_erlang_interop_wrapper` and answered
// `#(0)` for this program, silently, with the validator on.
#[test]
fn an_erlang_ffi_block_rebinding_a_body_local_in_a_loop_is_an_error() {
    let src = "Actor subclass: G
  state: hits = 0
  ffi =>
    t := 0
    ys := #(1, 2)
    (Erlang lists)
      map: [:x |
        t := t + 1
        n := 0
        ys do: [:y | n := n + y]
        n
      ]
      with: #(1)
";
    let e = compile(src).expect_err("must not answer #(0) silently");
    assert!(e.contains("BT-3939") && e.contains("`n`"), "{e}");
}

#[test]
fn a_construct_that_rebinds_a_captured_local_is_not_this_error() {
    // `c` is captured-mutated by the block (seeded into `StateAcc` by the
    // caller); the loop writes it, not a local the body bound.
    let src = "Actor subclass: G
  state: hits = 0
  go =>
    c := 0
    blk := [:xs | xs do: [:y | c := c + y]. c]
    blk value: #(1, 2)
";
    compile(src).expect("a captured-mutated local rebound in a loop compiles");
}
