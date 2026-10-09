// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3737: a closed `fun` body must not leak its `State` version into the enclosing scope.
//!
//! A conditional with a block-local write threads a `{Value, StateAcc}` pair even inside a
//! plain (Tier 1) block, and binds `StateAcc{N}` to unpack it. The inlined `inject:into:`
//! fold fun did not restore the generator's state version on exit, so inside a `StateAcc`
//! loop the enclosing loop's next statement read (and rebound from) a `StateAcc1` that was
//! only bound inside the fold fun: the `ThreadedIr` verifier's `UnboundVersion` /
//! `NonLinearVersion` pair, found by the class-variable agreement corpus (seeds
//! 4288439136652914866, 12525934088575929805 and 15420763691539516991 in
//! `tests/class_var_agreement.rs`).
//!
//! BT-3771 audited every other closed-`fun` lowering for the same shape. A closed `fun`
//! restores the enclosing `state_version` in one of two shared places, never per call site:
//! `CoreErlangGenerator::with_closed_fun_scope` (a Tier 1 block, the inlined `inject:into:`
//! fold fun), or the `BranchContextGuard` of `with_branch_context` (a Tier 2 block; every
//! fold lambda through `generate_foldl_loop_body`, now including the `sort:` comparator;
//! every loop `letrec` fun through `generate_letrec_body_ir`; every loop condition fun).
//! A `ClassBuilder` class-method fun is a method top frame: it resets the version on entry
//! and the builder context restores the enclosing one on exit. The audit found one site
//! outside both: the `sort:` comparator lowered its body against the enclosing version
//! (the two `sort_comparator_*` tests). [`closed_fun_corpus_keeps_enclosing_state_version`]
//! proves the invariant over the cross product of every closed-`fun` emitter, a
//! version-producing body and every enclosing scope kind that threads `State`/`StateAcc`.

use super::{assert_compiles_through_erlc, codegen};
use std::panic::{AssertUnwindSafe, catch_unwind};

fn check(src: &str) {
    // `codegen` runs the debug `ThreadedIr` verifier, which panics on the leak.
    let code = codegen(src);
    // `codegen` names the module `test`.
    assert_compiles_through_erlc("test", &code);
}

/// BT-3771: the `sort:` comparator fun lowered its body against the enclosing scope's
/// state version instead of its own `StateAcc` entry state. After a field write the
/// enclosing scope is at `State1`, so a nested loop in the comparator rendered
/// `StateAcc1` references nothing bound (`erlc` reported `StateAcc1` as an unbound variable).
#[test]
fn sort_comparator_with_nested_loop_after_field_write() {
    check(
        "Actor subclass: SortProbe
  state: n = 0

  h0: x -> Integer => 3

  probe -> Integer =>
    self.n := self.n + 1.
    #(2, 1) sort: [:e :f | j := 0. [j < 2] whileTrue: [self h0: j. j := j + 1]. e < f].
    self.n := self.n + 1.
    self.n
",
    );
}

/// BT-3771: the same inherited version made a comparator's field write read the
/// enclosing method's `State` (captured when the fun was built) instead of the `StateAcc`
/// it reads from the process dictionary, so every comparison started from the pre-sort
/// state and all but the last comparison's writes were lost.
#[test]
fn sort_comparator_field_write_reads_its_own_stateacc() {
    let code = codegen(
        "Actor subclass: SortCountProbe
  state: n = 0

  probe -> Integer =>
    #(3, 1, 2) sort: [:e :f | self.n := self.n + 1. e < f].
    self.n
",
    );
    let comparator = code
        .split("fun (E, F) -> ")
        .nth(1)
        .and_then(|rest| rest.split("'lists':'sort'").next())
        .expect("the comparator fun");
    assert!(
        comparator.contains("call 'maps':'get'('n', StateAcc)"),
        "the comparator must read the state it got from the process dictionary:\n{comparator}"
    );
    assert!(
        !comparator.contains("'n', State)"),
        "the comparator must not read the enclosing method's State:\n{comparator}"
    );
    assert_compiles_through_erlc("test", &code);
}

/// BT-3771: the comparator's statement shapes, now lowered by the shared fold-body
/// dispatch, in every context that reaches the mutating `sort:` path.
#[test]
fn sort_comparator_statement_shapes_compile() {
    for src in [
        // Last statement a local assignment, a field assignment, a self-send.
        "Actor subclass: S1
  state: n = 0

  h0: x -> Boolean => true

  probe =>
    c := 0.
    a := #(3, 1, 2) sort: [:e :f | c := c + 1. c < 9].
    b := #(3, 1, 2) sort: [:e :f | self.n := self.n + 1. e < f].
    d := #(3, 1, 2) sort: [:e :f | self.n := self.n + 1. self h0: e].
    c
",
        // A conditional with a threaded write, and a non-local return.
        "Actor subclass: S2
  state: n = 0

  probe =>
    c := 0.
    #(3, 1, 2) sort: [:e :f | e > 2 ifTrue: [c := c + 1]. e < f].
    #(3, 1, 2) sort: [:e :f | e > 5 ifTrue: [^c]. self.n := self.n + 1. e < f].
    c
",
        // A value-type method and a class method.
        "Value subclass: S3
  state: x = 0

  probe =>
    c := 0.
    #(3, 1, 2) sort: [:e :f | c := c + 1. e < f].
    c

  class cprobe =>
    c := 0.
    #(3, 1, 2) sort: [:e :f | c := c + 1. e < f].
    c
",
    ] {
        check(src);
    }
}

#[test]
fn inject_fold_with_conditional_local_write_inside_stateacc_loop() {
    // The `while` is `StateAcc` shaped (the loop-body self-send is a state effect to the
    // plan); the fold fun's `ifTrue:ifFalse:` writes the block-local `v`.
    check(
        "Object subclass: FoldProbe
  classState: n = 0

  class h0: x -> Integer => 3

  class probe -> Integer =>
    i := 0. [i < 2] whileTrue: [#(1, 4) inject: 0 into: [:a :e | 4 < 5 ifTrue: [self h0: a. 2] ifFalse: [v := 1]. i]. i := i + 1]
    self.n
",
    );
}

/// The enclosing scope a closed `fun` is lowered in. Each threads a `State`/`StateAcc`
/// version and reads (and rebinds from) it in the statement after `EXPR`, so a version
/// leaked out of `EXPR`'s `fun` is an unbound or non-linear reference there.
struct Enclosing {
    name: &'static str,
    /// The class header (including the `h0:` helper the bodies send).
    header: &'static str,
    /// The method, with `EXPR` standing for the closed-`fun` expression.
    method: &'static str,
}

const ENCLOSING: &[Enclosing] = &[
    // BT-3737's shape: a class method's `StateAcc` while loop.
    Enclosing {
        name: "class_stateacc_loop",
        header: "Object subclass: ClosedFunProbe
  classState: n = 0

  class h0: x -> Integer => 3
",
        method: "  class probe{i} -> Integer =>
    i := 0. [i < 2] whileTrue: [self h0: i. EXPR. i := i + 1]
    self.n
",
    },
    // An actor method whose `State{N}` chain runs through `EXPR`.
    Enclosing {
        name: "actor_field_chain",
        header: "Actor subclass: ClosedFunProbe
  state: n = 0

  h0: x -> Integer => 3
",
        method: "  probe{i} -> Integer =>
    self.n := self.n + 1.
    EXPR.
    self.n := self.n + 1.
    self.n
",
    },
    // An actor `StateAcc` while loop with a field write either side of `EXPR`.
    Enclosing {
        name: "actor_stateacc_loop",
        header: "Actor subclass: ClosedFunProbe
  state: n = 0

  h0: x -> Integer => 3
",
        method: "  probe{i} -> Integer =>
    i := 0. [i < 2] whileTrue: [self.n := self.n + 1. EXPR. self.n := self.n + 1. i := i + 1]
    self.n
",
    },
];

/// A block body that binds a `State`/`StateAcc` version inside the `fun` it is lowered
/// in (`BODY`, ending in a statement separator; `e` is the block's element parameter).
const BODIES: &[(&str, &str)] = &[
    // BT-3737's body: a conditional whose arm writes a block-local threads a pair.
    (
        "cond_local_write",
        "4 < 5 ifTrue: [self h0: e. 2] ifFalse: [v := 1].",
    ),
    // A nested `StateAcc` loop inside the `fun`.
    (
        "nested_loop",
        "j := 0. [j < 2] whileTrue: [self h0: j. j := j + 1].",
    ),
    // A nested closed `fun` inside the `fun`.
    (
        "nested_fold",
        "#(1) inject: 0 into: [:a :x | 4 < 5 ifTrue: [self h0: x. 2] ifFalse: [w := 1]. a].",
    ),
];

/// Every closed-`fun` emitter, as a source template over the body `BODY`.
const EMITTERS: &[(&str, &str)] = &[
    ("list_do", "#(1, 2) do: [:e | BODY e]"),
    ("collect", "#(1, 2) collect: [:e | BODY e]"),
    ("select", "#(1, 2) select: [:e | BODY true]"),
    ("reject", "#(1, 2) reject: [:e | BODY false]"),
    ("detect", "#(1, 2) detect: [:e | BODY true]"),
    (
        "detect_if_none",
        "#(1, 2) detect: [:e | BODY true] ifNone: [0]",
    ),
    ("any_satisfy", "#(1, 2) anySatisfy: [:e | BODY true]"),
    ("all_satisfy", "#(1, 2) allSatisfy: [:e | BODY true]"),
    ("count", "#(1, 2) count: [:e | BODY true]"),
    ("inject", "#(1, 2) inject: 0 into: [:acc :e | BODY acc]"),
    ("flat_map", "#(1, 2) flatMap: [:e | BODY #(1)]"),
    ("take_while", "#(1, 2) takeWhile: [:e | BODY true]"),
    ("drop_while", "#(1, 2) dropWhile: [:e | BODY false]"),
    ("group_by", "#(1, 2) groupBy: [:e | BODY e]"),
    ("partition", "#(1, 2) partition: [:e | BODY true]"),
    ("sort", "#(2, 1) sort: [:e :f | BODY e < f]"),
    (
        "each_with_index",
        "#(1, 2) eachWithIndex: [:e :ix | BODY e]",
    ),
    (
        "do_separated_by",
        "#(1, 2) do: [:e | BODY e] separatedBy: [nil]",
    ),
    ("dict_do", "#{#a => 1} do: [:e | BODY e]"),
    (
        "dict_keys_and_values_do",
        "#{#a => 1} keysAndValuesDo: [:k :e | BODY e]",
    ),
    ("tier1_block", "blk := [:e | BODY e]. blk value: 1"),
    (
        "tier2_block",
        "c := 0. blk := [:e | c := c + e. BODY e]. blk value: 1",
    ),
    ("to_do", "1 to: 2 do: [:e | BODY e]"),
    ("times_repeat", "e := 1. 2 timesRepeat: [BODY e]"),
    (
        "while_condition",
        "e := 1. [BODY e < 1] whileTrue: [e := e + 1]",
    ),
    // A `ClassBuilder` class-method fun (a method top frame of its own), followed by a
    // sibling map entry lowered after it.
    (
        "class_builder_fun",
        "Object classBuilder name: #ClosedFunBuilt; superclass: Object; classMethods: #{#m => [:self | e := 1. BODY e], #k => [:self | e := 2. BODY e]}; register",
    ),
];

/// BT-3771: no closed-`fun` lowering leaks a `State`/`StateAcc` version into the scope
/// that encloses it. Generates every emitter x body x enclosing-scope program; each must
/// pass the debug `ThreadedIr` verifier (run by `codegen`) and compile through `erlc`.
#[test]
fn closed_fun_corpus_keeps_enclosing_state_version() {
    let passes = |src: &str| {
        catch_unwind(AssertUnwindSafe(|| {
            assert_compiles_through_erlc("test", &codegen(src));
        }))
        .is_ok()
    };
    let mut failures: Vec<String> = Vec::new();
    for enclosing in ENCLOSING {
        for (body_name, body) in BODIES {
            let cases: Vec<(&str, String)> = EMITTERS
                .iter()
                .enumerate()
                .map(|(i, (emitter_name, emitter))| {
                    let method = enclosing
                        .method
                        .replace("{i}", &i.to_string())
                        .replace("EXPR", &emitter.replace("BODY", body));
                    (*emitter_name, method)
                })
                .collect();
            // One `erlc` run per enclosing scope and body, over every emitter at once...
            let methods: Vec<&str> = cases.iter().map(|(_, m)| m.as_str()).collect();
            if passes(&format!("{}\n{}", enclosing.header, methods.join("\n"))) {
                continue;
            }
            // ...and one per case only when that fails, so a failure names its case.
            for (emitter_name, method) in &cases {
                let single = format!("{}\n{method}", enclosing.header);
                if !passes(&single) {
                    failures.push(format!(
                        "{} / {body_name} / {emitter_name}:\n{single}",
                        enclosing.name
                    ));
                }
            }
        }
    }
    assert!(
        failures.is_empty(),
        "{} closed-fun case(s) fail verified codegen:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}
