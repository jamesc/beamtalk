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
//! `tests/class_var_agreement.rs`). Every closed `fun` lowering now goes through
//! `CoreErlangGenerator::with_closed_fun_scope`.

use super::{assert_compiles_through_erlc, codegen};

fn check(src: &str) {
    // `codegen` runs the debug `ThreadedIr` verifier, which panics on the leak.
    let code = codegen(src);
    // `codegen` names the module `test`.
    assert_compiles_through_erlc("test", &code);
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
