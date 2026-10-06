// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3694: a `whileTrue:`/`whileFalse:` condition that self-sends inside a class method.
//!
//! The loop used to take the `StateAcc` shape (a self-send counts as a state effect in an
//! actor instance method) and seeded it with an actor `State` variable that does not exist in
//! a class method, which `erlc` rejected as an unbound variable. A class method threads no
//! family (ADR 0130 §3: a class-variable write is an in-place `put`, a self-send rebinds
//! nothing), so its loops are the plain `letrec` shape.

use super::{assert_compiles_through_erlc, codegen};

fn check(src: &str) {
    let code = codegen(src);
    assert!(
        !code.contains("apply 'while'/1 (State)"),
        "a class-method loop must not be seeded with an actor State variable. Got:\n{code}"
    );
    // `codegen` names the module `test`.
    assert_compiles_through_erlc("test", &code);
}

#[test]
fn while_true_condition_self_send_in_class_method() {
    check(
        "Object subclass: WhileProbe
  classState: n = 0

  class foo -> Integer => 0

  class probe -> Integer =>
    [self foo > 0] whileTrue: [
      self foo
    ]
    self.n
",
    );
}

#[test]
fn while_false_condition_self_send_in_class_method() {
    check(
        "Object subclass: WhileFalseProbe
  classState: n = 0

  class foo -> Integer => 0

  class probe -> Integer =>
    [self foo > 0] whileFalse: [
      self foo
    ]
    self.n
",
    );
}

#[test]
fn while_condition_reads_class_var_and_body_writes_it() {
    // The BT-3704 matrix row: the condition reads through a self-send, the body writes.
    check(
        "Object subclass: WhileWrites
  classState: n = 0

  class bump -> Integer => self.n := self.n + 1

  class n -> Integer => self.n

  class probe -> Integer =>
    [self n < 3] whileTrue: [self bump]
    self.n
",
    );
}

#[test]
fn zz_probe() {
    let code = codegen(
        "Object subclass: Zz
  classState: n = 0

  class bump -> Integer => self.n := self.n + 1

  class row -> Integer =>
    [
      [
        [
          self bump
          1 / 0
        ] on: TypeError do: [:e | nil]
      ] ensure: [self bump]
    ] on: Error do: [:e | nil]
    self.n
",
    );
    std::fs::write("/tmp/claude-0/-home-user-beamtalk/153ecdb0-a002-556a-8d11-02c4013fbc84/scratchpad/zz.core", code).unwrap();
}

#[test]
fn pure_condition_loops_thread_nothing() {
    let code = codegen(
        "Object subclass: WhilePure
  classState: n = 0

  class foo -> Integer => 0

  class probe -> Integer =>
    [self foo > 0] whileTrue: [self foo]
    self.n
",
    );
    assert!(
        !code.contains("StateAcc"),
        "a class-method loop with no mutated local threads nothing. Got:\n{code}"
    );
}

#[test]
fn while_condition_with_local_mutation_in_class_method() {
    check(
        "Object subclass: WhileLocal
  classState: n = 0

  class bump -> Integer => self.n := self.n + 1

  class n -> Integer => self.n

  class probe -> Integer =>
    i := 0
    [self n < 3] whileTrue: [
      self bump
      i := i + 1
    ]
    i
",
    );
}
