// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3687: a statement-level `@expect` directive is a codegen no-op wherever a
//! statement sequence is lowered. `on:do:` bodies once lowered it as a plain
//! non-last statement (`let _ =  in`), which `erlc` rejects.

use super::{assert_compiles_through_erlc, codegen};

fn check(src: &str) {
    let code = codegen(src);
    assert!(
        !code.contains("let _ =  in"),
        "a directive must not lower to an empty expression. Got:\n{code}"
    );
    // `codegen` names the module `test`.
    assert_compiles_through_erlc("test", &code);
}

#[test]
fn expect_in_on_do_body_of_class_method() {
    check(
        "Object subclass: ExpectMin
  classState: n = 0

  class foo -> Integer => 0

  class withDirective -> Integer =>
    [
      @expect lint \"directive inside an on:do: body\"
      self foo
    ] on: Error do: [:e | nil]
    self.n

  class withoutDirective -> Integer =>
    [
      self foo
    ] on: Error do: [:e | nil]
    self.n
",
    );
}

#[test]
fn expect_as_last_and_middle_statement_in_on_do_body() {
    check(
        "Object subclass: ExpectLast
  classState: n = 0

  class foo -> Integer => 0

  class trailing -> Integer =>
    [
      self foo
      @expect lint \"trailing\"
    ] on: Error do: [:e | nil]
    self.n

  class middle -> Integer =>
    [
      self foo
      @expect lint \"middle\"
      self foo
    ] on: Error do: [:e | nil]
    self.n
",
    );
}

#[test]
fn expect_in_on_do_handler_and_ensure_arms() {
    check(
        "Object subclass: ExpectArms
  classState: n = 0

  class foo -> Integer => 0

  class inHandler -> Integer =>
    [self foo] on: Error do: [:e |
      @expect lint \"handler\"
      self foo
    ]
    self.n

  class inEnsure -> Integer =>
    [self foo] ensure: [
      @expect lint \"cleanup\"
      self foo
    ]
    self.n
",
    );
}

#[test]
fn expect_in_on_do_body_of_instance_methods() {
    check(
        "Actor subclass: ExpectActor
  state: n :: Integer = 0

  foo => 1

  run =>
    [
      @expect lint \"actor\"
      self foo
    ] on: Error do: [:e | nil]
    self.n
",
    );
    check(
        "Value subclass: ExpectValue
  state: n :: Integer = 0

  foo => 1

  run =>
    [
      @expect lint \"value\"
      self foo
    ] on: Error do: [:e | nil]
    self.n
",
    );
}

/// Wraps `body` (indented statements) in a class method of a class with
/// `classState:`, once with and once without the directive, so a failure in the
/// directive-free form is distinguishable from a directive-specific one.
fn class_method(body: &str) -> String {
    format!(
        "Object subclass: ExpectPositions
  classState: n = 0

  class foo -> Integer => 0

  class probe -> Integer =>
{body}
    self.n
"
    )
}

#[test]
fn expect_in_class_method_statement_positions() {
    let shapes = [
        "    @expect lint \"top\"\n    self foo\n    @expect lint \"again\"\n    self foo",
        "    1 to: 3 do: [:i |\n      @expect lint \"loop\"\n      self foo\n    ]",
        "    #(1, 2) do: [:x |\n      @expect lint \"list\"\n      self foo\n    ]",
        "    [false] whileTrue: [\n      @expect lint \"while\"\n      self foo\n    ]",
        "    1 > 0 ifTrue: [\n      @expect lint \"true arm\"\n      self foo\n    ] ifFalse: [\n      @expect lint \"false arm\"\n      self foo\n    ]",
        "    [\n      @expect lint \"block\"\n      self foo\n    ] value",
    ];
    for shape in shapes {
        check(&class_method(shape));
    }
}
