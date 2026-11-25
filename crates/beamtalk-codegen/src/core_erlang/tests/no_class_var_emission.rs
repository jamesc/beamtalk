// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0153 Phase 2: nothing the compiler emits calls `beamtalk_class_vars`.
//! Every compiled `on:do:` used to take a class-variable snapshot before its
//! `try` and restore it in the catch arm; each row here compiles a module that
//! uses `on:do:` (and an FFI call with a retrying argument, which used to read
//! the same snapshot) in every method context and checks the call is gone.

use super::*;

const ROWS: &[(&str, &str)] = &[
    (
        "actor instance method",
        "Actor subclass: P\n  state: n = 0\n\n  run: x =>\n    [self.n := x. x foo] on: Error do: [:e | self.n]\n",
    ),
    (
        "actor instance method in a loop",
        "Actor subclass: P\n  state: n = 0\n\n  run: x =>\n    1 to: 3 do: [:i | self.n := self.n + i. [x foo] on: Error do: [:e | nil]].\n    self.n\n",
    ),
    (
        "value-type method",
        "Object subclass: P\n  field: n = 0\n\n  run: x =>\n    [x foo] on: Error do: [:e | self.n]\n",
    ),
    (
        "class method",
        "Object subclass: P\n\n  class run: x =>\n    [x foo] on: Error do: [:e | 0]\n",
    ),
    (
        "nested on:do: with a non-local return",
        "Object subclass: P\n\n  run: x =>\n    [[x foo] on: ZeroDivide do: [:e | ^1]] on: Error do: [:e | 2]\n",
    ),
    (
        "direct FFI call with a String argument",
        "Object subclass: P\n\n  class run: s :: String =>\n    [Erlang erlang phash2: s] on: Error do: [:e | 0]\n",
    ),
];

#[test]
fn no_compiled_on_do_calls_beamtalk_class_vars() {
    let mut compiled = Vec::new();
    for (label, src) in ROWS {
        let code = codegen(src);
        assert_has(
            label,
            &code,
            &["'beamtalk_exception_handler':'matches_class'("],
        );
        assert_lacks(label, &code, &["beamtalk_class_vars", "'$bt_class_vars"]);
        compiled.push((*label, code));
    }
    assert_all_compile_through_erlc(&compiled);
}
