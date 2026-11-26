// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0153: a compiled `on:do:` is a plain `try`/`catch` with no snapshot or
//! restore around it. Each row here compiles a module that uses `on:do:` (and
//! an FFI call with a retrying argument) in every method context, checks the
//! catch arm filters through `matches_class`, and that the result compiles
//! through erlc.

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
fn compiled_on_do_is_a_plain_try_catch_in_every_method_context() {
    let mut compiled = Vec::new();
    for (label, src) in ROWS {
        let code = codegen(src);
        assert_has(
            label,
            &code,
            &["'beamtalk_exception_handler':'matches_class'("],
        );
        compiled.push((*label, code));
    }
    assert_all_compile_through_erlc(&compiled);
}

/// Class-side reflection in an `Actor` class method (`self fieldAt:`,
/// `fieldAt:put:`, `hasField:`, `clearField:`) is an ordinary send to the
/// class: it never takes the actor-instance fast path, which would read the
/// `State` a class method does not bind.
#[test]
fn class_side_reflection_in_an_actor_class_method_is_a_plain_send() {
    let src = "Actor subclass: P\n  state: n = 0\n\n  \
               class probeAt => self fieldAt: #missing\n  \
               class probePut => self fieldAt: #missing put: 1\n  \
               class probeHas => self hasField: #missing\n  \
               class probeClear => self clearField: #missing\n";
    let code = codegen(src);
    for selector in ["fieldAt:", "fieldAt:put:", "hasField:", "clearField:"] {
        // The receiver is the class method's `ClassSelf`, possibly rebound to
        // a sequencing temp first.
        let send = regex::Regex::new(&format!(
            r"'beamtalk_message_dispatch':'send'\((ClassSelf|_Receiver@\d+), '{}', \[",
            regex::escape(selector)
        ))
        .expect("valid regex");
        assert!(
            send.is_match(&code),
            "[{selector}] expected a dispatch send of '{selector}' to the class. Got:\n{code}"
        );
    }
    assert_lacks(
        "class-side reflection",
        &code,
        &["'beamtalk_primitive':'send'(State"],
    );
    assert_all_compile_through_erlc(&[("actor class-side reflection", code)]);
}
