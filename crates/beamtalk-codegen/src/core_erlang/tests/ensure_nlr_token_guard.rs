// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3912 (BT-3947): the `ensure:` catch for a `^` thrown from its protected block
//! (`push_ensure_nlr_cleanup`) takes only a throw that carries THIS activation's NLR
//! token. The wrapper's id is unique per module only, and a throw from another class's
//! method or from an outer activation of a recursive method (each activation has its own
//! token) would otherwise run this cleanup over a foreign construct's locals.
//!
//! No source program reaches the foreign shapes (a `^` in a closure, or a block value run
//! inside a protected block, is a compile error: BT-3993), so the guard is pinned over the
//! generated code of the shapes that do compile: every wrapper-catching arm carries the
//! token comparison.

use super::codegen;

const HOSTS: &[(&str, &str)] = &[
    (
        "class",
        "Object subclass: G
  class go =>
    t := 0
    [t := 1. ^t] ensure: [t printString]
",
    ),
    (
        "value",
        "Value subclass: G
  go =>
    t := 0
    [t := 1. ^t] ensure: [t printString]
",
    ),
    (
        "actor",
        "Actor subclass: G
  state: n = 0
  go =>
    t := 0
    [t := 1. ^t] ensure: [t printString]
",
    ),
];

#[test]
fn the_ensure_nlr_arm_is_guarded_by_the_activations_token() {
    for (context, src) in HOSTS {
        let code = codegen(src);
        let mut arms = 0;
        for (at, _) in code.match_indices("{'$bt_ens', 0, ") {
            // Only the catching pattern has a guard right after its closing braces.
            let rest = &code[at..];
            let Some(guard_at) = rest.find("}}}> when ") else {
                continue;
            };
            if rest[..guard_at].contains('~') {
                continue;
            }
            arms += 1;
            let guard = &rest[guard_at + "}}}> when ".len()..];
            assert!(
                guard.starts_with("call 'erlang':'=:='(_EnsTok"),
                "{context}: the catch must compare the thrown token with the activation's: {}",
                &guard[..guard.len().min(80)]
            );
        }
        assert!(arms > 0, "{context}: no catching arm in:\n{code}");
    }
}
