// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3936 / ADR 0137: a self-send compiled inside a closure used to carry the
//! `=:=` guard that raised `stateful_block_dispatch` when the callee changed
//! `State` (a closure could not hand it back). Under the live-state model an
//! actor closure runs against the actor-state cell, so the callee's change is
//! kept and no guard exists anywhere in the generated code. This corpus
//! generates actor shapes (self-send kind x block host x open/sealed class x
//! callee) and checks, for each, that the module holds no guard.
//!
//! `super` sends (BT-3985) are checked the same way by
//! `no_super_send_in_a_closure_is_guarded`.
//!
//! The behavioural side (state-changing sends keep their change) is
//! `stdlib/test/closure_self_send_matrix_test.bt` and
//! `stdlib/test/actor_closure_matrix_test.bt`.

use super::compile;

const GUARD: &str = "'stateful_block_dispatch'";

/// Self-send kinds a closure can contain: (label, expression using `x`).
const KINDS: &[(&str, &str)] = &[
    ("unary", "self peek"),
    ("keyword", "self peek: x"),
    ("keyword2", "self peek: x with: 1"),
    // BT-3985: a binary selector sent to `self` is a self-dispatch too.
    ("binary", "self + x"),
];

/// Hosts that make the block a real closure: (label, template with `{B}`).
const CLOSURE_HOSTS: &[(&str, &str)] = &[
    ("user_hom", "self each: [:x | {B}]"),
    ("stored", "blk := [:x | {B}]. self each: blk"),
    ("nested", "self each: [:x | #(1) collect: [:y | {B}]]"),
    (
        "fun_in_branch",
        "self each: [:x | x > 0 ifTrue: [{B}] ifFalse: [0]]",
    ),
    ("two_sends", "self each: [:x | {B}. {B}]"),
    ("on_do", "self each: [:x | [{B}] on: Error do: [:e | 0]]"),
];

/// Hosts that are inlined into the method (no closure): the send threads State.
const INLINED_HOSTS: &[(&str, &str)] = &[
    ("do_loop", "self.items do: [:x | {B}]"),
    ("if_true", "self.items isEmpty ifFalse: [{B}]"),
    ("plain", "{B}"),
];

fn actor(header: &str, body: &str) -> String {
    format!(
        "{header} P
  state: items = #(1, 2)
  state: total = 0
  each: blk => self.items collect: blk
  peek => self.total
  peek: x => self.total + x
  peek: x with: y => self.total + x + y
  + x => self.total + x
  run => {body}
"
    )
}

fn guards(src: &str) -> usize {
    let out = compile(src).unwrap_or_else(|e| panic!("did not compile: {e}\n{src}"));
    out.matches(GUARD).count()
}

#[test]
fn no_self_send_in_a_closure_is_guarded() {
    let mut checked = 0;
    for header in ["Actor subclass:", "sealed Actor subclass:"] {
        // The class machinery itself mentions the atom; a send adds none.
        let baseline = guards(&actor(header, "1"));
        for (kind, expr) in KINDS {
            for (host, template) in CLOSURE_HOSTS.iter().chain(INLINED_HOSTS) {
                let body = template.replace("{B}", expr);
                assert_eq!(
                    guards(&actor(header, &body)),
                    baseline,
                    "{header} {kind} in {host}: an actor closure runs on the cell, no guard\n{body}"
                );
                checked += 1;
            }
        }
    }
    assert_eq!(checked, 2 * 4 * 9);
}

/// `super` send kinds: (label, expression using `x`).
const SUPER_KINDS: &[(&str, &str)] = &[
    ("unary", "super peek"),
    ("keyword", "super peek: x"),
    ("binary", "super + x"),
];

fn super_actor(header: &str, body: &str) -> String {
    format!(
        "Actor subclass: Base
  state: total = 0
  peek => self.total
  peek: x => self.total := self.total + x
  + x => self.total := self.total + x

{header} P
  state: items = #(1, 2)
  each: blk => self.items collect: blk
  run => {body}
"
    )
}

#[test]
fn no_super_send_in_a_closure_is_guarded() {
    // A `super` send in a closure runs the superclass method against the cell
    // (`beamtalk_actor_state:super_send/5`), so its change is kept: no guard.
    let mut checked = 0;
    for header in ["Base subclass:", "sealed Base subclass:"] {
        let baseline = guards(&super_actor(header, "1"));
        for (kind, expr) in SUPER_KINDS {
            for (host, template) in CLOSURE_HOSTS {
                let body = template.replace("{B}", expr);
                assert_eq!(
                    guards(&super_actor(header, &body)),
                    baseline,
                    "{header} {kind} in {host}: a closure super-send runs on the cell\n{body}"
                );
                checked += 1;
            }
            assert_eq!(
                guards(&super_actor(header, expr)),
                baseline,
                "{header} {kind}: a method-level super-send threads State, no guard"
            );
            checked += 1;
        }
    }
    assert_eq!(checked, 2 * 3 * 7);
}

#[test]
fn value_types_and_class_methods_have_no_guard() {
    // A `Value` method cannot assign a slot (`self.x :=` is a compile error),
    // so a self-send in a closure never changes anything to drop.
    let value = "Value subclass: V
  field: n = 1
  peek: x => self.n + x
  run => #(1, 2) collect: [:x | self peek: x]
";
    assert_eq!(guards(value), guards("Value subclass: V\n  field: n = 1\n"));
    // A class method has no `State`; class variables are written in place
    // (ADR 0130), so a closure's class-side self-send has nothing to drop.
    let class_side = "Actor subclass: C
  state: total = 0
  class peek: x => x
  class run => #(1, 2) collect: [:x | self peek: x]
";
    assert_eq!(
        guards(class_side),
        guards("Actor subclass: C\n  state: total = 0\n")
    );
    // The same holds for a class-side `super` send in a closure: the class
    // method's own `context` is still `Actor`, but it has no instance `State`
    // (class-side `super` walks the metaclass chain via
    // `class_super_dispatch/4`, BT-4001).
    let class_super = "Actor subclass: C
  state: more = 0
  class run => #(1, 2) collect: [:x | super peek: x]
";
    assert_eq!(
        guards(class_super),
        guards("Actor subclass: C\n  state: more = 0\n")
    );
}

#[test]
fn a_tier2_closure_carries_state_back_and_no_closure_is_guarded() {
    // A block stored in a local that writes a captured local is a Tier 2 `fun`
    // (`StateAcc` in, `{Result, StateAccN}` out): the self-send's new `State`
    // is returned to the caller, so the guard (which exists for closures that
    // cannot hand it back) must not fire. (BT-3986: the host was a block an
    // actor self-send promoted; passing a writing block to a self-send is now
    // a compile error, so the stored-local host is used.)
    let tier2 = |body: &str| {
        format!(
            "Actor subclass: P
  state: hits = 0
  peek: x => self.hits + x
  sums =>
    c := 0
    blk := [:xs | c := c + 1. {body}]
    blk value: #(1, 2)
"
        )
    };
    assert_eq!(
        guards(&tier2("self peek: 1")),
        guards(&tier2("1")),
        "a Tier 2 closure carries State back; no guard"
    );
    // The same self-send in a plain Tier 1 closure is not guarded either: the
    // closure runs on the actor-state cell (ADR 0137).
    let tier1 = "Actor subclass: P
  state: total = 0
  each: blk => #(1, 2) collect: blk
  peek: x => self.total + x
  run => self each: [:x | self peek: x]
";
    assert_eq!(
        guards(tier1),
        guards(&tier1.replace("[:x | self peek: x]", "[:x | x]"))
    );
}
