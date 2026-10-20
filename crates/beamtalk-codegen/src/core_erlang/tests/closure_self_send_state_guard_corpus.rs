// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3936 (ADR 0133 § Error examples): a self-send compiled inside a closure
//! cannot hand the callee's `State` back, so every such dispatch must carry
//! the `=:=` guard that raises `stateful_block_dispatch` when the callee
//! changed it. This corpus generates actor shapes (self-send kind x block host
//! x open/sealed class x callee) and checks, for each, that the number of
//! guards in the generated module equals the number of self-sends written
//! inside a closure: none missing (a silent drop), none extra (a spurious
//! raise on a send that threads State normally).
//!
//! `super` sends (BT-3985) are checked the same way by
//! `every_super_send_in_a_closure_is_guarded_and_no_other_is`.
//!
//! The behavioural side (state-changing sends raise, read-only ones answer) is
//! `stdlib/test/closure_self_send_matrix_test.bt`.

use super::tier2_block_local_inner_loop::compile;

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

/// Number of self-sends written in `body` (each host template instantiates the
/// kind expression once per `{B}`).
fn sends(template: &str) -> usize {
    template.matches("{B}").count()
}

#[test]
fn every_self_send_in_a_closure_is_guarded_and_no_other_is() {
    let mut checked = 0;
    for header in ["Actor subclass:", "sealed Actor subclass:"] {
        let baseline = guards(&actor(header, "1"));
        // A sealed class emits each method body more than once (the standalone
        // `__sealed_` function and the `dispatch/4` clause), so the guard count
        // per self-send is a per-class multiplier, at least 1.
        let per_send = guards(&actor(header, "self each: [:x | self peek]")) - baseline;
        assert!(
            per_send >= 1,
            "{header}: a closure self-send must be guarded"
        );
        for (kind, expr) in KINDS {
            for (host, template) in CLOSURE_HOSTS {
                let body = template.replace("{B}", expr);
                let n = guards(&actor(header, &body)) - baseline;
                assert_eq!(
                    n,
                    sends(template) * per_send,
                    "{header} {kind} in {host}: one guard per self-send inside a closure\n{body}"
                );
                checked += 1;
            }
            for (host, template) in INLINED_HOSTS {
                let body = template.replace("{B}", expr);
                let n = guards(&actor(header, &body)) - baseline;
                assert_eq!(
                    n, 0,
                    "{header} {kind} in {host}: an inlined position threads State, no guard\n{body}"
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
fn every_super_send_in_a_closure_is_guarded_and_no_other_is() {
    // The instance `super` path (`beamtalk_dispatch:super/5`) dispatches
    // against the current State like a self-send, so inside a Tier 1 closure
    // it carries the same guard. A method-level `super` send threads State
    // (no guard); a `super` in a Value or class method has no State at all.
    let mut checked = 0;
    for header in ["Base subclass:", "sealed Base subclass:"] {
        let baseline = guards(&super_actor(header, "1"));
        let per_send = guards(&super_actor(header, "self each: [:x | super peek: x]")) - baseline;
        assert!(
            per_send >= 1,
            "{header}: a closure super-send must be guarded"
        );
        for (kind, expr) in SUPER_KINDS {
            for (host, template) in CLOSURE_HOSTS {
                let body = template.replace("{B}", expr);
                let n = guards(&super_actor(header, &body)) - baseline;
                assert_eq!(
                    n,
                    sends(template) * per_send,
                    "{header} {kind} in {host}: one guard per super-send inside a closure\n{body}"
                );
                checked += 1;
            }
            let body = expr.to_string();
            let n = guards(&super_actor(header, &body)) - baseline;
            assert_eq!(
                n, 0,
                "{header} {kind}: a method-level super-send threads State, no guard\n{body}"
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
}

#[test]
fn a_tier2_closure_carries_state_back_so_its_self_sends_are_not_guarded() {
    // `fwd:`'s block parameter is a promoted Tier 2 position (`promote` passes
    // it a field-writing block), so the block `sums` passes is a Tier 2 `fun`
    // (`StateAcc` in, `{Result, StateAccN}` out): the self-send's new `State` is
    // returned to the caller, so the guard (which exists for closures that
    // cannot hand it back) must not fire. A consumer that cannot take the state
    // back raises `stateful_block_dispatch` itself.
    let tier2 = |body: &str| {
        format!(
            "Actor subclass: P
  state: groups = #(#(1, 2), #(3))
  state: hits = 0
  sealed fwd: block => self.groups collect: block
  peek: x => self.hits + x
  promote => self fwd: [:xs | self.hits := self.hits + 1. xs]
  sums => self fwd: [:xs | {body}]
"
        )
    };
    assert_eq!(
        guards(&tier2("self peek: 1")),
        guards(&tier2("1")),
        "a Tier 2 closure carries State back; no guard"
    );
    // The same self-send in a plain Tier 1 closure is guarded.
    let tier1 = "Actor subclass: P
  state: total = 0
  each: blk => #(1, 2) collect: blk
  peek: x => self.total + x
  run => self each: [:x | self peek: x]
";
    let tier1_baseline = tier1.replace("[:x | self peek: x]", "[:x | x]");
    assert!(guards(tier1) > guards(&tier1_baseline));
}
