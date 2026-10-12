// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0131 §1a: the sequencing rule's plain-variable exemption, amended — a
//! plain-variable sibling that a later sibling's threaded set contains is
//! snapshot to a `Tmp` before that sibling's prelude runs, so it reads the
//! value from *before* the sibling's `LocalRebind` (source order: `t + ([t :=
//! t + 1. 1] …)` is `0 + 1`). The verifier backing is
//! `LocalReadAfterSiblingRebind` (`threaded_ir/tests/local_rebind.rs`).

use super::codegen;

/// The body of `go:` from a one-method actor whose method is `body`.
fn method_body(body: &str) -> String {
    let src = format!(
        "Actor subclass: SnapProbe\n  state: n = 0\n\n  id: x => x\n\n  go: flag =>\n{body}"
    );
    let out = codegen(&src);
    let start = out
        .find("<'go:'> when 'true' ->")
        .expect("go: dispatch arm");
    out[start..].lines().take(6).collect::<Vec<_>>().join("\n")
}

/// Asserts `body` snapshots `t` to a `Tmp` before the conditional's tuple is
/// built, and that the binary op's left operand reads that snapshot.
fn assert_t_snapshot_before_construct(body: &str) {
    let snapshot = body.find("let _Tmp").expect("a snapshot let for `t`");
    let construct = body.find("let _CF").expect("the conditional's tuple");
    assert!(
        snapshot < construct,
        "the snapshot must run before the right operand's prelude: {body}"
    );
    let tmp = &body[snapshot + "let ".len()..];
    let tmp = &tmp[..tmp.find(' ').unwrap_or(tmp.len())];
    assert!(
        body.contains(&format!("let {tmp} = T in")),
        "the snapshot reads `t`: {body}"
    );
    assert!(
        body.contains(&format!("= {tmp} in")),
        "the binary op's left operand reads the snapshot: {body}"
    );
}

#[test]
fn a_left_operand_the_right_operand_threads_is_snapshot_before_its_prelude() {
    assert_t_snapshot_before_construct(&method_body(
        "    t := 0\n    r := t + (flag ifTrue: [t := t + 1. 1] ifFalse: [0])\n    r\n",
    ));
}

/// BT-3748 review: the construct need not be the right operand itself — any
/// construct reachable inside it without crossing a closure boundary counts.
#[test]
fn a_construct_nested_as_an_argument_of_the_right_operand_snapshots_the_left() {
    assert_t_snapshot_before_construct(&method_body(
        "    t := 0\n    r := t + (self id: (flag ifTrue: [t := t + 1. 1] ifFalse: [0]))\n    r\n",
    ));
}

#[test]
fn a_construct_nested_as_the_receiver_of_the_right_operand_snapshots_the_left() {
    assert_t_snapshot_before_construct(&method_body(
        "    t := 0\n    r := t + ((flag ifTrue: [t := t + 1. 1] ifFalse: [0]) + 0)\n    r\n",
    ));
}

#[test]
fn a_write_inside_a_closure_of_the_right_operand_does_not_snapshot_the_left() {
    // A non-construct block argument is a closure: its write is not the
    // right operand's threaded set (ADR 0131 §6 deals with it).
    let body = method_body(
        "    t := 0\n    r := t + (self id: (self id: [t := t + 1. 1]) value)\n    r\n",
    );
    assert!(
        !body.contains("let _Tmp"),
        "a closure's write must not trigger the snapshot: {body}"
    );
}

#[test]
fn a_left_operand_the_right_operand_does_not_thread_stays_in_place() {
    let body = method_body(
        "    t := 0\n    u := 5\n    r := u + (flag ifTrue: [t := t + 1. 1] ifFalse: [0])\n    r\n",
    );
    assert!(
        !body.contains("let _Tmp"),
        "`u` is not in the conditional's threaded set, so the exemption holds: {body}"
    );
}
