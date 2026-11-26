// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5298: a block that may run more than once (a `do:` body, a
//! `whileTrue:` condition or body, a block handed to any method that may call
//! it repeatedly) starts each later run from the values an earlier run left in
//! the enclosing method's locals. Its body is therefore inferred from the
//! loop-head join: for every outer local it writes, the type before the block
//! joined with every type the body leaves it with. A block that runs at most
//! once (`ifTrue:` arms, `and:`, a `value` receiver, ...) keeps the plain
//! entry types.

use super::common::*;

fn type_messages(source: &str) -> Vec<String> {
    let (_, parse_diags) =
        crate::source_analysis::parse(crate::source_analysis::lex_with_eof(source));
    assert!(
        parse_diags.is_empty(),
        "fixture must parse: {parse_diags:?}"
    );
    type_dnu_diags(source)
        .iter()
        .map(|d| d.message.to_string())
        .collect()
}

fn return_mismatches(diags: &[String]) -> Vec<&String> {
    diags
        .iter()
        .filter(|m| m.contains("declares return type"))
        .collect()
}

/// The issue's repro (beamtalk-oauth `OAuthFlows loopbackLogin:`): a local
/// initialised to `nil`, read by an early return and reassigned later in the
/// same `do:` body. On the second run it holds a `String`, and `notNil`
/// narrows it to one.
#[test]
fn nil_local_reassigned_in_do_body_is_string_at_early_return() {
    let diags = type_messages(
        r#"Object subclass: Probe
  class a: items :: List(String) -> Result(Integer, String) =>
    refused := nil
    items do: [:each |
      (refused notNil and: [each =:= "x"]) ifTrue: [^Result error: refused]
      refused := each
    ]
    Result ok: 1
"#,
    );
    assert!(
        return_mismatches(&diags).is_empty(),
        "the loop write must reach the early return: {diags:?}"
    );
}

/// The same shape with the write in a nested one-shot arm and the loop a
/// `whileTrue:` whose condition block is the receiver.
#[test]
fn nested_write_in_while_true_body_reaches_its_early_return() {
    let diags = type_messages(
        r#"Object subclass: Probe
  class b: n :: Integer -> Result(Integer, String) =>
    last := nil
    i := 0
    [i < n] whileTrue: [
      last notNil ifTrue: [^Result error: last]
      i > 2 ifTrue: [last := "late"]
      i := i + 1
    ]
    Result ok: i
"#,
    );
    assert!(
        return_mismatches(&diags).is_empty(),
        "the nested write must reach the early return: {diags:?}"
    );
}

/// A genuine mismatch is still reported, and names the loop-written type
/// rather than the `nil` the local started as.
#[test]
fn loop_written_wrong_type_is_still_reported() {
    let diags = type_messages(
        r"Object subclass: Probe
  class c: items :: List(Integer) -> Result(Integer, String) =>
    refused := nil
    items do: [:each |
      refused notNil ifTrue: [^Result error: refused]
      refused := each
    ]
    Result ok: 1
",
    );
    let mismatches = return_mismatches(&diags);
    assert_eq!(mismatches.len(), 1, "expected one mismatch: {diags:?}");
    assert!(
        mismatches[0].contains("Integer)"),
        "the mismatch must name the loop-written Integer: {diags:?}"
    );
}

/// An `ifTrue:` arm runs at most once, so its body starts from the plain
/// entry type: `^x` before the write still answers only an `Integer`, which
/// is reported against `-> Symbol`. (Were the arm widened, `^x` would be
/// `Integer | String`, which the return check does not report.)
#[test]
fn run_once_arm_keeps_its_entry_type() {
    let diags = type_messages(
        r#"Object subclass: Probe
  class e: flag :: Boolean -> Symbol =>
    x := 3
    flag ifTrue: [
      x notNil ifTrue: [^x]
      x := "s"
    ]
    #none
"#,
    );
    let mismatches = return_mismatches(&diags);
    assert_eq!(mismatches.len(), 1, "expected one mismatch: {diags:?}");
    assert!(
        !mismatches[0].contains("String"),
        "a run-once arm must not see its own later write: {diags:?}"
    );
}

/// A block invoked with `value` runs once too.
#[test]
fn value_receiver_block_keeps_its_entry_type() {
    let diags = type_messages(
        r#"Object subclass: Probe
  class f -> Symbol =>
    x := 3
    [x notNil ifTrue: [^x]. x := "s"] value
    #none
"#,
    );
    let mismatches = return_mismatches(&diags);
    assert_eq!(mismatches.len(), 1, "expected one mismatch: {diags:?}");
    assert!(
        !mismatches[0].contains("String"),
        "a `value` receiver must not see its own later write: {diags:?}"
    );
}

/// The speculative pass that finds the loop-head types is rolled back: a
/// diagnostic inside a loop body is reported once, not once per pass.
#[test]
fn speculative_pass_does_not_duplicate_diagnostics() {
    let diags = type_messages(
        r"Object subclass: Probe
  class g: items :: List(Integer) -> Integer =>
    total := nil
    items do: [:each |
      each zzNoSuchSelector
      total := each
    ]
    0
",
    );
    let dnus: Vec<&String> = diags
        .iter()
        .filter(|m| m.contains("zzNoSuchSelector"))
        .collect();
    assert_eq!(dnus.len(), 1, "expected exactly one DNU: {diags:?}");
}

/// A local whose new value depends on another loop-written local needs a
/// second pass: `prev` takes `cur`'s value, which is `Nil` on the first run
/// and a `String` after, so `prev` is a `String` from the third run on.
#[test]
fn chained_writes_reach_a_fixpoint() {
    let diags = type_messages(
        r"Object subclass: Probe
  class h: items :: List(String) -> Result(Integer, String) =>
    prev := nil
    cur := nil
    items do: [:each |
      prev notNil ifTrue: [^Result error: prev]
      prev := cur
      cur := each
    ]
    Result ok: 1
",
    );
    assert!(
        return_mismatches(&diags).is_empty(),
        "`prev` holds a String from the third run on: {diags:?}"
    );
}

/// A `whileTrue:` condition runs again after its body, so a write in the
/// body reaches the condition's early return.
#[test]
fn while_true_condition_sees_body_writes() {
    let diags = type_messages(
        r#"Object subclass: Probe
  class s: n :: Integer -> Result(Integer, String) =>
    last := nil
    i := 0
    [(last notNil and: [i > 5]) ifTrue: [^Result error: last]. i < n] whileTrue: [
      last := "x"
      i := i + 1
    ]
    Result ok: i
"#,
    );
    assert!(
        return_mismatches(&diags).is_empty(),
        "the body's write must reach the condition: {diags:?}"
    );
}

/// `do:separatedBy:` runs its separator between runs of the body, so the
/// separator sees the body's writes.
#[test]
fn separator_block_sees_body_writes() {
    let diags = type_messages(
        r"Object subclass: Probe
  class t: items :: List(String) -> Result(Integer, String) =>
    last := nil
    items do: [:each | last := each] separatedBy: [
      last notNil ifTrue: [^Result error: last]
    ]
    Result ok: 1
",
    );
    assert!(
        return_mismatches(&diags).is_empty(),
        "the body's write must reach the separator: {diags:?}"
    );
}

/// A write through a block stored inside the loop body (not a send
/// argument) reaches the loop head too: the speculative pass starts a fresh
/// send frame, as the real body does.
#[test]
fn stored_block_write_in_loop_body_reaches_loop_head() {
    let diags = type_messages(
        r"Object subclass: Probe
  class u: items :: List(String) -> Result(Integer, String) =>
    x := nil
    items do: [:each |
      x notNil ifTrue: [^Result error: x]
      b := [x := each]
      b value
    ]
    Result ok: 1
",
    );
    assert!(
        return_mismatches(&diags).is_empty(),
        "the stored block's write must reach the loop head: {diags:?}"
    );
}

/// A chain longer than the speculative passes can follow never leaves a
/// local narrower than a later run sees: it enters the block as `Dynamic`.
#[test]
fn unconverged_loop_head_falls_back_to_dynamic() {
    let diags = type_messages(
        r"Object subclass: Probe
  class v: items :: List(String) -> Result(Integer, String) =>
    p := nil
    a := nil
    b := nil
    c := nil
    items do: [:each |
      p notNil ifTrue: [^Result error: p]
      p := a
      a := b
      b := c
      c := each
    ]
    Result ok: 1
",
    );
    assert!(
        return_mismatches(&diags).is_empty(),
        "`p` holds a String from the fifth run on: {diags:?}"
    );
}

/// A sibling is inferred with the parameter types its own pass resolved, so
/// the separator's writes leave `last` as `String | Nil`, not `Dynamic`: a
/// selector neither member understands is still reported after the send.
#[test]
fn sibling_with_parameters_keeps_a_precise_join() {
    let diags = type_messages(
        r#"Object subclass: Probe
  class w: items :: List(String) -> Integer =>
    last := nil
    items do: [:each | last := each] separatedBy: [last := "sep"]
    last zzNoSuchSelector
    0
"#,
    );
    assert!(
        diags.iter().any(|m| m.contains("zzNoSuchSelector")),
        "`last` must stay `String | Nil` after the send: {diags:?}"
    );
}

/// Every message of a cascade on a block literal runs it again, so the
/// target block gets the loop-head join even when the first message is
/// `value`.
#[test]
fn cascaded_value_block_gets_the_loop_head_join() {
    let diags = type_messages(
        r#"Object subclass: Probe
  class x -> Result(Integer, String) =>
    x := nil
    [x notNil ifTrue: [^Result error: x]. x := "s"] value; value
    Result ok: 1
"#,
    );
    assert!(
        return_mismatches(&diags).is_empty(),
        "the second `value` sees the first run's write: {diags:?}"
    );
}
