// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4715: a `self.<field>` nil narrowing must not survive a send to `self`
//! (or `super`), because the callee may assign the field back to `nil`.
//! Local-variable narrowing is unaffected.

use super::common::*;

fn type_diagnostics(source: &str) -> Vec<Diagnostic> {
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    run_with_expect(&module, &hierarchy)
        .into_iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect()
}

fn class_with(body: &str) -> String {
    format!(
        r"
typed Actor subclass: Holder
  state: f :: Integer | Nil = nil
  reset -> Nil => self.f := nil
  need: v :: Integer -> Integer => v
{body}
"
    )
}

fn assert_nil_leak(body: &str) {
    let diags = type_diagnostics(&class_with(body));
    assert!(
        diags
            .iter()
            .any(|d| d.message.contains("expects Integer, got Integer | Nil")),
        "expected `expects Integer, got Integer | Nil`, got: {diags:#?}"
    );
}

fn assert_clean(body: &str) {
    let diags = type_diagnostics(&class_with(body));
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:#?}");
}

#[test]
fn post_guard_narrowing_dropped_after_self_send() {
    assert_nil_leak(
        "  run -> Integer =>\n    self.f isNil ifTrue: [^0]\n    self reset\n    self need: self.f",
    );
}

#[test]
fn if_false_block_narrowing_dropped_after_self_send() {
    assert_nil_leak(
        "  run -> Integer =>\n    self.f isNil ifFalse: [self reset. ^self need: self.f]\n    0",
    );
}

#[test]
fn and_block_narrowing_dropped_after_self_send() {
    assert_nil_leak(
        "  run -> Boolean =>\n    self.f notNil and: [self reset. (self need: self.f) > 0]",
    );
}

#[test]
fn super_send_drops_narrowing() {
    assert_nil_leak(
        "  run -> Integer =>\n    self.f isNil ifTrue: [^0]\n    super reset\n    self need: self.f",
    );
}

#[test]
fn self_send_inside_unrelated_block_drops_narrowing() {
    assert_nil_leak(
        "  run: c :: Boolean -> Integer =>\n    self.f isNil ifTrue: [^0]\n    c ifTrue: [self reset]\n    self need: self.f",
    );
}

#[test]
fn cascade_on_self_drops_narrowing() {
    assert_nil_leak(
        "  run -> Integer =>\n    self.f isNil ifTrue: [^0]\n    self reset; reset\n    self need: self.f",
    );
}

#[test]
fn narrowing_still_works_with_no_send_between_test_and_use() {
    assert_clean("  run -> Integer =>\n    self.f isNil ifTrue: [^0]\n    self need: self.f");
    assert_clean("  run -> Integer =>\n    self.f isNil ifFalse: [^self need: self.f]\n    0");
    assert_clean("  run -> Boolean =>\n    self.f notNil and: [(self need: self.f) > 0]");
}

#[test]
fn argument_is_evaluated_before_the_send_so_it_stays_narrowed() {
    assert_clean("  run -> Integer =>\n    self.f isNil ifTrue: [^0]\n    self need: self.f");
}

#[test]
fn local_narrowing_unaffected_by_self_send() {
    assert_clean(
        "  run -> Integer =>\n    x := self.f\n    x isNil ifTrue: [^0]\n    self reset\n    self need: x",
    );
}

#[test]
fn copying_field_into_local_before_send_is_the_fix() {
    assert_clean(
        "  run -> Integer =>\n    v := self.f\n    v isNil ifTrue: [^0]\n    self reset\n    self need: v",
    );
}

#[test]
fn diverging_self_send_does_not_drop_earlier_narrowing() {
    assert_clean(
        "  run -> Integer =>\n    self.f isNil ifTrue: [^0]\n    self.f > 5 ifTrue: [self error: \"big\"]\n    self need: self.f",
    );
}

#[test]
fn self_send_in_match_arm_drops_narrowing() {
    assert_nil_leak(
        "  run: v :: Symbol -> Integer =>\n    self.f isNil ifTrue: [^0]\n    v match: [#reset -> self reset; _ -> nil]\n    self need: self.f",
    );
}

#[test]
fn value_type_self_send_keeps_narrowing() {
    // A value type's `self.f :=` rebinds the callee's own `self`; the
    // caller's field cannot change, so the narrowing survives.
    let source = r"
typed Object subclass: Box
  state: f :: Integer | Nil = nil
  with: v :: Integer | Nil -> Box => self.f := v
  need: v :: Integer -> Integer => v
  run -> Integer =>
    self.f isNil ifTrue: [^0]
    self with: nil
    self need: self.f
";
    let diags: Vec<_> = type_diagnostics(source)
        .into_iter()
        .filter(|d| d.message.contains("expects Integer"))
        .collect();
    assert!(diags.is_empty(), "got: {diags:#?}");
}

#[test]
fn self_send_in_non_diverging_sibling_block_blocks_post_guard_narrowing() {
    assert_nil_leak(
        "  run -> Integer =>\n    self.f isNil ifTrue: [^0] ifFalse: [self reset]\n    self need: self.f",
    );
}

/// Regression pin, not coverage of the receiver check: an `or:` guard with a
/// non-`isNil` disjunct is not a narrowing shape, so `self.f` is never
/// narrowed after it and the leak is reported with or without the
/// `fallthrough_may_self_send` receiver arm.
#[test]
fn unrecognised_compound_guard_with_self_send_stays_unnarrowed() {
    assert_nil_leak(
        "  stale -> Boolean => true\n  run -> Integer =>\n    (self.f isNil or: [self stale]) ifTrue: [^0]\n    self need: self.f",
    );
}

#[test]
fn self_send_in_diverging_guard_block_keeps_post_guard_narrowing() {
    assert_clean(
        "  run -> Integer =>\n    self.f isNil ifTrue: [self reset. ^0]\n    self need: self.f",
    );
}

#[test]
fn self_send_in_diverging_guard_block_keeps_other_fields_narrowed() {
    let source = r"
typed Actor subclass: Pair
  state: f :: Integer | Nil = nil
  state: g :: Integer | Nil = nil
  reset -> Nil => self.f := nil
  need: v :: Integer -> Integer => v
  run -> Integer =>
    self.g isNil ifTrue: [^0]
    self.f isNil ifTrue: [self reset. ^0]
    self need: self.g
";
    let diags = type_diagnostics(source);
    assert!(diags.is_empty(), "got: {diags:#?}");
}

#[test]
fn loop_body_with_only_a_diverging_self_send_keeps_narrowing() {
    assert_clean(
        "  run: xs :: List -> Integer =>\n    self.f isNil ifTrue: [^0]\n    xs do: [:x :: Integer | x < 0 ifTrue: [self error: \"neg\"]]\n    self need: self.f",
    );
}

#[test]
fn diverging_self_send_in_non_diverging_guard_branch_keeps_narrowing() {
    assert_clean(
        "  run -> Integer =>\n    self.f isNil ifTrue: [^0] ifFalse: [self.f < 0 ifTrue: [self error: \"neg\"]]\n    self need: self.f",
    );
}

#[test]
fn self_send_before_divergence_in_on_do_block_drops_narrowing() {
    // The exception is caught, so control continues after the send and the
    // earlier `self reset` has run.
    assert_nil_leak(
        "  run -> Integer =>\n    self.f isNil ifTrue: [^0]\n    [self reset. self error: \"x\"] on: Error do: [:e | nil]\n    self need: self.f",
    );
}

/// Known gap (documented on `infer_message_send_with_receiver_ty`): a
/// self-send late in a repeated block body is not seen by an earlier
/// statement of the next iteration, so this is accepted although `self.f` is
/// `nil` on the second pass. When loops are handled this test should flip to
/// `assert_nil_leak`.
#[test]
fn known_gap_loop_body_self_send_not_seen_by_earlier_statement() {
    assert_clean(
        "  run -> Integer =>\n    self.f isNil ifTrue: [^0]\n    1 to: 3 do: [:i | self need: self.f. self reset]\n    0",
    );
}
