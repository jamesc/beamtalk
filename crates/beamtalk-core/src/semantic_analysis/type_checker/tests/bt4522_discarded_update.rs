// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4522 (ADR 0145 § 3): the `discarded-update` warning.
//!
//! A non-final statement that is a plain send answering an updated copy of a
//! value receiver (`p withX: 5.` alone) throws the copy away. It shares the
//! `cascade_drops_update` predicate with `cascade-drops-update`, which keeps
//! cascade statements.

use super::common::*;
use crate::source_analysis::Severity;

const POINT: &str = "\
Value subclass: Point
  field: x :: Integer = 0
  field: y :: Integer = 0
  moved -> Self => self withX: self x + 1
  bumped -> Integer => 1

Point subclass: Point3D
  field: z :: Integer = 0

Actor subclass: Counter
  state: n :: Integer = 0
  bump -> Self => self
";

fn diagnostics_for(body: &str) -> Vec<Diagnostic> {
    let source = format!("{POINT}\nObject subclass: Probe\n{body}");
    let module = parse_source(&source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    run_with_expect(&module, &hierarchy)
}

fn discarded(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::DiscardedUpdate))
        .collect()
}

#[test]
fn non_final_with_star_statement_warns() {
    let diags = diagnostics_for("  run: p :: Point -> Point =>\n    p withX: 5.\n    p\n");
    let hits = discarded(&diags);
    assert_eq!(hits.len(), 1, "got: {diags:#?}");
    let d = hits[0];
    assert_eq!(d.message.as_str(), "the result of `withX:` is discarded");
    assert_eq!(d.severity, Severity::Warning);
    let hint = d.hint.as_deref().unwrap_or_default();
    assert!(
        hint.contains("assign the result (`p := p withX: …`) or chain with `|>`")
            && hint.contains("`_ := p withX: …`"),
        "got: {hint}"
    );
}

#[test]
fn hand_written_self_returning_value_method_warns() {
    let diags = diagnostics_for("  run: p :: Point -> Point =>\n    p moved.\n    p\n");
    assert_eq!(discarded(&diags).len(), 1, "got: {diags:#?}");
}

#[test]
fn inherited_with_star_warns() {
    let diags = diagnostics_for("  run: p :: Point3D -> Point3D =>\n    p withZ: 5.\n    p\n");
    assert_eq!(discarded(&diags).len(), 1, "got: {diags:#?}");
}

#[test]
fn block_body_non_final_statement_warns() {
    let diags = diagnostics_for("  run: p :: Point -> Object =>\n    [p withX: 5. 3] value\n");
    assert_eq!(discarded(&diags).len(), 1, "got: {diags:#?}");
}

#[test]
fn final_statement_is_silent() {
    let diags = diagnostics_for("  run: p :: Point -> Point =>\n    p x.\n    p withX: 5\n");
    assert!(discarded(&diags).is_empty(), "got: {diags:#?}");
}

#[test]
fn used_results_are_silent() {
    let diags = diagnostics_for(
        "  run: p :: Point -> Point =>\n    q := p withX: 5.\n    (p withX: 1) x.\n    self use: (p withY: 2).\n    p withX: 3 |> withY: 4\n  use: q :: Point -> Point => q\n",
    );
    assert!(discarded(&diags).is_empty(), "got: {diags:#?}");
}

#[test]
fn non_update_message_is_silent() {
    let diags = diagnostics_for("  run: p :: Point -> Point =>\n    p bumped.\n    p x.\n    p\n");
    assert!(discarded(&diags).is_empty(), "got: {diags:#?}");
}

#[test]
fn dynamic_receiver_is_silent() {
    let diags = diagnostics_for("  run: p =>\n    p withX: 5.\n    p\n");
    assert!(discarded(&diags).is_empty(), "got: {diags:#?}");
}

#[test]
fn union_receiver_is_silent() {
    let diags =
        diagnostics_for("  run: p :: Point | Point3D -> Object =>\n    p withX: 5.\n    p\n");
    assert!(discarded(&diags).is_empty(), "got: {diags:#?}");
}

#[test]
fn actor_receiver_is_silent() {
    let diags = diagnostics_for("  run: c :: Counter -> Counter =>\n    c bump.\n    c\n");
    assert!(discarded(&diags).is_empty(), "got: {diags:#?}");
}

#[test]
fn non_value_receiver_is_silent() {
    let diags = diagnostics_for(
        "  tick -> Self => self\n  run: o :: Probe -> Probe =>\n    o tick.\n    o\n",
    );
    assert!(discarded(&diags).is_empty(), "got: {diags:#?}");
}

#[test]
fn pipeline_statement_warns_once_at_last_stage() {
    let diags = diagnostics_for("  run: p :: Point -> Point =>\n    p |> withX: 5.\n    p\n");
    assert_eq!(discarded(&diags).len(), 1, "got: {diags:#?}");
}

#[test]
fn cascade_statement_is_left_to_cascade_drops_update() {
    let diags =
        diagnostics_for("  run: p :: Point -> Point =>\n    p withX: 5; withY: 10.\n    p\n");
    assert!(discarded(&diags).is_empty(), "got: {diags:#?}");
    assert!(
        diags
            .iter()
            .any(|d| d.category == Some(DiagnosticCategory::CascadeDropsUpdate)),
        "got: {diags:#?}"
    );
}

#[test]
fn binary_send_statement_warns() {
    let diags = diagnostics_for("  run: p :: Point -> Integer =>\n    p x + 1.\n    3\n");
    let hits = discarded(&diags);
    assert_eq!(hits.len(), 1, "got: {diags:#?}");
    assert_eq!(hits[0].message.as_str(), "the result of `+` is discarded");
}

#[test]
fn pure_binary_statement_is_left_to_the_effect_free_lint() {
    let diags = diagnostics_for("  run -> Integer =>\n    1 + 2.\n    3\n");
    assert!(discarded(&diags).is_empty(), "got: {diags:#?}");
}

#[test]
fn help_only_suggests_assignment_for_a_plain_local() {
    // `self`, a literal or an expression receiver cannot be reassigned.
    let diags = diagnostics_for(
        "  run: p :: Point -> Point =>\n    p x + 1.\n    self runSelf.\n    p\n  runSelf -> Self =>\n    self runSelf.\n    self\n",
    );
    let hints: Vec<_> = discarded(&diags)
        .iter()
        .map(|d| d.hint.clone().unwrap_or_default())
        .collect();
    assert!(!hints.is_empty(), "got: {diags:#?}");
    assert!(
        hints.iter().all(|h| !h.contains("assign the result (`")),
        "no assignment help for a non-local receiver, got: {hints:?}"
    );
}

#[test]
fn expect_discarded_update_suppresses() {
    let diags = diagnostics_for(
        "  run: p :: Point -> Point =>\n    @expect discarded_update\n    p withX: 5.\n    p\n",
    );
    assert!(discarded(&diags).is_empty(), "got: {diags:#?}");
    assert!(
        !diags.iter().any(|d| d.message.contains("stale")),
        "the @expect must be satisfied, got: {diags:#?}"
    );
}

#[test]
fn parenthesised_statement_warns() {
    let diags = diagnostics_for("  run: p :: Point -> Point =>\n    (p withX: 5).\n    p\n");
    assert_eq!(discarded(&diags).len(), 1, "got: {diags:#?}");
}

#[test]
fn block_argument_note_does_not_claim_no_effect() {
    let diags = diagnostics_for("  run -> Integer =>\n    #(1, 2) collect: [:i | i].\n    3\n");
    let hits = discarded(&diags);
    assert_eq!(hits.len(), 1, "got: {diags:#?}");
    let note = &hits[0].notes[0].message;
    assert!(!note.contains("no effect"), "got: {note}");
    assert!(
        hits[0]
            .hint
            .as_deref()
            .unwrap_or_default()
            .contains("`do:`"),
        "got: {:?}",
        hits[0].hint
    );
}

// ── BT-4739: a method that answers its receiver is not an updated copy ──────
//
// Every false positive found in beamtalk-buccaneer is a `-> Self` method on a
// value that works through a side effect (or only to raise) and then answers
// `self`. A value cannot mutate itself, so that answer is the receiver.

const EFFECTS: &str = "\
Value subclass: Handle
  field: id :: Integer = 0
  store: v :: Integer -> Self =>
    Erlang erlang put: #handle value: v.
    self
  send: data :: String -> Self =>
    Erlang gen_tcp send: self id data: data.
    ^ self
  logError: reason :: String -> Self =>
    Logger error: reason.
    self
  stopWith: meta :: Object -> Self =>
    Erlang telemetry execute: #stop meta: meta.
    self
  checkPseudo: n :: Integer -> Self =>
    n < 0 ifTrue: [^ self error: \"bad\"].
    self
  checkBlock: n :: Integer -> Self =>
    n < 0 ifTrue: [^ self].
    self
  rebuilt -> Self =>
    self withId: 9
  mixed: n :: Integer -> Self =>
    n < 0 ifTrue: [^ self withId: 1].
    self
  emptyBody -> Self =>
";

fn effect_diagnostics(body: &str) -> Vec<Diagnostic> {
    let source = format!("{EFFECTS}\nObject subclass: Probe\n{body}");
    let module = parse_source(&source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    run_with_expect(&module, &hierarchy)
}

#[test]
fn side_effect_then_answer_self_is_silent() {
    // Process-dictionary store, socket send, Logger, telemetry: Buccaneer
    // categories 1, 3, 4, 6, 7, 8, 9.
    let diags = effect_diagnostics(
        "  run: h :: Handle -> Integer =>\n    h store: 1.\n    h send: \"x\".\n    h logError: \"e\".\n    h stopWith: 1.\n    3\n",
    );
    assert!(discarded(&diags).is_empty(), "got: {diags:#?}");
}

#[test]
fn validator_that_only_raises_is_silent() {
    // Buccaneer category 5: `checkPseudo:` and friends answer `self` when valid.
    let diags = effect_diagnostics(
        "  run: h :: Handle -> Integer =>\n    h checkPseudo: 1.\n    h checkBlock: 2.\n    3\n",
    );
    assert!(discarded(&diags).is_empty(), "got: {diags:#?}");
}

#[test]
fn self_send_to_a_side_effecting_method_is_silent() {
    let diags = effect_diagnostics(
        "  run: h :: Handle -> Integer =>\n    h store: 1; send: \"x\".\n    3\n",
    );
    assert!(discarded(&diags).is_empty(), "got: {diags:#?}");
    assert!(
        !diags
            .iter()
            .any(|d| d.category == Some(DiagnosticCategory::CascadeDropsUpdate)),
        "a cascade through a self-answering method drops nothing, got: {diags:#?}"
    );
}

#[test]
fn method_that_builds_a_copy_still_warns() {
    // The BT-4522 true positives: the generated `withId:` and a hand-written
    // method whose answer is a new value.
    let diags = effect_diagnostics(
        "  run: h :: Handle -> Integer =>\n    h withId: 5.\n    h rebuilt.\n    3\n",
    );
    assert_eq!(discarded(&diags).len(), 2, "got: {diags:#?}");
}

#[test]
fn method_with_a_non_self_return_on_any_path_still_warns() {
    // `^ self withId: 1` inside the block can answer a new copy, so the method
    // does not answer its receiver on every path.
    let diags = effect_diagnostics("  run: h :: Handle -> Integer =>\n    h mixed: 1.\n    3\n");
    assert_eq!(discarded(&diags).len(), 1, "got: {diags:#?}");
}

#[test]
fn empty_body_is_not_a_self_answer() {
    let diags = effect_diagnostics("  run: h :: Handle -> Integer =>\n    h emptyBody.\n    3\n");
    assert_eq!(discarded(&diags).len(), 1, "got: {diags:#?}");
}

#[test]
fn underscore_assignment_discards_on_purpose() {
    let diags = diagnostics_for("  run: p :: Point -> Point =>\n    _ := p withX: 3.\n    p\n");
    assert!(discarded(&diags).is_empty(), "got: {diags:#?}");
}

#[test]
fn test_case_helper_assigning_a_field_still_warns() {
    // `TestCase` may assign its own slots, so the trailing `self` is the
    // updated copy and `self seed: 3.` really drops an update.
    let source = "\
TestCase subclass: FooTest
  field: n :: Integer = 0
  seed: v :: Integer -> Self =>
    self.n := v.
    self
  testIt =>
    self seed: 3.
    self assert: self n equals: 3
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    assert_eq!(discarded(&diags).len(), 1, "got: {diags:#?}");
}

#[test]
fn map_literal_receiver_warns() {
    // BT-4731: a symbol-keyed literal is a shape; it must still be seen as
    // the `Dictionary` value its `at:put:` leaves unchanged.
    let diags = diagnostics_for(
        "  run =>\n    opts := #{#port => 1}\n    opts at: #host put: 2.\n    opts\n",
    );
    assert_eq!(discarded(&diags).len(), 1, "got: {diags:#?}");
}
