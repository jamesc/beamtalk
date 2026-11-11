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
        hint.contains("assign the result (`p := p withX: …`) or chain with `|>`"),
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
        hints.iter().all(|h| !h.contains(":=")),
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
