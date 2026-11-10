// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4512 (ADR 0145 § 3): the `cascade-drops-update` warning.
//!
//! On a value receiver, a non-final cascade message that answers an updated
//! copy has that copy thrown away — `p withX: 5; withY: 10` sends `withY:`
//! to `p`, not to the copy `withX:` answered. The warning fires at the
//! dropped message when the target is statically a value kind and the
//! message resolves to a method whose defining class is a value kind and
//! whose return type is `Self` or that class (type arguments ignored).

use super::common::*;
use crate::source_analysis::Severity;

const POINT: &str = "\
Value subclass: Point
  field: x :: Integer = 0
  field: y :: Integer = 0

Point subclass: Point3D
  field: z :: Integer = 0

Actor subclass: Counter
  state: n :: Integer = 0
  bump -> Self => self
  reset -> Self => self
";

fn diagnostics_for(body: &str) -> Vec<Diagnostic> {
    let source = format!("{POINT}\nObject subclass: Probe\n{body}");
    let module = parse_source(&source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    run_with_expect(&module, &hierarchy)
}

fn drops(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::CascadeDropsUpdate))
        .collect()
}

#[test]
fn value_with_star_cascade_warns_at_dropped_message() {
    let diags = diagnostics_for("  run: p :: Point -> Point =>\n    p withX: 5; withY: 10\n");
    let hits = drops(&diags);
    assert_eq!(hits.len(), 1, "expected one warning, got: {diags:#?}");
    let d = hits[0];
    assert_eq!(d.message.as_str(), "the result of `withX:` is discarded");
    assert_eq!(d.severity, Severity::Warning);
    let hint = d.hint.as_deref().unwrap_or_default();
    assert!(
        hint.contains("`(p withX: …) withY: …`"),
        "help should suggest the nested form, got: {hint}"
    );
    assert!(!hint.contains("|>"), "help must not mention `|>`: {hint}");
}

#[test]
fn every_non_final_message_warns() {
    let diags =
        diagnostics_for("  run: p :: Point3D -> Point3D =>\n    p withX: 1; withY: 2; withZ: 3\n");
    let messages: Vec<_> = drops(&diags).iter().map(|d| d.message.clone()).collect();
    assert_eq!(
        messages,
        vec![
            "the result of `withX:` is discarded",
            "the result of `withY:` is discarded"
        ],
        "got: {diags:#?}"
    );
}

#[test]
fn inherited_with_star_on_subclass_target_warns() {
    let diags = diagnostics_for("  run: p :: Point3D -> Point3D =>\n    p withX: 5; withZ: 10\n");
    assert_eq!(drops(&diags).len(), 1, "got: {diags:#?}");
}

#[test]
fn dictionary_at_put_warns() {
    let diags = diagnostics_for(
        "  run -> Dictionary =>\n    d := #{}\n    d at: #a put: 1; at: #b put: 2\n",
    );
    let hits = drops(&diags);
    assert_eq!(hits.len(), 1, "got: {diags:#?}");
    assert_eq!(
        hits[0].message.as_str(),
        "the result of `at:put:` is discarded"
    );
}

#[test]
fn integer_max_warns() {
    let diags = diagnostics_for("  run -> Integer =>\n    0 max: 1; max: 2\n");
    let hits = drops(&diags);
    assert_eq!(hits.len(), 1, "got: {diags:#?}");
    // BT-4573: a literal receiver is rendered as written, not elided.
    let hint = hits[0].hint.as_deref().unwrap_or_default();
    assert!(
        hint.contains("`(0 max: …) max: …`"),
        "help should render the literal receiver, got: {hint}"
    );
}

#[test]
fn expect_cascade_drops_update_suppresses() {
    let diags = diagnostics_for(
        "  run -> Integer =>\n    @expect cascade_drops_update\n    0 max: 1; max: 2\n",
    );
    assert!(drops(&diags).is_empty(), "got: {diags:#?}");
    assert!(
        !diags.iter().any(|d| d.message.contains("stale")),
        "the @expect must be satisfied, got: {diags:#?}"
    );
}

#[test]
fn dynamic_target_is_silent() {
    let diags = diagnostics_for("  run: p => \n    p withX: 5; withY: 10\n");
    assert!(drops(&diags).is_empty(), "got: {diags:#?}");
}

#[test]
fn actor_target_is_silent() {
    let diags = diagnostics_for("  run: c :: Counter -> Counter =>\n    c bump; reset\n");
    assert!(drops(&diags).is_empty(), "got: {diags:#?}");
}

#[test]
fn object_methods_are_silent() {
    let diags = diagnostics_for(
        "  run: p :: Point -> Point =>\n    p yourself; withX: 1\n    p show: 1; showCr: 2; withY: 3\n",
    );
    assert!(drops(&diags).is_empty(), "got: {diags:#?}");
}

#[test]
fn message_answering_another_type_is_silent() {
    let diags = diagnostics_for(
        "  run: p :: Point -> Point =>\n    p printString; withX: 1\n    p x; y; withY: 2\n",
    );
    assert!(drops(&diags).is_empty(), "got: {diags:#?}");
}

#[test]
fn final_position_message_is_silent() {
    let diags = diagnostics_for("  run: p :: Point -> Point =>\n    p printString; withX: 1\n");
    assert!(drops(&diags).is_empty(), "got: {diags:#?}");
}

#[test]
fn union_target_is_silent() {
    let diags =
        diagnostics_for("  run: p :: Point | Point3D -> Object =>\n    p withX: 5; withY: 10\n");
    assert!(drops(&diags).is_empty(), "got: {diags:#?}");
}

#[test]
fn object_instance_target_is_silent() {
    let diags = diagnostics_for(
        "  tick -> Self => self\n  run: o :: Probe -> Probe =>\n    o tick; tick\n",
    );
    assert!(drops(&diags).is_empty(), "got: {diags:#?}");
}
