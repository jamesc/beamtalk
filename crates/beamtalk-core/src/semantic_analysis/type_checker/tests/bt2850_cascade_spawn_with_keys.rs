// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-2850: a cascade's `spawnWith: #{...}` continuation message now gets the
//! same literal-map key checking (ADR 0104 Phase 2, BT-2750) as any other
//! `spawnWith:` send, mirroring BT-2845's non-cascade fix for
//! `check_argument_types`.
//!
//! Root cause: `check_spawn_with_map_keys` is only called from
//! `infer_message_send_with_receiver_ty`, which handles the *first* message of
//! a cascade — in the class-reference branch and the Meta-typed receiver
//! branch. The `Expression::Cascade` arm's `for msg in messages` loop, which
//! handles every message *after* the first, never called it — so
//! `SomeActor create; spawnWith: #{typoKey => 1}` (typo'd key, `spawnWith:` as
//! a non-first cascade message) silently skipped the typo-suggestion warning,
//! unlike the same call written as its own statement or as a cascade's first
//! message.
//!
//! Fix: the cascade loop now also calls `check_spawn_with_map_keys` in its two
//! class-side-dispatch branches — the syntactic class-reference branch
//! (`is_class_ref`), and the branch that treats `self` inside a class method
//! as a class-side send (the cascade loop's equivalent of the non-cascade
//! Meta-typed-receiver branch, per BT-2845's three-way branch split).

use super::common::*;

fn key_diags(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.message.contains("state key"))
        .collect()
}

/// AC: `Counter spawn; spawnWith: #{#cuont => 0}` — a class-reference cascade
/// whose *second* message is `spawnWith:` with a typo'd key — now warns with a
/// typo suggestion, matching `Counter spawnWith: #{#cuont => 0}` written on
/// its own (see `adr_0104_integration::spawn_with_unknown_key_warns_with_suggestion`).
#[test]
fn bt2850_cascade_class_ref_continuation_spawn_with_typo_warns() {
    let source = "\
Actor subclass: Counter
  state: count = 0

Counter spawn; spawnWith: #{#cuont => 0}
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let diags = key_diags(&diags);
    assert_eq!(
        diags.len(),
        1,
        "cascade continuation `spawnWith:` with a typo'd key should warn: {diags:?}"
    );
    assert!(
        diags[0].message.contains("cuont") && diags[0].message.contains("did you mean"),
        "message should name the unknown key and suggest the nearest slot: {}",
        diags[0].message
    );
}

/// Negative control: the same cascade shape with a correctly-spelled key
/// produces no new diagnostic — the fix must not introduce false positives.
#[test]
fn bt2850_cascade_class_ref_continuation_spawn_with_known_key_is_clean() {
    let source = "\
Actor subclass: Counter
  state: count = 0

Counter spawn; spawnWith: #{#count => 0}
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    assert!(
        key_diags(&diags).is_empty(),
        "a correctly-spelled key on a cascade continuation must not warn: {diags:?}"
    );
}

/// AC: the `self`-inside-a-class-method cascade branch (the cascade loop's
/// equivalent of the non-cascade Meta-typed-receiver branch) also runs
/// `spawnWith:` key checking for a continuation message.
#[test]
fn bt2850_cascade_self_in_class_method_continuation_spawn_with_typo_warns() {
    let source = "\
Actor subclass: Counter
  state: count = 0

  class make -> Counter =>
    self spawn; spawnWith: #{#cuont => 0}
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let diags = key_diags(&diags);
    assert_eq!(
        diags.len(),
        1,
        "`self`-cascade continuation `spawnWith:` with a typo'd key should warn: {diags:?}"
    );
    assert!(
        diags[0].message.contains("cuont") && diags[0].message.contains("did you mean"),
        "message should name the unknown key and suggest the nearest slot: {}",
        diags[0].message
    );
}

/// Negative control for the `self` branch: a correctly-spelled key produces no
/// diagnostic.
#[test]
fn bt2850_cascade_self_in_class_method_continuation_spawn_with_known_key_is_clean() {
    let source = "\
Actor subclass: Counter
  state: count = 0

  class make -> Counter =>
    self spawn; spawnWith: #{#count => 0}
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    assert!(
        key_diags(&diags).is_empty(),
        "a correctly-spelled key on a `self`-cascade continuation must not warn: {diags:?}"
    );
}

/// No duplicate: when `spawnWith:` is the cascade's *first* message (checked
/// via `infer_message_send_with_receiver_ty`, not the continuation loop this
/// fix changes) followed by a valid continuation, exactly one warning fires —
/// the continuation loop must not re-check the first message.
#[test]
fn bt2850_cascade_first_message_spawn_with_typo_no_duplicate() {
    let source = "\
Actor subclass: Counter
  state: count = 0

Counter spawnWith: #{#cuont => 0}; yourself
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let diags = key_diags(&diags);
    assert_eq!(
        diags.len(),
        1,
        "the first cascade message's typo'd key should warn exactly once, not be duplicated: {diags:?}"
    );
}
