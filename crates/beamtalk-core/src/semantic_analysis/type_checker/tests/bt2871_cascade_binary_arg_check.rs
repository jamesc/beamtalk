// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-2871: a binary selector used as a cascade *continuation* message now
//! gets the same argument-type coverage as any other cascade continuation
//! message, mirroring BT-2843's non-cascade fix for `infer_message_send_
//! with_receiver_ty`.
//!
//! Root cause: the cascade-message loop in `Expression::Cascade`'s handling
//! (`for msg in messages`) unconditionally skipped `check_argument_types` for
//! `MessageSelector::Binary` continuations, on the mistaken premise that
//! `check_binary_operand_types` already covered them. But
//! `check_binary_operand_types` is only ever called from
//! `infer_message_send_with_receiver_ty`, which runs for the *first* message
//! of a send/cascade — never for cascade continuations. So a binary
//! continuation message's argument went completely unchecked, with no
//! fallback at all (worse than the pre-BT-2843 bug, which at least covered
//! Known/Known shapes on the first send).
//!
//! Fix: the cascade loop now always falls back to the generic
//! `check_argument_types` for binary continuation messages too — the
//! "simpler" option from BT-2871's AC. `check_binary_operand_types`'s only
//! value-add over `check_argument_types` is more specific wording for
//! arithmetic/comparison/concat, not broader coverage, so there is no
//! more-specific check to defer to for a continuation message anyway.

use super::common::*;

fn type_diagnostics(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect()
}

/// AC: a binary cascade continuation with an incompatible `Known` argument
/// now produces a Type diagnostic. `Thing` defines its own `+` operator
/// taking `Integer`; the cascade's second message sends `+ "wrong-type"` to
/// the same `c` receiver (cascade semantics — not chained onto the first
/// message's return value).
#[test]
fn bt2871_cascade_binary_continuation_known_arg_mismatch_warns() {
    let source = "\
typed Value subclass: Thing
  probeCascadeA: x :: Integer -> Thing => self
  + other :: Integer -> Integer => other

  class probeCascadeRun -> Nil =>
    c := self new
    c probeCascadeA: 1; + \"wrong-type\"
    nil
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let arg_warnings: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("expects Integer"))
        .collect();
    assert_eq!(
        arg_warnings.len(),
        1,
        "binary cascade continuation `+ \"wrong-type\"` should warn about the \
         String argument, got: {diags:#?}"
    );
}

/// AC: a binary cascade continuation with a `Union` argument containing no
/// compatible member also produces a diagnostic — mirrors BT-2843's AC (b)
/// for the cascade-continuation path.
#[test]
fn bt2871_cascade_binary_continuation_union_arg_mismatch_warns() {
    let source = "\
typed Value subclass: Thing
  probeCascadeA: x :: Integer -> Thing => self
  + other :: Integer -> Integer => other

  class probeCascadeRun: y :: String | Symbol -> Nil =>
    c := self new
    c probeCascadeA: 1; + y
    nil
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let arg_warnings: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("expects Integer"))
        .collect();
    assert_eq!(
        arg_warnings.len(),
        1,
        "binary cascade continuation `+ y` (y :: String | Symbol) should warn \
         — no union member is Integer, got: {diags:#?}"
    );
}

/// AC: a binary cascade continuation where the argument type is compatible
/// with the declared parameter type stays diagnostic-free — no false
/// positives introduced by the new coverage.
#[test]
fn bt2871_cascade_binary_continuation_compatible_no_diagnostic() {
    let source = "\
typed Value subclass: Thing
  probeCascadeA: x :: Integer -> Thing => self
  + other :: Integer -> Integer => other

  class probeCascadeRun -> Nil =>
    c := self new
    c probeCascadeA: 1; + 2
    nil
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    assert!(
        type_diagnostics(&diags).is_empty(),
        "compatible binary cascade continuation `+ 2` should stay \
         diagnostic-free, got: {diags:#?}"
    );
}

/// AC: no duplicate diagnostics on the *first* cascade message. The first
/// message of a cascade (`n + \"wrong\"`) is handled by
/// `infer_message_send_with_receiver_ty`, not the continuation loop this
/// issue changes — `Thing` isn't a numeric type, so
/// `check_binary_operand_types` has no bespoke logic for it (`handled ==
/// false`) and the existing BT-2843 fallback fires exactly once. The
/// continuation loop must not re-check that same first message.
#[test]
fn bt2871_cascade_first_message_binary_mismatch_no_duplicate() {
    let source = "\
typed Value subclass: Thing
  + other :: Integer -> Integer => other
  probeCascadeB -> Integer => 42

  class probeCascadeRun -> Nil =>
    c := self new
    c + \"wrong-type\"; probeCascadeB
    nil
";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let arg_warnings: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("expects Integer"))
        .collect();
    assert_eq!(
        arg_warnings.len(),
        1,
        "the first cascade message's binary mismatch should produce exactly \
         one diagnostic, not a duplicate: {diags:#?}"
    );
}
