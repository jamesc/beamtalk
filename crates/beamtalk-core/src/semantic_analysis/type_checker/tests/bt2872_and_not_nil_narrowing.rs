// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `X notNil and: [...]` narrows `X` to non-Nil inside the block argument,
//! including nested block-argument positions of binary sends (BT-2872).
//!
//! Root cause: there was no narrowing rule at all for `and:` — only
//! `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:` pushed a refinement layer before
//! type-checking a block argument. `local notNil and: [local > 0 and: [5 >=
//! local]]` therefore type-checked `local` as `Integer | Nil` throughout the
//! block. That went unnoticed while binary-send arguments skipped union
//! checking entirely; BT-2843 made binary-send argument checking
//! union-aware (matching pre-existing keyword-send behavior), which
//! surfaced the gap as a false-positive "Argument 1 of '>=' on Integer
//! expects Number, got Integer | Nil" diagnostic for `5 >= local` — the
//! *argument* position, not the receiver position `local > 0` (whose
//! own diagnostic path never validated the argument at all for a
//! Union-typed receiver, so it never fired regardless of narrowing).
//!
//! Fix: `infer_message_send_with_receiver_ty` special-cases `and:` sends
//! whose receiver is `<var> notNil`, pushing a non-nil refinement for `<var>`
//! before type-checking the block argument (mirroring `infer_block_with_narrowing`,
//! the same helper `ifTrue:` uses). Because `TypeEnv::child()` clones the
//! whole binding map, the refinement is visible to arbitrarily nested sends
//! inside the block, not just the block's top-level receiver positions.

use super::common::*;

fn type_diagnostics(source: &str) -> Vec<Diagnostic> {
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    run_with_expect(&module, &hierarchy)
        .into_iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect()
}

/// AC: `local notNil and: [local > 0 and: [5 >= local]]` — `local` is
/// narrowed to non-Nil inside nested `and:` blocks, including the
/// binary-message argument position `5 >= local`, not just the receiver
/// position `local > 0`.
#[test]
fn bt2872_not_nil_and_narrows_nested_binary_argument_position() {
    let source = r"
typed Object subclass: Checker
  check: x :: Integer | Nil -> Boolean =>
    local := x
    local notNil and: [
      local > 0 and: [5 >= local]
    ]
";
    let diags = type_diagnostics(source);
    assert!(
        diags.is_empty(),
        "`local` should be narrowed to Integer throughout the `and:` chain, got: {diags:#?}"
    );
}

/// A `self.field notNil and: [...]` receiver narrows the field the same way
/// as a local (BT-2048-style synthetic key path).
#[test]
fn bt2872_not_nil_and_narrows_self_field() {
    let source = r"
typed Actor subclass: Engine
  state: count :: Integer | Nil = nil
  check -> Boolean =>
    self.count notNil and: [5 >= self.count]
";
    let diags = type_diagnostics(source);
    assert!(
        diags.is_empty(),
        "`self.count` should be narrowed to Integer inside the `and:` block, got: {diags:#?}"
    );
}

/// An ordinary `and:` whose receiver is not a `notNil` test (e.g. a bare
/// `Boolean` local) must still type-check via the generic block-context
/// path — the new `and:` special-case must not swallow the fallback.
#[test]
fn bt2872_and_without_not_nil_receiver_falls_back_to_generic_dispatch() {
    let source = r"
typed Object subclass: Checker
  check: flag :: Boolean -> Boolean =>
    flag and: [true]
";
    let diags = type_diagnostics(source);
    assert!(
        diags.is_empty(),
        "plain `flag and: [...]` should type-check with no diagnostics, got: {diags:#?}"
    );
}

/// The non-nil narrowing must not leak past the `and:` block — a second,
/// unguarded use of the same nilable local after the `and:` send should
/// still see it as `Integer | Nil` and be flagged when passed to a
/// non-nilable parameter.
#[test]
fn bt2872_not_nil_and_narrowing_does_not_leak_past_the_block() {
    let source = r"
typed Object subclass: Consumer
  class take: v :: Integer => v

typed Object subclass: Checker
  check: x :: Integer | Nil -> Integer =>
    local := x
    local notNil and: [local > 0]
    Consumer take: local
";
    let diags = type_diagnostics(source);
    assert_eq!(
        diags.len(),
        1,
        "the unguarded `Consumer take: local` after the `and:` block should still \
         warn — narrowing must be block-scoped, got: {diags:#?}"
    );
}
