// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3409: `Collection>>detect:ifNone:` must widen its result type through
//! the `ifNone:` block's own return type (`E | T`) rather than the
//! catch-all `E | Object`, so a `nil`-returning `ifNone:` block infers `E |
//! Nil` — narrow enough for the type checker's existing `isNil`-guard flow
//! narrowing to apply to the result afterward.
//!
//! Before this fix, `detect:ifNone:` (and its `List`/`Stream`/
//! `SupervisionTree` overrides) declared the `ifNone:` block parameter as
//! `Block(Object)` and the return type as `E | Object` unconditionally.
//! `Object` is not `Nil`, so after an `isNil ifTrue: [^...]` early return,
//! the type checker's nil-narrowing (which only removes a `Nil` arm from a
//! union) left the value typed `E | Object` — unchanged — and sending it
//! any `E`-only message raised a spurious "Object does not understand ...
//! (in union E | Object)" diagnostic, forcing a manual re-annotation
//! workaround. Mirrors `Dictionary>>at:ifAbsent:`'s identical fix (BT-3408):
//! no type-checker logic changes were needed, only the stdlib declaration.

use super::common::*;

fn run_with_protocols(source: &str) -> Vec<Diagnostic> {
    let module = parse_source(source);
    let mut hierarchy = ClassHierarchy::with_builtins();
    let user_hierarchy = ClassHierarchy::build(&module).0.unwrap();
    hierarchy.merge(&user_hierarchy);

    let mut checker = TypeChecker::new();
    checker.check_module_with_protocols(
        &module,
        &hierarchy,
        &crate::semantic_analysis::protocol_registry::ProtocolRegistry::new(),
    );
    checker.take_diagnostics()
}

fn assert_no_dnu_diagnostic(diags: &[Diagnostic]) {
    let hits: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        hits.is_empty(),
        "expected no does-not-understand diagnostic, got: {hits:#?}"
    );
}

/// The exact idiom from the issue: `detect:ifNone: [nil]` followed by an
/// `isNil ifTrue: [^...]` early return, then sending an element-only message
/// to the (now flow-narrowed) result with no manual re-annotation.
#[test]
fn detect_ifnone_nil_block_narrows_after_isnil_guard() {
    let source = r"
Value subclass: Issue
  field: id :: Integer = 0

  id -> Integer => self.id

typed Actor subclass: Orchestrator
  releaseIfMissing: issueId :: Integer candidates: candidates :: List(Issue) -> Integer =>
    found := candidates detect: [:i | i id == issueId] ifNone: [nil]
    found isNil
      ifTrue: [^0]
    found id
";
    let diags = run_with_protocols(source);
    assert_no_dnu_diagnostic(&diags);
}

/// Same idiom through the `List`-specific `detect:ifNone:` override
/// (`stdlib/src/list.bt`), reached directly rather than via the inherited
/// `Collection` implementation.
#[test]
fn list_detect_ifnone_nil_block_narrows_after_isnil_guard() {
    let source = r"
Value subclass: Issue
  field: id :: Integer = 0

  id -> Integer => self.id

typed Actor subclass: Orchestrator
  find: issueId :: Integer in: issues :: List(Issue) -> Integer =>
    found := issues detect: [:i | i id == issueId] ifNone: [nil]
    found isNil
      ifTrue: [^0]
    found id
";
    let diags = run_with_protocols(source);
    assert_no_dnu_diagnostic(&diags);
}
