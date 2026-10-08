// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `verify_simple_bind` and `verify_body_with_opaque_version_gaps` coverage.

use super::*;

// ── verify_simple_bind ──────────────────────────────────────────────────
// Pins coverage for `generate_field_assignment`'s value-type (`Self{N}`) and
// instance-actor (`State{N}`) branches.

#[test]
fn verify_simple_bind_silent_on_a_method_first_mutation() {
    // The first `self.field := ...`/actor state mutation in a method:
    // source_version == 0 (the bare `Self`/`State` parameter, always
    // bound), target_version == 1.
    assert_eq!(
        verify_simple_bind(VersionPrefix::SelfVt, 0, 1, span(), ScopeKind::Instance),
        Vec::new()
    );
    assert_eq!(
        verify_simple_bind(VersionPrefix::State, 0, 1, span(), ScopeKind::Instance),
        Vec::new()
    );
}

#[test]
fn verify_simple_bind_silent_after_several_prior_mutations() {
    // A later mutation in the same method (source_version > 0) must not
    // spuriously fire UnboundVersion — the whole point of this helper's
    // backfill chain.
    assert_eq!(
        verify_simple_bind(VersionPrefix::SelfVt, 4, 5, span(), ScopeKind::Instance),
        Vec::new()
    );
    assert_eq!(
        verify_simple_bind(VersionPrefix::State, 7, 8, span(), ScopeKind::Instance),
        Vec::new()
    );
}

#[test]
fn verify_simple_bind_fires_when_target_reuses_an_already_minted_version() {
    // A counter bug that re-mints a version already reached earlier
    // *within this single call's* backfilled history (instead of
    // advancing past it) is a genuine NonLinearVersion collision — this
    // is the within-call shape the backfill chain actually catches (see
    // `verify_simple_bind`'s doc comment's "Scope, honestly stated"
    // section for how this differs from the cross-call shape below).
    let errors = verify_simple_bind(VersionPrefix::SelfVt, 2, 1, span(), ScopeKind::Instance);
    assert!(
        errors.iter().any(|e| matches!(
            e,
            VerifyError::NonLinearVersion { var, producers: 2, .. }
                if *var == VersionedVar::new(VersionPrefix::SelfVt, 1, FrameId::ROOT)
        )),
        "expected a NonLinearVersion collision on version 1, got: {errors:?}"
    );
}

/// Demonstrates that `verify()` itself *would* catch a `self_version`
/// stale-read bug shape (`with_branch_context` briefly resetting
/// `self_version` to 0 on branch entry instead of inheriting the outer
/// version) *if* the two mutation call sites' `Bind`s were accumulated
/// into one shared history before
/// verifying — a `self.field := ...` immediately preceding a branch
/// (minting `Self1`) followed by another `self.field := ...`
/// immediately *inside* the branch would, under the reset-to-0 policy,
/// also compute `source_version = 0, target_version = 1`, re-minting the
/// SAME `Self1` a second time, which `verify()` reports as
/// `NonLinearVersion`.
///
/// **This is NOT a regression test for `check_simple_field_bind_invariant`
/// / `verify_simple_bind`'s real production wiring** — see
/// `verify_simple_bind`'s doc comment's "Scope, honestly stated" section.
/// That wiring verifies each mutation site in isolation with no
/// accumulated method-wide history, so it would NOT catch this exact
/// bug shape today; this test only proves the underlying `verify()`
/// primitive is capable of it, motivating the follow-up to thread real
/// history through.
#[test]
fn verify_would_catch_the_bt_3131_regression_shape_given_accumulated_history() {
    let span = span();
    // outer: self.x := 1, before the branch (correct: source 0, target 1).
    let mut ir = verify_bind_ir_for_test(VersionPrefix::SelfVt, 0, 1, span);
    // buggy: self.y := 2, immediately inside a `with_branch_context` arm
    // whose entry wrongly reset `self_version` to 0 instead of
    // inheriting the outer value of 1 — recomputes source 0, target 1.
    ir.extend(verify_bind_ir_for_test(VersionPrefix::SelfVt, 0, 1, span));

    let errors = verify(&ir);
    assert!(
        errors.iter().any(|e| matches!(
            e,
            VerifyError::NonLinearVersion { var, producers: 2, .. }
                if *var == VersionedVar::new(VersionPrefix::SelfVt, 1, FrameId::ROOT)
        )),
        "expected the reset-on-entry bug to collide on Self1, got: {errors:?}"
    );
}

/// Builds the same single-`Bind` IR fragment `verify_simple_bind` would
/// verify in isolation, but without calling `verify` itself — lets the
/// regression test above accumulate two call sites' fixtures into one
/// shared history before verifying, exactly as two real
/// `generate_field_assignment` call sites in the same method would both
/// contribute `Bind`s toward the same method-wide `Self{N}` sequence.
fn verify_bind_ir_for_test(
    prefix: VersionPrefix,
    source_version: usize,
    target_version: usize,
    span: Span,
) -> Vec<ThreadedStmt> {
    vec![ThreadedStmt::Bind {
        target: VersionedVar::new(prefix.clone(), target_version, FrameId::ROOT),
        source: VersionedVar::new(prefix, source_version, FrameId::ROOT),
        op: BindOp::Direct(ValueRef::Literal("'_'")),
        span,
    }]
}

// ── verify_body_with_opaque_version_gaps ──────────────────────────────
// Pins the whole-Actor-body verification `lower_body_exprs_with_reply`
// calls once per body: an opaque Statement standing in for a shared
// multi-module helper (`generate_self_dispatch_open`, …) may advance
// `next_state_var`'s counter with no producing Bind of its own in this
// body's IR — the backfill must close that gap without masking a real
// regression among the Binds that ARE present.

#[test]
fn verify_body_with_opaque_version_gaps_backfills_gap_from_opaque_statement() {
    let span = span();
    let ir = vec![
        ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::State, 1, FrameId::ROOT),
            source: VersionedVar::new(VersionPrefix::State, 0, FrameId::ROOT),
            op: BindOp::Direct(ValueRef::Literal("'_'")),
            span,
        },
        // Stands in for a shared helper (e.g. `generate_self_dispatch_open`)
        // that internally calls `next_state_var()` twice more, advancing
        // State1 -> State3 with no `Bind` of its own in this body's IR.
        ThreadedStmt::Statement(docvec!["<opaque dispatch, mints State2 and State3>"], span),
        ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::State, 4, FrameId::ROOT),
            source: VersionedVar::new(VersionPrefix::State, 3, FrameId::ROOT),
            op: BindOp::Direct(ValueRef::Literal("'_'")),
            span,
        },
    ];
    let errors = verify_body_with_opaque_version_gaps(&ir, ScopeKind::Instance);
    assert_eq!(
        errors,
        Vec::new(),
        "backfilled gap should leave no UnboundVersion, got: {errors:?}"
    );
}

#[test]
fn verify_body_with_opaque_version_gaps_still_catches_a_real_non_linear_version() {
    let span = span();
    // Two real Binds both target State1 from State0 — a genuine
    // duplicate-producer regression among the REAL Binds, which the
    // gap-backfill must not paper over.
    let ir = vec![
        ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::State, 1, FrameId::ROOT),
            source: VersionedVar::new(VersionPrefix::State, 0, FrameId::ROOT),
            op: BindOp::Direct(ValueRef::Literal("'_'")),
            span,
        },
        ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::State, 1, FrameId::ROOT),
            source: VersionedVar::new(VersionPrefix::State, 0, FrameId::ROOT),
            op: BindOp::Direct(ValueRef::Literal("'_'")),
            span,
        },
    ];
    let errors = verify_body_with_opaque_version_gaps(&ir, ScopeKind::Instance);
    assert!(
        errors.iter().any(|e| matches!(
            e,
            VerifyError::NonLinearVersion { var, producers: 2, .. }
                if *var == VersionedVar::new(VersionPrefix::State, 1, FrameId::ROOT)
        )),
        "expected NonLinearVersion for the duplicate State1 producer, got: {errors:?}"
    );
}

// ── verify_simple_bind: per-scope invariant (BT-3725 follow-up) ─────────

#[test]
fn verify_simple_bind_rejects_family_mint_in_a_class_method_scope() {
    // A class method has no `State`/`Self` parameter and threads no family:
    // a simple-bind mint of either is the actor-state-in-class-method defect.
    for prefix in [VersionPrefix::State, VersionPrefix::SelfVt] {
        assert_eq!(
            verify_simple_bind(prefix.clone(), 0, 1, span(), ScopeKind::ClassMethod),
            vec![VerifyError::ActorStateInClassMethod {
                defect: ClassMethodDefect::FamilyVersion(prefix.clone()),
                at: span(),
            }],
        );
        // The same bind in an instance scope stays clean.
        assert_eq!(
            verify_simple_bind(prefix, 0, 1, span(), ScopeKind::Instance),
            Vec::new()
        );
    }
}
