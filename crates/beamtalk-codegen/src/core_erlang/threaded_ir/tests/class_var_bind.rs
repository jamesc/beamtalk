// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `construct_and_verify_class_var_bind` (ADR 0110 contract),
//! `verify_simple_bind`, and `verify_body_with_opaque_version_gaps`
//! coverage.

use super::*;

// ── construct_and_verify_class_var_bind (ADR 0110 contract) ───────────
// Pins the production replacement/extension covering the two Bind-
// emission sites named in ADR 0111 §Phase D:
// `expressions.rs::generate_field_assignment` (Put) and
// `dispatch_codegen.rs::emit_class_var_result_unwrap` (Direct rebind).

fn put_op() -> BindOp {
    BindOp::Put {
        field: "runs".to_string(),
        value: ValueRef::Var("_Val0".to_string()),
        class_tag: ValueRef::Var("ClassSelf".to_string()),
    }
}

#[test]
fn construct_and_verify_class_var_bind_put_silent_with_shadow_write() {
    // generate_field_assignment's real post-ADR-0110 shape: block_depth
    // == 0 (shadow_write: true, shadow_write_eligible: true), first
    // mutation in the method (source_version: 0, target_version: 1), at
    // the method's own top frame.
    let (bind, errors) =
        construct_and_verify_class_var_bind(put_op(), true, FrameId::ROOT, true, 0, 1, span());
    assert_eq!(errors, Vec::new());
    assert_eq!(
        bind,
        ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::ClassVars, 1, FrameId::ROOT),
            source: VersionedVar::new(VersionPrefix::ClassVars, 0, FrameId::ROOT),
            op: put_op(),
            shadow_write: true,
            span: span(),
        },
        "the returned Bind must be the exact node a caller renders — no second shape"
    );
}

#[test]
fn construct_and_verify_class_var_bind_put_fires_when_shadow_write_missing_at_top_frame() {
    // The regression this exists to catch: block_depth == 0 (a top-frame
    // mutation, shadow_write_eligible: true) but the shadow write was
    // forgotten.
    let (_, errors) =
        construct_and_verify_class_var_bind(put_op(), false, FrameId::ROOT, true, 0, 1, span());
    assert_eq!(
        errors,
        vec![VerifyError::ShadowWriteMissing {
            mutated: VersionedVar::new(VersionPrefix::ClassVars, 1, FrameId::ROOT),
            at: span(),
        }],
        "expected ShadowWriteMissing, got: {errors:?}"
    );
}

#[test]
fn construct_and_verify_class_var_bind_put_fires_even_without_a_local_nlr_catch_in_this_method() {
    // Pins the exact ADR 0110 repro shape, not just the ADR's abbreviated
    // worked example: `CollectionDriver countedRun:over:` mutates a class
    // var, then invokes a *caller-supplied* block (`aBlock value: x`)
    // that contains no literal `^` of its own — `has_block_nlr_or_walk`
    // is `false` for that method, so codegen allocates it NO local NLR
    // try/catch at all (`self.current_nlr_token()` is `None` throughout
    // its body). The foreign `^` still relays, caught one layer out by
    // `apply_class_method_fun/6`'s unconditional `?IS_NLR` clause — so
    // `shadow_write_eligible` must be driven by `block_depth == 0` alone,
    // NEVER by whether this method happens to have its own NLR catch:
    // gating on the latter (an earlier version of this helper's bug)
    // would silently exempt this exact shape from the check.
    let (_, errors) =
        construct_and_verify_class_var_bind(put_op(), false, FrameId::ROOT, true, 0, 1, span());
    assert!(
        errors
            .iter()
            .any(|e| matches!(e, VerifyError::ShadowWriteMissing { .. })),
        "expected ShadowWriteMissing even with no local NLR catch, got: {errors:?}"
    );
}

#[test]
fn construct_and_verify_class_var_bind_put_silent_below_top_frame() {
    // A class-var mutation inside a nested block (block_depth > 0) is
    // legitimately shadow_write: false — already discarded on normal
    // return, not a regression. ADR 0111 Addendum 9: modeled
    // via `shadow_write_eligible: false` at a nested `frame`, not by
    // `frame` alone. Matches
    // `verify_shadow_write_missing_silent_below_top_frame`.
    let (_, errors) =
        construct_and_verify_class_var_bind(put_op(), false, FrameId::new(1), false, 0, 1, span());
    assert_eq!(errors, Vec::new());
}

#[test]
fn construct_and_verify_class_var_bind_direct_rebind_silent_never_requires_shadow_write() {
    // dispatch_codegen.rs's inherited-self-dispatch rebind: never itself
    // a shadow-write producer (the callee's own Bind already wrote it
    // under the same ClassSelf-tagged key) — its call site always passes
    // `FrameId::ROOT` (honest — it never claims a real nested identity)
    // with `shadow_write_eligible: false` (ADR 0111 Addendum 9, Question
    // 2), so this stays silent unconditionally regardless of `frame`.
    let op = BindOp::Direct(ValueRef::Var("_CV0".to_string()));
    let (_, errors) =
        construct_and_verify_class_var_bind(op, false, FrameId::ROOT, false, 0, 1, span());
    assert_eq!(errors, Vec::new());
}

#[test]
fn construct_and_verify_class_var_bind_direct_rebind_silent_with_a_nonzero_source_version() {
    // Regression for a real bug caught by this migration's own tests,
    // re-pinned under ADR 0111 Addendum 9's widened frame model:
    // even at the caller's now-honest `FrameId::ROOT`, `verify()`'s
    // frame stack alone would NOT save this call site from a spurious
    // `UnboundVersion` on a nonzero backfilled version — it is
    // `shadow_write_eligible: false` (Question 2's second wrap trigger,
    // `frame != FrameId::ROOT || !shadow_write_eligible`) that forces
    // the wrap, and the wrap is what supplies the `Threaded` frame push
    // `check_use`'s frame-flow rule needs. A nested class-var rebind
    // (`emit_class_var_result_unwrap`) is ALWAYS `shadow_write_eligible:
    // false`, and its `source_version` is the real, possibly-nonzero
    // `class_var_version()` (e.g. an earlier top-frame mutation already
    // minted `ClassVars1` before this nested rebind runs) — this must
    // stay silent, not spuriously report `UnboundVersion` for the
    // backfilled version.
    let op = BindOp::Direct(ValueRef::Var("_CV3".to_string()));
    let (bind, errors) =
        construct_and_verify_class_var_bind(op, false, FrameId::ROOT, false, 2, 3, span());
    assert_eq!(errors, Vec::new(), "got: {errors:?}");
    assert_eq!(
        bind,
        ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::ClassVars, 3, FrameId::ROOT),
            source: VersionedVar::new(VersionPrefix::ClassVars, 2, FrameId::ROOT),
            op: BindOp::Direct(ValueRef::Var("_CV3".to_string())),
            shadow_write: false,
            span: span(),
        }
    );
}

#[test]
fn construct_and_verify_class_var_bind_uses_the_real_version_numbers_not_a_fixed_0_to_1_step() {
    // The whole point of the migration off the deleted
    // `verify_class_var_bind` — a mutation later in the method (source
    // version 3, minted to 4) must be silent, not spuriously flagged,
    // and the returned Bind must carry those exact versions (never the
    // old fixture's hardcoded 0 -> 1).
    let (bind, errors) =
        construct_and_verify_class_var_bind(put_op(), true, FrameId::ROOT, true, 3, 4, span());
    assert_eq!(errors, Vec::new(), "got: {errors:?}");
    assert_eq!(
        bind,
        ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::ClassVars, 4, FrameId::ROOT),
            source: VersionedVar::new(VersionPrefix::ClassVars, 3, FrameId::ROOT),
            op: put_op(),
            shadow_write: true,
            span: span(),
        }
    );
}

// ── verify_simple_bind ──────────────────────────────────────────────────
// Pins the new coverage for `generate_field_assignment`'s two previously
// uninstrumented sibling branches (`Self{N}`/`State{N}`), which — unlike
// the class-var branch — construct zero `ThreadedIr` fixture on `main`
// today.

#[test]
fn verify_simple_bind_silent_on_a_method_first_mutation() {
    // The first `self.field := ...`/actor state mutation in a method:
    // source_version == 0 (the bare `Self`/`State` parameter, always
    // bound), target_version == 1.
    assert_eq!(
        verify_simple_bind(VersionPrefix::SelfVt, 0, 1, span()),
        Vec::new()
    );
    assert_eq!(
        verify_simple_bind(VersionPrefix::State, 0, 1, span()),
        Vec::new()
    );
}

#[test]
fn verify_simple_bind_silent_after_several_prior_mutations() {
    // A later mutation in the same method (source_version > 0) must not
    // spuriously fire UnboundVersion — the whole point of this helper's
    // (and `construct_and_verify_class_var_bind`'s) backfill chain.
    assert_eq!(
        verify_simple_bind(VersionPrefix::SelfVt, 4, 5, span()),
        Vec::new()
    );
    assert_eq!(
        verify_simple_bind(VersionPrefix::State, 7, 8, span()),
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
    let errors = verify_simple_bind(VersionPrefix::SelfVt, 2, 1, span());
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
        shadow_write: false,
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
            shadow_write: false,
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
            shadow_write: false,
            span,
        },
    ];
    let errors = verify_body_with_opaque_version_gaps(&ir);
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
            shadow_write: false,
            span,
        },
        ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::State, 1, FrameId::ROOT),
            source: VersionedVar::new(VersionPrefix::State, 0, FrameId::ROOT),
            op: BindOp::Direct(ValueRef::Literal("'_'")),
            shadow_write: false,
            span,
        },
    ];
    let errors = verify_body_with_opaque_version_gaps(&ir);
    assert!(
        errors.iter().any(|e| matches!(
            e,
            VerifyError::NonLinearVersion { var, producers: 2, .. }
                if *var == VersionedVar::new(VersionPrefix::State, 1, FrameId::ROOT)
        )),
        "expected NonLinearVersion for the duplicate State1 producer, got: {errors:?}"
    );
}

// ── Joint ShadowWriteMissing visibility over a real class-method-body
//    shape (ClassVars backfill + NlrCatch) ─────────────────────────────
//
// Before this coverage, `lower_class_method_body`'s output rode as ONE opaque
// `Statement` next to the real `NlrCatch` `generate_class_method_functions`/
// `generate_class_method_fun_from_block` prepend — any class-var `Bind`
// inside that opaque body was invisible to the `verify()` call run over
// the enclosing method's real IR, so `ShadowWriteMissing` could never
// fire jointly with a real class-method `NlrCatch` (only the isolated,
// synthetic-marker fixture `construct_and_verify_class_var_bind` builds
// per call site could see it). These two tests build the REALISTIC
// shape `lower_class_method_body` + its callers' `NlrCatch`-prepend now
// produce for `class run: aBlock => aBlock value: [:x | ^x].
// self.runs := self.runs + 1` (an opaque `Statement` preamble for the
// non-last self-send, a real `ClassVars` `Bind` for the last-statement
// mutation, a `Statement` reply epilogue, and a real prepended
// `NlrCatch` — mirroring `lower_class_method_last_class_var_bind`'s own
// three-`ThreadedStmt` output exactly) — not a two-node minimal
// fixture — proving the new `ClassVars` backfill and `ShadowWriteMissing`
// compose correctly over that realistic interleaving, in both
// directions.

fn realistic_class_method_body_ir(shadow_write: bool) -> Vec<ThreadedStmt> {
    let span = span();
    vec![
        ThreadedStmt::NlrCatch {
            boundary: NlrBoundary::ClassMethod {
                has_class_vars: true,
            },
            token: TokenId::new("_NlrToken0".to_string()),
            frame: FrameId::ROOT,
            span,
        },
        // Non-last statement: `aBlock value: [:x | ^x]` — an ordinary
        // opaque AST-directed send, no class-var content of its own.
        ThreadedStmt::Statement(
            docvec!["let _seq1 = call 'beamtalk_message_dispatch':'send'(...) in "],
            span,
        ),
        // Last statement: `self.runs := self.runs + 1` —
        // `lower_class_method_last_class_var_bind`'s real 3-node shape.
        ThreadedStmt::Statement(
            docvec!["let _Val2 = call 'erlang':'+'(call 'maps':'get'('runs', ClassVars), 1) in "],
            span,
        ),
        ThreadedStmt::Bind {
            target: class_var(1, FrameId::ROOT),
            source: class_var(0, FrameId::ROOT),
            op: BindOp::Put {
                field: "runs".to_string(),
                value: ValueRef::Var("_Val2".to_string()),
                class_tag: ValueRef::Var("ClassSelf".to_string()),
            },
            shadow_write,
            span,
        },
        ThreadedStmt::Statement(docvec!["{'class_var_result', _Val2, ClassVars1}"], span),
    ]
}

#[test]
fn verify_body_with_opaque_version_gaps_catches_shadow_write_missing_in_realistic_class_method_body()
 {
    // The previously-invisible case: a real class-var `Bind` (from
    // `lower_class_method_last_class_var_bind`) missing its ADR 0110
    // shadow write, sharing a body with a real `NlrCatch` — both now
    // constructed by production and verified in ONE call, exactly the
    // shape this issue's acceptance criteria names.
    let ir = realistic_class_method_body_ir(false);
    let errors = verify_body_with_opaque_version_gaps(&ir);
    assert!(
        errors.iter().any(|e| matches!(
            e,
            VerifyError::ShadowWriteMissing { mutated, .. }
                if *mutated == class_var(1, FrameId::ROOT)
        )),
        "expected ShadowWriteMissing over the realistic class-method body shape, got: {errors:?}"
    );
}

#[test]
fn verify_body_with_opaque_version_gaps_silent_on_realistic_class_method_body_with_shadow_write() {
    // Production's actual (correct) shape: `shadow_write: true`. Clean —
    // proves the new joint check doesn't spuriously fire over ordinary,
    // correctly-shadow-written class-method bodies.
    let ir = realistic_class_method_body_ir(true);
    assert_eq!(
        verify_body_with_opaque_version_gaps(&ir),
        Vec::new(),
        "correctly shadow-written class-method body should verify cleanly"
    );
}

#[test]
fn verify_body_with_opaque_version_gaps_classvars_backfill_does_not_spuriously_fire_shadow_write_missing()
 {
    // `class doStuff => self bump. self.total := self.total + 1` — a
    // non-last self-send (`self bump`, opaque Statement standing in for
    // `emit_class_var_result_unwrap`'s own internal `next_class_var()`
    // bump, ClassVars0 -> ClassVars1, no Bind of its own in THIS body's
    // IR) followed by a real top-level, correctly shadow-written
    // last-statement class-var Bind (ClassVars1 -> ClassVars2). The
    // backfill must synthesize a ClassVars0->ClassVars1 gap-filler
    // Bind to avoid UnboundVersion — that synthetic Bind must NOT
    // itself spuriously trip ShadowWriteMissing merely because it
    // carries `shadow_write: false` at FrameId::ROOT.
    let span = span();
    let ir = vec![
        ThreadedStmt::NlrCatch {
            boundary: NlrBoundary::ClassMethod {
                has_class_vars: true,
            },
            token: TokenId::new("_NlrToken0".to_string()),
            frame: FrameId::ROOT,
            span,
        },
        ThreadedStmt::Statement(
            docvec!["<opaque self-send, mints ClassVars1 with no Bind of its own>"],
            span,
        ),
        ThreadedStmt::Statement(
            docvec!["let _Val1 = call 'erlang':'+'(call 'maps':'get'('total', ClassVars1), 1) in "],
            span,
        ),
        ThreadedStmt::Bind {
            target: class_var(2, FrameId::ROOT),
            source: class_var(1, FrameId::ROOT),
            op: BindOp::Put {
                field: "total".to_string(),
                value: ValueRef::Var("_Val1".to_string()),
                class_tag: ValueRef::Var("ClassSelf".to_string()),
            },
            shadow_write: true,
            span,
        },
        ThreadedStmt::Statement(docvec!["{'class_var_result', _Val1, ClassVars2}"], span),
    ];
    let errors = verify_body_with_opaque_version_gaps(&ir);
    assert_eq!(
        errors,
        Vec::new(),
        "ClassVars gap-backfill's own synthetic Bind must not spuriously trigger \
         ShadowWriteMissing, got: {errors:?}"
    );
}

#[test]
fn verify_a_spliced_direct_rebind_never_spuriously_fires_shadow_write_missing() {
    // ADR 0118 phase 5a: `class doStuff => self bump.
    // self.total := self.total + 1` where `bump` is one of this
    // class's own `class_method_selectors()` — `lower_class_method_body`
    // now splices the non-last self-send's REAL prelude (produced by
    // `emit_class_var_result_unwrap` via `class_method_prelude_producer`)
    // instead of wrapping one opaque `Statement` around it, so its
    // `BindOp::Direct` rebind (`shadow_write: false` always — see
    // `emit_class_var_result_unwrap`'s own doc comment: a rebind is
    // never itself a shadow-write producer) is now a real node THIS
    // body's own joint `verify()` sees directly, sharing the body with
    // the last statement's genuine, correctly shadow-written
    // `BindOp::Put`. This is the regression this issue's own review
    // caught: before the `BindOp::Put`-only gate on `ShadowWriteMissing`
    // (this same commit), a spliced `Direct` rebind's `shadow_write:
    // false` fired the check spuriously, because the isolated
    // `construct_and_verify_class_var_bind` check's `shadow_write_eligible`
    // exemption is a fixture-only wrapping trick that never reaches the
    // returned `Bind` node itself.
    let span = span();
    let ir = vec![
        ThreadedStmt::NlrCatch {
            boundary: NlrBoundary::ClassMethod {
                has_class_vars: true,
            },
            token: TokenId::new("_NlrToken0".to_string()),
            frame: FrameId::ROOT,
            span,
        },
        // Non-last statement: `self bump` — `emit_class_var_result_unwrap`'s
        // real three-`ThreadedStmt` prelude, spliced directly.
        ThreadedStmt::Statement(
            docvec!["let _CMR1 = call 'module':'class_bump'(ClassSelf, ClassVars) in "],
            span,
        ),
        ThreadedStmt::Bind {
            target: class_var(1, FrameId::ROOT),
            source: class_var(0, FrameId::ROOT),
            op: BindOp::Direct(ValueRef::Doc(Document::Str(
                "case _CMR1 of <{'class_var_result', _MR2, _CV3}> when 'true' -> _CV3 \
                 <_PCV4> when 'true' -> ClassVars end",
            ))),
            shadow_write: false,
            span,
        },
        ThreadedStmt::Statement(docvec!["let _Unwrapped5 = case _CMR1 of ... end in "], span),
        // Last statement: `self.total := self.total + 1` — a genuine,
        // correctly shadow-written mutation.
        ThreadedStmt::Statement(
            docvec!["let _Val6 = call 'erlang':'+'(call 'maps':'get'('total', ClassVars1), 1) in "],
            span,
        ),
        ThreadedStmt::Bind {
            target: class_var(2, FrameId::ROOT),
            source: class_var(1, FrameId::ROOT),
            op: BindOp::Put {
                field: "total".to_string(),
                value: ValueRef::Var("_Val6".to_string()),
                class_tag: ValueRef::Var("ClassSelf".to_string()),
            },
            shadow_write: true,
            span,
        },
        ThreadedStmt::Statement(docvec!["{'class_var_result', _Val6, ClassVars2}"], span),
    ];
    let errors = verify(&ir);
    assert_eq!(
        errors,
        Vec::new(),
        "a spliced Direct rebind's shadow_write: false must not spuriously trigger \
         ShadowWriteMissing when jointly verified with a real NlrCatch, got: {errors:?}"
    );
}
