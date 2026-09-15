// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3511 (ADR 0122 Phase 2): the unified storage-family result-tuple
//! append/extract emission helper (`control_flow::family_slots`) — pins the
//! exact `Document` output for the three tuple shapes ADR 0122 names
//! (`[State]`, `[State, ClassVars]`, `[State, SelfVt]`, matching
//! `tests/gen_server/state_threading_loops.rs`'s
//! `{'nil', StateAcc, ClassVars}` and
//! `tests/control_flow/list_and_loop_value_threading.rs`'s
//! `{'nil', StateAcc, Self}` byte-for-byte), plus the two-family
//! `[State, ClassVars]` combination inside a value-type class method — and
//! proves the safety net: an extraction that forgets a family `verify()`
//! catches as `UnboundVersion`.
//!
//! Not wired into any emission site yet (BT-3511's own scope) — every test
//! here builds `ThreadedFamilies` and `ThreadedIr` fixtures directly, the
//! same way `threaded_ir/tests/*.rs` pins `render()`/`verify()` shapes
//! without going through a real compile.

use crate::core_erlang::CoreErlangGenerator;
use crate::core_erlang::control_flow::analysis::ThreadedFamilies;
use crate::core_erlang::control_flow::family_slots::{
    FamilyVersionStep, append_baseline_family_slots, append_family_slots, extract_family_slots,
};
use crate::core_erlang::threaded_ir::{
    FrameId, RenderCtx, ThreadedStmt, ValueRef, VerifyError, VersionPrefix, VersionedVar, render,
    verify,
};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_core::source_analysis::Span;

fn span() -> Span {
    Span::new(0, 1)
}

fn state(version: usize) -> VersionedVar {
    VersionedVar::new(VersionPrefix::State, version, FrameId::ROOT)
}

fn class_vars(version: usize) -> VersionedVar {
    VersionedVar::new(VersionPrefix::ClassVars, version, FrameId::ROOT)
}

fn self_vt(version: usize) -> VersionedVar {
    VersionedVar::new(VersionPrefix::SelfVt, version, FrameId::ROOT)
}

/// Renders `append_family_slots`/`append_baseline_family_slots`'s output
/// with a throwaway generator's `in_loop_body` set — the context every real
/// `while_loops.rs`/`counted_loops.rs` exit-arm tuple (the two corpus tests
/// this file pins against) renders under, giving `VersionPrefix::State` its
/// `StateAcc` spelling rather than the top-level `State` one.
fn append_in_loop_body(
    base: Document<'static>,
    families: &ThreadedFamilies,
    current: impl Fn(&VersionPrefix) -> VersionedVar,
) -> String {
    let mut generator = CoreErlangGenerator::new("bt@family_slots_append_test");
    generator.in_loop_body = true;
    let ctx = RenderCtx::new(&mut generator);
    append_family_slots(base, families, current, &ctx).to_pretty_string()
}

/// Renders `extract_family_slots`'s produced `Bind`s through the same
/// `render()` production every spliced `Bind` goes through, at the
/// generator's DEFAULT (non-loop-body) context — the context a construct's
/// OWN extraction site runs in today (after the loop/conditional/exception
/// construct has already returned its tuple), so `VersionPrefix::State`
/// renders as plain `State{N}`, not `StateAcc{N}` — matching
/// `conditionals.rs`'s own post-construct `element/N` extraction Binds
/// (`BodyExprKind::ControlFlowWithMutations`).
fn render_extracted(stmts: &[ThreadedStmt]) -> String {
    let mut generator = CoreErlangGenerator::new("bt@family_slots_extract_test");
    let mut ctx = RenderCtx::new(&mut generator);
    render(stmts, &mut ctx).to_pretty_string()
}

// ─── append_family_slots / append_baseline_family_slots ────────────────────

#[test]
fn append_state_only_shape() {
    // ADR 0122's trivial case (`State` already fits — the predicate is
    // always true): a loop with no ClassVars/SelfVt mutation carries only
    // the mandatory scratch-map slot.
    let families = ThreadedFamilies::from_matches(&[VersionPrefix::State]);
    let doc = append_in_loop_body(docvec!["{'nil'"], &families, |prefix| match prefix {
        VersionPrefix::State => state(0),
        other => panic!("unexpected family queried: {other:?}"),
    });
    assert_eq!(doc, "{'nil', StateAcc}");
}

#[test]
fn append_state_and_class_vars_shape() {
    // Matches `tests/gen_server/state_threading_loops.rs`'s
    // `test_class_method_self_send_in_while_loop_body_compiles_and_threads_class_vars`
    // exit-arm shape byte-for-byte: `{'nil', StateAcc, ClassVars}`.
    let families =
        ThreadedFamilies::from_matches(&[VersionPrefix::State, VersionPrefix::ClassVars]);
    let doc = append_in_loop_body(docvec!["{'nil'"], &families, |prefix| match prefix {
        VersionPrefix::State => state(0),
        VersionPrefix::ClassVars => class_vars(0),
        other => panic!("unexpected family queried: {other:?}"),
    });
    assert_eq!(doc, "{'nil', StateAcc, ClassVars}");
}

#[test]
fn append_state_and_self_vt_shape() {
    // Matches `tests/control_flow/list_and_loop_value_threading.rs`'s
    // `test_value_type_field_write_in_to_do_threads_self_through_tail_call`
    // exit-arm shape byte-for-byte: `{'nil', StateAcc, Self}`.
    let families = ThreadedFamilies::from_matches(&[VersionPrefix::State, VersionPrefix::SelfVt]);
    let doc = append_in_loop_body(docvec!["{'nil'"], &families, |prefix| match prefix {
        VersionPrefix::State => state(0),
        VersionPrefix::SelfVt => self_vt(0),
        other => panic!("unexpected family queried: {other:?}"),
    });
    assert_eq!(doc, "{'nil', StateAcc, Self}");
}

#[test]
fn append_state_and_class_vars_inside_a_value_type_class_method() {
    // ADR 0122 Decision 2: "Slot 2 is always the scratch map ... regardless"
    // — the two-family `[State, ClassVars]` combination this describes for a
    // VALUE-TYPE class method's own loop/conditional (its `StateAcc` scratch
    // map for LOCAL-variable threading, plus a `ClassVars` mutation).
    //
    // Not reachable via `eligible_families()` as written today —
    // `VersionPrefix::State` is eligible only for an ACTOR instance method
    // (`context == Actor && !in_class_method()`), which is mutually
    // exclusive with `ClassVars`' own `in_class_method()` gate — widening
    // that gate so a class method's own scratch-map slot is modeled as a
    // genuine `ThreadedFamilies` member (rather than the construct's own
    // pre-existing, family-detector-independent `StateAcc`/tuple-acc
    // convention) is a LATER migration issue's job, not this helper's. This
    // test exercises the general-purpose helper against the full matrix ADR
    // 0122 promises, independent of what `eligible_families()` currently
    // restricts — constructing the combination directly via
    // `ThreadedFamilies::from_matches` rather than through a real compile.
    let families =
        ThreadedFamilies::from_matches(&[VersionPrefix::State, VersionPrefix::ClassVars]);
    // Not in a loop body (a class method's own `on:do:`/`ensure:`-style
    // top-level construct, e.g.) — `State` renders plain, not `StateAcc`.
    let mut generator = CoreErlangGenerator::new("bt@family_slots_class_method_test");
    let ctx = RenderCtx::new(&mut generator);
    let doc = append_family_slots(
        docvec!["{Value"],
        &families,
        |prefix| match prefix {
            VersionPrefix::State => state(2),
            VersionPrefix::ClassVars => class_vars(3),
            other => panic!("unexpected family queried: {other:?}"),
        },
        &ctx,
    )
    .to_pretty_string();
    assert_eq!(doc, "{Value, State2, ClassVars3}");
}

#[test]
fn append_empty_families_just_closes_the_base_tuple() {
    let families = ThreadedFamilies::from_matches(&[]);
    let doc = append_in_loop_body(docvec!["{'nil'"], &families, |prefix| {
        panic!("no family should be queried for an empty list, got: {prefix:?}")
    });
    assert_eq!(doc, "{'nil'}");
}

#[test]
fn append_baseline_family_slots_is_append_family_slots_with_the_baseline_source() {
    // ADR 0122's "non-taken arm" operation: a thin, self-documenting entry
    // point over `append_family_slots`, never a second tuple-building
    // implementation — same families, same base, a different (here:
    // deliberately DIFFERENT-valued, to prove it's really the closure that
    // drives the output, not a hardcoded baseline) version source must
    // produce the exact same shape `append_family_slots` would for that
    // same closure.
    let families =
        ThreadedFamilies::from_matches(&[VersionPrefix::State, VersionPrefix::ClassVars]);
    let baseline = |prefix: &VersionPrefix| match prefix {
        VersionPrefix::State => state(0),
        VersionPrefix::ClassVars => class_vars(1),
        other => panic!("unexpected family queried: {other:?}"),
    };
    let mut gen_a = CoreErlangGenerator::new("bt@family_slots_baseline_a");
    let ctx_a = RenderCtx::new(&mut gen_a);
    let via_baseline_op =
        append_baseline_family_slots(docvec!["{'nil'"], &families, baseline, &ctx_a)
            .to_pretty_string();

    let mut gen_b = CoreErlangGenerator::new("bt@family_slots_baseline_b");
    let ctx_b = RenderCtx::new(&mut gen_b);
    let via_append =
        append_family_slots(docvec!["{'nil'"], &families, baseline, &ctx_b).to_pretty_string();

    assert_eq!(via_baseline_op, via_append);
    assert_eq!(via_baseline_op, "{'nil', State, ClassVars1}");
}

#[test]
fn append_and_extract_agree_on_slot_positions_for_all_three_families_together() {
    // The core invariant the module doc comment advertises ("a construct's
    // append/extract pair can never independently drift on slot order or
    // count"), checked directly rather than only by two separately-pinned
    // shapes that happen to agree: build ONE `ThreadedFamilies` covering all
    // three families, append it, then extract from the same `base_arity`,
    // and confirm the `element/N` reads land on exactly the positions
    // `append_family_slots` put each family's value at (2 = StateAcc, 3 =
    // ClassVars, 4 = Self, matching `appended`'s own 1-based tuple layout).
    let families = ThreadedFamilies::from_matches(&[
        VersionPrefix::State,
        VersionPrefix::ClassVars,
        VersionPrefix::SelfVt,
    ]);
    let current = |prefix: &VersionPrefix| match prefix {
        VersionPrefix::State => state(0),
        VersionPrefix::ClassVars => class_vars(0),
        VersionPrefix::SelfVt => self_vt(0),
        other => panic!("unexpected family queried: {other:?}"),
    };
    let appended = append_in_loop_body(docvec!["{'nil'"], &families, current);
    assert_eq!(appended, "{'nil', StateAcc, ClassVars, Self}");

    let stmts = extract_family_slots(
        "Tuple",
        1,
        &families,
        |prefix| match prefix {
            VersionPrefix::State => FamilyVersionStep::new(state(0), state(1)),
            VersionPrefix::ClassVars => FamilyVersionStep::new(class_vars(0), class_vars(1)),
            VersionPrefix::SelfVt => FamilyVersionStep::new(self_vt(0), self_vt(1)),
            other => panic!("unexpected family queried: {other:?}"),
        },
        span(),
    );
    assert_eq!(
        render_extracted(&stmts),
        "let State1 = call 'erlang':'element'(2, Tuple) in \
         let ClassVars1 = call 'erlang':'element'(3, Tuple) in \
         let Self1 = call 'erlang':'element'(4, Tuple) in "
    );
}

// ─── extract_family_slots ───────────────────────────────────────────────────

#[test]
fn extract_state_only_shape() {
    let families = ThreadedFamilies::from_matches(&[VersionPrefix::State]);
    let stmts = extract_family_slots(
        "Tuple",
        1,
        &families,
        |prefix| match prefix {
            VersionPrefix::State => FamilyVersionStep::new(state(0), state(1)),
            other => panic!("unexpected family queried: {other:?}"),
        },
        span(),
    );
    assert_eq!(
        render_extracted(&stmts),
        "let State1 = call 'erlang':'element'(2, Tuple) in "
    );
}

#[test]
fn extract_state_and_class_vars_shape() {
    let families =
        ThreadedFamilies::from_matches(&[VersionPrefix::State, VersionPrefix::ClassVars]);
    let stmts = extract_family_slots(
        "Tuple",
        1,
        &families,
        |prefix| match prefix {
            VersionPrefix::State => FamilyVersionStep::new(state(0), state(1)),
            VersionPrefix::ClassVars => FamilyVersionStep::new(class_vars(0), class_vars(1)),
            other => panic!("unexpected family queried: {other:?}"),
        },
        span(),
    );
    assert_eq!(
        render_extracted(&stmts),
        "let State1 = call 'erlang':'element'(2, Tuple) in \
         let ClassVars1 = call 'erlang':'element'(3, Tuple) in "
    );
}

#[test]
fn extract_state_and_self_vt_shape() {
    let families = ThreadedFamilies::from_matches(&[VersionPrefix::State, VersionPrefix::SelfVt]);
    let stmts = extract_family_slots(
        "Tuple",
        1,
        &families,
        |prefix| match prefix {
            VersionPrefix::State => FamilyVersionStep::new(state(0), state(1)),
            VersionPrefix::SelfVt => FamilyVersionStep::new(self_vt(0), self_vt(1)),
            other => panic!("unexpected family queried: {other:?}"),
        },
        span(),
    );
    assert_eq!(
        render_extracted(&stmts),
        "let State1 = call 'erlang':'element'(2, Tuple) in \
         let Self1 = call 'erlang':'element'(3, Tuple) in "
    );
}

#[test]
fn extract_family_slots_honors_a_nonzero_base_arity() {
    // Every shape above pins `base_arity: 1` (a bare `{'nil', ...}`
    // loop-exit tuple). A conditional's own `{Value, StateAcc, ...}` base
    // (`finish_vt_conditional_branch`'s two already-filled slots) needs the
    // family slots to start one position later — `base_arity: 2` — so this
    // pins the general `base_arity + i + 1` formula, not just the `1` case
    // every other test in this file happens to share.
    let families =
        ThreadedFamilies::from_matches(&[VersionPrefix::State, VersionPrefix::ClassVars]);
    let stmts = extract_family_slots(
        "Tuple",
        2,
        &families,
        |prefix| match prefix {
            VersionPrefix::State => FamilyVersionStep::new(state(0), state(1)),
            VersionPrefix::ClassVars => FamilyVersionStep::new(class_vars(0), class_vars(1)),
            other => panic!("unexpected family queried: {other:?}"),
        },
        span(),
    );
    assert_eq!(
        render_extracted(&stmts),
        "let State1 = call 'erlang':'element'(3, Tuple) in \
         let ClassVars1 = call 'erlang':'element'(4, Tuple) in "
    );
}

#[test]
fn extract_family_slots_produces_binds_that_verify_cleanly_when_every_target_is_consumed() {
    let families =
        ThreadedFamilies::from_matches(&[VersionPrefix::State, VersionPrefix::ClassVars]);
    let mut ir = extract_family_slots(
        "Tuple",
        1,
        &families,
        |prefix| match prefix {
            VersionPrefix::State => FamilyVersionStep::new(state(0), state(1)),
            VersionPrefix::ClassVars => FamilyVersionStep::new(class_vars(0), class_vars(1)),
            other => panic!("unexpected family queried: {other:?}"),
        },
        span(),
    );
    ir.push(ThreadedStmt::Return(
        ValueRef::Var("Result".to_string()),
        state(1),
        span(),
    ));
    assert_eq!(verify(&ir), Vec::new());
}

#[test]
fn extract_family_slots_omitting_a_family_fails_verify_with_unbound_version() {
    // The safety net BT-3511's acceptance criteria names: a caller that
    // builds its `families` list wrong (here: forgets `ClassVars`, exactly
    // BT-3506's "sixth gap" mistake — ADR 0122 §"Why the gaps keep
    // happening") never produces a `ClassVars` `Bind` at all, so any later
    // statement that reads the version the (correct) `append_family_slots`
    // side of the SAME construct claimed to produce is an `UnboundVersion`
    // `verify()` catches — never a silent dropped mutation.
    let extraction_families = ThreadedFamilies::from_matches(&[VersionPrefix::State]); // BUG: omits ClassVars
    let mut ir = extract_family_slots(
        "Tuple",
        1,
        &extraction_families,
        |prefix| match prefix {
            VersionPrefix::State => FamilyVersionStep::new(state(0), state(1)),
            other => panic!("unexpected family queried: {other:?}"),
        },
        span(),
    );
    // The construct's OWN `append_family_slots` call (elsewhere) claimed a
    // `[State, ClassVars]` result tuple, so the code that follows reads
    // `ClassVars1` as if it were live — but nothing here ever bound it.
    ir.push(ThreadedStmt::Return(
        ValueRef::Var("Result".to_string()),
        class_vars(1),
        span(),
    ));
    let errors = verify(&ir);
    assert_eq!(
        errors,
        vec![VerifyError::UnboundVersion {
            var: class_vars(1),
            at: span(),
        }],
        "an extraction that omits a family the construct actually threads \
         must fail verify() with UnboundVersion, got: {errors:?}"
    );
}
