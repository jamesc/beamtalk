// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0137 Phase 2a (BT-4020): the `SyncedCall` node's rendering and its
//! version-step bookkeeping in the verifier.

use super::*;

fn state(version: usize, frame: FrameId) -> VersionedVar {
    VersionedVar::new(VersionPrefix::State, version, frame)
}

fn call_doc() -> Document<'static> {
    docvec!["call 'beamtalk_message_dispatch':'send'(Recv, 'do:', [Block])"]
}

fn synced(store: VersionedVar, load: Option<VersionedVar>) -> ThreadedStmt {
    ThreadedStmt::SyncedCall {
        store,
        call: call_doc(),
        result: Some("_Synced1".to_string()),
        load,
        span: span(),
        tier2_local_rebinds: Vec::new(),
        on_private_scratch: false,
        fast_path: None,
        reads: None,
    }
}

#[test]
fn a_synced_call_stores_calls_and_reloads() {
    let frame = FrameId::ROOT;
    let ir = [synced(state(3, frame), Some(state(4, frame)))];
    assert_eq!(
        lower_and_render(&ir).to_pretty_string(),
        "let _ = call 'beamtalk_actor_state':'store'(State3) in \
         let _Synced1 = call 'beamtalk_message_dispatch':'send'(Recv, 'do:', [Block]) in \
         let State4 = call 'beamtalk_actor_state':'take'() in "
    );
}

#[test]
fn a_store_only_synced_call_renders_the_statement_verbatim_after_the_store() {
    // The self-send form: the call is already a complete `let X = .. in`
    // statement and hands `State` back through its own reply.
    let frame = FrameId::ROOT;
    let ir = [ThreadedStmt::SyncedCall {
        store: state(2, frame),
        call: docvec!["let _SD1 = call 'm':'dispatch'() in "],
        result: None,
        load: None,
        span: span(),
        tier2_local_rebinds: Vec::new(),
        on_private_scratch: false,
        fast_path: None,
        reads: None,
    }];
    assert_eq!(
        lower_and_render(&ir).to_pretty_string(),
        "let _ = call 'beamtalk_actor_state':'store'(State2) in \
         let _SD1 = call 'm':'dispatch'() in "
    );
}

#[test]
fn a_synced_call_steps_state_like_a_bind_for_the_verifier() {
    let frame = FrameId::ROOT;
    // Reload `State1` from `State0`, then read it: clean.
    let ir = [
        synced(state(0, frame), Some(state(1, frame))),
        ThreadedStmt::Bind {
            target: state(2, frame),
            source: state(1, frame),
            op: BindOp::Put {
                field: "n".to_string(),
                value: ValueRef::Var("V".to_string()),
            },
            span: span(),
        },
    ];
    assert_eq!(verify(&ir), Vec::new());
}

#[test]
fn a_synced_call_storing_an_unproduced_version_is_unbound() {
    let frame = FrameId::ROOT;
    let ir = [synced(state(5, frame), Some(state(6, frame)))];
    let errors = verify(&ir);
    assert!(
        errors.contains(&VerifyError::UnboundVersion {
            var: state(5, frame),
            at: span()
        }),
        "{errors:?}"
    );
}

#[test]
fn a_store_only_synced_call_consumes_nothing() {
    // `State1` is consumed once, by the `Bind` that follows the dispatch;
    // a store-only bracket reading it too must not make it non-linear.
    let frame = FrameId::ROOT;
    let ir = [
        synced(state(0, frame), Some(state(1, frame))),
        ThreadedStmt::SyncedCall {
            store: state(1, frame),
            call: docvec!["let _SD1 = 'x' in "],
            result: None,
            load: None,
            span: span(),
            tier2_local_rebinds: Vec::new(),
            on_private_scratch: false,
            fast_path: None,
            reads: None,
        },
        ThreadedStmt::Bind {
            target: state(2, frame),
            source: state(1, frame),
            op: BindOp::Direct(ValueRef::Var("V".to_string())),
            span: span(),
        },
    ];
    assert_eq!(verify(&ir), Vec::new());
}

#[test]
fn two_reloads_of_the_same_version_are_non_linear() {
    let frame = FrameId::ROOT;
    let ir = [
        synced(state(0, frame), Some(state(1, frame))),
        synced(state(1, frame), Some(state(1, frame))),
    ];
    assert!(
        verify(&ir).iter().any(
            |e| matches!(e, VerifyError::NonLinearVersion { var, .. } if *var == state(1, frame))
        ),
        "{:?}",
        verify(&ir)
    );
}

#[test]
fn a_reload_in_another_frame_is_a_bind_frame_mismatch() {
    let ir = [synced(
        state(0, FrameId::ROOT),
        Some(state(1, FrameId::new(7))),
    )];
    assert!(
        verify(&ir)
            .iter()
            .any(|e| matches!(e, VerifyError::BindFrameMismatch { .. })),
        "{:?}",
        verify(&ir)
    );
}

#[test]
fn a_synced_call_in_a_loop_body_names_the_state_accumulator() {
    // Rendered in a `StateAcc` loop body the stored and reloaded versions are
    // `StateAccN`, like every other `State` step (`resolve_prefix`).
    let frame = FrameId::new(1);
    let ir = [synced(state(0, frame), Some(state(1, frame)))];
    let mut generator = CoreErlangGenerator::new("__synced_call_loop");
    generator.in_loop_body = true;
    let mut ctx = RenderCtx::new(&mut generator);
    let text = render(&ir, &mut ctx).to_pretty_string();
    assert!(
        text.contains("'store'(StateAcc) in")
            && text.contains("let StateAcc1 = call 'beamtalk_actor_state':'take'()"),
        "{text}"
    );
}

/// BT-4194: a bracket built on a private scratch map (an arm frame nested in a
/// tuple-acc or direct-params loop) would store that map over the cell.
#[test]
fn a_synced_call_on_a_private_scratch_is_reported() {
    let frame = FrameId::ROOT;
    let ir = [ThreadedStmt::SyncedCall {
        store: state(0, frame),
        call: call_doc(),
        result: Some("_Synced1".to_string()),
        load: Some(state(1, frame)),
        span: span(),
        tier2_local_rebinds: Vec::new(),
        on_private_scratch: true,
        fast_path: None,
        reads: None,
    }];
    let errors = verify(&ir);
    assert!(
        errors
            .iter()
            .any(|e| matches!(e, VerifyError::SyncedCallOnPrivateScratch { .. })),
        "{errors:?}"
    );
    // The well-formed twin is clean.
    assert_eq!(
        verify(&[synced(state(0, frame), Some(state(1, frame)))]),
        vec![]
    );
}

fn with_fast_path(mut stmt: ThreadedStmt) -> ThreadedStmt {
    if let ThreadedStmt::SyncedCall { fast_path: f, .. } = &mut stmt {
        *f = Some(fast_path());
    }
    stmt
}

fn fast_path() -> FastPath {
    FastPath {
        bindings: docvec!["let L = X in "],
        test: docvec!["call 'erlang':'is_number'(L)"],
        fast: docvec!["call 'erlang':'+'(L, 1)"],
        slow_result: "_SyncedSlow1".to_string(),
        slow_state: "_SyncedSlowState1".to_string(),
    }
}

/// BT-4279: a fast-path bracket hands the stored `State` through as the
/// reload on the guard's `true` arm and brackets only the other arm, stepping
/// `State` from `store` to `load` exactly like the plain bracket.
#[test]
fn a_fast_path_synced_call_brackets_only_its_slow_arm() {
    let frame = FrameId::ROOT;
    let ir = [with_fast_path(synced(
        state(3, frame),
        Some(state(4, frame)),
    ))];
    assert_eq!(
        lower_and_render(&ir).to_pretty_string(),
        "let L = X in let <_Synced1, State4> = case call 'erlang':'is_number'(L) of \
         <'true'> when 'true' -> <call 'erlang':'+'(L, 1), State3> \
         <_> when 'true' -> let _ = call 'beamtalk_actor_state':'store'(State3) in \
         let _SyncedSlow1 = call 'beamtalk_message_dispatch':'send'(Recv, 'do:', [Block]) in \
         let _SyncedSlowState1 = call 'beamtalk_actor_state':'take'() in \
         <_SyncedSlow1, _SyncedSlowState1> end in "
    );
    let ir = [
        with_fast_path(synced(state(0, frame), Some(state(1, frame)))),
        ThreadedStmt::Bind {
            target: state(2, frame),
            source: state(1, frame),
            op: BindOp::Put {
                field: "n".to_string(),
                value: ValueRef::Var("V".to_string()),
            },
            span: span(),
        },
    ];
    assert_eq!(verify(&ir), Vec::new());
}

/// BT-4279: a fast path on a bracket with no reload (the store-only
/// self-send form) has no `State` to hand through: reported.
#[test]
fn a_fast_path_on_a_store_only_bracket_is_reported() {
    let frame = FrameId::ROOT;
    let ir = [with_fast_path(synced(state(0, frame), None))];
    let errors = verify(&ir);
    assert!(
        errors
            .iter()
            .any(|e| matches!(e, VerifyError::FastPathOnUnsplittableBracket { .. })),
        "{errors:?}"
    );
}

// ── StalePrestore (BT-4612) ──────────────────────────────────────────────

/// The `^`-closure pre-store: a store-only bracket whose `store` is the
/// rendered name of `State` version `reads`.
fn prestore(reads: usize) -> ThreadedStmt {
    ThreadedStmt::SyncedCall {
        store: VersionedVar::new(
            VersionPrefix::Gensym(format!("State{reads}")),
            0,
            FrameId::ROOT,
        ),
        call: call_doc(),
        result: None,
        load: None,
        span: span(),
        tier2_local_rebinds: Vec::new(),
        on_private_scratch: false,
        fast_path: None,
        reads: Some(state(reads, FrameId::ROOT)),
    }
}

/// A pre-store that reads `State` version `reads` after an operand's
/// self-send stepped `State` from 0 to 1.
fn prestore_after_step(reads: usize) -> Vec<ThreadedStmt> {
    let step = ThreadedStmt::Bind {
        target: state(1, FrameId::ROOT),
        source: state(0, FrameId::ROOT),
        op: BindOp::Direct(ValueRef::Literal("'_'")),
        span: span(),
    };
    vec![step, prestore(reads)]
}

#[test]
fn a_prestore_older_than_a_state_the_frame_already_bound_is_stale() {
    // BT-4532's first cut: the dispatch pre-store read `State` before the
    // operands were threaded.
    assert_eq!(
        verify(&prestore_after_step(0)),
        vec![VerifyError::StalePrestore {
            reads: state(0, FrameId::ROOT),
            newest: state(1, FrameId::ROOT),
            at: span(),
        }]
    );
    assert_eq!(verify(&prestore_after_step(1)), Vec::new());
}

/// The pre-store is verified on its own, after the version its frame has
/// bound so far: that version stands for the earlier statement.
#[test]
fn a_prestore_older_than_the_state_bound_before_its_slice_is_stale() {
    let bound = state(3, FrameId::ROOT);
    assert_eq!(
        verify_in_scope_after(&[prestore(2)], ScopeKind::Instance, Some(bound.clone())),
        vec![VerifyError::StalePrestore {
            reads: state(2, FrameId::ROOT),
            newest: bound.clone(),
            at: span(),
        }]
    );
    assert_eq!(
        verify_in_scope_after(&[prestore(3)], ScopeKind::Instance, Some(bound)),
        Vec::new()
    );
    // Nothing bound before the slice: nothing to compare against.
    assert_eq!(verify(&[prestore(0)]), Vec::new());
}

/// The bound version is the slice's top level only: a nested frame's list
/// starts on its own.
#[test]
fn the_state_bound_before_a_slice_does_not_reach_a_nested_frame() {
    let nested = ThreadedStmt::BranchArm {
        frame: FrameId::ROOT,
        threads: Vec::new(),
        body: vec![prestore(0)],
    };
    assert_eq!(
        verify_in_scope_after(
            &[nested],
            ScopeKind::Instance,
            Some(state(1, FrameId::ROOT))
        ),
        Vec::new()
    );
}
