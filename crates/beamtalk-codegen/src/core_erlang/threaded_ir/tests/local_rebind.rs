// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0131 Phase 1b/1c: `ConstructTuple`, `LocalRebind`, `DiscardLocals`,
//! `MethodBody` and `BranchArm` — hand-built IR for every row of the §2
//! table (frame mode × membership), asserting the rendered `Document` text,
//! and the verifier's checks over them (`ThreadedLocalDropped`,
//! `LocalRebindModeMismatch`, `LocalRebindFrameMismatch`,
//! `LocalReadAfterSiblingRebind`). No production code builds these nodes
//! yet (Phase 2/3).

use super::*;
use crate::core_erlang::control_flow::analysis::ThreadedFamilies;
use crate::core_erlang::control_flow::family_slots::{FamilyVersionStep, extract_family_slots};

const CARRIER: &str = "_CF3";

fn state(version: usize, frame: FrameId) -> VersionedVar {
    VersionedVar::new(VersionPrefix::State, version, frame)
}

fn gensym(name: &str, frame: FrameId) -> VersionedVar {
    VersionedVar::new(VersionPrefix::Gensym(name.to_string()), 1, frame)
}

fn key_slot(local: &str, value_var: &str) -> ThreadedLocalSlot {
    ThreadedLocalSlot {
        local: local.to_string(),
        slot: CarrierSlot::Key(CoreErlangGenerator::local_state_key(local)),
        value_var: value_var.to_string(),
    }
}

fn tuple_doc() -> Document<'static> {
    docvec!["call 'm':'construct'(T)"]
}

/// The frame a producer's prelude is lowered by, read off a skeleton of the
/// enclosing node exactly as a producer's frame stack (and the verifier)
/// would.
fn frame_of(node: &ThreadedStmt) -> RebindFrame {
    RebindFrame::of(node).expect("a frame node")
}

fn threads(names: &[&str]) -> Vec<String> {
    names.iter().map(ToString::to_string).collect()
}

/// The lowering-time names a production caller would mint for `shape` —
/// `MapPut` steps the frame's `State` map from version 0 to 1, a
/// `LoopParam` steps the local's loop identity to `Gensym(value_var)`.
fn mint(
    frame: FrameId,
    key: &str,
) -> impl FnMut(&ThreadedLocalSlot, RebindShape) -> RebindLowering {
    let key = key.to_string();
    move |local, shape| match shape {
        RebindShape::Let => RebindLowering::Let,
        RebindShape::LoopParam => RebindLowering::LoopParam {
            source: VersionedVar::new(VersionPrefix::Local(local.local.clone()), 0, frame),
            target: gensym(&local.value_var, frame),
        },
        RebindShape::MapPut => RebindLowering::MapPut {
            key: key.clone(),
            source: state(0, frame),
            target: state(1, frame),
        },
    }
}

fn render_str(ir: &[ThreadedStmt]) -> String {
    lower_and_render(ir).to_pretty_string()
}

// ── The §2 table itself ──────────────────────────────────────────────────

#[test]
fn rebind_shape_table_covers_every_frame_mode_and_membership() {
    let state_acc = RebindFrameKind::Loop(ThreadingMode::StateAcc(StateAccFallbackReason::None));
    let cases = [
        (RebindFrameKind::MethodBody, true, RebindShape::MapPut),
        (RebindFrameKind::MethodBody, false, RebindShape::Let),
        (state_acc.clone(), true, RebindShape::MapPut),
        (state_acc, false, RebindShape::Let),
        (
            RebindFrameKind::Loop(ThreadingMode::DirectParams),
            true,
            RebindShape::LoopParam,
        ),
        (
            RebindFrameKind::Loop(ThreadingMode::DirectParams),
            false,
            RebindShape::Let,
        ),
        (
            RebindFrameKind::Loop(ThreadingMode::Hybrid),
            true,
            RebindShape::LoopParam,
        ),
        (
            RebindFrameKind::Loop(ThreadingMode::Hybrid),
            false,
            RebindShape::Let,
        ),
        (
            RebindFrameKind::Loop(ThreadingMode::TupleAcc(1)),
            true,
            RebindShape::LoopParam,
        ),
        (
            RebindFrameKind::Loop(ThreadingMode::TupleAcc(1)),
            false,
            RebindShape::Let,
        ),
        (RebindFrameKind::BranchArm, true, RebindShape::MapPut),
        (RebindFrameKind::BranchArm, false, RebindShape::Let),
    ];
    for (kind, member, expected) in cases {
        assert_eq!(
            RebindShape::for_frame(&kind, member),
            expected,
            "{kind:?} × member={member}"
        );
    }
}

#[test]
fn rebind_frame_reads_mode_and_threads_off_the_enclosing_node() {
    let method = build_method_body(FrameId::ROOT, threads(&["t"]), Vec::new());
    assert_eq!(
        frame_of(&method),
        RebindFrame {
            frame: FrameId::ROOT,
            kind: RebindFrameKind::MethodBody,
            threads: threads(&["t"]),
        }
    );
    let arm = build_branch_arm(FrameId::new(4), threads(&["u"]), Vec::new());
    assert_eq!(frame_of(&arm).kind, RebindFrameKind::BranchArm);
    let fold = ThreadedStmt::Threaded {
        mode: ThreadingMode::TupleAcc(0),
        frame: FrameId::new(2),
        threads: threads(&["t"]),
        body: Vec::new(),
        produces: Vec::new(),
        span: span(),
    };
    assert_eq!(
        frame_of(&fold),
        RebindFrame {
            frame: FrameId::new(2),
            kind: RebindFrameKind::Loop(ThreadingMode::TupleAcc(0)),
            threads: threads(&["t"]),
        }
    );
    assert!(RebindFrame::of(&ThreadedStmt::Statement(Document::Nil, span())).is_none());
}

#[test]
fn build_local_rebind_asks_for_the_frames_shape_and_records_the_frame() {
    let frame = FrameId::new(5);
    let enclosing = frame_of(&build_branch_arm(frame, threads(&["t"]), Vec::new()));
    let mut asked = None;
    let rebind = build_local_rebind(
        &enclosing,
        CARRIER,
        &key_slot("t", "_T1"),
        None,
        span(),
        |shape| {
            asked = Some(shape);
            RebindLowering::Let
        },
    );
    assert_eq!(asked, Some(RebindShape::MapPut));
    assert!(matches!(
        rebind,
        ThreadedStmt::LocalRebind { frame: f, ref local, .. } if f == frame && local == "t"
    ));
}

// ── ConstructTuple / DiscardLocals ───────────────────────────────────────

#[test]
fn construct_tuple_binds_the_carrier_and_discard_renders_nothing() {
    let ir = vec![
        build_construct_tuple(CARRIER, tuple_doc(), threads(&["t"]), span()),
        build_discard_locals(CARRIER, span()),
    ];
    assert_eq!(render_str(&ir), "let _CF3 = call 'm':'construct'(T) in ");
}

// ── MethodBody ───────────────────────────────────────────────────────────

#[test]
fn method_body_non_member_is_a_plain_let() {
    let enclosing = frame_of(&build_method_body(FrameId::ROOT, Vec::new(), Vec::new()));
    let prelude = build_local_threading_prelude(
        &enclosing,
        CARRIER,
        tuple_doc(),
        &[key_slot("t", "_T1")],
        Vec::new(),
        span(),
        mint(FrameId::ROOT, "__local__t"),
    );
    assert!(matches!(
        prelude[1],
        ThreadedStmt::LocalRebind {
            lowering: RebindLowering::Let,
            ..
        }
    ));
    let ir = vec![build_method_body(FrameId::ROOT, Vec::new(), prelude)];
    assert_eq!(
        render_str(&ir),
        "let _CF3 = call 'm':'construct'(T) in \
         let _T1 = call 'maps':'get'('__local__t', call 'erlang':'element'(2, _CF3)) in "
    );
    assert_eq!(verify(&ir), Vec::new());
}

#[test]
fn repl_method_body_member_puts_into_the_bindings_map() {
    // REPL: the root frame threads the bindings map's keys, and its map key
    // is the bare binding name.
    let enclosing = frame_of(&build_method_body(
        FrameId::ROOT,
        threads(&["t"]),
        Vec::new(),
    ));
    let slot = ThreadedLocalSlot {
        local: "t".to_string(),
        slot: CarrierSlot::Key("t".to_string()),
        value_var: "_T1".to_string(),
    };
    let prelude = build_local_threading_prelude(
        &enclosing,
        CARRIER,
        tuple_doc(),
        &[slot],
        Vec::new(),
        span(),
        mint(FrameId::ROOT, "t"),
    );
    let ir = vec![build_method_body(FrameId::ROOT, threads(&["t"]), prelude)];
    assert_eq!(
        render_str(&ir),
        "let _CF3 = call 'm':'construct'(T) in \
         let _T1 = call 'maps':'get'('t', call 'erlang':'element'(2, _CF3)) in \
         let State1 = call 'maps':'put'('t', _T1, State) in "
    );
    assert_eq!(verify(&ir), Vec::new());
}

// ── StateAcc loop / handler body ─────────────────────────────────────────

fn state_acc_node(frame: FrameId, body: Vec<ThreadedStmt>) -> ThreadedStmt {
    ThreadedStmt::Threaded {
        mode: ThreadingMode::StateAcc(StateAccFallbackReason::None),
        frame,
        threads: threads(&["t"]),
        body,
        produces: Vec::new(),
        span: span(),
    }
}

#[test]
fn state_acc_member_puts_its_local_key_and_non_member_is_a_plain_let() {
    let frame = FrameId::new(1);
    let enclosing = frame_of(&state_acc_node(frame, Vec::new()));
    let prelude = build_local_threading_prelude(
        &enclosing,
        CARRIER,
        tuple_doc(),
        &[key_slot("t", "_T1"), key_slot("tmp", "_Tmp2")],
        Vec::new(),
        span(),
        mint(frame, "__local__t"),
    );
    let ir = vec![state_acc_node(frame, prelude)];
    assert_eq!(
        render_str(&ir),
        "let _CF3 = call 'm':'construct'(T) in \
         let _T1 = call 'maps':'get'('__local__t', call 'erlang':'element'(2, _CF3)) in \
         let State1 = call 'maps':'put'('__local__t', _T1, State) in \
         let _Tmp2 = call 'maps':'get'('__local__tmp', call 'erlang':'element'(2, _CF3)) in ",
        "a member is put into the frame's map; a non-member (a temp the frame \
         does not thread) must not leave a stray `__local__` key behind"
    );
    assert_eq!(verify(&ir), Vec::new());
}

// ── DirectParams / Hybrid loops ──────────────────────────────────────────

fn while_loop(mode: ThreadingMode, frame: FrameId, body: Vec<ThreadedStmt>) -> ThreadedStmt {
    ThreadedStmt::ConditionalLoop {
        fn_name: "while".to_string(),
        mode,
        frame,
        threads: threads(&["t"]),
        counter: None,
        condition: Vec::new(),
        condition_value: ValueRef::Literal("'true'"),
        continue_arm: docvec!["<'true'> when 'true' -> "],
        body,
        produces: vec![local("t", 0, frame)],
        outer_args: None,
        exit_arm: docvec!["<'false'> when 'true' -> 'nil' end "],
        span: span(),
    }
}

fn flat_slot(local: &str, pos: usize, value_var: &str) -> ThreadedLocalSlot {
    ThreadedLocalSlot {
        local: local.to_string(),
        slot: CarrierSlot::Pos(pos),
        value_var: value_var.to_string(),
    }
}

#[test]
fn direct_params_member_joins_the_loop_argument_chain() {
    for mode in [ThreadingMode::DirectParams, ThreadingMode::Hybrid] {
        let frame = FrameId::new(1);
        let enclosing = frame_of(&while_loop(mode.clone(), frame, Vec::new()));
        let prelude = build_local_threading_prelude(
            &enclosing,
            CARRIER,
            tuple_doc(),
            &[flat_slot("t", 2, "_T7")],
            Vec::new(),
            span(),
            mint(frame, "__local__t"),
        );
        assert!(matches!(
            prelude[1],
            ThreadedStmt::LocalRebind {
                lowering: RebindLowering::LoopParam { .. },
                ..
            }
        ));
        let ir = vec![while_loop(mode.clone(), frame, prelude)];
        let rendered = render_str(&ir);
        assert!(
            rendered.contains(
                "let _CF3 = call 'm':'construct'(T) in  \
                 let _T7 = call 'erlang':'element'(2, _CF3) in "
            ),
            "{mode:?}: the rebind is a Gensym Bind reading the flat slot: {rendered}"
        );
        assert!(
            rendered.contains("apply 'while'/1 (_T7)"),
            "{mode:?}: the recursive call must carry the rebind's identity \
             (final_loop_arg_identities): {rendered}"
        );
        assert_eq!(verify(&ir), Vec::new(), "{mode:?}");
    }
}

#[test]
fn direct_params_non_member_is_a_plain_let_outside_the_loop_chain() {
    let frame = FrameId::new(1);
    let enclosing = frame_of(&while_loop(ThreadingMode::DirectParams, frame, Vec::new()));
    let prelude = build_local_threading_prelude(
        &enclosing,
        CARRIER,
        tuple_doc(),
        &[flat_slot("tmp", 2, "_Tmp7")],
        Vec::new(),
        span(),
        mint(frame, "__local__tmp"),
    );
    let ir = vec![while_loop(ThreadingMode::DirectParams, frame, prelude)];
    let rendered = render_str(&ir);
    assert!(
        rendered.contains("let _Tmp7 = call 'erlang':'element'(2, _CF3) in "),
        "{rendered}"
    );
    assert!(
        rendered.contains("apply 'while'/1 (T)"),
        "a non-member must not be forced into the loop's parameter list: {rendered}"
    );
}

// ── TupleAcc folds ───────────────────────────────────────────────────────

#[test]
fn tuple_acc_member_is_a_loop_param_and_non_member_a_plain_let() {
    let frame = FrameId::new(1);
    let fold = |body| ThreadedStmt::Threaded {
        mode: ThreadingMode::TupleAcc(0),
        frame,
        threads: threads(&["t"]),
        body,
        produces: Vec::new(),
        span: span(),
    };
    let enclosing = frame_of(&fold(Vec::new()));
    let prelude = build_local_threading_prelude(
        &enclosing,
        CARRIER,
        tuple_doc(),
        &[flat_slot("t", 2, "_T4"), flat_slot("tmp", 3, "_Tmp5")],
        Vec::new(),
        span(),
        mint(frame, "__local__t"),
    );
    assert!(matches!(
        (&prelude[1], &prelude[2]),
        (
            ThreadedStmt::LocalRebind {
                lowering: RebindLowering::LoopParam { .. },
                ..
            },
            ThreadedStmt::LocalRebind {
                lowering: RebindLowering::Let,
                ..
            },
        )
    ));
    let ir = vec![fold(prelude)];
    assert_eq!(
        render_str(&ir),
        "let _CF3 = call 'm':'construct'(T) in \
         let _T4 = call 'erlang':'element'(2, _CF3) in \
         let _Tmp5 = call 'erlang':'element'(3, _CF3) in "
    );
    assert_eq!(verify(&ir), Vec::new());
}

// ── BranchArm ────────────────────────────────────────────────────────────

#[test]
fn branch_arm_member_puts_into_the_seeded_state_acc_and_non_member_is_a_let() {
    let frame = FrameId::new(3);
    let enclosing = frame_of(&build_branch_arm(frame, threads(&["t"]), Vec::new()));
    let prelude = build_local_threading_prelude(
        &enclosing,
        CARRIER,
        tuple_doc(),
        &[key_slot("t", "_T1"), key_slot("tmp", "_Tmp2")],
        Vec::new(),
        span(),
        mint(frame, "__local__t"),
    );
    let ir = vec![build_branch_arm(frame, threads(&["t"]), prelude)];
    assert_eq!(
        render_str(&ir),
        "let _CF3 = call 'm':'construct'(T) in \
         let _T1 = call 'maps':'get'('__local__t', call 'erlang':'element'(2, _CF3)) in \
         let State1 = call 'maps':'put'('__local__t', _T1, State) in \
         let _Tmp2 = call 'maps':'get'('__local__tmp', call 'erlang':'element'(2, _CF3)) in "
    );
    assert_eq!(verify(&ir), Vec::new());
}

// ── Actor instance: State family first ───────────────────────────────────

#[test]
fn actor_state_family_bind_renders_first_and_rebinds_read_the_new_state() {
    // ADR 0131 §2: in an actor instance frame `element(2, CF)` is also the
    // `State` family's next version. The family `Bind` (`extract_family_slots`)
    // comes first, and each rebind reads from that new `State` version.
    let frame = FrameId::ROOT;
    let enclosing = frame_of(&build_method_body(frame, Vec::new(), Vec::new()));
    let families = ThreadedFamilies::from_matches(&[VersionPrefix::State]);
    // `{Value, StateAcc}`: one element before the family slot.
    let family_binds = extract_family_slots(
        CARRIER,
        1,
        &families,
        |_| FamilyVersionStep::new(state(0, frame), state(1, frame)),
        span(),
    );
    let prelude = build_local_threading_prelude(
        &enclosing,
        CARRIER,
        tuple_doc(),
        &[key_slot("t", "_T1")],
        family_binds,
        span(),
        mint(frame, "__local__t"),
    );
    assert!(matches!(prelude[1], ThreadedStmt::Bind { .. }));
    assert!(matches!(
        &prelude[2],
        ThreadedStmt::LocalRebind { state_first: Some(v), .. } if *v == state(1, frame)
    ));
    let ir = vec![build_method_body(frame, Vec::new(), prelude)];
    assert_eq!(
        render_str(&ir),
        "let _CF3 = call 'm':'construct'(T) in \
         let State1 = call 'erlang':'element'(2, _CF3) in \
         let _T1 = call 'maps':'get'('__local__t', State1) in "
    );
    assert_eq!(verify(&ir), Vec::new());
}

// ── verify plumbing ──────────────────────────────────────────────────────

#[test]
fn closing_a_rebind_prelude_opaquely_reports_the_dropped_local() {
    // ADR 0131 §4: a `LocalRebind` left in a prelude that is closed in an
    // `Opaque` context (a Tier 1 closure body, an FFI argument) is a dropped
    // local write — `StateEffectEscapesExpression`, never silence.
    let enclosing = frame_of(&build_method_body(FrameId::ROOT, Vec::new(), Vec::new()));
    let prelude = build_local_threading_prelude(
        &enclosing,
        CARRIER,
        tuple_doc(),
        &[key_slot("t", "_T1")],
        Vec::new(),
        span(),
        mint(FrameId::ROOT, "__local__t"),
    );
    let tv = ThreadedValue {
        prelude,
        value: ValueRef::Doc(docvec!["call 'erlang':'element'(1, _CF3)"]),
    };
    let mut generator = CoreErlangGenerator::new("local_rebind_close_opaque");
    let mut ctx = RenderCtx::new(&mut generator);
    let (_, errors) = tv.clone().close(&mut ctx, CloseContext::Opaque);
    assert_eq!(
        errors,
        vec![VerifyError::StateEffectEscapesExpression {
            prefix: VersionPrefix::Local("t".to_string()),
            at: span(),
        }]
    );
    let (_, errors) = tv.close(&mut ctx, CloseContext::ThreadsState);
    assert_eq!(errors, Vec::new());
}

#[test]
fn a_map_put_rebind_is_a_version_step_later_binds_can_source() {
    // A `MapPut` rebind's `State` step is counted exactly like a `Bind`'s,
    // so a later `Bind` sourcing it is bound and a second producer of the
    // same version is non-linear.
    let frame = FrameId::new(3);
    let enclosing = frame_of(&build_branch_arm(frame, threads(&["t"]), Vec::new()));
    let mut body = build_local_threading_prelude(
        &enclosing,
        CARRIER,
        tuple_doc(),
        &[key_slot("t", "_T1")],
        Vec::new(),
        span(),
        mint(frame, "__local__t"),
    );
    body.push(ThreadedStmt::Bind {
        target: state(2, frame),
        source: state(1, frame),
        op: BindOp::Direct(ValueRef::Literal("'_'")),
        span: span(),
    });
    let clean = vec![build_branch_arm(frame, threads(&["t"]), body.clone())];
    assert_eq!(verify(&clean), Vec::new());

    body.push(ThreadedStmt::Bind {
        target: state(1, frame),
        source: state(0, frame),
        op: BindOp::Direct(ValueRef::Literal("'_'")),
        span: span(),
    });
    let errors = verify(&[build_branch_arm(frame, threads(&["t"]), body)]);
    assert!(
        errors.iter().any(|e| matches!(
            e,
            VerifyError::NonLinearVersion { var, producers: 2, .. } if *var == state(1, frame)
        )),
        "{errors:?}"
    );
}

// ── ThreadedLocalDropped (ADR 0131 §4) ───────────────────────────────────

/// The consumer's read of the construct's value: `element(1, CF)`, opaque.
fn read_value() -> ThreadedStmt {
    ThreadedStmt::Statement(
        docvec!["let R = call 'erlang':'element'(1, _CF3) in "],
        span(),
    )
}

fn dropped(local: &str) -> VerifyError {
    VerifyError::ThreadedLocalDropped {
        local: local.to_string(),
        carrier: CARRIER.to_string(),
        at: span(),
    }
}

#[test]
fn o4_construct_read_without_its_rebind_drops_the_local() {
    // o4 pre-fix: `r := (#(1, 2) collect: [:x | t := t + x]) size` — the
    // fold's tuple is bound and its value read, but `t` is never rebound.
    let tuple = build_construct_tuple(CARRIER, tuple_doc(), threads(&["t"]), span());
    let read_first = vec![build_method_body(
        FrameId::ROOT,
        Vec::new(),
        vec![tuple.clone(), read_value()],
    )];
    assert_eq!(verify(&read_first), vec![dropped("t")]);
    let frame_ends = vec![build_method_body(FrameId::ROOT, Vec::new(), vec![tuple])];
    assert_eq!(verify(&frame_ends), vec![dropped("t")]);
}

#[test]
fn o4_construct_with_its_rebind_before_the_read_passes() {
    let enclosing = frame_of(&build_method_body(FrameId::ROOT, Vec::new(), Vec::new()));
    let mut body = build_local_threading_prelude(
        &enclosing,
        CARRIER,
        tuple_doc(),
        &[key_slot("t", "_T1")],
        Vec::new(),
        span(),
        mint(FrameId::ROOT, "__local__t"),
    );
    body.push(read_value());
    let ir = vec![build_method_body(FrameId::ROOT, Vec::new(), body)];
    assert_eq!(verify(&ir), Vec::new());
}

#[test]
fn a_rebind_after_an_intervening_statement_is_too_late() {
    let enclosing = frame_of(&build_method_body(FrameId::ROOT, Vec::new(), Vec::new()));
    let mut prelude = build_local_threading_prelude(
        &enclosing,
        CARRIER,
        tuple_doc(),
        &[key_slot("t", "_T1")],
        Vec::new(),
        span(),
        mint(FrameId::ROOT, "__local__t"),
    );
    prelude.insert(1, read_value());
    let errors = verify(&[build_method_body(FrameId::ROOT, Vec::new(), prelude)]);
    // The construct closes at the read (dropping `t`), and the late rebind
    // has no open construct to belong to.
    assert_eq!(errors, vec![dropped("t"), dropped("t")]);
}

#[test]
fn family_binds_may_read_the_carrier_ahead_of_the_rebinds() {
    // Covered end to end by `actor_state_family_bind_renders_first_…`; here
    // the `State` family `Bind` sits between the tuple and the rebind.
    let frame = FrameId::ROOT;
    let enclosing = frame_of(&build_method_body(frame, Vec::new(), Vec::new()));
    let families = ThreadedFamilies::from_matches(&[VersionPrefix::State]);
    let family_binds = extract_family_slots(
        CARRIER,
        1,
        &families,
        |_| FamilyVersionStep::new(state(0, frame), state(1, frame)),
        span(),
    );
    let mut body = build_local_threading_prelude(
        &enclosing,
        CARRIER,
        tuple_doc(),
        &[key_slot("t", "_T1")],
        family_binds,
        span(),
        mint(frame, "__local__t"),
    );
    body.push(read_value());
    assert_eq!(
        verify(&[build_method_body(frame, Vec::new(), body)]),
        Vec::new()
    );
}

#[test]
fn discard_locals_drops_legitimately_only_at_the_method_body() {
    let tuple = build_construct_tuple(CARRIER, tuple_doc(), threads(&["t"]), span());
    let discard = build_discard_locals(CARRIER, span());
    let at_root = vec![build_method_body(
        FrameId::ROOT,
        Vec::new(),
        vec![tuple.clone(), discard.clone(), read_value()],
    )];
    assert_eq!(verify(&at_root), Vec::new());
    // The implicit root of an un-wrapped slice is a non-REPL method body.
    assert_eq!(
        verify(&[tuple.clone(), discard.clone(), read_value()]),
        Vec::new()
    );

    // ADR 0131 §2: a construct in last position of a loop body (or branch
    // arm) must still rebind — its locals are that frame's threaded output.
    let frame = FrameId::new(1);
    let in_loop = vec![state_acc_node(frame, vec![tuple.clone(), discard.clone()])];
    assert_eq!(verify(&in_loop), vec![dropped("t")]);
    let in_arm = vec![build_branch_arm(
        frame,
        threads(&["t"]),
        vec![tuple, discard],
    )];
    assert_eq!(verify(&in_arm), vec![dropped("t")]);
}

#[test]
fn a_rebind_its_construct_does_not_thread_is_dropped_at_every_level() {
    // ADR 0131 §1 transitive-closure gap: inside a branch arm (one nesting
    // level down), the nested construct's `threads` omits `t` but the frame
    // rebinds it from that carrier — the construct never carried `t` out.
    let frame = FrameId::new(3);
    let enclosing = frame_of(&build_branch_arm(frame, threads(&["t"]), Vec::new()));
    let mut body = build_local_threading_prelude(
        &enclosing,
        CARRIER,
        tuple_doc(),
        &[key_slot("t", "_T1")],
        Vec::new(),
        span(),
        mint(frame, "__local__t"),
    );
    body[0] = build_construct_tuple(CARRIER, tuple_doc(), Vec::new(), span());
    let ir = vec![build_method_body(
        FrameId::ROOT,
        Vec::new(),
        vec![build_branch_arm(frame, threads(&["t"]), body)],
    )];
    assert_eq!(verify(&ir), vec![dropped("t")]);
}

// ── LocalRebindModeMismatch / LocalRebindFrameMismatch (ADR 0131 §4) ─────

/// A prelude for `slot` lowered against `enclosing`, but with the producer
/// answering `wrong` instead of the frame's shape.
fn mislowered(
    enclosing: &RebindFrame,
    slot: ThreadedLocalSlot,
    wrong: RebindLowering,
) -> Vec<ThreadedStmt> {
    build_local_threading_prelude(
        enclosing,
        CARRIER,
        tuple_doc(),
        &[slot],
        Vec::new(),
        span(),
        move |_, _| wrong.clone(),
    )
}

fn mode_mismatch(
    frame: FrameId,
    mode: RebindFrameKind,
    member: bool,
    lowered_as: RebindShape,
) -> VerifyError {
    VerifyError::LocalRebindModeMismatch {
        frame,
        mode,
        member,
        lowered_as,
        at: span(),
    }
}

#[test]
fn a_plain_let_for_a_member_of_a_state_acc_frame_is_a_mode_mismatch() {
    let frame = FrameId::new(1);
    let enclosing = frame_of(&state_acc_node(frame, Vec::new()));
    let ir = vec![state_acc_node(
        frame,
        mislowered(&enclosing, key_slot("t", "_T1"), RebindLowering::Let),
    )];
    assert_eq!(
        verify(&ir),
        vec![mode_mismatch(
            frame,
            RebindFrameKind::Loop(ThreadingMode::StateAcc(StateAccFallbackReason::None)),
            true,
            RebindShape::Let,
        )]
    );
}

#[test]
fn a_put_for_a_non_member_is_a_mode_mismatch() {
    // A temp the frame does not thread must not leave a stray `__local__`
    // key in the frame's map (BT-2717 class).
    let frame = FrameId::new(3);
    let enclosing = frame_of(&build_branch_arm(frame, threads(&["t"]), Vec::new()));
    let put = RebindLowering::MapPut {
        key: "__local__tmp".to_string(),
        source: state(0, frame),
        target: state(1, frame),
    };
    let ir = vec![build_branch_arm(
        frame,
        threads(&["t"]),
        mislowered(&enclosing, key_slot("tmp", "_Tmp2"), put),
    )];
    assert_eq!(
        verify(&ir),
        vec![mode_mismatch(
            frame,
            RebindFrameKind::BranchArm,
            false,
            RebindShape::MapPut
        )]
    );
}

#[test]
fn a_gensym_chain_entry_for_a_non_param_is_a_mode_mismatch() {
    let frame = FrameId::new(1);
    let enclosing = frame_of(&while_loop(ThreadingMode::DirectParams, frame, Vec::new()));
    let chain = RebindLowering::LoopParam {
        source: local("tmp", 0, frame),
        target: gensym("_Tmp7", frame),
    };
    let ir = vec![while_loop(
        ThreadingMode::DirectParams,
        frame,
        mislowered(&enclosing, flat_slot("tmp", 2, "_Tmp7"), chain),
    )];
    assert_eq!(
        verify(&ir),
        vec![mode_mismatch(
            frame,
            RebindFrameKind::Loop(ThreadingMode::DirectParams),
            false,
            RebindShape::LoopParam
        )]
    );
}

#[test]
fn a_rebind_spliced_into_another_frame_is_a_frame_mismatch() {
    // Lowered against a `Let`-shaped method root, then spliced into a
    // `StateAcc` loop body that threads `t`: both the frame and the shape
    // are wrong for where it now sits.
    let loop_frame = FrameId::new(5);
    let enclosing = frame_of(&build_method_body(FrameId::ROOT, Vec::new(), Vec::new()));
    let prelude = build_local_threading_prelude(
        &enclosing,
        CARRIER,
        tuple_doc(),
        &[key_slot("t", "_T1")],
        Vec::new(),
        span(),
        mint(FrameId::ROOT, "__local__t"),
    );
    let errors = verify(&[state_acc_node(loop_frame, prelude)]);
    assert_eq!(
        errors,
        vec![
            VerifyError::LocalRebindFrameMismatch {
                local: "t".to_string(),
                recorded: FrameId::ROOT,
                enclosing: loop_frame,
                at: span(),
            },
            mode_mismatch(
                loop_frame,
                RebindFrameKind::Loop(ThreadingMode::StateAcc(StateAccFallbackReason::None)),
                true,
                RebindShape::Let,
            ),
        ]
    );
}

#[test]
fn every_table_cell_lowered_by_its_frame_verifies_clean() {
    // The positive half: for every frame kind × membership, a rebind whose
    // lowering is the frame's own shape passes both rebind checks.
    let frame = FrameId::new(1);
    let nodes = [
        build_method_body(frame, Vec::new(), Vec::new()),
        build_method_body(frame, threads(&["t"]), Vec::new()),
        build_branch_arm(frame, threads(&["t"]), Vec::new()),
        state_acc_node(frame, Vec::new()),
        while_loop(ThreadingMode::DirectParams, frame, Vec::new()),
        while_loop(ThreadingMode::Hybrid, frame, Vec::new()),
    ];
    for node in nodes {
        for name in ["t", "tmp"] {
            let enclosing = frame_of(&node);
            if enclosing.kind == RebindFrameKind::MethodBody && enclosing.threads.is_empty() {
                // Non-REPL root: only the `Let` column exists.
                if name == "t" {
                    continue;
                }
            }
            let prelude = build_local_threading_prelude(
                &enclosing,
                CARRIER,
                tuple_doc(),
                &[flat_slot(name, 2, "_V9")],
                Vec::new(),
                span(),
                mint(frame, "__local__t"),
            );
            let mut wrapped = node.clone();
            match &mut wrapped {
                ThreadedStmt::MethodBody { body, .. }
                | ThreadedStmt::BranchArm { body, .. }
                | ThreadedStmt::Threaded { body, .. }
                | ThreadedStmt::ConditionalLoop { body, .. } => *body = prelude,
                _ => unreachable!(),
            }
            let errors = verify(std::slice::from_ref(&wrapped));
            assert!(
                !errors.iter().any(|e| matches!(
                    e,
                    VerifyError::LocalRebindModeMismatch { .. }
                        | VerifyError::LocalRebindFrameMismatch { .. }
                        | VerifyError::ThreadedLocalDropped { .. }
                )),
                "{:?} × {name}: {errors:?}",
                enclosing.kind
            );
        }
    }
}

// ── LocalReadAfterSiblingRebind (ADR 0131 §1a) ───────────────────────────

/// The §1a shape `t + ([t := t + 1. 1] on: Error do: [:e | 0])`: the right
/// operand's prelude rebinds `t`.
fn on_do_rebinding_t() -> Vec<ThreadedStmt> {
    let enclosing = frame_of(&build_method_body(FrameId::ROOT, Vec::new(), Vec::new()));
    build_local_threading_prelude(
        &enclosing,
        CARRIER,
        tuple_doc(),
        &[key_slot("t", "_T1")],
        Vec::new(),
        span(),
        mint(FrameId::ROOT, "__local__t"),
    )
}

#[test]
fn a_left_operand_read_in_place_after_the_right_operands_rebind_fails() {
    let right = on_do_rebinding_t();
    let left_span = Span::new(0, 1);
    let pre_fix = [
        SequencedSibling {
            reads_in_place: Some("t"),
            prelude: &[],
            span: left_span,
        },
        SequencedSibling {
            reads_in_place: None,
            prelude: &right,
            span: Span::new(4, 40),
        },
    ];
    assert_eq!(
        verify_sibling_reads(&pre_fix),
        vec![VerifyError::LocalReadAfterSiblingRebind {
            local: "t".to_string(),
            at: left_span,
        }]
    );
    // §1a fix: the left operand is snapshot to a `Tmp` before the right
    // operand's prelude, so it no longer reads `t` in place.
    let snapshot = [tmp_snapshot()];
    let fixed = [
        SequencedSibling {
            reads_in_place: None,
            prelude: &snapshot,
            span: left_span,
        },
        pre_fix[1],
    ];
    assert_eq!(verify_sibling_reads(&fixed), Vec::new());
}

fn tmp_snapshot() -> ThreadedStmt {
    ThreadedStmt::Statement(docvec!["let _Tmp1 = T in "], span())
}

#[test]
fn sibling_reads_ignore_earlier_rebinds_other_locals_and_nested_frames() {
    let right = on_do_rebinding_t();
    let nested = vec![build_branch_arm(
        FrameId::new(2),
        threads(&["t"]),
        right.clone(),
    )];
    let cases: [(&str, Vec<ThreadedStmt>, Vec<ThreadedStmt>); 3] = [
        // The rebind runs *before* the read: source order already agrees.
        ("t", right.clone(), Vec::new()),
        // A later sibling rebinds a different local.
        ("u", Vec::new(), right),
        // The later rebind belongs to an inner frame, not this binding.
        ("t", Vec::new(), nested),
    ];
    for (local, first, second) in cases {
        let siblings = [
            SequencedSibling {
                reads_in_place: None,
                prelude: &first,
                span: span(),
            },
            SequencedSibling {
                reads_in_place: Some(local),
                prelude: &[],
                span: span(),
            },
            SequencedSibling {
                reads_in_place: None,
                prelude: &second,
                span: span(),
            },
        ];
        assert_eq!(verify_sibling_reads(&siblings), Vec::new(), "{local}");
    }
}
