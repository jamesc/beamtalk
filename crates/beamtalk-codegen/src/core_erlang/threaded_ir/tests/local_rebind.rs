// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0131 Phase 1b: `ConstructTuple`, `LocalRebind`, `DiscardLocals`,
//! `MethodBody` and `BranchArm` — hand-built IR for every row of the §2
//! table (frame mode × membership), asserting the rendered `Document` text.
//! No production code builds these nodes yet (Phase 2/3); the verifier's own
//! checks over them are Phase 1c.

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
/// enclosing node exactly as a producer's frame stack would.
fn frame_of(node: &ThreadedStmt, loop_threads: &[&str]) -> RebindFrame {
    let loop_threads: Vec<String> = loop_threads.iter().map(ToString::to_string).collect();
    RebindFrame::of(node, &loop_threads).expect("a frame node")
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
        frame_of(&method, &[]),
        RebindFrame {
            frame: FrameId::ROOT,
            kind: RebindFrameKind::MethodBody,
            threads: threads(&["t"]),
        }
    );
    let arm = build_branch_arm(FrameId::new(4), threads(&["u"]), Vec::new());
    assert_eq!(frame_of(&arm, &[]).kind, RebindFrameKind::BranchArm);
    assert_eq!(frame_of(&arm, &["ignored"]).threads, threads(&["u"]));
    let fold = ThreadedStmt::Threaded {
        mode: ThreadingMode::TupleAcc(0),
        frame: FrameId::new(2),
        body: Vec::new(),
        produces: Vec::new(),
        span: span(),
    };
    assert_eq!(
        frame_of(&fold, &["t"]),
        RebindFrame {
            frame: FrameId::new(2),
            kind: RebindFrameKind::Loop(ThreadingMode::TupleAcc(0)),
            threads: threads(&["t"]),
        }
    );
    assert!(RebindFrame::of(&ThreadedStmt::Statement(Document::Nil, span()), &[]).is_none());
}

#[test]
fn build_local_rebind_asks_for_the_frames_shape_and_records_the_frame() {
    let frame = FrameId::new(5);
    let enclosing = frame_of(&build_branch_arm(frame, threads(&["t"]), Vec::new()), &[]);
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
    let enclosing = frame_of(
        &build_method_body(FrameId::ROOT, Vec::new(), Vec::new()),
        &[],
    );
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
    let enclosing = frame_of(
        &build_method_body(FrameId::ROOT, threads(&["t"]), Vec::new()),
        &[],
    );
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
        body,
        produces: Vec::new(),
        span: span(),
    }
}

#[test]
fn state_acc_member_puts_its_local_key_and_non_member_is_a_plain_let() {
    let frame = FrameId::new(1);
    let enclosing = frame_of(&state_acc_node(frame, Vec::new()), &["t"]);
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
        let enclosing = frame_of(&while_loop(mode.clone(), frame, Vec::new()), &["t"]);
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
    let enclosing = frame_of(
        &while_loop(ThreadingMode::DirectParams, frame, Vec::new()),
        &["t"],
    );
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
        body,
        produces: Vec::new(),
        span: span(),
    };
    let enclosing = frame_of(&fold(Vec::new()), &["t"]);
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
    let enclosing = frame_of(&build_branch_arm(frame, threads(&["t"]), Vec::new()), &[]);
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
    let enclosing = frame_of(&build_method_body(frame, Vec::new(), Vec::new()), &[]);
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
fn a_map_put_rebind_is_a_version_step_later_binds_can_source() {
    // Phase 1c adds the rebind-specific checks; until then a `MapPut`
    // rebind's `State` step is counted exactly like a `Bind`'s, so a later
    // `Bind` sourcing it is bound and a second producer of the same version
    // is non-linear.
    let frame = FrameId::new(3);
    let enclosing = frame_of(&build_branch_arm(frame, threads(&["t"]), Vec::new()), &[]);
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
