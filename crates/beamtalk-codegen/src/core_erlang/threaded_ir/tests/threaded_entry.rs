// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0132 Phase 1a (BT-3869): the `Acc` storage family, `ScopeKind::Threaded`,
//! the threaded `MethodBody` builder, and the four verifier checks that must
//! fail on hand-built IR before any threaded body lowering exists (ADR 0111,
//! CLAUDE.md § State-threading codegen).
//!
//! Each check's tests build the IR shape ADR 0132 §2/§3 names as the defect
//! (the pending fixtures are compile errors today and have no IR of their
//! own, so the hand-built IR is the repro), assert the check reports it, and
//! assert the well-formed neighbour verifies clean.

use super::*;
use crate::core_erlang::CodeGenContext;
use crate::core_erlang::control_flow::analysis::ThreadedFamilies;
use crate::core_erlang::threaded_ir::verify::verify_errors_to_diagnostic;

fn acc(version: usize, frame: FrameId) -> VersionedVar {
    VersionedVar::new(VersionPrefix::Acc, version, frame)
}

fn state(version: usize, frame: FrameId) -> VersionedVar {
    VersionedVar::new(VersionPrefix::State, version, frame)
}

fn bind(target: VersionedVar, source: VersionedVar, op: BindOp) -> ThreadedStmt {
    ThreadedStmt::Bind {
        target,
        source,
        op,
        span: span(),
    }
}

fn direct_doc() -> BindOp {
    BindOp::Direct(ValueRef::Doc(Document::Str("<call_threaded>")))
}

/// A `StateAcc` fold frame (`do:`'s accumulator) holding `body`.
fn state_acc_frame(frame: FrameId, body: Vec<ThreadedStmt>) -> ThreadedStmt {
    ThreadedStmt::Threaded {
        mode: ThreadingMode::StateAcc(StateAccFallbackReason::SelfSendInBody),
        frame,
        threads: vec!["t".to_string()],
        body,
        produces: Vec::new(),
        span: span(),
    }
}

fn nlr_throw(slot: Vec<VersionedVar>) -> ThreadedStmt {
    ThreadedStmt::NlrThrow {
        token: TokenId::new("NlrTok"),
        value: ValueRef::Var("Val".to_string()),
        slot,
        result_var: "_NlrThrow1".to_string(),
        span: span(),
    }
}

fn nlr_catch(slot: Vec<VersionPrefix>) -> ThreadedStmt {
    ThreadedStmt::NlrCatch {
        boundary: NlrBoundary::ValueType,
        token: TokenId::new("NlrTok"),
        frame: FrameId::ROOT,
        slot,
        span: span(),
    }
}

// ─── VersionPrefix::Acc: naming, counter, slot order ──────────────────────

#[test]
fn acc_versions_render_with_their_own_counter_including_version_zero() {
    assert_eq!(acc(0, FrameId::ROOT).render_name(), "Acc@0");
    assert_eq!(acc(3, FrameId::ROOT).render_name(), "Acc@3");
    let mut counter = VersionCounter::new();
    assert_eq!(counter.current_var(VersionPrefix::Acc), "Acc@0");
    assert_eq!(counter.next_var(VersionPrefix::Acc), "Acc@1");
    assert_eq!(counter.next_var(VersionPrefix::Acc), "Acc@2");
}

#[test]
fn acc_renders_the_same_in_and_out_of_a_loop_body() {
    // Like `SelfVt`, and unlike `State` (`StateN` vs `StateAccN`), `Acc` is
    // context-independent.
    let frame = FrameId::ROOT;
    let ir = vec![bind(acc(1, frame), acc(0, frame), direct_doc())];
    let mut generator = CoreErlangGenerator::new("bt@acc_render");
    let plain = render(&ir, &mut RenderCtx::new(&mut generator)).to_pretty_string();
    generator.in_loop_body = true;
    let in_loop = render(&ir, &mut RenderCtx::new(&mut generator)).to_pretty_string();
    assert_eq!(plain, "let Acc@1 = <call_threaded> in ");
    assert_eq!(plain, in_loop);
}

#[test]
fn acc_is_ordered_last_after_state_and_self_vt() {
    assert_eq!(
        FAMILY_SLOT_ORDER,
        [
            VersionPrefix::State,
            VersionPrefix::SelfVt,
            VersionPrefix::Acc
        ]
    );
    assert_eq!(
        canonical_families(&[
            VersionPrefix::Acc,
            VersionPrefix::SelfVt,
            VersionPrefix::State
        ]),
        vec![
            VersionPrefix::State,
            VersionPrefix::SelfVt,
            VersionPrefix::Acc
        ]
    );
    assert_eq!(
        ThreadedFamilies::from_matches(&[VersionPrefix::Acc, VersionPrefix::State]).as_slice(),
        &[VersionPrefix::State, VersionPrefix::Acc]
    );
}

// ─── eligible_families / threaded_scope ────────────────────────────────────

fn generator_for(
    context: CodeGenContext,
    class_method: bool,
    threaded: bool,
) -> CoreErlangGenerator {
    let mut generator = CoreErlangGenerator::new("bt@threaded_entry_scope");
    generator.context = context;
    generator.set_in_class_method(class_method);
    generator.in_threaded_body = threaded;
    generator
}

#[test]
fn eligible_families_adds_acc_last_only_inside_a_threaded_body() {
    let cases = [
        // ADR 0132 §2: `[Acc]` for a class body.
        (CodeGenContext::Actor, true, vec![VersionPrefix::Acc]),
        (CodeGenContext::ValueType, true, vec![VersionPrefix::Acc]),
        // `[SelfVt, Acc]` for a value-type body (`SelfVt` threads when it
        // writes a field, decided per body by `body_threaded_families`).
        (
            CodeGenContext::ValueType,
            false,
            vec![VersionPrefix::SelfVt, VersionPrefix::Acc],
        ),
        // `[State, Acc]` for an actor body.
        (
            CodeGenContext::Actor,
            false,
            vec![VersionPrefix::State, VersionPrefix::Acc],
        ),
    ];
    for (context, class_method, expected) in cases {
        let threaded = generator_for(context, class_method, true);
        assert_eq!(
            threaded.eligible_families(),
            expected,
            "{context:?} class_method={class_method}"
        );
        // Nothing changes for a method with no threading parameter.
        let plain = generator_for(context, class_method, false);
        assert!(
            !plain.eligible_families().contains(&VersionPrefix::Acc),
            "{context:?} class_method={class_method}"
        );
    }
}

#[test]
fn threaded_scope_is_threaded_only_inside_a_threaded_body() {
    let cases = [
        (
            CodeGenContext::Actor,
            true,
            ThreadedBodyKind::Class,
            ScopeKind::ClassMethod,
        ),
        (
            CodeGenContext::ValueType,
            false,
            ThreadedBodyKind::ValueType,
            ScopeKind::ValueType,
        ),
        (
            CodeGenContext::Actor,
            false,
            ThreadedBodyKind::Actor,
            ScopeKind::Instance,
        ),
    ];
    for (context, class_method, kind, plain_scope) in cases {
        assert_eq!(
            generator_for(context, class_method, true).threaded_scope(),
            ScopeKind::Threaded { kind }
        );
        assert_eq!(
            generator_for(context, class_method, false).threaded_scope(),
            plain_scope
        );
    }
    // The REPL compiles no threaded entry.
    assert_eq!(
        generator_for(CodeGenContext::Repl, false, true).threaded_body_kind(),
        None
    );
}

// ─── The threaded MethodBody builder ───────────────────────────────────────

#[test]
fn threaded_method_body_builder_puts_acc_last_and_threads_nothing() {
    for (families, expected) in [
        (vec![], vec![VersionPrefix::Acc]),
        (
            vec![VersionPrefix::Acc, VersionPrefix::SelfVt],
            vec![VersionPrefix::SelfVt, VersionPrefix::Acc],
        ),
        (
            vec![VersionPrefix::State],
            vec![VersionPrefix::State, VersionPrefix::Acc],
        ),
    ] {
        let node = build_threaded_method_body(FrameId::ROOT, &families, Vec::new(), span());
        assert_eq!(
            node,
            ThreadedStmt::MethodBody {
                kind: MethodBodyKind::Threaded,
                frame: FrameId::ROOT,
                threads: Vec::new(),
                families: expected,
                body: Vec::new(),
                span: span(),
            }
        );
        // Every rebind in it is the `Let` cell: `Acc` is never a rebind target.
        let frame = RebindFrame::of(&node).expect("a MethodBody is a frame");
        assert_eq!(frame.shape_for("t"), RebindShape::Let);
    }
}

#[test]
fn threaded_method_body_renders_its_body_straight_line_and_verifies_clean() {
    let frame = FrameId::ROOT;
    let body = vec![
        bind(acc(1, frame), acc(0, frame), direct_doc()),
        bind(acc(2, frame), acc(1, frame), direct_doc()),
    ];
    let node = build_threaded_method_body(frame, &[], body.clone(), span());
    assert_eq!(
        lower_and_render(std::slice::from_ref(&node)).to_pretty_string(),
        lower_and_render(&body).to_pretty_string()
    );
    for kind in [
        ThreadedBodyKind::Class,
        ThreadedBodyKind::ValueType,
        ThreadedBodyKind::Actor,
    ] {
        assert_eq!(
            verify_in_scope(std::slice::from_ref(&node), ScopeKind::Threaded { kind }),
            Vec::new(),
            "{kind:?}"
        );
    }
}

// ─── ActorStateInClassMethod: unchanged for State, silent for Acc ─────────

#[test]
fn actor_state_in_class_method_never_fires_for_acc() {
    let frame = FrameId::ROOT;
    let ir = vec![bind(acc(1, frame), acc(0, frame), direct_doc())];
    for scope in [
        ScopeKind::ClassMethod,
        ScopeKind::ValueType,
        ScopeKind::Instance,
        ScopeKind::Threaded {
            kind: ThreadedBodyKind::Class,
        },
        ScopeKind::Threaded {
            kind: ThreadedBodyKind::ValueType,
        },
        ScopeKind::Threaded {
            kind: ThreadedBodyKind::Actor,
        },
    ] {
        assert_eq!(verify_in_scope(&ir, scope), Vec::new(), "{scope:?}");
    }
}

#[test]
fn a_threaded_class_or_value_type_body_keeps_rejecting_the_state_family() {
    let frame = FrameId::ROOT;
    let ir = vec![build_threaded_method_body(
        frame,
        &[],
        vec![bind(state(1, frame), state(0, frame), direct_doc())],
        span(),
    )];
    let state_family = vec![VerifyError::ActorStateInClassMethod {
        defect: ClassMethodDefect::FamilyVersion(VersionPrefix::State),
        at: span(),
    }];
    for kind in [ThreadedBodyKind::Class, ThreadedBodyKind::ValueType] {
        assert_eq!(
            verify_in_scope(&ir, ScopeKind::Threaded { kind }),
            state_family,
            "{kind:?}"
        );
    }
    // An actor body threads `[State, Acc]`.
    assert_eq!(
        verify_in_scope(
            &ir,
            ScopeKind::Threaded {
                kind: ThreadedBodyKind::Actor
            }
        ),
        Vec::new()
    );
}

#[test]
fn a_threaded_class_body_rejects_self_vt_but_a_value_type_body_threads_it() {
    let frame = FrameId::ROOT;
    let ir = vec![bind(self_var(1, frame), self_var(0, frame), direct_doc())];
    assert_eq!(
        verify_in_scope(
            &ir,
            ScopeKind::Threaded {
                kind: ThreadedBodyKind::Class
            }
        ),
        vec![VerifyError::ActorStateInClassMethod {
            defect: ClassMethodDefect::FamilyVersion(VersionPrefix::SelfVt),
            at: span(),
        }]
    );
    assert_eq!(
        verify_in_scope(
            &ir,
            ScopeKind::Threaded {
                kind: ThreadedBodyKind::ValueType
            }
        ),
        Vec::new()
    );
}

// ─── AccAsScratchSeed (ADR 0132 §2, "never the scratch map") ──────────────

#[test]
fn acc_as_scratch_seed_rejects_a_state_acc_map_seeded_from_acc() {
    // ADR 0132 § Alternatives, "`Acc` as the body's scratch `StateAcc` map":
    // a fold's `StateAcc` map stepped from the caller's `Acc`.
    let frame = FrameId::new(1);
    let ir = vec![state_acc_frame(
        frame,
        vec![bind(
            state(1, frame),
            state(0, frame),
            BindOp::Direct(ValueRef::Version(acc(0, frame))),
        )],
    )];
    assert_eq!(
        verify(&ir),
        vec![VerifyError::AccAsScratchSeed {
            defect: AccSeedDefect::CrossFamilyStep {
                from: acc(0, frame),
                to: state(1, frame),
            },
            at: span(),
        }]
    );
}

#[test]
fn acc_as_scratch_seed_rejects_an_acc_chain_stepped_from_another_family() {
    let frame = FrameId::ROOT;
    let ir = vec![bind(acc(1, frame), state(0, frame), direct_doc())];
    assert_eq!(
        verify(&ir),
        vec![VerifyError::AccAsScratchSeed {
            defect: AccSeedDefect::CrossFamilyStep {
                from: state(0, frame),
                to: acc(1, frame),
            },
            at: span(),
        }]
    );
}

#[test]
fn acc_as_scratch_seed_rejects_a_put_into_or_unpack_out_of_acc() {
    let frame = FrameId::ROOT;
    let put = vec![bind(
        acc(1, frame),
        acc(0, frame),
        BindOp::Put {
            field: "count".to_string(),
            value: ValueRef::Literal("1"),
        },
    )];
    assert_eq!(
        verify(&put),
        vec![VerifyError::AccAsScratchSeed {
            defect: AccSeedDefect::PutInto {
                acc: acc(1, frame),
                key: "count".to_string(),
            },
            at: span(),
        }]
    );
    let unpack = vec![state_acc_frame(
        FrameId::new(1),
        vec![bind(
            local("count", 1, FrameId::new(1)),
            acc(0, FrameId::new(1)),
            BindOp::Unpack {
                field: "count".to_string(),
            },
        )],
    )];
    assert_eq!(
        verify(&unpack),
        vec![VerifyError::AccAsScratchSeed {
            defect: AccSeedDefect::UnpackFrom {
                acc: acc(0, FrameId::new(1)),
                key: "count".to_string(),
            },
            at: span(),
        }]
    );
}

#[test]
fn acc_as_scratch_seed_rejects_an_unpack_into_an_acc_target_from_another_family() {
    // The cross-family check runs for every op, not only `Direct`: an `Unpack`
    // reading a key of a scratch map into an `Acc` version seeds `Acc` from it.
    let frame = FrameId::new(1);
    let ir = vec![state_acc_frame(
        frame,
        vec![bind(
            acc(1, frame),
            state(0, frame),
            BindOp::Unpack {
                field: "__local__t".to_string(),
            },
        )],
    )];
    assert_eq!(
        verify(&ir),
        vec![VerifyError::AccAsScratchSeed {
            defect: AccSeedDefect::CrossFamilyStep {
                from: state(0, frame),
                to: acc(1, frame),
            },
            at: span(),
        }]
    );
}

#[test]
fn acc_as_scratch_seed_rejects_acc_stored_as_a_put_value() {
    // `Put { value: Version(Acc0) }` stores the caller's map under a key of
    // another map, whatever the target's family.
    let frame = FrameId::new(1);
    let ir = vec![state_acc_frame(
        frame,
        vec![bind(
            state(1, frame),
            state(0, frame),
            BindOp::Put {
                field: "__local__t".to_string(),
                value: ValueRef::Version(acc(0, frame)),
            },
        )],
    )];
    assert_eq!(
        verify(&ir),
        vec![VerifyError::AccAsScratchSeed {
            defect: AccSeedDefect::StoredAsValue {
                acc: acc(0, frame),
                into: state(1, frame),
            },
            at: span(),
        }]
    );
}

#[test]
fn a_put_of_acc_into_an_acc_map_is_reported_once() {
    // Both `PutInto` (the Acc target map) and `StoredAsValue` (the Acc value)
    // apply; the keyed finding wins and the bind is reported a single time.
    let frame = FrameId::ROOT;
    let ir = vec![bind(
        acc(1, frame),
        acc(0, frame),
        BindOp::Put {
            field: "count".to_string(),
            value: ValueRef::Version(acc(0, frame)),
        },
    )];
    assert_eq!(
        verify(&ir),
        vec![VerifyError::AccAsScratchSeed {
            defect: AccSeedDefect::PutInto {
                acc: acc(1, frame),
                key: "count".to_string(),
            },
            at: span(),
        }]
    );
}

#[test]
fn acc_as_scratch_seed_rejects_a_direct_rebind_of_acc_into_another_family() {
    // `Direct(Version(Acc0))` into a `State` target is the other half of (b).
    let frame = FrameId::new(1);
    let ir = vec![state_acc_frame(
        frame,
        vec![bind(
            state(1, frame),
            state(0, frame),
            BindOp::Direct(ValueRef::Version(acc(0, frame))),
        )],
    )];
    assert_eq!(
        verify(&ir),
        vec![VerifyError::AccAsScratchSeed {
            defect: AccSeedDefect::CrossFamilyStep {
                from: acc(0, frame),
                to: state(1, frame),
            },
            at: span(),
        }]
    );
}

#[test]
fn acc_as_scratch_seed_rejects_a_map_put_rebind_over_acc() {
    // `RebindShape::for_frame`: `Acc` is never a rebind target.
    let frame = FrameId::new(1);
    let rebind = ThreadedStmt::LocalRebind {
        local: "t".to_string(),
        carrier: "CF".to_string(),
        slot: CarrierSlot::Key("__local__t".to_string()),
        frame,
        value_var: "T1".to_string(),
        state_first: None,
        lowering: RebindLowering::MapPut {
            key: "__local__t".to_string(),
            source: acc(0, frame),
            target: acc(1, frame),
        },
        span: span(),
    };
    let ir = vec![state_acc_frame(
        frame,
        vec![
            ThreadedStmt::ConstructTuple {
                carrier: "CF".to_string(),
                doc: Document::Str("<construct>"),
                threads: vec!["t".to_string()],
                families: Vec::new(),
                span: span(),
            },
            rebind,
        ],
    )];
    assert_eq!(
        verify(&ir),
        vec![VerifyError::AccAsScratchSeed {
            defect: AccSeedDefect::RebindMap {
                local: "t".to_string(),
                acc: acc(0, frame),
            },
            at: span(),
        }]
    );
}

#[test]
fn acc_as_scratch_seed_rejects_a_threaded_body_that_threads_locals() {
    let mut node = build_threaded_method_body(FrameId::ROOT, &[], Vec::new(), span());
    if let ThreadedStmt::MethodBody { threads, .. } = &mut node {
        threads.push("t".to_string());
    }
    assert_eq!(
        verify(&[node]),
        vec![VerifyError::AccAsScratchSeed {
            defect: AccSeedDefect::ThreadedBodyThreadsLocals {
                threads: vec!["t".to_string()],
            },
            // Reported at the body's own span, not offset 0.
            at: span(),
        }]
    );
}

#[test]
fn acc_to_acc_steps_and_a_family_extraction_are_legal() {
    // `call_threaded/3` and a forward step `Acc` to `Acc`; a construct's
    // trailing-slot extraction is a `Direct` `Acc` step read off its carrier.
    let frame = FrameId::ROOT;
    let ir = vec![
        bind(acc(1, frame), acc(0, frame), direct_doc()),
        ThreadedStmt::ConstructTuple {
            carrier: "CF".to_string(),
            doc: Document::Str("<construct>"),
            threads: Vec::new(),
            families: vec![VersionPrefix::Acc],
            span: span(),
        },
        bind(
            acc(2, frame),
            acc(1, frame),
            BindOp::Direct(ValueRef::Doc(Document::Str("element(2, CF)"))),
        ),
    ];
    assert_eq!(verify(&ir), Vec::new());
}

// ─── OwnLocalKeyInAcc (ADR 0132 §2, "no key of the body's ever enters Acc") ─

#[test]
fn own_local_key_in_acc_rejects_a_local_put_into_acc() {
    let frame = FrameId::ROOT;
    let ir = vec![bind(
        acc(1, frame),
        acc(0, frame),
        BindOp::Put {
            field: CoreErlangGenerator::local_state_key("n"),
            value: ValueRef::Var("N1".to_string()),
        },
    )];
    assert_eq!(
        verify(&ir),
        vec![VerifyError::OwnLocalKeyInAcc {
            key: "__local__n".to_string(),
            acc: acc(1, frame),
            at: span(),
        }]
    );
}

#[test]
fn own_local_key_in_the_frames_own_scratch_map_is_legal() {
    // The body's locals live under their own keys in their own frame's
    // `StateAcc` map, never in `Acc`.
    let frame = FrameId::new(1);
    let ir = vec![state_acc_frame(
        frame,
        vec![bind(
            state(1, frame),
            state(0, frame),
            BindOp::Put {
                field: "__local__n".to_string(),
                value: ValueRef::Var("N1".to_string()),
            },
        )],
    )];
    assert_eq!(verify(&ir), Vec::new());
}

// ─── NlrSlotShapeMismatch (ADR 0132 §2, the NLR row) ──────────────────────

fn value_type_body(stmts: Vec<ThreadedStmt>) -> Vec<ThreadedStmt> {
    vec![build_threaded_method_body(
        FrameId::ROOT,
        &[VersionPrefix::SelfVt],
        stmts,
        span(),
    )]
}

fn value_type_scope() -> ScopeKind {
    ScopeKind::Threaded {
        kind: ThreadedBodyKind::ValueType,
    }
}

/// The verifier's errors other than [`VerifyError::NlrInThreadedBody`] (which
/// every NLR node in a threaded body raises): the slot-shape check alone.
fn verify_nlr_shape(ir: &[ThreadedStmt], scope: ScopeKind) -> Vec<VerifyError> {
    verify_in_scope(ir, scope)
        .into_iter()
        .filter(|e| !matches!(e, VerifyError::NlrInThreadedBody { .. }))
        .collect()
}

#[test]
fn any_nlr_node_in_a_threaded_body_is_refused() {
    // Phase 1c (BT-3871 review): a `^` throw carries the `Acc` version current
    // where its block is lowered, which a threaded callee can outrun. A
    // threaded body holds no NLR throw or catch at all, however well shaped.
    let frame = FrameId::ROOT;
    for ir in [
        value_type_body(vec![nlr_throw(vec![self_var(0, frame), acc(0, frame)])]),
        value_type_body(vec![nlr_catch(vec![
            VersionPrefix::SelfVt,
            VersionPrefix::Acc,
        ])]),
    ] {
        assert_eq!(
            verify_in_scope(&ir, value_type_scope()),
            vec![VerifyError::NlrInThreadedBody { at: span() }]
        );
    }
}

#[test]
fn nlr_slot_shape_mismatch_rejects_a_throw_without_acc_last() {
    // A value-type body's `^` must throw `{SelfN, AccN}`.
    let frame = FrameId::ROOT;
    let expected = vec![VersionPrefix::SelfVt, VersionPrefix::Acc];
    for (slot, found) in [
        (vec![self_var(0, frame)], vec![VersionPrefix::SelfVt]),
        (
            vec![acc(0, frame), self_var(0, frame)],
            vec![VersionPrefix::Acc, VersionPrefix::SelfVt],
        ),
    ] {
        assert_eq!(
            verify_nlr_shape(&value_type_body(vec![nlr_throw(slot)]), value_type_scope()),
            vec![VerifyError::NlrSlotShapeMismatch {
                expected: expected.clone(),
                found,
                at: span(),
            }]
        );
    }
    assert_eq!(
        verify_nlr_shape(
            &value_type_body(vec![nlr_throw(vec![self_var(0, frame), acc(0, frame)])]),
            value_type_scope()
        ),
        Vec::new()
    );
}

#[test]
fn nlr_slot_shape_mismatch_rejects_a_catch_that_does_not_unpack_acc() {
    assert_eq!(
        verify_nlr_shape(
            &value_type_body(vec![nlr_catch(Vec::new())]),
            value_type_scope()
        ),
        vec![VerifyError::NlrSlotShapeMismatch {
            expected: vec![VersionPrefix::SelfVt, VersionPrefix::Acc],
            found: Vec::new(),
            at: span(),
        }]
    );
    assert_eq!(
        verify_nlr_shape(
            &value_type_body(vec![nlr_catch(vec![
                VersionPrefix::SelfVt,
                VersionPrefix::Acc
            ])]),
            value_type_scope()
        ),
        Vec::new()
    );
}

#[test]
fn nlr_slot_shape_reaches_a_throw_nested_in_a_frame_of_the_body() {
    // A `^` inside a fold of a class body still throws the body's tuple, `Acc`.
    let frame = FrameId::new(1);
    let ir = vec![build_threaded_method_body(
        FrameId::ROOT,
        &[],
        vec![state_acc_frame(
            frame,
            vec![nlr_throw(vec![state(0, frame)])],
        )],
        span(),
    )];
    assert_eq!(
        verify_nlr_shape(
            &ir,
            ScopeKind::Threaded {
                kind: ThreadedBodyKind::Class
            }
        ),
        vec![VerifyError::NlrSlotShapeMismatch {
            expected: vec![VersionPrefix::Acc],
            found: vec![VersionPrefix::State],
            at: span(),
        }]
    );
}

#[test]
fn nlr_slots_outside_a_threaded_body_are_unconstrained() {
    let frame = FrameId::ROOT;
    let ir = vec![nlr_catch(Vec::new()), nlr_throw(vec![state(0, frame)])];
    assert_eq!(verify(&ir), Vec::new());
}

#[test]
fn an_nlr_throw_with_an_empty_slot_is_rejected_in_every_scope() {
    // `render_nlr_throw` would render the slot as `{}`: an empty tuple no catch
    // unpacks. Reported whether or not the throw sits in a threaded body (the
    // shape check `NlrSlotShapeMismatch` only fires inside one).
    let empty = vec![nlr_throw(Vec::new())];
    for scope in [
        ScopeKind::ClassMethod,
        ScopeKind::ValueType,
        ScopeKind::Instance,
    ] {
        assert_eq!(
            verify_in_scope(&empty, scope),
            vec![VerifyError::NlrThrowWithoutSlot { at: span() }],
            "{scope:?}"
        );
    }
    let in_body = vec![build_threaded_method_body(
        FrameId::ROOT,
        &[],
        vec![nlr_throw(Vec::new())],
        span(),
    )];
    let errors = verify_in_scope(
        &in_body,
        ScopeKind::Threaded {
            kind: ThreadedBodyKind::Class,
        },
    );
    assert!(
        errors.contains(&VerifyError::NlrThrowWithoutSlot { at: span() }),
        "{errors:?}"
    );
    // A throw that names its slot is fine outside a threaded body.
    let named = vec![nlr_throw(vec![state(0, FrameId::ROOT)])];
    assert_eq!(verify_in_scope(&named, ScopeKind::Instance), Vec::new());
}

#[test]
fn acc_never_renders_as_a_beamtalk_local_does() {
    // `to_core_var` only capitalises a local, so `acc`, `acc0` and `acc1`
    // render `Acc`, `Acc0` and `Acc1`; an `Acc` version must be none of them,
    // in any version, or a local would shadow the caller's map.
    let frame = FrameId::ROOT;
    let mut rendered_locals = std::collections::HashSet::new();
    for name in ["acc", "acc0", "acc1", "acc2", "_acc", "_acc0"] {
        for version in 0..=3 {
            let local = VersionedVar::new(VersionPrefix::Local(name.to_string()), version, frame);
            rendered_locals.insert(local.render_name());
        }
    }
    for version in 0..=12 {
        let rendered = acc(version, frame).render_name();
        assert!(
            !rendered_locals.contains(&rendered),
            "Acc version {version} renders as the local `{rendered}`"
        );
        assert_eq!(rendered, format!("Acc@{version}"));
    }
    // A local bind beside an `Acc` bind in one body keeps both names.
    let local =
        |version| VersionedVar::new(VersionPrefix::Local("acc".to_string()), version, frame);
    let ir = vec![
        bind(local(1), local(0), direct_doc()),
        bind(acc(1, frame), acc(0, frame), direct_doc()),
    ];
    assert_eq!(
        lower_and_render(&ir).to_pretty_string(),
        "let Acc1 = <call_threaded> in let Acc@1 = <call_threaded> in "
    );
}

#[test]
fn nlr_throw_renders_its_slot_bare_or_as_a_tuple() {
    let frame = FrameId::ROOT;
    assert_eq!(
        lower_and_render(&[nlr_throw(vec![acc(2, frame)])]).to_pretty_string(),
        "let _NlrThrow1 = call 'erlang':'throw'({'$bt_nlr', NlrTok, Val, Acc@2}) in "
    );
    assert_eq!(
        lower_and_render(&[nlr_throw(vec![self_var(1, frame), acc(2, frame)])]).to_pretty_string(),
        "let _NlrThrow1 = call 'erlang':'throw'({'$bt_nlr', NlrTok, Val, {Self1, Acc@2}}) in "
    );
}

// ─── ActorStateInThreadedBlock (ADR 0132 §3, "Actor instance method") ─────

fn actor_threaded_block() -> ScopeKind {
    ScopeKind::ThreadedBlock {
        caller: ThreadedBodyKind::Actor,
    }
}

#[test]
fn actor_state_in_threaded_block_rejects_a_field_write_or_read() {
    let frame = FrameId::new(1);
    // A field write puts into the block's map; a field read unpacks out of it.
    let write = bind(
        state(1, frame),
        state(0, frame),
        BindOp::Put {
            field: "count".to_string(),
            value: ValueRef::Literal("1"),
        },
    );
    let read = bind(
        local("count", 1, frame),
        state(0, frame),
        BindOp::Unpack {
            field: "count".to_string(),
        },
    );
    for (stmt, var) in [(write, state(1, frame)), (read, state(0, frame))] {
        let ir = vec![state_acc_frame(frame, vec![stmt])];
        assert_eq!(
            verify_in_scope(&ir, actor_threaded_block()),
            vec![VerifyError::ActorStateInThreadedBlock {
                var,
                field: Some("count".to_string()),
                at: span(),
            }]
        );
        // The same block in an ordinary actor method writes the actor state.
        assert_eq!(verify_in_scope(&ir, ScopeKind::Instance), Vec::new());
    }
}

#[test]
fn actor_state_in_threaded_block_rejects_a_method_level_state_step() {
    let frame = FrameId::ROOT;
    let ir = vec![bind(state(1, frame), state(0, frame), direct_doc())];
    assert_eq!(
        verify_in_scope(&ir, actor_threaded_block()),
        vec![VerifyError::ActorStateInThreadedBlock {
            var: state(1, frame),
            field: None,
            at: span(),
        }]
    );
}

#[test]
fn a_locals_only_threaded_block_verifies_clean() {
    // Its map carries only `'__local__'` keys.
    let frame = FrameId::new(1);
    let ir = vec![state_acc_frame(
        frame,
        vec![bind(
            state(1, frame),
            state(0, frame),
            BindOp::Put {
                field: "__local__n".to_string(),
                value: ValueRef::Var("N1".to_string()),
            },
        )],
    )];
    assert_eq!(verify_in_scope(&ir, actor_threaded_block()), Vec::new());
}

// ─── report_threaded_ir_verify_errors ──────────────────────────────────────

#[test]
fn every_new_finding_reaches_the_release_diagnostic_by_name() {
    let frame = FrameId::ROOT;
    let findings = [
        (
            VerifyError::AccAsScratchSeed {
                defect: AccSeedDefect::ThreadedBodyThreadsLocals {
                    threads: vec!["t".to_string()],
                },
                at: span(),
            },
            "AccAsScratchSeed",
        ),
        (
            VerifyError::OwnLocalKeyInAcc {
                key: "__local__t".to_string(),
                acc: acc(1, frame),
                at: span(),
            },
            "OwnLocalKeyInAcc",
        ),
        (
            VerifyError::NlrSlotShapeMismatch {
                expected: vec![VersionPrefix::Acc],
                found: Vec::new(),
                at: span(),
            },
            "NlrSlotShapeMismatch",
        ),
        (
            VerifyError::ActorStateInThreadedBlock {
                var: state(1, frame),
                field: None,
                at: span(),
            },
            "ActorStateInThreadedBlock",
        ),
        (
            VerifyError::NlrThrowWithoutSlot { at: span() },
            "NlrThrowWithoutSlot",
        ),
    ];
    for (error, name) in findings {
        let diag = verify_errors_to_diagnostic(&[error], "ADR 0132 check", span());
        assert!(diag.message.contains(name), "{name}: {:?}", diag.message);
    }
}
