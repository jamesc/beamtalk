// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3725: `verify_in_scope`'s `ActorStateInClassMethod` — a class-method
//! scope never opens a locals-less `StateAcc` loop or threads the
//! `State`/`SelfVt` family — and ADR 0131 §3's extension to
//! `ScopeKind::ValueType`: a value-type method never threads the actor
//! `State` family.

use super::*;
use crate::core_erlang::threaded_ir::verify_in_scope;

/// The pre-BT-3694-fix shape: `[self foo > 0] whileTrue: [self foo]` inside a
/// class method took the `StateAcc` loop shape with no threaded local (its
/// condition's self-send counted as a state effect), so its seed could only be
/// the ambient actor `State`.
fn locals_less_state_acc_loop(reason: StateAccFallbackReason) -> Vec<ThreadedStmt> {
    let frame = FrameId::new(1);
    vec![ThreadedStmt::ConditionalLoop {
        fn_name: "while".to_string(),
        mode: ThreadingMode::StateAcc(reason),
        frame,
        threads: Vec::new(),
        counter: None,
        condition: Vec::new(),
        condition_value: ValueRef::Literal("'true'"),
        continue_arm: Document::Str("<opaque continue arm>"),
        body: Vec::new(),
        produces: vec![VersionedVar::new(VersionPrefix::State, 0, frame)],
        outer_args: Some(vec![leaf::var("State".to_string())]),
        exit_arm: Document::Str("<opaque exit>"),
        span: span(),
    }]
}

#[test]
fn class_method_rejects_locals_less_state_acc_loop() {
    let ir = locals_less_state_acc_loop(StateAccFallbackReason::NoThreadedLocals);
    assert_eq!(
        verify_in_scope(&ir, ScopeKind::ClassMethod),
        vec![VerifyError::ActorStateInClassMethod {
            defect: ClassMethodDefect::StateAccLoopWithoutLocals,
            at: span(),
        }]
    );
}

#[test]
fn class_method_accepts_state_acc_loop_carrying_locals() {
    // A class-method loop that threads a local rides a `StateAcc` map seeded
    // from a fresh `maps:new()` — legitimate, and its reason is never
    // `NoThreadedLocals`.
    let ir = locals_less_state_acc_loop(StateAccFallbackReason::SelfSendInBody);
    assert_eq!(verify_in_scope(&ir, ScopeKind::ClassMethod), Vec::new());
}

#[test]
fn instance_scope_is_unconstrained_by_the_class_method_invariant() {
    let ir = locals_less_state_acc_loop(StateAccFallbackReason::NoThreadedLocals);
    assert_eq!(verify_in_scope(&ir, ScopeKind::Instance), Vec::new());
    assert_eq!(verify(&ir), Vec::new());
}

#[test]
fn class_method_rejects_state_family_bind() {
    let frame = FrameId::ROOT;
    let ir = vec![ThreadedStmt::Bind {
        target: VersionedVar::new(VersionPrefix::State, 1, frame),
        source: VersionedVar::new(VersionPrefix::State, 0, frame),
        op: BindOp::Direct(ValueRef::Literal("'ok'")),
        span: span(),
    }];
    assert_eq!(
        verify_in_scope(&ir, ScopeKind::ClassMethod),
        vec![VerifyError::ActorStateInClassMethod {
            defect: ClassMethodDefect::FamilyVersion(VersionPrefix::State),
            at: span(),
        }]
    );
    assert_eq!(verify_in_scope(&ir, ScopeKind::Instance), Vec::new());
}

#[test]
fn class_method_rejects_self_vt_family_bind() {
    let frame = FrameId::ROOT;
    let ir = vec![ThreadedStmt::Bind {
        target: self_var(1, frame),
        source: self_var(0, frame),
        op: BindOp::Direct(ValueRef::Literal("'ok'")),
        span: span(),
    }];
    assert_eq!(
        verify_in_scope(&ir, ScopeKind::ClassMethod),
        vec![VerifyError::ActorStateInClassMethod {
            defect: ClassMethodDefect::FamilyVersion(VersionPrefix::SelfVt),
            at: span(),
        }]
    );
}

#[test]
fn class_method_accepts_state_acc_map_versions_inside_a_threading_frame() {
    // Inside a `Threaded`/`ConditionalLoop` frame the `State` prefix is the
    // `StateAcc` map's version (it carries threaded locals in a class method),
    // not the actor `State` family — only a method-level `State` bind is.
    let frame = FrameId::new(1);
    let ir = vec![ThreadedStmt::Threaded {
        mode: ThreadingMode::StateAcc(StateAccFallbackReason::SelfSendInBody),
        frame,
        threads: Vec::new(),
        body: vec![ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::State, 1, frame),
            source: VersionedVar::new(VersionPrefix::State, 0, frame),
            op: BindOp::Direct(ValueRef::Literal("'ok'")),
            span: span(),
        }],
        produces: vec![VersionedVar::new(VersionPrefix::State, 1, frame)],
        span: span(),
    }];
    assert_eq!(verify_in_scope(&ir, ScopeKind::ClassMethod), Vec::new());
}

#[test]
fn class_method_accepts_local_binds() {
    let frame = FrameId::ROOT;
    let ir = vec![ThreadedStmt::Bind {
        target: local("Sum", 1, frame),
        source: local("Sum", 0, frame),
        op: BindOp::Direct(ValueRef::Literal("1")),
        span: span(),
    }];
    assert_eq!(verify_in_scope(&ir, ScopeKind::ClassMethod), Vec::new());
}

// ── ADR 0131 §3: ScopeKind::ValueType ────────────────────────────────────

/// The o3 value-type shape's pre-fix IR: `r := (4 =:= 5 ifTrue: [2] ifFalse:
/// [t := t + 1. 1]) + 1` in a value-type method. `inline_control_flow_producer`
/// hard-codes the `State` family, so the conditional's tuple is followed by a
/// method-level `State1 = element(2, CF)` extraction — a `State` the
/// value-type method never had (`erlc`: unbound `State`).
fn o3_value_type_pre_fix_ir() -> Vec<ThreadedStmt> {
    let frame = FrameId::ROOT;
    vec![
        ThreadedStmt::Statement(docvec!["let _CF1 = <conditional tuple> in "], span()),
        ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::State, 1, frame),
            source: VersionedVar::new(VersionPrefix::State, 0, frame),
            op: BindOp::Direct(ValueRef::Doc(docvec!["call 'erlang':'element'(2, _CF1)"])),
            span: span(),
        },
    ]
}

#[test]
fn value_type_method_rejects_the_actor_state_family() {
    assert_eq!(
        verify_in_scope(&o3_value_type_pre_fix_ir(), ScopeKind::ValueType),
        vec![VerifyError::ActorStateInClassMethod {
            defect: ClassMethodDefect::FamilyVersion(VersionPrefix::State),
            at: span(),
        }]
    );
    // The same IR is an ordinary `State` step in an actor instance method.
    assert_eq!(
        verify_in_scope(&o3_value_type_pre_fix_ir(), ScopeKind::Instance),
        Vec::new()
    );
}

#[test]
fn threaded_scope_derives_value_type_from_the_generator_context() {
    let mut generator = CoreErlangGenerator::new("scope_kind_derivation");
    generator.context = crate::core_erlang::CodeGenContext::ValueType;
    assert_eq!(generator.threaded_scope(), ScopeKind::ValueType);
    // A value type's class-side method is a class method first.
    generator.set_in_class_method(true);
    assert_eq!(generator.threaded_scope(), ScopeKind::ClassMethod);
    generator.set_in_class_method(false);
    generator.context = crate::core_erlang::CodeGenContext::Actor;
    assert_eq!(generator.threaded_scope(), ScopeKind::Instance);
}

#[test]
fn value_type_method_accepts_its_own_self_family() {
    let frame = FrameId::ROOT;
    let ir = vec![ThreadedStmt::Bind {
        target: self_var(1, frame),
        source: self_var(0, frame),
        op: BindOp::Direct(ValueRef::Literal("'ok'")),
        span: span(),
    }];
    assert_eq!(verify_in_scope(&ir, ScopeKind::ValueType), Vec::new());
}

#[test]
fn value_type_method_accepts_state_acc_map_versions_inside_a_threading_frame() {
    let frame = FrameId::new(1);
    let ir = vec![ThreadedStmt::Threaded {
        mode: ThreadingMode::StateAcc(StateAccFallbackReason::None),
        frame,
        threads: Vec::new(),
        body: vec![ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::State, 1, frame),
            source: VersionedVar::new(VersionPrefix::State, 0, frame),
            op: BindOp::Direct(ValueRef::Literal("'ok'")),
            span: span(),
        }],
        produces: vec![VersionedVar::new(VersionPrefix::State, 1, frame)],
        span: span(),
    }];
    assert_eq!(verify_in_scope(&ir, ScopeKind::ValueType), Vec::new());
}
