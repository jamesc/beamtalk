// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-2749: Pin sync-send actor typing and the cast (`!`) → `Nil` retype
//! (ADR 0104 Phase 1).
//!
//! Two behaviours are pinned here:
//!
//! 1. A **synchronous** send to a method on an Actor subclass (`counter
//!    increment`) types as an ordinary method send — it resolves to the
//!    method's declared or inferred return type. This already worked "by
//!    accident of representation"; these tests lock it in.
//! 2. A bare **cast** statement (`counter increment!`, the postfix `!` form)
//!    is fire-and-forget and evaluates to `Nil` (`UndefinedObject`), not the
//!    (asynchronous) reply — and not `Dynamic` as before.

use super::common::*;

/// Register a `Counter` class as an Actor subclass with two instance methods:
///
/// - `increment -> Integer` — models a method with a **declared** return type.
/// - `getValue -> Integer` — models a method whose return type was **inferred**
///   from its body (the hierarchy stores the resolved type either way).
fn add_counter_actor(hierarchy: &mut ClassHierarchy) {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};

    let info = ClassInfo {
        surface_incomplete: false,
        name: eco_string("Counter"),
        superclass: Some(eco_string("Actor")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        handle_scope: None,
        state: vec![eco_string("count")],
        state_types: {
            let mut m = std::collections::HashMap::new();
            m.insert(eco_string("count"), eco_string("Integer"));
            m
        },
        state_has_default: std::collections::HashMap::new(),
        methods: vec![
            MethodInfo {
                selector: eco_string("increment"),
                arity: 0,
                kind: MethodKind::Primary,
                defined_in: eco_string("Counter"),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                // Declared `-> Integer`.
                return_type: Some(eco_string("Integer")),
                param_types: vec![],
                doc: None,
            },
            MethodInfo {
                selector: eco_string("getValue"),
                arity: 0,
                kind: MethodKind::Primary,
                defined_in: eco_string("Counter"),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                // Inferred from body (`^self.count`) — resolved to Integer.
                return_type: Some(eco_string("Integer")),
                param_types: vec![],
                doc: None,
            },
        ],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };

    hierarchy.add_from_beam_meta(vec![info]);
}

/// Build a cast (postfix `!`) message send: `receiver selector!`.
fn cast_send(receiver: Expression, selector: MessageSelector) -> Expression {
    Expression::MessageSend {
        receiver: Box::new(receiver),
        selector,
        arguments: vec![],
        is_cast: true,
        span: span(),
    }
}

/// A synchronous unary send to a **declared-return** method on an Actor
/// subclass infers that declared return type.
#[test]
fn sync_send_declared_return_infers_declared_type() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_counter_actor(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("counter", InferredType::known("Counter"));

    let ty = checker.infer_expr(
        &msg_send(
            var("counter"),
            MessageSelector::Unary("increment".into()),
            vec![],
        ),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        ty,
        InferredType::known("Integer"),
        "sync send to a declared `-> Integer` actor method should infer Integer"
    );
}

/// A synchronous unary send to an **inferred-return** method on an Actor
/// subclass infers that method's (resolved) return type.
#[test]
fn sync_send_inferred_return_infers_method_type() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_counter_actor(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("counter", InferredType::known("Counter"));

    let ty = checker.infer_expr(
        &msg_send(
            var("counter"),
            MessageSelector::Unary("getValue".into()),
            vec![],
        ),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        ty,
        InferredType::known("Integer"),
        "sync send to an inferred-return actor method should infer its return type"
    );
}

/// BT-2749: a bare cast statement `counter increment!` evaluates to `Nil`
/// (`UndefinedObject`) — the fire-and-forget async send has no synchronous
/// reply. Previously this typed as `Dynamic`.
#[test]
fn cast_send_infers_nil() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_counter_actor(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("counter", InferredType::known("Counter"));

    let ty = checker.infer_expr(
        &cast_send(var("counter"), MessageSelector::Unary("increment".into())),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        ty,
        InferredType::known("UndefinedObject"),
        "a bare cast `counter increment!` should infer Nil (UndefinedObject)"
    );
}

/// The cast retype is independent of the target method's declared return type:
/// casting to a result-returning method still yields `Nil` (no reply is
/// awaited). This also documents the deliberate non-feature: no
/// "discarded reply" warning is emitted.
#[test]
fn cast_send_to_result_returning_method_still_nil() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_counter_actor(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("counter", InferredType::known("Counter"));

    let ty = checker.infer_expr(
        // `getValue` declares `-> Integer`, but the cast discards the reply.
        &cast_send(var("counter"), MessageSelector::Unary("getValue".into())),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        ty,
        InferredType::known("UndefinedObject"),
        "casting to a result-returning method still infers Nil"
    );
    assert!(
        checker.take_diagnostics().is_empty(),
        "casting to a result-returning method must NOT emit a discarded-reply warning"
    );
}
