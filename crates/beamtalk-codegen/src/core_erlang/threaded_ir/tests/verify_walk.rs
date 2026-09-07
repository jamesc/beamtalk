// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `verify()`'s `VerifyWalk` invariant coverage: the clean/silent case,
//! `UnboundVersion`, `NonLinearVersion`, `ThreadingModeUnpackMismatch`, and
//! `ShadowWriteMissing` (ADR 0110 contract).

use super::*;

// ── verify(): the clean/silent case ─────────────────────────────────

#[test]
fn verify_silent_on_well_formed_direct_params_fixture() {
    let frame = FrameId::new(1);
    let sum_source = local("sum", 0, frame);
    let count_source = local("count", 0, frame);
    let ir = vec![ThreadedStmt::Threaded {
        mode: ThreadingMode::DirectParams,
        frame,
        shadow_write_eligible: true,
        body: vec![
            ThreadedStmt::Bind {
                target: local("sum", 1, frame),
                source: sum_source.clone(),
                op: BindOp::Direct(ValueRef::Version(sum_source)),
                shadow_write: false,
                span: span(),
            },
            ThreadedStmt::Bind {
                target: local("count", 1, frame),
                source: count_source.clone(),
                op: BindOp::Direct(ValueRef::Version(count_source)),
                shadow_write: false,
                span: span(),
            },
        ],
        produces: vec![local("sum", 1, frame), local("count", 1, frame)],
        span: span(),
    }];
    assert_eq!(verify(&ir), Vec::new());
}

#[test]
fn verify_silent_on_well_formed_conditional_loop_fixture() {
    // ADR 0111 Addendum 2, Gap 1 / ADR 0118 phase 3 (BT-3419):
    // `verify()` treats `ConditionalLoop` almost exactly like `Threaded`
    // — push frame/mode once, walk `condition` then `body`, check_use
    // `condition_value` and each `produces` entry, pop — no new
    // `VerifyError` variant. Opaque fields (`continue_arm`/`exit_arm`)
    // and caller-supplied, non-threading fields (`fn_name`/`counter`)
    // are not, and cannot be, inspected — see the variant's doc comment.
    // `condition` empty + `condition_value` a literal here (a pure
    // condition, e.g. a counted loop's counter compare) — see
    // `verify_silent_on_conditional_loop_with_condition_prelude` below
    // for a condition with a real `Bind` in its own prelude.
    let frame = FrameId::new(1);
    let sum_v0 = local("sum", 0, frame);
    let sum_v1 = VersionedVar::new(VersionPrefix::Gensym("_Sum7".to_string()), 1, frame);
    let ir = vec![ThreadedStmt::ConditionalLoop {
        fn_name: "while".to_string(),
        mode: ThreadingMode::DirectParams,
        frame,
        shadow_write_eligible: true,
        counter: None,
        condition: Vec::new(),
        condition_value: ValueRef::Literal("'true'"),
        continue_arm: Document::Str("<opaque condition>"),
        body: vec![ThreadedStmt::Bind {
            target: sum_v1,
            source: sum_v0.clone(),
            op: BindOp::Direct(ValueRef::Doc(Document::Str("<opaque rhs>"))),
            shadow_write: false,
            span: span(),
        }],
        produces: vec![sum_v0],
        exit_arm: Document::Str("<opaque exit>"),
        span: span(),
    }];
    assert_eq!(verify(&ir), Vec::new());
}

#[test]
fn verify_silent_on_conditional_loop_with_condition_prelude() {
    // ADR 0118 phase 3 (BT-3419): a condition with a real `Bind` in its
    // own prelude (e.g. a self-send's `State` advance) — verified in
    // the SAME frame as `body`, and `condition_value` referencing the
    // version that `Bind` just produced resolves cleanly. This is the
    // exact shape phase 3 makes possible: before it, the condition's
    // Binds were never real IR at all.
    let frame = FrameId::new(1);
    let state_v0 = VersionedVar::new(VersionPrefix::State, 0, frame);
    let state_v1 = VersionedVar::new(VersionPrefix::State, 1, frame);
    let sum_v0 = local("sum", 0, frame);
    let sum_v1 = VersionedVar::new(VersionPrefix::Gensym("_Sum7".to_string()), 1, frame);
    let ir = vec![ThreadedStmt::ConditionalLoop {
        fn_name: "while".to_string(),
        mode: ThreadingMode::DirectParams,
        frame,
        shadow_write_eligible: true,
        counter: None,
        condition: vec![ThreadedStmt::Bind {
            target: state_v1,
            source: state_v0,
            op: BindOp::Direct(ValueRef::Doc(Document::Str("<opaque self-send reply>"))),
            shadow_write: false,
            span: span(),
        }],
        condition_value: ValueRef::Doc(Document::Str("<opaque bool expr>")),
        continue_arm: Document::Str("<opaque continue arm>"),
        body: vec![ThreadedStmt::Bind {
            target: sum_v1,
            source: sum_v0.clone(),
            op: BindOp::Direct(ValueRef::Doc(Document::Str("<opaque rhs>"))),
            shadow_write: false,
            span: span(),
        }],
        produces: vec![sum_v0],
        exit_arm: Document::Str("<opaque exit>"),
        span: span(),
    }];
    assert_eq!(verify(&ir), Vec::new());
}

#[test]
fn verify_unbound_version_conditional_loop_condition_references_unbound_version() {
    // ADR 0118 phase 3 (BT-3419): a `condition_value` referencing a
    // version nothing in `condition` (or an ancestor frame) produced is
    // caught, exactly like any other `check_use` site.
    let frame = FrameId::new(1);
    let ir = vec![ThreadedStmt::ConditionalLoop {
        fn_name: "while".to_string(),
        mode: ThreadingMode::DirectParams,
        frame,
        shadow_write_eligible: true,
        counter: None,
        condition: Vec::new(),
        condition_value: ValueRef::Version(VersionedVar::new(VersionPrefix::State, 1, frame)),
        continue_arm: Document::Str("<opaque continue arm>"),
        body: Vec::new(),
        produces: Vec::new(),
        exit_arm: Document::Str("<opaque exit>"),
        span: span(),
    }];
    let errors = verify(&ir);
    assert!(
        errors.iter().any(|e| matches!(
            e,
            VerifyError::UnboundVersion { var, .. }
                if *var == VersionedVar::new(VersionPrefix::State, 1, frame)
        )),
        "expected UnboundVersion for the condition_value's unbound State1, got: {errors:?}"
    );
}

#[test]
fn verify_unbound_version_conditional_loop_frame_flow_matches_threaded() {
    // ConditionalLoop's frame is pushed/popped exactly like Threaded's —
    // a Bind referencing a version from a frame never entered is still
    // caught.
    let frame = FrameId::new(1);
    let stray_frame = FrameId::new(99);
    let ir = vec![ThreadedStmt::ConditionalLoop {
        fn_name: "while".to_string(),
        mode: ThreadingMode::DirectParams,
        frame,
        shadow_write_eligible: true,
        counter: None,
        condition: Vec::new(),
        condition_value: ValueRef::Literal("'true'"),
        continue_arm: Document::Str("<opaque condition>"),
        body: vec![ThreadedStmt::Bind {
            target: local("sum", 1, frame),
            source: class_var(1, stray_frame),
            op: BindOp::Direct(ValueRef::Doc(Document::Str("<opaque rhs>"))),
            shadow_write: false,
            span: span(),
        }],
        produces: vec![local("sum", 1, frame)],
        exit_arm: Document::Str("<opaque exit>"),
        span: span(),
    }];
    let errors = verify(&ir);
    assert!(
        errors.iter().any(|e| matches!(
            e,
            VerifyError::UnboundVersion { var, .. } if var.frame == stray_frame
        )),
        "expected UnboundVersion for the out-of-scope frame, got: {errors:?}"
    );
}

// ── UnboundVersion ───────────────────────────────────────────────────

#[test]
fn verify_unbound_version_when_source_has_no_producer() {
    let f0 = FrameId::ROOT;
    // ClassVars2 is used as a Bind source but nothing ever produced it.
    let ir = vec![ThreadedStmt::Bind {
        target: class_var(1, f0),
        source: class_var(2, f0),
        op: BindOp::Put {
            field: "runs".to_string(),
            value: ValueRef::Var("_Val0".to_string()),
            class_tag: ValueRef::Var("ClassSelf".to_string()),
        },
        shadow_write: false,
        span: span(),
    }];
    let errors = verify(&ir);
    assert!(
        errors.iter().any(
            |e| matches!(e, VerifyError::UnboundVersion { var, .. } if *var == class_var(2, f0))
        ),
        "expected UnboundVersion for ClassVars2, got: {errors:?}"
    );
}

#[test]
fn verify_version_zero_is_always_bound() {
    let f0 = FrameId::ROOT;
    let ir = vec![ThreadedStmt::Bind {
        target: class_var(1, f0),
        source: class_var(0, f0), // frame-entry parameter — never a Bind target
        op: BindOp::Put {
            field: "runs".to_string(),
            value: ValueRef::Var("_Val0".to_string()),
            class_tag: ValueRef::Var("ClassSelf".to_string()),
        },
        shadow_write: true,
        span: span(),
    }];
    assert_eq!(verify(&ir), Vec::new());
}

#[test]
fn verify_unbound_version_when_frame_not_on_stack() {
    // References a version from a frame that is never entered (not
    // ROOT, not any nested Threaded's frame).
    let stray_frame = FrameId::new(99);
    let ir = vec![ThreadedStmt::Return(
        ValueRef::Literal("'nil'"),
        class_var(1, stray_frame),
        span(),
    )];
    let errors = verify(&ir);
    assert!(
        errors.iter().any(|e| matches!(
            e,
            VerifyError::UnboundVersion { var, .. } if var.frame == stray_frame
        )),
        "expected UnboundVersion for the out-of-scope frame, got: {errors:?}"
    );
}

#[test]
fn verify_frame_flow_grandparent_binding_visible_in_grandchild_frame() {
    // The explicit frame-flow rule: a version produced in an ancestor
    // frame is visible to a doubly-nested descendant frame, not just an
    // immediate child. F0 (root) binds ClassVars1; F2 (nested two
    // Threaded levels deep, inside F1) uses it as a Bind source.
    let f0 = FrameId::ROOT;
    let f1 = FrameId::new(1);
    let f2 = FrameId::new(2);
    let ir = vec![
        ThreadedStmt::Bind {
            target: class_var(1, f0),
            source: class_var(0, f0),
            op: BindOp::Direct(ValueRef::Literal("'a'")),
            shadow_write: false,
            span: span(),
        },
        ThreadedStmt::Threaded {
            mode: ThreadingMode::DirectParams,
            frame: f1,
            shadow_write_eligible: true,
            body: vec![ThreadedStmt::Threaded {
                mode: ThreadingMode::DirectParams,
                frame: f2,
                shadow_write_eligible: true,
                body: vec![ThreadedStmt::Bind {
                    target: class_var(2, f2),
                    source: class_var(1, f0), // grandparent's binding
                    op: BindOp::Direct(ValueRef::Literal("'b'")),
                    shadow_write: false,
                    span: span(),
                }],
                produces: vec![class_var(2, f2)],
                span: span(),
            }],
            produces: vec![],
            span: span(),
        },
    ];
    assert_eq!(verify(&ir), Vec::new());
}

#[test]
fn verify_frame_flow_sibling_frame_binding_not_visible() {
    // Two sibling Threaded frames (F1, F2) both nested directly under
    // root. A version produced inside F1 is NOT visible inside F2 — the
    // frame stack only contains ancestors, never siblings.
    let f1 = FrameId::new(1);
    let f2 = FrameId::new(2);
    let ir = vec![
        ThreadedStmt::Threaded {
            mode: ThreadingMode::DirectParams,
            frame: f1,
            shadow_write_eligible: true,
            body: vec![ThreadedStmt::Bind {
                target: local("sum", 1, f1),
                source: local("sum", 0, f1),
                op: BindOp::Direct(ValueRef::Literal("'a'")),
                shadow_write: false,
                span: span(),
            }],
            produces: vec![local("sum", 1, f1)],
            span: span(),
        },
        ThreadedStmt::Threaded {
            mode: ThreadingMode::DirectParams,
            frame: f2,
            shadow_write_eligible: true,
            body: vec![ThreadedStmt::Bind {
                target: local("count", 1, f2),
                // References F1's Sum1 — F1 is a sibling, not an
                // ancestor, of F2, so this must be UnboundVersion.
                source: local("sum", 1, f1),
                op: BindOp::Direct(ValueRef::Literal("'b'")),
                shadow_write: false,
                span: span(),
            }],
            produces: vec![local("count", 1, f2)],
            span: span(),
        },
    ];
    let errors = verify(&ir);
    assert!(
        errors.iter().any(|e| matches!(
            e,
            VerifyError::UnboundVersion { var, .. } if *var == local("sum", 1, f1)
        )),
        "expected UnboundVersion for the sibling frame's Sum1, got: {errors:?}"
    );
}

// ── NonLinearVersion ─────────────────────────────────────────────────

#[test]
fn verify_non_linear_version_when_produced_twice() {
    let f0 = FrameId::ROOT;
    // ClassVars1 is bound by two different Binds in the same frame.
    let ir = vec![
        ThreadedStmt::Bind {
            target: class_var(1, f0),
            source: class_var(0, f0),
            op: BindOp::Direct(ValueRef::Literal("'a'")),
            shadow_write: false,
            span: span(),
        },
        ThreadedStmt::Bind {
            target: class_var(1, f0),
            source: class_var(0, f0),
            op: BindOp::Direct(ValueRef::Literal("'b'")),
            shadow_write: false,
            span: span(),
        },
    ];
    let errors = verify(&ir);
    assert!(
        errors.iter().any(|e| matches!(
            e,
            VerifyError::NonLinearVersion { var, producers: 2, .. } if *var == class_var(1, f0)
        )),
        "expected NonLinearVersion(producers=2), got: {errors:?}"
    );
}

#[test]
fn verify_non_linear_version_when_consumed_twice() {
    let f0 = FrameId::ROOT;
    // ClassVars1 (produced once) is used as the `source` of two
    // successor Binds — fans out instead of chaining linearly.
    let ir = vec![
        ThreadedStmt::Bind {
            target: class_var(1, f0),
            source: class_var(0, f0),
            op: BindOp::Direct(ValueRef::Literal("'a'")),
            shadow_write: false,
            span: span(),
        },
        ThreadedStmt::Bind {
            target: class_var(2, f0),
            source: class_var(1, f0),
            op: BindOp::Direct(ValueRef::Literal("'b'")),
            shadow_write: false,
            span: span(),
        },
        ThreadedStmt::Bind {
            target: class_var(3, f0),
            source: class_var(1, f0),
            op: BindOp::Direct(ValueRef::Literal("'c'")),
            shadow_write: false,
            span: span(),
        },
    ];
    let errors = verify(&ir);
    assert!(
        errors.iter().any(|e| matches!(
            e,
            VerifyError::NonLinearVersion { var, consumers: 2, .. } if *var == class_var(1, f0)
        )),
        "expected NonLinearVersion(consumers=2), got: {errors:?}"
    );
}

#[test]
fn verify_non_linear_version_findings_are_deterministically_ordered() {
    // Two independent NonLinearVersion violations (Count1 and Sum1, each
    // produced twice) in the same frame. HashMap iteration order is
    // per-process-random, so this pins verify()'s sort — run repeatedly
    // (`cargo test -- --test-threads=1` doesn't reseed per test, but a
    // fresh process would) this must always come back in the same order.
    let f0 = FrameId::ROOT;
    let ir = vec![
        ThreadedStmt::Bind {
            target: local("sum", 1, f0),
            source: local("sum", 0, f0),
            op: BindOp::Direct(ValueRef::Literal("'a'")),
            shadow_write: false,
            span: span(),
        },
        ThreadedStmt::Bind {
            target: local("sum", 1, f0),
            source: local("sum", 0, f0),
            op: BindOp::Direct(ValueRef::Literal("'b'")),
            shadow_write: false,
            span: span(),
        },
        ThreadedStmt::Bind {
            target: local("count", 1, f0),
            source: local("count", 0, f0),
            op: BindOp::Direct(ValueRef::Literal("'c'")),
            shadow_write: false,
            span: span(),
        },
        ThreadedStmt::Bind {
            target: local("count", 1, f0),
            source: local("count", 0, f0),
            op: BindOp::Direct(ValueRef::Literal("'d'")),
            shadow_write: false,
            span: span(),
        },
    ];
    let errors = verify(&ir);
    assert_eq!(
        errors,
        vec![
            VerifyError::NonLinearVersion {
                var: local("count", 1, f0),
                producers: 2,
                consumers: 0,
            },
            VerifyError::NonLinearVersion {
                var: local("sum", 1, f0),
                producers: 2,
                consumers: 0,
            },
        ],
        "expected Count1 before Sum1 (sorted VersionedVar order), got: {errors:?}"
    );
}

// ── ThreadingModeUnpackMismatch ──────────────────────────────────────

#[test]
fn verify_unpack_mismatch_inside_direct_params_mode() {
    let frame = FrameId::new(1);
    let ir = vec![ThreadedStmt::Threaded {
        mode: ThreadingMode::DirectParams,
        frame,
        shadow_write_eligible: true,
        body: vec![ThreadedStmt::Bind {
            target: local("sum", 1, frame),
            source: local("sum", 0, frame),
            op: BindOp::Unpack {
                field: "__local__sum".to_string(),
            },
            shadow_write: false,
            span: span(),
        }],
        produces: vec![local("sum", 1, frame)],
        span: span(),
    }];
    let errors = verify(&ir);
    assert!(
        errors.iter().any(|e| matches!(
            e,
            VerifyError::ThreadingModeUnpackMismatch {
                mode: ThreadingMode::DirectParams,
                ..
            }
        )),
        "expected ThreadingModeUnpackMismatch, got: {errors:?}"
    );
}

#[test]
fn verify_unpack_silent_inside_stateacc_mode() {
    let frame = FrameId::new(1);
    let ir = vec![ThreadedStmt::Threaded {
        mode: ThreadingMode::StateAcc(StateAccFallbackReason::SelfSendInBody),
        frame,
        shadow_write_eligible: true,
        body: vec![ThreadedStmt::Bind {
            target: local("sum", 1, frame),
            source: local("sum", 0, frame),
            op: BindOp::Unpack {
                field: "__local__sum".to_string(),
            },
            shadow_write: false,
            span: span(),
        }],
        produces: vec![local("sum", 1, frame)],
        span: span(),
    }];
    assert_eq!(verify(&ir), Vec::new());
}

// ── ShadowWriteMissing (ADR 0110 contract) ───────────────────────────

#[test]
fn verify_shadow_write_missing_fires_on_top_frame_mutation_without_shadow() {
    let f0 = FrameId::ROOT;
    let ir = vec![
        ThreadedStmt::Bind {
            target: class_var(1, f0),
            source: class_var(0, f0),
            op: BindOp::Put {
                field: "runs".to_string(),
                value: ValueRef::Var("_Val0".to_string()),
                class_tag: ValueRef::Var("ClassSelf".to_string()),
            },
            shadow_write: false, // BUG: forgot the shadow write
            span: span(),
        },
        ThreadedStmt::NlrCatch {
            boundary: NlrBoundary::ClassMethod {
                has_class_vars: true,
            },
            token: TokenId::new("NlrTokenFixtureOnly"),
            frame: f0,
            span: span(),
        },
    ];
    let errors = verify(&ir);
    assert!(
        errors.iter().any(|e| matches!(
            e,
            VerifyError::ShadowWriteMissing { mutated, .. } if *mutated == class_var(1, f0)
        )),
        "expected ShadowWriteMissing, got: {errors:?}"
    );
}

#[test]
fn verify_shadow_write_silent_on_fixed_shape() {
    // ADR 0110's actual post-fix shape: shadow_write: true.
    let f0 = FrameId::ROOT;
    let ir = vec![
        ThreadedStmt::Bind {
            target: class_var(1, f0),
            source: class_var(0, f0),
            op: BindOp::Put {
                field: "runs".to_string(),
                value: ValueRef::Var("_Val0".to_string()),
                class_tag: ValueRef::Var("ClassSelf".to_string()),
            },
            shadow_write: true,
            span: span(),
        },
        ThreadedStmt::NlrCatch {
            boundary: NlrBoundary::ClassMethod {
                has_class_vars: true,
            },
            token: TokenId::new("NlrTokenFixtureOnly"),
            frame: f0,
            span: span(),
        },
    ];
    assert_eq!(verify(&ir), Vec::new());
}

#[test]
fn verify_shadow_write_silent_without_class_var_nlr_relay() {
    // No NlrCatch with has_class_vars: true present at all — the
    // ShadowWriteMissing precondition doesn't hold, so a missing
    // shadow write is not (yet) flagged.
    let f0 = FrameId::ROOT;
    let ir = vec![ThreadedStmt::Bind {
        target: class_var(1, f0),
        source: class_var(0, f0),
        op: BindOp::Put {
            field: "runs".to_string(),
            value: ValueRef::Var("_Val0".to_string()),
            class_tag: ValueRef::Var("ClassSelf".to_string()),
        },
        shadow_write: false,
        span: span(),
    }];
    assert_eq!(verify(&ir), Vec::new());
}

#[test]
fn verify_shadow_write_missing_silent_below_top_frame() {
    // Same missing-shadow-write shape, but the wrapper is not
    // shadow-write-eligible (block_depth > 0 analogue) — not a top-frame
    // mutation, so the ADR 0110 contract doesn't apply here (matches
    // `generate_field_assignment`'s `block_depth == 0` gate). ADR 0111
    // Addendum 9, Question 1: this is now expressed via the explicit
    // `shadow_write_eligible: false` field, not via `frame != ROOT` (a
    // non-ROOT frame alone is no longer sufficient to exempt a Bind —
    // see `verify_shadow_write_missing_fires_on_non_root_eligible_frame`
    // just below for the converse case this test used to conflate).
    let f0 = FrameId::ROOT;
    let inner = FrameId::new(1);
    let ir = vec![
        ThreadedStmt::Threaded {
            mode: ThreadingMode::StateAcc(StateAccFallbackReason::None),
            frame: inner,
            shadow_write_eligible: false,
            body: vec![ThreadedStmt::Bind {
                target: VersionedVar::new(VersionPrefix::ClassVars, 1, inner),
                source: VersionedVar::new(VersionPrefix::ClassVars, 0, inner),
                op: BindOp::Put {
                    field: "runs".to_string(),
                    value: ValueRef::Var("_Val0".to_string()),
                    class_tag: ValueRef::Var("ClassSelf".to_string()),
                },
                shadow_write: false,
                span: span(),
            }],
            produces: vec![VersionedVar::new(VersionPrefix::ClassVars, 1, inner)],
            span: span(),
        },
        ThreadedStmt::NlrCatch {
            boundary: NlrBoundary::ClassMethod {
                has_class_vars: true,
            },
            token: TokenId::new("NlrTokenFixtureOnly"),
            frame: f0,
            span: span(),
        },
    ];
    assert_eq!(verify(&ir), Vec::new());
}

#[test]
fn verify_shadow_write_missing_fires_on_non_root_eligible_frame() {
    // BT-3167 (ADR 0111 Addendum 9, Question 1): the exact gap this
    // issue closes — before the widened frame model, `ShadowWriteMissing`
    // gated on `target.frame == FrameId::ROOT` alone, so a class-var
    // mutation inside ANY non-ROOT frame (every loop/fold body's real
    // `FrameId`, minted fresh by `with_branch_context`) was silently
    // exempt from the check, regardless of whether it was really
    // shadow-write-eligible. A "control-flow-only" frame — the shape
    // BT-3140's amendment describes: a loop body never increments
    // `block_depth`, so it stays semantically "the method's own top
    // level" for shadow-write purposes even though it gets a fresh,
    // non-ROOT `FrameId` for version-linearity scoping — must still be
    // caught. This synthesizes exactly that: a `Threaded` wrapper at a
    // non-ROOT `frame`, but `shadow_write_eligible: true` (the
    // independent signal this issue introduces), containing a class-var
    // `Bind` with a forgotten shadow write.
    let f0 = FrameId::ROOT;
    let control_flow_only = FrameId::new(1);
    let ir = vec![
        ThreadedStmt::Threaded {
            mode: ThreadingMode::StateAcc(StateAccFallbackReason::None),
            frame: control_flow_only,
            shadow_write_eligible: true,
            body: vec![ThreadedStmt::Bind {
                target: VersionedVar::new(VersionPrefix::ClassVars, 1, control_flow_only),
                source: VersionedVar::new(VersionPrefix::ClassVars, 0, control_flow_only),
                op: BindOp::Put {
                    field: "runs".to_string(),
                    value: ValueRef::Var("_Val0".to_string()),
                    class_tag: ValueRef::Var("ClassSelf".to_string()),
                },
                shadow_write: false, // BUG: forgot the shadow write
                span: span(),
            }],
            produces: vec![VersionedVar::new(
                VersionPrefix::ClassVars,
                1,
                control_flow_only,
            )],
            span: span(),
        },
        ThreadedStmt::NlrCatch {
            boundary: NlrBoundary::ClassMethod {
                has_class_vars: true,
            },
            token: TokenId::new("NlrTokenFixtureOnly"),
            frame: f0,
            span: span(),
        },
    ];
    let errors = verify(&ir);
    assert!(
        errors.iter().any(|e| matches!(
            e,
            VerifyError::ShadowWriteMissing { mutated, .. }
                if *mutated == VersionedVar::new(VersionPrefix::ClassVars, 1, control_flow_only)
        )),
        "expected ShadowWriteMissing for the eligible non-ROOT frame, got: {errors:?}"
    );
}

#[test]
fn verify_shadow_write_eligible_stack_and_combines_nested_frames() {
    // ADR 0111 Addendum 9, Question 1's defense-in-depth: a nested
    // `Threaded` frame's own `shadow_write_eligible: true` must NOT
    // override an ineligible ancestor — the stack ANDs with the
    // parent's current top, not replaces it. No real lowering needs
    // this today (a nested node's own `block_depth`-derived flag
    // already encodes total nesting depth by construction), but a
    // hand-built fixture exercising the combinator directly must still
    // get it right. Outer frame: `shadow_write_eligible: false`. Inner
    // frame (nested two `Threaded` levels deep): `shadow_write_eligible:
    // true`. The class-var `Bind` at the inner frame, missing its
    // shadow write, must stay SILENT — the outer `false` wins.
    let outer = FrameId::new(1);
    let inner = FrameId::new(2);
    let f0 = FrameId::ROOT;
    let ir = vec![
        ThreadedStmt::Threaded {
            mode: ThreadingMode::StateAcc(StateAccFallbackReason::None),
            frame: outer,
            shadow_write_eligible: false,
            body: vec![ThreadedStmt::Threaded {
                mode: ThreadingMode::StateAcc(StateAccFallbackReason::None),
                frame: inner,
                shadow_write_eligible: true,
                body: vec![ThreadedStmt::Bind {
                    target: VersionedVar::new(VersionPrefix::ClassVars, 1, inner),
                    source: VersionedVar::new(VersionPrefix::ClassVars, 0, inner),
                    op: BindOp::Put {
                        field: "runs".to_string(),
                        value: ValueRef::Var("_Val0".to_string()),
                        class_tag: ValueRef::Var("ClassSelf".to_string()),
                    },
                    shadow_write: false,
                    span: span(),
                }],
                produces: vec![VersionedVar::new(VersionPrefix::ClassVars, 1, inner)],
                span: span(),
            }],
            produces: vec![],
            span: span(),
        },
        ThreadedStmt::NlrCatch {
            boundary: NlrBoundary::ClassMethod {
                has_class_vars: true,
            },
            token: TokenId::new("NlrTokenFixtureOnly"),
            frame: f0,
            span: span(),
        },
    ];
    assert_eq!(
        verify(&ir),
        Vec::new(),
        "an ineligible outer frame must veto an eligible inner frame's own flag"
    );
}
