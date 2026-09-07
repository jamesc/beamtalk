// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `ThreadedValue::close` (ADR 0118 §Decision 5) coverage.

use super::*;

// ── ThreadedValue::close (ADR 0118 §Decision 5) ────────────────────────

fn self_send_prelude(dispatch_var: &str) -> Vec<ThreadedStmt> {
    vec![
        ThreadedStmt::Statement(
            docvec![
                "let ",
                leaf::var(dispatch_var.to_string()),
                " = call 'm':'safe_dispatch'('bump', [], State) in ",
            ],
            span(),
        ),
        ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::State, 1, FrameId::ROOT),
            source: VersionedVar::new(VersionPrefix::State, 0, FrameId::ROOT),
            op: BindOp::Direct(ValueRef::Doc(docvec![
                "call 'erlang':'element'(2, ",
                leaf::var(dispatch_var.to_string()),
                ")",
            ])),
            shadow_write: false,
            span: span(),
        },
    ]
}

#[test]
fn close_with_empty_prelude_is_the_bare_value() {
    let mut generator = CoreErlangGenerator::new("close_empty_prelude");
    let tv = ThreadedValue::pure(ValueRef::Var("_Val0".to_string()));
    let mut ctx = RenderCtx::new(&mut generator);
    let (doc, errors) = tv.close(&mut ctx, CloseContext::Opaque);
    assert_eq!(doc.to_pretty_string(), "_Val0");
    assert!(
        errors.is_empty(),
        "a pure value has nothing to escape: {errors:?}"
    );
}

#[test]
fn close_reports_a_state_bind_the_context_cannot_thread() {
    let mut generator = CoreErlangGenerator::new("close_opaque_reports");
    let tv = ThreadedValue {
        prelude: self_send_prelude("_SD0"),
        value: ValueRef::Doc(docvec!["call 'erlang':'element'(1, _SD0)"]),
    };
    let mut ctx = RenderCtx::new(&mut generator);
    let (doc, errors) = tv.close(&mut ctx, CloseContext::Opaque);
    assert_eq!(
        doc.to_pretty_string(),
        "let _SD0 = call 'm':'safe_dispatch'('bump', [], State) in \
         let State1 = call 'erlang':'element'(2, _SD0) in \
         call 'erlang':'element'(1, _SD0)",
        "close() must render the prelude as nested lets around the value"
    );
    assert_eq!(
        errors,
        vec![VerifyError::StateEffectEscapesExpression {
            prefix: VersionPrefix::State,
            at: span(),
        }],
        "the State Bind is scoped away by the closed let — an escape"
    );
}

#[test]
fn close_is_silent_when_the_context_threads_state() {
    let mut generator = CoreErlangGenerator::new("close_threads_state_silent");
    let tv = ThreadedValue {
        prelude: self_send_prelude("_SD0"),
        value: ValueRef::Doc(docvec!["{call 'erlang':'element'(1, _SD0), State1}"]),
    };
    let mut ctx = RenderCtx::new(&mut generator);
    let (doc, errors) = tv.close(&mut ctx, CloseContext::ThreadsState);
    assert!(
        doc.to_pretty_string()
            .ends_with("{call 'erlang':'element'(1, _SD0), State1}"),
        "same rendering as the Opaque close: {}",
        doc.to_pretty_string()
    );
    assert!(
        errors.is_empty(),
        "a context that threads State itself has nothing escaping: {errors:?}"
    );
}

#[test]
fn close_does_not_report_binds_nested_inside_a_threaded_node() {
    // A `Threaded` node in the prelude threads its own Binds to its own
    // `{Value, State}` result; only top-level Binds escape the close.
    let mut generator = CoreErlangGenerator::new("close_nested_threaded_silent");
    let frame = FrameId::new(1);
    let tv = ThreadedValue {
        prelude: vec![ThreadedStmt::Threaded {
            mode: ThreadingMode::StateAcc(StateAccFallbackReason::None),
            frame,
            shadow_write_eligible: true,
            body: vec![ThreadedStmt::Bind {
                target: VersionedVar::new(VersionPrefix::State, 1, frame),
                source: VersionedVar::new(VersionPrefix::State, 0, frame),
                op: BindOp::Direct(ValueRef::Literal("'_'")),
                shadow_write: false,
                span: span(),
            }],
            produces: vec![],
            span: span(),
        }],
        value: ValueRef::Literal("'nil'"),
    };
    let mut ctx = RenderCtx::new(&mut generator);
    let (_doc, errors) = tv.close(&mut ctx, CloseContext::Opaque);
    assert!(
        errors.is_empty(),
        "nested-frame Binds are not escapes: {errors:?}"
    );
}

#[test]
fn value_is_trivial_only_for_bare_vars_and_literals() {
    assert!(ThreadedValue::pure(ValueRef::Var("_Tmp0".to_string())).value_is_trivial());
    assert!(ThreadedValue::pure(ValueRef::Literal("'nil'")).value_is_trivial());
    assert!(
        !ThreadedValue::pure(ValueRef::Doc(docvec!["call 'lists':'nth'(1, L)"])).value_is_trivial(),
        "an opaque computation may raise: it must be temp-bound before a later prelude"
    );
}
