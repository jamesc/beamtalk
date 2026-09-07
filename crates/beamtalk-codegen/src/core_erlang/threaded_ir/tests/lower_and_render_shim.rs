// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `lower_and_render` (test shim) coverage of `render()`'s `BindOp`/
//! `ThreadedStmt` variants.

use super::*;

// ── lower_and_render (test shim) ─────────────────────────────────────

#[test]
fn lower_and_render_put_renders_maps_put_chain() {
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
    let rendered = lower_and_render(&ir).to_pretty_string();
    assert!(
        rendered.contains("let ClassVars1 = call 'maps':'put'('runs', _Val0, ClassVars) in"),
        "got: {rendered}"
    );
}

#[test]
fn lower_and_render_put_with_shadow_write_appends_erlang_put() {
    let f0 = FrameId::ROOT;
    let ir = vec![ThreadedStmt::Bind {
        target: class_var(1, f0),
        source: class_var(0, f0),
        op: BindOp::Put {
            field: "runs".to_string(),
            value: ValueRef::Var("_Val0".to_string()),
            class_tag: ValueRef::Var("ClassSelf".to_string()),
        },
        shadow_write: true,
        span: span(),
    }];
    let rendered = lower_and_render(&ir).to_pretty_string();
    assert!(
        rendered.contains(
            "call 'erlang':'put'({'$bt_class_vars_shadow', call 'erlang':'element'(2, ClassSelf)}, ClassVars1) in"
        ),
        "got: {rendered}"
    );
}

#[test]
fn lower_and_render_unpack_renders_maps_get() {
    let frame = FrameId::new(1);
    let ir = vec![ThreadedStmt::Bind {
        target: local("sum", 1, frame),
        source: local("sum", 0, frame),
        op: BindOp::Unpack {
            field: "__local__sum".to_string(),
        },
        shadow_write: false,
        span: span(),
    }];
    let rendered = lower_and_render(&ir).to_pretty_string();
    assert_eq!(
        rendered,
        "let Sum1 = call 'maps':'get'('__local__sum', Sum) in "
    );
}

#[test]
fn lower_and_render_direct_renders_plain_let() {
    let f0 = FrameId::ROOT;
    let ir = vec![ThreadedStmt::Bind {
        target: local("sum", 1, f0),
        source: local("sum", 0, f0),
        op: BindOp::Direct(ValueRef::Var("Sum0".to_string())),
        shadow_write: false,
        span: span(),
    }];
    assert_eq!(
        lower_and_render(&ir).to_pretty_string(),
        "let Sum1 = Sum0 in "
    );
}

#[test]
fn lower_and_render_return_renders_tuple() {
    let ir = vec![ThreadedStmt::Return(
        ValueRef::Literal("'nil'"),
        VersionedVar::new(VersionPrefix::State, 2, FrameId::ROOT),
        span(),
    )];
    assert_eq!(lower_and_render(&ir).to_pretty_string(), "{'nil', State2}");
}

#[test]
fn lower_and_render_threaded_direct_params_emits_real_letrec() {
    // `Threaded { mode: DirectParams, .. }` renders a real `letrec`
    // (fresh-named via `fresh_temp_var("Loop")`, hence the `_Loop1` — see
    // `VariableContext::fresh_var`), not a flattened body.
    let frame = FrameId::new(1);
    let sum_source = local("sum", 0, frame);
    let ir = vec![ThreadedStmt::Threaded {
        mode: ThreadingMode::DirectParams,
        frame,
        shadow_write_eligible: true,
        body: vec![ThreadedStmt::Bind {
            target: local("sum", 1, frame),
            source: sum_source.clone(),
            op: BindOp::Direct(ValueRef::Version(sum_source)),
            shadow_write: false,
            span: span(),
        }],
        produces: vec![local("sum", 1, frame)],
        span: span(),
    }];
    let rendered = lower_and_render(&ir).to_pretty_string();
    assert_eq!(
        rendered,
        "letrec '_Loop1'/1 = fun (Sum) -> let Sum1 = Sum in \
         apply '_Loop1'/1 (Sum1) in apply '_Loop1'/1 (Sum)"
    );
}
