// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Dual-run byte-parity harness: pins the lowered-IR render
//! against the live generator's own hand-built output for the loop/NLR
//! shapes ADR 0111 Addendum 2 migrated.

use super::*;

// ── Dual-run byte-parity harness ───────────────────────────────────────
//
// Proves byte parity for at least direct-params and hybrid while loops
// against the legacy path. `ThreadedStmt::Threaded` does not model a loop's
// condition/exit-case (that stays AST-directed — ADR 0111 §Constraints,
// "everything else in codegen stays AST-directed and unaffected"), so
// there is no single production function today that emits *only* the
// letrec skeleton these tests check — `while_loops.rs`'s
// `generate_while_loop_direct`/`generate_while_loop_hybrid` interleave
// it with condition codegen. Each test below therefore hand-authors
// that skeleton directly against a real `CoreErlangGenerator` — via the
// SAME generator accessors `render`/`RenderCtx` themselves call
// (`fresh_temp_var`, `current_state_var`/`next_state_var`) wherever a
// production twin exists, never a re-derivation of the decision itself
// — and asserts `render(lower(..))`'s output against it byte-for-byte
// on a separately-constructed but identically-seeded generator. This is
// the harness's own proof that its rendering mechanism reproduces
// hand-authored `docvec!` output exactly; a later pilot migration is
// what first runs it against a real, refactored-to-be-swappable legacy
// call site.

#[test]
#[allow(clippy::too_many_lines)] // hand-authored dual-run fixture, ADR 0111 Addendum 2 / ADR 0118 phase 3
fn dual_run_conditional_loop_direct_params_byte_parity() {
    // ADR 0111 Addendum 2, Gap 1's own closing instruction, updated for
    // ADR 0118 phase 3: hand-author the condition/case-split
    // shape `render()`'s `ConditionalLoop` arm now produces for a PURE
    // condition (`condition: []`) — the case's scrutinee is
    // `condition_value` inlined directly, no `CondFun` closure/`apply`
    // indirection (the older shape `while_loops.rs`'s
    // `generate_while_loop_direct` still emits today, since that
    // production call site is not itself migrated to this IR by this
    // phase — see the module's own §Status doc). This proves `render()`
    // reproduces the shape ADR 0118 §Decision 6 specifies byte-for-byte;
    // `dual_run_conditional_loop_direct_params_condition_with_prelude`
    // below is the counterpart for a condition that itself has state
    // effects.
    //
    // Condition/RHS bodies are deliberately trivial, mint-free fragments
    // (not real binop codegen, which mints its own internal temps and is
    // separately tested elsewhere) — this test's job is proving the
    // SKELETON and the GENSYM'D NAMING match, not re-verifying
    // expression codegen. `Sum`-rebind/`ExitSA` are minted via the SAME
    // `fresh_temp_var` calls production makes, IN THE SAME ORDER (body
    // rebind, then the exit arm's `ExitSA` chain LAST) — an inverted
    // order would still pass this assertion by accident only if both
    // sides inverted identically, which is exactly why
    // `legacy_gen`/`render_gen` mint independently, on
    // separately-seeded generators, rather than sharing one generator's
    // counter.
    let mut legacy_gen = CoreErlangGenerator::new("dual_run_conditional_loop_direct");
    let cond_doc = docvec![
        "call 'erlang':'<'(",
        leaf::var("Sum"),
        ", ",
        leaf::int_lit(10),
        ")",
    ];
    let continue_header = docvec!["case ", cond_doc, " of ", "<'true'> when 'true' -> "];
    let sum_rebind = legacy_gen.fresh_temp_var("Sum"); // mint #1: body rebind, matching `generate_direct_var_update_in_loop`
    let body = docvec![
        "let ",
        leaf::var(sum_rebind.clone()),
        " = call 'erlang':'+'(",
        leaf::var("Sum"),
        ", ",
        leaf::int_lit(1),
        ") in ",
    ];
    let exit_sa = legacy_gen.fresh_temp_var("ExitSA"); // mint #2: exit arm, LAST — matching `generate_exit_stateacc`
    let exit_arm = docvec![
        "<'false'> when 'true' -> let ",
        leaf::var(exit_sa.clone()),
        " = call 'maps':'put'(",
        leaf::atom("__local__sum"),
        ", ",
        leaf::var("Sum"),
        ", State) in {'nil', ",
        leaf::var(exit_sa),
        "} end ",
    ];
    let legacy_doc = docvec![
        "letrec ",
        leaf::fname("while", 1),
        " = fun (",
        leaf::var("Sum"),
        ") -> ",
        continue_header,
        body,
        " apply ",
        leaf::fname("while", 1),
        " (",
        leaf::var(sum_rebind),
        ") ",
        exit_arm,
        "in apply ",
        leaf::fname("while", 1),
        " (",
        leaf::var("Sum"),
        ")",
    ]
    .to_pretty_string();

    // `render(..)` side: a real `ConditionalLoop` node, built on a
    // separately-constructed but identically-seeded generator — the
    // opaque `continue_arm`/`exit_arm` fields and the body's `Bind`
    // are minted via the SAME calls, in the SAME order, mirroring
    // exactly how the lowering pass would build them (Addendum 2's
    // ordering contract: body rebinds, then exit_arm LAST — condition
    // is empty here, so it mints nothing).
    let mut render_gen = CoreErlangGenerator::new("dual_run_conditional_loop_direct");
    let ir_cond_doc = docvec![
        "call 'erlang':'<'(",
        leaf::var("Sum"),
        ", ",
        leaf::int_lit(10),
        ")",
    ];
    let ir_continue_arm = Document::Str("<'true'> when 'true' -> ");
    let frame = FrameId::new(1);
    let sum_v0 = local("sum", 0, frame);
    let ir_sum_rebind = render_gen.fresh_temp_var("Sum");
    let sum_v1 = VersionedVar::new(VersionPrefix::Gensym(ir_sum_rebind), 1, frame);
    let value_doc = docvec![
        "call 'erlang':'+'(",
        leaf::var("Sum"),
        ", ",
        leaf::int_lit(1),
        ")",
    ];
    let ir_body = vec![ThreadedStmt::Bind {
        target: sum_v1,
        source: sum_v0.clone(),
        op: BindOp::Direct(ValueRef::Doc(value_doc)),
        shadow_write: false,
        span: span(),
    }];
    let ir_exit_sa = render_gen.fresh_temp_var("ExitSA");
    let ir_exit_arm = docvec![
        "<'false'> when 'true' -> let ",
        leaf::var(ir_exit_sa.clone()),
        " = call 'maps':'put'(",
        leaf::atom("__local__sum"),
        ", ",
        leaf::var("Sum"),
        ", State) in {'nil', ",
        leaf::var(ir_exit_sa),
        "} end ",
    ];
    let ir = vec![ThreadedStmt::ConditionalLoop {
        fn_name: "while".to_string(),
        mode: ThreadingMode::DirectParams,
        frame,
        shadow_write_eligible: true,
        counter: None,
        condition: Vec::new(),
        condition_value: ValueRef::Doc(ir_cond_doc),
        continue_arm: ir_continue_arm,
        body: ir_body,
        produces: vec![sum_v0],
        exit_arm: ir_exit_arm,
        span: span(),
    }];
    let mut ctx = RenderCtx::new(&mut render_gen);
    let rendered_doc = render(&ir, &mut ctx).to_pretty_string();

    assert_eq!(
        rendered_doc, legacy_doc,
        "render(..)'s ConditionalLoop arm must reproduce the ADR 0118 \
         phase 3 direct-params while-loop condition/case-split shape \
         byte-for-byte"
    );
}

#[test]
#[allow(clippy::too_many_lines)] // hand-authored dual-run fixture, ADR 0118 phase 3
fn dual_run_conditional_loop_direct_params_condition_with_prelude() {
    // ADR 0118 phase 3: the counterpart of the byte-parity
    // test above for a condition that itself has a prelude (e.g. a
    // self-send's own `State` advance, `while_loops.rs`'s
    // `generate_stateful_while_condition_tail`'s bare-self-send arm) —
    // `render()` must emit that prelude INSIDE the loop's own `fun`,
    // ahead of the `case`, in the SAME frame `body` uses, so a `Bind`
    // in `condition` and one in `body` can both reference the loop's
    // own per-iteration params.
    let mut generator = CoreErlangGenerator::new("dual_run_conditional_loop_direct_prelude");
    let frame = FrameId::new(1);
    let sum_v0 = local("sum", 0, frame);
    let state_v0 = VersionedVar::new(VersionPrefix::State, 0, frame);
    let state_v1 = VersionedVar::new(VersionPrefix::State, 1, frame);
    let sd_var = generator.fresh_temp_var("SD"); // mint #1: condition's self-send dispatch temp
    let condition = vec![
        ThreadedStmt::Statement(
            docvec![
                "let ",
                leaf::var(sd_var.clone()),
                " = call 'test':'safe_dispatch'('bumpCount', [], ",
                leaf::var("Sum"),
                ") in ",
            ],
            span(),
        ),
        ThreadedStmt::Bind {
            target: state_v1.clone(),
            source: state_v0,
            op: BindOp::Direct(ValueRef::Doc(docvec![
                "call 'erlang':'element'(2, ",
                leaf::var(sd_var.clone()),
                ")",
            ])),
            shadow_write: false,
            span: span(),
        },
    ];
    let condition_value = ValueRef::Doc(docvec![
        "call 'erlang':'>'(call 'erlang':'element'(1, ",
        leaf::var(sd_var),
        "), 0)",
    ]);
    let sum_rebind = generator.fresh_temp_var("Sum"); // mint #2: body rebind
    let body = vec![ThreadedStmt::Bind {
        target: VersionedVar::new(VersionPrefix::Gensym(sum_rebind.clone()), 1, frame),
        source: sum_v0.clone(),
        op: BindOp::Direct(ValueRef::Doc(docvec![
            "call 'erlang':'+'(",
            leaf::var("Sum"),
            ", 1)",
        ])),
        shadow_write: false,
        span: span(),
    }];
    let exit_arm = Document::Str("<'false'> when 'true' -> {'nil', Sum} end ");
    let ir = vec![ThreadedStmt::ConditionalLoop {
        fn_name: "while".to_string(),
        mode: ThreadingMode::DirectParams,
        frame,
        shadow_write_eligible: true,
        counter: None,
        condition,
        condition_value,
        continue_arm: Document::Str("<'true'> when 'true' -> "),
        body,
        produces: vec![sum_v0],
        exit_arm,
        span: span(),
    }];
    let mut ctx = RenderCtx::new(&mut generator);
    let rendered_doc = render(&ir, &mut ctx).to_pretty_string();

    assert!(
        rendered_doc.starts_with("letrec 'while'/1 = fun (Sum) -> let _SD1"),
        "the condition's own prelude must render INSIDE the loop's fun, \
         before the case. Got:\n{rendered_doc}"
    );
    assert!(
        rendered_doc.contains("let State1 = call 'erlang':'element'(2, _SD1"),
        "the condition's self-send Bind must be real, rendered IR. Got:\n{rendered_doc}"
    );
    assert!(
        rendered_doc.contains("case call 'erlang':'>'(call 'erlang':'element'(1, _SD1"),
        "the case scrutinee must be condition_value, inlined directly \
         (no CondFun closure). Got:\n{rendered_doc}"
    );
}

#[test]
#[allow(clippy::too_many_lines)] // hand-authored dual-run fixture, ADR 0111 Addendum 2 / ADR 0118 phase 3
fn dual_run_conditional_loop_hybrid_state_prefix_matches_live_generator() {
    // Hybrid-mode counterpart of the direct-params test above: proves
    // BOTH that `ConditionalLoop`'s condition/case-split shape (ADR 0118
    // phase 3: `condition_value` inlined directly, no `CondFun`
    // closure) renders correctly under Hybrid loop context, AND
    // (mirroring the pre-Addendum-2 test this replaces) that a
    // `State`-prefixed `Bind` nested in the body (e.g. a nested
    // construct's field mutation running inside the hybrid loop body)
    // resolves through the REAL production accessors
    // `current_state_var`/`next_state_var` under hybrid loop context —
    // not a hand-copy of their logic — so `RenderCtx::resolve_prefix`
    // (via the shared `render_state_prefix` helper both paths call)
    // matches live generator behavior bit-for-bit even when interleaved
    // with the condition/exit scaffolding.
    let mut legacy_gen = CoreErlangGenerator::new("dual_run_conditional_loop_hybrid");
    legacy_gen.in_hybrid_loop = true;
    legacy_gen.in_loop_body = true;
    let cond_doc = docvec![
        "call 'erlang':'<'(",
        leaf::var("Sum"),
        ", ",
        leaf::int_lit(10),
        ")"
    ];
    let continue_header = docvec!["case ", cond_doc, " of ", "<'true'> when 'true' -> "];
    let sum_rebind = legacy_gen.fresh_temp_var("Sum");
    let state_source_name = legacy_gen.current_state_var();
    let state_target_name = legacy_gen.next_state_var();
    let body = docvec![
        "let ",
        leaf::var(sum_rebind.clone()),
        " = call 'erlang':'+'(",
        leaf::var("Sum"),
        ", ",
        leaf::int_lit(1),
        ") in ",
        " ", // BodyKind::Letrec's inter-statement separator (generate_threaded_loop_body_inner)
        "let ",
        leaf::var(state_target_name),
        " = ",
        leaf::var(state_source_name),
        " in ",
    ];
    legacy_gen.in_hybrid_loop = false;
    legacy_gen.in_loop_body = false;
    let exit_sa = legacy_gen.fresh_temp_var("ExitSA");
    let exit_arm = docvec![
        "<'false'> when 'true' -> let ",
        leaf::var(exit_sa.clone()),
        " = call 'maps':'put'(",
        leaf::atom("__local__sum"),
        ", ",
        leaf::var("Sum"),
        ", State) in {'nil', ",
        leaf::var(exit_sa),
        "} end ",
    ];
    let legacy_doc = docvec![
        "letrec ",
        leaf::fname("while", 1),
        " = fun (",
        leaf::var("Sum"),
        ") -> ",
        continue_header,
        body,
        " apply ",
        leaf::fname("while", 1),
        " (",
        leaf::var(sum_rebind),
        ") ",
        exit_arm,
        "in apply ",
        leaf::fname("while", 1),
        " (",
        leaf::var("Sum"),
        ")",
    ]
    .to_pretty_string();

    let mut render_gen = CoreErlangGenerator::new("dual_run_conditional_loop_hybrid");
    let ir_cond_doc = docvec![
        "call 'erlang':'<'(",
        leaf::var("Sum"),
        ", ",
        leaf::int_lit(10),
        ")"
    ];
    let ir_continue_arm = Document::Str("<'true'> when 'true' -> ");
    let frame = FrameId::new(1);
    let sum_v0 = local("sum", 0, frame);
    let ir_sum_rebind = render_gen.fresh_temp_var("Sum");
    let sum_v1 = VersionedVar::new(VersionPrefix::Gensym(ir_sum_rebind), 1, frame);
    let value_doc = docvec![
        "call 'erlang':'+'(",
        leaf::var("Sum"),
        ", ",
        leaf::int_lit(1),
        ")",
    ];
    let ir_body = vec![
        ThreadedStmt::Bind {
            target: sum_v1,
            source: sum_v0.clone(),
            op: BindOp::Direct(ValueRef::Doc(value_doc)),
            shadow_write: false,
            span: span(),
        },
        ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::State, 1, frame),
            source: VersionedVar::new(VersionPrefix::State, 0, frame),
            op: BindOp::Direct(ValueRef::Version(VersionedVar::new(
                VersionPrefix::State,
                0,
                frame,
            ))),
            shadow_write: false,
            span: span(),
        },
    ];
    let ir_exit_sa = render_gen.fresh_temp_var("ExitSA");
    let ir_exit_arm = docvec![
        "<'false'> when 'true' -> let ",
        leaf::var(ir_exit_sa.clone()),
        " = call 'maps':'put'(",
        leaf::atom("__local__sum"),
        ", ",
        leaf::var("Sum"),
        ", State) in {'nil', ",
        leaf::var(ir_exit_sa),
        "} end ",
    ];
    let ir = vec![ThreadedStmt::ConditionalLoop {
        fn_name: "while".to_string(),
        mode: ThreadingMode::Hybrid,
        frame,
        shadow_write_eligible: true,
        counter: None,
        condition: Vec::new(),
        condition_value: ValueRef::Doc(ir_cond_doc),
        continue_arm: ir_continue_arm,
        body: ir_body,
        produces: vec![sum_v0],
        exit_arm: ir_exit_arm,
        span: span(),
    }];
    let mut ctx = RenderCtx::new(&mut render_gen);
    let rendered_doc = render(&ir, &mut ctx).to_pretty_string();

    assert_eq!(
        rendered_doc, legacy_doc,
        "render(..)'s ConditionalLoop arm must reproduce the real hybrid \
         while-loop condition/case-split shape byte-for-byte, including \
         the loop-context-aware State prefix"
    );
}

#[test]
fn render_loop_letrec_param_list_and_final_args_use_hybrid_context_even_when_nested_in_stateacc_ambient()
 {
    // Regression test for `render_loop_letrec`'s loop-context ordering:
    // the `fun (<param_list>) -> <body_doc> apply 'FnName'/N (<final_args>)`
    // declaration, its body, and its recursive tail call are all the
    // SAME `fun` block, so `param_list` and `final_args` must resolve
    // `VersionPrefix::State` under the identical Hybrid context
    // `body_doc` renders under — computing either of them under the
    // pre-loop ambient context instead would name the `fun`'s declared
    // parameter differently from what the body/recursive-call actually
    // reference, producing invalid Core Erlang (a reference to a
    // variable the `fun` never bound). The OUTER initial call remains
    // the one place `produces` correctly resolves under ambient context
    // — it's the calling scope's own reference, not the `fun`'s.
    //
    // Simulates a Hybrid loop nested inside an outer `StateAcc`-mode
    // loop body (`in_loop_body = true, in_hybrid_loop = false` ambient —
    // the exact shape that would trip this bug) to prove the `fun`
    // declaration/body/recursive-call all agree on `State`/`State1`
    // (the Hybrid loop's own context), while only the outer call uses
    // `StateAcc` (the ambient it's actually invoked from).
    let frame = FrameId::new(1);
    let ir = vec![ThreadedStmt::Threaded {
        mode: ThreadingMode::Hybrid,
        frame,
        shadow_write_eligible: true,
        body: vec![ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::State, 1, frame),
            source: VersionedVar::new(VersionPrefix::State, 0, frame),
            op: BindOp::Direct(ValueRef::Version(VersionedVar::new(
                VersionPrefix::State,
                0,
                frame,
            ))),
            shadow_write: false,
            span: span(),
        }],
        produces: vec![VersionedVar::new(VersionPrefix::State, 1, frame)],
        span: span(),
    }];
    let mut render_gen = CoreErlangGenerator::new("dual_run_hybrid_nested");
    // Ambient context as if this `Threaded` node sits inside an outer
    // StateAcc-mode loop body — the pre-existing, pre-fix bug would have
    // computed `param_list`/`final_args` under exactly this (wrong)
    // ambient.
    render_gen.in_loop_body = true;
    render_gen.in_hybrid_loop = false;
    let mut ctx = RenderCtx::new(&mut render_gen);
    let rendered = render(&ir, &mut ctx).to_pretty_string();

    assert_eq!(
        rendered,
        "letrec '_Loop1'/1 = fun (State) -> let State1 = State in \
         apply '_Loop1'/1 (State1) in apply '_Loop1'/1 (StateAcc)",
        "the fun declaration, body, and recursive tail call must all \
         agree on State/State1 (the Hybrid loop's own context) — only \
         the outer initial call should reference StateAcc (the outer \
         StateAcc ambient) — got: {rendered}"
    );
}

#[test]
fn dual_run_nlr_catch_reuses_wrap_body_with_nlr_catch_verbatim() {
    // NLR-catch rendering doesn't re-derive `wrap_body_with_nlr_catch`'s
    // scaffolding — it calls the exact same function real NLR call
    // sites use, so this test's "legacy" and `render(lower(..))` sides
    // are provably byte-identical by construction, not by luck. Kept as
    // an explicit dual-run test (rather than relying on that reasoning
    // alone) so a future refactor that breaks the direct-reuse
    // invariant fails loudly here instead of silently drifting.
    //
    // ADR 0111 Addendum 4 §Gap 3: the render side mints its token via
    // `fresh_temp_var` BEFORE building the IR — production's real mint
    // position — and carries the name in `TokenId`. Both sides now
    // consume counter slot 0 for the token and slots 1.. for the catch
    // vars, so an accidental reintroduction of render-time token
    // minting (which would allocate the token AFTER the catch vars'
    // relative position changed) shifts every `Nlr*` temp number and
    // fails this assertion.
    let mut legacy_gen = CoreErlangGenerator::new("dual_run_nlr");
    let legacy_token = legacy_gen.fresh_temp_var("NlrToken");
    let inner_body = docvec!["let ", leaf::var("Sum1"), " = ", leaf::var("Sum"), " in "];
    let legacy_doc = legacy_gen
        .wrap_body_with_nlr_catch(inner_body, &legacy_token, NlrBoundary::ActorReply)
        .to_pretty_string();

    let mut render_gen = CoreErlangGenerator::new("dual_run_nlr");
    let render_token = render_gen.fresh_temp_var("NlrToken");
    let frame = FrameId::new(1);
    let ir = vec![
        ThreadedStmt::NlrCatch {
            boundary: NlrBoundary::ActorReply,
            token: TokenId::new(render_token),
            frame,
            span: span(),
        },
        ThreadedStmt::Bind {
            target: local("sum", 1, frame),
            source: local("sum", 0, frame),
            op: BindOp::Direct(ValueRef::Version(local("sum", 0, frame))),
            shadow_write: false,
            span: span(),
        },
    ];
    let mut ctx = RenderCtx::new(&mut render_gen);
    let rendered_doc = render(&ir, &mut ctx).to_pretty_string();

    assert_eq!(rendered_doc, legacy_doc);
}
