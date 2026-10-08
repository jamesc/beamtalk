// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `ThreadedIr` -> Core Erlang [`Document`] emission. [`render`]
//! and its helpers reach the live generator only through [`RenderCtx`]'s
//! three narrow accessors, never through a raw field or an AST-directed
//! emission path. Depends only on [`super::ir`]; nothing here verifies IR.

use super::super::{CoreErlangGenerator, NlrBoundary};
use super::ir::{
    AccParam, BindOp, CarrierSlot, CatchClause, CatchStep, FrameId, LoopCounter, NlrThrowShape,
    OnDoCatchVars, RebindLowering, ThreadedStmt, ThreadingMode, ValueRef, VersionPrefix,
    VersionedVar,
};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, join, leaf};

// ─── RenderCtx (ADR 0111 §Addendum "Renderer design sketch") ──────

/// Loop-context flags captured/restored around rendering a nested
/// [`ThreadedStmt::Threaded`] body — see [`RenderCtx::with_loop_context`].
/// Mirrors `CoreErlangGenerator::in_loop_body`/`in_hybrid_loop` exactly (a
/// third, boolean-pair copy would be a duplication CLAUDE.md's
/// no-duplicate-implementations rule forbids; this type only names the pair
/// [`RenderCtx`] threads, it never becomes a second source of truth for
/// their values — [`RenderCtx::with_loop_context`] always reads/writes them
/// through the wrapped generator).
#[derive(Debug, Clone, Copy)]
struct LoopContextFlags {
    in_loop_body: bool,
    in_hybrid_loop: bool,
}

/// A narrow, purpose-built borrow of [`CoreErlangGenerator`]'s
/// rendering-time facilities (ADR 0111 §Addendum, "Renderer design
/// sketch") — deliberate god-object containment, NOT `&mut
/// CoreErlangGenerator` used wholesale: [`render`] and its helpers only
/// ever reach the generator through the three methods below, never through
/// a raw field or an AST-directed emission path.
///
/// Wraps `&mut CoreErlangGenerator` directly rather than a narrow trait
/// (the alternative the Addendum explicitly left open to this issue).
/// Chosen because it lets NLR-catch rendering reuse
/// [`CoreErlangGenerator::wrap_body_with_nlr_catch`] verbatim — the exact
/// function every real NLR try/catch in the codebase already goes through
/// — instead of re-deriving its scaffolding a second time (CLAUDE.md: "A
/// rule crossing the Rust/Erlang boundary needs a shared conformance
/// fixture or code generation, not a comment"; the same logic applies
/// within Rust here — reuse beats a parallel copy that could drift). The
/// trade-off this accepts is the trait form's unit-testing convenience
/// (constructing a `RenderCtx` without a full generator instance); since
/// [`CoreErlangGenerator::new`] is cheap (no I/O, pure data-structure
/// init), [`lower_and_render`] pays that cost directly instead.
pub(in crate::core_erlang) struct RenderCtx<'g> {
    generator: &'g mut CoreErlangGenerator,
}

impl<'g> RenderCtx<'g> {
    pub(in crate::core_erlang) fn new(generator: &'g mut CoreErlangGenerator) -> Self {
        Self { generator }
    }

    /// Fresh-variable allocation for `letrec` loop-function names and NLR
    /// token variables — delegates to
    /// [`CoreErlangGenerator::fresh_temp_var`], the single canonical
    /// allocator (naming stays centralized, never re-derived).
    fn fresh_temp_var(&mut self, base: &str) -> String {
        self.generator.fresh_temp_var(base)
    }

    /// Resolves `var`'s render-time name, honoring loop context for
    /// `VersionPrefix::State` (`StateN` vs `StateAccN`) exactly as
    /// [`CoreErlangGenerator::current_state_var`]/`next_state_var` do — both
    /// paths call the same [`super::super::render_state_prefix`] helper, so this
    /// can never independently drift from live-generator rendering. Every
    /// other prefix is context-independent and renders through
    /// [`VersionedVar::render_name`] unchanged.
    fn resolve_prefix(&self, var: &VersionedVar) -> String {
        match &var.prefix {
            VersionPrefix::State => super::super::render_state_prefix(
                self.generator.loop_mode.in_hybrid_loop,
                self.generator.in_loop_body,
                var.version,
            ),
            VersionPrefix::SelfVt | VersionPrefix::Local(_) | VersionPrefix::Gensym(_) => {
                var.render_name()
            }
        }
    }

    /// Runs `f` with `in_loop_body`/`in_hybrid_loop` set to `flags`,
    /// unconditionally restoring the previous values afterward — the
    /// render-time counterpart of
    /// [`CoreErlangGenerator::enter_branch_context`]'s save/restore
    /// discipline, narrowed to just the two loop-context flags
    /// [`Self::resolve_prefix`] reads (ADR 0111 §Addendum: "loop-context
    /// `StateAcc`/`State` prefix selection... decided at render time").
    ///
    /// RAII-restored via [`LoopContextGuard`]'s `Drop` impl — not a plain
    /// save/set/call/restore sequence — so a panic inside `f` still leaves
    /// `self.generator`'s flags correctly restored, matching
    /// [`CoreErlangGenerator::enter_branch_context`]'s own panic-safety
    /// guarantee (`BranchContextGuard`) rather than merely resembling it.
    fn with_loop_context<T>(
        &mut self,
        flags: LoopContextFlags,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let guard = LoopContextGuard::enter(self, flags);
        f(guard.ctx)
    }
}

/// RAII guard restoring [`CoreErlangGenerator::in_loop_body`]/
/// `in_hybrid_loop` to their pre-[`RenderCtx::with_loop_context`] values on
/// drop — including on unwind, mirroring [`BranchContextGuard`]'s
/// panic-safety discipline (this module's render path is currently
/// infallible, but a guard costs nothing extra and keeps the two save/
/// restore mechanisms in this file at parity instead of one silently being
/// weaker than the other it's explicitly modeled after).
struct LoopContextGuard<'a, 'g> {
    ctx: &'a mut RenderCtx<'g>,
    saved: LoopContextFlags,
}

impl<'a, 'g> LoopContextGuard<'a, 'g> {
    fn enter(ctx: &'a mut RenderCtx<'g>, flags: LoopContextFlags) -> Self {
        let saved = LoopContextFlags {
            in_loop_body: ctx.generator.in_loop_body,
            in_hybrid_loop: ctx.generator.loop_mode.in_hybrid_loop,
        };
        ctx.generator.in_loop_body = flags.in_loop_body;
        ctx.generator.loop_mode.in_hybrid_loop = flags.in_hybrid_loop;
        Self { ctx, saved }
    }
}

impl Drop for LoopContextGuard<'_, '_> {
    fn drop(&mut self) {
        self.ctx.generator.in_loop_body = self.saved.in_loop_body;
        self.ctx.generator.loop_mode.in_hybrid_loop = self.saved.in_hybrid_loop;
    }
}

// ─── render: full-fidelity ThreadedIr -> Document ────────────────

/// Renders `ir` to a [`Document`], full-fidelity for `Bind`, `Return`,
/// `TupleAccUnpack`, `NlrCatch`, and `Threaded` under every
/// [`ThreadingMode`] — real `letrec`/try-catch scaffolding for
/// `DirectParams`/`Hybrid`, and (ADR 0111 Addendum 15's Foldl migration) a
/// real merged unpack+body+epilogue sequence for `TupleAcc`/`StateAcc`, not
/// an earlier partial skeleton. See the module docs §Status for the full
/// per-shape history.
///
/// An [`ThreadedStmt::NlrCatch`] node has no `body` field of its own by
/// design (module docs on the variant): it models the true
/// `wrap_body_with_nlr_catch` call site, whose `body_doc` is "everything
/// that follows," so this function treats it as a boundary marker that
/// consumes the REST of `ir` at its own list position as its try-body, then
/// returns — nothing after an `NlrCatch` renders a second time outside the
/// wrap.
///
/// Has real (non-test) production callers — every later `ThreadedIr`
/// migration this module's §Status log records (conditionals, exception
/// handling, `gen_server` state threading, class-var/NLR routing) renders
/// through this function; see module docs §Status for the full history.
/// The FIRST such caller was `control_flow::while_loops`'s
/// `try_render_while_direct_via_threaded_ir` pilot (gated behind
/// `BEAMTALK_THREADED_IR_WHILE_DIRECT=1`), later deleted — see the module
/// docs §Status / ADR 0111 Addendum 13.
pub(in crate::core_erlang) fn render(
    ir: &[ThreadedStmt],
    ctx: &mut RenderCtx,
) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::new();
    for (i, stmt) in ir.iter().enumerate() {
        match stmt {
            ThreadedStmt::Bind {
                target,
                source,
                op,
                span: _,
            } => docs.push(render_bind(target, source, op, ctx)),
            ThreadedStmt::Threaded {
                mode,
                frame,
                body,
                produces,
                span: _,
            } => docs.push(render_threaded(mode, *frame, body, produces, ctx)),
            ThreadedStmt::NlrCatch {
                boundary, token, ..
            } => {
                let body_doc = render(&ir[i + 1..], ctx);
                docs.push(render_nlr_catch(*boundary, token.name(), body_doc, ctx));
                return Document::Vec(docs);
            }
            ThreadedStmt::Return(value, state, _) => docs.push(render_return(value, state, ctx)),
            ThreadedStmt::TupleAccUnpack {
                param,
                gate_slots,
                targets,
                ..
            } => docs.push(render_tuple_acc_unpack(param, *gate_slots, targets)),
            ThreadedStmt::ConditionalLoop {
                fn_name,
                mode,
                frame,
                counter,
                condition,
                condition_value,
                continue_arm,
                body,
                produces,
                outer_args,
                exit_arm,
                span: _,
            } => docs.push(render_conditional_loop(
                fn_name,
                mode,
                *frame,
                counter.as_ref(),
                condition,
                condition_value,
                continue_arm,
                body,
                produces,
                outer_args.as_deref(),
                exit_arm,
                ctx,
            )),
            // ADR 0111 Addendum 4: opaque AST-directed statement, emitted
            // verbatim. The doc carries its own trailing glue; this loop
            // adds no separator, and rendering mints nothing.
            ThreadedStmt::Statement(doc, _) => docs.push(doc.clone()),
            ThreadedStmt::OnDoCatch { vars, clauses, .. } => {
                docs.push(render_on_do_catch(vars, clauses));
            }
            ThreadedStmt::ConstructTuple { carrier, doc, .. } => docs.push(docvec![
                "let ",
                leaf::var(carrier.clone()),
                " = ",
                doc.clone(),
                " in ",
            ]),
            ThreadedStmt::LocalRebind {
                carrier,
                slot,
                value_var,
                state_first,
                lowering,
                ..
            } => docs.push(render_local_rebind(
                carrier,
                slot,
                value_var,
                state_first.as_ref(),
                lowering,
                ctx,
            )),
            // ADR 0131 §2: the method root's one legitimate drop. The
            // consumer reads `element(1, carrier)` itself; nothing to bind.
            ThreadedStmt::DiscardLocals { .. } => {}
            // ADR 0131 §2: frame nodes. Each `LocalRebind` in `body` already
            // carries the lowering its enclosing frame chose, so a frame
            // renders straight-line like any other sequence (an `NlrCatch`
            // inside still consumes the rest of `body` as its try-body).
            ThreadedStmt::MethodBody { body, .. } | ThreadedStmt::BranchArm { body, .. } => {
                docs.push(render(body, ctx));
            }
        }
    }
    Document::Vec(docs)
}

/// ADR 0131 §2's `<read>`: `maps:get(Key, element(2, CF))` for a
/// [`CarrierSlot::Key`] — or `maps:get(Key, StateN)` when `state_first`
/// names the `State` version the family `Bind` already extracted from the
/// carrier (actor instance frames) — and `element(k, CF)` for a
/// [`CarrierSlot::Pos`].
fn render_carrier_read(
    carrier: &str,
    slot: &CarrierSlot,
    state_first: Option<&VersionedVar>,
    ctx: &RenderCtx,
) -> Document<'static> {
    let element = |k: usize| {
        docvec![
            "call 'erlang':'element'(",
            leaf::int_lit(i64::try_from(k).unwrap_or(i64::MAX)),
            ", ",
            leaf::var(carrier.to_string()),
            ")",
        ]
    };
    match slot {
        CarrierSlot::Key(key) => {
            let map = match state_first {
                Some(state) => leaf::var(ctx.resolve_prefix(state)),
                None => element(2),
            };
            docvec![
                "call 'maps':'get'(",
                leaf::atom(key.clone()),
                ", ",
                map,
                ")"
            ]
        }
        CarrierSlot::Pos(k) => element(*k),
    }
}

/// Renders one [`ThreadedStmt::LocalRebind`] per its recorded
/// [`RebindLowering`] — the ADR 0131 §2 table cell its enclosing frame chose
/// ([`super::ir::RebindShape::for_frame`]):
/// - [`RebindLowering::Let`] (every non-member; a non-REPL `MethodBody`):
///   `let V = <read> in`;
/// - [`RebindLowering::LoopParam`] (`DirectParams`/`Hybrid`/`TupleAcc`
///   member): the `Gensym` `Bind` `let V = <read> in`, whose identity
///   [`final_loop_arg_identities`] carries into the loop's recursive call;
/// - [`RebindLowering::MapPut`] (`StateAcc` loop/handler, `BranchArm`, REPL
///   `MethodBody` member): `let V = <read> in` then the `Put` `Bind`
///   `let StateN+1 = maps:put(Key, V, StateN) in`.
///
/// Both `Bind`s go through [`render_bind`], so a rebind and an ordinary
/// mutation of the same shape can never differ in bytes.
fn render_local_rebind(
    carrier: &str,
    slot: &CarrierSlot,
    value_var: &str,
    state_first: Option<&VersionedVar>,
    lowering: &RebindLowering,
    ctx: &RenderCtx,
) -> Document<'static> {
    let read = render_carrier_read(carrier, slot, state_first, ctx);
    let plain_let = |read: Document<'static>| {
        docvec![
            "let ",
            leaf::var(value_var.to_string()),
            " = ",
            read,
            " in "
        ]
    };
    match lowering {
        RebindLowering::Let => plain_let(read),
        RebindLowering::LoopParam { source, target } => {
            render_bind(target, source, &BindOp::Direct(ValueRef::Doc(read)), ctx)
        }
        RebindLowering::MapPut {
            key,
            source,
            target,
        } => docvec![
            plain_let(read),
            render_bind(
                target,
                source,
                &BindOp::Put {
                    field: key.clone(),
                    value: ValueRef::Var(value_var.to_string()),
                },
                ctx,
            ),
        ],
    }
}

/// Full-fidelity rendering of [`ThreadedStmt::OnDoCatch`]: the open-ended
/// `catch <Type, Error, Stack> -> case {Type, Error} of ...` clause, complete
/// down to the closing `end end`: the exception-class filter's handler arm, its
/// `<'false'>` re-raise arm and its exhaustive fallback are steps of the node
/// ([`CatchStep::FilterHandler`], [`CatchStep::FilterMiss`]).
///
/// Clause order is the node's: both `$bt_nlr` pass-through arms re-raise
/// untouched; the non-NLR arm runs its steps in order, the class-variable
/// restore first (ADR 0130 §4), so every exception that is not a `^` discards
/// the protected region's writes before the filter runs.
fn render_on_do_catch(vars: &OnDoCatchVars, clauses: &[CatchClause]) -> Document<'static> {
    let raise = || {
        CoreErlangGenerator::emit_raw_raise(
            vars.type_var.clone(),
            vars.error_var.clone(),
            vars.stack_var.clone(),
        )
    };
    let mut docs: Vec<Document<'static>> = vec![docvec![
        "catch <",
        leaf::var(vars.type_var.clone()),
        ", ",
        leaf::var(vars.error_var.clone()),
        ", ",
        leaf::var(vars.stack_var.clone()),
        "> -> case {",
        leaf::var(vars.type_var.clone()),
        ", ",
        leaf::var(vars.error_var.clone()),
        "} of ",
    ]];
    for clause in clauses {
        match clause {
            CatchClause::NlrPassThrough(NlrThrowShape::Tuple4) => docs.push(docvec![
                "<{'throw', {'$bt_nlr', ",
                leaf::var(vars.nlr_tok_var.clone()),
                ", ",
                leaf::var(vars.nlr_val_var.clone()),
                ", ",
                leaf::var(vars.nlr_state_var.clone()),
                "}}> when 'true' -> ",
                raise(),
                " ",
            ]),
            CatchClause::NlrPassThrough(NlrThrowShape::Tuple3) => docs.push(docvec![
                "<{'throw', {'$bt_nlr', ",
                leaf::var(vars.nlr_tok_var2.clone()),
                ", ",
                leaf::var(vars.nlr_val_var2.clone()),
                "}}> when 'true' -> ",
                raise(),
                " ",
            ]),
            CatchClause::NonNlr { steps } => {
                docs.push(docvec![
                    "<",
                    leaf::var(vars.other_pair_var.clone()),
                    "> when 'true' -> "
                ]);
                for step in steps {
                    docs.push(render_catch_step(vars, step));
                }
            }
        }
    }
    Document::Vec(docs)
}

fn render_catch_step(vars: &OnDoCatchVars, step: &CatchStep) -> Document<'static> {
    match step {
        CatchStep::ClassVarRestore { snapshot } => {
            CoreErlangGenerator::class_var_restore_doc(snapshot)
        }
        CatchStep::WrapException => docvec![
            "let ",
            leaf::var(vars.built_stack_var.clone()),
            " = primop 'build_stacktrace'(",
            leaf::var(vars.stack_var.clone()),
            ") in ",
            "let ",
            leaf::var(vars.ex_obj_var.clone()),
            " = call 'beamtalk_exception_handler':'ensure_wrapped'(",
            leaf::var(vars.type_var.clone()),
            ", ",
            leaf::var(vars.error_var.clone()),
            ", ",
            leaf::var(vars.built_stack_var.clone()),
            ") in ",
        ],
        CatchStep::ClassFilter => docvec![
            "let ",
            leaf::var(vars.match_var.clone()),
            " = call 'beamtalk_exception_handler':'matches_class'(",
            leaf::var(vars.ex_class_var.clone()),
            ", ",
            leaf::var(vars.ex_obj_var.clone()),
            ") in ",
            "case ",
            leaf::var(vars.match_var.clone()),
            " of ",
            "<'true'> when 'true' -> ",
        ],
        CatchStep::FilterHandler(handler) => handler.clone(),
        CatchStep::FilterMiss => docvec![
            " <'false'> when 'true' -> ",
            CoreErlangGenerator::emit_raw_raise(
                vars.type_var.clone(),
                vars.error_var.clone(),
                vars.stack_var.clone(),
            ),
            CoreErlangGenerator::case_clause_fallback_doc(vars.filter_fallback_var.clone()),
            " end end",
        ],
    }
}

/// Full-fidelity rendering of a [`ThreadedStmt::Threaded`] node: real
/// `letrec` scaffolding for [`ThreadingMode::DirectParams`]/
/// [`ThreadingMode::Hybrid`] (the while-loop family modes a
/// dual-run harness proves parity for — see `render_tests` below);
/// `TupleAcc`/`StateAcc` flatten `body` (`render`'s straight-line
/// concatenation) — ADR 0111 Addendum 15's Foldl migration is what makes
/// this full-fidelity: the caller (`control_flow::body::generate_foldl_loop_body`)
/// now merges the fold's own unpack (`TupleAccUnpack`, or a `StateAcc`-map
/// `Statement` prelude), its per-statement body, and its accumulator
/// epilogue into ONE `body`, so the flattening here renders the fold's
/// real `fun (Elem, Acc) -> <unpack> <body> <epilogue>` content, not just
/// the narrow unpack-only shape this arm originally saw.
fn render_threaded(
    mode: &ThreadingMode,
    frame: FrameId,
    body: &[ThreadedStmt],
    produces: &[VersionedVar],
    ctx: &mut RenderCtx,
) -> Document<'static> {
    match mode {
        ThreadingMode::DirectParams => {
            let fn_name = ctx.fresh_temp_var("Loop");
            render_loop_skeleton(fn_name, frame, None, body, produces, None, ctx, None, None)
        }
        ThreadingMode::Hybrid => {
            let fn_name = ctx.fresh_temp_var("Loop");
            render_loop_skeleton(
                fn_name,
                frame,
                None,
                body,
                produces,
                None,
                ctx,
                Some(LoopContextFlags {
                    in_loop_body: true,
                    in_hybrid_loop: true,
                }),
                None,
            )
        }
        ThreadingMode::TupleAcc(_) | ThreadingMode::StateAcc(_) => render(body, ctx),
    }
}

/// Bundles the pieces [`ThreadedStmt::ConditionalLoop`]'s real condition/
/// case-split shape needs, so [`render_loop_skeleton`] can share its
/// `param_list`/`body_doc`/`final_args` plumbing with the bare unconditional
/// [`Threaded`](ThreadedStmt::Threaded) skeleton via one `Option`. ADR 0118
/// phase 3: `condition`/`condition_value` replace the earlier
/// opaque `continue_header` — the condition must render INSIDE this
/// function's own loop-context closure (alongside `body_doc`), never
/// outside it, because a self-send in the condition references the SAME
/// per-iteration params `body_doc` does (see the variant's own doc
/// comment). `Copy`: every field is a borrow, mirroring the earlier
/// `header_and_exit: Option<(&Document, &Document)>` tuple this replaces.
#[derive(Clone, Copy)]
struct ConditionalLoopHeader<'a> {
    condition: &'a [ThreadedStmt],
    condition_value: &'a ValueRef,
    continue_arm: &'a Document<'static>,
    exit_arm: &'a Document<'static>,
}

/// Builds `letrec 'FnName'/arity = fun (Param1, .., ParamN) -> <body> apply
/// 'FnName'/arity (<produces>) in apply 'FnName'/arity (<OuterArg1, ..,
/// OuterArgN>)` — the shared skeleton behind [`ThreadingMode::DirectParams`]
/// (`loop_context: None`) and [`ThreadingMode::Hybrid`] (`loop_context:
/// Some(..)`, flipping `in_loop_body`/`in_hybrid_loop` for the `fun`
/// declaration/body/recursive-call render so any `VersionPrefix::State`
/// reference among them resolves the same way `generate_hybrid_loop_body`
/// makes it resolve live: `State`/`StateN`, not `StateAcc`/`StateAccN`).
///
/// `produces`' own (post-body) versions render as the `fun`'s declared
/// parameters AND the recursive self-call's arguments — both textually
/// inside the same `fun` block as `body_doc`, so both resolve under the
/// loop's OWN context (`render_in_loop_body`, below). The OUTER initial
/// call is different: it is the calling scope's own reference to
/// `produces` at version 0, so it resolves under whatever context was
/// already ambient *before* this `Threaded` node — never the loop's own.
/// For a `Local`-prefixed var these two resolutions coincide
/// ([`VersionedVar::render_name`] names it purely from `(name, version)`,
/// context-independent — the common case, matching how the real
/// direct-params/hybrid generators reuse `to_core_erlang_var`-derived names
/// on both sides, `while_loops.rs`'s `initial_direct_args`/
/// `param_list_doc`); for a `State`-prefixed var nested inside a
/// differently-flagged ambient loop, they do NOT (the `fun`'s formal
/// parameter and the outer call's argument are legitimately different
/// names — a `fun`'s parameter name never has to match its caller's
/// argument expression).
///
///
/// ADR 0111 Addendum 2, Gap 1: factored out of the pre-existing
/// `render_loop_letrec` so the bare unconditional [`Threaded`](ThreadedStmt::Threaded)
/// skeleton (`header: None`) and [`ConditionalLoop`](ThreadedStmt::ConditionalLoop)'s
/// real condition/case-split skeleton (`header: Some(ConditionalLoopHeader { .. })`,
/// ADR 0118 phase 3) share one implementation instead of two
/// near-duplicates (CLAUDE.md's no-duplicate-implementations rule applies
/// within this file, not just across the Rust/Erlang boundary).
///
/// `fn_name` is already resolved by the caller — the bare shape mints it via
/// `ctx.fresh_temp_var("Loop")` (`render_threaded`, unchanged behavior);
/// `ConditionalLoop` carries its own caller-supplied, never-gensym'd name
/// (the variant's own doc comment: production never gensyms this name
/// either).
///
/// `produces`' own (post-body) versions render as the `fun`'s declared
/// parameters AND (for the bare shape only) the recursive self-call's
/// arguments — both textually inside the same `fun` block as `body_doc`, so
/// both resolve under the loop's OWN context (`render_in_loop_body`, below).
/// The OUTER initial call is different: it is the calling scope's own
/// reference to `produces` at version 0, so it resolves under whatever
/// context was already ambient *before* this node — never the loop's own.
/// For a `Local`-prefixed var these two resolutions coincide
/// ([`VersionedVar::render_name`] names it purely from `(name, version)`,
/// context-independent — the common case, matching how the real
/// direct-params/hybrid generators reuse `to_core_erlang_var`-derived names
/// on both sides, `while_loops.rs`'s `initial_direct_args`/
/// `param_list_doc`); for a `State`-prefixed var nested inside a
/// differently-flagged ambient loop, they do NOT (the `fun`'s formal
/// parameter and the outer call's argument are legitimately different
/// names — a `fun`'s parameter name never has to match its caller's
/// argument expression).
///
/// For `ConditionalLoop` (`header: Some(..)`), the recursive
/// self-call's arguments are NOT `produces` verbatim — they are
/// [`final_loop_arg_identities`]'s reconstruction of each local's REAL final
/// `Bind` target (a [`VersionPrefix::Gensym`] identity production actually
/// minted), because `produces` only ever carries each local's INITIAL
/// (`version == 0`) identity (see the variant's doc comment for why the
/// bare shape's "reset version to 0" trick cannot be reused the other
/// direction for a `Gensym` prefix). The body itself also renders
/// differently for `ConditionalLoop`: real loop bodies are `BodyKind::Letrec`
/// (lowered by `lower_letrec_body`, `control_flow/body.rs`), rendered with a
/// literal `" "` between statements by [`render_loop_body_statements`]; the
/// bare shape's body is a synthetic, condition-free fixture with no such
/// production twin, so it keeps rendering via plain [`render`].
#[allow(clippy::too_many_arguments)]
#[allow(clippy::too_many_lines)] // shared param_list/outer_args/body/final_args plumbing for both bare Threaded loops and real ConditionalLoop nodes
fn render_loop_skeleton(
    fn_name: String,
    frame: FrameId,
    counter: Option<&LoopCounter>,
    body: &[ThreadedStmt],
    produces: &[VersionedVar],
    outer_args_override: Option<&[Document<'static>]>,
    ctx: &mut RenderCtx,
    loop_context: Option<LoopContextFlags>,
    header: Option<ConditionalLoopHeader<'_>>,
) -> Document<'static> {
    let arity = produces.len() + usize::from(counter.is_some());

    // The OUTER initial call's arguments — this is the calling scope's own
    // reference to `produces` at version 0, so it must resolve under
    // whatever context was already ambient *before* this node, never the
    // loop's own context (that would rename a var the caller never bound
    // under). `counter` (a counted loop's own extra leading parameter,
    // never a `produces` entry — see its own doc comment) supplies its
    // OUTER value directly via `initial`, opaque to this resolution.
    //
    // `produces`' generic per-entry derivation is overridden wholesale by
    // `outer_args_override` when present — see
    // [`ThreadedStmt::ConditionalLoop::outer_args`]'s doc comment for why
    // the generic (version-0, ambient-context) spelling can never be
    // trusted for the caller's own live value.
    let outer_args = join(
        counter
            .map(|c| c.initial.clone())
            .into_iter()
            .chain(produces.iter().enumerate().map(|(i, v)| {
                match outer_args_override.and_then(|overrides| overrides.get(i)) {
                    Some(overridden) => overridden.clone(),
                    None => leaf::var(ctx.resolve_prefix(&VersionedVar::new(
                        v.prefix.clone(),
                        0,
                        frame,
                    ))),
                }
            })),
        &Document::Str(", "),
    );

    // `param_list` (the `fun (...)` declaration) and
    // `final_args` (the recursive self-call's arguments) both sit textually
    // inside the SAME `fun (...) -> <body_doc> apply ...` block as
    // `body_doc`, so all three must resolve `produces`' prefixes under the
    // identical loop-context flags — computing any of them under the
    // pre-loop ambient context instead would pick a different
    // `State`/`StateAcc` prefix than `body_doc` bound whenever a Hybrid loop
    // is nested inside a differently-flagged ambient context (e.g. inside a
    // `StateAcc`-mode loop), producing a reference to an unbound Core Erlang
    // variable — the `fun`'s own declared parameter name must match every
    // reference to it inside the `fun`'s body, including the recursive tail
    // call.
    let render_in_loop_body = |ctx: &mut RenderCtx| {
        let param_list = join(
            counter
                .map(|c| leaf::var(c.name.clone()))
                .into_iter()
                .chain(produces.iter().map(|v| {
                    leaf::var(ctx.resolve_prefix(&VersionedVar::new(v.prefix.clone(), 0, frame)))
                })),
            &Document::Str(", "),
        );
        // ADR 0118 phase 3: the condition prelude renders INSIDE
        // this closure, under the identical loop-context flags as
        // `body_doc` — a self-send's `Bind` in `condition` must resolve
        // `State`/`StateAcc` the same way the body's own Binds do (the
        // same reasoning `param_list`/`final_args` already document above).
        let condition_doc = header.map(|h| {
            docvec![
                render(h.condition, ctx),
                "case ",
                render_value(h.condition_value, ctx),
                " of ",
                h.continue_arm.clone(),
            ]
        });
        let body_doc = if header.is_some() {
            render_loop_body_statements(body, ctx)
        } else {
            render(body, ctx)
        };
        let final_args = if header.is_some() {
            join(
                counter.map(|c| c.next.clone()).into_iter().chain(
                    final_loop_arg_identities(body, produces)
                        .iter()
                        .map(|v| leaf::var(ctx.resolve_prefix(v))),
                ),
                &Document::Str(", "),
            )
        } else {
            join(
                produces.iter().map(|v| leaf::var(ctx.resolve_prefix(v))),
                &Document::Str(", "),
            )
        };
        (param_list, condition_doc, body_doc, final_args)
    };
    let (param_list, condition_doc, body_doc, final_args) = match loop_context {
        Some(flags) => ctx.with_loop_context(flags, render_in_loop_body),
        None => render_in_loop_body(ctx),
    };

    match header {
        None => docvec![
            "letrec ",
            leaf::fname(fn_name.clone(), arity),
            " = fun (",
            param_list,
            ") -> ",
            body_doc,
            "apply ",
            leaf::fname(fn_name.clone(), arity),
            " (",
            final_args,
            ")",
            " in apply ",
            leaf::fname(fn_name, arity),
            " (",
            outer_args,
            ")",
        ],
        Some(h) => docvec![
            "letrec ",
            leaf::fname(fn_name.clone(), arity),
            " = fun (",
            param_list,
            ") -> ",
            condition_doc.expect("condition_doc is always Some when header is Some"),
            body_doc,
            " apply ",
            leaf::fname(fn_name.clone(), arity),
            " (",
            final_args,
            ") ",
            h.exit_arm.clone(),
            // NOTE: no leading space here (unlike the bare-shape arm above) —
            // `exit_arm` (e.g. `"<'false'> ... end "`) already ends with a
            // trailing space, matching production's own `" end ",` +
            // `"in apply "` concatenation (`while_loops.rs`'s
            // `generate_while_loop_direct`) exactly; an extra leading space
            // here would double it.
            "in apply ",
            leaf::fname(fn_name, arity),
            " (",
            outer_args,
            ")",
        ],
    }
}

/// Renders a real (`ConditionalLoop`) loop body's statements, inserting the
/// literal `" "` separator `BodyKind::Letrec` bodies (lowered by
/// `lower_letrec_body`, `control_flow/body.rs`) need between statements —
/// confirmed against real compiled output (two consecutive threaded-local
/// rebinds emit `"... in  let ..."`, a double space: the statement's own
/// trailing `" in "` plus this separator). Each statement renders through
/// the general [`render`] dispatch (a one-element slice), so nested shapes
/// (a future `ConditionalLoop` body statement that isn't a bare `Bind`)
/// stay correctly handled without this function re-deriving `render`'s own
/// match.
fn render_loop_body_statements(body: &[ThreadedStmt], ctx: &mut RenderCtx) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::with_capacity(body.len() * 2);
    for (i, stmt) in body.iter().enumerate() {
        if i > 0 {
            docs.push(Document::Str(" "));
        }
        docs.push(render(std::slice::from_ref(stmt), ctx));
    }
    Document::Vec(docs)
}

/// Reconstructs each `produces` entry's REAL final identity by following its
/// `Bind` chain forward through `body` (ADR 0111 Addendum 2, Gap 1 — see
/// [`ThreadedStmt::ConditionalLoop`]'s doc comment for why this cannot be
/// derived from `produces` alone the way the bare loop shape derives
/// `param_list`/`outer_args` from it). `produces` seeds the search at each
/// local's INITIAL (`version == 0`) identity; a local never rebound in
/// `body` keeps that identity (matching legacy's `collect_final_local_args`
/// fallback — the fun's own unchanged parameter name IS the final arg).
/// Only scans `body`'s own top-level `Bind`s — a nested construct's `Bind`s
/// belong to a different [`FrameId`] and can never be this loop's own
/// rebind chain.
fn final_loop_arg_identities(
    body: &[ThreadedStmt],
    produces: &[VersionedVar],
) -> Vec<VersionedVar> {
    produces
        .iter()
        .map(|seed| {
            let mut current = seed.clone();
            while let Some(next) = body.iter().find_map(|stmt| match stmt {
                ThreadedStmt::Bind { source, target, .. } if *source == current => {
                    Some(target.clone())
                }
                // ADR 0131 §2: a member rebind of a `DirectParams`/`Hybrid`/
                // `TupleAcc` frame joins the chain exactly like a `Bind`.
                ThreadedStmt::LocalRebind {
                    lowering: RebindLowering::LoopParam { source, target },
                    ..
                } if *source == current => Some(target.clone()),
                _ => None,
            }) {
                current = next;
            }
            current
        })
        .collect()
}

/// Full-fidelity rendering of a [`ThreadedStmt::ConditionalLoop`] node (ADR
/// 0111 Addendum 2, Gap 1; condition fields per ADR 0118 phase 3):
/// delegates to [`render_loop_skeleton`] with the real `condition`/
/// `condition_value`/`continue_arm`/`exit_arm` bundle, reusing the exact
/// same `param_list`/`outer_args` plumbing the bare loop shape uses.
#[allow(clippy::too_many_arguments)]
fn render_conditional_loop(
    fn_name: &str,
    mode: &ThreadingMode,
    frame: FrameId,
    counter: Option<&LoopCounter>,
    condition: &[ThreadedStmt],
    condition_value: &ValueRef,
    continue_arm: &Document<'static>,
    body: &[ThreadedStmt],
    produces: &[VersionedVar],
    outer_args_override: Option<&[Document<'static>]>,
    exit_arm: &Document<'static>,
    ctx: &mut RenderCtx,
) -> Document<'static> {
    let loop_context = match mode {
        ThreadingMode::Hybrid => Some(LoopContextFlags {
            in_loop_body: true,
            in_hybrid_loop: true,
        }),
        // ADR 0111 Addendum 15: a `StateAcc`-mode loop's param_list/body_doc/
        // final_args must resolve `VersionPrefix::State` under LOOP context
        // (`StateAcc`/`StateAccN`, matching the fun's own literal `StateAcc`
        // parameter every real `StateAcc`-mode call site declares) — the
        // same reasoning `Hybrid` above already documents, minus
        // `in_hybrid_loop` (a `StateAcc`-mode loop is not a `Hybrid` one).
        // `outer_args` is always overridden wholesale by
        // `outer_args_override` for every real lowering (this toggle never
        // affects it) — see
        // [`ThreadedStmt::ConditionalLoop::outer_args`]'s doc comment.
        ThreadingMode::StateAcc(_) => Some(LoopContextFlags {
            in_loop_body: true,
            in_hybrid_loop: false,
        }),
        ThreadingMode::DirectParams | ThreadingMode::TupleAcc(_) => None,
    };
    render_loop_skeleton(
        fn_name.to_string(),
        frame,
        counter,
        body,
        produces,
        outer_args_override,
        ctx,
        loop_context,
        Some(ConditionalLoopHeader {
            condition,
            condition_value,
            continue_arm,
            exit_arm,
        }),
    )
}

/// Full-fidelity rendering of [`ThreadedStmt::TupleAccUnpack`]: `let V = call
/// 'erlang':'element'(idx, Param) in` chain — `idx` starts at `gate_slots + 1`
/// (1-based, past the leading gate slots) for the first target. No generator
/// context needed (every target renders through [`VersionPrefix::Gensym`]'s
/// context-independent verbatim naming, [`build_tuple_acc_unpack`]'s doc
/// comment). Real production output — see module docs §Status — not just a
/// byte-identical-by-inspection shape.
fn render_tuple_acc_unpack(
    param: &AccParam,
    gate_slots: usize,
    targets: &[VersionedVar],
) -> Document<'static> {
    let mut docs: Vec<Document<'static>> = Vec::new();
    for (i, target) in targets.iter().enumerate() {
        let idx = gate_slots + 1 + i;
        docs.push(docvec![
            "let ",
            leaf::var(target.render_name()),
            " = call 'erlang':'element'(",
            leaf::int_lit(i64::try_from(idx).unwrap_or(0)),
            ", ",
            leaf::var(param.0.clone()),
            ") in ",
        ]);
    }
    Document::Vec(docs)
}

pub(in crate::core_erlang) fn render_value(value: &ValueRef, ctx: &RenderCtx) -> Document<'static> {
    match value {
        ValueRef::Version(v) => leaf::var(ctx.resolve_prefix(v)),
        ValueRef::Var(name) => leaf::var(name.clone()),
        ValueRef::Literal(lit) => Document::Str(lit),
        ValueRef::Doc(doc) => doc.clone(),
    }
}

fn render_bind(
    target: &VersionedVar,
    source: &VersionedVar,
    op: &BindOp,
    ctx: &RenderCtx,
) -> Document<'static> {
    let target_name = ctx.resolve_prefix(target);
    let source_name = ctx.resolve_prefix(source);
    match op {
        BindOp::Put { field, value } => docvec![
            "let ",
            leaf::var(target_name),
            " = call 'maps':'put'(",
            leaf::atom(field.clone()),
            ", ",
            render_value(value, ctx),
            ", ",
            leaf::var(source_name),
            ") in ",
        ],
        BindOp::Unpack { field } => docvec![
            "let ",
            leaf::var(target_name),
            " = call 'maps':'get'(",
            leaf::atom(field.clone()),
            ", ",
            leaf::var(source_name),
            ") in ",
        ],
        BindOp::Direct(value) => docvec![
            "let ",
            leaf::var(target_name),
            " = ",
            render_value(value, ctx),
            " in ",
        ],
    }
}

/// Full-fidelity NLR try/catch scaffolding: reuses
/// [`CoreErlangGenerator::wrap_body_with_nlr_catch`] verbatim — the exact
/// function every real NLR try/catch in the codebase already goes through
/// (module docs on [`ThreadedStmt::NlrCatch`]: "the true call site
/// `ThreadedStmt::NlrCatch` faithfully models"). Zero re-derivation, so
/// this can never drift from production's try/catch shape.
///
/// ADR 0111 Addendum 4 §Gap 3: `token_var` is the [`TokenId`]-carried name
/// the lowering pass minted BEFORE the body's own temps (production's real
/// mint order — `gen_server/methods.rs`'s call sites mint `NlrToken` first,
/// unconditionally, then generate the body). This function no longer mints
/// anything; only the catch-scaffolding vars (`NlrResult`, `NlrCls`, …) are
/// still allocated here, matching production's own post-body
/// `alloc_nlr_catch_vars` position.
fn render_nlr_catch(
    boundary: NlrBoundary,
    token_var: &str,
    body_doc: Document<'static>,
    ctx: &mut RenderCtx,
) -> Document<'static> {
    ctx.generator
        .wrap_body_with_nlr_catch(body_doc, token_var, boundary)
}

fn render_return(value: &ValueRef, state: &VersionedVar, ctx: &RenderCtx) -> Document<'static> {
    docvec![
        "{",
        render_value(value, ctx),
        ", ",
        leaf::var(ctx.resolve_prefix(state)),
        "}"
    ]
}
