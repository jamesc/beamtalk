// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `ThreadedIr` -> Core Erlang [`Document`] emission (BT-3144). [`render`]
//! and its helpers reach the live generator only through [`RenderCtx`]'s
//! three narrow accessors, never through a raw field or an AST-directed
//! emission path. Depends only on [`super::ir`]; nothing here verifies IR.

use super::super::{CoreErlangGenerator, NlrBoundary};
use super::ir::{
    AccParam, BindOp, FrameId, ThreadedStmt, ThreadingMode, ValueRef, VersionPrefix, VersionedVar,
};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, join, leaf};

// ─── RenderCtx (BT-3144, ADR 0111 §Addendum "Renderer design sketch") ──────

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
    /// allocator (BT-875: naming stays centralized, never re-derived).
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
                self.generator.in_hybrid_loop,
                self.generator.in_loop_body,
                var.version,
            ),
            VersionPrefix::ClassVars
            | VersionPrefix::SelfVt
            | VersionPrefix::Local(_)
            | VersionPrefix::Gensym(_) => var.render_name(),
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
            in_hybrid_loop: ctx.generator.in_hybrid_loop,
        };
        ctx.generator.in_loop_body = flags.in_loop_body;
        ctx.generator.in_hybrid_loop = flags.in_hybrid_loop;
        Self { ctx, saved }
    }
}

impl Drop for LoopContextGuard<'_, '_> {
    fn drop(&mut self) {
        self.ctx.generator.in_loop_body = self.saved.in_loop_body;
        self.ctx.generator.in_hybrid_loop = self.saved.in_hybrid_loop;
    }
}

// ─── render: full-fidelity ThreadedIr -> Document (BT-3144) ────────────────

/// Renders `ir` to a [`Document`], full-fidelity for `Bind`, `Return`,
/// `TupleAccUnpack`, `NlrCatch`, and `Threaded` under
/// [`ThreadingMode::DirectParams`]/[`ThreadingMode::Hybrid`] — real
/// `letrec`/try-catch scaffolding, not the pre-BT-3144 skeleton. See the
/// module docs §Status for exactly which shapes are full-fidelity today and
/// why (`TupleAcc`/`StateAcc` extend later, driven by a real migration's
/// needs — issue body point 3).
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
/// The FIRST such caller, `control_flow::while_loops`'s
/// `try_render_while_direct_via_threaded_ir` pilot (BT-3145, gated behind
/// `BEAMTALK_THREADED_IR_WHILE_DIRECT=1`), was deleted by BT-3182 — see
/// §Status (BT-3182) / ADR 0111 Addendum 13.
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
                shadow_write,
                span: _,
            } => docs.push(render_bind(target, source, op, *shadow_write, ctx)),
            ThreadedStmt::Threaded {
                mode,
                frame,
                shadow_write_eligible: _, // rendering-irrelevant: verify()-only, see the field's doc comment
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
                shadow_write_eligible: _, // rendering-irrelevant: verify()-only, see the field's doc comment
                counter: _counter, // counted-loop rendering: a later migration wires a real counted-loop call site (BT-3182: the while-direct pilot that used `ConditionalLoop` for its own `mode`/`counter: None` shape was deleted — see ADR 0111 Addendum 13)
                condition,
                condition_value,
                continue_arm,
                body,
                produces,
                exit_arm,
                span: _,
            } => docs.push(render_conditional_loop(
                fn_name,
                mode,
                *frame,
                condition,
                condition_value,
                continue_arm,
                body,
                produces,
                exit_arm,
                ctx,
            )),
            // ADR 0111 Addendum 4: opaque AST-directed statement, emitted
            // verbatim. The doc carries its own trailing glue; this loop
            // adds no separator, and rendering mints nothing.
            ThreadedStmt::Statement(doc, _) => docs.push(doc.clone()),
        }
    }
    Document::Vec(docs)
}

/// Full-fidelity rendering of a [`ThreadedStmt::Threaded`] node: real
/// `letrec` scaffolding for [`ThreadingMode::DirectParams`]/
/// [`ThreadingMode::Hybrid`] (the while-loop family modes this issue's
/// dual-run harness proves parity for — see `render_tests` below);
/// `TupleAcc`/`StateAcc` still flatten the body (module docs §Status).
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
            render_loop_skeleton(fn_name, frame, body, produces, ctx, None, None)
        }
        ThreadingMode::Hybrid => {
            let fn_name = ctx.fresh_temp_var("Loop");
            render_loop_skeleton(
                fn_name,
                frame,
                body,
                produces,
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
/// phase 3 (BT-3419): `condition`/`condition_value` replace the pre-BT-3419
/// opaque `continue_header` — the condition must render INSIDE this
/// function's own loop-context closure (alongside `body_doc`), never
/// outside it, because a self-send in the condition references the SAME
/// per-iteration params `body_doc` does (see the variant's own doc
/// comment). `Copy`: every field is a borrow, mirroring the pre-BT-3419
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
/// ADR 0118 phase 3/BT-3419) share one implementation instead of two
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
/// (`generate_threaded_loop_body_inner`, `control_flow/mod.rs`), which
/// inserts a literal `" "` between statements — [`render_loop_body_statements`]
/// reproduces that; the bare shape's body is a synthetic, condition-free
/// fixture with no such production twin, so it keeps rendering via plain
/// [`render`].
fn render_loop_skeleton(
    fn_name: String,
    frame: FrameId,
    body: &[ThreadedStmt],
    produces: &[VersionedVar],
    ctx: &mut RenderCtx,
    loop_context: Option<LoopContextFlags>,
    header: Option<ConditionalLoopHeader<'_>>,
) -> Document<'static> {
    let arity = produces.len();

    // The OUTER initial call's arguments — this is the calling scope's own
    // reference to `produces` at version 0, so it must resolve under
    // whatever context was already ambient *before* this node, never the
    // loop's own context (that would rename a var the caller never bound
    // under).
    let outer_args = join(
        produces
            .iter()
            .map(|v| leaf::var(ctx.resolve_prefix(&VersionedVar::new(v.prefix.clone(), 0, frame)))),
        &Document::Str(", "),
    );

    // BT-3144 review: `param_list` (the `fun (...)` declaration) and
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
            produces.iter().map(|v| {
                leaf::var(ctx.resolve_prefix(&VersionedVar::new(v.prefix.clone(), 0, frame)))
            }),
            &Document::Str(", "),
        );
        // ADR 0118 phase 3 (BT-3419): the condition prelude renders INSIDE
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
                final_loop_arg_identities(body, produces)
                    .iter()
                    .map(|v| leaf::var(ctx.resolve_prefix(v))),
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

/// Renders a real (`ConditionalLoop`) loop body's statements with the
/// literal `" "` separator `generate_threaded_loop_body_inner` inserts
/// between statements for `BodyKind::Letrec` (`control_flow/mod.rs`) —
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
                _ => None,
            }) {
                current = next;
            }
            current
        })
        .collect()
}

/// Full-fidelity rendering of a [`ThreadedStmt::ConditionalLoop`] node (ADR
/// 0111 Addendum 2, Gap 1; condition fields per ADR 0118 phase 3, BT-3419):
/// delegates to [`render_loop_skeleton`] with the real `condition`/
/// `condition_value`/`continue_arm`/`exit_arm` bundle, reusing the exact
/// same `param_list`/`outer_args` plumbing the bare loop shape uses.
#[allow(clippy::too_many_arguments)]
fn render_conditional_loop(
    fn_name: &str,
    mode: &ThreadingMode,
    frame: FrameId,
    condition: &[ThreadedStmt],
    condition_value: &ValueRef,
    continue_arm: &Document<'static>,
    body: &[ThreadedStmt],
    produces: &[VersionedVar],
    exit_arm: &Document<'static>,
    ctx: &mut RenderCtx,
) -> Document<'static> {
    let loop_context = match mode {
        ThreadingMode::Hybrid => Some(LoopContextFlags {
            in_loop_body: true,
            in_hybrid_loop: true,
        }),
        ThreadingMode::DirectParams | ThreadingMode::TupleAcc(_) | ThreadingMode::StateAcc(_) => {
            None
        }
    };
    render_loop_skeleton(
        fn_name.to_string(),
        frame,
        body,
        produces,
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
/// comment). Real production output as of BT-3147 — see module docs §Status
/// — not just a byte-identical-by-inspection shape as pre-BT-3147.
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
    shadow_write: bool,
    ctx: &RenderCtx,
) -> Document<'static> {
    let target_name = ctx.resolve_prefix(target);
    let source_name = ctx.resolve_prefix(source);
    match op {
        BindOp::Put {
            field,
            value,
            class_tag,
        } => {
            let put_doc = docvec![
                "let ",
                leaf::var(target_name.clone()),
                " = call 'maps':'put'(",
                leaf::atom(field.clone()),
                ", ",
                render_value(value, ctx),
                ", ",
                leaf::var(source_name),
                ") in ",
            ];
            if shadow_write {
                docvec![
                    put_doc,
                    "let _ = call 'erlang':'put'({",
                    leaf::atom("$bt_class_vars_shadow"),
                    ", call 'erlang':'element'(2, ",
                    render_value(class_tag, ctx),
                    ")}, ",
                    leaf::var(target_name),
                    ") in ",
                ]
            } else {
                put_doc
            }
        }
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
