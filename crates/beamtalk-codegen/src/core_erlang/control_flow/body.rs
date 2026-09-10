// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The unified per-statement body generator for state-threaded `Letrec`
//! loop bodies ([`CoreErlangGenerator::generate_letrec_body_ir`]) and
//! `Foldl*` fold bodies ([`CoreErlangGenerator::generate_foldl_loop_body`]),
//! plus their last-expression finalizer helpers.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! split out of `control_flow/mod.rs`, no logic changes. ADR 0111
//! Addendum 15 then migrated both families onto real `ThreadedIr`: the
//! Letrec migration deleted the `BodyKind::Letrec` arm of the original
//! `generate_threaded_loop_body_inner`; the Foldl migration
//! converted what remained (`lower_foldl_body`, this file) from a
//! `Document`-returning per-statement dispatch into a `Vec<ThreadedStmt>`
//! one, merged with the fold's own unpack into one verified `Threaded` node
//! by [`CoreErlangGenerator::generate_foldl_loop_body`] — no legacy dual
//! path remains for either family.

use super::super::threaded_ir::{
    self, BindOp, FrameId, ThreadedStmt, ThreadingMode, ValueRef, VersionPrefix, VersionedVar,
};
use super::super::{CodeGenError, CoreErlangGenerator, Result};
use super::list_ops::BodyKind;
use super::plan::ThreadingPlan;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, leaf};
use beamtalk_core::ast::{Block, Expression};
use beamtalk_core::source_analysis::Span;

impl CoreErlangGenerator {
    /// ADR 0111 Addendum 15 (Foldl migration): merges the fold's own
    /// per-iteration unpack (`TupleAccUnpack` in tuple-acc mode; the
    /// `StateAcc` map's `maps:get` prelude in map mode) and its
    /// per-statement body into ONE `ThreadedStmt::Threaded` node,
    /// `verify()`s it once, and renders it — the Foldl counterpart of
    /// [`Self::generate_letrec_body_ir`] (which returns statements for the
    /// caller's own `ConditionalLoop` node instead, since a fold's
    /// accumulator — unlike a letrec's — has no sibling node shape to
    /// return into: `lists:foldl`'s callback is an ordinary Core Erlang
    /// `fun`, not a recursive `letrec`).
    ///
    /// Replaces the pre-migration two-step call sites made (a standalone
    /// tuple unpack — verified and rendered alone, via what was
    /// `ThreadingPlan::generate_tuple_unpack_docs` — or
    /// `generate_unpack_at_iteration_start`, immediately followed by the old
    /// `generate_threaded_loop_body`) — every call site now makes ONE call
    /// here instead, so the unpack and the body share one verified node
    /// (acceptance criterion: "`verify()` runs once per fold").
    ///
    /// `acc_param_name`/`node_gate_slots` carry the same meaning
    /// [`threaded_ir::build_tuple_acc_unpack`]'s own `param_name`/
    /// `node_gate_slots` parameters always have (ignored in map/`StateAcc`
    /// mode, where the unpack instead reads
    /// `plan.generate_unpack_at_iteration_start`). The unpack step itself
    /// still runs BEFORE `with_branch_context` is entered (mint order
    /// unchanged from the pre-migration two-step sequence — `fresh_temp_var`
    /// draws from one global counter shared across both steps); only the
    /// body lowering, verification, and rendering happen inside it, so
    /// every `VersionPrefix::State` reference the body's own real `Bind`s
    /// produce resolves under the SAME `in_loop_body` render-time context
    /// live production always used.
    pub(in crate::core_erlang) fn generate_foldl_loop_body(
        &mut self,
        body: &beamtalk_core::ast::Block,
        plan: &ThreadingPlan,
        kind: &BodyKind,
        acc_param_name: &str,
        node_gate_slots: usize,
    ) -> Result<(Document<'static>, usize)> {
        let span = body.span;
        let mut unpack_stmts: Vec<ThreadedStmt> = Vec::new();
        let mode = if plan.use_tuple_acc {
            let (unpack_node, targets) = threaded_ir::build_tuple_acc_unpack(
                acc_param_name,
                plan.tuple_acc_gate_slots,
                node_gate_slots,
                &plan.threaded_locals,
                span,
            );
            for (var_name, target) in plan.threaded_locals.iter().zip(&targets) {
                self.bind_var(var_name, &target.render_name());
            }
            let ThreadedStmt::Threaded {
                body: unpack_body, ..
            } = unpack_node
            else {
                unreachable!("build_tuple_acc_unpack always returns a Threaded node")
            };
            unpack_stmts = unpack_body;
            ThreadingMode::TupleAcc(plan.tuple_acc_gate_slots)
        } else {
            for doc in plan.generate_unpack_at_iteration_start(self) {
                unpack_stmts.push(ThreadedStmt::Statement(doc, span));
            }
            ThreadingMode::StateAcc(plan.fallback_reason.clone())
        };

        self.with_branch_context(|this| {
            let frame = this.current_branch_frame();
            // ADR 0111 Addendum 9, Question 1: read fresh from live generator
            // state at construction time — see `ThreadedStmt::Threaded`'s own
            // doc comment.
            let shadow_write_eligible = this.block_depth == 0;
            let mut stmts = unpack_stmts;
            stmts.extend(this.lower_foldl_body(body, plan, kind, frame)?);
            let final_state_version = this.state_version();
            let node = ThreadedStmt::Threaded {
                mode: mode.clone(),
                frame,
                shadow_write_eligible,
                body: stmts,
                produces: Vec::new(),
                span,
            };
            // ADR 0111 Addendum 4 technique (`backfill_opaque_version_gaps`,
            // generalized off `FrameId::ROOT` for this non-ROOT frame):
            // a self-send, Tier 2 call, or inline-conditional-with-
            // mutations statement bumps `next_state_var()` inside its own
            // opaque `Statement` text (no real `Bind` for that step) — a
            // following real `Bind` (e.g. a field assignment) reading that
            // already-advanced `state_version()` as its `source` would
            // otherwise fail `UnboundVersion`/`NonLinearVersion` against a
            // version this body's own IR never produced. Verification-fixture
            // only, built from a CLONE of `node`'s own body — `render` below
            // still renders the untouched real `node`.
            let ThreadedStmt::Threaded {
                body: real_body, ..
            } = &node
            else {
                unreachable!("node is always a Threaded node, constructed just above")
            };
            let backfilled_body = threaded_ir::backfill_opaque_version_gaps(real_body, frame);
            let fixture = ThreadedStmt::Threaded {
                mode,
                frame,
                shadow_write_eligible,
                body: backfilled_body,
                produces: Vec::new(),
                span,
            };
            let errors = threaded_ir::verify(std::slice::from_ref(&fixture));
            this.report_threaded_ir_verify_errors(&errors, "foldl body mode/shape mismatch", span);
            let mut ctx = threaded_ir::RenderCtx::new(this);
            let doc = threaded_ir::render(std::slice::from_ref(&node), &mut ctx);
            Ok((doc, final_state_version))
        })
    }

    /// ADR 0111 Addendum 15: lowers a `whileTrue:`/`whileFalse:`/
    /// `timesRepeat:`/`to:do:`/`to:by:do:`/`repeat` body directly to a
    /// `Vec<ThreadedStmt>` for the caller's own `ThreadedStmt::ConditionalLoop`
    /// node — the Letrec counterpart of [`Self::generate_foldl_loop_body`]
    /// (the `Foldl*` family's own merged-node builder). Mirrors that
    /// function's `with_branch_context` wrapping, plus its own
    /// `loop_threads_class_vars`/`last_loop_class_var` bookkeeping (a
    /// Letrec-only concern — `Foldl*` threads `ClassVars` via the
    /// `{ClassVars, StateAcc}` accumulator wrap instead, entirely inside
    /// `lower_foldl_body`); `while_loops.rs`/`counted_loops.rs` call this and
    /// build/`verify`/`render` their own `ConditionalLoop` node around the
    /// returned statements and `FrameId`.
    pub(super) fn generate_letrec_body_ir(
        &mut self,
        body: &Block,
        plan: &ThreadingPlan,
    ) -> Result<(Vec<ThreadedStmt>, FrameId)> {
        self.with_branch_context(|this| {
            this.loop_mode.loop_threads_class_vars = plan.threads_class_vars;
            let frame = this.current_branch_frame();
            let result = this.lower_letrec_body(body, plan, frame);
            if plan.threads_class_vars {
                this.loop_mode.last_loop_class_var = Some(this.current_class_var());
            }
            result.map(|stmts| (stmts, frame))
        })
    }

    /// The `ThreadedStmt`-producing counterpart of the pre-migration hybrid
    /// body wrappers (ADR 0111 Addendum 15): sets up the same
    /// `in_hybrid_loop`/`in_direct_params_loop`/`hybrid_readonly_field_params`/
    /// `hybrid_mutated_fields` state around [`Self::generate_letrec_body_ir`],
    /// then restores it — mirroring those functions' save/restore discipline
    /// exactly. Unlike them, this has no `final_mutated_field_args` output:
    /// `render`'s `final_loop_arg_identities` reconstructs each hybrid
    /// field's final identity from its own `Bind` chain, so the caller no
    /// longer needs to capture it here.
    pub(super) fn generate_letrec_hybrid_body_ir(
        &mut self,
        body: &Block,
        plan: &ThreadingPlan,
        all_field_params: &std::collections::HashMap<String, String>,
    ) -> Result<(Vec<ThreadedStmt>, FrameId)> {
        let prev_hybrid = self.loop_mode.in_hybrid_loop;
        let prev_direct_params_loop = self.loop_mode.in_direct_params_loop;
        let prev_readonly_field_params = std::mem::replace(
            &mut self.loop_mode.hybrid_readonly_field_params,
            all_field_params.clone(),
        );
        let prev_mutated_fields = std::mem::replace(
            &mut self.loop_mode.hybrid_mutated_fields,
            plan.mutated_fields.iter().cloned().collect(),
        );
        self.loop_mode.in_hybrid_loop = true;
        self.loop_mode.in_direct_params_loop = true;
        let result = self.generate_letrec_body_ir(body, plan);
        self.loop_mode.hybrid_readonly_field_params = prev_readonly_field_params;
        self.loop_mode.hybrid_mutated_fields = prev_mutated_fields;
        self.loop_mode.in_hybrid_loop = prev_hybrid;
        self.loop_mode.in_direct_params_loop = prev_direct_params_loop;
        result
    }

    /// ADR 0111 Addendum 15: rebases the loop body's own class-var (or
    /// value-type `Self`) `Bind` chain onto its `produces` seed identity.
    ///
    /// `render_loop_skeleton`'s `outer_args`/`param_list` always render
    /// `produces`' entries at version `0` (the "loop's own frame-entry
    /// parameter" convention `VersionPrefix::Local`/`Gensym` already rely
    /// on) — but `VersionPrefix::ClassVars`'s rendering is version-driven
    /// (`ClassVars`, `ClassVars1`, …), not context-toggled the way `State`/
    /// `StateAcc` is (`render_conditional_loop`'s `loop_context`), so a
    /// class-var seed built from the method's own LIVE `class_var_version`
    /// (nonzero whenever this loop is not the method's first class-var
    /// mutation) would make the fun's own declared parameter (rendered at
    /// the hardcoded `0`, i.e. bare `ClassVars`) disagree with every
    /// in-body reference to the SAME incoming value (rendered at the real,
    /// nonzero version) — an unbound-variable defect `just verify-threaded-ir`
    /// caught empirically. The fix mirrors hybrid fields' own seeding
    /// (`VersionPrefix::Gensym` of the pre-minted name, version-independent
    /// by construction): the caller seeds `produces` with
    /// `Gensym(class_var_param_name)@0`, and this rewrites the loop body's
    /// OWN first class-var `Bind` — built by the shared, globally-versioned
    /// `lower_class_var_field_assignment_bind`/`class_method_prelude_producer`
    /// producers `lower_letrec_field_assignment` reuses verbatim — so its
    /// `source` matches that seed instead of the method's live version,
    /// re-anchoring the rest of the chain (and hence
    /// `final_loop_arg_identities`) onto it. A no-op if no `Bind` sources
    /// from `from` (the loop threads no class-var mutation, or `stmts`
    /// doesn't contain one at the top level — a nested construct's own
    /// `Bind`s belong to a different `FrameId` and are never touched here).
    ///
    /// `VersionPrefix::SelfVt` has the identical version-driven
    /// (`Self`, `Self1`, …), never context-toggled rendering, so a
    /// value-type `Self`-threading loop seeds and rebases through this same
    /// function rather than a second copy of it — the reason it is named for
    /// the mechanism (a loop's `produces` seed) rather than for `ClassVars`.
    pub(super) fn rebase_loop_seed(
        stmts: &mut [ThreadedStmt],
        from: &VersionedVar,
        to: &VersionedVar,
    ) {
        for stmt in stmts {
            if let ThreadedStmt::Bind { source, .. } = stmt {
                if source == from {
                    *source = to.clone();
                    return;
                }
            }
        }
    }

    /// The per-statement lowering pass behind [`Self::generate_letrec_body_ir`]
    /// — the `Vec<ThreadedStmt>`-producing sibling of
    /// [`Self::generate_threaded_loop_body_inner`]'s (now Foldl-only)
    /// per-statement dispatch, covering exactly the statement shapes reachable
    /// for `BodyKind::Letrec` there: field assignment, actor/class-method
    /// self-send, local-var assignment (plain-let / direct-params-or-hybrid /
    /// `StateAcc`), destructure assignment, and the generic fallback. Tier 2
    /// value calls and inline-conditional-with-mutations statements are
    /// excluded — both were already gated `!matches!(kind, BodyKind::Letrec)`
    /// in the deleted dispatch. `frame` is the loop's own `FrameId` (shared by
    /// every `Bind` this pass produces and the caller's own `produces` seeds).
    fn lower_letrec_body(
        &mut self,
        body: &Block,
        plan: &ThreadingPlan,
        frame: FrameId,
    ) -> Result<Vec<ThreadedStmt>> {
        let filtered_body = super::super::util::collect_body_exprs(&body.body);
        let has_direct_field_assignments =
            filtered_body.iter().any(|e| Self::is_field_assignment(e));
        let mut stmts: Vec<ThreadedStmt> = Vec::new();

        for (i, expr) in filtered_body.iter().enumerate() {
            let is_last = i == filtered_body.len() - 1;
            let span = expr.span();

            // see `generate_threaded_loop_body_inner`'s identical
            // check for the full rationale — a nested loop/fold statement
            // whose own body threads a `ClassVars` mutation must be rejected
            // here too, ahead of every dispatch branch below.
            if let Some(mutation) = self.nested_loop_lost_class_var_mutation(expr) {
                let location = self.span_to_line(expr.span()).map_or_else(
                    || format!("offset {}", expr.span().start()),
                    |line| format!("line {line}"),
                );
                return Err(CodeGenError::ClassVarMutationLostAcrossNestedLoop {
                    mutation,
                    location,
                });
            }

            // the value-type `Self` mirror of the check just above,
            // with the same deliberate scope limit — nothing unpacks a
            // nested Letrec loop's own trailing `Self` tuple slot back into
            // THIS body's statement sequence, so the inner mutation would be
            // silently discarded once the inner loop exits. See
            // `nested_loop_lost_value_self_mutation`'s doc comment.
            if let Some(mutation) = self.nested_loop_lost_value_self_mutation(expr) {
                let location = self.span_to_line(expr.span()).map_or_else(
                    || format!("offset {}", expr.span().start()),
                    |line| format!("line {line}"),
                );
                return Err(CodeGenError::ValueSelfMutationLostAcrossNestedLoop {
                    mutation,
                    location,
                });
            }

            // BT-3488: a value-type `self.field := ...` write this loop cannot
            // thread out — here, one nested inside a conditional branch (or
            // any other nested block) rather than being a bare top-level
            // statement, the only shape `plan.threads_value_self` carries.
            // Rejected with the SAME diagnostic the identical class-var shape
            // already gets; see
            // `reject_unthreadable_value_self_field_write`.
            self.reject_unthreadable_value_self_field_write(expr, plan.threads_value_self)?;

            if Self::is_field_assignment(expr) {
                self.lower_letrec_field_assignment(expr, frame, span, &mut stmts)?;
            } else if self.is_actor_self_send(expr) {
                // Emit diagnostic for synchronous self-send in loop body.
                self.emit_self_send_in_loop_diagnostic(expr, span);
                // ADR 0118 phase 2a: `threaded_expression`'s
                // self-send producer path (`generate_self_dispatch_parts`)
                // builds the exact same `Statement`+`Bind` pair
                // `generate_self_dispatch_open` used to hand-render — see
                // that function's own doc comment for the byte-for-byte
                // equivalence. Letrec's own body value is always discarded,
                // so `tv.value` is intentionally never referenced.
                let tv = self.threaded_expression(expr, frame)?;
                stmts.extend(tv.prelude);
            } else if self.is_class_method_self_send(expr) {
                if self.loop_mode.loop_threads_class_vars {
                    let tv = self.threaded_expression(expr, frame)?;
                    stmts.extend(tv.prelude);
                } else {
                    // Defensive fallback — see the deleted dispatch's
                    // identical comment in `generate_threaded_loop_body_inner`
                    // (git history) for why this is not expected to be
                    // live in practice.
                    let selector = if let Expression::MessageSend { selector, .. } = expr {
                        selector.name().to_string()
                    } else {
                        unreachable!("is_class_method_self_send only matches MessageSend")
                    };
                    let location = self.span_to_line(expr.span()).map_or_else(
                        || format!("offset {}", expr.span().start()),
                        |line| format!("line {line}"),
                    );
                    return Err(CodeGenError::ClassMethodSelfSendInThreadedLoopBody {
                        selector,
                        location,
                    });
                }
            } else if Self::is_local_var_assignment(expr) {
                self.lower_letrec_local_var_assignment(
                    expr, plan, is_last, frame, span, &mut stmts,
                )?;
            } else if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                let binding_docs = self.generate_destructure_bindings(pattern, value)?;
                for d in binding_docs {
                    stmts.push(ThreadedStmt::Statement(d, span));
                }
            } else {
                self.lower_letrec_non_assign_expr(
                    &mut stmts,
                    expr,
                    frame,
                    is_last,
                    has_direct_field_assignments,
                )?;
            }
        }
        Ok(stmts)
    }

    /// `Bind`-producing lowering of a Letrec body's field assignment
    /// (ADR 0111 Addendum 15): a direct class-var write reuses
    /// [`Self::lower_class_var_field_assignment_bind`] (the same producer
    /// `generate_field_assignment_open`'s own class-var branch calls) and
    /// pushes its returned `Bind` directly rather than pre-rendering it into
    /// an opaque `Document`; a hybrid-mode mutated field reproduces
    /// `generate_field_assignment_open`'s hybrid rebind as a real `Bind`
    /// (needed so [`super::super::threaded_ir::VerifyError`]-free rendering's
    /// `final_loop_arg_identities` can trace this field's own chain); every
    /// other (plain `State`/`StateAcc`) field write reuses
    /// [`Self::lower_field_assignment_bind`] verbatim — its own hybrid bypass
    /// and `reject_class_var_field_assignment` guard apply unchanged.
    fn lower_letrec_field_assignment(
        &mut self,
        expr: &Expression,
        frame: FrameId,
        span: Span,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<()> {
        let Expression::Assignment { target, value, .. } = expr else {
            unreachable!("is_field_assignment guarantees an Assignment expr");
        };
        let Expression::FieldAccess { field, .. } = target.as_ref() else {
            unreachable!("is_field_assignment guarantees a FieldAccess target");
        };

        if self.is_class_var_assignment(expr) && self.loop_mode.loop_threads_class_vars {
            let branch_frame = self.current_branch_frame();
            let (preamble_doc, bind, _val_var) =
                self.lower_class_var_field_assignment_bind(&field.name, value, branch_frame)?;
            stmts.push(ThreadedStmt::Statement(preamble_doc, span));
            stmts.push(bind);
            return Ok(());
        }

        if self.loop_mode.in_hybrid_loop
            && self
                .loop_mode
                .hybrid_mutated_fields
                .contains(field.name.as_str())
        {
            // Mirrors `generate_field_assignment_open`'s hybrid branch
            // (`let Val = <value> in let NewFieldVar = Val in`, rebinding
            // `hybrid_readonly_field_params`), decomposed into a real `Bind`
            // so the field's own rebind chain is traceable from its
            // `produces` seed (`VersionPrefix::Gensym` of the pre-extracted
            // param name).
            let val_var = self.fresh_temp_var("Val");
            let saved_field_params = self.loop_mode.hybrid_readonly_field_params.clone();
            let val_doc = self.expression_doc(value)?;
            self.loop_mode.hybrid_readonly_field_params = saved_field_params;
            let current_field_var = self
                .loop_mode
                .hybrid_readonly_field_params
                .get(field.name.as_str())
                .cloned()
                .unwrap_or_else(|| field.name.to_string());
            let source = VersionedVar::new(VersionPrefix::Gensym(current_field_var), 0, frame);
            let new_field_var =
                self.fresh_temp_var(&format!("{}Field", Self::to_core_erlang_var(&field.name)));
            self.loop_mode
                .hybrid_readonly_field_params
                .insert(field.name.to_string(), new_field_var.clone());
            stmts.push(ThreadedStmt::Statement(
                docvec!["let ", leaf::var(val_var.clone()), " = ", val_doc, " in ",],
                span,
            ));
            stmts.push(ThreadedStmt::Bind {
                target: VersionedVar::new(VersionPrefix::Gensym(new_field_var), 1, frame),
                source,
                op: BindOp::Direct(ValueRef::Var(val_var)),
                shadow_write: false,
                span,
            });
            return Ok(());
        }

        let _ = self.lower_field_assignment_bind(expr, frame, span, stmts)?;
        Ok(())
    }

    /// `Bind`-producing lowering of a Letrec body's local-var assignment
    /// (ADR 0111 Addendum 15): a block-local not in `threaded_locals` stays a
    /// plain opaque `let` (via [`Self::try_generate_block_local_plain_let`]);
    /// direct-params/hybrid mode reuses
    /// [`Self::lower_direct_var_update_in_loop_bind`]; the `StateAcc`
    /// fallback reuses [`Self::lower_local_var_assignment_bind`] verbatim.
    fn lower_letrec_local_var_assignment(
        &mut self,
        expr: &Expression,
        plan: &ThreadingPlan,
        is_last: bool,
        frame: FrameId,
        span: Span,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<()> {
        if let Some(doc) =
            self.try_generate_block_local_plain_let(expr, is_last, &plan.threaded_locals)?
        {
            stmts.push(ThreadedStmt::Statement(doc, span));
            return Ok(());
        }
        if plan.use_direct_params || plan.use_hybrid_params {
            self.lower_direct_var_update_in_loop_bind(expr, frame, span, stmts)?;
        } else {
            let _ = self.lower_local_var_assignment_bind(expr, frame, span, stmts)?;
        }
        Ok(())
    }

    /// `Bind`-producing lowering of the generic (non-assignment) fallback —
    /// the direct counterpart of `emit_non_assign_expr`'s `BodyKind::Letrec`
    /// arm (ADR 0111 Addendum 15). Every non-`is_last`/non-tuple-producing
    /// shape is a pure opaque `Statement` (it touches none of `produces`'
    /// tracked identities); the `is_last` nested-loop/fold shape is a real
    /// `State` `Bind` (needed for `final_loop_arg_identities` to see it).
    fn lower_letrec_non_assign_expr(
        &mut self,
        stmts: &mut Vec<ThreadedStmt>,
        expr: &Expression,
        frame: FrameId,
        is_last: bool,
        has_direct_field_assignments: bool,
    ) -> Result<()> {
        let span = expr.span();
        let mut prelude_stmts: Vec<ThreadedStmt> = Vec::new();
        let thread_scope = self.thread_ahead(expr, &mut prelude_stmts, frame)?;
        let hoisted_anything = !prelude_stmts.is_empty();
        stmts.extend(prelude_stmts);

        if self.loop_mode.in_direct_params_loop {
            // see `emit_non_assign_expr`'s identical branch — a
            // nested list op's own open let-chain, emitted verbatim so its
            // variable rebindings escape to the outer (this loop's) scope.
            let expr_code = self.expression_doc(expr)?;
            stmts.push(ThreadedStmt::Statement(expr_code, span));
        } else if is_last && !has_direct_field_assignments {
            let produces_tuple = !hoisted_anything
                && (self.get_control_flow_threaded_vars(expr).is_some()
                    || self.control_flow_has_mutations(expr));
            if produces_tuple {
                let tuple_var =
                    super::super::util::versioned_var("_NestTuple", self.state_version() + 1);
                let expr_code = self.expression_doc(expr)?;
                let source_version = self.state_version();
                let _ = self.next_state_var();
                let target_version = self.state_version();
                stmts.push(ThreadedStmt::Statement(
                    docvec![
                        "let ",
                        leaf::var(tuple_var.clone()),
                        " = ",
                        expr_code,
                        " in ",
                    ],
                    span,
                ));
                stmts.push(ThreadedStmt::Bind {
                    target: VersionedVar::new(VersionPrefix::State, target_version, frame),
                    source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                    op: BindOp::Direct(ValueRef::Doc(docvec![
                        "call 'erlang':'element'(2, ",
                        leaf::var(tuple_var),
                        ")",
                    ])),
                    shadow_write: false,
                    span,
                });
            } else {
                let expr_code = self.expression_doc(expr)?;
                stmts.push(ThreadedStmt::Statement(
                    docvec!["let _ = ", expr_code, " in"],
                    span,
                ));
            }
        } else {
            let expr_code = self.expression_doc(expr)?;
            stmts.push(ThreadedStmt::Statement(
                docvec!["let _ = ", expr_code, " in"],
                span,
            ));
        }
        self.finish_precompiled_scope(thread_scope)?;
        Ok(())
    }

    /// ADR 0111 Addendum 15 (Foldl migration): the `Vec<ThreadedStmt>`-
    /// producing sibling of [`Self::lower_letrec_body`], covering the
    /// `Foldl*` per-statement dispatch [`Self::generate_foldl_loop_body`]
    /// (its only caller, always inside `with_branch_context` so
    /// `in_loop_body = true` and `state_version = 0`) merges with the
    /// fold's own unpack into one verified `Threaded` node. Field
    /// assignment and local-var assignment lower through the SAME shared
    /// `Bind` producers [`Self::lower_letrec_field_assignment`]/
    /// [`Self::lower_letrec_local_var_assignment`] already reuse
    /// (`lower_field_assignment_bind`/`lower_local_var_assignment_bind`/
    /// `lower_direct_var_update_in_loop_bind` — each documented as
    /// byte-identical to the hand-rolled `Document` producer it replaces);
    /// every other statement shape (self-send, Tier 2 value call,
    /// destructure, inline-conditional-with-mutations, the generic
    /// fallback) and the per-`BodyKind` accumulator epilogue keep their
    /// existing hand-built `Document` text, now embedded as opaque
    /// [`ThreadedStmt::Statement`] entries — mirroring
    /// [`ThreadedStmt::ConditionalLoop`]'s own `exit_arm` precedent (ADR
    /// 0111 §Verifier-honesty: a fold's per-`BodyKind` exit shape is
    /// exactly as legitimately opaque as a loop's exit repack).
    #[allow(clippy::too_many_lines)]
    fn lower_foldl_body(
        &mut self,
        body: &beamtalk_core::ast::Block,
        plan: &ThreadingPlan,
        kind: &BodyKind,
        frame: FrameId,
    ) -> Result<Vec<ThreadedStmt>> {
        let filtered_body = super::super::util::collect_body_exprs(&body.body);

        let mut stmts: Vec<ThreadedStmt> = Vec::new();
        let mut has_mutations = false;
        let mut has_plain_lets = false;

        // For predicate-based body kinds, allocate a pred_var upfront.
        let pred_var: Option<String> = if matches!(
            kind,
            BodyKind::FoldlFilter { .. }
                | BodyKind::FoldlBoolPredicate { .. }
                | BodyKind::FoldlDetect { .. }
                | BodyKind::FoldlCount
                | BodyKind::FoldlTakeWhile { .. }
                | BodyKind::FoldlDropWhile { .. }
                | BodyKind::FoldlPartition { .. }
                | BodyKind::FoldlGroupBy { .. }
        ) {
            Some(self.fresh_temp_var("Pred"))
        } else {
            None
        };

        for (i, expr) in filtered_body.iter().enumerate() {
            let is_last = i == filtered_body.len() - 1;
            let span = expr.span();

            // `expr` is a top-level statement of THIS loop/fold's
            // own body — if it's itself a nested loop/fold whose own body
            // threads a `ClassVars` mutation, no downstream branch (this
            // function's own field-assignment/self-send/tier2/local-var/
            // destructure/control-flow-has-mutations dispatch above
            // `emit_non_assign_expr`'s generic fallback, direct-params, the
            // `is_last`/`element(2)` unpacks, or `push_discarded_stmt`'s
            // open-scope tracking) unpacks that nested construct's
            // `ClassVars` value, so the mutation would be silently lost or
            // (for a nested `Foldl*`, confirmed empirically) crash `erlc`
            // with an unbound-variable error — regardless of which branch
            // below would otherwise handle `expr`, `is_last`, or
            // `plan.threads_class_vars`. Checked once here, at the very top
            // of the per-statement loop (ahead of every dispatch branch,
            // not just `emit_non_assign_expr`'s fallback) because a nested
            // `do:`/`collect:`/etc. statement is classified
            // `DispatchKind::ControlFlow` (`is_state_threading_keyword_selector`
            // includes the `Foldl*` selectors, not just conditionals) and so
            // is actually intercepted by the `control_flow_has_mutations`
            // branch below, never reaching `emit_non_assign_expr` at all —
            // confirmed empirically when this check lived only there and
            // silently failed to catch the `Foldl*`-in-`Foldl*` repro.
            // Reject rather than emit code that's silently wrong or
            // malformed — see `CodeGenError::ClassVarMutationLostAcrossNestedLoop`'s
            // doc comment.
            if let Some(mutation) = self.nested_loop_lost_class_var_mutation(expr) {
                let location = self.span_to_line(expr.span()).map_or_else(
                    || format!("offset {}", expr.span().start()),
                    |line| format!("line {line}"),
                );
                return Err(CodeGenError::ClassVarMutationLostAcrossNestedLoop {
                    mutation,
                    location,
                });
            }

            // the value-type `Self` mirror of the check just above,
            // with the same deliberate scope limit — nothing unpacks a
            // nested Letrec loop's own trailing `Self` tuple slot back into
            // THIS body's statement sequence, so the inner mutation would be
            // silently discarded once the inner loop exits. See
            // `nested_loop_lost_value_self_mutation`'s doc comment.
            if let Some(mutation) = self.nested_loop_lost_value_self_mutation(expr) {
                let location = self.span_to_line(expr.span()).map_or_else(
                    || format!("offset {}", expr.span().start()),
                    |line| format!("line {line}"),
                );
                return Err(CodeGenError::ValueSelfMutationLostAcrossNestedLoop {
                    mutation,
                    location,
                });
            }

            // BT-3488: the `Foldl*` half of the same check the Letrec body
            // makes — and strictly broader there, since `threads_value_self`
            // is never set for a `Foldl*` plan (a fold accumulator has no
            // trailing `Self` slot), so EVERY value-type field write in this
            // body is unthreadable, top-level statement included. Same shared
            // helper, so the two call sites cannot drift; see
            // `reject_unthreadable_value_self_field_write`.
            self.reject_unthreadable_value_self_field_write(expr, plan.threads_value_self)?;

            if Self::is_field_assignment(expr) {
                has_mutations = true;
                // ADR 0111 Addendum 15: reuses the SAME `Bind` producer
                // `lower_letrec_field_assignment`'s own non-class-var branch
                // calls — `lower_field_assignment_bind`'s doc comment
                // documents it as byte-identical to
                // `generate_field_assignment_open`'s hand-rolled `Document`
                // (same helper calls, same mint order, including its own
                // `thread_ahead` step). A direct class-var write
                // never reaches here (`reject_class_var_field_assignment`,
                // called internally, rejects it at compile time — unchanged
                // by this migration; only a same-class self-send can mutate
                // a class var inside a `Foldl*` body, handled below).
                self.lower_field_assignment_bind(expr, frame, span, &mut stmts)?;
                if is_last {
                    self.emit_field_assign_last_expr(&mut stmts, kind, pred_var.as_ref(), span);
                }
            } else if self.is_actor_self_send(expr) {
                has_mutations = true;
                // Emit diagnostic for synchronous self-send in loop body.
                self.emit_self_send_in_loop_diagnostic(expr, expr.span());
                let (doc, dispatch_var) = self.generate_self_dispatch_open(expr)?;
                stmts.push(ThreadedStmt::Statement(doc, span));
                if is_last {
                    self.emit_self_send_last_expr(
                        &mut stmts,
                        kind,
                        pred_var.as_ref(),
                        &dispatch_var,
                        span,
                    );
                }
            } else if self.is_tier2_value_call(expr) {
                // a bare (non-assigned) Tier 2 `value(:...)` statement
                // (field-stored or local-var-stored block) inside a foldl-based
                // loop body (do:/collect:/select:/etc). Before this fix, such a
                // statement fell through to `emit_non_assign_expr`, which treats
                // it as an ordinary expression and emits a Tier-1-only apply —
                // crashing with badarity for a genuinely Tier 2 (state-threading)
                // stored block. `generate_tier2_value_call_doc` always returns a
                // {Result, NewState} tuple — unpack it and thread the state
                // forward, mirroring the already-working `Tier2ValueCall`
                // handling in conditionals.rs and gen_server/methods.rs.
                //
                // Excluded for Letrec (whileTrue:/timesRepeat:) loop bodies:
                // this construct's repro and matrix coverage are
                // do:/collect:/nested-do: only.
                has_mutations = true;
                let tuple_var = self.fresh_temp_var("T2LoopTuple");
                let expr_doc = self.generate_tier2_value_call_doc(expr)?;
                let new_state = self.next_state_var();
                stmts.push(ThreadedStmt::Statement(
                    docvec![
                        "let ",
                        leaf::var(tuple_var.clone()),
                        " = ",
                        expr_doc,
                        " in let ",
                        leaf::var(new_state),
                        " = call 'erlang':'element'(2, ",
                        leaf::var(tuple_var.clone()),
                        ") in ",
                    ],
                    span,
                ));
                if is_last {
                    self.emit_tier2_value_call_last_expr(
                        &mut stmts,
                        kind,
                        pred_var.as_ref(),
                        &tuple_var,
                        span,
                    );
                }
            } else if Self::is_local_var_assignment(expr) {
                if let Some(doc) =
                    self.try_generate_block_local_plain_let(expr, is_last, &plan.threaded_locals)?
                {
                    has_plain_lets = true;
                    stmts.push(ThreadedStmt::Statement(doc, span));
                } else if plan.use_direct_params || plan.use_tuple_acc || plan.use_hybrid_params {
                    // Direct-params, tuple-acc, or
                    // hybrid mode. ADR 0111 Addendum 15: reuses the SAME
                    // `Bind` producer `lower_letrec_local_var_assignment`'s
                    // own direct-params/hybrid branch calls
                    // (`lower_direct_var_update_in_loop_bind`, documented as
                    // byte-identical to `generate_direct_var_update_in_loop`'s
                    // hand-rolled `Document`). That producer binds the local
                    // in generator scope exactly as its `Document`-returning
                    // sibling does, so the newly-bound name is recovered via
                    // `lookup_var` for the epilogue below instead of a
                    // direct return value.
                    has_mutations = true;
                    self.lower_direct_var_update_in_loop_bind(expr, frame, span, &mut stmts)?;
                    let new_var = if let Expression::Assignment { target, .. } = expr
                        && let Expression::Identifier(id) = target.as_ref()
                    {
                        self.lookup_var(&id.name).cloned()
                    } else {
                        None
                    };
                    if is_last {
                        self.emit_local_assign_last_expr(
                            &mut stmts,
                            kind,
                            pred_var.as_ref(),
                            plan,
                            new_var.as_deref(),
                            span,
                        );
                    }
                } else {
                    // ADR 0111 Addendum 15: reuses the SAME `Bind` producer
                    // `lower_letrec_local_var_assignment`'s own `StateAcc`
                    // branch calls (`lower_local_var_assignment_bind`,
                    // documented as byte-identical to
                    // `generate_local_var_assignment_in_loop`'s hand-rolled
                    // `Document`). The returned value var name is discarded
                    // here too, matching the pre-migration call site
                    // (`emit_local_assign_last_expr` always receives `None`
                    // on this path — its `plan.use_tuple_acc` branch, the
                    // only one that reads it, is unreachable here).
                    has_mutations = true;
                    self.lower_local_var_assignment_bind(expr, frame, span, &mut stmts)?;
                    if is_last {
                        self.emit_local_assign_last_expr(
                            &mut stmts,
                            kind,
                            pred_var.as_ref(),
                            plan,
                            None,
                            span,
                        );
                    }
                }
            } else if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                has_plain_lets = true;
                let binding_docs = self.generate_destructure_bindings(pattern, value)?;
                for d in binding_docs {
                    stmts.push(ThreadedStmt::Statement(d, span));
                }
                if is_last {
                    self.emit_destructure_last_expr(
                        &mut stmts,
                        kind,
                        pred_var.as_ref(),
                        has_mutations,
                        span,
                    );
                }
            } else if self.control_flow_has_mutations(expr)
                || Self::inline_conditional_writes_threaded(
                    expr,
                    &plan.threaded_locals,
                    &self.semantic_facts,
                )
            {
                // Inline conditional with mutations returns {Result, NewStateAcc}.
                // Unpack element(2) so subsequent iterations see the updated StateAcc.
                // This applies to ALL foldl body kinds (do:, collect:, select:, reject:,
                // inject:into:), not just do: — otherwise mutations inside conditionals
                // nested within collect:/select:/etc. are silently lost.
                has_mutations = true;
                let tuple_var = self.fresh_temp_var("CondResult");
                // the vars THIS construct itself threads, read before
                // `generate_expression` below (which may push/pop scopes) so
                // the lookup reflects this statement's own captured set.
                let inner_threaded_vars = self.get_control_flow_threaded_vars(expr);
                let doc = self.generate_expression(expr)?;
                let new_state = self.next_state_var();
                stmts.push(ThreadedStmt::Statement(
                    docvec![
                        "let ",
                        leaf::var(tuple_var.clone()),
                        " = ",
                        doc,
                        " in let ",
                        leaf::var(new_state.clone()),
                        " = call 'erlang':'element'(2, ",
                        leaf::var(tuple_var.clone()),
                        ") in ",
                    ],
                    span,
                ));
                // a non-last (or last) ensure:/on:do:/ifNotNil:/nested-loop
                // statement here only bumps the StateAcc *version pointer* above —
                // it does NOT rebind the specific local vars it threads. Both
                // StateAcc (map) mode and tuple-acc mode bind each threaded local
                // to a FIXED Core Erlang variable once per iteration (see
                // `generate_unpack_at_iteration_start`/`generate_foldl_loop_body`),
                // not via a live re-lookup through the state pointer — so without
                // this, any read of the var later in the SAME block invocation
                // (e.g. the next statement) would see the stale pre-statement
                // value instead of what this construct just wrote. Shares
                // `rebind_threaded_vars_from_state` with `conditionals.rs`'s
                // `push_control_flow_threaded_var_rereads`, which does the same
                // rebind for the `ThreadedIr`-rendered conditional-arm path.
                if let Some(inner_vars) = inner_threaded_vars {
                    for d in self.rebind_threaded_vars_from_state(&inner_vars, &new_state) {
                        stmts.push(ThreadedStmt::Statement(d, span));
                    }
                }
                if is_last {
                    match kind {
                        BodyKind::FoldlDo => {
                            // do: discards the result value, returns state only
                            stmts.push(ThreadedStmt::Statement(
                                leaf::var(self.current_state_var()),
                                span,
                            ));
                        }
                        BodyKind::FoldlCollect => {
                            // collect: needs the result value for the list
                            let result_var = self.fresh_temp_var("CondVal");
                            stmts.push(ThreadedStmt::Statement(
                                docvec![
                                    "let ",
                                    leaf::var(result_var.clone()),
                                    " = call 'erlang':'element'(1, ",
                                    leaf::var(tuple_var),
                                    ") in {[",
                                    leaf::var(result_var),
                                    " | AccList], ",
                                    leaf::var(self.current_state_var()),
                                    "}",
                                ],
                                span,
                            ));
                        }
                        BodyKind::FoldlFilter { .. }
                        | BodyKind::FoldlBoolPredicate { .. }
                        | BodyKind::FoldlDetect { .. }
                        | BodyKind::FoldlCount
                        | BodyKind::FoldlTakeWhile { .. }
                        | BodyKind::FoldlDropWhile { .. }
                        | BodyKind::FoldlPartition { .. }
                        | BodyKind::FoldlGroupBy { .. } => {
                            // predicate-based selectors — bind predicate result
                            if let Some(pv) = pred_var.as_ref() {
                                let result_var = self.fresh_temp_var("CondVal");
                                stmts.push(ThreadedStmt::Statement(
                                    docvec![
                                        "let ",
                                        leaf::var(result_var.clone()),
                                        " = call 'erlang':'element'(1, ",
                                        leaf::var(tuple_var),
                                        ") in let ",
                                        leaf::var(pv.clone()),
                                        " = ",
                                        leaf::var(result_var),
                                        " in ",
                                    ],
                                    span,
                                ));
                            }
                        }
                        BodyKind::FoldlInject => {
                            // inject:into: — result is the new accumulator
                            let result_var = self.fresh_temp_var("CondVal");
                            stmts.push(ThreadedStmt::Statement(
                                docvec![
                                    "let ",
                                    leaf::var(result_var.clone()),
                                    " = call 'erlang':'element'(1, ",
                                    leaf::var(tuple_var),
                                    ") in {",
                                    leaf::var(result_var),
                                    ", ",
                                    leaf::var(self.current_state_var()),
                                    "}",
                                ],
                                span,
                            ));
                        }
                    }
                }
            } else {
                // Non-assignment expression: handling depends on BodyKind.
                self.emit_non_assign_expr(
                    &mut stmts,
                    expr,
                    i,
                    is_last,
                    &mut has_mutations,
                    has_plain_lets,
                    kind,
                    pred_var.as_ref(),
                    plan,
                )?;
            }
        }

        // The eight per-`BodyKind` accumulator epilogue blocks below all
        // push exactly one opaque `Statement` — same hand-built `Document`
        // text as before this migration (ADR 0111 §Verifier-honesty: this
        // mirrors `ConditionalLoop`'s own `exit_arm` opacity precedent).
        let epilogue_span = body.span;

        // FoldlFilter: append the predicate case expression after all statements.
        if let BodyKind::FoldlFilter { item_var, negate } = kind {
            if let Some(pv) = &pred_var {
                let condition_doc: Document<'static> = if *negate {
                    docvec!["call 'erlang':'not'(", leaf::var(pv.clone()), ")",]
                } else {
                    leaf::var(pv.clone())
                };
                if plan.use_tuple_acc {
                    // Tuple mode — repack current var bindings into the result tuple.
                    let vars_doc = plan.current_vars_doc(self);
                    stmts.push(ThreadedStmt::Statement(
                        docvec![
                            "case ",
                            condition_doc,
                            " of <'true'> when 'true' -> {[",
                            leaf::var(item_var.clone()),
                            " | AccList], ",
                            vars_doc.clone(),
                            "} <'false'> when 'true' -> {AccList, ",
                            vars_doc,
                            "} end",
                        ],
                        epilogue_span,
                    ));
                } else {
                    let final_state = if has_mutations {
                        self.current_state_var()
                    } else {
                        "StateAcc".to_string()
                    };
                    stmts.push(ThreadedStmt::Statement(
                        docvec![
                            "case ",
                            condition_doc,
                            " of <'true'> when 'true' -> {[",
                            leaf::var(item_var.clone()),
                            " | AccList], ",
                            leaf::var(final_state.clone()),
                            "} <'false'> when 'true' -> {AccList, ",
                            leaf::var(final_state),
                            "} end",
                        ],
                        epilogue_span,
                    ));
                }
            }
        }

        // FoldlBoolPredicate — update boolean accumulator based on predicate result.
        // Match only 'true'/'false' explicitly (consistent with FoldlFilter and lists:any/all).
        if let BodyKind::FoldlBoolPredicate { is_all } = kind {
            if let Some(pv) = &pred_var {
                if plan.use_tuple_acc {
                    let vars_doc = plan.current_vars_doc(self);
                    if *is_all {
                        // allSatisfy: pred=false → set BoolAcc to false; pred=true → keep
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "case ",
                                leaf::var(pv.clone()),
                                " of <'false'> when 'true' -> {'false', ",
                                vars_doc.clone(),
                                "} <'true'> when 'true' -> {BoolAcc, ",
                                vars_doc,
                                "} end",
                            ],
                            epilogue_span,
                        ));
                    } else {
                        // anySatisfy: pred=true → set BoolAcc to true; pred=false → keep
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "case ",
                                leaf::var(pv.clone()),
                                " of <'true'> when 'true' -> {'true', ",
                                vars_doc.clone(),
                                "} <'false'> when 'true' -> {BoolAcc, ",
                                vars_doc,
                                "} end",
                            ],
                            epilogue_span,
                        ));
                    }
                } else {
                    let final_state = if has_mutations {
                        self.current_state_var()
                    } else {
                        "StateAcc".to_string()
                    };
                    if *is_all {
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "case ",
                                leaf::var(pv.clone()),
                                " of <'false'> when 'true' -> {'false', ",
                                leaf::var(final_state.clone()),
                                "} <'true'> when 'true' -> {BoolAcc, ",
                                leaf::var(final_state),
                                "} end",
                            ],
                            epilogue_span,
                        ));
                    } else {
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "case ",
                                leaf::var(pv.clone()),
                                " of <'true'> when 'true' -> {'true', ",
                                leaf::var(final_state.clone()),
                                "} <'false'> when 'true' -> {BoolAcc, ",
                                leaf::var(final_state),
                                "} end",
                            ],
                            epilogue_span,
                        ));
                    }
                }
            }
        }

        // FoldlDetect — update found-item accumulator on first match.
        // Accumulator is {FoundItem, FoundFlag, StateVars...}.
        // Only update FoundItem when pred=true AND FoundFlag='false' (first match only).
        if let BodyKind::FoldlDetect { item_var } = kind {
            if let Some(pv) = &pred_var {
                if plan.use_tuple_acc {
                    let vars_doc = plan.current_vars_doc(self);
                    stmts.push(ThreadedStmt::Statement(
                        docvec![
                            "case ",
                            leaf::var(pv.clone()),
                            " of <'true'> when 'true' -> case FoundFlag of <'false'> when 'true' -> {",
                            leaf::var(item_var.clone()),
                            ", 'true', ",
                            vars_doc.clone(),
                            "} <'true'> when 'true' -> {FoundItem, 'true', ",
                            vars_doc.clone(),
                            "} end <'false'> when 'true' -> {FoundItem, FoundFlag, ",
                            vars_doc,
                            "} end",
                        ],
                        epilogue_span,
                    ));
                } else {
                    let final_state = if has_mutations {
                        self.current_state_var()
                    } else {
                        "StateAcc".to_string()
                    };
                    stmts.push(ThreadedStmt::Statement(
                        docvec![
                            "case ",
                            leaf::var(pv.clone()),
                            " of <'true'> when 'true' -> case FoundFlag of <'false'> when 'true' -> {",
                            leaf::var(item_var.clone()),
                            ", 'true', ",
                            leaf::var(final_state.clone()),
                            "} <'true'> when 'true' -> {FoundItem, 'true', ",
                            leaf::var(final_state.clone()),
                            "} end <'false'> when 'true' -> {FoundItem, FoundFlag, ",
                            leaf::var(final_state),
                            "} end",
                        ],
                        epilogue_span,
                    ));
                }
            }
        }

        // FoldlCount — increment count accumulator on predicate match.
        // Accumulator is {Count, StateVars...}.
        if matches!(kind, BodyKind::FoldlCount) {
            if let Some(pv) = &pred_var {
                if plan.use_tuple_acc {
                    let vars_doc = plan.current_vars_doc(self);
                    stmts.push(ThreadedStmt::Statement(
                        docvec![
                            "case ",
                            leaf::var(pv.clone()),
                            " of <'true'> when 'true' -> {call 'erlang':'+'(CountAcc, 1), ",
                            vars_doc.clone(),
                            "} <'false'> when 'true' -> {CountAcc, ",
                            vars_doc,
                            "} end",
                        ],
                        epilogue_span,
                    ));
                } else {
                    let final_state = if has_mutations {
                        self.current_state_var()
                    } else {
                        "StateAcc".to_string()
                    };
                    stmts.push(ThreadedStmt::Statement(
                        docvec![
                            "case ",
                            leaf::var(pv.clone()),
                            " of <'true'> when 'true' -> {call 'erlang':'+'(CountAcc, 1), ",
                            leaf::var(final_state.clone()),
                            "} <'false'> when 'true' -> {CountAcc, ",
                            leaf::var(final_state),
                            "} end",
                        ],
                        epilogue_span,
                    ));
                }
            }
        }

        // FoldlTakeWhile — include item while predicate holds.
        // Once predicate returns false, StillTaking flips to false and all subsequent
        // elements are excluded. Accumulator: {ResultList, StillTaking, StateVars...}.
        if let BodyKind::FoldlTakeWhile { item_var } = kind {
            if let Some(pv) = &pred_var {
                if plan.use_tuple_acc {
                    let vars_doc = plan.current_vars_doc(self);
                    stmts.push(ThreadedStmt::Statement(
                        docvec![
                            "case StillTaking of \
                             <'false'> when 'true' -> {AccList, 'false', ",
                            vars_doc.clone(),
                            "} <'true'> when 'true' -> case ",
                            leaf::var(pv.clone()),
                            " of <'true'> when 'true' -> {[",
                            leaf::var(item_var.clone()),
                            " | AccList], 'true', ",
                            vars_doc.clone(),
                            "} <'false'> when 'true' -> {AccList, 'false', ",
                            vars_doc,
                            "} end end",
                        ],
                        epilogue_span,
                    ));
                } else {
                    let final_state = if has_mutations {
                        self.current_state_var()
                    } else {
                        "StateAcc".to_string()
                    };
                    stmts.push(ThreadedStmt::Statement(
                        docvec![
                            "case StillTaking of \
                             <'false'> when 'true' -> {AccList, 'false', ",
                            leaf::var(final_state.clone()),
                            "} <'true'> when 'true' -> case ",
                            leaf::var(pv.clone()),
                            " of <'true'> when 'true' -> {[",
                            leaf::var(item_var.clone()),
                            " | AccList], 'true', ",
                            leaf::var(final_state.clone()),
                            "} <'false'> when 'true' -> {AccList, 'false', ",
                            leaf::var(final_state),
                            "} end end",
                        ],
                        epilogue_span,
                    ));
                }
            }
        }

        // FoldlDropWhile — drop items while predicate holds.
        // Once predicate returns false, StillDropping flips to false and all subsequent
        // elements are included. Accumulator: {ResultList, StillDropping, StateVars...}.
        if let BodyKind::FoldlDropWhile { item_var } = kind {
            if let Some(pv) = &pred_var {
                if plan.use_tuple_acc {
                    let vars_doc = plan.current_vars_doc(self);
                    stmts.push(ThreadedStmt::Statement(
                        docvec![
                            "case StillDropping of \
                             <'false'> when 'true' -> {[",
                            leaf::var(item_var.clone()),
                            " | AccList], 'false', ",
                            vars_doc.clone(),
                            "} <'true'> when 'true' -> case ",
                            leaf::var(pv.clone()),
                            " of <'true'> when 'true' -> {AccList, 'true', ",
                            vars_doc.clone(),
                            "} <'false'> when 'true' -> {[",
                            leaf::var(item_var.clone()),
                            " | AccList], 'false', ",
                            vars_doc,
                            "} end end",
                        ],
                        epilogue_span,
                    ));
                } else {
                    let final_state = if has_mutations {
                        self.current_state_var()
                    } else {
                        "StateAcc".to_string()
                    };
                    stmts.push(ThreadedStmt::Statement(
                        docvec![
                            "case StillDropping of \
                             <'false'> when 'true' -> {[",
                            leaf::var(item_var.clone()),
                            " | AccList], 'false', ",
                            leaf::var(final_state.clone()),
                            "} <'true'> when 'true' -> case ",
                            leaf::var(pv.clone()),
                            " of <'true'> when 'true' -> {AccList, 'true', ",
                            leaf::var(final_state.clone()),
                            "} <'false'> when 'true' -> {[",
                            leaf::var(item_var.clone()),
                            " | AccList], 'false', ",
                            leaf::var(final_state),
                            "} end end",
                        ],
                        epilogue_span,
                    ));
                }
            }
        }

        // FoldlPartition — route item to one of two lists based on predicate.
        // Accumulator: {MatchList, NoMatchList, StateVars...}.
        if let BodyKind::FoldlPartition { item_var } = kind {
            if let Some(pv) = &pred_var {
                if plan.use_tuple_acc {
                    let vars_doc = plan.current_vars_doc(self);
                    stmts.push(ThreadedStmt::Statement(
                        docvec![
                            "case ",
                            leaf::var(pv.clone()),
                            " of <'true'> when 'true' -> {[",
                            leaf::var(item_var.clone()),
                            " | MatchList], NoMatchList, ",
                            vars_doc.clone(),
                            "} <'false'> when 'true' -> {MatchList, [",
                            leaf::var(item_var.clone()),
                            " | NoMatchList], ",
                            vars_doc,
                            "} end",
                        ],
                        epilogue_span,
                    ));
                } else {
                    let final_state = if has_mutations {
                        self.current_state_var()
                    } else {
                        "StateAcc".to_string()
                    };
                    stmts.push(ThreadedStmt::Statement(
                        docvec![
                            "case ",
                            leaf::var(pv.clone()),
                            " of <'true'> when 'true' -> {[",
                            leaf::var(item_var.clone()),
                            " | MatchList], NoMatchList, ",
                            leaf::var(final_state.clone()),
                            "} <'false'> when 'true' -> {MatchList, [",
                            leaf::var(item_var.clone()),
                            " | NoMatchList], ",
                            leaf::var(final_state),
                            "} end",
                        ],
                        epilogue_span,
                    ));
                }
            }
        }

        // FoldlGroupBy — group item by key.
        // The pred_var holds the key result. Each element is added to the key's list in a map.
        // Accumulator: {Map, StateVars...}.
        if let BodyKind::FoldlGroupBy { item_var } = kind {
            if let Some(pv) = &pred_var {
                // Use maps:get/3 to get current list for key (default []), prepend item, put back.
                let key_var = pv;
                if plan.use_tuple_acc {
                    let vars_doc = plan.current_vars_doc(self);
                    let existing_var = self.fresh_temp_var("ExistingList");
                    let new_list_var = self.fresh_temp_var("NewList");
                    let new_map_var = self.fresh_temp_var("NewMap");
                    stmts.push(ThreadedStmt::Statement(
                        docvec![
                            "let ",
                            leaf::var(existing_var.clone()),
                            " = call 'maps':'get'(",
                            leaf::var(key_var.clone()),
                            ", GroupMap, []) in let ",
                            leaf::var(new_list_var.clone()),
                            " = [",
                            leaf::var(item_var.clone()),
                            " | ",
                            leaf::var(existing_var),
                            "] in let ",
                            leaf::var(new_map_var.clone()),
                            " = call 'maps':'put'(",
                            leaf::var(key_var.clone()),
                            ", ",
                            leaf::var(new_list_var),
                            ", GroupMap) in {",
                            leaf::var(new_map_var),
                            ", ",
                            vars_doc,
                            "}",
                        ],
                        epilogue_span,
                    ));
                } else {
                    let final_state = if has_mutations {
                        self.current_state_var()
                    } else {
                        "StateAcc".to_string()
                    };
                    let existing_var = self.fresh_temp_var("ExistingList");
                    let new_list_var = self.fresh_temp_var("NewList");
                    let new_map_var = self.fresh_temp_var("NewMap");
                    stmts.push(ThreadedStmt::Statement(
                        docvec![
                            "let ",
                            leaf::var(existing_var.clone()),
                            " = call 'maps':'get'(",
                            leaf::var(key_var.clone()),
                            ", GroupMap, []) in let ",
                            leaf::var(new_list_var.clone()),
                            " = [",
                            leaf::var(item_var.clone()),
                            " | ",
                            leaf::var(existing_var),
                            "] in let ",
                            leaf::var(new_map_var.clone()),
                            " = call 'maps':'put'(",
                            leaf::var(key_var.clone()),
                            ", ",
                            leaf::var(new_list_var),
                            ", GroupMap) in {",
                            leaf::var(new_map_var),
                            ", ",
                            leaf::var(final_state),
                            "}",
                        ],
                        epilogue_span,
                    ));
                }
            }
        }

        // ADR 0111 Addendum 9, Question 6: whenever this fold body
        // threads `ClassVars`, wrap its returned TAIL VALUE — regardless of
        // which `BodyKind` arm above produced it, and regardless of that
        // arm's own internal shape (a bare `StateAcc`, `{[Result|AccList],
        // StateAcc}`, `{AccOut, StateAcc}`, a filter/predicate tuple, …) —
        // as `{ClassVars, <original tail value>}`. This is the single choke
        // point every `Foldl*` exit arm's tail value flows through
        // (`generate_threaded_loop_body`'s only call site into this
        // function), so it closes the silent-loss gap uniformly
        // without touching any of the ~15 individual exit-arm branches
        // above: each keeps building exactly the value it always did.
        //
        // Deliberately `docs.pop()` + re-push, NOT `let FoldTail = <all of
        // docs> in {ClassVars, FoldTail}` (an earlier, rejected version of
        // this fix): `docs` is an OPEN Core Erlang let-chain — every element
        // but the last ends in `in `, and the last is a bare tail
        // expression, still lexically inside every preceding `let`'s scope.
        // Wrapping the WHOLE chain as the RHS of a fresh `let` closes that
        // scope at the chain's own tail expression, making any name a
        // mid-chain statement bound (e.g. a self-send's own `ClassVarsN`
        // rebind, `emit_class_var_result_unwrap`) unreachable from outside
        // — confirmed the hard way: `erlc` rejected it with "unbound
        // variable", not a scoping warning. Popping and rewrapping only the
        // last element leaves every earlier `let`'s scope untouched and
        // still open, so `cv` (itself possibly bound by one of those
        // `let`s) stays visible at the exact point it's used.
        //
        // Read AFTER the loop body is fully generated (not before) so a
        // class-method self-send's own `ClassVarsN` rebind inside this
        // iteration (`emit_class_var_result_unwrap`, frame-scoped to this
        // loop body's `current_branch_frame()` per Question 2) is reflected.
        //
        // this `{ClassVars, tail}` accumulator wrap is the `Foldl*`
        // shape's own mechanism (Question 6) — `while_loops.rs`/
        // `counted_loops.rs`'s Letrec loops build their own, textually
        // different `{ClassVars1, <tail>}` true-arm shape via the loop's
        // extra recursive-tail-call fun parameter (Question 3), lowered
        // through `ThreadedStmt::ConditionalLoop` instead (ADR 0111
        // Addendum 15) — this function's callers now only ever pass a
        // `Foldl*` `kind`, so `plan.threads_class_vars` here always means
        // this shape.
        if plan.threads_class_vars {
            let cv = self.current_class_var();
            // record this closure's peak class-var version (BEFORE
            // `with_branch_context`'s guard restores it on drop, right after
            // this function returns) so `ThreadingPlan::foldl_call_doc` can
            // fast-forward past it — see `last_foldl_class_var_peak`'s own
            // doc comment for why a naive post-fold `next_class_var()` call
            // would otherwise mint an already-used name.
            self.set_foldl_class_var_peak(self.class_var_version());
            let ThreadedStmt::Statement(tail, tail_span) = stmts
                .pop()
                .expect("a Foldl* body must push at least one tail-expression Statement")
            else {
                unreachable!(
                    "every Foldl* body push above is a ThreadedStmt::Statement by construction"
                );
            };
            stmts.push(ThreadedStmt::Statement(
                docvec!["{", leaf::var(cv), ", ", tail, "}"],
                tail_span,
            ));
        }
        Ok(stmts)
    }

    // ── Body finalizer helpers (called from lower_foldl_body) ────────────────

    fn emit_field_assign_last_expr(
        &self,
        stmts: &mut Vec<ThreadedStmt>,
        kind: &BodyKind,
        pred_var: Option<&String>,
        span: Span,
    ) {
        match kind {
            BodyKind::FoldlDo => {
                stmts.push(ThreadedStmt::Statement(
                    leaf::var(self.current_state_var()),
                    span,
                ));
            }
            BodyKind::FoldlCollect => {
                stmts.push(ThreadedStmt::Statement(
                    docvec![
                        "{[_Val | AccList], ",
                        leaf::var(self.current_state_var()),
                        "}",
                    ],
                    span,
                ));
            }
            BodyKind::FoldlFilter { .. }
            | BodyKind::FoldlBoolPredicate { .. }
            | BodyKind::FoldlDetect { .. }
            | BodyKind::FoldlCount
            | BodyKind::FoldlTakeWhile { .. }
            | BodyKind::FoldlDropWhile { .. }
            | BodyKind::FoldlPartition { .. }
            | BodyKind::FoldlGroupBy { .. } => {
                if let Some(pv) = pred_var {
                    stmts.push(ThreadedStmt::Statement(
                        docvec!["let ", leaf::var(pv.clone()), " = _Val in ",],
                        span,
                    ));
                }
            }
            BodyKind::FoldlInject => {
                stmts.push(ThreadedStmt::Statement(
                    docvec!["{_Val, ", leaf::var(self.current_state_var()), "}",],
                    span,
                ));
            }
        }
    }

    fn emit_self_send_last_expr(
        &mut self,
        stmts: &mut Vec<ThreadedStmt>,
        kind: &BodyKind,
        pred_var: Option<&String>,
        dispatch_var: &str,
        span: Span,
    ) {
        match kind {
            BodyKind::FoldlDo => {
                stmts.push(ThreadedStmt::Statement(
                    leaf::var(self.current_state_var()),
                    span,
                ));
            }
            BodyKind::FoldlCollect => {
                let fs = self.current_state_var();
                let ir = self.fresh_temp_var("ItemResult");
                stmts.push(ThreadedStmt::Statement(
                    docvec![
                        "let ",
                        leaf::var(ir.clone()),
                        " = call 'erlang':'element'(1, ",
                        leaf::var(dispatch_var.to_string()),
                        ") in {[",
                        leaf::var(ir),
                        " | AccList], ",
                        leaf::var(fs),
                        "}",
                    ],
                    span,
                ));
            }
            BodyKind::FoldlFilter { .. }
            | BodyKind::FoldlBoolPredicate { .. }
            | BodyKind::FoldlDetect { .. }
            | BodyKind::FoldlCount
            | BodyKind::FoldlTakeWhile { .. }
            | BodyKind::FoldlDropWhile { .. }
            | BodyKind::FoldlPartition { .. }
            | BodyKind::FoldlGroupBy { .. } => {
                if let Some(pv) = pred_var {
                    stmts.push(ThreadedStmt::Statement(
                        docvec![
                            "let ",
                            leaf::var(pv.clone()),
                            " = call 'erlang':'element'(1, ",
                            leaf::var(dispatch_var.to_string()),
                            ") in ",
                        ],
                        span,
                    ));
                }
            }
            BodyKind::FoldlInject => {
                let fs = self.current_state_var();
                let ar = self.fresh_temp_var("AccResult");
                stmts.push(ThreadedStmt::Statement(
                    docvec![
                        "let ",
                        leaf::var(ar.clone()),
                        " = call 'erlang':'element'(1, ",
                        leaf::var(dispatch_var.to_string()),
                        ") in {",
                        leaf::var(ar),
                        ", ",
                        leaf::var(fs),
                        "}",
                    ],
                    span,
                ));
            }
        }
    }

    /// emits the `is_last`-position tail for a bare Tier 2 `value(:...)`
    /// loop-body statement, per `BodyKind`. `tuple_var` holds the full
    /// `{Result, NewState}` tuple returned by `generate_tier2_value_call_doc`;
    /// `self.current_state_var()` already reflects `NewState` (bound by the
    /// caller before this is invoked). Mirrors `emit_self_send_last_expr`.
    fn emit_tier2_value_call_last_expr(
        &mut self,
        stmts: &mut Vec<ThreadedStmt>,
        kind: &BodyKind,
        pred_var: Option<&String>,
        tuple_var: &str,
        span: Span,
    ) {
        match kind {
            BodyKind::FoldlDo => {
                stmts.push(ThreadedStmt::Statement(
                    leaf::var(self.current_state_var()),
                    span,
                ));
            }
            BodyKind::FoldlCollect => {
                let fs = self.current_state_var();
                let ir = self.fresh_temp_var("T2LoopVal");
                stmts.push(ThreadedStmt::Statement(
                    docvec![
                        "let ",
                        leaf::var(ir.clone()),
                        " = call 'erlang':'element'(1, ",
                        leaf::var(tuple_var.to_string()),
                        ") in {[",
                        leaf::var(ir),
                        " | AccList], ",
                        leaf::var(fs),
                        "}",
                    ],
                    span,
                ));
            }
            BodyKind::FoldlFilter { .. }
            | BodyKind::FoldlBoolPredicate { .. }
            | BodyKind::FoldlDetect { .. }
            | BodyKind::FoldlCount
            | BodyKind::FoldlTakeWhile { .. }
            | BodyKind::FoldlDropWhile { .. }
            | BodyKind::FoldlPartition { .. }
            | BodyKind::FoldlGroupBy { .. } => {
                if let Some(pv) = pred_var {
                    stmts.push(ThreadedStmt::Statement(
                        docvec![
                            "let ",
                            leaf::var(pv.clone()),
                            " = call 'erlang':'element'(1, ",
                            leaf::var(tuple_var.to_string()),
                            ") in ",
                        ],
                        span,
                    ));
                }
            }
            BodyKind::FoldlInject => {
                let fs = self.current_state_var();
                let ar = self.fresh_temp_var("T2LoopVal");
                stmts.push(ThreadedStmt::Statement(
                    docvec![
                        "let ",
                        leaf::var(ar.clone()),
                        " = call 'erlang':'element'(1, ",
                        leaf::var(tuple_var.to_string()),
                        ") in {",
                        leaf::var(ar),
                        ", ",
                        leaf::var(fs),
                        "}",
                    ],
                    span,
                ));
            }
        }
    }

    fn emit_local_assign_last_expr(
        &self,
        stmts: &mut Vec<ThreadedStmt>,
        kind: &BodyKind,
        pred_var: Option<&String>,
        plan: &ThreadingPlan,
        last_val: Option<&str>,
        span: Span,
    ) {
        if plan.use_tuple_acc {
            // Tuple mode — repack current bindings as tuple accumulator.
            // `last_val` is the newly-bound variable name from `generate_direct_var_update_in_loop`
            // (e.g. `"Sum1"`). Used for FoldlCollect/FoldlFilter/FoldlInject where the loop
            // result value must be referenced explicitly; falls back to `"_Val"` when not set.
            let val = last_val.unwrap_or("_Val");
            let vars_doc = plan.current_vars_doc(self);
            match kind {
                BodyKind::FoldlDo => {
                    stmts.push(ThreadedStmt::Statement(docvec![" {", vars_doc, "}"], span));
                }
                BodyKind::FoldlCollect => {
                    stmts.push(ThreadedStmt::Statement(
                        docvec![
                            " {[",
                            leaf::var(val.to_string()),
                            " | AccList], ",
                            vars_doc,
                            "}",
                        ],
                        span,
                    ));
                }
                BodyKind::FoldlFilter { .. }
                | BodyKind::FoldlBoolPredicate { .. }
                | BodyKind::FoldlDetect { .. }
                | BodyKind::FoldlCount
                | BodyKind::FoldlTakeWhile { .. }
                | BodyKind::FoldlDropWhile { .. }
                | BodyKind::FoldlPartition { .. }
                | BodyKind::FoldlGroupBy { .. } => {
                    if let Some(pv) = pred_var {
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                " let ",
                                leaf::var(pv.clone()),
                                " = ",
                                leaf::var(val.to_string()),
                                " in ",
                            ],
                            span,
                        ));
                    }
                }
                BodyKind::FoldlInject => {
                    stmts.push(ThreadedStmt::Statement(
                        docvec![" {", leaf::var(val.to_string()), ", ", vars_doc, "}",],
                        span,
                    ));
                }
            }
            return;
        }
        match kind {
            BodyKind::FoldlDo => {
                stmts.push(ThreadedStmt::Statement(
                    docvec![" ", leaf::var(self.current_state_var())],
                    span,
                ));
            }
            BodyKind::FoldlCollect => {
                stmts.push(ThreadedStmt::Statement(
                    docvec![
                        " {[_Val | AccList], ",
                        leaf::var(self.current_state_var()),
                        "}",
                    ],
                    span,
                ));
            }
            BodyKind::FoldlFilter { .. }
            | BodyKind::FoldlBoolPredicate { .. }
            | BodyKind::FoldlDetect { .. }
            | BodyKind::FoldlCount
            | BodyKind::FoldlTakeWhile { .. }
            | BodyKind::FoldlDropWhile { .. }
            | BodyKind::FoldlPartition { .. }
            | BodyKind::FoldlGroupBy { .. } => {
                if let Some(pv) = pred_var {
                    stmts.push(ThreadedStmt::Statement(
                        docvec![" let ", leaf::var(pv.clone()), " = _Val in ",],
                        span,
                    ));
                }
            }
            BodyKind::FoldlInject => {
                stmts.push(ThreadedStmt::Statement(
                    docvec![" {_Val, ", leaf::var(self.current_state_var()), "}",],
                    span,
                ));
            }
        }
    }

    fn emit_destructure_last_expr(
        &self,
        stmts: &mut Vec<ThreadedStmt>,
        kind: &BodyKind,
        pred_var: Option<&String>,
        has_mutations: bool,
        span: Span,
    ) {
        match kind {
            BodyKind::FoldlDo => {
                stmts.push(ThreadedStmt::Statement(
                    leaf::var(self.current_state_var()),
                    span,
                ));
            }
            BodyKind::FoldlCollect => {
                let fs = if has_mutations {
                    self.current_state_var()
                } else {
                    "StateAcc".to_string()
                };
                stmts.push(ThreadedStmt::Statement(
                    docvec!["{['nil' | AccList], ", leaf::var(fs), "}",],
                    span,
                ));
            }
            BodyKind::FoldlFilter { .. }
            | BodyKind::FoldlBoolPredicate { .. }
            | BodyKind::FoldlDetect { .. }
            | BodyKind::FoldlCount
            | BodyKind::FoldlTakeWhile { .. }
            | BodyKind::FoldlDropWhile { .. }
            | BodyKind::FoldlPartition { .. }
            | BodyKind::FoldlGroupBy { .. } => {
                if let Some(pv) = pred_var {
                    stmts.push(ThreadedStmt::Statement(
                        docvec!["let ", leaf::var(pv.clone()), " = 'false' in ",],
                        span,
                    ));
                }
            }
            BodyKind::FoldlInject => {
                let fs = if has_mutations {
                    self.current_state_var()
                } else {
                    "StateAcc".to_string()
                };
                stmts.push(ThreadedStmt::Statement(
                    docvec!["{'nil', ", leaf::var(fs), "}"],
                    span,
                ));
            }
        }
    }

    /// ADR 0111 Addendum 9, Question 6: builds `"let <result_var> =
    /// <expr's value> in "` — the exact prelude every `is_last` `Foldl*`
    /// exit arm below builds by hand.
    ///
    /// ADR 0118 phase 5b: `expr`'s own top-level class-var
    /// producer (a same-class self-send or a class-var assignment) is now
    /// threaded ahead of this call by the caller's own `thread_ahead`
    /// (`emit_non_assign_expr`'s first statement), which splices a real
    /// `Bind` into the fold body's own frame — visible to every subsequent
    /// statement (including this function's own final `{ClassVars, tail}`
    /// wrap) without needing a scope to be kept open and re-paired here.
    /// The 2-tuple-pairing dance this replaced was the pre-ADR-0118
    /// mechanism for keeping a self-send's `ClassVarsN` rebind visible past
    /// its own closed-expression boundary — `plan.threads_class_vars`
    /// itself no longer changes what this function builds, since there is
    /// no separate scope left to pair.
    fn bind_closed_expr_threading_class_vars(
        &mut self,
        expr: &Expression,
        result_var: &str,
        _plan: &ThreadingPlan,
    ) -> Result<Document<'static>> {
        // `expr` may dispatch a class-method self-send (locally declared or
        // inherited) that rebinds `ClassVarsN` opaquely, closed
        // by the time this call returns — `refresh_class_var_after_opaque_scope`
        // recovers the live value via the ADR 0110 shadow write (rather than
        // relying on lexical scope) so the fold's own `{ClassVars, tail}`
        // wrap, built from `current_class_var()` after this call, sees it
        // regardless of nesting depth.
        let cv_version_before = self.class_var_version();
        let expr_code = self.expression_doc(expr)?;
        let refresh = self
            .refresh_class_var_after_opaque_scope(cv_version_before)
            .unwrap_or(Document::Nil);
        Ok(docvec![
            "let ",
            leaf::var(result_var.to_string()),
            " = ",
            expr_code,
            " in ",
            refresh,
        ])
    }

    #[allow(
        clippy::too_many_arguments,
        clippy::fn_params_excessive_bools,
        clippy::too_many_lines
    )]
    fn emit_non_assign_expr(
        &mut self,
        stmts: &mut Vec<ThreadedStmt>,
        expr: &Expression,
        _i: usize,
        is_last: bool,
        has_mutations: &mut bool,
        has_plain_lets: bool,
        kind: &BodyKind,
        pred_var: Option<&String>,
        plan: &ThreadingPlan,
    ) -> Result<()> {
        let span = expr.span();
        // ADR 0118 phase 2b: thread every state-effecting
        // sub-expression nested in `expr` (e.g. `1 + (self bumpCount)`)
        // ahead of `expr`'s own compile, via the sequencing rule
        // (`Self::thread_ahead`) — the drop-in replacement for the earlier
        // planner-based emission (now deleted). Every branch below compiles `expr` through
        // `expression_doc`/`closed_expression_doc`/
        // `bind_closed_expr_threading_class_vars`/`push_discarded_stmt` —
        // all four route through `generate_expression`, so whichever one
        // fires consults the registered substitution; `finish_precompiled_scope`
        // is therefore called once, after the whole `match` below, rather
        // than duplicated at every one of those call sites. Must run before
        // every arm below reads `state_version()`/`current_state_var()`/
        // `*has_mutations` — a threaded prelude is exactly as
        // state-advancing as any other mutation this function already
        // detects, so `*has_mutations` is set here BEFORE any arm's own
        // `current_state_var()` vs `"StateAcc"` choice reads it (both the
        // ones inside this function and the caller's own post-loop
        // `FoldlFilter`/`FoldlBoolPredicate`-family wrap, which reads the
        // same accumulator after this call returns).
        let frame = self.current_frame();
        let mut prelude_stmts: Vec<ThreadedStmt> = Vec::new();
        let thread_scope = self.thread_ahead(expr, &mut prelude_stmts, frame)?;
        let hoisted_anything = !prelude_stmts.is_empty();
        if hoisted_anything {
            stmts.extend(prelude_stmts);
            *has_mutations = true;
        }
        let has_mutations = *has_mutations;

        // the nested-loop/fold `ClassVars`-loss check runs once, at
        // the top of `generate_threaded_loop_body_inner`'s per-statement
        // loop — ahead of every dispatch branch, not just this function's
        // own fallback — since a nested `do:`/`collect:`/etc. statement is
        // classified `DispatchKind::ControlFlow` and is actually intercepted
        // by that loop's `control_flow_has_mutations` branch before ever
        // reaching here. See that check's own comment for why.
        match kind {
            BodyKind::FoldlDo => {
                if is_last {
                    // When preceding let-bindings exist (has_mutations/
                    // has_plain_lets), the last expression must also be bound with
                    // `let _ =` before `in StateAcc`. Without this,
                    // `let Y = ... in <expr> in StateAcc` is invalid Core Erlang
                    // (the `in StateAcc` has no corresponding `let`).
                    //
                    // close any class self-send open scope so
                    // the trailing `… in {vars}` / `… in StateAcc` does not
                    // dangle a second `in` — `bind_closed_expr_threading_class_vars`
                    // also threads a self-send's own `ClassVarsN` rebind
                    // forward past this `let _ = …` boundary when
                    // `plan.threads_class_vars` (see its own doc comment).
                    //
                    // This arm must take the same
                    // unconditional-threading path as `FoldlCollect`/
                    // `FoldlInject`/the predicate arms whenever
                    // `plan.threads_class_vars` — not just when
                    // `has_mutations || has_plain_lets`. A bare, last-statement
                    // self-send with no co-occurring local mutation (e.g.
                    // `aList do: [:x | self bump]`) previously fell to
                    // `closed_expression_doc`, which closes the self-send's own
                    // `ClassVarsN` rebind out of scope — but the generic
                    // `{ClassVars, tail}` wrap in `generate_threaded_loop_body_inner`
                    // (fired whenever `plan.threads_class_vars`, independent of
                    // `has_mutations`) then referenced that now-out-of-scope name,
                    // an `erlc` "unbound variable" regression confirmed empirically.
                    let threads_here = has_mutations || has_plain_lets || plan.threads_class_vars;
                    if threads_here {
                        let doc = self.bind_closed_expr_threading_class_vars(expr, "_", plan)?;
                        stmts.push(ThreadedStmt::Statement(doc, span));
                    } else {
                        let doc = self.expression_doc(expr)?;
                        stmts.push(ThreadedStmt::Statement(doc, span));
                    }
                    if threads_here {
                        if plan.use_tuple_acc {
                            // Repack threaded locals as tuple.
                            stmts.push(ThreadedStmt::Statement(
                                docvec!["{", plan.current_vars_doc(self), "}"],
                                span,
                            ));
                        } else {
                            let fs = if has_mutations {
                                self.current_state_var()
                            } else {
                                "StateAcc".to_string()
                            };
                            stmts.push(ThreadedStmt::Statement(leaf::var(fs), span));
                        }
                    }
                } else {
                    // ClassVars-visible discard for non-last statements
                    // (a class self-send leaves an open let-chain whose ClassVarsN
                    // must stay visible to following statements).
                    stmts.push(ThreadedStmt::Statement(
                        docvec!["let _ = ", self.expression_doc(expr)?, " in "],
                        span,
                    ));
                }
            }
            BodyKind::FoldlCollect => {
                if is_last {
                    let result_var = self.fresh_temp_var("CollectItem");
                    // threads a self-send's own `ClassVarsN` rebind
                    // forward past this `let` boundary when
                    // `plan.threads_class_vars` — see
                    // `bind_closed_expr_threading_class_vars`'s doc comment.
                    // pushed as its OWN `stmts` entry, separate from
                    // the tuple-construction push below — this function's
                    // own final `{ClassVars, tail}` wrap only pops the LAST
                    // `stmts` entry, so a self-send's `ClassVarsN` rebind
                    // inside `bind_doc` (an open, not-yet-closed chain) must
                    // stay a strictly EARLIER entry, not fused into the same
                    // one as the tuple it precedes — fusing them would place
                    // the wrap's `{ClassVars, …}` reference to `ClassVarsN`
                    // BEFORE the `let` that defines it (confirmed empirically
                    // — `erlc` "unbound variable", the same failure mode this
                    // whole helper exists to avoid).
                    let bind_doc =
                        self.bind_closed_expr_threading_class_vars(expr, &result_var, plan)?;
                    stmts.push(ThreadedStmt::Statement(bind_doc, span));
                    if plan.use_tuple_acc {
                        // Tuple mode — repack current vars.
                        let vars_doc = plan.current_vars_doc(self);
                        stmts.push(ThreadedStmt::Statement(
                            docvec!["{[", leaf::var(result_var), " | AccList], ", vars_doc, "}",],
                            span,
                        ));
                    } else {
                        let fs = if has_mutations {
                            self.current_state_var()
                        } else {
                            "StateAcc".to_string()
                        };
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "{[",
                                leaf::var(result_var),
                                " | AccList], ",
                                leaf::var(fs),
                                "}",
                            ],
                            span,
                        ));
                    }
                } else {
                    // a non-last statement may be a class self-send that
                    // emits an open let-chain; close it (keeping ClassVarsN visible)
                    // so the surrounding sequencing does not dangle a second `in`.
                    stmts.push(ThreadedStmt::Statement(
                        docvec!["let _ = ", self.expression_doc(expr)?, " in "],
                        span,
                    ));
                }
            }
            BodyKind::FoldlFilter { .. }
            | BodyKind::FoldlBoolPredicate { .. }
            | BodyKind::FoldlDetect { .. }
            | BodyKind::FoldlCount
            | BodyKind::FoldlTakeWhile { .. }
            | BodyKind::FoldlDropWhile { .. }
            | BodyKind::FoldlPartition { .. }
            | BodyKind::FoldlGroupBy { .. } => {
                if is_last {
                    if let Some(pv) = pred_var {
                        // threads a self-send's own `ClassVarsN`
                        // rebind forward past this `let` boundary when
                        // `plan.threads_class_vars` — see
                        // `bind_closed_expr_threading_class_vars`'s doc
                        // comment. This is the exact shape a `select:`
                        // predicate self-send needs.
                        let doc = self.bind_closed_expr_threading_class_vars(expr, pv, plan)?;
                        stmts.push(ThreadedStmt::Statement(doc, span));
                    }
                } else {
                    // see FoldlCollect — close a non-last open scope while
                    // keeping ClassVarsN visible for following statements.
                    stmts.push(ThreadedStmt::Statement(
                        docvec!["let _ = ", self.expression_doc(expr)?, " in "],
                        span,
                    ));
                }
            }
            BodyKind::FoldlInject => {
                if is_last {
                    let acc_var = self.fresh_temp_var("AccOut");
                    // pushed as its OWN `stmts` entry, separate from
                    // the tuple-construction push below — see the identical
                    // `FoldlCollect` comment above for why fusing them is
                    // wrong (confirmed empirically, `erlc` "unbound
                    // variable").
                    let bind_doc =
                        self.bind_closed_expr_threading_class_vars(expr, &acc_var, plan)?;
                    stmts.push(ThreadedStmt::Statement(bind_doc, span));
                    if plan.use_tuple_acc {
                        // Tuple mode — repack current vars.
                        let vars_doc = plan.current_vars_doc(self);
                        stmts.push(ThreadedStmt::Statement(
                            docvec!["{", leaf::var(acc_var), ", ", vars_doc, "}",],
                            span,
                        ));
                    } else {
                        let fs = if has_mutations {
                            self.current_state_var()
                        } else {
                            "StateAcc".to_string()
                        };
                        stmts.push(ThreadedStmt::Statement(
                            docvec!["{", leaf::var(acc_var), ", ", leaf::var(fs), "}",],
                            span,
                        ));
                    }
                } else {
                    // see FoldlCollect — close a non-last open scope while
                    // keeping ClassVarsN visible for following statements.
                    stmts.push(ThreadedStmt::Statement(
                        docvec!["let _ = ", self.expression_doc(expr)?, " in "],
                        span,
                    ));
                }
            }
        }
        self.finish_precompiled_scope(thread_scope)?;
        Ok(())
    }
}
