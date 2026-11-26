// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The conditional arm-statement walker and `render_branch_arm`
//! (split from `conditionals.rs`).

use super::super::gen_server::BodyExprKind;
use super::super::statement_kinds::{StmtCx, StmtTail};
use super::super::threaded_ir::{
    self, BindOp, FrameId, RebindFrame, RebindFrameKind, ThreadedStmt, ThreadingMode, ValueRef,
    VersionPrefix, VersionedVar,
};
use super::super::threaded_rebind::{RebindLayout, RebindNaming};
use super::super::{CodeGenError, CoreErlangGenerator, Result};
use super::LocalWrite;
use super::StateAccFallbackReason;
use super::conditionals::ArmFrame;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::form;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Block, Expression};
use beamtalk_core::source_analysis::Span;

impl CoreErlangGenerator {
    /// Generates an inline block body for a conditional branch with field mutation threading.
    ///
    /// **Precondition**: Caller must set `in_loop_body = true` and `state_version = 0`
    /// before calling, and restore them afterwards. The initial state inside the branch
    /// is named `StateAcc` (bound to the outer state by the caller via
    /// `let StateAcc = State{N} in ...`).
    ///
    /// Returns `(body_doc, final_state_version)`. The generated code ends with
    /// `{<result>, <final_state>}`.
    ///
    /// ADR 0111 Addendum 5: this arm's mutation sequence is built
    /// as real [`ThreadedStmt`]s (per the addendum's C1–C13 per-shape
    /// decomposition table), wrapped in one [`ThreadedStmt::Threaded`] node
    /// (`mode: StateAcc(None)`, this arm's own [`FrameId`] — Rule 3), then
    /// [`threaded_ir::verify`]d and [`threaded_ir::render`]ed — the
    /// `render`()ed `Document` IS this function's return value, byte-identical
    /// by construction (every shape below reuses the exact codegen calls and
    /// mint order the pre-migration hand-rolled version used). `StateAcc`
    /// mode's rendering is a transparent pass-through (`render_threaded`'s
    /// `ThreadingMode::StateAcc(_) => render(body, ctx)` arm), so wrapping
    /// changes zero bytes versus rendering `stmts` directly.
    pub(in crate::core_erlang) fn generate_conditional_branch_inline(
        &mut self,
        block: &Block,
    ) -> Result<(Document<'static>, usize)> {
        if self.is_repl_mode() {
            return self.generate_repl_arm_inline(block);
        }
        self.generate_plain_arm(block)
    }

    /// ADR 0133 Phase 3 (BT-3911): an arm of a construct that is not a
    /// local-threading producer (a conditional, a `detect:ifNone:` handler, a
    /// `match:` arm or a Tier 2 block inlined over the enclosing scratch map)
    /// is a `BranchArm` frame too, so the frame it runs in is always a
    /// registered one: the scratch map its statements write is the frame's,
    /// and a local the frame does not carry (one bound in the arm) is never
    /// written into it. It carries the locals the arm's block threads
    /// ([`Self::branch_threaded_locals`]: none for an arm that writes only
    /// locals of its own).
    ///
    /// BT-3949 / BT-4460: it registers its statements' producer sites by the
    /// rule a producer's arm uses (`with_body_producer_sites` with `arm`), so
    /// whatever is a producer at one of them is lowered into this frame
    /// through its `ConstructTuple`, family `Bind`s and `LocalRebind`s: a loop
    /// or fold (the statement itself, or one at a sequenced operand site of it
    /// — a map-literal value, an array element, a keyword argument), and a
    /// nested conditional, `ifNil:`, `and:`/`or:`, `match:` or
    /// `on:do:`/`ensure:` that writes a local of the arm. Without them a
    /// construct that threads a family (an actor fold whose block self-sends)
    /// at an operand site was compiled in place, and its `{Value, State}`
    /// carrier became the operand's value (`ConstructCarrierInPlace`); and a
    /// nested construct writing an arm-local was compiled in place and its
    /// locals reread out of its state carrier
    /// (`ConstructLocalsNotRebound`), which in an actor left the
    /// arm-local's scratch key in the actor's state and crashed a loop in a
    /// nested `on:do:`/`ensure:` body (`invalid argument`).
    ///
    /// Must be called inside [`Self::with_branch_context`].
    fn generate_plain_arm(&mut self, block: &Block) -> Result<(Document<'static>, usize)> {
        let arm = ArmFrame {
            threads: self.branch_threaded_locals(&[block]),
        };
        let rebind = RebindFrame {
            frame: self.current_branch_frame(),
            kind: RebindFrameKind::BranchArm,
            threads: arm.threads.clone(),
        };
        let body = super::super::util::collect_body_exprs(&block.body);
        #[cfg(any(test, feature = "test"))]
        let _plain = super::super::fault::PlainArmScope::enter(true);
        self.with_rebind_frame(rebind, |this| {
            // Test-only fault (`PlainArmOpaqueProducers`): the producer sites
            // BT-3949 registers, withheld.
            if fault!(PlainArmOpaqueProducers) {
                return this.generate_conditional_arm_statements(block, Some(&arm));
            }
            this.with_body_producer_sites(&body, false, true, |this| {
                this.generate_conditional_arm_statements(block, Some(&arm))
            })
        })
    }

    /// BT-3823: [`Self::generate_conditional_branch_inline`] for a *REPL arm*
    /// (a conditional arm, an `on:do:`/`ensure:` block or a `tryDo:` block,
    /// inlined over the bindings map): a producer at one of its statements'
    /// `repl_arm_producer_sites` adopts that map
    /// ([`Self::with_repl_arm_producer_sites`]). Outside the REPL it is
    /// [`Self::generate_conditional_branch_inline`].
    pub(in crate::core_erlang) fn generate_repl_arm_inline(
        &mut self,
        block: &Block,
    ) -> Result<(Document<'static>, usize)> {
        let body = super::super::util::collect_body_exprs(&block.body);
        self.with_repl_arm_rebind_frame(|this| {
            this.with_repl_arm_producer_sites(&body, |this| {
                this.generate_conditional_arm_statements(block, None)
            })
        })
    }

    /// ADR 0131 §2 (Phase 3): one arm of a conditional that is a
    /// local-threading producer in a class or value-type method, lowered as a
    /// `BranchArm` frame. Registers the arm's [`RebindFrame`] (its `threads`
    /// are the construct's seeded set), and the arm statements' producer
    /// sites (the rule a loop body uses, `with_body_producer_sites`), so a
    /// loop or fold in the arm is a producer whose `LocalRebind`s are lowered
    /// by this frame: a `Put` into the arm's seeded `StateAcc` for a local the
    /// construct threads, a plain `let` for any other.
    ///
    /// Must be called inside [`Self::with_branch_context`] (the frame is
    /// [`Self::current_branch_frame`]).
    pub(in crate::core_erlang) fn generate_conditional_arm(
        &mut self,
        block: &Block,
        arm: &ArmFrame,
    ) -> Result<(Document<'static>, usize)> {
        let frame = self.current_branch_frame();
        let rebind = RebindFrame {
            frame,
            kind: RebindFrameKind::BranchArm,
            threads: arm.threads.clone(),
        };
        let body = super::super::util::collect_body_exprs(&block.body);
        #[cfg(any(test, feature = "test"))]
        let _plain = super::super::fault::PlainArmScope::enter(false);
        self.with_rebind_frame(rebind, |this| {
            this.with_body_producer_sites(&body, false, true, |this| {
                this.generate_conditional_arm_statements(block, Some(arm))
            })
        })
    }

    /// The arm lowering behind [`Self::generate_conditional_branch_inline`]
    /// (`arm: None`, the legacy arm: its nested constructs unpack their own
    /// tuples) and [`Self::generate_conditional_arm`] (`arm: Some`, a
    /// `BranchArm` frame whose nested producers rebind by it, a plain arm's
    /// included: [`Self::generate_plain_arm`]).
    #[allow(clippy::too_many_lines)]
    fn generate_conditional_arm_statements(
        &mut self,
        block: &Block,
        arm: Option<&ArmFrame>,
    ) -> Result<(Document<'static>, usize)> {
        let frame = self.current_branch_frame();
        self.push_scope();

        let body = super::super::util::collect_body_exprs(&block.body);

        // Empty block returns nil with unchanged state (C13).
        if body.is_empty() {
            let final_version = self.state_version();
            self.pop_scope();
            return Ok(self.render_branch_arm(
                vec![ThreadedStmt::Return(
                    ValueRef::Literal("'nil'"),
                    VersionedVar::new(VersionPrefix::State, final_version, frame),
                    block.span,
                )],
                frame,
                final_version,
                block.span,
                arm,
            ));
        }

        let mut stmts: Vec<ThreadedStmt> = Vec::new();
        let mut last_result: Option<ValueRef> = None;
        // BT-3949: the producer sites of an arm frame's statements, read off the
        // AST just before the statement that holds them is lowered (a local
        // bound by an earlier statement is live), and checked against the
        // lowered arm once it is built: the rule a Tier 2 block body and the
        // loop and fold bodies apply to their own statements (BT-3939).
        let mut producer_sites: Vec<(Span, Vec<String>)> = Vec::new();
        // BT-4461: a REPL arm's producer sites that write a binding the arm
        // holds in a variable, checked the same way once the arm is built.
        let mut repl_sites: Vec<(Span, Vec<String>)> = Vec::new();

        for (i, expr) in body.iter().enumerate() {
            // BT-3838: a NON-last arm statement's value is discarded, so `(s)` is exactly `s`
            // (see `lower_body_exprs_with_reply_in_sites`); the last one is the arm's value.
            // BT-5031: same policy and same reason as the actor walker there, pinned by
            // `paren_statement_walkers_test.bt`.
            let expr = if i == body.len() - 1 {
                expr
            } else {
                &expr.unwrap_parens()
            };
            if arm.is_some() {
                producer_sites.extend(self.frame_producer_sites(expr, false, true));
            } else {
                repl_sites.extend(self.repl_frame_producer_sites(expr));
            }
            // Classified with the shared classifier just before the statement
            // is lowered, so a local bound earlier in this arm is live when a
            // catch boundary or loop in the statement is judged against the
            // scope (ADR 0131 Phase 4, BT-3840).
            let kind = self.classify_body_expr(expr);
            // BT-3823: a statement of a REPL arm that is itself a producer there
            // (`with_repl_arm_producer_sites`) is lowered by the producer
            // branch below, whatever the classifier says (the REPL has no
            // dispatch facts, so a loop is not `ControlFlowWithMutations`, and
            // an evaluated literal block is a `Tier2ValueCall`).
            let kind = if arm.is_none()
                && self.is_repl_mode()
                && self.local_producer_locals(expr).is_some()
            {
                BodyExprKind::ControlFlowWithMutations
            } else {
                kind
            };
            let is_last = i == body.len() - 1;
            let span = expr.span();

            let cx = StmtCx {
                expr,
                span,
                is_last,
                frame,
            };
            if let BodyExprKind::EarlyReturn = kind
                && let Expression::Return { value, .. } = expr
                && self.lower_arm_early_return_value(cx, value, &mut last_result, &mut stmts)?
            {
                continue;
            }

            // BT-4000: `self.f := <conditional whose receiver is sequenced>` is
            // lowered by C1, whose `lower_field_assignment_bind` threads the
            // value ahead, so the receiver's `Bind`s are this arm's own
            // statements rather than text inside C6's raw tuple. BT-3982: so is
            // `self.f := <evaluated block that is a producer>`.
            let kind = match kind {
                BodyExprKind::FieldAssignmentControlFlow
                    if matches!(expr, Expression::Assignment { value, .. }
                        if self.is_construct_with_sequenced_operand(value)
                            || self.evaluated_block_is_producer(value)) =>
                {
                    BodyExprKind::FieldAssignment
                }
                kind => kind,
            };
            match kind {
                // C1
                BodyExprKind::FieldAssignment => {
                    self.lower_stmt_field_assignment(
                        cx,
                        &mut StmtTail::Arm {
                            last_result: &mut last_result,
                        },
                        &mut stmts,
                    )?;
                }
                // C2/C3/C4
                BodyExprKind::LocalAssignPure
                | BodyExprKind::LocalAssignTier2
                | BodyExprKind::LocalAssignControlFlow
                | BodyExprKind::LocalAssignSelfSend => {
                    self.lower_arm_local_assign(cx, &mut last_result, &mut stmts)?;
                }
                // C5 — exempt from Bind modeling: no state version is
                // produced or consumed, every binding is a plain local.
                BodyExprKind::DestructureAssignment => {
                    self.lower_stmt_destructure_assignment(
                        cx,
                        &mut StmtTail::Arm {
                            last_result: &mut last_result,
                        },
                        &mut stmts,
                    )?;
                }
                // C6 — self.field := <control-flow-with-mutations>.
                // The sanctioned Gensym two-hop.
                //
                // BT-3913: a RHS that is a local-threading producer at a
                // producer site (`self.n := #(1, 2) do: [...]` in an arm) is
                // spliced as a prelude by `thread_ahead` and `expression_doc`
                // reads back its already-unwrapped value; compiling it in place
                // drops the write and trips `ProducerSiteNotThreaded`. That is
                // exactly the method-root field assignment, so it goes through
                // the one lowering (`lower_field_assignment_bind`, BT-4369):
                // thread ahead, `let Val = <value>`, `Put` the field.
                //
                // The unwrapped value covers loop and fold producers too, not
                // just `thread_ahead_unwraps` shapes (inline control flow,
                // exception and evaluated-block producers): `thread_ahead`
                // splices a loop's prelude and `expression_doc` answers
                // `element(1, <carrier>)`, so the tuple two-hop below would
                // re-unwrap the plain value and raise `badarg` at run time.
                // Like C3b's guard (`control_flow_has_mutations &&
                // !expr_has_local_producer && !thread_ahead_unwraps`), only a
                // RHS with mutations but no local producer still answers a
                // closed `{Value, State}` tuple here.
                BodyExprKind::FieldAssignmentControlFlow => {
                    self.lower_arm_field_assignment_control_flow(cx, &mut last_result, &mut stmts)?;
                }
                // C7 — self fieldAt: name put: value. Dynamic field
                // name ⇒ §Dynamic-field-puts option 1 (Direct, not Put).
                BodyExprKind::SelfFieldAtPut => {
                    self.lower_stmt_self_field_at_put(
                        cx,
                        &mut StmtTail::Arm {
                            last_result: &mut last_result,
                        },
                        &mut stmts,
                    )?;
                }
                // C7b — self clearField: name (ADR 0124 §1/B4), mirroring C7
                // immediately above: same `BindOp::Direct` shape with
                // `maps:remove` in place of `maps:put` and no value argument
                // to thread. Returns `Self` (`clearField: -> Self`,
                // `lower_stmt_self_clear_field`'s own convention) rather
                // than a written value.
                BodyExprKind::SelfClearField => {
                    self.lower_stmt_self_clear_field(
                        cx,
                        &mut StmtTail::Arm {
                            last_result: &mut last_result,
                        },
                        &mut stmts,
                    )?;
                }
                // C8 — self fieldAt: name put: <control-flow-with-mutations>.
                BodyExprKind::SelfFieldAtPutControlFlow => {
                    self.lower_arm_self_field_at_put_control_flow(
                        cx,
                        &mut last_result,
                        &mut stmts,
                    )?;
                }
                // BT-4000: `{a, b} := <conditional whose receiver is sequenced>`:
                // the value's prelude is this arm's own statements (C9's raw
                // tuple would hold the receiver's `Bind`s inside it).
                BodyExprKind::DestructureAssignmentControlFlow
                    if matches!(expr, Expression::DestructureAssignment { value, .. }
                        if self.is_construct_with_sequenced_operand(value)) =>
                {
                    self.lower_arm_destructure_assignment_producer(
                        cx,
                        &mut last_result,
                        &mut stmts,
                    )?;
                }
                // C9 — {a, b} := <control-flow-with-mutations>. No
                // Gensym hop — element(2, tuple) binds State(v+1) directly.
                BodyExprKind::DestructureAssignmentControlFlow => {
                    self.lower_arm_destructure_assignment_control_flow(
                        cx,
                        &mut last_result,
                        &mut stmts,
                    )?;
                }
                // ADR 0131 Phase 3: a loop or fold that is itself the statement of
                // a `BranchArm` frame is a local-threading producer there. Its
                // prelude (the construct tuple and one `LocalRebind` per local,
                // lowered by this arm) is spliced, so the arm's `StateAcc` keeps
                // every other key it carries.
                // BT-3823: so is one that is a statement of a REPL arm
                // (`generate_repl_arm_inline`): its prelude adopts the bindings
                // map the arm's `StateAcc` is.
                // BT-4000: and so is a conditional whose receiver advances
                // `State`, in every arm, as at the method root and in a loop
                // body: through `threaded_expression` its receiver's prelude
                // (`conditional_receiver_operand`) and its `State` extraction are
                // this arm's own statements. Compiled as the raw tuple of C10
                // below, the receiver's `Bind`s were rendered inside the tuple
                // document, and the arm's extraction read a version no `Bind` of
                // the arm produced (`UnboundVersion`).
                BodyExprKind::ControlFlowWithMutations
                    if self.arm_statement_is_producer(expr, arm.is_some()) =>
                {
                    self.lower_arm_control_flow_producer(cx, &mut last_result, &mut stmts)?;
                }
                // C10
                BodyExprKind::ControlFlowWithMutations => {
                    self.lower_arm_control_flow_with_mutations(cx, &mut last_result, &mut stmts)?;
                }
                // C11 — a Tier 2 `value(:...)` call on a stored
                // block. Non-last carries the newline quirk
                // (`")\n in let "`) confirmed against real compiled output.
                BodyExprKind::Tier2ValueCall => {
                    self.lower_arm_tier2_value_call(cx, &mut last_result, &mut stmts)?;
                }
                // C12b — a same-class self-send dispatched through
                // `safe_dispatch`/a sealed call, which itself may mutate
                // `self`'s state — returns `{Result, NewState}`. Unlike the
                // C12 catch-all below (whose `expression_doc` render+`let`
                // discards everything but the raw tuple value), this must
                // Bind the returned `NewState` as this branch's own next
                // real `State` version, mirroring C11
                // (`ControlFlowWithMutations`)'s tuple-unpack `Bind` above —
                // otherwise a mutation performed via the self-send (as
                // opposed to a direct `self.field := value`) is silently
                // dropped once the branch closes.
                BodyExprKind::DispatchingSelfSend => {
                    self.lower_stmt_dispatching_self_send(
                        cx,
                        &mut StmtTail::Arm {
                            last_result: &mut last_result,
                        },
                        &mut stmts,
                    )?;
                }
                // C12 — catch-all pure statements (EarlyReturn, SuperSend,
                // ErrorSend, Pure).
                //
                // ADR 0118 phase 2a/2b: before compiling,
                // thread ahead (as real `Bind`s pushed into `stmts`) every
                // state-effecting sub-expression nested anywhere inside
                // this statement, via `thread_ahead` — a producer or a
                // covered send gets the sequencing rule ("decide once,
                // sequence all or none", now made universal);
                // every parent kind the rule covers (message sends, cast
                // sends, literal
                // containers, string interpolation, `^`/`:=`, a self-
                // cascade) is threaded in full — there is no longer an
                // order-unsafe subset that only gets a warning (the
                // gap the deleted planner used to leave). When nothing
                // needed threading (the overwhelmingly common case), this
                // is a no-op and `expression_doc` below behaves exactly as
                // before.
                _ => {
                    self.lower_arm_catch_all(cx, &mut last_result, &mut stmts)?;
                }
            }
        }

        let errors = threaded_ir::verify_producer_sites_threaded(&stmts, &producer_sites);
        self.report_threaded_ir_verify_errors(
            &errors,
            "a conditional, match: or handler arm's producer must be lowered as a producer",
            block.span,
        );
        self.check_repl_frame_producer_sites(&stmts, &repl_sites, block.span);

        // C13 — the arm closer: {<result>, <final_state>}.
        let final_version = self.state_version();
        let result_value = last_result.unwrap_or(ValueRef::Literal("'nil'"));
        let closing_span = body.last().map_or(block.span, |e| e.span());
        stmts.push(ThreadedStmt::Return(
            result_value,
            VersionedVar::new(VersionPrefix::State, final_version, frame),
            closing_span,
        ));

        self.pop_scope();
        Ok(self.render_branch_arm(stmts, frame, final_version, block.span, arm))
    }

    // C0b — `^self otherMethod` (early return whose value is
    // a dispatching actor self-send, e.g. `ifTrue: [^self
    // configureVictim]`). `classify_body_expr` always classifies the
    // outer `Return` as `EarlyReturn` (never `DispatchingSelfSend` —
    // that classification only applies to a bare, un-returned
    // self-send), so without this check the statement below falls to
    // the C12 catch-all, which renders the whole `Return` as one
    // opaque `expression_doc` blob via the generic AST-directed
    // `Expression::Return` handler. That handler (pre-ADR-0118-phase-5b)
    // reached `generate_self_dispatch`'s *closed* form for a plain
    // self-send value — which computed the call's `Result` but
    // dropped its `NewState`, so the NLR throw's 4-tuple carried
    // this branch's
    // pre-call `StateAcc` instead of the mutation the self-send just
    // made. The call still runs; its effects vanish the instant this
    // `^` unwinds. Confirmed by `ThreadedIr::verify()` itself: the
    // opaque blob's raw text and this loop's own version bookkeeping
    // silently diverge (`report_threaded_ir_verify_errors` panics
    // debug-fatal on this exact shape).
    //
    // Mirrors C12b's `DispatchingSelfSend` Bind pattern immediately
    // below: dispatch via `generate_self_dispatch_call_doc_for`, Bind the
    // arm's next real `State` version from the call's `NewState`
    // element, then throw the NLR tuple against *that* version
    // instead of the branch's stale entry state — the same
    // `generate_self_dispatch_parts`-vs-closed distinction
    // `lower_body_exprs_with_reply`'s own `DispatchingSelfSend` arm
    // already applies for the top-level (unnested) `^self foo` shape
    // — just expressed as this loop's own `Bind`+`Statement`
    // pair rather than that flat body lowering's open let-chain, since
    // this arm's `ThreadedIr` wrapper needs the version bump to be a
    // real, verifiable production, not text buried in an opaque `Doc`.
    // excludes class methods (`in_class_method()`) for the
    // same reason `mod.rs`'s `Expression::Return` handler does —
    // `generate_self_dispatch_call_doc_for` unconditionally threads
    // `current_state_var()` (Actor instance state), which a class
    // method does not have — so a class-method self-send here falls
    // to the C12 catch-all below instead, unchanged from before this
    // fix.
    fn lower_arm_early_return_value(
        &mut self,
        cx: StmtCx<'_>,
        value: &Expression,
        last_result: &mut Option<ValueRef>,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<bool> {
        let StmtCx {
            span,
            is_last,
            frame,
            ..
        } = cx;
        if !self.threads_no_actor_state()
            && matches!(
                self.classify_body_expr(value),
                BodyExprKind::DispatchingSelfSend
            )
        {
            // ADR 0118 phase 2a: `threaded_expression`'s
            // producer path (`is_prelude_producer`, matching this
            // arm's own `is_dispatching_actor_self_send`
            // classification exactly, per C12b's identical swap
            // above) replaces `dispatch_self_send_as_bind` —
            // same `Statement`+`Bind` pair, plus (a genuine fix)
            // `value`'s own arguments are now sequenced too
            // (`^self log: (self nextId)`), which the old direct
            // call never threaded.
            let tv = self.threaded_expression(value, frame)?;
            stmts.extend(tv.prelude);
            let dispatch_value_doc = self.threaded_value_doc(&tv.value);
            let nlr_token = self.current_nlr_token().cloned().ok_or_else(|| {
                CodeGenError::Internal(
                    "BT-3374: EarlyReturn classification implies an active NLR \
                         context, but none is set"
                        .to_string(),
                )
            })?;
            let new_state = self.current_state_var();
            // BT-4535: the node stores `new_state` to the cell
            // before the throw where the cell is live.
            let (throw, throw_var) = self.nlr_throw_stmt(
                nlr_token,
                ValueRef::Doc(dispatch_value_doc),
                new_state,
                span,
            );
            stmts.push(throw);
            if is_last {
                *last_result = Some(ValueRef::Var(throw_var));
            }
            return Ok(true);
        }

        // BT-3495: `^ (self.field := ...)` — the RHS is itself
        // a field write, at any parenthesization depth (the
        // same "value-carrying parent" shape BT-3493 fixed for
        // `r := (self.x := ...)`, but here the wrapper is
        // `Return` instead of a local assignment). Lower it
        // through the SAME real-`Bind` producer C1
        // (`lower_field_assignment_bind`) uses for a bare
        // `self.field := ...` statement, then throw the NLR
        // tuple carrying the assigned value — mirroring C0b's
        // `DispatchingSelfSend` throw immediately above, just
        // with the field write's own value in place of the
        // self-send's result.
        if let Some(field_write) = self.local_assign_field_write(value) {
            let field_val_var =
                self.lower_field_assignment_bind(field_write, frame, span, stmts)?;
            let nlr_token = self.current_nlr_token().cloned().ok_or_else(|| {
                CodeGenError::Internal(
                    "BT-3495: EarlyReturn classification implies an active NLR \
                         context, but none is set"
                        .to_string(),
                )
            })?;
            let new_state = self.current_state_var();
            // BT-4535: as above.
            let (throw, throw_var) =
                self.nlr_throw_stmt(nlr_token, ValueRef::Var(field_val_var), new_state, span);
            stmts.push(throw);
            if is_last {
                *last_result = Some(ValueRef::Var(throw_var));
            }
            return Ok(true);
        }
        Ok(false)
    }

    fn lower_arm_catch_all(
        &mut self,
        cx: StmtCx<'_>,
        last_result: &mut Option<ValueRef>,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<()> {
        let StmtCx {
            expr,
            span,
            is_last,
            frame,
            ..
        } = cx;
        let thread_scope = self.thread_ahead(expr, stmts, frame)?;
        if is_last {
            let result_var = self.fresh_temp_var("BranchResult");
            let expr_doc = self.expression_doc(expr)?;
            self.finish_precompiled_scope(thread_scope)?;
            stmts.push(ThreadedStmt::Statement(
                docvec![
                    "let ",
                    leaf::var(result_var.clone()),
                    " = ",
                    expr_doc,
                    " in "
                ],
                span,
            ));
            *last_result = Some(ValueRef::Var(result_var));
        } else {
            let seq_var = self.fresh_temp_var("seq");
            let expr_doc = self.expression_doc(expr)?;
            self.finish_precompiled_scope(thread_scope)?;
            stmts.push(ThreadedStmt::Statement(
                docvec!["let ", leaf::var(seq_var), " = ", expr_doc, " in "],
                span,
            ));
        }
        Ok(())
    }

    // Not shared with the method walker (`lower_reply_tier2_value_call`); see `lower_arm_local_assign`.
    fn lower_arm_tier2_value_call(
        &mut self,
        cx: StmtCx<'_>,
        last_result: &mut Option<ValueRef>,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<()> {
        let StmtCx {
            expr,
            span,
            is_last,
            frame,
            ..
        } = cx;
        if is_last {
            let tuple_var = self.fresh_temp_var("T2Tuple");
            let result_var = self.fresh_temp_var("BranchResult");
            let expr_doc = self.generate_tier2_value_call_doc(expr)?;
            let source_version = self.state_version();
            stmts.push(ThreadedStmt::Statement(
                docvec![
                    "let ",
                    leaf::var(tuple_var.clone()),
                    " = ",
                    expr_doc,
                    " in let ",
                    leaf::var(result_var.clone()),
                    " = ",
                    form::element(1, leaf::var(tuple_var.clone())),
                    " in ",
                ],
                span,
            ));
            self.bind_tuple_state(stmts, frame, source_version, &tuple_var, span);
            *last_result = Some(ValueRef::Var(result_var));
        } else {
            let tuple_var = self.fresh_temp_var("T2Tuple");
            let discard_var = self.fresh_temp_var("T2Discard");
            let expr_doc = self.generate_tier2_value_call_doc(expr)?;
            let source_version = self.state_version();
            stmts.push(ThreadedStmt::Statement(
                docvec![
                    "let ",
                    leaf::var(tuple_var.clone()),
                    " = ",
                    expr_doc,
                    " in let ",
                    leaf::var(discard_var),
                    " = ",
                    form::element(1, leaf::var(tuple_var.clone())),
                    "\n in ",
                ],
                span,
            ));
            self.bind_tuple_state(stmts, frame, source_version, &tuple_var, span);
            // rebind captured local-var mutations from
            // NewState so a later read in this branch sees the
            // mutated value.
            if let Some(mutations) = self.get_inline_block_captured_mutations(expr) {
                let new_state_name = self.current_state_var();
                let re_reads: Vec<Document<'static>> = self.rebind_threaded_locals(
                    &mutations,
                    CoreErlangGenerator::local_scratch_key,
                    &new_state_name,
                    RebindNaming::ReuseBound,
                    RebindLayout::Inline,
                );
                if !re_reads.is_empty() {
                    stmts.push(ThreadedStmt::Statement(Document::Vec(re_reads), span));
                }
            }
        }
        Ok(())
    }

    // Not shared with the method walker (`lower_reply_control_flow_with_mutations`); see `lower_arm_local_assign`.
    fn lower_arm_control_flow_with_mutations(
        &mut self,
        cx: StmtCx<'_>,
        last_result: &mut Option<ValueRef>,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<()> {
        let StmtCx {
            expr,
            span,
            is_last,
            frame,
            ..
        } = cx;
        if is_last {
            let tuple_var = self.fresh_temp_var("Tuple");
            let result_var = self.fresh_temp_var("BranchResult");
            let expr_doc = self.expression_doc(expr)?;
            let source_version = self.state_version();
            stmts.push(ThreadedStmt::Statement(
                docvec![
                    "let ",
                    leaf::var(tuple_var.clone()),
                    " = ",
                    expr_doc,
                    " in let ",
                    leaf::var(result_var.clone()),
                    " = ",
                    form::element(1, leaf::var(tuple_var.clone())),
                    " in ",
                ],
                span,
            ));
            self.bind_tuple_state(stmts, frame, source_version, &tuple_var, span);
            *last_result = Some(ValueRef::Var(result_var));
        } else {
            let tuple_var = self.fresh_temp_var("Tuple");
            let expr_doc = self.expression_doc(expr)?;
            let source_version = self.state_version();
            stmts.push(ThreadedStmt::Statement(
                docvec![
                    "let ",
                    leaf::var(tuple_var.clone()),
                    " = ",
                    expr_doc,
                    " in "
                ],
                span,
            ));
            self.bind_tuple_state(stmts, frame, source_version, &tuple_var, span);
            // BT-4460 / BT-4462: a construct reaching here threads
            // no local (every arm registers a nested construct
            // writing an arm-local as a producer, lowered above);
            // one that does is `ConstructLocalsNotRebound`.
            stmts.extend(self.construct_locals_in_place(expr, span));
        }
        Ok(())
    }

    // Not shared with the method walker (`lower_reply_control_flow_with_mutations`); see `lower_arm_local_assign`.
    fn lower_arm_control_flow_producer(
        &mut self,
        cx: StmtCx<'_>,
        last_result: &mut Option<ValueRef>,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<()> {
        let StmtCx {
            expr,
            span,
            is_last,
            frame,
            ..
        } = cx;
        let tv = self.threaded_expression(expr, frame)?;
        stmts.extend(tv.prelude);
        let value_doc = self.threaded_value_doc(&tv.value);
        let var = self.fresh_temp_var(if is_last { "BranchResult" } else { "seq" });
        stmts.push(ThreadedStmt::Statement(
            docvec!["let ", leaf::var(var.clone()), " = ", value_doc, " in "],
            span,
        ));
        if is_last {
            *last_result = Some(ValueRef::Var(var));
        }
        Ok(())
    }

    // Not shared with the method walker (`lower_reply_destructure_assignment_control_flow`); see `lower_arm_local_assign`.
    fn lower_arm_destructure_assignment_control_flow(
        &mut self,
        cx: StmtCx<'_>,
        last_result: &mut Option<ValueRef>,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<()> {
        let StmtCx {
            expr,
            span,
            is_last,
            frame,
            ..
        } = cx;
        if let Expression::DestructureAssignment { pattern, value, .. } = expr {
            let tuple_var = self.fresh_temp_var("CfTuple");
            let actual_val = self.fresh_temp_var("CfVal");
            let value_str = self.expression_doc(value)?;
            let source_version = self.state_version();
            stmts.push(ThreadedStmt::Statement(
                docvec![
                    "let ",
                    leaf::var(tuple_var.clone()),
                    " = ",
                    value_str,
                    " in let ",
                    leaf::var(actual_val.clone()),
                    " = ",
                    form::element(1, leaf::var(tuple_var.clone())),
                    " in ",
                ],
                span,
            ));
            self.bind_tuple_state(stmts, frame, source_version, &tuple_var, span);
            stmts.extend(self.construct_locals_in_place(value, span));
            // BT-4439: each name is written to the arm's slot.
            let _ = self.lower_destructure_bind_from_var(
                pattern,
                &actual_val,
                frame,
                span,
                stmts,
                &|_| LocalWrite::Slot,
            )?;
            if is_last {
                *last_result = Some(ValueRef::Var(actual_val));
            }
        }
        Ok(())
    }

    // Not shared with the method walker (`lower_reply_destructure_assignment_control_flow`); see `lower_arm_local_assign`.
    fn lower_arm_destructure_assignment_producer(
        &mut self,
        cx: StmtCx<'_>,
        last_result: &mut Option<ValueRef>,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<()> {
        let StmtCx {
            expr,
            span,
            is_last,
            frame,
            ..
        } = cx;
        if let Expression::DestructureAssignment { pattern, value, .. } = expr {
            let tv = self.threaded_expression(value, frame)?;
            stmts.extend(tv.prelude);
            let actual_val = self.fresh_temp_var("CfVal");
            let value_doc = self.threaded_value_doc(&tv.value);
            stmts.push(ThreadedStmt::Statement(
                docvec![
                    "let ",
                    leaf::var(actual_val.clone()),
                    " = ",
                    value_doc,
                    " in "
                ],
                span,
            ));
            // BT-4439: each name is written to the arm's slot.
            let _ = self.lower_destructure_bind_from_var(
                pattern,
                &actual_val,
                frame,
                span,
                stmts,
                &|_| LocalWrite::Slot,
            )?;
            if is_last {
                *last_result = Some(ValueRef::Var(actual_val));
            }
        }
        Ok(())
    }

    // Not shared with the method walker (`lower_reply_self_field_at_put_control_flow`); see `lower_arm_local_assign`.
    fn lower_arm_self_field_at_put_control_flow(
        &mut self,
        cx: StmtCx<'_>,
        last_result: &mut Option<ValueRef>,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<()> {
        let StmtCx {
            expr,
            span,
            is_last,
            frame,
            ..
        } = cx;
        if let Expression::MessageSend { arguments, .. } = expr {
            // BT-4369: a dynamic-key field write records itself for the loop checkout.
            self.note_field_write("<dynamic>", span);
            let name_var = self.fresh_temp_var("Name");
            let name_code = self.expression_doc(&arguments[0])?;
            let tuple_var = self.fresh_temp_var("CfTuple");
            let val_var = self.fresh_temp_var("CfVal");
            let val_code = self.expression_doc(&arguments[1])?;
            let rhs_state = self.fresh_temp_var("CfState");
            let source_version = self.state_version();
            stmts.push(ThreadedStmt::Statement(
                docvec![
                    "let ",
                    leaf::var(name_var.clone()),
                    " = ",
                    name_code,
                    " in let ",
                    leaf::var(tuple_var.clone()),
                    " = ",
                    val_code,
                    " in let ",
                    leaf::var(val_var.clone()),
                    " = ",
                    form::element(1, leaf::var(tuple_var.clone())),
                    " in ",
                ],
                span,
            ));
            let gensym_state =
                VersionedVar::new(VersionPrefix::Gensym(rhs_state.clone()), 1, frame);
            stmts.push(ThreadedStmt::Bind {
                target: gensym_state.clone(),
                source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                op: BindOp::Direct(ValueRef::Doc(docvec![form::element(
                    2,
                    leaf::var(tuple_var)
                ),])),
                span,
            });
            self.push_state_bind(
                stmts,
                frame,
                gensym_state,
                BindOp::Direct(ValueRef::Doc(docvec![
                    "call 'maps':'put'(",
                    leaf::var(name_var),
                    ", ",
                    leaf::var(val_var.clone()),
                    ", ",
                    leaf::var(rhs_state),
                    ")",
                ])),
                span,
            );
            stmts.extend(self.construct_locals_in_place(&arguments[1], span));
            if is_last {
                *last_result = Some(ValueRef::Var(val_var));
            }
        }
        Ok(())
    }

    // Not shared with the method walker (`lower_reply_field_assignment_control_flow`); see `lower_arm_local_assign`.
    fn lower_arm_field_assignment_control_flow(
        &mut self,
        cx: StmtCx<'_>,
        last_result: &mut Option<ValueRef>,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<()> {
        let StmtCx {
            expr,
            span,
            is_last,
            frame,
            ..
        } = cx;
        if let Expression::Assignment { target, value, .. } = expr
            && let Expression::FieldAccess { field, .. } = target.as_ref()
        {
            if self.thread_ahead_unwraps(value) || self.expr_has_local_producer(value) {
                let val_var = self.lower_field_assignment_bind(expr, frame, span, stmts)?;
                if is_last {
                    *last_result = Some(ValueRef::Var(val_var));
                }
                return Ok(());
            }
            let thread_scope = self.thread_ahead(value, stmts, frame)?;
            let rhs_doc = self.expression_doc(value)?;
            self.finish_precompiled_scope(thread_scope)?;
            // BT-4369: a field write in an arm of a loop body.
            self.note_field_write(&field.name, span);
            let tuple_var = self.fresh_temp_var("CfTuple");
            let val_var = self.fresh_temp_var("CfVal");
            let rhs_state = self.fresh_temp_var("CfState");
            let source_version = self.state_version();
            stmts.push(ThreadedStmt::Statement(
                docvec![
                    "let ",
                    leaf::var(tuple_var.clone()),
                    " = ",
                    rhs_doc,
                    " in let ",
                    leaf::var(val_var.clone()),
                    " = ",
                    form::element(1, leaf::var(tuple_var.clone())),
                    " in ",
                ],
                span,
            ));
            let gensym_state = VersionedVar::new(VersionPrefix::Gensym(rhs_state), 1, frame);
            stmts.push(ThreadedStmt::Bind {
                target: gensym_state.clone(),
                source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                op: BindOp::Direct(ValueRef::Doc(docvec![form::element(
                    2,
                    leaf::var(tuple_var)
                ),])),
                span,
            });
            self.push_state_bind(
                stmts,
                frame,
                gensym_state,
                BindOp::Put {
                    field: field.name.to_string(),
                    value: ValueRef::Var(val_var.clone()),
                },
                span,
            );
            stmts.extend(self.construct_locals_in_place(value, span));
            if is_last {
                *last_result = Some(ValueRef::Var(val_var));
            }
        }
        Ok(())
    }

    // Not shared with the actor-method walker (`lower_reply_local_assign_*`):
    // an arm writes the local to its frame's scratch slot (`push_local_slot_put`)
    // so `StateAcc` carries it out of the conditional, and for a Tier 2 or
    // control-flow right-hand side threads the call's `NewState` through a
    // Gensym state. A method body binds the local's own Core var and advances
    // the `State` version directly. See `lower_reply_local_assign_pure`.
    fn lower_arm_local_assign(
        &mut self,
        cx: StmtCx<'_>,
        last_result: &mut Option<ValueRef>,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<()> {
        let StmtCx {
            expr,
            span,
            is_last,
            frame,
            ..
        } = cx;
        let val_var = self.lower_local_var_assignment_bind(expr, frame, span, stmts)?;
        if is_last {
            *last_result = Some(ValueRef::Var(val_var));
        }
        Ok(())
    }

    /// ADR 0111 Addendum 5, Rule 3: wraps a branch arm's real `Bind`/`Statement`
    /// sequence in one node, `verify()`s it and `render()`s it. With an
    /// [`ArmFrame`] the arm is a [`ThreadedStmt::BranchArm`] node recording the
    /// construct's seeded `threads` (ADR 0131 §2), so the verifier reads the
    /// frame's rebind shapes off the node (a conditional arm, and since BT-3939
    /// a Tier 2 block body, `generate_block_stateful`); without one it is the
    /// legacy `Threaded` wrapper (`StateAcc` mode, a transparent pass-through
    /// whose only job is giving `verify()` the arm's non-ROOT frame), which
    /// threads nothing of its own.
    pub(in crate::core_erlang) fn render_branch_arm(
        &mut self,
        stmts: Vec<ThreadedStmt>,
        frame: FrameId,
        final_version: usize,
        span: Span,
        arm: Option<&ArmFrame>,
    ) -> (Document<'static>, usize) {
        if let Some(arm) = arm {
            let wrapper = vec![threaded_ir::build_branch_arm(
                frame,
                arm.threads.clone(),
                stmts,
            )];
            let errors = self.verify_threaded_ir(&wrapper);
            self.report_threaded_ir_verify_errors(
                &errors,
                "conditional branch arm ThreadedIr must be well-formed",
                span,
            );
            let mut ctx = threaded_ir::RenderCtx::new(self);
            let doc = threaded_ir::render(&wrapper, &mut ctx);
            return (doc, final_version);
        }
        let produces = if final_version > 0 {
            vec![VersionedVar::new(
                VersionPrefix::State,
                final_version,
                frame,
            )]
        } else {
            Vec::new()
        };
        let wrapper = vec![ThreadedStmt::Threaded {
            mode: ThreadingMode::StateAcc(StateAccFallbackReason::None),
            frame,
            threads: self.registered_state_acc_threads(frame),
            body: stmts,
            produces,
            span,
        }];
        let errors = self.verify_threaded_ir(&wrapper);
        self.report_threaded_ir_verify_errors(
            &errors,
            "conditional branch arm ThreadedIr must be well-formed",
            span,
        );
        let mut ctx = threaded_ir::RenderCtx::new(self);
        let doc = threaded_ir::render(&wrapper, &mut ctx);
        (doc, final_version)
    }
}
