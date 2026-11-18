// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `on:do:` tier-1 try, the structural fallbacks and the threaded exception
//! body lowering (split from `exception_handling.rs`).

use super::super::actor_state_access::{actor_state_call, actor_state_helper};
use super::super::ensure_nlr_relay::EnsureCatchTriple;
use super::super::intrinsics::STATEFUL_BLOCK_DISPATCH_HINT;
use super::super::threaded_expr::ProducerTail;
use super::super::threaded_ir::{
    EnsureCleanup, FrameId, RebindFrame, RebindFrameKind, ThreadedStmt, TryRegion,
};
use super::super::{CoreErlangGenerator, Result};
use super::LocalWrite;
use super::analysis::ThreadedFamilies;
use super::conditionals::ArmFrame;
use super::exception_handling::{ArmEntry, ExceptionArm};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::form::{self, Layout};
use beamtalk_cerl_doc::{join, leaf};
use beamtalk_core::ast::{Block, Expression};

impl CoreErlangGenerator {
    /// Builds the Tier 1 (pure protected block) `try`/`catch` body for
    /// `generate_on_do_structural_fallback` — factored out to keep that
    /// function under clippy's line-count limit. Reuses
    /// `on_do_catch_boundary`'s NLR-passthrough + `matches_class` structure;
    /// only the handler's tier (arity 0 = pure 0-arg, arity 1 = pure 1-arg,
    /// anything else = stateful) is discriminated dynamically here, since it
    /// isn't known statically the way `generate_on_do` knows it from the
    /// literal block AST.
    fn generate_on_do_tier1_try(
        &mut self,
        self_var: &'static str,
        ex_class_param: String,
        handler_param: String,
        class_name: &str,
    ) -> Document<'static> {
        let result_var = self.fresh_temp_var("Result");
        let catch_vars = self.fresh_on_do_catch_vars(ex_class_param);
        let ex_obj_var = catch_vars.ex_obj_var.clone();

        // arity 1 is ambiguous between a pure 1-arg handler and a stateful
        // 0-arg handler — same documented ambiguity as Block's blockValue*
        // family (deferred disambiguation); anything else is Tier 2.
        let handler_stateful_error = self.generate_stateful_block_dispatch_error(
            "on:do:",
            class_name,
            STATEFUL_BLOCK_DISPATCH_HINT,
        );
        let handler_dispatch = arity_dispatch_doc(
            &leaf::var(handler_param.clone()),
            // BT-4366: each apply is nested in the arity `case`.
            self.annotate_current_send_call(docvec![
                "apply ",
                leaf::var(handler_param.clone()),
                " ()"
            ]),
            self.annotate_current_send_call(docvec![
                "apply ",
                leaf::var(handler_param),
                " (",
                leaf::var(ex_obj_var.clone()),
                ")"
            ]),
            handler_stateful_error,
        );

        // No source span: this is the generically-dispatched `onDo` body.
        self.on_do_catch_boundary(
            catch_vars,
            "State",
            TryRegion {
                body: docvec![
                    self.annotate_current_send_call(docvec![
                        "apply ",
                        Document::Str(self_var),
                        " ()"
                    ]),
                    " "
                ],
                result_var,
            },
            beamtalk_core::source_analysis::Span::default(),
            handler_dispatch,
        )
    }

    /// Generates the fallback method body for `onDo` — Block's
    /// `on:do:`. Reached only via generic dispatch bypassing the call-site
    /// interception `generate_on_do` normally provides (e.g. `perform:`). See
    /// `generate_block_value_structural_fallback` for the general
    /// Tier 1/Tier 2 discrimination rationale.
    ///
    /// Reuses `on_do_catch_boundary`'s NLR-passthrough + `matches_class`
    /// structure so the Tier 1 (pure) case stays behaviourally identical to
    /// the AST-driven `generate_on_do` — only the receiver/handler *tier*
    /// discrimination differs, since a generically dispatched handler's
    /// declared arity (0 or 1) isn't known statically the way it is when
    /// `generate_on_do` reads it straight off the literal block AST.
    /// `current_method_params` are `[ExClass, Handler]` (the `on:`/`do:`
    /// keyword arguments); `Self` is the protected block.
    pub(in crate::core_erlang) fn generate_on_do_structural_fallback(
        &mut self,
        class_name: &str,
    ) -> Document<'static> {
        let self_var = if self.in_class_method() {
            "ClassSelf"
        } else {
            "Self"
        };
        let ex_class_param = self
            .current_method_params
            .first()
            .cloned()
            .unwrap_or_else(|| "ExClass".to_string());
        let handler_param = self
            .current_method_params
            .get(1)
            .cloned()
            .unwrap_or_else(|| "Handler".to_string());

        let runtime_module =
            super::super::primitive_bindings::PrimitiveBindingTable::runtime_module_for_class(
                class_name,
            );
        let params_doc = join(
            [
                leaf::var(ex_class_param.clone()),
                leaf::var(handler_param.clone()),
            ],
            &Document::Str(", "),
        );
        let placeholder_branch = docvec![
            "call ",
            leaf::atom(runtime_module),
            ":'dispatch'('onDo', [",
            params_doc,
            "], ",
            Document::Str(self_var),
            ")",
        ];

        let tier1_try =
            self.generate_on_do_tier1_try(self_var, ex_class_param, handler_param, class_name);

        let self_stateful_error = self.generate_stateful_block_dispatch_error(
            "on:do:",
            class_name,
            STATEFUL_BLOCK_DISPATCH_HINT,
        );
        arity_dispatch_doc(
            &Document::Str(self_var),
            tier1_try,
            self_stateful_error,
            placeholder_branch,
        )
    }

    /// The catch of `Block>>ensure:`'s structural fallback, an
    /// [`ThreadedStmt::EnsureCatch`](threaded_ir::ThreadedStmt::EnsureCatch)
    /// with no relay: the cleanup, then the re-raise (BT-4536).
    fn ensure_structural_fallback_catch(&mut self, cleanup_param: &str) -> Document<'static> {
        let type_var = self.fresh_temp_var("Type");
        let error_var = self.fresh_temp_var("Error");
        let stack_var = self.fresh_temp_var("Stack");
        // Every `^` that crosses this catch is foreign (this is the Block
        // method's frame). The cleanup is a closure, which writes only the
        // actor-state cell, in place, and the method a `^` returns from
        // commits the cell (ADR 0146): the throw is re-raised as it came
        // (`ensure_nlr_relay`). `note_unwound/1` tells the test-build commit
        // check that the cell may be ahead of it now; for any other
        // exception it does nothing.
        let cleanup_apply = || docvec!["apply ", leaf::var(cleanup_param.to_string()), " ()"];
        let other = form::seq(
            self.annotate_current_send_call(cleanup_apply()),
            form::seq(
                actor_state_call(
                    actor_state_helper::NOTE_UNWOUND,
                    [leaf::var(error_var.clone())],
                ),
                Self::emit_raw_raise(type_var.clone(), error_var.clone(), stack_var.clone()),
                Layout::Flat,
            ),
            Layout::Flat,
        );
        self.ensure_catch(
            EnsureCatchTriple {
                type_var,
                error_var,
                stack_var,
                carried_var: None,
            },
            (None, false, false),
            EnsureCleanup::Closure { writes_cell: true },
            (None, None),
            other,
            beamtalk_core::source_analysis::Span::default(),
        )
    }

    /// Generates the fallback method body for `ensure` — Block's
    /// `ensure:`. Reached only via generic dispatch bypassing the call-site
    /// interception `generate_ensure` normally provides (e.g. `perform:`).
    /// See `generate_block_value_structural_fallback` for the
    /// general Tier 1/Tier 2 discrimination rationale.
    ///
    /// Both receiver and cleanup block must be Tier 1 (pure, 0-arg funs) for
    /// the generic try/catch below to be correct — Core Erlang's try/catch
    /// mechanics don't themselves need the block's AST, only ADR-0041's
    /// state-threading convention does, so the pure case is fully generic.
    /// `current_method_params[0]` is the cleanup block; `Self` is the
    /// protected block.
    pub(in crate::core_erlang) fn generate_ensure_structural_fallback(
        &mut self,
        class_name: &str,
    ) -> Document<'static> {
        let self_var = if self.in_class_method() {
            "ClassSelf"
        } else {
            "Self"
        };
        let cleanup_param = self
            .current_method_params
            .first()
            .cloned()
            .unwrap_or_else(|| "CleanupBlock".to_string());

        let runtime_module =
            super::super::primitive_bindings::PrimitiveBindingTable::runtime_module_for_class(
                class_name,
            );
        let placeholder_branch = docvec![
            "call ",
            leaf::atom(runtime_module),
            ":'dispatch'('ensure', [",
            leaf::var(cleanup_param.clone()),
            "], ",
            Document::Str(self_var),
            ")",
        ];

        let try_result_var = self.fresh_temp_var("TryResult");
        let result_var = self.fresh_temp_var("Result");
        let catch = self.ensure_structural_fallback_catch(&cleanup_param);
        let cleanup_apply = || docvec!["apply ", leaf::var(cleanup_param.clone()), " ()"];
        // Not `form::try_of_catch`: `of R` is the bare spelling (not
        // `of <R>`), and the `catch` clause arrives as a finished document.
        let tier1_try = docvec![
            "try ",
            form::let_in(
                leaf::var(try_result_var.clone()),
                self.annotate_current_send_call(docvec!["apply ", Document::Str(self_var), " ()"]),
                leaf::var(try_result_var),
                Layout::Flat,
            ),
            " of ",
            leaf::var(result_var.clone()),
            " -> ",
            form::let_discard(self.annotate_current_send_call(cleanup_apply())),
            leaf::var(result_var),
            " ",
            catch,
        ];

        let cleanup_stateful_error = self.generate_stateful_block_dispatch_error(
            "ensure:",
            class_name,
            STATEFUL_BLOCK_DISPATCH_HINT,
        );
        let cleanup_tier_check = arity_dispatch_doc(
            &leaf::var(cleanup_param),
            tier1_try,
            cleanup_stateful_error,
            placeholder_branch.clone(),
        );

        let self_stateful_error = self.generate_stateful_block_dispatch_error(
            "ensure:",
            class_name,
            STATEFUL_BLOCK_DISPATCH_HINT,
        );
        arity_dispatch_doc(
            &Document::Str(self_var),
            cleanup_tier_check,
            self_stateful_error,
            placeholder_branch,
        )
    }

    /// Generates block body expressions with state mutation threading.
    ///
    /// Follows the same pattern as `generate_while_body_with_threading`:
    /// - Sets `in_loop_body = true` so field reads/writes use `StateAcc`
    /// - Resets `state_version` to 0 (`StateAcc` is version 0)
    /// - Threads field assignments, self-sends, and local var assignments
    /// - Returns an [`ExceptionArm`] — the Document holding the generated
    ///   code, the variable holding the last expression's result, the final
    ///   state version number, and (BT-3486/BT-3506) the arm's final version
    ///   of whichever extra family `families` names, when it actually
    ///   mutated it
    ///
    /// The caller must have already bound `StateAcc` to the current state
    /// before calling this function.
    ///
    /// BT-3486/BT-3506: the extra family's version must be read from INSIDE
    /// `with_branch_context` — that guard lets each arm inherit the outer
    /// version on entry (so both arms are true siblings, both sourcing their
    /// mutation chains from the same pre-`try` baseline) and restores it on
    /// exit (so an arm's own in-`try` version, unreachable from outside the
    /// Core Erlang `try`, never leaks into the enclosing method's live
    /// version). That restore is exactly why the caller cannot read it back
    /// afterwards, and why it is returned here.
    pub(super) fn generate_exception_body_with_threading(
        &mut self,
        body: &Block,
        families: &ThreadedFamilies,
        frame: Option<&ArmFrame>,
        entry: Option<&ArmEntry>,
    ) -> Result<ExceptionArm> {
        let statements = super::super::util::collect_body_exprs(&body.body);
        self.with_branch_context(|this| {
            // ADR 0131 §2 (Phase 3b): a `BranchArm` frame of an exception
            // producer. Registers the arm's `RebindFrame` and the arm
            // statements' producer sites (the rule a conditional arm uses), so a
            // loop, fold, conditional or `on:do:` in the arm is a producer whose
            // `LocalRebind`s this frame lowers.
            if let Some(arm) = frame {
                let rebind = RebindFrame {
                    frame: this.current_branch_frame(),
                    kind: RebindFrameKind::BranchArm,
                    threads: arm.threads.clone(),
                };
                return this.with_rebind_frame(rebind, |this| {
                    this.with_body_producer_sites(&statements, false, true, |this| {
                        this.generate_exception_body_with_threading_inner(
                            body,
                            families,
                            Some(arm),
                            entry,
                        )
                    })
                });
            }
            // ADR 0133 Phase 3 (BT-3911): an arm of an `on:do:`/`ensure:` that
            // is not a producer is a `BranchArm` frame too (see
            // `generate_plain_arm`): it carries the locals its block threads
            // and registers no producer sites.
            if !this.is_repl_mode() {
                let arm = ArmFrame {
                    threads: this.branch_threaded_locals(&[body]),
                };
                let rebind = RebindFrame {
                    frame: this.current_branch_frame(),
                    kind: RebindFrameKind::BranchArm,
                    threads: arm.threads.clone(),
                };
                return this.with_rebind_frame(rebind, |this| {
                    this.generate_exception_body_with_threading_inner(
                        body,
                        families,
                        Some(&arm),
                        entry,
                    )
                });
            }
            // BT-3823: in the REPL the block is inlined over the bindings map,
            // and a producer at one of its statements' sites adopts it
            // (`with_repl_arm_producer_sites`).
            this.with_repl_arm_rebind_frame(|this| {
                this.with_repl_arm_producer_sites(&statements, |this| {
                    this.generate_exception_body_with_threading_inner(body, families, None, entry)
                })
            })
        })
    }

    /// The frame an arm entered after the try body ([`ArmEntry`]) lowers its
    /// entry `LocalRebind`s by: the frame the arm itself is rendered as
    /// (`render_branch_arm`), so the verifier's `LocalRebindModeMismatch`
    /// reads the same cell of ADR 0131 §2's table. Outside the REPL that is
    /// the `BranchArm` frame carrying the arm's `threads`. A REPL arm is not a
    /// `BranchArm`: it runs over the bindings map (the `StateAcc` frame
    /// [`Self::with_repl_arm_rebind_frame`] registers), where every binding is
    /// a member (`RebindFrame::with_repl_binding`), so an entry rebind is a
    /// `Put` of the binding's key (BT-4424: inside a REPL loop body, a binding
    /// the body wrote before the `ensure:` is bound in the generated code and
    /// threaded here, and was lowered as a plain `let`).
    fn arm_entry_rebind_frame(
        &self,
        frame: FrameId,
        arm: Option<&ArmFrame>,
        locals: &[String],
    ) -> Result<RebindFrame> {
        if !self.is_repl_mode() {
            return Ok(RebindFrame {
                frame,
                kind: RebindFrameKind::BranchArm,
                threads: arm.map(|a| a.threads.clone()).unwrap_or_default(),
            });
        }
        let repl_arm = self.rebind_frame_of(frame)?;
        Ok(locals
            .iter()
            .fold(repl_arm, |frame, local| frame.with_repl_binding(local)))
    }

    /// The `BranchArm` frame shared by the arms of an `on:do:`/`ensure:` that
    /// is a local-threading producer: the construct's seeded set
    /// ([`Self::branch_threaded_locals`] over `blocks`). `None` for the legacy
    /// statement lowering, whose arms are not frames.
    pub(super) fn exception_arm_frame(
        &self,
        blocks: &[&Block],
        producer: bool,
    ) -> Option<ArmFrame> {
        (producer && !self.is_repl_mode()).then(|| ArmFrame {
            threads: self.branch_threaded_locals(blocks),
        })
    }

    /// Inner implementation called inside `with_branch_context`.
    ///
    /// ADR 0111 Addendum 5: this arm's mutation sequence is built
    /// as real [`ThreadedStmt`]s — the E1–E7 per-shape decomposition table
    /// (E1/E3 reuse `conditionals.rs`'s C1/C2 helpers,
    /// `lower_field_assignment_bind`/`lower_local_var_assignment_bind`,
    /// exactly — same shape, same mint order) — then `verify()`d and
    /// `render()`d via `conditionals.rs`'s `render_branch_arm`.
    /// Rule 2's separator divergence (this file inserts a literal `" "`
    /// between SOURCE-level statements, `conditionals.rs` inserts none at
    /// all) is reproduced by pushing that literal space as its own
    /// `ThreadedStmt::Statement` at each source-statement boundary —
    /// **not** by feeding the flat per-shape `Bind`/`Statement` sequence
    /// through `render_loop_body_statements` (that function separates every
    /// *raw* `ThreadedStmt` entry, which would inject a spurious extra
    /// space inside any shape that itself decomposes into more than one
    /// entry, e.g. E1's Statement+Bind pair — a real double-space bug, not
    /// a cosmetic one).
    /// `render_branch_arm`'s plain `render()` has no separator
    /// of its own, so the manually-inserted space is the only one that
    /// ends up in the output, at exactly the position this same
    /// `if i > 0 { docs.push(" ") }` loop below has always put it — this
    /// is the loop's own IR-producing form, not a match against some
    /// other, deleted implementation.
    #[allow(clippy::too_many_lines)]
    fn generate_exception_body_with_threading_inner(
        &mut self,
        body: &Block,
        families: &ThreadedFamilies,
        arm: Option<&ArmFrame>,
        entry: Option<&ArmEntry>,
    ) -> Result<ExceptionArm> {
        let frame = self.current_branch_frame();
        // the baseline this arm inherits, for whichever extra family
        // `families` names (see `generate_exception_body_with_threading`'s
        // doc comment) — compared against the post-body version below to
        // detect a mutation, exactly as `build_vt_conditional_branch_pieces_inner`
        // does for a conditional arm's own `SelfVt` diff.
        let family_before = families
            .as_slice()
            .first()
            .map(|prefix| (prefix.clone(), self.family_version(prefix)));
        // push a scope so a local-var assignment's `bind_var` rebind
        // (from `lower_local_var_assignment_bind`) is scoped to this try
        // body and doesn't leak into the enclosing method scope — matching the
        // bracket `generate_conditional_branch_inline` already has (conditionals.rs).
        self.push_scope();

        let has_direct_field_assignments = body
            .body
            .iter()
            .any(|s| self.is_field_assignment(&s.expression));

        let mut result_var = "'nil'".to_string();
        let mut stmts: Vec<ThreadedStmt> = Vec::new();
        // BT-4310: the arm's loops and folds threading only `State` at an
        // operand another lowering compiles, checked against the lowered arm.
        let family_sites =
            self.family_only_operand_sites_of(&super::super::util::collect_body_exprs(&body.body));

        // ADR 0131 §2 (Phase 3b): an arm entered after the try body (the
        // success-path cleanup of `ensure:`) reads the locals the try body
        // wrote back from its returned tuple, as `LocalRebind`s lowered by the
        // frame the arm is rendered as ([`Self::arm_entry_rebind_frame`]: a
        // REPL arm `Put`s each binding over the bindings map, BT-4424).
        if let Some(entry) = entry {
            let enclosing = self.arm_entry_rebind_frame(frame, arm, &entry.locals)?;
            let carrier = self.fresh_temp_var("ExEntry");
            let prelude = self.construct_rebind_prelude(
                &enclosing,
                &carrier,
                leaf::var(entry.tuple_var.clone()),
                &entry.locals,
                ProducerTail::Rebind,
                Vec::new(),
                frame,
                body.span,
            );
            stmts.extend(prelude);
        }

        // BT-3687: `@expect` directives are compile-time-only annotations with no
        // runtime value; lowering one as a statement would emit `let _ =  in`.
        // `collect_body_exprs` is the canonical directive filter.
        let body_exprs = super::super::util::collect_body_exprs(&body.body);
        // BT-4461: a REPL arm's producer sites that write a binding the arm
        // holds in a variable (`repl_frame_producer_sites`), read before each
        // statement is lowered and checked once the arm is built.
        let mut repl_sites: Vec<(beamtalk_core::source_analysis::Span, Vec<String>)> = Vec::new();
        for (i, &expr) in body_exprs.iter().enumerate() {
            let span = expr.span();
            if arm.is_none() {
                repl_sites.extend(self.repl_frame_producer_sites(expr));
            }
            if i > 0 {
                // Rule 2: the literal space `generate_exception_body_with_threading_inner`
                // has always inserted between source-level statements —
                // modeled as its own opaque Statement so it renders exactly
                // once per statement boundary, never inside a shape's own
                // multi-entry decomposition.
                stmts.push(ThreadedStmt::Statement(Document::Str(" "), span));
            }
            let is_last = i == body_exprs.len() - 1;
            // A value-type `self.field := ...` write nested inside a further
            // construct of this arm's own body — most notably another
            // `on:do:`/`ensure:` — is not a bare top-level statement, so
            // `block_writes_vt_self_field` (top-level-only, same reason as
            // `loop_body_threads_value_self`) cannot see it and the
            // enclosing construct's `Self` slot misses the mutation. Reject
            // it with the same diagnostic
            // [`CoreErlangGenerator::reject_unthreadable_value_self_field_write`]
            // gives the identical loop-nesting shape. This single
            // per-statement pass covers every exception-construct arm (try
            // body, `on:do:` handler, `ensure:` cleanup), since all three
            // lower through this function. A no-op outside value-type
            // context, and a no-op for this statement when it is itself the
            // bare top-level write `exception_construct_families`
            // already threads.
            self.reject_unthreadable_value_self_field_write(expr, self.is_field_assignment(expr))?;
            if self.is_field_assignment(expr) {
                // E1 — same shape/mint-order as C1; reused directly.
                let val_var = self.lower_field_assignment_bind(expr, frame, span, &mut stmts)?;
                if is_last {
                    // `:=` evaluates to the assigned value: a block that ends in
                    // a field write answers it (`lower_field_assignment_bind`
                    // bound it to `val_var`).
                    result_var = val_var;
                }
            } else if self.is_actor_self_send(expr) {
                let Expression::MessageSend {
                    selector,
                    arguments,
                    ..
                } = expr
                else {
                    unreachable!("is_actor_self_send guarantees a MessageSend");
                };
                // E2 — BT-3814: the one self-send-statement lowering
                // (`sequenced_self_dispatch`, shared with
                // `threaded_expression`'s producer, the Letrec body and the
                // fold bodies): operands sequenced ahead, then the dispatch
                // `Statement` + the `State` `Bind` (`element(2, _SD)`). This
                // arm keeps its own `ExResult` epilogue below.
                //
                // `expr` stays on this path, NOT `threaded_expression`'s
                // producer: `is_actor_self_send` (this arm's guard) is WIDER
                // than `is_dispatching_actor_self_send` (the producer's
                // gate) — it also matches a well-known selector (`self
                // class`, `self hash`, …) that the dispatch call dispatches
                // uniformly through `safe_dispatch` here (an `on:do:`/
                // `ensure:` body's own long-standing behavior), where the
                // producer would instead intercept it specially — a real
                // behavior change this call site must not make.
                let (prelude, dispatch_var) =
                    self.sequenced_self_dispatch(selector, arguments, frame, span)?;
                stmts.extend(prelude);
                if is_last {
                    // Self-dispatch result is in dispatch_var
                    let rv = self.fresh_temp_var("ExResult");
                    stmts.push(ThreadedStmt::Statement(
                        docvec![form::let_prefix(
                            leaf::var(rv.clone()),
                            form::element(1, leaf::var(dispatch_var))
                        )],
                        span,
                    ));
                    result_var = rv;
                }
            } else if Self::is_local_var_assignment(expr) {
                // E3 — same shape/mint-order as C2/C3/C4; reused directly.
                let val_var =
                    self.lower_local_var_assignment_bind(expr, frame, span, &mut stmts)?;
                if is_last {
                    // As above: the block's value is the assigned value.
                    result_var = val_var;
                }
            } else if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                // E4 — the right-hand side's own prelude is threaded ahead
                // into this body (BT-4000), and each name is written to the
                // body's slot as E3 writes `name := ...` (BT-4439).
                let (rhs_var, _) =
                    self.lower_destructure_bind(pattern, value, frame, span, &mut stmts, &|_| {
                        LocalWrite::Slot
                    })?;
                if is_last {
                    // BT-3889: the block answers the right-hand side.
                    result_var = rhs_var;
                }
            } else if is_last {
                if has_direct_field_assignments {
                    // E6 — has_direct_field_assignments sub-branch.
                    // ADR 0118 phase 2a: thread every
                    // state-effecting sub-expression (`1 + (self bump)`) —
                    // AND, since ADR 0118 phase 5b widened
                    // `subexpr_needs_prelude` to recognize a class-var
                    // producer too, a bare class-method self-send with no
                    // enclosing assignment (e.g. `self bump` as this try
                    // body's last statement) — as real `Bind`s ahead of the
                    // compile, via `thread_ahead`. The plain compile below
                    // then reads the already-threaded value back via
                    // `precompiled_subexprs` substitution; no open let-chain
                    // reaches this point any more.
                    let hoist_scope = self.thread_ahead(expr, &mut stmts, frame)?;
                    let rv = self.fresh_temp_var("ExResult");
                    let expr_doc = self.expression_doc(expr)?;
                    self.finish_precompiled_scope(hoist_scope)?;
                    stmts.push(ThreadedStmt::Statement(
                        form::let_header(leaf::var(rv.clone()), expr_doc),
                        span,
                    ));
                    result_var = rv;
                } else {
                    // Last expression with no direct field assignments.
                    // If this is a nested control flow construct returning {Result, State},
                    // destructure it. Otherwise just capture the result.
                    // A producer of this arm (a `BranchArm` frame, or a REPL arm,
                    // BT-3823; a conditional whose receiver advances `State`, BT-4000) goes to
                    // `thread_ahead` below instead.
                    if self.control_flow_has_mutations(expr)
                        && !self.arm_statement_is_producer(expr, arm.is_some())
                    {
                        // E5 — nested mutation construct returns {Result,
                        // State} tuple; C10-last's shape with ExResult
                        // naming. Legacy read the target state name via
                        // `peek_next_state_var()` (no mint) before building
                        // the expr doc purely to have the LHS text ready —
                        // the Bind below re-derives the same rendered name
                        // from the version number instead, so the peek call
                        // itself is no longer needed (it never minted
                        // anything; only the ordering of the real mints
                        // around it matters, and that ordering — Tuple,
                        // ExResult, expr doc's own mints, then the bump —
                        // is unchanged below).
                        let tuple_var = self.fresh_temp_var("Tuple");
                        let rv = self.fresh_temp_var("ExResult");
                        let expr_doc = self.expression_doc(expr)?;
                        let source_version = self.state_version();
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                form::let_prefix(leaf::var(tuple_var.clone()), expr_doc),
                                form::let_prefix(
                                    leaf::var(rv.clone()),
                                    form::element(1, leaf::var(tuple_var.clone()))
                                )
                            ],
                            span,
                        ));
                        self.bind_tuple_state(&mut stmts, frame, source_version, &tuple_var, span);
                        result_var = rv;
                    } else {
                        // E6/E7 — plain expression, no direct field
                        // assignments in this body. See the E6
                        // has_direct_field_assignments sub-branch above.
                        let hoist_scope = self.thread_ahead(expr, &mut stmts, frame)?;
                        let rv = self.fresh_temp_var("ExResult");
                        let expr_doc = self.expression_doc(expr)?;
                        self.finish_precompiled_scope(hoist_scope)?;
                        stmts.push(ThreadedStmt::Statement(
                            form::let_header(leaf::var(rv.clone()), expr_doc),
                            span,
                        ));
                        result_var = rv;
                    }
                }
            } else {
                // E7 — non-last plain expression. ADR 0118 phase 5b:
                // `thread_ahead` threads any state-effecting producer into
                // `stmts` as a real `Bind`, in the SAME frame every later
                // statement in this body shares — visible to them by
                // construction — so the plain compile below never has an
                // open scope to propagate.
                // ADR 0118 phase 2a: see the E6 sub-branch above.
                let hoist_scope = self.thread_ahead(expr, &mut stmts, frame)?;
                let expr_doc = self.expression_doc(expr)?;
                self.finish_precompiled_scope(hoist_scope)?;
                stmts.push(ThreadedStmt::Statement(
                    form::let_header(leaf::var("_"), expr_doc),
                    span,
                ));
            }
        }
        let final_state_version = self.state_version();
        let family_mutated_version = family_before.and_then(|(prefix, before)| {
            let after = self.family_version(&prefix);
            (after != before).then_some((prefix, after))
        });
        self.pop_scope();
        self.check_family_only_operand_sites(
            &stmts,
            &family_sites,
            "`on:do:`/`ensure:` arm",
            body.span,
        );
        self.check_repl_frame_producer_sites(&stmts, &repl_sites, body.span);
        let (doc, _) = self.render_branch_arm(stmts, frame, final_state_version, body.span, arm);
        Ok(ExceptionArm {
            doc,
            result_var,
            state_version: final_state_version,
            family_mutated_version,
        })
    }
}

/// The structural fallbacks' arity dispatch on a block `fun`:
/// `case is_function(F, 0) of <'true'> -> zero_arity <'false'> -> case
/// is_function(F, 1) of <'true'> -> one_arity <'false'> -> other end end`.
fn arity_dispatch_doc(
    fun: &Document<'static>,
    zero_arity: Document<'static>,
    one_arity: Document<'static>,
    other: Document<'static>,
) -> Document<'static> {
    let arity_test =
        |arity: i64| form::call("erlang", "is_function", [fun.clone(), leaf::int_lit(arity)]);
    let on_true = |body| form::clause([leaf::atom("true")], None, body, Layout::Flat);
    let on_false = |body| form::clause([leaf::atom("false")], None, body, Layout::Flat);
    form::case_of(
        arity_test(0),
        [
            on_true(zero_arity),
            on_false(form::case_of(
                arity_test(1),
                [on_true(one_arity), on_false(other)],
                Layout::Flat,
            )),
        ],
        Layout::Flat,
    )
}
