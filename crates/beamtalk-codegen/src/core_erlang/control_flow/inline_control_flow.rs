// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Inline control-flow lowering and the field / local assignment binds
//! (split from `conditionals.rs`).

use super::super::threaded_expr::ProducerTail;
use super::super::threaded_ir::{
    BindOp, FrameId, RebindFrameKind, ThreadedStmt, ThreadedValue, ValueRef, VersionPrefix,
    VersionedVar,
};
use super::super::{CodeGenError, CoreErlangGenerator, Result};
use super::conditionals::{Closure, FieldWriteSite, SoloBoolean};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::form;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Expression, MessageSelector};
use beamtalk_core::source_analysis::Span;

impl CoreErlangGenerator {
    /// ADR 0118 phase 4: `true` if `expr` (already paren-unwrapped)
    /// is an inline-threaded control-flow construct — `ifTrue:`/`ifFalse:`/
    /// `ifTrue:ifFalse:`, `and:`/`or:`, the nil-conditional family, or
    /// `match:` — that needs mutation threading.
    /// The pure gate behind
    /// [`Self::inline_control_flow_producer`], reusing the exact same
    /// per-family predicates that producer dispatches on, so the two can
    /// never disagree. Called by
    /// [`CoreErlangGenerator::subexpr_needs_prelude`] (`threaded_prelude.rs`) to decide
    /// whether compiling `expr` — receiver, argument, or any other
    /// sequenced position — would give it a non-empty prelude.
    pub(in crate::core_erlang) fn inline_control_flow_needs_threading(
        &self,
        expr: &Expression,
    ) -> bool {
        let expr = expr.unwrap_parens();
        if let Expression::Match { arms, .. } = expr {
            return self.match_needs_state_threading(arms);
        }
        // BT-3982: an actor evaluated literal block that threads `State` is a
        // producer wherever it sits (`generate_evaluated_block_producer`).
        if self.actor_evaluated_block_producer_parts(expr).is_some() {
            return true;
        }
        let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        else {
            return false;
        };
        let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
        match (sel.as_str(), arguments.as_slice()) {
            ("and:" | "or:", [Expression::Block(block)]) => {
                self.and_or_needs_mutation_threading(receiver, block)
            }
            ("ifTrue:" | "ifFalse:" | "ifNotNil:" | "ifNil:", [Expression::Block(block)]) => {
                self.conditional_needs_mutation_threading(receiver, &[block])
            }
            (
                "ifTrue:ifFalse:" | "ifNil:ifNotNil:" | "ifNotNil:ifNil:",
                [Expression::Block(b0), Expression::Block(b1)],
            ) => self.conditional_needs_mutation_threading(receiver, &[b0, b1]),
            _ => false,
        }
    }

    /// ADR 0118 phase 4 (ADR 0118 §Decision 2): the [`ThreadedValue`]
    /// producer for every inline-threaded control-flow construct that can
    /// sit anywhere in expression position — `ifTrue:`/`ifFalse:`/
    /// `ifTrue:ifFalse:`, `and:`/`or:`, the nil-conditional family, and
    /// `match:`. `Ok(None)` when `expr` (already paren-unwrapped) is not one
    /// of these, or is but [`Self::inline_control_flow_needs_threading`]
    /// says it doesn't need threading — the caller then falls through to
    /// its ordinary (unthreaded) compile.
    ///
    /// The single recognizer both
    /// [`CoreErlangGenerator::threaded_expression`]'s inline-control-flow
    /// branch (nested/argument expression position — the row that used to
    /// leak a `{Result, State}` tuple into an argument) and
    /// [`super::super::threaded_expr`]'s Actor-body
    /// `emit_actor_threaded_last_stmts`/`emit_actor_threaded_assign_rhs_stmts`
    /// (an already-classified top-level `ControlFlowWithMutations`
    /// statement) call, so a decision to inline can never disagree with
    /// what inlines — and the one place any of these six-plus-`match:`
    /// producers' preludes are built (via
    /// [`Self::control_flow_tuple_to_threaded_value`]), so no
    /// `{Result, State}` tuple `Document` crosses an expression boundary
    /// through this path.
    pub(in crate::core_erlang) fn inline_control_flow_producer(
        &mut self,
        expr: &Expression,
    ) -> Result<Option<ThreadedValue>> {
        self.inline_control_flow_producer_with(expr, ProducerTail::Rebind)
    }

    /// [`Self::inline_control_flow_producer`] with the tail of the construct's
    /// locals chosen by the caller (ADR 0131 §2): `Rebind` rebinds every
    /// threaded local; `Discard` is the method root's last expression, whose
    /// rebinds nothing can observe.
    ///
    /// A conditional that is a root local-threading producer
    /// ([`Self::conditional_producer_locals`]) is lowered even when the
    /// statement-position gate ([`Self::inline_control_flow_needs_threading`],
    /// read+write analysis) would not inline it: a write-only arm
    /// (`c ifTrue: [t := 5]`) threads `t` all the same.
    pub(in crate::core_erlang) fn inline_control_flow_producer_with(
        &mut self,
        expr: &Expression,
        tail: ProducerTail,
    ) -> Result<Option<ThreadedValue>> {
        let expr = expr.unwrap_parens();
        // BT-4366: the construct's nested call nodes carry its line.
        self.with_current_span(expr.span(), |this| {
            this.inline_control_flow_producer_inner(expr, tail)
        })
    }

    fn inline_control_flow_producer_inner(
        &mut self,
        expr: &Expression,
        tail: ProducerTail,
    ) -> Result<Option<ThreadedValue>> {
        let root_producer_locals = if self.is_producer_frame_for(self.current_frame(), expr) {
            self.conditional_producer_locals(expr)
        } else {
            None
        };
        // BT-4499: a block parameter a REPL conditional writes joins the
        // bindings map for the construct (and leaves it after), as for any
        // binding its frame holds in a variable. Only a construct that will be
        // lowered here is seeded.
        let lowers_here =
            root_producer_locals.is_some() || self.inline_control_flow_needs_threading(expr);
        let params = if lowers_here {
            self.repl_param_bindings_written_by(expr)
        } else {
            Vec::new()
        };
        let frame = self.current_frame();
        let (seed_prelude, seeds) = self.seed_repl_params(&params, frame, expr.span())?;
        let mut lowered =
            self.lower_inline_control_flow(expr, tail, root_producer_locals.is_some())?;
        if !seeds.is_empty() {
            let Some(tv) = lowered.as_mut() else {
                return Err(CodeGenError::Internal(
                    "a REPL conditional seeded its block parameters but was not lowered"
                        .to_string(),
                ));
            };
            let mut prelude = seed_prelude;
            prelude.append(&mut tv.prelude);
            prelude.extend(self.unseed_repl_params(&seeds, frame, expr.span()));
            tv.prelude = prelude;
        }
        if let Some(tv) = &lowered {
            self.report_rebinds_visible(&tv.prelude, expr.span());
        }
        #[cfg(any(test, feature = "test"))]
        if let (Some(_), Some(locals)) = (&lowered, &root_producer_locals) {
            super::super::fault::record_producer(expr.span(), locals);
        }
        Ok(lowered)
    }

    /// [`Self::inline_control_flow_producer_with`] once `expr` is
    /// paren-unwrapped; `root_producer` says it is a conditional local-threading
    /// producer ([`Self::conditional_producer_locals`]).
    #[allow(clippy::too_many_lines)] // one arm per conditional selector
    fn lower_inline_control_flow(
        &mut self,
        expr: &Expression,
        tail: ProducerTail,
        root_producer: bool,
    ) -> Result<Option<ThreadedValue>> {
        if !root_producer && !self.inline_control_flow_needs_threading(expr) {
            return Ok(None);
        }
        if let Expression::Match { value, arms, .. } = expr {
            let span = expr.span();
            // BT-3947: a `match:` that is a local-threading producer here has
            // one `BranchArm` frame per arm, like a conditional's blocks.
            if root_producer && !self.is_repl_mode() {
                return Ok(Some(self.generate_match_producer(value, arms, span, tail)?));
            }
            // BT-4461: a REPL `match:` adopts the bindings map whole, like the
            // REPL's other producers, so the bindings its frame holds in
            // variables that it writes are read out of that map
            // (`repl_adoption_prelude`).
            let cached = self.repl_producer_cached_bindings(expr);
            if !cached.is_empty() {
                let frame = self.current_frame();
                let _ = self.drop_cached_repl_bindings(&cached);
                let tuple_doc = self.generate_match(value, arms)?;
                let carrier = self.fresh_temp_var("CF");
                let adopt = self.extract_state_family(&carrier, frame, span);
                let prelude =
                    self.repl_adoption_prelude(&carrier, tuple_doc, &cached, adopt, frame, span)?;
                return Ok(Some(ThreadedValue {
                    prelude,
                    value: ValueRef::Doc(docvec![form::element(1, leaf::var(carrier)),]),
                }));
            }
            let frame = self.current_frame();
            let tuple_doc = self.generate_match(value, arms)?;
            return Ok(Some(
                self.control_flow_tuple_to_threaded_value(tuple_doc, frame, span)?,
            ));
        }
        // ADR 0131 Phase 2d (BT-3798): a `Dictionary` lookup whose handler
        // writes an outer local, a producer here (never `needs_threading`).
        if root_producer && let Some(tv) = self.generate_lookup_producer(expr, tail)? {
            return Ok(Some(tv));
        }
        // BT-3841: an evaluated literal block (`[...] value`) that is a
        // local-threading producer here is a conditional whose one arm always
        // runs; BT-4263: so is one with arguments (`[:y | t := y] value: 5`),
        // its arguments threaded first. BT-3982: so is an actor evaluated
        // block of the whole `value` family that threads `State`, wherever it
        // sits.
        let evaluated = root_producer
            .then(|| beamtalk_core::semantic_analysis::block_facts::literal_block_value_send(expr))
            .flatten()
            .or_else(|| self.actor_evaluated_block_producer_parts(expr));
        if let Some((block, arguments)) = evaluated {
            return Ok(Some(self.generate_evaluated_block_producer(
                block,
                arguments,
                tail,
                root_producer,
            )?));
        }
        let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        else {
            // Unreachable: `inline_control_flow_needs_threading` only
            // returns `true` for a `Match` or a keyword `MessageSend`.
            return Ok(None);
        };
        let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
        if let (Some(solo), [Expression::Block(block)]) =
            (SoloBoolean::from_keyword(&sel), arguments.as_slice())
        {
            return Ok(Some(self.generate_solo_boolean_with_mutations(
                solo,
                receiver,
                block,
                tail,
                root_producer,
            )?));
        }
        let tv = match (sel.as_str(), arguments.as_slice()) {
            (
                "ifTrue:ifFalse:",
                [
                    Expression::Block(true_block),
                    Expression::Block(false_block),
                ],
            ) => self.generate_if_true_if_false_with_mutations(
                receiver,
                true_block,
                false_block,
                tail,
                root_producer,
            )?,
            ("ifNotNil:", [Expression::Block(block)]) => self
                .generate_nil_conditional_with_mutations(
                    receiver,
                    None,
                    Some(block),
                    tail,
                    root_producer,
                )?,
            ("ifNil:", [Expression::Block(block)]) => self
                .generate_nil_conditional_with_mutations(
                    receiver,
                    Some(block),
                    None,
                    tail,
                    root_producer,
                )?,
            (
                "ifNil:ifNotNil:",
                [
                    Expression::Block(nil_block),
                    Expression::Block(not_nil_block),
                ],
            )
            | (
                "ifNotNil:ifNil:",
                [
                    Expression::Block(not_nil_block),
                    Expression::Block(nil_block),
                ],
            ) => self.generate_nil_conditional_with_mutations(
                receiver,
                Some(nil_block),
                Some(not_nil_block),
                tail,
                root_producer,
            )?,
            _ => return Ok(None),
        };
        Ok(Some(tv))
    }

    /// ADR 0111 Addendum 5 §C1: lowers a `self.field := value`
    /// conditional-branch/block-body statement to its real `Bind` sequence,
    /// appending it to `stmts` and returning the assigned value's temp var
    /// name. Mirrors [`CoreErlangGenerator::generate_field_assignment_open`]'s
    /// normal (non hybrid-full-extract) branch exactly — same helper calls,
    /// same mint order, same [`FieldWriteSite`] dispatch — but
    /// models the state mutation as a [`ThreadedStmt::Bind`] instead of a
    /// hand-rolled `maps:put` `Document` fragment.
    ///
    /// The hybrid full-extract sub-branch (`in_hybrid_loop &&
    /// hybrid_mutated_fields.contains(field)`) rebinds a direct fun parameter
    /// instead of `maps:put`-ing into `StateAcc`; it lowers through
    /// [`Self::lower_hybrid_field_rebind`], the one lowering
    /// `generate_field_assignment_open`'s hybrid branch renders too.
    /// §Scope: a mutation-carrying conditional forces `StateAcc` fallback,
    /// mutually exclusive with hybrid mode, but `generate_block_stateful_body`
    /// and the Letrec bodies reach this with the exclusion less directly
    /// self-evident, so the check is real rather than assumed away.
    pub(in crate::core_erlang) fn lower_field_assignment_bind(
        &mut self,
        expr: &Expression,
        frame: FrameId,
        span: Span,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<String> {
        let Expression::Assignment { target, value, .. } = expr else {
            unreachable!("field-assignment lowering requires an Assignment expr");
        };
        let Expression::FieldAccess { field, .. } = target.as_ref() else {
            unreachable!("field-assignment lowering requires a FieldAccess target");
        };
        // BT-4122 (`FieldReadFromStateAcc` fun-body arm): a Tier 2 `fun`
        // threads this write through its `StateAcc`. After the merge the
        // `fun` writes the cell instead; a merged body never reaches here.
        if self.in_tier2_stateful_body && self.synced_calls_scope() {
            self.record_tier2_stateacc_gap(
                format!(
                    "{} := (tier2 fun body writes field via StateAcc)",
                    field.name
                ),
                span,
            );
        }
        if self.loop_mode.in_hybrid_loop
            && self
                .loop_mode
                .hybrid_mutated_fields
                .contains(field.name.as_str())
        {
            return self.lower_hybrid_field_rebind(&field.name, value, frame, span, stmts);
        }

        // ADR 0118 phase 2a: `self.log := self.log ++
        // #(self getValue)` — thread every order-safe self-send nested in
        // the RHS as real `Bind`s *before* `source_version` is read below,
        // so this assignment's own `Bind` chains from the post-dispatch
        // state and the RHS compile that follows is pure (the exact call
        // site an earlier, reverted version-bump-on-compile prototype
        // desynced).
        let site = FieldWriteSite::for_context(self.context);
        let thread_scope = self.thread_ahead(value, stmts, frame)?;
        let (preamble, bind, val_var) = self.lower_simple_field_write_bind(
            site,
            Closure::Open,
            &field.name,
            value,
            frame,
            span,
        )?;
        self.finish_precompiled_scope(thread_scope)?;
        stmts.push(ThreadedStmt::Statement(preamble, span));
        stmts.push(bind);
        Ok(val_var)
    }

    /// The one lowering of a hybrid-loop field rebind (BT-3811): in a hybrid
    /// loop a mutated field is extracted to a direct fun parameter, so
    /// `self.field := value` rebinds that parameter instead of stepping the
    /// `State` map (`let Val = <value> in let FieldN = Val in`, plus an update
    /// of `hybrid_readonly_field_params` so later reads see the new variable),
    /// decomposed into a real `Bind` so the field's own rebind chain is
    /// traceable from its `produces` seed (`VersionPrefix::Gensym` of the
    /// pre-extracted param name).
    ///
    /// Appends to `stmts`, in `frame`: the RHS's ahead prelude (a field
    /// assignment or self-send nested in the RHS, threaded by
    /// [`Self::thread_ahead`] so its steps land in the caller's statement
    /// list, not in the RHS document), the opaque `let Val = ...`, and the
    /// rebind `Bind`. Returns the assigned value's temp var.
    ///
    /// Shared by [`Self::lower_field_assignment_bind`] (every `Vec<ThreadedStmt>`
    /// consumer: Letrec bodies, conditional arms, stateful-block bodies) and
    /// [`CoreErlangGenerator::generate_field_assignment_open`] (the
    /// `Document`-returning consumers, which render these statements through
    /// [`CoreErlangGenerator::rendered_field_rebind_doc`]).
    pub(in crate::core_erlang) fn lower_hybrid_field_rebind(
        &mut self,
        field: &str,
        value: &Expression,
        frame: FrameId,
        span: Span,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<String> {
        // BT-4369: a hybrid loop never brackets, but one nested in a bracketing
        // loop would be a field write in its body (ADR 0137 §4b).
        self.note_field_write(field, span);
        let val_var = self.fresh_temp_var("Val");
        let ahead_scope = self.thread_ahead(value, stmts, frame)?;
        // `thread_ahead` above already committed any nested field
        // assignment's update (`self.x := (self.y := 42)` keeps `y`'s new
        // param); only the RHS's own (precompiled, value-only) compile below
        // is scoped, so snapshot the params around it.
        let saved_field_params = self.loop_mode.hybrid_readonly_field_params.clone();
        let val_doc = self.expression_doc(value)?;
        self.finish_precompiled_scope(ahead_scope)?;
        self.loop_mode.hybrid_readonly_field_params = saved_field_params;
        let current_field_var = self
            .loop_mode
            .hybrid_readonly_field_params
            .get(field)
            .cloned()
            .unwrap_or_else(|| field.to_string());
        let source = VersionedVar::new(VersionPrefix::Gensym(current_field_var), 0, frame);
        let new_field_var =
            self.fresh_temp_var(&format!("{}Field", Self::to_core_erlang_var(field)));
        self.loop_mode
            .hybrid_readonly_field_params
            .insert(field.to_string(), new_field_var.clone());
        stmts.push(ThreadedStmt::Statement(
            docvec!["let ", leaf::var(val_var.clone()), " = ", val_doc, " in "],
            span,
        ));
        stmts.push(ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::Gensym(new_field_var), 1, frame),
            source,
            op: BindOp::Direct(ValueRef::Var(val_var.clone())),
            span,
        });
        Ok(val_var)
    }

    /// ADR 0111 Addendum 5 §C2/§C3/§C4: lowers a `LocalAssignPure`/
    /// `LocalAssignTier2`/`LocalAssignControlFlow`/`LocalAssignSelfSend`
    /// conditional-branch statement to its real `Bind` sequence, appending
    /// it to `stmts` and returning the assigned value's temp var name. The
    /// mutation is a [`ThreadedStmt::Bind`] rather than a hand-rolled
    /// `maps:put` `Document` fragment. Unlike the top-level `BodyExprKind`
    /// shapes `gen_server/methods.rs` lowers (which route
    /// `LocalAssignSelfSend` through `generate_self_dispatch_parts`
    /// instead), the four kinds above ALL reach this one lowering — its own
    /// `is_tier2_value_call`/open-scope checks decide the actual shape at
    /// runtime, not `classify_body_expr`'s static kind.
    ///
    /// The one `StateAcc` local-write lowering: a conditional branch, a
    /// `StateAcc` loop body, a carried while condition (BT-4015, which
    /// deleted the `Document`-returning twin it was written to mirror) and
    /// `expressions.rs`'s `generate_block_stateful_body` (the Tier 2
    /// stateful-block-body local var assignment case) all call it.
    #[allow(clippy::too_many_lines)]
    pub(in crate::core_erlang) fn lower_local_var_assignment_bind(
        &mut self,
        expr: &Expression,
        frame: FrameId,
        span: Span,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<String> {
        let Expression::Assignment { target, value, .. } = expr else {
            unreachable!("LocalAssign* kinds must ensure expr is an Assignment");
        };
        let Expression::Identifier(id) = target.as_ref() else {
            unreachable!("LocalAssign* kinds must ensure assignment target is an Identifier");
        };

        let val_var = self.fresh_temp_var("Val");
        // In REPL mode, use the plain variable name as the key (no
        // __local__ prefix), so reads and writes agree on the key.
        let state_key = self.local_scratch_key(&id.name);

        // C2z — BT-3493: the RHS is itself a field write (`r := (self.x
        // := ...)`, at any parenthesization depth) — the shared ADR 0111
        // Addendum 5 statement classifier (`classify_body_expr`) sees only
        // the outer `var := <expr>` shape and hands every RHS here as
        // `LocalAssignPure`, so this is the "see through the value-carrying
        // parent" step the field write itself needs: lower it through the
        // SAME real-`Bind` producer C1 (`lower_field_assignment_bind`) uses
        // for a bare `self.field := ...` statement — arbitrary-RHS support
        // and `thread_ahead`-based sequencing all included — then alias this
        // statement's own local var to the identical assigned value, exactly
        // as `:=`'s "the whole assignment evaluates to the assigned value"
        // semantics require. Checked before C3/C3b/C4 below: a field write's
        // own target is `self.<field>`, which can never itself satisfy
        // `is_tier2_value_call`/`control_flow_has_mutations`/
        // `is_dispatching_actor_self_send` (all of which pattern-match a
        // `MessageSend`, not an `Assignment`), so this check cannot shadow
        // any of theirs.
        //
        // Claude Review follow-up on this PR: the field write's own `Bind`
        // chain (built by `lower_field_assignment_bind`, above) advances
        // `State`/`StateAcc` but says nothing about THIS statement's local
        // var — like every other branch below, `r` must also be `Put` into
        // the arm's own `StateAcc` under its `__local__` key, mirroring C2's
        // own `Put` immediately below. Without it, `seed_conditional_locals`'s
        // documented invariant (every taken branch overwrites the SEEDED key
        // for any local it mutates) breaks for exactly this shape: a local
        // declared before the conditional and reassigned via `r := (self.x
        // := ...)` inside one arm reads back the STALE pre-conditional
        // value once the branches merge and a later statement re-extracts
        // `r` via `maps:get` — silently wrong, not a crash, since the local's
        // in-branch lexical binding (`bind_var`) is real but never escapes
        // this arm's own `StateAcc`.
        if let Some(field_write) = self.local_assign_field_write(value) {
            let field_val_var =
                self.lower_field_assignment_bind(field_write, frame, span, stmts)?;
            self.push_local_slot_put(stmts, frame, &id.name, state_key, &field_val_var, span);
            self.bind_var(&id.name, &field_val_var);
            return Ok(field_val_var);
        }

        // C3 — Tier 2 block-call RHS returns {Result, NewStateAcc};
        // the sanctioned Gensym two-hop (ADR 0111 Addendum 5's "opaque
        // nested-construct state extraction feeding a maps:put" idiom).
        // In every frame, a value that is itself a local-threading producer
        // here is threaded ahead by C2 below instead, like a loop or fold
        // (BT-3823). In a method frame that never changes the verdict: an
        // evaluated block that is a producer there is not a Tier 2 call
        // (`is_tier2_value_call` defers to `block_value_producer_in_frame`). In
        // a REPL arm it routes the evaluated block to the REPL producer. Only
        // the value itself counts: a Tier 2 call whose *operand* is a producer
        // keeps this lowering, so the call's own `NewState` is never closed
        // away by C2.
        if self.is_tier2_value_call(value) && self.local_producer_locals(value).is_none() {
            let t2_tuple = self.fresh_temp_var("T2");
            let t2_state = self.fresh_temp_var("T2St");
            let value_code = self.generate_tier2_value_call_doc(value)?;
            let source_version = self.state_version();
            stmts.push(ThreadedStmt::Statement(
                docvec![
                    "let ",
                    leaf::var(t2_tuple.clone()),
                    " = ",
                    value_code,
                    " in let ",
                    leaf::var(val_var.clone()),
                    " = ",
                    form::element(1, leaf::var(t2_tuple.clone())),
                    " in ",
                ],
                span,
            ));
            let gensym_state = VersionedVar::new(VersionPrefix::Gensym(t2_state.clone()), 1, frame);
            stmts.push(ThreadedStmt::Bind {
                target: gensym_state.clone(),
                source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                op: BindOp::Direct(ValueRef::Doc(docvec![form::element(
                    2,
                    leaf::var(t2_tuple)
                ),])),
                span,
            });
            self.push_local_slot_put_from(
                stmts,
                frame,
                &id.name,
                state_key,
                &val_var,
                gensym_state,
                span,
            );
            self.bind_var(&id.name, &val_var);
            return Ok(val_var);
        }

        // C3b — RHS is itself control-flow-with-mutations (a
        // mutating list-op like `collect:`/`do:`/`select:` whose block —
        // or whose own receiver — needs state threading, or a nested
        // `ifTrue:ifFalse:`/`match:`/`on:do:` with field mutations).
        // Codegen for such an RHS (`expression_doc`) returns a closed
        // `{Value, StateAcc}` 2-tuple — the same shape C3's Tier 2 call
        // returns — which must be unwrapped exactly like C3, instead of
        // falling into C2 below (which assumes a single, non-tuple value
        // and would bind `val_var` to the raw tuple itself). Mirrors
        // `emit_actor_threaded_assign_rhs_stmts`, the top-level flat-body
        // counterpart of this same `LocalAssignControlFlow` RHS shape —
        // this body loop's C2 fallback previously had no equivalent,
        // silently leaking the tuple as the assigned value and the
        // branch's own result.
        //
        // ADR 0131 Phase 2b: not when the RHS is a loop or fold that is a
        // local-threading producer in this body — C2 below threads it ahead
        // (`thread_ahead` splices the construct tuple and its `LocalRebind`s,
        // lowered by this frame's mode) and reads its unwrapped value back.
        // BT-4000: nor an inline-threaded construct (a conditional, `match:`,
        // an exception or evaluated-block producer): `thread_ahead` splices its
        // prelude and registers its already-unwrapped value, as at the method
        // root (`emit_actor_threaded_assign_rhs_stmts`), so C2 reads it back.
        // Unwrapped a second time here, `element(1, ...)` of the value raised
        // `badarg` at run time.
        if self.control_flow_has_mutations(value)
            && !self.expr_has_local_producer(value)
            && !self.thread_ahead_unwraps(value)
        {
            let cf_tuple = self.fresh_temp_var("CfTuple");
            let cf_state = self.fresh_temp_var("CfSt");
            let thread_scope = self.thread_ahead(value, stmts, frame)?;
            let value_code = self.expression_doc(value)?;
            self.finish_precompiled_scope(thread_scope)?;
            let source_version = self.state_version();
            stmts.push(ThreadedStmt::Statement(
                docvec![
                    "let ",
                    leaf::var(cf_tuple.clone()),
                    " = ",
                    value_code,
                    " in let ",
                    leaf::var(val_var.clone()),
                    " = ",
                    form::element(1, leaf::var(cf_tuple.clone())),
                    " in ",
                ],
                span,
            ));
            let gensym_state = VersionedVar::new(VersionPrefix::Gensym(cf_state.clone()), 1, frame);
            stmts.push(ThreadedStmt::Bind {
                target: gensym_state.clone(),
                source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                op: BindOp::Direct(ValueRef::Doc(docvec![form::element(
                    2,
                    leaf::var(cf_tuple)
                ),])),
                span,
            });
            self.push_local_slot_put_from(
                stmts,
                frame,
                &id.name,
                state_key,
                &val_var,
                gensym_state,
                span,
            );
            self.bind_var(&id.name, &val_var);
            stmts.extend(self.construct_locals_in_place(value, span));
            return Ok(val_var);
        }

        // ADR 0118 phase 2a: as in `lower_field_assignment_bind`
        // — thread every order-safe self-send nested in the RHS (or the RHS
        // itself, `v := self bump`) as real `Bind`s before the RHS compiles
        // and before either `source_version` below is read. ADR 0118 phase
        // 5b: `thread_ahead`'s `subexpr_needs_prelude` now
        // recognizes a class-var producer too (a same-class self-send or
        // class-var assignment), so the former open-scope branch here (C4 —
        // already dead in practice per ADR 0111 Addendum 5, no
        // live program reached it) is now structurally unreachable: any
        // such producer is threaded ahead of this point, and the plain
        // compile below reads it back via `precompiled_subexprs`
        // substitution, exactly like the C3b branch above.
        let thread_scope = self.thread_ahead(value, stmts, frame)?;
        let value_code = self.expression_doc(value)?;
        self.finish_precompiled_scope(thread_scope)?;

        // C2 — the common case (plain / REPL-mode key).
        stmts.push(ThreadedStmt::Statement(
            docvec![
                "let ",
                leaf::var(val_var.clone()),
                " = ",
                value_code,
                " in "
            ],
            span,
        ));
        self.push_local_slot_put(stmts, frame, &id.name, state_key, &val_var, span);
        self.bind_var(&id.name, &val_var);
        Ok(val_var)
    }

    /// Whether an assignment to `local` in `frame` writes the local's slot in
    /// the frame's scratch map (its `__local__` key, or the binding in the
    /// REPL). ADR 0133 Phase 3 (BT-3911): in an arm frame ([`RebindFrameKind::BranchArm`])
    /// only a local the frame carries (`threads`) has a slot: the construct's
    /// result tuple hands back exactly those, and a local bound in the arm is
    /// a plain `let`. Every other frame keeps its slot write.
    fn local_has_scratch_slot(&self, frame: FrameId, local: &str) -> bool {
        if self.is_repl_mode() {
            return true;
        }
        if fault!(PutUncarriedLocal) {
            return true;
        }
        match self.rebind_frames.get(&frame) {
            Some(rebind) if rebind.kind == RebindFrameKind::BranchArm => {
                rebind.threads_local(local)
            }
            _ => true,
        }
    }

    /// The `Put` of `val_var` into `local`'s scratch slot that follows an
    /// assignment of it ([`Self::local_has_scratch_slot`]), stepping the
    /// frame's current `State` version; nothing for a local the frame does not
    /// carry.
    pub(super) fn push_local_slot_put(
        &mut self,
        stmts: &mut Vec<ThreadedStmt>,
        frame: FrameId,
        local: &str,
        state_key: String,
        val_var: &str,
        span: Span,
    ) {
        if !self.local_has_scratch_slot(frame, local) {
            return;
        }
        let source = VersionedVar::new(VersionPrefix::State, self.state_version(), frame);
        self.push_local_slot_put_from(stmts, frame, local, state_key, val_var, source, span);
    }

    /// [`Self::push_local_slot_put`] stepping from `source`: the `State`
    /// version a Tier 2 call's or construct's closed-over tuple handed back
    /// (a `Gensym` identity), which the frame's `State` chain must continue
    /// from whether or not `local` has a slot to write it into.
    #[allow(clippy::too_many_arguments)] // one argument per fact of the write
    fn push_local_slot_put_from(
        &mut self,
        stmts: &mut Vec<ThreadedStmt>,
        frame: FrameId,
        local: &str,
        state_key: String,
        val_var: &str,
        source: VersionedVar,
        span: Span,
    ) {
        let slot = self.local_has_scratch_slot(frame, local);
        let _ = self.next_state_var();
        let target_version = self.state_version();
        let target = VersionedVar::new(VersionPrefix::State, target_version, frame);
        let op = if slot {
            // BT-4461: the binding's variable now mirrors its map key.
            if self.is_repl_mode() {
                self.repl_map_vars.insert(val_var.to_string());
            }
            BindOp::Put {
                field: state_key,
                value: ValueRef::Var(val_var.to_string()),
            }
        } else {
            BindOp::Direct(ValueRef::Version(source.clone()))
        };
        stmts.push(ThreadedStmt::Bind {
            target,
            source,
            op,
            span,
        });
    }
}
