// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Boolean conditional compilation with field mutation state threading.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! When `ifTrue:`, `ifFalse:`, or `ifTrue:ifFalse:` is used inside an
//! actor method and the block argument(s) contain field mutations
//! (`self.slot :=`), the compiler generates inline case expressions
//! that thread actor state correctly through both branches.
//!
//! Without this, mutations inside `ifTrue:` blocks are lost because
//! the block executes as a Tier 1 closure: the mutated state is bound
//! as a local let-variable inside the closure but never returned to
//! the enclosing actor method's `handle_call`.
//!
//! # Generated Pattern
//!
//! `flag ifTrue: [self.count := self.count + 1]` generates:
//!
//! ```erlang
//! let _Cond1 = flag in
//! case _Cond1 of
//!   <'true'> when 'true' ->
//!     let StateAcc = State0 in
//!     let _Val1 = call 'erlang':'+'(call 'maps':'get'('count', StateAcc), 1) in
//!     let StateAcc1 = call 'maps':'put'('count', _Val1, StateAcc) in
//!     {_Val1, StateAcc1}
//!   <'false'> when 'true' ->
//!     {'nil', State0}
//!   <_CondNoMatch2> when 'true' ->
//!     call 'erlang':'error'({'case_clause', _CondNoMatch2})
//! end
//! ```
//!
//! The caller (method body generator) unpacks `{Result, NewState}` via
//! `element/2` to thread the new state to subsequent expressions.
//!
//! The trailing wildcard clause (BT-3161) is unreachable at runtime — `flag`
//! is always a genuine boolean here — but is required to make the `case`
//! *statically* exhaustive to the Core Erlang compiler; see
//! `CoreErlangGenerator::case_clause_fallback`'s doc comment for why an
//! implicit fallback isn't good enough when this `case` is nested inside a
//! `try`'s protected region (`on:do:`/`ensure:`).
//!
//! # State Naming
//!
//! Branch bodies use `StateAcc` / `StateAcc{N}` (loop-body naming) so
//! that inner state variable names do not conflict with the outer
//! `State{N}` chain managed by the method body generator.

use super::super::gen_server::BodyExprKind;
use super::super::threaded_ir::{
    self, BindOp, FrameId, ThreadedStmt, ThreadingMode, ValueRef, VersionPrefix, VersionedVar,
};
use super::super::{CodeGenError, CoreErlangGenerator, OpenScopeResult, Result};
use super::StateAccFallbackReason;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Block, Expression, MessageSelector};
use beamtalk_core::source_analysis::Span;

impl CoreErlangGenerator {
    /// BT-2355: Seeds the `__local__` keys for the outer locals a conditional's
    /// branches thread, returning `(seed_doc, base_state_var)`.
    ///
    /// Each branch — including the synthetic non-taken branch (`{'nil', State}`)
    /// and any branch that does not itself write a given local — must return a
    /// state map that already contains every threaded `__local__` key, so the
    /// method-body sequencer's `maps:get/2` extraction never hits a missing key.
    /// Seeding the keys from the locals' current (pre-conditional) bindings before
    /// the `case` makes the base state self-consistent; taken branches simply
    /// overwrite the seeded value via `maps:put`.
    ///
    /// When there are no threaded locals, returns `(Document::Nil, outer_state)`
    /// so field-only conditionals emit byte-for-byte the same code as before.
    ///
    /// BT-3160: also reused by `exception_handling.rs`'s `on:do:`/`ensure:`
    /// generators — a try (receiver) block and its handler/cleanup block(s) are
    /// the same "compiled-but-not-all-taken" shape as a conditional's branches,
    /// so they share this seeding helper rather than re-deriving it.
    pub(in crate::core_erlang::control_flow) fn seed_conditional_locals(
        &mut self,
        blocks: &[&Block],
        outer_state: &str,
    ) -> (Document<'static>, String) {
        let threaded = self.conditional_threaded_locals(blocks);
        if threaded.is_empty() {
            return (Document::Nil, outer_state.to_string());
        }

        let seeded_state = self.fresh_temp_var("SeededState");
        let mut chain: Document<'static> = leaf::var(outer_state.to_string());
        // Fold from the last var inward so the emitted nesting reads naturally.
        for var in threaded.iter().rev() {
            let core_var = self
                .lookup_var(var)
                .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
            chain = docvec![
                "call 'maps':'put'(",
                leaf::atom(Self::local_state_key(var)),
                ", ",
                leaf::var(core_var),
                ", ",
                chain,
                ")",
            ];
        }
        let seed_doc = docvec![
            "let ",
            leaf::var(seeded_state.clone()),
            " = ",
            chain,
            " in "
        ];
        (seed_doc, seeded_state)
    }

    /// BT-1942/BT-3382: compiles a conditional/`ifNotNil:` receiver, returning
    /// `(preamble, value_doc)` — `preamble` is any open let-chain that must be
    /// emitted BEFORE the `case`'s condition binding (so a mutated binding
    /// like `ClassVarsN` or an actor self-send's new `State`/`StateAcc` stays
    /// in scope inside the `case`), and `value_doc` is the receiver's own
    /// boolean/nil value to test. Shared by all four `generate_if_*_with_mutations`
    /// generators below (previously each hand-duplicated this exact match —
    /// CLAUDE.md's no-duplicate-implementations rule).
    ///
    /// Two receiver shapes thread state through this position:
    /// - BT-1942: a class-method self-send (or sub-expression containing one)
    ///   emits its class-var mutation as an open let-chain, surfaced generically
    ///   via `expression_doc_with_open_scope`.
    /// - BT-3382: an ACTOR-INSTANCE self-send used directly as the receiver
    ///   (optionally parenthesized) — e.g. `(self recordOnce: which)
    ///   ifTrue:ifFalse:` — needs the same treatment, special-cased here
    ///   directly via `generate_self_dispatch_open_for` rather than through
    ///   `expression_doc_with_open_scope`'s generic side-channel:
    ///   `generate_self_dispatch` (the codegen for an ordinary actor
    ///   self-send *sub-expression*, e.g. a self-send nested inside some
    ///   other operator or nested one level deeper inside a block body)
    ///   deliberately still discards its callee's state — publishing it
    ///   through the shared `last_open_scope_result` side-channel
    ///   unconditionally would silently break any OTHER call site that reads
    ///   `state_version()`/`current_state_var()` before compiling a value
    ///   expression and assumes computing that expression cannot itself
    ///   advance the counter (confirmed by a real regression against
    ///   `self.log := self.log ++ #(self getValue)` while prototyping the
    ///   general fix — a field assignment's own `Bind` source was captured
    ///   before its RHS ran). This ONE receiver position is safe to
    ///   special-case because the surrounding code
    ///   (`self.current_state_var()` read immediately after, and every
    ///   branch keyed off `StateAcc`) is already written to expect a
    ///   correctly-threaded state exactly here. See BT-3382 Linear follow-up
    ///   issues for widening this to other self-send-as-sub-expression shapes
    ///   (nested inside a block body, `and:`/`or:` receivers, arguments).
    fn compile_conditional_receiver(
        &mut self,
        receiver: &Expression,
    ) -> Result<(Document<'static>, Document<'static>)> {
        let unwrapped = receiver.unwrap_parens();
        if self.is_dispatching_actor_self_send(unwrapped) {
            if let Expression::MessageSend {
                selector,
                arguments,
                ..
            } = unwrapped
            {
                let (open_doc, dispatch_var) =
                    self.generate_self_dispatch_open_for(selector, arguments)?;
                let result_var = self.fresh_temp_var("CondSelfResult");
                let preamble = docvec![
                    open_doc,
                    "let ",
                    leaf::var(result_var.clone()),
                    " = call 'erlang':'element'(1, ",
                    leaf::var(dispatch_var),
                    ") in ",
                ];
                return Ok((preamble, leaf::var(result_var)));
            }
        }
        let (cond_chain, cond_open_scope) = self.expression_doc_with_open_scope(receiver)?;
        Ok(match cond_open_scope {
            Some(OpenScopeResult::Value(result_var)) => (cond_chain, leaf::var(result_var)),
            // BT-3053: no single value — substitute do:'s own `nil` contract.
            Some(OpenScopeResult::NoValue) => (cond_chain, Document::Str("'nil'")),
            None => (Document::Nil, cond_chain),
        })
    }

    /// BT-3392: recursively dispatches every actor self-send nested as an
    /// operand of a binary-operator chain within `expr` — e.g. both `self
    /// a`s in `(self a) + ((self a) * 2)` — via a real `ThreadedStmt::Bind`
    /// (`dispatch_self_send_as_bind`, the same mechanism C11/C12b above use),
    /// in left-to-right evaluation order, *before* the C12 catch-all in
    /// `generate_conditional_branch_inline` compiles this statement.
    /// Registers each dispatch's result variable in
    /// `hoisted_self_send_results` (keyed by the self-send's own receiver
    /// span) so `try_handle_self_dispatch` substitutes a reference to it —
    /// instead of dispatching (and re-running the method) a second time —
    /// the moment `expression_doc` reaches that same node.
    ///
    /// Deliberately narrow: only unwraps parens and recurses through
    /// `MessageSelector::Binary` sends (`+`, `-`, `*`, `/`, `<`, `==`, …) —
    /// this is the literal shape BT-3392 confirmed still broken (`1 + (self
    /// recordOnce: flag)` inside an `ifTrue:` block). It does NOT recurse
    /// into keyword-message receivers/arguments, block bodies, or
    /// field-assignment RHS trees — `lower_field_assignment_bind` (a
    /// completely separate call path, C1 above, never reached from here)
    /// is exactly the call site whose pre-captured `source_version` a fully
    /// general "thread every self-dispatch sub-expression" version of this
    /// mechanism broke during BT-3382's own prototyping (see
    /// `hoisted_self_send_results`'s doc comment in `mod.rs`) — staying out
    /// of that tree entirely, rather than trying to special-case it, is
    /// what keeps this addition safe.
    ///
    /// A hoisted self-send's `Bind` always lands in `stmts` ahead of the
    /// full `expression_doc(expr)` compile at the call site — so it always
    /// *executes* first, regardless of where in the source tree it sits.
    /// That's fine among self-sends themselves (this function visits them
    /// left-to-right, same as their natural evaluation order, and a raise
    /// from an earlier one still short-circuits the later `Bind`s via
    /// ordinary Core Erlang `let`-chain semantics) — but it's wrong the
    /// moment a NON-hoisted operand precedes a self-send in evaluation
    /// order and that operand could itself raise or have an effect: e.g.
    /// `(self.items at: idx) + (self bumpCount)` must not run `bumpCount`'s
    /// mutation before `at:` has had a chance to fail. `safe_to_hoist`
    /// tracks this left-to-right: it flips to `false` the first time this
    /// traversal visits an operand that is neither a self-send nor
    /// provably `is_effect_free_operand` (a literal, plain identifier,
    /// class reference, or field read), and every self-send visited after
    /// that point is left un-hoisted — its mutation stays dropped, same as
    /// every other shape this issue leaves out of scope, rather than being
    /// reordered ahead of an operand it must not run before.
    fn hoist_self_sends_for_binary_op(
        &mut self,
        expr: &Expression,
        frame: FrameId,
        span: Span,
        stmts: &mut Vec<ThreadedStmt>,
        safe_to_hoist: &mut bool,
    ) -> Result<()> {
        let expr = expr.unwrap_parens();
        if self.is_dispatching_actor_self_send(expr) {
            if *safe_to_hoist {
                if let Expression::MessageSend { receiver, .. } = expr {
                    let receiver_span = receiver.span();
                    let dispatch_var =
                        self.dispatch_self_send_as_bind(expr, frame, span, stmts)?;
                    self.hoisted_self_send_results
                        .insert(receiver_span, dispatch_var);
                }
            }
            return Ok(());
        }
        if let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = expr
        {
            if matches!(selector, MessageSelector::Binary(_)) {
                self.hoist_self_sends_for_binary_op(receiver, frame, span, stmts, safe_to_hoist)?;
                if let Some(arg) = arguments.first() {
                    self.hoist_self_sends_for_binary_op(arg, frame, span, stmts, safe_to_hoist)?;
                }
                return Ok(());
            }
        }
        if !self.is_effect_free_operand(expr) {
            *safe_to_hoist = false;
        }
        Ok(())
    }

    /// BT-3392: true for an operand that provably cannot raise or have a
    /// side effect, so its relative evaluation order against a hoisted
    /// self-send doesn't matter — a literal, a plain local/parameter
    /// identifier, a class reference, `super`, or a direct field read
    /// (recursing into its own receiver, which is normally `self`/`super`).
    /// Anything else — in particular any message send, including a
    /// non-self-send one that could itself raise (`anArray at: idx`,
    /// `respondsTo:`, …) — is conservatively NOT effect-free. See
    /// `hoist_self_sends_for_binary_op`'s doc comment for why this matters.
    fn is_effect_free_operand(&self, expr: &Expression) -> bool {
        match expr.unwrap_parens() {
            Expression::Literal(_, _)
            | Expression::Identifier(_)
            | Expression::ClassReference { .. }
            | Expression::Super(_) => true,
            Expression::FieldAccess { receiver, .. } => self.is_effect_free_operand(receiver),
            _ => false,
        }
    }

    /// Generates inline code for `flag ifTrue: [block]` in actor context
    /// when the block contains field mutations.
    ///
    /// Returns `{Result, NewState}`:
    /// - True branch: `{block_result, mutated_state}`
    /// - False branch: `{'nil', unchanged_state}`
    pub(in crate::core_erlang) fn generate_if_true_with_mutations(
        &mut self,
        receiver: &Expression,
        block: &Block,
    ) -> Result<Document<'static>> {
        let (cond_preamble, cond_val_doc) = self.compile_conditional_receiver(receiver)?;
        let cond_var = self.fresh_temp_var("Cond");
        let outer_state = self.current_state_var();
        // BT-2355: seed threaded outer-locals so the non-taken (false) branch and
        // the post-conditional extraction always see the `__local__` keys.
        let (seed_doc, base_state) = self.seed_conditional_locals(&[block], &outer_state);

        // ADR 0111 Addendum 5 (BT-3146): `generate_conditional_branch_inline`
        // now builds, `verify()`s, and `render()`s this arm's real per-frame
        // `ThreadedIr` internally — the pre-migration
        // `check_branch_frame_linearity` scalar-synthesis scaffolding check
        // that used to run here is gone; `NonLinearVersion`/`UnboundVersion`
        // are live checks against the real IR now.
        let (branch_doc, _branch_final) =
            self.with_branch_context(|this| this.generate_conditional_branch_inline(block))?;
        // BT-3161: explicit wildcard so this boolean `case` is statically
        // exhaustive — see `case_clause_fallback`'s doc comment.
        let no_match_fallback = self.case_clause_fallback("CondNoMatch");

        Ok(docvec![
            cond_preamble,
            seed_doc,
            "let ",
            leaf::var(cond_var.clone()),
            " = ",
            cond_val_doc,
            " in case ",
            leaf::var(cond_var),
            " of <'true'> when 'true' -> let StateAcc = ",
            leaf::var(base_state.clone()),
            " in ",
            branch_doc,
            " <'false'> when 'true' -> {'nil', ",
            leaf::var(base_state),
            "}",
            no_match_fallback,
            " end",
        ])
    }

    /// Generates inline code for `flag ifFalse: [block]` in actor context
    /// when the block contains field mutations.
    ///
    /// Returns `{Result, NewState}`:
    /// - True branch: `{'nil', unchanged_state}`
    /// - False branch: `{block_result, mutated_state}`
    pub(in crate::core_erlang) fn generate_if_false_with_mutations(
        &mut self,
        receiver: &Expression,
        block: &Block,
    ) -> Result<Document<'static>> {
        let (cond_preamble, cond_val_doc) = self.compile_conditional_receiver(receiver)?;
        let cond_var = self.fresh_temp_var("Cond");
        let outer_state = self.current_state_var();
        // BT-2355: seed threaded outer-locals so the non-taken (true) branch and
        // the post-conditional extraction always see the `__local__` keys.
        let (seed_doc, base_state) = self.seed_conditional_locals(&[block], &outer_state);

        // ADR 0111 Addendum 5 (BT-3146): see generate_if_true_with_mutations'
        // matching comment — real per-frame verify() now runs inside
        // generate_conditional_branch_inline itself.
        let (branch_doc, _branch_final) =
            self.with_branch_context(|this| this.generate_conditional_branch_inline(block))?;
        // BT-3161: explicit wildcard so this boolean `case` is statically
        // exhaustive — see `case_clause_fallback`'s doc comment.
        let no_match_fallback = self.case_clause_fallback("CondNoMatch");

        Ok(docvec![
            cond_preamble,
            seed_doc,
            "let ",
            leaf::var(cond_var.clone()),
            " = ",
            cond_val_doc,
            " in case ",
            leaf::var(cond_var),
            " of <'true'> when 'true' -> {'nil', ",
            leaf::var(base_state.clone()),
            "} <'false'> when 'true' -> let StateAcc = ",
            leaf::var(base_state),
            " in ",
            branch_doc,
            no_match_fallback,
            " end",
        ])
    }

    /// Generates inline code for `flag ifTrue: [t_block] ifFalse: [f_block]` in actor context
    /// when at least one block contains field mutations.
    ///
    /// Returns `{Result, NewState}` from whichever branch is taken.
    pub(in crate::core_erlang) fn generate_if_true_if_false_with_mutations(
        &mut self,
        receiver: &Expression,
        true_block: &Block,
        false_block: &Block,
    ) -> Result<Document<'static>> {
        let (cond_preamble, cond_val_doc) = self.compile_conditional_receiver(receiver)?;
        let cond_var = self.fresh_temp_var("Cond");
        let outer_state = self.current_state_var();
        // BT-2355: seed threaded outer-locals so a branch that does not itself
        // write a given local (and the post-conditional extraction) still sees the
        // `__local__` key.
        let (seed_doc, base_state) =
            self.seed_conditional_locals(&[true_block, false_block], &outer_state);

        // True branch
        let (true_branch_doc, _true_final) =
            self.with_branch_context(|this| this.generate_conditional_branch_inline(true_block))?;

        // False branch (reset to same initial state)
        let (false_branch_doc, _false_final) =
            self.with_branch_context(|this| this.generate_conditional_branch_inline(false_block))?;

        // ADR 0111 Addendum 5 (BT-3146): the true/false arms are sibling
        // with_branch_context frames — each `generate_conditional_branch_inline`
        // call mints its own fresh FrameId (`current_branch_frame`) and
        // `verify()`s its own real IR internally, so either arm
        // independently reaching the same version number as the other
        // (e.g. both perform exactly one field mutation, both producing
        // "StateAcc1" in their own frame) is correctly NOT a
        // NonLinearVersion violation — the check that used to run here
        // (`check_branch_frame_linearity`) is gone; real per-frame
        // verification now happens where the IR is actually built.
        // BT-3161: explicit wildcard so this boolean `case` is statically
        // exhaustive — see `case_clause_fallback`'s doc comment.
        let no_match_fallback = self.case_clause_fallback("CondNoMatch");

        Ok(docvec![
            cond_preamble,
            seed_doc,
            "let ",
            leaf::var(cond_var.clone()),
            " = ",
            cond_val_doc,
            " in case ",
            leaf::var(cond_var),
            " of <'true'> when 'true' -> let StateAcc = ",
            leaf::var(base_state.clone()),
            " in ",
            true_branch_doc,
            " <'false'> when 'true' -> let StateAcc = ",
            leaf::var(base_state),
            " in ",
            false_branch_doc,
            no_match_fallback,
            " end",
        ])
    }

    /// Generates inline code for `obj ifNotNil: [block]` or `obj ifNotNil: [:v | block]`
    /// in actor context when the block contains field mutations.
    ///
    /// Returns `{Result, NewState}`:
    /// - Nil branch: `{'nil', unchanged_state}`
    /// - Non-nil branch: `{block_result, mutated_state}`
    ///
    /// If the block has a parameter (`:v`), it is bound to the receiver object value
    /// inside the branch body.
    pub(in crate::core_erlang) fn generate_if_not_nil_with_mutations(
        &mut self,
        receiver: &Expression,
        block: &Block,
    ) -> Result<Document<'static>> {
        let (recv_preamble, recv_val_doc) = self.compile_conditional_receiver(receiver)?;
        let obj_var = self.fresh_temp_var("Obj");
        let outer_state = self.current_state_var();
        // BT-2355: seed threaded outer-locals so the non-taken (nil) branch and the
        // post-conditional extraction always see the `__local__` keys.
        let (seed_doc, base_state) = self.seed_conditional_locals(&[block], &outer_state);

        // ADR 0111 Addendum 5 (BT-3146): see generate_if_true_with_mutations'
        // matching comment — real per-frame verify() now runs inside
        // generate_conditional_branch_inline itself.
        let (branch_doc, _branch_final) = self.with_branch_context(|this| {
            // Push a scope so the block-parameter binding is cleaned up after generation
            this.push_scope();
            if let Some(param) = block.parameters.first() {
                // Bind the block parameter to the receiver value (already bound to obj_var)
                this.bind_var(&param.name, &obj_var);
            }
            let result = this.generate_conditional_branch_inline(block);
            this.pop_scope();
            result
        })?;

        Ok(docvec![
            recv_preamble,
            seed_doc,
            "let ",
            leaf::var(obj_var.clone()),
            " = ",
            recv_val_doc,
            " in case ",
            leaf::var(obj_var),
            " of <'nil'> when 'true' -> {'nil', ",
            leaf::var(base_state.clone()),
            "} <_> when 'true' -> let StateAcc = ",
            leaf::var(base_state),
            " in ",
            branch_doc,
            " end",
        ])
    }

    /// ADR 0111 Addendum 5 §C1: lowers a `self.field := value`
    /// conditional-branch/block-body statement to its real `Bind` sequence,
    /// appending it to `stmts` and returning the assigned value's temp var
    /// name. Mirrors `generate_field_assignment_open`'s normal (non
    /// hybrid-full-extract) branch exactly — same helper calls, same mint
    /// order — but models the state mutation as a [`ThreadedStmt::Bind`]
    /// instead of a hand-rolled `maps:put` `Document` fragment.
    ///
    /// `generate_field_assignment_open`'s hybrid full-extract sub-branch
    /// (`in_hybrid_loop && hybrid_mutated_fields.contains(field)`) produces
    /// no state-version step at all — it rebinds a direct fun parameter
    /// instead of `maps:put`-ing into `StateAcc` — so it is delegated to
    /// unchanged and modeled as a single opaque `Statement`, exactly as it
    /// already renders. §Scope (BT-3146): a mutation-carrying conditional
    /// forces `StateAcc` fallback, mutually exclusive with hybrid mode, so
    /// this branch is unreached from `generate_conditional_branch_inline`;
    /// BT-3149 reuses this helper from `expressions.rs`'s
    /// `generate_block_stateful_body`, where the exclusion is less directly
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
        // §Scope: class-var mutations never legitimately reach these arms —
        // shares `generate_field_assignment_open`'s rejection via
        // `reject_class_var_field_assignment` (util.rs) so the two call
        // sites can't drift out of sync.
        self.reject_class_var_field_assignment(expr, field)?;

        if self.in_hybrid_loop && self.hybrid_mutated_fields.contains(field.name.as_str()) {
            let (doc, val_var) = self.generate_field_assignment_open(expr)?;
            stmts.push(ThreadedStmt::Statement(doc, span));
            return Ok(val_var);
        }

        let val_var = self.fresh_temp_var("Val");
        let source_version = self.state_version();
        let value_str = self.generate_field_assignment_value_doc(value)?;
        stmts.push(ThreadedStmt::Statement(
            docvec!["let ", leaf::var(val_var.clone()), " = ", value_str, " in ",],
            span,
        ));
        let _ = self.next_state_var();
        let target_version = self.state_version();
        stmts.push(ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::State, target_version, frame),
            source: VersionedVar::new(VersionPrefix::State, source_version, frame),
            op: BindOp::Put {
                field: field.name.to_string(),
                value: ValueRef::Var(val_var.clone()),
                class_tag: ValueRef::Literal("'nil'"),
            },
            shadow_write: false,
            span,
        });
        Ok(val_var)
    }

    /// ADR 0111 Addendum 5 §C2/§C3/§C4: lowers a `LocalAssignPure`/
    /// `LocalAssignTier2`/`LocalAssignControlFlow`/`LocalAssignSelfSend`
    /// conditional-branch statement to its real `Bind` sequence, appending
    /// it to `stmts` and returning the assigned value's temp var name.
    /// Mirrors `generate_local_var_assignment_in_loop`'s three sub-branches
    /// (BT-153/BT-912/BT-1397) exactly — same helper calls, same mint
    /// order — but models the mutation as a [`ThreadedStmt::Bind`] instead
    /// of a hand-rolled `maps:put` `Document` fragment. Unlike the
    /// top-level `BodyExprKind` shapes `gen_server/methods.rs` lowers
    /// (which route `LocalAssignSelfSend` through
    /// `generate_self_dispatch_open` instead), the four kinds above ALL
    /// reach `generate_local_var_assignment_in_loop` in this body loop —
    /// its own `is_tier2_value_call`/open-scope checks decide the actual
    /// shape at runtime, not `classify_body_expr`'s static kind.
    ///
    /// BT-3149 also calls this directly from `expressions.rs`'s
    /// `generate_block_stateful_body` (the Tier 2 stateful-block-body local
    /// var assignment case) — same shape, same mint order, one fewer
    /// hand-rolled duplicate.
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
        // BT-790: In REPL mode, use the plain variable name as the key (no
        // __local__ prefix) — see `generate_local_var_assignment_in_loop`.
        let state_key: String = if self.is_repl_mode() {
            id.name.to_string()
        } else {
            Self::local_state_key(&id.name)
        };

        // C3 — BT-912: Tier 2 block-call RHS returns {Result, NewStateAcc};
        // the sanctioned Gensym two-hop (ADR 0111 Addendum 5's "opaque
        // nested-construct state extraction feeding a maps:put" idiom).
        if self.is_tier2_value_call(value) {
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
                    " = call 'erlang':'element'(1, ",
                    leaf::var(t2_tuple.clone()),
                    ") in ",
                ],
                span,
            ));
            let gensym_state = VersionedVar::new(VersionPrefix::Gensym(t2_state.clone()), 1, frame);
            stmts.push(ThreadedStmt::Bind {
                target: gensym_state.clone(),
                source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                op: BindOp::Direct(ValueRef::Doc(docvec![
                    "call 'erlang':'element'(2, ",
                    leaf::var(t2_tuple),
                    ")",
                ])),
                shadow_write: false,
                span,
            });
            let _ = self.next_state_var();
            let target_version = self.state_version();
            stmts.push(ThreadedStmt::Bind {
                target: VersionedVar::new(VersionPrefix::State, target_version, frame),
                source: gensym_state,
                op: BindOp::Put {
                    field: state_key,
                    value: ValueRef::Var(val_var.clone()),
                    // Unused placeholder: only rendered when shadow_write is
                    // true, which only class-var Puts ever set (ADR 0110) —
                    // never reachable here (§Scope: class-var mutations
                    // never route through these arms).
                    class_tag: ValueRef::Literal("'nil'"),
                },
                shadow_write: false,
                span,
            });
            self.bind_var(&id.name, &val_var);
            return Ok(val_var);
        }

        let (value_code, open_scope) = self.expression_doc_with_open_scope(value)?;

        if let Some(open_scope_result) = open_scope {
            // C4 (BT-1397) — ADR 0111 Addendum 5 found no compilable repro
            // reaching this sub-branch through this body loop: a
            // class-method self-send routes through
            // `value_type_codegen.rs`'s vt-conditional path instead
            // (§Scope, §C4). The shape decomposes with the exact same
            // idiom as the plain case below (a value-temp Statement, then
            // a real mutation Bind) — modeled here for completeness rather
            // than left opaque, even though no live program exercises it
            // today.
            let open_scope_value_doc = match open_scope_result {
                OpenScopeResult::Value(v) => leaf::var(v),
                OpenScopeResult::NoValue => Document::Str("'nil'"),
            };
            stmts.push(ThreadedStmt::Statement(value_code, span));
            stmts.push(ThreadedStmt::Statement(
                docvec![
                    "let ",
                    leaf::var(val_var.clone()),
                    " = ",
                    open_scope_value_doc,
                    " in ",
                ],
                span,
            ));
            let source_version = self.state_version();
            let current_state_name = self.current_state_var();
            let _ = self.next_state_var();
            let target_version = self.state_version();
            stmts.push(ThreadedStmt::Bind {
                target: VersionedVar::new(VersionPrefix::State, target_version, frame),
                source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                op: BindOp::Direct(ValueRef::Doc(docvec![
                    "call 'maps':'put'(",
                    leaf::atom(state_key),
                    ", ",
                    leaf::var(val_var.clone()),
                    ", ",
                    leaf::var(current_state_name),
                    ")",
                ])),
                shadow_write: false,
                span,
            });
            self.bind_var(&id.name, &val_var);
            return Ok(val_var);
        }

        // C2 — the common case (plain / REPL-mode key).
        let source_version = self.state_version();
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
        let _ = self.next_state_var();
        let target_version = self.state_version();
        stmts.push(ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::State, target_version, frame),
            source: VersionedVar::new(VersionPrefix::State, source_version, frame),
            op: BindOp::Put {
                field: state_key,
                value: ValueRef::Var(val_var.clone()),
                class_tag: ValueRef::Literal("'nil'"),
            },
            shadow_write: false,
            span,
        });
        self.bind_var(&id.name, &val_var);
        Ok(val_var)
    }

    /// Dispatches a self-send (`expr`) and `Bind`s its returned `NewState` as
    /// this branch's own next real `State` version — shared by the C12b
    /// (`DispatchingSelfSend`) and C0b (BT-3374's nested `^self otherMethod`)
    /// arms below, both of which dispatch a self-send mid-branch and must
    /// thread its `NewState` forward instead of discarding it (mirroring C11
    /// `ControlFlowWithMutations`'s tuple-unpack `Bind`).
    fn dispatch_self_send_as_bind(
        &mut self,
        expr: &Expression,
        frame: FrameId,
        span: Span,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<String> {
        let source_version = self.state_version();
        let (call_doc, dispatch_var) = self.generate_self_dispatch_call_doc(expr)?;
        stmts.push(ThreadedStmt::Statement(call_doc, span));
        let target_version = self.state_version();
        stmts.push(ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::State, target_version, frame),
            source: VersionedVar::new(VersionPrefix::State, source_version, frame),
            op: BindOp::Direct(ValueRef::Doc(docvec![
                "call 'erlang':'element'(2, ",
                leaf::var(dispatch_var.clone()),
                ")",
            ])),
            shadow_write: false,
            span,
        });
        Ok(dispatch_var)
    }

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
    /// ADR 0111 Addendum 5 (BT-3146): this arm's mutation sequence is built
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
    #[allow(clippy::too_many_lines)]
    pub(in crate::core_erlang) fn generate_conditional_branch_inline(
        &mut self,
        block: &Block,
    ) -> Result<(Document<'static>, usize)> {
        let frame = self.current_branch_frame();
        self.push_scope();

        let body = super::super::util::collect_body_exprs(&block.body);

        // Empty block returns nil with unchanged state (C13).
        if body.is_empty() {
            let final_version = self.state_version();
            self.pop_scope();
            return Ok(self.verify_and_render_branch_arm(
                vec![ThreadedStmt::Return(
                    ValueRef::Literal("'nil'"),
                    VersionedVar::new(VersionPrefix::State, final_version, frame),
                    block.span,
                )],
                frame,
                final_version,
                block.span,
            ));
        }

        // Classify every expression upfront using the shared classifier (BT-1447).
        let plan: Vec<BodyExprKind> = body.iter().map(|e| self.classify_body_expr(e)).collect();

        let mut stmts: Vec<ThreadedStmt> = Vec::new();
        let mut last_result: Option<ValueRef> = None;

        for (i, (expr, kind)) in body.iter().zip(plan.into_iter()).enumerate() {
            let is_last = i == body.len() - 1;
            let span = expr.span();

            // C0b — BT-3374: `^self otherMethod` (early return whose value is
            // a dispatching actor self-send, e.g. `ifTrue: [^self
            // configureVictim]`). `classify_body_expr` always classifies the
            // outer `Return` as `EarlyReturn` (never `DispatchingSelfSend` —
            // that classification only applies to a bare, un-returned
            // self-send), so without this check the statement below falls to
            // the C12 catch-all, which renders the whole `Return` as one
            // opaque `expression_doc` blob via the generic AST-directed
            // `Expression::Return` handler. That handler reaches
            // `generate_self_dispatch`'s *closed* form for a plain self-send
            // value (`last_open_scope_result` is only populated by
            // class-method self-sends/class-var assignments, per its own doc
            // comment) — which computes the call's `Result` but drops its
            // `NewState`, so the NLR throw's 4-tuple carries this branch's
            // pre-call `StateAcc` instead of the mutation the self-send just
            // made. The call still runs; its effects vanish the instant this
            // `^` unwinds. Confirmed by `ThreadedIr::verify()` itself: the
            // opaque blob's raw text and this loop's own version bookkeeping
            // silently diverge (`report_threaded_ir_verify_errors` panics
            // debug-fatal on this exact shape).
            //
            // Mirrors C12b's `DispatchingSelfSend` Bind pattern immediately
            // below: dispatch via `generate_self_dispatch_call_doc`, Bind the
            // arm's next real `State` version from the call's `NewState`
            // element, then throw the NLR tuple against *that* version
            // instead of the branch's stale entry state — the same
            // `generate_self_dispatch_open`-vs-closed distinction
            // `lower_body_exprs_with_reply`'s own `DispatchingSelfSend` arm
            // already applies for the top-level (unnested) `^self foo` shape
            // (BT-1432) — just expressed as this loop's own `Bind`+`Statement`
            // pair rather than that flat body lowering's open let-chain, since
            // this arm's `ThreadedIr` wrapper needs the version bump to be a
            // real, verifiable production, not text buried in an opaque `Doc`.
            // BT-3374: excludes class methods (`in_class_method()`) for the
            // same reason `mod.rs`'s `Expression::Return` handler does —
            // `generate_self_dispatch_call_doc` unconditionally threads
            // `current_state_var()` (Actor instance state), never
            // `current_class_var()` (ADR 0110's ClassVars mechanism a class
            // method actually needs) — so a class-method self-send here
            // falls to the C12 catch-all below instead, unchanged from
            // before this fix.
            if let BodyExprKind::EarlyReturn = kind {
                if let Expression::Return { value, .. } = expr {
                    if !self.in_class_method()
                        && matches!(
                            self.classify_body_expr(value),
                            BodyExprKind::DispatchingSelfSend
                        )
                    {
                        let dispatch_var =
                            self.dispatch_self_send_as_bind(value, frame, span, &mut stmts)?;
                        let nlr_token = self.current_nlr_token().cloned().ok_or_else(|| {
                            CodeGenError::Internal(
                                "BT-3374: EarlyReturn classification implies an active NLR \
                                 context, but none is set"
                                    .to_string(),
                            )
                        })?;
                        let new_state = self.current_state_var();
                        let throw_var = self.fresh_temp_var("NlrThrow");
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let ",
                                leaf::var(throw_var.clone()),
                                " = call 'erlang':'throw'({'$bt_nlr', ",
                                leaf::var(nlr_token),
                                ", call 'erlang':'element'(1, ",
                                leaf::var(dispatch_var),
                                "), ",
                                leaf::var(new_state),
                                "}) in ",
                            ],
                            span,
                        ));
                        if is_last {
                            last_result = Some(ValueRef::Var(throw_var));
                        }
                        continue;
                    }
                }
            }

            match kind {
                // C1
                BodyExprKind::FieldAssignment => {
                    let val_var =
                        self.lower_field_assignment_bind(expr, frame, span, &mut stmts)?;
                    if is_last {
                        // BT-884: val_var holds the assigned value variable
                        last_result = Some(ValueRef::Var(val_var));
                    }
                }
                // C2/C3/C4
                BodyExprKind::LocalAssignPure
                | BodyExprKind::LocalAssignTier2
                | BodyExprKind::LocalAssignControlFlow
                | BodyExprKind::LocalAssignSelfSend => {
                    let val_var =
                        self.lower_local_var_assignment_bind(expr, frame, span, &mut stmts)?;
                    if is_last {
                        last_result = Some(ValueRef::Var(val_var));
                    }
                }
                // C5 — exempt from Bind modeling: no state version is
                // produced or consumed, every binding is a plain local.
                BodyExprKind::DestructureAssignment => {
                    if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                        let binding_docs = self.generate_destructure_bindings(pattern, value)?;
                        for d in binding_docs {
                            stmts.push(ThreadedStmt::Statement(d, span));
                        }
                    }
                }
                // C6 — BT-1477: self.field := <control-flow-with-mutations>.
                // The sanctioned Gensym two-hop.
                BodyExprKind::FieldAssignmentControlFlow => {
                    if let Expression::Assignment { target, value, .. } = expr {
                        if let Expression::FieldAccess { field, .. } = target.as_ref() {
                            let tuple_var = self.fresh_temp_var("CfTuple");
                            let val_var = self.fresh_temp_var("CfVal");
                            let rhs_doc = self.expression_doc(value)?;
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
                                    " = call 'erlang':'element'(1, ",
                                    leaf::var(tuple_var.clone()),
                                    ") in ",
                                ],
                                span,
                            ));
                            let gensym_state =
                                VersionedVar::new(VersionPrefix::Gensym(rhs_state), 1, frame);
                            stmts.push(ThreadedStmt::Bind {
                                target: gensym_state.clone(),
                                source: VersionedVar::new(
                                    VersionPrefix::State,
                                    source_version,
                                    frame,
                                ),
                                op: BindOp::Direct(ValueRef::Doc(docvec![
                                    "call 'erlang':'element'(2, ",
                                    leaf::var(tuple_var),
                                    ")",
                                ])),
                                shadow_write: false,
                                span,
                            });
                            let _ = self.next_state_var();
                            let target_version = self.state_version();
                            stmts.push(ThreadedStmt::Bind {
                                target: VersionedVar::new(
                                    VersionPrefix::State,
                                    target_version,
                                    frame,
                                ),
                                source: gensym_state,
                                op: BindOp::Put {
                                    field: field.name.to_string(),
                                    value: ValueRef::Var(val_var.clone()),
                                    class_tag: ValueRef::Literal("'nil'"),
                                },
                                shadow_write: false,
                                span,
                            });
                            self.push_control_flow_threaded_var_rereads(value, span, &mut stmts);
                            if is_last {
                                last_result = Some(ValueRef::Var(val_var));
                            }
                        }
                    }
                }
                // C7 — BT-1479: self fieldAt: name put: value. Dynamic field
                // name ⇒ §Dynamic-field-puts option 1 (Direct, not Put).
                BodyExprKind::SelfFieldAtPut => {
                    if let Expression::MessageSend { arguments, .. } = expr {
                        let name_var = self.fresh_var("Name");
                        let val_var = self.fresh_temp_var("Val");
                        let name_code = self.expression_doc(&arguments[0])?;
                        let source_version = self.state_version();
                        let current_state_name = self.current_state_var();
                        let val_code = self.expression_doc(&arguments[1])?;
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let ",
                                leaf::var(name_var.clone()),
                                " = ",
                                name_code,
                                " in let ",
                                leaf::var(val_var.clone()),
                                " = ",
                                val_code,
                                " in ",
                            ],
                            span,
                        ));
                        let _ = self.next_state_var();
                        let target_version = self.state_version();
                        stmts.push(ThreadedStmt::Bind {
                            target: VersionedVar::new(VersionPrefix::State, target_version, frame),
                            source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                            op: BindOp::Direct(ValueRef::Doc(docvec![
                                "call 'maps':'put'(",
                                leaf::var(name_var),
                                ", ",
                                leaf::var(val_var.clone()),
                                ", ",
                                leaf::var(current_state_name),
                                ")",
                            ])),
                            shadow_write: false,
                            span,
                        });
                        if is_last {
                            last_result = Some(ValueRef::Var(val_var));
                        }
                    }
                }
                // C8 — BT-1479: self fieldAt: name put: <control-flow-with-mutations>.
                BodyExprKind::SelfFieldAtPutControlFlow => {
                    if let Expression::MessageSend { arguments, .. } = expr {
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
                                " = call 'erlang':'element'(1, ",
                                leaf::var(tuple_var.clone()),
                                ") in ",
                            ],
                            span,
                        ));
                        let gensym_state =
                            VersionedVar::new(VersionPrefix::Gensym(rhs_state.clone()), 1, frame);
                        stmts.push(ThreadedStmt::Bind {
                            target: gensym_state.clone(),
                            source: VersionedVar::new(VersionPrefix::State, source_version, frame),
                            op: BindOp::Direct(ValueRef::Doc(docvec![
                                "call 'erlang':'element'(2, ",
                                leaf::var(tuple_var),
                                ")",
                            ])),
                            shadow_write: false,
                            span,
                        });
                        let _ = self.next_state_var();
                        let target_version = self.state_version();
                        stmts.push(ThreadedStmt::Bind {
                            target: VersionedVar::new(VersionPrefix::State, target_version, frame),
                            source: gensym_state,
                            op: BindOp::Direct(ValueRef::Doc(docvec![
                                "call 'maps':'put'(",
                                leaf::var(name_var),
                                ", ",
                                leaf::var(val_var.clone()),
                                ", ",
                                leaf::var(rhs_state),
                                ")",
                            ])),
                            shadow_write: false,
                            span,
                        });
                        self.push_control_flow_threaded_var_rereads(
                            &arguments[1],
                            span,
                            &mut stmts,
                        );
                        if is_last {
                            last_result = Some(ValueRef::Var(val_var));
                        }
                    }
                }
                // C9 — BT-1479: {a, b} := <control-flow-with-mutations>. No
                // Gensym hop — element(2, tuple) binds State(v+1) directly.
                BodyExprKind::DestructureAssignmentControlFlow => {
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
                                " = call 'erlang':'element'(1, ",
                                leaf::var(tuple_var.clone()),
                                ") in ",
                            ],
                            span,
                        ));
                        let _ = self.next_state_var();
                        let target_version = self.state_version();
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
                        self.push_control_flow_threaded_var_rereads(value, span, &mut stmts);
                        let binding_docs =
                            self.generate_destructure_bindings_from_var(pattern, &actual_val)?;
                        for d in binding_docs {
                            stmts.push(ThreadedStmt::Statement(d, span));
                        }
                    }
                }
                // C10
                BodyExprKind::ControlFlowWithMutations => {
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
                                " = call 'erlang':'element'(1, ",
                                leaf::var(tuple_var.clone()),
                                ") in ",
                            ],
                            span,
                        ));
                        let _ = self.next_state_var();
                        let target_version = self.state_version();
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
                        last_result = Some(ValueRef::Var(result_var));
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
                        let _ = self.next_state_var();
                        let target_version = self.state_version();
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
                        self.push_control_flow_threaded_var_rereads(expr, span, &mut stmts);
                    }
                }
                // C11 — BT-2797: a Tier 2 `value(:...)` call on a stored
                // block. Non-last carries the newline quirk
                // (`")\n in let "`) confirmed against real compiled output.
                BodyExprKind::Tier2ValueCall => {
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
                                " = call 'erlang':'element'(1, ",
                                leaf::var(tuple_var.clone()),
                                ") in ",
                            ],
                            span,
                        ));
                        let _ = self.next_state_var();
                        let target_version = self.state_version();
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
                        last_result = Some(ValueRef::Var(result_var));
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
                                " = call 'erlang':'element'(1, ",
                                leaf::var(tuple_var.clone()),
                                ")\n in ",
                            ],
                            span,
                        ));
                        let _ = self.next_state_var();
                        let target_version = self.state_version();
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
                        // BT-1213: rebind captured local-var mutations from
                        // NewState so a later read in this branch sees the
                        // mutated value.
                        if let Some(mutations) = self.get_inline_block_captured_mutations(expr) {
                            let new_state_name = self.current_state_var();
                            let mut re_reads: Vec<Document<'static>> = Vec::new();
                            for var in &mutations {
                                let core_var = self
                                    .lookup_var(var)
                                    .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                                re_reads.push(docvec![
                                    "let ",
                                    leaf::var(core_var),
                                    " = call 'maps':'get'(",
                                    leaf::atom(Self::local_state_key(var)),
                                    ", ",
                                    leaf::var(new_state_name.clone()),
                                    ") in ",
                                ]);
                            }
                            if !re_reads.is_empty() {
                                stmts.push(ThreadedStmt::Statement(Document::Vec(re_reads), span));
                            }
                        }
                    }
                }
                // C12b — BT-3178: a same-class self-send dispatched through
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
                    let dispatch_var =
                        self.dispatch_self_send_as_bind(expr, frame, span, &mut stmts)?;
                    if is_last {
                        let result_var = self.fresh_temp_var("SDResultVal");
                        stmts.push(ThreadedStmt::Statement(
                            docvec![
                                "let ",
                                leaf::var(result_var.clone()),
                                " = call 'erlang':'element'(1, ",
                                leaf::var(dispatch_var),
                                ") in ",
                            ],
                            span,
                        ));
                        last_result = Some(ValueRef::Var(result_var));
                    }
                }
                // C12 — catch-all pure statements (EarlyReturn, SuperSend,
                // ErrorSend, Tier2SelfSend, Pure).
                //
                // BT-3392: before compiling, hoist (as real `Bind`s pushed
                // into `stmts`) any self-send nested as a binary-op operand
                // of this statement, so `try_handle_self_dispatch` reuses
                // the already-threaded result instead of discarding its
                // mutation — see `hoist_self_sends_for_binary_op`'s doc
                // comment. When nothing needed hoisting (the overwhelmingly
                // common case), this is a no-op and `expression_doc` below
                // behaves exactly as before.
                _ => {
                    let mut safe_to_hoist = true;
                    self.hoist_self_sends_for_binary_op(
                        expr,
                        frame,
                        span,
                        &mut stmts,
                        &mut safe_to_hoist,
                    )?;
                    if is_last {
                        let result_var = self.fresh_temp_var("BranchResult");
                        let expr_doc = self.expression_doc(expr)?;
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
                        last_result = Some(ValueRef::Var(result_var));
                    } else {
                        let seq_var = self.fresh_temp_var("seq");
                        let expr_doc = self.expression_doc(expr)?;
                        stmts.push(ThreadedStmt::Statement(
                            docvec!["let ", leaf::var(seq_var), " = ", expr_doc, " in "],
                            span,
                        ));
                    }
                }
            }
        }

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
        Ok(self.verify_and_render_branch_arm(stmts, frame, final_version, block.span))
    }

    /// ADR 0111 Addendum 5 (BT-1213/BT-2355 rebind idiom, shared by C6/C8/
    /// C9/C10): rebinds each of a nested control-flow construct's threaded
    /// `__local__` captured vars from the just-produced state, appending
    /// one `Statement` (or none, if there are no threaded vars) to `stmts`.
    /// A rebind read is never itself a version mutation (Rule 1) — always a
    /// `Statement`, never a `Bind`.
    fn push_control_flow_threaded_var_rereads(
        &mut self,
        source_expr: &Expression,
        span: Span,
        stmts: &mut Vec<ThreadedStmt>,
    ) {
        let Some(threaded_vars) = self.get_control_flow_threaded_vars(source_expr) else {
            return;
        };
        let new_state_name = self.current_state_var();
        let re_reads = self.rebind_threaded_vars_from_state(&threaded_vars, &new_state_name);
        if !re_reads.is_empty() {
            stmts.push(ThreadedStmt::Statement(Document::Vec(re_reads), span));
        }
    }

    /// ADR 0111 Addendum 5, Rule 3: wraps a branch arm's real `Bind`/
    /// `Statement` sequence in one `ThreadedStmt::Threaded { mode:
    /// StateAcc(None), frame, .. }` node, `verify()`s it, and `render()`s
    /// it. `StateAcc` mode's rendering is a transparent pass-through
    /// (`render_threaded`'s `TupleAcc(_) | StateAcc(_) => render(body,
    /// ctx)` arm) — wrapping changes zero output bytes versus rendering
    /// `stmts` directly, while giving `verify()`'s frame-scoped `check_use`
    /// the correct non-ROOT frame to walk against (`verify`'s top-level
    /// slice is always implicitly `FrameId::ROOT` — see `FrameId`'s doc
    /// comment — so an un-wrapped `verify(&stmts)` would falsely reject
    /// every reference to this arm's own frame).
    ///
    /// BT-3149 also calls this directly from `expressions.rs`'s
    /// `generate_block_stateful` — a single-arm `with_branch_context` use
    /// (the Tier 2 stateful-block-body threading), same wrap/verify/render
    /// shape as a conditional branch arm.
    pub(in crate::core_erlang) fn verify_and_render_branch_arm(
        &mut self,
        stmts: Vec<ThreadedStmt>,
        frame: FrameId,
        final_version: usize,
        span: Span,
    ) -> (Document<'static>, usize) {
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
            // ADR 0111 Addendum 9, Question 1's scope check: a conditional
            // branch arm never carries a class-var mutation by construction
            // (`reject_class_var_field_assignment` fires before mode
            // selection for any threaded body, conditionals included), so
            // this value is inert here — set per the general lowering rule
            // (`self.block_depth == 0`, independently re-derived) for
            // consistency/forward-compatibility, not because this call site
            // needs it today.
            shadow_write_eligible: self.block_depth == 0,
            body: stmts,
            produces,
            span,
        }];
        let errors = threaded_ir::verify(&wrapper);
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core_erlang::tests::codegen;

    // ── ADR 0111 Addendum 5 / BT-3146: NonLinearVersion / UnboundVersion ──
    // are now LIVE checks for branch-arm IR (previously scaffolding-only —
    // `check_branch_frame_linearity`'s scalar synthesis could never
    // construct two arms colliding on the same frame, by construction).
    // Both regressions below are built through the exact production
    // types/constructors `generate_conditional_branch_inline`'s lowering
    // uses — a real `CoreErlangGenerator::with_branch_context` call for
    // frame allocation (`current_branch_frame`), real `VersionedVar`/
    // `ThreadedStmt::Bind`/`BindOp::Put` construction — not an isolated
    // hand-fixture disconnected from any real construction path.

    #[test]
    fn test_bt3146_nonlinear_version_detected_via_production_lowering_types() {
        let mut generator = CoreErlangGenerator::new("bt3146_regression_nonlinear");
        let errors = generator.with_branch_context(|this| {
            let frame = this.current_branch_frame();
            // Two field-mutation Binds that BOTH (incorrectly) target
            // State(1)@frame from State(0)@frame — the exact shape a
            // broken lowering (e.g. forgetting to call `next_state_var()`
            // between two field assignments in the same arm) would
            // produce. Mirrors C1's real `BindOp::Put` shape exactly.
            let source = VersionedVar::new(VersionPrefix::State, 0, frame);
            let target = VersionedVar::new(VersionPrefix::State, 1, frame);
            let make_put = |field: &str, val: &str, target: VersionedVar, source: VersionedVar| {
                ThreadedStmt::Bind {
                    target,
                    source,
                    op: BindOp::Put {
                        field: field.to_string(),
                        value: ValueRef::Var(val.to_string()),
                        class_tag: ValueRef::Literal("'nil'"),
                    },
                    shadow_write: false,
                    span: Span::default(),
                }
            };
            let wrapper = vec![ThreadedStmt::Threaded {
                mode: ThreadingMode::StateAcc(StateAccFallbackReason::None),
                frame,
                shadow_write_eligible: true, // State-prefix fixture, not class-var — inert
                body: vec![
                    make_put("n", "_Val1", target.clone(), source.clone()),
                    make_put("n", "_Val2", target.clone(), source),
                ],
                produces: vec![target],
                span: Span::default(),
            }];
            threaded_ir::verify(&wrapper)
        });

        assert!(
            errors.iter().any(|e| matches!(
                e,
                threaded_ir::VerifyError::NonLinearVersion { producers: 2, .. }
            )),
            "expected NonLinearVersion(producers: 2) for the duplicate \
             State(1) producer, got: {errors:?}"
        );
    }

    #[test]
    fn test_bt3146_unbound_version_detected_via_production_lowering_types() {
        let mut generator = CoreErlangGenerator::new("bt3146_regression_unbound");
        let errors = generator.with_branch_context(|this| {
            let frame = this.current_branch_frame();
            // A Bind whose source references a version this frame never
            // produced — the exact shape a broken lowering (e.g. reading a
            // stale `state_version()` snapshot from before an earlier
            // mutation actually landed) would produce.
            let phantom_source = VersionedVar::new(VersionPrefix::State, 5, frame);
            let target = VersionedVar::new(VersionPrefix::State, 6, frame);
            let bind = ThreadedStmt::Bind {
                target: target.clone(),
                source: phantom_source,
                op: BindOp::Put {
                    field: "n".to_string(),
                    value: ValueRef::Var("_Val1".to_string()),
                    class_tag: ValueRef::Literal("'nil'"),
                },
                shadow_write: false,
                span: Span::default(),
            };
            let wrapper = vec![ThreadedStmt::Threaded {
                mode: ThreadingMode::StateAcc(StateAccFallbackReason::None),
                frame,
                shadow_write_eligible: true, // State-prefix fixture, not class-var — inert
                body: vec![bind],
                produces: vec![target],
                span: Span::default(),
            }];
            threaded_ir::verify(&wrapper)
        });

        assert!(
            errors.iter().any(|e| matches!(
                e,
                threaded_ir::VerifyError::UnboundVersion { var, .. } if var.version == 5
            )),
            "expected UnboundVersion for the phantom State(5) source, got: {errors:?}"
        );
    }

    #[test]
    fn test_bt2797_bare_tier2_value_call_in_conditional_branch_unpacks_tuple() {
        // BT-2797 (PR #2899 review fix): a bare (non-assigned) `self.field
        // value:` statement inside an `ifTrue:` branch that *also* contains an
        // explicit field assignment (so `block_analysis` detects the branch
        // needs state threading and routes it through
        // `generate_conditional_branch_inline`). Before the fix, the
        // catch-all `_ =>` arm called `expression_doc` directly on the
        // Tier2ValueCall statement, discarding the `{Result, NewState}`
        // tuple's second element — silently dropping the field mutation
        // performed inside the stored block. Structural check only (see
        // stdlib/test/tier2_subexpr_and_conditional_block_test.bt for the
        // runtime end-to-end check).
        let src = "Actor subclass: Ctr\n  state: total = 0\n  state: calls = 0\n  state: onTick = nil\n\n  setup => self.onTick := [:x | self.total := self.total + x]\n\n  tickIfPositive: x =>\n    x > 0 ifTrue: [self.calls := self.calls + 1. self.onTick value: x].\n    self.total\n";
        let code = codegen(src);
        assert!(
            regex::Regex::new(r"let _T2Tuple\w* = .*is_function.*in let _BranchResult\w* = call 'erlang':'element'\(1, _T2Tuple")
                .unwrap()
                .is_match(&code),
            "the bare Tier2ValueCall statement inside the ifTrue: branch must \
             unpack the returned {{Result, NewState}} tuple via element/1 and \
             thread element/2 forward as the branch's new state. Got: {code}"
        );
        assert!(
            regex::Regex::new(r"let StateAcc\w* = call 'erlang':'element'\(2, _T2Tuple")
                .unwrap()
                .is_match(&code),
            "the branch's new state must be threaded forward from the \
             Tier2ValueCall's tuple. Got: {code}"
        );
    }

    #[test]
    fn test_if_true_mutation_bypasses_runtime_dispatch() {
        // Actor ifTrue: with field mutation compiles to inline case (not runtime dispatch).
        // Also verifies the non-taken (false) branch returns {'nil', State}.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  inc: flag =>\n    flag ifTrue: [self.n := self.n + 1].\n    self.n\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "ifTrue: with field mutation should generate inline case. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('n'"),
            "True branch should update 'n' via maps:put. Got:\n{code}"
        );
        assert!(
            !code.contains("'beamtalk_message_dispatch':'send'"),
            "ifTrue: with mutation should NOT use runtime dispatch. Got:\n{code}"
        );
        // Non-taken branch returns {'nil', unchanged_state} with StateAcc naming
        assert!(
            code.contains("{'nil',"),
            "Non-taken branch should return {{'nil', State}}. Got:\n{code}"
        );
        assert!(
            code.contains("StateAcc"),
            "Branch bodies should use StateAcc naming. Got:\n{code}"
        );
    }

    #[test]
    fn test_nested_list_op_in_branch_threads_outer_local() {
        // BT-2356 case (B): a nested list op (`do:`) inside an `ifTrue:` branch
        // mutates an outer local. The conditional must be recognised as
        // state-threading (via the nested cross-scope mutation), the local must be
        // seeded, packed by the nested op, and extracted after the conditional.
        let src = concat!(
            "Actor subclass: Ctr\n",
            "  state: x = 0\n\n",
            "  run: flag =>\n",
            "    sum := 0\n",
            "    flag ifTrue: [#(1, 2, 3) do: [:i | sum := sum + i]]\n",
            "    sum\n",
        );
        let code = codegen(src);
        // The conditional must compile to an inline case, NOT a runtime dispatch.
        // A runtime `send(_, 'ifTrue:', [Fun])` returns `nil` on the false branch, so
        // the sequencer's `element(2, _)` unpack crashes with badarg (BT-2356 regression).
        //
        // ADR 0087 Phase 2 (BT-2298): `register_class/0` now bakes a methodXref
        // index that lists `ifTrue:` as a sent selector (metadata, not dispatch).
        // Exclude that line so the assertion still checks only for a real runtime
        // dispatch of the selector.
        let code_no_xref: String = code
            .lines()
            .filter(|line| !line.contains("'methodXref'"))
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            !code_no_xref.contains("'ifTrue:'"),
            "nested-op-in-branch must inline ifTrue: (no runtime 'ifTrue:' dispatch). Got:\n{code}"
        );
        // The outer local 'sum' is seeded into the StateAcc before the branch.
        assert!(
            code.contains("maps':'put'('__local__sum'"),
            "nested-op-in-branch should pack/seed '__local__sum'. Got:\n{code}"
        );
        // The method body must extract the threaded 'sum' back after the conditional.
        assert!(
            code.contains("maps':'get'('__local__sum'"),
            "nested-op-in-branch should extract 'sum' via maps:get after the conditional. Got:\n{code}"
        );
    }

    #[test]
    fn test_if_false_mutation_threads_state_in_false_arm() {
        // Actor ifFalse: with field mutation compiles to inline case with false branch mutating
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  dec: flag =>\n    flag ifFalse: [self.n := self.n - 1].\n    self.n\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "ifFalse: with field mutation should generate inline case. Got:\n{code}"
        );
        // False branch maps:put happens in the '<false>' arm
        assert!(
            code.contains("maps':'put'('n'"),
            "False branch should update 'n' via maps:put. Got:\n{code}"
        );
        // True arm returns nil with unchanged state, as {'nil', State}
        assert!(
            code.contains("{'nil',"),
            "True arm of ifFalse: should return {{'nil', State}}. Got:\n{code}"
        );
    }

    #[test]
    fn test_if_true_if_false_with_mutations_generates_two_threaded_branches() {
        // ifTrue:ifFalse: with mutations in both branches generates two threaded arms
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  toggle: flag =>\n    flag ifTrue: [self.n := 1] ifFalse: [self.n := 0].\n    self.n\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "ifTrue:ifFalse: with mutations should generate inline case. Got:\n{code}"
        );
        // maps:put should appear at least twice (once per branch)
        let put_count = code.matches("maps':'put'('n'").count();
        assert!(
            put_count >= 2,
            "Both branches should call maps:put for 'n'. Found {put_count}. Got:\n{code}"
        );
    }

    #[test]
    fn test_bt2355_write_only_conditional_seeds_and_extracts_local() {
        // BT-2355: `flag ifTrue: [m := 9]` then read `m` (non-last). The outer
        // local `m` must thread back even though the block only writes it.
        let src = "Actor subclass: Cps\n\n  m: flag =>\n    val := 0\n    flag ifTrue: [val := 9]\n    val\n";
        let code = codegen(src);
        // The non-taken branch and extraction need the seeded key.
        assert!(
            code.contains("maps':'put'('__local__val'"),
            "write-only conditional should seed/put '__local__val'. Got:\n{code}"
        );
        // After the conditional, the local is read back out of the threaded state.
        assert!(
            code.contains("maps':'get'('__local__val'"),
            "outer local 'val' should be extracted via maps:get after the conditional. Got:\n{code}"
        );
    }

    #[test]
    fn test_bt2355_read_write_conditional_extracts_local() {
        // BT-2355: `flag ifTrue: [sum := sum + 7]` then read `sum` (non-last).
        let src = "Actor subclass: Cps\n\n  m: flag =>\n    sum := 0\n    flag ifTrue: [sum := sum + 7]\n    sum\n";
        let code = codegen(src);
        assert!(
            code.contains("maps':'get'('__local__sum'"),
            "outer local 'sum' should be extracted via maps:get after the conditional. Got:\n{code}"
        );
    }

    #[test]
    fn test_bt2355_if_true_if_false_seeds_local_for_both_branches() {
        // BT-2355: a local written in only one branch must still be extractable, so
        // the seed key must precede the case (be present in both branches' base).
        let src = "Actor subclass: Cps\n\n  m: flag =>\n    x := 1\n    flag ifTrue: [x := x + 1] ifFalse: [x := x + 100]\n    x\n";
        let code = codegen(src);
        // One seed put (before the case) + one put per branch = 3 puts of '__local__x'.
        let put_count = code.matches("maps':'put'('__local__x'").count();
        assert!(
            put_count >= 3,
            "expected a seed put plus a put in each branch for '__local__x' (>=3), found {put_count}. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'get'('__local__x'"),
            "outer local 'x' should be extracted after ifTrue:ifFalse:. Got:\n{code}"
        );
    }

    #[test]
    fn test_local_var_in_if_true_block_reads_back_correctly() {
        // BT-1225: Local var assigned inside ifTrue: block must be readable in subsequent
        // expressions of the same block without a {badkey,VarName} runtime crash.
        // The write uses '__local__y' key; reads must resolve to the temp var, not 'y'.
        let src = "Actor subclass: BrokenActor\n  state: x = 5\n\n  myMethod: cond =>\n    cond ifTrue: [\n      y := self.x + 1.\n      self.x := y\n    ].\n    self.x\n";
        let code = codegen(src);
        // The local var 'y' should be stored with __local__ prefix
        assert!(
            code.contains("__local__y"),
            "Local var 'y' should use __local__ prefix in maps:put. Got:\n{code}"
        );
        // The field assignment self.x := y should NOT use maps:get('y', ...) — that would
        // be the buggy read (key mismatch). Instead, it should reference the temp var directly.
        assert!(
            !code.contains("maps':'get'('y'"),
            "Should NOT generate maps:get('y',...) — reads of 'y' should use temp var. Got:\n{code}"
        );
    }

    #[test]
    fn test_local_var_in_if_false_block_reads_back_correctly() {
        // BT-1225: Same fix applies to ifFalse: blocks.
        let src = "Actor subclass: TestActor\n  state: x = 10\n\n  myMethod: cond =>\n    cond ifFalse: [\n      y := self.x - 1.\n      self.x := y\n    ].\n    self.x\n";
        let code = codegen(src);
        assert!(
            code.contains("__local__y"),
            "Local var 'y' should use __local__ prefix in maps:put. Got:\n{code}"
        );
        assert!(
            !code.contains("maps':'get'('y'"),
            "Should NOT generate maps:get('y',...) — reads of 'y' should use temp var. Got:\n{code}"
        );
    }

    #[test]
    fn test_value_type_if_true_local_mutation_generates_inline_case() {
        // BT-1392: Value type ifTrue: with captured local mutation should
        // generate an inline case expression (not runtime dispatch).
        let src = "Object subclass: Foo\n\n  test: flag =>\n    x := 1\n    flag ifTrue: [x := 2]\n    x\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "Value type ifTrue: with local mutation should generate inline case. Got:\n{code}"
        );
        assert!(
            !code.contains("'beamtalk_message_dispatch':'send'"),
            "Value type ifTrue: with mutation should NOT use runtime dispatch. Got:\n{code}"
        );
    }

    #[test]
    fn test_value_type_if_false_local_mutation_generates_inline_case() {
        // BT-1392: Value type ifFalse: with captured local mutation
        let src = "Object subclass: Foo\n\n  test: flag =>\n    x := 1\n    flag ifFalse: [x := 2]\n    x\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "Value type ifFalse: with local mutation should generate inline case. Got:\n{code}"
        );
    }

    #[test]
    fn test_value_type_if_true_if_false_local_mutation() {
        // BT-1392: Value type ifTrue:ifFalse: with captured local mutation
        let src = "Object subclass: Foo\n\n  test: flag =>\n    x := 1\n    flag ifTrue: [x := 2] ifFalse: [x := 3]\n    x\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "Value type ifTrue:ifFalse: with local mutation should generate inline case. Got:\n{code}"
        );
    }

    #[test]
    fn test_collect_wrapping_if_true_with_field_mutation() {
        // BT-1477: collect: block containing ifTrue: with self.field := mutation.
        // The field mutation inside the conditional must be threaded through the
        // collect: loop accumulator, not silently lost.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  m: list =>\n    list collect: [:each | each > 0 ifTrue: [self.n := self.n + 1]. each * 2]\n    self.n\n";
        let code = codegen(src);
        // The collect: should use a stateful accumulator (maps:put for field 'n')
        assert!(
            code.contains("maps':'put'('n'"),
            "BT-1477: collect: wrapping ifTrue: with field mutation should thread state. Got:\n{code}"
        );
    }

    #[test]
    fn test_if_true_wrapping_do_with_field_mutation() {
        // BT-1477: ifTrue: wrapping do: block with self.field := mutation.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  m: flag list: list =>\n    flag ifTrue: [list do: [:each | self.n := self.n + each]]\n    self.n\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "BT-1477: ifTrue: wrapping do: should generate inline case. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('n'"),
            "BT-1477: do: inside ifTrue: should thread field mutations. Got:\n{code}"
        );
    }

    #[test]
    fn test_do_wrapping_if_true_with_field_mutation() {
        // BT-1477: do: block containing ifTrue: with self.field := mutation.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  m: list =>\n    list do: [:each | each > 0 ifTrue: [self.n := self.n + each]]\n    self.n\n";
        let code = codegen(src);
        assert!(
            code.contains("maps':'put'('n'"),
            "BT-1477: do: wrapping ifTrue: with field mutation should thread state. Got:\n{code}"
        );
    }

    #[test]
    fn test_triple_nested_if_true_do_if_true_with_field_mutation() {
        // BT-1477: ifTrue: wrapping do: wrapping ifTrue: with self.field := mutation.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  m: flag list: list =>\n    flag ifTrue: [list do: [:each | each > 0 ifTrue: [self.n := self.n + each]]]\n    self.n\n";
        let code = codegen(src);
        assert!(
            code.contains("case "),
            "BT-1477: triple-nested should generate inline case. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('n'"),
            "BT-1477: triple-nested should thread field mutations. Got:\n{code}"
        );
    }

    #[test]
    fn test_field_assignment_control_flow_rhs_unpacks_tuple() {
        // BT-1479: self.field := <control-flow-with-mutations> must unpack {Value, State}
        let src = "Actor subclass: A\n  state: x = 0\n  state: y = 0\n\n  m: flag =>\n    self.x := flag ifTrue: [self.y := 1. 42] ifFalse: [0]\n    self.x\n";
        let code = codegen(src);
        assert!(
            code.contains("element'(1,"),
            "FieldAssignmentControlFlow should unpack element(1) from RHS tuple. Got:\n{code}"
        );
        assert!(
            code.contains("element'(2,"),
            "FieldAssignmentControlFlow should unpack element(2) for state. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('x'"),
            "Should update field 'x' via maps:put. Got:\n{code}"
        );
    }

    #[test]
    fn test_self_field_at_put_in_conditional_branch() {
        // BT-1479: SelfFieldAtPut inside conditional branch must not fall through to wildcard
        let src = "Actor subclass: A\n  state: x = 0\n\n  m: flag =>\n    flag ifTrue: [self fieldAt: #x put: 42]\n    self.x\n";
        let code = codegen(src);
        assert!(
            code.contains("maps':'put'("),
            "SelfFieldAtPut in conditional branch should generate maps:put. Got:\n{code}"
        );
    }

    #[test]
    fn test_self_field_at_put_in_method_body() {
        // BT-1479: self fieldAt: name put: value in method body (non-conditional context)
        let src = "Actor subclass: A\n  state: x = 0\n\n  m =>\n    self fieldAt: #x put: 42\n    self.x\n";
        let code = codegen(src);
        assert!(
            code.contains("maps':'put'("),
            "SelfFieldAtPut should generate maps:put. Got:\n{code}"
        );
    }
}
