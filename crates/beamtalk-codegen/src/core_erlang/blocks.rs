// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Block (closure) code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! This module handles code generation for Beamtalk blocks (closures):
//! - Tier 1 (plain fun) and Tier 2 (stateful `StateAcc`-threading) block
//!   compilation, and the Erlang-interop wrapper that strips the Tier 2
//!   protocol for a block crossing into plain Erlang code
//! - Block-body statement sequencing (classification and per-statement
//!   dispatch: destructure, field/local assignment, threaded control flow)
//! - The captured-mutation / unsafe-self-send analysis shared by both
//!
//! Note: pattern matching (`match:`) and destructuring extraction live in
//! [`super::patterns`]; other expression code generation stays in
//! [`super::expressions`].

use super::threaded_ir::{self, ThreadedStmt, ValueRef, VersionPrefix, VersionedVar};
use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result};
use crate::core_erlang::threading_analysis::ThreadedLocals;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Block, Expression};

/// Classification of how a block body expression should be handled.
/// Produced by [`CoreErlangGenerator::classify_block_expr`] and consumed
/// by [`CoreErlangGenerator::generate_block_expr`]. `pub(super)`: both are
/// also called from `while_loops.rs` (ADR 0118 phase 3) — see
/// their own doc comments.
pub(super) enum BlockExprKind {
    /// `{a, b} := expr` or `#[a, b] := expr` — destructure assignment.
    /// Carries `is_last` so the handler can answer the right-hand side (the
    /// destructure's value, BT-3889) when it is the final expression in the block.
    Destructure { is_last: bool },
    /// Last expression (general case) — its value is the block's result.
    LastExpr,
    /// `self.field := value` — direct field assignment (non-last).
    FieldAssignment,
    /// `var := expr` — local variable assignment (non-last).
    LocalAssignment,
    /// `whileTrue:` / `whileFalse:` / `timesRepeat:` with threaded vars (non-last).
    ControlFlowWithThreadedVars,
    /// Expression evaluated for side effects only — result discarded.
    SideEffect,
}

impl CoreErlangGenerator {
    /// Extracts a block literal from an expression, unwrapping parentheses.
    ///
    /// Returns `Some(&Block)` for `Expression::Block` and for
    /// `Expression::Parenthesized` wrappers around a block (recursively).
    /// Returns `None` for all other expression kinds (identifiers, message sends, etc.).
    ///
    /// Used by Erlang interop sites to detect block arguments regardless of whether
    /// the caller wrote `[:x | x + 1]` or `([:x | x + 1])`.
    pub(super) fn extract_block_literal(expr: &Expression) -> Option<&Block> {
        match expr {
            Expression::Block(block) => Some(block),
            Expression::Parenthesized { expression, .. } => Self::extract_block_literal(expression),
            _ => None,
        }
    }

    /// This is the Tier 2 promotion check: non-empty → stateful block, empty → pure block.
    /// Centralised here so both `generate_block` and `generate_erlang_interop_wrapper`
    /// use identical logic and cannot drift independently.
    pub(super) fn captured_mutations_for_block(block: &Block) -> Vec<String> {
        use crate::core_erlang::block_analysis::analyze_block;
        Self::captured_mutations_from_analysis(&analyze_block(block))
    }

    /// Same check as [`Self::captured_mutations_for_block`], but from an already-computed
    /// analysis — lets callers that need more than one fact off a single `analyze_block`
    /// pass (e.g. [`Self::generate_block`], which also checks `field_writes`) avoid
    /// re-walking the block's AST.
    fn captured_mutations_from_analysis(
        analysis: &crate::core_erlang::block_analysis::BlockMutationAnalysis,
    ) -> Vec<String> {
        beamtalk_core::semantic_analysis::block_facts::captured_local_mutations_from_analysis(
            analysis,
        )
    }

    /// Returns captured mutation variable names if `expr` is a
    /// `[block] value`/`value:`/etc. with a literal block that mutates outer locals,
    /// sent a selector of the block's own arity (BT-3881: a block of another
    /// arity is applied, never inlined, `block_literal_of_other_arity`).
    pub(super) fn inline_block_captured_mutations(expr: &Expression) -> Option<Vec<String>> {
        if let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = expr
        {
            let is_value_selector = selector
                .well_known()
                .is_some_and(beamtalk_core::ast::well_known::WellKnownSelector::is_block_value);
            if is_value_selector
                && beamtalk_core::semantic_analysis::block_facts::block_literal_of_other_arity(
                    receiver,
                    selector,
                    arguments.len(),
                )
                .is_none()
            {
                if let Expression::Block(block) = receiver.as_ref() {
                    let mutations = Self::captured_mutations_for_block(block);
                    if !mutations.is_empty() {
                        return Some(mutations);
                    }
                }
            }
        }
        None
    }

    /// Generates code for a block (closure).
    ///
    /// Automatically selects Tier 1 (plain) or Tier 2 (stateful) codegen
    /// based on `BlockMutationAnalysis`:
    ///
    /// - **Tier 2 (stateful):** blocks with captured variable mutations emit
    ///   `fun(Params..., StateAcc) -> {Result, NewStateAcc}`
    /// - **Plain fun:** pure blocks (no captured mutations) emit
    ///   `fun(Params...) -> Result` — zero overhead for stateless blocks
    ///
    /// Captured mutations = variables written inside the block that were also
    /// read from the outer scope (i.e. `local_writes ∩ captured_reads`).
    /// Field writes (`self.x := ...`) and self-sends are handled separately:
    /// - Field writes are threaded via `gen_server` State at the method level (for Tier 2).
    /// - Self-sends are pre-scanned via `generate_tier2_self_send_open`.
    ///
    /// ADR 0130 §5: a block literal that reads a class variable binds its
    /// creation-time capture around the `fun` ([`Self::with_class_var_capture`]).
    pub(super) fn generate_block(&mut self, block: &Block) -> Result<Document<'static>> {
        self.with_class_var_capture(block, |this| this.generate_block_fun(block))
    }

    /// [`Self::generate_block`] without the class-variable capture binding: the
    /// `fun` itself, selecting Tier 1 or Tier 2.
    fn generate_block_fun(&mut self, block: &Block) -> Result<Document<'static>> {
        use crate::core_erlang::block_analysis::analyze_block;
        let analysis = analyze_block(block);

        // `self.field :=` inside a block that reaches this generic
        // fallback can't correctly thread state — see `validate_stored_closure`
        // for why. Lifting this once stored/opaque blocks get proper Tier 2
        // support is tracked separately. Checked *before* the
        // captured-local-mutation promotion below: a block with both a field
        // write and a captured-local mutation (e.g. `[:x | outerCount :=
        // outerCount + x. self.total := self.total + outerCount]`) would
        // otherwise be routed to Tier 2 for the local mutation alone, which
        // fixes nothing — `generate_block_value_call` and friends still call
        // the resulting 2-arity stateful fun with only its declared params
        // (no `StateAcc` argument), producing a `badarity` crash at runtime
        // instead of the erlc-time failure this check exists to catch.
        // Location is a lazy thunk: format!/span_to_line only run on the
        // (rare) error path, not on every block that reaches this line.
        //
        // Guarded on field_writes specifically (not a bare call to
        // validate_stored_closure) so a block with *only* captured-local
        // mutations — the legitimate Tier 2 case handled below — doesn't
        // spuriously trip validate_stored_closure's separate local-mutation
        // branch, which only applies to blocks that never reach Tier 2 at all.
        //
        // ADR 0130 §3: not in a class method, where `self.x :=` can only name a
        // class variable and is an in-place `put` that needs no threading, so a
        // block may write class variables wherever it runs.
        if !analysis.field_writes.is_empty() && !self.in_class_method() {
            Self::validate_stored_closure(&analysis, || self.location_label(block.span))?;
        }

        let captured_mutations = Self::captured_mutations_from_analysis(&analysis);

        // Blocks with captured local mutations use Tier 2 stateful calling convention.
        if !captured_mutations.is_empty() {
            return self.generate_block_stateful_fun(block, &captured_mutations);
        }

        // Pure block: plain fun (no mutations to thread via Tier 2)
        //
        // `with_closed_fun_scope` saves and restores `state_version` (BT-3737
        // made this the one shared discipline for every closed `fun`). A pure
        // block's body can still
        // contain a conditional/field-mutation whose own state threading
        // bumps `state_version` (deliberately visible to later statements
        // *within this same block* — see `generate_block_body_slice`'s doc
        // comment), but the closure is a separate Core Erlang `fun`: any
        // `StateN` it binds is scoped to that `fun` and unreachable once it
        // returns. Without restoring here, the bumped counter leaked into
        // the enclosing method, which went on referencing a `StateN` that
        // was never bound in its own scope — an unbound-variable `core_lint`
        // failure at a call site with two sibling block arguments (e.g.
        // `ifOk:ifError:`), where the second block, or the code following
        // the whole call, read the leaked version.
        let body_result = self.with_closed_fun_scope(|this| {
            // Track block nesting so self-cast sends route through the mailbox
            this.block_depth += 1;

            let mut param_parts: Vec<Document<'static>> = Vec::new();
            for (i, param) in block.parameters.iter().enumerate() {
                if i > 0 {
                    param_parts.push(Document::Str(", "));
                }
                let var_name = this.fresh_var(&param.name);
                param_parts.push(leaf::var(var_name));
            }
            let header = docvec!["fun (", Document::Vec(param_parts), ") -> "];

            // Generate block body as Document; restore block_depth even on error.
            let body = this.generate_block_body(block);
            this.block_depth -= 1;
            body.map(|body| (header, body))
        });
        // The block is a closed `fun () -> ... end` expression. Any
        // open let-chain produced inside the body is closed by the body
        // handlers and scoped inside the fun, so the block as a whole MUST
        // NOT propagate an open scope to its outer context. Clear the
        // side-channel in case the body's last statement left it set.
        self.loop_mode.direct_params_do_open_chain = false;
        let (header, body) = body_result?;
        Ok(docvec![header, body])
    }

    /// Generates a Tier 2 stateful block (ADR 0041 Phase 0).
    ///
    /// Emits a block with the stateful calling convention:
    /// `fun(Param1, ..., ParamN, StateAcc) -> {Result, NewStateAcc}`
    ///
    /// Captured-mutated variables are unpacked from `StateAcc` at the start,
    /// assignments thread through `StateAcc` via `maps:put`, and the final
    /// expression is wrapped in a `{Result, StateAccN}` tuple.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// fun (X, StateAcc) ->
    ///     let Count = call 'maps':'get'('__local__count', StateAcc) in
    ///     let _Sum = call 'erlang':'+'(Count, X) in
    ///     let StateAcc1 = call 'maps':'put'('__local__count', _Sum, StateAcc) in
    ///     {_Sum, StateAcc1}
    /// end
    /// ```
    ///
    /// ADR 0111 close-out: this arm's mutation sequence is built
    /// as real [`ThreadedStmt`]s (the same C1/C2-style shapes
    /// `conditionals.rs`'s ADR 0111 Addendum 5 pinned, via
    /// `lower_field_assignment_bind`/`lower_local_var_assignment_bind`),
    /// wrapped in one [`threaded_ir::ThreadedStmt::Threaded`] node (this
    /// single-arm `with_branch_context`'s own [`threaded_ir::FrameId`]),
    /// [`threaded_ir::verify`]d and [`threaded_ir::render`]ed via
    /// `conditionals.rs`'s `verify_and_render_branch_arm`. `UnboundVersion`
    /// is a live check against this arm's real IR (see the scalar-synthesis
    /// `check_branch_frame_linearity` scaffolding's own doc comment for why
    /// it could not check this before).
    ///
    /// ADR 0130 §5: binds the block's class-variable capture around the `fun`
    /// when it reads a class variable, exactly as [`Self::generate_block`].
    pub(super) fn generate_block_stateful(
        &mut self,
        block: &Block,
        captured_vars: &[String],
    ) -> Result<Document<'static>> {
        self.with_class_var_capture(block, |this| {
            this.generate_block_stateful_fun(block, captured_vars)
        })
    }

    /// [`Self::generate_block_stateful`] without the capture binding.
    fn generate_block_stateful_fun(
        &mut self,
        block: &Block,
        captured_vars: &[String],
    ) -> Result<Document<'static>> {
        self.push_scope();
        // Track block nesting so self-cast sends route through the mailbox
        self.block_depth += 1;

        // Bind block parameters
        let mut param_parts: Vec<Document<'static>> = Vec::new();
        for (i, param) in block.parameters.iter().enumerate() {
            if i > 0 {
                param_parts.push(Document::Str(", "));
            }
            let var_name = self.fresh_var(&param.name);
            param_parts.push(leaf::var(var_name));
        }

        // Add StateAcc parameter
        if !block.parameters.is_empty() {
            param_parts.push(Document::Str(", "));
        }
        param_parts.push(Document::Str("StateAcc"));

        let header = docvec!["fun (", Document::Vec(param_parts), ") -> "];

        // Set up loop body context for StateAcc-based threading
        let result = self.with_branch_context(|this| {
            let frame = this.current_branch_frame();
            let mut stmts: Vec<ThreadedStmt> = Vec::new();

            // Unpack captured-mutated vars from StateAcc.
            // Use maps:get/3 with the current outer value as fallback so the block
            // remains callable even when StateAcc was not pre-seeded with the local state keys
            // (e.g. when called through the runtime arity normalization wrapper).
            for var_name in captured_vars {
                let core_var = Self::to_core_erlang_var(var_name);
                let key = Self::local_state_key(var_name);
                // Look up the outer binding BEFORE rebinding so the fallback refers to the
                // value at block-definition time (not the newly introduced inner binding).
                // If no outer binding exists (e.g. REPL mode where vars come from state dict),
                // fall back to maps:get/2 (original behavior) to avoid referencing unbound vars.
                let outer_binding = this.lookup_var(var_name).cloned();
                this.bind_var(var_name, &core_var);
                let unpack_doc = if let Some(outer_var) = outer_binding {
                    docvec![
                        "let ",
                        leaf::var(core_var),
                        " = call 'maps':'get'(",
                        leaf::atom(key),
                        ", StateAcc, ",
                        leaf::var(outer_var),
                        ") in "
                    ]
                } else {
                    docvec![
                        "let ",
                        leaf::var(core_var),
                        " = call 'maps':'get'(",
                        leaf::atom(key),
                        ", StateAcc) in "
                    ]
                };
                stmts.push(ThreadedStmt::Statement(unpack_doc, block.span));
            }

            // Generate body expressions with state threading
            this.generate_block_stateful_body(block, frame, captured_vars, &mut stmts)?;
            let branch_final = this.state_version();
            Ok::<_, crate::core_erlang::CodeGenError>(this.verify_and_render_branch_arm(
                stmts,
                frame,
                branch_final,
                block.span,
            ))
        });
        // Ensure block_depth and scope are restored even on error.
        self.block_depth -= 1;
        self.pop_scope();
        // Stateful blocks are also closed `fun (...) -> {Result, NewStateAcc}`
        // expressions and must not propagate an open scope from their body to
        // the outer context.
        self.loop_mode.direct_params_do_open_chain = false;

        let (body_doc, _branch_final) = result?;

        Ok(docvec![header, body_doc])
    }

    /// Generates an Erlang-compatible wrapper for a block at an Erlang call site.
    ///
    /// Erlang/Elixir code does not know about the Beamtalk state-threading protocol.
    /// When a Beamtalk block is passed to an Erlang call site (e.g. `lists:map/2`),
    /// this function generates a wrapper that strips the `StateAcc` protocol:
    ///
    /// ```erlang
    /// %% For a stateful block: fun(X, StateAcc) -> {X + Count, StateAcc1}
    /// let _BtBlock = fun(X, StateAcc) -> ... in
    /// fun(X) ->
    ///     let _WT = apply _BtBlock(X, CurrentState) in
    ///     let _WRes = call 'erlang':'element'(1, _WT) in _WRes
    /// ```
    ///
    /// The wrapper captures `CurrentState` from the enclosing scope so that reads of
    /// captured variables succeed. Mutations made inside the block are dropped — the
    /// updated `StateAcc` is not propagated back to the Beamtalk caller.
    ///
    /// For **pure blocks** (no captured mutations), returns the plain Tier 1 block
    /// `fun(Params...) -> Body` unchanged and `is_stateful = false`.
    ///
    /// For **stateful blocks** (captured mutations), returns the wrapper expression
    /// and `is_stateful = true`. Callers should emit a diagnostic warning because
    /// mutations will be silently dropped.
    pub(super) fn generate_erlang_interop_wrapper(
        &mut self,
        block: &Block,
    ) -> Result<(Document<'static>, bool)> {
        // Use the shared helper to ensure consistent Tier 2 promotion logic.
        let captured_mutations = Self::captured_mutations_for_block(block);

        if captured_mutations.is_empty() {
            // Pure block — plain Tier 1 fun, no wrapping needed.
            return Ok((self.generate_block(block)?, false));
        }

        // Stateful block (captured local mutations) — generate Tier 2 block,
        // then wrap it to strip the protocol so Erlang can call it as a plain fun.
        let bt_block_var = self.fresh_temp_var("BtBlock");

        // Generate the Tier 2 block: fun(Params..., StateAcc) -> {Result, NewStateAcc}
        let tier2_doc = self.generate_block_stateful(block, &captured_mutations)?;

        // Seed StateAcc with captured locals before passing to the Tier 2 block.
        // The Tier 2 block expects keys like "__local__count" in the StateAcc for
        // captured mutations. For ValueType context, start with an empty map since
        // there's no State var in the signature. For Actor/Repl, start from current_state.
        let mut seed_state = if matches!(self.context, CodeGenContext::ValueType) {
            self.fresh_temp_var("WStateAcc")
        } else {
            self.current_state_var().clone()
        };
        let mut seed_docs: Vec<Document<'static>> = Vec::new();

        // If ValueType, initialize StateAcc to empty map.
        if matches!(self.context, CodeGenContext::ValueType) {
            seed_docs.push(docvec![
                "let ",
                leaf::var(seed_state.clone()),
                " = ~{}~ in "
            ]);
        }

        // Pack captured locals into StateAcc under "__local__*" keys.
        for var_name in &captured_mutations {
            let next_state = self.fresh_temp_var("WStateAcc");
            let key = Self::local_state_key(var_name);

            // Generate code to fetch the captured variable.
            // If the variable is bound in the local scope, use it directly.
            // Otherwise, fetch it from the current State map (for REPL/Actor contexts).
            let var_ref = if let Some(bound_var) = self.lookup_var(var_name).cloned() {
                docvec![leaf::var(bound_var)]
            } else {
                // Variable not in scope — fetch from State map
                // In REPL context, captured mutations are stored in State (the bindings map)
                let state_var = self.current_state_var();
                docvec![
                    "call 'maps':'get'(",
                    leaf::atom(var_name.clone()),
                    ", ",
                    leaf::var(state_var),
                    ")"
                ]
            };

            seed_docs.push(docvec![
                "let ",
                leaf::var(next_state.clone()),
                " = call 'maps':'put'(",
                leaf::atom(key),
                ", ",
                var_ref,
                ", ",
                leaf::var(seed_state),
                ") in "
            ]);
            seed_state = next_state;
        }

        // Fresh parameter names for the wrapper fun (separate scope from the Tier 2 block).
        let n_params = block.parameters.len();
        let wrap_params: Vec<String> = (0..n_params).map(|_| self.fresh_temp_var("WArg")).collect();

        // Build wrapper parameter list: "WArg1, WArg2, ..."
        let mut param_parts: Vec<Document<'static>> = Vec::new();
        for (i, p) in wrap_params.iter().enumerate() {
            if i > 0 {
                param_parts.push(Document::Str(", "));
            }
            param_parts.push(leaf::var(p.clone()));
        }

        // Build apply arguments: "WArg1, ..., WArgN, SeedStateAcc"
        let mut apply_parts: Vec<Document<'static>> = Vec::new();
        for (i, p) in wrap_params.iter().enumerate() {
            if i > 0 {
                apply_parts.push(Document::Str(", "));
            }
            apply_parts.push(leaf::var(p.clone()));
        }
        if !wrap_params.is_empty() {
            apply_parts.push(Document::Str(", "));
        }
        apply_parts.push(leaf::var(seed_state));

        // Emit:
        // let _BtBlock = fun(Params..., StateAcc) -> ... in
        // let WStateAcc0 = ~{}~ in  (if ValueType)
        // let WStateAcc1 = call 'maps':'put'('__local__count', Count, WStateAcc0) in
        // ... (for each captured local)
        // fun(WArg1, ..., WArgN) ->
        //     let _WT = apply _BtBlock(WArg1, ..., WArgN, WStateAccN) in
        //     let _WRes = call 'erlang':'element'(1, _WT) in _WRes
        //
        // NOTE 1: `let {_WRes, _} = apply ...` is invalid Core Erlang inside a fun body
        // (erlc rejects tuple patterns in let). Use element/2 extraction instead.
        // NOTE 2: In Core Erlang, `fun (Params) -> Body` does NOT use `end` to
        // terminate the fun — the Body expression ends the fun.

        let wrap_tuple = self.fresh_temp_var("WT");
        let wrap_result = self.fresh_temp_var("WRes");
        let wrapper_doc = docvec![
            "let ",
            leaf::var(bt_block_var.clone()),
            " = ",
            tier2_doc,
            " in ",
            Document::Vec(seed_docs),
            "fun (",
            Document::Vec(param_parts),
            ") -> let ",
            leaf::var(wrap_tuple.clone()),
            " = apply ",
            leaf::var(bt_block_var),
            " (",
            Document::Vec(apply_parts),
            ") in let ",
            leaf::var(wrap_result.clone()),
            " = call 'erlang':'element'(1, ",
            leaf::var(wrap_tuple),
            ") in ",
            leaf::var(wrap_result),
        ];

        Ok((wrapper_doc, true))
    }

    /// The names `expr` assigns (`x := ...`), not entering a block literal (a
    /// closure's assignments are its own business). Source order, once each.
    fn assigned_outside_closures(expr: &Expression) -> Vec<String> {
        let mut block_spans = Vec::new();
        beamtalk_core::ast_walker::walk_expression(expr, &mut |e| {
            if let Expression::Block(b) = e {
                block_spans.push(b.span);
            }
        });
        let mut names: Vec<String> = Vec::new();
        beamtalk_core::ast_walker::walk_expression(expr, &mut |e| {
            let Expression::Assignment { target, .. } = e else {
                return;
            };
            let Expression::Identifier(id) = target.as_ref() else {
                return;
            };
            let at = e.span();
            let in_block = block_spans
                .iter()
                .any(|b| b.start() <= at.start() && at.end() <= b.end());
            if !in_block && !names.iter().any(|n| n == id.name.as_str()) {
                names.push(id.name.to_string());
            }
        });
        names
    }

    /// BT-3939 (PIN-BUG, fixed by BT-3947's frame work): a Tier 2 block body
    /// lowers a loop or fold inside an opaque `let _ = ( ... )`, so the loop's
    /// rebind of a local the body bound (`n := 0. xs do: [:y | n := n + y]. n`)
    /// is scoped inside that statement: the block would answer the value, and
    /// the state, from before the loop, silently. Until the body is a frame
    /// that lowers the loop as a producer, that shape is a compile error.
    ///
    /// The rule is semantic, not syntactic: any name a construct reachable from
    /// `stmt` without crossing a closure (`reachable_threaded_locals`, the
    /// recognizer `threaded_locals_of` and the Phase 0 check share, so a loop
    /// nested in an operand, a conditional arm or a `[...] value` block is
    /// seen) threads, that this body itself bound (`bound_since`: assigned,
    /// destructured, however first bound) and that is neither a block parameter
    /// nor a captured-mutated variable the caller seeds into `StateAcc`.
    /// `scope_depth` is the variable-scope depth at the body's entry.
    fn reject_loop_rebind_of_body_local(
        &self,
        stmt: &Expression,
        block: &Block,
        captured_vars: &[String],
        scope_depth: usize,
    ) -> Result<()> {
        // An assignment's, destructuring's or `^`'s own write is not a
        // construct's: look at the value it holds.
        let subject = match stmt {
            Expression::Assignment { value, .. }
            | Expression::DestructureAssignment { value, .. }
            | Expression::Return { value, .. } => value.as_ref(),
            other => other,
        };
        let is_param = |n: &str| block.parameters.iter().any(|p| p.name == n);
        // A local assigned by this same statement before the construct runs
        // (`r := (n := 0) + (xs inject: 0 into: [... n := n + y ...])`, an
        // earlier keyword argument or cascade message) is not in scope yet
        // when the statement is lowered, so the scope sees neither the local
        // nor the construct's write to it. Conservatively, every name assigned
        // anywhere in the statement outside a closure counts as bound by the
        // body, and the one threaded-writes walker is asked with that scope.
        let assigned = Self::assigned_outside_closures(subject);
        let mut names = self.reachable_threaded_locals(subject);
        for write in beamtalk_core::semantic_analysis::block_facts::expression_threaded_writes(
            subject,
            &|name| self.binds_threadable_local(name) || assigned.iter().any(|a| a == name),
        ) {
            let name = write.name.to_string();
            if !names.contains(&name) {
                names.push(name);
            }
        }
        let Some(local) = names.into_iter().find(|n| {
            (self.bound_since(n, scope_depth) || assigned.contains(n))
                && !is_param(n)
                && !captured_vars.iter().any(|c| c == n)
        }) else {
            return Ok(());
        };
        Err(CodeGenError::UnsupportedFeature {
            feature: format!(
                "a Tier 2 block assigns local `{local}` and rebinds it inside a loop, fold or \
                 other construct; the block would answer the value, and the state, from \
                 before it (BT-3939, until BT-3947). Compute it in a method of its own and \
                 call that from the block"
            ),
            span: Some(subject.span()),
        })
    }

    /// Generates the body of a Tier 2 stateful block with state threading.
    ///
    /// ADR 0111 close-out (the last real production
    /// caller of `check_branch_frame_linearity`'s scalar-synthesis
    /// scaffolding): field-/local-var-assignment mutations lower
    /// through `conditionals.rs`'s `lower_field_assignment_bind`/
    /// `lower_local_var_assignment_bind` — real [`ThreadedStmt::Bind`]
    /// nodes, not a hand-rolled `maps:put` `Document` fragment — appended
    /// to `stmts` alongside every other (non-mutating) statement as an
    /// opaque [`ThreadedStmt::Statement`]. The two shared helpers only
    /// build the *mutation* itself; this function's own is-last/non-last
    /// result wrapping (the `maps:get` re-read on `is_last`, the
    /// `Refreshed` rebind on non-last local-var assignment) is unchanged —
    /// neither shape appears in `conditionals.rs`'s own C1/C2 arm closer,
    /// which uses the freshly-bound value var directly instead of
    /// re-reading it back out of the state map — so it stays here,
    /// modeled as its own `Statement`/[`ThreadedStmt::Return`], preserving
    /// the exact pre-migration bytes.
    #[allow(clippy::too_many_lines)]
    fn generate_block_stateful_body(
        &mut self,
        block: &Block,
        frame: threaded_ir::FrameId,
        captured_vars: &[String],
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<()> {
        let filtered_body = super::util::collect_body_exprs(&block.body);
        let scope_depth = self.scope_depth();

        for (i, expr) in filtered_body.iter().enumerate() {
            let is_last = i == filtered_body.len() - 1;
            let span = expr.span();
            self.reject_loop_rebind_of_body_local(expr, block, captured_vars, scope_depth)?;

            if self.is_field_assignment(expr) {
                let _val_var = self.lower_field_assignment_bind(expr, frame, span, stmts)?;
                if is_last {
                    // Return the assigned value and updated state.
                    // Extract the field name from the assignment target.
                    let final_version = self.state_version();
                    let state = self.current_state_var();
                    if let Expression::Assignment { target, .. } = expr {
                        if let Expression::FieldAccess { field, .. } = target.as_ref() {
                            stmts.push(ThreadedStmt::Return(
                                ValueRef::Doc(docvec![
                                    "call 'maps':'get'(",
                                    leaf::atom(field.name.to_string()),
                                    ", ",
                                    leaf::var(state),
                                    ")",
                                ]),
                                VersionedVar::new(VersionPrefix::State, final_version, frame),
                                span,
                            ));
                        }
                    }
                }
            } else if Self::is_local_var_assignment(expr) {
                let _val_var = self.lower_local_var_assignment_bind(expr, frame, span, stmts)?;
                if let Expression::Assignment { target, .. } = expr {
                    if let Expression::Identifier(id) = target.as_ref() {
                        let key = Self::local_state_key(&id.name);
                        if is_last {
                            // Last expression is an assignment — result is the assigned value.
                            // Read it from the updated state map (the _Val was put there).
                            let final_version = self.state_version();
                            let state = self.current_state_var();
                            stmts.push(ThreadedStmt::Return(
                                ValueRef::Doc(docvec![
                                    "call 'maps':'get'(",
                                    leaf::atom(key),
                                    ", ",
                                    leaf::var(state),
                                    ")",
                                ]),
                                VersionedVar::new(VersionPrefix::State, final_version, frame),
                                span,
                            ));
                        } else {
                            // Non-last assignment: refresh scope binding so subsequent reads
                            // of this variable see the updated value (not the initial unpack).
                            let fresh = self.fresh_temp_var("Refreshed");
                            self.bind_var(&id.name, &fresh);
                            let state = self.current_state_var();
                            stmts.push(ThreadedStmt::Statement(
                                docvec![
                                    "let ",
                                    leaf::var(fresh),
                                    " = call 'maps':'get'(",
                                    leaf::atom(key),
                                    ", ",
                                    leaf::var(state),
                                    ") in "
                                ],
                                span,
                            ));
                        }
                    }
                }
            } else if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                let (binding_docs, rhs_var) =
                    self.generate_destructure_bindings_with_value(pattern, value)?;
                for d in binding_docs {
                    stmts.push(ThreadedStmt::Statement(d, span));
                }
                if is_last {
                    // BT-3889: the block answers the right-hand side.
                    let final_version = self.state_version();
                    stmts.push(ThreadedStmt::Return(
                        ValueRef::Var(rhs_var),
                        VersionedVar::new(VersionPrefix::State, final_version, frame),
                        span,
                    ));
                }
            } else {
                // Non-assignment expression
                // ADR 0118 phase 2a: thread every state-effecting
                // sub-expression (`1 + (self bump)`) as real `Bind`s ahead
                // of the compile, via `thread_ahead` — the Tier 2
                // counterpart of `conditionals.rs`'s C12 catch-all.
                let hoist_scope = self.thread_ahead(expr, stmts, frame)?;
                // ADR 0118 phase 5b: `thread_ahead` already
                // threads any class-var producer nested in `expr` (at any
                // depth) as a real `Bind` in `stmts` above — the plain
                // compile below reads the substituted value back via
                // `precompiled_subexprs`, so no open scope reaches this
                // point any more.
                let doc = self.expression_doc(expr)?;
                self.finish_precompiled_scope(hoist_scope)?;
                if is_last {
                    // Wrap result in {Result, StateAcc} tuple
                    let final_version = self.state_version();
                    let result_var = self.fresh_temp_var("T2Res");
                    stmts.push(ThreadedStmt::Statement(
                        docvec!["let ", leaf::var(result_var.clone()), " = ", doc, " in "],
                        span,
                    ));
                    stmts.push(ThreadedStmt::Return(
                        ValueRef::Var(result_var),
                        VersionedVar::new(VersionPrefix::State, final_version, frame),
                        span,
                    ));
                } else {
                    stmts.push(ThreadedStmt::Statement(
                        docvec![Document::Str("let _ = "), doc, Document::Str(" in ")],
                        span,
                    ));
                }
            }
        }
        Ok(())
    }

    /// Generates the body of a block with proper state threading.
    ///
    /// Handles field assignments specially to keep State{n} variables in scope
    /// for subsequent expressions. See inline comments for threading details.
    pub(super) fn generate_block_body(&mut self, block: &Block) -> Result<Document<'static>> {
        if block.body.is_empty() {
            return Ok(Document::Str("'nil'"));
        }

        // Filter out @expect directives — they are compile-time only and generate no code.
        let body = super::util::collect_body_exprs(&block.body);

        self.generate_block_body_slice(&body)
    }

    /// Generates a slice of block body expressions with proper state threading.
    ///
    /// Handles field assignments specially to keep State{n} variables in scope
    /// for subsequent expressions. Called recursively for tuple destructuring.
    ///
    /// # State threading
    ///
    /// For a block like: `[self.value := self.value + 1. ^self.value]`
    /// We need:
    ///   `let _Val1 = ... in let State1 = ... in <return expression>`
    /// NOT:
    ///   `let _seq1 = (let _Val1 = ... in let State1 = ... in _Val1) in <return expression>`
    ///
    /// The difference is crucial: in the first form, `State1` is visible in `<return expression>`.
    ///
    /// For local variable assignments like: `[count := 0. count + 1]`
    /// We need:
    ///   `let Count = 0 in Count + 1`
    pub(super) fn generate_block_body_slice(
        &mut self,
        body: &[&Expression],
    ) -> Result<Document<'static>> {
        if body.is_empty() {
            return Ok(Document::Str("'nil'"));
        }

        let mut docs: Vec<Document<'static>> = Vec::with_capacity(body.len());

        for (i, expr) in body.iter().enumerate() {
            let is_last = i == body.len() - 1;
            let kind = Self::classify_block_expr(self, expr, is_last);
            let doc = self.generate_block_expr(expr, &kind)?;
            docs.push(doc);
        }

        Ok(Document::Vec(docs))
    }

    /// Classify a block body expression for dispatch.
    ///
    /// The order of checks matters: more specific patterns (e.g. destructuring,
    /// field assignment) must come before general ones (e.g. pure expression).
    ///
    /// `pub(super)` (ADR 0118 phase 3): `while_loops.rs`'s stateful
    /// while-condition compile reuses this dispatch for a condition block's
    /// own NON-TAIL statements (its local writes thread exactly as this
    /// already gives every other block body) rather than re-deriving the
    /// same classification — see `generate_stateful_while_condition`.
    pub(super) fn classify_block_expr(&self, expr: &Expression, is_last: bool) -> BlockExprKind {
        if matches!(expr, Expression::DestructureAssignment { .. }) {
            return BlockExprKind::Destructure { is_last };
        }

        if is_last {
            return BlockExprKind::LastExpr;
        }

        if self.is_field_assignment(expr) {
            return BlockExprKind::FieldAssignment;
        }

        if Self::is_local_var_assignment(expr) {
            return BlockExprKind::LocalAssignment;
        }

        if self
            .threaded_locals_of(expr)
            .and_then(ThreadedLocals::into_lowered)
            .is_some()
        {
            return BlockExprKind::ControlFlowWithThreadedVars;
        }

        BlockExprKind::SideEffect
    }

    /// Generate code for a single block body expression, dispatching by kind.
    ///
    /// `pub(super)` (ADR 0118 phase 3): see
    /// [`Self::classify_block_expr`]'s doc comment.
    pub(super) fn generate_block_expr(
        &mut self,
        expr: &Expression,
        kind: &BlockExprKind,
    ) -> Result<Document<'static>> {
        match *kind {
            BlockExprKind::Destructure { is_last } => {
                self.generate_block_destructure(expr, is_last)
            }
            BlockExprKind::LastExpr => {
                // Last expression: its value is the block's result. ADR 0118
                // phase 5b: `threaded_expression` closes any prelude (e.g. one
                // nested in a message's receiver/args) into a self-contained
                // `Document` so the block body is a complete closed
                // expression.
                let frame = self.current_frame();
                self.threaded_expression_doc(expr, frame)
            }
            BlockExprKind::FieldAssignment => {
                // Field assignment not at end: generate WITHOUT closing the value.
                // This leaves the let bindings open for subsequent expressions.
                let (doc, _val_var) = self.generate_field_assignment_open(expr)?;
                Ok(doc)
            }
            BlockExprKind::LocalAssignment => self.generate_block_local_assignment(expr),
            BlockExprKind::ControlFlowWithThreadedVars => {
                self.generate_block_control_flow_threaded(expr)
            }
            BlockExprKind::SideEffect => {
                // Not an assignment or loop — generate and discard the
                // result. ADR 0118 phase 5b: `threaded_expression`
                // threads a producer (or one nested in a message's
                // receiver/args) as a real prelude, closed here
                // into a self-contained `Document` since a Tier 1 block body
                // is a flat statement sequence with no `ThreadedIr` frame of
                // its own to splice into.
                let frame = self.current_frame();
                let tv = self.threaded_expression(expr, frame)?;
                let prelude_doc = self.threaded_prelude_doc(&tv.prelude);
                let value_doc = self.threaded_value_doc(&tv.value);
                Ok(docvec![prelude_doc, "let _Unit = ", value_doc, " in "])
            }
        }
    }

    /// Handle destructure assignment expressions in block bodies, through
    /// the shared destructuring extraction. When `is_last` is true the block
    /// answers the right-hand side: a destructuring assignment evaluates to it
    /// (BT-3889).
    fn generate_block_destructure(
        &mut self,
        expr: &Expression,
        is_last: bool,
    ) -> Result<Document<'static>> {
        let Expression::DestructureAssignment { pattern, value, .. } = expr else {
            unreachable!("caller guarantees DestructureAssignment");
        };
        let (mut docs, rhs_var) = self.generate_destructure_bindings_with_value(pattern, value)?;
        if is_last {
            docs.push(leaf::var(rhs_var));
        }
        Ok(Document::Vec(docs))
    }

    /// Handle local variable assignment in a block body (non-last position).
    fn generate_block_local_assignment(&mut self, expr: &Expression) -> Result<Document<'static>> {
        let Expression::Assignment { target, value, .. } = expr else {
            return Ok(Document::Nil);
        };
        let Expression::Identifier(id) = target.as_ref() else {
            return Ok(Document::Nil);
        };

        // Stored blocks with mutations are supported via Tier 2.
        // generate_block() handles stateful emission; no validation needed here.

        let var_name = &id.name;
        // Determine the Core Erlang variable name:
        // - If the variable is already bound (e.g. block parameter), reuse that Core var.
        // - Otherwise, create a new Core Erlang variable name.
        let core_var = self
            .lookup_var(var_name)
            .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
        // Capture the value expression (preserves side effects)
        // Important: capture BEFORE updating the mapping,
        // so that any uses of the variable in the RHS see the previous binding.
        //
        // A class method threads nothing (ADR 0130 §3): `value` is compiled as
        // an ordinary expression and bound, with no prelude of its own.
        if self.in_class_method() {
            let val_doc = self.expression_doc(value)?;
            self.bind_var(var_name, &core_var);
            return Ok(docvec!["let ", leaf::var(core_var), " = ", val_doc, " in ",]);
        }
        let frame = self.current_frame();
        let tv = self.threaded_expression(value, frame)?;
        let prelude_doc = self.threaded_prelude_doc(&tv.prelude);
        let value_doc = self.threaded_value_doc(&tv.value);
        // Now update the mapping so subsequent expressions see this binding.
        self.bind_var(var_name, &core_var);
        Ok(docvec![
            prelude_doc,
            "let ",
            leaf::var(core_var),
            " = ",
            value_doc,
            " in "
        ])
    }

    /// Handle a non-last block-body statement that is a mutating control-flow
    /// construct (`whileTrue:`/`whileFalse:`/`timesRepeat:`/loops, `ifTrue:`/
    /// `ifFalse:`/`ifTrue:ifFalse:`/`ifNotNil:`, `on:do:`/`ensure:`, …) — one
    /// this block's own top-level [`BlockMutationAnalysis`] doesn't classify
    /// as captured-mutating (see [`Self::threaded_locals_of`]),
    /// so the enclosing block still compiled as a plain (non-`StateAcc`) fun.
    ///
    /// Every one of these constructs' `generate_*_with_mutations`
    /// generator returns a `{Result, StateAcc}` 2-tuple (`StateAcc` a map
    /// keyed by [`Self::local_state_key`]) — `expr` alone is never the bare
    /// scalar value. `element(2, ...)` + `maps:get` unpacks it the same way
    /// the actor method-body sequencer's own "real state Bind" arm does
    /// (`gen_server/methods.rs`); binding `core_var` directly to the raw
    /// tuple (the pre-fix behavior) silently produced a `{Val, Map}` pair
    /// where a scalar was expected, corrupting any later read of `var`.
    ///
    /// For multiple vars, we'd need to unpack more than one key — not yet
    /// supported.
    fn generate_block_control_flow_threaded(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        let threaded_vars = self
            .threaded_locals_of(expr)
            .and_then(ThreadedLocals::into_lowered)
            .expect("caller guarantees control flow with threaded vars");

        if threaded_vars.len() == 1 {
            let var = &threaded_vars[0];
            // Get the Core Erlang variable name for this var
            let core_var = self
                .lookup_var(var)
                .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
            let expr_doc = self.expression_doc(expr)?;
            let tuple_var = self.fresh_temp_var("CtrlFlowResult");
            let state_var = self.fresh_temp_var("CtrlFlowState");
            Ok(docvec![
                "let ",
                leaf::var(tuple_var.clone()),
                " = ",
                expr_doc,
                " in let ",
                leaf::var(state_var.clone()),
                " = call 'erlang':'element'(2, ",
                leaf::var(tuple_var),
                ") in let ",
                leaf::var(core_var),
                " = call 'maps':'get'(",
                leaf::atom(Self::local_state_key(var)),
                ", ",
                leaf::var(state_var),
                ") in "
            ])
        } else {
            // Multi-var case not supported yet
            Err(CodeGenError::UnsupportedFeature {
                feature: "Multiple threaded variables in control flow".to_string(),
                span: Some(expr.span()),
            })
        }
    }
}

#[cfg(test)]
mod tests;
