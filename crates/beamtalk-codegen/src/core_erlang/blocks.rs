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

use super::control_flow::ArmFrame;
use super::threaded_ir::{self, ThreadedStmt, ValueRef, VersionPrefix, VersionedVar};
use super::{CodeGenError, CoreErlangGenerator, Result};
use crate::core_erlang::threading_analysis::ThreadedLocals;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Block, Expression};
use beamtalk_core::source_analysis::Span;

/// A merged Tier 2 `fun` being lowered (BT-4122): the locals its body
/// threads through its `StateAcc` parameter/return. Its fields go
/// straight to the cell. (A Tier 2 call inside the fun cannot happen:
/// a use inside a nested block is never a safe use, so no Tier 2 local
/// is promotable there.)
#[derive(Clone)]
pub(in crate::core_erlang) struct Tier2MergedFun {
    /// Captured locals the fun threads (its `captured_vars`).
    pub(in crate::core_erlang) captured: Vec<String>,
}

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
    /// A construct that threads outer locals (non-last) but is not a
    /// local-threading producer in this frame: no lowering rebinds what it
    /// threads, so it is a compile error rather than a dropped write. The Phase
    /// 0 check rejects every such shape before codegen (an agreement test pins
    /// the two rules); BT-3912 deleted the single-local unpack that used to
    /// handle it.
    ThreadedConstructNotAProducer,
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
    /// Centralised here so both `generate_block` and `generate_erlang_interop_block`
    /// use identical logic and cannot drift independently.
    pub(super) fn captured_mutations_for_block(block: &Block) -> Vec<String> {
        use crate::core_erlang::block_analysis::analyze_block;
        Self::captured_mutations_from_analysis(&analyze_block(block))
    }

    /// Whether a Tier 2 `fun` for a block with this analysis takes the
    /// merged shape (BT-4122): a cell-mode body for fields, `StateAcc` for
    /// locals only. Actor instance methods only — anywhere else (and under
    /// the legacy fault) Tier 2 keeps threading everything through
    /// `StateAcc`, which is self-consistent there.
    pub(super) fn tier2_merged_for_analysis(
        &self,
        analysis: &super::block_analysis::BlockMutationAnalysis,
    ) -> bool {
        #[cfg(test)]
        let fault_legacy = super::synced_call::FAULT_LEGACY_TIER2.with(std::cell::Cell::get);
        #[cfg(not(test))]
        let fault_legacy = false;
        if fault_legacy {
            return false;
        }
        self.synced_calls_scope() && analysis.may_write_actor_state()
    }

    /// Same check as [`Self::captured_mutations_for_block`], but from an already-computed
    /// analysis — lets callers that need more than one fact off a single `analyze_block`
    /// pass (e.g. [`Self::generate_block`], which also checks `field_writes`) avoid
    /// re-walking the block's AST.
    pub(super) fn captured_mutations_from_analysis(
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
    /// Field writes (`self.x := ...`) in an actor closure go through the
    /// actor-state cell (ADR 0137); self-sends in one are ordinary messages.
    ///
    /// ADR 0130 §5: a block literal that reads a class variable binds its
    /// creation-time capture around the `fun` ([`Self::with_class_var_capture`]).
    pub(super) fn generate_block(&mut self, block: &Block) -> Result<Document<'static>> {
        self.with_actor_closure(block.span, |this| {
            this.with_class_var_capture(block, |this| this.generate_block_fun(block))
        })
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
        // ADR 0137: not in an actor instance method either, where the block
        // writes the actor's state through the cell.
        if !analysis.field_writes.is_empty() && !self.in_class_method() {
            if self.cell_closures_apply() {
                // The field write is legal; a captured local the block also
                // mutates is not (ADR 0133: nothing hands it back).
                let mut locals_only = analysis.clone();
                locals_only.field_writes.clear();
                Self::validate_stored_closure(&locals_only, || self.location_label(block.span))?;
            } else {
                Self::validate_stored_closure(&analysis, || self.location_label(block.span))?;
            }
        }

        let captured_mutations = Self::captured_mutations_from_analysis(&analysis);

        // Blocks with captured local mutations use Tier 2 stateful calling convention.
        if !captured_mutations.is_empty() {
            // BT-4122: a may-write block lowers merged (cell mode for
            // fields); the actor closure is already pushed by
            // `generate_block`, so the merged builder reuses it.
            if self.tier2_merged_for_analysis(&analysis) {
                return self.generate_block_merged_fun(block, &captured_mutations);
            }
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
    /// `conditionals.rs`'s `render_branch_arm`. `UnboundVersion`
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
        // BT-4122 (ADR 0137 Tier 2 merge): a Tier 2 `fun` that may write the
        // actor's fields lowers merged — a cell-mode body for fields with
        // the captured locals unpacked from `StateAcc` on entry and flushed
        // back on exit — instead of threading fields through `StateAcc`.
        // Anywhere else a Tier 2 `fun` takes the caller's `State` as its
        // `StateAcc` argument and answers it back (`{Result, NewStateAcc}`),
        // the convention of a compiled method, threading fields and locals
        // through that map like one.
        let analysis = super::block_analysis::analyze_block(block);
        if self.tier2_merged_for_analysis(&analysis) {
            return self.with_class_var_capture(block, |this| {
                this.with_actor_closure(block.span, |this| {
                    this.generate_block_merged_fun(block, captured_vars)
                })
            });
        }
        self.with_class_var_capture(block, |this| {
            this.generate_block_stateful_fun(block, captured_vars)
        })
    }

    /// One `let` unpacking a captured local from the Tier 2 `StateAcc`
    /// parameter (falling back to the creation-site value), binding its
    /// Core var. Shared by the legacy and merged Tier 2 funs (BT-4122).
    fn tier2_entry_unpack_doc(&mut self, var_name: &str) -> Document<'static> {
        let core_var = Self::to_core_erlang_var(var_name);
        let key = Self::local_state_key(var_name);
        // Look up the outer binding BEFORE rebinding so the fallback refers to the
        // value at block-definition time (not the newly introduced inner binding).
        // If no outer binding exists (e.g. REPL mode where vars come from state dict),
        // fall back to maps:get/2 (original behavior) to avoid referencing unbound vars.
        let outer_binding = self.lookup_var(var_name).cloned();
        self.bind_var(var_name, &core_var);
        if let Some(outer_var) = outer_binding {
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
        }
    }

    /// [`Self::generate_block_stateful`] without the capture binding.
    fn generate_block_stateful_fun(
        &mut self,
        block: &Block,
        captured_vars: &[String],
    ) -> Result<Document<'static>> {
        // BT-4122: mark the body so field access lowerings can report the
        // `FieldReadFromStateAcc` fun-body arm. Save/restore: Tier 2 nests.
        let saved = std::mem::replace(&mut self.in_tier2_stateful_body, true);
        let out = self.generate_block_stateful_fun_inner(block, captured_vars);
        self.in_tier2_stateful_body = saved;
        out
    }

    /// [`Self::generate_block_stateful_fun`] without the body mark.
    fn generate_block_stateful_fun_inner(
        &mut self,
        block: &Block,
        captured_vars: &[String],
    ) -> Result<Document<'static>> {
        self.push_scope();
        // Track block nesting so self-cast sends route through the mailbox
        self.block_depth += 1;
        // A Tier 2 body is its own bracket scope: a window open around the call
        // this block is an operand of does not cover the body's own sends, so they
        // are checked against the body's brackets (ADR 0137).
        let outer_window_depth = std::mem::replace(&mut self.synced.window_depth, 0);

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
                let unpack_doc = this.tier2_entry_unpack_doc(var_name);
                stmts.push(ThreadedStmt::Statement(unpack_doc, block.span));
            }

            // BT-3939: the body is an arm frame (ADR 0131 §2, `BranchArm`)
            // that carries every local its `StateAcc` scratch map holds: the
            // captured-mutated ones unpacked above and the block's own
            // top-level assignments. A loop or fold statement of the body is
            // then a local-threading producer whose `LocalRebind`s this frame
            // lowers (a `Put` into the map for a carried local), spliced into
            // the body's statement list — not scoped inside an opaque
            // `let _ = ( ... )` that drops the post-loop local and state.
            let body = super::util::collect_body_exprs(&block.body);
            let arm = ArmFrame {
                threads: Self::tier2_body_threads(captured_vars, &body),
            };
            let rebind = threaded_ir::RebindFrame {
                frame,
                kind: threaded_ir::RebindFrameKind::BranchArm,
                threads: arm.threads.clone(),
            };
            this.with_rebind_frame(rebind, |this| {
                // Test-only fault (`Tier2OpaqueProducers`): the producer sites
                // BT-3939 registers, withheld.
                #[cfg(test)]
                if super::threaded_expr::fault::active()
                    == Some(super::threaded_expr::fault::LocalRebindFault::Tier2OpaqueProducers)
                {
                    return this.generate_block_stateful_body(block, frame, &mut stmts);
                }
                // Generate body expressions with state threading
                if this.is_repl_mode() {
                    return this.generate_block_stateful_body(block, frame, &mut stmts);
                }
                this.with_body_producer_sites(&body, false, true, |this| {
                    this.generate_block_stateful_body(block, frame, &mut stmts)
                })
            })?;
            let branch_final = this.state_version();
            Ok::<_, crate::core_erlang::CodeGenError>(this.render_branch_arm(
                stmts,
                frame,
                branch_final,
                block.span,
                Some(&arm),
            ))
        });
        // Ensure block_depth and scope are restored even on error.
        self.synced.window_depth = outer_window_depth;
        self.block_depth -= 1;
        self.pop_scope();
        // Stateful blocks are also closed `fun (...) -> {Result, NewStateAcc}`
        // expressions and must not propagate an open scope from their body to
        // the outer context.
        self.loop_mode.direct_params_do_open_chain = false;

        let (body_doc, _branch_final) = result?;

        Ok(docvec![header, body_doc])
    }

    /// Builds a merged Tier 2 `fun` (BT-4122, ADR 0137 Tier 2 merge): the
    /// same `(params, StateAcc) -> {Result, StateAcc}` protocol as the
    /// legacy one, but the body lowers Tier-1-style — fields through the
    /// cell — with the captured locals unpacked from `StateAcc` on entry
    /// and flushed back on exit. The actor closure must already be pushed
    /// (by `generate_block`, or by `generate_block_stateful` for the
    /// prescan path) — see [`Tier2MergedFun`].
    fn generate_block_merged_fun(
        &mut self,
        block: &Block,
        captured_vars: &[String],
    ) -> Result<Document<'static>> {
        self.with_closed_fun_scope(|this| {
            // Track block nesting so self-cast sends route through the mailbox
            this.block_depth += 1;
            // Tier 2 marks (save/restore: Tier 2 nests — no `?`
            // between, so the error path restores too, like the legacy
            // builder below).
            let saved_body = std::mem::replace(&mut this.in_tier2_stateful_body, true);
            let saved_merged = this.tier2_merged_fun.replace(Tier2MergedFun {
                captured: captured_vars.to_vec(),
            });
            let result = this.generate_block_merged_fun_inner(block, captured_vars);
            this.tier2_merged_fun = saved_merged;
            this.in_tier2_stateful_body = saved_body;
            this.block_depth -= 1;
            result
        })
    }

    /// [`Self::generate_block_merged_fun`] without the scope and marks.
    fn generate_block_merged_fun_inner(
        &mut self,
        block: &Block,
        captured_vars: &[String],
    ) -> Result<Document<'static>> {
        // Parameters (mirrors Tier 1), plus the `StateAcc` accumulator map.
        let mut param_parts: Vec<Document<'static>> = Vec::new();
        for (i, param) in block.parameters.iter().enumerate() {
            if i > 0 {
                param_parts.push(Document::Str(", "));
            }
            let var_name = self.fresh_var(&param.name);
            param_parts.push(leaf::var(var_name));
        }
        if !block.parameters.is_empty() {
            param_parts.push(Document::Str(", "));
        }
        param_parts.push(Document::Str("StateAcc"));
        let header = docvec!["fun (", Document::Vec(param_parts), ") -> "];

        // Entry: unpack the captured locals from `StateAcc`.
        let mut prefix: Vec<Document<'static>> = Vec::with_capacity(captured_vars.len());
        for var_name in captured_vars {
            prefix.push(self.tier2_entry_unpack_doc(var_name));
        }

        // Body, Tier-1-style: fields through the cell, locals as Core vars.
        let body_doc = self.generate_block_body(block)?;

        // Exit: the body wrote fields straight to the cell, so the
        // threaded accumulator holds stale fields — take the live cell,
        // flush the current Core-var values into it, store the merged map
        // back (the operand bracket's `take()` reads it there), and answer
        // it. Both callers — the statement unpacker (element 2) and the
        // operand bracket (the cell) — see one complete state. (Mirrors the
        // `^` flush in `mod.rs`, which throws past this exit.)
        let take_var = self.fresh_temp_var("T2Cell");
        let (flush_doc, final_acc) = self.tier2_flush_locals_doc(&take_var, captured_vars);
        let result_var = self.fresh_temp_var("T2Res");
        let fun_doc = docvec![
            header,
            Document::Vec(prefix),
            "let ",
            leaf::var(result_var.clone()),
            " = ",
            body_doc,
            " in let ",
            leaf::var(take_var.clone()),
            " = ",
            threaded_ir::cell_take_call(),
            " in ",
            flush_doc,
            "let _ = ",
            threaded_ir::cell_put_call(leaf::var(final_acc.clone())),
            " in ",
            "{",
            leaf::var(result_var),
            ", ",
            leaf::var(final_acc),
            "}",
        ];

        // No capture binding here (mirrors Tier 1): the body references
        // the pushed closure's capture directly, and `with_actor_closure`
        // binds `ASCapture` itself when nested.
        Ok(fun_doc)
    }

    /// The `FieldReadFromStateAcc` fun-body read arm (BT-4122): a Tier 2
    /// `fun` reads this field through its `StateAcc`. Merged bodies never
    /// reach here (their pushed actor closure routes reads through the
    /// cell first), so a hit is a legacy `fun`, which drops field writes
    /// in positions that discard its return — loud, forcing the merged
    /// shape.
    pub(super) fn note_tier2_fun_body_field_read(&mut self, name: &str, span: Span) {
        if self.in_tier2_stateful_body && self.synced_calls_scope() {
            self.record_tier2_stateacc_gap(
                format!("{name} (tier2 fun body reads field via StateAcc)"),
                span,
            );
        }
    }

    /// Opaque `let`s putting `captured` locals' current Core-var values
    /// into the map `base_var`, answering the lets and the final variable.
    /// The merged Tier 2 `fun`'s exit flush (and its `^` flush) share this:
    /// fresh temps throughout, so no `State` version is minted or consumed.
    pub(super) fn tier2_flush_locals_doc(
        &mut self,
        base_var: &str,
        captured: &[String],
    ) -> (Document<'static>, String) {
        let mut acc = base_var.to_string();
        let mut parts: Vec<Document<'static>> = Vec::new();
        for var_name in captured {
            // The entry unpack bound every captured local; nested scopes
            // rebind by shadowing, so the innermost binding is the live value.
            let core_var = self
                .lookup_var(var_name)
                .cloned()
                .unwrap_or_else(|| Self::to_core_erlang_var(var_name));
            let next = self.fresh_temp_var("T2Acc");
            parts.push(docvec![
                "let ",
                leaf::var(next.clone()),
                " = call 'maps':'put'(",
                leaf::atom(Self::local_state_key(var_name)),
                ", ",
                leaf::var(core_var),
                ", ",
                leaf::var(acc.clone()),
                ") in ",
            ]);
            acc = next;
        }
        (Document::Vec(parts), acc)
    }

    /// Generates a block literal handed to an Erlang call site (an FFI
    /// argument, or the callable of `lists:foreach/map/filter`).
    ///
    /// Erlang calls the block as a plain fun and cannot carry the Tier 2
    /// `StateAcc` protocol back, so only a Tier 1 block may cross. A block
    /// literal that writes a captured local is rejected at compile time by
    /// semantic analysis (ADR 0133 `Escape::Ffi`, BT-3917) and the list ops
    /// route a mutating block down their `_with_mutations` path first, so a
    /// block that reaches here with captured mutations is a compiler bug:
    /// it raises an internal error instead of emitting a wrapper that would
    /// silently drop the writes (BT-3986 removed that wrapper).
    pub(super) fn generate_erlang_interop_block(
        &mut self,
        block: &Block,
    ) -> Result<Document<'static>> {
        let captured_mutations = Self::captured_mutations_for_block(block);
        if !captured_mutations.is_empty() {
            return Err(CodeGenError::Internal(format!(
                "a block literal that writes the captured local(s) {} reached an Erlang call \
                 site; semantic analysis rejects it (ADR 0133 `Escape::Ffi`, BT-3917)",
                captured_mutations.join(", ")
            )));
        }
        self.generate_block(block)
    }

    /// The locals a Tier 2 block body's `StateAcc` scratch map carries (the
    /// `threads` of its `BranchArm` frame): the captured-mutated locals the
    /// caller seeded, then the block's own top-level assignment targets, each
    /// once, in first-appearance order.
    fn tier2_body_threads(captured_vars: &[String], body: &[&Expression]) -> Vec<String> {
        let mut threads: Vec<String> = Vec::new();
        let assigned = body.iter().filter_map(|stmt| match stmt {
            Expression::Assignment { target, .. } => match target.as_ref() {
                Expression::Identifier(id) => Some(id.name.to_string()),
                _ => None,
            },
            _ => None,
        });
        for name in captured_vars.iter().cloned().chain(assigned) {
            if !threads.contains(&name) {
                threads.push(name);
            }
        }
        threads
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
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<()> {
        let filtered_body = super::util::collect_body_exprs(&block.body);
        // BT-3939: the producer sites this body threads a local through,
        // read off the AST just before the statement that holds them is
        // lowered (a local bound by an earlier statement is live), and checked
        // against the lowered IR once the body is built.
        let mut producer_sites: Vec<(beamtalk_core::source_analysis::Span, Vec<String>)> =
            Vec::new();

        for (i, expr) in filtered_body.iter().enumerate() {
            let is_last = i == filtered_body.len() - 1;
            let span = expr.span();
            // (Not in the REPL: its producers adopt the bindings map, which a
            // stored block's `StateAcc` is not, and no source reaches a Tier 2
            // body there: a block that writes a captured binding is an ADR 0133
            // error.)
            producer_sites.extend(self.frame_producer_sites(expr, false, true));

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
                // BT-3982: the right-hand side is sequenced (`thread_ahead`),
                // so a producer there (an evaluated block that threads
                // `State`) lands its carrier and `Bind` in this body instead
                // of destructuring its raw `{Value, State}` tuple.
                let (binding_docs, rhs_var) =
                    self.generate_destructure_bindings_sequenced(pattern, value, frame, stmts)?;
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
        let errors = threaded_ir::verify_producer_sites_threaded(stmts, &producer_sites);
        self.report_threaded_ir_verify_errors(
            &errors,
            "a Tier 2 block body's local-threading producer must be lowered as a producer",
            block.span,
        );
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

        // BT-3912: a plain closure's body (a pure block, a fold lambda, a
        // loop-condition closure that threads nothing) is a frame of its own, so
        // a loop, fold, conditional or exception construct in it is a producer
        // that rebinds the closure's locals with plain `let`s.
        self.with_closure_body_frame(&body, |this| this.generate_block_body_slice(&body))
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

        // BT-3912: a construct that is a local-threading producer in this frame
        // (a plain closure body is one) is lowered by `threaded_expression`,
        // which splices its construct tuple and rebinds every local it
        // threads; the single-local unpack below is for what is not.
        if self.local_producer_locals(expr).is_some() {
            return BlockExprKind::SideEffect;
        }

        if self
            .threaded_locals_of(expr)
            .and_then(ThreadedLocals::into_lowered)
            .is_some()
        {
            return BlockExprKind::ThreadedConstructNotAProducer;
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
                let tv = self.threaded_expression(expr, frame)?;
                let prelude_doc = self.closure_prelude_doc(&tv.prelude, expr.span());
                let value_doc = self.threaded_value_doc(&tv.value);
                Ok(docvec![prelude_doc, value_doc])
            }
            BlockExprKind::FieldAssignment => {
                // Field assignment not at end: generate WITHOUT closing the value.
                // This leaves the let bindings open for subsequent expressions.
                let (doc, _val_var) = self.generate_field_assignment_open(expr)?;
                Ok(doc)
            }
            BlockExprKind::LocalAssignment => self.generate_block_local_assignment(expr),
            BlockExprKind::ThreadedConstructNotAProducer => Err(CodeGenError::Internal(
                "ADR 0133: a construct in a block body threads outer locals but is not a \
                 local-threading producer in its frame (the Phase 0 check rejects this)"
                    .to_string(),
            )),
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
                let prelude_doc = self.closure_prelude_doc(&tv.prelude, expr.span());
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
        if self.threads_no_actor_state() {
            let val_doc = self.expression_doc(value)?;
            self.bind_var(var_name, &core_var);
            return Ok(docvec!["let ", leaf::var(core_var), " = ", val_doc, " in ",]);
        }
        let frame = self.current_frame();
        let tv = self.threaded_expression(value, frame)?;
        let prelude_doc = self.closure_prelude_doc(&tv.prelude, expr.span());
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
}

#[cfg(test)]
mod tests;
