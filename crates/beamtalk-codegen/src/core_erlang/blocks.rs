// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Block (closure) code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! BT-3465: split out of `expressions.rs`, no logic changes. This module
//! handles code generation for Beamtalk blocks (closures):
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
use super::util::index_lit;
use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result};
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::{Block, Expression, Pattern};

/// Classification of how a block body expression should be handled.
/// Produced by [`CoreErlangGenerator::classify_block_expr`] and consumed
/// by [`CoreErlangGenerator::generate_block_expr`]. `pub(super)`: both are
/// also called from `while_loops.rs` (ADR 0118 phase 3, BT-3419) — see
/// their own doc comments.
pub(super) enum BlockExprKind {
    /// `{a, b} := expr` or `#[a, b] := expr` — destructure assignment.
    /// Carries `is_last` so the handler can append `'nil'` when the destructure
    /// is the final expression in the block.
    Destructure { is_last: bool },
    /// Last expression that is a class method self-send (BT-1397).
    LastClassMethodSelfSend,
    /// Last expression (general case) — its value is the block's result.
    LastExpr,
    /// `self.field := value` — direct field assignment (non-last).
    FieldAssignment,
    /// `var := expr` — local variable assignment (non-last).
    LocalAssignment,
    /// `whileTrue:` / `whileFalse:` / `timesRepeat:` with threaded vars (non-last).
    ControlFlowWithThreadedVars,
    /// Class method self-send as non-last expression (BT-1397).
    ClassMethodSelfSend,
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
        analysis
            .local_writes
            .intersection(&analysis.captured_reads)
            .cloned()
            .collect::<std::collections::BTreeSet<_>>()
            .into_iter()
            .collect()
    }

    /// BT-1213: Returns captured mutation variable names if `expr` is a
    /// `[block] value`/`value:`/etc. with a literal block that mutates outer locals.
    pub(super) fn inline_block_captured_mutations(expr: &Expression) -> Option<Vec<String>> {
        if let Expression::MessageSend {
            receiver, selector, ..
        } = expr
        {
            let is_value_selector = match selector {
                beamtalk_core::ast::MessageSelector::Unary(name) => name == "value",
                beamtalk_core::ast::MessageSelector::Keyword(parts) => {
                    let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
                    matches!(
                        sel.as_str(),
                        "value:" | "value:value:" | "value:value:value:"
                    )
                }
                beamtalk_core::ast::MessageSelector::Binary(_) => false,
            };
            if is_value_selector {
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

    /// BT-3151: Rejects a same-class self-send inside a block whose target
    /// selector isn't provably free of class-variable mutation (see
    /// `ClassMethodSelfSendInUnthreadedBlock`'s doc comment for the full
    /// rationale) — such a block has no way to thread a classState mutation
    /// back to the class method that owns it, silently losing it otherwise.
    ///
    /// Scoped to class-method context only (this is a classState concern,
    /// not an actor-state one), and gated on the class actually declaring
    /// class variables: with none, there is no classState a self-send could
    /// possibly lose, so the conservative "not defined locally" fallback
    /// below (which can't see inherited methods — e.g. `Actor`'s
    /// `spawnWith:` called from a native Actor subclass with no
    /// `classState:` of its own, like `Subprocess`) would otherwise reject
    /// sends to safe inherited methods it has no way to prove safe. See
    /// `class_var_names`.
    ///
    /// Deliberately NOT called from `generate_block` itself — that function
    /// is the universal block-to-closure compiler, reached from contexts
    /// that are safe (a block passed to a *different* class's class-side
    /// method always runs in that class's own `gen_server` process, so a
    /// same-class self-send inside it is genuine cross-process messaging,
    /// not the lossy in-process direct-call optimization — see ADR 0110
    /// BT-3039 / `shadow_cross_class_owner.bt`) or merely unproven (an
    /// `ifTrue:`/`ifFalse:` block reached via generic dynamic dispatch is a
    /// long-documented ADR 0110 "known limitation" (BT-1550), not something
    /// this guard introduces). `generate_block` has no way to tell those apart
    /// from its own call site. Instead, called individually from each
    /// call site *confirmed* unsafe (same-process, in-process self-send,
    /// mutation empirically lost). Most list-op call sites share this via
    /// `check_bare_list_op_block_self_sends` (`control_flow/list_ops/mod.rs`)
    /// — `do:`/`collect:`/`select:`/`reject:`/`detect:`/`detect:ifNone:`/
    /// `anySatisfy:`/`allSatisfy:`/`count:`/`flatMap:`/`takeWhile:`/
    /// `dropWhile:`/`partition:`/`groupBy:`/`sort:`, plus the
    /// `eachWithIndex:`/`do:separatedBy:` desugar fallbacks
    /// (`enumeration_ops.rs`) — every one a bare, no-mutation-threading
    /// block that falls through to a plain/BIF dispatch. Called directly
    /// (not through that shared helper) at three shapes it doesn't cover:
    /// `generate_list_inject`'s BT-1327 pure-block fast path (bypasses
    /// `generate_block` entirely — calls `generate_block_body` directly to
    /// avoid wrapper overhead), a `whileTrue:`/`whileFalse:` condition
    /// block, a bare `timesRepeat:`/`to:do:`/`to:by:do:` body that falls
    /// through to the stdlib's own `Integer`/value-type loop implementation,
    /// and a block argument crossing the Erlang interop boundary in a
    /// direct `(Erlang mod) fn: arg` call (`generate_direct_erlang_call`'s
    /// keyword branch, `dispatch_codegen.rs`) — same
    /// `generate_erlang_interop_wrapper` → `generate_block` mechanism as
    /// the list-op call sites above.
    pub(super) fn check_no_unsafe_class_method_self_sends(
        &self,
        analysis: &crate::core_erlang::block_analysis::BlockMutationAnalysis,
        span: beamtalk_core::source_analysis::Span,
    ) -> Result<()> {
        if !self.in_class_method() || self.class_var_names().is_empty() {
            return Ok(());
        }
        let mut unsafe_selectors: Vec<&String> = analysis
            .self_send_selectors
            .iter()
            .filter(|sel| {
                self.class_var_mutating_selectors().contains(sel.as_str())
                    || !self.class_method_selectors().contains(sel.as_str())
            })
            .collect();
        if let Some(selector) = {
            unsafe_selectors.sort_unstable();
            unsafe_selectors.into_iter().next()
        } {
            return Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock {
                selector: selector.clone(),
                location: self.location_label(span),
            });
        }
        Ok(())
    }

    // BT-3430 (ADR 0118 §Decision 5 follow-up — design decision, not yet
    // implemented; see this issue): investigated routing this predicate's
    // ~10 call sites through `ThreadedValue::close(ctx, CloseContext::Opaque)`
    // / `VerifyError::StateEffectEscapesExpression` instead of (or on top
    // of) `class_var_mutating_selectors()` above. Kept separate — full
    // finding below.
    //
    // **Where things actually stand** (re-verified against this repo state,
    // not the BT-3422 issue body's forward-looking sketch):
    // `generate_class_method_self_send` (`dispatch_codegen.rs`) already
    // returns a real `ThreadedValue` (ADR 0118 phases 5a/5b, BT-3421/
    // BT-3422) — its `ClassVars` `Bind` is a genuine, un-rendered
    // `ThreadedStmt::Bind` in the prelude, not baked eagerly into a
    // `Document`. The *ambient* re-entry point every self-send reached via
    // ordinary (non-`threaded_expression`) `generate_expression` funnels
    // through — `try_handle_class_method_self_send` →
    // `Self::close_threaded_value_doc` (`util.rs`) — already exists too.
    // But `close_threaded_value_doc` deliberately does NOT call
    // `ThreadedValue::close`: it renders prelude-then-value unconditionally,
    // preserving "the pre-ADR-0118 open-let-chain's own contract" (its own
    // doc comment) for EVERY ambient self-send site alike, trusting this
    // predicate to have already rejected any block where that would be
    // wrong.
    //
    // **Why `close_threaded_value_doc` can't just start calling `close()`
    // with `CloseContext::Opaque`.** It is one shared choke point reached
    // from self-sends in BOTH kinds of position this predicate's own doc
    // comment distinguishes: the safe/unproven ones (a class method's own
    // flat top level; a block passed to a *different* class's class-side
    // method, genuine cross-process messaging — `shadow_cross_class_owner.bt`
    // guards this one) and the confirmed-unsafe bare-block ones this
    // predicate rejects pre-flight. Telling them apart at
    // `close_threaded_value_doc`'s call time — after arbitrary-depth
    // `generate_expression` recursion, with no record of which message send
    // the innermost enclosing block literal is even an argument to — means
    // re-deriving the SAME receiver-class-identity classification each of
    // this predicate's ~10 call sites already computes once, per block
    // literal, with the actual message send in hand. A generator-side
    // context flag threaded through that recursion cannot easily stay
    // correct across a *nested* block of the opposite safety (a same-class
    // self-send inside a block-argument-to-a-different-class nested inside
    // an outer unsafe bare block, or vice versa) without becoming a second,
    // could-drift-independently copy of this predicate's own call-site
    // reasoning (CLAUDE.md's no-duplicate-implementations rule) — and
    // `report_threaded_ir_verify_errors` `debug_assert!`-aborts in debug/CI
    // on ANY misclassification, so a false positive here is not a quiet
    // regression, it is a build break across the corpus. That is real,
    // non-trivial redesign risk against the very protection this predicate
    // (and its 20 `test_class_method_self_send_*` pins) exists to keep sound
    // — comparable in shape to BT-3423's own "genuinely different questions"
    // scope boundary, not a small change scoped to this one predicate.
    //
    // **Why `compute_class_var_mutating_selectors` can't be replaced
    // either.** It is a `beamtalk-core` (Compilation) whole-class,
    // syntax-only fixed point computed once before any codegen of any block
    // runs; `beamtalk-core` never depends on `beamtalk-codegen`
    // (`docs/development/architecture-principles.md` §1), so it structurally
    // cannot name `ThreadedValue`/`close()`/`VerifyError` at all — see its
    // own doc comment (`beamtalk-core/src/semantic_analysis/block_facts.rs`)
    // for this half of the finding.
    //
    // **Disposition:** kept separate, cross-referenced here and at
    // `ThreadedValue::close`/`CloseContext`/`VerifyError::StateEffectEscapesExpression`
    // (`threaded_ir.rs`) and `close_threaded_value_doc` (`util.rs`) — this
    // predicate stays the sole gate for class-method self-sends; `close()`'s
    // `CloseContext::Opaque` arm remains test-only (no production caller).
    // Revisit only alongside a deliberate redesign of the ambient
    // self-send re-entry path that carries real block-literal-to-message-send
    // context through it, not as a follow-up scoped to this predicate alone.

    /// Generates code for a block (closure).
    ///
    /// BT-852: Automatically selects Tier 1 (plain) or Tier 2 (stateful) codegen
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
    /// - Field writes are threaded via `gen_server` State at the method level (BT-1140 for Tier 2).
    /// - Self-sends are pre-scanned via `generate_tier2_self_send_open` (BT-851).
    pub(super) fn generate_block(&mut self, block: &Block) -> Result<Document<'static>> {
        use crate::core_erlang::block_analysis::analyze_block;
        let analysis = analyze_block(block);

        // BT-2792: `self.field :=` inside a block that reaches this generic
        // fallback can't correctly thread state — see `validate_stored_closure`
        // for why, and BT-2797 for the follow-up that will lift this once
        // stored/opaque blocks get proper Tier 2 support. Checked *before* the
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
        if !analysis.field_writes.is_empty() {
            Self::validate_stored_closure(&analysis, || self.location_label(block.span))?;
        }

        // BT-3151: deliberately NOT calling `check_no_unsafe_class_method_self_sends`
        // here — `generate_block` is the universal block-to-closure compiler,
        // reached both from genuinely unsafe bare-block call sites (a
        // `select:`/`do:`/`inject:into:` argument, a `whileTrue:` condition —
        // all same-process, in-process self-send contexts where the mutation
        // is provably lost) AND from contexts that are safe or cannot be
        // proven unsafe here: an `ifTrue:`/`ifFalse:` block reached via
        // generic dynamic dispatch (a long-documented ADR 0110 "known
        // limitation", not newly introduced by this guard), and a block
        // passed to a message send whose receiver may be a *different*
        // class's class-side method — which always executes in that class's
        // own gen_server process (`docs/beamtalk-language-features.md` §
        // Passing Blocks Through Class Methods), so a same-class self-send
        // inside it is genuine cross-process messaging, not the in-process
        // direct-call optimization, and correctly commits (confirmed by the
        // pre-existing, passing `shadow_cross_class_owner.bt` fixture/
        // `testCrossClassMutationDoesNotCorruptForeignProcessShadow`, ADR
        // 0110 BT-3039). `generate_block` has no way to distinguish these
        // from its own call site, so the check instead lives at each
        // specific, individually-verified-unsafe call site: see
        // `check_no_unsafe_class_method_self_sends`'s doc comment for the
        // full list.

        let captured_mutations = Self::captured_mutations_from_analysis(&analysis);

        // BT-852: Blocks with captured local mutations use Tier 2 stateful calling convention.
        if !captured_mutations.is_empty() {
            return self.generate_block_stateful(block, &captured_mutations);
        }

        // Pure block: plain fun (no mutations to thread via Tier 2)
        self.push_scope();
        // BT-1475: Track block nesting so self-cast sends route through the mailbox
        self.block_depth += 1;
        // BT-1550: Save class_var_version so that self-calls inside the closure
        // don't leak ClassVars{N} bindings into the enclosing scope.  The closure
        // is a separate Core Erlang `fun`, so any let-bindings inside it are not
        // visible to the outer method body.
        let saved_class_var_version = self.class_var_version();
        // BT-3433: Save state_version too. A pure block's body can still
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
        let saved_state_version = self.state_version();

        let mut param_parts: Vec<Document<'static>> = Vec::new();
        for (i, param) in block.parameters.iter().enumerate() {
            if i > 0 {
                param_parts.push(Document::Str(", "));
            }
            let var_name = self.fresh_var(&param.name);
            param_parts.push(leaf::var(var_name));
        }
        let header = docvec!["fun (", Document::Vec(param_parts), ") -> "];

        // Generate block body as Document.
        // BT-1475: Ensure block_depth and scope are restored even on error.
        let body_result = self.generate_block_body(block);
        self.block_depth -= 1;
        self.set_class_var_version(saved_class_var_version);
        self.set_state_version(saved_state_version);
        self.pop_scope();
        // BT-1937: The block is a closed `fun () -> ... end` expression. Any
        // open let-chain produced inside the body is closed by the body
        // handlers and scoped inside the fun, so the block as a whole MUST
        // NOT propagate an open scope to its outer context. Clear the
        // side-channel in case the body's last statement left it set.
        self.loop_mode.direct_params_do_open_chain = false;
        Ok(docvec![header, body_result?])
    }

    /// BT-851: Generates a Tier 2 stateful block (ADR 0041 Phase 0).
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
    /// BT-3149 (ADR 0111 close-out): this arm's mutation sequence is built
    /// as real [`ThreadedStmt`]s (the same C1/C2-style shapes
    /// `conditionals.rs`'s ADR 0111 Addendum 5 pinned, via
    /// `lower_field_assignment_bind`/`lower_local_var_assignment_bind`),
    /// wrapped in one [`threaded_ir::ThreadedStmt::Threaded`] node (this
    /// single-arm `with_branch_context`'s own [`threaded_ir::FrameId`]),
    /// [`threaded_ir::verify`]d and [`threaded_ir::render`]ed via
    /// `conditionals.rs`'s `verify_and_render_branch_arm` — the last real
    /// production caller of the scalar-synthesis `check_branch_frame_linearity`
    /// scaffolding is gone; `UnboundVersion` is a live check against this
    /// arm's real IR for the first time (see that scaffolding's own doc
    /// comment on why it could never fire from here before).
    pub(super) fn generate_block_stateful(
        &mut self,
        block: &Block,
        captured_vars: &[String],
    ) -> Result<Document<'static>> {
        self.push_scope();
        // BT-1475: Track block nesting so self-cast sends route through the mailbox
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
            // BT-909: Use maps:get/3 with the current outer value as fallback so the block
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
            this.generate_block_stateful_body(block, frame, &mut stmts)?;
            let branch_final = this.state_version();
            Ok::<_, crate::core_erlang::CodeGenError>(this.verify_and_render_branch_arm(
                stmts,
                frame,
                branch_final,
                block.span,
            ))
        });
        // BT-1475: Ensure block_depth and scope are restored even on error.
        self.block_depth -= 1;
        self.pop_scope();
        // BT-1937: Stateful blocks are also closed `fun (...) -> {Result, NewStateAcc}`
        // expressions and must not propagate an open scope from their body to
        // the outer context.
        self.loop_mode.direct_params_do_open_chain = false;

        let (body_doc, _branch_final) = result?;

        Ok(docvec![header, body_doc])
    }

    /// BT-855: Generates an Erlang-compatible wrapper for a block at an Erlang call site.
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

            // BT-857: Generate code to fetch the captured variable.
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

    /// BT-851: Generates the body of a Tier 2 stateful block with state threading.
    ///
    /// BT-3149 (ADR 0111 close-out, task 2 — the last real production
    /// caller of `check_branch_frame_linearity`'s scalar-synthesis
    /// scaffolding): field-/local-var-assignment mutations now lower
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

        for (i, expr) in filtered_body.iter().enumerate() {
            let is_last = i == filtered_body.len() - 1;
            let span = expr.span();

            if Self::is_field_assignment(expr) {
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
                let binding_docs = self.generate_destructure_bindings(pattern, value)?;
                for d in binding_docs {
                    stmts.push(ThreadedStmt::Statement(d, span));
                }
                if is_last {
                    let final_version = self.state_version();
                    stmts.push(ThreadedStmt::Return(
                        ValueRef::Literal("'nil'"),
                        VersionedVar::new(VersionPrefix::State, final_version, frame),
                        span,
                    ));
                }
            } else {
                // Non-assignment expression
                // ADR 0118 phase 2a (BT-3417): thread every state-effecting
                // sub-expression (`1 + (self bump)`) as real `Bind`s ahead
                // of the compile, via `thread_ahead` — the Tier 2
                // counterpart of `conditionals.rs`'s C12 catch-all.
                let hoist_scope = self.thread_ahead(expr, stmts, frame)?;
                // ADR 0118 phase 5b (BT-3422): `thread_ahead` already
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
    /// `pub(super)` (ADR 0118 phase 3, BT-3419): `while_loops.rs`'s stateful
    /// while-condition compile reuses this dispatch for a condition block's
    /// own NON-TAIL statements (its local writes thread exactly as this
    /// already gives every other block body) rather than re-deriving the
    /// same classification — see `generate_stateful_while_condition`.
    pub(super) fn classify_block_expr(&self, expr: &Expression, is_last: bool) -> BlockExprKind {
        if matches!(expr, Expression::DestructureAssignment { .. }) {
            return BlockExprKind::Destructure { is_last };
        }

        if is_last && self.is_class_method_self_send(expr) {
            return BlockExprKind::LastClassMethodSelfSend;
        }

        if is_last {
            return BlockExprKind::LastExpr;
        }

        if Self::is_field_assignment(expr) {
            return BlockExprKind::FieldAssignment;
        }

        if Self::is_local_var_assignment(expr) {
            return BlockExprKind::LocalAssignment;
        }

        if self.get_control_flow_threaded_vars(expr).is_some() {
            return BlockExprKind::ControlFlowWithThreadedVars;
        }

        if self.is_class_method_self_send(expr) {
            return BlockExprKind::ClassMethodSelfSend;
        }

        BlockExprKind::SideEffect
    }

    /// Generate code for a single block body expression, dispatching by kind.
    ///
    /// `pub(super)` (ADR 0118 phase 3, BT-3419): see
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
            BlockExprKind::LastClassMethodSelfSend | BlockExprKind::LastExpr => {
                // Last expression: its value is the block's result. ADR 0118
                // phase 5b (BT-3422): `threaded_expression` closes any
                // `ClassVars` prelude (a class-method self-send, or one
                // nested in a message's receiver/args) into a self-contained
                // `Document` so the block body is a complete closed
                // expression. The ClassVarsN bindings inside stay scoped
                // inside the block's `fun () -> ... end` and do not leak to
                // the outer method body — that is handled in generate_block
                // by saving/restoring class_var_version.
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
            BlockExprKind::ClassMethodSelfSend | BlockExprKind::SideEffect => {
                // Not an assignment or loop — generate and discard the
                // result. ADR 0118 phase 5b (BT-3422): `threaded_expression`
                // threads a class-method self-send (or one nested in a
                // message's receiver/args) as a real prelude, closed here
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

    /// Handle destructure assignment expressions in block bodies.
    ///
    /// Dispatches to sub-helpers by pattern kind: array, tuple, map.
    /// When `is_last` is true, appends `'nil'` as the block result value since
    /// destructuring only creates bindings and does not produce a value itself.
    fn generate_block_destructure(
        &mut self,
        expr: &Expression,
        is_last: bool,
    ) -> Result<Document<'static>> {
        let Expression::DestructureAssignment { pattern, value, .. } = expr else {
            unreachable!("caller guarantees DestructureAssignment");
        };

        match pattern {
            Pattern::Array { elements, rest, .. } => {
                let mut doc =
                    self.generate_block_array_destructure(elements, rest.as_deref(), value)?;
                if is_last {
                    doc = docvec![doc, "'nil'"];
                }
                Ok(doc)
            }
            Pattern::Tuple { .. } | Pattern::Map { .. } => {
                let mut docs = self.generate_destructure_bindings(pattern, value)?;
                if is_last {
                    docs.push(Document::Str("'nil'"));
                }
                Ok(Document::Vec(docs))
            }
            _ => Err(CodeGenError::UnsupportedFeature {
                feature: "Unsupported destructuring pattern kind".to_string(),
                span: Some(value.span()),
            }),
        }
    }

    /// Generate array destructure bindings in a block body.
    ///
    /// Example: `#[a, b] := expr` generates:
    ///   `let _Arr1 = <expr> in let A = send(_Arr1, 'at:', [1]) in let B = send(...) in`
    /// With rest: `#[a, ...rest] := expr` additionally generates:
    ///   `let Rest = beamtalk_array:slice_from(_Arr1, 2) in`
    fn generate_block_array_destructure(
        &mut self,
        elements: &[Pattern],
        rest: Option<&Pattern>,
        value: &Expression,
    ) -> Result<Document<'static>> {
        let arr_var = self.fresh_temp_var("Arr");
        let val_doc = self.expression_doc(value)?;
        let mut docs = vec![docvec![
            "let ",
            leaf::var(arr_var.clone()),
            " = ",
            val_doc,
            " in "
        ]];

        for (idx, elem) in elements.iter().enumerate() {
            let one_based = index_lit(idx + 1);
            match elem {
                Pattern::Variable(id) => {
                    let core_var = Self::to_core_erlang_var(&id.name);
                    self.bind_var(&id.name, &core_var);
                    docs.push(docvec![
                        "let ",
                        leaf::var(core_var),
                        " = call 'beamtalk_message_dispatch':'send'(",
                        leaf::var(arr_var.clone()),
                        ", 'at:', [",
                        one_based,
                        "]) in "
                    ]);
                }
                Pattern::Literal(lit, _span) => {
                    // Guard-check: extract element and assert it equals the
                    // literal.  Raises `{badmatch, Array}` on mismatch.
                    let elem_var = self.fresh_temp_var("Elem");
                    let guard_ok_var = self.fresh_temp_var("GuardOk");
                    let mismatch_var = self.fresh_temp_var("Mismatch");
                    let lit_doc = self.generate_literal(lit)?;
                    docs.push(docvec![
                        "let ",
                        leaf::var(elem_var.clone()),
                        " = call 'beamtalk_message_dispatch':'send'(",
                        leaf::var(arr_var.clone()),
                        ", 'at:', [",
                        one_based,
                        "]) in "
                    ]);
                    docs.push(docvec![
                        "let ",
                        leaf::var(guard_ok_var),
                        " = case ",
                        leaf::var(elem_var),
                        " of <",
                        lit_doc,
                        "> when 'true' -> 'ok' <",
                        leaf::var(mismatch_var),
                        "> when 'true' -> call 'erlang':'error'({'badmatch', ",
                        leaf::var(arr_var.clone()),
                        "}) end in "
                    ]);
                }
                Pattern::Wildcard(_) => {} // no binding needed
                _ => {
                    return Err(CodeGenError::UnsupportedFeature {
                        feature: "Nested patterns in array destructuring".to_string(),
                        span: Some(elem.span()),
                    });
                }
            }
        }

        // Rest pattern: `...rest` binds remaining elements as a sub-array
        // Pattern::Wildcard — no binding needed, only Variable generates code.
        if let Some(Pattern::Variable(id)) = rest {
            let core_var = Self::to_core_erlang_var(&id.name);
            self.bind_var(&id.name, &core_var);
            let from_idx = index_lit(elements.len() + 1);
            docs.push(docvec![
                "let ",
                leaf::var(core_var),
                " = call 'beamtalk_array':'slice_from'(",
                leaf::var(arr_var.clone()),
                ", ",
                from_idx,
                ") in "
            ]);
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

        // BT-852: Stored blocks with mutations are now supported via Tier 2.
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
        // ADR 0118 phase 5b (BT-3422): a class method's own top-level body
        // splices a class-var producer's prelude directly (`lower_class_method_body`),
        // but a block nested inside a class method (this function) still
        // reaches this assignment for `result := self foo`-shaped RHSes. When
        // `value` is ITSELF a recognized producer (`is_class_var_assignment`/
        // `is_class_method_self_send`), `threaded_expression` gives it a real
        // prelude whose rebound `ClassVarsN` stays lexically visible here.
        // Otherwise `value` may still dispatch one that the compile below
        // reaches opaquely and closes (e.g. BT-2007 inherited dispatch) —
        // closing loses the mutated name's LEXICAL visibility, but not the
        // mutation itself: the compiler's OWN `current_class_var()`
        // bookkeeping advances to track it regardless, so a later statement
        // in this same block body (e.g. `^result`, which reads
        // `current_class_var()`) would otherwise reference a name never
        // bound in its own scope. `refresh_class_var_after_opaque_scope`
        // recovers the live value via the ADR 0110 shadow write and re-binds
        // it to a name that IS in scope here.
        if self.in_class_method()
            && !(self.is_class_var_assignment(value) || self.is_class_method_self_send(value))
        {
            let cv_version_before = self.class_var_version();
            let val_doc = self.expression_doc(value)?;
            self.bind_var(var_name, &core_var);
            let refresh = self
                .refresh_class_var_after_opaque_scope(cv_version_before)
                .unwrap_or(Document::Nil);
            return Ok(docvec![
                "let ",
                leaf::var(core_var),
                " = ",
                val_doc,
                " in ",
                refresh,
            ]);
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
    /// as captured-mutating (see [`Self::get_control_flow_threaded_vars`]),
    /// so the enclosing block still compiled as a plain (non-`StateAcc`) fun.
    ///
    /// BT-3162: every one of these constructs' `generate_*_with_mutations`
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
            .get_control_flow_threaded_vars(expr)
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
