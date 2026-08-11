// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! While loop control flow code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates code for `whileTrue:` and `whileFalse:` loop constructs
//! with both pure and state-threading variants.

use super::super::document::{Document, join, leaf};
use super::super::intrinsics::{STATEFUL_BLOCK_DISPATCH_HINT, validate_block_arity_exact};
use super::super::threaded_ir::{
    self, BindOp, FrameId, RenderCtx, ThreadedStmt, ThreadingMode, ValueRef, VersionPrefix,
    VersionedVar,
};
use super::super::{CodeGenError, CoreErlangGenerator, Result, block_analysis};
use super::{BodyKind, ThreadingPlan};
use crate::ast::{Block, Expression};
use crate::docvec;
use std::collections::HashMap;

/// Result of pre-extracting hybrid loop fields: pre-extraction docs, readonly params, mutated params.
///
/// Each param is a `(field_name, core_erlang_var)` pair.
type HybridFieldExtraction = (
    Vec<Document<'static>>,
    Vec<(String, String)>,
    Vec<(String, String)>,
);

impl CoreErlangGenerator {
    pub(in crate::codegen::core_erlang) fn generate_while_true(
        &mut self,
        condition: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // BT-493: Validate body block arity (must be 0-arg)
        validate_block_arity_exact(
            body,
            0,
            "whileTrue:",
            "Fix: The body block must take no arguments:\n\
             \x20 [x < 10] whileTrue: [x := x + 1]",
        )?;

        // Check if body is a literal block (enables mutation analysis)
        if let Expression::Block(body_block) = body {
            // Use mutations version if there are any writes (local or field)
            // BT-153: Include local_writes only in REPL mode
            // BT-1329: Also check for nested list ops with cross-scope mutations
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis)
                || self.body_has_list_op_cross_scope_mutations(body_block)
            {
                return self.generate_while_true_with_mutations(condition, body_block);
            }
        }

        // Simple case: no mutations
        self.generate_while_true_simple(condition, body)
    }

    pub(in crate::codegen::core_erlang) fn generate_while_true_simple(
        &mut self,
        condition: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        self.generate_while_simple(condition, body, false)
    }

    pub(in crate::codegen::core_erlang) fn generate_while_true_with_mutations(
        &mut self,
        condition: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        self.generate_while_loop_with_mutations(condition, body, false)
    }

    pub(in crate::codegen::core_erlang) fn generate_while_false(
        &mut self,
        condition: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        // BT-493: Validate body block arity (must be 0-arg)
        validate_block_arity_exact(
            body,
            0,
            "whileFalse:",
            "Fix: The body block must take no arguments:\n\
             \x20 [x > 0] whileFalse: [x := x + 1]",
        )?;

        // Check if body is a literal block (enables mutation analysis)
        if let Expression::Block(body_block) = body {
            // Use mutations version if there are any writes (local or field)
            // BT-153: Include local_writes only in REPL mode
            // BT-1329: Also check for nested list ops with cross-scope mutations
            let analysis = block_analysis::analyze_block(body_block);
            if self.needs_mutation_threading(&analysis)
                || self.body_has_list_op_cross_scope_mutations(body_block)
            {
                return self.generate_while_false_with_mutations(condition, body_block);
            }
        }

        // Simple case: no mutations
        self.generate_while_false_simple(condition, body)
    }

    pub(in crate::codegen::core_erlang) fn generate_while_false_simple(
        &mut self,
        condition: &Expression,
        body: &Expression,
    ) -> Result<Document<'static>> {
        self.generate_while_simple(condition, body, true)
    }

    /// Shared letrec loop structure for `whileTrue:` (negate=false) and `whileFalse:` (negate=true).
    ///
    /// Generates a recursive function:
    /// ```text
    /// letrec '_LoopN'/0 = fun () ->
    ///     let _CondFun = <condition> in
    ///     case apply _CondFun () of
    ///       <continue_atom> when 'true' -> let _BodyFun = <body> in
    ///                                      let _ = apply _BodyFun () in
    ///                                      apply '_LoopN'/0 ()
    ///       <exit_atom> when 'true' -> 'nil'
    ///     end
    /// in apply '_LoopN'/0 ()
    /// ```
    fn generate_while_simple(
        &mut self,
        condition: &Expression,
        body: &Expression,
        negate: bool,
    ) -> Result<Document<'static>> {
        let (continue_atom, exit_atom) = if negate {
            ("'false'", "'true'")
        } else {
            ("'true'", "'false'")
        };

        let loop_fn = self.fresh_temp_var("Loop");
        let cond_var = self.fresh_temp_var("CondFun");
        let cond_code = self.expression_doc(condition)?;
        let body_var = self.fresh_temp_var("BodyFun");
        let body_code = self.expression_doc(body)?;

        let doc = docvec![
            "letrec ",
            leaf::fname(loop_fn.clone(), 0),
            " = fun () -> let ",
            leaf::var(cond_var.clone()),
            " = ",
            cond_code,
            " in case apply ",
            leaf::var(cond_var),
            " () of <",
            continue_atom,
            "> when 'true' -> let ",
            leaf::var(body_var.clone()),
            " = ",
            body_code,
            " in let _ = apply ",
            leaf::var(body_var),
            " () in apply ",
            leaf::fname(loop_fn.clone(), 0),
            " () <",
            exit_atom,
            "> when 'true' -> 'nil' end in apply ",
            leaf::fname(loop_fn, 0),
            " ()",
        ];

        Ok(doc)
    }

    pub(in crate::codegen::core_erlang) fn generate_while_false_with_mutations(
        &mut self,
        condition: &Expression,
        body: &Block,
    ) -> Result<Document<'static>> {
        self.generate_while_loop_with_mutations(condition, body, true)
    }

    /// Shared implementation for `whileTrue:` and `whileFalse:` stateful loops.
    ///
    /// `negate = false` → continue when condition is `'true'` (whileTrue:).
    /// `negate = true`  → continue when condition is `'false'` (whileFalse:).
    ///
    /// In direct-params mode (BT-1275, no field mutations) the fun signature is
    /// `(Var1, ..., VarN)` instead of `(StateAcc)`.
    fn generate_while_loop_with_mutations(
        &mut self,
        condition: &Expression,
        body: &Block,
        negate: bool,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new_for_letrec(self, body, Some(condition));
        self.emit_loop_convention_diagnostic(&plan, body.span);

        if plan.use_direct_params {
            return self.generate_while_loop_direct(condition, body, &plan, negate);
        }
        if plan.use_hybrid_params {
            return self.generate_while_loop_hybrid(condition, body, &plan, negate);
        }

        let cond_var = self.fresh_temp_var("CondFun");

        let (pack_doc, init_state) = plan.generate_pack_prefix(self);

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(pack_doc);
        docs.push(docvec!["letrec 'while'/1 = fun (StateAcc) -> "]);

        // BT-598: At the start of each loop iteration, read threaded locals from StateAcc.
        // Use push_scope so bindings don't leak to caller after the letrec.
        self.push_scope();
        let unpack_docs = plan.generate_unpack_at_iteration_start(self);
        docs.extend(unpack_docs);

        docs.push(docvec![
            "let ",
            leaf::var(cond_var.clone()),
            " = fun (StateAcc) -> ",
        ]);

        // Generate condition inside branch context
        let cond_doc = self.with_branch_context(|this| {
            if let Expression::Block(cond_block) = condition {
                // BT-3151: this condition block bypasses `generate_block`'s own
                // self-send check by calling `generate_block_body` directly —
                // see `check_no_unsafe_class_method_self_sends`'s doc comment.
                let analysis =
                    crate::codegen::core_erlang::block_analysis::analyze_block(cond_block);
                this.check_no_unsafe_class_method_self_sends(&analysis, cond_block.span)?;
                this.generate_block_body(cond_block)
            } else {
                this.generate_expression(condition)
            }
        })?;
        docs.push(cond_doc);

        // Condition application and true/false arm headers
        let cond_apply_arm = if negate {
            "<'false'> when 'true' -> "
        } else {
            "<'true'> when 'true' -> "
        };
        docs.push(docvec![
            " in case apply ",
            leaf::var(cond_var),
            " (StateAcc) of ",
            cond_apply_arm,
        ]);

        let (body_doc, final_state_version) =
            self.generate_threaded_loop_body(body, &plan, &BodyKind::Letrec)?;
        docs.push(body_doc);
        let final_state_var = super::super::util::versioned_var("StateAcc", final_state_version);

        let exit_arm = if negate {
            "<'true'> when 'true' -> {'nil', StateAcc} "
        } else {
            "<'false'> when 'true' -> {'nil', StateAcc} "
        };
        docs.push(docvec![
            " apply 'while'/1 (",
            leaf::var(final_state_var),
            ") ",
            exit_arm,
            "end ",
        ]);

        // Pop scope to restore original bindings (before the letrec)
        self.pop_scope();

        // Initial call with packed state
        docs.push(docvec!["in apply 'while'/1 (", leaf::var(init_state), ")"]);

        Ok(Document::Vec(docs))
    }

    /// ADR 0111 Addendum 2 (BT-3145 pilot): the loop's condition body is
    /// ordinary AST-directed expression codegen with no state-threading
    /// content of its own (the same class of embed
    /// `ThreadedStmt::ConditionalLoop`'s `continue_header` uses it as —
    /// §Verifier honesty). Factored out of `generate_while_loop_direct` so
    /// both the legacy path and `try_render_while_direct_via_threaded_ir`
    /// call the SAME condition-codegen, never a re-derivation
    /// (CLAUDE.md's no-duplicate-implementations rule).
    fn generate_loop_condition_body(
        &mut self,
        condition: &Expression,
    ) -> Result<Document<'static>> {
        self.with_branch_context(|this| {
            if let Expression::Block(cond_block) = condition {
                // BT-3151: see the analogous check in `generate_while_loop`.
                let analysis =
                    crate::codegen::core_erlang::block_analysis::analyze_block(cond_block);
                this.check_no_unsafe_class_method_self_sends(&analysis, cond_block.span)?;
                this.generate_block_body(cond_block)
            } else {
                this.generate_expression(condition)
            }
        })
    }

    /// ADR 0111 Addendum 2 pilot (BT-3145): env-flag measurement gate for
    /// routing `generate_while_loop_direct` through `ThreadedIr`
    /// (lower → verify → render) instead of the legacy Document-construction
    /// path. Off by default — this pilot's coverage is deliberately narrow
    /// (see `while_direct_body_is_bind_representable`'s doc comment), so the
    /// default stays the always-correct legacy path until a follow-up
    /// closes the discovered gap and the ≤3% measurement gate is re-run
    /// against full coverage.
    fn threaded_ir_while_direct_enabled() -> bool {
        std::env::var("BEAMTALK_THREADED_IR_WHILE_DIRECT").is_ok_and(|v| v == "1")
    }

    /// ADR 0111 Addendum 2 pilot (BT-3145): a conservative, purely
    /// AST-level (zero generator side effects) check for whether `body` is
    /// representable as a `ThreadedStmt::ConditionalLoop` `Bind` chain —
    /// every statement must be a straight-line reassignment of a threaded
    /// local from a "simple" RHS (`is_simple_threaded_rhs`).
    ///
    /// This is intentionally narrower than everything `select_direct_params`
    /// allows through a direct-params loop body: plain-let temporaries
    /// (non-threaded locals, `try_generate_block_local_plain_let`),
    /// destructuring, non-assignment statements, and any RHS that could
    /// reach `direct_params_list_op_result`'s open-chain codegen path (a
    /// tuple-safe nested list op, `list_ops/*.rs`) are NOT yet representable
    /// in this pilot's IR shape — `ThreadedStmt` has no "opaque non-`Bind`
    /// body statement" node, and `ValueRef` cannot represent an open-chain
    /// fragment (two chained `let`s) as a single `Bind`'s value. Bailing
    /// here falls back to the unmodified legacy path, unconditionally
    /// correct, just not migrated onto `ThreadedIr` yet.
    ///
    /// This is a real, additional scope boundary discovered empirically
    /// while implementing this call site (verified against real compiled
    /// output, the same evidentiary standard as BT-3145's original
    /// investigation), beyond ADR 0111 Addendum 2's own Gap 1/Gap 2 —
    /// recorded on the BT-3145 Linear issue as a named follow-up, not
    /// silently absorbed into "done."
    fn while_direct_body_is_bind_representable(body: &Block, plan: &ThreadingPlan) -> bool {
        !body.body.is_empty()
            && body.body.iter().all(|stmt| match &stmt.expression {
                Expression::Assignment { target, value, .. } => match target.as_ref() {
                    Expression::Identifier(id) => {
                        plan.threaded_locals.contains(&id.name.to_string())
                            && Self::is_simple_threaded_rhs(value)
                    }
                    _ => false,
                },
                _ => false,
            })
    }

    /// Conservative allowlist for a threaded-local rebind's RHS shape — see
    /// `while_direct_body_is_bind_representable`'s doc comment for why this
    /// is deliberately narrow: default-deny for anything not explicitly
    /// listed, including any nested `Block` anywhere in the subtree — every
    /// list-op selector (`collect:`/`select:`/`inject:into:`/…) requires a
    /// block-literal argument, so excluding `Block` safely excludes
    /// `direct_params_list_op_result`'s open-chain shape too, without
    /// needing to duplicate `list_ops/*.rs`'s selector set here.
    fn is_simple_threaded_rhs(expr: &Expression) -> bool {
        match expr {
            Expression::Literal(..) | Expression::Identifier(_) => true,
            Expression::FieldAccess { receiver, .. }
            | Expression::Parenthesized {
                expression: receiver,
                ..
            } => Self::is_simple_threaded_rhs(receiver),
            Expression::MessageSend {
                receiver,
                arguments,
                ..
            } => {
                Self::is_simple_threaded_rhs(receiver)
                    && arguments.iter().all(Self::is_simple_threaded_rhs)
            }
            _ => false,
        }
    }

    /// ADR 0111 Addendum 2 pilot (BT-3145): attempts to lower, verify, and
    /// render `generate_while_loop_direct`'s condition/case-split shape
    /// through `ThreadedIr`. Returns `Ok(None)` (zero generator mutation)
    /// when `body` isn't representable in this pilot's narrow `Bind`-chain
    /// shape (`while_direct_body_is_bind_representable`) or when a threaded
    /// local's outer-scope binding has already diverged from its plain
    /// `to_core_erlang_var` form (see the `initial_direct_args` guard below)
    /// — the caller falls back to the unmodified legacy path unconditionally
    /// in either case.
    ///
    /// Mint order mirrors `generate_while_loop_direct` exactly (ADR 0111
    /// Addendum 2, Gap 2's ordering contract, load-bearing for
    /// byte-identity): `CondFun` + condition first, then each body rebind
    /// as its `Bind` is lowered (in encounter order), and the exit arm's
    /// `ExitSA` chain LAST — lowering mints via the SAME `fresh_temp_var`
    /// calls production makes, never gensym'd by `render`.
    #[allow(clippy::too_many_lines)] // ThreadedIr lowering mirroring generate_while_loop_direct's own shape, ADR 0111 Addendum 2 pilot (BT-3145)
    fn try_render_while_direct_via_threaded_ir(
        &mut self,
        condition: &Expression,
        body: &Block,
        plan: &ThreadingPlan,
        negate: bool,
    ) -> Result<Option<Document<'static>>> {
        if !Self::while_direct_body_is_bind_representable(body, plan) {
            return Ok(None);
        }

        // Pure lookups (`&self`) — safe to check before committing to any
        // mutation. `render_loop_skeleton`'s outer-call-args rendering
        // (shared with the bare `Threaded` shape) derives each param's
        // OUTER reference from its plain `to_core_erlang_var` form; if the
        // outer scope already rebound a threaded local to a different Core
        // Erlang name (e.g. an earlier construct gensym'd it), that
        // rendering would silently diverge from `initial_direct_args`'s real
        // value — bail rather than risk an unbound-variable reference.
        let initial_direct_args = plan.initial_direct_args(self);
        if plan
            .threaded_locals
            .iter()
            .zip(initial_direct_args.iter())
            .any(|(name, arg)| *arg != CoreErlangGenerator::to_core_erlang_var(name))
        {
            return Ok(None);
        }

        let param_names: Vec<String> = plan
            .threaded_locals
            .iter()
            .map(|v| CoreErlangGenerator::to_core_erlang_var(v))
            .collect();
        let frame = FrameId::new(1);
        let param_list_doc = || {
            join(
                param_names.iter().map(|v| leaf::var(v.clone())),
                &Document::Str(", "),
            )
        };

        let cond_var = self.fresh_temp_var("CondFun"); // mint #1: CondFun, matching `while_loops.rs`'s legacy order

        self.push_scope();
        plan.generate_unpack_at_iteration_start(self);

        let cond_doc = self.generate_loop_condition_body(condition)?;
        let case_arm = if negate {
            "<'false'> when 'true' -> "
        } else {
            "<'true'> when 'true' -> "
        };
        let continue_header = docvec![
            "let ",
            leaf::var(cond_var.clone()),
            " = fun (",
            param_list_doc(),
            ") -> ",
            cond_doc,
            " in case apply ",
            leaf::var(cond_var),
            " (",
            param_list_doc(),
            ") of ",
            case_arm,
        ];

        // Lower the body: each statement is (per eligibility) a
        // straight-line reassignment of a threaded local — mint each
        // rebind's fresh name in ENCOUNTER order, exactly where
        // `generate_direct_var_update_in_loop` mints today. Wrapped in
        // `with_branch_context` exactly like `generate_threaded_loop_body`
        // itself is (`in_loop_body = true`, `state_version` reset to 0) —
        // required for a rebind RHS that reads a field (`self.step`) to
        // resolve `StateAcc` (loop context) rather than `State` (ambient
        // outer context), confirmed against real compiled output: without
        // this wrap, `maps:get('step', State)` diverges from legacy's
        // `maps:get('step', StateAcc)`.
        let mut ir_body: Vec<ThreadedStmt> = Vec::with_capacity(body.body.len());
        self.with_branch_context(|this| -> Result<()> {
            let mut current: HashMap<String, VersionedVar> = plan
                .threaded_locals
                .iter()
                .map(|name| {
                    (
                        name.clone(),
                        VersionedVar::new(VersionPrefix::Local(name.clone()), 0, frame),
                    )
                })
                .collect();
            let mut rebind_counts: HashMap<String, usize> = plan
                .threaded_locals
                .iter()
                .map(|n| (n.clone(), 0))
                .collect();
            for stmt in &body.body {
                let Expression::Assignment { target, value, .. } = &stmt.expression else {
                    unreachable!(
                        "while_direct_body_is_bind_representable guarantees every \
                         statement is an Assignment"
                    );
                };
                let Expression::Identifier(id) = target.as_ref() else {
                    unreachable!(
                        "while_direct_body_is_bind_representable guarantees the \
                         target is an Identifier"
                    );
                };
                let name = id.name.to_string();
                this.direct_params_list_op_result = None;
                let value_doc = this.expression_doc(value)?; // mints inside expression codegen, same order as legacy
                // Defense in depth: `is_simple_threaded_rhs` excludes any RHS
                // containing a `Block` literal (every list-op selector needs
                // one), which should make `direct_params_list_op_result`
                // impossible here — EXCEPT a list-op called with a block
                // held in a *variable* (`list collect: storedBlock`) has no
                // `Block` literal in this expression's own AST at all, so the
                // static check cannot rule it out by construction. Verified
                // empirically that this specific flag cannot actually fire
                // for `generate_while_loop_direct` today — the list-op
                // codegen functions that set it all gate on
                // `self.in_direct_params_loop`, which only
                // `generate_counted_stateful_loop_direct`/
                // `generate_hybrid_loop_body` ever set `true`, never this
                // (pure, non-hybrid) while-direct call site — but that is a
                // property of the CURRENT `in_direct_params_loop` wiring
                // elsewhere in this file, not something this function
                // controls or that `is_simple_threaded_rhs` proves, so this
                // check stays as a real guard against future coupling
                // (e.g. a while-direct loop nested inside a construct that
                // sets `in_direct_params_loop` ambiently). Legacy's
                // open-chain shape for that case (`value_code` ending in an
                // open `"... in "`, not a self-contained expression) does
                // not fit `BindOp::Direct(ValueRef::Doc(..))`'s "closed
                // expression" contract — wrapping it in `"let TARGET = " +
                // value_doc + " in "` would emit invalid Core Erlang. Fail
                // loudly with a structured internal error instead of
                // emitting that: by this point mints have already been
                // consumed for this (and possibly earlier) statements, so
                // silently falling back to the legacy path here would
                // re-mint from scratch and diverge from what legacy alone
                // would have produced — no longer safe once lowering has
                // started (see this function's doc comment on why
                // eligibility is checked BEFORE any mutation, never
                // re-checked mid-lowering).
                if this.direct_params_list_op_result.take().is_some() {
                    return Err(CodeGenError::Internal(format!(
                        "ThreadedIr while-direct pilot (BT-3145): rebind of `{name}` \
                         resolved to a list-op open-chain RHS (a block held in a \
                         variable, not a literal) — not representable as a single \
                         Bind value; this shape must not reach ConditionalLoop \
                         lowering. Retry without BEAMTALK_THREADED_IR_WHILE_DIRECT=1."
                    )));
                }
                let source = current.get(&name).cloned().unwrap_or_else(|| {
                    VersionedVar::new(VersionPrefix::Local(name.clone()), 0, frame)
                });
                let core_var = CoreErlangGenerator::to_core_erlang_var(&name);
                let new_var = this.fresh_temp_var(&core_var); // mint: body rebind, matches `generate_direct_var_update_in_loop`'s own order
                this.bind_var(&name, &new_var);
                let count = rebind_counts.entry(name.clone()).or_insert(0);
                *count += 1;
                let target_var = VersionedVar::new(VersionPrefix::Gensym(new_var), *count, frame);
                ir_body.push(ThreadedStmt::Bind {
                    target: target_var.clone(),
                    source,
                    op: BindOp::Direct(ValueRef::Doc(value_doc)),
                    shadow_write: false,
                    span: stmt.expression.span(),
                });
                current.insert(name, target_var);
            }
            Ok(())
        })?;

        // Unlike legacy, `final_args` (the recursive tail call's arguments)
        // is NOT collected here — `render` reconstructs it from the `Bind`
        // chain via `final_loop_arg_identities` (see `ConditionalLoop`'s doc
        // comment for why `produces` alone cannot supply it for a `Gensym`
        // target).
        let exit_stateacc = plan.generate_exit_stateacc(&param_names, self); // mint: ExitSA chain, LAST — matches legacy order
        let exit_case_arm = if negate {
            "<'true'> when 'true' -> "
        } else {
            "<'false'> when 'true' -> "
        };
        let exit_arm = docvec![exit_case_arm, exit_stateacc, " end "];

        self.pop_scope();

        let produces: Vec<VersionedVar> = plan
            .threaded_locals
            .iter()
            .map(|name| VersionedVar::new(VersionPrefix::Local(name.clone()), 0, frame))
            .collect();

        let node = ThreadedStmt::ConditionalLoop {
            fn_name: "while".to_string(),
            mode: ThreadingMode::DirectParams,
            frame,
            counter: None,
            continue_header,
            body: ir_body,
            produces,
            exit_arm,
            span: body.span,
        };

        let errors = threaded_ir::verify(std::slice::from_ref(&node));
        self.report_threaded_ir_verify_errors(&errors, "while-direct ConditionalLoop", body.span);

        let mut ctx = RenderCtx::new(self);
        let rendered = threaded_ir::render(std::slice::from_ref(&node), &mut ctx);

        Ok(Some(rendered))
    }

    /// BT-1275: Direct-params variant of `generate_while_loop_with_mutations`.
    ///
    /// Uses `fun (Var1, ..., VarN)` instead of `fun (StateAcc)`.
    /// The `StateAcc` map is rebuilt only once in the false (exit) arm.
    #[allow(clippy::too_many_lines)] // direct-params state-threading codegen, BT-1275
    fn generate_while_loop_direct(
        &mut self,
        condition: &Expression,
        body: &Block,
        plan: &ThreadingPlan,
        negate: bool,
    ) -> Result<Document<'static>> {
        if Self::threaded_ir_while_direct_enabled()
            && let Some(doc) =
                self.try_render_while_direct_via_threaded_ir(condition, body, plan, negate)?
        {
            return Ok(doc);
        }

        // Collect initial arg values from the outer scope (before push_scope).
        let initial_direct_args = plan.initial_direct_args(self);

        let param_names: Vec<String> = plan
            .threaded_locals
            .iter()
            .map(|v| CoreErlangGenerator::to_core_erlang_var(v))
            .collect();
        let arity = param_names.len();
        let param_list_doc = || {
            join(
                param_names.iter().map(|v| leaf::var(v.clone())),
                &Document::Str(", "),
            )
        };

        let cond_var = self.fresh_temp_var("CondFun");

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(docvec![
            "letrec ",
            leaf::fname("while", arity),
            " = fun (",
            param_list_doc(),
            ") -> ",
        ]);

        self.push_scope();
        // Register var bindings — no unpack docs in direct-params mode
        // (structurally guaranteed by `generate_unpack_at_iteration_start`'s
        // own `if !use_direct_params && !use_hybrid_params` guard).
        plan.generate_unpack_at_iteration_start(self);

        // The condition closure captures the current vars from scope.
        // We pass only the params (not StateAcc) since there is no StateAcc.
        docs.push(docvec![
            "let ",
            leaf::var(cond_var.clone()),
            " = fun (",
            param_list_doc(),
            ") -> ",
        ]);

        let cond_doc = self.generate_loop_condition_body(condition)?;
        docs.push(cond_doc);

        // Apply condition with current params.
        let case_arm = if negate {
            "<'false'> when 'true' -> "
        } else {
            "<'true'> when 'true' -> "
        };
        docs.push(docvec![
            " in case apply ",
            leaf::var(cond_var),
            " (",
            param_list_doc(),
            ") of ",
            case_arm,
        ]);

        let (body_doc, _) = self.generate_threaded_loop_body(body, plan, &BodyKind::Letrec)?;
        docs.push(body_doc);

        // Collect final var names after body execution.
        let final_args = self.collect_final_local_args(plan);

        // Build exit StateAcc using the CURRENT iteration's param names.
        let exit_stateacc = plan.generate_exit_stateacc(&param_names, self);

        let final_args_doc = join(final_args.into_iter().map(leaf::var), &Document::Str(", "));
        let exit_arm = if negate {
            "<'true'> when 'true' -> "
        } else {
            "<'false'> when 'true' -> "
        };
        docs.push(docvec![
            " apply ",
            leaf::fname("while", arity),
            " (",
            final_args_doc,
            ") ",
            exit_arm,
            exit_stateacc,
            " end ",
        ]);

        self.pop_scope();

        let initial_args_doc = join(
            initial_direct_args.into_iter().map(leaf::var),
            &Document::Str(", "),
        );
        docs.push(docvec![
            "in apply ",
            leaf::fname("while", arity),
            " (",
            initial_args_doc,
            ")",
        ]);

        Ok(Document::Vec(docs))
    }

    /// BT-1326/BT-1342: Full-extract variant of `generate_while_loop_with_mutations`.
    ///
    /// Uses `fun (Var1, ..., VarN, RField1, ..., MField1, ...)` — locals, read-only fields,
    /// AND mutated fields as direct fun parameters. No `State` parameter.
    ///
    /// Field reads resolve to direct parameters. Field writes become simple variable
    /// rebindings (no `maps:put` per iteration). At loop exit, mutated fields are repacked
    /// into the initial State map.
    fn generate_while_loop_hybrid(
        &mut self,
        condition: &Expression,
        body: &Block,
        plan: &ThreadingPlan,
        negate: bool,
    ) -> Result<Document<'static>> {
        let initial_local_args = plan.initial_direct_args(self);
        let initial_state = plan.initial_state_var.clone();

        // Pre-extract ALL fields (readonly + mutated) before the letrec.
        let (pre_extract_docs, readonly_params, mutated_params) =
            self.pre_extract_hybrid_fields(plan, &initial_state, ("", " "));

        let local_param_names: Vec<String> = plan
            .threaded_locals
            .iter()
            .map(|v| CoreErlangGenerator::to_core_erlang_var(v))
            .collect();
        let readonly_param_names: Vec<String> =
            readonly_params.iter().map(|(_, v)| v.clone()).collect();
        let mutated_param_names: Vec<String> =
            mutated_params.iter().map(|(_, v)| v.clone()).collect();
        let arity =
            local_param_names.len() + readonly_param_names.len() + mutated_param_names.len();

        let param_list_doc = || {
            Self::build_hybrid_param_list(
                &local_param_names,
                &readonly_param_names,
                &mutated_param_names,
            )
        };

        let cond_var = self.fresh_temp_var("CondFun");

        let all_field_params = Self::build_field_params_map(&readonly_params, &mutated_params);

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.extend(pre_extract_docs);
        docs.push(docvec![
            "letrec ",
            leaf::fname("while", arity),
            " = fun (",
            param_list_doc(),
            ") -> ",
        ]);

        self.push_scope();
        // Register var bindings — no unpack docs in hybrid mode (structurally
        // guaranteed by `generate_unpack_at_iteration_start`'s own
        // `if !use_direct_params && !use_hybrid_params` guard).
        plan.generate_unpack_at_iteration_start(self);

        docs.push(docvec![
            "let ",
            leaf::var(cond_var.clone()),
            " = fun (",
            param_list_doc(),
            ") -> ",
        ]);

        let cond_doc = self.generate_hybrid_condition(condition, plan, &all_field_params)?;
        docs.push(cond_doc);

        let case_arm = if negate {
            "<'false'> when 'true' -> "
        } else {
            "<'true'> when 'true' -> "
        };
        docs.push(docvec![
            " in case apply ",
            leaf::var(cond_var),
            " (",
            param_list_doc(),
            ") of ",
            case_arm,
        ]);

        let (body_doc, final_mutated_field_args) =
            self.generate_hybrid_loop_body(body, plan, &all_field_params, &mutated_params)?;
        docs.push(body_doc);

        let final_local_args = self.collect_final_local_args(plan);
        let exit_stateacc = plan.generate_exit_stateacc_full_extract(
            &local_param_names,
            &mutated_param_names,
            &initial_state,
            self,
        );

        Self::append_hybrid_loop_tail(
            &mut docs,
            negate,
            arity,
            &final_local_args,
            &readonly_param_names,
            final_mutated_field_args,
            exit_stateacc,
        );

        self.pop_scope();

        Self::append_hybrid_initial_call(
            &mut docs,
            "while",
            arity,
            initial_local_args,
            &readonly_param_names,
            &mutated_param_names,
        );

        Ok(Document::Vec(docs))
    }

    /// Pre-extracts all fields (readonly + mutated) from the state map into local variables.
    ///
    /// Returns the pre-extraction docs, readonly params (field, var) pairs, and mutated params.
    /// The `let_wrap` pair controls formatting before `let` and after `in`.
    pub(super) fn pre_extract_hybrid_fields(
        &mut self,
        plan: &ThreadingPlan,
        initial_state: &str,
        let_wrap: (&'static str, &'static str),
    ) -> HybridFieldExtraction {
        let (prefix, suffix) = let_wrap;
        let mut pre_extract_docs: Vec<Document<'static>> = Vec::new();

        let extract_field =
            |docs: &mut Vec<Document<'static>>, codegen: &mut Self, field: &str| -> String {
                let var_name = codegen.fresh_temp_var(&format!(
                    "{}Field",
                    CoreErlangGenerator::to_core_erlang_var(field)
                ));
                docs.push(docvec![
                    prefix,
                    "let ",
                    leaf::var(var_name.clone()),
                    " = call 'maps':'get'(",
                    leaf::atom(field.to_string()),
                    ", ",
                    leaf::var(initial_state.to_string()),
                    ") in",
                    suffix,
                ]);
                var_name
            };

        let readonly_params: Vec<(String, String)> = plan
            .readonly_fields
            .iter()
            .map(|field| {
                let var_name = extract_field(&mut pre_extract_docs, self, field);
                (field.clone(), var_name)
            })
            .collect();

        let mutated_params: Vec<(String, String)> = plan
            .mutated_fields
            .iter()
            .map(|field| {
                let var_name = extract_field(&mut pre_extract_docs, self, field);
                (field.clone(), var_name)
            })
            .collect();

        (pre_extract_docs, readonly_params, mutated_params)
    }

    /// Builds a combined field params map from readonly and mutated params.
    fn build_field_params_map(
        readonly_params: &[(String, String)],
        mutated_params: &[(String, String)],
    ) -> std::collections::HashMap<String, String> {
        let mut all_field_params: std::collections::HashMap<String, String> =
            readonly_params.iter().cloned().collect();
        for (field, var) in mutated_params {
            all_field_params.insert(field.clone(), var.clone());
        }
        all_field_params
    }

    /// Builds the param list document for hybrid mode: `(Var1, ..., VarN, RField1, ..., MField1, ...)`.
    fn build_hybrid_param_list(
        local_param_names: &[String],
        readonly_param_names: &[String],
        mutated_param_names: &[String],
    ) -> Document<'static> {
        join(
            local_param_names
                .iter()
                .map(|v| leaf::var(v.clone()))
                .chain(readonly_param_names.iter().map(|v| leaf::var(v.clone())))
                .chain(mutated_param_names.iter().map(|v| leaf::var(v.clone()))),
            &Document::Str(", "),
        )
    }

    /// Generates the condition expression within hybrid loop context.
    ///
    /// Sets up hybrid field params, generates the condition, and restores state on exit.
    fn generate_hybrid_condition(
        &mut self,
        condition: &Expression,
        plan: &ThreadingPlan,
        all_field_params: &std::collections::HashMap<String, String>,
    ) -> Result<Document<'static>> {
        let prev_readonly_field_params = std::mem::replace(
            &mut self.hybrid_readonly_field_params,
            all_field_params.clone(),
        );
        let prev_mutated_fields = std::mem::replace(
            &mut self.hybrid_mutated_fields,
            plan.mutated_fields.iter().cloned().collect(),
        );
        let cond_result = self.with_branch_context(|this| {
            let prev_hybrid = this.in_hybrid_loop;
            this.in_hybrid_loop = true;
            let result = if let Expression::Block(cond_block) = condition {
                // BT-3151: see the analogous check in `generate_while_loop`.
                let analysis =
                    crate::codegen::core_erlang::block_analysis::analyze_block(cond_block);
                this.check_no_unsafe_class_method_self_sends(&analysis, cond_block.span)
                    .and_then(|()| this.generate_block_body(cond_block))
            } else {
                this.generate_expression(condition)
            };
            this.in_hybrid_loop = prev_hybrid;
            result
        });

        self.hybrid_readonly_field_params = prev_readonly_field_params;
        self.hybrid_mutated_fields = prev_mutated_fields;
        cond_result
    }

    /// Generates the body of a hybrid loop and captures final mutated field var names.
    ///
    /// Returns the body document and the final mutated field argument names.
    /// Saves and restores all hybrid loop state (`in_hybrid_loop`, `in_direct_params_loop`,
    /// `hybrid_readonly_field_params`, `hybrid_mutated_fields`).
    pub(super) fn generate_hybrid_loop_body(
        &mut self,
        body: &Block,
        plan: &ThreadingPlan,
        all_field_params: &std::collections::HashMap<String, String>,
        mutated_params: &[(String, String)],
    ) -> Result<(Document<'static>, Vec<String>)> {
        let prev_hybrid = self.in_hybrid_loop;
        let prev_direct_params_loop = self.in_direct_params_loop;
        let prev_readonly_field_params = std::mem::replace(
            &mut self.hybrid_readonly_field_params,
            all_field_params.clone(),
        );
        let prev_mutated_fields = std::mem::replace(
            &mut self.hybrid_mutated_fields,
            plan.mutated_fields.iter().cloned().collect(),
        );
        self.in_hybrid_loop = true;
        self.in_direct_params_loop = true;
        let body_result = self.generate_threaded_loop_body(body, plan, &BodyKind::Letrec);

        // BT-1342: Capture final mutated field var names BEFORE restoring maps.
        let final_mutated_field_args: Vec<String> = plan
            .mutated_fields
            .iter()
            .map(|field| {
                self.hybrid_readonly_field_params
                    .get(field)
                    .cloned()
                    .unwrap_or_else(|| {
                        mutated_params.iter().find(|(f, _)| f == field).map_or_else(
                            || {
                                unreachable!(
                                    "hybrid while: missing mutated field mapping for `{field}`"
                                )
                            },
                            |(_, v)| v.clone(),
                        )
                    })
            })
            .collect();

        self.hybrid_readonly_field_params = prev_readonly_field_params;
        self.hybrid_mutated_fields = prev_mutated_fields;
        self.in_hybrid_loop = prev_hybrid;
        self.in_direct_params_loop = prev_direct_params_loop;
        let (body_doc, _) = body_result?;
        Ok((body_doc, final_mutated_field_args))
    }

    /// Collects the current Core Erlang variable names for each threaded local after the body executes.
    ///
    /// Uses the current scope bindings; falls back to the canonical `to_core_erlang_var` name
    /// if the variable has not been rebound in this iteration.
    pub(super) fn collect_final_local_args(&self, plan: &ThreadingPlan) -> Vec<String> {
        plan.threaded_locals
            .iter()
            .map(|v| {
                self.lookup_var(v)
                    .cloned()
                    .unwrap_or_else(|| CoreErlangGenerator::to_core_erlang_var(v))
            })
            .collect()
    }

    /// Appends the recursive call and exit arm to the while loop docs.
    #[allow(clippy::too_many_arguments)]
    fn append_hybrid_loop_tail(
        docs: &mut Vec<Document<'static>>,
        negate: bool,
        arity: usize,
        final_local_args: &[String],
        readonly_param_names: &[String],
        final_mutated_field_args: Vec<String>,
        exit_stateacc: Document<'static>,
    ) {
        let final_args_doc = join(
            final_local_args
                .iter()
                .map(|v| leaf::var(v.clone()))
                .chain(readonly_param_names.iter().map(|v| leaf::var(v.clone())))
                .chain(final_mutated_field_args.into_iter().map(leaf::var)),
            &Document::Str(", "),
        );
        let exit_arm = if negate {
            "<'true'> when 'true' -> "
        } else {
            "<'false'> when 'true' -> "
        };
        docs.push(docvec![
            " apply ",
            leaf::fname("while", arity),
            " (",
            final_args_doc,
            ") ",
            exit_arm,
            exit_stateacc,
            " end ",
        ]);
    }

    /// BT-2908: Generates the fallback method body for `whileTrue`/`whileFalse`
    /// — Block's `whileTrue:`/`whileFalse:`. Reached only when something
    /// bypasses the call-site interception these selectors normally get (e.g.
    /// `perform:`/`perform:withArguments:`) — the same gap BT-2812 closed for
    /// Block's `value*` family. See
    /// `generate_block_value_structural_fallback`'s doc comment for the full
    /// rationale (Tier 1/Tier 2 discrimination via `erlang:is_function/2`
    /// arity, the arity-ambiguity caveat, and the `stateful_block_dispatch`
    /// error shape).
    ///
    /// Unlike `value*` (a single receiver to discriminate), both the
    /// *condition* (`Self`) and the *body* (`current_method_params[0]`) must
    /// independently be Tier 1 for the generic loop below to be correct —
    /// either being Tier 2 raises `stateful_block_dispatch`, mirroring
    /// `generate_block_value_structural_fallback`'s single-receiver case.
    pub(in crate::codegen::core_erlang) fn generate_while_structural_fallback(
        &mut self,
        intrinsic_name: &str,
        negate: bool,
        real_selector: &str,
        class_name: &str,
    ) -> Document<'static> {
        let self_var = if self.in_class_method() {
            "ClassSelf"
        } else {
            "Self"
        };
        let body_param = self
            .current_method_params
            .first()
            .cloned()
            .unwrap_or_else(|| "BodyBlock".to_string());

        let runtime_module =
            super::super::primitive_bindings::PrimitiveBindingTable::runtime_module_for_class(
                class_name,
            );
        let placeholder_branch = docvec![
            "call ",
            leaf::atom(runtime_module),
            ":'dispatch'(",
            leaf::atom(intrinsic_name),
            ", [",
            leaf::var(body_param.clone()),
            "], ",
            Document::Str(self_var),
            ")",
        ];

        let (continue_atom, exit_atom) = if negate {
            ("'false'", "'true'")
        } else {
            ("'true'", "'false'")
        };
        let loop_fn = self.fresh_temp_var("Loop");
        let tier1_loop = docvec![
            "letrec ",
            leaf::fname(loop_fn.clone(), 0),
            " = fun () -> case apply ",
            Document::Str(self_var),
            " () of <",
            continue_atom,
            "> when 'true' -> let _ = apply ",
            leaf::var(body_param.clone()),
            " () in apply ",
            leaf::fname(loop_fn.clone(), 0),
            " () <",
            exit_atom,
            "> when 'true' -> 'nil' end in apply ",
            leaf::fname(loop_fn, 0),
            " ()",
        ];

        let body_stateful_error = self.generate_stateful_block_dispatch_error(
            real_selector,
            class_name,
            STATEFUL_BLOCK_DISPATCH_HINT,
        );
        let body_tier_check = docvec![
            "case call 'erlang':'is_function'(",
            leaf::var(body_param.clone()),
            ", 0) of <'true'> when 'true' -> ",
            tier1_loop,
            " <'false'> when 'true' -> case call 'erlang':'is_function'(",
            leaf::var(body_param),
            ", 1) of <'true'> when 'true' -> ",
            body_stateful_error,
            " <'false'> when 'true' -> ",
            placeholder_branch.clone(),
            " end end",
        ];

        let self_stateful_error = self.generate_stateful_block_dispatch_error(
            real_selector,
            class_name,
            STATEFUL_BLOCK_DISPATCH_HINT,
        );
        docvec![
            "case call 'erlang':'is_function'(",
            Document::Str(self_var),
            ", 0) of <'true'> when 'true' -> ",
            body_tier_check,
            " <'false'> when 'true' -> case call 'erlang':'is_function'(",
            Document::Str(self_var),
            ", 1) of <'true'> when 'true' -> ",
            self_stateful_error,
            " <'false'> when 'true' -> ",
            placeholder_branch,
            " end end",
        ]
    }

    /// BT-2908: Generates the fallback method body for `repeat` — Block's
    /// `repeat`. See `generate_while_structural_fallback` for the general
    /// rationale; `repeat` only has a receiver to discriminate (no argument
    /// block — `[self processNextMessage] repeat` takes no arguments).
    pub(in crate::codegen::core_erlang) fn generate_repeat_structural_fallback(
        &mut self,
        class_name: &str,
    ) -> Document<'static> {
        let self_var = if self.in_class_method() {
            "ClassSelf"
        } else {
            "Self"
        };

        let runtime_module =
            super::super::primitive_bindings::PrimitiveBindingTable::runtime_module_for_class(
                class_name,
            );
        let placeholder_branch = docvec![
            "call ",
            leaf::atom(runtime_module),
            ":'dispatch'('repeat', [], ",
            Document::Str(self_var),
            ")",
        ];

        let loop_fn = self.fresh_temp_var("Loop");
        let tier1_loop = docvec![
            "letrec ",
            leaf::fname(loop_fn.clone(), 0),
            " = fun () -> let _ = apply ",
            Document::Str(self_var),
            " () in apply ",
            leaf::fname(loop_fn.clone(), 0),
            " () in apply ",
            leaf::fname(loop_fn, 0),
            " ()",
        ];

        let stateful_error = self.generate_stateful_block_dispatch_error(
            "repeat",
            class_name,
            STATEFUL_BLOCK_DISPATCH_HINT,
        );
        docvec![
            "case call 'erlang':'is_function'(",
            Document::Str(self_var),
            ", 0) of <'true'> when 'true' -> ",
            tier1_loop,
            " <'false'> when 'true' -> case call 'erlang':'is_function'(",
            Document::Str(self_var),
            ", 1) of <'true'> when 'true' -> ",
            stateful_error,
            " <'false'> when 'true' -> ",
            placeholder_branch,
            " end end",
        ]
    }

    /// Appends the initial call to a hybrid loop function.
    pub(super) fn append_hybrid_initial_call(
        docs: &mut Vec<Document<'static>>,
        fn_name: &str,
        arity: usize,
        initial_local_args: Vec<String>,
        readonly_param_names: &[String],
        mutated_param_names: &[String],
    ) {
        let initial_args_doc = join(
            initial_local_args
                .into_iter()
                .map(leaf::var)
                .chain(readonly_param_names.iter().map(|v| leaf::var(v.clone())))
                .chain(mutated_param_names.iter().map(|v| leaf::var(v.clone()))),
            &Document::Str(", "),
        );
        docs.push(docvec![
            "in apply ",
            leaf::fname(fn_name.to_string(), arity),
            " (",
            initial_args_doc,
            ")",
        ]);
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::core_erlang::tests::codegen;

    #[test]
    fn test_while_true_simple_generates_letrec() {
        // Pure whileTrue: (no mutations) generates a letrec-based loop
        let src =
            "Actor subclass: Runner\n  state: x = 0\n\n  run =>\n    [false] whileTrue: [42]\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "whileTrue: should generate a letrec for the loop. Got:\n{code}"
        );
        // Simple variant evaluates condition via case apply (distinguishing it from repeat)
        assert!(
            code.contains("case apply"),
            "whileTrue: should evaluate condition via case apply. Got:\n{code}"
        );
        assert!(
            code.contains("<'true'> when 'true'"),
            "whileTrue: should match true to continue. Got:\n{code}"
        );
        assert!(
            code.contains("<'false'> when 'true' -> 'nil'"),
            "whileTrue: should return nil when condition is false. Got:\n{code}"
        );
    }

    #[test]
    fn test_while_false_simple_generates_letrec_with_opposite_pattern() {
        // Pure whileFalse: (no mutations) generates a letrec matching false to continue
        let src =
            "Actor subclass: Runner\n  state: x = 0\n\n  run =>\n    [false] whileFalse: [42]\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "whileFalse: should generate a letrec. Got:\n{code}"
        );
        assert!(
            code.contains("<'false'> when 'true' ->"),
            "whileFalse: should continue when condition is false. Got:\n{code}"
        );
        assert!(
            code.contains("<'true'> when 'true' -> 'nil'"),
            "whileFalse: should stop when condition is true. Got:\n{code}"
        );
    }

    #[test]
    fn test_while_true_with_field_mutation_threads_actor_state() {
        // whileTrue: with field mutation uses actor state threading (not simple letrec)
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    [self.n < 10] whileTrue: [self.n := self.n + 1]\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "whileTrue: with mutation should generate a letrec. Got:\n{code}"
        );
        // State-threading variant uses 'while'/1 with StateAcc parameter
        assert!(
            code.contains("'while'/1"),
            "Mutating whileTrue: should use 'while'/1 with state parameter. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('n'"),
            "whileTrue: body should update 'n' via maps:put. Got:\n{code}"
        );
    }

    // ── BT-1275: direct-params optimisation ──────────────────────────────────

    #[test]
    fn test_while_true_local_var_only_uses_direct_params() {
        // whileTrue: with only local-var mutations uses direct fun params, not StateAcc.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    [sum < 10] whileTrue: [sum := sum + 1]\n    self.n := sum\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "whileTrue: with local mutation should generate a letrec. Got:\n{code}"
        );
        // Direct-params: fun takes (Sum) not (StateAcc).
        assert!(
            code.contains("fun (Sum)"),
            "direct-params: whileTrue: fun should take Sum as direct param. Got:\n{code}"
        );
        assert!(
            !code.contains("fun (StateAcc)"),
            "direct-params: whileTrue: fun must not use StateAcc signature. Got:\n{code}"
        );
        // Exactly one maps:put for exit StateAcc rebuild.
        assert!(
            code.match_indices("maps':'put'('__local__sum'").count() == 1,
            "direct-params: whileTrue: at most one maps:put (exit rebuild). Got:\n{code}"
        );
        assert!(
            code.contains("ExitSA"),
            "direct-params: whileTrue: exit StateAcc rebuild expected. Got:\n{code}"
        );
    }

    #[test]
    fn test_while_false_local_var_only_uses_direct_params() {
        // whileFalse: with only local-var mutations uses direct fun params.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    [sum >= 10] whileFalse: [sum := sum + 1]\n    self.n := sum\n";
        let code = codegen(src);
        assert!(
            code.contains("fun (Sum)"),
            "direct-params: whileFalse: fun should take Sum as direct param. Got:\n{code}"
        );
        assert!(
            !code.contains("fun (StateAcc)"),
            "direct-params: whileFalse: fun must not use StateAcc signature. Got:\n{code}"
        );
        assert!(
            code.contains("ExitSA"),
            "direct-params: whileFalse: exit StateAcc rebuild expected. Got:\n{code}"
        );
    }

    // ── BT-1326/BT-1342: full-extract direct-params + field extraction ───────

    #[test]
    fn test_while_true_field_plus_local_mutation_uses_full_extract() {
        // BT-1342: whileTrue: with BOTH local var mutation AND field mutation uses
        // full-extract mode — mutated field 'n' is a direct param, no State param.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    [sum < 10] whileTrue: [sum := sum + 1. self.n := self.n + 1]\n    sum\n";
        let code = codegen(src);
        assert!(
            code.contains("letrec"),
            "full-extract: whileTrue: should generate a letrec. Got:\n{code}"
        );
        // No State in fun signature — mutated field 'n' is a direct param.
        assert!(
            !code.contains("fun (Sum, State)"),
            "full-extract: whileTrue: fun must not have State param. Got:\n{code}"
        );
        assert!(
            !code.contains("fun (StateAcc)"),
            "full-extract: whileTrue: fun must not use StateAcc signature. Got:\n{code}"
        );
        // Mutated field 'n' pre-extracted as NField param.
        assert!(
            code.contains("NField"),
            "full-extract: whileTrue: 'n' should be extracted as NField param. Got:\n{code}"
        );
        // Exit rebuilds StateAcc with repacked field + locals.
        assert!(
            code.contains("ExitSA"),
            "full-extract: whileTrue: exit StateAcc rebuild expected. Got:\n{code}"
        );
        // Exit packs mutated field + local into ExitSA.
        assert!(
            code.contains("maps':'put'('n'"),
            "full-extract: whileTrue: exit arm must repack 'n'. Got:\n{code}"
        );
        assert!(
            code.match_indices("maps':'put'('__local__sum'").count() <= 1,
            "full-extract: whileTrue: at most one maps:put for local (exit rebuild). Got:\n{code}"
        );
    }

    #[test]
    fn test_while_true_readonly_field_pre_extracted_as_direct_param() {
        // BT-1326/BT-1342: whileTrue: body reads self.step (never written) and writes self.n:
        // self.step is pre-extracted as read-only param, self.n is pre-extracted as mutated param.
        let src = "Actor subclass: Ctr\n  state: n = 0\n  state: step = 1\n\n  run =>\n    sum := 0\n    [sum < 10] whileTrue: [sum := sum + self.step. self.n := self.n + 1]\n    sum\n";
        let code = codegen(src);
        assert!(
            !code.contains("fun (StateAcc)"),
            "readonly field: whileTrue: must not use StateAcc signature. Got:\n{code}"
        );
        // 'step' is read-only — pre-extracted before letrec.
        assert!(
            code.contains("StepField"),
            "readonly field: whileTrue: fun should have a StepField param. Got:\n{code}"
        );
        // Only one maps:get for 'step' (the pre-extraction).
        assert!(
            code.match_indices("maps':'get'('step'").count() == 1,
            "readonly field: whileTrue: exactly one maps:get for 'step'. Got:\n{code}"
        );
        // Mutable field 'n' is also pre-extracted and repacked at exit.
        assert!(
            code.contains("NField"),
            "readonly field: whileTrue: 'n' should be extracted as NField param. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'put'('n'"),
            "readonly field: whileTrue: exit arm must repack 'n'. Got:\n{code}"
        );
    }

    // ── BT-1343: codegen diagnostics ─────────────────────────────────────────

    fn codegen_with_diagnostics(
        src: &str,
        enabled: bool,
    ) -> (String, Vec<crate::source_analysis::Diagnostic>) {
        let tokens = crate::source_analysis::lex_with_eof(src);
        let (module, _) = crate::source_analysis::parse(tokens);
        let result = crate::codegen::core_erlang::generate_module_with_warnings(
            &module,
            crate::codegen::core_erlang::CodegenOptions::new("test")
                .with_workspace_mode(true)
                .with_codegen_diagnostics(enabled),
        )
        .expect("codegen should succeed");
        (result.code, result.warnings)
    }

    #[test]
    fn test_bt1343_diagnostics_off_by_default() {
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    x := 0\n    [x < 10] whileTrue: [x := x + 1]\n";
        let (_, warnings) = codegen_with_diagnostics(src, false);
        let codegen_diags: Vec<&str> = warnings
            .iter()
            .filter(|w| {
                let m = w.message.as_str();
                m.contains("direct-params")
                    || m.contains("tuple-acc")
                    || m.contains("StateAcc fallback")
                    || m.contains("dynamic dispatch")
            })
            .map(|w| w.message.as_str())
            .collect();
        assert!(
            codegen_diags.is_empty(),
            "Expected no codegen diagnostics when disabled. Got: {codegen_diags:?}"
        );
    }

    #[test]
    fn test_bt1343_direct_params_diagnostic() {
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    x := 0\n    [x < 10] whileTrue: [x := x + 1]\n";
        let (_, warnings) = codegen_with_diagnostics(src, true);
        let diag_msgs: Vec<&str> = warnings.iter().map(|w| w.message.as_str()).collect();
        assert!(
            diag_msgs.iter().any(|m| m.contains("direct-params")),
            "Expected direct-params diagnostic. Got: {diag_msgs:?}"
        );
    }

    #[test]
    fn test_bt1343_stateacc_fallback_diagnostic() {
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    [self.n < 10] whileTrue: [self.n := self.n + 1]\n";
        let (_, warnings) = codegen_with_diagnostics(src, true);
        let diag_msgs: Vec<&str> = warnings.iter().map(|w| w.message.as_str()).collect();
        assert!(
            diag_msgs
                .iter()
                .any(|m| m.contains("StateAcc fallback") || m.contains("hybrid")),
            "Expected StateAcc or hybrid diagnostic for field-only mutations. Got: {diag_msgs:?}"
        );
    }

    // ── BT-1609: value-type local threading ─────────────────────────────

    #[test]
    fn test_value_type_while_true_extracts_threaded_locals() {
        // BT-1609: whileTrue: in value-type context must extract threaded locals
        // from the returned {'nil', StateAcc} tuple after the loop.
        let src = "Object subclass: Calc\n\n  run =>\n    counter := 3\n    steps := 0\n    [counter > 0] whileTrue: [\n      counter := counter - 1\n      steps := steps + 1\n    ]\n    steps\n";
        let code = codegen(src);
        assert!(
            code.contains("WhileResult") || code.contains("whileResult"),
            "value-type whileTrue: should bind loop result to WhileResult var. Got:\n{code}"
        );
        assert!(
            code.contains("element'(2,"),
            "value-type whileTrue: should extract state from element 2. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'get'('__local__counter'"),
            "value-type whileTrue: should extract counter from state. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'get'('__local__steps'"),
            "value-type whileTrue: should extract steps from state. Got:\n{code}"
        );
    }

    #[test]
    fn test_value_type_while_false_extracts_threaded_locals() {
        let src = "Object subclass: Calc\n\n  run =>\n    counter := 0\n    [counter >= 3] whileFalse: [\n      counter := counter + 1\n    ]\n    counter\n";
        let code = codegen(src);
        assert!(
            code.contains("WhileResult") || code.contains("whileResult"),
            "value-type whileFalse: should bind loop result. Got:\n{code}"
        );
        assert!(
            code.contains("maps':'get'('__local__counter'"),
            "value-type whileFalse: should extract counter from state. Got:\n{code}"
        );
    }

    #[test]
    fn test_value_type_while_last_expr_unwraps_nil() {
        // BT-2308: a mutating whileTrue: as the method's LAST expression must return the
        // loop's logical value (element 1 = nil), not the raw {nil, StateAcc} tuple.
        let src =
            "Object subclass: Calc\n\n  run =>\n    n := 0\n    [n < 3] whileTrue: [n := n + 1]\n";
        let code = codegen(src);
        assert!(
            code.contains("element'(1,"),
            "last-expr whileTrue: should unwrap element 1 (nil). Got:\n{code}"
        );
    }

    // ── ADR 0111 Addendum 2 pilot (BT-3145): ThreadedIr measurement gate ──

    /// Runs `codegen(src)` with `BEAMTALK_THREADED_IR_WHILE_DIRECT` forced to
    /// the given value for the duration of the call. Mutates process-global
    /// state (`std::env`), so callers must not assume isolation from other
    /// concurrently-running tests — acceptable here because the flag's own
    /// contract is "identical output when representable, an untouched
    /// legacy passthrough otherwise" (`try_render_while_direct_via_threaded_ir`'s
    /// doc comment), so a concurrently-running test observing the flag
    /// toggled cannot itself produce different output because of it.
    fn codegen_with_threaded_ir_while_direct(src: &str, enabled: bool) -> String {
        // SAFETY: single-process test env var toggle around a single
        // synchronous `codegen` call, restored to unset immediately after —
        // no other thread in this process reads/writes this specific
        // variable concurrently with a way to observe a torn value (see
        // this fn's caller-side doc comment for why a racy toggle is
        // tolerable here regardless).
        unsafe {
            if enabled {
                std::env::set_var("BEAMTALK_THREADED_IR_WHILE_DIRECT", "1");
            } else {
                std::env::remove_var("BEAMTALK_THREADED_IR_WHILE_DIRECT");
            }
        }
        let code = codegen(src);
        // SAFETY: same as above — restoring the env var to unset.
        unsafe {
            std::env::remove_var("BEAMTALK_THREADED_IR_WHILE_DIRECT");
        }
        code
    }

    #[test]
    fn threaded_ir_while_direct_single_local_byte_identical_to_legacy() {
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    [sum < 10] whileTrue: [sum := sum + 1]\n    self.n := sum\n";
        let legacy = codegen_with_threaded_ir_while_direct(src, false);
        let via_ir = codegen_with_threaded_ir_while_direct(src, true);
        assert_eq!(
            legacy, via_ir,
            "ThreadedIr-rendered while-direct output must be byte-identical \
             to the legacy path for a single-local straight-line body"
        );
    }

    #[test]
    fn threaded_ir_while_direct_multi_local_byte_identical_to_legacy() {
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    count := 0\n    [sum < 10] whileTrue: [sum := sum + 1. count := count + 1]\n    self.n := sum + count\n";
        let legacy = codegen_with_threaded_ir_while_direct(src, false);
        let via_ir = codegen_with_threaded_ir_while_direct(src, true);
        assert_eq!(
            legacy, via_ir,
            "ThreadedIr-rendered while-direct output must be byte-identical \
             to the legacy path for a multi-local straight-line body"
        );
    }

    #[test]
    fn threaded_ir_while_direct_negated_condition_byte_identical_to_legacy() {
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    [sum >= 10] whileFalse: [sum := sum + 1]\n    self.n := sum\n";
        let legacy = codegen_with_threaded_ir_while_direct(src, false);
        let via_ir = codegen_with_threaded_ir_while_direct(src, true);
        assert_eq!(
            legacy, via_ir,
            "ThreadedIr-rendered whileFalse: (negated) output must be \
             byte-identical to the legacy path"
        );
    }

    #[test]
    fn threaded_ir_while_direct_same_local_rebound_twice_in_one_iteration_byte_identical_to_legacy()
    {
        // `sum` is reassigned TWICE within a single loop iteration — exercises
        // `try_render_while_direct_via_threaded_ir`'s `current`/`rebind_counts`
        // chaining (the second rebind's `Bind::source` must be the first
        // rebind's own fresh `Gensym` target, not the fun's version-0 param),
        // a code path the single/multi-local tests above (one rebind per
        // local) don't reach.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    [sum < 10] whileTrue: [sum := sum + 1. sum := sum * 2]\n    self.n := sum\n";
        let legacy = codegen_with_threaded_ir_while_direct(src, false);
        let via_ir = codegen_with_threaded_ir_while_direct(src, true);
        assert_eq!(
            legacy, via_ir,
            "ThreadedIr-rendered while-direct output must be byte-identical \
             to the legacy path when the same local is rebound twice in one \
             iteration"
        );
    }

    #[test]
    fn threaded_ir_while_direct_field_read_in_rhs_byte_identical_to_legacy() {
        // A field READ (not a write) in the rebind's RHS — `select_direct_params`
        // permits this (only WRITES/self-sends disqualify direct-params), and
        // `is_simple_threaded_rhs` allows `FieldAccess` through, so this must
        // still route through `ConditionalLoop` and match legacy exactly.
        let src = "Actor subclass: Ctr\n  state: step = 1\n  state: n = 0\n\n  run =>\n    sum := 0\n    [sum < 10] whileTrue: [sum := sum + self.step]\n    self.n := sum\n";
        let legacy = codegen_with_threaded_ir_while_direct(src, false);
        let via_ir = codegen_with_threaded_ir_while_direct(src, true);
        assert_eq!(
            legacy, via_ir,
            "ThreadedIr-rendered while-direct output must be byte-identical \
             to the legacy path when the rebind RHS reads a field"
        );
    }

    #[test]
    fn threaded_ir_while_direct_falls_back_for_unrepresentable_body() {
        // A plain-let temporary (`half`, never a threaded local) inside the
        // loop body — outside this pilot's narrow `Bind`-chain coverage
        // (`while_direct_body_is_bind_representable`'s doc comment) — must
        // fall back to the legacy path untouched, not error or diverge.
        let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run =>\n    sum := 0\n    [sum < 10] whileTrue: [half := sum / 2. sum := sum + 1 + half - half]\n    self.n := sum\n";
        let legacy = codegen_with_threaded_ir_while_direct(src, false);
        let via_ir = codegen_with_threaded_ir_while_direct(src, true);
        assert_eq!(
            legacy, via_ir,
            "an unrepresentable body must fall back to the legacy path \
             byte-for-byte, flag on or off"
        );
    }
}
