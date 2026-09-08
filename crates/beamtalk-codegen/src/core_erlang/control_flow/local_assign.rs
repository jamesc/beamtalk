// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Local-variable assignment codegen inside state-threaded loop bodies.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! BT-3459: split out of `control_flow/mod.rs`, no logic changes.

use super::super::threaded_ir::{
    BindOp, FrameId, ThreadedStmt, ValueRef, VersionPrefix, VersionedVar,
};
use super::super::{CodeGenError, CoreErlangGenerator, Result};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, leaf};
use beamtalk_core::ast::Expression;
use beamtalk_core::source_analysis::Span;

impl CoreErlangGenerator {
    /// BT-1224: Try to generate a plain `let Var = value in` binding for a block-local
    /// variable assignment that does NOT need `StateAcc` threading.
    ///
    /// Returns `Some(doc)` when the assignment is:
    /// - not the last expression in the block (`!is_last`)
    /// - not in the `threaded` set (block-local, not captured from outer scope)
    /// - not in REPL mode (REPL always uses `StateAcc` for local vars)
    ///
    /// Returns `None` when the variable needs `StateAcc` threading (caller should use
    /// `generate_local_var_assignment_in_loop` instead).
    pub(super) fn try_generate_block_local_plain_let(
        &mut self,
        expr: &Expression,
        is_last: bool,
        threaded: &[String],
    ) -> Result<Option<Document<'static>>> {
        if is_last || self.is_repl_mode() {
            return Ok(None);
        }
        let Expression::Assignment { target, value, .. } = expr else {
            return Ok(None);
        };
        let Expression::Identifier(id) = target.as_ref() else {
            return Ok(None);
        };
        if threaded.contains(&id.name.to_string()) {
            return Ok(None);
        }
        // BT-912: Tier-2 block calls return {Result, NewStateAcc}. Fall back to
        // generate_local_var_assignment_in_loop which already handles Tier-2 unpacking
        // and StateAcc propagation correctly.
        if self.is_tier2_value_call(value) {
            return Ok(None);
        }
        // BT-3428: a control-flow-with-mutations RHS (e.g. a mutating list-op
        // like `collect:` whose block mutates a DIFFERENT outer local than
        // `id.name`, or a nested `ifTrue:ifFalse:`/`match:`/`on:do:` with
        // mutations) also returns a closed `{Value, StateAcc}` 2-tuple — same
        // shape as the Tier 2 case just above. `id.name` itself may correctly
        // be absent from `threaded` (this assignment's own target needs no
        // StateAcc threading), but the RHS's *other* mutated locals still do,
        // and the plain `let core_var = <val_doc> in` below has no way to
        // unwrap the tuple or rethread them — it would bind `core_var` to the
        // raw tuple and silently drop every other local the RHS's block
        // mutated. Fall back to `generate_local_var_assignment_in_loop`,
        // which (as of BT-3428) unwraps this exact shape and rebinds those
        // other locals via `push_control_flow_threaded_var_rereads`'s
        // loop-body counterpart.
        if self.control_flow_has_mutations(value) {
            return Ok(None);
        }
        let core_var = self
            .lookup_var(&id.name)
            .map_or_else(|| Self::to_core_erlang_var(&id.name), String::clone);
        // ADR 0118 phase 5b (BT-3422): a class-method self-send on the RHS
        // (`x := self bump`), at any nesting depth, threads as a real
        // prelude via `threaded_expression` — spliced ahead of this
        // `let core_var = ... in` (mirrors
        // `generate_local_var_assignment_in_loop`'s BT-1397 fix, now built
        // on `ThreadedValue` rather than an open-chain side channel).
        let frame = self.current_frame();
        let tv = self.threaded_expression(value, frame)?;
        let prelude_doc = self.threaded_prelude_doc(&tv.prelude);
        let value_doc = self.threaded_value_doc(&tv.value);
        self.bind_var(&id.name, &core_var);
        let doc = docvec![
            prelude_doc,
            "let ",
            leaf::var(core_var),
            " = ",
            value_doc,
            " in ",
        ];
        Ok(Some(doc))
    }

    /// BT-1275: Generate a local variable assignment in a direct-params loop body.
    ///
    /// In direct-params mode, threaded locals are fun parameters — no `StateAcc` map needed.
    /// Generates `let NewVar = <value> in` and updates the binding so subsequent
    /// uses and the recursive `apply` pick up the latest version.
    ///
    /// ```erlang
    /// %% Old StateAcc pattern:
    /// let _Val5 = Sum + I in let StateAcc1 = maps:put('__local__sum', _Val5, StateAcc) in
    ///
    /// %% Direct params pattern (this function):
    /// let Sum1 = Sum + I in
    /// ```
    ///
    /// Returns `(doc, Some(new_var_name))` so callers (e.g. `emit_local_assign_last_expr`)
    /// can reference the newly-bound variable by name (e.g. for FoldlCollect/FoldlInject).
    ///
    /// BT-3150 review follow-up: unlike `try_generate_block_local_plain_let`, `value`
    /// here never needs open-scope handling for a class-method self-send RHS
    /// (`x := self bump`) — `use_direct_params`/`use_tuple_acc`/`use_hybrid_params`
    /// (this function's only callers, see `generate_threaded_loop_body_inner`) are
    /// all unconditionally disabled whenever the block has *any* self-send
    /// (`BlockMutationAnalysis::has_state_effects`/`has_self_sends`, checked by
    /// `select_direct_params`/`select_tuple_acc`/`select_hybrid_params`), so `value`
    /// can never be, or contain at this level, one.
    pub(super) fn generate_direct_var_update_in_loop(
        &mut self,
        expr: &Expression,
    ) -> Result<(Document<'static>, Option<String>)> {
        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::Identifier(id) = target.as_ref() {
                // BT-1329: Clear any pending list op result before generating the value.
                self.direct_params_list_op_result = None;
                let value_code = self.expression_doc(value)?;

                // BT-1329: If the value expression was a list op in direct-params mode,
                // it produced an open let-chain and stored the result variable name.
                // We emit the chain directly (so variable rebindings escape to outer scope),
                // then bind the assigned variable to the stored result.
                if let Some(result_var) = self.direct_params_list_op_result.take() {
                    let new_var =
                        self.fresh_temp_var(&CoreErlangGenerator::to_core_erlang_var(&id.name));
                    self.bind_var(&id.name, &new_var);
                    let doc = docvec![
                        value_code,
                        "let ",
                        leaf::var(new_var.clone()),
                        " = ",
                        leaf::var(result_var),
                        " in ",
                    ];
                    return Ok((doc, Some(new_var)));
                }

                // Allocate a fresh versioned name (e.g. Sum1, Sum2 ...) and rebind.
                let new_var =
                    self.fresh_temp_var(&CoreErlangGenerator::to_core_erlang_var(&id.name));
                self.bind_var(&id.name, &new_var);
                let doc = docvec![
                    "let ",
                    leaf::var(new_var.clone()),
                    " = ",
                    value_code,
                    " in ",
                ];
                return Ok((doc, Some(new_var)));
            }
        }
        Ok((Document::Nil, None))
    }

    /// ADR 0111 Addendum 15: `Bind`-producing sibling of
    /// [`Self::generate_direct_var_update_in_loop`], for the Letrec-onto-
    /// `ThreadedIr` body lowering (`control_flow::body::lower_letrec_body`).
    /// Reproduces that function's own two shapes — the common rebind and the
    /// BT-1329 open-let-chain list-op-result case — as a `ThreadedStmt::Bind`
    /// (`render_bind`'s `Direct` arm renders `"let NewVar = <rhs> in "`,
    /// byte-identical to the sibling's own `docvec!`) instead of a hand-built
    /// `Document`, so `render`'s `final_loop_arg_identities` can trace this
    /// local's own rebind chain from its `produces` seed.
    ///
    /// `source` is the local's identity BEFORE this rebind — [`VersionPrefix::Local`]
    /// at the loop's own `produces` seed until the first rebind,
    /// [`VersionPrefix::Gensym`] of the current `lookup_var` name afterward
    /// (mirroring `collect_final_local_args`'s own scope-lookup mechanism, so
    /// the chain this builds always terminates at the SAME final identity
    /// that mechanism would find).
    pub(super) fn lower_direct_var_update_in_loop_bind(
        &mut self,
        expr: &Expression,
        frame: FrameId,
        span: Span,
        stmts: &mut Vec<ThreadedStmt>,
    ) -> Result<()> {
        let Expression::Assignment { target, value, .. } = expr else {
            return Ok(());
        };
        let Expression::Identifier(id) = target.as_ref() else {
            return Ok(());
        };
        let canonical = CoreErlangGenerator::to_core_erlang_var(&id.name);
        let current = self
            .lookup_var(&id.name)
            .cloned()
            .unwrap_or_else(|| canonical.clone());
        let source = if current == canonical {
            VersionedVar::new(VersionPrefix::Local(id.name.to_string()), 0, frame)
        } else {
            VersionedVar::new(VersionPrefix::Gensym(current), 1, frame)
        };

        // BT-1329: Clear any pending list op result before generating the value.
        self.direct_params_list_op_result = None;
        let value_code = self.expression_doc(value)?;

        if let Some(result_var) = self.direct_params_list_op_result.take() {
            let new_var = self.fresh_temp_var(&canonical);
            self.bind_var(&id.name, &new_var);
            stmts.push(ThreadedStmt::Statement(value_code, span));
            stmts.push(ThreadedStmt::Bind {
                target: VersionedVar::new(VersionPrefix::Gensym(new_var), 1, frame),
                source,
                op: BindOp::Direct(ValueRef::Var(result_var)),
                shadow_write: false,
                span,
            });
            return Ok(());
        }

        let new_var = self.fresh_temp_var(&canonical);
        self.bind_var(&id.name, &new_var);
        stmts.push(ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::Gensym(new_var), 1, frame),
            source,
            op: BindOp::Direct(ValueRef::Doc(value_code)),
            shadow_write: false,
            span,
        });
        Ok(())
    }

    /// BT-3428 (Claude Review follow-up on PR #3727): shared by
    /// `generate_local_var_assignment_in_loop`'s Tier 2 value-call case and
    /// its control-flow-with-mutations case — both RHS shapes compile
    /// (`generate_tier2_value_call_doc`/`expression_doc` respectively) to a
    /// closed `{Value, StateAcc}` 2-tuple needing identical treatment:
    /// unwrap element 1 into `val_var`, thread element 2 into `new_state`
    /// via `maps:put`, then rebind any OTHER outer local `source_expr`'s own
    /// block mutated. The rebind is a no-op for a Tier 2 call —
    /// `get_control_flow_threaded_vars` only matches the control-flow
    /// selectors `control_flow_has_mutations` gates the other case on, never
    /// a bare `value`/`value:` call — so folding both callers through this
    /// one helper is structurally identical for the pre-existing Tier 2
    /// case, though not byte-identical: `fresh_temp_var` draws from one
    /// global counter, and this helper now mints `tuple_var`/`tuple_state_var`
    /// *after* `value_code` is built (previously minted before), shifting the
    /// numeric suffixes assigned to those two temps and everything
    /// `generate_tier2_value_call_doc` mints internally — every name stays
    /// unique, and existing tests match this shape with `\w*` wildcards.
    ///
    /// Extracted after a Claude Review finding on this PR: before this
    /// helper, the control-flow-with-mutations case was a fourth
    /// near-identical copy of this idiom (the Tier 2 case here, plus
    /// `conditionals.rs`'s C3/C3b, which use the `ThreadedStmt`-based arm
    /// emitter rather than this file's plain `Document` composition and so
    /// are not folded in here) — exactly the shape of gap this PR's own P7
    /// regression closed for the third copy.
    fn emit_tuple_unwrap_pack_and_rebind(
        &mut self,
        temp_var_prefixes: (&str, &str),
        value_code: Document<'static>,
        val_var: &str,
        state_key: &str,
        new_state: &str,
        source_expr: &Expression,
    ) -> Document<'static> {
        let (tuple_prefix, tuple_state_prefix) = temp_var_prefixes;
        let tuple_var = self.fresh_temp_var(tuple_prefix);
        let tuple_state_var = self.fresh_temp_var(tuple_state_prefix);
        let mut docs = vec![docvec![
            "let ",
            leaf::var(tuple_var.clone()),
            " = ",
            value_code,
            " in let ",
            leaf::var(val_var.to_string()),
            " = call 'erlang':'element'(1, ",
            leaf::var(tuple_var.clone()),
            ") in let ",
            leaf::var(tuple_state_var.clone()),
            " = call 'erlang':'element'(2, ",
            leaf::var(tuple_var),
            ") in let ",
            leaf::var(new_state.to_string()),
            " = call 'maps':'put'(",
            leaf::atom(state_key.to_string()),
            ", ",
            leaf::var(val_var.to_string()),
            ", ",
            leaf::var(tuple_state_var),
            ") in ",
        ]];
        if let Some(threaded_vars) = self.get_control_flow_threaded_vars(source_expr) {
            docs.extend(self.rebind_threaded_vars_from_state(&threaded_vars, new_state));
        }
        Document::Vec(docs)
    }

    /// BT-153: Generate a local variable assignment inside a loop body with state threading.
    ///
    /// Generates code like:
    /// ```erlang
    /// let _Val = <value> in let StateAccN = maps:put('varname', _Val, StateAcc{N-1}) in
    /// ```
    ///
    /// BT-912: When the RHS is a Tier 2 block call returning `{Result, NewStateAcc}`,
    /// unpacks the tuple and uses `NewStateAcc` for `maps:put` so that mutations made
    /// by the called block (e.g. captured variable updates) are preserved in the
    /// threading state rather than discarded.
    #[allow(clippy::too_many_lines)]
    pub(super) fn generate_local_var_assignment_in_loop(
        &mut self,
        expr: &Expression,
    ) -> Result<(Document<'static>, String)> {
        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::Identifier(id) = target.as_ref() {
                let val_var = self.fresh_temp_var("Val");

                // BT-790: In REPL mode, use the plain variable name as the key
                // (no __local__ prefix) since there are no actor fields to collide with.
                // This ensures reads (`maps:get('x', StateAcc)`) match writes
                // (`maps:put('x', ..., StateAcc)`), allowing mutations to accumulate
                // correctly across loop iterations.
                let state_key = if self.is_repl_mode() {
                    id.name.clone()
                } else {
                    Self::local_state_key(&id.name).into()
                };

                // BT-912: If the RHS is a Tier 2 block call, it returns {Result, NewStateAcc}.
                // Unpack the tuple so that:
                //   - `Val` is bound to `Result` (not the whole tuple)
                //   - `maps:put` uses `NewStateAcc` (preserving the block's captured mutations)
                //     rather than the old `StateAcc` (which would discard them).
                if self.is_tier2_value_call(value) {
                    let value_code = self.generate_tier2_value_call_doc(value)?;

                    let _ = self.next_state_var();
                    let new_state = if self.in_loop_body {
                        self.current_state_var()
                    } else {
                        super::super::util::versioned_var("State", self.state_version())
                    };

                    // BT-2703: Rebind the local to the freshly-written value so a later
                    // read *within the same iteration* (`idx := idx + 1` … `x * idx`) sees
                    // the new value instead of the stale iteration-start `maps:get` binding.
                    // Mirrors the tuple-acc path (`generate_direct_var_update_in_loop`).
                    self.bind_var(&id.name, &val_var);

                    // BT-1053: Return val_var so callers (e.g. generate_conditional_branch_inline)
                    // can use it as the branch result.
                    let doc = self.emit_tuple_unwrap_pack_and_rebind(
                        ("T2", "T2St"),
                        value_code,
                        &val_var,
                        &state_key,
                        &new_state,
                        value,
                    );
                    return Ok((doc, val_var));
                }

                // BT-3428: RHS is itself control-flow-with-mutations (e.g. a
                // mutating list-op like `collect:`/`do:`/`select:` whose
                // block — or its own receiver — needs state threading, or a
                // nested `ifTrue:ifFalse:`/`match:`/`on:do:` with mutations).
                // Same `{Value, StateAcc}` 2-tuple shape as the Tier 2 case
                // above, produced by the ordinary `expression_doc` path
                // rather than `generate_tier2_value_call_doc` — must be
                // unwrapped identically, instead of falling into the generic
                // path below (which assumes a single, non-tuple value and
                // would bind `val_var`/`maps:put` to the raw tuple itself).
                // Mirrors `lower_local_var_assignment_bind`'s C3b case
                // (`conditionals.rs`) — the conditional-branch-arm sibling
                // this loop-body function has always structurally
                // paralleled — including its rebind of any OTHER outer local
                // the RHS's own block mutated (BT-3428, found via the review
                // of that C3b fix).
                if self.control_flow_has_mutations(value) {
                    let frame = self.current_frame();
                    let mut prelude_stmts: Vec<ThreadedStmt> = Vec::new();
                    let thread_scope = self.thread_ahead(value, &mut prelude_stmts, frame)?;
                    let prelude_doc = self.threaded_prelude_doc(&prelude_stmts);
                    let value_code = self.expression_doc(value)?;
                    self.finish_precompiled_scope(thread_scope)?;

                    let _ = self.next_state_var();
                    let new_state = if self.in_loop_body {
                        self.current_state_var()
                    } else {
                        super::super::util::versioned_var("State", self.state_version())
                    };

                    self.bind_var(&id.name, &val_var);

                    let doc = self.emit_tuple_unwrap_pack_and_rebind(
                        ("CfTuple", "CfSt"),
                        value_code,
                        &val_var,
                        &state_key,
                        &new_state,
                        value,
                    );
                    return Ok((Document::Vec(vec![prelude_doc, doc]), val_var));
                }

                // ADR 0118 phase 2b (BT-3418): thread every state-effecting
                // sub-expression nested in the RHS (or the RHS itself, `v :=
                // self bump`) ahead of `value`'s own compile — mirrors
                // `lower_local_var_assignment_bind`'s identical `thread_ahead`
                // step (`conditionals.rs`), the branch-arm sibling this
                // loop-body function has always structurally paralleled.
                // Scoped to the non-Tier2 path only, matching that sibling's
                // own scope: a Tier2 value call already returned its own
                // `{Result, NewStateAcc}` tuple above via a dedicated helper
                // that never consults `precompiled_subexprs`, so
                // pre-threading it there would double-compile (and
                // double-dispatch) any self-send nested in its arguments.
                let frame = self.current_frame();
                let mut prelude_stmts: Vec<ThreadedStmt> = Vec::new();
                let thread_scope = self.thread_ahead(value, &mut prelude_stmts, frame)?;
                let prelude_doc = self.threaded_prelude_doc(&prelude_stmts);

                // Capture value expression (ADR 0018 bridge). ADR 0118 phase
                // 5b (BT-3422): `thread_ahead` above already threads any
                // class-var producer nested in `value` (at any depth) as a
                // real prelude, so the plain compile here reads the
                // substituted value back via `precompiled_subexprs` — no
                // open scope reaches this point any more.
                let value_code = self.expression_doc(value)?;
                self.finish_precompiled_scope(thread_scope)?;

                // BT-3418: read AFTER `thread_ahead` above, so a threaded
                // prelude's own state-version bump (e.g. a nested self-send's
                // dispatch `Bind`) is reflected in the `maps:put` source
                // below — reading it any earlier would reference the
                // pre-dispatch state.
                let current_state =
                    super::super::util::versioned_var("StateAcc", self.state_version());

                // Increment state version for the new state
                let _ = self.next_state_var();
                let new_state = if self.in_loop_body {
                    self.current_state_var()
                } else {
                    super::super::util::versioned_var("State", self.state_version())
                };

                // BT-2703: Rebind the local to the freshly-written value so a later read
                // *within the same iteration* sees the new value rather than the stale
                // iteration-start `maps:get` binding (the map-acc analogue of the
                // tuple-acc rebind in `generate_direct_var_update_in_loop`).
                self.bind_var(&id.name, &val_var);

                // BT-1053: Return val_var so callers (e.g. generate_conditional_branch_inline)
                // can use it as the branch result.
                return Ok((
                    docvec![
                        prelude_doc,
                        "let ",
                        leaf::var(val_var.clone()),
                        " = ",
                        value_code,
                        " in let ",
                        leaf::var(new_state),
                        " = call 'maps':'put'(",
                        leaf::atom(state_key.to_string()),
                        ", ",
                        leaf::var(val_var.clone()),
                        ", ",
                        leaf::var(current_state),
                        ") in ",
                    ],
                    val_var,
                ));
            }
        }
        Err(CodeGenError::Internal(
            "generate_local_var_assignment_in_loop called on non-assignment expression".to_string(),
        ))
    }
}
