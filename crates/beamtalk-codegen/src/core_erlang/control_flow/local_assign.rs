// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Local-variable assignment codegen inside state-threaded loop bodies.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! split out of `control_flow/mod.rs`, no logic changes.

use super::super::threaded_ir::{
    BindOp, FrameId, ThreadedStmt, ValueRef, VersionPrefix, VersionedVar,
};
use super::super::{CoreErlangGenerator, Result};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;
use beamtalk_core::ast::Expression;
use beamtalk_core::source_analysis::Span;

impl CoreErlangGenerator {
    /// Try to generate a plain `let Var = value in` binding for a block-local
    /// variable assignment that does NOT need `StateAcc` threading.
    ///
    /// Returns `Some(stmts)` (the RHS's threaded prelude, then the `let`) when
    /// the assignment is:
    /// - not the last expression in the block (`!is_last`)
    /// - not in the `threaded` set (block-local, not captured from outer scope)
    /// - not in REPL mode (REPL always uses `StateAcc` for local vars)
    ///
    /// Returns `None` when the variable needs `StateAcc` threading (caller should use
    /// `lower_local_var_assignment_bind` instead).
    pub(super) fn try_generate_block_local_plain_let(
        &mut self,
        expr: &Expression,
        is_last: bool,
        threaded: &[String],
    ) -> Result<Option<Vec<ThreadedStmt>>> {
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
        // Tier-2 block calls return {Result, NewStateAcc}. Fall back to
        // lower_local_var_assignment_bind which already handles Tier-2 unpacking
        // and StateAcc propagation correctly.
        if self.is_tier2_value_call(value) {
            return Ok(None);
        }
        // a control-flow-with-mutations RHS (e.g. a mutating list-op
        // like `collect:` whose block mutates a DIFFERENT outer local than
        // `id.name`, or a nested `ifTrue:ifFalse:`/`match:`/`on:do:` with
        // mutations) also returns a closed `{Value, StateAcc}` 2-tuple — same
        // shape as the Tier 2 case just above. `id.name` itself may correctly
        // be absent from `threaded` (this assignment's own target needs no
        // StateAcc threading), but the RHS's *other* mutated locals still do,
        // and the plain `let core_var = <val_doc> in` below has no way to
        // unwrap the tuple or rethread them — it would bind `core_var` to the
        // raw tuple and silently drop every other local the RHS's block
        // mutated. Fall back to `lower_local_var_assignment_bind`,
        // which unwraps this exact shape and rebinds those
        // other locals via `push_control_flow_threaded_var_rereads`'s
        // loop-body counterpart.
        //
        // ADR 0131 Phase 2b: not a loop or fold that is a local-threading
        // producer in this body — `threaded_expression` below splices its
        // prelude (the construct tuple and its `LocalRebind`s) and yields
        // the unwrapped value.
        // BT-4000: nor an inline-threaded construct, whose value
        // `threaded_expression` below yields unwrapped (as at the method root).
        if self.control_flow_has_mutations(value)
            && !self.expr_has_local_producer(value)
            && !self.thread_ahead_unwraps(value)
        {
            return Ok(None);
        }
        // BT-3493: the RHS is itself a field write (`r := (self.x :=
        // ...)`, at any parenthesization depth) — its `State`/`StateAcc`
        // mutation must fold into this loop's OWN threaded state, which
        // this plain, unthreaded `let` (built from `threaded_expression`
        // below) has no way to do: `threaded_expression`'s generic
        // value-carrying-parent handling compiles the field write via
        // ordinary `generate_expression`, whose internal state-version bump
        // never becomes a real `Bind` here. Fall back to
        // `lower_local_var_assignment_bind` (via the caller,
        // `lower_letrec_local_var_assignment`), which has its own BT-3493
        // fix for exactly this shape.
        if self.local_assign_field_write(value).is_some() {
            return Ok(None);
        }
        let core_var = self
            .lookup_var(&id.name)
            .map_or_else(|| Self::to_core_erlang_var(&id.name), String::clone);
        // ADR 0118 phase 5b: a class-method self-send on the RHS
        // (`x := self bump`), at any nesting depth, threads as a real
        // prelude via `threaded_expression` — spliced ahead of this
        // `let core_var = ... in` (mirrors
        // `lower_local_var_assignment_bind`'s analogous handling, now
        // built on `ThreadedValue` rather than an open-chain side channel).
        let frame = self.current_frame();
        let tv = self.threaded_expression(value, frame)?;
        let value_doc = self.threaded_value_doc(&tv.value);
        self.bind_var(&id.name, &core_var);
        // The prelude stays IR (spliced into the loop body's own statement
        // list): a local-threading producer's `LocalRebind`s extend the
        // loop's argument chain, which a prelude rendered to a `Document`
        // here would hide from `final_loop_arg_identities`.
        let mut stmts = tv.prelude;
        stmts.push(ThreadedStmt::Statement(
            docvec!["let ", leaf::var(core_var), " = ", value_doc, " in ",],
            expr.span(),
        ));
        Ok(Some(stmts))
    }

    /// ADR 0111 Addendum 15: `Bind`-producing local-var-update lowering shared
    /// by both `Letrec` (`control_flow::body::lower_letrec_body`) and `Foldl*`
    /// (`control_flow::body::lower_foldl_body`) direct-params/tuple-acc/
    /// hybrid-mode bodies — the sole surviving implementation of this shape
    /// since both migrations' hand-built `Document` predecessor
    /// (`generate_direct_var_update_in_loop`) was deleted once every
    /// call site converted to this `ThreadedStmt::Bind` producer
    /// (`render_bind`'s `Direct` arm renders `"let NewVar = <rhs> in "`,
    /// byte-identical to the deleted function's own `docvec!`), so `render`'s
    /// `final_loop_arg_identities` can trace this local's own rebind chain
    /// from its `produces` seed.
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
        // ADR 0131 Phase 2b: a loop or fold in the RHS (or an operand of it)
        // that is a local-threading producer in this body is threaded ahead:
        // its prelude rebinds the locals it writes (lowered by this frame's
        // mode, i.e. extending the loop-argument chain) before the RHS reads
        // the unwrapped value. Done before the target's own identity is read
        // below, because that prelude may rebind the target itself.
        let thread_scope = if self.local_assign_field_write(value).is_none()
            && self.expr_has_local_producer(value)
        {
            Some(self.thread_ahead(value, stmts, frame)?)
        } else {
            None
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

        // BT-3493: the RHS is itself a field write (`r := (self.x :=
        // ...)`, at any parenthesization depth) — hybrid/direct-params mode
        // threads a field write through `lower_field_assignment_bind`'s
        // own class-var/hybrid-mutated-field/plain dispatch (the SAME
        // producer this loop's own bare `self.field := ...` statements
        // already lower through), never through this function's generic
        // `expression_doc` compile below, which has no way to fold that
        // mutation into the field's own direct-param rebind chain (mirrors
        // `lower_local_var_assignment_bind`'s matching BT-3493 fix for the
        // StateAcc-fallback sibling of this exact shape). Alias this local
        // to the field write's own returned value, exactly as `:=`'s "the
        // whole assignment evaluates to the assigned value" semantics
        // require.
        if let Some(field_write) = self.local_assign_field_write(value) {
            let field_val_var =
                self.lower_field_assignment_bind(field_write, frame, span, stmts)?;
            let new_var = self.fresh_temp_var(&canonical);
            self.bind_var(&id.name, &new_var);
            stmts.push(ThreadedStmt::Bind {
                target: VersionedVar::new(VersionPrefix::Gensym(new_var), 1, frame),
                source,
                op: BindOp::Direct(ValueRef::Var(field_val_var)),
                span,
            });
            return Ok(());
        }

        let value_code = self.expression_doc(value)?;
        if let Some(scope) = thread_scope {
            self.finish_precompiled_scope(scope)?;
        }

        let new_var = self.fresh_temp_var(&canonical);
        self.bind_var(&id.name, &new_var);
        stmts.push(ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::Gensym(new_var), 1, frame),
            source,
            op: BindOp::Direct(ValueRef::Doc(value_code)),
            span,
        });
        Ok(())
    }
}
