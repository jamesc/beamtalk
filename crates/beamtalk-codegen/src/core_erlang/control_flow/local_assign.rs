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
use beamtalk_core::ast::{Expression, Pattern};
use beamtalk_core::semantic_analysis::extract_pattern_bindings;
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
        // A control-flow-with-mutations RHS that is not a producer here (an
        // actor list-op whose block only sends to `self`, a nested
        // `ifTrue:ifFalse:`/`match:`/`on:do:` writing only fields) answers a
        // closed `{Value, StateAcc}` 2-tuple — the same shape as the Tier 2
        // case just above — which the plain `let core_var = <val_doc> in`
        // below cannot unwrap. Fall back to `lower_local_var_assignment_bind`,
        // whose C3b unwrap binds the value and the state.
        //
        // BT-4462: that fallback is an unwrap only. A construct that threads a
        // local of this body is a producer here (below), so it never takes
        // it; the legacy write-back that once reread such a construct's other
        // locals out of the carrier is deleted, and C3b reports one that
        // reaches it (`ConstructLocalsNotRebound`).
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
        let source = self.direct_local_identity(&id.name, frame);

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
            self.push_direct_local_rebind(
                &id.name,
                source,
                ValueRef::Var(field_val_var),
                frame,
                span,
                stmts,
            );
            return Ok(());
        }

        let value_code = self.expression_doc(value)?;
        if let Some(scope) = thread_scope {
            self.finish_precompiled_scope(scope)?;
        }

        self.push_direct_local_rebind(
            &id.name,
            source,
            ValueRef::Doc(value_code),
            frame,
            span,
            stmts,
        );
        Ok(())
    }

    /// The identity `local` has in a direct-params/tuple-acc/hybrid loop
    /// body's rebind chain before its next rebind:
    /// [`VersionPrefix::Local`] at the loop's own `produces` seed until the
    /// first rebind, [`VersionPrefix::Gensym`] of the current `lookup_var`
    /// name afterward.
    fn direct_local_identity(&self, local: &str, frame: FrameId) -> VersionedVar {
        let canonical = CoreErlangGenerator::to_core_erlang_var(local);
        match self.lookup_var(local) {
            Some(current) if *current != canonical => {
                VersionedVar::new(VersionPrefix::Gensym(current.clone()), 1, frame)
            }
            _ => VersionedVar::new(VersionPrefix::Local(local.to_string()), 0, frame),
        }
    }

    /// Rebinds `local` to `value` in a direct-params/tuple-acc/hybrid loop
    /// body: a fresh name in generator scope and the `Bind` from `source`
    /// (its identity before the rebind) that extends the loop's argument
    /// chain, which `final_loop_arg_identities` traces.
    fn push_direct_local_rebind(
        &mut self,
        local: &str,
        source: VersionedVar,
        value: ValueRef,
        frame: FrameId,
        span: Span,
        stmts: &mut Vec<ThreadedStmt>,
    ) {
        let new_var = self.fresh_temp_var(&CoreErlangGenerator::to_core_erlang_var(local));
        self.bind_var(local, &new_var);
        stmts.push(ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::Gensym(new_var), 1, frame),
            source,
            op: BindOp::Direct(value),
            span,
        });
    }

    /// BT-4439: the one lowering of a destructuring assignment
    /// `#(a, b) := expr` that is a statement of a threaded body — a loop or
    /// fold body, a conditional arm, a protected block or handler, a Tier 2
    /// stateful block. The right-hand side is sequenced and its elements
    /// extracted ([`Self::generate_destructure_extractions`]), then
    /// every name the pattern binds is written to the frame exactly as
    /// `name := <element>` writes it there, by `write_of(name)`:
    ///
    /// - [`LocalWrite::Slot`]: the frame's scratch-slot `Put` (the
    ///   `__local__` key, or the binding itself in the REPL), as
    ///   `lower_local_var_assignment_bind` writes it;
    /// - [`LocalWrite::Direct`]: a rebind in a direct-params loop's
    ///   argument chain, as [`Self::lower_direct_var_update_in_loop_bind`];
    /// - [`LocalWrite::Plain`]: nothing more than the `let` — a block local
    ///   no construct threads out.
    ///
    /// Before BT-4439 every name took the `Plain` path in every body, so a
    /// threaded local or a REPL binding written by destructuring kept its old
    /// value once the body's frame ended.
    ///
    /// Returns the variable holding the right-hand side (the statement's
    /// value) and whether any name was written beyond its `let`.
    pub(in crate::core_erlang) fn lower_destructure_bind(
        &mut self,
        pattern: &Pattern,
        value: &Expression,
        frame: FrameId,
        span: Span,
        stmts: &mut Vec<ThreadedStmt>,
        write_of: &dyn Fn(&str) -> LocalWrite,
    ) -> Result<(String, bool)> {
        // The right-hand side is sequenced first, as
        // `generate_destructure_bindings_sequenced` sequences it: its prelude
        // may rebind a name the pattern binds (a producer writing `w` in
        // `#(w, _) := #(<loop writing w>, 0)`), so each name's direct-rebind
        // source is read after it, as `lower_direct_var_update_in_loop_bind`
        // reads the target's.
        let scope = self.thread_ahead(value, stmts, frame)?;
        let sources = self.destructure_sources(pattern, frame);
        let (docs, rhs_var, bound) =
            self.generate_destructure_extractions(pattern, value, "let ", " in ")?;
        self.finish_precompiled_scope(scope)?;
        let wrote =
            self.write_destructured_locals(docs, &sources, &bound, frame, span, stmts, write_of);
        Ok((rhs_var, wrote))
    }

    /// [`Self::lower_destructure_bind`] for a right-hand side already
    /// evaluated into `rhs_var` (an arm's `#(a, b) := (self.x := ...)`,
    /// whose field write is lowered first).
    pub(in crate::core_erlang) fn lower_destructure_bind_from_var(
        &mut self,
        pattern: &Pattern,
        rhs_var: &str,
        frame: FrameId,
        span: Span,
        stmts: &mut Vec<ThreadedStmt>,
        write_of: &dyn Fn(&str) -> LocalWrite,
    ) -> Result<bool> {
        let sources = self.destructure_sources(pattern, frame);
        let (docs, bound) =
            self.generate_pattern_extractions_from_var(pattern, rhs_var, "let ", " in ")?;
        Ok(self.write_destructured_locals(docs, &sources, &bound, frame, span, stmts, write_of))
    }

    /// Each name `pattern` binds, with the identity it has before the
    /// extraction's own `let`s rebind it in generator scope: the source a
    /// direct rebind steps from.
    fn destructure_sources(
        &self,
        pattern: &Pattern,
        frame: FrameId,
    ) -> Vec<(String, VersionedVar)> {
        extract_pattern_bindings(pattern)
            .0
            .into_iter()
            .map(|id| {
                let source = self.direct_local_identity(&id.name, frame);
                (id.name.to_string(), source)
            })
            .collect()
    }

    /// Pushes a destructuring's extraction `docs`, then writes each bound
    /// name (`bound`: `(name, core_var)`) by `write_of`. Whether any name was
    /// written beyond its `let`.
    #[allow(clippy::too_many_arguments)] // one argument per fact of the write
    fn write_destructured_locals(
        &mut self,
        docs: Vec<beamtalk_cerl_doc::Document<'static>>,
        sources: &[(String, VersionedVar)],
        bound: &[(String, String)],
        frame: FrameId,
        span: Span,
        stmts: &mut Vec<ThreadedStmt>,
        write_of: &dyn Fn(&str) -> LocalWrite,
    ) -> bool {
        for d in docs {
            stmts.push(ThreadedStmt::Statement(d, span));
        }
        let mut wrote = false;
        for (name, source) in sources {
            let Some((_, core_var)) = bound.iter().find(|(n, _)| n == name) else {
                continue;
            };
            let core_var = core_var.clone();
            match write_of(name) {
                LocalWrite::Plain => {}
                LocalWrite::Direct => {
                    wrote = true;
                    self.push_direct_local_rebind(
                        name,
                        source.clone(),
                        ValueRef::Var(core_var),
                        frame,
                        span,
                        stmts,
                    );
                }
                LocalWrite::Slot => {
                    wrote = true;
                    let key = self.local_scratch_key(name);
                    self.push_local_slot_put(stmts, frame, name, key, &core_var, span);
                    self.bind_var(name, &core_var);
                }
            }
        }
        wrote
    }
}

/// BT-4439: how a threaded body writes a local a destructuring assignment
/// binds — the write `name := <value>` performs in the same body. See
/// [`CoreErlangGenerator::lower_destructure_bind`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::core_erlang) enum LocalWrite {
    /// The `let` alone: a block local no construct threads out.
    Plain,
    /// A rebind in a direct-params/tuple-acc/hybrid loop's argument chain.
    Direct,
    /// The frame's scratch-slot `Put`.
    Slot,
}
