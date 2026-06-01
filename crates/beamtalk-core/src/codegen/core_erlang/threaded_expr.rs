// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Context-agnostic outer-local mutation threading (BT-2361 steps 1-2).
//!
//! **DDD Context:** Code Generation
//!
//! A captured-local loop (`whileTrue:`/`whileFalse:`, `to:do:`/`to:by:do:`/
//! `timesRepeat:`) or a foldl list-op
//! (`collect:`/`select:`/`reject:`/`inject:into:`/`count:`/`detect:`) lowers to a
//! `{Value, StateAcc}` 2-tuple whose element 1 is the logical result and element 2 is
//! a map of the mutated outer locals (ADR 0041's calling convention). A read+write
//! conditional (`ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:`) is instead inlined as a
//! `case` — in last position binding only its branch's logical value, in non-last /
//! assign-RHS position yielding a flat `{LogicalValue, Mut1, …, MutN}` tuple whose
//! trailing elements rebind the threaded locals positionally. Either way every
//! construct exposes the same two things — a logical value and the outer-local
//! mutations — so the three former consumption paths (value-type, class-method, …)
//! did not disagree about *what* a construct produces, only about what to do with it
//! at the **method boundary**.
//!
//! This module splits that tangle along one line:
//!
//! * **Transform (shared, written once).** Given a last/return-position threading
//!   construct, [`CoreErlangGenerator::lower_threaded_last`] binds its logical value
//!   (tuple element 1) to a fresh result var. The threaded locals do not escape in
//!   last position, so element 2 is discarded. This subsumes the value-type
//!   `emit_vt_last_expr` threading branch, `emit_vt_conditional_last`, and the
//!   class-method `try_generate_class_method_threaded_last`.
//! * **Boundary (per-context adapter, the only thing that varies).** A
//!   [`ThreadingBoundary`] captures how the bound result var is returned/stored:
//!   the value-type `{Result, Self{N}}` / bare-`Result` shape, the class-method
//!   `{class_var_result, Result, ClassVarsN}` / bare-`Result` shape, or the Actor
//!   `{'reply', Reply, NewState}` `gen_server` reply shape (BT-2378). This mirrors the
//!   [`NlrBoundary`](super::NlrBoundary) precedent (BT-2361 step 4 / PR #2408).
//!
//! The Actor boundary is structurally distinct from the other two: its mutated outer
//! locals do not ride a *separate* `StateAcc` map that is discarded at the boundary —
//! element 2 of the construct's `{Value, NewState}` tuple **is** the `gen_server` `State`
//! map (with `__local__`-prefixed local keys threaded in). The boundary therefore binds
//! element 2 to the next state version and threads it onward, supplying that primitive
//! via [`CoreErlangGenerator::lower_actor_threaded_last`] /
//! [`CoreErlangGenerator::emit_actor_threaded_assign_rhs`] — a genuine *extension* of the
//! seam, not a fold of the existing `{Value, StateAcc}` transform.

use super::document::{Document, leaf};
use super::{CoreErlangGenerator, Result};
use crate::ast::Expression;
use crate::docvec;

/// Where a threading construct sits in its enclosing method body. Drives how the
/// bound logical value (tuple element 1) is returned/stored.
///
/// Both `Last` and `Return` flow through the unified emitter (the class-method
/// generator passes `Return` for explicit `^ expr` and `Last` for the implicit final
/// expression; the value-type generator passes `Last`). They currently receive
/// identical post-construct treatment — `Return` is recorded distinctly so the call
/// site's intent is preserved for the later non-last / assign-RHS migration steps. The
/// non-last open-let-chain paths (`generate_vt_*_open`) are already context-shared and
/// are not (yet) routed through this emitter.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ThreadingPosition {
    /// Implicit last expression of the method body.
    Last,
    /// Explicit `^ expr` return.
    Return,
}

/// The per-context threading boundary — the *only* thing that differs between the
/// value-type and class-method consumption of a threaded construct's logical value
/// once the transform is shared.
///
/// Both contexts bind the construct's logical value (tuple element 1) to a result var
/// identically; they disagree only about the Document that returns/stores it. This
/// enum captures that single axis so the last/return-position emitter is written once
/// (see [`CoreErlangGenerator::threading_result_tail`]) instead of being copy-evolved
/// per context — exactly mirroring the [`NlrBoundary`](super::NlrBoundary) seam.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ThreadingBoundary {
    /// Value-type methods: the tail yields `{Result, Self{N}}` when a non-local return
    /// is active (so the normal and NLR-catch paths share a shape), or bare `Result`.
    ValueType { has_nlr: bool },
    /// Class methods: the tail yields `{'class_var_result', Result, ClassVarsN}` when an
    /// earlier statement mutated a class var, or bare `Result` otherwise. Threading
    /// constructs mutate *locals*, not class vars, so the wrapping is driven solely by
    /// `class_var_mutated()`.
    ClassMethod,
    /// Actor (`gen_server`) methods: BT-2378. Structurally distinct from the value-type
    /// and class-method boundaries:
    ///
    /// * **Where threaded state lives.** Actors do not carry mutated outer locals in a
    ///   *separate* `StateAcc` map that is discarded at the boundary; element 2 of the
    ///   construct's `{Value, NewState}` tuple **is** the `gen_server` `State` map (with
    ///   `__local__`-prefixed local keys threaded in). The boundary therefore binds
    ///   element 2 to the next state version and threads it onward, rather than discarding
    ///   it.
    /// * **How the body returns.** Actor method bodies return the `gen_server` reply tuple
    ///   `{'reply', Reply, NewState}`, not a bare value (or a `{Value, Self}` /
    ///   `{class_var_result, …}` value-type shape).
    Actor,
}

/// A threading construct normalized for the boundary to consume.
///
/// The transform lowers a construct into this; the boundary lowers it out to Core
/// Erlang via [`CoreErlangGenerator::threading_result_tail`]. `value_doc` is the open
/// let-chain that binds the logical value (tuple element 1) to `result_var`;
/// `threaded_locals` is the set of outer locals carried in tuple element 2 (discarded
/// in last position, where they do not escape); `position` records last vs explicit
/// return.
pub(super) struct ThreadedExpr {
    /// Open let-chain binding the construct's logical value to `result_var`.
    pub(super) value_doc: Document<'static>,
    /// Core Erlang variable bound to the construct's logical value (tuple element 1).
    pub(super) result_var: String,
    /// Core Erlang variable bound to the construct's threaded state (tuple element 2),
    /// when the boundary needs it. `None` for value-type / class-method last position,
    /// where the mutated outer locals ride a *separate* `StateAcc` map that does not
    /// escape and is discarded. `Some` for the Actor boundary (BT-2378), where element 2
    /// **is** the `gen_server` `State` map that must be returned in the reply tuple.
    pub(super) state_var: Option<String>,
    /// Outer locals the construct threads via tuple element 2. Unused in last position
    /// (the mutations do not escape), retained for the design's representation and any
    /// future non-last routing through the unified emitter.
    #[allow(dead_code)]
    pub(super) threaded_locals: Vec<String>,
    /// Last expression vs explicit `^`-return (same post-construct treatment).
    #[allow(dead_code)]
    pub(super) position: ThreadingPosition,
}

impl CoreErlangGenerator {
    /// Lowers a last/return-position threading construct into a [`ThreadedExpr`],
    /// binding its logical value (tuple element 1) to a fresh result var. The transform is
    /// selected by `boundary`: the value-type / class-method boundaries share the BT-2342
    /// `{Value, StateAcc}` transform primitives below; the Actor boundary delegates to
    /// [`Self::lower_actor_threaded_last`] (BT-2378), whose element 2 is the threaded
    /// `gen_server` `State` rather than a discardable `StateAcc`.
    ///
    /// Returns `None` when `expr` (after peeling redundant parentheses) is not a
    /// recognized threading construct, so the caller falls back to its generic
    /// last-expression path. Handles (value-type / class-method boundaries):
    ///
    /// * loops / foldl list-ops yielding `{Value, StateAcc}` — element 1 is unwrapped
    ///   (loops put `'nil'` there; foldl list-ops put the collected/folded value);
    /// * read+write conditionals — inlined as a `case` binding the branch's logical
    ///   value, avoiding the 0-arg dispatch crash on a stateful arity-1 block.
    ///
    /// The threaded locals do not escape in last position, so tuple element 2 is
    /// discarded. BT-2358: redundant parentheses (`^(items collect: …)`) are peeled so
    /// the construct inside is unwrapped rather than leaking its raw tuple.
    pub(super) fn lower_threaded_last(
        &mut self,
        expr: &Expression,
        position: ThreadingPosition,
        boundary: ThreadingBoundary,
    ) -> Result<Option<ThreadedExpr>> {
        if matches!(boundary, ThreadingBoundary::Actor) {
            return self.lower_actor_threaded_last(expr, position);
        }
        let expr = Self::peel_parens(expr);
        let mut parts: Vec<Document<'static>> = Vec::new();
        let result_var = if self.expr_yields_vt_threaded_tuple(expr) {
            self.emit_vt_threaded_tuple_unwrap_to_var(expr, &mut parts)?
        } else if self.is_conditional_with_vt_local_threading(expr) {
            match self.emit_vt_conditional_case_to_var(expr, &mut parts)? {
                Some(result_var) => result_var,
                None => return Ok(None),
            }
        } else {
            return Ok(None);
        };
        Ok(Some(ThreadedExpr {
            value_doc: Document::Vec(parts),
            result_var,
            state_var: None,
            threaded_locals: Vec::new(),
            position,
        }))
    }

    /// BT-2378: Actor-boundary transform for a last/return-position control-flow construct
    /// that threads state (field mutations, conditionals, loops, foldl list-ops, exception
    /// handlers). Such a construct lowers — via the actor-context `expression_doc` path — to
    /// a `{Value, NewState}` tuple whose element 2 **is** the `gen_server` `State` map (with
    /// any `__local__`-prefixed outer-local keys threaded in).
    ///
    /// Binds element 1 to a fresh result var and element 2 to the next state version, so the
    /// boundary tail can emit `{'reply', Result, NewState}`. Returns `None` when `expr` is not
    /// a state-threading control-flow construct, so the caller falls back to its generic path.
    fn lower_actor_threaded_last(
        &mut self,
        expr: &Expression,
        position: ThreadingPosition,
    ) -> Result<Option<ThreadedExpr>> {
        if !self.control_flow_has_mutations(expr) {
            return Ok(None);
        }
        let tuple_var = self.fresh_temp_var("Tuple");
        let result_var = self.fresh_temp_var("Result");
        let expr_doc = self.expression_doc(expr)?;
        let new_state = self.next_state_var();
        let value_doc = docvec![
            "let ",
            leaf::var(tuple_var.clone()),
            " = ",
            expr_doc,
            " in let ",
            leaf::var(result_var.clone()),
            " = call 'erlang':'element'(1, ",
            leaf::var(tuple_var.clone()),
            ") in let ",
            leaf::var(new_state.clone()),
            " = call 'erlang':'element'(2, ",
            leaf::var(tuple_var),
            ") in ",
        ];
        Ok(Some(ThreadedExpr {
            value_doc,
            result_var,
            state_var: Some(new_state),
            threaded_locals: Vec::new(),
            position,
        }))
    }

    /// Emits a last/return-position threading construct, applying `boundary`'s return
    /// shape — or returns `None` (without mutating `body_parts`) when `expr` is not a
    /// threading construct, so the caller falls back to its generic path.
    ///
    /// This is the single shared entry point that subsumes the value-type
    /// `emit_vt_last_expr`/`emit_vt_conditional_last` threading branches and the
    /// class-method `try_generate_class_method_threaded_last`.
    pub(super) fn emit_threaded_last(
        &mut self,
        expr: &Expression,
        position: ThreadingPosition,
        boundary: ThreadingBoundary,
        body_parts: &mut Vec<Document<'static>>,
    ) -> Result<bool> {
        let Some(threaded) = self.lower_threaded_last(expr, position, boundary)? else {
            return Ok(false);
        };
        body_parts.push(threaded.value_doc);
        body_parts.push(self.threading_result_tail(
            &threaded.result_var,
            threaded.state_var.as_deref(),
            boundary,
        ));
        Ok(true)
    }

    /// Emits a local assignment whose RHS is a threading construct, binding the target
    /// to the construct's logical value (tuple element 1) and rebinding any threaded
    /// sibling outer-locals from element 2 — so both the assigned value and the
    /// mutations are visible to subsequent statements, rather than the target being
    /// bound to the raw `{value, StateAcc}` tuple.
    ///
    /// Returns the Core Erlang variable bound to the assignment target, or `None` (without
    /// mutating `body_parts`) when the RHS is not a threading construct, so the caller
    /// falls back to its generic local-binding path. Handles both the loop / foldl
    /// list-op RHS (`emit_vt_threaded_local_assignment`) and the read+write conditional
    /// RHS (`emit_vt_conditional_assign_rhs`, BT-2359/BT-2371). BT-2358: the conditional
    /// RHS is peeled of redundant parentheses first.
    ///
    /// Shared by the value-type instance-method body sequencer and the class-method
    /// non-last local-var binder, which previously re-derived this branch independently.
    pub(super) fn emit_threaded_assign_rhs(
        &mut self,
        var_name: &str,
        value: &Expression,
        boundary: ThreadingBoundary,
        body_parts: &mut Vec<Document<'static>>,
    ) -> Result<Option<String>> {
        if matches!(boundary, ThreadingBoundary::Actor) {
            return self.emit_actor_threaded_assign_rhs(var_name, value, body_parts);
        }
        if self.expr_yields_vt_threaded_tuple(value) {
            return Ok(Some(
                self.emit_vt_threaded_local_assignment(var_name, value, body_parts)?,
            ));
        }
        let rhs = Self::peel_parens(value);
        if self.is_conditional_with_vt_local_threading(rhs) {
            return Ok(Some(
                self.emit_vt_conditional_assign_rhs(var_name, rhs, body_parts)?,
            ));
        }
        Ok(None)
    }

    /// BT-2378: Actor-boundary assign-RHS transform — `var := <control-flow-with-mutations>`.
    ///
    /// The RHS lowers to a `{Value, NewState}` tuple whose element 2 **is** the `gen_server`
    /// `State` map. Binds the target to element 1, advances the state version to element 2,
    /// and rebinds any `__local__`-threaded sibling outer-locals from the new state so both
    /// the assigned value and the mutations are visible to subsequent statements. Returns the
    /// Core Erlang variable bound to the target, or `None` (without mutating `body_parts`)
    /// when the RHS is not a state-threading control-flow construct.
    fn emit_actor_threaded_assign_rhs(
        &mut self,
        var_name: &str,
        value: &Expression,
        body_parts: &mut Vec<Document<'static>>,
    ) -> Result<Option<String>> {
        if !self.control_flow_has_mutations(value) {
            return Ok(None);
        }
        let core_var = self
            .lookup_var(var_name)
            .map_or_else(|| Self::to_core_erlang_var(var_name), String::clone);
        let tuple_var = self.fresh_temp_var("Tuple");
        let new_state = self.peek_next_state_var();
        let value_str = self.expression_doc(value)?;
        let mut doc_parts: Vec<Document<'static>> = vec![docvec![
            "let ",
            leaf::var(tuple_var.clone()),
            " = ",
            value_str,
            " in let ",
            leaf::var(core_var.clone()),
            " = call 'erlang':'element'(1, ",
            leaf::var(tuple_var.clone()),
            ") in let ",
            leaf::var(new_state.clone()),
            " = call 'erlang':'element'(2, ",
            leaf::var(tuple_var),
            ") in ",
        ]];
        let _ = self.next_state_var();
        self.bind_var(var_name, &core_var);

        if let Some(threaded_vars) = self.get_control_flow_threaded_vars(value) {
            for var in &threaded_vars {
                let tv_core = self
                    .lookup_var(var)
                    .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
                doc_parts.push(docvec![
                    "let ",
                    leaf::var(tv_core),
                    " = call 'maps':'get'(",
                    leaf::atom(Self::local_state_key(var)),
                    ", ",
                    leaf::var(new_state.clone()),
                    ") in ",
                ]);
            }
        }
        body_parts.push(Document::Vec(doc_parts));
        Ok(Some(core_var))
    }

    /// Builds the Document that returns/stores an already-bound threaded result var for
    /// `boundary`. This is the single place the value-type vs class-method divergence
    /// lives — the boundary adapter the design calls for.
    ///
    /// BT-875: Use Document/docvec! — never format!() for Core Erlang fragments.
    pub(super) fn threading_result_tail(
        &self,
        result_var: &str,
        state_var: Option<&str>,
        boundary: ThreadingBoundary,
    ) -> Document<'static> {
        match boundary {
            ThreadingBoundary::Actor => {
                // BT-2378: gen_server reply. `state_var` is element 2 of the construct's
                // `{Value, NewState}` tuple — the threaded gen_server `State` map.
                let state = state_var.map_or_else(
                    || self.current_state_var(),
                    std::string::ToString::to_string,
                );
                docvec![
                    "{'reply', ",
                    leaf::var(result_var.to_string()),
                    ", ",
                    leaf::var(state),
                    "}",
                ]
            }
            ThreadingBoundary::ValueType { has_nlr: true } => {
                let final_self = self.current_self_var();
                docvec![
                    "    {",
                    leaf::var(result_var.to_string()),
                    ", ",
                    leaf::var(final_self),
                    "}\n",
                ]
            }
            ThreadingBoundary::ValueType { has_nlr: false } => {
                docvec!["    ", leaf::var(result_var.to_string()), "\n"]
            }
            ThreadingBoundary::ClassMethod if self.class_var_mutated() => {
                let final_cv = self.current_class_var();
                docvec![
                    "{'class_var_result', ",
                    leaf::var(result_var.to_string()),
                    ", ",
                    leaf::var(final_cv),
                    "}",
                ]
            }
            ThreadingBoundary::ClassMethod => leaf::var(result_var.to_string()),
        }
    }
}
