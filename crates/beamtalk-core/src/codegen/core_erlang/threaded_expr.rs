// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Context-agnostic outer-local mutation threading (BT-2361 steps 1-2).
//!
//! **DDD Context:** Code Generation
//!
//! Every threading construct — a captured-local loop (`whileTrue:`/`whileFalse:`,
//! `to:do:`/`to:by:do:`/`timesRepeat:`), a foldl list-op
//! (`collect:`/`select:`/`reject:`/`inject:into:`/`count:`/`detect:`), or a
//! read+write conditional (`ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:`) — lowers to the
//! same runtime shape: a `{Value, StateAcc}` 2-tuple whose element 1 is the logical
//! result and element 2 is a map of the mutated outer locals (ADR 0041's calling
//! convention). The three former consumption paths (value-type, class-method, …)
//! did not disagree about that tuple — they only disagreed about what to do with it
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
//!   the value-type `{Result, Self{N}}` / bare-`Result` shape, or the class-method
//!   `{class_var_result, Result, ClassVarsN}` / bare-`Result` shape. This mirrors the
//!   [`NlrBoundary`](super::NlrBoundary) precedent (BT-2361 step 4 / PR #2408).

use super::document::{Document, leaf};
use super::{CoreErlangGenerator, Result};
use crate::ast::Expression;
use crate::docvec;

/// Where a threading construct sits in its enclosing method body. Drives how the
/// bound logical value (tuple element 1) is returned/stored.
///
/// Only `Last` is currently produced through the unified emitter; the non-last
/// open-let-chain paths (`generate_vt_*_open`) are already context-shared and feed
/// the same `{Value, StateAcc}` tuple. `Return` is the explicit `^`-return variant of
/// `Last` — identical post-construct treatment, recorded distinctly for clarity.
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
    /// binding its logical value (tuple element 1) to a fresh result var via the shared
    /// value-type transform primitives.
    ///
    /// Returns `None` when `expr` (after peeling redundant parentheses) is not a
    /// recognized threading construct, so the caller falls back to its generic
    /// last-expression path. Handles:
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
    ) -> Result<Option<ThreadedExpr>> {
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
        let Some(threaded) = self.lower_threaded_last(expr, position)? else {
            return Ok(false);
        };
        body_parts.push(threaded.value_doc);
        body_parts.push(self.threading_result_tail(&threaded.result_var, boundary));
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
        body_parts: &mut Vec<Document<'static>>,
    ) -> Result<Option<String>> {
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

    /// Builds the Document that returns/stores an already-bound threaded result var for
    /// `boundary`. This is the single place the value-type vs class-method divergence
    /// lives — the boundary adapter the design calls for.
    ///
    /// BT-875: Use Document/docvec! — never format!() for Core Erlang fragments.
    pub(super) fn threading_result_tail(
        &self,
        result_var: &str,
        boundary: ThreadingBoundary,
    ) -> Document<'static> {
        match boundary {
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
