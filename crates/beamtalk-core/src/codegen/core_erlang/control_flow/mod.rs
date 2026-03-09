// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Control flow compilation with state mutation analysis.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! This module handles the compilation of iteration and loop constructs that may
//! mutate actor state. Each construct follows a consistent pattern:
//!
//! 1. **Pure variant**: No state mutations detected, uses simple functional style
//! 2. **Stateful variant**: Mutations detected, requires state threading
//!
//! # Supported Constructs
//!
//! - **List iteration**: `do:`, `collect:`, `select:`, `reject:`, `inject:into:`
//! - **While loops**: `whileTrue:`, `whileFalse:`
//! - **Counted loops**: `repeat`, `timesRepeat:`, `to:do:`, `to:by:do:`
//!
//! Submodules organize the code by domain:
//! - [`list_ops`] — List iteration constructs
//! - [`while_loops`] — While loop constructs
//! - [`counted_loops`] — Counted loop constructs

mod conditionals;
mod counted_loops;
mod exception_handling;
mod list_ops;
mod while_loops;

use std::fmt::Write as FmtWrite;

use super::document::{Document, join};
use super::{CodeGenContext, CoreErlangGenerator, Result, block_analysis};
use crate::ast::Expression;
use crate::docvec;

// ─── ThreadingPlan ────────────────────────────────────────────────────────────

/// Selects the naming convention for state-map keys.
#[derive(Clone, Debug)]
pub(super) enum KeyStyle {
    /// `__local__x` prefix (actor and value-type methods).
    LocalPrefixed,
    /// Plain variable name (REPL mode).
    ReplPlain,
}

/// Pre-computed contract for threading mutable state through a loop body.
///
/// Created once per loop and shared across pack / unpack / extract steps,
/// eliminating the copy-paste that previously existed in 7+ generators.
pub(super) struct ThreadingPlan {
    /// Variables that must be threaded through the loop's `StateAcc`.
    pub threaded_locals: Vec<String>,
    /// The `StateAcc` variable name in effect before the loop begins.
    pub initial_state_var: String,
    /// Determines how map keys are named for threaded locals.
    pub key_style: KeyStyle,
    /// The code-generation context (Actor, `ValueType`, or `Repl`).
    pub context: CodeGenContext,
    /// When `true`, thread locals as direct fun parameters instead of a `StateAcc` map.
    ///
    /// Set when the loop body has no field mutations or self-sends (BT-1275).
    /// Eliminates per-iteration `maps:get` / `maps:put` overhead; the `StateAcc`
    /// map is only rebuilt once at loop exit (in the false arm).
    pub use_direct_params: bool,
    /// When `true`, use a flat tuple as the foldl accumulator instead of a `StateAcc` map.
    ///
    /// Set for `do:`, `collect:`, `select:`/`reject:`, `inject:into:` (Group B — foldl-based)
    /// when the body has only local variable mutations (no field writes, no self-sends,
    /// no complex control flow that generates `StateAcc`-dependent code). BT-1276.
    ///
    /// Eliminates per-iteration `maps:get` / `maps:put` for locally-threaded vars.
    /// The accumulator becomes `{Var1, Var2, ..., VarN}` (for `do:`) or
    /// `{FoldAcc, Var1, ..., VarN}` (for `collect:` / `inject:`).
    pub use_tuple_acc: bool,
}

impl ThreadingPlan {
    /// Creates a `ThreadingPlan` for a foldl-based loop body (`do:`, `collect:`, etc.).
    ///
    /// Always sets `use_direct_params = false` — foldl loops carry state in a `StateAcc`
    /// accumulator map, so direct-parameter threading is not applicable.
    ///
    /// Also sets `repl_loop_mutated` on the generator when in REPL mode.
    pub fn new(
        generator: &mut CoreErlangGenerator,
        body: &crate::ast::Block,
        condition: Option<&Expression>,
    ) -> Self {
        Self::new_impl(generator, body, condition, false, false)
    }

    /// Creates a `ThreadingPlan` for a letrec-based loop body (whileTrue:, timesRepeat:, etc.).
    ///
    /// BT-1275: Sets `use_direct_params = true` when the body has no field mutations or
    /// self-sends, eliminating per-iteration `maps:get`/`maps:put` overhead.
    ///
    /// Only valid for letrec loops where each variable can be passed as a fun parameter.
    pub fn new_for_letrec(
        generator: &mut CoreErlangGenerator,
        body: &crate::ast::Block,
        condition: Option<&Expression>,
    ) -> Self {
        Self::new_impl(generator, body, condition, true, false)
    }

    /// BT-1276: Creates a `ThreadingPlan` for a foldl list-op body with tuple accumulator
    /// optimization (`do:`, `collect:`, `select:`/`reject:`, `inject:into:`).
    ///
    /// Sets `use_tuple_acc = true` when eligible: body has only simple local variable
    /// mutations (no field writes, no self-sends, no complex control flow, no tier-2
    /// assignments to threaded locals). Replaces per-iteration `StateAcc` map operations
    /// with a flat tuple accumulator.
    pub fn new_for_foldl_list_op(
        generator: &mut CoreErlangGenerator,
        body: &crate::ast::Block,
    ) -> Self {
        Self::new_impl(generator, body, None, false, true)
    }

    fn new_impl(
        generator: &mut CoreErlangGenerator,
        body: &crate::ast::Block,
        condition: Option<&Expression>,
        allow_direct_params: bool,
        allow_tuple_acc: bool,
    ) -> Self {
        if generator.is_repl_mode {
            generator.repl_loop_mutated = true;
        }
        let key_style = if generator.is_repl_mode {
            KeyStyle::ReplPlain
        } else {
            KeyStyle::LocalPrefixed
        };
        let context = generator.context;
        let threaded_locals = generator.compute_threaded_locals_for_loop(body, condition);
        let initial_state_var = generator.current_state_var();

        // BT-1275: Use direct fun parameters only for letrec loops when:
        // - caller opts in via `allow_direct_params`
        // - there are local vars to thread
        // - the body has no field mutations or self-sends
        // - the body has no Tier-2 block calls on threaded locals (those return {Result, StateAcc}
        //   and require the StateAcc extraction that `generate_local_var_assignment_in_loop` does)
        // Field mutations require StateAcc to persist actor state across iterations.
        // Pre-analyze body once — reused for both `use_direct_params` and `use_tuple_acc`.
        let body_analysis = block_analysis::analyze_block(body);

        let use_direct_params = if !allow_direct_params || threaded_locals.is_empty() {
            false
        } else {
            let cond_has_state_effects = condition.is_some_and(|c| {
                if let Expression::Block(cb) = c {
                    block_analysis::analyze_block(cb).has_state_effects()
                } else {
                    false
                }
            });
            // Guard: if any threaded-local assignment's RHS is a Tier-2 block call,
            // fall back to StateAcc mode so `generate_local_var_assignment_in_loop`
            // can properly unpack the {Result, NewStateAcc} tuple.
            let body_has_tier2_threaded_assign = body.body.iter().any(|s| {
                if let Expression::Assignment { target, value, .. } = &s.expression {
                    if let Expression::Identifier(id) = target.as_ref() {
                        if threaded_locals.contains(&id.name.to_string()) {
                            return generator.is_tier2_value_call(value);
                        }
                    }
                }
                false
            });
            !body_analysis.has_state_effects()
                && !cond_has_state_effects
                && !body_has_tier2_threaded_assign
        };

        // BT-1276: Tuple accumulator optimization for foldl list-ops.
        //
        // Eligible when the body has only simple local var mutations — no field writes,
        // no self-sends, no tier-2 assignments, and no complex control-flow subexpressions
        // that generate `StateAcc`-dependent code (e.g. `ifTrue:` with inner mutations).
        let use_tuple_acc = if !allow_tuple_acc || threaded_locals.is_empty() {
            false
        } else {
            let body_has_tier2_threaded_assign_tuple = body.body.iter().any(|s| {
                if let Expression::Assignment { target, value, .. } = &s.expression {
                    if let Expression::Identifier(id) = target.as_ref() {
                        if threaded_locals.contains(&id.name.to_string()) {
                            return generator.is_tier2_value_call(value);
                        }
                    }
                }
                false
            });
            let body_has_cf_mutations = body
                .body
                .iter()
                .any(|s| generator.control_flow_has_mutations(&s.expression));
            let body_has_conditional_threaded_writes = body.body.iter().any(|s| {
                CoreErlangGenerator::inline_conditional_writes_threaded(
                    &s.expression,
                    &threaded_locals,
                )
            });
            !body_analysis.has_state_effects()
                && !body_has_tier2_threaded_assign_tuple
                && !body_has_cf_mutations
                && !body_has_conditional_threaded_writes
        };

        Self {
            threaded_locals,
            initial_state_var,
            key_style,
            context,
            use_direct_params,
            use_tuple_acc,
        }
    }

    /// Returns the state-map key for a threaded local variable.
    pub fn state_key(&self, var_name: &str) -> String {
        match self.key_style {
            KeyStyle::LocalPrefixed => CoreErlangGenerator::local_state_key(var_name),
            KeyStyle::ReplPlain => var_name.to_string(),
        }
    }

    /// Generates the pack prefix: a `maps:put` chain that loads the initial `StateAcc`.
    ///
    /// Returns `(pack_doc, init_state_var)` where `init_state_var` names the variable
    /// to pass as the initial `StateAcc` argument to the loop.
    ///
    /// For value-type methods (BT-1053), starts from a fresh `maps:new()` instead
    /// of the actor State (which does not exist in value-type context).
    ///
    /// In direct-params mode (BT-1275) this is a no-op — returns `(Nil, initial_state_var)` since
    /// variables are passed as separate fun arguments instead.
    pub fn generate_pack_prefix(
        &self,
        generator: &mut CoreErlangGenerator,
    ) -> (Document<'static>, String) {
        if self.threaded_locals.is_empty() || self.use_direct_params {
            return (Document::Nil, self.initial_state_var.clone());
        }
        let mut pack_str = String::new();
        // BT-1053: Value-type methods have no actor State — start from a fresh empty map.
        let mut current = if matches!(self.context, CodeGenContext::ValueType) {
            let init_map_var = generator.fresh_temp_var("InitMap");
            let _ = write!(pack_str, "let {init_map_var} = call 'maps':'new'() in ");
            init_map_var
        } else {
            self.initial_state_var.clone()
        };
        for var_name in &self.threaded_locals {
            let packed_var = generator.fresh_temp_var("Packed");
            let core_var = generator
                .lookup_var(var_name)
                .cloned()
                .unwrap_or_else(|| CoreErlangGenerator::to_core_erlang_var(var_name));
            let key = self.state_key(var_name);
            let _ = write!(
                pack_str,
                "let {packed_var} = call 'maps':'put'('{key}', {core_var}, {current}) in "
            );
            current = packed_var;
        }
        (Document::String(pack_str), current)
    }

    /// Generates `let X = maps:get(key, StateAcc) in` for each threaded local
    /// and registers each binding in the generator's current scope.
    ///
    /// Returns the binding documents to prepend to the loop body.
    ///
    /// In direct-params mode (BT-1275) the variables are already fun parameters,
    /// so this only registers the bindings and returns no documents.
    pub fn generate_unpack_at_iteration_start(
        &self,
        generator: &mut CoreErlangGenerator,
    ) -> Vec<Document<'static>> {
        let mut docs = Vec::new();
        for var_name in &self.threaded_locals {
            let core_var = CoreErlangGenerator::to_core_erlang_var(var_name);
            generator.bind_var(var_name, &core_var);
            if !self.use_direct_params {
                let key = self.state_key(var_name);
                docs.push(docvec![
                    "let ",
                    Document::String(core_var),
                    " = call 'maps':'get'('",
                    Document::String(key),
                    "', StateAcc) in ",
                ]);
            }
        }
        docs
    }

    /// Returns the initial argument values for a direct-params loop call (BT-1275).
    ///
    /// These are the current bindings of each threaded local in the generator's
    /// outer scope (before `push_scope` has been called for the loop).
    pub fn initial_direct_args(&self, generator: &CoreErlangGenerator) -> Vec<String> {
        self.threaded_locals
            .iter()
            .map(|v| {
                generator
                    .lookup_var(v)
                    .cloned()
                    .unwrap_or_else(|| CoreErlangGenerator::to_core_erlang_var(v))
            })
            .collect()
    }

    /// Generates the exit `{'nil', StateAcc}` expression for the false arm of a
    /// direct-params loop (BT-1275).
    ///
    /// Because variables are carried as fun parameters, not in a map, the
    /// `StateAcc` must be rebuilt once at loop exit so that the caller can extract
    /// updated values using the same `maps:get` protocol as before.
    ///
    /// `param_names` are the Core Erlang names of each threaded local IN THE CURRENT
    /// ITERATION (i.e. the fun parameter names at the point of the false arm).
    /// For the false-arm case these are the initial parameter names, not updated ones.
    pub fn generate_exit_stateacc(
        &self,
        param_names: &[String],
        generator: &mut CoreErlangGenerator,
    ) -> Document<'static> {
        if self.threaded_locals.is_empty() {
            return docvec![
                "{'nil', ",
                Document::String(self.initial_state_var.clone()),
                "}",
            ];
        }
        let mut docs: Vec<Document<'static>> = Vec::new();
        // BT-1053: Value-type methods have no actor State — start from a fresh empty map.
        let mut current = if matches!(self.context, CodeGenContext::ValueType) {
            let exit_var = generator.fresh_temp_var("ExitSA");
            docs.push(docvec![
                "let ",
                Document::String(exit_var.clone()),
                " = call 'maps':'new'() in ",
            ]);
            exit_var
        } else {
            self.initial_state_var.clone()
        };
        for (var_name, param) in self.threaded_locals.iter().zip(param_names.iter()) {
            let key = self.state_key(var_name);
            let next_var = generator.fresh_temp_var("ExitSA");
            docs.push(docvec![
                "let ",
                Document::String(next_var.clone()),
                " = call 'maps':'put'('",
                Document::String(key),
                "', ",
                Document::String(param.clone()),
                ", ",
                Document::String(current),
                ") in ",
            ]);
            current = next_var;
        }
        docs.push(docvec!["{'nil', ", Document::String(current), "}",]);
        Document::Vec(docs)
    }

    /// Generates `let X = maps:get(key, FinalState) in` for each threaded local
    /// to extract updated values after the loop completes.
    ///
    /// Returns the extract code as a `String` for easy embedding in `format!` expressions.
    pub fn generate_extract_suffix(
        &self,
        final_state_var: &str,
        generator: &CoreErlangGenerator,
    ) -> String {
        let mut s = String::new();
        for var_name in &self.threaded_locals {
            let core_var = generator
                .lookup_var(var_name)
                .cloned()
                .unwrap_or_else(|| CoreErlangGenerator::to_core_erlang_var(var_name));
            let key = self.state_key(var_name);
            let _ = write!(
                s,
                "let {core_var} = call 'maps':'get'('{key}', {final_state_var}) in "
            );
        }
        s
    }

    // ─── BT-1276: Tuple accumulator helpers ───────────────────────────────────

    /// Returns the current Core Erlang bindings of all threaded locals as a
    /// comma-separated string.  Used to build tuple repacks at iteration end.
    ///
    /// Example: `threaded_locals = ["sum", "count"]` → `"Sum1, Count"`.
    pub fn current_vars_str(&self, generator: &CoreErlangGenerator) -> String {
        self.threaded_locals
            .iter()
            .map(|v| {
                generator
                    .lookup_var(v)
                    .cloned()
                    .unwrap_or_else(|| CoreErlangGenerator::to_core_erlang_var(v))
            })
            .collect::<Vec<_>>()
            .join(", ")
    }

    /// Returns the initial-values tuple string constructed from outer-scope bindings.
    ///
    /// Call this **before** `push_scope()` so the bindings reflect the state before
    /// the loop.  Example: `threaded_locals = ["sum"]` → `"{Sum}"`.
    pub fn initial_vars_tuple_str(&self, generator: &CoreErlangGenerator) -> String {
        format!("{{{}}}", self.current_vars_str(generator))
    }

    /// Generates `let V = call 'erlang':'element'(idx, src) in` docs for each
    /// threaded local, and registers the bindings in the generator scope.
    ///
    /// `source_var` — the lambda parameter holding the tuple (e.g. `"StateAcc"` or
    ///   the `acc_state_var` name for `collect:`/`inject:`).
    /// `index_offset` — 1-based index of the first threaded var:
    ///   - 1 for `do:` (whole tuple is the vars)
    ///   - 2 for `collect:` / `filter:` / `inject:` (slot 1 is `AccList` or `Acc`)
    pub fn generate_tuple_unpack_docs(
        &self,
        generator: &mut CoreErlangGenerator,
        source_var: &str,
        index_offset: usize,
    ) -> Vec<Document<'static>> {
        let mut docs = Vec::new();
        for (i, var_name) in self.threaded_locals.iter().enumerate() {
            let core_var = CoreErlangGenerator::to_core_erlang_var(var_name);
            let idx = index_offset + i;
            generator.bind_var(var_name, &core_var);
            docs.push(docvec![
                "let ",
                Document::String(core_var),
                " = call 'erlang':'element'(",
                Document::String(idx.to_string()),
                ", ",
                Document::String(source_var.to_string()),
                ") in ",
            ]);
        }
        docs
    }

    /// Returns element-extraction code after foldl completes for tuple mode.
    ///
    /// Generates `let V = call 'erlang':'element'(idx, acc) in ...` using the
    /// outer-scope binding names (from `lookup_var`) as targets.
    ///
    /// `index_offset` — same as in `generate_tuple_unpack_docs`.
    pub fn generate_tuple_extract_suffix_str(
        &self,
        final_acc_var: &str,
        index_offset: usize,
        generator: &CoreErlangGenerator,
    ) -> String {
        let mut s = String::new();
        for (i, var_name) in self.threaded_locals.iter().enumerate() {
            let core_var = generator
                .lookup_var(var_name)
                .cloned()
                .unwrap_or_else(|| CoreErlangGenerator::to_core_erlang_var(var_name));
            let idx = index_offset + i;
            let _ = write!(
                s,
                "let {core_var} = call 'erlang':'element'({idx}, {final_acc_var}) in "
            );
        }
        s
    }

    /// BT-1276: Re-packs updated locals back into the `StateAcc` map after a tuple-acc loop.
    ///
    /// The tuple accumulator eliminates per-iteration `maps:get`/`maps:put`, but the outer
    /// method-body threading in `generate_method_body_with_reply` still uses `maps:get` to
    /// extract updated locals from the `{result, StateAcc}` return of each list-op expression.
    ///
    /// This method appends `let PkSt1 = maps:put(key1, Var1, State) in ... in` to `s`
    /// and returns the name of the final packed-state variable (`PkStN`).
    ///
    /// Must be called AFTER `generate_tuple_extract_suffix_str` so that the Core Erlang
    /// variable names (e.g. `Total`) refer to the extracted (updated) values.
    pub fn append_repack_stateacc(
        &self,
        s: &mut String,
        generator: &mut CoreErlangGenerator,
    ) -> String {
        let mut current = self.initial_state_var.clone();
        for var_name in &self.threaded_locals {
            let core_var = generator
                .lookup_var(var_name)
                .cloned()
                .unwrap_or_else(|| CoreErlangGenerator::to_core_erlang_var(var_name));
            let key = self.state_key(var_name);
            let pack_var = generator.fresh_temp_var("PkSt");
            let _ = write!(
                s,
                "let {pack_var} = call 'maps':'put'('{key}', {core_var}, {current}) in "
            );
            current = pack_var;
        }
        current
    }
}

// ─── BodyKind ─────────────────────────────────────────────────────────────────

/// Controls how `generate_threaded_loop_body` handles the final expression.
pub(super) enum BodyKind {
    /// Letrec loop body: document ends with a trailing ` in `; caller appends
    /// the recursive `apply` call.  The last non-assignment expression uses the
    /// nested-state-extraction pattern when there are no direct field assignments.
    Letrec,

    /// Foldl `do:` body: final accumulator is `StateAcc{N}`.
    FoldlDo,

    /// Foldl `collect:` body: final accumulator is `{[Result | AccList], StateAcc{N}}`.
    FoldlCollect,

    /// Foldl `select:`/`reject:` body: last expression becomes a predicate;
    /// a `case` expression conditionally includes the item.
    FoldlFilter {
        /// The item variable used to include in the result list.
        item_var: String,
        /// When `true`, negates the predicate (for `reject:`).
        negate: bool,
    },

    /// Foldl `inject:into:` body: final accumulator is `{NewAcc, StateAcc{N}}`.
    FoldlInject,
}

// ─── CountedLoopFrame ─────────────────────────────────────────────────────────

/// Describes the loop-type-specific structure of a counted (`letrec`-based) loop.
///
/// Each `generate_*_with_mutations` for counted loops becomes a thin wrapper
/// that builds a `CountedLoopFrame` and calls `generate_counted_stateful_loop`.
pub(super) struct CountedLoopFrame {
    /// Variable bindings emitted before the `letrec` (e.g. `let N = recv in`).
    pub preamble: Document<'static>,
    /// Name of the letrec function (e.g. `"repeat"` or `"loop"`).
    pub fn_name: String,
    /// The condition header up to `<'true'> when 'true' ->` for the continue arm.
    pub continue_header: Document<'static>,
    /// Expression used as the next counter in the recursive call
    /// (e.g. `"call 'erlang':'+'(I, 1)"`).
    pub next_counter: String,
    /// Initial counter argument for the first `apply` call (e.g. `"1"` or `StartVar`).
    pub initial_counter: String,
    /// The `false` arm and `end` (e.g. `"<'false'> when 'true' -> {'nil', StateAcc} end"`).
    pub false_arm: Document<'static>,
    /// Optional Beamtalk block-parameter name to bind to `"I"`.
    pub body_param: Option<String>,
}

// ─── CoreErlangGenerator impls ────────────────────────────────────────────────

impl CoreErlangGenerator {
    /// Generates the per-statement body for a stateful loop with state threading.
    ///
    /// This is the **single, unified body generator** replacing 7+
    /// `generate_*_body_with_threading` copies.  All per-statement dispatch
    /// (field / self-send / local var / block-local / Tier 2 / nested construct)
    /// lives here exactly once.
    ///
    /// # Caller responsibilities
    ///
    /// - Push any necessary scope **before** calling this function (for block params
    ///   and/or threaded-local unpack bindings).
    /// - Pop the scope **after** this function returns.
    /// - Emit the unpack bindings via `plan.generate_unpack_at_iteration_start`
    ///   before the body (for list ops) or as part of the loop preamble (for letrec loops).
    ///
    /// # Returns
    ///
    /// `(body_doc, final_state_version)` — body document and the `StateAcc` version
    /// number in effect at the end of the body.
    #[allow(clippy::too_many_lines)]
    pub(super) fn generate_threaded_loop_body(
        &mut self,
        body: &crate::ast::Block,
        plan: &ThreadingPlan,
        kind: &BodyKind,
    ) -> Result<(Document<'static>, usize)> {
        let saved_state_version = self.state_version();
        self.set_state_version(0);
        let previous_in_loop_body = self.in_loop_body;
        self.in_loop_body = true;

        // Needed for Letrec: detect whether body has direct field assignments.
        let has_direct_field_assignments = body
            .body
            .iter()
            .any(|s| Self::is_field_assignment(&s.expression));

        let filtered_body: Vec<&Expression> = body
            .body
            .iter()
            .map(|s| &s.expression)
            .filter(|e| !matches!(e, Expression::ExpectDirective { .. }))
            .collect();

        let mut docs: Vec<Document<'static>> = Vec::new();
        let mut has_mutations = false;
        let mut has_plain_lets = false;

        // For FoldlFilter, allocate a pred_var upfront.
        let pred_var: Option<String> = if matches!(kind, BodyKind::FoldlFilter { .. }) {
            Some(self.fresh_temp_var("Pred"))
        } else {
            None
        };

        for (i, expr) in filtered_body.iter().enumerate() {
            let is_last = i == filtered_body.len() - 1;

            // Letrec body uses a space separator between statements.
            if i > 0 && matches!(kind, BodyKind::Letrec) {
                docs.push(Document::Str(" "));
            }

            if Self::is_field_assignment(expr) {
                has_mutations = true;
                let doc = self.generate_field_assignment_open(expr)?;
                docs.push(doc);
                if is_last {
                    self.emit_field_assign_last_expr(&mut docs, kind, pred_var.as_ref());
                }
            } else if self.is_actor_self_send(expr) {
                has_mutations = true;
                let doc = self.generate_self_dispatch_open(expr)?;
                docs.push(doc);
                if is_last {
                    self.emit_self_send_last_expr(&mut docs, kind, pred_var.as_ref());
                }
            } else if Self::is_local_var_assignment(expr) {
                if let Some(doc) =
                    self.try_generate_block_local_plain_let(expr, is_last, &plan.threaded_locals)?
                {
                    has_plain_lets = true;
                    docs.push(doc);
                } else if plan.use_direct_params || plan.use_tuple_acc {
                    // BT-1275/BT-1276: Direct-params or tuple-acc mode — emit
                    // `let NewVar = value in` without a StateAcc map. The var binding
                    // is updated so the final tuple repack references the latest version.
                    has_mutations = true;
                    let assign_doc = self.generate_direct_var_update_in_loop(expr)?;
                    docs.push(assign_doc);
                    if is_last {
                        self.emit_local_assign_last_expr(&mut docs, kind, pred_var.as_ref(), plan);
                    }
                } else {
                    has_mutations = true;
                    let assign_doc = self.generate_local_var_assignment_in_loop(expr)?;
                    docs.push(assign_doc);
                    if is_last {
                        self.emit_local_assign_last_expr(&mut docs, kind, pred_var.as_ref(), plan);
                    }
                }
            } else if let Expression::DestructureAssignment { pattern, value, .. } = expr {
                has_plain_lets = true;
                let binding_docs = self.generate_destructure_bindings(pattern, value)?;
                for d in binding_docs {
                    docs.push(d);
                }
                if is_last {
                    self.emit_destructure_last_expr(
                        &mut docs,
                        kind,
                        pred_var.as_ref(),
                        has_mutations,
                    );
                }
            } else if matches!(kind, BodyKind::FoldlDo)
                && (self.control_flow_has_mutations(expr)
                    || Self::inline_conditional_writes_threaded(expr, &plan.threaded_locals))
            {
                // BT-1053: Inline conditional with local mutations returns {Result, NewStateAcc}.
                // Unpack element(2) so subsequent iterations see the updated StateAcc.
                has_mutations = true;
                let tuple_var = self.fresh_temp_var("CondResult");
                let doc = self.generate_expression(expr)?;
                let new_version = self.state_version() + 1;
                self.set_state_version(new_version);
                let new_state = self.current_state_var();
                docs.push(docvec![
                    "let ",
                    Document::String(tuple_var.clone()),
                    " = ",
                    doc,
                    " in let ",
                    Document::String(new_state.clone()),
                    " = call 'erlang':'element'(2, ",
                    Document::String(tuple_var),
                    ") in ",
                ]);
                if is_last {
                    docs.push(Document::String(self.current_state_var()));
                }
            } else {
                // Non-assignment expression: handling depends on BodyKind.
                self.emit_non_assign_expr(
                    &mut docs,
                    expr,
                    i,
                    is_last,
                    has_mutations,
                    has_plain_lets,
                    has_direct_field_assignments,
                    kind,
                    pred_var.as_ref(),
                    plan,
                )?;
            }
        }

        // FoldlFilter: append the predicate case expression after all statements.
        if let BodyKind::FoldlFilter { item_var, negate } = kind {
            if let Some(pv) = &pred_var {
                let condition = if *negate {
                    format!("call 'erlang':'not'({pv})")
                } else {
                    pv.clone()
                };
                if plan.use_tuple_acc {
                    // BT-1276: Tuple mode — repack current var bindings into the result tuple.
                    let vars = plan.current_vars_str(self);
                    docs.push(Document::String(format!(
                        "case {condition} of \
                         <'true'> when 'true' -> {{[{item_var} | AccList], {vars}}} \
                         <'false'> when 'true' -> {{AccList, {vars}}} end"
                    )));
                } else {
                    let final_state = if has_mutations {
                        self.current_state_var()
                    } else {
                        "StateAcc".to_string()
                    };
                    docs.push(Document::String(format!(
                        "case {condition} of \
                         <'true'> when 'true' -> {{[{item_var} | AccList], {final_state}}} \
                         <'false'> when 'true' -> {{AccList, {final_state}}} end"
                    )));
                }
            }
        }

        let final_state_version = self.state_version();
        self.in_loop_body = previous_in_loop_body;
        self.set_state_version(saved_state_version);
        Ok((Document::Vec(docs), final_state_version))
    }

    // ── Body finalizer helpers (called from generate_threaded_loop_body) ─────

    fn emit_field_assign_last_expr(
        &self,
        docs: &mut Vec<Document<'static>>,
        kind: &BodyKind,
        pred_var: Option<&String>,
    ) {
        match kind {
            BodyKind::Letrec => {
                // Trailing " in " already in doc; caller appends recursive call.
            }
            BodyKind::FoldlDo => {
                docs.push(Document::String(self.current_state_var()));
            }
            BodyKind::FoldlCollect => {
                let fs = self.current_state_var();
                docs.push(Document::String(format!("{{[_Val | AccList], {fs}}}")));
            }
            BodyKind::FoldlFilter { .. } => {
                if let Some(pv) = pred_var {
                    docs.push(Document::String(format!("let {pv} = _Val in ")));
                }
            }
            BodyKind::FoldlInject => {
                let fs = self.current_state_var();
                docs.push(Document::String(format!("{{_Val, {fs}}}")));
            }
        }
    }

    fn emit_self_send_last_expr(
        &mut self,
        docs: &mut Vec<Document<'static>>,
        kind: &BodyKind,
        pred_var: Option<&String>,
    ) {
        match kind {
            BodyKind::Letrec => {
                // Nothing; caller appends recursive call.
            }
            BodyKind::FoldlDo => {
                docs.push(Document::String(self.current_state_var()));
            }
            BodyKind::FoldlCollect => {
                let fs = self.current_state_var();
                if let Some(dv) = self.last_dispatch_var.clone() {
                    let ir = self.fresh_temp_var("ItemResult");
                    docs.push(Document::String(format!(
                        "let {ir} = call 'erlang':'element'(1, {dv}) in \
                         {{[{ir} | AccList], {fs}}}"
                    )));
                } else {
                    docs.push(Document::String(format!("{{['nil' | AccList], {fs}}}")));
                }
            }
            BodyKind::FoldlFilter { .. } => {
                if let Some(pv) = pred_var {
                    if let Some(dv) = self.last_dispatch_var.clone() {
                        docs.push(Document::String(format!(
                            "let {pv} = call 'erlang':'element'(1, {dv}) in "
                        )));
                    } else {
                        docs.push(Document::String(format!("let {pv} = 'nil' in ")));
                    }
                }
            }
            BodyKind::FoldlInject => {
                let fs = self.current_state_var();
                if let Some(dv) = self.last_dispatch_var.clone() {
                    let ar = self.fresh_temp_var("AccResult");
                    docs.push(Document::String(format!(
                        "let {ar} = call 'erlang':'element'(1, {dv}) in {{{ar}, {fs}}}"
                    )));
                } else {
                    docs.push(Document::String(format!("{{'nil', {fs}}}")));
                }
            }
        }
    }

    fn emit_local_assign_last_expr(
        &self,
        docs: &mut Vec<Document<'static>>,
        kind: &BodyKind,
        pred_var: Option<&String>,
        plan: &ThreadingPlan,
    ) {
        if plan.use_tuple_acc {
            // BT-1276: Tuple mode — repack current bindings as tuple accumulator.
            let vars = plan.current_vars_str(self);
            match kind {
                BodyKind::Letrec => {}
                BodyKind::FoldlDo => {
                    docs.push(Document::String(format!(" {{{vars}}}")));
                }
                BodyKind::FoldlCollect => {
                    docs.push(Document::String(format!(" {{[_Val | AccList], {vars}}}")));
                }
                BodyKind::FoldlFilter { .. } => {
                    if let Some(pv) = pred_var {
                        docs.push(Document::String(format!(" let {pv} = _Val in ")));
                    }
                }
                BodyKind::FoldlInject => {
                    docs.push(Document::String(format!(" {{_Val, {vars}}}")));
                }
            }
            return;
        }
        match kind {
            BodyKind::Letrec => {
                // Nothing; caller appends recursive call.
            }
            BodyKind::FoldlDo => {
                docs.push(Document::String(format!(" {}", self.current_state_var())));
            }
            BodyKind::FoldlCollect => {
                let fs = self.current_state_var();
                docs.push(Document::String(format!(" {{[_Val | AccList], {fs}}}")));
            }
            BodyKind::FoldlFilter { .. } => {
                if let Some(pv) = pred_var {
                    docs.push(Document::String(format!(" let {pv} = _Val in ")));
                }
            }
            BodyKind::FoldlInject => {
                let fs = self.current_state_var();
                docs.push(Document::String(format!(" {{_Val, {fs}}}")));
            }
        }
    }

    fn emit_destructure_last_expr(
        &self,
        docs: &mut Vec<Document<'static>>,
        kind: &BodyKind,
        pred_var: Option<&String>,
        has_mutations: bool,
    ) {
        match kind {
            BodyKind::Letrec => {
                // Nothing; caller appends recursive call.
            }
            BodyKind::FoldlDo => {
                docs.push(Document::String(self.current_state_var()));
            }
            BodyKind::FoldlCollect => {
                let fs = if has_mutations {
                    self.current_state_var()
                } else {
                    "StateAcc".to_string()
                };
                docs.push(Document::String(format!("{{['nil' | AccList], {fs}}}")));
            }
            BodyKind::FoldlFilter { .. } => {
                if let Some(pv) = pred_var {
                    docs.push(Document::String(format!("let {pv} = 'false' in ")));
                }
            }
            BodyKind::FoldlInject => {
                let fs = if has_mutations {
                    self.current_state_var()
                } else {
                    "StateAcc".to_string()
                };
                docs.push(Document::String(format!("{{'nil', {fs}}}")));
            }
        }
    }

    #[allow(
        clippy::too_many_arguments,
        clippy::fn_params_excessive_bools,
        clippy::too_many_lines
    )]
    fn emit_non_assign_expr(
        &mut self,
        docs: &mut Vec<Document<'static>>,
        expr: &Expression,
        _i: usize,
        is_last: bool,
        has_mutations: bool,
        has_plain_lets: bool,
        has_direct_field_assignments: bool,
        kind: &BodyKind,
        pred_var: Option<&String>,
        plan: &ThreadingPlan,
    ) -> Result<()> {
        match kind {
            BodyKind::Letrec => {
                if is_last && !has_direct_field_assignments {
                    // BT-478/BT-483: Mutations come from nested constructs.
                    // Extract updated state via element(2).
                    let next_version = self.state_version() + 1;
                    let next_var = format!("StateAcc{next_version}");
                    let tuple_var = format!("_NestTuple{next_version}");
                    let expr_code = self.expression_doc(expr)?;
                    self.set_state_version(next_version);
                    docs.push(docvec![
                        format!("let {tuple_var} = "),
                        expr_code,
                        format!(" in let {next_var} = call 'erlang':'element'(2, {tuple_var}) in"),
                    ]);
                } else {
                    let expr_code = self.expression_doc(expr)?;
                    docs.push(docvec!["let _ = ", expr_code, " in"]);
                }
            }
            BodyKind::FoldlDo => {
                if !is_last {
                    docs.push(Document::Str("let _ = "));
                }
                let doc = self.generate_expression(expr)?;
                docs.push(doc);
                if is_last && (has_mutations || has_plain_lets) {
                    if plan.use_tuple_acc {
                        // BT-1276: Repack threaded locals as tuple.
                        let vars = plan.current_vars_str(self);
                        docs.push(Document::String(format!(" in {{{vars}}}")));
                    } else {
                        docs.push(Document::String(format!(
                            " in {}",
                            self.current_state_var()
                        )));
                    }
                } else if !is_last {
                    docs.push(Document::Str(" in "));
                }
            }
            BodyKind::FoldlCollect => {
                if !is_last {
                    docs.push(Document::Str("let _ = "));
                }
                if is_last {
                    let result_var = self.fresh_temp_var("CollectItem");
                    let expr_code = self.expression_doc(expr)?;
                    if plan.use_tuple_acc {
                        // BT-1276: Tuple mode — repack current vars.
                        let vars = plan.current_vars_str(self);
                        docs.push(docvec![
                            format!("let {result_var} = "),
                            expr_code,
                            format!(" in {{[{result_var} | AccList], {vars}}}"),
                        ]);
                    } else {
                        let fs = if has_mutations {
                            self.current_state_var()
                        } else {
                            "StateAcc".to_string()
                        };
                        docs.push(docvec![
                            format!("let {result_var} = "),
                            expr_code,
                            format!(" in {{[{result_var} | AccList], {fs}}}"),
                        ]);
                    }
                } else {
                    let doc = self.generate_expression(expr)?;
                    docs.push(doc);
                    docs.push(Document::Str(" in "));
                }
            }
            BodyKind::FoldlFilter { .. } => {
                if !is_last {
                    docs.push(Document::Str("let _ = "));
                }
                if is_last {
                    if let Some(pv) = pred_var {
                        let expr_code = self.expression_doc(expr)?;
                        docs.push(docvec![format!("let {pv} = "), expr_code, " in "]);
                    }
                } else {
                    let doc = self.generate_expression(expr)?;
                    docs.push(doc);
                    docs.push(Document::Str(" in "));
                }
            }
            BodyKind::FoldlInject => {
                if !is_last {
                    docs.push(Document::Str("let _ = "));
                }
                if is_last {
                    let acc_var = self.fresh_temp_var("AccOut");
                    let expr_code = self.expression_doc(expr)?;
                    if plan.use_tuple_acc {
                        // BT-1276: Tuple mode — repack current vars.
                        let vars = plan.current_vars_str(self);
                        docs.push(docvec![
                            format!("let {acc_var} = "),
                            expr_code,
                            format!(" in {{{acc_var}, {vars}}}"),
                        ]);
                    } else {
                        let fs = if has_mutations {
                            self.current_state_var()
                        } else {
                            "StateAcc".to_string()
                        };
                        docs.push(docvec![
                            format!("let {acc_var} = "),
                            expr_code,
                            format!(" in {{{acc_var}, {fs}}}"),
                        ]);
                    }
                } else {
                    let doc = self.generate_expression(expr)?;
                    docs.push(doc);
                    docs.push(Document::Str(" in "));
                }
            }
        }
        Ok(())
    }

    /// Generates a stateful counted loop using a `letrec`.
    ///
    /// Handles `timesRepeat:`, `to:do:`, and `to:by:do:` by accepting a `CountedLoopFrame`
    /// that captures the loop-type-specific preamble, condition, and step expression.
    ///
    /// In standard mode the fun signature is `(I, StateAcc)`.
    /// In direct-params mode (BT-1275, no field mutations) it is `(I, Var1, ..., VarN)`
    /// eliminating per-iteration `maps:get` / `maps:put` calls.
    pub(super) fn generate_counted_stateful_loop(
        &mut self,
        frame: &CountedLoopFrame,
        body: &crate::ast::Block,
        plan: &ThreadingPlan,
    ) -> Result<Document<'static>> {
        if plan.use_direct_params {
            return self.generate_counted_stateful_loop_direct(frame, body, plan);
        }

        let (pack_doc, init_state) = plan.generate_pack_prefix(self);

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(pack_doc);
        docs.push(frame.preamble.clone());
        docs.push(Document::String(format!(
            " letrec '{fn_name}'/2 = fun (I, StateAcc) -> ",
            fn_name = frame.fn_name
        )));

        self.push_scope();

        // Bind the block counter param if any (e.g. to:do: [:i | ...] → bind "i" → "I")
        if let Some(ref bt_name) = frame.body_param {
            self.bind_var(bt_name, "I");
        }

        // Unpack threaded locals at the top of each iteration
        let unpack_docs = plan.generate_unpack_at_iteration_start(self);
        docs.extend(unpack_docs);

        // Condition + true arm
        docs.push(frame.continue_header.clone());

        // Body
        let (body_doc, final_state_version) =
            self.generate_threaded_loop_body(body, plan, &BodyKind::Letrec)?;
        docs.push(body_doc);
        let final_state_var = if final_state_version == 0 {
            "StateAcc".to_string()
        } else {
            format!("StateAcc{final_state_version}")
        };

        self.pop_scope();

        // Recursive call + false arm + initial apply
        docs.push(docvec![
            format!(
                " apply '{fn_name}'/2 ({next}, {fstate}) ",
                fn_name = frame.fn_name,
                next = frame.next_counter,
                fstate = final_state_var,
            ),
            frame.false_arm.clone(),
            Document::String(format!(
                "in apply '{fn_name}'/2 ({init_ctr}, {init_state})",
                fn_name = frame.fn_name,
                init_ctr = frame.initial_counter,
            )),
        ]);

        Ok(Document::Vec(docs))
    }

    /// BT-1275: Direct-params variant of `generate_counted_stateful_loop`.
    ///
    /// Uses `fun (I, Var1, ..., VarN)` instead of `fun (I, StateAcc)`.
    /// The `StateAcc` map is rebuilt only once in the false (exit) arm.
    fn generate_counted_stateful_loop_direct(
        &mut self,
        frame: &CountedLoopFrame,
        body: &crate::ast::Block,
        plan: &ThreadingPlan,
    ) -> Result<Document<'static>> {
        // Collect initial arg values from the outer scope (before push_scope overwrites them).
        let initial_direct_args = plan.initial_direct_args(self);

        // Build the fun parameter list: (I, Var1, ..., VarN)
        let param_names: Vec<String> = plan
            .threaded_locals
            .iter()
            .map(|v| CoreErlangGenerator::to_core_erlang_var(v))
            .collect();
        let arity = 1 + param_names.len();
        let param_list_doc = join(
            std::iter::once(Document::Str("I"))
                .chain(param_names.iter().map(|v| Document::String(v.clone()))),
            &Document::Str(", "),
        );

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(frame.preamble.clone());
        docs.push(docvec![
            " letrec '",
            Document::String(frame.fn_name.clone()),
            "'/",
            Document::String(arity.to_string()),
            " = fun (",
            param_list_doc,
            ") -> ",
        ]);

        self.push_scope();

        // Bind the block counter param if any (e.g. to:do: [:i | ...] → bind "i" → "I")
        if let Some(ref bt_name) = frame.body_param {
            self.bind_var(bt_name, "I");
        }

        // Register var → param bindings (no unpack docs emitted in direct-params mode).
        let unpack_docs = plan.generate_unpack_at_iteration_start(self);
        debug_assert!(
            unpack_docs.is_empty(),
            "direct params: unpack should emit no code"
        );

        // Condition + true arm
        docs.push(frame.continue_header.clone());

        // Body
        let (body_doc, _) = self.generate_threaded_loop_body(body, plan, &BodyKind::Letrec)?;
        docs.push(body_doc);

        // Collect final var names after body execution (updated bindings inside scope).
        let final_args: Vec<String> = plan
            .threaded_locals
            .iter()
            .map(|v| {
                self.lookup_var(v)
                    .cloned()
                    .unwrap_or_else(|| CoreErlangGenerator::to_core_erlang_var(v))
            })
            .collect();

        // Build exit StateAcc using the INITIAL param names (current iteration values).
        let exit_stateacc = plan.generate_exit_stateacc(&param_names, self);

        self.pop_scope();

        // Build Document arg lists for the recursive call and the initial apply.
        let recursive_args_doc = join(
            std::iter::once(Document::String(frame.next_counter.clone()))
                .chain(final_args.into_iter().map(Document::String)),
            &Document::Str(", "),
        );
        let initial_args_doc = join(
            std::iter::once(Document::String(frame.initial_counter.clone()))
                .chain(initial_direct_args.into_iter().map(Document::String)),
            &Document::Str(", "),
        );

        // Recursive call + false arm (with rebuilt StateAcc) + initial apply.
        docs.push(docvec![
            " apply '",
            Document::String(frame.fn_name.clone()),
            "'/",
            Document::String(arity.to_string()),
            " (",
            recursive_args_doc,
            ") ",
            "<'false'> when 'true' -> ",
            exit_stateacc,
            " end ",
            "in apply '",
            Document::String(frame.fn_name.clone()),
            "'/",
            Document::String(arity.to_string()),
            " (",
            initial_args_doc,
            ")",
        ]);

        Ok(Document::Vec(docs))
    }

    // ── Compat shim ───────────────────────────────────────────────────────────

    /// Generates the foldl lambda body for a `do:` loop with state threading.
    ///
    /// This is a forwarding shim used by `value_type_codegen::generate_value_type_do_open`,
    /// which manages its own pack/extract prefix/suffix independently.
    pub(in crate::codegen::core_erlang) fn generate_list_do_body_with_threading(
        &mut self,
        body: &crate::ast::Block,
        item_var: &str,
    ) -> Result<Document<'static>> {
        let plan = ThreadingPlan::new(self, body, None);
        self.push_scope();
        if let Some(param) = body.parameters.first() {
            self.bind_var(&param.name, item_var);
        }
        let mut docs = plan.generate_unpack_at_iteration_start(self);
        let (body_doc, _) = self.generate_threaded_loop_body(body, &plan, &BodyKind::FoldlDo)?;
        docs.push(body_doc);
        self.pop_scope();
        Ok(Document::Vec(docs))
    }

    // ── Shared helpers ────────────────────────────────────────────────────────

    /// BT-598: Returns the state map key for a local variable.
    /// Uses a `__local__` prefix to prevent collision with actor field names.
    pub(super) fn local_state_key(var_name: &str) -> String {
        format!("__local__{var_name}")
    }

    /// BT-598/BT-1053: Compute local variables that need threading through a loop's `StateAcc`.
    ///
    /// For actor methods: returns vars that are both read and written in the block
    /// (excluding block parameters). Reads from an optional condition block are merged.
    ///
    /// For value-type methods (BT-1053): returns vars that are captured from the outer
    /// scope AND written in the block. Using `captured_reads` (not all reads) avoids
    /// threading block-internal temporaries that happen to be read+written within the block.
    ///
    /// Returns empty for REPL mode (handled separately) and other contexts.
    pub(super) fn compute_threaded_locals_for_loop(
        &self,
        body: &crate::ast::Block,
        condition: Option<&Expression>,
    ) -> Vec<String> {
        if self.is_repl_mode {
            return Vec::new();
        }

        let analysis = block_analysis::analyze_block(body);
        let block_params: std::collections::HashSet<String> =
            body.parameters.iter().map(|p| p.name.to_string()).collect();

        match self.context {
            CodeGenContext::Actor => {
                // BT-1224: Only thread vars captured from the outer scope that are also
                // written in the block. Using `captured_reads` (not `local_reads`) excludes
                // block-internal temporaries that are first defined then read within the block.
                // Using `local_reads` caused unbound_var errors in dispatch/4 because packing
                // code tried to reference unbound Core Erlang variables (e.g. `Y`) that only
                // exist inside the lambda, not in the outer dispatch/4 function.
                let mut all_captured_reads = analysis.captured_reads.clone();
                let mut all_writes = analysis.local_writes.clone();
                if let Some(Expression::Block(cond_block)) = condition {
                    let cond_analysis = block_analysis::analyze_block(cond_block);
                    all_captured_reads = all_captured_reads
                        .union(&cond_analysis.captured_reads)
                        .cloned()
                        .collect();
                    // BT-1224: Also include writes from the condition block so that
                    // variables first written in a condition are included in threading.
                    all_writes = all_writes
                        .union(&cond_analysis.local_writes)
                        .cloned()
                        .collect();
                }
                all_captured_reads
                    .intersection(&all_writes)
                    .filter(|v| !block_params.contains(*v))
                    .cloned()
                    .collect::<std::collections::BTreeSet<_>>()
                    .into_iter()
                    .collect()
            }
            CodeGenContext::ValueType => {
                // BT-1053: Only thread vars captured from the outer scope that are also
                // written in the block. `captured_reads` excludes block-internal temps.
                analysis
                    .captured_reads
                    .intersection(&analysis.local_writes)
                    .filter(|v| !block_params.contains(*v))
                    .cloned()
                    .collect::<std::collections::BTreeSet<_>>()
                    .into_iter()
                    .collect()
            }
            CodeGenContext::Repl => Vec::new(),
        }
    }

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
        if is_last || self.is_repl_mode {
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
        let core_var = self
            .lookup_var(&id.name)
            .map_or_else(|| Self::to_core_erlang_var(&id.name), String::clone);
        let val_doc = self.expression_doc(value)?;
        self.bind_var(&id.name, &core_var);
        Ok(Some(docvec![
            "let ",
            Document::String(core_var),
            " = ",
            val_doc,
            " in ",
        ]))
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
    pub(super) fn generate_direct_var_update_in_loop(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::Identifier(id) = target.as_ref() {
                let value_code = self.expression_doc(value)?;
                // Allocate a fresh versioned name (e.g. Sum1, Sum2 ...) and rebind.
                let new_var =
                    self.fresh_temp_var(&CoreErlangGenerator::to_core_erlang_var(&id.name));
                self.bind_var(&id.name, &new_var);
                return Ok(docvec![
                    "let ",
                    Document::String(new_var),
                    " = ",
                    value_code,
                    " in ",
                ]);
            }
        }
        Ok(Document::Nil)
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
    pub(super) fn generate_local_var_assignment_in_loop(
        &mut self,
        expr: &Expression,
    ) -> Result<Document<'static>> {
        if let Expression::Assignment { target, value, .. } = expr {
            if let Expression::Identifier(id) = target.as_ref() {
                let val_var = self.fresh_temp_var("Val");
                let current_state = if self.state_version() == 0 {
                    "StateAcc".to_string()
                } else {
                    format!("StateAcc{}", self.state_version())
                };

                // BT-790: In REPL mode, use the plain variable name as the key
                // (no __local__ prefix) since there are no actor fields to collide with.
                // This ensures reads (`maps:get('x', StateAcc)`) match writes
                // (`maps:put('x', ..., StateAcc)`), allowing mutations to accumulate
                // correctly across loop iterations.
                let state_key = if self.is_repl_mode {
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
                    let t2_tuple = self.fresh_temp_var("T2");
                    let t2_state = self.fresh_temp_var("T2St");
                    let value_code = self.expression_doc(value)?;

                    let _ = self.next_state_var();
                    let new_state = if self.in_loop_body {
                        self.current_state_var()
                    } else {
                        format!("State{}", self.state_version())
                    };

                    // BT-1053: Record val_var so callers (e.g. generate_conditional_branch_inline)
                    // can use it as the branch result via last_open_scope_result.
                    self.last_open_scope_result = Some(val_var.clone());

                    return Ok(docvec![
                        "let ",
                        Document::String(t2_tuple.clone()),
                        " = ",
                        value_code,
                        " in let ",
                        Document::String(val_var.clone()),
                        " = call 'erlang':'element'(1, ",
                        Document::String(t2_tuple.clone()),
                        ") in let ",
                        Document::String(t2_state.clone()),
                        " = call 'erlang':'element'(2, ",
                        Document::String(t2_tuple),
                        ") in let ",
                        Document::String(new_state),
                        " = call 'maps':'put'('",
                        state_key.to_string(),
                        "', ",
                        Document::String(val_var.clone()),
                        ", ",
                        Document::String(t2_state),
                        ") in ",
                    ]);
                }

                // Capture value expression (ADR 0018 bridge)
                let value_code = self.expression_doc(value)?;

                // Increment state version for the new state
                let _ = self.next_state_var();
                let new_state = if self.in_loop_body {
                    self.current_state_var()
                } else {
                    format!("State{}", self.state_version())
                };

                // BT-1053: Record val_var so callers (e.g. generate_conditional_branch_inline)
                // can use it as the branch result via last_open_scope_result.
                self.last_open_scope_result = Some(val_var.clone());

                return Ok(docvec![
                    "let ",
                    val_var.clone(),
                    " = ",
                    value_code,
                    " in let ",
                    new_state,
                    " = call 'maps':'put'('",
                    state_key.to_string(),
                    "', ",
                    val_var.clone(),
                    ", ",
                    current_state,
                    ") in ",
                ]);
            }
        }
        Ok(Document::Nil)
    }

    /// Returns `true` if `expr` is an inline conditional (`ifTrue:` / `ifFalse:` /
    /// `ifTrue:ifFalse:`) whose block argument writes to at least one variable in `threaded`.
    ///
    /// This catches the "pure-overwrite" pattern like `each > max ifTrue: [max := each]`
    /// where `max` is in `threaded` but the inner block's `captured_reads` is empty
    /// (no read-before-write), so `control_flow_has_mutations` returns false even
    /// though we must thread `max` through `StateAcc`.
    pub(super) fn inline_conditional_writes_threaded(
        expr: &Expression,
        threaded: &[String],
    ) -> bool {
        use crate::ast::MessageSelector;
        if let Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        {
            let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
            if matches!(
                sel.as_str(),
                "ifTrue:" | "ifFalse:" | "ifTrue:ifFalse:" | "ifNotNil:"
            ) {
                for arg in arguments {
                    if let Expression::Block(block) = arg {
                        let analysis = block_analysis::analyze_block(block);
                        if analysis.local_writes.iter().any(|v| threaded.contains(v)) {
                            return true;
                        }
                    }
                }
            }
        }
        false
    }
}
